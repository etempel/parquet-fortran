!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Turning a parquet file's columns into `parquet_column` stores: deciding each column's kind
!! from the file schema, driving the per-kind readers, and freeing the Arrow buffers as it goes.
!!
!! The work is split in two on purpose. **Classification** (`table_classify`) settles a column's
!! kind, width and readability from the file SCHEMA alone and runs for every column at open time;
!! it reads no data, which is what lets `%kind`/`%width` answer about a column nobody has touched.
!! **Materialization** (`table_materialize`) reads the values, and happens on first touch.
!!
!! The memory rule this file exists to enforce: the table owns the sole Fortran-side copy of each
!! column, and the reader's decoded Arrow array is released the moment that copy exists. Without
!! that release a materialized table would hold the whole file twice for the reader's entire
!! lifetime. A struct is cached by the reader as ONE array covering all its leaves, which is why
!! there are two release policies rather than one -- see `table_materialize_all`.
submodule (parquet_tables) parquet_tables_read
    implicit none
    !
contains
    !
    module procedure table_kind_from_type
        ! parquet_get_column_type reports the ELEMENT type of a vector column, so col_size is
        ! what decides between the scalar and *_VEC form of each kind.
        logical :: vec
        !
        ok = .true.
        vec = col_size > 1
        select case (trim(type_name))
        case ("int32")
            kind = merge(PK_INT32_VEC, PK_INT32, vec)
        case ("int64")
            kind = merge(PK_INT64_VEC, PK_INT64, vec)
        case ("float32")
            kind = merge(PK_FLOAT32_VEC, PK_FLOAT32, vec)
        case ("float64")
            kind = merge(PK_FLOAT64_VEC, PK_FLOAT64, vec)
        case ("boolean")
            kind = merge(PK_LOGICAL_VEC, PK_LOGICAL, vec)
        case ("string")
            kind = merge(PK_STRING_VEC, PK_STRING, vec)
        case ("date")
            kind = merge(PK_DATE_VEC, PK_DATE, vec)
        case ("time")
            kind = merge(PK_TIME_VEC, PK_TIME, vec)
        case ("timestamp")
            kind = merge(PK_TIMESTAMP_VEC, PK_TIMESTAMP, vec)
        case default
            kind = PK_NONE
            ok = .false.
        end select
    end procedure table_kind_from_type
    !
    module procedure table_classify
        character(len=:), allocatable :: type_name
        integer :: kind, col_size
        logical :: ok
        !
        associate (slot => cache%cols(idx))
            ! Ask whether the type is readable BEFORE asking what it is: parquet_get_column_type
            ! error stops on a type outside its nine canonical tokens, so probing with
            ! parquet_column_exists(types=...) first is what keeps one exotic column from making
            ! the whole file unopenable.
            if (.not. parquet_column_exists(cache%reader, slot%file_name, &
                    types="int32,int64,float32,float64,string,boolean,date,time,timestamp")) then
                slot%supported = .false.
                slot%declared_kind = PK_NONE
                slot%width = 1
                slot%residency = RES_EMPTY
                return
            end if
            call parquet_get_column_type(cache%reader, slot%file_name, type_name)
            ! Supportedness comes from the element type alone -- table_kind_from_type only uses
            ! col_size to pick between a kind and its *_VEC form -- so it can be settled here even
            ! for a column whose width is not knowable yet.
            call table_kind_from_type(type_name, 1, kind, ok)
            if (.not. ok) then
                slot%supported = .false.
                slot%declared_kind = PK_NONE
                slot%width = 1
                slot%residency = RES_EMPTY
                return
            end if
            slot%supported = .true.
            slot%residency = RES_EMPTY
            ! A plain LIST/LARGE_LIST is the one type whose width lives in the data rather than the
            ! schema, so it is DEFERRED rather than measured here: classifying it now would mean
            ! decoding the column at open, which is exactly what makes a lazy open worthless. Its
            ! kind and width are resolved by table_resolve_width on first use.
            if (parquet_column_width_needs_data(cache%reader, slot%file_name)) then
                slot%declared_kind = PK_NONE
                slot%width = 0
                slot%width_pending = .true.
                return
            end if
            ! Every remaining type -- scalar or FIXED_SIZE_LIST -- answers from the schema, free.
            call parquet_get_col_size(cache%reader, slot%file_name, col_size)
            call table_kind_from_type(type_name, col_size, kind, ok)
            slot%declared_kind = kind
            slot%width = max(col_size, 1)
        end associate
    end procedure table_classify
    !
    module procedure table_materialize
        associate (slot => cache%cols(idx))
            ! Units are not read from the file in this milestone: their source is a read-time
            ! MAML's `unit:` key, which does not exist yet. %unit therefore reports "" for a
            ! file-backed column, and %add_column(unit=) is the only way to set one.
            if (sc%regime == REGIME_SLICE) then
                call materialize_slice(cache, sc, idx)
            else
                call table_materialize_kind(slot%declared_kind, cache%reader, slot%file_name, &
                    slot%values, sc%nrows, int(slot%width, int32), "")
            end if
            slot%residency = RES_FULL
        end associate
        cache%reads_started = .true.
    end procedure table_materialize
    !
    !> Assembles one column from just the row groups covering the table's slice.
    !!
    !! The full regime reads a column in one call; a slice cannot, because a whole-column read
    !! would decode every row group in the file -- the very cost the slice regime exists to
    !! avoid. So each covering row group is read on its own, the first and last are trimmed to
    !! the slice's own bounds, and the pieces are concatenated.
    !!
    !! Trimming happens on the CHUNK, not on the assembled column: the overshoot is then bounded
    !! by one row group at each end instead of being carried through the whole assembly.
    subroutine materialize_slice(cache, sc, idx)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        type(table_scope), intent(in) :: sc               !! rows this table covers.
        integer, intent(in) :: idx                        !! slot to fill.
        type(parquet_column) :: chunk
        logical, allocatable :: keep(:)
        integer(int64) :: rg, rg_lo, rg_hi, lo, hi, rows_rg
        !
        associate (slot => cache%cols(idx), bounds => cache%rg_bounds)
            call slot%values%init(slot%declared_kind, 0_int64, int(slot%width, int32), "")
            do rg = 1_int64, size(bounds, 2, kind=int64)
                rg_lo = bounds(1, rg)
                rg_hi = bounds(2, rg)
                if (rg_hi < sc%row_lo .or. rg_lo > sc%row_hi) cycle
                rows_rg = rg_hi - rg_lo + 1_int64
                call table_materialize_chunk_kind(slot%declared_kind, cache%reader, &
                    slot%file_name, rg, chunk, rows_rg, int(slot%width, int32), "")
                lo = max(sc%row_lo, rg_lo)
                hi = min(sc%row_hi, rg_hi)
                if (lo > rg_lo .or. hi < rg_hi) then
                    allocate(keep(rows_rg))
                    keep = .false.
                    keep(lo - rg_lo + 1_int64:hi - rg_lo + 1_int64) = .true.
                    call chunk%delete_by_mask(keep)
                    deallocate(keep)
                end if
                call slot%values%append(chunk)
                call chunk%clear()
            end do
        end associate
    end subroutine materialize_slice
    !
    module procedure table_release_one
        call parquet_release_column(cache%reader, top_level_of(name))
    end procedure table_release_one
    !
    module procedure record_open_thread
#ifdef _OPENMP
        use omp_lib, only : omp_in_parallel, omp_get_thread_num
        cache%opened_in_parallel = omp_in_parallel()
        if (cache%opened_in_parallel) then
            cache%owner_thread = omp_get_thread_num()
        else
            cache%owner_thread = -1
        end if
#else
        cache%opened_in_parallel = .false.
        cache%owner_thread = -1
#endif
    end procedure record_open_thread
    !
    module procedure unsafe_first_touch
#ifdef _OPENMP
        use omp_lib, only : omp_in_parallel, omp_get_thread_num
        unsafe = .false.
        if (.not. omp_in_parallel()) return
        ! A table this very thread opened inside the region cannot be shared with another thread
        ! -- that is the shape of a parallel per-slice program (each thread opens its own table
        ! over its own row range), and refusing it would make the slice regime unusable exactly
        ! where it is most useful. Anything else may be shared, and a first touch on a shared
        ! store is what RF4 forbids.
        unsafe = .not. (cache%opened_in_parallel .and. cache%owner_thread == omp_get_thread_num())
#else
        unsafe = .false.
#endif
    end procedure unsafe_first_touch
    !
    module procedure table_resolve_width
        character(len=:), allocatable :: type_name, sfx
        integer(int64) :: rg_lo, rg_hi
        integer :: kind, w
        logical :: ok
        !
        associate (slot => cache%cols(idx))
            if (.not. slot%width_pending) return
            ! Resolving reads data, so it is a first touch as far as RF4 is concerned even when it
            ! does not materialize anything -- a shared store must not have it happen concurrently.
            if (unsafe_first_touch(cache)) then
                call table_context_suffix(cache, slot%name, sfx)
                error stop EP // trim(proc) // ": this column's width is not known yet, and " // &
                    "finding it means reading the column; do it before the parallel region " // &
                    "(%kind, %width, %prefetch or %materialize_all)" // sfx
            end if
            call resolve_width_row_groups(cache, sc, rg_lo, rg_hi)
            call parquet_measure_list_width(cache%reader, slot%file_name, rg_lo, rg_hi, proven, w)
            call parquet_get_column_type(cache%reader, slot%file_name, type_name)
            call table_kind_from_type(type_name, w, kind, ok)
            slot%declared_kind = kind
            ! An empty column measures as 0; a descriptor width is always at least 1, exactly as
            ! table_classify's own max(col_size, 1) does for the schema-known kinds.
            slot%width = max(w, 1)
            slot%width_pending = .false.
        end associate
    end procedure table_resolve_width
    !
    !> The 1-based inclusive row-group range a scope covers, as table_resolve_width's measurement
    !> bounds. 0/0 means "every row group" -- which is both what the whole-file regime wants and
    !> what the C++ side reads as "no range given".
    subroutine resolve_width_row_groups(cache, sc, rg_lo, rg_hi)
        type(parquet_table_cache), intent(in) :: cache !! the column store.
        type(table_scope), intent(in) :: sc            !! rows this table covers.
        integer(int64), intent(out) :: rg_lo           !! first row group, or 0 for all.
        integer(int64), intent(out) :: rg_hi           !! last row group, or 0 for all.
        integer(int64) :: rg
        !
        rg_lo = 0_int64
        rg_hi = 0_int64
        if (sc%regime /= REGIME_SLICE) return
        if (.not. allocated(cache%rg_bounds)) return
        do rg = 1_int64, size(cache%rg_bounds, 2, kind=int64)
            if (cache%rg_bounds(2, rg) < sc%row_lo .or. cache%rg_bounds(1, rg) > sc%row_hi) cycle
            if (rg_lo == 0_int64) rg_lo = rg
            rg_hi = rg
        end do
    end subroutine resolve_width_row_groups
    !
    module procedure table_touch
        character(len=:), allocatable :: sfx
        !
        if (cache%cols(idx)%residency == RES_FULL) return
        ! Already-resident reads never get here, which is the point: they take no lock and run
        ! fully parallel. A FIRST touch is different -- it allocates and publishes shared state
        ! with no ordering guarantee behind it, so another thread could see RES_FULL before the
        ! values it advertises are visible. Rather than build double-checked locking around a
        ! read path that must stay free, v1 forbids the situation outright and says how to avoid
        ! it (RF4). A table the touching thread opened inside the region is exempt -- see
        ! unsafe_first_touch.
        if (unsafe_first_touch(cache)) then
            call table_context_suffix(cache, cache%cols(idx)%name, sfx)
            error stop EP // trim(proc) // ": this column was not read before the parallel " // &
                "region; call table%prefetch(...) or table%materialize_all() before it. Note " // &
                "%kind and %width count as a read for a variable-length LIST column, whose " // &
                "width can only be found by reading it" // sfx
        end if
        associate (slot => cache%cols(idx))
            if (.not. slot%file_source) then
                call table_context_suffix(cache, slot%name, sfx)
                error stop EP // trim(proc) // ": this column holds no values and has no file " // &
                    "column to read them from" // sfx
            end if
            ! A deferred-width column has to be classified before it can be materialized, since the
            ! materialize dispatches on the kind. The footer screen's unproven candidate is enough
            ! here on purpose: the read below checks every row's length against the width it was
            ! given (get_uniform_list_values, parquet_wrapper.cpp) and aborts on a mismatch, so a
            ! wrong candidate fails loudly and the read doubles as the proof -- which is what keeps
            ! this to ONE pass over the data instead of measuring first and then reading.
            call table_resolve_width(cache, sc, idx, .false., proc)
            if (sc%detached) then
                ! D6: once a row-structural mutation has changed the row set, a column read from
                ! the file would no longer line up with the columns already in memory.
                call table_context_suffix(cache, slot%name, sfx)
                error stop EP // trim(proc) // ": this table has been detached from its file by " // &
                    "a row-structural change; materialize a column before mutating rows" // sfx
            end if
        end associate
        call table_materialize(cache, sc, idx)
        ! Release policy for a SINGLE first touch: free the Arrow buffers straight away. For a
        ! struct leaf that means the struct's array is decoded again when a sibling leaf is first
        ! touched -- the deliberate trade, since holding it would keep the whole struct alive for
        ! a table that may never read the other leaves at all. %prefetch of several leaves at
        ! once avoids the re-decode (see table_materialize_all).
        call table_release_one(cache, cache%cols(idx)%file_name)
    end procedure table_touch
    !
    module procedure table_materialize_all
        logical, allocatable :: want(:)
        !
        allocate(want(cache%ncols))
        want = .true.
        call materialize_marked(cache, sc, want)
    end procedure table_materialize_all
    !
    !> Reads every marked slot in ONE pass, with the batch release policy.
    !!
    !! The reader caches a struct as ONE array shared by all its leaves, so releasing after every
    !! leaf would re-read the struct once per leaf. Slots are in file schema order, which puts a
    !! struct's leaves next to each other, so releasing the PREVIOUS top-level name as soon as the
    !! top-level changes frees each array exactly once, at the earliest point it is safe to. A
    !! single first touch cannot use this (there is no following column to compare against) and
    !! releases immediately instead -- see `table_touch`. That difference is the whole reason
    !! `%prefetch` of a struct's leaves is cheaper than touching them one at a time.
    subroutine materialize_marked(cache, sc, want)
        type(parquet_table_cache), intent(inout) :: cache !! the column store.
        type(table_scope), intent(in) :: sc               !! rows this table covers.
        logical, intent(in) :: want(:)                    !! .true. for each slot to read.
        integer :: i
        character(len=:), allocatable :: top, prev_top
        !
        prev_top = ""
        do i = 1, cache%ncols
            if (.not. want(i)) cycle
            if (.not. cache%cols(i)%supported) cycle
            if (.not. cache%cols(i)%file_source) cycle
            if (cache%cols(i)%residency == RES_FULL) cycle
            call table_materialize(cache, sc, i)
            top = top_level_of(cache%cols(i)%file_name)
            if (len(prev_top) > 0 .and. prev_top /= top) then
                call parquet_release_column(cache%reader, prev_top)
            end if
            prev_top = top
        end do
        if (len(prev_top) > 0) call parquet_release_column(cache%reader, prev_top)
    end subroutine materialize_marked
    !
    module procedure table_materialize_every
        call table_check_open(self, "materialize_all")
        call table_materialize_all(self%cache, table_scope_of(self))
    end procedure table_materialize_every
    !
    module procedure prefetch_one
        integer :: idx
        !
        call table_prefetch_resolve(self, name, "prefetch", idx, found)
        if (idx == 0) return
        call table_touch(self%cache, table_scope_of(self), idx, "prefetch")
    end procedure prefetch_one
    !
    module procedure prefetch_many
        logical, allocatable :: want(:)
        integer :: i, idx
        logical :: got
        !
        call table_check_open(self, "prefetch")
        allocate(want(self%cache%ncols))
        want = .false.
        if (present(found)) found = .true.
        do i = 1, size(names)
            ! `found` has to be forwarded conditionally, not just passed along: handing the
            ! callee a present dummy would turn every missing name into a quiet miss, including
            ! for a caller who asked for no `found=` and therefore expects an abort.
            if (present(found)) then
                call table_prefetch_resolve(self, trim(names(i)), "prefetch", idx, got)
                if (.not. got) then
                    ! One miss does not abandon the rest: the caller asked for a set, and the
                    ! ones that do exist are still worth reading. `found` reports whether ALL
                    ! of them were found.
                    found = .false.
                    cycle
                end if
            else
                call table_prefetch_resolve(self, trim(names(i)), "prefetch", idx)
            end if
            want(idx) = .true.
        end do
        call materialize_marked(self%cache, table_scope_of(self), want)
    end procedure prefetch_many
    !
    !> Resolves a name for %prefetch: a miss obeys `found=`, and an unsupported column is an
    !! error either way -- asking to read a column this library cannot read is a mistake, not a
    !! quiet no-op, even though it is harmless.
    subroutine table_prefetch_resolve(self, name, proc, idx, found)
        class(parquet_table), intent(in) :: self  !! the table.
        character(len=*), intent(in) :: name      !! column name.
        character(len=*), intent(in) :: proc      !! calling procedure, for the message.
        integer, intent(out) :: idx               !! slot index, or 0 on a reported miss.
        logical, intent(out), optional :: found   !! present: report a miss instead of aborting.
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, proc)
        idx = table_find(self, name)
        if (idx == 0) then
            if (present(found)) then
                found = .false.
                return
            end if
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": no column of this name" // sfx
        end if
        if (.not. self%cache%cols(idx)%supported) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // trim(proc) // ": this column's type is not supported by " // &
                "parquet_table, so it cannot be read" // sfx
        end if
        if (present(found)) found = .true.
    end subroutine table_prefetch_resolve
    !
    module procedure table_reload
        integer :: idx
        character(len=:), allocatable :: sfx
        !
        call table_prefetch_resolve(self, name, "reload", idx, found)
        if (idx == 0) return
        if (.not. self%cache%cols(idx)%file_source) then
            ! A column added in memory has no file behind it, so there is nothing to reload
            ! FROM -- and silently keeping the current values would make %reload look like it
            ! worked when it did nothing.
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "reload: this column was not read from a file, so there is " // &
                "nothing to reload it from" // sfx
        end if
        if (self%detached) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "reload: this table has been detached from its file by a " // &
                "row-structural change, so a re-read would no longer line up" // sfx
        end if
        ! Drop what is there and take the first-touch path again, so a reload and a first read
        ! cannot drift apart.
        call self%cache%cols(idx)%values%clear()
        self%cache%cols(idx)%residency = RES_EMPTY
        call table_touch(self%cache, table_scope_of(self), idx, "reload")
    end procedure table_reload
    !
    !> The part of a (possibly dotted) column path before its first "." -- i.e. the name the
    !! reader caches the decoded array under. A name with no dot is its own top level.
    function top_level_of(path) result(top)
        character(len=*), intent(in) :: path      !! column path, dotted or not.
        character(len=:), allocatable :: top      !! the top-level field name.
        integer :: dot
        !
        dot = index(path, ".")
        if (dot == 0) then
            top = trim(path)
        else
            top = path(1:dot - 1)
        end if
    end function top_level_of
    !
end submodule parquet_tables_read
