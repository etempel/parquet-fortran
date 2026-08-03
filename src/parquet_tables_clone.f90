!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Copying a `parquet_table`: `%clone` (an independent deep copy) and `%clone_structure` (an
!! empty table with the same columns).
!!
!! **`%clone` is the only rollback mechanism this type has.** Mutation is in place, and there is
!! no undo log -- deliberately, since an implicit one would double the memory of every table on
!! the chance that someone wants to go back. The documented substitute is to copy before mutating,
!! which is only a real option if copying is a first-class, obvious operation. Hence this file.
!!
!! Both take `class(parquet_table), intent(out) :: out` rather than returning an allocatable
!! result. Fortran resolves a type-bound procedure reference against the DECLARED type, so a
!! `class(parquet_table), allocatable` result would make `out%mass()` a compile error on a
!! generated table and force `select type` around every use of a clone -- defeating the point of
!! the generated accessors. With `intent(out)` the caller declares the concrete type and
!! everything resolves normally; `intent(out)` on a finalizable type also runs the finalizer and
!! resets every component for free, which is exactly what a clone target wants.
submodule (parquet_tables) parquet_tables_clone
    implicit none
    !
contains
    !
    module procedure table_clone
        integer :: i
        !
        call table_check_open(self, "clone")
        call clone_check_same_type(self, out, "clone")
        call clone_new_cache(self, out)
        do i = 1, self%cache%ncols
            call clone_copy_descriptor(self%cache%cols(i), out%cache%cols(i))
            ! A column that was never read stays unread in the clone: a clone costs what the
            ! source actually holds, not what its file contains. The clone's own reader (below)
            ! is what lets it read those columns later.
            if (self%cache%cols(i)%residency == RES_FULL) then
                call self%cache%cols(i)%values%deep_copy(out%cache%cols(i)%values)
            end if
        end do
        out%cache%ncols = self%cache%ncols
        out%detached = self%detached
        out%regime = self%regime
        out%row_lo = self%row_lo
        out%row_hi = self%row_hi
        out%row_count = self%row_count
        out%cache%reads_started = self%cache%reads_started
        ! The composed read-time transform travels with the clone, so clone_reopen_reader below
        ! reattaches exactly what the source was opened with. Copied rather than re-derived: it is
        ! already translated to file names and already merged with the MAML's own, and the clone
        ! opens the same file, so re-deriving it could only drift.
        out%cache%read_qc_soft = self%cache%read_qc_soft
        if (allocated(self%cache%read_filter)) out%cache%read_filter = self%cache%read_filter
        if (allocated(self%cache%read_sort)) out%cache%read_sort = self%cache%read_sort
        if (allocated(self%cache%read_qc_schema)) out%cache%read_qc_schema = self%cache%read_qc_schema
        if (allocated(self%cache%read_sample_fraction)) &
            out%cache%read_sample_fraction = self%cache%read_sample_fraction
        if (allocated(self%cache%read_sample_seed)) &
            out%cache%read_sample_seed = self%cache%read_sample_seed
        ! Explicit allocate-then-copy, not `out%cache%rg_bounds = self%cache%rg_bounds`: the plain
        ! assignment relies on F2003 automatic reallocation, which should be a no-op concern here
        ! since out%cache%rg_bounds is always freshly unallocated (clone_new_cache just allocated
        ! out%cache itself) -- but confirmed via a real, reproducible run under gfortran's
        ! -fcheck=bounds that it instead raises "Array bound mismatch for dimension 1" for this
        ! exact shape (a rank-2 allocatable array component reached through a POINTER-typed
        ! intermediate, `out%cache` being `type(parquet_table_cache), pointer`). Sidestepping
        ! automatic reallocation entirely, the same way this project already does for other
        ! confirmed compiler-codegen quirks, rather than relying on it.
        if (allocated(self%cache%rg_bounds)) then
            allocate(out%cache%rg_bounds(size(self%cache%rg_bounds, 1), size(self%cache%rg_bounds, 2)))
            out%cache%rg_bounds(:, :) = self%cache%rg_bounds(:, :)
        end if
        ! The slice's own file-row range and the physical geometry it was resolved against. Both
        ! are what clone_reopen_reader needs to put the clone's reader in the same state: the
        ! source's own row_lo/row_hi copied above are TABLE rows on a masked slice, so they cannot
        ! stand in for the physical range the filter has to be scoped to.
        out%cache%slice_row_lo = self%cache%slice_row_lo
        out%cache%slice_row_hi = self%cache%slice_row_hi
        ! Physical geometry travels with the clone as data. A clone of a DETACHED table has no
        ! file to re-derive it from, and it is no less true of where those rows came from.
        out%cache%unfiltered_rows = self%cache%unfiltered_rows
        out%cache%rg_extent_rows = self%cache%rg_extent_rows
        ! Whether the automatic row-index column has been given a real slot travels with the copy:
        ! %clone copies the slots themselves, so the flag has to agree with what is there.
        out%cache%row_index_live = self%cache%row_index_live
        out%cache%row_index_shadowed = self%cache%row_index_shadowed
        if (allocated(self%cache%rg_bounds_physical)) then
            allocate(out%cache%rg_bounds_physical(size(self%cache%rg_bounds_physical, 1), &
                size(self%cache%rg_bounds_physical, 2)))
            out%cache%rg_bounds_physical(:, :) = self%cache%rg_bounds_physical(:, :)
        end if
        ! A detached source has no file left to reopen, and an in-memory one never had one, so
        ! both produce a clone with no reader. Only a live file-backed table opens its own.
        if (self%cache%file_backed .and. .not. self%detached) call clone_reopen_reader(self, out)
    end procedure table_clone
    !
    module procedure table_clone_structure
        integer :: i, n
        logical :: only_res
        character(len=:), allocatable :: sfx
        !
        call table_check_open(self, "clone_structure")
        call clone_check_same_type(self, out, "clone_structure")
        only_res = .false.
        if (present(resident_only)) only_res = resident_only
        call clone_new_cache(self, out)
        n = 0
        do i = 1, self%cache%ncols
            ! An unsupported column is skipped: there is no kind to give it, so nothing could
            ! ever fill it. %append refuses a table holding one for the same reason, and points
            ! at %drop_column.
            if (.not. self%cache%cols(i)%supported) cycle
            if (only_res .and. self%cache%cols(i)%residency /= RES_FULL) cycle
            ! A plain-LIST column's kind is not known until its width is, and a batch column has
            ! no file to measure it from later -- so it is measured now, for real. That reads
            ! data for that one column, exactly as %kind already does.
            call table_resolve_width(self%cache, table_scope_of(self), i, .true., "clone_structure")
            n = n + 1
            call clone_copy_descriptor(self%cache%cols(i), out%cache%cols(n))
            call clone_empty_column(self%cache%cols(i), out%cache%cols(n)%values)
            out%cache%cols(n)%file_source = .false.
            out%cache%cols(n)%residency = RES_FULL
        end do
        out%cache%ncols = n
        ! The result is an in-memory table, not a detached one: it was never attached to a file
        ! in the first place, which is the same state parquet_new_table leaves a table in.
        out%detached = .false.
        out%regime = REGIME_FULL
        out%row_lo = 1_int64
        out%row_hi = 0_int64
        out%row_count = 0_int64
    end procedure table_clone_structure
    !
    !> error stops unless source and destination have the same dynamic type.
    !!
    !! Cloning a generated table into a bare `parquet_table` would compile and run, and silently
    !! produce a copy without the predefined accessors the caller is about to reach for. Failing
    !! loudly here is the whole reason the guard exists.
    subroutine clone_check_same_type(self, out, proc)
        class(parquet_table), intent(in) :: self !! the source table.
        class(parquet_table), intent(in) :: out  !! the destination table.
        character(len=*), intent(in) :: proc     !! calling procedure, for the message.
        !
        if (same_type_as(self, out)) return
        error stop EP // trim(proc) // ": source and destination must be the same table type; " // &
            "cloning into a plain parquet_table would silently drop the predefined columns"
    end subroutine clone_check_same_type
    !
    !> Gives `out` its own fresh, empty cache, sized to hold the source's columns.
    !!
    !! Never shares the source's cache. Two tables pointing at one store would double-free it and
    !! would make a mutation through either one visible through the other -- the exact hazard the
    !! blocked intrinsic assignment exists to prevent, so a clone must not reintroduce it.
    subroutine clone_new_cache(self, out)
        class(parquet_table), intent(in) :: self    !! the source table.
        class(parquet_table), intent(inout) :: out  !! the destination table.
        integer :: i
        !
        allocate(out%cache)
        call record_open_thread(out%cache)
        allocate(out%cache%cols(max(self%cache%ncols, 1) + COL_HEADROOM))
        out%cache%ncols = 0
        out%cache%file_backed = .false.
        if (allocated(self%cache%source_file)) out%cache%source_file = self%cache%source_file
        ! The source file's metadata comes across as data, not by re-reading: a clone of a
        ! DETACHED table has no file left to reopen, and its metadata is no less true for that.
        ! Explicit allocate-then-copy, not `out%cache%meta_keys = self%cache%meta_keys`: the plain
        ! assignment leans on F2003 automatic reallocation to establish BOTH the shape and the
        ! deferred LENGTH of a character array component that is reached through a POINTER-typed
        ! intermediate (`out%cache`). That is the same shape this file already sidesteps for
        ! `rg_bounds` below, and it is worse for a character array, because the length has to be
        ! set as well -- a clone of a table carrying file metadata was seen to segfault inside
        ! libc's allocator here on GitLab CI's (older) gfortran, while the identical code runs
        ! clean on gfortran 15. Allocating with an explicit length and copying element by element
        ! removes the reallocation from the picture entirely. Element-wise, not `dst = src`, per
        ! the whole-array-assignment hazard in CLAUDE.md's "Compiler & language gotchas".
        if (allocated(self%cache%meta_keys)) then
            allocate(character(len=len(self%cache%meta_keys)) :: out%cache%meta_keys(size(self%cache%meta_keys)))
            do i = 1, size(self%cache%meta_keys)
                out%cache%meta_keys(i) = self%cache%meta_keys(i)
            end do
        end if
        if (allocated(self%cache%meta_values)) then
            allocate(character(len=len(self%cache%meta_values)) :: out%cache%meta_values(size(self%cache%meta_values)))
            do i = 1, size(self%cache%meta_values)
                out%cache%meta_values(i) = self%cache%meta_values(i)
            end do
        end if
    end subroutine clone_new_cache
    !
    !> Copies everything about a column EXCEPT its values.
    subroutine clone_copy_descriptor(src, dst)
        type(parquet_table_column), intent(in) :: src  !! the source descriptor.
        type(parquet_table_column), intent(inout) :: dst !! the descriptor to fill.
        !
        dst%name = src%name
        dst%file_name = src%file_name
        dst%declared_kind = src%declared_kind
        dst%width = src%width
        dst%width_pending = src%width_pending
        dst%time_unit = src%time_unit
        dst%time_utc = src%time_utc
        if (allocated(src%unit)) dst%unit = src%unit
        dst%file_source = src%file_source
        dst%predefined = src%predefined
        dst%user_populated = src%user_populated
        dst%supported = src%supported
        dst%residency = src%residency
    end subroutine clone_copy_descriptor
    !
    !> Creates a zero-row column of the same kind, width and unit as `src`.
    !> Builds the empty column one descriptor slot implies: same kind, width and unit, zero rows.
    !!
    !! Takes its shape from the DESCRIPTOR, not from the slot's values, and that is the whole
    !! point: a column that has never been read holds no values, so reading `values%kindof()`
    !! answered PK_NONE and `%clone_structure` on a freshly opened (lazy) table aborted inside the
    !! column store. Every supported column's kind and width are known from the file's schema at
    !! open -- the one exception, a plain LIST whose width lives in the data, is resolved by the
    !! caller before it gets here -- so the descriptor can always answer and a batch can be cloned
    !! from a table that has read nothing at all.
    !!
    !! The unit comes from the descriptor too where there is one (a read-in MAML's `unit:` key),
    !! and otherwise from the values, which is where `%add_column(unit=)` puts it.
    subroutine clone_empty_column(src, dst)
        type(parquet_table_column), intent(in) :: src !! the slot to take the shape of.
        type(parquet_column), intent(inout) :: dst    !! receives the empty column.
        character(len=:), allocatable :: unit
        !
        if (allocated(src%unit)) then
            unit = src%unit
        else
            call src%values%unit_string(unit)
        end if
        call dst%init(src%declared_kind, 0_int64, src%width, unit)
    end subroutine clone_empty_column
    !
    !> Opens the clone's own reader on the same file, so the clone stays lazy.
    !!
    !! A failure here is a hard error naming the file rather than a quiet fallback to a
    !! reader-less clone: degrading silently would leave the caller with a table that
    !! mysteriously refuses to read a column much later, far from the cause.
    !!
    !! **The reopen is NOT a bare `parquet_open_reader`**, and must never become one again: it goes
    !! through the same `table_open_reader_with_transform` `parquet_open_table` itself uses, so the
    !! clone's reader carries the identical filter/sort/qc/sample. Without that, the clone's
    !! lazily-read columns would come back with rows the source had filtered away -- two different
    !! lengths inside one table, with nothing to report it. `table_clone` copies the cache's
    !! `read_*` components across before calling this, which is what that helper reads.
    subroutine clone_reopen_reader(self, out)
        class(parquet_table), intent(in) :: self    !! the source table.
        class(parquet_table), intent(inout) :: out  !! the destination table.
        !
        allocate(out%cache%reader)
        call table_open_reader_with_transform(out, out%cache%source_file)
        out%cache%file_backed = .true.
    end subroutine clone_reopen_reader
    !
end submodule parquet_tables_clone
