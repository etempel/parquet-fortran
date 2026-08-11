!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Mutation that leaves a `parquet_table`'s ROW SET alone: the single-cell validity writers
!! (`%set_null`, `%clear_null`, `%compact_validity`) and the column-structural operations
!! (`%drop_column`, `%rename_column`, `%copy_column`, `%cast`).
!!
!! **Nothing in this file detaches the table** (F-mut-7). That is the whole reason it is a
!! separate file from `parquet_tables_rowmutate.f90`: adding a column, dropping one, renaming one
!! or writing one cell leaves every column the same length, so the table can still read the
!! columns it has not read yet. Only a change to the *row set* breaks that alignment, and every
!! such operation lives in the other file. A new procedure added here must preserve that property
!! -- if it cannot, it belongs next door.
!!
!! **The file split tracks the ROW SET, not pointer stability, and `%compact`/`%reserve` are what
!! make the difference visible.** Everywhere else the two coincide: a row-structural change
!! reallocates storage, so it both detaches and invalidates every `%col` pointer, while nothing
!! here did either. `%compact` and `%reserve` reallocate storage *without* changing the row set --
!! so they belong here (they detach nothing, and a column not yet read can still be read
!! afterwards) and yet they DO invalidate every outstanding pointer and row handle. They are the
!! only two procedures in this file that do. Pointer invalidation is therefore the union of
!! "changes the row set" and "reallocates storage", and only the first half decides which file a
!! procedure lives in.
!!
!! The per-kind `%set_element` writers are generated rather than written here; they live in
!! `parquet_tables_access.f90` alongside `%set`, whose shape they follow.
submodule (parquet_tables) parquet_tables_mutate
    use ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    implicit none
    !
contains
    !
    ! ---- single-cell validity -------------------------------------------------------------
    !
    module procedure set_null_i32
        call self%set_null(name, int(i, int64), found)
    end procedure set_null_i32
    !
    module procedure set_null_i64
        integer :: idx
        !
        ! Resolved before anything is written, so found=.false. means nothing changed.
        call table_resolve(self, name, "set_null", idx, found)
        if (idx == 0) return
        call table_check_shared_write(self, idx, "set_null", nulling=.true.)
        call table_require_row(self, i, "set_null")
        call parquet_column_set_null(self%cache%cols(idx)%values, i)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_null_i64
    !
    module procedure set_null_mask
        integer :: idx
        integer(int64) :: i
        character(len=32) :: got, want
        character(len=:), allocatable :: sfx
        !
        call table_resolve(self, name, "set_null", idx, found)
        if (idx == 0) return
        call table_check_shared_write(self, idx, "set_null", nulling=.true.)
        if (size(is_valid, kind=int64) /= self%row_count) then
            write(got, "(I0)") size(is_valid, kind=int64)
            write(want, "(I0)") self%row_count
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "set_null: the mask has " // trim(got) // " entries but the table " // &
                "has " // trim(want) // " rows" // sfx
        end if
        ! Only ever adds nulls: a .true. entry says nothing about a row that is already null, and
        ! clearing it would make this the inverse of a call the caller did not make.
        do i = 1_int64, self%row_count
            if (.not. is_valid(i)) call parquet_column_set_null(self%cache%cols(idx)%values, i)
        end do
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_null_mask
    !
    module procedure set_null_e32
        call self%set_null(name, int(i, int64), int(e, int64), found)
    end procedure set_null_e32
    !
    module procedure set_null_e64
        integer :: idx
        !
        call table_resolve(self, name, "set_null", idx, found)
        if (idx == 0) return
        call table_check_shared_write(self, idx, "set_null", nulling=.true.)
        call table_require_row(self, i, "set_null")
        call parquet_column_set_null(self%cache%cols(idx)%values, i, e)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_null_e64
    !
    module procedure set_null_mask_elem
        integer :: idx, wdt
        integer(int64) :: i, e
        character(len=64) :: got, want
        character(len=:), allocatable :: sfx
        !
        call table_resolve(self, name, "set_null", idx, found)
        if (idx == 0) return
        call table_check_shared_write(self, idx, "set_null", nulling=.true.)
        wdt = self%cache%cols(idx)%values%colwidth()
        if (size(is_valid, 1, kind=int64) /= int(wdt, int64) .or. &
            size(is_valid, 2, kind=int64) /= self%row_count) then
            write(got, "(I0,A,I0)") size(is_valid, 1, kind=int64), " x ", size(is_valid, 2, kind=int64)
            write(want, "(I0,A,I0)") wdt, " x ", self%row_count
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "set_null: the mask is shaped " // trim(got) // " but the column " // &
                "is " // trim(want) // " (width x rows)" // sfx
        end if
        ! Only ever adds nulls, exactly as the row form does and for the same reason.
        do i = 1_int64, self%row_count
            do e = 1_int64, int(wdt, int64)
                if (.not. is_valid(e, i)) call parquet_column_set_null(self%cache%cols(idx)%values, i, e)
            end do
        end do
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_null_mask_elem
    !
    module procedure clear_null_i32
        call self%clear_null(name, int(i, int64), found)
    end procedure clear_null_i32
    !
    module procedure clear_null_i64
        integer :: idx
        !
        call table_resolve(self, name, "clear_null", idx, found)
        if (idx == 0) return
        call table_check_shared_write(self, idx, "clear_null", nulling=.true.)
        call table_require_row(self, i, "clear_null")
        call parquet_column_clear_null(self%cache%cols(idx)%values, i)
        self%cache%cols(idx)%user_populated = .true.
    end procedure clear_null_i64
    !
    module procedure clear_null_e32
        call self%clear_null(name, int(i, int64), int(e, int64), found)
    end procedure clear_null_e32
    !
    module procedure clear_null_e64
        integer :: idx
        !
        call table_resolve(self, name, "clear_null", idx, found)
        if (idx == 0) return
        call table_check_shared_write(self, idx, "clear_null", nulling=.true.)
        call table_require_row(self, i, "clear_null")
        call parquet_column_clear_null(self%cache%cols(idx)%values, i, e)
        self%cache%cols(idx)%user_populated = .true.
    end procedure clear_null_e64
    !
    module procedure table_compact_validity
        integer :: idx
        !
        ! Deallocating the bitmap is a change to the column's storage, not a value write: a
        ! concurrent %is_null would read through it, and a concurrent %set_null would re-allocate
        ! it. Refused on a shared table for the same reason every other structural change is.
        call table_check_not_shared(self, "compact_validity")
        call table_resolve(self, name, "compact_validity", idx, found)
        if (idx == 0) return
        call self%cache%cols(idx)%values%compact_validity()
    end procedure table_compact_validity
    !
    ! ---- capacity --------------------------------------------------------------------------
    !
    !> Releases the spare capacity appending left behind (see the interface in
    !! `parquet_tables.f90` for the full contract).
    module procedure table_compact
        integer :: i
        logical :: released, any_released
        !
        ! Refused on a shared table for the same reason every other storage change is: it
        ! reallocates under any pointer another thread is holding. %append is the one mutation a
        ! shared table permits, and it takes the lock instead -- so compaction happens after the
        ! parallel region closes, not inside it.
        call table_check_not_shared(self, "compact")
        call table_check_open(self, "compact")
        any_released = .false.
        do i = 1, self%cache%ncols
            ! A column that was never read has no storage to shrink, and touching it here would
            ! read the file for no reason -- exactly what %compact exists to avoid paying for.
            if (.not. table_mutable_column(self, i)) cycle
            call self%cache%cols(i)%values%shrink_to_fit(released)
            if (released) any_released = .true.
        end do
        ! Advanced ONLY when something actually moved. %generation() is the documented way for a
        ! caller to find out whether a %col pointer is still good ("take it before, compare it
        ! after, re-fetch if it moved"), so bumping it here when nothing was reallocated would
        ! force a needless re-fetch on every table that had nothing to release -- which is every
        ! table that has not been appended to. This is also what makes the no-op case testable.
        if (any_released) self%cache%generation = self%cache%generation + 1_int64
    end procedure table_compact
    !
    module procedure table_reserve_i32
        call self%reserve(int(n, int64))
    end procedure table_reserve_i32
    !
    !> Makes room for `n` rows in every resident column (see the interface in
    !! `parquet_tables.f90`).
    module procedure table_reserve_i64
        integer :: i
        character(len=32) :: got
        !
        call table_check_not_shared(self, "reserve")
        call table_check_open(self, "reserve")
        if (n < 0_int64) then
            write(got, "(I0)") n
            error stop EP // "reserve: cannot reserve " // trim(got) // " rows"
        end if
        ! `n` is the TOTAL row count to make room for, not an increment, so a reserve below what
        ! the table already holds is a no-op rather than a shrink -- %compact is what shrinks.
        if (n <= self%row_count) return
        do i = 1, self%cache%ncols
            if (.not. table_mutable_column(self, i)) cycle
            call self%cache%cols(i)%values%reserve(n)
        end do
        ! Reserving reallocates, so it invalidates pointers exactly as %compact does, and advances
        ! the generation for the same reason. Unconditional here: the early return above has
        ! already handled every case in which nothing can move.
        self%cache%generation = self%cache%generation + 1_int64
    end procedure table_reserve_i64
    !
    ! ---- column-structural ----------------------------------------------------------------
    !
    module procedure table_put_column
        integer :: idx
        !
        call table_check_open(self, "add_column")
        call table_fix_nrows(self, name, col%length())
        call table_new_slot(self, name, .false., idx)
        call col%deep_copy(self%cache%cols(idx)%values)
        self%cache%cols(idx)%declared_kind = col%kindof()
        self%cache%cols(idx)%width = col%colwidth()
        self%cache%cols(idx)%residency = RES_FULL
        self%cache%cols(idx)%user_populated = .true.
    end procedure table_put_column
    !
    module procedure move_table_column
        if (allocated(src%name)) then
            dst%name = src%name
        else if (allocated(dst%name)) then
            deallocate(dst%name)
        end if
        if (allocated(src%file_name)) then
            dst%file_name = src%file_name
        else if (allocated(dst%file_name)) then
            deallocate(dst%file_name)
        end if
        if (allocated(src%unit)) then
            dst%unit = src%unit
        else if (allocated(dst%unit)) then
            deallocate(dst%unit)
        end if
        dst%declared_kind = src%declared_kind
        dst%width = src%width
        dst%width_pending = src%width_pending
        dst%cast_pending = src%cast_pending
        dst%file_source = src%file_source
        dst%predefined = src%predefined
        dst%user_populated = src%user_populated
        dst%supported = src%supported
        dst%residency = src%residency
        if (allocated(src%rg_loaded)) then
            call move_alloc(src%rg_loaded, dst%rg_loaded)
        else if (allocated(dst%rg_loaded)) then
            deallocate(dst%rg_loaded)
        end if
        call dst%values%move_from(src%values)
    end procedure move_table_column
    !
    module procedure table_drop_column
        integer :: idx, i
        logical :: forced
        character(len=:), allocatable :: sfx
        !
        call table_check_not_shared(self, "drop_column")
        ! Deliberately table_lookup_or_fail, not table_resolve: dropping a column that was never
        ! read is the cheap memory-reclaiming case, and reading it first to throw it away would
        ! defeat the point.
        call table_lookup_or_fail(self, name, "drop_column", idx, found)
        if (idx == 0) return
        forced = .false.
        if (present(force)) forced = force
        ! R8: a predefined column may be dropped only to reclaim memory, and only on purpose.
        ! `predefined` is set by %bind_predefined, i.e. by a generated table type's %init -- so a
        ! program using the generated accessors can rely on those columns being there.
        if (self%cache%cols(idx)%predefined .and. .not. forced) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "drop_column: this is a predefined column, which a program using " // &
                "the generated accessors expects to be there; pass force=.true. if you really " // &
                "mean to drop it (to reclaim memory)" // sfx
        end if
        call self%cache%cols(idx)%values%clear()
        ! Shift the tail down over the dropped slot. The store is name-keyed, so the order of the
        ! remaining slots is not itself meaningful -- but %column_names reports it, and keeping it
        ! stable makes a drop look like a drop rather than a reshuffle.
        ! MOVED, not assigned: intrinsic assignment on a parquet_table_column deep-copies its
        ! `values`, so shifting the tail of a 40-column, 8 GB table down by one used to memcpy
        ! roughly 8 GB inside the procedure documented as the way to give memory back. %move_from
        ! hands the storage over instead, so the shift costs a descriptor's worth of pointers per
        ! slot whatever the column holds.
        do i = idx, self%cache%ncols - 1
            call move_table_column(self%cache%cols(i), self%cache%cols(i + 1))
        end do
        ! Reset the vacated tail slot field-by-field rather than via `parquet_table_column()`:
        ! ifx rejects a default structure constructor here (Structure constructor may not have
        ! components with the PRIVATE attribute) because `values` is a parquet_column, whose own
        ! components are private to a different module -- even though this constructor never
        ! specifies `values` explicitly. `values` itself was just cleared above, so only the
        ! metadata fields need resetting, mirroring their declared defaults in parquet_tables.f90.
        associate (slot => self%cache%cols(self%cache%ncols))
            if (allocated(slot%name)) deallocate(slot%name)
            if (allocated(slot%file_name)) deallocate(slot%file_name)
            slot%declared_kind = PK_NONE
            slot%width = 1
            slot%width_pending = .false.
            slot%cast_pending = .false.
            slot%file_source = .false.
            slot%predefined = .false.
            slot%user_populated = .false.
            slot%supported = .true.
            slot%residency = RES_EMPTY
            if (allocated(slot%rg_loaded)) deallocate(slot%rg_loaded)
        end associate
        self%cache%ncols = self%cache%ncols - 1
        ! A drop shifts every slot above it down by one, so the whole name index is renumbered --
        ! an incremental fix-up would have to touch most of it anyway.
        call cache_name_index_rebuild(self%cache)
        self%cache%generation = self%cache%generation + 1_int64
    end procedure table_drop_column
    !
    module procedure table_rename_column
        integer :: idx
        character(len=:), allocatable :: sfx
        !
        call table_check_not_shared(self, "rename_column")
        ! `found` reports a missing SOURCE column only. A NEW name that is already taken is a
        ! different mistake -- the caller named a column that does exist -- and stays fatal.
        call table_lookup_or_fail(self, old_name, "rename_column", idx, found)
        if (idx == 0) return
        ! Same as drop_column's own predefined guard above: written now, unreachable until the
        ! generated table type exists. There is no `force=` here on purpose -- M3 -- because a
        ! renamed predefined column would break a compile-time-bound accessor with no diagnostic,
        ! and no use case justifies that.
        ! gcov attribution artifact, same guard-clause shape as drop_column's above.
        if (self%cache%cols(idx)%predefined) then ! GCOVR_EXCL_START
            call table_context_suffix(self%cache, old_name, sfx)
            error stop EP // "rename_column: this is a predefined column, whose accessor is " // &
                "bound to its name at compile time, so renaming it would break that accessor " // &
                "with no diagnostic" // sfx
        end if
        ! GCOVR_EXCL_STOP
        if (len_trim(new_name) == 0) then
            call table_context_suffix(self%cache, old_name, sfx)
            error stop EP // "rename_column: the new name is blank" // sfx
        end if
        if (trim(new_name) /= trim(old_name)) then
            if (table_find(self, new_name) > 0) then
                call table_context_suffix(self%cache, new_name, sfx)
                error stop EP // "rename_column: a column of the new name already exists" // sfx
            end if
        end if
        ! Only the INTERNAL name changes. file_name is what a later first touch reads from, so a
        ! renamed but still-unread file-backed column keeps working.
        self%cache%cols(idx)%name = trim(new_name)
        ! The slot keeps its index but changes its sort position, so the index is rebuilt rather
        ! than patched: this is the one column-set change that leaves `ncols` untouched, which is
        ! exactly why it would be easy to forget.
        call cache_name_index_rebuild(self%cache)
        self%cache%generation = self%cache%generation + 1_int64
    end procedure table_rename_column
    !
    module procedure table_copy_column
        integer :: idx, new_idx, want_kind
        logical :: strict
        character(len=:), allocatable :: sfx
        !
        call table_check_not_shared(self, "copy_column")
        ! table_resolve, not table_lookup_or_fail: a copy has to have the values in hand, so an
        ! unread column is read here. There is no deferred form of a copy -- unlike %cast, which
        ! can hand its conversion to the read that has not happened yet, a copy needs a second
        ! column's worth of values now.
        call table_resolve(self, name, "copy_column", idx, found)
        if (idx == 0) return
        if (len_trim(new_name) == 0) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "copy_column: the new name is blank" // sfx
        end if
        if (table_find(self, new_name) > 0) then
            call table_context_suffix(self%cache, new_name, sfx)
            error stop EP // "copy_column: a column of the new name already exists" // sfx
        end if
        want_kind = self%cache%cols(idx)%values%kindof()
        if (present(to_kind)) want_kind = to_kind
        strict = .true.
        if (present(exact)) strict = exact
        ! Validate and check BEFORE the new slot exists, so a copy that cannot be represented
        ! leaves the table exactly as it was rather than adding a half-converted column.
        if (want_kind /= self%cache%cols(idx)%values%kindof()) then
            call cast_check_pair(self%cache, name, "copy_column", &
                self%cache%cols(idx)%values%kindof(), want_kind)
            call cast_zero_null_rows(self%cache%cols(idx)%values)
            call cast_check_values(self%cache, name, "copy_column", &
                self%cache%cols(idx)%values, want_kind, strict)
        end if
        call table_new_slot(self, new_name, .false., new_idx)
        call self%cache%cols(idx)%values%deep_copy(self%cache%cols(new_idx)%values)
        if (want_kind /= self%cache%cols(new_idx)%values%kindof()) then
            call cast_apply(self%cache%cols(new_idx)%values, want_kind)
        end if
        self%cache%cols(new_idx)%declared_kind = self%cache%cols(new_idx)%values%kindof()
        self%cache%cols(new_idx)%width = self%cache%cols(new_idx)%values%colwidth()
        self%cache%cols(new_idx)%residency = RES_FULL
        self%cache%cols(new_idx)%user_populated = .true.
        self%cache%generation = self%cache%generation + 1_int64
    end procedure table_copy_column
    !
    module procedure table_cast
        integer :: idx
        logical :: strict, deferred
        character(len=:), allocatable :: sfx
        !
        call table_check_not_shared(self, "cast")
        strict = .false.
        if (present(exact)) strict = exact
        call table_check_open(self, "cast")
        ! Deliberately table_lookup_or_fail, not table_resolve: resolving TOUCHES, and the whole
        ! point of the deferred path below is to decide before the read happens. The two guards
        ! table_resolve would have applied on the way past are applied here instead.
        call table_lookup_or_fail(self, name, "cast", idx, found)
        if (idx == 0) return
        if (.not. self%cache%cols(idx)%supported) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "cast: this column's type is not supported by parquet_table, so " // &
                "its values were never read" // sfx
        end if
        ! A plain LIST/LARGE_LIST from a foreign writer has no kind yet, and its kind is what
        ! decides whether this cast is legal at all -- so settle it first. That reads the column,
        ! which is why such a column never takes the deferred path below.
        if (self%cache%cols(idx)%width_pending) then
            call table_resolve_width(self%cache, table_scope_of(self), idx, .false., "cast")
        end if
        if (self%cache%cols(idx)%declared_kind == to_kind) return
        call cast_check_pair(self%cache, name, "cast", self%cache%cols(idx)%declared_kind, to_kind)
        !
        ! %cast is deliberately unaware of `user_populated`, in BOTH directions: it never sets it
        ! and never reads it. A cast changes a column's KIND, not whose values those are -- and on
        ! the deferred path below nothing has even been read yet, so there would be no values to
        ! claim. It follows that a cast column may be evicted and reloaded freely: %reload re-reads
        ! into the column's CURRENT kind, so the file's values come back already converted and the
        ! cast survives (test_cast_deferred asserts exactly that).
        !
        ! Both halves are easy to undo by symmetry once %evict_column and %reload have force=
        ! guards. Neither belongs here. What a caller wrote with %set before casting stays marked
        ! by that %set, which is correct: those values are still theirs, merely converted.
        !
        ! The deferred path: a file-backed column nothing has read yet does not need to be read
        ! and then converted. Rewriting `declared_kind` is enough, because table_materialize reads
        ! into whatever kind the slot declares -- so the first touch decodes STRAIGHT into the
        ! target kind, in one pass, through the reader's own numeric conversions (the same set
        ! this procedure allows).
        !
        ! Two things disqualify it. `exact=` asks for precision checks the reader does not
        ! perform, so it must have the values in hand. And a cast on a column that is ALREADY
        ! waiting for one has to materialize first: otherwise `%cast(x, PK_INT32)` followed by
        ! `%cast(x, PK_FLOAT64)` would silently forget the rounding the first one asked for.
        deferred = self%cache%cols(idx)%file_source .and. &
            self%cache%cols(idx)%residency /= RES_FULL .and. &
            .not. self%cache%cols(idx)%cast_pending .and. &
            .not. self%cache%cols(idx)%width_pending .and. &
            .not. self%detached .and. .not. strict
        if (deferred) then
            self%cache%cols(idx)%declared_kind = to_kind
            self%cache%cols(idx)%cast_pending = .true.
            ! Bumped here as well as on the eager path below, even though no pointer into this
            ! column can exist (taking one would have materialized it, which disqualifies the
            ! deferred path). The counter is documented as advancing on EVERY structural entry
            ! point precisely so a caller never has to know which internal path a call took --
            ! and %kind answers differently from here on, which is a structural change by any
            ! reading. A spare bump costs a re-fetch; a missing one gives false confidence.
            self%cache%generation = self%cache%generation + 1_int64
            return
        end if
        !
        call table_touch(self%cache, table_scope_of(self), idx, "cast")
        ! Re-check against the kind actually READ. A width_pending column only learned its kind
        ! above, and a cast_pending one is now holding the kind an earlier deferred cast asked
        ! for -- either can have turned this into a no-op.
        if (self%cache%cols(idx)%values%kindof() == to_kind) then
            self%cache%cols(idx)%declared_kind = to_kind
            self%cache%cols(idx)%cast_pending = .false.
            return
        end if
        call cast_check_pair(self%cache, name, "cast", self%cache%cols(idx)%values%kindof(), to_kind)
        call cast_zero_null_rows(self%cache%cols(idx)%values)
        call cast_check_values(self%cache, name, "cast", self%cache%cols(idx)%values, to_kind, strict)
        call cast_apply(self%cache%cols(idx)%values, to_kind)
        self%cache%cols(idx)%declared_kind = to_kind
        self%cache%cols(idx)%cast_pending = .false.
        self%cache%generation = self%cache%generation + 1_int64
    end procedure table_cast
    !
    ! ---- conversion between numeric kinds, shared by %cast and %copy_column ------------------
    !
    !> Whether a PK_* kind is one the conversion path can produce or consume: the four numeric
    !! kinds, scalar or vector. Logical, string and temporal are excluded because neither the
    !! reader nor the writer converts them either -- there is no answer to "what is a null date
    !! as an integer?" that would not have to be invented here.
    pure logical function cast_is_numeric(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. kind == PK_FLOAT64 &
            .or. kind == PK_INT32_VEC .or. kind == PK_INT64_VEC .or. kind == PK_FLOAT32_VEC &
            .or. kind == PK_FLOAT64_VEC
    end function cast_is_numeric
    !
    !> Whether a PK_* kind is one of the vector (per-row width > 1) numeric kinds.
    pure logical function cast_is_vector(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32_VEC .or. kind == PK_INT64_VEC .or. kind == PK_FLOAT32_VEC &
            .or. kind == PK_FLOAT64_VEC
    end function cast_is_vector
    !
    !> error stops unless `from_kind` can be converted to `to_kind` at all.
    !!
    !! Rank is part of the answer, not an afterthought: a scalar column and a vector one differ in
    !! how many values each row holds, so turning one into the other is a reshape rather than a
    !! conversion, and a caller who asked for it has confused a column's kind with its width.
    subroutine cast_check_pair(cache, name, proc, from_kind, to_kind)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        integer, intent(in) :: from_kind               !! the column's current PK_* kind.
        integer, intent(in) :: to_kind                 !! the requested PK_* kind.
        character(len=:), allocatable :: sfx, from_txt, to_txt
        !
        call parquet_kind_name(from_kind, from_txt)
        call parquet_kind_name(to_kind, to_txt)
        if (.not. (cast_is_numeric(from_kind) .and. cast_is_numeric(to_kind))) then
            call table_context_suffix(cache, name, sfx)
            error stop EP // trim(proc) // ": only the numeric kinds convert, not from " // &
                from_txt // " to " // to_txt // sfx
        end if
        if (cast_is_vector(from_kind) .neqv. cast_is_vector(to_kind)) then
            call table_context_suffix(cache, name, sfx)
            error stop EP // trim(proc) // ": converting from " // from_txt // " to " // to_txt // &
                " would change the column's width, which is a reshape rather than a conversion" // sfx
        end if
    end subroutine cast_check_pair
    !
    !> Writes a zero into the storage of every NULL row, in place.
    !!
    !! This is what lets the two passes that follow work on whole arrays with no null mask at
    !! all. A null row's stored value is unspecified by `parquet_column`'s own contract, and it
    !! really can be uninitialized memory (`init` allocates without filling); left alone it would
    !! fail the range and fractional-part checks for reasons that have nothing to do with the
    !! caller's data, and -- worse -- an out-of-range real converted to an integer is undefined
    !! behaviour, not merely a wrong number. Zero converts cleanly into every kind here, so
    !! writing one first makes both passes unconditional.
    !!
    !! It does mean a later `%clear_null` on such a row exposes 0 rather than whatever happened to
    !! be there. That is exactly what "the value behind it is unspecified until written" allows.
    subroutine cast_zero_null_rows(col)
        type(parquet_column), intent(inout) :: col !! the column whose null rows are zeroed.
        logical, allocatable :: rowvalid(:)
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        integer(int64) :: r
        !
        ! Unallocated means the column has no nulls at all, which is the common case and costs
        ! nothing beyond the query itself.
        call col%row_validity(rowvalid)
        if (.not. allocated(rowvalid)) return
        select case (col%kindof())
        case (PK_INT32)
            call col%data_ptr(p_i32)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_i32(r) = 0_int32
            end do
        case (PK_INT64)
            call col%data_ptr(p_i64)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_i64(r) = 0_int64
            end do
        case (PK_FLOAT32)
            call col%data_ptr(p_f32)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_f32(r) = 0.0_real32
            end do
        case (PK_FLOAT64)
            call col%data_ptr(p_f64)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_f64(r) = 0.0_real64
            end do
        case (PK_INT32_VEC)
            call col%data_ptr(p_i32v)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_i32v(:, r) = 0_int32
            end do
        case (PK_INT64_VEC)
            call col%data_ptr(p_i64v)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_i64v(:, r) = 0_int64
            end do
        case (PK_FLOAT32_VEC)
            call col%data_ptr(p_f32v)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_f32v(:, r) = 0.0_real32
            end do
        case (PK_FLOAT64_VEC)
            call col%data_ptr(p_f64v)
            do r = 1_int64, col%length()
                if (.not. rowvalid(r)) p_f64v(:, r) = 0.0_real64
            end do
        end select
    end subroutine cast_zero_null_rows
    !
    !> error stops on the first value that cannot be converted into `to_kind`, naming the row and
    !! the value. Reads only -- nothing is written until `cast_apply`, so a rejected conversion
    !! leaves the column exactly as it was.
    !!
    !! `strict` is the whole difference between `%cast`'s default and `%copy_column`'s. Either way
    !! an integer overflow and a fractional value converted to an integer kind are errors, because
    !! both are errors on the read and write paths too. `strict` additionally rejects the losses
    !! those paths make silently: real64 narrowed to real32, and an integer too large for a real
    !! kind to hold exactly.
    subroutine cast_check_values(cache, name, proc, col, to_kind, strict)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        type(parquet_column), intent(in), target :: col !! the column to check.
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        logical, intent(in) :: strict                  !! .true. to reject precision loss too.
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        integer(int64) :: r
        !
        ! An empty column has no storage allocated to point at, and no values to reject.
        if (col%length() == 0_int64) return
        ! Scalar kinds check the whole array in one call and let the element index BE the row
        ! (row = 0 says so); vector kinds hand one row's slice to the same checker at a time, so
        ! the numeric rules below exist once rather than once per rank.
        select case (col%kindof())
        case (PK_INT32)
            call col%data_ptr(p_i32)
            call chk_from_i32(cache, name, proc, p_i32, 0_int64, to_kind, strict)
        case (PK_INT64)
            call col%data_ptr(p_i64)
            call chk_from_i64(cache, name, proc, p_i64, 0_int64, to_kind, strict)
        case (PK_FLOAT32)
            call col%data_ptr(p_f32)
            call chk_from_f32(cache, name, proc, p_f32, 0_int64, to_kind, strict)
        case (PK_FLOAT64)
            call col%data_ptr(p_f64)
            call chk_from_f64(cache, name, proc, p_f64, 0_int64, to_kind, strict)
        case (PK_INT32_VEC)
            call col%data_ptr(p_i32v)
            do r = 1_int64, col%length()
                call chk_from_i32(cache, name, proc, p_i32v(:, r), r, to_kind, strict)
            end do
        case (PK_INT64_VEC)
            call col%data_ptr(p_i64v)
            do r = 1_int64, col%length()
                call chk_from_i64(cache, name, proc, p_i64v(:, r), r, to_kind, strict)
            end do
        case (PK_FLOAT32_VEC)
            call col%data_ptr(p_f32v)
            do r = 1_int64, col%length()
                call chk_from_f32(cache, name, proc, p_f32v(:, r), r, to_kind, strict)
            end do
        case (PK_FLOAT64_VEC)
            call col%data_ptr(p_f64v)
            do r = 1_int64, col%length()
                call chk_from_f64(cache, name, proc, p_f64v(:, r), r, to_kind, strict)
            end do
        end select
    end subroutine cast_check_values
    !
    !> Checks one contiguous run of int32 values against `to_kind`. Widening to int64 or real64 is
    !! exact by construction and checks nothing; only real32 can lose an int32 (24 mantissa bits
    !! against 31 value bits), and only when `strict` asks about it.
    subroutine chk_from_i32(cache, name, proc, v, row, to_kind, strict)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        integer(int32), intent(in) :: v(:)             !! the values to check.
        integer(int64), intent(in) :: row              !! the row they belong to, or 0 for "index = row".
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        logical, intent(in) :: strict                  !! .true. to reject precision loss too.
        integer(int64) :: j
        !
        if (.not. strict) return
        select case (to_kind)
        case (PK_FLOAT32, PK_FLOAT32_VEC)
            do j = 1_int64, size(v, kind=int64)
                ! real32 -> real64 is exact, so this compares the round trip without ever
                ! converting a real back into an integer (which could be out of range).
                if (real(real(v(j), real32), real64) /= real(v(j), real64)) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_INT32, to_kind, &
                        .true., ival=int(v(j), int64))
                end if
            end do
        end select
    end subroutine chk_from_i32
    !
    !> Checks one contiguous run of int64 values against `to_kind`. Narrowing to int32 is range
    !! checked ALWAYS -- the reader and writer both do -- while the mantissa loss of a large
    !! integer put into a real kind is only reported when `strict` asks.
    subroutine chk_from_i64(cache, name, proc, v, row, to_kind, strict)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        integer(int64), intent(in) :: v(:)             !! the values to check.
        integer(int64), intent(in) :: row              !! the row they belong to, or 0 for "index = row".
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        logical, intent(in) :: strict                  !! .true. to reject precision loss too.
        integer(int64) :: j
        !
        select case (to_kind)
        case (PK_INT32, PK_INT32_VEC)
            do j = 1_int64, size(v, kind=int64)
                if (v(j) < -huge(0_int32) - 1_int64 .or. v(j) > huge(0_int32)) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_INT64, to_kind, &
                        .false., ival=v(j))
                end if
            end do
        case (PK_FLOAT32, PK_FLOAT32_VEC)
            if (.not. strict) return
            do j = 1_int64, size(v, kind=int64)
                if (.not. int64_survives_real(real(real(v(j), real32), real64), v(j))) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_INT64, to_kind, &
                        .true., ival=v(j))
                end if
            end do
        case (PK_FLOAT64, PK_FLOAT64_VEC)
            if (.not. strict) return
            do j = 1_int64, size(v, kind=int64)
                if (.not. int64_survives_real(real(v(j), real64), v(j))) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_INT64, to_kind, &
                        .true., ival=v(j))
                end if
            end do
        end select
    end subroutine chk_from_i64
    !
    !> Checks one contiguous run of real32 values against `to_kind`. Widening to real64 is exact;
    !! an integer target demands a whole number in range, which is what the reader and the writer
    !! both demand of a real column written to an integer one.
    subroutine chk_from_f32(cache, name, proc, v, row, to_kind, strict)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        real(real32), intent(in) :: v(:)               !! the values to check.
        integer(int64), intent(in) :: row              !! the row they belong to, or 0 for "index = row".
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        logical, intent(in) :: strict                  !! accepted for symmetry; real32 has no strict-only rule.
        integer(int64) :: j
        !
        ! No `strict` branch here on purpose: real32 -> real64 is exact, and the integer targets
        ! are checked unconditionally below, so there is no loss left for `strict` to catch.
        select case (to_kind)
        case (PK_INT32, PK_INT32_VEC, PK_INT64, PK_INT64_VEC)
            do j = 1_int64, size(v, kind=int64)
                if (.not. real_fits_integer(real(v(j), real64), to_kind)) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_FLOAT32, to_kind, &
                        .false., rval=real(v(j), real64))
                end if
            end do
        end select
    end subroutine chk_from_f32
    !
    !> Checks one contiguous run of real64 values against `to_kind`. An integer target demands a
    !! whole number in range. A real32 target rejects a FINITE value too large for real32 --
    !! overflowing to infinity is a bigger surprise than losing digits -- and, under `strict`,
    !! any value that would not come back the same.
    subroutine chk_from_f64(cache, name, proc, v, row, to_kind, strict)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        real(real64), intent(in) :: v(:)               !! the values to check.
        integer(int64), intent(in) :: row              !! the row they belong to, or 0 for "index = row".
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        logical, intent(in) :: strict                  !! .true. to reject precision loss too.
        integer(int64) :: j
        !
        select case (to_kind)
        case (PK_INT32, PK_INT32_VEC, PK_INT64, PK_INT64_VEC)
            do j = 1_int64, size(v, kind=int64)
                if (.not. real_fits_integer(v(j), to_kind)) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_FLOAT64, to_kind, &
                        .false., rval=v(j))
                end if
            end do
        case (PK_FLOAT32, PK_FLOAT32_VEC)
            do j = 1_int64, size(v, kind=int64)
                ! NaN and +-Infinity carry across unchanged and are not overflow: a stored
                ! infinity IS representable in real32, and only a finite value that no longer
                ! fits has been lost. ieee_is_nan rather than v /= v, per the project convention.
                if (ieee_is_nan(v(j))) cycle
                if (ieee_is_finite(v(j)) .and. abs(v(j)) > real(huge(0.0_real32), real64)) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_FLOAT64, to_kind, &
                        .false., rval=v(j))
                end if
                if (.not. strict) cycle
                if (real(real(v(j), real32), real64) /= v(j)) then
                    call cast_reject(cache, name, proc, cast_row_of(row, j), PK_FLOAT64, to_kind, &
                        .true., rval=v(j))
                end if
            end do
        end select
    end subroutine chk_from_f64
    !
    !> The row a checked value belongs to: `row` when the caller handed over one row's slice of a
    !! vector column, and the element index itself when it handed over a whole scalar column.
    pure integer(int64) function cast_row_of(row, j) result(r)
        integer(int64), intent(in) :: row !! the row, or 0 for "the index is the row".
        integer(int64), intent(in) :: j   !! 1-based index within the slice handed over.
        r = row
        if (row == 0_int64) r = j
    end function cast_row_of
    !
    !> Whether `d`, the real form of `v`, converts back to exactly `v`.
    !!
    !! Deliberately compared through int64 rather than through real64: `real(v, real64)` is itself
    !! inexact past 2**53, so comparing two real64s would report a loss that the real64 comparison
    !! introduced. Converting the real back to an integer is only safe below 2**63, so a magnitude
    !! at or beyond that is simply reported as lost -- the only values affected sit within one ulp
    !! of int64's own ceiling.
    pure logical function int64_survives_real(d, v) result(ok)
        real(real64), intent(in) :: d  !! the value converted to real (and back to real64 exactly).
        integer(int64), intent(in) :: v !! the integer it came from.
        ok = .false.
        if (abs(d) >= 9223372036854775808.0_real64) return
        ok = int(d, int64) == v
    end function int64_survives_real
    !
    !> Whether real64 `d` is a whole number that fits the integer kind `to_kind`. NaN fails the
    !! whole-number test (`anint` of a NaN is a NaN, which equals nothing), which is the answer
    !! wanted: there is no integer a NaN could become.
    pure logical function real_fits_integer(d, to_kind) result(ok)
        real(real64), intent(in) :: d  !! the value to test.
        integer, intent(in) :: to_kind !! PK_INT32/PK_INT64 or their _VEC forms.
        ok = .false.
        if (d /= anint(d)) return
        if (to_kind == PK_INT32 .or. to_kind == PK_INT32_VEC) then
            ok = d >= -2147483648.0_real64 .and. d <= 2147483647.0_real64
        else
            ! The upper bound is written as 2**63 with a strict <, because huge(int64) itself is
            ! not representable in real64 and rounds UP to 2**63 -- comparing <= against it would
            ! admit exactly the one value that overflows.
            ok = d >= -9223372036854775808.0_real64 .and. d < 9223372036854775808.0_real64
        end if
    end function real_fits_integer
    !
    !> error stops naming the offending row, its value and the conversion that cannot carry it.
    !!
    !! The value arrives as `ival` or `rval`, whichever the source kind actually holds, rather
    !! than as text: formatting it here keeps every rejection message identical in shape, and
    !! keeps the checkers free of the `character(len=:), allocatable` locals they would otherwise
    !! each need (a plain character-returning helper is not an option -- see CLAUDE.md).
    subroutine cast_reject(cache, name, proc, row, from_kind, to_kind, precision_only, ival, rval)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! column name, for the message.
        character(len=*), intent(in) :: proc           !! calling procedure, for the message.
        integer(int64), intent(in) :: row              !! the 1-based row that failed.
        integer, intent(in) :: from_kind               !! the column's current PK_* kind.
        integer, intent(in) :: to_kind                 !! the requested PK_* kind.
        logical, intent(in) :: precision_only          !! .true. when only exactness was lost.
        integer(int64), intent(in), optional :: ival   !! the offending value, integer source.
        real(real64), intent(in), optional :: rval     !! the offending value, real source.
        character(len=:), allocatable :: sfx, from_txt, to_txt, word
        character(len=64) :: ks, vs
        !
        call parquet_kind_name(from_kind, from_txt)
        call parquet_kind_name(to_kind, to_txt)
        word = " "
        if (precision_only) word = " exactly "
        vs = "?"
        if (present(ival)) write(vs, "(I0)") ival
        if (present(rval)) write(vs, "(ES23.15E3)") rval
        write(ks, "(I0)") row
        call table_context_suffix(cache, name, sfx)
        error stop EP // trim(proc) // ": the value at row " // trim(adjustl(ks)) // " (" // &
            trim(adjustl(vs)) // ") cannot be represented" // trim(word) // " as " // to_txt // &
            ", so converting from " // from_txt // " would lose information" // sfx
    end subroutine cast_reject
    !
    !> Rewrites `col`'s storage as `to_kind`, in place. Every value has already been checked by
    !! `cast_check_values` and every null row zeroed by `cast_zero_null_rows`, so each arm below
    !! is a single whole-array expression with nothing to guard against.
    !!
    !! `%adopt` rather than `%init` + `%set_all`: the converted array is a temporary that the
    !! column can simply take over, which halves the copying. Adopting drops the unit and the null
    !! bitmap, so both are captured first and put back afterwards -- the nulls one row at a time,
    !! which is the only per-row work here and only happens for a column that has nulls at all.
    !!
    !! Peak memory is the old array plus the new one: an in-place conversion still has to build
    !! the result before it can release the source.
    subroutine cast_apply(col, to_kind)
        type(parquet_column), intent(inout) :: col !! the column to convert.
        integer, intent(in) :: to_kind             !! the target PK_* kind.
        integer(int32), pointer :: p_i32(:), p_i32v(:,:)
        integer(int64), pointer :: p_i64(:), p_i64v(:,:)
        real(real32), pointer :: p_f32(:), p_f32v(:,:)
        real(real64), pointer :: p_f64(:), p_f64v(:,:)
        integer(int32), allocatable :: d_i32(:), d_i32v(:,:)
        integer(int64), allocatable :: d_i64(:), d_i64v(:,:)
        real(real32), allocatable :: d_f32(:), d_f32v(:,:)
        real(real64), allocatable :: d_f64(:), d_f64v(:,:)
        logical, allocatable :: rowvalid(:)
        character(len=:), allocatable :: unit
        integer(int64) :: n, r
        integer(int32) :: w
        !
        n = col%length()
        w = col%colwidth()
        call col%unit_string(unit)
        call col%row_validity(rowvalid)
        ! An empty column has no storage allocated at all, so there is nothing to point at and
        ! nothing to convert -- just re-declare it as the target kind.
        if (n == 0_int64) then
            call col%init(to_kind, 0_int64, w, unit)
            return
        end if
        select case (col%kindof())
        case (PK_INT32)
            call col%data_ptr(p_i32)
            select case (to_kind)
            case (PK_INT64)
                allocate(d_i64(n)); d_i64 = int(p_i32, int64); call col%adopt(d_i64, unit)
            case (PK_FLOAT32)
                allocate(d_f32(n)); d_f32 = real(p_i32, real32); call col%adopt(d_f32, unit)
            case default
                allocate(d_f64(n)); d_f64 = real(p_i32, real64); call col%adopt(d_f64, unit)
            end select
        case (PK_INT64)
            call col%data_ptr(p_i64)
            select case (to_kind)
            case (PK_INT32)
                allocate(d_i32(n)); d_i32 = int(p_i64, int32); call col%adopt(d_i32, unit)
            case (PK_FLOAT32)
                allocate(d_f32(n)); d_f32 = real(p_i64, real32); call col%adopt(d_f32, unit)
            case default
                allocate(d_f64(n)); d_f64 = real(p_i64, real64); call col%adopt(d_f64, unit)
            end select
        case (PK_FLOAT32)
            call col%data_ptr(p_f32)
            select case (to_kind)
            case (PK_INT32)
                allocate(d_i32(n)); d_i32 = int(anint(p_f32), int32); call col%adopt(d_i32, unit)
            case (PK_INT64)
                allocate(d_i64(n)); d_i64 = int(anint(p_f32), int64); call col%adopt(d_i64, unit)
            case default
                allocate(d_f64(n)); d_f64 = real(p_f32, real64); call col%adopt(d_f64, unit)
            end select
        case (PK_FLOAT64)
            call col%data_ptr(p_f64)
            select case (to_kind)
            case (PK_INT32)
                allocate(d_i32(n)); d_i32 = int(anint(p_f64), int32); call col%adopt(d_i32, unit)
            case (PK_INT64)
                allocate(d_i64(n)); d_i64 = int(anint(p_f64), int64); call col%adopt(d_i64, unit)
            case default
                allocate(d_f32(n)); d_f32 = real(p_f64, real32); call col%adopt(d_f32, unit)
            end select
        case (PK_INT32_VEC)
            call col%data_ptr(p_i32v)
            select case (to_kind)
            case (PK_INT64_VEC)
                allocate(d_i64v(w, n)); d_i64v = int(p_i32v, int64); call col%adopt(d_i64v, unit)
            case (PK_FLOAT32_VEC)
                allocate(d_f32v(w, n)); d_f32v = real(p_i32v, real32); call col%adopt(d_f32v, unit)
            case default
                allocate(d_f64v(w, n)); d_f64v = real(p_i32v, real64); call col%adopt(d_f64v, unit)
            end select
        case (PK_INT64_VEC)
            call col%data_ptr(p_i64v)
            select case (to_kind)
            case (PK_INT32_VEC)
                allocate(d_i32v(w, n)); d_i32v = int(p_i64v, int32); call col%adopt(d_i32v, unit)
            case (PK_FLOAT32_VEC)
                allocate(d_f32v(w, n)); d_f32v = real(p_i64v, real32); call col%adopt(d_f32v, unit)
            case default
                allocate(d_f64v(w, n)); d_f64v = real(p_i64v, real64); call col%adopt(d_f64v, unit)
            end select
        case (PK_FLOAT32_VEC)
            call col%data_ptr(p_f32v)
            select case (to_kind)
            case (PK_INT32_VEC)
                allocate(d_i32v(w, n)); d_i32v = int(anint(p_f32v), int32); call col%adopt(d_i32v, unit)
            case (PK_INT64_VEC)
                allocate(d_i64v(w, n)); d_i64v = int(anint(p_f32v), int64); call col%adopt(d_i64v, unit)
            case default
                allocate(d_f64v(w, n)); d_f64v = real(p_f32v, real64); call col%adopt(d_f64v, unit)
            end select
        case default
            call col%data_ptr(p_f64v)
            select case (to_kind)
            case (PK_INT32_VEC)
                allocate(d_i32v(w, n)); d_i32v = int(anint(p_f64v), int32); call col%adopt(d_i32v, unit)
            case (PK_INT64_VEC)
                allocate(d_i64v(w, n)); d_i64v = int(anint(p_f64v), int64); call col%adopt(d_i64v, unit)
            case default
                allocate(d_f32v(w, n)); d_f32v = real(p_f64v, real32); call col%adopt(d_f32v, unit)
            end select
        end select
        if (.not. allocated(rowvalid)) return
        do r = 1_int64, n
            if (.not. rowvalid(r)) call col%set_null(r)
        end do
    end subroutine cast_apply
    !
end submodule parquet_tables_mutate
