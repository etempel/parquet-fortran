!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Mutation that leaves a `parquet_table`'s ROW SET alone: the single-cell validity writers
!! (`%set_null`, `%clear_null`, `%compact_validity`) and the column-structural operations
!! (`%drop_column`, `%rename_column`, `%cast_column`).
!!
!! **Nothing in this file detaches the table** (F-mut-7). That is the whole reason it is a
!! separate file from `parquet_tables_rowmutate.f90`: adding a column, dropping one, renaming one
!! or writing one cell leaves every column the same length, so the table can still read the
!! columns it has not read yet. Only a change to the *row set* breaks that alignment, and every
!! such operation lives in the other file. A new procedure added here must preserve that property
!! -- if it cannot, it belongs next door.
!!
!! The per-kind `%set_element` writers are generated rather than written here; they live in
!! `parquet_tables_access.f90` alongside `%set`, whose shape they follow.
submodule (parquet_tables) parquet_tables_mutate
    implicit none
    !
contains
    !
    ! ---- single-cell validity -------------------------------------------------------------
    !
    module procedure set_null_i32
        call self%set_null(name, int(i, int64))
    end procedure set_null_i32
    !
    module procedure set_null_i64
        integer :: idx
        !
        call table_resolve(self, name, "set_null", idx)
        call table_require_row(self, i, "set_null")
        call self%cache%cols(idx)%values%set_null(i)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_null_i64
    !
    module procedure clear_null_i32
        call self%clear_null(name, int(i, int64))
    end procedure clear_null_i32
    !
    module procedure clear_null_i64
        integer :: idx
        !
        call table_resolve(self, name, "clear_null", idx)
        call table_require_row(self, i, "clear_null")
        call self%cache%cols(idx)%values%clear_null(i)
        self%cache%cols(idx)%user_populated = .true.
    end procedure clear_null_i64
    !
    module procedure table_compact_validity
        integer :: idx
        !
        call table_resolve(self, name, "compact_validity", idx)
        call self%cache%cols(idx)%values%compact_validity()
    end procedure table_compact_validity
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
    module procedure table_drop_column
        integer :: idx, i
        logical :: forced
        character(len=:), allocatable :: sfx
        !
        ! Deliberately table_lookup_or_fail, not table_resolve: dropping a column that was never
        ! read is the cheap memory-reclaiming case, and reading it first to throw it away would
        ! defeat the point.
        call table_lookup_or_fail(self, name, "drop_column", idx)
        forced = .false.
        if (present(force)) forced = force
        ! Not reachable yet, and deliberately written now: `predefined` is only ever set by the
        ! generated table type, which arrives in a later milestone. Writing the guard with the
        ! feature it protects keeps the rule (R8: a predefined column may be dropped only to
        ! reclaim memory, and only on purpose) next to the code rather than in a to-do list.
        ! gcov attribution artifact: the `if` line itself is evaluated on every call and so shows
        ! hits, while the body below never runs -- see CLAUDE.md's "Fortran gcov attribution
        ! artifacts", the guard-clause shape.
        if (self%cache%cols(idx)%predefined .and. .not. forced) then ! GCOVR_EXCL_START
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "drop_column: this is a predefined column, which a program using " // &
                "the generated accessors expects to be there; pass force=.true. if you really " // &
                "mean to drop it (to reclaim memory)" // sfx
        end if
        ! GCOVR_EXCL_STOP
        call self%cache%cols(idx)%values%clear()
        ! Shift the tail down over the dropped slot. The store is name-keyed, so the order of the
        ! remaining slots is not itself meaningful -- but %column_names reports it, and keeping it
        ! stable makes a drop look like a drop rather than a reshuffle.
        do i = idx, self%cache%ncols - 1
            self%cache%cols(i) = self%cache%cols(i + 1)
        end do
        call self%cache%cols(self%cache%ncols)%values%clear()
        self%cache%cols(self%cache%ncols) = parquet_table_column()
        self%cache%ncols = self%cache%ncols - 1
    end procedure table_drop_column
    !
    module procedure table_rename_column
        integer :: idx
        character(len=:), allocatable :: sfx
        !
        call table_lookup_or_fail(self, old_name, "rename_column", idx)
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
    end procedure table_rename_column
    !
    module procedure table_cast_column
        integer :: idx, new_idx
        integer(int64) :: n, k
        integer :: from_kind
        character(len=:), allocatable :: sfx, from_txt, to_txt, unit
        real(real64), allocatable :: buf(:)
        logical, allocatable :: was_null(:)
        !
        call table_resolve(self, name, "cast_column", idx)
        from_kind = self%cache%cols(idx)%values%kindof()
        call parquet_kind_name(from_kind, from_txt)
        call parquet_kind_name(to_kind, to_txt)
        if (.not. (cast_is_numeric(from_kind) .and. cast_is_numeric(to_kind))) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "cast_column: can only cast between the numeric scalar kinds, " // &
                "not from " // from_txt // " to " // to_txt // sfx
        end if
        if (table_find(self, new_name) > 0) then
            call table_context_suffix(self%cache, new_name, sfx)
            error stop EP // "cast_column: a column of the new name already exists" // sfx
        end if
        if (len_trim(new_name) == 0) then
            call table_context_suffix(self%cache, name, sfx)
            error stop EP // "cast_column: the new name is blank" // sfx
        end if
        n = self%cache%cols(idx)%values%length()
        ! Read the whole source through real64 first and check EVERY value before allocating the
        ! destination, so a cast that cannot be represented leaves the table exactly as it was
        ! rather than half-converted.
        allocate(buf(max(n, 1_int64)))
        allocate(was_null(max(n, 1_int64)))
        buf = 0.0_real64
        was_null = .false.
        do k = 1_int64, n
            was_null(k) = self%cache%cols(idx)%values%is_null(k)
            if (was_null(k)) cycle
            call cast_read_value(self%cache%cols(idx)%values, k, buf(k))
            call cast_check_value(self%cache, name, buf(k), k, to_kind, from_txt, to_txt)
        end do
        call table_new_slot(self, new_name, .false., new_idx)
        call self%cache%cols(idx)%values%unit_string(unit)
        call cast_fill_column(self%cache%cols(new_idx)%values, to_kind, n, buf, was_null, unit)
        self%cache%cols(new_idx)%declared_kind = to_kind
        self%cache%cols(new_idx)%width = 1_int32
        self%cache%cols(new_idx)%residency = RES_FULL
        self%cache%cols(new_idx)%user_populated = .true.
    end procedure table_cast_column
    !
    !> Whether a PK_* kind is one `%cast_column` can convert between: the four numeric SCALAR
    !! kinds. Logical, string, temporal and every *_VEC kind are excluded -- each would need its
    !! own semantics (what is a null date as an integer?), and none is needed by the one caller
    !! this exists for, which is `%append`'s kind-mismatch escape hatch (RF14).
    pure logical function cast_is_numeric(kind) result(ok)
        integer, intent(in) :: kind !! the PK_* discriminator to test.
        ok = kind == PK_INT32 .or. kind == PK_INT64 .or. kind == PK_FLOAT32 .or. kind == PK_FLOAT64
    end function cast_is_numeric
    !
    !> Reads element `k` of a numeric scalar column as a real64, whatever its stored kind.
    !!
    !! real64 is the one type that holds every value the four source kinds can produce without
    !! loss: int32 and float32 fit outright, and an int64 beyond 2**53 is caught by
    !! `cast_check_value`'s exact-round-trip test rather than being silently rounded here.
    subroutine cast_read_value(col, k, value)
        type(parquet_column), intent(in) :: col  !! the source column.
        integer(int64), intent(in) :: k          !! 1-based row index.
        real(real64), intent(out) :: value       !! the value, widened to real64.
        integer(int32) :: v32
        integer(int64) :: v64
        real(real32) :: r32
        !
        select case (col%kindof())
        case (PK_INT32)
            call col%get_at(k, v32)
            value = real(v32, real64)
        case (PK_INT64)
            call col%get_at(k, v64)
            value = real(v64, real64)
        case (PK_FLOAT32)
            call col%get_at(k, r32)
            value = real(r32, real64)
        case default
            call col%get_at(k, value)
        end select
    end subroutine cast_read_value
    !
    !> error stops unless `value` survives the trip into `to_kind` unchanged (Q3c-3).
    !!
    !! A cast the caller asked for by name is exactly where a silent truncation is most expensive
    !! to find later, so a value that would not come back the same aborts, naming the row and the
    !! value. Integral targets additionally require the value to be a whole number, mirroring the
    !! `value == anint(value)` test the numeric writer already applies.
    subroutine cast_check_value(cache, name, value, k, to_kind, from_txt, to_txt)
        type(parquet_table_cache), intent(in) :: cache !! the store, for the message context.
        character(len=*), intent(in) :: name           !! source column name, for the message.
        real(real64), intent(in) :: value              !! the value to check.
        integer(int64), intent(in) :: k                !! its 1-based row index.
        integer, intent(in) :: to_kind                 !! the target PK_* kind.
        character(len=*), intent(in) :: from_txt       !! source kind name, for the message.
        character(len=*), intent(in) :: to_txt         !! target kind name, for the message.
        character(len=:), allocatable :: sfx
        character(len=64) :: vs, ks
        logical :: ok
        !
        ok = .true.
        select case (to_kind)
        case (PK_INT32)
            ok = value == anint(value) .and. abs(value) <= real(huge(0_int32), real64)
        case (PK_INT64)
            ok = value == anint(value) .and. abs(value) <= real(huge(0_int64), real64)
        case (PK_FLOAT32)
            ok = abs(value) <= real(huge(0.0_real32), real64) .or. .not. (value == value)
            if (ok) ok = real(real(value, real32), real64) == value
        end select
        if (ok) return
        write(vs, "(ES23.15E3)") value
        write(ks, "(I0)") k
        call table_context_suffix(cache, name, sfx)
        error stop EP // "cast_column: the value at row " // trim(ks) // " (" // trim(adjustl(vs)) // &
            ") cannot be represented as " // to_txt // ", so casting from " // from_txt // &
            " would lose information" // sfx
    end subroutine cast_check_value
    !
    !> Fills a freshly created column of `to_kind` from the checked real64 buffer, restoring the
    !! source's nulls row for row. `unit` carries over unchanged: a kind cast is not a unit change
    !! (RF14), and inventing one would pre-empt the deferred unit-conversion feature.
    subroutine cast_fill_column(col, to_kind, n, buf, was_null, unit)
        type(parquet_column), intent(inout) :: col !! the destination column.
        integer, intent(in) :: to_kind             !! target PK_* kind.
        integer(int64), intent(in) :: n            !! row count.
        real(real64), intent(in) :: buf(:)         !! the checked values.
        logical, intent(in) :: was_null(:)         !! per row: .true. where the source was null.
        character(len=*), intent(in) :: unit       !! the source's unit string.
        integer(int64) :: k
        !
        call col%init(to_kind, n, 1_int32, unit)
        do k = 1_int64, n
            if (was_null(k)) then
                call col%set_null(k)
                cycle
            end if
            select case (to_kind)
            case (PK_INT32)
                call col%set_at(k, int(anint(buf(k)), int32))
            case (PK_INT64)
                call col%set_at(k, int(anint(buf(k)), int64))
            case (PK_FLOAT32)
                call col%set_at(k, real(buf(k), real32))
            case default
                call col%set_at(k, buf(k))
            end select
        end do
    end subroutine cast_fill_column
    !
end submodule parquet_tables_mutate
