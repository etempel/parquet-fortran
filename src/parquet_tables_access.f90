!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_tables.py
! The kind table lives in tools/generate_parquet_columns.py; edit it there, not here.
!
!> Per-kind value access for `parquet_table`: the zero-copy pointer path (`%col`), the
!! widening copy-out (`%get`) and the copy-back (`%set`).
submodule (parquet_tables) parquet_tables_access
    implicit none
    !
    !> Rows a `stat_*` scan takes per block: the length of the mask it asks
    !! `parquet_column_row_validity_range` for at a time. 4096 rows is a 16 KiB mask, which stays
    !! in the first-level cache beside the block of values it screens. test/error_scenarios.f90's
    !! `scenario_table_print_stat_scan` fixture is sized to span more than two of these blocks, so
    !! a boundary mistake here has a test that can see it -- change one and the other together.
    integer(int64), parameter :: STAT_BLOCK = 4096_int64
    !
contains
    !
    module procedure col_ptr_i32
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_INT32, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_i32
    !
    module procedure col_ptr_i64
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_INT64, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_i64
    !
    module procedure col_ptr_f32
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_FLOAT32, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_f32
    !
    module procedure col_ptr_f64
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_FLOAT64, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_f64
    !
    module procedure col_ptr_bool
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_LOGICAL, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_bool
    !
    module procedure col_ptr_date
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_DATE, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_date
    !
    module procedure col_ptr_time
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_TIME, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_time
    !
    module procedure col_ptr_ts
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_TIMESTAMP, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_ts
    !
    module procedure col_ptr_i32v
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_INT32_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_i32v
    !
    module procedure col_ptr_i64v
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_INT64_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_i64v
    !
    module procedure col_ptr_f32v
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_FLOAT32_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_f32v
    !
    module procedure col_ptr_f64v
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_FLOAT64_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_f64v
    !
    module procedure col_ptr_boolv
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_LOGICAL_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_boolv
    !
    module procedure col_ptr_datev
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_DATE_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_datev
    !
    module procedure col_ptr_timev
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_TIME_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_timev
    !
    module procedure col_ptr_tsv
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        ! A copy, not an alias: validity is a packed bitmap, so there is no logical array in the
        ! column for a pointer to refer to. It is a snapshot -- writing through `p` afterwards
        ! does not update it, and nor does %set_null.
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call cache_require_ptr_kind(self%cache, idx, PK_TIMESTAMP_VEC, "col")
        call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
    end procedure col_ptr_tsv
    !
    module procedure col_ptr_strcol
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "col")
        call parquet_column_string_column(self%cache%cols(idx)%values, p)
    end procedure col_ptr_strcol
    !
    module procedure set_arr_strcol
        integer :: idx
        integer(int64) :: n, k
        logical :: mod_nulls
        logical, allocatable :: was_null(:)
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, arr%size(), "set")
        ! Replaces the packed store wholesale with an independent copy, so the caller's own column
        ! and the table's do not end up sharing storage. %set is a value replacement, exactly as
        ! the character-array form is; it is not a way to hand ownership over.
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! modify_nulls = .false. keeps a row null if it was null HERE, on top of whatever the
        ! source says -- the union of the two, not one replacing the other. The character-array
        ! sibling (`refill_string_store`) can simply restore the destination's mask because its
        ! source is a plain array with no validity of its own; this source carries nulls, and
        ! discarding them would throw away something the caller explicitly supplied. Where the
        ! source is null-free the two rules agree, which is every case the sibling covers.
        ! Captured BEFORE the overwrite, or there is nothing left to read it from.
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        if (.not. mod_nulls) then
            n = store%size()
            allocate(was_null(n))
            do k = 1_int64, n
                was_null(k) = store%is_null(k)
            end do
        end if
        store = arr%clone()
        if (.not. mod_nulls) then
            do k = 1_int64, n
                if (was_null(k)) call store%set_null(k)
            end do
        end if
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_strcol
    !
    module procedure table_column_stat_text
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        select case (values%kindof())
        case (PK_INT32)
            call stat_i32(values, nulls, min_s, max_s)
        case (PK_INT64)
            call stat_i64(values, nulls, min_s, max_s)
        case (PK_FLOAT32)
            call stat_f32(values, nulls, min_s, max_s)
        case (PK_FLOAT64)
            call stat_f64(values, nulls, min_s, max_s)
        case (PK_LOGICAL)
            call stat_bool(values, nulls, min_s, max_s)
        case (PK_STRING)
            call stat_str(values, nulls, min_s, max_s)
        case (PK_DATE)
            call stat_date(values, nulls, min_s, max_s)
        case (PK_TIME)
            call stat_time(values, nulls, min_s, max_s)
        case (PK_TIMESTAMP)
            call stat_ts(values, nulls, min_s, max_s)
        case (PK_INT32_VEC)
            call stat_i32v(values, nulls, min_s, max_s)
        case (PK_INT64_VEC)
            call stat_i64v(values, nulls, min_s, max_s)
        case (PK_FLOAT32_VEC)
            call stat_f32v(values, nulls, min_s, max_s)
        case (PK_FLOAT64_VEC)
            call stat_f64v(values, nulls, min_s, max_s)
        case (PK_LOGICAL_VEC)
            call stat_boolv(values, nulls, min_s, max_s)
        case (PK_STRING_VEC)
            call stat_strv(values, nulls, min_s, max_s)
        case (PK_DATE_VEC)
            call stat_datev(values, nulls, min_s, max_s)
        case (PK_TIME_VEC)
            call stat_timev(values, nulls, min_s, max_s)
        case (PK_TIMESTAMP_VEC)
            call stat_tsv(values, nulls, min_s, max_s)
        case (PK_LIST, PK_MAP)
            ! A list or a map has no min/max VALUE -- there is no order on a whole row -- so the
            ! stat columns report the shortest and longest ROW instead, which is the one summary a
            ! reader of a %print_stat table actually wants from a ragged column.
            call stat_container_lengths(values, nulls, min_s, max_s)
        case default
            ! PK_NONE, and PK_STRUCT: nothing to summarize. A struct's rows all carry the same
            ! field count by construction, so a length extreme would print the same number twice.
            ! The null count still counts.
            nulls = count_null_rows(values)
            return
        end select
    end procedure table_column_stat_text
    !
    !> Rows that are null, for a kind with nothing else to report (PK_STRUCT; PK_NONE holds no
    !! rows): the block walk the `stat_*` scans use, with no values to read beside it.
    integer(int64) function count_null_rows(values) result(nulls)
        type(parquet_column), intent(in), target :: values !! the column.
        integer(int64) :: n, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK)
        !
        n = values%length()
        nvalid = 0_int64
        do lo = 1_int64, n, STAT_BLOCK
            hi = min(lo + STAT_BLOCK - 1_int64, n)
            nb = hi - lo + 1_int64
            call parquet_column_row_validity_range(values, lo, hi, valid)
            nvalid = nvalid + count(valid(1:nb), kind=int64)
        end do
        nulls = n - nvalid
    end function count_null_rows
    !
    !> Shortest and longest ROW of a container column, excluding null rows, and the null count.
    !!
    !! **Reads nothing.** The lengths come from the container's own offsets, which are resident
    !! whenever the column is -- %print_stat's documented contract is that it leaves a lazy table
    !! lazy, and a stat routine that triggered a read would break it silently (the table's own
    !! %print_stat test is what asserts that, not this procedure).
    !!
    !! **A NULL row has no length and is excluded from both extremes**, rather than counted as
    !! zero: a column of mostly nulls would otherwise report `min = 0` for rows that do not exist.
    !! An all-null column reports "-" for both, exactly as an unsummarizable kind does. Note this
    !! is a different question from a row of length zero, which is a real, present, empty list and
    !! IS counted -- `test/fixtures/list_widths.parquet`'s `with_empty` column has both.
    subroutine stat_container_lengths(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values !! the column to summarize.
        integer(int64), intent(out) :: nulls               !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s !! shortest present row, or "-".
        character(len=:), allocatable, intent(out) :: max_s !! longest present row, or "-".
        class(parquet_container_column), pointer :: c
        integer(int64) :: k, n, lo, hi, len_k
        logical :: seen
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        call parquet_column_container(values, c)
        if (.not. associated(c)) return
        n = c%nrows()
        seen = .false.
        lo = 0_int64
        hi = 0_int64
        do k = 1_int64, n
            if (c%is_null_row(k)) then
                nulls = nulls + 1_int64
                cycle
            end if
            call container_row_length(c, k, len_k)
            if (.not. seen) then
                lo = len_k
                hi = len_k
                seen = .true.
            else
                lo = min(lo, len_k)
                hi = max(hi, len_k)
            end if
        end do
        if (.not. seen) return
        write (buf, '(i0)') lo
        min_s = trim(buf)
        write (buf, '(i0)') hi
        max_s = trim(buf)
    end subroutine stat_container_lengths
    !
    !> The number of elements (list) or entries (map) in row `k` of a container.
    !!
    !! A `select type` rather than a twelfth deferred binding on the abstract base: a row length is
    !! a display feature, and the base's bindings are the ones every structural operation needs.
    !! Adding one there would oblige every future container type to implement it for a `%print_stat`
    !! column -- see feature_container_phase6.md's Q4.
    subroutine container_row_length(c, k, n)
        class(parquet_container_column), intent(in) :: c !! the container.
        integer(int64), intent(in) :: k                  !! 1-based row.
        integer(int64), intent(out) :: n                 !! elements/entries in that row.
        !
        n = 0_int64
        select type (c)
        type is (parquet_list_column)
            n = c%length(k)
        type is (parquet_map_column)
            n = c%length(k)
        class default ! GCOVR_EXCL_START -- only PK_LIST and PK_MAP reach here; PK_STRUCT is
            ! handled by table_column_stat_text's own case default above.
            n = 0_int64
        end select ! GCOVR_EXCL_STOP
    end subroutine container_row_length
    !
    !> One float extreme as text, carrying the leading zero a value below one needs.
    !!
    !! `G0.d` leaves that zero to the processor -- gfortran and nagfor write `0.500000`, flang and
    !! ifx write `.500000` -- so without this the same table prints a different report per
    !! compiler. `parquet_qc_format_real` (`src/parquet_write_numeric.f90`) puts it back for the
    !! qc-violation warning and `real_text` (`src/parquet_tables_display.f90`) for `%print_rows`;
    !! keep the three in step.
    subroutine stat_real_text(buf, text)
        character(len=*), intent(in) :: buf                !! the `G0.d` rendering, as written.
        character(len=:), allocatable, intent(out) :: text !! that rendering, trimmed.
        !
        text = trim(adjustl(buf))
        if (len(text) > 0) then
            if (text(1:1) == ".") then
                text = "0" // text ! GCOVR_EXCL_LINE
            else if (len(text) > 1) then
                if (text(1:2) == "-.") text = "-0" // text(2:)
            end if
        end if
    end subroutine stat_real_text
    !
    !> Smallest and largest PK_INT32 value over the rows that hold one, and the null count, in one pass.
    !!
    !! A column with no nulls is two whole-column reductions; one with nulls walks a block at a
    !! time behind the row mask, its running extremes seeded with the kind's own bounds so no
    !! "first value" test sits in the loop.
    subroutine stat_i32(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int32), pointer :: p(:)
        integer(int32) :: mn, mx
        integer(int64) :: n, i, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK)
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        if (.not. parquet_column_any_null(values)) then
            mn = minval(p(1:n))
            mx = maxval(p(1:n))
        else
            mn = huge(mn)
            mx = -huge(mx) - 1
            nvalid = 0_int64
            do lo = 1_int64, n, STAT_BLOCK
                hi = min(lo + STAT_BLOCK - 1_int64, n)
                nb = hi - lo + 1_int64
                call parquet_column_row_validity_range(values, lo, hi, valid)
                do i = lo, hi
                    if (valid(i - lo + 1_int64)) then
                        mn = min(mn, p(i))
                        mx = max(mx, p(i))
                    end if
                end do
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            end do
            nulls = n - nvalid
            if (nvalid == 0_int64) return
        end if
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i32

    !> Smallest and largest PK_INT64 value over the rows that hold one, and the null count, in one pass.
    !!
    !! A column with no nulls is two whole-column reductions; one with nulls walks a block at a
    !! time behind the row mask, its running extremes seeded with the kind's own bounds so no
    !! "first value" test sits in the loop.
    subroutine stat_i64(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int64), pointer :: p(:)
        integer(int64) :: mn, mx
        integer(int64) :: n, i, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK)
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        if (.not. parquet_column_any_null(values)) then
            mn = minval(p(1:n))
            mx = maxval(p(1:n))
        else
            mn = huge(mn)
            mx = -huge(mx) - 1
            nvalid = 0_int64
            do lo = 1_int64, n, STAT_BLOCK
                hi = min(lo + STAT_BLOCK - 1_int64, n)
                nb = hi - lo + 1_int64
                call parquet_column_row_validity_range(values, lo, hi, valid)
                do i = lo, hi
                    if (valid(i - lo + 1_int64)) then
                        mn = min(mn, p(i))
                        mx = max(mx, p(i))
                    end if
                end do
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            end do
            nulls = n - nvalid
            if (nvalid == 0_int64) return
        end if
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i64

    !> Smallest and largest PK_FLOAT32 value over the rows that hold one, and the null count, in one
    !! pass.
    !!
    !! A NaN never enters the ordering. It is excluded, as `pf_minmax` excludes it and as
    !! Parquet's own statistics do, and a column whose every value is NaN reports NaN.
    subroutine stat_f32(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real32), pointer :: p(:)
        real(real32) :: mn, mx, v, nanv
        integer(int64) :: n, i, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK), first, saw_nan, has_nulls
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        has_nulls = parquet_column_any_null(values)
        first = .true.
        saw_nan = .false.
        nvalid = 0_int64
        do lo = 1_int64, n, STAT_BLOCK
            hi = min(lo + STAT_BLOCK - 1_int64, n)
            nb = hi - lo + 1_int64
            if (has_nulls) then
                call parquet_column_row_validity_range(values, lo, hi, valid)
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            else
                nvalid = nvalid + nb
            end if
            do i = lo, hi
                if (has_nulls) then
                    if (.not. valid(i - lo + 1_int64)) cycle
                end if
                v = p(i)
                if (v /= v) then
                    if (.not. saw_nan) then
                        nanv = v
                        saw_nan = .true.
                    end if
                else if (first) then
                    mn = v
                    mx = v
                    first = .false.
                else
                    mn = min(mn, v)
                    mx = max(mx, v)
                end if
            end do
        end do
        nulls = n - nvalid
        if (first) then
            ! "-" is reserved for a column with nothing to report. Values that are all NaN are
            ! values, so they report NaN -- and `nanv` carries one of the column's own rather
            ! than building a fresh one, which nagfor would trap on (`0.0/0.0` raises).
            if (.not. saw_nan) return
            mn = nanv
            mx = nanv
        end if
        write(buf, "(G0.6)") mn
        call stat_real_text(buf, min_s)
        write(buf, "(G0.6)") mx
        call stat_real_text(buf, max_s)
    end subroutine stat_f32

    !> Smallest and largest PK_FLOAT64 value over the rows that hold one, and the null count, in one
    !! pass.
    !!
    !! A NaN never enters the ordering. It is excluded, as `pf_minmax` excludes it and as
    !! Parquet's own statistics do, and a column whose every value is NaN reports NaN.
    subroutine stat_f64(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real64), pointer :: p(:)
        real(real64) :: mn, mx, v, nanv
        integer(int64) :: n, i, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK), first, saw_nan, has_nulls
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        has_nulls = parquet_column_any_null(values)
        first = .true.
        saw_nan = .false.
        nvalid = 0_int64
        do lo = 1_int64, n, STAT_BLOCK
            hi = min(lo + STAT_BLOCK - 1_int64, n)
            nb = hi - lo + 1_int64
            if (has_nulls) then
                call parquet_column_row_validity_range(values, lo, hi, valid)
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            else
                nvalid = nvalid + nb
            end if
            do i = lo, hi
                if (has_nulls) then
                    if (.not. valid(i - lo + 1_int64)) cycle
                end if
                v = p(i)
                if (v /= v) then
                    if (.not. saw_nan) then
                        nanv = v
                        saw_nan = .true.
                    end if
                else if (first) then
                    mn = v
                    mx = v
                    first = .false.
                else
                    mn = min(mn, v)
                    mx = max(mx, v)
                end if
            end do
        end do
        nulls = n - nvalid
        if (first) then
            ! "-" is reserved for a column with nothing to report. Values that are all NaN are
            ! values, so they report NaN -- and `nanv` carries one of the column's own rather
            ! than building a fresh one, which nagfor would trap on (`0.0/0.0` raises).
            if (.not. saw_nan) return
            mn = nanv
            mx = nanv
        end if
        write(buf, "(G0.6)") mn
        call stat_real_text(buf, min_s)
        write(buf, "(G0.6)") mx
        call stat_real_text(buf, max_s)
    end subroutine stat_f64

    !> PK_LOGICAL: true/false counts rather than an ordering, and the null count, in one pass.
    subroutine stat_bool(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! "T:<n>".
        character(len=:), allocatable, intent(out) :: max_s    !! "F:<n>".
        logical, pointer :: p(:)
        integer(int64) :: n, nt, nf, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK)
        character(len=32) :: buf
        !
        nulls = 0_int64
        nt = 0_int64
        nf = 0_int64
        n = values%length()
        if (n > 0_int64) then
            call parquet_column_data_ptr(values, p)
            if (.not. parquet_column_any_null(values)) then
                nt = count(p(1:n), kind=int64)
                nf = n - nt
            else
                nvalid = 0_int64
                do lo = 1_int64, n, STAT_BLOCK
                    hi = min(lo + STAT_BLOCK - 1_int64, n)
                    nb = hi - lo + 1_int64
                    call parquet_column_row_validity_range(values, lo, hi, valid)
                    nt = nt + count(p(lo:hi) .and. valid(1:nb), kind=int64)
                    nvalid = nvalid + count(valid(1:nb), kind=int64)
                end do
                nf = nvalid - nt
                nulls = n - nvalid
            end if
        end if
        write(buf, "(I0)") nt
        min_s = "T:" // trim(buf)
        write(buf, "(I0)") nf
        max_s = "F:" // trim(buf)
    end subroutine stat_bool

    !> PK_STRING: lexicographically smallest and largest value, and the null count, in one pass.
    subroutine stat_str(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        type(parquet_string_column), pointer :: store
        character(len=:), allocatable :: sv
        integer(int64) :: imin, imax
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        call parquet_column_string_column(values, store)
        ! One element per row, so the store's own counter is the row count.
        nulls = store%null_count()
        ! Found by INDEX inside the store, over every non-null element: `%argminmax` carries its
        ! two candidates' payload bounds across the scan and pays one byte comparison per element,
        ! where a `%compare` against each running winner re-validated both indices and re-read
        ! both offset pairs twice per element, and `%get` would allocate a deferred-length string
        ! per row for a scan that only ever keeps two of them (feature_risks.md Risk-60). Its
        ! ordering is Fortran's own `<`, blanks and all.
        call store%argminmax(imin, imax)
        if (imin == 0_int64) return
        ! Only the two winners are materialized. Trimmed for display only: a vector string column
        ! stores its values blank-padded to the widest element, and printing that padding says
        ! nothing. Fortran's own comparison blank-pads the shorter operand anyway, so trimming
        ! cannot change which value won.
        call store%get(imin, sv)
        min_s = trim(sv)
        call store%get(imax, sv)
        max_s = trim(sv)
    end subroutine stat_str

    !> PK_DATE: earliest and latest value, in ISO-8601 form, and the null count, in one pass.
    subroutine stat_date(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_date), pointer :: p(:)
        type(parquet_date) :: mn, mx
        integer(int64) :: n, i
        logical :: first
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        first = .true.
        do i = 1_int64, n
            if (p(i)%is_null()) then
                nulls = nulls + 1_int64
                cycle
            end if
            if (first) then
                mn = p(i)
                mx = p(i)
                first = .false.
            else
                if (p(i) < mn) mn = p(i)
                if (mx < p(i)) mx = p(i)
            end if
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_date

    !> PK_TIME: earliest and latest value, in ISO-8601 form, and the null count, in one pass.
    subroutine stat_time(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_time), pointer :: p(:)
        type(parquet_time) :: mn, mx
        integer(int64) :: n, i
        logical :: first
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        first = .true.
        do i = 1_int64, n
            if (p(i)%is_null()) then
                nulls = nulls + 1_int64
                cycle
            end if
            if (first) then
                mn = p(i)
                mx = p(i)
                first = .false.
            else
                if (p(i) < mn) mn = p(i)
                if (mx < p(i)) mx = p(i)
            end if
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_time

    !> PK_TIMESTAMP: earliest and latest value, in ISO-8601 form, and the null count, in one pass.
    subroutine stat_ts(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_timestamp), pointer :: p(:)
        type(parquet_timestamp) :: mn, mx
        integer(int64) :: n, i
        logical :: first
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        first = .true.
        do i = 1_int64, n
            if (p(i)%is_null()) then
                nulls = nulls + 1_int64
                cycle
            end if
            if (first) then
                mn = p(i)
                mx = p(i)
                first = .false.
            else
                if (p(i) < mn) mn = p(i)
                if (mx < p(i)) mx = p(i)
            end if
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_ts

    !> Smallest and largest PK_INT32_VEC value over the rows that hold one, and the null count, in one pass.
    !!
    !! A column with no nulls is two whole-column reductions; one with nulls walks a block at a
    !! time behind the row mask, its running extremes seeded with the kind's own bounds so no
    !! "first value" test sits in the loop.
    subroutine stat_i32v(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int32), pointer :: p(:,:)
        integer(int32) :: mn, mx
        integer(int64) :: n, i, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK)
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        if (size(p, 1) < 1) return
        if (.not. parquet_column_any_null(values)) then
            mn = minval(p(:, 1:n))
            mx = maxval(p(:, 1:n))
        else
            mn = huge(mn)
            mx = -huge(mx) - 1
            nvalid = 0_int64
            do lo = 1_int64, n, STAT_BLOCK
                hi = min(lo + STAT_BLOCK - 1_int64, n)
                nb = hi - lo + 1_int64
                call parquet_column_row_validity_range(values, lo, hi, valid)
                do i = lo, hi
                    if (valid(i - lo + 1_int64)) then
                        mn = min(mn, minval(p(:, i)))
                        mx = max(mx, maxval(p(:, i)))
                    end if
                end do
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            end do
            nulls = n - nvalid
            if (nvalid == 0_int64) return
        end if
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i32v

    !> Smallest and largest PK_INT64_VEC value over the rows that hold one, and the null count, in one pass.
    !!
    !! A column with no nulls is two whole-column reductions; one with nulls walks a block at a
    !! time behind the row mask, its running extremes seeded with the kind's own bounds so no
    !! "first value" test sits in the loop.
    subroutine stat_i64v(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int64), pointer :: p(:,:)
        integer(int64) :: mn, mx
        integer(int64) :: n, i, lo, hi, nb, nvalid
        logical :: valid(STAT_BLOCK)
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        if (size(p, 1) < 1) return
        if (.not. parquet_column_any_null(values)) then
            mn = minval(p(:, 1:n))
            mx = maxval(p(:, 1:n))
        else
            mn = huge(mn)
            mx = -huge(mx) - 1
            nvalid = 0_int64
            do lo = 1_int64, n, STAT_BLOCK
                hi = min(lo + STAT_BLOCK - 1_int64, n)
                nb = hi - lo + 1_int64
                call parquet_column_row_validity_range(values, lo, hi, valid)
                do i = lo, hi
                    if (valid(i - lo + 1_int64)) then
                        mn = min(mn, minval(p(:, i)))
                        mx = max(mx, maxval(p(:, i)))
                    end if
                end do
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            end do
            nulls = n - nvalid
            if (nvalid == 0_int64) return
        end if
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i64v

    !> Smallest and largest PK_FLOAT32_VEC value over the rows that hold one, and the null count, in one
    !! pass.
    !!
    !! A NaN never enters the ordering. It is excluded, as `pf_minmax` excludes it and as
    !! Parquet's own statistics do, and a column whose every value is NaN reports NaN.
    subroutine stat_f32v(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real32), pointer :: p(:,:)
        real(real32) :: mn, mx, v, nanv
        integer(int64) :: n, i, lo, hi, nb, nvalid
        integer :: e
        logical :: valid(STAT_BLOCK), first, saw_nan, has_nulls
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        has_nulls = parquet_column_any_null(values)
        first = .true.
        saw_nan = .false.
        nvalid = 0_int64
        do lo = 1_int64, n, STAT_BLOCK
            hi = min(lo + STAT_BLOCK - 1_int64, n)
            nb = hi - lo + 1_int64
            if (has_nulls) then
                call parquet_column_row_validity_range(values, lo, hi, valid)
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            else
                nvalid = nvalid + nb
            end if
            do i = lo, hi
                if (has_nulls) then
                    if (.not. valid(i - lo + 1_int64)) cycle
                end if
                do e = 1, size(p, 1)
                    v = p(e, i)
                    if (v /= v) then
                        if (.not. saw_nan) then
                            nanv = v
                            saw_nan = .true.
                        end if
                    else if (first) then
                        mn = v
                        mx = v
                        first = .false.
                    else
                        mn = min(mn, v)
                        mx = max(mx, v)
                    end if
                end do
            end do
        end do
        nulls = n - nvalid
        if (first) then
            ! "-" is reserved for a column with nothing to report. Values that are all NaN are
            ! values, so they report NaN -- and `nanv` carries one of the column's own rather
            ! than building a fresh one, which nagfor would trap on (`0.0/0.0` raises).
            if (.not. saw_nan) return
            mn = nanv
            mx = nanv
        end if
        write(buf, "(G0.6)") mn
        call stat_real_text(buf, min_s)
        write(buf, "(G0.6)") mx
        call stat_real_text(buf, max_s)
    end subroutine stat_f32v

    !> Smallest and largest PK_FLOAT64_VEC value over the rows that hold one, and the null count, in one
    !! pass.
    !!
    !! A NaN never enters the ordering. It is excluded, as `pf_minmax` excludes it and as
    !! Parquet's own statistics do, and a column whose every value is NaN reports NaN.
    subroutine stat_f64v(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real64), pointer :: p(:,:)
        real(real64) :: mn, mx, v, nanv
        integer(int64) :: n, i, lo, hi, nb, nvalid
        integer :: e
        logical :: valid(STAT_BLOCK), first, saw_nan, has_nulls
        character(len=32) :: buf
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        has_nulls = parquet_column_any_null(values)
        first = .true.
        saw_nan = .false.
        nvalid = 0_int64
        do lo = 1_int64, n, STAT_BLOCK
            hi = min(lo + STAT_BLOCK - 1_int64, n)
            nb = hi - lo + 1_int64
            if (has_nulls) then
                call parquet_column_row_validity_range(values, lo, hi, valid)
                nvalid = nvalid + count(valid(1:nb), kind=int64)
            else
                nvalid = nvalid + nb
            end if
            do i = lo, hi
                if (has_nulls) then
                    if (.not. valid(i - lo + 1_int64)) cycle
                end if
                do e = 1, size(p, 1)
                    v = p(e, i)
                    if (v /= v) then
                        if (.not. saw_nan) then
                            nanv = v
                            saw_nan = .true.
                        end if
                    else if (first) then
                        mn = v
                        mx = v
                        first = .false.
                    else
                        mn = min(mn, v)
                        mx = max(mx, v)
                    end if
                end do
            end do
        end do
        nulls = n - nvalid
        if (first) then
            ! "-" is reserved for a column with nothing to report. Values that are all NaN are
            ! values, so they report NaN -- and `nanv` carries one of the column's own rather
            ! than building a fresh one, which nagfor would trap on (`0.0/0.0` raises).
            if (.not. saw_nan) return
            mn = nanv
            mx = nanv
        end if
        write(buf, "(G0.6)") mn
        call stat_real_text(buf, min_s)
        write(buf, "(G0.6)") mx
        call stat_real_text(buf, max_s)
    end subroutine stat_f64v

    !> PK_LOGICAL_VEC: true/false counts rather than an ordering, and the null count, in one pass.
    subroutine stat_boolv(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! "T:<n>".
        character(len=:), allocatable, intent(out) :: max_s    !! "F:<n>".
        logical, pointer :: p(:,:)
        integer(int64) :: n, nt, nf, lo, hi, nb, nvalid, w, i
        logical :: valid(STAT_BLOCK)
        character(len=32) :: buf
        !
        nulls = 0_int64
        nt = 0_int64
        nf = 0_int64
        n = values%length()
        if (n > 0_int64) then
            call parquet_column_data_ptr(values, p)
            w = int(size(p, 1), int64)
            if (.not. parquet_column_any_null(values)) then
                nt = count(p(:, 1:n), kind=int64)
                nf = n*w - nt
            else
                nvalid = 0_int64
                do lo = 1_int64, n, STAT_BLOCK
                    hi = min(lo + STAT_BLOCK - 1_int64, n)
                    nb = hi - lo + 1_int64
                    call parquet_column_row_validity_range(values, lo, hi, valid)
                    do i = lo, hi
                        if (valid(i - lo + 1_int64)) nt = nt + count(p(:, i), kind=int64)
                    end do
                    nvalid = nvalid + count(valid(1:nb), kind=int64)
                end do
                nf = nvalid*w - nt
                nulls = n - nvalid
            end if
        end if
        write(buf, "(I0)") nt
        min_s = "T:" // trim(buf)
        write(buf, "(I0)") nf
        max_s = "F:" // trim(buf)
    end subroutine stat_boolv

    !> PK_STRING_VEC: lexicographically smallest and largest value, and the null count, in one pass.
    subroutine stat_strv(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        type(parquet_string_column), pointer :: store
        character(len=:), allocatable :: sv
        integer(int64) :: imin, imax
        integer(int64) :: n, w, i, e, base
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        call parquet_column_string_column(values, store)
        ! A row is null when ANY of its elements is (the row form's rule), and a store that never
        ! materialized a validity bitmap has none.
        n = values%length()
        w = int(values%colwidth(), int64)
        if (store%has_validity()) then
            do i = 1_int64, n
                base = (i - 1_int64)*w
                do e = 1_int64, w
                    if (store%is_null(base + e)) then
                        nulls = nulls + 1_int64
                        exit
                    end if
                end do
            end do
        end if
        ! Found by INDEX inside the store, over every non-null element: `%argminmax` carries its
        ! two candidates' payload bounds across the scan and pays one byte comparison per element,
        ! where a `%compare` against each running winner re-validated both indices and re-read
        ! both offset pairs twice per element, and `%get` would allocate a deferred-length string
        ! per row for a scan that only ever keeps two of them (feature_risks.md Risk-60). Its
        ! ordering is Fortran's own `<`, blanks and all.
        call store%argminmax(imin, imax)
        if (imin == 0_int64) return
        ! Only the two winners are materialized. Trimmed for display only: a vector string column
        ! stores its values blank-padded to the widest element, and printing that padding says
        ! nothing. Fortran's own comparison blank-pads the shorter operand anyway, so trimming
        ! cannot change which value won.
        call store%get(imin, sv)
        min_s = trim(sv)
        call store%get(imax, sv)
        max_s = trim(sv)
    end subroutine stat_strv

    !> PK_DATE_VEC: earliest and latest value, in ISO-8601 form, and the null count, in one pass.
    subroutine stat_datev(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_date), pointer :: p(:,:)
        type(parquet_date) :: mn, mx
        integer(int64) :: n, i
        integer :: e
        logical :: first
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        first = .true.
        do i = 1_int64, n
            if (any(p(:, i)%is_null())) then
                nulls = nulls + 1_int64
                cycle
            end if
            do e = 1, size(p, 1)
                if (first) then
                    mn = p(e, i)
                    mx = p(e, i)
                    first = .false.
                else
                    if (p(e, i) < mn) mn = p(e, i)
                    if (mx < p(e, i)) mx = p(e, i)
                end if
            end do
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_datev

    !> PK_TIME_VEC: earliest and latest value, in ISO-8601 form, and the null count, in one pass.
    subroutine stat_timev(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_time), pointer :: p(:,:)
        type(parquet_time) :: mn, mx
        integer(int64) :: n, i
        integer :: e
        logical :: first
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        first = .true.
        do i = 1_int64, n
            if (any(p(:, i)%is_null())) then
                nulls = nulls + 1_int64
                cycle
            end if
            do e = 1, size(p, 1)
                if (first) then
                    mn = p(e, i)
                    mx = p(e, i)
                    first = .false.
                else
                    if (p(e, i) < mn) mn = p(e, i)
                    if (mx < p(e, i)) mx = p(e, i)
                end if
            end do
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_timev

    !> PK_TIMESTAMP_VEC: earliest and latest value, in ISO-8601 form, and the null count, in one pass.
    subroutine stat_tsv(values, nulls, min_s, max_s)
        type(parquet_column), intent(in), target :: values     !! the column.
        integer(int64), intent(out) :: nulls                   !! rows that are null.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_timestamp), pointer :: p(:,:)
        type(parquet_timestamp) :: mn, mx
        integer(int64) :: n, i
        integer :: e
        logical :: first
        !
        nulls = 0_int64
        min_s = "-"
        max_s = "-"
        n = values%length()
        if (n < 1_int64) return
        call parquet_column_data_ptr(values, p)
        first = .true.
        do i = 1_int64, n
            if (any(p(:, i)%is_null())) then
                nulls = nulls + 1_int64
                cycle
            end if
            do e = 1, size(p, 1)
                if (first) then
                    mn = p(e, i)
                    mx = p(e, i)
                    first = .false.
                else
                    if (p(e, i) < mn) mn = p(e, i)
                    if (mx < p(e, i)) mx = p(e, i)
                end if
            end do
        end do
        if (first) return
        call mn%to_string(min_s)
        call mx%to_string(max_s)
    end subroutine stat_tsv

    module procedure get_arr_i32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int32), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i32
    !
    module procedure get_arr_i64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int64), pointer :: p(:)
        integer(int32), pointer :: p_i32(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case (PK_INT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p_i32)
            allocate(arr(size(p_i32, kind=int64)))
            arr = p_i32
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i64
    !
    module procedure get_arr_f32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real32), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f32
    !
    module procedure get_arr_f64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real64), pointer :: p(:)
        real(real32), pointer :: p_f32(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT64)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case (PK_FLOAT32)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p_f32)
            allocate(arr(size(p_f32, kind=int64)))
            arr = p_f32
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f64
    !
    module procedure get_arr_bool
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        logical, pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_bool
    !
    module procedure get_arr_date
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_date), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_date
    !
    module procedure get_arr_time
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_time), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_time
    !
    module procedure get_arr_ts
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_timestamp), pointer :: p(:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_ts
    !
    module procedure get_arr_i32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int32), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i32v
    !
    module procedure get_arr_i64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        integer(int64), pointer :: p(:,:)
        integer(int32), pointer :: p_i32v(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT64_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case (PK_INT32_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p_i32v)
            allocate(arr(size(p_i32v, 1, kind=int64), size(p_i32v, 2, kind=int64)))
            arr = p_i32v
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_i64v
    !
    module procedure get_arr_f32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real32), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f32v
    !
    module procedure get_arr_f64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        real(real64), pointer :: p(:,:)
        real(real32), pointer :: p_f32v(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT64_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case (PK_FLOAT32_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p_f32v)
            allocate(arr(size(p_f32v, 1, kind=int64), size(p_f32v, 2, kind=int64)))
            arr = p_f32v
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_f64v
    !
    module procedure get_arr_boolv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        logical, pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_boolv
    !
    module procedure get_arr_datev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_date), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_datev
    !
    module procedure get_arr_timev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_time), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_timev
    !
    module procedure get_arr_tsv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
        type(parquet_timestamp), pointer :: p(:,:)
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP_VEC)
            call parquet_column_data_ptr(self%cache%cols(idx)%values, p)
            allocate(arr(size(p, 1, kind=int64), size(p, 2, kind=int64)))
            arr = p
        case default
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "get: column kind (" // kname // ") cannot be copied into this array" // sfx
        end select
    end procedure get_arr_tsv
    !
    module procedure get_arr_str
        integer :: idx
        type(parquet_string_column), pointer :: src
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING, "get")
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call parquet_column_string_column(self%cache%cols(idx)%values, src)
        arr = src%clone()
    end procedure get_arr_str
    !
    module procedure get_arr_chr
        integer :: idx, maxlen
        integer(int64) :: i, n
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call table_require_kind(self, idx, PK_STRING, "get")
        n = self%cache%cols(idx)%values%length()
        ! Two passes: the width must be the longest element present, and a fixed-length array
        ! cannot be grown per element. A null reads back as "" and so contributes length 0.
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! `%length` measures without allocating and `%copy_to` fills a fixed-length slot without
        ! allocating, so neither pass materializes a string. `maxlen` starts at 1 so that an
        ! all-empty column still yields `character(len=1)` rather than `len=0`.
        maxlen = 1
        do i = 1, n
            if (int(store%length(i)) > maxlen) maxlen = int(store%length(i))
        end do
        allocate(character(len=maxlen) :: arr(n))
        do i = 1, n
            call store%copy_to(i, arr(i), allow_null=.true.)
        end do
    end procedure get_arr_chr
    !
    module procedure get_arr_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: i, n, flat
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of_elem(self%cache, idx, is_valid)
        call table_require_kind(self, idx, PK_STRING_VEC, "get")
        n = self%cache%cols(idx)%values%length()
        wdt = self%cache%cols(idx)%values%colwidth()
        ! A vector string column is ONE flat string store of width*nrows elements, element
        ! (e, i) living at (i-1)*width + e -- reaching it directly is what lets each element
        ! come back as an allocatable string, which the two-pass width measurement needs.
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, n))
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%copy_to(flat, arr(e, i), allow_null=.true.)
            end do
        end do
    end procedure get_arr_chrv
    !
    module procedure set_arr_i32
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i32
    !
    module procedure set_arr_i64
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i64
    !
    module procedure set_arr_f32
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f32
    !
    module procedure set_arr_f64
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f64
    !
    module procedure set_arr_bool
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_bool
    !
    module procedure set_arr_date
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_date
    !
    module procedure set_arr_time
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_time
    !
    module procedure set_arr_ts
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_ts
    !
    module procedure set_arr_i32v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i32v
    !
    module procedure set_arr_i64v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i64v
    !
    module procedure set_arr_f32v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f32v
    !
    module procedure set_arr_f64v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f64v
    !
    module procedure set_arr_boolv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_boolv
    !
    module procedure set_arr_datev
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_datev
    !
    module procedure set_arr_timev
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_timev
    !
    module procedure set_arr_tsv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_tsv
    !
    module procedure set_arr_chr
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, size(arr, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chr
    !
    module procedure set_arr_chrv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid_elem(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_chrv
    !
    module procedure get_element_i32_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i32_i32
    !
    module procedure get_element_i32_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0_int32
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_i32(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_i32_i64
    module procedure get_element_i64_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i64_i32
    !
    module procedure get_element_i64_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0_int64
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_i64(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_i64_i64
    module procedure get_element_f32_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f32_i32
    !
    module procedure get_element_f32_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0.0_real32
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_f32(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_f32_i64
    module procedure get_element_f64_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f64_i32
    !
    module procedure get_element_f64_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0.0_real64
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_f64(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_f64_i64
    module procedure get_element_bool_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_bool_i32
    !
    module procedure get_element_bool_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = .false.
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_bool(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_bool_i64
    module procedure get_element_date_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_date_i32
    !
    module procedure get_element_date_i64
        integer :: idx
        type(parquet_date) :: blank
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_date(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_date_i64
    module procedure get_element_time_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_time_i32
    !
    module procedure get_element_time_i64
        integer :: idx
        type(parquet_time) :: blank
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_time(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_time_i64
    module procedure get_element_ts_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_ts_i32
    !
    module procedure get_element_ts_i64
        integer :: idx
        type(parquet_timestamp) :: blank
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_ts(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_ts_i64
    module procedure get_element_i32v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i32v_i32
    !
    module procedure get_element_i32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_i32v(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_i32v_i64
    module procedure get_element_i64v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i64v_i32
    !
    module procedure get_element_i64v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_i64v(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_i64v_i64
    module procedure get_element_f32v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f32v_i32
    !
    module procedure get_element_f32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_f32v(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_f32v_i64
    module procedure get_element_f64v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f64v_i32
    !
    module procedure get_element_f64v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_f64v(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_f64v_i64
    module procedure get_element_boolv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_boolv_i32
    !
    module procedure get_element_boolv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_boolv(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_boolv_i64
    module procedure get_element_datev_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_datev_i32
    !
    module procedure get_element_datev_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_datev(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_datev_i64
    module procedure get_element_timev_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_timev_i32
    !
    module procedure get_element_timev_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_timev(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_timev_i64
    module procedure get_element_tsv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_tsv_i32
    !
    module procedure get_element_tsv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_tsv(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_tsv_i64
    module procedure get_element_chr_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chr_i32
    !
    module procedure get_element_chr_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = ""
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_str(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_chr_i64
    !
    module procedure get_element_chrv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chrv_i32
    !
    module procedure get_element_chrv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: value(0))
            return
        end if
        call table_require_row(self, i, "get_element")
        call col_fetch_strv(self%cache, idx, self%cache%cols(idx)%declared_kind, i, value, "get_element")
    end procedure get_element_chrv_i64
    !
    module procedure set_element_i32_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_i32_i32
    !
    module procedure set_element_i32_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_i32(self%cache, idx, i, value, "set_element")
    end procedure set_element_i32_i64
    !
    module procedure set_element_i64_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_i64_i32
    !
    module procedure set_element_i64_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_i64(self%cache, idx, i, value, "set_element")
    end procedure set_element_i64_i64
    !
    module procedure set_element_f32_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_f32_i32
    !
    module procedure set_element_f32_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_f32(self%cache, idx, i, value, "set_element")
    end procedure set_element_f32_i64
    !
    module procedure set_element_f64_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_f64_i32
    !
    module procedure set_element_f64_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_f64(self%cache, idx, i, value, "set_element")
    end procedure set_element_f64_i64
    !
    module procedure set_element_bool_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_bool_i32
    !
    module procedure set_element_bool_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_bool(self%cache, idx, i, value, "set_element")
    end procedure set_element_bool_i64
    !
    module procedure set_element_date_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_date_i32
    !
    module procedure set_element_date_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_date(self%cache, idx, i, value, "set_element")
    end procedure set_element_date_i64
    !
    module procedure set_element_time_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_time_i32
    !
    module procedure set_element_time_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_time(self%cache, idx, i, value, "set_element")
    end procedure set_element_time_i64
    !
    module procedure set_element_ts_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_ts_i32
    !
    module procedure set_element_ts_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_ts(self%cache, idx, i, value, "set_element")
    end procedure set_element_ts_i64
    !
    module procedure set_element_i32v_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_i32v_i32
    !
    module procedure set_element_i32v_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_i32v(self%cache, idx, i, value, "set_element")
    end procedure set_element_i32v_i64
    !
    module procedure set_element_i64v_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_i64v_i32
    !
    module procedure set_element_i64v_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_i64v(self%cache, idx, i, value, "set_element")
    end procedure set_element_i64v_i64
    !
    module procedure set_element_f32v_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_f32v_i32
    !
    module procedure set_element_f32v_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_f32v(self%cache, idx, i, value, "set_element")
    end procedure set_element_f32v_i64
    !
    module procedure set_element_f64v_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_f64v_i32
    !
    module procedure set_element_f64v_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_f64v(self%cache, idx, i, value, "set_element")
    end procedure set_element_f64v_i64
    !
    module procedure set_element_boolv_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_boolv_i32
    !
    module procedure set_element_boolv_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_boolv(self%cache, idx, i, value, "set_element")
    end procedure set_element_boolv_i64
    !
    module procedure set_element_datev_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_datev_i32
    !
    module procedure set_element_datev_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_datev(self%cache, idx, i, value, "set_element")
    end procedure set_element_datev_i64
    !
    module procedure set_element_timev_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_timev_i32
    !
    module procedure set_element_timev_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_timev(self%cache, idx, i, value, "set_element")
    end procedure set_element_timev_i64
    !
    module procedure set_element_tsv_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_tsv_i32
    !
    module procedure set_element_tsv_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_tsv(self%cache, idx, i, value, "set_element")
    end procedure set_element_tsv_i64
    !
    module procedure set_element_chr_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_chr_i32
    !
    module procedure set_element_chr_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_str(self%cache, idx, i, value, "set_element")
    end procedure set_element_chr_i64
    !
    module procedure set_element_chrv_i32
        call self%set_element(name, int(i, int64), value, found)
    end procedure set_element_chrv_i32
    !
    module procedure set_element_chrv_i64
        integer :: idx
        !
        ! The lookup happens BEFORE anything is written, which is what lets found=.false. mean
        ! "nothing was changed" rather than "something was changed and then a problem arose".
        call table_resolve(self, name, "set_element", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_row(self, i, "set_element")
        call col_store_strv(self%cache, idx, i, value, "set_element")
    end procedure set_element_chrv_i64
    !
    module procedure row_get_i32
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i32
    !
    module procedure row_get_i64
        integer :: idx
        integer(int32) :: v_i32
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case (PK_INT32)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, v_i32)
            value = v_i32
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i64
    !
    module procedure row_get_f32
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f32
    !
    module procedure row_get_f64
        integer :: idx
        real(real32) :: v_f32
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case (PK_FLOAT32)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, v_f32)
            value = v_f32
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f64
    !
    module procedure row_get_bool
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_bool
    !
    module procedure row_get_str
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING)
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! allow_null keeps a null row from aborting: it reads back as "", and %is_null is how a
        ! caller tells the two apart.
        call store%get(self%irow, value, allow_null=.true.)
    end procedure row_get_str
    !
    module procedure row_get_date
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_date
    !
    module procedure row_get_time
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_time
    !
    module procedure row_get_ts
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP)
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_ts
    !
    module procedure row_get_i32v
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i32v
    !
    module procedure row_get_i64v
        integer :: idx
        integer(int32), allocatable :: v_i32v(:)
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case (PK_INT32_VEC)
            allocate(v_i32v(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, v_i32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_i32v
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_i64v
    !
    module procedure row_get_f32v
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f32v
    !
    module procedure row_get_f64v
        integer :: idx
        real(real32), allocatable :: v_f32v(:)
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case (PK_FLOAT32_VEC)
            allocate(v_f32v(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, v_f32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_f32v
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_f64v
    !
    module procedure row_get_boolv
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_boolv
    !
    module procedure row_get_strv
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING_VEC)
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, i) at
        ! (i-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%copy_to(flat, value(e), allow_null=.true.)
        end do
    end procedure row_get_strv
    !
    module procedure row_get_datev
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_datev
    !
    module procedure row_get_timev
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_timev
    !
    module procedure row_get_tsv
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call parquet_column_get_at(self%cache%cols(idx)%values, self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_tsv
    !
    module procedure row_set_i32
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT32)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i32
    !
    module procedure row_set_i64
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT64)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i64
    !
    module procedure row_set_f32
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT32)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f32
    !
    module procedure row_set_f64
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT64)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f64
    !
    module procedure row_set_bool
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_LOGICAL)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_bool
    !
    module procedure row_set_str
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_STRING)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_str
    !
    module procedure row_set_date
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_DATE)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_date
    !
    module procedure row_set_time
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIME)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_time
    !
    module procedure row_set_ts
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIMESTAMP)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_ts
    !
    module procedure row_set_i32v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT32_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i32v
    !
    module procedure row_set_i64v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT64_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i64v
    !
    module procedure row_set_f32v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT32_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f32v
    !
    module procedure row_set_f64v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT64_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f64v
    !
    module procedure row_set_boolv
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_LOGICAL_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_boolv
    !
    module procedure row_set_strv
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_STRING_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_strv
    !
    module procedure row_set_datev
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_DATE_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_datev
    !
    module procedure row_set_timev
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIME_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_timev
    !
    module procedure row_set_tsv
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIMESTAMP_VEC)
        call parquet_column_set_at(self%cache%cols(idx)%values, self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_tsv
    !
    module procedure row_get_col_i32
        call row_require_col(self, c, "get")
        call col_fetch_i32(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_i32
    !
    module procedure row_get_col_i64
        call row_require_col(self, c, "get")
        call col_fetch_i64(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_i64
    !
    module procedure row_get_col_f32
        call row_require_col(self, c, "get")
        call col_fetch_f32(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_f32
    !
    module procedure row_get_col_f64
        call row_require_col(self, c, "get")
        call col_fetch_f64(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_f64
    !
    module procedure row_get_col_bool
        call row_require_col(self, c, "get")
        call col_fetch_bool(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_bool
    !
    module procedure row_get_col_str
        call row_require_col(self, c, "get")
        call col_fetch_str(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_str
    !
    module procedure row_get_col_date
        call row_require_col(self, c, "get")
        call col_fetch_date(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_date
    !
    module procedure row_get_col_time
        call row_require_col(self, c, "get")
        call col_fetch_time(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_time
    !
    module procedure row_get_col_ts
        call row_require_col(self, c, "get")
        call col_fetch_ts(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_ts
    !
    module procedure row_get_col_i32v
        call row_require_col(self, c, "get")
        call col_fetch_i32v(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_i32v
    !
    module procedure row_get_col_i64v
        call row_require_col(self, c, "get")
        call col_fetch_i64v(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_i64v
    !
    module procedure row_get_col_f32v
        call row_require_col(self, c, "get")
        call col_fetch_f32v(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_f32v
    !
    module procedure row_get_col_f64v
        call row_require_col(self, c, "get")
        call col_fetch_f64v(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_f64v
    !
    module procedure row_get_col_boolv
        call row_require_col(self, c, "get")
        call col_fetch_boolv(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_boolv
    !
    module procedure row_get_col_strv
        call row_require_col(self, c, "get")
        call col_fetch_strv(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_strv
    !
    module procedure row_get_col_datev
        call row_require_col(self, c, "get")
        call col_fetch_datev(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_datev
    !
    module procedure row_get_col_timev
        call row_require_col(self, c, "get")
        call col_fetch_timev(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_timev
    !
    module procedure row_get_col_tsv
        call row_require_col(self, c, "get")
        call col_fetch_tsv(self%cache, c%slot, c%colkind, self%irow, value, "get")
    end procedure row_get_col_tsv
    !
    module procedure row_set_col_i32
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_i32(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_i32
    !
    module procedure row_set_col_i64
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_i64(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_i64
    !
    module procedure row_set_col_f32
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_f32(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_f32
    !
    module procedure row_set_col_f64
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_f64(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_f64
    !
    module procedure row_set_col_bool
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_bool(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_bool
    !
    module procedure row_set_col_str
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_str(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_str
    !
    module procedure row_set_col_date
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_date(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_date
    !
    module procedure row_set_col_time
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_time(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_time
    !
    module procedure row_set_col_ts
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_ts(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_ts
    !
    module procedure row_set_col_i32v
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_i32v(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_i32v
    !
    module procedure row_set_col_i64v
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_i64v(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_i64v
    !
    module procedure row_set_col_f32v
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_f32v(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_f32v
    !
    module procedure row_set_col_f64v
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_f64v(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_f64v
    !
    module procedure row_set_col_boolv
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_boolv(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_boolv
    !
    module procedure row_set_col_strv
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_strv(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_strv
    !
    module procedure row_set_col_datev
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_datev(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_datev
    !
    module procedure row_set_col_timev
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_timev(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_timev
    !
    module procedure row_set_col_tsv
        call row_require_col(self, c, "set")
        call cache_check_shared_write(self%cache, c%slot, "set", nulling=.false.)
        call col_store_tsv(self%cache, c%slot, self%irow, value, "set")
    end procedure row_set_col_tsv
    !
    module procedure row_ref_i32
        integer :: idx
        integer(int32), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_INT32)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_i32
    !
    module procedure row_ref_i64
        integer :: idx
        integer(int64), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_INT64)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_i64
    !
    module procedure row_ref_f32
        integer :: idx
        real(real32), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_FLOAT32)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_f32
    !
    module procedure row_ref_f64
        integer :: idx
        real(real64), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_FLOAT64)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_f64
    !
    module procedure row_ref_bool
        integer :: idx
        logical, pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_LOGICAL)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_bool
    !
    module procedure row_ref_date
        integer :: idx
        type(parquet_date), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_DATE)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_date
    !
    module procedure row_ref_time
        integer :: idx
        type(parquet_time), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_TIME)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_time
    !
    module procedure row_ref_ts
        integer :: idx
        type(parquet_timestamp), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_TIMESTAMP)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(self%irow)
    end procedure row_ref_ts
    !
    module procedure row_ref_i32v
        integer :: idx
        integer(int32), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_INT32_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_i32v
    !
    module procedure row_ref_i64v
        integer :: idx
        integer(int64), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_INT64_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_i64v
    !
    module procedure row_ref_f32v
        integer :: idx
        real(real32), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_FLOAT32_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_f32v
    !
    module procedure row_ref_f64v
        integer :: idx
        real(real64), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_FLOAT64_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_f64v
    !
    module procedure row_ref_boolv
        integer :: idx
        logical, pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_LOGICAL_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_boolv
    !
    module procedure row_ref_datev
        integer :: idx
        type(parquet_date), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_DATE_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_datev
    !
    module procedure row_ref_timev
        integer :: idx
        type(parquet_time), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_TIME_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_timev
    !
    module procedure row_ref_tsv
        integer :: idx
        type(parquet_timestamp), pointer :: store(:,:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_TIMESTAMP_VEC)
        call parquet_column_data_ptr(self%cache%cols(idx)%values, store)
        p => store(:, self%irow)
    end procedure row_ref_tsv
    !
    module procedure get_slice_i32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i32
    !
    module procedure get_slice_i64
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        integer(int32) :: v_i32
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case (PK_INT32)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), v_i32)
                arr(k) = v_i32
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i64
    !
    module procedure get_slice_f32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f32
    !
    module procedure get_slice_f64
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        real(real32) :: v_f32
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case (PK_FLOAT32)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), v_f32)
                arr(k) = v_f32
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f64
    !
    module procedure get_slice_bool
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_bool
    !
    module procedure get_slice_date
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_date
    !
    module procedure get_slice_time
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_time
    !
    module procedure get_slice_ts
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP)
            allocate(arr(size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_ts
    !
    module procedure get_slice_i32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i32v
    !
    module procedure get_slice_i64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        integer(int32), allocatable :: v_i32v(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case (PK_INT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            allocate(v_i32v(self%cache%cols(idx)%width))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), v_i32v)
                arr(:, k) = v_i32v
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_i64v
    !
    module procedure get_slice_f32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f32v
    !
    module procedure get_slice_f64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        real(real32), allocatable :: v_f32v(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case (PK_FLOAT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            allocate(v_f32v(self%cache%cols(idx)%width))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), v_f32v)
                arr(:, k) = v_f32v
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_f64v
    !
    module procedure get_slice_boolv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_boolv
    !
    module procedure get_slice_datev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_datev
    !
    module procedure get_slice_timev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_timev
    !
    module procedure get_slice_tsv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows, kind=int64)))
            do k = 1, size(rows, kind=int64)
                call parquet_column_get_at(self%cache%cols(idx)%values, rows(k), arr(:, k))
            end do
        case default
            call slice_kind_error(self, name, idx)
        end select
    end procedure get_slice_tsv
    !
    module procedure get_slice_str
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Built element by element rather than copied and trimmed: a gather has no contiguous
        ! source range to clone from, and appending keeps the result compact.
        ! `%append_from` carries both the bytes and the null state, so the is_null fork this
        ! replaced is redundant and no per-row string is materialized.
        do k = 1, size(rows, kind=int64)
            call arr%append_from(store, rows(k))
        end do
    end procedure get_slice_str
    !
    module procedure get_slice_chr
        integer :: idx, maxlen
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Two passes: a fixed-length array's width must be the longest element SELECTED, which
        ! is not known until every selected row has been looked at.
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            if (int(store%length(rows(k))) > maxlen) maxlen = int(store%length(rows(k)))
        end do
        allocate(character(len=maxlen) :: arr(size(rows, kind=int64)))
        do k = 1, size(rows, kind=int64)
            call store%copy_to(rows(k), arr(k), allow_null=.true.)
        end do
    end procedure get_slice_chr
    !
    module procedure get_slice_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: k, flat
        integer(int64), allocatable :: rows(:)
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0,0))
            if (present(is_valid)) allocate(is_valid(0,0))
            return
        end if
        call table_require_kind(self, idx, PK_STRING_VEC, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows_elem(self%cache, idx, rows, is_valid)
        wdt = self%cache%cols(idx)%width
        call parquet_column_string_column(self%cache%cols(idx)%values, store)
        ! Measured with `%length` and filled with `%copy_to`, so neither pass allocates.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                if (int(store%length(flat)) > maxlen) maxlen = int(store%length(flat))
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, size(rows, kind=int64)))
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%copy_to(flat, arr(e, k), allow_null=.true.)
            end do
        end do
    end procedure get_slice_chrv
    !
    module procedure set_slice_i32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_i32
    !
    module procedure set_slice_i64
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_i64
    !
    module procedure set_slice_f32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_f32
    !
    module procedure set_slice_f64
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_f64
    !
    module procedure set_slice_bool
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_bool
    !
    module procedure set_slice_date
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_date
    !
    module procedure set_slice_time
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_time
    !
    module procedure set_slice_ts
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_ts
    !
    module procedure set_slice_i32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_i32v
    !
    module procedure set_slice_i64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_i64v
    !
    module procedure set_slice_f32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_f32v
    !
    module procedure set_slice_f64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_f64v
    !
    module procedure set_slice_boolv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_boolv
    !
    module procedure set_slice_datev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_datev
    !
    module procedure set_slice_timev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_timev
    !
    module procedure set_slice_tsv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_tsv
    !
    module procedure set_slice_chr
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        ! One row at a time through set_at, which the string store supports in place -- unlike
        ! %paste, which cannot overwrite a packed variable-length store's range wholesale.
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_chr
    !
    module procedure set_slice_chrv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found, writing=.true.)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        ! One row at a time through set_at, which the string store supports in place -- unlike
        ! %paste, which cannot overwrite a packed variable-length store's range wholesale.
        do k = 1, size(rows, kind=int64)
            call parquet_column_set_at(self%cache%cols(idx)%values, rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows_elem(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_chrv
    !
end submodule parquet_tables_access ! GCOVR_EXCL_LINE
