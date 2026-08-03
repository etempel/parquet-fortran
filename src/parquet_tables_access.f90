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
contains
    !
    module procedure col_ptr_i32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_INT32) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i32
    !
    module procedure col_ptr_i64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_INT64) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i64
    !
    module procedure col_ptr_f32
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT32) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f32
    !
    module procedure col_ptr_f64
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT64) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f64
    !
    module procedure col_ptr_bool
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_LOGICAL) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_bool
    !
    module procedure col_ptr_date
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_DATE) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_date
    !
    module procedure col_ptr_time
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_TIME) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_time
    !
    module procedure col_ptr_ts
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_TIMESTAMP) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_ts
    !
    module procedure col_ptr_i32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_INT32_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i32v
    !
    module procedure col_ptr_i64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_INT64_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_i64v
    !
    module procedure col_ptr_f32v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT32_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f32v
    !
    module procedure col_ptr_f64v
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_FLOAT64_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_f64v
    !
    module procedure col_ptr_boolv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_LOGICAL_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_boolv
    !
    module procedure col_ptr_datev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_DATE_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_datev
    !
    module procedure col_ptr_timev
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_TIME_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_timev
    !
    module procedure col_ptr_tsv
        integer :: idx
        character(len=:), allocatable :: sfx, kname
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
        if (self%cache%cols(idx)%values%kindof() /= PK_TIMESTAMP_VEC) then
            call table_context_suffix(self%cache, name, sfx)
            call parquet_kind_name(self%cache%cols(idx)%values%kindof(), kname)
            error stop EP // "col: pointer kind does not match the stored kind (" // kname // &
                "); the pointer path never widens -- use %get to copy with widening" // sfx
        end if
        call self%cache%cols(idx)%values%data_ptr(p)
    end procedure col_ptr_tsv
    !
    module procedure col_ptr_strcol
        integer :: idx
        !
        nullify(p)
        call table_resolve(self, name, "col", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "col")
        call self%cache%cols(idx)%values%string_column(p)
    end procedure col_ptr_strcol
    !
    module procedure set_arr_strcol
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "set", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set")
        call table_require_length(self, idx, arr%size(), "set")
        ! Replaces the packed store wholesale with an independent copy, so the caller's own column
        ! and the table's do not end up sharing storage. %set is a value replacement, exactly as
        ! the character-array form is; it is not a way to hand ownership over.
        call self%cache%cols(idx)%values%string_column(store)
        store = arr%clone()
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_strcol
    !
    module procedure table_column_stat_text
        min_s = "-"
        max_s = "-"
        select case (values%kindof())
        case (PK_INT32)
            call stat_i32(values, min_s, max_s)
        case (PK_INT64)
            call stat_i64(values, min_s, max_s)
        case (PK_FLOAT32)
            call stat_f32(values, min_s, max_s)
        case (PK_FLOAT64)
            call stat_f64(values, min_s, max_s)
        case (PK_LOGICAL)
            call stat_bool(values, min_s, max_s)
        case (PK_STRING)
            call stat_str(values, min_s, max_s)
        case (PK_DATE)
            call stat_date(values, min_s, max_s)
        case (PK_TIME)
            call stat_time(values, min_s, max_s)
        case (PK_TIMESTAMP)
            call stat_ts(values, min_s, max_s)
        case (PK_INT32_VEC)
            call stat_i32v(values, min_s, max_s)
        case (PK_INT64_VEC)
            call stat_i64v(values, min_s, max_s)
        case (PK_FLOAT32_VEC)
            call stat_f32v(values, min_s, max_s)
        case (PK_FLOAT64_VEC)
            call stat_f64v(values, min_s, max_s)
        case (PK_LOGICAL_VEC)
            call stat_boolv(values, min_s, max_s)
        case (PK_STRING_VEC)
            call stat_strv(values, min_s, max_s)
        case (PK_DATE_VEC)
            call stat_datev(values, min_s, max_s)
        case (PK_TIME_VEC)
            call stat_timev(values, min_s, max_s)
        case (PK_TIMESTAMP_VEC)
            call stat_tsv(values, min_s, max_s)
        case default
            ! PK_NONE, and the reserved container kinds: nothing to summarize.
            return
        end select
    end procedure table_column_stat_text
    !
    !> PK_INT32: smallest and largest value, over the rows that hold one.
    subroutine stat_i32(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int32), pointer :: p(:)
        integer(int32) :: mn, mx
        integer(int64) :: i
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                if (first) then
                    mn = p(i)
                    mx = p(i)
                    first = .false.
                else
                    mn = min(mn, p(i))
                    mx = max(mx, p(i))
                end if
        end do
        if (first) return
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i32

    !> PK_INT64: smallest and largest value, over the rows that hold one.
    subroutine stat_i64(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int64), pointer :: p(:)
        integer(int64) :: mn, mx
        integer(int64) :: i
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                if (first) then
                    mn = p(i)
                    mx = p(i)
                    first = .false.
                else
                    mn = min(mn, p(i))
                    mx = max(mx, p(i))
                end if
        end do
        if (first) return
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i64

    !> PK_FLOAT32: smallest and largest value, over the rows that hold one.
    subroutine stat_f32(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real32), pointer :: p(:)
        real(real32) :: mn, mx
        integer(int64) :: i
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                if (first) then
                    mn = p(i)
                    mx = p(i)
                    first = .false.
                else
                    mn = min(mn, p(i))
                    mx = max(mx, p(i))
                end if
        end do
        if (first) return
        write(buf, "(G0.6)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(G0.6)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_f32

    !> PK_FLOAT64: smallest and largest value, over the rows that hold one.
    subroutine stat_f64(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real64), pointer :: p(:)
        real(real64) :: mn, mx
        integer(int64) :: i
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                if (first) then
                    mn = p(i)
                    mx = p(i)
                    first = .false.
                else
                    mn = min(mn, p(i))
                    mx = max(mx, p(i))
                end if
        end do
        if (first) return
        write(buf, "(G0.6)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(G0.6)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_f64

    !> PK_LOGICAL: true/false counts rather than an ordering.
    subroutine stat_bool(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! "T:<n>".
        character(len=:), allocatable, intent(out) :: max_s    !! "F:<n>".
        logical, pointer :: p(:)
        integer(int64) :: i, nt, nf
        character(len=32) :: buf
        !
        nt = 0_int64
        nf = 0_int64
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                if (p(i)) then
                    nt = nt + 1
                else
                    nf = nf + 1
                end if
        end do
        write(buf, "(I0)") nt
        min_s = "T:" // trim(buf)
        write(buf, "(I0)") nf
        max_s = "F:" // trim(buf)
    end subroutine stat_bool

    !> PK_STRING: lexicographically smallest and largest value.
    subroutine stat_str(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        type(parquet_string_column), pointer :: store
        character(len=:), allocatable :: sv
        integer(int64) :: i, n
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%string_column(store)
        n = values%length()
        do i = 1_int64, n
            if (store%is_null(i)) cycle
            call store%get(i, sv)
            ! Trimmed for display only: a vector string column stores its values blank-padded to
            ! the widest element, and printing that padding says nothing. Fortran's own comparison
            ! blank-pads the shorter operand anyway, so trimming cannot change which value wins.
            sv = trim(sv)
            if (first) then
                min_s = sv
                max_s = sv
                first = .false.
            else
                if (sv < min_s) min_s = sv
                if (max_s < sv) max_s = sv
            end if
        end do
    end subroutine stat_str

    !> PK_DATE: earliest and latest value, in ISO-8601 form.
    subroutine stat_date(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_date), pointer :: p(:)
        type(parquet_date) :: mn, mx
        integer(int64) :: i
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
            if (p(i)%is_null()) cycle
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

    !> PK_TIME: earliest and latest value, in ISO-8601 form.
    subroutine stat_time(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_time), pointer :: p(:)
        type(parquet_time) :: mn, mx
        integer(int64) :: i
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
            if (p(i)%is_null()) cycle
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

    !> PK_TIMESTAMP: earliest and latest value, in ISO-8601 form.
    subroutine stat_ts(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_timestamp), pointer :: p(:)
        type(parquet_timestamp) :: mn, mx
        integer(int64) :: i
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
            if (p(i)%is_null()) cycle
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

    !> PK_INT32_VEC: smallest and largest value, over the rows that hold one.
    subroutine stat_i32v(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int32), pointer :: p(:,:)
        integer(int32) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (first) then
                        mn = p(e, i)
                        mx = p(e, i)
                        first = .false.
                    else
                        mn = min(mn, p(e, i))
                        mx = max(mx, p(e, i))
                    end if
                end do
        end do
        if (first) return
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i32v

    !> PK_INT64_VEC: smallest and largest value, over the rows that hold one.
    subroutine stat_i64v(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        integer(int64), pointer :: p(:,:)
        integer(int64) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (first) then
                        mn = p(e, i)
                        mx = p(e, i)
                        first = .false.
                    else
                        mn = min(mn, p(e, i))
                        mx = max(mx, p(e, i))
                    end if
                end do
        end do
        if (first) return
        write(buf, "(I0)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(I0)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_i64v

    !> PK_FLOAT32_VEC: smallest and largest value, over the rows that hold one.
    subroutine stat_f32v(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real32), pointer :: p(:,:)
        real(real32) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (first) then
                        mn = p(e, i)
                        mx = p(e, i)
                        first = .false.
                    else
                        mn = min(mn, p(e, i))
                        mx = max(mx, p(e, i))
                    end if
                end do
        end do
        if (first) return
        write(buf, "(G0.6)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(G0.6)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_f32v

    !> PK_FLOAT64_VEC: smallest and largest value, over the rows that hold one.
    subroutine stat_f64v(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        real(real64), pointer :: p(:,:)
        real(real64) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        character(len=32) :: buf
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (first) then
                        mn = p(e, i)
                        mx = p(e, i)
                        first = .false.
                    else
                        mn = min(mn, p(e, i))
                        mx = max(mx, p(e, i))
                    end if
                end do
        end do
        if (first) return
        write(buf, "(G0.6)") mn
        min_s = trim(adjustl(buf))
        write(buf, "(G0.6)") mx
        max_s = trim(adjustl(buf))
    end subroutine stat_f64v

    !> PK_LOGICAL_VEC: true/false counts rather than an ordering.
    subroutine stat_boolv(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! "T:<n>".
        character(len=:), allocatable, intent(out) :: max_s    !! "F:<n>".
        logical, pointer :: p(:,:)
        integer(int64) :: i, nt, nf
        integer :: e
        character(len=32) :: buf
        !
        nt = 0_int64
        nf = 0_int64
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (p(e, i)) then
                        nt = nt + 1
                    else
                        nf = nf + 1
                    end if
                end do
        end do
        write(buf, "(I0)") nt
        min_s = "T:" // trim(buf)
        write(buf, "(I0)") nf
        max_s = "F:" // trim(buf)
    end subroutine stat_boolv

    !> PK_STRING_VEC: lexicographically smallest and largest value.
    subroutine stat_strv(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! smallest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! largest value, or "-".
        type(parquet_string_column), pointer :: store
        character(len=:), allocatable :: sv
        integer(int64) :: i, n
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%string_column(store)
        n = values%length() * int(values%colwidth(), int64)
        do i = 1_int64, n
            if (store%is_null(i)) cycle
            call store%get(i, sv)
            ! Trimmed for display only: a vector string column stores its values blank-padded to
            ! the widest element, and printing that padding says nothing. Fortran's own comparison
            ! blank-pads the shorter operand anyway, so trimming cannot change which value wins.
            sv = trim(sv)
            if (first) then
                min_s = sv
                max_s = sv
                first = .false.
            else
                if (sv < min_s) min_s = sv
                if (max_s < sv) max_s = sv
            end if
        end do
    end subroutine stat_strv

    !> PK_DATE_VEC: earliest and latest value, in ISO-8601 form.
    subroutine stat_datev(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_date), pointer :: p(:,:)
        type(parquet_date) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (p(e, i)%is_null()) cycle
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

    !> PK_TIME_VEC: earliest and latest value, in ISO-8601 form.
    subroutine stat_timev(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_time), pointer :: p(:,:)
        type(parquet_time) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (p(e, i)%is_null()) cycle
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

    !> PK_TIMESTAMP_VEC: earliest and latest value, in ISO-8601 form.
    subroutine stat_tsv(values, min_s, max_s)
        type(parquet_column), intent(in) :: values             !! the column.
        character(len=:), allocatable, intent(out) :: min_s    !! earliest value, or "-".
        character(len=:), allocatable, intent(out) :: max_s    !! latest value, or "-".
        type(parquet_timestamp), pointer :: p(:,:)
        type(parquet_timestamp) :: mn, mx
        integer(int64) :: i
        integer :: e
        logical :: first
        !
        min_s = "-"
        max_s = "-"
        first = .true.
        call values%data_ptr(p)
        do i = 1_int64, values%length()
            if (values%is_null(i)) cycle
                do e = 1, size(p, 1)
                    if (p(e, i)%is_null()) cycle
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case (PK_INT32)
            call self%cache%cols(idx)%values%data_ptr(p_i32)
            allocate(arr(size(p_i32)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
            arr = p
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%data_ptr(p_f32)
            allocate(arr(size(p_f32)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
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
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_INT64_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case (PK_INT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p_i32v)
            allocate(arr(size(p_i32v,1), size(p_i32v,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_FLOAT64_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
            arr = p
        case (PK_FLOAT32_VEC)
            call self%cache%cols(idx)%values%data_ptr(p_f32v)
            allocate(arr(size(p_f32v,1), size(p_f32v,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_LOGICAL_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_DATE_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIME_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        select case (self%cache%cols(idx)%values%kindof())
        case (PK_TIMESTAMP_VEC)
            call self%cache%cols(idx)%values%data_ptr(p)
            allocate(arr(size(p,1), size(p,2)))
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
        call self%cache%cols(idx)%values%string_column(src)
        arr = src%clone()
    end procedure get_arr_str
    !
    module procedure get_arr_chr
        integer :: idx, maxlen
        integer(int64) :: i, n
        character(len=:), allocatable :: s
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
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do i = 1, n
            call store%get(i, s, allow_null=.true.)
            if (len(s) > maxlen) maxlen = len(s)
        end do
        allocate(character(len=maxlen) :: arr(n))
        do i = 1, n
            call store%get(i, s, allow_null=.true.)
            arr(i) = s
        end do
    end procedure get_arr_chr
    !
    module procedure get_arr_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: i, n, flat
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get", idx, found)
        if (idx == 0) then
            allocate(character(len=1) :: arr(0,0))
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        if (present(is_valid)) call table_valid_mask_of(self%cache, idx, is_valid)
        call table_require_kind(self, idx, PK_STRING_VEC, "get")
        n = self%cache%cols(idx)%values%length()
        wdt = self%cache%cols(idx)%values%colwidth()
        ! A vector string column is ONE flat string store of width*nrows elements, element
        ! (e, i) living at (i-1)*width + e -- reaching it directly is what lets each element
        ! come back as an allocatable string, which the two-pass width measurement needs.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, s, allow_null=.true.)
                if (len(s) > maxlen) maxlen = len(s)
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, n))
        do i = 1, n
            do e = 1, wdt
                flat = (i - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, s, allow_null=.true.)
                arr(e, i) = s
            end do
        end do
    end procedure get_arr_chrv
    !
    module procedure set_arr_i32
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_INT32_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i32v
    !
    module procedure set_arr_i64v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_INT64_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_i64v
    !
    module procedure set_arr_f32v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f32v
    !
    module procedure set_arr_f64v
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_f64v
    !
    module procedure set_arr_boolv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_boolv
    !
    module procedure set_arr_datev
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_DATE_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_datev
    !
    module procedure set_arr_timev
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_TIME_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_timev
    !
    module procedure set_arr_tsv
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_arr_tsv
    !
    module procedure set_arr_chr
        integer :: idx
        !
        call table_resolve(self, name, "set", idx)
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
        call table_resolve(self, name, "set", idx)
        call table_require_kind(self, idx, PK_STRING_VEC, "set")
        call table_require_length(self, idx, size(arr, 2, kind=int64), "set")
        call self%cache%cols(idx)%values%set_all(arr, modify_nulls)
        ! Applied AFTER the values, because a whole-column %set drops the null bitmap by default:
        ! marking the nulls first would leave nothing behind.
        if (present(is_valid)) call table_apply_valid(self, idx, is_valid, name, "set")
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
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_INT32, "get_element")
        end select
    end procedure get_element_i32_i64
    !
    module procedure get_element_i64_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i64_i32
    !
    module procedure get_element_i64_i64
        integer :: idx
        integer(int32) :: v_i32
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0_int64
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64)
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(i, v_i32)
            value = v_i32
        case default
            call table_require_kind(self, idx, PK_INT64, "get_element")
        end select
    end procedure get_element_i64_i64
    !
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
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_FLOAT32, "get_element")
        end select
    end procedure get_element_f32_i64
    !
    module procedure get_element_f64_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f64_i32
    !
    module procedure get_element_f64_i64
        integer :: idx
        real(real32) :: v_f32
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = 0.0_real64
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64)
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(i, v_f32)
            value = v_f32
        case default
            call table_require_kind(self, idx, PK_FLOAT64, "get_element")
        end select
    end procedure get_element_f64_i64
    !
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
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_LOGICAL, "get_element")
        end select
    end procedure get_element_bool_i64
    !
    module procedure get_element_date_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_date_i32
    !
    module procedure get_element_date_i64
        integer :: idx
        !
        type(parquet_date) :: blank
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_DATE, "get_element")
        end select
    end procedure get_element_date_i64
    !
    module procedure get_element_time_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_time_i32
    !
    module procedure get_element_time_i64
        integer :: idx
        !
        type(parquet_time) :: blank
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIME, "get_element")
        end select
    end procedure get_element_time_i64
    !
    module procedure get_element_ts_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_ts_i32
    !
    module procedure get_element_ts_i64
        integer :: idx
        !
        type(parquet_timestamp) :: blank
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            value = blank
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP)
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIMESTAMP, "get_element")
        end select
    end procedure get_element_ts_i64
    !
    module procedure get_element_i32v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i32v_i32
    !
    module procedure get_element_i32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_INT32_VEC, "get_element")
        end select
    end procedure get_element_i32v_i64
    !
    module procedure get_element_i64v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_i64v_i32
    !
    module procedure get_element_i64v_i64
        integer :: idx
        integer(int32), allocatable :: v_i32v(:)
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_INT32_VEC)
            allocate(v_i32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, v_i32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_i32v
        case default
            call table_require_kind(self, idx, PK_INT64_VEC, "get_element")
        end select
    end procedure get_element_i64v_i64
    !
    module procedure get_element_f32v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f32v_i32
    !
    module procedure get_element_f32v_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_FLOAT32_VEC, "get_element")
        end select
    end procedure get_element_f32v_i64
    !
    module procedure get_element_f64v_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_f64v_i32
    !
    module procedure get_element_f64v_i64
        integer :: idx
        real(real32), allocatable :: v_f32v(:)
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case (PK_FLOAT32_VEC)
            allocate(v_f32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, v_f32v)
            allocate(value(self%cache%cols(idx)%width))
            value = v_f32v
        case default
            call table_require_kind(self, idx, PK_FLOAT64_VEC, "get_element")
        end select
    end procedure get_element_f64v_i64
    !
    module procedure get_element_boolv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_boolv_i32
    !
    module procedure get_element_boolv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_LOGICAL_VEC, "get_element")
        end select
    end procedure get_element_boolv_i64
    !
    module procedure get_element_datev_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_datev_i32
    !
    module procedure get_element_datev_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_DATE_VEC, "get_element")
        end select
    end procedure get_element_datev_i64
    !
    module procedure get_element_timev_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_timev_i32
    !
    module procedure get_element_timev_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIME_VEC, "get_element")
        end select
    end procedure get_element_timev_i64
    !
    module procedure get_element_tsv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_tsv_i32
    !
    module procedure get_element_tsv_i64
        integer :: idx
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) then
            ! `value` stays unallocated, which is how %get reports a miss too.
            return
        end if
        call table_require_row(self, i, "get_element")
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(value(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(i, value)
        case default
            call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "get_element")
        end select
    end procedure get_element_tsv_i64
    !
    module procedure get_element_chr_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chr_i32
    !
    module procedure get_element_chr_i64
        integer :: idx
        type(parquet_string_column), pointer :: store
        !
        value = ""
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_element")
        call table_require_row(self, i, "get_element")
        call self%cache%cols(idx)%values%string_column(store)
        ! allow_null keeps a null row from aborting: it reads back as "", and %is_null is how a
        ! caller tells the two apart -- the same rule the row handle's %get follows.
        call store%get(i, value, allow_null=.true.)
    end procedure get_element_chr_i64
    !
    module procedure get_element_chrv_i32
        call self%get_element(name, int(i, int64), value, found)
    end procedure get_element_chrv_i32
    !
    module procedure get_element_chrv_i64
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        character(len=:), allocatable :: str1
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "get_element")
        call table_require_row(self, i, "get_element")
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, row) at
        ! (row-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%get(flat, str1, allow_null=.true.)
            if (len(str1) > maxlen) maxlen = len(str1)
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (i - 1_int64) * int(wdt, int64) + int(e, int64)
            call store%get(flat, str1, allow_null=.true.)
            value(e) = str1
        end do
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
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
        call table_resolve(self, name, "set_element", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "set_element")
        call table_require_row(self, i, "set_element")
        call self%cache%cols(idx)%values%set_at(i, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_element_chrv_i64
    !
    module procedure row_get_i32
        integer :: idx
        !
        call row_resolve(self, name, "get", idx)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_INT32)
            call self%cache%cols(idx)%values%get_at(self%irow, v_i32)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_FLOAT32)
            call self%cache%cols(idx)%values%get_at(self%irow, v_f32)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
        call self%cache%cols(idx)%values%string_column(store)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_INT32_VEC)
            allocate(v_i32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, v_i32v)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case (PK_FLOAT32_VEC)
            allocate(v_f32v(self%cache%cols(idx)%width))
            call self%cache%cols(idx)%values%get_at(self%irow, v_f32v)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
        case default
            call row_kind_error(self, name, idx)
        end select
    end procedure row_get_boolv
    !
    module procedure row_get_strv
        integer :: idx, e, wdt, maxlen
        integer(int64) :: flat
        character(len=:), allocatable :: s
        type(parquet_string_column), pointer :: store
        !
        call row_resolve(self, name, "get", idx)
        call row_require_kind(self, name, idx, PK_STRING_VEC)
        wdt = self%cache%cols(idx)%width
        ! A vector string column is ONE flat store of width*nrows elements, element (e, i) at
        ! (i-1)*width + e. Two passes, because a fixed-length array cannot be grown per element.
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%get(flat, s, allow_null=.true.)
            if (len(s) > maxlen) maxlen = len(s)
        end do
        allocate(character(len=maxlen) :: value(wdt))
        do e = 1, wdt
            flat = (self%irow - 1) * int(wdt, int64) + int(e, int64)
            call store%get(flat, s, allow_null=.true.)
            value(e) = s
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
            call self%cache%cols(idx)%values%get_at(self%irow, value)
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
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i32
    !
    module procedure row_set_i64
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT64)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i64
    !
    module procedure row_set_f32
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT32)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f32
    !
    module procedure row_set_f64
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT64)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f64
    !
    module procedure row_set_bool
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_LOGICAL)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_bool
    !
    module procedure row_set_str
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_STRING)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_str
    !
    module procedure row_set_date
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_DATE)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_date
    !
    module procedure row_set_time
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIME)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_time
    !
    module procedure row_set_ts
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIMESTAMP)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_ts
    !
    module procedure row_set_i32v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT32_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i32v
    !
    module procedure row_set_i64v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_INT64_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_i64v
    !
    module procedure row_set_f32v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT32_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f32v
    !
    module procedure row_set_f64v
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_FLOAT64_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_f64v
    !
    module procedure row_set_boolv
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_LOGICAL_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_boolv
    !
    module procedure row_set_strv
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_STRING_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_strv
    !
    module procedure row_set_datev
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_DATE_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_datev
    !
    module procedure row_set_timev
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIME_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_timev
    !
    module procedure row_set_tsv
        integer :: idx
        !
        call row_resolve(self, name, "set", idx)
        call row_require_kind(self, name, idx, PK_TIMESTAMP_VEC)
        call self%cache%cols(idx)%values%set_at(self%irow, value)
        self%cache%cols(idx)%user_populated = .true.
    end procedure row_set_tsv
    !
    module procedure row_ref_i32
        integer :: idx
        integer(int32), pointer :: store(:)
        !
        nullify(p)
        call row_resolve(self, name, "ref", idx)
        call row_require_kind(self, name, idx, PK_INT32)
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
        call self%cache%cols(idx)%values%data_ptr(store)
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case (PK_INT32)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_i32)
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
            end do
        case (PK_FLOAT32)
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_f32)
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
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
            allocate(arr(size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(k))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_INT64_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case (PK_INT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            allocate(v_i32v(self%cache%cols(idx)%width))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_i32v)
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_FLOAT64_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
            end do
        case (PK_FLOAT32_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            allocate(v_f32v(self%cache%cols(idx)%width))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), v_f32v)
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_LOGICAL_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_DATE_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIME_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
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
            if (present(is_valid)) allocate(is_valid(0))
            return
        end if
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        select case (self%cache%cols(idx)%declared_kind)
        case (PK_TIMESTAMP_VEC)
            allocate(arr(self%cache%cols(idx)%width, size(rows)))
            do k = 1, size(rows, kind=int64)
                call self%cache%cols(idx)%values%get_at(rows(k), arr(:, k))
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
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call self%cache%cols(idx)%values%string_column(store)
        ! Built element by element rather than copied and trimmed: a gather has no contiguous
        ! source range to clone from, and appending keeps the result compact.
        do k = 1, size(rows, kind=int64)
            if (store%is_null(rows(k))) then
                call arr%append_null()
            else
                call store%get(rows(k), sv)
                call arr%append_string(sv)
            end if
        end do
    end procedure get_slice_str
    !
    module procedure get_slice_chr
        integer :: idx, maxlen
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        call self%cache%cols(idx)%values%string_column(store)
        ! Two passes: a fixed-length array's width must be the longest element SELECTED, which
        ! is not known until every selected row has been looked at.
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            call store%get(rows(k), sv, allow_null=.true.)
            if (len(sv) > maxlen) maxlen = len(sv)
        end do
        allocate(character(len=maxlen) :: arr(size(rows)))
        do k = 1, size(rows, kind=int64)
            call store%get(rows(k), sv, allow_null=.true.)
            arr(k) = sv
        end do
    end procedure get_slice_chr
    !
    module procedure get_slice_chrv
        integer :: idx, maxlen, e, wdt
        integer(int64) :: k, flat
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: sv
        type(parquet_string_column), pointer :: store
        !
        call table_resolve(self, name, "get_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "get_slice")
        call slice_resolve(s, self%row_count, rows, "get_slice")
        if (present(is_valid)) call table_valid_mask_rows(self%cache, idx, rows, is_valid)
        wdt = self%cache%cols(idx)%width
        call self%cache%cols(idx)%values%string_column(store)
        maxlen = 1
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, sv, allow_null=.true.)
                if (len(sv) > maxlen) maxlen = len(sv)
            end do
        end do
        allocate(character(len=maxlen) :: arr(wdt, size(rows)))
        do k = 1, size(rows, kind=int64)
            do e = 1, wdt
                flat = (rows(k) - 1) * int(wdt, int64) + int(e, int64)
                call store%get(flat, sv, allow_null=.true.)
                arr(e, k) = sv
            end do
        end do
    end procedure get_slice_chrv
    !
    module procedure set_slice_i32
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT32_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_i32v
    !
    module procedure set_slice_i64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_INT64_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_i64v
    !
    module procedure set_slice_f32v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT32_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_f32v
    !
    module procedure set_slice_f64v
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_FLOAT64_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_f64v
    !
    module procedure set_slice_boolv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_LOGICAL_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_boolv
    !
    module procedure set_slice_datev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_DATE_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_datev
    !
    module procedure set_slice_timev
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIME_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_timev
    !
    module procedure set_slice_tsv
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_TIMESTAMP_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_tsv
    !
    module procedure set_slice_chr
        integer :: idx
        integer(int64) :: k
        integer(int64), allocatable :: rows(:)
        !
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, kind=int64), size(rows, kind=int64), name, "set_slice")
        ! One row at a time through set_at, which the string store supports in place -- unlike
        ! %paste, which cannot overwrite a packed variable-length store's range wholesale.
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(k), modify_nulls)
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
        call table_resolve(self, name, "set_slice", idx, found)
        if (idx == 0) return
        call table_require_kind(self, idx, PK_STRING_VEC, "set_slice")
        call slice_resolve(s, self%row_count, rows, "set_slice")
        call table_require_slice_size(self, size(arr, 2, kind=int64), size(rows, kind=int64), name, "set_slice")
        ! One row at a time through set_at, which the string store supports in place -- unlike
        ! %paste, which cannot overwrite a packed variable-length store's range wholesale.
        do k = 1, size(rows, kind=int64)
            call self%cache%cols(idx)%values%set_at(rows(k), arr(:, k), modify_nulls)
        end do
        if (present(is_valid)) call table_apply_valid_rows(self, idx, rows, is_valid, name, "set_slice")
        self%cache%cols(idx)%user_populated = .true.
    end procedure set_slice_chrv
    !
end submodule parquet_tables_access
