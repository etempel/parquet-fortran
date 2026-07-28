!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The string kinds of `parquet_column` (`PK_STRING`, `PK_STRING_VEC`), which are the one place
!! the type-erased storage is not a Fortran array.
!!
!! Per DD1 a string column **embeds a `parquet_string_column`** — already a self-contained
!! offsets + payload + validity store — and delegates to it rather than duplicating variable-
!! length storage or hoisting string validity into the column bitmap. That is why these
!! procedures are hand-written while every array kind's equivalents are generated: they are
!! delegation, not array indexing.
!!
!! `PK_STRING_VEC` uses **one** string store for the whole column, flat-indexed
!! `(i-1)*width + e` (RF6/s1). One store rather than `width` stores means a single set of
!! offsets and one payload buffer, and — because the layout is element-major within a row — one
!! row's whole vector stays contiguous, which is what row access and append-row want.
!!
!! There is deliberately **no `data_ptr`** for the string kinds: their storage is not a plain
!! Fortran array, so there is nothing kind-matched to alias. Use `string_column` to reach the
!! embedded store (and its zero-copy `view`/`view_slice` handles) instead.
submodule (parquet_columns) parquet_columns_string
    implicit none
contains
    !
    !> Aliases the embedded string store, so callers can use `parquet_string_column`'s own API
    !! (handles, searching, statistics) on a string column's values.
    module procedure string_column
        if (.not. is_string_kind(self%kind)) then
            error stop EP//"string_column: column kind is "//trim(kind_text(self%kind))// &
                ", but this call requires a string kind"
        end if
        if (.not. allocated(self%str)) error stop EP//"string_column: string storage is not allocated"
        p => self%str
    end procedure string_column
    !
    !> Reads string element `i` out of a PK_STRING column.
    module procedure get_at_str
        call check_kind(self, PK_STRING, "get_at")
        call check_index(self, i, "get_at")
        call self%str%get(i, value, allow_null=.true.)
    end procedure get_at_str
    !
    !> Reads row `i`'s whole string vector out of a PK_STRING_VEC column.
    !!
    !! Values are blank-padded into the caller's fixed-length array; a value longer than the
    !! caller's element length is truncated by the assignment, which is the same contract the
    !! library's existing fixed-width string reads use.
    module procedure get_at_strv
        integer(int64) :: e, base, w
        character(len=:), allocatable :: s
        call check_kind(self, PK_STRING_VEC, "get_at")
        call check_index(self, i, "get_at")
        w = int(self%width, int64)
        call check_width(self, size(value, kind=int64), "get_at")
        base = (i - 1_int64)*w
        do e = 1_int64, w
            call self%str%get(base + e, s, allow_null=.true.)
            value(e) = s
        end do
    end procedure get_at_strv
    !
    !> Writes string element `i` of a PK_STRING column.
    module procedure set_at_str
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING, "set_at")
        call check_index(self, i, "set_at")
        if (.not. mod_nulls) then
            if (self%str%is_null(i)) return
        end if
        call self%str%set(i, value)
    end procedure set_at_str
    !
    !> Writes row `i`'s whole string vector in a PK_STRING_VEC column.
    module procedure set_at_strv
        logical :: mod_nulls
        integer(int64) :: e, base, w
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING_VEC, "set_at")
        call check_index(self, i, "set_at")
        call check_width(self, size(value, kind=int64), "set_at")
        w = int(self%width, int64)
        base = (i - 1_int64)*w
        if (.not. mod_nulls) then
            if (self%str%is_null(base + 1_int64)) return
        end if
        do e = 1_int64, w
            call self%str%set(base + e, value(e))
        end do
    end procedure set_at_strv
    !
    !> Replaces every value of a PK_STRING column.
    !!
    !! With the default `modify_nulls=.true.` this clears the column's null state as it goes
    !! (RF9: writing a value to a null cell makes it non-null) — the string counterpart of the
    !! bitmap kinds dropping their bitmap outright.
    module procedure set_all_str
        logical :: mod_nulls
        integer(int64) :: k
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        do k = 1_int64, self%nrows
            if (.not. mod_nulls) then
                if (self%str%is_null(k)) cycle
            end if
            call self%str%set(k, values(k))
        end do
    end procedure set_all_str
    !
    !> Replaces every value of a PK_STRING_VEC column, from a (width, nrows) array.
    module procedure set_all_strv
        logical :: mod_nulls
        integer(int64) :: k, e, base, w
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        w = int(self%width, int64)
        do k = 1_int64, self%nrows
            base = (k - 1_int64)*w
            if (.not. mod_nulls) then
                if (self%str%is_null(base + 1_int64)) cycle
            end if
            do e = 1_int64, w
                call self%str%set(base + e, values(e, k))
            end do
        end do
    end procedure set_all_strv
    !
    !> Appends rows to a PK_STRING column.
    module procedure append_values_str
        integer(int64) :: k, n
        call check_kind(self, PK_STRING, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        do k = 1_int64, n
            call self%str%append_string(values(k))
        end do
        self%nrows = self%nrows + n
    end procedure append_values_str
    !
    !> Appends rows to a PK_STRING_VEC column, from a (width, n) array.
    module procedure append_values_strv
        integer(int64) :: k, e, n, w
        call check_kind(self, PK_STRING_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        w = int(self%width, int64)
        do k = 1_int64, n
            do e = 1_int64, w
                call self%str%append_string(values(e, k))
            end do
        end do
        self%nrows = self%nrows + n
    end procedure append_values_strv
    !
end submodule parquet_columns_string
