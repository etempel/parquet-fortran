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
    !> Writes row `i`'s whole string vector in a PK_STRING_VEC column. `value` is an ARRAY, so its
    !! trailing blanks are trimmed for the reason `refill_string_store` gives; `set_at_str` above
    !! takes a scalar and stores it verbatim.
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
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: a row with
        ! one null element still has its other elements written.
        do e = 1_int64, w
            if (.not. mod_nulls) then
                if (self%str%is_null(base + e)) cycle
            end if
            call self%str%set(base + e, value(e), trim=.true.)
        end do
    end procedure set_at_strv
    !
    !> Replaces every value of a PK_STRING column. Trailing blanks are trimmed -- see
    !! `refill_string_store` for why an ARRAY argument trims where a scalar one does not.
    !!
    !! With the default `modify_nulls=.true.` this clears the column's null state as it goes
    !! (RF9: writing a value to a null cell makes it non-null) — the string counterpart of the
    !! bitmap kinds dropping their bitmap outright.
    module procedure set_all_str
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        call refill_string_store(self%str, values, self%nrows, mod_nulls)
    end procedure set_all_str
    !
    !> Replaces every value of a PK_STRING_VEC column, from a (width, nrows) array. Trailing
    !! blanks are trimmed, as in `set_all_str`.
    module procedure set_all_strv
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        ! `values` is passed to an assumed-size dummy, so sequence association flattens it in
        ! column-major order -- which IS the store's own element-major (i-1)*width + e layout
        ! (RF6/s1), so the flat index the helper walks and the index this kind uses agree.
        call refill_string_store(self%str, values, self%nrows*int(self%width, int64), mod_nulls)
    end procedure set_all_strv
    !
    !> Rebuilds a string store from a flat array of `n` elements, in ONE linear pass.
    !!
    !! **Why a rebuild rather than `n` calls to `%set`.** `parquet_string_column%set` shifts the
    !! payload tail and rewrites every later offset whenever an element's length changes, so it
    !! is O(n) per element -- and filling a column changes every element's length. Setting each
    !! element in turn is therefore O(n²): a 15.6M-row column did not finish in ten minutes.
    !! Appending into a fresh store instead is linear, because `ensure_offsets_cap`/
    !! `ensure_data_cap` grow geometrically. Do not "simplify" this back into a per-element loop
    !! over `%set` — no test will fail, the column will simply stop being fillable at scale.
    !!
    !! **Trailing blanks are trimmed, and only for array arguments.** Every element of a
    !! `character(len=*)` array shares one declared length, so a shorter value is blank-padded by
    !! Fortran and its trailing blanks carry no information the caller could have meant. A
    !! `character(len=*)` SCALAR is exactly as long as the caller wrote it, so `set_at_str` stores
    !! it verbatim. `parquet_string_column`'s own API also stores bytes verbatim by design and
    !! offers explicit `trim=`/`strip=`.
    !!
    !! `modify_nulls = .false.` preserves a null element exactly: nulls carry no payload (`set_null`
    !! shrinks the span to zero width), so re-appending a null reproduces it.
    subroutine refill_string_store(str, values, n, modify_nulls)
        type(parquet_string_column), intent(inout) :: str !! the store to refill, in place.
        character(len=*), intent(in) :: values(*)         !! `n` elements, in flat store order.
        integer(int64), intent(in) :: n                   !! elements to write.
        logical, intent(in) :: modify_nulls               !! .false. leaves null elements untouched.
        type(parquet_string_column) :: rebuilt
        integer(int64) :: k, nchars
        !
        ! One pass for the exact trimmed byte count, so the payload is allocated once at its final
        ! size rather than grown into. len_trim is what process_bounds' trim branch computes.
        nchars = 0_int64
        do k = 1_int64, n
            if (.not. modify_nulls) then
                if (str%is_null(k)) cycle
            end if
            nchars = nchars + int(len_trim(values(k)), int64)
        end do
        call rebuilt%reserve(n, nchars)
        do k = 1_int64, n
            if (.not. modify_nulls) then
                if (str%is_null(k)) then
                    call rebuilt%append_null()
                    cycle
                end if
            end if
            call rebuilt%append_string(values(k), trim=.true.)
        end do
        call str%move_from(rebuilt)
    end subroutine refill_string_store
    !
    !> Appends rows to a PK_STRING column. `values` is an ARRAY, so trailing blanks are trimmed --
    !! the same rule `refill_string_store` states, applied here so that a column filled by
    !! `%append` and one filled by `%set_all` hold the same bytes.
    module procedure append_values_str
        integer(int64) :: k, n
        call check_kind(self, PK_STRING, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        do k = 1_int64, n
            call self%str%append_string(values(k), trim=.true.)
        end do
        self%nrows = self%nrows + n
    end procedure append_values_str
    !
    !> Appends rows to a PK_STRING_VEC column, from a (width, n) array. Trailing blanks are
    !! trimmed, as in `append_values_str`.
    module procedure append_values_strv
        integer(int64) :: k, e, n, w
        call check_kind(self, PK_STRING_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        w = int(self%width, int64)
        do k = 1_int64, n
            do e = 1_int64, w
                call self%str%append_string(values(e, k), trim=.true.)
            end do
        end do
        self%nrows = self%nrows + n
    end procedure append_values_strv
    !
end submodule parquet_columns_string
