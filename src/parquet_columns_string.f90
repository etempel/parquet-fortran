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
    !!
    !! **This is the implementation; the `string_column` binding below forwards to it.** Every
    !! per-cell accessor in this file comes in that pair -- see feature_ifx.md, and the typed-tier
    !! banner in `parquet_columns.f90`, for why the body has to live at the `type` end.
    !!
    !! Note the residual this file cannot remove: `col%str` is a `parquet_string_column` and its
    !! own accessors are type-bound, so a string read still converts `type` to `class` on the way
    !! into `parquet_strings`. That descriptor is one record of 248 bytes against `parquet_column`'s
    !! twenty of 1248, and closing it would mean adding public names to a deliberately
    !! near-independent module for an allocation-dominated path. Measure before widening scope.
    module procedure parquet_column_string_column
        if (.not. is_string_kind(col%kind)) then
            error stop EP//"string_column: column kind is "//trim(kind_text(col%kind))// &
                ", but this call requires a string kind"
        end if
        if (.not. allocated(col%str)) error stop EP//"string_column: string storage is not allocated"
        p => col%str
    end procedure parquet_column_string_column
    !
    !> Aliases the embedded string store (polymorphic form).
    module procedure string_column
        call parquet_column_string_column(self, p)
    end procedure string_column
    !
    !> Reads string element `i` out of a PK_STRING column.
    module procedure parquet_column_get_at_str
        call parquet_column_check_kind(col, PK_STRING, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        call parquet_string_column_get(col%str, i, value, allow_null=.true.)
    end procedure parquet_column_get_at_str
    !
    !> Reads string element `i` out of a PK_STRING column (polymorphic form).
    module procedure get_at_str
        call parquet_column_get_at_str(self, i, value)
    end procedure get_at_str
    !
    !> Reads row `i`'s whole string vector out of a PK_STRING_VEC column.
    !!
    !! Values are blank-padded into the caller's fixed-length array; a value longer than the
    !! caller's element length is truncated by the assignment, which is the same contract the
    !! library's existing fixed-width string reads use.
    module procedure parquet_column_get_at_strv
        integer(int64) :: e, base, w
        call parquet_column_check_kind(col, PK_STRING_VEC, "get_at")
        call parquet_column_check_index(col, i, "get_at")
        w = int(col%width, int64)
        call parquet_column_check_width(col, size(value, kind=int64), "get_at")
        base = (i - 1_int64)*w
        ! `%copy_to` rather than `%get` into a temporary: `value(e)` is already a fixed-length slot,
        ! so going through an allocatable string costs a heap round trip per element for a copy that
        ! ends up blank-padded either way. Truncation semantics are identical -- see `%copy_to`.
        do e = 1_int64, w
            call parquet_string_column_copy_to(col%str, base + e, value(e), allow_null=.true.)
        end do
    end procedure parquet_column_get_at_strv
    !
    !> Reads row `i`'s whole string vector out of a PK_STRING_VEC column (polymorphic form).
    module procedure get_at_strv
        call parquet_column_get_at_strv(self, i, value)
    end procedure get_at_strv
    !
    !> Reads ONE element of row `i`'s vector out of a PK_STRING_VEC column.
    !!
    !! A vector string column is one flat store of `width * nrows` elements, element (e, row) at
    !! `(row-1)*width + e`. Reading a single one therefore needs no array and no padding, which is
    !! the whole difference from `get_at_strv` above.
    module procedure parquet_column_get_elem_strv
        call parquet_column_check_kind(col, PK_STRING_VEC, "get_elem")
        call parquet_column_check_index(col, i, "get_elem")
        call parquet_column_check_element(col, e, "get_elem")
        ! allow_null keeps a null element from aborting: it reads back as "", and %is_null is how
        ! a caller tells the two apart.
        call parquet_string_column_get(col%str, (i - 1_int64)*int(col%width, int64) + e, value, allow_null=.true.)
    end procedure parquet_column_get_elem_strv
    !
    !> Reads ONE element of row `i`'s string vector (polymorphic form).
    module procedure get_elem_strv
        call parquet_column_get_elem_strv(self, i, e, value)
    end procedure get_elem_strv
    !
    !> Writes ONE element of row `i`'s vector in a PK_STRING_VEC column.
    module procedure parquet_column_set_elem_strv
        call parquet_column_check_kind(col, PK_STRING_VEC, "set_elem")
        call parquet_column_check_index(col, i, "set_elem")
        call parquet_column_check_element(col, e, "set_elem")
        ! The store's own %set clears that element's null, exactly as it does for `set_at_str`.
        call parquet_string_column_set(col%str, (i - 1_int64)*int(col%width, int64) + e, value)
    end procedure parquet_column_set_elem_strv
    !
    !> Writes ONE element of row `i`'s string vector (polymorphic form).
    module procedure set_elem_strv
        call parquet_column_set_elem_strv(self, i, e, value)
    end procedure set_elem_strv
    !
    !> Writes string element `i` of a PK_STRING column.
    module procedure parquet_column_set_at_str
        logical :: mod_nulls
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_STRING, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        if (.not. mod_nulls) then
            if (parquet_string_column_is_null(col%str, i)) return
        end if
        call parquet_string_column_set(col%str, i, value)
    end procedure parquet_column_set_at_str
    !
    !> Writes string element `i` of a PK_STRING column (polymorphic form).
    module procedure set_at_str
        call parquet_column_set_at_str(self, i, value, modify_nulls)
    end procedure set_at_str
    !
    !> Writes row `i`'s whole string vector in a PK_STRING_VEC column. `value` is an ARRAY, so its
    !! trailing blanks are trimmed for the reason `refill_string_store` gives; `set_at_str` above
    !! takes a scalar and stores it verbatim.
    module procedure parquet_column_set_at_strv
        logical :: mod_nulls
        integer(int64) :: e, base, w
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call parquet_column_check_kind(col, PK_STRING_VEC, "set_at")
        call parquet_column_check_index(col, i, "set_at")
        call parquet_column_check_width(col, size(value, kind=int64), "set_at")
        w = int(col%width, int64)
        base = (i - 1_int64)*w
        ! modify_nulls=.false. protects individual null ELEMENTS, not the whole row: a row with
        ! one null element still has its other elements written.
        do e = 1_int64, w
            if (.not. mod_nulls) then
                if (parquet_string_column_is_null(col%str, base + e)) cycle
            end if
            call parquet_string_column_set(col%str, base + e, value(e), trim=.true.)
        end do
    end procedure parquet_column_set_at_strv
    !
    !> Writes row `i`'s whole string vector (polymorphic form).
    module procedure set_at_strv
        call parquet_column_set_at_strv(self, i, value, modify_nulls)
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
        character(len=:), allocatable :: values_c(:) !! `values`, copied when it is strided.
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING, "set_all")
        call check_nrows(self, size(values, kind=int64), "set_all")
        if (is_contiguous(values)) then
            call refill_string_store(self%str, values, self%nrows, mod_nulls)
        else
            values_c = values
            call refill_string_store(self%str, values_c, self%nrows, mod_nulls)
        end if
    end procedure set_all_str
    !
    !> Replaces every value of a PK_STRING_VEC column, from a (width, nrows) array. Trailing
    !! blanks are trimmed, as in `set_all_str`.
    module procedure set_all_strv
        logical :: mod_nulls
        character(len=:), allocatable :: values_c(:,:) !! `values`, copied when it is strided.
        mod_nulls = .true.
        if (present(modify_nulls)) mod_nulls = modify_nulls
        call check_kind(self, PK_STRING_VEC, "set_all")
        call check_width(self, size(values, 1, kind=int64), "set_all")
        call check_nrows(self, size(values, 2, kind=int64), "set_all")
        ! `values` is passed to an assumed-size dummy, so sequence association flattens it in
        ! column-major order -- which IS the store's own element-major (i-1)*width + e layout
        ! (RF6/s1), so the flat index the helper walks and the index this kind uses agree.
        if (is_contiguous(values)) then
            call refill_string_store(self%str, values, self%nrows*int(self%width, int64), mod_nulls)
        else
            values_c = values
            call refill_string_store(self%str, values_c, self%nrows*int(self%width, int64), mod_nulls)
        end if
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
    !!
    !! **A caller holding a strided `values` copies it into an allocatable first**, as `set_all_str`
    !! and `set_all_strv` do: passed here as it is, it would be copied by the compiler into an array
    !! temporary, which ifx makes on the stack, where a large array overflows it.
    subroutine refill_string_store(str, values, n, modify_nulls)
        type(parquet_string_column), intent(inout) :: str !! the store to refill, in place.
        character(len=*), intent(in) :: values(*)         !! `n` elements, in flat store order.
        integer(int64), intent(in) :: n                   !! elements to write.
        logical, intent(in) :: modify_nulls               !! .false. leaves null elements untouched.
        logical, allocatable :: keep(:)
        integer(int64) :: k
        !
        ! One call rather than a sizing loop plus `n` x `%append_string`. The bulk build does the
        ! same two passes this used to -- exact byte count, then fill -- but inside `parquet_strings`,
        ! where the payload copy is a section-to-section assignment between two `character(len=1)`
        ! arrays instead of a `transfer` with a temporary per element. Measured on 1M x
        ! `character(len=24)`: `%set_all` 65.2 ms -> 9.5 ms (feature_optimise_A7.md, S7-5). The
        ! trimming rule is unchanged and now lives in one place: `%build_from` trims, because an
        ! array's elements share a declared length (see this procedure's own doc-comment above).
        if (modify_nulls) then
            call str%build_from(values(1:n))
        else
            ! `modify_nulls = .false.` preserves a null element exactly: nulls carry no payload, so
            ! the mask is all the bulk build needs to reproduce them. Reading it costs `n` calls,
            ! but the previous shape paid `2n` of the same call on this path, so it is cheaper here
            ! too -- and it is the rare path, `.true.` being the default.
            allocate(keep(n))
            do k = 1_int64, n
                keep(k) = str%is_null(k)
            end do
            call str%build_from(values(1:n), is_null=keep)
        end if
    end subroutine refill_string_store
    !
    !> Relays a flat run of `n` elements into `parquet_string_column%append_values`. Its only job is
    !! the assumed-size dummy: a rank-2 `values` sequence-associates with it and is then passed on
    !! as the contiguous rank-1 section the bulk entry point takes, with no copy. `reshape` would
    !! do the same flattening by copying the whole array. A caller holding a strided `values`
    !! copies it first, for the reason `refill_string_store` gives.
    subroutine append_flat_strings(str, values, n)
        type(parquet_string_column), intent(inout) :: str !! the store to append to.
        character(len=*), intent(in) :: values(*)         !! `n` elements, in flat store order.
        integer(int64), intent(in) :: n                   !! elements to append.
        call parquet_string_column_append_values(str, values(1:n))
    end subroutine append_flat_strings
    !
    !> Takes over a `parquet_string_column`, making this a PK_STRING column; see the interface's
    !! own doc comment in `parquet_columns.f90` for why a string kind needs its own `adopt`.
    module procedure adopt_string_column
        call self%clear()
        self%kind = PK_STRING
        self%width = 1_int32
        self%nrows = values%size()
        self%cap = self%nrows
        allocate(self%str)
        call parquet_string_column_move_from(self%str, values)
    end procedure adopt_string_column
    !
    !> Appends rows to a PK_STRING column. `values` is an ARRAY, so trailing blanks are trimmed --
    !! the same rule `refill_string_store` states, applied here so that a column filled by
    !! `%append` and one filled by `%set_all` hold the same bytes.
    module procedure append_values_str
        integer(int64) :: n
        call check_kind(self, PK_STRING, "append_values")
        n = size(values, kind=int64)
        if (n == 0_int64) return
        ! One bulk append rather than n x %append_string, for the reason refill_string_store gives.
        call parquet_string_column_append_values(self%str, values)
        self%nrows = self%nrows + n
    end procedure append_values_str
    !
    !> Appends rows to a PK_STRING_VEC column, from a (width, n) array. Trailing blanks are
    !! trimmed, as in `append_values_str`.
    module procedure append_values_strv
        integer(int64) :: n
        character(len=:), allocatable :: values_c(:,:) !! `values`, copied when it is strided.
        call check_kind(self, PK_STRING_VEC, "append_values")
        call check_width(self, size(values, 1, kind=int64), "append_values")
        n = size(values, 2, kind=int64)
        if (n == 0_int64) return
        ! A rank-2 `values` is contiguous in column-major order, which IS the store's own
        ! element-major (i-1)*width + e layout (RF6/s1) -- the same identity `set_all_strv` relies
        ! on -- so the whole (width, n) block appends as one flat run of width*n elements.
        ! It reaches the rank-1 bulk entry point through an assumed-size relay rather than
        ! `reshape`, which would copy the entire array to produce a flattening that sequence
        ! association gives for free.
        if (is_contiguous(values)) then
            call append_flat_strings(self%str, values, size(values, kind=int64))
        else
            values_c = values
            call append_flat_strings(self%str, values_c, size(values_c, kind=int64))
        end if
        self%nrows = self%nrows + n
    end procedure append_values_strv
    !
end submodule parquet_columns_string
