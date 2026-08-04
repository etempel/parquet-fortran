!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> Turns each supported element type into the canonical key form the C++ engine takes, and
!! implements `pf_sort_keys`.
!!
!! **This file decides nothing about order.** It extracts values, says which rows are null, and
!! passes the caller's `descending`/`nulls_first` flags through -- every ordering decision is made
!! in one place (`sort_compare_key`, `src/parquet_wrapper.cpp`), which is what stops a raw-array
!! sort, a table sort and a read-time `sort_by=` from ever disagreeing.
!!
!! The canonical form is deliberately narrow: an integer key, a real key, or a packed
!! (offsets, data) string key, each with an optional per-row validity array. Everything else
!! reduces to one of those three -- a `logical` and every temporal kind order exactly as their
!! stored integers do, and a `parquet_timestamp` becomes two integer keys rather than one.
submodule (parquet_sorting) parquet_sorting_keys
    implicit none
    !
contains
    !
    module procedure extract_i32
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            buf(1)%ints(k) = int(values(k), int64)
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_i32
    !
    module procedure extract_i64
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            buf(1)%ints(k) = values(k)
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_i64
    !
    module procedure extract_f32
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        buf(1)%reals = 0.0_real64
        do k = 1_int64, n
            buf(1)%reals(k) = real(values(k), real64)
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_f32
    !
    module procedure extract_f64
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        buf(1)%reals = 0.0_real64
        do k = 1_int64, n
            buf(1)%reals(k) = values(k)
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_f64
    !
    module procedure extract_bool
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            buf(1)%ints(k) = merge(1_int64, 0_int64, values(k))
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_bool
    !
    module procedure extract_chr
        integer(int64) :: k, n, total, pos, j, ln
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        ! Sorted on the FULL declared length, trailing blanks included, which is exactly
        ! Fortran's own `<` for equal-length strings -- so pf_is_sorted agrees with a
        ! hand-written a(k) <= a(k+1) loop rather than quietly trimming behind it.
        ln = int(len(values), int64)
        allocate(buf(1)%offsets(n + 1_int64))
        total = 0_int64
        do k = 1_int64, n + 1_int64
            buf(1)%offsets(k) = total
            total = total + ln
        end do
        buf(1)%offsets(n + 1_int64) = n * ln
        allocate(buf(1)%data(max(n * ln, 1_int64)))
        pos = 0_int64
        do k = 1_int64, n
            do j = 1_int64, ln
                buf(1)%data(pos + j) = values(k)(j:j)
            end do
            pos = pos + ln
        end do
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_chr
    !
    module procedure extract_date
        integer(int64) :: k, n
        logical, allocatable :: mask(:)
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            buf(1)%ints(k) = int(values(k)%raw(), int64)
        end do
        allocate(mask(max(n, 1_int64)))
        mask = .true.
        do k = 1_int64, n
            mask(k) = .not. values(k)%is_null()
        end do
        call valid_from_mask(mask, n, proc, buf(1)%valid)
    end procedure extract_date
    !
    module procedure extract_time
        integer(int64) :: k, n
        logical, allocatable :: mask(:)
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            buf(1)%ints(k) = values(k)%raw()
        end do
        allocate(mask(max(n, 1_int64)))
        mask = .true.
        do k = 1_int64, n
            mask(k) = .not. values(k)%is_null()
        end do
        call valid_from_mask(mask, n, proc, buf(1)%valid)
    end procedure extract_time
    !
    module procedure extract_ts
        integer(int64) :: k, n, s
        integer(int32) :: ns
        logical, allocatable :: mask(:)
        !
        n = size(values, kind=int64)
        ! A timestamp is TWO integer keys: folding (seconds, nanoseconds) into one int64
        ! as s*10**9 + ns overflows outside roughly 1678-2262, well inside the range this
        ! library handles. Seconds lead, nanoseconds break their ties, and both carry the
        ! same validity so the pair can never disagree about which rows are null.
        allocate(buf(2))
        buf(:)%family = SK_INT
        buf(:)%descending = descending
        buf(:)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        buf(2)%ints = 0_int64
        allocate(mask(max(n, 1_int64)))
        mask = .true.
        do k = 1_int64, n
            call values(k)%get_raw(s, ns)
            buf(1)%ints(k) = s
            buf(2)%ints(k) = int(ns, int64)
            mask(k) = .not. values(k)%is_null()
        end do
        call valid_from_mask(mask, n, proc, buf(1)%valid)
        if (allocated(buf(1)%valid)) buf(2)%valid = buf(1)%valid
    end procedure extract_ts
    !
    module procedure extract_strcol
        integer(int64) :: k, n, total, pos, j, ln
        character(len=:), allocatable :: s
        logical :: any_null
        !
        n = values%size()
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%offsets(n + 1_int64))
        buf(1)%offsets(1) = 0_int64
        total = 0_int64
        do k = 1_int64, n
            total = total + values%length(k)
            buf(1)%offsets(k + 1_int64) = total
        end do
        allocate(buf(1)%data(max(total, 1_int64)))
        pos = 0_int64
        any_null = .false.
        do k = 1_int64, n
            if (values%is_null(k)) any_null = .true.
            call values%get(k, s, allow_null=.true.)
            ln = int(len(s), int64)
            do j = 1_int64, ln
                buf(1)%data(pos + j) = s(j:j)
            end do
            pos = pos + ln
        end do
        if (any_null) then
            allocate(buf(1)%valid(max(n, 1_int64)))
            buf(1)%valid = 1_c_int8_t
            do k = 1_int64, n
                if (values%is_null(k)) buf(1)%valid(k) = 0_c_int8_t
            end do
        end if
    end procedure extract_strcol
    !
    module procedure extract_col
        integer(int64) :: n
        integer :: kind
        !
        n = values%length()
        ! One column, whose element type is only known at runtime -- so this reduces to
        ! whichever of the four extractors above matches, and a PK_TIMESTAMP column
        ! produces two keys exactly as the bare type does.
        kind = values%kindof()
        if (values%colwidth() /= 1) then
            error stop EP // proc // ": a vector column cannot be a sort key; there is no " // &
                "defined order on a whole vector row"
        end if
        select case (kind)
        case (PK_INT32, PK_INT64, PK_LOGICAL, PK_DATE, PK_TIME)
            call extract_col_integer(values, n, descending, nulls_first, buf)
        case (PK_FLOAT32, PK_FLOAT64)
            call extract_col_real(values, n, descending, nulls_first, buf)
        case (PK_STRING)
            call extract_col_string(values, n, descending, nulls_first, buf)
        case (PK_TIMESTAMP)
            call extract_col_timestamp(values, n, descending, nulls_first, buf)
        case default
            block
                character(len=:), allocatable :: kname
                call parquet_kind_name(kind, kname)
                error stop EP // proc // ": a " // kname // " column cannot be a sort key"
            end block
        end select
        call col_valid_flags(values, n, buf)
    end procedure extract_col
    !
    !> Reads every integer-valued scalar kind of a column as int64 -- including logical and the
    !! two date/time kinds, whose stored values order exactly as the values they represent.
    subroutine extract_col_integer(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int64) :: k
        integer(int32) :: v32
        logical :: b
        type(parquet_date) :: d
        type(parquet_time) :: tm
        !
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        do k = 1_int64, n
            select case (col%kindof())
            case (PK_INT32)
                call col%get_at(k, v32)
                buf(1)%ints(k) = int(v32, int64)
            case (PK_INT64)
                call col%get_at(k, buf(1)%ints(k))
            case (PK_LOGICAL)
                call col%get_at(k, b)
                buf(1)%ints(k) = merge(1_int64, 0_int64, b)
            case (PK_DATE)
                call col%get_at(k, d)
                buf(1)%ints(k) = int(d%raw(), int64)
            case default
                call col%get_at(k, tm)
                buf(1)%ints(k) = tm%raw()
            end select
        end do
    end subroutine extract_col_integer
    !
    !> Reads a float32 or float64 column as real64. NaNs pass straight through: the engine tiers
    !! them itself, exactly as it does for a read-time sort.
    subroutine extract_col_real(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        integer(int64) :: k
        real(real32) :: r32
        !
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        buf(1)%reals = 0.0_real64
        do k = 1_int64, n
            if (col%kindof() == PK_FLOAT32) then
                call col%get_at(k, r32)
                buf(1)%reals(k) = real(r32, real64)
            else
                call col%get_at(k, buf(1)%reals(k))
            end if
        end do
    end subroutine extract_col_real
    !
    !> Packs a string column into the (offsets, data) pair the engine takes: row k occupies
    !! `data(offsets(k)+1 : offsets(k+1))`, with `offsets` 0-based because the C++ side indexes
    !! with it directly.
    subroutine extract_col_string(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives one key.
        character(len=:), allocatable :: s
        integer(int64) :: k, total, pos, j
        !
        allocate(buf(1))
        buf(1)%family = SK_STR
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%offsets(n + 1_int64))
        buf(1)%offsets(1) = 0_int64
        total = 0_int64
        do k = 1_int64, n
            call col%get_at(k, s)
            total = total + int(len(s), int64)
            buf(1)%offsets(k + 1_int64) = total
        end do
        allocate(buf(1)%data(max(total, 1_int64)))
        pos = 0_int64
        do k = 1_int64, n
            call col%get_at(k, s)
            do j = 1_int64, int(len(s), int64)
                buf(1)%data(pos + j) = s(j:j)
            end do
            pos = pos + int(len(s), int64)
        end do
    end subroutine extract_col_string
    !
    !> Splits a timestamp column into its (seconds, nanoseconds) pair of integer keys -- see
    !! `extract_ts` for why a timestamp becomes two keys rather than one.
    subroutine extract_col_timestamp(col, n, descending, nulls_first, buf)
        type(parquet_column), intent(in) :: col                    !! the key column.
        integer(int64), intent(in) :: n                            !! row count.
        logical, intent(in) :: descending                          !! .true. sorts high to low.
        logical, intent(in) :: nulls_first                         !! .true. places nulls first.
        type(sort_key_buf), allocatable, intent(out) :: buf(:)     !! receives two keys.
        integer(int64) :: k, s
        integer(int32) :: ns
        type(parquet_timestamp) :: ts
        !
        allocate(buf(2))
        buf(:)%family = SK_INT
        buf(:)%descending = descending
        buf(:)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)), buf(2)%ints(max(n, 1_int64)))
        buf(1)%ints = 0_int64
        buf(2)%ints = 0_int64
        do k = 1_int64, n
            call col%get_at(k, ts)
            call ts%get_raw(s, ns)
            buf(1)%ints(k) = s
            buf(2)%ints(k) = int(ns, int64)
        end do
    end subroutine extract_col_timestamp
    !
    !> Copies a column's own per-row validity onto every key extracted from it, leaving it
    !! UNALLOCATED for a null-free column -- which is the engine's no-nulls fast path, and the same
    !! convention `%row_validity` itself uses by leaving its own result unallocated.
    !!
    !! **Deliberately built from per-row `%is_null` rather than `%row_validity`**, which would be
    !! the obvious choice: `%row_validity` is `intent(inout)` (a temporal column rescans and
    !! caches its null count there), so reaching it from a public `pf_argsort(column)` would mean
    !! either making that argument `intent(inout)` -- wrong for a query, and it would stop a caller
    !! passing their own `intent(in)` dummy -- or taking a copy of the column, which is a full deep
    !! copy of every value on the way to sorting it. `%is_null` is `intent(in)` and is always
    !! correct without consulting any cache, so it costs one extra O(n) pass in front of an
    !! O(n log n) sort and nothing else.
    subroutine col_valid_flags(col, n, buf)
        type(parquet_column), intent(in) :: col                 !! the key column.
        integer(int64), intent(in) :: n                         !! row count.
        type(sort_key_buf), intent(inout) :: buf(:)             !! the keys extracted from it.
        integer(int64) :: k
        integer :: ik
        logical :: any_null
        !
        any_null = .false.
        do k = 1_int64, n
            if (col%is_null(k)) then
                any_null = .true.
                exit
            end if
        end do
        if (.not. any_null) return
        allocate(buf(1)%valid(max(n, 1_int64)))
        buf(1)%valid = 1_c_int8_t
        do k = 1_int64, n
            if (col%is_null(k)) buf(1)%valid(k) = 0_c_int8_t
        end do
        do ik = 2, size(buf)
            buf(ik)%valid = buf(1)%valid
        end do
    end subroutine col_valid_flags
    !
    module procedure add_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i32(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_i32
    !
    module procedure add_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i64(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_i64
    !
    module procedure add_f32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f32(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_f32
    !
    module procedure add_f64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f64(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_f64
    !
    module procedure add_bool
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_bool(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_bool
    !
    module procedure add_chr
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_chr(values, buf, desc, nlo, "pf_sort_keys%add", is_valid=is_valid)
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_chr
    !
    module procedure add_date
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_date(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_date
    !
    module procedure add_time
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_time(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_time
    !
    module procedure add_ts
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_ts(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_ts
    !
    module procedure add_strcol
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_strcol(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_strcol
    !
    module procedure add_col
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_col(values, buf, desc, nlo, "pf_sort_keys%add")
        call keys_append(self, buf, "pf_sort_keys%add")
    end procedure add_col
    !
    module procedure keys_count
        n = self%nkeys
    end procedure keys_count
    !
    module procedure keys_clear
        self%nkeys = 0
        self%nrows = -1_int64
        if (allocated(self%keys)) deallocate(self%keys)
    end procedure keys_clear
    !
    module procedure keys_append
        type(sort_key_buf), allocatable :: bigger(:)
        integer(int64) :: n
        integer :: ik
        character(len=32) :: got_str, want_str
        !
        n = key_rows(buf(1))
        if (self%nkeys == 0) then
            self%nrows = n
        else if (n /= self%nrows) then
            write (got_str, "(i0)") n
            write (want_str, "(i0)") self%nrows
            error stop EP // proc // ": every key must describe the same number of rows; this " // &
                "one has " // trim(got_str) // " where the first key has " // trim(want_str)
        end if
        ! Grown exactly, not geometrically: a key list is a handful of entries, and each entry
        ! holds only allocatable descriptors (the value buffers themselves are moved, not copied).
        if (.not. allocated(self%keys)) allocate(self%keys(0))
        allocate(bigger(self%nkeys + size(buf)))
        do ik = 1, self%nkeys
            call move_key(self%keys(ik), bigger(ik))
        end do
        do ik = 1, size(buf)
            call move_key(buf(ik), bigger(self%nkeys + ik))
        end do
        self%nkeys = self%nkeys + size(buf)
        call move_alloc(bigger, self%keys)
        deallocate(buf)
    end procedure keys_append
    !
    !> Moves one key's buffers from `src` to `dst` without copying them.
    subroutine move_key(src, dst)
        type(sort_key_buf), intent(inout) :: src !! the key to move from; left empty.
        type(sort_key_buf), intent(inout) :: dst !! the key to move into.
        dst%family = src%family
        dst%descending = src%descending
        dst%nulls_first = src%nulls_first
        if (allocated(src%ints)) call move_alloc(src%ints, dst%ints)
        if (allocated(src%reals)) call move_alloc(src%reals, dst%reals)
        if (allocated(src%offsets)) call move_alloc(src%offsets, dst%offsets)
        if (allocated(src%data)) call move_alloc(src%data, dst%data)
        if (allocated(src%valid)) call move_alloc(src%valid, dst%valid)
    end subroutine move_key
    !
    !> How many rows one extracted key describes.
    pure function key_rows(buf) result(n)
        type(sort_key_buf), intent(in) :: buf !! the key.
        integer(int64) :: n                   !! its row count.
        select case (buf%family)
        case (SK_REAL)
            n = size(buf%reals, kind=int64)
        case (SK_STR)
            n = size(buf%offsets, kind=int64) - 1_int64
        case default
            n = size(buf%ints, kind=int64)
        end select
    end function key_rows
    !
    module procedure valid_from_mask
        integer(int64) :: k, m
        character(len=32) :: got_str, want_str
        !
        m = size(mask, kind=int64)
        if (m /= n) then
            write (got_str, "(i0)") m
            write (want_str, "(i0)") n
            error stop EP // proc // ": is_valid has " // trim(got_str) // " elements but the " // &
                "values have " // trim(want_str)
        end if
        ! Left UNALLOCATED when nothing is null: the caller turns that into a null pointer, which
        ! the engine reads as "no nulls" and takes its own fast path for. Answering `.true.` for
        ! every row instead would be correct and measurably slower.
        if (all(mask(1:n))) return
        allocate(valid(max(n, 1_int64)))
        valid = 1_c_int8_t
        do k = 1_int64, n
            if (.not. mask(k)) valid(k) = 0_c_int8_t
        end do
    end procedure valid_from_mask
    !
    module procedure drive_engine
        type(c_ptr) :: builder
        integer(int64) :: status
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once"
        end if
        ! Sized EXACTLY, never max(nrows, 1): a zero-row sort must hand back a zero-length
        ! permutation, or `size(perm)` lies and a caller's `do k = 1, size(perm)` reads element 1
        ! of an empty array. The engine is not called at all below two rows, so nothing downstream
        ! needs the one-element floor the extraction buffers use.
        allocate(perm(nrows))
        do ik = 1_int64, nrows
            perm(ik) = ik
        end do
        if (nrows < 2_int64) return
        if (size(keys) == 1) then
            ! One key needs no builder at all: the one-shot entry points BORROW the buffer that
            ! was just extracted, so this saves a handle allocation and a second copy of every
            ! value. Multi-key has to go through the builder, which owns its keys.
            call engine_one_shot(keys(1), nrows, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build(builder, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects -- kept
            ! because silently ignoring a nonzero status is how a real failure goes unnoticed.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine
    !
    module procedure engine_is_sorted
        type(c_ptr) :: builder
        integer(int64) :: res
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once"
        end if
        answer = .true.
        if (nrows < 2_int64) return
        if (size(keys) == 1) then
            call engine_one_shot_is_sorted(keys(1), nrows, answer)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        res = parquet_sort_builder_is_sorted(builder)
        call parquet_sort_builder_free(builder)
        if (res < 0_int64) then
            error stop EP // proc // ": the sort engine had no key to test" ! GCOVR_EXCL_LINE
        end if
        answer = res == 1_int64
    end procedure engine_is_sorted
    !
    !> Argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot(key, nrows, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(inout) :: perm(:)      !! receives the 1-based permutation.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_argsort_double(nrows, key%reals, vp, df, nf, perm)
        case (SK_STR)
            call parquet_sort_argsort_string(nrows, key%offsets, key%data, vp, df, nf, perm)
        case default
            call parquet_sort_argsort_int64(nrows, key%ints, vp, df, nf, perm)
        end select
    end subroutine engine_one_shot
    !
    !> Tests one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_is_sorted(key, nrows, answer)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        logical, intent(out) :: answer                !! .true. when already in order.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        integer(int64) :: res
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            res = parquet_sort_is_sorted_double(nrows, key%reals, vp, df, nf)
        case (SK_STR)
            res = parquet_sort_is_sorted_string(nrows, key%offsets, key%data, vp, df, nf)
        case default
            res = parquet_sort_is_sorted_int64(nrows, key%ints, vp, df, nf)
        end select
        answer = res == 1_int64
    end subroutine engine_one_shot_is_sorted
    !
    !> Adds one already-extracted key to a C++ builder.
    subroutine engine_add_key(builder, key, nrows)
        type(c_ptr), intent(in) :: builder            !! the builder handle.
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_builder_add_key_double(builder, key%reals, vp, df, nf)
        case (SK_STR)
            call parquet_sort_builder_add_key_string(builder, key%offsets, key%data, vp, df, nf)
        case default
            call parquet_sort_builder_add_key_int64(builder, key%ints, vp, df, nf)
        end select
    end subroutine engine_add_key
    !
    !> The three scalars every engine call needs: a pointer to the validity array (or a null
    !! pointer when the key has no nulls) and the two order flags as int8.
    subroutine key_flags(key, valid_ptr, desc_flag, nulls_flag)
        type(sort_key_buf), intent(in), target :: key   !! the key.
        type(c_ptr), intent(out) :: valid_ptr           !! its validity array, or C_NULL_PTR.
        integer(c_int8_t), intent(out) :: desc_flag     !! nonzero for descending.
        integer(c_int8_t), intent(out) :: nulls_flag    !! nonzero to place nulls first.
        !
        valid_ptr = c_null_ptr
        if (allocated(key%valid)) valid_ptr = c_loc(key%valid)
        desc_flag = merge(1_c_int8_t, 0_c_int8_t, key%descending)
        nulls_flag = merge(1_c_int8_t, 0_c_int8_t, key%nulls_first)
    end subroutine key_flags
    !
    module procedure check_permutation
        integer(int8), allocatable :: seen(:)
        integer(int64) :: k, v, word
        character(len=32) :: a_str, b_str
        !
        if (size(perm, kind=int64) /= n) then
            write (a_str, "(i0)") size(perm, kind=int64)
            write (b_str, "(i0)") n
            error stop EP // proc // ": perm has " // trim(a_str) // " elements but the values " // &
                "have " // trim(b_str)
        end if
        if (n < 1_int64) return
        ! A BIT-PACKED seen-set, not a LOGICAL array: gfortran's default LOGICAL is 32 bits, so a
        ! plain seen(n) would cost 4n bytes of scratch to validate a permutation whose own payload
        ! is 8n -- a 50% overhead on an operation whose whole point is to be cheap. This is n/8.
        allocate(seen((n + 7_int64) / 8_int64))
        seen = 0_int8
        do k = 1_int64, n
            v = perm(k)
            if (v < 1_int64 .or. v > n) then
                write (a_str, "(i0)") k
                write (b_str, "(i0)") v
                error stop EP // proc // ": perm(" // trim(a_str) // ") is " // trim(b_str) // &
                    ", which is outside the valid index range"
            end if
            word = (v - 1_int64) / 8_int64 + 1_int64
            if (btest(seen(word), int(mod(v - 1_int64, 8_int64)))) then
                write (a_str, "(i0)") v
                error stop EP // proc // ": perm is not a permutation -- the index " // &
                    trim(a_str) // " appears more than once"
            end if
            seen(word) = ibset(seen(word), int(mod(v - 1_int64, 8_int64)))
        end do
    end procedure check_permutation
    !
    module procedure narrow_perm
        integer(int64) :: n
        character(len=32) :: n_str
        !
        n = size(perm64, kind=int64)
        if (n > int(huge(1_int32), int64)) then
            write (n_str, "(i0)") n
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, which " // &
                "does not fit an int32 permutation; declare perm as integer(int64)"
        end if
        allocate(perm32(n))
        perm32 = int(perm64, int32)
    end procedure narrow_perm
    !
end submodule parquet_sorting_keys ! GCOVR_EXCL_LINE
