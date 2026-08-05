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
        integer(int64) :: status, nthreads
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
        call resolve_thread_count(threads, nrows, nthreads)
        if (size(keys) == 1) then
            ! One key needs no builder at all: the one-shot entry points BORROW the buffer that
            ! was just extracted, so this saves a handle allocation and a second copy of every
            ! value. Multi-key has to go through the builder, which owns its keys.
            call engine_one_shot(keys(1), nrows, nthreads, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build(builder, nthreads, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects -- kept
            ! because silently ignoring a nonzero status is how a real failure goes unnoticed.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine
    !
    module procedure drive_engine_partial
        type(c_ptr) :: builder
        integer(int64) :: status, ik
        integer :: jk
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given"
        end if
        allocate(perm(count))
        do ik = 1_int64, count
            perm(ik) = ik
        end do
        if (count < 1_int64 .or. nrows < 2_int64) return
        if (size(keys) == 1) then
            call engine_one_shot_partial(keys(1), nrows, count, perm)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do jk = 1, size(keys)
            call engine_add_key(builder, keys(jk), nrows)
        end do
        status = parquet_sort_builder_build_partial(builder, count, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure drive_engine_partial
    !
    module procedure engine_nth_index
        type(c_ptr) :: builder
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given"
        end if
        if (size(keys) == 1) then
            call engine_one_shot_nth(keys(1), nrows, nth, idx)
            return
        end if
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        idx = parquet_sort_builder_nth_element(builder, nth)
        call parquet_sort_builder_free(builder)
        if (idx < 1_int64) then
            ! The C side answers 0 for an empty key list or an out-of-range rank; both are already
            ! rejected above and by the caller's own bounds check.
            error stop EP // proc // ": the sort engine could not resolve that rank" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_nth_index
    !
    module procedure resolve_count
        character(len=32) :: n_str
        !
        ! Clamped, not refused: `n` is very often derived (a fraction of a row count, a config
        ! value, a post-filter survivor count), and aborting would put min(n, size(v)) at every
        ! call site. A NEGATIVE n is a different thing -- a caller error, not a boundary.
        if (n < 0) then
            write (n_str, "(i0)") n
            error stop EP // proc // ": n is " // trim(n_str) // ", which is negative"
        end if
        count = min(int(n, int64), nrows)
    end procedure resolve_count
    !
    !> Partially argsorts one already-extracted key through the matching one-shot entry point.
    subroutine engine_one_shot_partial(key, nrows, count, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: count           !! leading entries to order.
        integer(int64), intent(inout) :: perm(:)      !! receives `count` 1-based indices.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_partial_argsort_double(nrows, key%reals, vp, df, nf, count, perm)
        case (SK_STR)
            call parquet_sort_partial_argsort_string(nrows, key%offsets, key%data, vp, df, nf, count, perm)
        case default
            call parquet_sort_partial_argsort_int64(nrows, key%ints, vp, df, nf, count, perm)
        end select
    end subroutine engine_one_shot_partial
    !
    !> Resolves one already-extracted key's nth index through the matching one-shot entry point.
    subroutine engine_one_shot_nth(key, nrows, nth, idx)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nth             !! 1-based rank wanted.
        integer(int64), intent(out) :: idx            !! 1-based row index at that rank.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            idx = parquet_sort_nth_index_double(nrows, key%reals, vp, df, nf, nth)
        case (SK_STR)
            idx = parquet_sort_nth_index_string(nrows, key%offsets, key%data, vp, df, nf, nth)
        case default
            idx = parquet_sort_nth_index_int64(nrows, key%ints, vp, df, nf, nth)
        end select
    end subroutine engine_one_shot_nth
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
    subroutine engine_one_shot(key, nrows, nthreads, perm)
        type(sort_key_buf), intent(in), target :: key !! the key.
        integer(int64), intent(in) :: nrows           !! its row count.
        integer(int64), intent(in) :: nthreads        !! resolved thread count; 1 sorts serially.
        integer(int64), intent(inout) :: perm(:)      !! receives the 1-based permutation.
        type(c_ptr) :: vp
        integer(c_int8_t) :: df, nf
        !
        call key_flags(key, vp, df, nf)
        select case (key%family)
        case (SK_REAL)
            call parquet_sort_argsort_double(nrows, key%reals, vp, df, nf, nthreads, perm)
        case (SK_STR)
            call parquet_sort_argsort_string(nrows, key%offsets, key%data, vp, df, nf, nthreads, perm)
        case default
            call parquet_sort_argsort_int64(nrows, key%ints, vp, df, nf, nthreads, perm)
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
    module procedure check_rank
        character(len=32) :: a_str, b_str
        !
        if (nth < 1_int64 .or. nth > nrows) then
            write (a_str, "(i0)") nth
            write (b_str, "(i0)") nrows
            error stop EP // proc // ": nth is " // trim(a_str) // ", which is outside 1.." // &
                trim(b_str)
        end if
    end procedure check_rank
    !
    module procedure key_valid_count
        integer(int64) :: k
        !
        ! An unallocated `valid` is the module's "no nulls at all" convention, so the whole array
        ! counts -- the same fast path the engine itself takes.
        if (.not. allocated(keys(1)%valid)) then
            n_valid = nrows
            return
        end if
        n_valid = 0_int64
        do k = 1_int64, nrows
            if (keys(1)%valid(k) /= 0_c_int8_t) n_valid = n_valid + 1_int64
        end do
    end procedure key_valid_count
    !
    module procedure fold_token
        integer :: k, ic
        !
        tok = trim(adjustl(text))
        do k = 1, len(tok)
            ic = iachar(tok(k:k))
            if (ic >= iachar("A") .and. ic <= iachar("Z")) tok(k:k) = achar(ic + 32)
        end do
    end procedure fold_token
    !
    module procedure resolve_rounding
        character(len=:), allocatable :: tok, shown
        !
        mode = RND_NEAREST
        if (.not. present(rounding)) return
        call fold_token(rounding, tok)
        select case (tok)
        case ("nearest")
            mode = RND_NEAREST
        case ("down")
            mode = RND_DOWN
        case ("up")
            mode = RND_UP
        case default
            ! Capped to a short preview: the caller controls this string's length, and ifx's
            ! ERROR STOP runtime corrupts the heap once the composed message reaches 8192 bytes
            ! (CLAUDE.md). Same shape as parquet_filter_add's own rule preview.
            shown = trim(adjustl(rounding))
            if (len(shown) > 100) shown = shown(1:100) // "..."
            error stop EP // proc // ": rounding='" // shown // "' is not recognized; use " // &
                "'nearest' (the default), 'down' or 'up'"
        end select
    end procedure resolve_rounding
    !
    module procedure quantile_rank
        real(real64) :: pos
        !
        if (quantile < 0.0_real64 .or. quantile > 1.0_real64 .or. quantile /= quantile) then
            ! The NaN arm is what the self-comparison catches; ieee_is_nan would need another
            ! import here for one test, and this expression is exact with no arithmetic drift.
            error stop EP // proc // ": quantile must lie on a 0-1 scale (note: NOT 0-100)"
        end if
        if (n_valid < 1_int64) then
            error stop EP // proc // ": every value is null, so no quantile exists; guard with " // &
                "count(is_valid) (or the column's own null count) if that can happen"
        end if
        ! Position on the 0-based index scale of the non-null values, so quantile=0 gives the
        ! smallest and quantile=1 the largest exactly, with no rounding involved at either end.
        pos = quantile * real(n_valid - 1_int64, real64)
        select case (mode)
        case (RND_DOWN)
            rank = int(floor(pos), int64) + 1_int64
        case (RND_UP)
            rank = int(ceiling(pos), int64) + 1_int64
        case default
            rank = int(nint(pos, int64), int64) + 1_int64
        end select
        if (rank < 1_int64) rank = 1_int64
        if (rank > n_valid) rank = n_valid
    end procedure quantile_rank
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
    module procedure pf_sort_threads
#ifdef _OPENMP
        use omp_lib, only : omp_get_max_threads, omp_in_parallel
#endif
        !
        n = 1
#ifdef _OPENMP
        ! Serial inside a parallel region, deliberately. This is not a refusal and not a
        ! correctness guard -- it picks a DEFAULT, exactly as parallel_prefetch_ok
        ! (parquet_tables_read.f90) does for the table's own internally-parallel read, whose
        ! comment states the reason: nested regions are the caller's business. Without it, T
        ! OpenMP threads would each ask for T more, and T*T oversubscription is slower than not
        ! threading at all. An EXPLICIT threads= is still honoured there -- see
        ! resolve_thread_count, which only consults this when the caller said nothing.
        if (.not. omp_in_parallel()) n = omp_get_max_threads()
#endif
    end procedure pf_sort_threads
    !
    module procedure resolve_thread_count
        !
        if (present(threads)) then
            ! An explicit request is honoured wherever it is made, including inside a parallel
            ! region: the caller has said what they want, and refusing it there would leave no way
            ! to thread a sort at all from code that is itself parallel.
            count = max(1_int64, int(threads, int64))
        else
            count = int(pf_sort_threads(), int64)
        end if
        ! Never more threads than rows; the C++ side clamps again by its own minimum chunk size.
        if (count > nrows) count = max(nrows, 1_int64)
    end procedure resolve_thread_count
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
    module procedure narrow_i64
        character(len=32) :: v_str
        !
        if (value > int(huge(1_int32), int64)) then
            write (v_str, "(i0)") value
            error stop EP // proc // ": the " // noun // " is " // trim(v_str) // ", which does " // &
                "not fit an int32; declare that argument as integer(int64)"
        end if
        dst = int(value, int32)
    end procedure narrow_i64
    !
    module procedure narrow_i64_array
        integer(int64) :: n, biggest
        character(len=32) :: v_str
        !
        n = size(src, kind=int64)
        if (n > 0_int64) then
            biggest = maxval(src)
            if (biggest > int(huge(1_int32), int64)) then
                write (v_str, "(i0)") biggest
                error stop EP // proc // ": the largest " // noun // " is " // trim(v_str) // &
                    ", which does not fit an int32; declare that argument as integer(int64)"
            end if
        end if
        allocate(dst(n))
        dst = int(src, int32)
    end procedure narrow_i64_array
    !
    module procedure resolve_rank_method
        character(len=:), allocatable :: tok, shown
        !
        mode = RANK_COMPETITION
        if (.not. present(method)) return
        call fold_token(method, tok)
        select case (tok)
        case ("competition")
            mode = RANK_COMPETITION
        case ("dense")
            mode = RANK_DENSE
        case ("ordinal")
            mode = RANK_ORDINAL
        case default
            ! Capped to a short preview, exactly as resolve_rounding is: the caller controls this
            ! string's length and ifx's ERROR STOP runtime corrupts the heap at 8192 bytes.
            shown = trim(adjustl(method))
            if (len(shown) > 100) shown = shown(1:100) // "..."
            error stop EP // proc // ": method='" // shown // "' is not recognized; use " // &
                "'competition' (the default), 'dense' or 'ordinal'"
        end select
    end procedure resolve_rank_method
    !
    module procedure key_null_mask
        integer(int64) :: k
        !
        allocate(isnull(max(nrows, 1_int64)))
        isnull = .false.
        if (.not. allocated(keys(1)%valid)) return
        do k = 1_int64, nrows
            isnull(k) = keys(1)%valid(k) == 0_c_int8_t
        end do
    end procedure key_null_mask
    !
    module procedure key_value_count
        integer(int64) :: k
        logical :: has_valid, is_real
        !
        ! Tier 0 of sort_tier_of, counted on the Fortran side rather than asked of the engine: it
        ! is the same two questions (is this row null, and -- for a real key only -- is it a NaN)
        ! and neither needs a comparison. A NaN is skipped because it is not a minimum or a maximum
        ! of anything, while remaining an ordinary value everywhere else in this module.
        has_valid = allocated(keys(1)%valid)
        is_real = keys(1)%family == SK_REAL
        n_value = 0_int64
        do k = 1_int64, nrows
            if (has_valid) then
                if (keys(1)%valid(k) == 0_c_int8_t) cycle
            end if
            if (is_real) then
                if (ieee_is_nan(keys(1)%reals(k))) cycle
            end if
            n_value = n_value + 1_int64
        end do
    end procedure key_value_count
    !
    module procedure check_sorted_input
        logical :: ok
        !
        call engine_is_sorted(keys, nrows, proc, ok)
        if (.not. ok) then
            error stop EP // proc // ": " // what // " is not sorted in the order given by " // &
                "descending/nulls_first; sort it first, or pass assume_sorted=.true. only for " // &
                "an order you have already established"
        end if
    end procedure check_sorted_input
    !
    module procedure buf_append
        integer :: ik
        integer(int64) :: total_d, total_s, k, n
        integer(int64), allocatable :: newoff(:), newints(:)
        real(real64), allocatable :: newreals(:)
        character(kind=c_char), allocatable :: newdata(:)
        integer(c_int8_t), allocatable :: newvalid(:)
        !
        if (.not. allocated(dst) .or. .not. allocated(src)) then
            ! Both come straight from an extract_* call, which always allocates.
            error stop EP // proc // ": internal error: a sort key was not extracted" ! GCOVR_EXCL_LINE
        end if
        if (size(dst) /= size(src)) then
            ! Only reachable if two different types were extracted into one pair, which no
            ! generated caller does -- every one extracts both sides with the same extractor.
            error stop EP // proc // ": internal error: mismatched key counts" ! GCOVR_EXCL_LINE
        end if
        n = nd + ns
        do ik = 1, size(dst)
            select case (dst(ik)%family)
            case (SK_REAL)
                allocate(newreals(max(n, 1_int64)))
                newreals = 0.0_real64
                if (nd > 0_int64) newreals(1:nd) = dst(ik)%reals(1:nd)
                if (ns > 0_int64) newreals(nd + 1_int64:n) = src(ik)%reals(1:ns)
                call move_alloc(newreals, dst(ik)%reals)
            case (SK_STR)
                ! The offsets are byte positions into `data`, so the appended half's have to be
                ! rebased by however many bytes the first half occupies -- this is the one family
                ! where concatenating two keys is not just concatenating two arrays.
                total_d = dst(ik)%offsets(nd + 1_int64)
                total_s = src(ik)%offsets(ns + 1_int64)
                allocate(newoff(n + 1_int64))
                newoff(1:nd + 1_int64) = dst(ik)%offsets(1:nd + 1_int64)
                do k = 1_int64, ns
                    newoff(nd + 1_int64 + k) = total_d + src(ik)%offsets(k + 1_int64)
                end do
                allocate(newdata(max(total_d + total_s, 1_int64)))
                if (total_d > 0_int64) newdata(1:total_d) = dst(ik)%data(1:total_d)
                if (total_s > 0_int64) newdata(total_d + 1_int64:total_d + total_s) = src(ik)%data(1:total_s)
                call move_alloc(newoff, dst(ik)%offsets)
                call move_alloc(newdata, dst(ik)%data)
            case default
                allocate(newints(max(n, 1_int64)))
                newints = 0_int64
                if (nd > 0_int64) newints(1:nd) = dst(ik)%ints(1:nd)
                if (ns > 0_int64) newints(nd + 1_int64:n) = src(ik)%ints(1:ns)
                call move_alloc(newints, dst(ik)%ints)
            end select
            ! Materialized only when at least one side has nulls, so a null-free append stays on
            ! the engine's no-nulls fast path. An absent half is all-valid, which is exactly what
            ! the unallocated convention means.
            if (allocated(dst(ik)%valid) .or. allocated(src(ik)%valid)) then
                allocate(newvalid(max(n, 1_int64)))
                newvalid = 1_c_int8_t
                if (allocated(dst(ik)%valid) .and. nd > 0_int64) newvalid(1:nd) = dst(ik)%valid(1:nd)
                if (allocated(src(ik)%valid) .and. ns > 0_int64) newvalid(nd + 1_int64:n) = src(ik)%valid(1:ns)
                call move_alloc(newvalid, dst(ik)%valid)
            end if
        end do
    end procedure buf_append
    !
    ! ---- The M3 engine drivers ----
    !
    ! Unlike drive_engine above, these three always go through the builder, even for a single key.
    ! The one-shot entry points exist to skip a copy on the hottest path in the library, and none
    ! of these is it -- run detection, binary search and merging each cost one extra copy of an
    ! already-extracted buffer in exchange for one entry point per operation instead of three.
    !
    module procedure engine_build_runs
        type(c_ptr) :: builder
        integer(int64) :: status, k, nthreads
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(nrows))
        allocate(tie(max(nrows, 1_int64)))
        tie = 0_c_int8_t
        do k = 1_int64, nrows
            perm(k) = k
        end do
        if (nrows < 2_int64) return
        call resolve_thread_count(threads, nrows, nthreads)
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_build_runs(builder, nthreads, perm, tie)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not build a permutation" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_build_runs
    !
    module procedure engine_search
        type(c_ptr) :: builder
        integer(c_int8_t) :: wflag
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        wflag = merge(1_c_int8_t, 0_c_int8_t, upper)
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        pos = parquet_sort_builder_search(builder, n_search, wflag)
        call parquet_sort_builder_free(builder)
        if (pos < 1_int64) then
            ! The C side answers -1 only for an empty key list, rejected above.
            error stop EP // proc // ": the sort engine had no key to search" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_search
    !
    module procedure engine_merge
        type(c_ptr) :: builder
        integer(int64) :: status, k
        integer :: ik
        !
        if (size(keys) < 1) then
            error stop EP // proc // ": no sort key was given" ! GCOVR_EXCL_LINE
        end if
        allocate(perm(nrows))
        do k = 1_int64, nrows
            perm(k) = k
        end do
        if (nrows < 2_int64) return
        builder = parquet_sort_builder_new(nrows)
        do ik = 1, size(keys)
            call engine_add_key(builder, keys(ik), nrows)
        end do
        status = parquet_sort_builder_merge(builder, na, perm)
        call parquet_sort_builder_free(builder)
        if (status /= 0_int64) then
            ! Only reachable with an empty key list, which the guard above already rejects.
            error stop EP // proc // ": the sort engine could not merge" ! GCOVR_EXCL_LINE
        end if
    end procedure engine_merge
    !
end submodule parquet_sorting_keys ! GCOVR_EXCL_LINE
