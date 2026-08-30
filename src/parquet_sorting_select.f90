!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_partial_sort` and `pf_partial_argsort` -- ordering only the first `n` elements.
!!
!! Same engine, same comparator, same tiers as a full sort (`feature_risks.md` Risk-34): these
!! reach `std::partial_sort` through the very object `std::sort` is given, so a partial result can
!! never disagree with the corresponding prefix of a full one.
!!
!! **`n` is clamped, not checked.** Asking for more elements than the array holds returns all of
!! them, in order. That is deliberate -- `n` is very often derived, and refusing it would put
!! `min(n, size(v))` at every call site. A negative `n` is a caller error and aborts.
!!
!! **A partial sort that is not actually partial is invisible**: returning the first `n` of a FULL
!! sort is correct and merely slower, so no correctness test can tell the two apart. That is what
!! `parquet_debug_get_sort_comparisons` exists for, and why the guide states the complexity claim
!! with its own caveat rather than as a free win.
submodule (parquet_sorting) parquet_sorting_select
    implicit none
    !
contains
    !
    module procedure partial_argsort_i32_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_i32(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_i32_i32
    !
    module procedure partial_argsort_i32_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_i32(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_i32_i64
    !
    module procedure partial_argsort_i64_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_i64(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_i64_i32
    !
    module procedure partial_argsort_i64_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_i64(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_i64_i64
    !
    module procedure partial_argsort_f32_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_f32(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_f32_i32
    !
    module procedure partial_argsort_f32_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_f32(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_f32_i64
    !
    module procedure partial_argsort_f64_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_f64(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_f64_i32
    !
    module procedure partial_argsort_f64_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_f64(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_f64_i64
    !
    module procedure partial_argsort_bool_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_bool(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_bool_i32
    !
    module procedure partial_argsort_bool_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_bool(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_bool_i64
    !
    module procedure partial_argsort_chr_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_chr(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_chr_i32
    !
    module procedure partial_argsort_chr_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_chr(values, buf, desc, nlo, "pf_partial_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_chr_i64
    !
    module procedure partial_argsort_date_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_date(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_date_i32
    !
    module procedure partial_argsort_date_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_date(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_date_i64
    !
    module procedure partial_argsort_time_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_time(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_time_i32
    !
    module procedure partial_argsort_time_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_time(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_time_i64
    !
    module procedure partial_argsort_ts_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_ts(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_ts_i32
    !
    module procedure partial_argsort_ts_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_ts(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_ts_i64
    !
    module procedure partial_argsort_strcol_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = values%size()
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_strcol(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_strcol_i32
    !
    module procedure partial_argsort_strcol_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = values%size()
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_strcol(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_strcol_i64
    !
    module procedure partial_argsort_col_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = values%length()
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_col(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_col_i32
    !
    module procedure partial_argsort_col_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = values%length()
        call resolve_count(n, nrows, "pf_partial_argsort", count)
        call extract_col(values, buf, desc, nlo, "pf_partial_argsort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_col_i64
    !
    module procedure partial_argsort_keys_i32
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: count
        !
        if (keys%nkeys < 1) then
            error stop EP // "pf_partial_argsort: this pf_sort_keys has no key; " // &
                "call keys%add(...) at least once before sorting"
        end if
        call resolve_count(n, keys%nrows, "pf_partial_argsort", count)
        call drive_engine_partial(keys%keys(1:keys%nkeys), keys%nrows, count, &
            "pf_partial_argsort", perm64)
        call narrow_perm(perm64, "pf_partial_argsort", perm, threads=threads)
    end procedure partial_argsort_keys_i32
    !
    module procedure partial_argsort_keys_i64
        integer(int64), allocatable :: perm64(:)
        integer(int64) :: count
        !
        if (keys%nkeys < 1) then
            error stop EP // "pf_partial_argsort: this pf_sort_keys has no key; " // &
                "call keys%add(...) at least once before sorting"
        end if
        call resolve_count(n, keys%nrows, "pf_partial_argsort", count)
        call drive_engine_partial(keys%keys(1:keys%nkeys), keys%nrows, count, &
            "pf_partial_argsort", perm64)
        call move_alloc(perm64, perm)
    end procedure partial_argsort_keys_i64
    !
    module procedure partial_sort_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_i32(values, buf, desc, nlo, "pf_partial_sort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
        if (present(sorted_valid)) then
            allocate(sorted_valid(count))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, count
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure partial_sort_i32
    !
    module procedure partial_sort_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_i64(values, buf, desc, nlo, "pf_partial_sort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
        if (present(sorted_valid)) then
            allocate(sorted_valid(count))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, count
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure partial_sort_i64
    !
    module procedure partial_sort_f32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_f32(values, buf, desc, nlo, "pf_partial_sort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
        if (present(sorted_valid)) then
            allocate(sorted_valid(count))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, count
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure partial_sort_f32
    !
    module procedure partial_sort_f64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_f64(values, buf, desc, nlo, "pf_partial_sort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
        if (present(sorted_valid)) then
            allocate(sorted_valid(count))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, count
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure partial_sort_f64
    !
    module procedure partial_sort_bool
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_bool(values, buf, desc, nlo, "pf_partial_sort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
        if (present(sorted_valid)) then
            allocate(sorted_valid(count))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, count
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure partial_sort_bool
    !
    module procedure partial_sort_chr
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_chr(values, buf, desc, nlo, "pf_partial_sort", is_valid=is_valid, threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(character(len=len(values)) :: sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
        if (present(sorted_valid)) then
            allocate(sorted_valid(count))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, count
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure partial_sort_chr
    !
    module procedure partial_sort_date
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_date(values, buf, desc, nlo, "pf_partial_sort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
    end procedure partial_sort_date
    !
    module procedure partial_sort_time
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_time(values, buf, desc, nlo, "pf_partial_sort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
    end procedure partial_sort_time
    !
    module procedure partial_sort_ts
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, nrows, count
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        nrows = size(values, kind=int64)
        call resolve_count(n, nrows, "pf_partial_sort", count)
        call extract_ts(values, buf, desc, nlo, "pf_partial_sort", threads=threads)
        call drive_engine_partial(buf, nrows, count, "pf_partial_sort", perm)
        allocate(sorted(count))
        do k = 1_int64, count
            sorted(k) = values(perm(k))
        end do
    end procedure partial_sort_ts
    !
    module procedure nth_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_i32_i32
    !
    module procedure nth_i32_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_i32_i32_i32
    !
    module procedure nth_i32_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_i32_i32_i64
    !
    module procedure nth_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_i32_i64
    !
    module procedure nth_i32_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_i32_i64_i32
    !
    module procedure nth_i32_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_i32_i64_i64
    !
    module procedure nth_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_i64_i32
    !
    module procedure nth_i64_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_i64_i32_i32
    !
    module procedure nth_i64_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_i64_i32_i64
    !
    module procedure nth_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_i64_i64
    !
    module procedure nth_i64_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_i64_i64_i32
    !
    module procedure nth_i64_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_i64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_i64_i64_i64
    !
    module procedure nth_f32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_f32_i32
    !
    module procedure nth_f32_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_f32_i32_i32
    !
    module procedure nth_f32_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_f32_i32_i64
    !
    module procedure nth_f32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_f32_i64
    !
    module procedure nth_f32_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_f32_i64_i32
    !
    module procedure nth_f32_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f32(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_f32_i64_i64
    !
    module procedure nth_f64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_f64_i32
    !
    module procedure nth_f64_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_f64_i32_i32
    !
    module procedure nth_f64_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_f64_i32_i64
    !
    module procedure nth_f64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_f64_i64
    !
    module procedure nth_f64_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_f64_i64_i32
    !
    module procedure nth_f64_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_f64(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_f64_i64_i64
    !
    module procedure nth_bool_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_bool(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_bool_i32
    !
    module procedure nth_bool_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_bool(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_bool_i32_i32
    !
    module procedure nth_bool_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_bool(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_bool_i32_i64
    !
    module procedure nth_bool_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_bool(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_bool_i64
    !
    module procedure nth_bool_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_bool(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_bool_i64_i32
    !
    module procedure nth_bool_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_bool(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_bool_i64_i64
    !
    module procedure nth_chr_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_chr(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_chr_i32
    !
    module procedure nth_chr_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_chr(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_chr_i32_i32
    !
    module procedure nth_chr_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_chr(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_chr_i32_i64
    !
    module procedure nth_chr_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_chr(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
    end procedure nth_chr_i64
    !
    module procedure nth_chr_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_chr(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_chr_i64_i32
    !
    module procedure nth_chr_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_chr(values, int(nth, int64), p_value, idx, desc, nlo, is_valid=is_valid, threads=threads)
        index = idx
    end procedure nth_chr_i64_i64
    !
    module procedure nth_date_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_date(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_date_i32
    !
    module procedure nth_date_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_date(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_date_i32_i32
    !
    module procedure nth_date_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_date(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_date_i32_i64
    !
    module procedure nth_date_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_date(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_date_i64
    !
    module procedure nth_date_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_date(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_date_i64_i32
    !
    module procedure nth_date_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_date(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_date_i64_i64
    !
    module procedure nth_time_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_time(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_time_i32
    !
    module procedure nth_time_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_time(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_time_i32_i32
    !
    module procedure nth_time_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_time(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_time_i32_i64
    !
    module procedure nth_time_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_time(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_time_i64
    !
    module procedure nth_time_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_time(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_time_i64_i32
    !
    module procedure nth_time_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_time(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_time_i64_i64
    !
    module procedure nth_ts_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_ts(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_ts_i32
    !
    module procedure nth_ts_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_ts(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_ts_i32_i32
    !
    module procedure nth_ts_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_ts(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_ts_i32_i64
    !
    module procedure nth_ts_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_ts(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_ts_i64
    !
    module procedure nth_ts_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_ts(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_ts_i64_i32
    !
    module procedure nth_ts_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_ts(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_ts_i64_i64
    !
    module procedure nth_strcol_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_strcol(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_strcol_i32
    !
    module procedure nth_strcol_i32_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_strcol(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_strcol_i32_i32
    !
    module procedure nth_strcol_i32_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_strcol(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_strcol_i32_i64
    !
    module procedure nth_strcol_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_strcol(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
    end procedure nth_strcol_i64
    !
    module procedure nth_strcol_i64_i32
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_strcol(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        call narrow_index(idx, "pf_nth_element", index)
    end procedure nth_strcol_i64_i32
    !
    module procedure nth_strcol_i64_i64
        integer(int64) :: idx
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call nth_impl_strcol(values, int(nth, int64), p_value, idx, desc, nlo, threads=threads)
        index = idx
    end procedure nth_strcol_i64_i64
    !
    module procedure quantile_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_i32(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_i32
    !
    module procedure quantile_i32_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_i32(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_i32_i32
    !
    module procedure quantile_i32_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_i32(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_i32_i64
    !
    module procedure quantile_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_i64(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_i64
    !
    module procedure quantile_i64_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_i64(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_i64_i32
    !
    module procedure quantile_i64_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_i64(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_i64_i64
    !
    module procedure quantile_f32
        integer(int64) :: idx, nn
        !
        call quantile_impl_f32(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_f32
    !
    module procedure quantile_f32_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_f32(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_f32_i32
    !
    module procedure quantile_f32_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_f32(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_f32_i64
    !
    module procedure quantile_f64
        integer(int64) :: idx, nn
        !
        call quantile_impl_f64(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_f64
    !
    module procedure quantile_f64_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_f64(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_f64_i32
    !
    module procedure quantile_f64_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_f64(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_f64_i64
    !
    module procedure quantile_bool
        integer(int64) :: idx, nn
        !
        call quantile_impl_bool(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_bool
    !
    module procedure quantile_bool_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_bool(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_bool_i32
    !
    module procedure quantile_bool_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_bool(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_bool_i64
    !
    module procedure quantile_chr
        integer(int64) :: idx, nn
        !
        call quantile_impl_chr(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_chr
    !
    module procedure quantile_chr_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_chr(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_chr_i32
    !
    module procedure quantile_chr_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_chr(values, quantile, p_value, idx, nn, rounding, is_valid=is_valid, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_chr_i64
    !
    module procedure quantile_date
        integer(int64) :: idx, nn
        !
        call quantile_impl_date(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_date
    !
    module procedure quantile_date_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_date(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_date_i32
    !
    module procedure quantile_date_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_date(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_date_i64
    !
    module procedure quantile_time
        integer(int64) :: idx, nn
        !
        call quantile_impl_time(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_time
    !
    module procedure quantile_time_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_time(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_time_i32
    !
    module procedure quantile_time_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_time(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_time_i64
    !
    module procedure quantile_ts
        integer(int64) :: idx, nn
        !
        call quantile_impl_ts(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_ts
    !
    module procedure quantile_ts_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_ts(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_ts_i32
    !
    module procedure quantile_ts_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_ts(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_ts_i64
    !
    module procedure quantile_strcol
        integer(int64) :: idx, nn
        !
        call quantile_impl_strcol(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
    end procedure quantile_strcol
    !
    module procedure quantile_strcol_i32
        integer(int64) :: idx, nn
        !
        call quantile_impl_strcol(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        call narrow_index(idx, "pf_nth_quantile", index)
    end procedure quantile_strcol_i32
    !
    module procedure quantile_strcol_i64
        integer(int64) :: idx, nn
        !
        call quantile_impl_strcol(values, quantile, p_value, idx, nn, rounding, threads=threads, ok=ok)
        ! `n_null` is set even on the ok=.false. path, so a caller taking the `ok`
        ! route still learns how many values were excluded.
        if (present(n_null)) n_null = nn
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit.
        if (present(ok)) then
            if (.not. ok) return   ! idx is undefined on this path
        end if
        index = idx
    end procedure quantile_strcol_i64
    !
    !> Shared worker behind every pf_nth_element specific for a 32-bit integer array.
    subroutine nth_impl_i32(values, nth, p_value, idx, descending, nulls_first, is_valid, threads)
        integer(int32), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        integer(int32), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_i32(values, buf, descending, nulls_first, "pf_nth_element", is_valid=is_valid, threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_i32
    !
    !> Shared worker behind every pf_nth_quantile specific for a 32-bit integer array.
    subroutine quantile_impl_i32(values, quantile, p_value, idx, n_null, rounding, is_valid, threads, ok)
        integer(int32), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        integer(int32), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_i32(values, buf, .false., .false., "pf_nth_quantile", is_valid=is_valid, threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_i32
    !
    !> Shared worker behind every pf_nth_element specific for a 64-bit integer array.
    subroutine nth_impl_i64(values, nth, p_value, idx, descending, nulls_first, is_valid, threads)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        integer(int64), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_i64(values, buf, descending, nulls_first, "pf_nth_element", is_valid=is_valid, threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_i64
    !
    !> Shared worker behind every pf_nth_quantile specific for a 64-bit integer array.
    subroutine quantile_impl_i64(values, quantile, p_value, idx, n_null, rounding, is_valid, threads, ok)
        integer(int64), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        integer(int64), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_i64(values, buf, .false., .false., "pf_nth_quantile", is_valid=is_valid, threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_i64
    !
    !> Shared worker behind every pf_nth_element specific for a 32-bit real array.
    subroutine nth_impl_f32(values, nth, p_value, idx, descending, nulls_first, is_valid, threads)
        real(real32), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        real(real32), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_f32(values, buf, descending, nulls_first, "pf_nth_element", is_valid=is_valid, threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_f32
    !
    !> Shared worker behind every pf_nth_quantile specific for a 32-bit real array.
    subroutine quantile_impl_f32(values, quantile, p_value, idx, n_null, rounding, is_valid, threads, ok)
        real(real32), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        real(real32), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_f32(values, buf, .false., .false., "pf_nth_quantile", is_valid=is_valid, threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_f32
    !
    !> Shared worker behind every pf_nth_element specific for a 64-bit real array.
    subroutine nth_impl_f64(values, nth, p_value, idx, descending, nulls_first, is_valid, threads)
        real(real64), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        real(real64), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_f64(values, buf, descending, nulls_first, "pf_nth_element", is_valid=is_valid, threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_f64
    !
    !> Shared worker behind every pf_nth_quantile specific for a 64-bit real array.
    subroutine quantile_impl_f64(values, quantile, p_value, idx, n_null, rounding, is_valid, threads, ok)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        real(real64), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_f64(values, buf, .false., .false., "pf_nth_quantile", is_valid=is_valid, threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_f64
    !
    !> Shared worker behind every pf_nth_element specific for a logical array.
    subroutine nth_impl_bool(values, nth, p_value, idx, descending, nulls_first, is_valid, threads)
        logical, intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        logical, intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_bool(values, buf, descending, nulls_first, "pf_nth_element", is_valid=is_valid, threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_bool
    !
    !> Shared worker behind every pf_nth_quantile specific for a logical array.
    subroutine quantile_impl_bool(values, quantile, p_value, idx, n_null, rounding, is_valid, threads, ok)
        logical, intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        logical, intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_bool(values, buf, .false., .false., "pf_nth_quantile", is_valid=is_valid, threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_bool
    !
    !> Shared worker behind every pf_nth_element specific for a string array.
    subroutine nth_impl_chr(values, nth, p_value, idx, descending, nulls_first, is_valid, threads)
        character(len=*), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_chr(values, buf, descending, nulls_first, "pf_nth_element", is_valid=is_valid, threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_chr
    !
    !> Shared worker behind every pf_nth_quantile specific for a string array.
    subroutine quantile_impl_chr(values, quantile, p_value, idx, n_null, rounding, is_valid, threads, ok)
        character(len=*), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_chr(values, buf, .false., .false., "pf_nth_quantile", is_valid=is_valid, threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_chr
    !
    !> Shared worker behind every pf_nth_element specific for a date array.
    subroutine nth_impl_date(values, nth, p_value, idx, descending, nulls_first, threads)
        type(parquet_date), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        type(parquet_date), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_date(values, buf, descending, nulls_first, "pf_nth_element", threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_date
    !
    !> Shared worker behind every pf_nth_quantile specific for a date array.
    subroutine quantile_impl_date(values, quantile, p_value, idx, n_null, rounding, threads, ok)
        type(parquet_date), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        type(parquet_date), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_date(values, buf, .false., .false., "pf_nth_quantile", threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_date
    !
    !> Shared worker behind every pf_nth_element specific for a time array.
    subroutine nth_impl_time(values, nth, p_value, idx, descending, nulls_first, threads)
        type(parquet_time), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        type(parquet_time), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_time(values, buf, descending, nulls_first, "pf_nth_element", threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_time
    !
    !> Shared worker behind every pf_nth_quantile specific for a time array.
    subroutine quantile_impl_time(values, quantile, p_value, idx, n_null, rounding, threads, ok)
        type(parquet_time), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        type(parquet_time), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_time(values, buf, .false., .false., "pf_nth_quantile", threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_time
    !
    !> Shared worker behind every pf_nth_element specific for a timestamp array.
    subroutine nth_impl_ts(values, nth, p_value, idx, descending, nulls_first, threads)
        type(parquet_timestamp), intent(in) :: values(:)
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        type(parquet_timestamp), intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = size(values, kind=int64)
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_ts(values, buf, descending, nulls_first, "pf_nth_element", threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        p_value = values(idx)
    end subroutine nth_impl_ts
    !
    !> Shared worker behind every pf_nth_quantile specific for a timestamp array.
    subroutine quantile_impl_ts(values, quantile, p_value, idx, n_null, rounding, threads, ok)
        type(parquet_timestamp), intent(in) :: values(:)
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        type(parquet_timestamp), intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_ts(values, buf, .false., .false., "pf_nth_quantile", threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        p_value = values(idx)
    end subroutine quantile_impl_ts
    !
    !> Shared worker behind every pf_nth_element specific for a packed string column array.
    subroutine nth_impl_strcol(values, nth, p_value, idx, descending, nulls_first, threads)
        type(parquet_string_column), intent(in) :: values
        integer(int64), intent(in) :: nth   !! 1-based rank wanted.
        character(len=:), allocatable, intent(out) :: p_value !! the value at that rank.
        integer(int64), intent(out) :: idx  !! which element of `values` that was.
        logical, intent(in) :: descending   !! .true. ranks high to low.
        logical, intent(in) :: nulls_first  !! .true. ranks nulls first.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows
        !
        nrows = values%size()
        call check_rank(nth, nrows, "pf_nth_element")
        call extract_strcol(values, buf, descending, nulls_first, "pf_nth_element", threads=threads)
        call engine_nth_index(buf, nrows, nth, "pf_nth_element", idx, threads=threads)
        call values%get(idx, p_value, allow_null=.true.)
    end subroutine nth_impl_strcol
    !
    !> Shared worker behind every pf_nth_quantile specific for a packed string column array.
    subroutine quantile_impl_strcol(values, quantile, p_value, idx, n_null, rounding, threads, ok)
        type(parquet_string_column), intent(in) :: values
        real(real64), intent(in) :: quantile !! position on a 0-1 scale.
        character(len=:), allocatable, intent(out) :: p_value !! the value at that quantile.
        integer(int64), intent(out) :: idx     !! which element of `values` that was.
        integer(int64), intent(out) :: n_null  !! how many values were null.
        character(len=*), intent(in), optional :: rounding !! rounding token.
        integer, intent(in), optional :: threads !! thread request; absent = the automatic policy.
        logical, intent(out), optional :: ok !! .false. when every value was null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: nrows, n_valid, rank
        integer :: mode
        !
        call resolve_rounding(rounding, "pf_nth_quantile", mode)
        nrows = values%size()
        ! Ascending with nulls LAST, unconditionally: the population is the non-null
        ! values, so a rank in 1..n_valid can never address a null.
        call extract_strcol(values, buf, .false., .false., "pf_nth_quantile", threads=threads)
        call key_valid_count(buf, nrows, n_valid)
        n_null = nrows - n_valid
        ! quantile_rank owns BOTH the argument validation and the empty-population
        ! decision, which is what keeps the abort's message in one place and keeps a
        ! bad `quantile` aborting even when `ok` was passed.
        call quantile_rank(quantile, n_valid, mode, "pf_nth_quantile", rank, ok=ok)
        if (present(ok)) then
            if (.not. ok) return   ! rank is unset on this path
        end if
        call engine_nth_index(buf, nrows, rank, "pf_nth_quantile", idx, threads=threads)
        call values%get(idx, p_value, allow_null=.true.)
    end subroutine quantile_impl_strcol
    !
    !> Narrows a 1-based int64 index to int32, aborting rather than truncating.
    subroutine narrow_index(idx64, proc, idx32)
        integer(int64), intent(in) :: idx64  !! the index.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int32), intent(out) :: idx32 !! the narrowed copy.
        character(len=32) :: n_str
        !
        if (idx64 > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array
            ! that would trip it cannot be built by any fixture this repository can run.
            ! Kept because the alternative is a silent truncation into a wrong index.
            write (n_str, "(i0)") idx64
            error stop EP // proc // ": the answer is at element " // trim(n_str) // &
                ", which does not fit an int32 index; declare index as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        idx32 = int(idx64, int32)
    end subroutine narrow_index
    !
end submodule parquet_sorting_select ! GCOVR_EXCL_LINE
