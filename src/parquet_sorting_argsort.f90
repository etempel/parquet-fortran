!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_argsort` and `pf_sort` for the element types that need a parquet column, a packed string
!! store or a temporal element, plus `pf_sort` for every type and the multi-key `pf_sort_keys` form.
!!
!! The six intrinsic types' `pf_argsort` specifics are one tier down, in
!! `src/parquet_argsort_kernel.f90`.
!!
!! Both are thin: they extract the values into the engine's canonical key form, run the engine, and
!! -- for `pf_sort` -- gather the result. No ordering decision is made here.
!!
!! **The int32 permutation forms exist because a caller with a default-kind `INTEGER` should not be
!! forced to widen one** (`.claude/rules/api-conventions.md`'s "Numeric arguments: int32 and int64"). They compute in int64 and narrow
!! at the end, aborting rather than truncating when the array is longer than `huge(1_int32)`.
submodule (parquet_sorting) parquet_sorting_argsort
    implicit none
    !
contains
    !
    module procedure argsort_date_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_date(values, buf, desc, nlo, "pf_argsort", threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_date_i32
    !
    module procedure argsort_date_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_date(values, buf, desc, nlo, "pf_argsort", threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_date_i64
    !
    module procedure argsort_time_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_time(values, buf, desc, nlo, "pf_argsort", threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_time_i32
    !
    module procedure argsort_time_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_time(values, buf, desc, nlo, "pf_argsort", threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_time_i64
    !
    module procedure argsort_ts_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_ts(values, buf, desc, nlo, "pf_argsort", threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_ts_i32
    !
    module procedure argsort_ts_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_ts(values, buf, desc, nlo, "pf_argsort", threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_ts_i64
    !
    module procedure argsort_strcol_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_strcol(values, buf, desc, nlo, "pf_argsort", threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, values%size(), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, values%size(), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_strcol_i32
    !
    module procedure argsort_strcol_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_strcol(values, buf, desc, nlo, "pf_argsort", threads=threads)
        call drive_engine_grouped(buf, values%size(), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_strcol_i64
    !
    module procedure argsort_col_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_col(values, buf, desc, nlo, "pf_argsort", threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, values%length(), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, values%length(), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_col_i32
    !
    module procedure argsort_col_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_col(values, buf, desc, nlo, "pf_argsort", threads=threads)
        call drive_engine_grouped(buf, values%length(), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_col_i64
    !
    module procedure argsort_keys_i32
        integer :: gek
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        if (keys%nkeys < 1) then
            error stop EP // "pf_argsort: this pf_sort_keys has no key; " // &
                "call keys%add(...) at least once before sorting"
        end if
        call resolve_group_nkeys(keys, group_nkeys, present(group_offsets), "pf_argsort", gek)
        if (present(group_offsets)) then
            call drive_engine_grouped(keys%keys(1:keys%nkeys), keys%nrows, "pf_argsort", &
                perm64, threads=threads, group_offsets=go64, group_ekeys=gek)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(keys%keys(1:keys%nkeys), keys%nrows, "pf_argsort", &
                perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_keys_i32
    !
    module procedure argsort_keys_i64
        integer :: gek
        !
        if (keys%nkeys < 1) then
            error stop EP // "pf_argsort: this pf_sort_keys has no key; " // &
                "call keys%add(...) at least once before sorting"
        end if
        call resolve_group_nkeys(keys, group_nkeys, present(group_offsets), "pf_argsort", gek)
        call drive_engine_grouped(keys%keys(1:keys%nkeys), keys%nrows, "pf_argsort", perm, &
            threads=threads, group_offsets=group_offsets, group_ekeys=gek)
    end procedure argsort_keys_i64
    !
    module procedure sort_i32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_i32(values, buf, desc, nlo, "pf_sort", is_valid=is_valid, threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's
        ! "unallocated means no nulls" convention governs an INPUT, where unallocated is
        ! the caller declining to supply information; an output they explicitly asked
        ! for is a direct question, and answering it with an unallocated array would
        ! force `if (allocated(...))` around every use.
        if (present(sorted_valid)) then
            allocate(sorted_valid(n))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, n
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure sort_i32
    !
    module procedure sort_i64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_i64(values, buf, desc, nlo, "pf_sort", is_valid=is_valid, threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's
        ! "unallocated means no nulls" convention governs an INPUT, where unallocated is
        ! the caller declining to supply information; an output they explicitly asked
        ! for is a direct question, and answering it with an unallocated array would
        ! force `if (allocated(...))` around every use.
        if (present(sorted_valid)) then
            allocate(sorted_valid(n))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, n
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure sort_i64
    !
    module procedure sort_f32
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_f32(values, buf, desc, nlo, "pf_sort", is_valid=is_valid, threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's
        ! "unallocated means no nulls" convention governs an INPUT, where unallocated is
        ! the caller declining to supply information; an output they explicitly asked
        ! for is a direct question, and answering it with an unallocated array would
        ! force `if (allocated(...))` around every use.
        if (present(sorted_valid)) then
            allocate(sorted_valid(n))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, n
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure sort_f32
    !
    module procedure sort_f64
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_f64(values, buf, desc, nlo, "pf_sort", is_valid=is_valid, threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's
        ! "unallocated means no nulls" convention governs an INPUT, where unallocated is
        ! the caller declining to supply information; an output they explicitly asked
        ! for is a direct question, and answering it with an unallocated array would
        ! force `if (allocated(...))` around every use.
        if (present(sorted_valid)) then
            allocate(sorted_valid(n))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, n
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure sort_f64
    !
    module procedure sort_bool
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_bool(values, buf, desc, nlo, "pf_sort", is_valid=is_valid, threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's
        ! "unallocated means no nulls" convention governs an INPUT, where unallocated is
        ! the caller declining to supply information; an output they explicitly asked
        ! for is a direct question, and answering it with an unallocated array would
        ! force `if (allocated(...))` around every use.
        if (present(sorted_valid)) then
            allocate(sorted_valid(n))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, n
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure sort_bool
    !
    module procedure sort_chr
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_chr(values, buf, desc, nlo, "pf_sort", is_valid=is_valid, threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(character(len=len(values)) :: sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
        ! Deliberately ALLOCATED even when `is_valid` was absent. The module's
        ! "unallocated means no nulls" convention governs an INPUT, where unallocated is
        ! the caller declining to supply information; an output they explicitly asked
        ! for is a direct question, and answering it with an unallocated array would
        ! force `if (allocated(...))` around every use.
        if (present(sorted_valid)) then
            allocate(sorted_valid(n))
            sorted_valid = .true.
            if (present(is_valid)) then
                do k = 1_int64, n
                    sorted_valid(k) = is_valid(perm(k))
                end do
            end if
        end if
    end procedure sort_chr
    !
    module procedure sort_date
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_date(values, buf, desc, nlo, "pf_sort", threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
    end procedure sort_date
    !
    module procedure sort_time
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_time(values, buf, desc, nlo, "pf_sort", threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
    end procedure sort_time
    !
    module procedure sort_ts
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, n
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        n = size(values, kind=int64)
        call extract_ts(values, buf, desc, nlo, "pf_sort", threads=threads)
        call drive_engine(buf, n, "pf_sort", perm, threads=threads)
        allocate(sorted(n))
        do k = 1_int64, n
            sorted(k) = values(perm(k))
        end do
    end procedure sort_ts
    !
end submodule parquet_sorting_argsort ! GCOVR_EXCL_LINE
