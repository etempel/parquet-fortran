!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_minmax`, `pf_argminmax` and `pf_merge`.
!!
!! **The extremes are two `nth_element` calls, not a hand-written scan.** Rank 1 ascending is the
!! minimum and rank 1 DESCENDING is the maximum, so reaching them through the engine means the
!! answers cannot disagree with `pf_sort`'s own ends (`feature_risks.md` Risk-34). Both calls are
!! O(n), the same as the scan would be, and neither needs a per-type comparison written here.
!!
!! Rank `n_value` of the ascending order would name the same maximum VALUE, but the last of a tied
!! run rather than the first -- see `minmax_impl_*`'s own comment for why that asymmetry is not
!! acceptable in a pair of answers a caller reads together.
!!
!! `n_value` counts rows that are neither null nor NaN. A NaN is skipped because it is not the
!! minimum or maximum of anything -- while staying an ordinary value everywhere else in this
!! module, which is exactly the asymmetry `sort_tier_of` already encodes.
!!
!! **`pf_merge` concatenates the two inputs into one key and merges the halves.** The alternative,
!! comparing an element of `a` against an element of `b` through a second comparison path, is the
!! drift this module has spent three milestones avoiding.
submodule (parquet_sorting) parquet_sorting_reduce
    implicit none
    !
contains
    !
    module procedure minmax_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_i32(values, "pf_minmax", i1, i2, is_valid=is_valid)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_i32
    !
    module procedure minmax_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_i64(values, "pf_minmax", i1, i2, is_valid=is_valid)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_i64
    !
    module procedure minmax_f32
        integer(int64) :: i1, i2
        !
        call minmax_impl_f32(values, "pf_minmax", i1, i2, is_valid=is_valid)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_f32
    !
    module procedure minmax_f64
        integer(int64) :: i1, i2
        !
        call minmax_impl_f64(values, "pf_minmax", i1, i2, is_valid=is_valid)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_f64
    !
    module procedure minmax_chr
        integer(int64) :: i1, i2
        !
        call minmax_impl_chr(values, "pf_minmax", i1, i2, is_valid=is_valid)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_chr
    !
    module procedure minmax_date
        integer(int64) :: i1, i2
        !
        call minmax_impl_date(values, "pf_minmax", i1, i2)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_date
    !
    module procedure minmax_time
        integer(int64) :: i1, i2
        !
        call minmax_impl_time(values, "pf_minmax", i1, i2)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_time
    !
    module procedure minmax_ts
        integer(int64) :: i1, i2
        !
        call minmax_impl_ts(values, "pf_minmax", i1, i2)
        vmin = values(i1)
        vmax = values(i2)
    end procedure minmax_ts
    !
    module procedure minmax_strcol
        integer(int64) :: i1, i2
        !
        call minmax_impl_strcol(values, "pf_minmax", i1, i2)
        call values%get(i1, vmin, allow_null=.true.)
        call values%get(i2, vmax, allow_null=.true.)
    end procedure minmax_strcol
    !
    module procedure argminmax_i32_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_i32(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_i32_i32
    !
    module procedure argminmax_i32_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_i32(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        imin = i1
        imax = i2
    end procedure argminmax_i32_i64
    !
    module procedure argminmax_i64_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_i64(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_i64_i32
    !
    module procedure argminmax_i64_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_i64(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        imin = i1
        imax = i2
    end procedure argminmax_i64_i64
    !
    module procedure argminmax_f32_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_f32(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_f32_i32
    !
    module procedure argminmax_f32_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_f32(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        imin = i1
        imax = i2
    end procedure argminmax_f32_i64
    !
    module procedure argminmax_f64_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_f64(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_f64_i32
    !
    module procedure argminmax_f64_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_f64(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        imin = i1
        imax = i2
    end procedure argminmax_f64_i64
    !
    module procedure argminmax_chr_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_chr(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_chr_i32
    !
    module procedure argminmax_chr_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_chr(values, "pf_argminmax", i1, i2, is_valid=is_valid)
        imin = i1
        imax = i2
    end procedure argminmax_chr_i64
    !
    module procedure argminmax_date_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_date(values, "pf_argminmax", i1, i2)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_date_i32
    !
    module procedure argminmax_date_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_date(values, "pf_argminmax", i1, i2)
        imin = i1
        imax = i2
    end procedure argminmax_date_i64
    !
    module procedure argminmax_time_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_time(values, "pf_argminmax", i1, i2)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_time_i32
    !
    module procedure argminmax_time_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_time(values, "pf_argminmax", i1, i2)
        imin = i1
        imax = i2
    end procedure argminmax_time_i64
    !
    module procedure argminmax_ts_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_ts(values, "pf_argminmax", i1, i2)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_ts_i32
    !
    module procedure argminmax_ts_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_ts(values, "pf_argminmax", i1, i2)
        imin = i1
        imax = i2
    end procedure argminmax_ts_i64
    !
    module procedure argminmax_strcol_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_strcol(values, "pf_argminmax", i1, i2)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_strcol_i32
    !
    module procedure argminmax_strcol_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_strcol(values, "pf_argminmax", i1, i2)
        imin = i1
        imax = i2
    end procedure argminmax_strcol_i64
    !
    module procedure argminmax_col_i32
        integer(int64) :: i1, i2
        !
        call minmax_impl_col(values, "pf_argminmax", i1, i2)
        call narrow_i64(i1, "pf_argminmax", "index of the smallest value", imin)
        call narrow_i64(i2, "pf_argminmax", "index of the largest value", imax)
    end procedure argminmax_col_i32
    !
    module procedure argminmax_col_i64
        integer(int64) :: i1, i2
        !
        call minmax_impl_col(values, "pf_argminmax", i1, i2)
        imin = i1
        imax = i2
    end procedure argminmax_col_i64
    !
    module procedure merge_i32
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_i32(a, bufa, desc, nlo, "pf_merge", is_valid=is_valid_a)
        call extract_i32(b, bufb, desc, nlo, "pf_merge", is_valid=is_valid_b)
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
        ! Always allocated when asked for, all .true. when neither input mask was
        ! supplied -- the same rule pf_sort's own sorted_valid follows.
        if (present(merged_valid)) then
            allocate(merged_valid(n))
            merged_valid = .true.
            do k = 1_int64, n
                j = perm(k)
                if (j <= na) then
                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)
                else
                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)
                end if
            end do
        end if
    end procedure merge_i32
    !
    module procedure merge_i64
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_i64(a, bufa, desc, nlo, "pf_merge", is_valid=is_valid_a)
        call extract_i64(b, bufb, desc, nlo, "pf_merge", is_valid=is_valid_b)
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
        ! Always allocated when asked for, all .true. when neither input mask was
        ! supplied -- the same rule pf_sort's own sorted_valid follows.
        if (present(merged_valid)) then
            allocate(merged_valid(n))
            merged_valid = .true.
            do k = 1_int64, n
                j = perm(k)
                if (j <= na) then
                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)
                else
                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)
                end if
            end do
        end if
    end procedure merge_i64
    !
    module procedure merge_f32
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_f32(a, bufa, desc, nlo, "pf_merge", is_valid=is_valid_a)
        call extract_f32(b, bufb, desc, nlo, "pf_merge", is_valid=is_valid_b)
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
        ! Always allocated when asked for, all .true. when neither input mask was
        ! supplied -- the same rule pf_sort's own sorted_valid follows.
        if (present(merged_valid)) then
            allocate(merged_valid(n))
            merged_valid = .true.
            do k = 1_int64, n
                j = perm(k)
                if (j <= na) then
                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)
                else
                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)
                end if
            end do
        end if
    end procedure merge_f32
    !
    module procedure merge_f64
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_f64(a, bufa, desc, nlo, "pf_merge", is_valid=is_valid_a)
        call extract_f64(b, bufb, desc, nlo, "pf_merge", is_valid=is_valid_b)
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
        ! Always allocated when asked for, all .true. when neither input mask was
        ! supplied -- the same rule pf_sort's own sorted_valid follows.
        if (present(merged_valid)) then
            allocate(merged_valid(n))
            merged_valid = .true.
            do k = 1_int64, n
                j = perm(k)
                if (j <= na) then
                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)
                else
                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)
                end if
            end do
        end if
    end procedure merge_f64
    !
    module procedure merge_bool
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_bool(a, bufa, desc, nlo, "pf_merge", is_valid=is_valid_a)
        call extract_bool(b, bufb, desc, nlo, "pf_merge", is_valid=is_valid_b)
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
        ! Always allocated when asked for, all .true. when neither input mask was
        ! supplied -- the same rule pf_sort's own sorted_valid follows.
        if (present(merged_valid)) then
            allocate(merged_valid(n))
            merged_valid = .true.
            do k = 1_int64, n
                j = perm(k)
                if (j <= na) then
                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)
                else
                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)
                end if
            end do
        end if
    end procedure merge_bool
    !
    module procedure merge_chr
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        character(len=max(len(a), len(b))), allocatable :: pa(:), pb(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        ! Both halves are widened to one common element length before extraction, or
        ! the packed keys would compare strings of two different widths against each
        ! other. Element by element, because a whole-array assignment into an
        ! allocatable is the reallocation hazard CLAUDE.md documents.
        allocate(pa(na), pb(nb))
        do k = 1_int64, na
            pa(k) = a(k)
        end do
        do k = 1_int64, nb
            pb(k) = b(k)
        end do
        call extract_chr(pa, bufa, desc, nlo, "pf_merge", is_valid=is_valid_a)
        call extract_chr(pb, bufb, desc, nlo, "pf_merge", is_valid=is_valid_b)
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(character(len=max(len(a), len(b))) :: merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
        ! Always allocated when asked for, all .true. when neither input mask was
        ! supplied -- the same rule pf_sort's own sorted_valid follows.
        if (present(merged_valid)) then
            allocate(merged_valid(n))
            merged_valid = .true.
            do k = 1_int64, n
                j = perm(k)
                if (j <= na) then
                    if (present(is_valid_a)) merged_valid(k) = is_valid_a(j)
                else
                    if (present(is_valid_b)) merged_valid(k) = is_valid_b(j - na)
                end if
            end do
        end if
    end procedure merge_chr
    !
    module procedure merge_date
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_date(a, bufa, desc, nlo, "pf_merge")
        call extract_date(b, bufb, desc, nlo, "pf_merge")
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
    end procedure merge_date
    !
    module procedure merge_time
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_time(a, bufa, desc, nlo, "pf_merge")
        call extract_time(b, bufb, desc, nlo, "pf_merge")
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
    end procedure merge_time
    !
    module procedure merge_ts
        type(sort_key_buf), allocatable :: bufa(:), bufb(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, j, na, nb, n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        na = size(a, kind=int64)
        nb = size(b, kind=int64)
        n = na + nb
        call extract_ts(a, bufa, desc, nlo, "pf_merge")
        call extract_ts(b, bufb, desc, nlo, "pf_merge")
        if (check) then
            call check_sorted_input(bufa, na, "pf_merge", "a")
            call check_sorted_input(bufb, nb, "pf_merge", "b")
        end if
        call buf_append(bufa, na, bufb, nb, "pf_merge")
        call engine_merge(bufa, n, na, "pf_merge", perm)
        allocate(merged(n))
        do k = 1_int64, n
            if (perm(k) <= na) then
                merged(k) = a(perm(k))
            else
                merged(k) = b(perm(k) - na)
            end if
        end do
    end procedure merge_ts
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a 32-bit integer array.
    subroutine minmax_impl_i32(values, proc, imin, imax, is_valid)
        integer(int32), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_i32(values, buf, .false., .false., proc, is_valid=is_valid)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_i32
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a 64-bit integer array.
    subroutine minmax_impl_i64(values, proc, imin, imax, is_valid)
        integer(int64), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_i64(values, buf, .false., .false., proc, is_valid=is_valid)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_i64
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a 32-bit real array.
    subroutine minmax_impl_f32(values, proc, imin, imax, is_valid)
        real(real32), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_f32(values, buf, .false., .false., proc, is_valid=is_valid)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_f32
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a 64-bit real array.
    subroutine minmax_impl_f64(values, proc, imin, imax, is_valid)
        real(real64), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_f64(values, buf, .false., .false., proc, is_valid=is_valid)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_f64
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a string array.
    subroutine minmax_impl_chr(values, proc, imin, imax, is_valid)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_chr(values, buf, .false., .false., proc, is_valid=is_valid)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_chr
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a date array.
    subroutine minmax_impl_date(values, proc, imin, imax)
        type(parquet_date), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_date(values, buf, .false., .false., proc)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_date
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a time array.
    subroutine minmax_impl_time(values, proc, imin, imax)
        type(parquet_time), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_time(values, buf, .false., .false., proc)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_time
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a timestamp array.
    subroutine minmax_impl_ts(values, proc, imin, imax)
        type(parquet_timestamp), intent(in) :: values(:)
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = size(values, kind=int64)
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_ts(values, buf, .false., .false., proc)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_ts
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a packed string column array.
    subroutine minmax_impl_strcol(values, proc, imin, imax)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = values%size()
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_strcol(values, buf, .false., .false., proc)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_strcol
    !
    !> Shared worker behind pf_minmax and pf_argminmax for a type-erased column array.
    subroutine minmax_impl_col(values, proc, imin, imax)
        type(parquet_column), intent(in) :: values
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), intent(out) :: imin  !! where the smallest value is.
        integer(int64), intent(out) :: imax  !! where the largest value is.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64) :: n, n_value
        !
        n = values%length()
        ! Ascending with nulls LAST, unconditionally: the population is the values, so
        ! ranks 1 and n_value address them and nothing else.
        call extract_col(values, buf, .false., .false., proc)
        call key_value_count(buf, n, n_value)
        if (n_value < 1_int64) then
            error stop EP // proc // ": every value is null or NaN, so there is no " // &
                "minimum or maximum; guard with count(is_valid) (or the column's own " // &
                "null count) if that can happen"
        end if
        call engine_nth_index(buf, n, 1_int64, proc, imin)
        ! The maximum is rank 1 of the DESCENDING order, not rank n_value of the
        ! ascending one. Both name the same value, but a stable ascending sort puts the
        ! LAST of a tied run at the end, so rank n_value would report the last equal
        ! maximum while imin reported the first equal minimum -- the same call answering
        ! two different questions at the two ends. Flipping the key's own direction keeps
        ! both as "rank 1", so both report the first occurrence. Tiers are absolute, so
        ! this moves no null and no NaN out of the way of rank 1.
        buf(:)%descending = .true.
        call engine_nth_index(buf, n, 1_int64, proc, imax)
    end subroutine minmax_impl_col
    !
end submodule parquet_sorting_reduce ! GCOVR_EXCL_LINE
