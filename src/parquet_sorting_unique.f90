!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_unique_count`, `pf_unique` and `pf_rank` -- questions about repeated values.
!!
!! All three rest on one engine call (`engine_build_runs`), which sorts and reports where the runs
!! of EQUAL rows are in the same pass. Equality is the sort comparator's own, minus the index
!! tiebreaker that makes it a total order -- so "distinct" here means exactly "the sort would not
!! have to choose between them", and two NaNs are one value even though `==` says otherwise.
!!
!! **Nulls are outside the population, in all three.** They are excluded from a count, excluded
!! from the distinct values, and given rank 0 rather than a place in the ranking. That is why none
!! of the three takes `nulls_first`: there is no null tier to position. Extraction is therefore
!! always `nulls_first=.false.`, which puts every null last and contiguous -- the property the
!! walks below rely on to stop counting.
submodule (parquet_sorting) parquet_sorting_unique
    implicit none
    !
contains
    !
    module procedure unique_count_i32_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_i32(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_i32_i32
    !
    module procedure unique_count_i32_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_i32(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_i32_i64
    !
    module procedure unique_count_i64_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_i64(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_i64_i32
    !
    module procedure unique_count_i64_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_i64(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_i64_i64
    !
    module procedure unique_count_f32_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_f32(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_f32_i32
    !
    module procedure unique_count_f32_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_f32(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_f32_i64
    !
    module procedure unique_count_f64_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_f64(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_f64_i32
    !
    module procedure unique_count_f64_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_f64(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_f64_i64
    !
    module procedure unique_count_bool_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_bool(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_bool_i32
    !
    module procedure unique_count_bool_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_bool(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_bool_i64
    !
    module procedure unique_count_chr_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_chr(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_chr_i32
    !
    module procedure unique_count_chr_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_chr(values, .false., "pf_unique_count", idxs, nd, nn, is_valid=is_valid, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_chr_i64
    !
    module procedure unique_count_date_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_date(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_date_i32
    !
    module procedure unique_count_date_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_date(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_date_i64
    !
    module procedure unique_count_time_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_time(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_time_i32
    !
    module procedure unique_count_time_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_time(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_time_i64
    !
    module procedure unique_count_ts_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_ts(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_ts_i32
    !
    module procedure unique_count_ts_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_ts(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_ts_i64
    !
    module procedure unique_count_strcol_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_strcol(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_strcol_i32
    !
    module procedure unique_count_strcol_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_strcol(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_strcol_i64
    !
    module procedure unique_count_col_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_col(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        call narrow_i64(nd, "pf_unique_count", "distinct-value count", count)
    end procedure unique_count_col_i32
    !
    module procedure unique_count_col_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: nd, nn
        !
        call unique_impl_col(values, .false., "pf_unique_count", idxs, nd, nn, &
            threads=threads)
        if (present(n_null)) n_null = nn
        count = nd
    end procedure unique_count_col_i64
    !
    module procedure unique_i32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_i32(values, desc, "pf_unique", idxs, nd, nn, is_valid=is_valid, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_i32
    !
    module procedure unique_i64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_i64(values, desc, "pf_unique", idxs, nd, nn, is_valid=is_valid, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_i64
    !
    module procedure unique_f32
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_f32(values, desc, "pf_unique", idxs, nd, nn, is_valid=is_valid, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_f32
    !
    module procedure unique_f64
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_f64(values, desc, "pf_unique", idxs, nd, nn, is_valid=is_valid, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_f64
    !
    module procedure unique_bool
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_bool(values, desc, "pf_unique", idxs, nd, nn, is_valid=is_valid, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_bool
    !
    module procedure unique_chr
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_chr(values, desc, "pf_unique", idxs, nd, nn, is_valid=is_valid, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(character(len=len(values)) :: distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_chr
    !
    module procedure unique_date
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_date(values, desc, "pf_unique", idxs, nd, nn, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_date
    !
    module procedure unique_time
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_time(values, desc, "pf_unique", idxs, nd, nn, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_time
    !
    module procedure unique_ts
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_ts(values, desc, "pf_unique", idxs, nd, nn, threads=threads)
        if (present(n_null)) n_null = nn
        allocate(distinct(nd))
        do k = 1_int64, nd
            distinct(k) = values(idxs(k))
        end do
    end procedure unique_ts
    !
    module procedure unique_strcol
        integer(int64), allocatable :: idxs(:)
        integer(int64) :: k, nd, nn
        logical :: desc
        character(len=:), allocatable :: s
        !
        desc = .false.
        if (present(descending)) desc = descending
        call unique_impl_strcol(values, desc, "pf_unique", idxs, nd, nn, threads=threads)
        if (present(n_null)) n_null = nn
        call distinct%clear()
        do k = 1_int64, nd
            call values%get(idxs(k), s, allow_null=.true.)
            call distinct%append_string(s)
        end do
    end procedure unique_strcol
    !
    module procedure rank_i32_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_i32(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_i32_i32
    !
    module procedure rank_i32_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_i32(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_i32_i64
    !
    module procedure rank_i64_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_i64(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_i64_i32
    !
    module procedure rank_i64_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_i64(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_i64_i64
    !
    module procedure rank_f32_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_f32(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_f32_i32
    !
    module procedure rank_f32_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_f32(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_f32_i64
    !
    module procedure rank_f64_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_f64(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_f64_i32
    !
    module procedure rank_f64_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_f64(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_f64_i64
    !
    module procedure rank_bool_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_bool(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_bool_i32
    !
    module procedure rank_bool_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_bool(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_bool_i64
    !
    module procedure rank_chr_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_chr(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_chr_i32
    !
    module procedure rank_chr_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_chr(values, method, desc, "pf_rank", r64, is_valid=is_valid, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_chr_i64
    !
    module procedure rank_date_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_date(values, method, desc, "pf_rank", r64, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_date_i32
    !
    module procedure rank_date_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_date(values, method, desc, "pf_rank", r64, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_date_i64
    !
    module procedure rank_time_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_time(values, method, desc, "pf_rank", r64, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_time_i32
    !
    module procedure rank_time_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_time(values, method, desc, "pf_rank", r64, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_time_i64
    !
    module procedure rank_ts_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_ts(values, method, desc, "pf_rank", r64, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_ts_i32
    !
    module procedure rank_ts_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_ts(values, method, desc, "pf_rank", r64, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_ts_i64
    !
    module procedure rank_strcol_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_strcol(values, method, desc, "pf_rank", r64, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_strcol_i32
    !
    module procedure rank_strcol_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_strcol(values, method, desc, "pf_rank", r64, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_strcol_i64
    !
    module procedure rank_col_i32
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_col(values, method, desc, "pf_rank", r64, threads=threads)
        call narrow_i64_array(r64, "pf_rank", "rank", ranks)
    end procedure rank_col_i32
    !
    module procedure rank_col_i64
        integer(int64), allocatable :: r64(:)
        logical :: desc
        !
        desc = .false.
        if (present(descending)) desc = descending
        call rank_impl_col(values, method, desc, "pf_rank", r64, threads=threads)
        call move_alloc(r64, ranks)
    end procedure rank_col_i64
    !
    !> Shared worker behind pf_unique_count and pf_unique for a 32-bit integer array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_i32(values, descending, proc, first_idx, ndist, nnull, is_valid, threads)
        integer(int32), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_i32(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_i32
    !
    !> Shared worker behind every pf_rank specific for a 32-bit integer array.
    subroutine rank_impl_i32(values, method, descending, proc, ranks, is_valid, threads)
        integer(int32), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_i32(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_i32
    !
    !> Shared worker behind pf_unique_count and pf_unique for a 64-bit integer array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_i64(values, descending, proc, first_idx, ndist, nnull, is_valid, threads)
        integer(int64), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_i64(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_i64
    !
    !> Shared worker behind every pf_rank specific for a 64-bit integer array.
    subroutine rank_impl_i64(values, method, descending, proc, ranks, is_valid, threads)
        integer(int64), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_i64(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_i64
    !
    !> Shared worker behind pf_unique_count and pf_unique for a 32-bit real array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_f32(values, descending, proc, first_idx, ndist, nnull, is_valid, threads)
        real(real32), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_f32(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_f32
    !
    !> Shared worker behind every pf_rank specific for a 32-bit real array.
    subroutine rank_impl_f32(values, method, descending, proc, ranks, is_valid, threads)
        real(real32), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_f32(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_f32
    !
    !> Shared worker behind pf_unique_count and pf_unique for a 64-bit real array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_f64(values, descending, proc, first_idx, ndist, nnull, is_valid, threads)
        real(real64), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_f64(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_f64
    !
    !> Shared worker behind every pf_rank specific for a 64-bit real array.
    subroutine rank_impl_f64(values, method, descending, proc, ranks, is_valid, threads)
        real(real64), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_f64(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_f64
    !
    !> Shared worker behind pf_unique_count and pf_unique for a logical array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_bool(values, descending, proc, first_idx, ndist, nnull, is_valid, threads)
        logical, intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_bool(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_bool
    !
    !> Shared worker behind every pf_rank specific for a logical array.
    subroutine rank_impl_bool(values, method, descending, proc, ranks, is_valid, threads)
        logical, intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_bool(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_bool
    !
    !> Shared worker behind pf_unique_count and pf_unique for a string array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_chr(values, descending, proc, first_idx, ndist, nnull, is_valid, threads)
        character(len=*), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_chr(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_chr
    !
    !> Shared worker behind every pf_rank specific for a string array.
    subroutine rank_impl_chr(values, method, descending, proc, ranks, is_valid, threads)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_chr(values, buf, descending, .false., proc, is_valid=is_valid)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_chr
    !
    !> Shared worker behind pf_unique_count and pf_unique for a date array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_date(values, descending, proc, first_idx, ndist, nnull, threads)
        type(parquet_date), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_date(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_date
    !
    !> Shared worker behind every pf_rank specific for a date array.
    subroutine rank_impl_date(values, method, descending, proc, ranks, threads)
        type(parquet_date), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_date(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_date
    !
    !> Shared worker behind pf_unique_count and pf_unique for a time array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_time(values, descending, proc, first_idx, ndist, nnull, threads)
        type(parquet_time), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_time(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_time
    !
    !> Shared worker behind every pf_rank specific for a time array.
    subroutine rank_impl_time(values, method, descending, proc, ranks, threads)
        type(parquet_time), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_time(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_time
    !
    !> Shared worker behind pf_unique_count and pf_unique for a timestamp array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_ts(values, descending, proc, first_idx, ndist, nnull, threads)
        type(parquet_timestamp), intent(in) :: values(:)
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = size(values, kind=int64)
        call extract_ts(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_ts
    !
    !> Shared worker behind every pf_rank specific for a timestamp array.
    subroutine rank_impl_ts(values, method, descending, proc, ranks, threads)
        type(parquet_timestamp), intent(in) :: values(:)
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = size(values, kind=int64)
        call extract_ts(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_ts
    !
    !> Shared worker behind pf_unique_count and pf_unique for a packed string column array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_strcol(values, descending, proc, first_idx, ndist, nnull, threads)
        type(parquet_string_column), intent(in) :: values
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = values%size()
        call extract_strcol(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_strcol
    !
    !> Shared worker behind every pf_rank specific for a packed string column array.
    subroutine rank_impl_strcol(values, method, descending, proc, ranks, threads)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = values%size()
        call extract_strcol(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_strcol
    !
    !> Shared worker behind pf_unique_count and pf_unique for a type-erased column array: the
    !! 1-based index of the FIRST occurrence of each distinct non-null value, in order.
    subroutine unique_impl_col(values, descending, proc, first_idx, ndist, nnull, threads)
        type(parquet_column), intent(in) :: values
        logical, intent(in) :: descending    !! .true. reports the distinct values high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: first_idx(:) !! where each distinct value is.
        integer(int64), intent(out) :: ndist !! how many distinct non-null values.
        integer(int64), intent(out) :: nnull !! how many values were null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, n
        !
        n = values%length()
        call extract_col(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(first_idx(max(n, 1_int64)))
        first_idx = 0_int64
        ndist = 0_int64
        nnull = 0_int64
        do k = 1_int64, n
            ! nulls_first=.false. puts every null in the last tier, so the first one ends
            ! the walk and the rest of the array is exactly the null count.
            if (isnull(perm(k))) then
                nnull = n - k + 1_int64
                exit
            end if
            if (tie(k) == 0_c_int8_t) then
                ndist = ndist + 1_int64
                first_idx(ndist) = perm(k)
            end if
        end do
    end subroutine unique_impl_col
    !
    !> Shared worker behind every pf_rank specific for a type-erased column array.
    subroutine rank_impl_col(values, method, descending, proc, ranks, threads)
        type(parquet_column), intent(in) :: values
        character(len=*), intent(in), optional :: method !! tie-handling token.
        logical, intent(in) :: descending    !! .true. ranks high to low.
        character(len=*), intent(in) :: proc !! calling procedure, for messages.
        integer(int64), allocatable, intent(out) :: ranks(:) !! rank of each element; 0 for a null.
        integer, intent(in), optional :: threads !! thread request; absent = auto.
        type(sort_key_buf), allocatable :: buf(:)
        integer(int64), allocatable :: perm(:)
        integer(c_int8_t), allocatable :: tie(:)
        logical, allocatable :: isnull(:)
        integer(int64) :: k, i, n, seen, dense, run_start
        integer :: mode
        !
        call resolve_rank_method(method, proc, mode)
        n = values%length()
        call extract_col(values, buf, descending, .false., proc)
        call engine_build_runs(buf, n, proc, perm, tie, threads=threads)
        call key_null_mask(buf, n, isnull)
        allocate(ranks(n))
        if (n < 1_int64) return
        ranks = 0_int64
        seen = 0_int64
        dense = 0_int64
        run_start = 0_int64
        do k = 1_int64, n
            i = perm(k)
            ! A null keeps rank 0. `cycle` rather than `exit` even though the nulls are
            ! contiguous at the end: the walk should not depend on that placement twice.
            if (isnull(i)) cycle
            if (tie(k) == 0_c_int8_t) then
                run_start = seen + 1_int64
                dense = dense + 1_int64
            end if
            seen = seen + 1_int64
            select case (mode)
            case (RANK_DENSE)
                ranks(i) = dense
            case (RANK_ORDINAL)
                ranks(i) = seen
            case default
                ranks(i) = run_start
            end select
        end do
    end subroutine rank_impl_col
    !
end submodule parquet_sorting_unique ! GCOVR_EXCL_LINE
