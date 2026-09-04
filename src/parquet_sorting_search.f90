!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> `pf_lower_bound`, `pf_upper_bound` and `pf_equal_range` -- locating a value in an array that
!! is ALREADY sorted.
!!
!! **The target is appended to the array's own key and compared as row n+1.** That is the whole
!! design: there is no compare-a-row-against-a-value arm anywhere, so a search cannot drift from
!! the order `pf_sort` produces (`feature_risks.md` Risk-34). It costs one element of copy.
!!
!! **Searching unsorted input is the worst failure this module can have** -- a plausible index, no
!! abort, no symptom. So sortedness is checked by default, at O(n) in front of an O(log n) search,
!! and `assume_sorted=.true.` is the caller's explicit statement that they have established the
!! order themselves. Do not flip that default.
submodule (parquet_sorting) parquet_sorting_search
    implicit none
    !
contains
    !
    module procedure lower_bound_i32_i32
        integer(int64) :: lo, hi
        !
        call search_impl_i32(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_i32_i32
    !
    module procedure lower_bound_i32_i64
        integer(int64) :: lo, hi
        !
        call search_impl_i32(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        pos = lo
    end procedure lower_bound_i32_i64
    !
    module procedure lower_bound_i64_i32
        integer(int64) :: lo, hi
        !
        call search_impl_i64(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_i64_i32
    !
    module procedure lower_bound_i64_i64
        integer(int64) :: lo, hi
        !
        call search_impl_i64(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        pos = lo
    end procedure lower_bound_i64_i64
    !
    module procedure lower_bound_f32_i32
        integer(int64) :: lo, hi
        !
        call search_impl_f32(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_f32_i32
    !
    module procedure lower_bound_f32_i64
        integer(int64) :: lo, hi
        !
        call search_impl_f32(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        pos = lo
    end procedure lower_bound_f32_i64
    !
    module procedure lower_bound_f64_i32
        integer(int64) :: lo, hi
        !
        call search_impl_f64(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_f64_i32
    !
    module procedure lower_bound_f64_i64
        integer(int64) :: lo, hi
        !
        call search_impl_f64(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        pos = lo
    end procedure lower_bound_f64_i64
    !
    module procedure lower_bound_bool_i32
        integer(int64) :: lo, hi
        !
        call search_impl_bool(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_bool_i32
    !
    module procedure lower_bound_bool_i64
        integer(int64) :: lo, hi
        !
        call search_impl_bool(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        pos = lo
    end procedure lower_bound_bool_i64
    !
    module procedure lower_bound_chr_i32
        integer(int64) :: lo, hi
        !
        call search_impl_chr(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_chr_i32
    !
    module procedure lower_bound_chr_i64
        integer(int64) :: lo, hi
        !
        call search_impl_chr(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        pos = lo
    end procedure lower_bound_chr_i64
    !
    module procedure lower_bound_date_i32
        integer(int64) :: lo, hi
        !
        call search_impl_date(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_date_i32
    !
    module procedure lower_bound_date_i64
        integer(int64) :: lo, hi
        !
        call search_impl_date(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        pos = lo
    end procedure lower_bound_date_i64
    !
    module procedure lower_bound_time_i32
        integer(int64) :: lo, hi
        !
        call search_impl_time(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_time_i32
    !
    module procedure lower_bound_time_i64
        integer(int64) :: lo, hi
        !
        call search_impl_time(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        pos = lo
    end procedure lower_bound_time_i64
    !
    module procedure lower_bound_ts_i32
        integer(int64) :: lo, hi
        !
        call search_impl_ts(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_ts_i32
    !
    module procedure lower_bound_ts_i64
        integer(int64) :: lo, hi
        !
        call search_impl_ts(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        pos = lo
    end procedure lower_bound_ts_i64
    !
    module procedure lower_bound_strcol_i32
        integer(int64) :: lo, hi
        !
        call search_impl_strcol(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        call narrow_i64(lo, "pf_lower_bound", "insertion point", pos)
    end procedure lower_bound_strcol_i32
    !
    module procedure lower_bound_strcol_i64
        integer(int64) :: lo, hi
        !
        call search_impl_strcol(values, target, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        pos = lo
    end procedure lower_bound_strcol_i64
    !
    module procedure upper_bound_i32_i32
        integer(int64) :: lo, hi
        !
        call search_impl_i32(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_i32_i32
    !
    module procedure upper_bound_i32_i64
        integer(int64) :: lo, hi
        !
        call search_impl_i32(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        pos = hi
    end procedure upper_bound_i32_i64
    !
    module procedure upper_bound_i64_i32
        integer(int64) :: lo, hi
        !
        call search_impl_i64(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_i64_i32
    !
    module procedure upper_bound_i64_i64
        integer(int64) :: lo, hi
        !
        call search_impl_i64(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        pos = hi
    end procedure upper_bound_i64_i64
    !
    module procedure upper_bound_f32_i32
        integer(int64) :: lo, hi
        !
        call search_impl_f32(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_f32_i32
    !
    module procedure upper_bound_f32_i64
        integer(int64) :: lo, hi
        !
        call search_impl_f32(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        pos = hi
    end procedure upper_bound_f32_i64
    !
    module procedure upper_bound_f64_i32
        integer(int64) :: lo, hi
        !
        call search_impl_f64(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_f64_i32
    !
    module procedure upper_bound_f64_i64
        integer(int64) :: lo, hi
        !
        call search_impl_f64(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        pos = hi
    end procedure upper_bound_f64_i64
    !
    module procedure upper_bound_bool_i32
        integer(int64) :: lo, hi
        !
        call search_impl_bool(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_bool_i32
    !
    module procedure upper_bound_bool_i64
        integer(int64) :: lo, hi
        !
        call search_impl_bool(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        pos = hi
    end procedure upper_bound_bool_i64
    !
    module procedure upper_bound_chr_i32
        integer(int64) :: lo, hi
        !
        call search_impl_chr(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_chr_i32
    !
    module procedure upper_bound_chr_i64
        integer(int64) :: lo, hi
        !
        call search_impl_chr(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        pos = hi
    end procedure upper_bound_chr_i64
    !
    module procedure upper_bound_date_i32
        integer(int64) :: lo, hi
        !
        call search_impl_date(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_date_i32
    !
    module procedure upper_bound_date_i64
        integer(int64) :: lo, hi
        !
        call search_impl_date(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        pos = hi
    end procedure upper_bound_date_i64
    !
    module procedure upper_bound_time_i32
        integer(int64) :: lo, hi
        !
        call search_impl_time(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_time_i32
    !
    module procedure upper_bound_time_i64
        integer(int64) :: lo, hi
        !
        call search_impl_time(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        pos = hi
    end procedure upper_bound_time_i64
    !
    module procedure upper_bound_ts_i32
        integer(int64) :: lo, hi
        !
        call search_impl_ts(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_ts_i32
    !
    module procedure upper_bound_ts_i64
        integer(int64) :: lo, hi
        !
        call search_impl_ts(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        pos = hi
    end procedure upper_bound_ts_i64
    !
    module procedure upper_bound_strcol_i32
        integer(int64) :: lo, hi
        !
        call search_impl_strcol(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        call narrow_i64(hi, "pf_upper_bound", "insertion point", pos)
    end procedure upper_bound_strcol_i32
    !
    module procedure upper_bound_strcol_i64
        integer(int64) :: lo, hi
        !
        call search_impl_strcol(values, target, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        pos = hi
    end procedure upper_bound_strcol_i64
    !
    module procedure equal_range_i32_i32
        integer(int64) :: lo, hi
        !
        call search_impl_i32(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_i32_i32
    !
    module procedure equal_range_i32_i64
        integer(int64) :: lo, hi
        !
        call search_impl_i32(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_i32_i64
    !
    module procedure equal_range_i64_i32
        integer(int64) :: lo, hi
        !
        call search_impl_i64(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_i64_i32
    !
    module procedure equal_range_i64_i64
        integer(int64) :: lo, hi
        !
        call search_impl_i64(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_i64_i64
    !
    module procedure equal_range_f32_i32
        integer(int64) :: lo, hi
        !
        call search_impl_f32(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_f32_i32
    !
    module procedure equal_range_f32_i64
        integer(int64) :: lo, hi
        !
        call search_impl_f32(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_f32_i64
    !
    module procedure equal_range_f64_i32
        integer(int64) :: lo, hi
        !
        call search_impl_f64(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_f64_i32
    !
    module procedure equal_range_f64_i64
        integer(int64) :: lo, hi
        !
        call search_impl_f64(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_f64_i64
    !
    module procedure equal_range_bool_i32
        integer(int64) :: lo, hi
        !
        call search_impl_bool(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_bool_i32
    !
    module procedure equal_range_bool_i64
        integer(int64) :: lo, hi
        !
        call search_impl_bool(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_bool_i64
    !
    module procedure equal_range_chr_i32
        integer(int64) :: lo, hi
        !
        call search_impl_chr(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_chr_i32
    !
    module procedure equal_range_chr_i64
        integer(int64) :: lo, hi
        !
        call search_impl_chr(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_chr_i64
    !
    module procedure equal_range_date_i32
        integer(int64) :: lo, hi
        !
        call search_impl_date(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_date_i32
    !
    module procedure equal_range_date_i64
        integer(int64) :: lo, hi
        !
        call search_impl_date(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_date_i64
    !
    module procedure equal_range_time_i32
        integer(int64) :: lo, hi
        !
        call search_impl_time(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_time_i32
    !
    module procedure equal_range_time_i64
        integer(int64) :: lo, hi
        !
        call search_impl_time(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_time_i64
    !
    module procedure equal_range_ts_i32
        integer(int64) :: lo, hi
        !
        call search_impl_ts(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_ts_i32
    !
    module procedure equal_range_ts_i64
        integer(int64) :: lo, hi
        !
        call search_impl_ts(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_ts_i64
    !
    module procedure equal_range_strcol_i32
        integer(int64) :: lo, hi
        !
        call search_impl_strcol(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        call narrow_i64(lo, "pf_equal_range", "first matching index", first)
        call narrow_i64(hi - 1_int64, "pf_equal_range", "last matching index", last)
    end procedure equal_range_strcol_i32
    !
    module procedure equal_range_strcol_i64
        integer(int64) :: lo, hi
        !
        call search_impl_strcol(values, target, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        first = lo
        last = hi - 1_int64
    end procedure equal_range_strcol_i64
    !
    !> Shared worker behind every search specific for a 32-bit integer array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_i32(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_i32(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_i32([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_i32
    !
    !> Shared worker behind every search specific for a 64-bit integer array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_i64(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_i64(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_i64([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_i64
    !
    !> Shared worker behind every search specific for a 32-bit real array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_f32(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_f32(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_f32([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_f32
    !
    !> Shared worker behind every search specific for a 64-bit real array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_f64(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_f64(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_f64([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_f64
    !
    !> Shared worker behind every search specific for a logical array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_bool(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        logical, intent(in) :: values(:)
        logical, intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_bool(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_bool([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_bool
    !
    !> Shared worker behind every search specific for a string array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_chr(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        character(len=len(values)) :: padded
        character(len=32) :: a_str, b_str
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_chr(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        ! Compared at the ARRAY's element length, which is what makes this agree with
        ! Fortran's own `<` on the same two operands: a shorter target is blank-padded,
        ! exactly as a comparison would pad it. A target with non-blank characters past
        ! that length has no exact answer here at all, so it is refused rather than
        ! silently truncated into a different value.
        if (len_trim(target) > len(values)) then
            write (a_str, "(i0)") len_trim(target)
            write (b_str, "(i0)") len(values)
            error stop EP // proc // ": target has " // trim(a_str) // " non-blank " // &
                "characters but values holds " // trim(b_str) // " per element, so no " // &
                "exact comparison exists; widen values or trim target"
        end if
        padded = target
        call extract_chr([padded], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_chr
    !
    !> Shared worker behind every search specific for a date array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_date(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_date(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_date([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_date
    !
    !> Shared worker behind every search specific for a time array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_time(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_time(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_time([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_time
    !
    !> Shared worker behind every search specific for a timestamp array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_ts(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        call extract_ts(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_ts([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_ts
    !
    !> Shared worker behind every search specific for a packed string column array. Extracts once,
    !! checks the order once, and runs one or both binary searches over the result.
    subroutine search_impl_strcol(values, target, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: target !! the value to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo                 !! lower-bound answer, or 0.
        integer(int64), intent(out) :: hi                 !! upper-bound answer, or 0.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = values%size()
        call extract_strcol(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        ! A parquet_string_column stores bytes verbatim, so the target is used verbatim
        ! too -- trailing blanks included. There is no declared width to pad to.
        call extract_chr([target], tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, 1_int64, proc)
        lo = 0_int64
        hi = 0_int64
        if (want /= SRCH_UPPER) call engine_search(buf, n + 1_int64, n, .false., proc, lo)
        if (want /= SRCH_LOWER) call engine_search(buf, n + 1_int64, n, .true., proc, hi)
    end subroutine search_impl_strcol
    !
    module procedure lower_bound_i32_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i32(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_i32_i32_many
    !
    module procedure lower_bound_i32_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i32(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_i32_i64_many
    !
    module procedure lower_bound_i64_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i64(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_i64_i32_many
    !
    module procedure lower_bound_i64_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i64(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_i64_i64_many
    !
    module procedure lower_bound_f32_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f32(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_f32_i32_many
    !
    module procedure lower_bound_f32_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f32(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_f32_i64_many
    !
    module procedure lower_bound_f64_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f64(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_f64_i32_many
    !
    module procedure lower_bound_f64_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f64(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_f64_i64_many
    !
    module procedure lower_bound_bool_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_bool(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_bool_i32_many
    !
    module procedure lower_bound_bool_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_bool(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_bool_i64_many
    !
    module procedure lower_bound_chr_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_chr(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_chr_i32_many
    !
    module procedure lower_bound_chr_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_chr(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_chr_i64_many
    !
    module procedure lower_bound_date_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_date(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_date_i32_many
    !
    module procedure lower_bound_date_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_date(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_date_i64_many
    !
    module procedure lower_bound_time_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_time(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_time_i32_many
    !
    module procedure lower_bound_time_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_time(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_time_i64_many
    !
    module procedure lower_bound_ts_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_ts(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_ts_i32_many
    !
    module procedure lower_bound_ts_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_ts(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_ts_i64_many
    !
    module procedure lower_bound_strcol_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_strcol(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_lower_bound", "insertion point", pos(k))
        end do
    end procedure lower_bound_strcol_i32_many
    !
    module procedure lower_bound_strcol_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_lower_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_strcol(values, targets, SRCH_LOWER, descending, nulls_first, &
            assume_sorted, "pf_lower_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = lo(k)
        end do
    end procedure lower_bound_strcol_i64_many
    !
    module procedure upper_bound_i32_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i32(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_i32_i32_many
    !
    module procedure upper_bound_i32_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i32(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_i32_i64_many
    !
    module procedure upper_bound_i64_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i64(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_i64_i32_many
    !
    module procedure upper_bound_i64_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i64(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_i64_i64_many
    !
    module procedure upper_bound_f32_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f32(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_f32_i32_many
    !
    module procedure upper_bound_f32_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f32(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_f32_i64_many
    !
    module procedure upper_bound_f64_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f64(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_f64_i32_many
    !
    module procedure upper_bound_f64_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f64(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_f64_i64_many
    !
    module procedure upper_bound_bool_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_bool(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_bool_i32_many
    !
    module procedure upper_bound_bool_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_bool(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_bool_i64_many
    !
    module procedure upper_bound_chr_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_chr(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_chr_i32_many
    !
    module procedure upper_bound_chr_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_chr(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_chr_i64_many
    !
    module procedure upper_bound_date_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_date(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_date_i32_many
    !
    module procedure upper_bound_date_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_date(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_date_i64_many
    !
    module procedure upper_bound_time_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_time(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_time_i32_many
    !
    module procedure upper_bound_time_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_time(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_time_i64_many
    !
    module procedure upper_bound_ts_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_ts(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_ts_i32_many
    !
    module procedure upper_bound_ts_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_ts(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_ts_i64_many
    !
    module procedure upper_bound_strcol_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_strcol(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            call narrow_i64(hi(k), "pf_upper_bound", "insertion point", pos(k))
        end do
    end procedure upper_bound_strcol_i32_many
    !
    module procedure upper_bound_strcol_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(pos, kind=int64), m, "pf_upper_bound", "pos")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_strcol(values, targets, SRCH_UPPER, descending, nulls_first, &
            assume_sorted, "pf_upper_bound", lo, hi)
        do k = 1_int64, m
            pos(k) = hi(k)
        end do
    end procedure upper_bound_strcol_i64_many
    !
    module procedure equal_range_i32_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i32(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_i32_i32_many
    !
    module procedure equal_range_i32_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i32(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_i32_i64_many
    !
    module procedure equal_range_i64_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i64(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_i64_i32_many
    !
    module procedure equal_range_i64_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_i64(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_i64_i64_many
    !
    module procedure equal_range_f32_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f32(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_f32_i32_many
    !
    module procedure equal_range_f32_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f32(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_f32_i64_many
    !
    module procedure equal_range_f64_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f64(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_f64_i32_many
    !
    module procedure equal_range_f64_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_f64(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_f64_i64_many
    !
    module procedure equal_range_bool_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_bool(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_bool_i32_many
    !
    module procedure equal_range_bool_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_bool(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_bool_i64_many
    !
    module procedure equal_range_chr_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_chr(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_chr_i32_many
    !
    module procedure equal_range_chr_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_chr(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi, is_valid=is_valid)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_chr_i64_many
    !
    module procedure equal_range_date_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_date(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_date_i32_many
    !
    module procedure equal_range_date_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_date(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_date_i64_many
    !
    module procedure equal_range_time_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_time(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_time_i32_many
    !
    module procedure equal_range_time_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_time(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_time_i64_many
    !
    module procedure equal_range_ts_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_ts(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_ts_i32_many
    !
    module procedure equal_range_ts_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_ts(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_ts_i64_many
    !
    module procedure equal_range_strcol_i32_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_strcol(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            call narrow_i64(lo(k), "pf_equal_range", "first matching index", first(k))
            call narrow_i64(hi(k) - 1_int64, "pf_equal_range", "last matching index", last(k))
        end do
    end procedure equal_range_strcol_i32_many
    !
    module procedure equal_range_strcol_i64_many
        integer(int64), allocatable :: lo(:), hi(:)
        integer(int64) :: m, k
        !
        m = size(targets, kind=int64)
        call check_many_size(size(first, kind=int64), m, "pf_equal_range", "first")
        call check_many_size(size(last, kind=int64), m, "pf_equal_range", "last")
        allocate(lo(max(m, 1_int64)), hi(max(m, 1_int64)))
        call search_many_impl_strcol(values, targets, SRCH_BOTH, descending, nulls_first, &
            assume_sorted, "pf_equal_range", lo, hi)
        do k = 1_int64, m
            first(k) = lo(k)
            last(k) = hi(k) - 1_int64
        end do
    end procedure equal_range_strcol_i64_many
    !
    !> Refuses a result array that is not one entry per target, before anything is searched.
    subroutine check_many_size(got, want, proc, what)
        integer(int64), intent(in) :: got     !! entries the caller supplied.
        integer(int64), intent(in) :: want    !! targets given.
        character(len=*), intent(in) :: proc  !! calling procedure, for messages.
        character(len=*), intent(in) :: what  !! the argument's name.
        character(len=32) :: a_str, b_str
        !
        if (got /= want) then
            write (a_str, "(i0)") got
            write (b_str, "(i0)") want
            error stop EP // proc // ": " // what // " has " // trim(a_str) // &
                " entries but " // trim(b_str) // " targets were given; it takes one per target"
        end if
    end subroutine check_many_size
    !
    !> Shared worker behind every BULK search specific for a 32-bit integer array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_i32(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        integer(int32), intent(in) :: values(:)
        integer(int32), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_i32(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_i32(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_i32
    !
    !> Shared worker behind every BULK search specific for a 64-bit integer array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_i64(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        integer(int64), intent(in) :: values(:)
        integer(int64), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_i64(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_i64(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_i64
    !
    !> Shared worker behind every BULK search specific for a 32-bit real array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_f32(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        real(real32), intent(in) :: values(:)
        real(real32), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_f32(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_f32(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_f32
    !
    !> Shared worker behind every BULK search specific for a 64-bit real array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_f64(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        real(real64), intent(in) :: values(:)
        real(real64), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_f64(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_f64(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_f64
    !
    !> Shared worker behind every BULK search specific for a logical array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_bool(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        logical, intent(in) :: values(:)
        logical, intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_bool(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_bool(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_bool
    !
    !> Shared worker behind every BULK search specific for a string array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_chr(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi, is_valid)
        character(len=*), intent(in) :: values(:)
        character(len=*), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        logical, intent(in), optional :: is_valid(:) !! per element: .false. marks a null.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        character(len=len(values)), allocatable :: padded(:)
        character(len=32) :: a_str, b_str
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_chr(values, buf, desc, nlo, proc, is_valid=is_valid)
        if (check) call check_sorted_input(buf, n, proc, "values")
        ! Every target is compared at the ARRAY's element length, exactly as the scalar
        ! form pads a single target -- see emit_target_extract's note. The two arrays may
        ! legitimately have different declared lengths, so the padding is per target and
        ! a target with non-blank characters past that length is refused rather than
        ! silently truncated into a different value.
        allocate(character(len=len(values)) :: padded(m))
        do k = 1_int64, m
            if (len_trim(targets(k)) > len(values)) then
                write (a_str, "(i0)") len_trim(targets(k))
                write (b_str, "(i0)") len(values)
                error stop EP // proc // ": a target has " // trim(a_str) // " non-blank " // &
                    "characters but values holds " // trim(b_str) // " per element, so no " // &
                    "exact comparison exists; widen values or trim the target"
            end if
            padded(k) = targets(k)
        end do
        call extract_chr(padded, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_chr
    !
    !> Shared worker behind every BULK search specific for a date array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_date(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_date), intent(in) :: values(:)
        type(parquet_date), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_date(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_date(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_date
    !
    !> Shared worker behind every BULK search specific for a time array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_time(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_time), intent(in) :: values(:)
        type(parquet_time), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_time(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_time(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_time
    !
    !> Shared worker behind every BULK search specific for a timestamp array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_ts(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_timestamp), intent(in) :: values(:)
        type(parquet_timestamp), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = size(values, kind=int64)
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_ts(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        call extract_ts(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_ts
    !
    !> Shared worker behind every BULK search specific for a packed string column array.
    !!
    !! The whole reason this exists: `values` is extracted and order-checked ONCE and every
    !! target is appended to that one key, so m searches cost O(n + m log n). The scalar
    !! worker above re-extracts per call, which is O(n) each and is what makes a LOOP of
    !! scalar searches quadratic.
    !!
    !! It reaches `sort_search_position` directly rather than through `engine_search`,
    !! because the C++ oracle's entry point has no target-row argument -- it always searches
    !! for the row just past the searched range. Nothing is lost: the oracle A/B still
    !! covers the scalar path, which runs the same comparator and the same binary search,
    !! and the bulk path is checked against the scalar one element for element.
    subroutine search_many_impl_strcol(values, targets, want, descending, nulls_first, &
            assume_sorted, proc, lo, hi)
        type(parquet_string_column), intent(in) :: values
        character(len=*), intent(in) :: targets(:) !! the values to look for.
        integer, intent(in) :: want                       !! SRCH_LOWER / SRCH_UPPER / SRCH_BOTH.
        logical, intent(in), optional :: descending       !! .true. for high-to-low order.
        logical, intent(in), optional :: nulls_first      !! .true. when nulls come first.
        logical, intent(in), optional :: assume_sorted    !! .true. skips the order check.
        character(len=*), intent(in) :: proc              !! calling procedure, for messages.
        integer(int64), intent(out) :: lo(:)              !! per target: lower-bound answer.
        integer(int64), intent(out) :: hi(:)              !! per target: upper-bound answer.
        type(sort_key_buf), allocatable :: buf(:), tbuf(:)
        integer(int64) :: n, m, k
        logical :: desc, nlo, check
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        check = .true.
        if (present(assume_sorted)) check = .not. assume_sorted
        n = values%size()
        m = size(targets, kind=int64)
        lo = 0_int64
        hi = 0_int64
        if (m < 1_int64) return
        call extract_strcol(values, buf, desc, nlo, proc)
        if (check) call check_sorted_input(buf, n, proc, "values")
        ! A parquet_string_column stores bytes verbatim, so the targets are used verbatim
        ! too -- trailing blanks included. There is no declared width to pad to.
        call extract_chr(targets, tbuf, desc, nlo, proc)
        call buf_append(buf, n, tbuf, m, proc)
        do k = 1_int64, m
            ! Target k sits at row n+k of the one appended key.
            if (want /= SRCH_UPPER) lo(k) = sort_search_position(buf, n, .false., target_row=n + k)
            if (want /= SRCH_LOWER) hi(k) = sort_search_position(buf, n, .true., target_row=n + k)
        end do
    end subroutine search_many_impl_strcol
    !
end submodule parquet_sorting_search ! GCOVR_EXCL_LINE
