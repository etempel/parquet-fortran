!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_parquet_sorting.py
! The type table lives in that script; edit it there, not here.
!
!> The six intrinsic `pf_argsort` specifics, their key extraction, and the engine dispatch.
!!
!! **This is the argsort tier's implementation, and what it does NOT reach is the point.** Nothing
!! here names `parquet_bindings`: the C++ engine is reached through the procedure pointers
!! `parquet_argsort` holds, which `parquet_sorting_oracle` binds and which a program that never
!! imports that module never has bound. See `parquet_argsort`'s own header.
!!
!! **This file decides nothing about order.** It extracts values, says which rows are null, and
!! passes the caller's `descending`/`nulls_first` flags through -- every ordering decision is made
!! in `sort_compare_key` (src/parquet_argsort_engine.f90), which is what stops a raw-array sort, a
!! table sort and a read-time `sort_by=` from ever disagreeing.
submodule (parquet_argsort) parquet_argsort_kernel
    implicit none
    !
contains
    !
    module procedure extract_i32
        integer(int64) :: n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        !
        ! **The threaded arm lives in `extract_i32_par`, and keeping it OUT of this
        ! procedure is load-bearing rather than tidiness.** With the two `!$omp parallel do`
        ! regions written inline here, gfortran's codegen for the SERIAL branch below -- whose
        ! statements are unchanged either way -- measured **2.4x slower**: 0.251 -> 0.609
        ! ns/element on `i64` and 0.250 -> 0.356 on `f64` (machine A, gfortran 15.2, n = 5e6,
        ! `--serial`, against a 0.004 ns cross-build floor). That arm is taken by every caller
        ! passing `threads=1`, by a single-core machine, and by every sort inside an existing
        ! OpenMP region, since `pf_sort_threads()` answers 1 there.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            call extract_i32_par(buf(1)%ints, values, n, team)
        else
            call extract_i32_ser(buf(1)%ints, values, n)
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_i32
    !
    module procedure extract_i64
        integer(int64) :: n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        !
        ! **The threaded arm lives in `extract_i64_par`, and keeping it OUT of this
        ! procedure is load-bearing rather than tidiness.** With the two `!$omp parallel do`
        ! regions written inline here, gfortran's codegen for the SERIAL branch below -- whose
        ! statements are unchanged either way -- measured **2.4x slower**: 0.251 -> 0.609
        ! ns/element on `i64` and 0.250 -> 0.356 on `f64` (machine A, gfortran 15.2, n = 5e6,
        ! `--serial`, against a 0.004 ns cross-build floor). That arm is taken by every caller
        ! passing `threads=1`, by a single-core machine, and by every sort inside an existing
        ! OpenMP region, since `pf_sort_threads()` answers 1 there.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            call extract_i64_par(buf(1)%ints, values, n, team)
        else
            call extract_i64_ser(buf(1)%ints, values, n)
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_i64
    !
    module procedure extract_f32
        integer(int64) :: n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        !
        ! **The threaded arm lives in `extract_f32_par`, and keeping it OUT of this
        ! procedure is load-bearing rather than tidiness.** With the two `!$omp parallel do`
        ! regions written inline here, gfortran's codegen for the SERIAL branch below -- whose
        ! statements are unchanged either way -- measured **2.4x slower**: 0.251 -> 0.609
        ! ns/element on `i64` and 0.250 -> 0.356 on `f64` (machine A, gfortran 15.2, n = 5e6,
        ! `--serial`, against a 0.004 ns cross-build floor). That arm is taken by every caller
        ! passing `threads=1`, by a single-core machine, and by every sort inside an existing
        ! OpenMP region, since `pf_sort_threads()` answers 1 there.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            call extract_f32_par(buf(1)%reals, values, n, team)
        else
            call extract_f32_ser(buf(1)%reals, values, n)
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_f32
    !
    module procedure extract_f64
        integer(int64) :: n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_REAL
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%reals(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        !
        ! **The threaded arm lives in `extract_f64_par`, and keeping it OUT of this
        ! procedure is load-bearing rather than tidiness.** With the two `!$omp parallel do`
        ! regions written inline here, gfortran's codegen for the SERIAL branch below -- whose
        ! statements are unchanged either way -- measured **2.4x slower**: 0.251 -> 0.609
        ! ns/element on `i64` and 0.250 -> 0.356 on `f64` (machine A, gfortran 15.2, n = 5e6,
        ! `--serial`, against a 0.004 ns cross-build floor). That arm is taken by every caller
        ! passing `threads=1`, by a single-core machine, and by every sort inside an existing
        ! OpenMP region, since `pf_sort_threads()` answers 1 there.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            call extract_f64_par(buf(1)%reals, values, n, team)
        else
            call extract_f64_ser(buf(1)%reals, values, n)
        end if
        if (present(is_valid)) call valid_from_mask(is_valid, n, proc, buf(1)%valid)
    end procedure extract_f64
    !
    module procedure extract_bool
        integer(int64) :: n, nth
        integer :: team
        !
        n = size(values, kind=int64)
        allocate(buf(1))
        buf(1)%family = SK_INT
        buf(1)%descending = descending
        buf(1)%nulls_first = nulls_first
        allocate(buf(1)%ints(max(n, 1_int64)))
        ! **The pre-fill and the extraction share ONE static schedule, and that pairing is
        ! the point.** Whichever pass writes a page first decides which NUMA node it lives
        ! on for the rest of the sort, so a serial blanket fill puts the whole key buffer
        ! on the master thread's node and every other thread then reads it across the
        ! interconnect. Filling under the same `schedule(static)` the extraction uses means
        ! each thread faults exactly the range it is about to write.
        !
        ! **Do NOT drop the fill as a redundant pass**, however obviously the loop below
        ! covers every one of `1..n`. Removing it measured **23% SLOWER** at 64 threads
        ! (6.46 against 5.26 ns/element, f64, n = 5e6, ifx), reproduced against three
        ! earlier runs: the fill is a pure sequential sweep and faults pages far faster
        ! than the extraction loop, which interleaves a read of `values`. Cheaper work is
        ! not always less time.
        !
        ! **`threads=` is honoured here, and the absent case falls back to the automatic
        ! policy.** `resolve_thread_count` is the same procedure the engine uses, so an
        ! explicit `threads=1` really does make the whole operation serial -- which the
        ! CHANGELOG promises for `pf_argsort`/`pf_sort`/`pf_unique*`/`pf_rank`, and which a
        ! bare `pf_sort_threads()` here would have quietly broken. Entry points that take
        ! no thread argument at all (`pf_sort_keys%add`, `pf_merge`, `pf_is_sorted`,
        ! `pf_partial_*`) pass nothing and get the automatic answer, which is the only
        ! thing they could ever have got.
        !
        ! **The threaded arm lives in `extract_bool_par`, and keeping it OUT of this
        ! procedure is load-bearing rather than tidiness.** With the two `!$omp parallel do`
        ! regions written inline here, gfortran's codegen for the SERIAL branch below -- whose
        ! statements are unchanged either way -- measured **2.4x slower**: 0.251 -> 0.609
        ! ns/element on `i64` and 0.250 -> 0.356 on `f64` (machine A, gfortran 15.2, n = 5e6,
        ! `--serial`, against a 0.004 ns cross-build floor). That arm is taken by every caller
        ! passing `threads=1`, by a single-core machine, and by every sort inside an existing
        ! OpenMP region, since `pf_sort_threads()` answers 1 there.
        call resolve_thread_count(threads, n, nth)
        team = tail_team(nth, n)
        if (team > 1) then
            call extract_bool_par(buf(1)%ints, values, n, team)
        else
            call extract_bool_ser(buf(1)%ints, values, n)
        end if
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
    !> Threaded pre-fill and extraction for `extract_i32` -- the 32-bit integer arm.
    !!
    !! **The pre-fill and the extraction share ONE static schedule, and that pairing is the
    !! point.** Whichever pass writes a page first decides which NUMA node it lives on for the
    !! rest of the sort, so a serial blanket fill puts the whole key buffer on the master
    !! thread's node and every other thread then reads it across the interconnect.
    !!
    !! **Do NOT drop the fill as a redundant pass**, however obviously the second loop covers
    !! every one of `1..n`. Removing it measured 23% SLOWER at 64 threads (6.46 against 5.26
    !! ns/element, f64, n = 5e6, ifx): the fill is a pure sequential sweep and faults pages far
    !! faster than the extraction loop, which interleaves a read of `values`.
    subroutine extract_i32_par(dst, values, n, team)
        integer(int64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        integer(int32), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer, intent(in) :: team !! threads to use; the caller has already checked it is > 1.
        integer(int64) :: k
        !
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = 0_int64
        end do
        !$omp end parallel do
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = int(values(k), int64)
        end do
        !$omp end parallel do
    end subroutine extract_i32_par
    !
    !> Threaded pre-fill and extraction for `extract_i64` -- the 64-bit integer arm.
    !!
    !! **The pre-fill and the extraction share ONE static schedule, and that pairing is the
    !! point.** Whichever pass writes a page first decides which NUMA node it lives on for the
    !! rest of the sort, so a serial blanket fill puts the whole key buffer on the master
    !! thread's node and every other thread then reads it across the interconnect.
    !!
    !! **Do NOT drop the fill as a redundant pass**, however obviously the second loop covers
    !! every one of `1..n`. Removing it measured 23% SLOWER at 64 threads (6.46 against 5.26
    !! ns/element, f64, n = 5e6, ifx): the fill is a pure sequential sweep and faults pages far
    !! faster than the extraction loop, which interleaves a read of `values`.
    subroutine extract_i64_par(dst, values, n, team)
        integer(int64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        integer(int64), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer, intent(in) :: team !! threads to use; the caller has already checked it is > 1.
        integer(int64) :: k
        !
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = 0_int64
        end do
        !$omp end parallel do
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = values(k)
        end do
        !$omp end parallel do
    end subroutine extract_i64_par
    !
    !> Threaded pre-fill and extraction for `extract_f32` -- the 32-bit real arm.
    !!
    !! **The pre-fill and the extraction share ONE static schedule, and that pairing is the
    !! point.** Whichever pass writes a page first decides which NUMA node it lives on for the
    !! rest of the sort, so a serial blanket fill puts the whole key buffer on the master
    !! thread's node and every other thread then reads it across the interconnect.
    !!
    !! **Do NOT drop the fill as a redundant pass**, however obviously the second loop covers
    !! every one of `1..n`. Removing it measured 23% SLOWER at 64 threads (6.46 against 5.26
    !! ns/element, f64, n = 5e6, ifx): the fill is a pure sequential sweep and faults pages far
    !! faster than the extraction loop, which interleaves a read of `values`.
    subroutine extract_f32_par(dst, values, n, team)
        real(real64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        real(real32), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer, intent(in) :: team !! threads to use; the caller has already checked it is > 1.
        integer(int64) :: k
        !
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = 0.0_real64
        end do
        !$omp end parallel do
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = real(values(k), real64)
        end do
        !$omp end parallel do
    end subroutine extract_f32_par
    !
    !> Threaded pre-fill and extraction for `extract_f64` -- the 64-bit real arm.
    !!
    !! **The pre-fill and the extraction share ONE static schedule, and that pairing is the
    !! point.** Whichever pass writes a page first decides which NUMA node it lives on for the
    !! rest of the sort, so a serial blanket fill puts the whole key buffer on the master
    !! thread's node and every other thread then reads it across the interconnect.
    !!
    !! **Do NOT drop the fill as a redundant pass**, however obviously the second loop covers
    !! every one of `1..n`. Removing it measured 23% SLOWER at 64 threads (6.46 against 5.26
    !! ns/element, f64, n = 5e6, ifx): the fill is a pure sequential sweep and faults pages far
    !! faster than the extraction loop, which interleaves a read of `values`.
    subroutine extract_f64_par(dst, values, n, team)
        real(real64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        real(real64), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer, intent(in) :: team !! threads to use; the caller has already checked it is > 1.
        integer(int64) :: k
        !
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = 0.0_real64
        end do
        !$omp end parallel do
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = values(k)
        end do
        !$omp end parallel do
    end subroutine extract_f64_par
    !
    !> Threaded pre-fill and extraction for `extract_bool` -- the logical arm.
    !!
    !! **The pre-fill and the extraction share ONE static schedule, and that pairing is the
    !! point.** Whichever pass writes a page first decides which NUMA node it lives on for the
    !! rest of the sort, so a serial blanket fill puts the whole key buffer on the master
    !! thread's node and every other thread then reads it across the interconnect.
    !!
    !! **Do NOT drop the fill as a redundant pass**, however obviously the second loop covers
    !! every one of `1..n`. Removing it measured 23% SLOWER at 64 threads (6.46 against 5.26
    !! ns/element, f64, n = 5e6, ifx): the fill is a pure sequential sweep and faults pages far
    !! faster than the extraction loop, which interleaves a read of `values`.
    subroutine extract_bool_par(dst, values, n, team)
        integer(int64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        logical, intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer, intent(in) :: team !! threads to use; the caller has already checked it is > 1.
        integer(int64) :: k
        !
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = 0_int64
        end do
        !$omp end parallel do
        !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
        do k = 1_int64, n
            dst(k) = merge(1_int64, 0_int64, values(k))
        end do
        !$omp end parallel do
    end subroutine extract_bool_par
    !
    !> Serial pre-fill and extraction for `extract_i32` -- the 32-bit integer arm.
    !!
    !! The blanket fill is kept for the reason given on the threaded twin: it is a sequential
    !! sweep that faults pages faster than the extraction loop, which interleaves a read of
    !! `values`. Dropping it measured 23% SLOWER at 64 threads.
    subroutine extract_i32_ser(dst, values, n)
        integer(int64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        integer(int32), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer(int64) :: k
        !
        dst = 0_int64
        do k = 1_int64, n
            dst(k) = int(values(k), int64)
        end do
    end subroutine extract_i32_ser
    !
    !> Serial pre-fill and extraction for `extract_i64` -- the 64-bit integer arm.
    !!
    !! The blanket fill is kept for the reason given on the threaded twin: it is a sequential
    !! sweep that faults pages faster than the extraction loop, which interleaves a read of
    !! `values`. Dropping it measured 23% SLOWER at 64 threads.
    subroutine extract_i64_ser(dst, values, n)
        integer(int64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        integer(int64), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer(int64) :: k
        !
        dst = 0_int64
        do k = 1_int64, n
            dst(k) = values(k)
        end do
    end subroutine extract_i64_ser
    !
    !> Serial pre-fill and extraction for `extract_f32` -- the 32-bit real arm.
    !!
    !! The blanket fill is kept for the reason given on the threaded twin: it is a sequential
    !! sweep that faults pages faster than the extraction loop, which interleaves a read of
    !! `values`. Dropping it measured 23% SLOWER at 64 threads.
    subroutine extract_f32_ser(dst, values, n)
        real(real64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        real(real32), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer(int64) :: k
        !
        dst = 0.0_real64
        do k = 1_int64, n
            dst(k) = real(values(k), real64)
        end do
    end subroutine extract_f32_ser
    !
    !> Serial pre-fill and extraction for `extract_f64` -- the 64-bit real arm.
    !!
    !! The blanket fill is kept for the reason given on the threaded twin: it is a sequential
    !! sweep that faults pages faster than the extraction loop, which interleaves a read of
    !! `values`. Dropping it measured 23% SLOWER at 64 threads.
    subroutine extract_f64_ser(dst, values, n)
        real(real64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        real(real64), intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer(int64) :: k
        !
        dst = 0.0_real64
        do k = 1_int64, n
            dst(k) = values(k)
        end do
    end subroutine extract_f64_ser
    !
    !> Serial pre-fill and extraction for `extract_bool` -- the logical arm.
    !!
    !! The blanket fill is kept for the reason given on the threaded twin: it is a sequential
    !! sweep that faults pages faster than the extraction loop, which interleaves a read of
    !! `values`. Dropping it measured 23% SLOWER at 64 threads.
    subroutine extract_bool_ser(dst, values, n)
        integer(int64), intent(out), contiguous :: dst(:) !! the key buffer to fill.
        logical, intent(in) :: values(:) !! the caller's values.
        integer(int64), intent(in) :: n !! elements to extract.
        integer(int64) :: k
        !
        dst = 0_int64
        do k = 1_int64, n
            dst(k) = merge(1_int64, 0_int64, values(k))
        end do
    end subroutine extract_bool_ser
    !
    ! `key_rows` was here, measuring a key's row count back from its own buffer. It was WRONG by
    ! construction and is deliberately not replaced: the extractors allocate with a `max(n, 1)`
    ! floor, so it could not tell a zero-row key from a one-row one, and every caller now passes
    ! the count it already has. Do not reintroduce it -- see keys_append's interface.
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
        integer(int64) :: nthreads
        !
        if (size(keys) < 1) then
            ! Unreachable: every public entry point rejects an empty key list before reaching here.
            error stop EP // proc // ": no sort key was given; call keys%add(...) at least once" ! GCOVR_EXCL_LINE
        end if
        ! Sized EXACTLY, never max(nrows, 1): a zero-row sort must hand back a zero-length
        ! permutation, or `size(perm)` lies and a caller's `do k = 1, size(perm)` reads element 1
        ! of an empty array. The engine is not called at all below two rows, so nothing downstream
        ! needs the one-element floor the extraction buffers use.
        allocate(perm(nrows))
        ! Resolved BEFORE any identity fill, so a fill can be threaded. It was one of three
        ! whole-column serial loops that together were 48% of a 64-thread end-to-end sort before
        ! they were threaded -- see `bench/benchmark_sort_tail.f90`, which sizes each one.
        call resolve_thread_count(threads, nrows, nthreads)
        if (nrows < 2_int64) then
            ! Zero or one row: the identity IS the answer and no engine runs.
            call fill_identity(perm, nrows, nthreads)
            return
        end if
        ! **The Fortran engine gets NO identity fill, and that is checked rather than assumed.**
        ! Each of its four paths establishes `perm` itself: the counting and radix paths write
        ! every slot directly, `sort_comparison_permutation` opens with its own `perm(k) = k`
        ! loop, and `sort_radix_multi_permutation` fills before its first key. Filling here as
        ! well was an extra whole-column pass on every sort -- around 0.3-0.4 ns/element, which on
        ! a counting-path sort is a large share of the total.
        !
        ! **A new Fortran-engine path must fill `perm` itself, or restore a fill here.** A path
        ! that reads `perm` expecting the identity will usually still pass its tests, because a
        ! fresh allocation reads back as zeros -- the trap CLAUDE.md records under 'An intermittent
        ! test failure has THREE causes'. Verified against that by running the whole suite under an
        ! LD_PRELOAD malloc filling every block with 0xFF.
        if (dbg_fortran_engine) then
            ! Stage 2 scaffolding -- see `dbg_fortran_engine`'s declaration. **Stage 4 made this
            ! branch honour `threads`**, and the resolution above is deliberately SHARED with the
            ! C++ path below rather than repeated here, so the two engines are handed the same
            ! number by the same procedure and an A/B compares engines rather than policies.
            !
            ! The A/B stays valid at every thread count for a reason that is about the ordering and
            ! not about either implementation: both comparators end in a row-index tiebreaker, so no
            ! two distinct rows compare equal, exactly one permutation is correct, and a threaded
            ! answer that differs from a serial one is WRONG rather than merely different.
            call sort_build_permutation_threaded(keys, nrows, nthreads, perm)
            return
        end if
        ! **The C++ engine, reached through the pointer parquet_sorting_oracle bound.**
        ! Naming its bind(C) entry points here would put parquet_bindings -- and with it the
        ! whole Arrow stack -- into the use graph of every program that sorts anything, which
        ! is exactly what this tier exists to avoid. The oracle is TEST-ONLY: the shipped path
        ! is the Fortran branch above, and a build that never imports the oracle never
        ! compiles it. check_oracle aborts rather than falling back -- a silent fallback would
        ! make the A/B conformance tests compare the Fortran engine against itself and pass.
        call oracle_argsort(keys, nrows, nthreads, proc, perm)
    end procedure drive_engine
    !
    module procedure pf_sort_threads
        use parquet_settings_base, only : parquet_get_sort_threads
        use parquet_settings_base, only : parquet_auto_thread_count
        !
        ! **The rule itself lives in parquet_auto_thread_count (src/parquet_settings_base.f90)** --
        ! the serial-inside-a-parallel-region default, why the predicate is omp_get_level rather
        ! than omp_in_parallel, the libgomp deadlock behind that choice, why a cap may only lower
        ! the answer, and the omp_get_num_procs clamp. It was moved there when parquet_random gained
        ! a threaded bulk permutation and needed the same answer: CLAUDE.md's auto-threading note
        ! names a further copy of this rule as the mistake, and parquet_random is pure Fortran, so
        ! it cannot reach this module without acquiring the C++ dependency parquet_sorting carries.
        ! Behaviour here is unchanged.
        !
        ! What stays here is which SETTING caps the sort, and this is the ONE place it is read:
        ! pf_sort_threads is public precisely so a read-time sort_by= and a raw-array sort ask the
        ! same question, and a second reader is how the two would come to disagree.
        n = parquet_auto_thread_count(parquet_get_sort_threads(), "sorting")
    end procedure pf_sort_threads
    !
    module procedure resolve_thread_count
        use parquet_settings_base, only : parquet_nested_team_unsafe
#ifdef _OPENMP
        use omp_lib, only : omp_get_num_procs
#endif
        !
        if (present(threads)) then
            ! An explicit request is honoured wherever it is made, including inside a parallel
            ! region: the caller has said what they want, and refusing it there would leave no way
            ! to thread a sort at all from code that is itself parallel.
            count = max(1_int64, int(threads, int64))
            ! **One exception, and it is narrow on purpose: an enclosing region that is not
            ! actually running in parallel.** `parquet_nested_team_unsafe`
            ! (src/parquet_settings_base.f90) is that predicate and carries the reduction and the
            ! bisection behind it. It lives there rather than here so that parquet_random's threaded
            ! bulk permutation asks the identical question; two copies of a deadlock guard is
            ! exactly the shape that comes apart later.
            !
            ! **This clamps BOTH engines, and that is not a detail to get wrong.** The count
            ! resolved here is the one `drive_engine` hands to `sort_build_permutation_threaded`
            ! (Fortran) and to `parquet_sort_builder_build` (C++) alike, so the C++ engine is not
            ! exempt merely because it threads with `std::thread` rather than OpenMP. Only the
            ! ENCLOSING region's active level decides, never which engine is selected.
            if (parquet_nested_team_unsafe()) count = 1_int64
        else
            count = int(pf_sort_threads(), int64)
        end if
        ! Never more threads than rows; the C++ side clamps again by its own minimum chunk size.
        if (count > nrows) count = max(nrows, 1_int64)
#ifdef _OPENMP
        ! **Clamped to the processors actually available, and this is the ONE place that protects
        ! the tail.** The engine has always clamped separately (`sort_build_permutation_threaded`),
        ! so a bound process merely sorted serially -- but key extraction, the identity fill and the
        ! narrowing size their teams from this count and had no clamp at all, so they opened a full
        ! team on however few processors the mask allowed. Measured on machine B with
        ! `OMP_PLACES=cores`, where `omp_get_num_procs()` reports 2 while `omp_get_max_threads()`
        ! reports 64: extraction went **0.35 -> 2.58 ns/element**, a 7.3x loss from 32x
        ! oversubscription of two cores, while the engine only went serial.
        !
        ! **An explicit `threads=` is clamped too**, deliberately, unlike the parallel-region rule
        ! above which honours it. Those are different questions: a caller inside a parallel region
        ! has said something the library should obey, whereas a caller asking for 64 threads on a
        ! 2-processor mask has asked for something that cannot happen -- the threads would time-share
        ! and run slower than the serial path.
        !
        ! **The clamp cannot be avoided by asking the place list instead.** `omp_get_num_places()`
        ! and `omp_get_place_num_procs()` were measured on a process whose initial thread was
        ! pre-bound to 2 CPUs: they report 2 places totalling 2 processors, not the machine's 384,
        ! and a team of 64 then lands on 2 distinct CPUs. The true machine size is not recoverable
        ! from inside the process.
        !
        ! **The clamp and its warning both live in `parquet_clamp_to_affinity`**
        ! (src/parquet_settings_base.f90), which is the one place four resolvers share -- this one,
        ! `pf_sort_threads` and the bulk random draws through `parquet_auto_thread_count`,
        ! `prefetch_thread_count` and `parquet_string_threads`. It receives the PRE-clamp count, so
        ! the message names what this call actually asked for rather than the environment's ICV.
        !
        ! **`pf_sort_threads()` has already clamped on the automatic path**, so the call below is a
        ! no-op there and the warning has come from inside `pf_sort_threads` instead. It is NOT a
        ! silent clamp: `parquet_clamp_to_affinity` has no silent variant, deliberately, and its own
        ! doc-comment says why. Letting a query clamp without printing was tried and is exactly what
        ! made the warning unreachable for the job it was written for -- an automatic path arriving
        ! here already clamped, from a caller who asked for 64 threads through `OMP_NUM_THREADS` and
        ! silently got 2. Before the clamp was shared, this site was the ONLY one that warned.
        count = int(parquet_clamp_to_affinity(int(count), "sorting"), int64)
#endif
    end procedure resolve_thread_count
    !
    module procedure tail_team
        !> Elements each thread must get from a tail pass for the team to be worth opening.
        !!
        !! **The tail's floor is its OWN, and this is the change that separated it.** It used to
        !! read the since-retired `sort_parallel_min_rows` setting -- which also decided whether the
        !! RADIX threads -- on the reasoning that the two should "decline together". They should
        !! not: a tail pass is memcpy-shaped (extraction, the identity fill, the int32 narrowing)
        !! while the sort is compute-bound over many passes, so one number could not be right for
        !! both and was measured being right for neither.
        !!
        !! **The floor is `max(SORT_TAIL_MIN_ROWS, SORT_TAIL_ELEMS_PER_THREAD * nt)`, and unlike the
        !! refinement floor it is dominated by its ABSOLUTE term.** Measured on machine B with
        !! `benchmark_sort_tail --extract-only`, threaded against serial over n x team: the flat 8192
        !! this replaces is wrong by **16.22x** under gfortran (n = 8192 at 64 threads) and 2.75x
        !! under ifx.
        !!
        !! **The two compilers genuinely disagree here, and ifx is weighted per the maintainer's
        !! rule.** Under ifx extraction threads profitably from n = 32768 at every team size (1.25x
        !! to 1.36x) and the gains reach 74x at 4 M rows; under gfortran it does not pay until
        !! 65536, and not until 262144 at 64 threads. The rule below costs gfortran at most 2.28x
        !! (n = 32768, 32 threads) while taking ifx's worst case to 1.25x -- against 16.22x and 2.75x
        !! for the constant it replaces, so both compilers gain substantially even though only one
        !! of them got its preferred value.
        integer(int64), parameter :: SORT_TAIL_MIN_ROWS = 32768_int64
        integer(int64), parameter :: SORT_TAIL_ELEMS_PER_THREAD = 1024_int64
        integer(int64) :: floor_rows !! resolved floor, after any debug override.
        !
        team = 1
        if (nthreads <= 1_int64) return
        floor_rows = max(SORT_TAIL_MIN_ROWS, SORT_TAIL_ELEMS_PER_THREAD * nthreads)
        if (dbg_sort_tail_min_rows >= 0_int64) floor_rows = dbg_sort_tail_min_rows
        if (n < floor_rows) return
        team = int(min(nthreads, int(huge(0), int64)))
        if (team < 1) team = 1
    end procedure tail_team
    !
    module procedure fill_identity
        integer(int64) :: ik
        integer :: team
        !
        team = tail_team(nthreads, n)
        ! **The threaded arm is currently UNREACHABLE, and is kept rather than deleted.** The one
        ! caller in the library is `drive_engine`, which fills the identity only when `nrows < 2` --
        ! every engine path establishes `perm` itself -- and `resolve_thread_count` clamps its count
        ! to `max(nrows, 1)`, so `nthreads` is always 1 here and `tail_team` declines. No fixture and
        ! no debug override can change that: `dbg_sort_tail_min_rows` lowers the element floor, not
        ! the thread count. It stays because this is a PUBLIC procedure of the argsort tier whose
        ! documented contract is "threaded when `nthreads` and `n` justify it", and because a future
        ! engine path that reinstates a whole-column fill would want it back -- at which point the
        ! markers below come off and `tools/coverage.sh` reports the exclusion as stale.
        if (team > 1) then
            ! GCOVR_EXCL_START
            !$omp parallel do num_threads(team) default(shared) private(ik) schedule(static)
            do ik = 1_int64, n
                perm(ik) = ik
            end do
            !$omp end parallel do
            return
            ! GCOVR_EXCL_STOP
        end if
        do ik = 1_int64, n
            perm(ik) = ik
        end do
    end procedure fill_identity
    !
    module procedure narrow_perm
        integer(int64) :: n, nthreads, k
        character(len=32) :: n_str
        integer :: team
        !
        n = size(perm64, kind=int64)
        if (n > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-element sort; the array that
            ! would trip it cannot be built by any fixture this repository can run. Kept because
            ! the alternative is a silent truncation into a plausible wrong index.
            write (n_str, "(i0)") n
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, which " // &
                "does not fit an int32 permutation; declare perm as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        allocate(perm32(n))
        ! Threaded for the same reason `fill_identity` is: at 64 threads this whole-column copy was
        ! ~13% of an end-to-end argsort while every other phase had been parallelised around it.
        call resolve_thread_count(threads, n, nthreads)
        team = tail_team(nthreads, n)
        if (team > 1) then
            !$omp parallel do num_threads(team) default(shared) private(k) schedule(static)
            do k = 1_int64, n
                perm32(k) = int(perm64(k), int32)
            end do
            !$omp end parallel do
            return
        end if
        perm32 = int(perm64, int32)
    end procedure narrow_perm
    !
    module procedure narrow_offsets
        integer(int64) :: sentinel
        character(len=32) :: n_str
        !
        ! The SENTINEL, not the length -- and the difference is exactly one row. A permutation's
        ! largest entry is n, but this array's is n + 1, so at n == huge(int32) narrow_perm's own
        ! test passes while this one must not: a wrapped sentinel makes the last group's
        ! o(g+1) - 1 a huge negative bound, i.e. a silently wrong slice instead of an abort.
        sentinel = offsets64(size(offsets64))
        if (sentinel > int(huge(1_int32), int64)) then
            ! GCOVR_EXCL_START -- unreachable without a >2-billion-row sort; the same reason
            ! narrow_perm's own guard has no test either. Kept because the failure it prevents is
            ! silent, which is precisely when an untestable guard earns its place. The marker sits
            ! INSIDE the test: the test itself runs on every call, and excluding it too would file
            ! it forever as a stale-exclusion candidate in tools/coverage.sh's own report.
            write (n_str, "(i0)") sentinel - 1_int64
            error stop EP // proc // ": this array has " // trim(n_str) // " elements, so the " // &
                "group offsets do not fit int32; declare group_offsets as integer(int64)"
            ! GCOVR_EXCL_STOP
        end if
        allocate(offsets32(size(offsets64, kind=int64)))
        offsets32 = int(offsets64, int32)
    end procedure narrow_offsets
    !
    ! ---- The M3 engine drivers ----
    ! Unlike drive_engine above, these three always go through the builder, even for a single key.
    ! The one-shot entry points exist to skip a copy on the hottest path in the library, and none
    ! of these is it -- run detection, binary search and merging each cost one extra copy of an
    ! already-extracted buffer in exchange for one entry point per operation instead of three.
    module procedure engine_build_runs
        integer(int64) :: k, nthreads, gek
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
        if (present(resolved_threads)) resolved_threads = 1_int64
        if (nrows < 2_int64) return
        call resolve_thread_count(threads, nrows, nthreads)
        if (present(resolved_threads)) resolved_threads = nthreads
        ! Resolved HERE, never in C++: the boundary carries a real count, never a "0 means all"
        ! sentinel, so the C++ side obeys rather than interprets what a prefix of zero would mean.
        gek = int(size(keys), int64)
        if (present(group_ekeys)) gek = int(group_ekeys, int64)
        if (dbg_fortran_engine) then
            ! **`nthreads` is passed, and that is the whole of what makes `threads=` reach this
            ! path.** It was dropped here for as long as the grouped path existed: the count was
            ! resolved above, handed to the oracle below, and discarded on the branch that actually
            ! ships -- so every `pf_argsort(..., group_offsets=)`, `pf_match`, `pf_unique`,
            ! `pf_rank` and `parquet_table%join` sorted serially whatever the caller asked for,
            ! while the C++ engine on the other branch had been threading all along. Nothing failed:
            ! the permutation is identical at every team size, so only
            ! `parquet_debug_sort_threads_used` could see it.
            call sort_build_runs_permutation(keys, nrows, gek, nthreads, perm, tie)
            return
        end if
        ! **The C++ engine, reached through the pointer parquet_sorting_oracle bound.**
        ! Naming its bind(C) entry points here would put parquet_bindings -- and with it the
        ! whole Arrow stack -- into the use graph of every program that sorts anything, which
        ! is exactly what this tier exists to avoid. The oracle is TEST-ONLY: the shipped path
        ! is the Fortran branch above, and a build that never imports the oracle never
        ! compiles it. check_oracle aborts rather than falling back -- a silent fallback would
        ! make the A/B conformance tests compare the Fortran engine against itself and pass.
        call oracle_runs(keys, nrows, nthreads, gek, proc, perm, tie)
    end procedure engine_build_runs
    !
    module procedure drive_engine_grouped
        integer(c_int8_t), allocatable :: tie(:)
        integer(int64) :: nt !! the builder's resolved count, for the offsets pass.
        !
        if (.not. present(group_offsets)) then
            ! Nothing to report, so nothing is given up: this is drive_engine exactly, one-shot
            ! borrow and all. The branch is what keeps asking for boundaries the only thing that
            ! costs anything.
            call drive_engine(keys, nrows, proc, perm, threads=threads)
            return
        end if
        call engine_build_runs(keys, nrows, proc, perm, tie, threads=threads, group_ekeys=group_ekeys, &
            resolved_threads=nt)
        ! The count the builder resolved, handed on rather than resolved a second time: one
        ! resolution per call is what keeps the sort, the tie pass and this pass on one team,
        ! and a second `resolve_thread_count` would re-run the affinity clamp for nothing.
        call runs_to_offsets(tie, nrows, group_offsets, nt)
    end procedure drive_engine_grouped
    !
    module procedure runs_to_offsets
        integer(int64) :: k, ngroups, pos, c, lo, hi, chunk
        integer(int64), allocatable :: cnt(:) !! per chunk: its groups, then its first slot.
        integer :: team
        !
        team = 1
        if (present(nthreads)) team = tail_team(nthreads, nrows)
        dbg_sort_offsets_threads_used = int(team, int64)
        if (team > 1) then
            ! Three passes on a team: each chunk of ROWS counts the groups that start inside it,
            ! a serial prefix over the chunks turns the counts into each chunk's first slot, and
            ! each chunk fills its own slice. No two chunks write one slot, and the answer is the
            ! serial one below exactly. Chunked by rows rather than by group so that a long run
            ! of ties costs its thread only the flags it reads.
            chunk = (nrows + int(team, int64) - 1_int64) / int(team, int64)
            allocate(cnt(team))
            !$omp parallel do num_threads(team) default(shared) private(c, k, lo, hi, pos) schedule(static)
            do c = 1_int64, int(team, int64)
                lo = (c - 1_int64) * chunk + 1_int64
                hi = min(c * chunk, nrows)
                pos = 0_int64
                do k = lo, hi
                    if (tie(k) == 0_c_int8_t) pos = pos + 1_int64
                end do
                cnt(c) = pos
            end do
            !$omp end parallel do
            ngroups = 0_int64
            do c = 1_int64, int(team, int64)
                pos = cnt(c)
                cnt(c) = ngroups
                ngroups = ngroups + pos
            end do
            allocate(offsets(ngroups + 1_int64))
            !$omp parallel do num_threads(team) default(shared) private(c, k, lo, hi, pos) schedule(static)
            do c = 1_int64, int(team, int64)
                lo = (c - 1_int64) * chunk + 1_int64
                hi = min(c * chunk, nrows)
                pos = cnt(c)
                do k = lo, hi
                    if (tie(k) == 0_c_int8_t) then
                        pos = pos + 1_int64
                        offsets(pos) = k
                    end if
                end do
            end do
            !$omp end parallel do
            offsets(ngroups + 1_int64) = nrows + 1_int64
            return
        end if
        ! Bounded by nrows, NEVER by size(tie): engine_build_runs allocates tie with a
        ! max(nrows, 1) floor, so a zero-row sort leaves one element in it that describes no row
        ! and would otherwise be counted as a group.
        ngroups = 0_int64
        do k = 1_int64, nrows
            if (tie(k) == 0_c_int8_t) ngroups = ngroups + 1_int64
        end do
        allocate(offsets(ngroups + 1_int64))
        pos = 0_int64
        do k = 1_int64, nrows
            if (tie(k) == 0_c_int8_t) then
                pos = pos + 1_int64
                offsets(pos) = k
            end if
        end do
        ! The sentinel. It is what lets group g be perm(o(g) : o(g+1) - 1) for EVERY g including
        ! the last -- and for a zero-row sort it is the array's only entry, so `[1]` means "no
        ! groups" rather than an unallocated result every caller would have to test for.
        offsets(ngroups + 1_int64) = nrows + 1_int64
    end procedure runs_to_offsets
    !
    module procedure argsort_i32_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i32(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_i32_i32
    !
    module procedure argsort_i32_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i32(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_i32_i64
    !
    module procedure argsort_i64_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i64(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_i64_i32
    !
    module procedure argsort_i64_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_i64(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_i64_i64
    !
    module procedure argsort_f32_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f32(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_f32_i32
    !
    module procedure argsort_f32_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f32(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_f32_i64
    !
    module procedure argsort_f64_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f64(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_f64_i32
    !
    module procedure argsort_f64_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_f64(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_f64_i64
    !
    module procedure argsort_bool_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_bool(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_bool_i32
    !
    module procedure argsort_bool_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_bool(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_bool_i64
    !
    module procedure argsort_chr_i32
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        integer(int64), allocatable :: perm64(:), go64(:)
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_chr(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        if (present(group_offsets)) then
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, &
                threads=threads, group_offsets=go64)
            call narrow_offsets(go64, "pf_argsort", group_offsets)
        else
            call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm64, threads=threads)
        end if
        call narrow_perm(perm64, "pf_argsort", perm, threads=threads)
    end procedure argsort_chr_i32
    !
    module procedure argsort_chr_i64
        type(sort_key_buf), allocatable :: buf(:)
        logical :: desc, nlo
        !
        desc = .false.
        if (present(descending)) desc = descending
        nlo = .false.
        if (present(nulls_first)) nlo = nulls_first
        call extract_chr(values, buf, desc, nlo, "pf_argsort", is_valid=is_valid, threads=threads)
        call drive_engine_grouped(buf, size(values, kind=int64), "pf_argsort", perm, threads=threads, &
            group_offsets=group_offsets)
    end procedure argsort_chr_i64
    !
end submodule parquet_argsort_kernel ! GCOVR_EXCL_LINE
