!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file. The module SPEC that declares everything here IS generated
! (tools/generate_parquet_stats.py), so a signature change means editing that script's template
! text while a body change means editing this file.
!
!> The `real64` moment engine of `parquet_stats`, and the argument guards the whole module shares.
!!
!! **One decomposition, at every thread count and in every profile.** The population is split into
!! fixed blocks of `STATS_BLOCK` elements, each block is reduced by the same serial code, and the
!! block results are combined by a fixed binary tree over block index, low to high. That single
!! mechanism is doing two jobs at once, which is why it is here in the first phase rather than
!! arriving with threading later: it IS pairwise summation, so it buys the O(log n) error growth a
!! naive left-to-right sum does not have; and because the tree is a function of the population size
!! alone, a threaded walk of it (P5) cannot change a single bit of any published answer. Introducing
!! it later would have moved every number this module had already printed.
!!
!! **Two traversals of the population, however many statistics are asked for.** Pass one applies the
!! exclusion rules, compacts the survivors and reduces the block sums; pass two accumulates the
!! central moments against the mean pass one produced. Two-pass is both more accurate and faster
!! than a Welford update over a resident array.
!!
!! **`pf_stats` is that engine plus a place to keep the answer, and its two lifecycles are two
!! different promises.** A RETAINED accumulator keeps the surviving values, so `%update` and
!! `%merge` append and defer, and the deferred recomputation is the same two-pass walk over the
!! concatenation -- which makes them EXACT, equal to `%compute` over the concatenated input bit for
!! bit, and costs one recomputation however many batches were folded. A STREAMING accumulator has
!! no buffer to re-walk, so it combines by the Chan/Pebay formulas in `stats_chan`, one traversal
!! per batch and O(1) memory, and its accuracy is theirs. `stats_chan` is the only implementation
!! of those formulas here, reused for the per-element step, the carry tree and `%merge` alike,
!! because they are the part of this file a reader cannot check by inspection.
!!
!! **The accuracy claim is testable and is tested**: `pf_variance(x + 1e9)` agrees with
!! `pf_variance(x)` to a few ulp, which the textbook `sum(x**2) - sum(x)**2/n` misses by orders of
!! magnitude. `test_shift_invariance` in test/test_stats.f90 is that test, and the expectations it
!! compares against come from a 50-digit `mpmath` oracle that shares no arithmetic with this file
!! (tools/generate_stats_vectors.py).
!!
!! **A NaN is tested for with `x /= x`, never `ieee_is_nan`.** These are per-element paths, and
!! `ieee_is_nan` is a genuine runtime call on ifx and on nagfor -- measured at +2.5 ns and +1.6 ns
!! per test, against +0.03 ns for the comparison -- while gfortran inlines both and cannot see the
!! difference. The two are exactly equivalent here: a NaN is the only value not equal to itself, and
!! both spellings are QUIET on a quiet NaN, which `<` and `>` are not. Producing a NaN still goes
!! through `ieee_value`, because building one with `anint`/`int` traps under nagfor's `-ieee=stop`.
submodule (parquet_stats) parquet_stats_core
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
#ifdef _OPENMP
    ! Only for the team observable below, which is written from inside the region so that it
    ! reports what RAN rather than what was decided. No other OpenMP entry point is called here:
    ! the thread COUNT comes from `resolve_thread_count`, one tier down.
    use omp_lib, only : omp_get_num_threads
#endif
    implicit none

    !> Elements per block of the fixed decomposition.
    !!
    !! A function of nothing but the population size -- 128 is numpy's own pairwise block and is
    !! large enough that the per-block bookkeeping is noise, small enough that the serial sum inside
    !! a block contributes no meaningful error. **Changing it changes published answers in the last
    !! bits**, so it is a frozen constant rather than a tuning knob.
    integer(int64), parameter :: STATS_BLOCK = 128_int64

    !> How many full traversals of a population's values this process has performed.
    !!
    !! Read by `parquet_debug_stats_scans()` and by nothing else. Process-global and deliberately
    !! unsynchronised: it is a test observable, not library state, and the `stats` suite is
    !! excluded from `test/run_tester.f90`'s per-suite parallelism for exactly this reason.
    integer(int64), save :: stats_scan_count = 0_int64

    !> The deepest carry level the streaming combiner can need.
    !!
    !! One level per bit of an `integer(int64)` block count, so it cannot be reached.
    integer, parameter :: STATS_MAXLEV = 63

    !> Survivors each thread must get from pass two before a team is worth opening.
    !!
    !! **Measured, on the machine and with the harness named below, not chosen.**
    !! `bench/benchmark_stats.sh --mode=thread` times pass two over a resident buffer on a thread
    !! ladder; on machine A (gfortran 15.2, 8 cores) the speedup at 8 threads runs
    !!
    !!     n =    16384   32768   65536  131072  262144  524288    1e6    1e7    1e8
    !!     8t   =  0.55x   0.66x   0.99x   1.23x   1.57x   1.84x  1.91x  2.22x  2.31x
    !!
    !! so a team opened below about 32768 survivors PER THREAD is a loss, and at 16384 elements
    !! with eight threads it is a **1.8x loss** rather than a small one. The rule below therefore
    !! caps the team at `n / STATS_MIN_PER_THREAD` and runs serially when that leaves fewer than
    !! two, which admits no measured loss at any size on that ladder.
    !!
    !! **It is deliberately NOT `tail_team`'s floor**, though the shape is the same: that one is
    !! for a memcpy-shaped pass and its own comment says one number cannot be right for both
    !! profiles. Pass two is compute-bound -- eight flops per element over four independent
    !! accumulators -- so it wants its own number.
    !!
    !! **This is a `parquet_debug_set_*` hook, not a setting**, per the design's settings analysis:
    !! it changes how fast the module runs and not what it answers, so it would pass the admission
    !! test, but no user has a reason to tune it and its only job is to let a test reach both sides
    !! of a size-dependent branch (CLAUDE.md's size-threshold rule).
    integer(int64), parameter :: STATS_MIN_PER_THREAD = 32768_int64

    !> Test-only override for `STATS_MIN_PER_THREAD`; negative means the measured value applies.
    integer(int64), save :: dbg_stats_min_per_thread = -1_int64

    !> The team size pass two actually ran on, most recently; 1 means it ran serially.
    !!
    !! Read by `parquet_debug_stats_team()` and by nothing else. It exists because **an equality
    !! test cannot see threading**: comparing `threads=1` against `threads=8` passes identically
    !! against an engine that never opens a team, which is precisely what the fixed decomposition
    !! guarantees. Without this, every threading assertion in the suite would be vacuous.
    !!
    !! **It is written from INSIDE the region, by `omp_get_num_threads`, and that is the whole
    !! point.** Recording instead what `stats_pass_two_team` decided would make the observable
    !! agree with the engine's intention rather than with its behaviour -- a mutation deleting the
    !! `!$omp` directive outright would leave it still reporting 8, and the negative control it
    !! exists to be would pass against an engine that had stopped threading entirely. That was not
    !! hypothetical: the first version of this counter did exactly that, and a mutation round found
    !! it by surviving.
    integer(int64), save :: stats_team_used = 1_int64


contains

    ! ==================================================================================
    ! The shared argument guards
    !
    ! These are separate module procedures, and both submodules of `parquet_stats` call them.
    ! nagfor binds a separate module procedure's name to an external at its first call site, so
    ! every one of them is IMPLEMENTED here before anything below calls it (CLAUDE.md).
    ! ==================================================================================

    module procedure stats_i2s
        write(res, '(i0)') v
    end procedure stats_i2s

    module procedure stats_check_sizes
        if (present(is_valid)) then
            if (size(is_valid, kind=int64) /= nv) &
                error stop what // ": is_valid has " // trim(stats_i2s(size(is_valid, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(nv))
        end if
        if (present(weights)) then
            if (size(weights, kind=int64) /= nv) &
                error stop what // ": weights has " // trim(stats_i2s(size(weights, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(nv))
        end if
    end procedure stats_check_sizes

    module procedure stats_check_weight
        ! Three separate statements: Fortran does not short-circuit, so `w /= w .or. w < 0` would
        ! evaluate the `<` on a NaN and raise IEEE_INVALID, which nagfor's default -ieee=stop turns
        ! into a dead process. Only `==` and `/=` are quiet on a NaN.
        if (w /= w) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is NaN; weights must be finite and non-negative"
        if (w < 0.0_real64) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is negative; weights must be finite and non-negative"
        if (w > huge(0.0_real64)) error stop what // ": weight " // trim(stats_i2s(i)) // &
            " is infinite; weights must be finite and non-negative"
    end procedure stats_check_weight

    ! ==================================================================================
    ! Private helpers
    ! ==================================================================================

    !> A quiet NaN, the value every undefined statistic in this module returns.
    !!
    !! Built with `ieee_value` rather than by arithmetic: `0.0/0.0` and anything routed through
    !! `anint`/`int` raise under nagfor's default `-ieee=stop`, which terminates the process.
    pure function stats_nan() result(res)
        real(real64) :: res !! a quiet NaN.
        res = ieee_value(1.0_real64, ieee_quiet_nan)
    end function stats_nan

    !> Reduces `a(1:n)` in place by a fixed binary tree over index, leaving the total in `a(1)`.
    !!
    !! The tree is determined by `n` alone -- adjacent pairs low to high, an odd tail carried
    !! forward untouched -- so the result does not depend on who walks it. That is what makes the
    !! module's answers identical at every thread count, and it is pairwise summation, so the
    !! relative error grows as O(log n) rather than the O(n) of a running total.
    pure subroutine pair_reduce(a, n)
        real(real64), intent(inout) :: a(:) !! the per-block partials; overwritten.
        integer(int64), intent(in) :: n     !! how many of them are live.
        integer(int64) :: m, k, j

        m = n
        do while (m > 1_int64)
            k = 0_int64
            do j = 1_int64, m - 1_int64, 2_int64
                k = k + 1_int64
                a(k) = a(j) + a(j + 1_int64)
            end do
            if (mod(m, 2_int64) == 1_int64) then
                k = k + 1_int64
                a(k) = a(m)
            end if
            m = k
        end do
    end subroutine pair_reduce

    !> Resolves `weight_type` to the one bit the formulas actually branch on.
    subroutine stats_weight_kind(what, weight_type, freq)
        character(len=*), intent(in) :: what             !! the public procedure's name.
        character(len=*), intent(in), optional :: weight_type !! the caller's token, if any.
        logical, intent(out) :: freq                     !! .true. for frequency weights.
        integer, parameter :: ECHO = 60                  !! cap the echoed token; see below.

        freq = .false.
        if (.not. present(weight_type)) return
        select case (trim(adjustl(weight_type)))
        case ("reliability")
            freq = .false.
        case ("frequency")
            freq = .true.
        case default
            ! The echoed token is capped: ifx's ERROR STOP runtime corrupts the heap once the
            ! composed message reaches 8192 bytes, and the caller controls this string's length.
            error stop what // ": weight_type """ // trim(adjustl(weight_type(1:min(ECHO, len(weight_type))))) // &
                """ is not recognised; use ""reliability"" (the default) or ""frequency"""
        end select
    end subroutine stats_weight_kind

    ! ==================================================================================
    ! The engine
    ! ==================================================================================

    !> Reduces one population to its tier-A quantities, in two traversals.
    !!
    !! Pass one walks `values`, applies the exclusion rules in their fixed order, compacts the
    !! survivors into `xb` and closes each block's `sum(w)` and `sum(w*x)` as it fills. Pass two
    !! walks `xb` and accumulates the central moments against the mean pass one produced. Both
    !! passes use the same block boundaries, and `pair_reduce` combines the partials by the same
    !! fixed tree, so nothing here depends on how the blocks are walked.
    !!
    !! The unweighted, mask-free, NaN-skipping case gets its own compaction loop. It is the shape
    !! nearly every call has, and giving it a loop with no per-element `present` test or weight
    !! branch is the difference between a tight copy and a predicted branch per element.
    subroutine stats_engine(values, what, is_valid, weights, skipnan, acc, keep_x, keep_w, threads)
        real(real64), intent(in) :: values(:)                 !! the population, before exclusions.
        character(len=*), intent(in) :: what                  !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)          !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)      !! per element weight.
        logical, intent(in), optional :: skipnan              !! .true. (default) excludes a NaN.
        type(stats_acc), intent(out) :: acc                   !! everything tier A holds.
        real(real64), allocatable, intent(out), optional :: keep_x(:)
        !! when present, receives pass one's compacted survivors. `acc%n_valid` of them are live
        !! and the rest of the array is spare capacity -- handing the buffer over rather than
        !! re-deriving it is what lets a retaining `%compute` cost two traversals rather than three.
        real(real64), allocatable, intent(out), optional :: keep_w(:)
        !! likewise their weights, allocated only when `weights` was supplied.
        integer, intent(in), optional :: threads
        !! how many threads pass two may use. Absent takes the automatic rule; see
        !! `stats_pass_two_team`. **The answer does not depend on this argument** -- the block
        !! decomposition is a function of the population size alone, so every thread count,
        !! including 1 and including a serial build, returns the identical bits.

        real(real64), allocatable :: xb(:), wb(:), pw(:), px(:), q1(:), q2(:), q3(:), q4(:)
        real(real64) :: x, w, sw, sx, mu, delta
        integer(int64) :: nv, m, nb, i, c, j, team
        logical :: skip, weighted, masked, threaded

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, what, is_valid, weights)
        stats_scan_count = stats_scan_count + 1_int64   ! pass one
        skip = .true.
        if (present(skipnan)) skip = skipnan
        weighted = present(weights)
        masked = present(is_valid)

        allocate(xb(nv))
        if (weighted) allocate(wb(nv))
        allocate(px(nv / STATS_BLOCK + 1_int64))
        if (weighted) allocate(pw(nv / STATS_BLOCK + 1_int64))

        ! ---- Pass one: exclude, compact, and close each block's sums ----
        m = 0_int64
        nb = 0_int64
        c = 0_int64
        sw = 0.0_real64
        sx = 0.0_real64
        if (.not. masked .and. .not. weighted .and. skip) then
            do i = 1_int64, nv
                x = values(i)
                if (x /= x) then
                    acc%n_nan = acc%n_nan + 1_int64
                    cycle
                end if
                m = m + 1_int64
                xb(m) = x
                sx = sx + x
                if (m == 1_int64) then
                    acc%vmin = x
                    acc%vmax = x
                else
                    if (x < acc%vmin) acc%vmin = x
                    if (x > acc%vmax) acc%vmax = x
                end if
                c = c + 1_int64
                if (c == STATS_BLOCK) then
                    nb = nb + 1_int64
                    px(nb) = sx
                    sx = 0.0_real64
                    c = 0_int64
                end if
            end do
        else
            do i = 1_int64, nv
                ! The family's exclusion order: nullness, then NaN, then weight. An element already
                ! out of the population never has its weight examined, which is what keeps a NaN
                ! weight beside a null value -- the shape `1/err**2` produces -- from aborting.
                if (masked) then
                    if (.not. is_valid(i)) then
                        acc%n_null = acc%n_null + 1_int64
                        cycle
                    end if
                end if
                x = values(i)
                if (skip) then
                    if (x /= x) then
                        acc%n_nan = acc%n_nan + 1_int64
                        cycle
                    end if
                else if (x /= x) then
                    acc%saw_nan = .true.
                end if
                w = 1.0_real64
                if (weighted) then
                    call stats_check_weight(weights(i), i, what)
                    if (weights(i) <= 0.0_real64) cycle
                    w = weights(i)
                end if
                m = m + 1_int64
                xb(m) = x
                if (weighted) wb(m) = w
                if (.not. acc%saw_nan) then
                    if (m == 1_int64) then
                        acc%vmin = x
                        acc%vmax = x
                    else
                        if (x < acc%vmin) acc%vmin = x
                        if (x > acc%vmax) acc%vmax = x
                    end if
                end if
                sw = sw + w
                sx = sx + w * x
                c = c + 1_int64
                if (c == STATS_BLOCK) then
                    nb = nb + 1_int64
                    px(nb) = sx
                    if (weighted) pw(nb) = sw
                    sx = 0.0_real64
                    sw = 0.0_real64
                    c = 0_int64
                end if
            end do
        end if
        if (c > 0_int64) then
            nb = nb + 1_int64
            px(nb) = sx
            if (weighted) pw(nb) = sw
        end if

        acc%n_valid = m
        acc%empty = (m == 0_int64)
        if (acc%empty) then
            ! The additive identity is what numpy and pandas return for an empty sum, and `n_valid`
            ! sits beside it, so nothing is hidden by answering 0 rather than NaN here.
            acc%vsum = 0.0_real64
            call stats_undefine(acc)
            call stats_hand_over(xb, wb, keep_x, keep_w)
            return
        end if
        if (acc%saw_nan) then
            ! `skipnan = .false.` and a NaN survived: the caller asked for propagation, so every
            ! answer is NaN. Returning before the arithmetic also keeps the min/max comparisons and
            ! (in P8) the transcendentals away from it.
            acc%vsum = stats_nan()
            call stats_undefine(acc)
            call stats_hand_over(xb, wb, keep_x, keep_w)
            return
        end if

        call pair_reduce(px, nb)
        acc%vsum = px(1)
        if (weighted) then
            call pair_reduce(pw, nb)
            acc%w_sum = pw(1)
            acc%w_sq = sum_of_squares(wb, m, nb)
        else
            acc%w_sum = real(m, real64)
            acc%w_sq = real(m, real64)
        end if
        mu = acc%vsum / acc%w_sum
        stats_scan_count = stats_scan_count + 1_int64   ! pass two

        ! ---- Pass two: the central moments, against the mean pass one produced ----
        !
        ! `q1` accumulates `sum(w*d)`, which is algebraically ZERO and in floating point is the
        ! rounding error left in `mu`. Carrying it is what makes the THIRD and FOURTH moments
        ! shift-invariant, and it is not optional: the second moment is immune to a perturbed mean
        ! because its derivative there vanishes, and the higher ones are not. `m3` picks the error
        ! up linearly through `3*delta*m2`, which at an offset of 1e9 was measured costing eight
        ! significant digits of the skewness while the variance was still correct to fifteen.
        !
        ! **Threaded over blocks, and that cannot move a bit.** Each block writes only its own
        ! `q1(j)..q4(j)` from values only it reads, and the four `pair_reduce` calls below then walk
        ! a tree fixed by `nb` alone -- so who computed which block is not observable in the result.
        ! `bench/benchmark_stats.sh --mode=thread` asserts exactly that before reporting any timing,
        ! and `test_threading_changes_no_bit` asserts it in the suite.
        !
        ! Pass ONE is deliberately left serial. Its compaction is a prefix-dependent scatter -- the
        ! slot an element lands in depends on how many earlier ones were excluded -- so threading it
        ! needs a count-then-scatter restructure rather than a directive. It is roughly half the
        ! call and the restructure is not free; see feature_pandas_S4.md's P5 section.
        allocate(q1(nb), q2(nb), q3(nb), q4(nb))
        team = stats_pass_two_team(threads, m)
        stats_team_used = 1_int64
        threaded = .false.
#ifdef _OPENMP
        if (team > 1_int64) then
            threaded = .true.
            !$omp parallel default(shared) private(j) num_threads(int(team))
            !$omp single
            stats_team_used = int(omp_get_num_threads(), int64)
            !$omp end single nowait
            !$omp do schedule(static)
            do j = 1_int64, nb
                call stats_block_moments(xb, wb, mu, j, m, q1(j), q2(j), q3(j), q4(j))
            end do
            !$omp end do
            !$omp end parallel
        end if
#endif
        if (.not. threaded) then
            do j = 1_int64, nb
                call stats_block_moments(xb, wb, mu, j, m, q1(j), q2(j), q3(j), q4(j))
            end do
        end if
        call pair_reduce(q1, nb)
        call pair_reduce(q2, nb)
        call pair_reduce(q3, nb)
        call pair_reduce(q4, nb)

        ! Re-centre on `mu + delta`. Expanding `sum(w*(d-delta)**k)` and using `sum(w*d) = delta*W`
        ! collapses every cross term, leaving these four lines. `delta` is a rounding error, so the
        ! corrections are tiny and none of them can cancel anything significant away.
        delta = q1(1) / acc%w_sum
        acc%mean = mu + delta
        acc%m2 = q2(1) - delta * delta * acc%w_sum
        acc%m3 = q3(1) - 3.0_real64 * delta * q2(1) + 2.0_real64 * delta**3 * acc%w_sum
        acc%m4 = q4(1) - 4.0_real64 * delta * q3(1) + 6.0_real64 * delta * delta * q2(1) &
            - 3.0_real64 * delta**4 * acc%w_sum
        call stats_hand_over(xb, wb, keep_x, keep_w)
    end subroutine stats_engine

    !> One block of pass two: the four weighted central-moment sums about `mu`.
    !!
    !! Extracted from `stats_engine` so that the threaded and serial walks share **one** copy of
    !! the arithmetic rather than two that could drift -- which for this procedure would not be a
    !! maintenance annoyance but a wrong answer at one thread count and not the other.
    !!
    !! `pure` on purpose: it reads `xb`/`wb` and writes nothing but its own four results, which is
    !! what makes the region above correct by inspection rather than by argument.
    !!
    !! **`wb` is OPTIONAL, and that is a conformance requirement rather than a convenience.** The
    !! engine allocates its weight buffer only for a weighted call, so on the common path the actual
    !! argument is an unallocated allocatable -- which may not be associated with a non-optional
    !! dummy at all (F2018 15.5.2.4). Passed to an optional one it is simply ABSENT, which is a
    !! documented Fortran rule this library already relies on elsewhere, and `present(wb)` then
    !! *is* the weighted test, so there is no second flag that could disagree with it. gfortran
    !! accepts the non-conforming form silently; nagfor's `-C=all` is what would not.
    pure subroutine stats_block_moments(xb, wb, mu, j, m, o1, o2, o3, o4)
        real(real64), intent(in) :: xb(:)     !! the compacted survivors.
        real(real64), intent(in), optional :: wb(:)
        !! their weights. Absent for an unweighted population, which is what an unallocated actual
        !! argument produces; its presence is the weighted test.
        real(real64), intent(in) :: mu        !! the mean pass one produced.
        integer(int64), intent(in) :: j       !! which block, 1-based.
        integer(int64), intent(in) :: m       !! how many survivors there are in total.
        real(real64), intent(out) :: o1       !! `sum(w*d)`, the re-centring term.
        real(real64), intent(out) :: o2       !! `sum(w*d**2)`.
        real(real64), intent(out) :: o3       !! `sum(w*d**3)`.
        real(real64), intent(out) :: o4       !! `sum(w*d**4)`.
        real(real64) :: s1, s2, s3, s4, d, dd
        integer(int64) :: i, lo, hi

        lo = (j - 1_int64) * STATS_BLOCK + 1_int64
        hi = min(j * STATS_BLOCK, m)
        s1 = 0.0_real64
        s2 = 0.0_real64
        s3 = 0.0_real64
        s4 = 0.0_real64
        if (present(wb)) then
            do i = lo, hi
                d = xb(i) - mu
                dd = d * d
                s1 = s1 + wb(i) * d
                s2 = s2 + wb(i) * dd
                s3 = s3 + wb(i) * dd * d
                s4 = s4 + wb(i) * dd * dd
            end do
        else
            do i = lo, hi
                d = xb(i) - mu
                dd = d * d
                s1 = s1 + d
                s2 = s2 + dd
                s3 = s3 + dd * d
                s4 = s4 + dd * dd
            end do
        end if
        o1 = s1
        o2 = s2
        o3 = s3
        o4 = s4
    end subroutine stats_block_moments

    !> How many threads pass two may open, given the caller's request and the work available.
    !!
    !! Two rules, and neither is this module's own invention:
    !!
    !!   * **the thread count comes from `resolve_thread_count`** (`parquet_argsort`), which is the
    !!     one place that honours an explicit `threads=`, refuses a nested team where libgomp would
    !!     deadlock (`feature_risks.md` Risk-104), reads `parquet_sort_threads` for the automatic
    !!     case and clamps to `omp_get_num_procs()`. The design's settings analysis is explicit that
    !!     this module adds no thread knob of its own: a `parquet_set_stats_threads` would be a
    !!     second answer to a question that already has one.
    !!   * **the work floor is this module's own**, because pass two is compute-bound where the
    !!     sort's tail pass is memcpy-shaped. See `STATS_MIN_PER_THREAD` for the ladder it was
    !!     measured from and for why a team opened too early is a 1.8x loss rather than a rounding
    !!     error.
    !!
    !! Returns 1 on a build without OpenMP, so the caller's serial branch is the only live one
    !! there and the `#ifdef` around the region has nothing to fall through to.
    function stats_pass_two_team(threads, m) result(team)
        integer, intent(in), optional :: threads !! the caller's request; absent means automatic.
        integer(int64), intent(in) :: m          !! survivors pass two will walk.
        integer(int64) :: team                   !! threads to open; 1 runs serially.
        integer(int64) :: nt, floor_n

        team = 1_int64
#ifdef _OPENMP
        call resolve_thread_count(threads, m, nt)
        if (nt <= 1_int64) return
        floor_n = STATS_MIN_PER_THREAD
        if (dbg_stats_min_per_thread >= 0_int64) floor_n = dbg_stats_min_per_thread
        ! A floor of 0 is the override saying "team up whatever the size", which is how a test
        ! reaches the threaded branch on a fixture small enough to check by hand.
        if (floor_n <= 0_int64) then
            team = nt
            return
        end if
        team = min(nt, m / floor_n)
        if (team < 2_int64) team = 1_int64
#else
        ! No OpenMP: there is no team to open, whatever was asked for. The two locals are assigned
        ! so that a serial build does not report them unused, and neither can change the answer.
        nt = 1_int64
        floor_n = 1_int64
        if (present(threads)) team = max(1_int64, nt * floor_n)
#endif
    end function stats_pass_two_team

    !> Hands pass one's compacted buffers to a caller that asked for them, or lets them go.
    !!
    !! `move_alloc` rather than a copy: the arrays are sized to the whole input, so copying them
    !! would put an O(n) allocation back on the path this hand-off exists to remove. `wb` is
    !! unallocated for an unweighted population, and `move_alloc` of an unallocated source leaves
    !! the destination unallocated, which is exactly the state `pf_stats` records as unweighted.
    subroutine stats_hand_over(xb, wb, keep_x, keep_w)
        real(real64), allocatable, intent(inout) :: xb(:) !! pass one's survivors.
        real(real64), allocatable, intent(inout) :: wb(:) !! their weights, if any.
        real(real64), allocatable, intent(out), optional :: keep_x(:) !! the caller's buffer.
        real(real64), allocatable, intent(out), optional :: keep_w(:) !! the caller's weights.
        if (present(keep_x)) call move_alloc(xb, keep_x)
        if (present(keep_w)) then
            if (allocated(wb)) call move_alloc(wb, keep_w)
        end if
    end subroutine stats_hand_over

    !> Marks every moment of `acc` undefined, leaving the counts and `vsum` alone.
    pure subroutine stats_undefine(acc)
        type(stats_acc), intent(inout) :: acc !! the accumulator to blank.
        acc%mean = stats_nan()
        acc%m2 = stats_nan()
        acc%m3 = stats_nan()
        acc%m4 = stats_nan()
        acc%vmin = stats_nan()
        acc%vmax = stats_nan()
        acc%w_sum = 0.0_real64
        acc%w_sq = 0.0_real64
    end subroutine stats_undefine

    !> `sum(w(1:m)**2)` over the same block tree everything else here uses.
    function sum_of_squares(w, m, nb) result(res)
        real(real64), intent(in) :: w(:)  !! the weights, already compacted.
        integer(int64), intent(in) :: m   !! how many are live.
        integer(int64), intent(in) :: nb  !! how many blocks they occupy.
        real(real64) :: res               !! the pairwise sum of their squares.
        real(real64), allocatable :: p(:)
        real(real64) :: s
        integer(int64) :: j, i, lo, hi

        allocate(p(nb))
        do j = 1_int64, nb
            lo = (j - 1_int64) * STATS_BLOCK + 1_int64
            hi = min(j * STATS_BLOCK, m)
            s = 0.0_real64
            do i = lo, hi
                s = s + w(i) * w(i)
            end do
            p(j) = s
        end do
        call pair_reduce(p, nb)
        res = p(1)
    end function sum_of_squares

    ! ==================================================================================
    ! Derived quantities
    !
    ! Each returns a quiet NaN outside its domain rather than aborting: an empty group, a group of
    ! one, or a group whose values are all identical are ordinary things for a per-group loop to
    ! meet, and a library that aborts there forces every caller to guard every reduction.
    ! ==================================================================================

    !> The effective sample size the degrees of freedom are charged against.
    !!
    !! `sum(w)` for frequency weights, where a weight of 3 says the value occurred three times;
    !! Kish's `sum(w)**2 / sum(w**2)` for reliability weights, where it says the value is that much
    !! more precise. Both are exactly the population size when every weight is 1, which is what
    !! makes the two conventions indistinguishable until the weights are unequal.
    pure function stats_neff(acc, freq) result(res)
        type(stats_acc), intent(in) :: acc !! the population.
        logical, intent(in) :: freq        !! .true. for frequency weights.
        real(real64) :: res                !! the effective count.
        if (acc%w_sq <= 0.0_real64) then
            res = 0.0_real64
        else if (freq) then
            res = acc%w_sum
        else
            res = acc%w_sum * acc%w_sum / acc%w_sq
        end if
    end function stats_neff

    !> The variance, or NaN when the degrees of freedom leave nothing to divide by.
    pure function stats_var(acc, ddof, freq) result(res)
        type(stats_acc), intent(in) :: acc !! the population.
        integer, intent(in) :: ddof        !! delta degrees of freedom.
        logical, intent(in) :: freq        !! .true. for frequency weights.
        real(real64) :: res                !! the variance, or NaN.
        real(real64) :: denom

        if (acc%empty .or. acc%saw_nan .or. acc%w_sum <= 0.0_real64) then
            res = stats_nan()
            return
        end if
        if (freq) then
            denom = acc%w_sum - real(ddof, real64)
        else
            denom = acc%w_sum - real(ddof, real64) * acc%w_sq / acc%w_sum
        end if
        if (denom > 0.0_real64) then
            res = acc%m2 / denom
        else
            res = stats_nan()
        end if
    end function stats_var

    !> The skewness: `g1` uncorrected, `G1` corrected, NaN when the population has no spread.
    pure function stats_skew(acc, bias, freq) result(res)
        type(stats_acc), intent(in) :: acc !! the population.
        logical, intent(in) :: bias        !! .true. leaves it uncorrected (scipy's default).
        logical, intent(in) :: freq        !! .true. for frequency weights.
        real(real64) :: res                !! the skewness, or NaN.
        real(real64) :: p2, p3, n

        res = stats_nan()
        if (acc%empty .or. acc%saw_nan .or. acc%w_sum <= 0.0_real64) return
        p2 = acc%m2 / acc%w_sum
        p3 = acc%m3 / acc%w_sum
        if (p2 <= 0.0_real64) return
        res = p3 / (p2 * sqrt(p2))
        if (bias) return
        n = stats_neff(acc, freq)
        if (n <= 2.0_real64) then
            res = stats_nan()
        else
            res = res * sqrt(n * (n - 1.0_real64)) / (n - 2.0_real64)
        end if
    end function stats_skew

    !> The kurtosis: `g2` uncorrected, `G2` corrected, excess unless `excess` says otherwise.
    pure function stats_kurt(acc, bias, excess, freq) result(res)
        type(stats_acc), intent(in) :: acc !! the population.
        logical, intent(in) :: bias        !! .true. leaves it uncorrected (scipy's default).
        logical, intent(in) :: excess      !! .true. subtracts 3, so a normal population gives 0.
        logical, intent(in) :: freq        !! .true. for frequency weights.
        real(real64) :: res                !! the kurtosis, or NaN.
        real(real64) :: p2, p4, n

        res = stats_nan()
        if (acc%empty .or. acc%saw_nan .or. acc%w_sum <= 0.0_real64) return
        p2 = acc%m2 / acc%w_sum
        p4 = acc%m4 / acc%w_sum
        if (p2 <= 0.0_real64) return
        res = p4 / (p2 * p2) - 3.0_real64
        if (.not. bias) then
            n = stats_neff(acc, freq)
            if (n <= 3.0_real64) then
                res = stats_nan()
                return
            end if
            res = ((n + 1.0_real64) * res + 6.0_real64) * (n - 1.0_real64) &
                / ((n - 2.0_real64) * (n - 3.0_real64))
        end if
        if (.not. excess) res = res + 3.0_real64
    end function stats_kurt

    !> The standard error of the mean: `stddev / sqrt(n_eff)`.
    pure function stats_sem(acc, ddof, freq) result(res)
        type(stats_acc), intent(in) :: acc !! the population.
        integer, intent(in) :: ddof        !! delta degrees of freedom.
        logical, intent(in) :: freq        !! .true. for frequency weights.
        real(real64) :: res                !! the standard error, or NaN.
        real(real64) :: v, n

        v = stats_var(acc, ddof, freq)
        n = stats_neff(acc, freq)
        ! Two statements, not `v /= v .or. n <= 0`: Fortran does not short-circuit, and while `n`
        ! cannot be NaN here, writing the guard this way is what stops the next edit relying on it.
        res = stats_nan()
        if (v /= v) return
        if (n > 0.0_real64) res = sqrt(v) / sqrt(n)
    end function stats_sem

    !> The square root of a variance, propagating NaN without reaching `sqrt` on one.
    pure function stats_sqrt(v) result(res)
        real(real64), intent(in) :: v !! a variance, possibly NaN.
        real(real64) :: res           !! its square root, or NaN.
        ! The NaN test must stand alone: `v /= v .or. v < 0` evaluates the `<` on a NaN, which
        ! raises IEEE_INVALID and kills the process under nagfor's default -ieee=stop.
        res = stats_nan()
        if (v /= v) return
        if (v >= 0.0_real64) res = sqrt(v)
    end function stats_sqrt

    ! ==================================================================================
    ! The public real64 specifics
    !
    ! Each is the same four steps: resolve the options, run the engine, report the counts, and
    ! answer. `ok` is set from the answer itself -- `.true.` exactly when a number came back -- so
    ! it cannot drift from the value beside it.
    ! ==================================================================================

    module procedure sum_f64
        type(stats_acc) :: acc
        call stats_engine(values, "pf_sum", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        s = acc%vsum
        if (present(ok)) ok = (s == s)
    end procedure sum_f64

    module procedure mean_f64
        type(stats_acc) :: acc
        call stats_engine(values, "pf_mean", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        m = acc%mean
        if (present(ok)) ok = (m == m)
    end procedure mean_f64

    module procedure variance_f64
        type(stats_acc) :: acc
        logical :: freq
        integer :: dd
        call stats_weight_kind("pf_variance", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        call stats_engine(values, "pf_variance", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        v = stats_var(acc, dd, freq)
        if (present(ok)) ok = (v == v)
    end procedure variance_f64

    module procedure stddev_f64
        type(stats_acc) :: acc
        logical :: freq
        integer :: dd
        call stats_weight_kind("pf_stddev", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        call stats_engine(values, "pf_stddev", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        sd = stats_sqrt(stats_var(acc, dd, freq))
        if (present(ok)) ok = (sd == sd)
    end procedure stddev_f64

    module procedure sem_f64
        type(stats_acc) :: acc
        logical :: freq
        integer :: dd
        call stats_weight_kind("pf_sem", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        call stats_engine(values, "pf_sem", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        se = stats_sem(acc, dd, freq)
        if (present(ok)) ok = (se == se)
    end procedure sem_f64

    module procedure skewness_f64
        type(stats_acc) :: acc
        logical :: freq, bs
        call stats_weight_kind("pf_skewness", weight_type, freq)
        bs = .false.
        if (present(bias)) bs = bias
        call stats_engine(values, "pf_skewness", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        g = stats_skew(acc, bs, freq)
        if (present(ok)) ok = (g == g)
    end procedure skewness_f64

    module procedure kurtosis_f64
        type(stats_acc) :: acc
        logical :: freq, bs, ex
        call stats_weight_kind("pf_kurtosis", weight_type, freq)
        bs = .false.
        if (present(bias)) bs = bias
        ex = .true.
        if (present(excess)) ex = excess
        call stats_engine(values, "pf_kurtosis", is_valid, weights, skipnan, acc, threads=threads)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        k = stats_kurt(acc, bs, ex, freq)
        if (present(ok)) ok = (k == k)
    end procedure kurtosis_f64

    module procedure moments_f64
        type(stats_acc) :: acc
        logical :: freq, bs, ex
        integer :: dd

        call stats_weight_kind("pf_moments", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        bs = .false.
        if (present(bias)) bs = bias
        ex = .true.
        if (present(excess)) ex = excess
        call stats_engine(values, "pf_moments", is_valid, weights, skipnan, acc, threads=threads)

        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        if (present(n_valid)) n_valid = acc%n_valid
        if (present(mean)) mean = acc%mean
        if (present(variance)) variance = stats_var(acc, dd, freq)
        if (present(stddev)) stddev = stats_sqrt(stats_var(acc, dd, freq))
        if (present(sem)) sem = stats_sem(acc, dd, freq)
        if (present(skewness)) skewness = stats_skew(acc, bs, freq)
        if (present(kurtosis)) kurtosis = stats_kurt(acc, bs, ex, freq)
        if (present(vsum)) vsum = acc%vsum
        if (present(vmin)) vmin = acc%vmin
        if (present(vmax)) vmax = acc%vmax
    end procedure moments_f64

    ! ==================================================================================
    ! Combining populations
    !
    ! One routine folds one accumulator into another, and everything that is not a two-pass walk
    ! of a resident array is built from it: the per-element step of a streaming update, the carry
    ! tree that combines its blocks, and `%merge` in streaming mode. Having exactly one
    ! implementation of the Chan/Pebay formulas is the point -- they are the part of this file a
    ! reader cannot check by inspection, so they are tested once and reused rather than restated.
    ! ==================================================================================

    !> Folds `b` into `a`, combining the counts and the first four central moments.
    !!
    !! The Chan/Pebay parallel-update formulas, with the element counts replaced by weight sums,
    !! which is the weighted generalisation and reduces to the textbook form at unit weights. The
    !! assignment ORDER below is load-bearing: `m4` reads the old `m2` and `m3`, and `m3` reads the
    !! old `m2`, so computing them in any other order silently uses already-updated inputs.
    !!
    !! Three states short-circuit, and each is a real case rather than a guard against nothing: an
    !! accumulator already poisoned by a NaN cannot be un-poisoned; an empty source contributes
    !! only its counts; and an empty destination takes the source's moments wholesale, which is
    !! what makes `%init` followed by one `%update` exact rather than a combination of an empty
    !! population with a full one.
    pure subroutine stats_chan(a, b)
        type(stats_acc), intent(inout) :: a !! the destination; folded in place.
        type(stats_acc), intent(in) :: b    !! the source, left alone.
        real(real64) :: wa, wb, w, d, d2

        a%n_null = a%n_null + b%n_null
        a%n_nan = a%n_nan + b%n_nan
        a%n_valid = a%n_valid + b%n_valid
        if (a%saw_nan) return
        if (b%saw_nan) then
            a%saw_nan = .true.
            a%empty = a%empty .and. b%empty
            a%vsum = stats_nan()
            call stats_undefine(a)
            return
        end if
        if (b%empty) return
        if (a%empty) then
            a%w_sum = b%w_sum
            a%w_sq = b%w_sq
            a%vsum = b%vsum
            a%mean = b%mean
            a%m2 = b%m2
            a%m3 = b%m3
            a%m4 = b%m4
            a%vmin = b%vmin
            a%vmax = b%vmax
            a%empty = .false.
            return
        end if

        wa = a%w_sum
        wb = b%w_sum
        w = wa + wb
        d = b%mean - a%mean
        d2 = d * d
        a%m4 = a%m4 + b%m4 + d2 * d2 * wa * wb * (wa * wa - wa * wb + wb * wb) / (w * w * w) &
            + 6.0_real64 * d2 * (wa * wa * b%m2 + wb * wb * a%m2) / (w * w) &
            + 4.0_real64 * d * (wa * b%m3 - wb * a%m3) / w
        a%m3 = a%m3 + b%m3 + d2 * d * wa * wb * (wa - wb) / (w * w) &
            + 3.0_real64 * d * (wa * b%m2 - wb * a%m2) / w
        a%m2 = a%m2 + b%m2 + d2 * wa * wb / w
        a%mean = a%mean + d * wb / w
        a%vsum = a%vsum + b%vsum
        a%w_sq = a%w_sq + b%w_sq
        a%w_sum = w
        if (b%vmin < a%vmin) a%vmin = b%vmin
        if (b%vmax > a%vmax) a%vmax = b%vmax
    end subroutine stats_chan

    !> The accumulator describing one element: the identity every streaming fold starts from.
    pure subroutine stats_single(x, w, acc)
        real(real64), intent(in) :: x       !! the value.
        real(real64), intent(in) :: w       !! its weight.
        type(stats_acc), intent(out) :: acc !! the one-element accumulator.
        acc%n_valid = 1_int64
        acc%w_sum = w
        acc%w_sq = w * w
        acc%vsum = w * x
        acc%mean = x
        acc%vmin = x
        acc%vmax = x
        acc%empty = .false.
    end subroutine stats_single

    !> Reduces one batch to an accumulator in ONE traversal, for the streaming lifecycle.
    !!
    !! Elements are folded into a block accumulator, and completed blocks are combined by a CARRY
    !! TREE -- level `k` holds the combination of `2**k` consecutive blocks, and a new block
    !! carries upward exactly as a binary increment does. That gives the same balanced tree
    !! `pair_reduce` builds for the two-pass path, so a batch's result does not depend on where the
    !! caller happened to split it into blocks, while costing `STATS_MAXLEV` accumulators of memory
    !! rather than one per block. The stack is folded highest level first, which is earliest first.
    !!
    !! The accuracy is the combination formulas' own, and is not the two-pass path's. That is the
    !! trade streaming makes: there is no buffer to walk a second time.
    subroutine stats_stream(values, what, is_valid, weights, skip, acc)
        real(real64), intent(in) :: values(:)            !! the batch, before exclusions.
        character(len=*), intent(in) :: what             !! the caller's name, for a message.
        logical, intent(in), optional :: is_valid(:)     !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:) !! per element weight.
        logical, intent(in) :: skip                      !! .true. excludes a NaN.
        type(stats_acc), intent(out) :: acc              !! this batch's contribution.

        type(stats_acc) :: blk, one, stack(0:STATS_MAXLEV), cur
        logical :: busy(0:STATS_MAXLEV)
        real(real64) :: x, w
        integer(int64) :: nv, i, c, nblocks, q
        integer :: lev
        logical :: weighted, masked

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, what, is_valid, weights)
        stats_scan_count = stats_scan_count + 1_int64
        weighted = present(weights)
        masked = present(is_valid)
        busy = .false.
        c = 0_int64
        nblocks = 0_int64

        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight.
            if (masked) then
                if (.not. is_valid(i)) then
                    acc%n_null = acc%n_null + 1_int64
                    cycle
                end if
            end if
            x = values(i)
            if (skip) then
                if (x /= x) then
                    acc%n_nan = acc%n_nan + 1_int64
                    cycle
                end if
            else if (x /= x) then
                acc%saw_nan = .true.
            end if
            w = 1.0_real64
            if (weighted) then
                call stats_check_weight(weights(i), i, what)
                if (weights(i) <= 0.0_real64) cycle
                w = weights(i)
            end if
            call stats_single(x, w, one)
            call stats_chan(blk, one)
            c = c + 1_int64
            if (c == STATS_BLOCK) then
                call stats_push(blk, stack, busy, nblocks)
                c = 0_int64
            end if
        end do
        if (c > 0_int64) call stats_push(blk, stack, busy, nblocks)

        ! Fold the carry stack highest level first: a higher level holds an earlier, larger group,
        ! so this combines the batch in index order.
        !
        ! **This order is a DETERMINISM choice, not an accuracy one, and no test can tell it from
        ! the reverse.** A mutation reversing it (same levels, opposite direction) was applied and
        ! SURVIVED the whole suite: both orders are equally deterministic and equally accurate, and
        ! they differ only in the last bits. It still must not be changed casually -- doing so moves
        ! every streaming answer this module has published -- but do not go looking for the test
        ! that would catch it, and do not read the survival as a coverage gap.
        do lev = STATS_MAXLEV, 0, -1
            if (busy(lev)) then
                cur = stack(lev)
                exit
            end if
        end do
        q = acc%n_null
        do lev = lev - 1, 0, -1
            if (busy(lev)) call stats_chan(cur, stack(lev))
        end do
        if (nblocks > 0_int64) then
            blk = cur
        else
            call stats_clear_acc(blk)
        end if
        blk%n_null = q
        blk%n_nan = acc%n_nan
        blk%saw_nan = blk%saw_nan .or. acc%saw_nan
        if (blk%saw_nan) then
            blk%vsum = stats_nan()
            call stats_undefine(blk)
        end if
        acc = blk
    end subroutine stats_stream

    !> Pushes one completed block onto the carry stack and resets it.
    !!
    !! `nblocks` doubles as the binary counter that decides how far the carry runs, so the tree is
    !! a function of the block index alone.
    pure subroutine stats_push(blk, stack, busy, nblocks)
        type(stats_acc), intent(inout) :: blk                    !! the completed block; reset here.
        type(stats_acc), intent(inout) :: stack(0:STATS_MAXLEV)  !! the carry stack.
        logical, intent(inout) :: busy(0:STATS_MAXLEV)           !! which levels are occupied.
        integer(int64), intent(inout) :: nblocks                 !! blocks pushed so far.
        type(stats_acc) :: cur
        integer :: lev

        cur = blk
        lev = 0
        do while (busy(lev))
            call stats_chan(stack(lev), cur)   ! earlier group first, later second
            cur = stack(lev)
            busy(lev) = .false.
            lev = lev + 1
        end do
        stack(lev) = cur
        busy(lev) = .true.
        nblocks = nblocks + 1_int64
        call stats_clear_acc(blk)
    end subroutine stats_push

    !> Returns one accumulator to its default-initialised state.
    !!
    !! `intent(out)` already default-initialises every component, and the two LOGICALs are
    !! nonetheless assigned explicitly: this repository has one confirmed gfortran case where a
    !! scalar logical relying solely on that implicit reset read back stale, and both of these are
    !! correctness-critical -- `empty` decides whether a merge takes the source's moments wholesale,
    !! and `saw_nan` decides whether the answer is a number at all.
    pure subroutine stats_clear_acc(acc)
        type(stats_acc), intent(out) :: acc !! the accumulator to blank.
        acc%empty = .true.
        acc%saw_nan = .false.
    end subroutine stats_clear_acc

    ! ==================================================================================
    ! pf_stats
    !
    ! The object is a thin shell over the two routines above: `%compute` is the two-pass engine,
    ! a streaming `%update` is `stats_stream`, and a retained `%update` appends and defers. The
    ! counts (`c_valid`, `c_null`, `c_nan`, `n_seen`) are carried on the OBJECT as integers rather
    ! than read back out of the accumulator, so they are exact across any number of updates and
    ! merges and need no recomputation to answer.
    ! ==================================================================================

    !> Returns a `pf_stats` to its default-initialised state.
    subroutine stats_reset(self)
        class(pf_stats), intent(inout) :: self !! the accumulator.
        if (allocated(self%keep)) deallocate(self%keep)
        if (allocated(self%keep_w)) deallocate(self%keep_w)
        call stats_clear_acc(self%acc)
        self%keep_n = 0_int64
        self%n_seen = 0_int64
        self%c_valid = 0_int64
        self%c_null = 0_int64
        self%c_nan = 0_int64
        self%hold = .true.
        self%live = .false.
        self%freq = .false.
        self%wtd = .false.
        self%skip = .true.
        self%stale = .false.
    end subroutine stats_reset

    !> Aborts unless `%compute` or `%init` has run.
    !!
    !! Using an accumulator that holds nothing is misuse, not a data condition, so it aborts where
    !! an empty POPULATION -- which `%init` produces and which is a perfectly ordinary thing for a
    !! per-group loop to meet -- answers NaN.
    subroutine stats_require_live(self, what)
        class(pf_stats), intent(in) :: self  !! the accumulator.
        character(len=*), intent(in) :: what !! the binding's name, for the message.
        if (.not. self%live) error stop what // ": this pf_stats holds no population; " // &
            "call %compute or %init on it first"
    end subroutine stats_require_live

    !> Grows the retained buffers to hold at least `need` elements, preserving what is live.
    !!
    !! Geometric growth by 1.5, matching `parquet_column`'s own rule, so a loop of `%update` costs
    !! amortised O(1) per element rather than O(n) per batch. The weight buffer is BACKFILLED with
    !! 1 when weights appear for the first time part-way through a stream: an unweighted element
    !! and a weight-1 element are the same thing, and without the backfill the retained weights
    !! would silently be shorter than the values beside them.
    subroutine stats_reserve(self, need)
        class(pf_stats), intent(inout) :: self !! the accumulator.
        integer(int64), intent(in) :: need     !! the capacity required.
        real(real64), allocatable :: tmp(:)
        integer(int64) :: cap, want

        cap = 0_int64
        if (allocated(self%keep)) cap = size(self%keep, kind=int64)
        if (cap < need) then
            want = max(need, cap + cap / 2_int64)
            allocate(tmp(want))
            if (self%keep_n > 0_int64) tmp(1:self%keep_n) = self%keep(1:self%keep_n)
            call move_alloc(tmp, self%keep)
        end if
        if (.not. self%wtd) return
        cap = 0_int64
        if (allocated(self%keep_w)) cap = size(self%keep_w, kind=int64)
        if (cap < need) then
            want = max(need, cap + cap / 2_int64)
            allocate(tmp(want))
            if (cap > 0_int64 .and. self%keep_n > 0_int64) then
                tmp(1:self%keep_n) = self%keep_w(1:self%keep_n)
            else if (self%keep_n > 0_int64) then
                tmp(1:self%keep_n) = 1.0_real64
            end if
            call move_alloc(tmp, self%keep_w)
        end if
    end subroutine stats_reserve

    !> Appends one batch's survivors to the retained buffers, in ONE traversal.
    subroutine stats_append(self, values, what, is_valid, weights)
        class(pf_stats), intent(inout) :: self           !! the accumulator.
        real(real64), intent(in) :: values(:)            !! the batch, before exclusions.
        character(len=*), intent(in) :: what             !! the binding's name, for a message.
        logical, intent(in), optional :: is_valid(:)     !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:) !! per element weight.
        real(real64) :: x
        integer(int64) :: nv, i
        logical :: weighted, masked

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, what, is_valid, weights)
        stats_scan_count = stats_scan_count + 1_int64
        weighted = present(weights)
        masked = present(is_valid)
        if (weighted) self%wtd = .true.
        call stats_reserve(self, self%keep_n + nv)

        do i = 1_int64, nv
            ! The family's exclusion order: nullness, then NaN, then weight.
            if (masked) then
                if (.not. is_valid(i)) then
                    self%c_null = self%c_null + 1_int64
                    cycle
                end if
            end if
            x = values(i)
            if (self%skip) then
                if (x /= x) then
                    self%c_nan = self%c_nan + 1_int64
                    cycle
                end if
            end if
            if (weighted) then
                call stats_check_weight(weights(i), i, what)
                if (weights(i) <= 0.0_real64) cycle
            end if
            self%keep_n = self%keep_n + 1_int64
            self%keep(self%keep_n) = x
            if (self%wtd) then
                if (weighted) then
                    self%keep_w(self%keep_n) = weights(i)
                else
                    self%keep_w(self%keep_n) = 1.0_real64
                end if
            end if
        end do
        self%n_seen = self%n_seen + nv
        self%c_valid = self%keep_n
    end subroutine stats_append

    !> Completes a deferred recomputation, if one is outstanding.
    !!
    !! Re-running the two-pass engine over the retained survivors is what makes a retained
    !! `%update`/`%merge` EXACT: pass one's compaction is a no-op over an already-compacted buffer,
    !! so the block boundaries, the tree and every partial sum are the ones `%compute` over the
    !! concatenated input would have produced. The counts are restored afterwards because the
    !! retained buffer no longer contains the excluded elements that produced them.
    subroutine stats_ensure(self)
        class(pf_stats), intent(inout) :: self !! the accumulator.
        if (.not. self%stale) return
        self%stale = .false.
        if (self%wtd) then
            call stats_engine(self%keep(1:self%keep_n), "pf_stats", weights=self%keep_w(1:self%keep_n), &
                skipnan=self%skip, acc=self%acc)
        else
            call stats_engine(self%keep(1:self%keep_n), "pf_stats", skipnan=self%skip, acc=self%acc)
        end if
        self%acc%n_valid = self%c_valid
        self%acc%n_null = self%c_null
        self%acc%n_nan = self%c_nan
    end subroutine stats_ensure

    !> Folds one source accumulator into `self`; the worker both `%merge` forms share.
    subroutine stats_merge_worker(self, other, consume)
        class(pf_stats), intent(inout) :: self  !! the destination.
        type(pf_stats), intent(inout) :: other  !! the source.
        logical, intent(in) :: consume          !! whether to clear the source afterwards.
        integer(int64) :: i

        if (.not. other%live) error stop "pf_stats%merge: the source holds no population; " // &
            "call %compute or %init on it first"
        if (other%hold .neqv. self%hold) error stop "pf_stats%merge: the destination and the " // &
            "source disagree on retain; merging a streaming accumulator into a retained one " // &
            "would leave the retained values describing only part of its own population"
        if (other%freq .neqv. self%freq) error stop "pf_stats%merge: the destination and the " // &
            "source disagree on weight_type"

        if (other%n_seen /= 0_int64) then
            if (self%hold) then
                if (other%wtd) self%wtd = .true.
                call stats_reserve(self, self%keep_n + other%keep_n)
                do i = 1_int64, other%keep_n
                    self%keep(self%keep_n + i) = other%keep(i)
                end do
                if (self%wtd) then
                    if (other%wtd) then
                        do i = 1_int64, other%keep_n
                            self%keep_w(self%keep_n + i) = other%keep_w(i)
                        end do
                    else
                        do i = 1_int64, other%keep_n
                            self%keep_w(self%keep_n + i) = 1.0_real64
                        end do
                    end if
                end if
                self%keep_n = self%keep_n + other%keep_n
                self%stale = .true.
            else
                call stats_chan(self%acc, other%acc)
                if (other%wtd) self%wtd = .true.
            end if
            self%n_seen = self%n_seen + other%n_seen
            self%c_valid = self%c_valid + other%c_valid
            self%c_null = self%c_null + other%c_null
            self%c_nan = self%c_nan + other%c_nan
            self%acc%n_valid = self%c_valid
            self%acc%n_null = self%c_null
            self%acc%n_nan = self%c_nan
        end if
        if (consume) call stats_reset(other)
    end subroutine stats_merge_worker

    ! ---- The lifecycle bindings ----

    module procedure obj_clear
        call stats_reset(self)
    end procedure obj_clear

    module procedure obj_init
        call stats_reset(self)
        if (present(retain)) self%hold = retain
        if (present(skipnan)) self%skip = skipnan
        call stats_weight_kind("pf_stats%init", weight_type, self%freq)
        ! An empty population, not an uncomputed object: every moment is NaN from here, and the
        ! sum is the additive identity, exactly as `%compute` over an empty array would leave it.
        call stats_undefine(self%acc)
        self%acc%vsum = 0.0_real64
        self%live = .true.
    end procedure obj_init

    module procedure obj_compute_f64
        call stats_reset(self)
        if (present(retain)) self%hold = retain
        if (present(skipnan)) self%skip = skipnan
        call stats_weight_kind("pf_stats%compute", weight_type, self%freq)
        self%wtd = present(weights)
        if (self%hold) then
            call stats_engine(values, "pf_stats%compute", is_valid, weights, skipnan, self%acc, &
                self%keep, self%keep_w, threads=threads)
            self%keep_n = self%acc%n_valid
        else
            call stats_engine(values, "pf_stats%compute", is_valid, weights, skipnan, self%acc, &
                threads=threads)
        end if
        self%n_seen = size(values, kind=int64)
        self%c_valid = self%acc%n_valid
        self%c_null = self%acc%n_null
        self%c_nan = self%acc%n_nan
        self%live = .true.
    end procedure obj_compute_f64

    module procedure obj_update_f64
        type(stats_acc) :: batch
        call stats_require_live(self, "pf_stats%update")
        if (self%hold) then
            call stats_append(self, values, "pf_stats%update", is_valid, weights)
            self%stale = .true.
            self%acc%n_valid = self%c_valid
            self%acc%n_null = self%c_null
            self%acc%n_nan = self%c_nan
        else
            call stats_stream(values, "pf_stats%update", is_valid, weights, self%skip, batch)
            call stats_chan(self%acc, batch)
            if (present(weights)) self%wtd = .true.
            self%n_seen = self%n_seen + size(values, kind=int64)
            self%c_valid = self%acc%n_valid
            self%c_null = self%acc%n_null
            self%c_nan = self%acc%n_nan
        end if
    end procedure obj_update_f64

    module procedure obj_merge_one
        logical :: eat
        call stats_require_live(self, "pf_stats%merge")
        eat = .false.
        if (present(consume)) eat = consume
        call stats_merge_worker(self, other, eat)
    end procedure obj_merge_one

    module procedure obj_merge_many
        logical :: eat
        integer :: k
        call stats_require_live(self, "pf_stats%merge")
        eat = .false.
        if (present(consume)) eat = consume
        ! Ascending index, always: the whole reason this form exists is that the answer must not
        ! depend on which thread filled which slot first.
        do k = 1, size(others)
            call stats_merge_worker(self, others(k), eat)
        end do
    end procedure obj_merge_many

    ! ---- The tier-A queries ----

    module procedure obj_is_computed
        res = self%live
    end procedure obj_is_computed

    module procedure obj_retains
        res = self%hold
    end procedure obj_retains

    module procedure obj_n
        call stats_require_live(self, "pf_stats%n")
        res = self%n_seen
    end procedure obj_n

    module procedure obj_n_valid
        call stats_require_live(self, "pf_stats%n_valid")
        res = self%c_valid
    end procedure obj_n_valid

    module procedure obj_n_null
        call stats_require_live(self, "pf_stats%n_null")
        res = self%c_null
    end procedure obj_n_null

    module procedure obj_n_nan
        call stats_require_live(self, "pf_stats%n_nan")
        res = self%c_nan
    end procedure obj_n_nan

    module procedure obj_sum_weights
        call stats_require_live(self, "pf_stats%sum_weights")
        call stats_ensure(self)
        res = self%acc%w_sum
    end procedure obj_sum_weights

    module procedure obj_sum
        call stats_require_live(self, "pf_stats%sum")
        call stats_ensure(self)
        res = self%acc%vsum
    end procedure obj_sum

    module procedure obj_mean
        call stats_require_live(self, "pf_stats%mean")
        call stats_ensure(self)
        res = self%acc%mean
    end procedure obj_mean

    module procedure obj_variance
        integer :: dd
        call stats_require_live(self, "pf_stats%variance")
        call stats_ensure(self)
        dd = 1
        if (present(ddof)) dd = ddof
        res = stats_var(self%acc, dd, self%freq)
    end procedure obj_variance

    module procedure obj_stddev
        integer :: dd
        call stats_require_live(self, "pf_stats%stddev")
        call stats_ensure(self)
        dd = 1
        if (present(ddof)) dd = ddof
        res = stats_sqrt(stats_var(self%acc, dd, self%freq))
    end procedure obj_stddev

    module procedure obj_sem
        integer :: dd
        call stats_require_live(self, "pf_stats%sem")
        call stats_ensure(self)
        dd = 1
        if (present(ddof)) dd = ddof
        res = stats_sem(self%acc, dd, self%freq)
    end procedure obj_sem

    module procedure obj_skewness
        logical :: bi
        call stats_require_live(self, "pf_stats%skewness")
        call stats_ensure(self)
        bi = .false.
        if (present(bias)) bi = bias
        res = stats_skew(self%acc, bi, self%freq)
    end procedure obj_skewness

    module procedure obj_kurtosis
        logical :: bi, ex
        call stats_require_live(self, "pf_stats%kurtosis")
        call stats_ensure(self)
        bi = .false.
        ex = .true.
        if (present(bias)) bi = bias
        if (present(excess)) ex = excess
        res = stats_kurt(self%acc, bi, ex, self%freq)
    end procedure obj_kurtosis

    module procedure obj_vmin
        call stats_require_live(self, "pf_stats%vmin")
        call stats_ensure(self)
        res = self%acc%vmin
    end procedure obj_vmin

    module procedure obj_vmax
        call stats_require_live(self, "pf_stats%vmax")
        call stats_ensure(self)
        res = self%acc%vmax
    end procedure obj_vmax

    module procedure obj_range
        call stats_require_live(self, "pf_stats%range")
        call stats_ensure(self)
        res = self%acc%vmax - self%acc%vmin
    end procedure obj_range

    ! ---- The traversal counter ----

    module procedure parquet_debug_stats_scans
        res = stats_scan_count
    end procedure parquet_debug_stats_scans

    module procedure parquet_debug_reset_stats_scans
        stats_scan_count = 0_int64
    end procedure parquet_debug_reset_stats_scans

    module procedure parquet_debug_stats_team
        res = stats_team_used
    end procedure parquet_debug_stats_team

    module procedure parquet_debug_set_stats_min_per_thread
        dbg_stats_min_per_thread = n
    end procedure parquet_debug_set_stats_min_per_thread

end submodule parquet_stats_core ! GCOVR_EXCL_LINE
