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
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, &
        ieee_negative_inf
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
    !! **Measured, on the machines and with the harness named below, not chosen** -- and measured
    !! on `pf_variance` itself rather than on a replica, which is the part that had to change.
    !!
    !! **How to re-derive it, and why the obvious mode is the wrong one.** Run
    !! `bench/benchmark_stats.sh --mode=teamsweep`. Neither of the two older modes can answer this
    !! question. `--mode=thread` times a replica of pass two over a buffer allocated ONCE outside
    !! the timed loop, so it reports a ceiling and never pays what a real call pays; `--mode=library`
    !! runs the shipped rule, so this very constant censors its own measurement -- every size below
    !! `2 * STATS_MIN_PER_THREAD` reports exactly 1.00x because the engine ran serially, which is
    !! the floor working and not a datum about where it belongs. `--mode=teamsweep` lifts the floor
    !! through `parquet_debug_set_stats_min_per_thread(0)` and drives the team directly, so what it
    !! reports is the real crossover on the code that ships.
    !!
    !! **Machine B (2 x EPYC 9654, 384 logical, 8 cores per L3), gfortran 15.2.1, 64-core mask,
    !! best speedup over a whole `pf_variance` call against `threads=1`:**
    !!
    !!     n        =    10000   100000     1e6     1e7
    !!     best     =    1.12x    1.45x   1.55x   1.56x
    !!     at team  =       8t      32t     32t     32t
    !!     per thd  =     1250     3125   31250  312500
    !!
    !! and the losing cells are only the ones with almost no work per thread: 312 per thread is
    !! 0.90x and 156 per thread is worse. **Break-even is about 500-600 survivors per thread**, on
    !! both compilers. The value below carries roughly an order of magnitude of margin over that,
    !! deliberately: the crossover is shallow on one side and steep on the other, so paying a
    !! little of the available gain to stay well clear of the cliff is the right trade.
    !!
    !! **Why it is no longer 32768.** That value came from machine A, from `--mode=thread`, and
    !! from an engine in which pass one always materialised a compacted copy of the population.
    !! Handing a team a buffer one thread had just written is what made a team expensive: on
    !! machine B the same four-thread team measured **1.13x inside one L3 and 0.25x spread across
    !! eight**, and the shipped call LOST up to 6.5x at every size the old floor admitted. That
    !! copy is now deferred (see `stats_engine`), which removed the cliff the old value was
    !! avoiding -- so the floor that was calibrated against it is both too high for the engine that
    !! exists now and, as the campaign that found this showed, was never protecting what it looked
    !! like it was protecting.
    !!
    !! **Machine A and machine C have NOT been re-measured since that change**, and their old
    !! numbers do not carry over, for exactly the reason above. The value below is a machine-B
    !! measurement with margin, chosen to sit at or below machine A's own last-known break-even
    !! (~8192 per thread at 8 threads, from the superseded ladder) so that lowering it cannot cost
    !! machine A anything it was previously getting. Re-run `--mode=teamsweep` on A and C and
    !! tighten it if they agree.
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
    integer(int64), parameter :: STATS_MIN_PER_THREAD = 8192_int64

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
    module procedure stats_nan
        res = ieee_value(1.0_real64, ieee_quiet_nan)
    end procedure stats_nan

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
    module procedure stats_weight_kind
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
    end procedure stats_weight_kind

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
    subroutine stats_engine(values, what, is_valid, weights, skipnan, acc, keep_x, keep_w, threads, &
            nmom)
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
        integer, intent(in), optional :: nmom
        !! **How many central moments pass two must produce. Absent means all four.**
        !!
        !! Pass two is a second traversal of the whole population and roughly half the cost of a
        !! call, so a caller that needs none of it should not pay for it, and one that needs the
        !! mean should not pay for the fourth moment. The levels are cumulative:
        !!
        !! * **0** stops after pass one, leaving the counts, `vsum` and the compacted survivors,
        !!   and marking every central moment -- and the extremes, `w_sum` and `w_sq` -- undefined.
        !!   That is what `pf_sum` and an ORDER statistic want.
        !! * **1** adds the refined mean, and nothing else. `pf_mean`.
        !! * **2** adds `m2`, so the variance family. `pf_variance`, `pf_stddev`, `pf_sem`.
        !! * **3** adds `m3`. `pf_skewness`.
        !! * **4** adds `m4`, the full set. `pf_kurtosis`, `pf_moments`, `pf_stats`.
        !!
        !! **The answer at any level is bit-for-bit what level 4 would have produced for the same
        !! output.** A level below 4 changes only whether pass two runs at all (level 0) and which
        !! of its four block arrays are reduced and read afterwards -- never how any surviving
        !! accumulator is summed. `stats_block_moments` therefore keeps ONE loop body for every
        !! level, and its header says what happened when that was specialised instead.
        !! `test_moment_levels_are_bit_exact` asserts the property against the level-4 answer for
        !! every entry point, over four populations that reach different pass-one branches.
        integer, intent(in), optional :: threads
        !! how many threads pass two may use. Absent takes the automatic rule; see
        !! `stats_pass_two_team`. **The answer does not depend on this argument** -- the block
        !! decomposition is a function of the population size alone, so every thread count,
        !! including 1 and including a serial build, returns the identical bits.

        real(real64), allocatable :: xb(:), wb(:), pw(:), px(:), q1(:), q2(:), q3(:), q4(:)
        real(real64) :: x, w, sw, sx, mu, delta, vmin_l, vmax_l
        integer(int64) :: nv, m, nb, i, c, team, istart, n_null_l, n_nan_l, wbits
        logical :: saw_nan_l
        logical :: skip, weighted, masked, deferrable, compacting, nonfinite_mean
        integer :: nm

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, what, is_valid, weights)
        stats_scan_count = stats_scan_count + 1_int64   ! pass one
        skip = .true.
        if (present(skipnan)) skip = skipnan
        nm = 4
        if (present(nmom)) nm = nmom
        weighted = present(weights)
        masked = present(is_valid)

        ! **The compaction buffer is not allocated on the fast path until something needs it.**
        ! When nothing is masked, weighted or NaN, pass one's "compaction" copies `values` to
        ! itself: `xb(i) == values(i)` for every element, and pass two can read the caller's array
        ! and return the identical bits. Materialising that copy anyway cost an allocation, a write
        ! and a free of a whole population per call, and -- because the copy was written by ONE
        ! thread immediately before a team read it -- it was also what made threading pass two lose
        ! on a machine whose cores do not share a cache. See `stats_pass_two` for the measurement.
        !
        ! `is_contiguous` is part of the test because the no-copy path hands pass two whatever
        ! stride the caller's array has, where the copy always produced a contiguous buffer. A
        ! discontiguous actual argument is rare and the compaction is worth keeping for it.
        deferrable = (.not. masked) .and. (.not. weighted) .and. skip .and. is_contiguous(values)
        compacting = .not. deferrable
        if (compacting) allocate(xb(nv))
        if (weighted) allocate(wb(nv))
        allocate(px(nv / STATS_BLOCK + 1_int64))
        if (weighted) allocate(pw(nv / STATS_BLOCK + 1_int64))

        ! ---- Pass one: exclude, compact, and close each block's sums ----
        !
        ! **The counters, the extremes and the NaN flag are LOCALS here and are written back to
        ! `acc` once, after the loop.** They are logically `acc`'s, but as components of a dummy
        ! argument every read and write of them is memory the compiler must assume can alias
        ! `values`, `weights` or `is_valid` -- so `if (.not. acc%saw_nan)` became a load on every
        ! element and `acc%vmin` a load-compare-store. As locals they live in registers for the
        ! whole traversal. Nothing about the arithmetic or its order changes, which is what makes
        ! this safe to do to a routine whose whole contract is bit-reproducibility.
        n_null_l = acc%n_null
        n_nan_l = acc%n_nan
        saw_nan_l = acc%saw_nan
        vmin_l = acc%vmin
        vmax_l = acc%vmax
        m = 0_int64
        nb = 0_int64
        c = 0_int64
        sw = 0.0_real64
        sx = 0.0_real64
        if (.not. masked .and. .not. weighted .and. skip) then
            ! **Two loops, not one loop with a flag in it.** The deferred scan below differs from
            ! the compacting loop after it by a single store, so the obvious shape is one loop with
            ! `if (compacting) xb(m) = x` in the body. That was written first and measured 5%
            ! slower at every size that fits in cache: the flag is loop-invariant but neither
            ! compiler hoisted it out of a body that also carries a `cycle`. Splitting costs a
            ! duplicated block-close and buys that 5% back.
            !
            ! `i` survives the loop it left: after a normal completion it is `nv + 1`, and after
            ! the `exit` it is the index of the first excluded element, which is exactly where the
            ! compacting loop has to resume. Nothing before that point has been written to `xb`,
            ! and nothing after it can be skipped.
            istart = nv + 1_int64
            if (deferrable) then
                do i = 1_int64, nv
                    x = values(i)
                    if (x /= x) exit
                    m = m + 1_int64
                    ! **An infinity is counted and ranged, but never added.** See the fold-back
                    ! below `pair_reduce` for why, and for where it comes back.
                    if (abs(x) <= huge(0.0_real64)) sx = sx + x
                    if (m == 1_int64) then
                        vmin_l = x
                        vmax_l = x
                    else
                        if (x < vmin_l) vmin_l = x
                        if (x > vmax_l) vmax_l = x
                    end if
                    c = c + 1_int64
                    if (c == STATS_BLOCK) then
                        nb = nb + 1_int64
                        px(nb) = sx
                        sx = 0.0_real64
                        c = 0_int64
                    end if
                end do
                istart = i
                if (istart <= nv) then
                    ! The first exclusion is where a deferred compaction becomes a real one: from
                    ! here on the survivors no longer sit at their own indices in `values`, so the
                    ! prefix that has matched so far is copied across in one move and the loop
                    ! below starts writing. `m` is the survivor count, which before any exclusion
                    ! is exactly `istart - 1`, so `values(1:m)` is precisely that prefix.
                    allocate(xb(nv))
                    if (m > 0_int64) xb(1:m) = values(1:m)
                    compacting = .true.
                end if
            else
                istart = 1_int64
            end if
            do i = istart, nv
                x = values(i)
                if (x /= x) then
                    n_nan_l = n_nan_l + 1_int64
                    cycle
                end if
                m = m + 1_int64
                xb(m) = x
                if (abs(x) <= huge(0.0_real64)) sx = sx + x   ! infinity kept out; see the fold-back
                if (m == 1_int64) then
                    vmin_l = x
                    vmax_l = x
                else
                    if (x < vmin_l) vmin_l = x
                    if (x > vmax_l) vmax_l = x
                end if
                c = c + 1_int64
                if (c == STATS_BLOCK) then
                    nb = nb + 1_int64
                    px(nb) = sx
                    sx = 0.0_real64
                    c = 0_int64
                end if
            end do
        else if (.not. weighted) then
            ! **The unweighted arm of the general branch, split out for the same reason the two
            ! fast loops above are split.** A caller with `is_valid=` or `skipnan=.false.` but no
            ! weights is a common shape and pays nothing here for the weighted one: no `weights(i)`
            ! load, no screen, no `wb(m)` store, no `sw`, and `sx + x` rather than `sx + w*x`.
            !
            ! Bit-exact against the merged loop it replaces: `w` is exactly `1.0` on this path, and
            ! multiplying a `real64` by one is exact for every value including the infinities and
            ! NaNs, so `sx + 1.0*x` and `sx + x` are the same number -- contracted into an FMA or
            ! not, since a single-rounding `fma(1.0, x, sx)` is also `sx + x` rounded once. The
            ! element order and the block boundaries are untouched.
            do i = 1_int64, nv
                if (masked) then
                    if (.not. is_valid(i)) then
                        n_null_l = n_null_l + 1_int64
                        cycle
                    end if
                end if
                x = values(i)
                if (skip) then
                    if (x /= x) then
                        n_nan_l = n_nan_l + 1_int64
                        cycle
                    end if
                else if (x /= x) then
                    saw_nan_l = .true.
                end if
                m = m + 1_int64
                xb(m) = x
                if (.not. saw_nan_l) then
                    if (m == 1_int64) then
                        vmin_l = x
                        vmax_l = x
                    else
                        if (x < vmin_l) vmin_l = x
                        if (x > vmax_l) vmax_l = x
                    end if
                    ! Inside the `saw_nan_l` guard so that `abs(x)` is never applied to a NaN: a
                    ! `<=` against one raises IEEE_INVALID exactly as the infinity arithmetic
                    ! would. A population that has already seen a kept NaN answers NaN whatever
                    ! this sum holds, so leaving it out of the accumulation costs nothing.
                    if (abs(x) <= huge(0.0_real64)) sx = sx + x
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
                        n_null_l = n_null_l + 1_int64
                        cycle
                    end if
                end if
                x = values(i)
                if (skip) then
                    if (x /= x) then
                        n_nan_l = n_nan_l + 1_int64
                        cycle
                    end if
                else if (x /= x) then
                    saw_nan_l = .true.
                end if
                w = 1.0_real64
                if (weighted) then
                    w = weights(i)
                    wbits = transfer(w, 0_int64)
                    if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then
                        call stats_check_weight(w, i, what)
                        if (w <= 0.0_real64) cycle
                    else if (wbits == 0_int64) then
                        cycle
                    end if
                end if
                m = m + 1_int64
                xb(m) = x
                if (weighted) wb(m) = w
                if (.not. saw_nan_l) then
                    if (m == 1_int64) then
                        vmin_l = x
                        vmax_l = x
                    else
                        if (x < vmin_l) vmin_l = x
                        if (x > vmax_l) vmax_l = x
                    end if
                    ! `w` is finite and strictly positive here (`stats_check_weight`, and a zero
                    ! weight has already been cycled), so `w * x` is infinite exactly when `x` is
                    ! and the same screen serves. Guarded against a NaN `x` as the arm above is.
                    if (abs(x) <= huge(0.0_real64)) sx = sx + w * x
                end if
                sw = sw + w
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
        acc%n_null = n_null_l
        acc%n_nan = n_nan_l
        acc%saw_nan = saw_nan_l
        acc%vmin = vmin_l
        acc%vmax = vmax_l

        ! A caller that asked to keep the survivors gets the buffer whether or not pass one needed
        ! one for itself. Deferring the copy is an internal saving and must not change what
        ! `keep_x` receives, so it is paid here instead -- once, as a contiguous move, rather than
        ! element by element inside the loop above.
        if (present(keep_x)) then
            if (.not. allocated(xb)) then
                allocate(xb(nv))
                if (m > 0_int64) xb(1:m) = values(1:m)
            end if
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
        ! ---- The infinities, folded back in ----
        !
        ! **Pass one keeps every infinity out of the block sums, and this is where it returns.**
        ! The reason is that `Inf + (-Inf)` raises IEEE_INVALID, and nagfor unmasks the IEEE traps
        ! by default (`-ieee=stop`), so executing it kills the process -- on data this module
        ! documents itself as ANSWERING rather than refusing (the non-finite table in
        ! doc/pages/utilities/statistics.md). The trap cannot be masked away instead: NAG's
        ! `ieee_set_halting_mode` does not lift it under `-ieee=stop`, measured against 7.2, so
        ! the offending operation has to not happen.
        !
        ! **The signs come from the extremes pass one already tracked, so the screen costs one
        ! comparison per element and nothing per population.** `vmax` is `+Inf` exactly when the
        ! population held one, and `vmin` likewise -- both are maintained on the unscreened value,
        ! and a zero-weighted element never reaches them because it is cycled before `m` moves.
        ! The answers below are exactly what the addition would have produced: both signs cancel
        ! to a NaN, and one sign swallows whatever finite partial sum sits beside it.
        !
        ! Bit-exact on a finite population, which is every population any accuracy test uses: the
        ! screen excludes nothing there, the block tree is unchanged -- an excluded element still
        ! advances `c` -- and this branch is not taken.
        if (acc%vmax > huge(0.0_real64) .and. acc%vmin < -huge(0.0_real64)) then
            acc%vsum = stats_nan()
        else if (acc%vmax > huge(0.0_real64)) then
            acc%vsum = acc%vmax
        else if (acc%vmin < -huge(0.0_real64)) then
            acc%vsum = acc%vmin
        end if
        if (weighted) then
            call pair_reduce(pw, nb)
            acc%w_sum = pw(1)
            acc%w_sq = sum_of_squares(wb, m, nb)
        else
            acc%w_sum = real(m, real64)
            acc%w_sq = real(m, real64)
        end if
        if (nm <= 0) then
            ! Pass one answered everything this caller asked for. `stats_undefine` marks the
            ! central moments NaN rather than leaving them zero, so a caller that reads one anyway
            ! gets the module's own "undefined" answer instead of a plausible wrong number.
            call stats_undefine(acc)
            call stats_hand_over(xb, wb, keep_x, keep_w)
            return
        end if
        mu = acc%vsum / acc%w_sum

        ! **A population holding an infinity never enters pass two at all.** `mu` is then an
        ! infinity (or, with both signs present, a NaN), so `x - mu` is `Inf - Inf` for the
        ! infinite element itself -- an IEEE_INVALID, and so a dead process under nagfor's default
        ! `-ieee=stop`, on data this module documents itself as answering rather than refusing.
        ! Every central moment is a NaN there -- `(x - Inf)**2` is `Inf` for a finite element and
        ! `Inf - Inf` for the infinite one -- which is numpy's answer and this file's own (the
        ! non-finite table in doc/pages/utilities/statistics.md), so the pass could not have
        ! produced anything else and skipping it is a saving rather than a compromise.
        !
        ! The same extremes pass one tracked answer it, so the screen is two comparisons for the
        ! whole population and pass two is not entered at all.
        !
        ! An `mu` made infinite by OVERFLOW rather than by an infinite element is deliberately NOT
        ! caught: nothing in the population is then infinite, so every `x - mu` is finite minus
        ! infinite and raises nothing, and the `nonfinite_mean` branch below still gives that case
        ! its own answer. `stats_scan_count` is not bumped either, because no second pass ran.
        if (acc%vmax > huge(0.0_real64) .or. acc%vmin < -huge(0.0_real64)) then
            acc%mean = mu
            acc%m2 = stats_nan()
            acc%m3 = stats_nan()
            acc%m4 = stats_nan()
            call stats_hand_over(xb, wb, keep_x, keep_w)
            return
        end if
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
        !
        ! **The four block arrays are all allocated whatever `nm` is, and that is deliberate.**
        ! `nb` is the population over `STATS_BLOCK`, so the four together are `m/4` bytes -- 2.5 MB
        ! against 80 MB of population at ten million elements -- while making them conditional
        ! would put an unallocated actual argument against `stats_block_moments`'s scalar dummies,
        ! which cannot be optional the way `wb` is. What `nm` saves is the per-element arithmetic
        ! and the `pair_reduce` walks, which is where the cost actually is.
        allocate(q1(nb), q2(nb), q3(nb), q4(nb))
        team = stats_pass_two_team(threads, m)
        ! `xb` is unallocated exactly when pass one found nothing to exclude and no caller asked to
        ! keep the survivors -- in which case the survivors ARE `values(1:m)`, element for element,
        ! and reading them there returns the identical bits at a whole population less traffic.
        if (allocated(xb)) then
            call stats_pass_two(xb, wb, mu, m, nb, team, q1, q2, q3, q4)
        else
            call stats_pass_two(values, wb, mu, m, nb, team, q1, q2, q3, q4)
        end if
        call pair_reduce(q1, nb)
        if (nm >= 2) call pair_reduce(q2, nb)
        if (nm >= 3) call pair_reduce(q3, nb)
        if (nm >= 4) call pair_reduce(q4, nb)

        ! Re-centre on `mu + delta`. Expanding `sum(w*(d-delta)**k)` and using `sum(w*d) = delta*W`
        ! collapses every cross term, leaving these four lines. `delta` is a rounding error, so the
        ! corrections are tiny and none of them can cancel anything significant away.
        ! **A non-finite mean is never refined, and that is a correctness requirement.** `mu` is
        ! an infinity when the population holds one (or overflows), and a NaN when it holds both
        ! signs of one. Every term of `q1 = sum(w*(x - mu))` is then `Inf - Inf`, so `delta` is a
        ! NaN and `mu + delta` would replace an answer that IS correct with one that is not:
        ! numpy and pandas both answer `inf` for the mean of a population containing `+Inf`, and
        ! so does this module's own `pf_sum` over the same data, which does not go through the
        ! refinement. Refining would leave `sum` and `mean` disagreeing on one population.
        !
        ! The central moments are handed on unrefined for the same reason and are NaN in their own
        ! right -- `(x - Inf)**2` is `Inf` for a finite element and `Inf - Inf` for the infinite
        ! one -- which is numpy's answer for the variance here, so nothing needs forcing.
        nonfinite_mean = .false.
        if (mu /= mu) then
            nonfinite_mean = .true.
        else if (abs(mu) > huge(0.0_real64)) then
            nonfinite_mean = .true.
        end if
        !
        ! A moment above `nm` was never accumulated, so it is marked undefined rather than left
        ! holding whatever its block array happened to contain -- the same contract `stats_undefine`
        ! gives the `nm == 0` path, for the same reason: a caller that reads one anyway gets this
        ! module's own "undefined" answer instead of a plausible wrong number.
        if (nonfinite_mean) then
            acc%mean = mu
            acc%m2 = merge(q2(1), stats_nan(), nm >= 2)
            acc%m3 = merge(q3(1), stats_nan(), nm >= 3)
            acc%m4 = merge(q4(1), stats_nan(), nm >= 4)
            call stats_hand_over(xb, wb, keep_x, keep_w)
            return
        end if
        delta = q1(1) / acc%w_sum
        acc%mean = mu + delta
        acc%m2 = stats_nan()
        acc%m3 = stats_nan()
        acc%m4 = stats_nan()
        if (nm >= 2) acc%m2 = q2(1) - delta * delta * acc%w_sum
        if (nm >= 3) acc%m3 = q3(1) - 3.0_real64 * delta * q2(1) + 2.0_real64 * delta**3 * acc%w_sum
        if (nm >= 4) acc%m4 = q4(1) - 4.0_real64 * delta * q3(1) + 6.0_real64 * delta * delta * q2(1) &
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
        ! **One loop, whatever `nmom` is, and that is a correctness requirement rather than an
        ! oversight.** Specialising the body per level -- one `addpd` chain for level 1 where level
        ! 4 has four -- was written first and is NOT bit-exact: with fewer live accumulators the
        ! compiler unrolls and vectorises differently, which regroups the partial sums inside the
        ! block and moves the last bits of `q1`. `pf_mean` and `pf_stats%mean` then stopped
        ! matching `pf_moments`, which the suite catches in three places.
        !
        ! Nothing is lost by it, because the arithmetic was never the cost. Measured on machine B
        ! at ten million elements, the specialised arms ran 1.930 (level 1), 1.943 (level 2) and
        ! 1.963 ns/elem (level 4) -- a 1.7% spread across dropping three of four accumulators,
        ! because pass two streams the whole population and is bound by that, not by its flops.
        ! What `nmom` is worth is the level-0 short circuit in `stats_engine`, which skips this
        ! traversal outright and is worth 2.2x on `pf_sum`.
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

    !> Pass two over one population: the four central-moment block sums, serial or on a team.
    !!
    !! Extracted so that the two arrays pass two can legitimately be given -- pass one's compacted
    !! buffer, or the caller's own `values` when nothing needed compacting -- reach **one** copy of
    !! the region rather than two that could drift apart. The array is a dummy here precisely so
    !! that which one it is cannot be observed in the answer.
    !!
    !! **Threaded over blocks, and that cannot move a bit.** Each block writes only its own
    !! `q1(j)..q4(j)` from values only it reads, and the four `pair_reduce` calls the caller makes
    !! afterwards walk a tree fixed by `nb` alone -- so who computed which block is not observable
    !! in the result. `bench/benchmark_stats.sh --mode=thread` asserts exactly that before
    !! reporting any timing, and `test_threading_changes_no_bit` asserts it in the suite.
    !!
    !! **What a team is worth here is set by where the data already is, not by the block count.**
    !! Pass one is serial, so on entry every survivor is hot in whichever cache the calling thread
    !! owns. A team drawn from cores that share that cache reads them there; a team spread wider
    !! has to drag the whole population across the machine before it can start, and it pays that on
    !! every call. Measured on a 2-socket EPYC 9654 (8 cores per L3) at 1000000 elements, the same
    !! four-thread team returned **1.13x inside one L3 and 0.25x spread across eight** -- a 4.5x
    !! swing with nothing changed but which cores the team was allowed to use. That is the
    !! measurement behind `STATS_MIN_PER_THREAD`'s floor and behind the caution in it; run
    !! `bench/benchmark_stats.sh --mode=teamsweep` to re-derive both on a new machine.
    subroutine stats_pass_two(xv, wb, mu, m, nb, team, q1, q2, q3, q4)
        real(real64), intent(in) :: xv(:)
        !! the survivors; `xv(1:m)` are live. **Assumed-shape and deliberately NOT `contiguous`.**
        !! Marking it contiguous looks free -- both call sites do pass a contiguous actual -- and
        !! measured 2.7x WORSE at 10000000 elements: `values` reaches `stats_engine` as an
        !! assumed-shape dummy whose contiguity no compiler can prove at that point, so a
        !! `contiguous` dummy here makes it copy the whole population into a temporary on every
        !! call, which is the copy this deferral exists to remove.
        real(real64), intent(in), optional :: wb(:)  !! their weights, absent when unweighted.
        real(real64), intent(in) :: mu               !! the mean pass one produced.
        integer(int64), intent(in) :: m              !! how many survivors there are.
        integer(int64), intent(in) :: nb             !! blocks pass one closed.
        integer(int64), intent(in) :: team           !! threads to open; 1 runs serially.
        real(real64), intent(out) :: q1(:)           !! per block, `sum(w*d)`.
        real(real64), intent(out) :: q2(:)           !! per block, `sum(w*d**2)`.
        real(real64), intent(out) :: q3(:)           !! per block, `sum(w*d**3)`.
        real(real64), intent(out) :: q4(:)           !! per block, `sum(w*d**4)`.
        integer(int64) :: j
        logical :: threaded

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
                call stats_block_moments(xv, wb, mu, j, m, q1(j), q2(j), q3(j), q4(j))
            end do
            !$omp end do
            !$omp end parallel
        end if
#endif
        if (.not. threaded) then
            do j = 1_int64, nb
                call stats_block_moments(xv, wb, mu, j, m, q1(j), q2(j), q3(j), q4(j))
            end do
        end if
    end subroutine stats_pass_two

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
        call stats_engine(values, "pf_sum", is_valid, weights, skipnan, acc, threads=threads, nmom=0)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        s = acc%vsum
        if (present(ok)) ok = (s == s)
    end procedure sum_f64

    module procedure mean_f64
        type(stats_acc) :: acc
        call stats_engine(values, "pf_mean", is_valid, weights, skipnan, acc, threads=threads, nmom=1)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        m = acc%mean
        if (present(ok)) ok = (m == m)
    end procedure mean_f64

    !> The shared body of `pf_gmean` and `pf_hmean`: one compaction, then one accumulation.
    !!
    !! **Every value that would reach a transcendental or a reciprocal is screened first**, and
    !! that ordering is a correctness requirement rather than tidiness. `log(0)` raises
    !! `IEEE_DIVIDE_BY_ZERO` and `log` of a negative raises `IEEE_INVALID`; nagfor unmasks the IEEE
    !! traps by default (`-ieee=stop`), so reaching either would terminate the process on that
    !! compiler while returning a plausible `0` or NaN on every other one. The same screen also
    !! keeps a propagating NaN away from `log` under `skipnan = .false.`, which matters on ifx --
    !! it pairs adjacent transcendental calls into one SVML call, and the vectorised routine
    !! raises `IEEE_INVALID` on a NaN element where the scalar one is quiet.
    subroutine power_mean(values, what, harmonic, is_valid, weights, skipnan, res, &
            n_null, n_nan, ok)
        real(real64), intent(in) :: values(:)             !! the population, before exclusions.
        character(len=*), intent(in) :: what              !! the public procedure's name.
        logical, intent(in) :: harmonic                   !! .true. for hmean, .false. for gmean.
        logical, intent(in), optional :: is_valid(:)      !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)  !! per element weight.
        logical, intent(in), optional :: skipnan          !! .true. (default) excludes a NaN.
        real(real64), intent(out) :: res                  !! the mean.
        integer(int64), intent(out), optional :: n_null   !! how many were null.
        integer(int64), intent(out), optional :: n_nan    !! how many were NaN.
        logical, intent(out), optional :: ok              !! .false. when the answer is NaN.
        real(real64), allocatable :: keep(:), keep_w(:)
        integer(int64) :: m, nnull, nnan
        logical :: poisoned, good

        call stats_compact(values, what, is_valid, weights, skipnan, keep, keep_w, m, nnull, &
            nnan, poisoned)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (allocated(keep_w)) then
            call power_mean_kept(keep, m, harmonic, poisoned, res, good, w=keep_w)
        else
            call power_mean_kept(keep, m, harmonic, poisoned, res, good)
        end if
        if (present(ok)) ok = good
    end subroutine power_mean

    !> The domain screen and the accumulation, over an ALREADY-COMPACTED population.
    !!
    !! Split out of `power_mean` so that `pf_stats%gmean`/`%hmean` can answer off the object's
    !! retained buffer without compacting a second time -- and, more to the point, so that the two
    !! routes cannot drift on the domain rules, which are the part of a power mean that is easy to
    !! get quietly wrong (a zero is the limit and not an error; a negative is undefined; a
    !! population holding both is undefined, not zero).
    subroutine power_mean_kept(keep, m, harmonic, poisoned, res, ok, w)
        real(real64), intent(in) :: keep(:)         !! the survivors; the first `m` are live.
        integer(int64), intent(in) :: m             !! how many survivors there are.
        logical, intent(in) :: harmonic             !! .true. for hmean, .false. for gmean.
        logical, intent(in) :: poisoned             !! .true. when a kept NaN makes every answer NaN.
        real(real64), intent(out) :: res            !! the mean, or NaN.
        logical, intent(out) :: ok                  !! .false. when the answer is a NaN.
        real(real64), intent(in), optional :: w(:)  !! the survivors' weights, when weighted.
        real(real64) :: acc_sum, w_sum, wi, x
        integer(int64) :: i
        logical :: saw_zero

        res = stats_nan()
        ok = .false.
        if (m == 0_int64 .or. poisoned) return

        ! Pass one: the domain screen, in full, before any arithmetic. A negative anywhere makes
        ! the answer undefined; a zero makes it exactly zero. Both are DATA conditions and neither
        ! aborts. The two are checked in this order because a population holding both a negative
        ! and a zero has no defined mean, and scipy answers NaN there too.
        saw_zero = .false.
        do i = 1_int64, m
            if (keep(i) < 0.0_real64) return
            if (keep(i) == 0.0_real64) saw_zero = .true.
        end do
        if (saw_zero) then
            ! Exactly 0, which is the limit of both means and what scipy returns. Reported as a
            ! defined answer -- `ok` is .true. -- because it is one.
            res = 0.0_real64
            ok = .true.
            return
        end if

        acc_sum = 0.0_real64
        w_sum = 0.0_real64
        do i = 1_int64, m
            wi = 1.0_real64
            if (present(w)) wi = w(i)
            x = keep(i)
            if (harmonic) then
                acc_sum = acc_sum + wi / x
            else
                acc_sum = acc_sum + wi * log(x)
            end if
            w_sum = w_sum + wi
        end do
        if (w_sum <= 0.0_real64) return
        if (harmonic) then
            if (acc_sum <= 0.0_real64) return
            res = w_sum / acc_sum
        else
            res = exp(acc_sum / w_sum)
        end if
        ok = (res == res)
    end subroutine power_mean_kept

    module procedure gmean_f64
        call power_mean(values, "pf_gmean", .false., is_valid, weights, skipnan, g, &
            n_null, n_nan, ok)
    end procedure gmean_f64

    module procedure hmean_f64
        call power_mean(values, "pf_hmean", .true., is_valid, weights, skipnan, h, &
            n_null, n_nan, ok)
    end procedure hmean_f64

    module procedure variance_f64
        type(stats_acc) :: acc
        logical :: freq
        integer :: dd
        call stats_weight_kind("pf_variance", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        call stats_engine(values, "pf_variance", is_valid, weights, skipnan, acc, threads=threads, nmom=2)
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
        call stats_engine(values, "pf_stddev", is_valid, weights, skipnan, acc, threads=threads, nmom=2)
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
        call stats_engine(values, "pf_sem", is_valid, weights, skipnan, acc, threads=threads, nmom=2)
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
        call stats_engine(values, "pf_skewness", is_valid, weights, skipnan, acc, threads=threads, nmom=3)
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
        integer :: dd, nm

        call stats_weight_kind("pf_moments", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        bs = .false.
        if (present(bias)) bs = bias
        ex = .true.
        if (present(excess)) ex = excess

        ! **Only the moments the caller asked for are computed.** `pf_moments` is the one entry
        ! point whose cost is genuinely a function of its argument list rather than of its name, so
        ! it derives its level instead of naming one: asking for the mean and the variance costs
        ! two accumulators in pass two, not four. The floor is 1 rather than 0 because level 0 is
        ! the "pass one only" path, which marks the EXTREMES undefined along with the moments --
        ! and `vmin=`/`vmax=` are pass-one products a caller may legitimately ask for alone.
        nm = 1
        if (present(variance) .or. present(stddev) .or. present(sem)) nm = 2
        if (present(skewness)) nm = max(nm, 3)
        if (present(kurtosis)) nm = 4
        call stats_engine(values, "pf_moments", is_valid, weights, skipnan, acc, threads=threads, &
            nmom=nm)

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
        ! **Only the outputs the caller asked for are tested**, which is the one reading of `ok`
        ! that works on a procedure with nine of them: `vsum` over an empty population is a
        ! correct `0` and `vmin` is a NaN, so testing all nine would report a failure to a caller
        ! who asked only for the sum and got the right answer. The counts are always defined and
        ! are not tested.
        if (present(ok)) then
            ok = .true.
            if (present(mean)) ok = ok .and. (mean == mean)
            if (present(variance)) ok = ok .and. (variance == variance)
            if (present(stddev)) ok = ok .and. (stddev == stddev)
            if (present(sem)) ok = ok .and. (sem == sem)
            if (present(skewness)) ok = ok .and. (skewness == skewness)
            if (present(kurtosis)) ok = ok .and. (kurtosis == kurtosis)
            if (present(vsum)) ok = ok .and. (vsum == vsum)
            if (present(vmin)) ok = ok .and. (vmin == vmin)
            if (present(vmax)) ok = ok .and. (vmax == vmax)
        end if
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
        self%ordered = .false.
        self%mad_ready = .false.
        self%mad_raw = 0.0_real64
        self%mad_c = 0.0_real64
    end subroutine stats_reset

    !> Drops tiers B and C: the retained values may no longer be in order.
    !!
    !! **Called by every mutation, unconditionally, and before it mutates anything.** The
    !! unconditional part is the point: writing `if (self%hold) self%ordered = .false.` inside a
    !! branch would be correct today and would be lost the first time the branch structure changes,
    !! and what it costs is one store on a path that is about to do O(n) work anyway.
    !!
    !! **The failure this prevents is silent.** A median cached across an `%update` is not wrong in
    !! any way an assertion can see -- it is a plausible number for a population that no longer
    !! exists. `test_update_drops_the_order_cache` is the only thing in the suite that can tell the
    !! difference, and it was written before this procedure and confirmed to fail without it.
    pure subroutine stats_invalidate_order(self)
        class(pf_stats), intent(inout) :: self !! the accumulator being mutated.
        self%ordered = .false.
        ! Tier C goes with tier B, and it is the more dangerous of the two to leave behind: a
        ! cached deviation is a bare `real(real64)` with nothing about it to suggest which
        ! population it describes, where a stale ORDERING is at least still an ordering of values
        ! the object holds. One flag, dropped in the one place every mutation passes through.
        self%mad_ready = .false.
    end subroutine stats_invalidate_order

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
    module procedure stats_ensure
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
    end procedure stats_ensure

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
        ! The third policy flag, guarded for the same reason as the other two: `skipnan` decides
        ! what the population IS, so folding a NaN-skipping partial into a NaN-propagating one
        ! produces a number describing neither convention. All three are fixed at `%init`/
        ! `%compute` and all three must agree across a merge.
        if (other%skip .neqv. self%skip) error stop "pf_stats%merge: the destination and the " // &
            "source disagree on skipnan"
        ! After the guards, before anything is written: a refused merge must leave the destination
        ! exactly as it was, ordering included.
        call stats_invalidate_order(self)

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
        call stats_invalidate_order(self)
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

    !> The retain guard the two power-mean bindings share.
    !!
    !! Separate from `stats_require_live` because the two failures have different fixes and a
    !! caller reading the message needs to be told which one they hit.
    subroutine stats_require_hold(self, what)
        class(pf_stats), intent(in) :: self  !! the accumulator.
        character(len=*), intent(in) :: what !! the binding's name, for the message.
        if (.not. self%hold) &
            error stop what // ": this accumulator was created with retain=.false., so no " // &
                "values were kept and a power mean cannot be formed from the central moments; " // &
                "use retain=.true. (the default for %compute) if you need one"
    end subroutine stats_require_hold

    module procedure obj_gmean
        logical :: good
        call stats_require_live(self, "pf_stats%gmean")
        call stats_require_hold(self, "pf_stats%gmean")
        call stats_ensure(self)
        ! **An accumulator that has retained nothing has no array to hand on.** `%init` leaves
        ! `keep` unallocated and only `%update`'s first value allocates it, so a `%gmean` called
        ! straight after `%init` would associate an unallocated allocatable with
        ! `power_mean_kept`'s non-optional `keep` dummy -- not permitted (F2018 15.5.2.12), and
        ! reported as `ALLOCATABLE SELF%KEEP is not currently allocated` by nagfor's `-C=all`.
        ! Every other compiler in the fleet runs it silently, which is why this needed a checked
        ! build to see. The answer is unchanged: `power_mean_kept` returns NaN for an empty
        ! population in its first statement, so the guard reproduces it rather than replacing it.
        if (self%keep_n == 0_int64) then
            res = stats_nan()
            return
        end if
        if (self%wtd) then
            call power_mean_kept(self%keep, self%keep_n, .false., self%acc%saw_nan, res, good, &
                w=self%keep_w)
        else
            call power_mean_kept(self%keep, self%keep_n, .false., self%acc%saw_nan, res, good)
        end if
    end procedure obj_gmean

    module procedure obj_hmean
        logical :: good
        call stats_require_live(self, "pf_stats%hmean")
        call stats_require_hold(self, "pf_stats%hmean")
        call stats_ensure(self)
        ! **An accumulator that has retained nothing has no array to hand on.** `%init` leaves
        ! `keep` unallocated and only `%update`'s first value allocates it, so a `%hmean` called
        ! straight after `%init` would associate an unallocated allocatable with
        ! `power_mean_kept`'s non-optional `keep` dummy -- not permitted (F2018 15.5.2.12), and
        ! reported as `ALLOCATABLE SELF%KEEP is not currently allocated` by nagfor's `-C=all`.
        ! Every other compiler in the fleet runs it silently, which is why this needed a checked
        ! build to see. The answer is unchanged: `power_mean_kept` returns NaN for an empty
        ! population in its first statement, so the guard reproduces it rather than replacing it.
        if (self%keep_n == 0_int64) then
            res = stats_nan()
            return
        end if
        if (self%wtd) then
            call power_mean_kept(self%keep, self%keep_n, .true., self%acc%saw_nan, res, good, &
                w=self%keep_w)
        else
            call power_mean_kept(self%keep, self%keep_n, .true., self%acc%saw_nan, res, good)
        end if
    end procedure obj_hmean

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

    !> One block of the two-sample pass two: the three centred cross sums about `(mux, muy)`.
    !!
    !! The single-sample twin of this is `stats_block_moments`, and the two are deliberately
    !! adjacent: `sxy` accumulates `w*dx*dy` in exactly the order that one accumulates `w*d*d`,
    !! which is what makes the diagonal case bit-identical rather than merely close.
    pure subroutine stats_pair_block(kx, ky, kw, mux, muy, j, m, o1x, o1y, oxx, oxy, oyy)
        real(real64), intent(in) :: kx(:)     !! the surviving first-sample values.
        real(real64), intent(in) :: ky(:)     !! the surviving second-sample values, paired.
        real(real64), intent(in), optional :: kw(:)
        !! their weights. Absent for an unweighted population, which is what an unallocated actual
        !! argument produces; its presence is the weighted test.
        real(real64), intent(in) :: mux       !! the first sample's mean from pass one.
        real(real64), intent(in) :: muy       !! the second's.
        integer(int64), intent(in) :: j       !! which block, 1-based.
        integer(int64), intent(in) :: m       !! how many pairs there are in total.
        real(real64), intent(out) :: o1x      !! `sum(w*dx)`, the first re-centring term.
        real(real64), intent(out) :: o1y      !! `sum(w*dy)`.
        real(real64), intent(out) :: oxx      !! `sum(w*dx**2)`.
        real(real64), intent(out) :: oxy      !! `sum(w*dx*dy)`.
        real(real64), intent(out) :: oyy      !! `sum(w*dy**2)`.
        real(real64) :: s1x, s1y, sxx, sxy, syy, dx, dy
        integer(int64) :: i, lo, hi

        lo = (j - 1_int64) * STATS_BLOCK + 1_int64
        hi = min(j * STATS_BLOCK, m)
        s1x = 0.0_real64
        s1y = 0.0_real64
        sxx = 0.0_real64
        sxy = 0.0_real64
        syy = 0.0_real64
        if (present(kw)) then
            do i = lo, hi
                dx = kx(i) - mux
                dy = ky(i) - muy
                s1x = s1x + kw(i) * dx
                s1y = s1y + kw(i) * dy
                sxx = sxx + kw(i) * dx * dx
                sxy = sxy + kw(i) * dx * dy
                syy = syy + kw(i) * dy * dy
            end do
        else
            do i = lo, hi
                dx = kx(i) - mux
                dy = ky(i) - muy
                s1x = s1x + dx
                s1y = s1y + dy
                sxx = sxx + dx * dx
                sxy = sxy + dx * dy
                syy = syy + dy * dy
            end do
        end if
        o1x = s1x
        o1y = s1y
        oxx = sxx
        oxy = sxy
        oyy = syy
    end subroutine stats_pair_block

    module procedure stats_pair_moments
        real(real64), allocatable :: px(:), py(:), pw(:)
        real(real64), allocatable :: q1x(:), q1y(:), qxx(:), qxy(:), qyy(:)
        real(real64) :: sx, sy, sw, mux, muy, dx, dy
        integer(int64) :: i, c, nb
        logical :: weighted, xpinf, xninf, ypinf, yninf

        weighted = allocated(kw)
        xpinf = .false.
        xninf = .false.
        ypinf = .false.
        yninf = .false.
        mean_x = stats_nan()
        mean_y = stats_nan()
        sxx = 0.0_real64
        sxy = 0.0_real64
        syy = 0.0_real64
        w_sum = 0.0_real64
        w_sq = 0.0_real64
        if (m == 0_int64) return

        ! ---- Pass one: the two weighted sums, over the SAME fixed block tree stats_engine uses.
        !
        ! Same block size, same partial-sum layout, same `pair_reduce` walk -- so for `x == y` the
        ! sum below is bit-identical to the one `stats_engine` computes for its `vsum`. That is
        ! the whole reason this lives beside it.
        allocate(px(m / STATS_BLOCK + 1_int64), py(m / STATS_BLOCK + 1_int64))
        if (weighted) allocate(pw(m / STATS_BLOCK + 1_int64))
        nb = 0_int64
        c = 0_int64
        sx = 0.0_real64
        sy = 0.0_real64
        sw = 0.0_real64
        do i = 1_int64, m
            ! **An infinity is recorded and never added**, exactly as `stats_engine`'s pass one
            ! does it and for the same reason: `Inf + (-Inf)` raises IEEE_INVALID, and nagfor
            ! unmasks the IEEE traps by default (`-ieee=stop`), so executing it kills the process
            ! on data this module answers rather than refuses. The fold-back is below the
            ! reduction. Each arm keeps the exact expression it had, because `pf_cov(x, x)` is
            ! asserted to be `pf_variance(x)` bit for bit and both sides must stay unchanged on a
            ! finite population, which is every population that assertion uses.
            !
            ! No NaN guard is needed on the `abs`, unlike `stats_engine`: the pair compaction in
            ! parquet_stats_relate.f90 drops an element whose x OR y is a NaN unconditionally --
            ! there is no `skipnan = .false.` route into this procedure -- so neither array can
            ! hold one here.
            if (abs(kx(i)) > huge(0.0_real64)) then
                if (kx(i) > 0.0_real64) then
                    xpinf = .true.
                else
                    xninf = .true.
                end if
            else if (weighted) then
                sx = sx + kw(i) * kx(i)
            else
                sx = sx + kx(i)
            end if
            if (abs(ky(i)) > huge(0.0_real64)) then
                if (ky(i) > 0.0_real64) then
                    ypinf = .true.
                else
                    yninf = .true.
                end if
            else if (weighted) then
                sy = sy + kw(i) * ky(i)
            else
                sy = sy + ky(i)
            end if
            if (weighted) then
                sw = sw + kw(i)
            else
                sw = sw + 1.0_real64
            end if
            c = c + 1_int64
            if (c == STATS_BLOCK) then
                nb = nb + 1_int64
                px(nb) = sx
                py(nb) = sy
                if (weighted) pw(nb) = sw
                sx = 0.0_real64
                sy = 0.0_real64
                sw = 0.0_real64
                c = 0_int64
            end if
        end do
        if (c > 0_int64) then
            nb = nb + 1_int64
            px(nb) = sx
            py(nb) = sy
            if (weighted) pw(nb) = sw
        end if
        call pair_reduce(px, nb)
        call pair_reduce(py, nb)
        ! The infinities, folded back: both signs cancel to a NaN, one sign swallows whatever
        ! finite partial sum sits beside it. Same rule as `stats_engine`'s, which reads its signs
        ! off the extremes it already tracks; this procedure tracks none, so it carries flags.
        if (xpinf .and. xninf) then
            px(1) = stats_nan()
        else if (xpinf) then
            px(1) = ieee_value(1.0_real64, ieee_positive_inf)
        else if (xninf) then
            px(1) = ieee_value(1.0_real64, ieee_negative_inf)
        end if
        if (ypinf .and. yninf) then
            py(1) = stats_nan()
        else if (ypinf) then
            py(1) = ieee_value(1.0_real64, ieee_positive_inf)
        else if (yninf) then
            py(1) = ieee_value(1.0_real64, ieee_negative_inf)
        end if
        if (weighted) then
            call pair_reduce(pw, nb)
            w_sum = pw(1)
            w_sq = sum_of_squares(kw, m, nb)
        else
            w_sum = real(m, real64)
            w_sq = real(m, real64)
        end if
        if (w_sum <= 0.0_real64) return
        mux = px(1) / w_sum
        muy = py(1) / w_sum

        ! **An infinity in EITHER variable stops here, before pass two runs at all.** The matching
        ! mean is then infinite (or NaN), so `kx(i) - mux` is `Inf - Inf` for the infinite element
        ! itself -- IEEE_INVALID, and a dead process under nagfor's default `-ieee=stop`. All
        ! three centred sums are reported undefined rather than only the two the poisoned variable
        ! enters: this procedure answers about a PAIR, and `syy` alone is what `pf_variance(y)`
        ! is for. That matches the single-variable family's own rule, where a population holding
        ! an infinity has every central moment NaN (the non-finite table in
        ! doc/pages/utilities/statistics.md), so `pf_cov` and `pf_corr` come back NaN with
        ! `ok = .false.` there rather than aborting.
        if (xpinf .or. xninf .or. ypinf .or. yninf) then
            mean_x = mux
            mean_y = muy
            sxx = stats_nan()
            sxy = stats_nan()
            syy = stats_nan()
            return
        end if

        ! ---- Pass two: the three centred sums, then the same re-centring correction.
        !
        allocate(q1x(nb), q1y(nb), qxx(nb), qxy(nb), qyy(nb))
        do i = 1_int64, nb
            call stats_pair_block(kx, ky, mux=mux, muy=muy, j=i, m=m, kw=kw, o1x=q1x(i), &
                o1y=q1y(i), oxx=qxx(i), oxy=qxy(i), oyy=qyy(i))
        end do
        call pair_reduce(q1x, nb)
        call pair_reduce(q1y, nb)
        call pair_reduce(qxx, nb)
        call pair_reduce(qxy, nb)
        call pair_reduce(qyy, nb)
        ! `sum(w*(dx - deltax)*(dy - deltay))` expanded, using `sum(w*dx) = deltax*W`: every cross
        ! term collapses and one product of the two rounding errors is left. The single-sample
        ! form `q2 - delta**2 * W` is this with `x == y`.
        !
        ! **The correction is UNOBSERVABLE at double precision, and is kept for symmetry.** Both
        ! deltas are rounding errors of size `eps*|mu|`, so the term is about `eps**2 * mu**2 * W`
        ! against a `qxy` of about `var * W` -- it could only matter once `|mu|/sigma` passed
        ! `1/eps`, roughly 1e16, a shift at which the values themselves have no resolution left.
        ! Deleting it changes no answer, and a mutation that does so SURVIVES the whole suite.
        ! It stays because this procedure has to mirror `stats_engine` line for line: that is what
        ! makes `pf_cov(x, x)` exactly `pf_variance(x)` by construction rather than by the
        ! correction happening to vanish on whatever fixture a test picked. The single-sample
        ! engine needs it for real, because `m3` and `m4` pick the error up linearly.
        dx = q1x(1) / w_sum
        dy = q1y(1) / w_sum
        mean_x = mux + dx
        mean_y = muy + dy
        sxx = qxx(1) - dx * dx * w_sum
        sxy = qxy(1) - dx * dy * w_sum
        syy = qyy(1) - dy * dy * w_sum
    end procedure stats_pair_moments

    module procedure stats_mean_sd
        type(stats_acc) :: acc
        integer :: dd

        dd = 1
        if (present(ddof)) dd = ddof
        call stats_engine(values, what, is_valid, skipnan=skipnan, acc=acc, nmom=2)
        mean = acc%mean
        sd = sqrt(stats_var(acc, dd, .false.))
        n_valid = acc%n_valid
        n_null = acc%n_null
        n_nan = acc%n_nan
        saw_nan = acc%saw_nan
    end procedure stats_mean_sd

    module procedure stats_compact
        type(stats_acc) :: acc
        call stats_engine(values, what, is_valid, weights, skipnan, acc, keep_x, keep_w, &
            nmom=0)
        n_valid = acc%n_valid
        n_null = acc%n_null
        n_nan = acc%n_nan
        ! `skipnan = .false.` and a NaN survived. The engine already answers NaN for every moment
        ! in this state; the order tier cannot inherit that for free, because a NaN sorts to one
        ! END of the buffer rather than poisoning it, so a median would come back as a perfectly
        ! ordinary number from a population the caller asked to have poisoned. Every order
        ! statistic therefore has to test this flag itself.
        saw_nan = acc%saw_nan
        ! An unweighted population leaves `keep_w` unallocated, and the order tier reads that as
        ! "unweighted" rather than carrying a separate flag -- the same convention `pf_stats` uses.
    end procedure stats_compact

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
