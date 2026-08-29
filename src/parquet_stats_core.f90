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
!! than a Welford update over a resident array -- the streaming form arrives with `%update` in P3,
!! where there is no buffer to re-walk and nothing else is possible.
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
    implicit none

    !> Elements per block of the fixed decomposition.
    !!
    !! A function of nothing but the population size -- 128 is numpy's own pairwise block and is
    !! large enough that the per-block bookkeeping is noise, small enough that the serial sum inside
    !! a block contributes no meaningful error. **Changing it changes published answers in the last
    !! bits**, so it is a frozen constant rather than a tuning knob.
    integer(int64), parameter :: STATS_BLOCK = 128_int64

    !> Everything tier A holds about one population: the counts, the weight sums and the central
    !! moments about the mean, all accumulated over the block tree.
    type :: stats_acc
        integer(int64) :: n_valid = 0_int64 !! elements that survived every exclusion.
        integer(int64) :: n_null = 0_int64  !! elements `is_valid` excluded.
        integer(int64) :: n_nan = 0_int64   !! excluded as NaN, and not already null.
        real(real64) :: w_sum = 0.0_real64  !! `sum(w)`; the population size when unweighted.
        real(real64) :: w_sq = 0.0_real64   !! `sum(w**2)`, which the reliability count needs.
        real(real64) :: vsum = 0.0_real64   !! `sum(w*x)`; exactly 0 for an empty population.
        real(real64) :: mean = 0.0_real64   !! `vsum / w_sum`.
        real(real64) :: m2 = 0.0_real64     !! `sum(w*(x-mean)**2)`.
        real(real64) :: m3 = 0.0_real64     !! `sum(w*(x-mean)**3)`.
        real(real64) :: m4 = 0.0_real64     !! `sum(w*(x-mean)**4)`.
        real(real64) :: vmin = 0.0_real64   !! the smallest surviving value.
        real(real64) :: vmax = 0.0_real64   !! the largest surviving value.
        logical :: empty = .true.           !! .true. when nothing survived; every moment is then NaN.
        logical :: saw_nan = .false.        !! a NaN entered the population under `skipnan = .false.`.
    end type stats_acc

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
    subroutine stats_engine(values, what, is_valid, weights, skipnan, acc)
        real(real64), intent(in) :: values(:)                 !! the population, before exclusions.
        character(len=*), intent(in) :: what                  !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)          !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)      !! per element weight.
        logical, intent(in), optional :: skipnan              !! .true. (default) excludes a NaN.
        type(stats_acc), intent(out) :: acc                   !! everything tier A holds.

        real(real64), allocatable :: xb(:), wb(:), pw(:), px(:), q1(:), q2(:), q3(:), q4(:)
        real(real64) :: x, w, d, dd, sw, sx, s1, s2, s3, s4, mu, delta
        integer(int64) :: nv, m, nb, i, c, lo, hi, j
        logical :: skip, weighted, masked

        nv = size(values, kind=int64)
        call stats_check_sizes(nv, what, is_valid, weights)
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
            return
        end if
        if (acc%saw_nan) then
            ! `skipnan = .false.` and a NaN survived: the caller asked for propagation, so every
            ! answer is NaN. Returning before the arithmetic also keeps the min/max comparisons and
            ! (in P8) the transcendentals away from it.
            acc%vsum = stats_nan()
            call stats_undefine(acc)
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

        ! ---- Pass two: the central moments, against the mean pass one produced ----
        !
        ! `q1` accumulates `sum(w*d)`, which is algebraically ZERO and in floating point is the
        ! rounding error left in `mu`. Carrying it is what makes the THIRD and FOURTH moments
        ! shift-invariant, and it is not optional: the second moment is immune to a perturbed mean
        ! because its derivative there vanishes, and the higher ones are not. `m3` picks the error
        ! up linearly through `3*delta*m2`, which at an offset of 1e9 was measured costing eight
        ! significant digits of the skewness while the variance was still correct to fifteen.
        allocate(q1(nb), q2(nb), q3(nb), q4(nb))
        do j = 1_int64, nb
            lo = (j - 1_int64) * STATS_BLOCK + 1_int64
            hi = min(j * STATS_BLOCK, m)
            s1 = 0.0_real64
            s2 = 0.0_real64
            s3 = 0.0_real64
            s4 = 0.0_real64
            if (weighted) then
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
            q1(j) = s1
            q2(j) = s2
            q3(j) = s3
            q4(j) = s4
        end do
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
    end subroutine stats_engine

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
        call stats_engine(values, "pf_sum", is_valid, weights, skipnan, acc)
        if (present(n_null)) n_null = acc%n_null
        if (present(n_nan)) n_nan = acc%n_nan
        s = acc%vsum
        if (present(ok)) ok = (s == s)
    end procedure sum_f64

    module procedure mean_f64
        type(stats_acc) :: acc
        call stats_engine(values, "pf_mean", is_valid, weights, skipnan, acc)
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
        call stats_engine(values, "pf_variance", is_valid, weights, skipnan, acc)
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
        call stats_engine(values, "pf_stddev", is_valid, weights, skipnan, acc)
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
        call stats_engine(values, "pf_sem", is_valid, weights, skipnan, acc)
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
        call stats_engine(values, "pf_skewness", is_valid, weights, skipnan, acc)
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
        call stats_engine(values, "pf_kurtosis", is_valid, weights, skipnan, acc)
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
        call stats_engine(values, "pf_moments", is_valid, weights, skipnan, acc)

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

end submodule parquet_stats_core ! GCOVR_EXCL_LINE
