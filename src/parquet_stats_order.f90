!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file. The module SPEC that declares everything here IS generated
! (tools/generate_parquet_stats.py), so a signature change means editing that script's template
! text while a body change means editing this file.
!
!> Tier B of `parquet_stats`: the ordered population, and every statistic read off it.
!!
!! **One ordering, then O(log n) probes into it.** `pf_stats` sorts its retained survivors the
!! first time an order statistic is asked for and answers every later one off that ordering, which
!! is the whole reason the type exists rather than eight one-shot calls. The sort happens IN PLACE
!! in the retained buffer, so tier B costs no memory of its own -- and that is why nothing here
!! ever hands the retained values back in their original order, and why no binding should be added
!! that does.
!!
!! **The cache's one hard rule is that any mutation drops it.** `%update` and `%merge` invalidate
!! the ordering unconditionally. A cached median that survives an `%update` is not a loud failure:
!! the answer is merely stale, and a stale median is a perfectly plausible number. Nothing in an
!! ordinary assertion can see it, which is why `parquet_debug_stats_sorts()` exists and why its
!! `sorts == 2`-after-`%update` test was written before the invalidation it checks.
!!
!! **There is ONE definition of what a quantile is** -- `quantile_sorted` below. The one-shot
!! procedures, `pf_stats`' bindings, `pf_median` and both quartiles of `pf_iqr` all land there, so
!! two of them cannot come to disagree about an interpolation rule. A second copy would be a defect
!! rather than an optimisation.
!!
!! **The weighted position rule reduces EXACTLY to Hyndman-Fan type 7 at equal weights**, which is
!! the property it was chosen for: a script that adds uniform weights must not see its numbers
!! move. No reference library interpolates a weighted quantile, so that invariant -- rather than a
!! cross-check -- is what pins the rule, and `method="inverted_cdf"` is the numpy-comparable
!! escape hatch. See `feature_pandas_S4.md`'s "Weights" section for the derivation.
!!
!! **A one-shot single probe SELECTS rather than sorts.** `pf_median(v, m)` over an unweighted
!! population is O(n) through `pf_nth_element`, not O(n log n); weights force the sort, because the
!! position rule needs the cumulative weight in sorted order. `pf_quantiles` sorts once above a
!! small probability count and selects below it.
submodule (parquet_stats) parquet_stats_order
    implicit none

    ! ---- The interpolation rules, as codes rather than tokens ----
    !
    ! Resolved once per call by `method_code` and passed down as an integer, so no inner loop ever
    ! compares strings. The values are arbitrary and private; only the resolver and the
    ! interpolator know them.
    integer, parameter :: QM_LINEAR = 1       !! Hyndman-Fan type 7; numpy's and pandas' default.
    integer, parameter :: QM_LOWER = 2        !! the order statistic below the position.
    integer, parameter :: QM_HIGHER = 3       !! the order statistic above it.
    integer, parameter :: QM_NEAREST = 4      !! whichever of the two is nearer.
    integer, parameter :: QM_MIDPOINT = 5     !! their mean.
    integer, parameter :: QM_INVERTED_CDF = 6 !! a step function on the plain cumulative scale.

    ! ---- How a value equal to the score is counted, for percentile_of_score ----
    integer, parameter :: PS_RANK = 1   !! ties count half; scipy's default.
    integer, parameter :: PS_WEAK = 2   !! a value <= score counts.
    integer, parameter :: PS_STRICT = 3 !! only < score counts.
    integer, parameter :: PS_MEAN = 4   !! the average of weak and strict.

    !> How many times any `pf_stats` has ordered its retained buffer.
    !!
    !! Read by `parquet_debug_stats_sorts()` and by nothing else. Process-global and deliberately
    !! unsynchronised: it is a test observable, not library state, and the `stats` suite is
    !! excluded from `test/run_tester.f90`'s per-suite parallelism for exactly that reason.
    integer(int64), save :: stats_sort_count = 0_int64

    !> Probabilities at or above which `pf_quantiles` sorts instead of selecting one at a time.
    !!
    !! Selection is O(n) per probe and a sort is O(n log n) once, so the crossover is around
    !! `log2(n)` probes -- but the constant matters more than the asymptotics at the sizes this is
    !! reached with, and a small fixed number is both defensible and cheap to reason about. Four is
    !! deliberately above the two-probe case `pf_iqr` uses, so the quartiles select.
    integer(int64), parameter :: QUANTILE_SORT_MIN = 4_int64

    !> Test-only override for `QUANTILE_SORT_MIN`; negative means the shipped value applies.
    integer(int64), save :: dbg_quantile_sort_min = -1_int64

contains

    ! ==================================================================================
    ! The order tier's own helpers
    !
    ! Ordinary contained procedures, not separate module procedures: only this submodule calls
    ! them, so no interface is needed -- which also keeps them clear of nagfor's rule that a
    ! separate module procedure must be IMPLEMENTED before it is CALLED within one submodule.
    ! ==================================================================================

    !> Resolves a `method=` token to a `QM_*` code, or aborts naming all six.
    subroutine method_code(what, method, weighted_freq, code)
        character(len=*), intent(in) :: what              !! the public procedure's name.
        character(len=*), intent(in), optional :: method  !! the caller's token, if any.
        logical, intent(in) :: weighted_freq              !! .true. for weight_type="frequency".
        integer, intent(out) :: code                      !! the resolved rule.
        character(len=32) :: tok
        integer :: i, c

        if (.not. present(method)) then
            ! `weight_type="frequency"` changes the DEFAULT, because `inverted_cdf` IS frequency
            ! expansion: on x = [1,2,3] with w = [1,1,2] it agrees at every probability with the
            ! expanded [1,2,3,3], where the linear rule does not. Leaving the default alone would
            ! hand a caller who declared frequency weights the reliability-shaped median.
            if (weighted_freq) then
                code = QM_INVERTED_CDF
            else
                code = QM_LINEAR
            end if
            return
        end if
        tok = ""
        do i = 1, min(len_trim(method), len(tok))
            c = iachar(method(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            tok(i:i) = achar(c)
        end do
        select case (trim(tok))
        case ("linear");       code = QM_LINEAR
        case ("lower");        code = QM_LOWER
        case ("higher");       code = QM_HIGHER
        case ("nearest");      code = QM_NEAREST
        case ("midpoint");     code = QM_MIDPOINT
        case ("inverted_cdf"); code = QM_INVERTED_CDF
        case default
            error stop what // ': method="' // trim(method) // &
                '" is not recognised; use "linear", "lower", "higher", "nearest", "midpoint" ' // &
                'or "inverted_cdf"'
        end select
    end subroutine method_code

    !> Resolves a `kind=` token for `pf_percentile_of_score`, or aborts naming all four.
    subroutine score_kind_code(what, kind, code)
        character(len=*), intent(in) :: what            !! the public procedure's name.
        character(len=*), intent(in), optional :: kind  !! the caller's token, if any.
        integer, intent(out) :: code                    !! the resolved convention.
        character(len=32) :: tok
        integer :: i, c

        if (.not. present(kind)) then
            code = PS_RANK
            return
        end if
        tok = ""
        do i = 1, min(len_trim(kind), len(tok))
            c = iachar(kind(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            tok(i:i) = achar(c)
        end do
        select case (trim(tok))
        case ("rank");   code = PS_RANK
        case ("weak");   code = PS_WEAK
        case ("strict"); code = PS_STRICT
        case ("mean");   code = PS_MEAN
        case default
            error stop what // ': kind="' // trim(kind) // &
                '" is not recognised; use "rank", "weak", "strict" or "mean"'
        end select
    end subroutine score_kind_code

    !> Aborts unless every entry of `probs` lies in `[0, 1]`.
    !!
    !! A NaN is caught by the same test, because `p >= 0` is false for it -- and the comparison is
    !! written that way round rather than as `p < 0 .or. p > 1` so that a NaN falls into the abort
    !! rather than out of it.
    subroutine check_probs(what, probs)
        character(len=*), intent(in) :: what      !! the public procedure's name.
        real(real64), intent(in) :: probs(:)      !! the probabilities to validate.
        integer(int64) :: i
        character(len=32) :: buf

        do i = 1_int64, size(probs, kind=int64)
            if (.not. (probs(i) >= 0.0_real64 .and. probs(i) <= 1.0_real64)) then
                write (buf, '(es13.6)') probs(i)
                error stop what // ": probability " // trim(stats_i2s(i)) // " is " // &
                    trim(adjustl(buf)) // "; every probability must lie in [0, 1]"
            end if
        end do
    end subroutine check_probs

    !> The position each sorted value sits at, on the 0-1 scale the interpolation runs over.
    !!
    !! Unweighted this is type 7's `(i-1)/(m-1)`. Weighted it is `S_{i-1} / (W - w_i)`, whose
    !! defining property is that it collapses to exactly `(i-1)/(m-1)` when every weight is equal
    !! -- so a script that adds uniform weights does not see its numbers move. It is
    !! non-decreasing for any positive weights, and `p_1 = 0`, `p_m = 1`, so `q(0)` and `q(1)` are
    !! the minimum and the maximum exactly.
    pure subroutine value_positions(w, m, pos)
        real(real64), intent(in), optional :: w(:)  !! the weights, in sorted order.
        integer(int64), intent(in) :: m             !! how many survivors there are.
        real(real64), intent(out) :: pos(:)         !! their positions; at least `m` long.
        real(real64) :: total, run, denom
        integer(int64) :: i

        if (m == 1_int64) then
            pos(1) = 0.0_real64
            return
        end if
        if (.not. present(w)) then
            do i = 1_int64, m
                pos(i) = real(i - 1_int64, real64) / real(m - 1_int64, real64)
            end do
            return
        end if
        total = 0.0_real64
        do i = 1_int64, m
            total = total + w(i)
        end do
        run = 0.0_real64
        do i = 1_int64, m
            denom = total - w(i)
            if (denom > 0.0_real64) then
                pos(i) = run / denom
            else
                ! Every weight is on this one element, so it is simultaneously the minimum and the
                ! maximum; position 0 keeps the sequence non-decreasing and `q(p)` constant.
                pos(i) = 0.0_real64
            end if
            run = run + w(i)
        end do
        ! The last position is 1 by construction (`S_{m-1} = W - w_m`); assign it exactly rather
        ! than leaving it to round, so `q(1)` is the maximum bit for bit.
        pos(m) = 1.0_real64
    end subroutine value_positions

    !> Reads one quantile off an already-sorted population. **The only definition in the module.**
    subroutine quantile_sorted(x, w, p, code, res)
        real(real64), intent(in) :: x(:)            !! the survivors, ascending.
        real(real64), intent(in), optional :: w(:)  !! their weights, in the same order.
        real(real64), intent(in) :: p               !! the probability, already validated.
        integer, intent(in) :: code                 !! the QM_* rule.
        real(real64), intent(out) :: res            !! the quantile.
        real(real64), allocatable :: pos(:)
        real(real64) :: total, run, want, t
        integer(int64) :: m, i, lo, hi

        m = size(x, kind=int64)
        if (m == 0_int64) then
            res = stats_nan()
            return
        end if
        if (m == 1_int64) then
            res = x(1)
            return
        end if

        if (code == QM_INVERTED_CDF) then
            ! The plain cumulative scale: the smallest value whose cumulative share reaches `p`.
            ! A step function, no interpolation, and the one numpy itself accepts weights with.
            total = 0.0_real64
            if (present(w)) then
                do i = 1_int64, m
                    total = total + w(i)
                end do
            else
                total = real(m, real64)
            end if
            want = p * total
            run = 0.0_real64
            do i = 1_int64, m
                if (present(w)) then
                    run = run + w(i)
                else
                    run = run + 1.0_real64
                end if
                if (run >= want) then
                    res = x(i)
                    return
                end if
            end do
            res = x(m)
            return
        end if

        if (.not. present(w)) then
            ! **The unweighted position is taken on the RANK scale, `g = p*(m-1)`, not by dividing
            ! two entries of a position array.** Both are algebraically the same number and they
            ! are not the same floating-point number: `15/31` and `16/31` are each inexact, so
            ! `(p - pos(lo))/(pos(hi) - pos(lo))` lands a few ulp either side of an exact tie --
            ! and `nearest` reads exactly that tie. One multiply gives `g = 15.5` exactly, which is
            ! also numpy's own formulation, so the two agree by construction rather than by luck.
            ! This is what made the ordering path and the selection path disagree at `p = 0.5` on
            ! an even-length population; they now compute the identical quantity.
            t = p * real(m - 1_int64, real64)
            lo = min(m, int(t, int64) + 1_int64)
            t = t - real(lo - 1_int64, real64)
            hi = min(lo + 1_int64, m)
        else
            allocate(pos(m))
            call value_positions(w, m, pos)
            ! `pos` is non-decreasing, so a linear walk is correct. A bisection would be faster and
            ! is deliberately not used: `pf_quantiles` already amortises this over one ordering,
            ! and a bisection over ties needs care this does not.
            lo = 1_int64
            do i = 1_int64, m
                if (pos(i) <= p) then
                    lo = i
                else
                    exit
                end if
            end do
            hi = min(lo + 1_int64, m)
            if (pos(hi) > pos(lo)) then
                t = (p - pos(lo)) / (pos(hi) - pos(lo))
            else
                t = 0.0_real64
            end if
        end if
        call apply_method(x, lo, hi, t, code, res)
    end subroutine quantile_sorted

    !> Turns a bracketing pair and a fractional offset into one value, under a `QM_*` rule.
    !!
    !! Shared by the ordering path and the selection path so the two cannot resolve a position
    !! differently -- which they did, at exactly one input, before this existed.
    !!
    !! **`nearest` breaks an exact tie the way numpy does: to the EVEN lower index.** That rule
    !! only ever fires at `t == 0.5`, which needs an even-length population and a probability
    !! landing exactly between two ranks -- `p = 0.5` on 32 elements is the case a user meets
    !! first. Rounding down instead would be defensible in isolation and would disagree with
    !! `np.quantile(..., method="nearest")` on the most obvious input there is.
    pure subroutine apply_method(x, lo, hi, t, code, res)
        real(real64), intent(in) :: x(:)      !! the survivors, ascending.
        integer(int64), intent(in) :: lo      !! the lower bracketing rank, 1-based.
        integer(int64), intent(in) :: hi      !! the upper one; equal to `lo` at the top end.
        real(real64), intent(in) :: t         !! the fractional offset in `[0, 1]`.
        integer, intent(in) :: code           !! the QM_* rule.
        real(real64), intent(out) :: res      !! the resolved value.

        select case (code)
        case (QM_LINEAR)
            res = x(lo) + t * (x(hi) - x(lo))
        case (QM_LOWER)
            res = x(lo)
        case (QM_HIGHER)
            if (t > 0.0_real64) then
                res = x(hi)
            else
                res = x(lo)
            end if
        case (QM_NEAREST)
            if (t > 0.5_real64) then
                res = x(hi)
            else if (t < 0.5_real64) then
                res = x(lo)
            else if (mod(lo - 1_int64, 2_int64) == 0_int64) then
                res = x(lo)          ! the zero-based lower index is even: numpy keeps it
            else
                res = x(hi)
            end if
        case (QM_MIDPOINT)
            if (t > 0.0_real64) then
                res = 0.5_real64 * (x(lo) + x(hi))
            else
                res = x(lo)
            end if
        case default
            error stop "parquet_stats: unreachable quantile method code"   ! GCOVR_EXCL_LINE
        end select
    end subroutine apply_method

    !> Sorts the survivors ascending, carrying their weights with them.
    !!
    !! `pf_argsort` plus `pf_permute` rather than `pf_sort`, because the weights have to travel
    !! with their values -- a weight separated from the value it belongs to is a silent wrong
    !! answer with nothing to report it. The unweighted case takes the same route, which costs one
    !! permutation and keeps a single code path.
    subroutine order_in_place(x, w, m, threads)
        real(real64), intent(inout) :: x(:)             !! the survivors; ascending afterwards.
        real(real64), allocatable, intent(inout) :: w(:) !! their weights, or unallocated.
        integer(int64), intent(in) :: m                 !! how many are live.
        integer, intent(in), optional :: threads        !! passed straight to `pf_argsort`.
        integer(int64), allocatable :: perm(:)

        if (m <= 1_int64) return
        call pf_argsort(x(1:m), perm, threads=threads)
        call pf_permute(x(1:m), perm)
        if (allocated(w)) call pf_permute(w(1:m), perm)
    end subroutine order_in_place

    !> One quantile of an unweighted population, by SELECTION rather than a sort.
    !!
    !! O(n) per probe through `pf_nth_element`, against O(n log n) for a sort, which is what makes
    !! a single `pf_median` cheap. Two selections are needed for an interpolating method because
    !! the answer straddles two order statistics; `pf_nth_element` takes `values` as `intent(in)`,
    !! so the second call is independent of the first and no partition state is carried.
    !!
    !! Only ever reached unweighted: the weighted position rule needs the cumulative weight in
    !! sorted order, which selection does not produce.
    subroutine quantile_by_selection(x, m, p, code, threads, res)
        real(real64), intent(in) :: x(:)          !! the survivors, in any order.
        integer(int64), intent(in) :: m           !! how many are live.
        real(real64), intent(in) :: p             !! the probability, already validated.
        integer, intent(in) :: code               !! the QM_* rule.
        integer, intent(in), optional :: threads  !! passed to `pf_nth_element`.
        real(real64), intent(out) :: res          !! the quantile.
        real(real64) :: gpos, t, x_lo, x_hi
        integer(int64) :: lo, hi

        if (m == 0_int64) then
            res = stats_nan()
            return
        end if
        if (m == 1_int64) then
            res = x(1)
            return
        end if
        if (code == QM_INVERTED_CDF) then
            ! ceil(p*m), clamped: the smallest rank whose cumulative share reaches p.
            lo = max(1_int64, min(m, ceiling(p * real(m, real64), int64)))
            call pf_nth_element(x(1:m), lo, res, threads=threads)
            return
        end if
        ! Type 7's position on the RANK scale: g = p*(m-1), zero-based, so rank lo = floor(g)+1.
        gpos = p * real(m - 1_int64, real64)
        lo = min(m, int(gpos, int64) + 1_int64)
        t = gpos - real(lo - 1_int64, real64)
        hi = min(lo + 1_int64, m)
        call pf_nth_element(x(1:m), lo, x_lo, threads=threads)
        if (hi > lo) then
            call pf_nth_element(x(1:m), hi, x_hi, threads=threads)
        else
            x_hi = x_lo
            t = 0.0_real64
        end if
        ! The same resolver the ordering path uses, over the two selected values rather than two
        ! entries of the sorted buffer -- which is what makes the two paths bit-identical.
        call apply_method([x_lo, x_hi], 1_int64, 2_int64, t, code, res)
    end subroutine quantile_by_selection

    !> The share of a population at or below `score`, under one of the four conventions.
    !!
    !! **`rank` reproduces scipy's `percentileofscore(kind='rank')` exactly when unweighted**, half
    !! a place above `mean` whenever there are ties and equal to `strict`/`weak` when there are
    !! none. That half a place is scipy's continuity correction and it is expressed in ELEMENTS, so
    !! the weighted form uses one average element's worth (`total/m`) -- which is exactly 1 when
    !! every weight is 1, so the unweighted answer is unchanged and the weighted one is the same
    !! rule rather than a second one. `mean` carries no such correction and is the token to reach
    !! for when a weighted answer should be a plain weight share.
    pure subroutine score_share(x, w, m, score, code, res)
        real(real64), intent(in) :: x(:)            !! the survivors, ascending.
        real(real64), intent(in), optional :: w(:)  !! their weights, in the same order.
        integer(int64), intent(in) :: m             !! how many are live.
        real(real64), intent(in) :: score           !! the value to locate.
        integer, intent(in) :: code                 !! the PS_* convention.
        real(real64), intent(out) :: res            !! the share, 0 to 1.
        real(real64) :: below, equal, total, wi
        integer(int64) :: i

        below = 0.0_real64
        equal = 0.0_real64
        total = 0.0_real64
        do i = 1_int64, m
            if (present(w)) then
                wi = w(i)
            else
                wi = 1.0_real64
            end if
            total = total + wi
            if (x(i) < score) then
                below = below + wi
            else if (x(i) == score) then
                equal = equal + wi
            end if
        end do
        if (total <= 0.0_real64) then
            res = stats_nan()
            return
        end if
        select case (code)
        case (PS_RANK)
            if (equal > 0.0_real64) then
                res = (below + 0.5_real64 * equal + 0.5_real64 * total / real(m, real64)) / total
            else
                ! No ties: scipy's rank collapses to the plain share below, which is also what
                ! `strict` and `weak` both give here, so all three agree.
                res = below / total
            end if
        case (PS_WEAK);   res = (below + equal) / total
        case (PS_STRICT); res = below / total
        case (PS_MEAN);   res = (below + 0.5_real64 * equal) / total
        end select
    end subroutine score_share

    ! ==================================================================================
    ! The one-shot real64 order procedures
    !
    ! Each applies the exclusion rules through `stats_compact` -- pass one of the moment engine
    ! and nothing else, so no order statistic pays for four central moments it would discard --
    ! and then either selects or orders, per `single_probe` below.
    ! ==================================================================================

    !> Whether a one-shot call should SELECT rather than order the population.
    !!
    !! Selection is O(n) per probe against O(n log n) once for a sort, so a single probe selects
    !! and several order. Weights force the ordering whatever the count: the weighted position rule
    !! needs the cumulative weight in sorted order, which selection does not produce.
    pure function should_select(nprobs, weighted) result(res)
        integer(int64), intent(in) :: nprobs !! how many probabilities are wanted.
        logical, intent(in) :: weighted      !! .true. when weights were supplied.
        logical :: res                       !! .true. to select, .false. to order.
        integer(int64) :: floor_n

        res = .false.
        if (weighted) return
        floor_n = QUANTILE_SORT_MIN
        if (dbg_quantile_sort_min >= 0_int64) floor_n = dbg_quantile_sort_min
        res = (nprobs < floor_n)
    end function should_select

    !> The whole one-shot quantile path: exclude, then select or order, then interpolate.
    !!
    !! Every one-shot order procedure lands here, so the exclusion rules, the empty-population
    !! answer and the select-versus-order decision are each written once.
    subroutine one_shot_quantiles(values, what, probs, out, is_valid, weights, weight_type, &
            skipnan, method, n_null, n_nan, ok, threads)
        real(real64), intent(in) :: values(:)                 !! the population.
        character(len=*), intent(in) :: what                  !! the public procedure's name.
        real(real64), intent(in) :: probs(:)                  !! the probabilities.
        real(real64), intent(out) :: out(:)                   !! one quantile each.
        logical, intent(in), optional :: is_valid(:)          !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)      !! per element weight.
        character(len=*), intent(in), optional :: weight_type !! "reliability" or "frequency".
        logical, intent(in), optional :: skipnan              !! .true. (default) excludes a NaN.
        character(len=*), intent(in), optional :: method      !! the interpolation rule.
        integer(int64), intent(out), optional :: n_null       !! how many were null.
        integer(int64), intent(out), optional :: n_nan        !! how many were NaN.
        logical, intent(out), optional :: ok                  !! .false. when the answer is NaN.
        integer, intent(in), optional :: threads              !! passed to the sort or selection.
        real(real64), allocatable :: keep(:), keep_w(:)
        integer(int64) :: m, nnull, nnan, k, i
        integer :: code
        logical :: freq

        if (size(out, kind=int64) /= size(probs, kind=int64)) &
            error stop what // ": out has " // trim(stats_i2s(size(out, kind=int64))) // &
                " elements but probs has " // trim(stats_i2s(size(probs, kind=int64)))
        call check_probs(what, probs)
        call stats_weight_kind(what, weight_type, freq)
        call method_code(what, method, freq .and. present(weights), code)
        call stats_compact(values, what, is_valid, weights, skipnan, keep, keep_w, m, nnull, nnan)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan

        k = size(probs, kind=int64)
        if (m == 0_int64) then
            do i = 1_int64, k
                out(i) = stats_nan()
            end do
            if (present(ok)) ok = .false.
            return
        end if
        if (should_select(k, allocated(keep_w))) then
            do i = 1_int64, k
                call quantile_by_selection(keep, m, probs(i), code, threads, out(i))
            end do
        else
            call order_in_place(keep, keep_w, m, threads)
            do i = 1_int64, k
                if (allocated(keep_w)) then
                    call quantile_sorted(keep(1:m), keep_w(1:m), probs(i), code, out(i))
                else
                    call quantile_sorted(keep(1:m), p=probs(i), code=code, res=out(i))
                end if
            end do
        end if
        if (present(ok)) ok = .true.
    end subroutine one_shot_quantiles

    module procedure median_f64
        real(real64) :: one(1)
        call one_shot_quantiles(values, "pf_median", [0.5_real64], one, is_valid, weights, &
            weight_type, skipnan, method, n_null, n_nan, ok, threads)
        med = one(1)
    end procedure median_f64

    module procedure quantile_f64
        real(real64) :: one(1), pp(1)
        pp(1) = p
        call one_shot_quantiles(values, "pf_quantile", pp, one, is_valid, weights, weight_type, &
            skipnan, method, n_null, n_nan, ok, threads)
        q = one(1)
    end procedure quantile_f64

    module procedure quantiles_f64
        call one_shot_quantiles(values, "pf_quantiles", probs, out, is_valid, weights, &
            weight_type, skipnan, method, n_null, n_nan, ok, threads)
    end procedure quantiles_f64

    module procedure iqr_f64
        real(real64) :: two(2)
        ! Both quartiles from ONE call, so they share one ordering and are computed the same way.
        ! Taking them from two `pf_quantile` calls would traverse the population twice and would
        ! let a future edit give the two quartiles different methods.
        call one_shot_quantiles(values, "pf_iqr", [0.25_real64, 0.75_real64], two, is_valid, &
            weights, weight_type, skipnan, method, n_null, n_nan, ok, threads)
        r = two(2) - two(1)
    end procedure iqr_f64

    module procedure trim_mean_f64
        real(real64), allocatable :: keep(:), keep_w(:)
        real(real64) :: sw, sx
        integer(int64) :: nv, nnull, nnan, cut, lo, hi, i

        ! Note the output dummy is `m`, so the survivor count is `nv` here -- the one place in this
        ! file where the usual name is taken.
        if (.not. (prop >= 0.0_real64 .and. prop < 0.5_real64)) &
            error stop "pf_trim_mean: prop must satisfy 0 <= prop < 0.5; trimming half or more " // &
                "from each tail would leave nothing to average"
        call stats_compact(values, "pf_trim_mean", is_valid, weights, skipnan, keep, keep_w, &
            nv, nnull, nnan)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        m = stats_nan()
        if (present(ok)) ok = .false.
        if (nv == 0_int64) return

        ! Ordering is unavoidable here even at `prop = 0`: WHICH elements the trim removes is a
        ! statement about the order, so there is no selection shortcut as there is for a quantile.
        call order_in_place(keep, keep_w, nv, threads)
        ! `floor(prop * nv)` from EACH end, matching scipy's `trim_mean`, so the result changes in
        ! steps as `prop` grows rather than continuously.
        cut = int(prop * real(nv, real64), int64)
        lo = cut + 1_int64
        hi = nv - cut
        if (hi < lo) return

        sw = 0.0_real64
        sx = 0.0_real64
        if (allocated(keep_w)) then
            do i = lo, hi
                sw = sw + keep_w(i)
                sx = sx + keep_w(i) * keep(i)
            end do
        else
            do i = lo, hi
                sw = sw + 1.0_real64
                sx = sx + keep(i)
            end do
        end if
        if (sw <= 0.0_real64) return
        m = sx / sw
        if (present(ok)) ok = .true.
    end procedure trim_mean_f64

    module procedure percentile_of_score_f64
        real(real64), allocatable :: keep(:), keep_w(:)
        integer(int64) :: m, nnull, nnan
        integer :: code

        if (score /= score .or. abs(score) > huge(0.0_real64)) &
            error stop "pf_percentile_of_score: score must be finite; a NaN or infinite score " // &
                "can only come from the caller's own arithmetic, unlike a NaN in the population"
        call score_kind_code("pf_percentile_of_score", kind, code)
        call stats_compact(values, "pf_percentile_of_score", is_valid, weights, skipnan, keep, &
            keep_w, m, nnull, nnan)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (m == 0_int64) then
            p = stats_nan()
            if (present(ok)) ok = .false.
            return
        end if
        ! No ordering needed: every convention is a weighted count, which one pass answers.
        if (allocated(keep_w)) then
            call score_share(keep, keep_w, m, score, code, p)
        else
            call score_share(keep, m=m, score=score, code=code, res=p)
        end if
        if (present(ok)) ok = (p == p)
    end procedure percentile_of_score_f64

    ! ==================================================================================
    ! Tier B on the object: the cache, and every statistic read off it
    ! ==================================================================================

    !> Ensures the retained buffer is ordered, building tier B if it is not.
    !!
    !! **This is the only place `stats_sort_count` is incremented**, so the counter measures
    !! orderings performed rather than orderings requested -- which is what makes `sorts == 1`
    !! across four different order statistics an assertion about the cache rather than about how
    !! many times a test happened to ask.
    !!
    !! Aborts on a streaming accumulator, because there is nothing to order.
    subroutine ensure_ordered(self, what)
        class(pf_stats), intent(inout) :: self !! the accumulator.
        character(len=*), intent(in) :: what   !! the binding's name, for the message.

        if (.not. self%live) &
            error stop what // ": this accumulator holds no population yet; call %compute or " // &
                "%init first"
        if (.not. self%hold) &
            error stop what // ": this accumulator was created with retain=.false., so no values " // &
                "were kept and there is nothing to order; use retain=.true. (the default for " // &
                "%compute) if you need order statistics"
        ! A deferred tier-A recomputation must land FIRST: it is what makes `keep_n` current, and
        ! ordering a buffer whose live length is still the previous batch's would order the wrong
        ! elements. `stats_ensure` is a no-op when nothing is pending.
        call stats_ensure(self)
        if (self%ordered) return
        call order_in_place(self%keep, self%keep_w, self%keep_n)
        self%ordered = .true.
        stats_sort_count = stats_sort_count + 1_int64
    end subroutine ensure_ordered

    !> One quantile off the object's ordered buffer, shared by every tier-B binding.
    subroutine obj_quantile_at(self, p, method, res)
        class(pf_stats), intent(inout) :: self             !! the accumulator, already ordered.
        real(real64), intent(in) :: p                      !! the probability, already validated.
        character(len=*), intent(in), optional :: method   !! the interpolation rule.
        real(real64), intent(out) :: res                   !! the quantile.
        integer :: code

        call method_code("pf_stats%quantile", method, self%freq .and. self%wtd, code)
        if (self%keep_n == 0_int64) then
            res = stats_nan()
            return
        end if
        if (self%wtd) then
            call quantile_sorted(self%keep(1:self%keep_n), self%keep_w(1:self%keep_n), p, code, res)
        else
            call quantile_sorted(self%keep(1:self%keep_n), p=p, code=code, res=res)
        end if
    end subroutine obj_quantile_at

    module procedure obj_median
        call ensure_ordered(self, "pf_stats%median")
        call obj_quantile_at(self, 0.5_real64, method, res)
    end procedure obj_median

    module procedure obj_quantile
        call check_probs("pf_stats%quantile", [p])
        call ensure_ordered(self, "pf_stats%quantile")
        call obj_quantile_at(self, p, method, res)
    end procedure obj_quantile

    module procedure obj_quantiles
        integer(int64) :: i

        if (size(out, kind=int64) /= size(probs, kind=int64)) &
            error stop "pf_stats%quantiles: out has " // &
                trim(stats_i2s(size(out, kind=int64))) // " elements but probs has " // &
                trim(stats_i2s(size(probs, kind=int64)))
        call check_probs("pf_stats%quantiles", probs)
        call ensure_ordered(self, "pf_stats%quantiles")
        do i = 1_int64, size(probs, kind=int64)
            call obj_quantile_at(self, probs(i), method, out(i))
        end do
    end procedure obj_quantiles

    module procedure obj_iqr
        real(real64) :: q1, q3

        call ensure_ordered(self, "pf_stats%iqr")
        call obj_quantile_at(self, 0.25_real64, method, q1)
        call obj_quantile_at(self, 0.75_real64, method, q3)
        res = q3 - q1
    end procedure obj_iqr

    module procedure obj_trim_mean
        real(real64) :: sw, sx
        integer(int64) :: cut, lo, hi, i

        if (.not. (prop >= 0.0_real64 .and. prop < 0.5_real64)) &
            error stop "pf_stats%trim_mean: prop must satisfy 0 <= prop < 0.5; trimming half or " // &
                "more from each tail would leave nothing to average"
        call ensure_ordered(self, "pf_stats%trim_mean")
        res = stats_nan()
        if (self%keep_n == 0_int64) return
        cut = int(prop * real(self%keep_n, real64), int64)
        lo = cut + 1_int64
        hi = self%keep_n - cut
        if (hi < lo) return
        sw = 0.0_real64
        sx = 0.0_real64
        if (self%wtd) then
            do i = lo, hi
                sw = sw + self%keep_w(i)
                sx = sx + self%keep_w(i) * self%keep(i)
            end do
        else
            do i = lo, hi
                sw = sw + 1.0_real64
                sx = sx + self%keep(i)
            end do
        end if
        if (sw > 0.0_real64) res = sx / sw
    end procedure obj_trim_mean

    module procedure obj_percentile_of_score
        integer :: code

        if (score /= score .or. abs(score) > huge(0.0_real64)) &
            error stop "pf_stats%percentile_of_score: score must be finite; a NaN or infinite " // &
                "score can only come from the caller's own arithmetic"
        call score_kind_code("pf_stats%percentile_of_score", kind, code)
        ! This one needs no ORDER -- every convention is a weighted count -- but it still goes
        ! through `ensure_ordered`, for two reasons: the streaming and uncomputed guards live there
        ! and must apply here too, and a deferred tier-A recomputation must land before `keep_n` is
        ! read. The ordering it may perform is not wasted, since anything else the caller asks of
        ! this object will want it.
        call ensure_ordered(self, "pf_stats%percentile_of_score")
        if (self%keep_n == 0_int64) then
            res = stats_nan()
            return
        end if
        if (self%wtd) then
            call score_share(self%keep, self%keep_w, self%keep_n, score, code, res)
        else
            call score_share(self%keep, m=self%keep_n, score=score, code=code, res=res)
        end if
    end procedure obj_percentile_of_score

    module procedure obj_prepare_order
        call ensure_ordered(self, "pf_stats%prepare_order")
    end procedure obj_prepare_order

    module procedure obj_release_order
        ! The VALUES stay -- they are what makes tier A exact across %update and %merge. Only the
        ! claim that they are ordered is dropped, so the next order statistic sorts again.
        self%ordered = .false.
    end procedure obj_release_order

    module procedure obj_is_ordered
        res = self%ordered
    end procedure obj_is_ordered

    module procedure parquet_debug_stats_sorts
        res = stats_sort_count
    end procedure parquet_debug_stats_sorts

    module procedure parquet_debug_reset_stats_sorts
        stats_sort_count = 0_int64
    end procedure parquet_debug_reset_stats_sorts

    module procedure parquet_debug_set_stats_quantile_sort_min
        dbg_quantile_sort_min = n
    end procedure parquet_debug_set_stats_quantile_sort_min

end submodule parquet_stats_order ! GCOVR_EXCL_LINE
