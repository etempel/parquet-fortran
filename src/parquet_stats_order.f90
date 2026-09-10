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
    !! **One, i.e. never select -- and the reasoning that once said four was wrong about its
    !! premise rather than about its arithmetic.** The premise was that selection is O(n) per probe
    !! against O(n log n) once for a sort, putting the crossover near `log2(n)` probes. It is not:
    !! `pf_nth_element` answers a large array by ORDERING it (`SORT_NTH_ORDER_MIN`,
    !! `src/parquet_argsort_engine.f90`), because that is faster than quickselecting at every size
    !! worth the question. So k probes cost k orderings where a sort costs one, and selecting can
    !! never win for k > 1.
    !!
    !! Measured on machine A (gfortran 15.2) with `bench/benchmark_stats.sh --mode=iqr`, whose two
    !! arms are this procedure with the constant moved either side of the probe count and whose
    !! answers are compared bit for bit before any timing. sort/select, so below 1 means sorting
    !! wins:
    !!
    !! | probes | n = 1 000 | n = 100 000 | n = 1 000 000 |
    !! |---|---|---|---|
    !! | 1 (`pf_median`) | 0.52 | 0.44 | 0.68-0.69 |
    !! | 2 (`pf_iqr`) | 0.29 | 0.20 | 0.27-0.35 |
    !!
    !! **One probe loses too**, which is the part that looks wrong at a glance: `pf_median` on an
    !! even-length population interpolates between TWO order statistics, so its "one probe" is two
    !! selections. There is no probe count at which selecting is the cheaper route.
    !!
    !! Keeping the constant rather than deleting the branch is deliberate: the selection route is
    !! still correct, `should_select` is still where the decision is written down, and the
    !! `parquet_debug_set_stats_quantile_sort_min` override still drives both arms -- which is what
    !! lets `bench/benchmark_stats.sh --mode=iqr` re-ask this question on a machine with a different
    !! core count without a rebuild. See feature_pandas_S4.md, "X-2 is FIXED".
    integer(int64), parameter :: QUANTILE_SORT_MIN = 1_int64

    !> Test-only override for `QUANTILE_SORT_MIN`; negative means the shipped value applies.
    integer(int64), save :: dbg_quantile_sort_min = -1_int64

    !> `1/Phi^-1(3/4)`, where `Phi^-1(3/4) = 0.6744897501960817` is the third quartile of the
    !! standard normal. `pf_mad`'s "normal" scale MULTIPLIES by this, which is what makes the median
    !! absolute deviation a consistent estimator of the standard deviation for Gaussian data.
    !!
    !! **Full precision, not the rounded 1.4826 every textbook quotes.** The two differ by 1.5e-06
    !! relative, which is four million times the double-precision noise floor and showed up
    !! immediately as a cross-check failure against `scipy.stats.median_abs_deviation`. Matching
    !! scipy is the whole reason this token is called "normal".
    !!
    !! **SEVENTEEN digits, because sixteen do not name this double.** The literal is the correctly
    !! rounded double nearest the exact `1/Phi^-1(3/4)` = 1.48260221850560186054707652936...,
    !! which is `0x3ff7b8bd1a975673`. It is deliberately NOT `1.0_real64 /
    !! 0.6744897501960817_real64`: rounding the quartile to a double and then taking its reciprocal
    !! in double rounds twice and lands on `...674`, one ulp high. That double, spelled
    !! `1.482602218505602`, was this constant's value up to and including v2.3.0. Against the exact
    !! quotient the correction takes the worst relative error of `raw * MAD_NORMAL_SCALE` from
    !! 2.3e-16 down to 1.4e-16 -- past the 1.7e-16 that scipy's own `mad / 0.6744897501960817`
    !! reaches. **Do not respell this with sixteen digits**; that is a different double, and
    !! `tools/generate_probit_reference.py --self-test` fails the moment the two diverge.
    !!
    !! **One constant, applied ONE way, and that is the point of it being a multiplier.** Both
    !! scalings in this file go through it: `resolve_mad_scale`, behind `pf_mad`/`%mad`, and
    !! `slice_mad_std`, behind `pf_sigma_clipped_stats(stdfunc="mad_std")`. One multiplying by a
    !! reciprocal while the other divides by `Phi^-1(3/4)` is NOT the same operation in binary
    !! floating point -- measured, the two disagree by one ulp on about 58% of inputs -- so the
    !! same population could give the two paths different answers with nothing saying which was
    !! meant. **Do not reintroduce a division by `Phi^-1(3/4)` in either path.** scipy is the one
    !! that divides, so a "normal" answer here sits within one ulp of scipy's rather than on it;
    !! that ulp is the price of the two paths in this file agreeing bit for bit with each other.
    real(real64), parameter :: MAD_NORMAL_SCALE = 1.4826022185056018_real64

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
            ! gcov attribution artifact: this line reports a positive hit count while never
            ! executing -- gcov charges the `select case` dispatch to the `case default` label.
            ! The proof it does not run is that the suite passes: an `error stop` that executed
            ! would have killed the process.
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
        ! **The counter lives HERE, at the one choke point every ordering in this submodule passes
        ! through** -- not in `ensure_ordered`, which is only the object's route to it. Counting at
        ! the choke point is what lets a ONE-SHOT procedure's cost be asserted: without it,
        ! `pf_sigma_clipped_stats` could sort once per round and no test could tell, because every
        ! number it returns would be identical.
        stats_sort_count = stats_sort_count + 1_int64
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
    !! **Answers `.false.` for every probe count as shipped**, because `QUANTILE_SORT_MIN` is 1 --
    !! see its own comment for the measurement. The decision is still written here, and still
    !! overridable, rather than the branch being deleted: the selection route is correct, and the
    !! override is how the question gets re-asked on a machine with a different core count.
    !! Weights force the ordering whatever the count in any case: the weighted position rule needs
    !! the cumulative weight in sorted order, which selection does not produce.
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
        logical :: freq, poisoned

        if (size(out, kind=int64) /= size(probs, kind=int64)) &
            error stop what // ": out has " // trim(stats_i2s(size(out, kind=int64))) // &
                " elements but probs has " // trim(stats_i2s(size(probs, kind=int64)))
        call check_probs(what, probs)
        call stats_weight_kind(what, weight_type, freq)
        call method_code(what, method, freq .and. present(weights), code)
        call stats_compact(values, what, is_valid, weights, skipnan, keep, keep_w, m, nnull, &
            nnan, poisoned)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan

        k = size(probs, kind=int64)
        ! `skipnan = .false.` and a NaN is in the population: the caller asked for propagation and
        ! every answer is NaN, exactly as the moments give. This has to be TESTED rather than
        ! inherited -- a NaN sorts to one end of the buffer instead of poisoning the interpolation,
        ! so without this an ordinary-looking median comes back from a population the caller asked
        ! to have poisoned. Grouped with the empty case because the answer is the same one.
        if (m == 0_int64 .or. poisoned) then
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
        logical :: poisoned

        ! Note the output dummy is `m`, so the survivor count is `nv` here -- the one place in this
        ! file where the usual name is taken.
        if (.not. (prop >= 0.0_real64 .and. prop < 0.5_real64)) &
            error stop "pf_trim_mean: prop must satisfy 0 <= prop < 0.5; trimming half or more " // &
                "from each tail would leave nothing to average"
        call stats_compact(values, "pf_trim_mean", is_valid, weights, skipnan, keep, keep_w, &
            nv, nnull, nnan, poisoned)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        m = stats_nan()
        if (present(ok)) ok = .false.
        ! A surviving NaN under `skipnan = .false.` poisons this as it poisons every other answer
        ! -- and here it would otherwise be TRIMMED AWAY, since a NaN sorts to one end, which is
        ! the most misleading form the defect can take.
        if (nv == 0_int64 .or. poisoned) return

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
        logical :: poisoned

        if (score /= score .or. abs(score) > huge(0.0_real64)) &
            error stop "pf_percentile_of_score: score must be finite; a NaN or infinite score " // &
                "can only come from the caller's own arithmetic, unlike a NaN in the population"
        call score_kind_code("pf_percentile_of_score", kind, code)
        call stats_compact(values, "pf_percentile_of_score", is_valid, weights, skipnan, keep, &
            keep_w, m, nnull, nnan, poisoned)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        ! As above: a NaN in the population under `skipnan = .false.` makes every answer NaN, and
        ! here it would otherwise merely fail every comparison and quietly shrink the denominator.
        if (m == 0_int64 .or. poisoned) then
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
    ! Tier C: the median absolute deviation
    ! ==================================================================================

    !> Resolves a `scale=` token to the factor the raw deviation is multiplied by.
    subroutine scale_factor(what, scale, factor)
        character(len=*), intent(in) :: what               !! the public procedure's name.
        character(len=*), intent(in), optional :: scale    !! the caller's token, if any.
        real(real64), intent(out) :: factor                !! 1.4826 or 1.
        character(len=16) :: tok
        integer :: i, c

        ! Set ONCE, before the token is even looked at, so the default and an explicit
        ! `scale="normal"` cannot come to differ -- which is exactly what happened when the two
        ! carried their own copies of the constant: a mutation to one of them survived the whole
        ! suite, because no test passed the token explicitly.
        factor = MAD_NORMAL_SCALE
        if (.not. present(scale)) return
        tok = ""
        do i = 1, min(len(scale), len(tok))
            c = iachar(scale(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            tok(i:i) = achar(c)
        end do
        select case (trim(tok))
        case ("normal")
            continue      ! the default, already set above
        case ("raw")
            factor = 1.0_real64
        case default
            error stop what // ': unrecognised scale "' // trim(scale) // &
                '"; the tokens are "normal" (the default, x1.4826) and "raw"'
        end select
    end subroutine scale_factor

    !> Aborts unless a caller-supplied centre is a finite number.
    subroutine check_center(what, center)
        character(len=*), intent(in) :: what            !! the public procedure's name.
        real(real64), intent(in), optional :: center    !! the caller's centre, if any.

        if (.not. present(center)) return
        ! `x /= x` rather than `ieee_is_nan`, per this module's standing rule -- and tested before
        ! `abs`, which is quiet on a NaN but whose RESULT would then compare false against `huge`
        ! and let the NaN through.
        if (center /= center .or. abs(center) > huge(0.0_real64)) &
            error stop what // ": center must be finite; a NaN or infinite centre can only come " // &
                "from the caller's own arithmetic, unlike a NaN in the population"
    end subroutine check_center

    !> The UNSCALED median absolute deviation of `x(1:m)` about `c`.
    !!
    !! The deviations are a derived population: `|x - c|` is not monotone in `x`, so tier B's
    !! ordering of the values says nothing about the order of the deviations and they have to be
    !! ordered (or selected over) in their own right. That is why supplying `center` skips one
    !! selection rather than the whole traversal.
    subroutine raw_mad(x, w, m, c, threads, res)
        real(real64), intent(in) :: x(:)                  !! the survivors, in any order.
        real(real64), allocatable, intent(in) :: w(:)     !! their weights, or unallocated.
        integer(int64), intent(in) :: m                   !! how many are live.
        real(real64), intent(in) :: c                     !! the centre.
        integer, intent(in), optional :: threads          !! passed to the sort or the selection.
        real(real64), intent(out) :: res                  !! the raw deviation.
        real(real64), allocatable :: dev(:), devw(:)
        integer(int64) :: i

        allocate(dev(m))
        do i = 1_int64, m
            dev(i) = abs(x(i) - c)
        end do
        if (allocated(w)) then
            ! A weight belongs to its ELEMENT, so it travels with that element's deviation.
            allocate(devw(m))
            do i = 1_int64, m
                devw(i) = w(i)
            end do
            call order_in_place(dev, devw, m, threads)
            call quantile_sorted(dev(1:m), devw(1:m), 0.5_real64, QM_LINEAR, res)
        else
            call quantile_by_selection(dev, m, 0.5_real64, QM_LINEAR, threads, res)
        end if
    end subroutine raw_mad

    module procedure mad_f64
        real(real64), allocatable :: keep(:), keep_w(:)
        real(real64) :: factor, c, raw
        integer(int64) :: nv, nnull, nnan
        logical :: poisoned

        call scale_factor("pf_mad", scale, factor)
        call check_center("pf_mad", center)
        call stats_compact(values, "pf_mad", is_valid, weights, skipnan, keep, keep_w, &
            nv, nnull, nnan, poisoned)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        m = stats_nan()
        if (present(ok)) ok = .false.
        if (nv == 0_int64 .or. poisoned) return

        if (present(center)) then
            c = center
        else
            ! The population's own median, which is what scipy's `median_abs_deviation` uses.
            ! Selected rather than sorted when unweighted: this is one probe, and the deviations
            ! that follow are a different population anyway, so an ordering here buys nothing.
            if (allocated(keep_w)) then
                call order_in_place(keep, keep_w, nv, threads)
                call quantile_sorted(keep(1:nv), keep_w(1:nv), 0.5_real64, QM_LINEAR, c)
            else
                call quantile_by_selection(keep, nv, 0.5_real64, QM_LINEAR, threads, c)
            end if
        end if
        call raw_mad(keep, keep_w, nv, c, threads, raw)
        m = factor * raw
        if (present(ok)) ok = (m == m)
    end procedure mad_f64

    module procedure describe_f64
        ! `%compute` then the ordering, which is the whole of it -- and the ordering goes through
        ! `ensure_ordered` rather than through `s%prepare_order()`, because nagfor binds a separate
        ! module procedure's name to an external at the first call site and would then reject the
        ! `module procedure obj_prepare_order` further down this same file (CLAUDE.md). The
        ! contained procedure is reached by host association and has no such rule.
        call s%compute(values, is_valid=is_valid, weights=weights, weight_type=weight_type, &
            skipnan=skipnan, threads=threads)
        call ensure_ordered(s, "pf_describe")
        ! The mean is the whole test, and deliberately: it is NaN in exactly the two cases that
        ! leave every tier-A query on the object NaN -- an empty population, and a NaN kept under
        ! `skipnan = .false.` An infinite mean is not one of them, so `ok` stays .true. there,
        ! consistently with `pf_mean` over the same data.
        if (present(ok)) ok = (s%mean() == s%mean())
    end procedure describe_f64


    ! ==================================================================================
    ! pf_sigma_clipped_stats -- astropy's iterative clip, on ONE ordering
    !
    ! The keep condition is an INTERVAL, so the survivors of every round are a contiguous
    ! sub-range of the sorted order and each later round only moves two indices inward. That is
    ! what turns an O(k n log n) routine into O(n log n + k n), and it is why the ordering happens
    ! once, above the loop, rather than inside it.
    ! ==================================================================================

    !> Resolves `cenfunc=`/`stdfunc=` tokens, or aborts naming the pair that was expected.
    subroutine clip_token(what, arg, given, a, b, second)
        character(len=*), intent(in) :: what             !! the public procedure's name.
        character(len=*), intent(in) :: arg              !! which argument, for the message.
        character(len=*), intent(in), optional :: given  !! the caller's token, if any.
        character(len=*), intent(in) :: a                !! the default token.
        character(len=*), intent(in) :: b                !! the alternative.
        logical, intent(out) :: second                   !! .true. when the caller asked for `b`.
        character(len=16) :: tok
        integer :: i, c

        second = .false.
        if (.not. present(given)) return
        tok = ""
        do i = 1, min(len(given), len(tok))
            c = iachar(given(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            tok(i:i) = achar(c)
        end do
        if (trim(tok) == a) return
        if (trim(tok) == b) then
            second = .true.
            return
        end if
        error stop what // ": unrecognised " // arg // ' "' // trim(given) // &
            '"; the tokens are "' // a // '" (the default) and "' // b // '"'
    end subroutine clip_token

    !> The mean and POPULATION standard deviation of one contiguous slice of the sorted buffer.
    !!
    !! **`ddof = 0`, not this module's usual 1, and that is a deliberate match rather than an
    !! oversight.** astropy's `sigma_clipped_stats` reaches `np.nanstd` for both the per-round
    !! scale and the standard deviation it reports, and `np.nanstd` is `ddof = 0`. Using the
    !! sample form here instead would put the reported spread 0.13% out on a 400-element
    !! population and would move the clip boundaries as well -- a difference with nothing at the
    !! call site to explain it, for a procedure whose whole purpose is to reproduce astropy's.
    pure subroutine slice_mean_sd(xs, lo, hi, mean, sd)
        real(real64), intent(in) :: xs(:)      !! the sorted survivors.
        integer(int64), intent(in) :: lo       !! first index of the slice.
        integer(int64), intent(in) :: hi       !! last index of the slice.
        real(real64), intent(out) :: mean      !! the slice's mean.
        real(real64), intent(out) :: sd        !! its standard deviation at ddof=0, or NaN.
        real(real64) :: s, d
        integer(int64) :: i, n
        logical :: pinf, ninf

        n = hi - lo + 1_int64
        mean = stats_nan()
        sd = stats_nan()
        if (n <= 0_int64) return
        ! **An infinity in the slice is answered from the ENDS, never summed over.** `xs` is
        ! sorted, so its two extremes settle in O(1) whether the slice holds an infinity and of
        ! which sign, and the answers below are exactly what the arithmetic would have produced:
        ! one sign sums to that infinity, both signs to a NaN, and every deviation from an infinite
        ! mean is either infinite or `Inf - Inf`, so the standard deviation is a NaN either way
        ! (the non-finite table in doc/pages/utilities/statistics.md).
        !
        ! Screening rather than computing is what keeps `Inf + (-Inf)` and `Inf - Inf` from being
        ! EXECUTED. Both raise IEEE_INVALID, and nagfor unmasks the IEEE traps by default
        ! (`-ieee=stop`), so reaching either kills the process -- on data this module documents
        ! itself as answering rather than refusing. Two comparisons for the whole slice, so the
        ! screen is free where a per-element test would not be.
        pinf = (xs(hi) > huge(0.0_real64))
        ninf = (xs(lo) < -huge(0.0_real64))
        if (pinf .or. ninf) then
            if (pinf .and. ninf) return
            if (pinf) mean = xs(hi)
            if (ninf) mean = xs(lo)
            return
        end if
        s = 0.0_real64
        do i = lo, hi
            s = s + xs(i)
        end do
        mean = s / real(n, real64)
        ! Two-pass, as everywhere else in this module: the mean first, the central moment against
        ! it. The slice is contiguous and typically small by the time this matters, so the fixed
        ! block tree the whole-population engine uses would buy nothing here.
        s = 0.0_real64
        do i = lo, hi
            d = xs(i) - mean
            s = s + d * d
        end do
        sd = sqrt(s / real(n, real64))
    end subroutine slice_mean_sd

    !> The median of one contiguous slice of an already-sorted buffer, interpolating.
    pure subroutine slice_median(xs, lo, hi, res)
        real(real64), intent(in) :: xs(:)      !! the sorted survivors.
        integer(int64), intent(in) :: lo       !! first index of the slice.
        integer(int64), intent(in) :: hi       !! last index of the slice.
        real(real64), intent(out) :: res       !! the median, or NaN for an empty slice.
        integer(int64) :: n, k

        n = hi - lo + 1_int64
        if (n <= 0_int64) then
            res = stats_nan()
            return
        end if
        k = lo + (n - 1_int64) / 2_int64
        if (mod(n, 2_int64) == 1_int64) then
            res = xs(k)
        else
            ! The mean of the two middle values -- `linear` interpolation at p = 0.5, which is what
            ! `pf_median` gives and what numpy, pandas and astropy all mean by "median".
            res = 0.5_real64 * (xs(k) + xs(k + 1_int64))
        end if
    end subroutine slice_median

    !> The normal-scaled median absolute deviation of one sorted slice, about its own median.
    subroutine slice_mad_std(xs, lo, hi, res)
        real(real64), intent(in) :: xs(:)      !! the sorted survivors.
        integer(int64), intent(in) :: lo       !! first index of the slice.
        integer(int64), intent(in) :: hi       !! last index of the slice.
        real(real64), intent(out) :: res       !! the scaled deviation, or NaN.
        real(real64), allocatable :: dev(:), nw(:)
        real(real64) :: c
        integer(int64) :: n, i

        n = hi - lo + 1_int64
        res = stats_nan()
        if (n <= 0_int64) return
        call slice_median(xs, lo, hi, c)
        ! The deviations are a DIFFERENT population from the values: |x - c| is not monotone in x,
        ! so the slice's own order says nothing about theirs and they have to be ordered again.
        ! That is the one part of a `stdfunc="mad_std"` round that is not O(n).
        allocate(dev(n))
        do i = 1_int64, n
            dev(i) = abs(xs(lo + i - 1_int64) - c)
        end do
        call order_in_place(dev, nw, n)
        call slice_median(dev, 1_int64, n, res)
        ! MULTIPLIED, not divided: the same expression `resolve_mad_scale` applies, so a
        ! `stdfunc="mad_std"` scale and a `pf_mad(scale="normal")` over the same population agree
        ! bit for bit. See MAD_NORMAL_SCALE's own comment.
        res = res * MAD_NORMAL_SCALE
    end subroutine slice_mad_std

    module procedure sigma_clipped_stats_f64
        real(real64), allocatable :: keep_x(:), keep_w(:)
        real(real64) :: centre, scale, slo, shi, blo, bhi, junk
        integer(int64) :: m, nnull, nnan, lo, hi, prev_lo, prev_hi, i, rounds, cap
        integer :: iters
        logical :: poisoned, use_mean, use_mad, done, bad, no_spread

        ! There is no `weights` dummy at all, which is stronger than a runtime refusal: a weighted
        ! scale estimator is a further definitional choice that no workload in evidence asks for,
        ! and its ABSENCE from the signature makes passing one a compile error rather than an
        ! abort. Same reasoning as `pf_mode`'s missing real kinds.
        call clip_token("pf_sigma_clipped_stats", "cenfunc", cenfunc, "median", "mean", use_mean)
        call clip_token("pf_sigma_clipped_stats", "stdfunc", stdfunc, "std", "mad_std", use_mad)
        slo = 3.0_real64
        shi = 3.0_real64
        if (present(sigma)) then
            slo = sigma
            shi = sigma
        end if
        if (present(sigma_lower)) slo = sigma_lower
        if (present(sigma_upper)) shi = sigma_upper
        ! Two statements, not one expression: Fortran does not short-circuit, so a combined test
        ! would evaluate `slo < 0` on a NaN and raise IEEE_INVALID -- which under nagfor's default
        ! `-ieee=stop` kills the process instead of issuing the abort this line exists to issue.
        ! Only `==` and `/=` are quiet on a NaN.
        bad = (slo /= slo) .or. (shi /= shi)
        if (.not. bad) bad = (slo < 0.0_real64) .or. (shi < 0.0_real64)
        if (bad) &
            error stop "pf_sigma_clipped_stats: sigma, sigma_lower and sigma_upper must be " // &
                "non-negative numbers; a NaN or negative clip width can only be a caller mistake"
        iters = 5
        if (present(maxiters)) iters = maxiters

        mean = stats_nan()
        median = stats_nan()
        stddev = stats_nan()
        if (present(n_clipped)) n_clipped = 0_int64
        if (present(converged)) converged = .true.
        if (present(ok)) ok = .false.
        if (present(keep)) then
            if (size(keep, kind=int64) /= size(values, kind=int64)) &
                error stop "pf_sigma_clipped_stats: keep has " // &
                    trim(stats_i2s(size(keep, kind=int64))) // " elements but values has " // &
                    trim(stats_i2s(size(values, kind=int64)))
            keep = .false.
        end if

        call stats_compact(values, "pf_sigma_clipped_stats", is_valid, skipnan=skipnan, &
            keep_x=keep_x, keep_w=keep_w, n_valid=m, n_null=nnull, n_nan=nnan, saw_nan=poisoned)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (m == 0_int64 .or. poisoned) return

        ! THE one ordering. Everything below indexes a contiguous sub-range of it.
        call order_in_place(keep_x, keep_w, m, threads)
        lo = 1_int64
        hi = m
        blo = keep_x(1)
        bhi = keep_x(m)

        ! `maxiters <= 0` means "until nothing more is removed", capped at 100 -- and reaching the
        ! cap REPORTS rather than aborting. Each non-terminating round removes at least one point,
        ! so the honest bound is `m` rather than a small constant, and aborting there would break
        ! this module's own rule that it never aborts on a data condition.
        cap = 100_int64
        if (iters > 0) cap = int(iters, int64)
        rounds = 0_int64
        done = .false.
        do while (rounds < cap)
            rounds = rounds + 1_int64
            if (use_mean) then
                call slice_mean_sd(keep_x, lo, hi, centre, scale)
            else
                call slice_median(keep_x, lo, hi, centre)
                ! `junk` rather than passing `scale` twice: one variable given to two dummies that
                ! can both be defined is illegal (F2018 15.5.2.13) and nothing diagnoses it.
                call slice_mean_sd(keep_x, lo, hi, junk, scale)
            end if
            if (use_mad) call slice_mad_std(keep_x, lo, hi, scale)
            ! The NaN test stands alone, and ahead of the `>`: `scale` is a NaN whenever the
            ! slice holds an infinity (`slice_mean_sd` answers one there by construction), and
            ! `NaN > 0` raises IEEE_INVALID -- a dead process under nagfor's default `-ieee=stop`,
            ! on the very population this procedure documents itself as reporting `ok = .false.`
            ! for. The verdict is unchanged: `.not. (NaN > 0)` was already `.true.`.
            no_spread = (scale /= scale)
            if (.not. no_spread) no_spread = .not. (scale > 0.0_real64)
            if (no_spread) then
                ! No spread left to clip against -- a constant slice, or one too short for a
                ! standard deviation. Stopping is the answer: every remaining point is at the
                ! centre, so no interval could remove one.
                done = .true.
                exit
            end if
            blo = centre - slo * scale
            bhi = centre + shi * scale
            prev_lo = lo
            prev_hi = hi
            ! Inclusive at both ends, matching astropy's rejection test. The survivors are a
            ! contiguous range because the test is on VALUE and the buffer is sorted.
            do while (lo <= hi)
                if (keep_x(lo) >= blo) exit
                lo = lo + 1_int64
            end do
            do while (hi >= lo)
                if (keep_x(hi) <= bhi) exit
                hi = hi - 1_int64
            end do
            if (hi - lo + 1_int64 < 2_int64) then
                ! A round that would leave fewer than two values stops instead, and the PREVIOUS
                ! round's survivors are what is reported -- a one-element or empty set has no
                ! scale, so continuing could only produce NaNs.
                lo = prev_lo
                hi = prev_hi
                done = .true.
                exit
            end if
            if (lo == prev_lo .and. hi == prev_hi) then
                done = .true.
                exit
            end if
        end do
        if (present(converged)) converged = done

        call slice_mean_sd(keep_x, lo, hi, mean, stddev)
        call slice_median(keep_x, lo, hi, median)
        if (present(n_clipped)) n_clipped = m - (hi - lo + 1_int64)
        if (present(keep)) then
            ! Recovered from the surviving VALUE interval rather than by carrying indices through
            ! the sort, and that is exact rather than nearly so: the keep test is on value, so
            ! equal values are always kept or clipped together, which makes `keep_x(lo-1)`
            ! strictly less than `keep_x(lo)` whenever `lo > 1`. There is therefore no value that
            ! sits inside the interval and was nonetheless clipped.
            blo = keep_x(lo)
            bhi = keep_x(hi)
            do i = 1_int64, size(values, kind=int64)
                if (present(is_valid)) then
                    if (.not. is_valid(i)) cycle
                end if
                if (values(i) /= values(i)) cycle
                keep(i) = (values(i) >= blo .and. values(i) <= bhi)
            end do
        end if
        ! `ok` reports whether the ANSWER is usable, which is what a caller needs; `converged`
        ! reports how the iteration ended, and the two are deliberately independent.
        !
        ! **All THREE outputs are tested, not just the mean.** This is the family's only procedure
        ! answering more than one number, and they do not fail together: a population holding
        ! `+Inf` clips to a finite median and an infinite mean, whose deviations are then all
        ! infinite, so the standard deviation is a NaN while the mean is not. Testing the mean
        ! alone would ship a NaN under `ok = .true.`, which is exactly what `ok` exists to prevent.
        if (present(ok)) ok = (mean == mean) .and. (median == median) .and. (stddev == stddev)
    end procedure sigma_clipped_stats_f64

    ! ==================================================================================
    ! pf_mode -- the most common value
    !
    ! Every specific is the same three steps: survive the exclusions, order the survivors, then
    ! pick the heaviest RUN of equal values. The steps are shared and only the comparison and the
    ! result type differ per kind, which is what keeps five specifics from drifting on the tie
    ! rule -- the one thing about a mode that is easy to get subtly, silently wrong.
    ! ==================================================================================

    !> The indices of the elements that survive `pf_mode`'s exclusions, and their weights.
    !!
    !! Indices rather than values, because the values have five different types and the exclusion
    !! rules have none: nullness and weight decide this and the value never does. No NaN clause,
    !! because no kind `pf_mode` accepts can hold one.
    subroutine mode_survivors(n, what, is_valid, weights, idx, w, m, n_null)
        integer(int64), intent(in) :: n                     !! how many elements were offered.
        character(len=*), intent(in) :: what                !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)        !! per element: .false. marks a null.
        real(real64), intent(in), optional :: weights(:)    !! per element weight.
        integer(int64), allocatable, intent(out) :: idx(:)  !! surviving indices, ascending.
        real(real64), allocatable, intent(out) :: w(:)
        !! the surviving elements' weights, indexed the same way as `idx` -- allocated only when
        !! `weights` was supplied, which is the convention the rest of this module uses to mean
        !! "unweighted" rather than carrying a separate flag.
        integer(int64), intent(out) :: m                    !! how many survived.
        integer(int64), intent(out) :: n_null               !! how many `is_valid` excluded.
        integer(int64) :: i, wbits

        call stats_check_sizes(n, what, is_valid, weights)
        allocate(idx(n))
        if (present(weights)) allocate(w(n))
        m = 0_int64
        n_null = 0_int64
        do i = 1_int64, n
            if (present(is_valid)) then
                if (.not. is_valid(i)) then
                    n_null = n_null + 1_int64
                    cycle
                end if
            end if
            ! Nullness first, then weight -- the family's exclusion ORDER, so a weight column that
            ! is garbage exactly where the value column is null costs nothing and aborts nothing.
            if (present(weights)) then
                ! One integer compare on the happy path; see STATS_W_LIM. The validator is
                ! reached only by a weight that cannot be valid.
                wbits = transfer(weights(i), 0_int64)
                if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then
                    call stats_check_weight(weights(i), i, what)
                    if (weights(i) <= 0.0_real64) cycle
                else if (wbits == 0_int64) then
                    cycle
                end if
            end if
            m = m + 1_int64
            idx(m) = i
            if (present(weights)) w(m) = weights(i)
        end do
    end subroutine mode_survivors

    !> Picks the winning run from a sorted population's run-start flags.
    !!
    !! **The tie rule lives here and nowhere else.** Runs are visited in ascending value order and
    !! the comparison is a strict `>`, so the FIRST run to reach the maximum keeps it -- which is
    !! the smallest tied value, matching `scipy.stats.mode`. A `>=` would silently hand ties to the
    !! largest value instead, and every count in every test would still be right.
    !!
    !! `tied` reports EVERY run that reaches the maximum, which is what pandas' `Series.mode()`
    !! returns, and it is derived in a second pass rather than accumulated in the first: the
    !! maximum is not known until the first pass ends, so a single pass would have to keep and
    !! discard candidate lists. The second pass costs one more walk of the run structure only
    !! when a caller asks for it, and it cannot disagree with `at` -- `tied(1)` IS `at`, because
    !! both take the first run reaching the same `best_w`.
    subroutine best_run(starts, order, w, m, at, cnt, tied)
        logical, intent(in) :: starts(:)
        !! `starts(i)`: sorted position `i` holds a different value from position `i-1`.
        integer(int64), intent(in) :: order(:)          !! the ascending permutation.
        real(real64), allocatable, intent(in) :: w(:)   !! weights indexed as `order` is, or none.
        integer(int64), intent(in) :: m                 !! how many survivors there are.
        integer(int64), intent(out) :: at               !! sorted position of the winner's first element.
        integer(int64), intent(out) :: cnt              !! how many elements the winner holds.
        integer(int64), allocatable, intent(out), optional :: tied(:)
        !! sorted position of the first element of EVERY run reaching the maximum, ascending.
        !! Allocated to size 0 for an empty population, never left unallocated.
        integer(int64) :: i, run_lo, run_n, ntied
        real(real64) :: run_w, best_w

        at = 1_int64
        cnt = 0_int64
        best_w = -1.0_real64
        run_lo = 1_int64
        run_n = 0_int64
        run_w = 0.0_real64
        do i = 1_int64, m
            if (i > 1_int64) then
                if (starts(i)) then
                    if (run_w > best_w) then
                        best_w = run_w
                        at = run_lo
                        cnt = run_n
                    end if
                    run_lo = i
                    run_n = 0_int64
                    run_w = 0.0_real64
                end if
            end if
            run_n = run_n + 1_int64
            if (allocated(w)) then
                run_w = run_w + w(i)
            else
                run_w = run_w + 1.0_real64
            end if
        end do
        if (run_w > best_w) then
            best_w = run_w
            at = run_lo
            cnt = run_n
        end if
        if (.not. present(tied)) return

        ! Pass two: every run whose total equals the maximum. The equality is exact and has to be:
        ! these are sums of the SAME weights in the same order, so two runs that tie produce
        ! bit-identical totals, and a tolerance here would merge runs that are genuinely different.
        ! An empty population leaves a zero-length array rather than an unallocated one, so a
        ! caller's `size(modes)` is always defined.
        ntied = 0_int64
        run_n = 0_int64
        run_w = 0.0_real64
        do i = 1_int64, m
            if (i > 1_int64) then
                if (starts(i)) then
                    if (run_w == best_w) ntied = ntied + 1_int64
                    run_n = 0_int64
                    run_w = 0.0_real64
                end if
            end if
            run_n = run_n + 1_int64
            if (allocated(w)) then
                run_w = run_w + w(i)
            else
                run_w = run_w + 1.0_real64
            end if
        end do
        if (m > 0_int64) then
            if (run_w == best_w) ntied = ntied + 1_int64
        end if
        allocate(tied(ntied))
        ntied = 0_int64
        run_lo = 1_int64
        run_n = 0_int64
        run_w = 0.0_real64
        do i = 1_int64, m
            if (i > 1_int64) then
                if (starts(i)) then
                    if (run_w == best_w) then
                        ntied = ntied + 1_int64
                        tied(ntied) = run_lo
                    end if
                    run_lo = i
                    run_n = 0_int64
                    run_w = 0.0_real64
                end if
            end if
            run_n = run_n + 1_int64
            if (allocated(w)) then
                run_w = run_w + w(i)
            else
                run_w = run_w + 1.0_real64
            end if
        end do
        if (m > 0_int64) then
            if (run_w == best_w) then
                ntied = ntied + 1_int64
                tied(ntied) = run_lo
            end if
        end if
    end subroutine best_run

    !> Reports `count`, `n_null` and `ok` for a `pf_mode` specific, so five bodies cannot disagree.
    pure subroutine mode_report(cnt, nnull, count, n_null, ok)
        integer(int64), intent(in) :: cnt                  !! how many elements the winner holds.
        integer(int64), intent(in) :: nnull                !! how many were null.
        integer(int64), intent(out), optional :: count     !! the caller's count, if wanted.
        integer(int64), intent(out), optional :: n_null    !! the caller's null count, if wanted.
        logical, intent(out), optional :: ok                !! .false. for an empty population.

        if (present(count)) count = cnt
        if (present(n_null)) n_null = nnull
        if (present(ok)) ok = (cnt > 0_int64)
    end subroutine mode_report

    module procedure mode_i32
        integer(int32), allocatable :: kept(:)
        integer(int64), allocatable :: idx(:), perm(:)
        real(real64), allocatable :: w(:), sw(:)
        logical, allocatable :: starts(:)
        integer(int64), allocatable :: tied(:)
        integer(int64) :: nv, nnull, i, at, cnt

        call mode_survivors(size(values, kind=int64), "pf_mode", is_valid, weights, idx, w, &
            nv, nnull)
        if (nv == 0_int64) then
            if (present(modes)) allocate(modes(0))
            call mode_report(0_int64, nnull, count, n_null, ok)
            return
        end if
        kept = values(idx(1:nv))
        call pf_argsort(kept, perm)
        allocate(starts(nv))
        starts(1) = .true.
        do i = 2_int64, nv
            starts(i) = (kept(perm(i)) /= kept(perm(i - 1_int64)))
        end do
        ! `w` is indexed by SURVIVOR, and `perm` permutes survivors, so the weight of sorted
        ! position i is `w(perm(i))` -- which is what the reindexed copy below hands `best_run`.
        ! `w` is indexed by SURVIVOR and `perm` permutes survivors, so sorted position i carries
        ! weight `w(perm(i))`. Reindexed into its own array rather than gathered onto itself: an
        ! array-section assignment whose two sides are the same array is a heap temporary the
        ! compiler cannot elide (CLAUDE.md), and a weight that lost its value would be a silent
        ! wrong answer with nothing to report it.
        if (allocated(w)) then
            allocate(sw(nv))
            do i = 1_int64, nv
                sw(i) = w(perm(i))
            end do
        end if
        ! `tied` is asked for only when the caller wants every mode, so the second walk of the
        ! run structure is never paid by a caller who does not. `tied(1)` is `at` by construction,
        ! which is what keeps `m` and `modes(1)` from ever disagreeing.
        if (present(modes)) then
            call best_run(starts, perm, sw, nv, at, cnt, tied)
            allocate(modes(size(tied, kind=int64)))
            do i = 1_int64, size(tied, kind=int64)
                modes(i) = kept(perm(tied(i)))
            end do
        else
            call best_run(starts, perm, sw, nv, at, cnt)
        end if
        m = kept(perm(at))
        call mode_report(cnt, nnull, count, n_null, ok)
    end procedure mode_i32

    module procedure mode_i64
        integer(int64), allocatable :: kept(:)
        integer(int64), allocatable :: idx(:), perm(:)
        real(real64), allocatable :: w(:), sw(:)
        logical, allocatable :: starts(:)
        integer(int64), allocatable :: tied(:)
        integer(int64) :: nv, nnull, i, at, cnt

        call mode_survivors(size(values, kind=int64), "pf_mode", is_valid, weights, idx, w, &
            nv, nnull)
        if (nv == 0_int64) then
            if (present(modes)) allocate(modes(0))
            call mode_report(0_int64, nnull, count, n_null, ok)
            return
        end if
        kept = values(idx(1:nv))
        call pf_argsort(kept, perm)
        allocate(starts(nv))
        starts(1) = .true.
        do i = 2_int64, nv
            starts(i) = (kept(perm(i)) /= kept(perm(i - 1_int64)))
        end do
        ! `w` is indexed by SURVIVOR and `perm` permutes survivors, so sorted position i carries
        ! weight `w(perm(i))`. Reindexed into its own array rather than gathered onto itself: an
        ! array-section assignment whose two sides are the same array is a heap temporary the
        ! compiler cannot elide (CLAUDE.md), and a weight that lost its value would be a silent
        ! wrong answer with nothing to report it.
        if (allocated(w)) then
            allocate(sw(nv))
            do i = 1_int64, nv
                sw(i) = w(perm(i))
            end do
        end if
        ! `tied` is asked for only when the caller wants every mode, so the second walk of the
        ! run structure is never paid by a caller who does not. `tied(1)` is `at` by construction,
        ! which is what keeps `m` and `modes(1)` from ever disagreeing.
        if (present(modes)) then
            call best_run(starts, perm, sw, nv, at, cnt, tied)
            allocate(modes(size(tied, kind=int64)))
            do i = 1_int64, size(tied, kind=int64)
                modes(i) = kept(perm(tied(i)))
            end do
        else
            call best_run(starts, perm, sw, nv, at, cnt)
        end if
        m = kept(perm(at))
        call mode_report(cnt, nnull, count, n_null, ok)
    end procedure mode_i64

    module procedure mode_bool
        logical, allocatable :: kept(:)
        integer(int64), allocatable :: idx(:), perm(:)
        real(real64), allocatable :: w(:), sw(:)
        logical, allocatable :: starts(:)
        integer(int64), allocatable :: tied(:)
        integer(int64) :: nv, nnull, i, at, cnt

        call mode_survivors(size(values, kind=int64), "pf_mode", is_valid, weights, idx, w, &
            nv, nnull)
        if (nv == 0_int64) then
            if (present(modes)) allocate(modes(0))
            call mode_report(0_int64, nnull, count, n_null, ok)
            return
        end if
        kept = values(idx(1:nv))
        call pf_argsort(kept, perm)
        allocate(starts(nv))
        starts(1) = .true.
        do i = 2_int64, nv
            ! `.neqv.` rather than `/=`, which is not defined for LOGICAL.
            starts(i) = (kept(perm(i)) .neqv. kept(perm(i - 1_int64)))
        end do
        ! `w` is indexed by SURVIVOR and `perm` permutes survivors, so sorted position i carries
        ! weight `w(perm(i))`. Reindexed into its own array rather than gathered onto itself: an
        ! array-section assignment whose two sides are the same array is a heap temporary the
        ! compiler cannot elide (CLAUDE.md), and a weight that lost its value would be a silent
        ! wrong answer with nothing to report it.
        if (allocated(w)) then
            allocate(sw(nv))
            do i = 1_int64, nv
                sw(i) = w(perm(i))
            end do
        end if
        ! `tied` is asked for only when the caller wants every mode, so the second walk of the
        ! run structure is never paid by a caller who does not. `tied(1)` is `at` by construction,
        ! which is what keeps `m` and `modes(1)` from ever disagreeing.
        if (present(modes)) then
            call best_run(starts, perm, sw, nv, at, cnt, tied)
            allocate(modes(size(tied, kind=int64)))
            do i = 1_int64, size(tied, kind=int64)
                modes(i) = kept(perm(tied(i)))
            end do
        else
            call best_run(starts, perm, sw, nv, at, cnt)
        end if
        m = kept(perm(at))
        call mode_report(cnt, nnull, count, n_null, ok)
    end procedure mode_bool

    module procedure mode_chr
        character(len=len(values)), allocatable :: kept(:)
        integer :: wid
        integer(int64), allocatable :: idx(:), perm(:)
        real(real64), allocatable :: w(:), sw(:)
        logical, allocatable :: starts(:)
        integer(int64), allocatable :: tied(:)
        integer(int64) :: nv, nnull, i, at, cnt

        call mode_survivors(size(values, kind=int64), "pf_mode", is_valid, weights, idx, w, &
            nv, nnull)
        if (nv == 0_int64) then
            if (present(modes)) allocate(character(len=0) :: modes(0))
            call mode_report(0_int64, nnull, count, n_null, ok)
            return
        end if
        allocate(kept(nv))
        do i = 1_int64, nv
            kept(i) = values(idx(i))
        end do
        call pf_argsort(kept, perm)
        allocate(starts(nv))
        starts(1) = .true.
        do i = 2_int64, nv
            starts(i) = (kept(perm(i)) /= kept(perm(i - 1_int64)))
        end do
        ! `w` is indexed by SURVIVOR and `perm` permutes survivors, so sorted position i carries
        ! weight `w(perm(i))`. Reindexed into its own array rather than gathered onto itself: an
        ! array-section assignment whose two sides are the same array is a heap temporary the
        ! compiler cannot elide (CLAUDE.md), and a weight that lost its value would be a silent
        ! wrong answer with nothing to report it.
        if (allocated(w)) then
            allocate(sw(nv))
            do i = 1_int64, nv
                sw(i) = w(perm(i))
            end do
        end if
        ! `tied` is asked for only when the caller wants every mode, so the second walk of the
        ! run structure is never paid by a caller who does not. `tied(1)` is `at` by construction,
        ! which is what keeps `m` and `modes(1)` from ever disagreeing.
        if (present(modes)) then
            call best_run(starts, perm, sw, nv, at, cnt, tied)
            ! **The length is the LONGEST tied value's, taken over every one of them before the
            ! array is allocated.** A deferred-length allocatable character array has ONE length
            ! for all its elements, so sizing it from `tied(1)` -- the natural spelling, and the
            ! smallest tied value -- would truncate every longer mode beside it. That is the
            ! sized-from-the-first-element bug this repository has a standing rule about, and the
            ! ascending order makes the first element systematically likely to be the short one.
            wid = 0
            do i = 1_int64, size(tied, kind=int64)
                wid = max(wid, len_trim(kept(perm(tied(i)))))
            end do
            allocate(character(len=wid) :: modes(size(tied, kind=int64)))
            do i = 1_int64, size(tied, kind=int64)
                ! Element by element: a whole-array assignment to a deferred-length allocatable
                ! reallocates it to the RHS's length (CLAUDE.md), which would undo the sizing above.
                modes(i) = trim(kept(perm(tied(i))))
            end do
        else
            call best_run(starts, perm, sw, nv, at, cnt)
        end if
        ! Trimmed, so the result is as long as the value rather than as long as whatever width the
        ! caller happened to declare -- the same rule a character array meets on its way into a
        ! `parquet_column`. Every element of `values` shares one declared length, so the padding
        ! cannot be anything the caller meant.
        m = trim(kept(perm(at)))
        call mode_report(cnt, nnull, count, n_null, ok)
    end procedure mode_chr

    module procedure mode_str
        integer(int64), allocatable :: perm(:), order(:)
        real(real64), allocatable :: w(:)
        logical, allocatable :: starts(:)
        character(len=:), allocatable :: a, b
        integer :: wid
        integer(int64), allocatable :: tied(:)
        integer(int64) :: n, nv, nnull, i, at, cnt, wbits

        if (present(is_valid)) &
            error stop "pf_mode: a parquet_string_column carries its own validity, so passing " // &
                "is_valid= alongside one gives two sources of truth that can disagree; drop it"
        n = parquet_string_column_size(values)
        if (present(weights)) call stats_check_sizes(n, "pf_mode", weights=weights)
        ! The WHOLE column is ordered and the survivors are filtered out of the permutation
        ! afterwards, rather than the other way round: `pf_argsort` orders a packed string column
        ! without unpacking it, and copying the survivors into a `character` array first would
        ! need a common declared length this column need not have.
        call pf_argsort(values, perm)
        allocate(order(n))
        if (present(weights)) allocate(w(n))
        nv = 0_int64
        nnull = 0_int64
        do i = 1_int64, n
            if (parquet_string_column_is_null(values, perm(i))) cycle
            if (present(weights)) then
                ! One integer compare on the happy path; see STATS_W_LIM. The validator is
                ! reached only by a weight that cannot be valid.
                wbits = transfer(weights(perm(i)), 0_int64)
                if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then
                    call stats_check_weight(weights(perm(i)), perm(i), "pf_mode")
                    if (weights(perm(i)) <= 0.0_real64) cycle
                else if (wbits == 0_int64) then
                    cycle
                end if
            end if
            nv = nv + 1_int64
            order(nv) = perm(i)
            if (present(weights)) w(nv) = weights(perm(i))
        end do
        ! Counted over the column rather than over the filtered permutation, so the count does not
        ! depend on the order the survivors were visited in.
        nnull = parquet_string_column_null_count(values)
        if (nv == 0_int64) then
            if (present(modes)) allocate(character(len=0) :: modes(0))
            call mode_report(0_int64, nnull, count, n_null, ok)
            return
        end if
        allocate(starts(nv))
        starts(1) = .true.
        do i = 2_int64, nv
            call parquet_string_column_get(values, order(i), a)
            call parquet_string_column_get(values, order(i - 1_int64), b)
            starts(i) = (a /= b)
        end do
        ! `w` was filled in survivor order as the permutation was filtered, so it already lines up
        ! with `order` and needs none of the array kinds' reindexing.
        ! `tied` is asked for only when the caller wants every mode, so the second walk of the
        ! run structure is never paid by a caller who does not. `tied(1)` is `at` by construction,
        ! which is what keeps `m` and `modes(1)` from ever disagreeing.
        if (present(modes)) then
            call best_run(starts, order, w, nv, at, cnt, tied)
            ! Two passes over the tied values: one to find the longest, because a deferred-length
            ! allocatable character array has ONE length for all its elements and sizing it from
            ! the first -- the SMALLEST tied value -- would truncate every longer one beside it.
            wid = 0
            do i = 1_int64, size(tied, kind=int64)
                call parquet_string_column_get(values, order(tied(i)), a)
                wid = max(wid, len(a))
            end do
            allocate(character(len=wid) :: modes(size(tied, kind=int64)))
            do i = 1_int64, size(tied, kind=int64)
                call parquet_string_column_get(values, order(tied(i)), a)
                ! Element by element: a whole-array assignment to a deferred-length allocatable
                ! reallocates it to the RHS's length (CLAUDE.md), undoing the sizing above.
                modes(i) = a
            end do
        else
            call best_run(starts, order, w, nv, at, cnt)
        end if
        call parquet_string_column_get(values, order(at), m)
        call mode_report(cnt, nnull, count, n_null, ok)
    end procedure mode_str

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
    end subroutine ensure_ordered

    !> Whether every order statistic of this object is undefined, before any of them is computed.
    !!
    !! Two causes with one answer. An EMPTY population has nothing to take an order statistic of.
    !! And a population holding a NaN under `skipnan = .false.` was poisoned deliberately by the
    !! caller, so every answer is NaN -- which the order tier has to test for rather than inherit,
    !! since a NaN sorts to one END of the buffer instead of poisoning the interpolation. The
    !! moments already answer NaN in the same state, so this is what keeps `%mean()` and
    !! `%median()` from disagreeing about whether the population has an answer at all.
    pure function obj_undefined(self) result(res)
        class(pf_stats), intent(in) :: self !! the accumulator.
        logical :: res                      !! .true. when every order statistic is NaN.
        res = (self%keep_n == 0_int64) .or. self%acc%saw_nan
    end function obj_undefined

    !> One quantile off the object's ordered buffer, shared by every tier-B binding.
    subroutine obj_quantile_at(self, p, method, res)
        class(pf_stats), intent(inout) :: self             !! the accumulator, already ordered.
        real(real64), intent(in) :: p                      !! the probability, already validated.
        character(len=*), intent(in), optional :: method   !! the interpolation rule.
        real(real64), intent(out) :: res                   !! the quantile.
        integer :: code

        call method_code("pf_stats%quantile", method, self%freq .and. self%wtd, code)
        if (obj_undefined(self)) then
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
        if (obj_undefined(self)) return
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
        if (obj_undefined(self)) then
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
        ! claim that they are ordered is dropped, so the next order statistic sorts again. Tier C
        ! goes with it: `%release_order` is documented as freeing both, and a cached deviation
        ! that outlived the ordering it was built from would be the one piece of state a
        ! `%release_order` did not release.
        self%ordered = .false.
        self%mad_ready = .false.
    end procedure obj_release_order

    module procedure obj_is_ordered
        res = self%ordered
    end procedure obj_is_ordered

    ! ==================================================================================
    ! Tier C on the object, and the descriptive dump
    ! ==================================================================================

    module procedure obj_has_deviation
        res = self%mad_ready
    end procedure obj_has_deviation

    module procedure obj_mad
        real(real64) :: factor, c

        call scale_factor("pf_stats%mad", scale, factor)
        call check_center("pf_stats%mad", center)
        ! Tier B first, for three separate reasons: the streaming and uncomputed guards live in
        ! `ensure_ordered`, a deferred tier-A recomputation has to land before `keep_n` is read,
        ! and the default centre IS the median.
        call ensure_ordered(self, "pf_stats%mad")
        res = stats_nan()
        if (obj_undefined(self)) return

        if (present(center)) then
            c = center
        else
            call obj_quantile_at(self, 0.5_real64, res=c)
        end if
        ! The cache is keyed on the RESOLVED centre, not on whether `center` was supplied: that
        ! makes repeated default calls hit, makes a `center=` equal to the median hit too, and
        ! makes a different centre rebuild rather than answer the wrong population's deviation.
        ! Bit equality is the right test because `mad_c` is a stored copy of a value this
        ! procedure computed, not two independent derivations of one quantity.
        if (.not. (self%mad_ready .and. self%mad_c == c)) then
            call raw_mad(self%keep, self%keep_w, self%keep_n, c, res=self%mad_raw)
            self%mad_c = c
            self%mad_ready = .true.
        end if
        res = factor * self%mad_raw
    end procedure obj_mad

    module procedure obj_print
        integer :: u
        real(real64) :: q1, q2, q3
        character(len=:), allocatable :: head, stream

        ! Solicited output, so `verbosity = "silent"` governs it exactly as it governs every other
        ! printer in this library: the call returns having written nothing.
        if (parquet_output_is_suppressed()) return
        if (present(unit)) then
            u = unit
        else
            ! Resolved from the setting's own token rather than from its private integer, so this
            ! reaches `parquet_settings_base` through the same public surface a caller would.
            call parquet_get_message_stream(stream)
            if (stream == "stderr") then
                u = error_unit
            else
                u = output_unit
            end if
        end if
        head = "pf_stats"
        if (present(name)) head = "pf_stats " // trim(name)
        if (.not. self%live) then
            write(u, '(a)') trim(head) // ": no population (call %compute or %init first)"
            return
        end if
        ! A deferred tier-A recomputation must land before any count is read.
        call stats_ensure(self)
        write(u, '(a)') trim(head)
        write(u, '(2x,a,i0)') "n         ", self%n()
        write(u, '(2x,a,i0)') "n_valid   ", self%n_valid()
        write(u, '(2x,a,i0)') "n_null    ", self%n_null()
        ! Shown unconditionally, and this is the mitigation that makes `skipnan = .true.` safe:
        ! a silently skipped NaN is visible here rather than nowhere.
        write(u, '(2x,a,i0)') "n_nan     ", self%n_nan()
        write(u, '(2x,a,g0)') "mean      ", self%mean()
        write(u, '(2x,a,g0)') "stddev    ", self%stddev()
        write(u, '(2x,a,g0)') "min       ", self%vmin()
        write(u, '(2x,a,g0)') "max       ", self%vmax()
        if (.not. self%hold) then
            ! A streaming accumulator kept no values, so the quartiles do not exist. Saying so is
            ! the whole point: a printer that aborted here would be a poor way to find out what an
            ! object holds, and one that silently printed six rows instead of nine would be worse.
            write(u, '(2x,a)') "25% 50% 75%  (not available: retain=.false.)"
            return
        end if
        call ensure_ordered(self, "pf_stats%print")
        call obj_quantile_at(self, 0.25_real64, res=q1)
        call obj_quantile_at(self, 0.50_real64, res=q2)
        call obj_quantile_at(self, 0.75_real64, res=q3)
        write(u, '(2x,a,g0)') "25%       ", q1
        write(u, '(2x,a,g0)') "50%       ", q2
        write(u, '(2x,a,g0)') "75%       ", q3
    end procedure obj_print

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
