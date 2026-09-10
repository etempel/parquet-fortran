!===========================================
! parquet-fortran
!
! Copyright (c) Elmo Tempel
! Licensed under the MIT License - see LICENSE
!===========================================
!> The relational tier of `parquet_stats`: statistics of TWO samples, of one sample transformed
!! element by element, and the four running folds.
!!
!! **Everything here is VECTOR-shaped in at least one direction**, which is what groups an
!! otherwise odd set: `pf_cov`/`pf_corr` take two arrays in, `pf_zscore` and the cumulative family
!! give one array out. Nothing here reduces a population to a single number without also needing
!! a second sequence to do it with.
!!
!! **Pairwise-complete is the rule, and it is not a preference.** A pair enters a two-sample
!! population only when both of its elements are usable; handling nullness independently per array
!! would leave a covariance between vectors of different lengths, which is not a number. `n_null`
!! and `n_nan` therefore count PAIRS here, and the doc-comments say so, because a count that
!! silently changed meaning between families would be worse than no count.
!!
!! **The covariance is computed by `stats_pair_moments`, in `parquet_stats_core`, and not here.**
!! That is what makes `pf_cov(x, x)` exactly `pf_variance(x)`: both reach the same block tree, the
!! same `pair_reduce` and the same re-centring correction. A local accumulation loop would agree
!! to fifteen digits and differ in the sixteenth, and the identity is the more valuable of the two.
!!
!! **The cumulative family's null rule is NOT the reduction family's, and `cum_scan` is where it
!! lives.** An excluded element yields an excluded OUTPUT element and the running value carries
!! past it unchanged -- pandas' rule -- so "the output element is undefined" and "the rest of the
!! output is undefined" are one careless line apart and only the first is meant. One loop serves
!! all four folds for exactly that reason: four copies of that rule would be four chances to get
!! the second sentence instead of the first.
submodule (parquet_stats) parquet_stats_relate
    implicit none

    !> Which fold `cum_scan` applies. Private to this submodule: they name a branch, not a rule a
    !! caller chooses between, so every public entry point picks one for itself.
    integer, parameter :: CUM_SUM = 1   !! the running sum.
    integer, parameter :: CUM_PROD = 2  !! the running product.
    integer, parameter :: CUM_MAX = 3   !! the running maximum.
    integer, parameter :: CUM_MIN = 4   !! the running minimum.

contains

    ! ==================================================================================
    ! Shared plumbing
    ! ==================================================================================

    !> Applies the pairwise-complete rule, handing back the surviving pairs and their weights.
    !!
    !! The single-sample twin is `stats_compact`, and the exclusion ORDER is the same one: a pair
    !! is dropped for nullness first, then for a NaN on either side, then for a zero weight -- so
    !! a weight that is garbage exactly where a value is null is never examined.
    !!
    !! There is no `skipnan` here, and its absence is the specification: a NaN on either side
    !! always drops the pair, because `skipnan = .false.` would have to mean "keep a pair whose
    !! value is not a number", and a covariance over such a population is NaN by construction
    !! rather than by choice.
    subroutine pair_compact(x, y, what, is_valid, weights, kx, ky, kw, m, n_null, n_nan)
        real(real64), intent(in) :: x(:)                      !! the first sample.
        real(real64), intent(in) :: y(:)                      !! the second sample.
        character(len=*), intent(in) :: what                  !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)          !! per pair: .false. marks it unusable.
        real(real64), intent(in), optional :: weights(:)      !! per pair weight.
        real(real64), allocatable, intent(out) :: kx(:)       !! the surviving first values.
        real(real64), allocatable, intent(out) :: ky(:)       !! the surviving second values.
        real(real64), allocatable, intent(out) :: kw(:)       !! their weights, or unallocated.
        integer(int64), intent(out) :: m                      !! how many pairs survived.
        integer(int64), intent(out) :: n_null                 !! pairs dropped as null.
        integer(int64), intent(out) :: n_nan                  !! pairs dropped for a NaN.
        integer(int64) :: n, i, wbits

        n = size(x, kind=int64)
        if (size(y, kind=int64) /= n) &
            error stop what // ": the two samples must be the same size, but the first has " // &
                trim(stats_i2s(n)) // " elements and the second has " // &
                trim(stats_i2s(size(y, kind=int64)))
        call stats_check_sizes(n, what, is_valid, weights)
        allocate(kx(n), ky(n))
        if (present(weights)) allocate(kw(n))
        m = 0_int64
        n_null = 0_int64
        n_nan = 0_int64
        do i = 1_int64, n
            if (present(is_valid)) then
                if (.not. is_valid(i)) then
                    n_null = n_null + 1_int64
                    cycle
                end if
            end if
            ! `x /= x` rather than `ieee_is_nan`: this module's standing rule, because the test is
            ! per element and `ieee_is_nan` is a runtime call on half the compiler fleet.
            if (x(i) /= x(i) .or. y(i) /= y(i)) then
                n_nan = n_nan + 1_int64
                cycle
            end if
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
            kx(m) = x(i)
            ky(m) = y(i)
            if (present(weights)) kw(m) = weights(i)
        end do
    end subroutine pair_compact

    !> Resolves `pf_corr`'s `method=` token to `.true.` for Spearman, or aborts naming both.
    subroutine corr_method(what, method, weighted, spearman)
        character(len=*), intent(in) :: what               !! the public procedure's name.
        character(len=*), intent(in), optional :: method   !! the caller's token, if any.
        logical, intent(in) :: weighted                    !! .true. when weights were supplied.
        logical, intent(out) :: spearman                   !! .true. for Spearman.
        character(len=16) :: tok
        integer :: i, c

        spearman = .false.
        if (.not. present(method)) return
        tok = ""
        do i = 1, min(len(method), len(tok))
            c = iachar(method(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            tok(i:i) = achar(c)
        end do
        select case (trim(tok))
        case ("pearson")
            spearman = .false.
        case ("spearman")
            spearman = .true.
        case default
            error stop what // ': unrecognised method "' // trim(method) // &
                '"; the tokens are "pearson" (the default) and "spearman"'
        end select
        if (spearman .and. weighted) &
            error stop what // ': method="spearman" does not accept weights; a weighted midrank ' // &
                'is a further definitional choice that no reference library makes, so this ' // &
                'refuses rather than inventing one'
    end subroutine corr_method

    !> Resolves `pf_normal_scores`' `method=` token to its plotting position, or aborts.
    !!
    !! **Two shapes behind one argument, so the flag is not decoration.** Five of the six tokens
    !! name a constant `a` in `(r - a)/(m + 1 - 2a)`; "filliben" is the MEDIAN rank and is not of
    !! that family at all, so it comes back as its own flag rather than as an `a` that could not
    !! express it. Reaching for a sentinel `a` here -- a negative, say -- would put a number that
    !! is not a plotting position into a variable whose whole purpose is to hold one.
    subroutine nscore_method(what, method, a, filliben)
        character(len=*), intent(in) :: what               !! the public procedure's name.
        character(len=*), intent(in), optional :: method   !! the caller's token, if any.
        real(real64), intent(out) :: a                     !! the plotting-position constant.
        logical, intent(out) :: filliben                   !! .true. for the median rank.
        character(len=16) :: tok
        integer :: i, c

        a = 0.375_real64
        filliben = .false.
        if (.not. present(method)) return
        tok = ""
        do i = 1, min(len(method), len(tok))
            c = iachar(method(i:i))
            if (c >= iachar("A") .and. c <= iachar("Z")) c = c + 32
            tok(i:i) = achar(c)
        end do
        select case (trim(tok))
        case ("blom")
            a = 0.375_real64
        case ("weibull")
            a = 0.0_real64
        case ("tukey")
            a = 1.0_real64 / 3.0_real64
        case ("hazen")
            a = 0.5_real64
        case ("cunnane")
            a = 0.4_real64
        case ("filliben")
            filliben = .true.
        case default
            error stop what // ': unrecognised method "' // trim(method) // &
                '"; the tokens are "blom" (the default), "weibull", "tukey", "hazen", ' // &
                '"cunnane" and "filliben"'
        end select
    end subroutine nscore_method

    !> The probability a midrank marks: what `Phi**(-1)` is actually asked for.
    !!
    !! **The result is never 0 and never 1**, for every token and every `m`, so `pf_probit` here
    !! never returns an infinity. For the `a`-family that is arithmetic: `a` lies in `[0, 1/2]`,
    !! so the smallest position `(1-a)/(m+1-2a)` is positive and the largest `(m-a)/(m+1-2a)` is
    !! below 1 for any `a < 1`. Filliben's two end forms are `1 - 0.5**(1/m)` and `0.5**(1/m)`,
    !! both strictly inside. That is why this family needs no clamp, and a clamp added here would
    !! hide a genuine defect rather than guard against one.
    pure function nscore_position(r, m, a, den, filliben) result(p)
        real(real64), intent(in) :: r      !! the midrank, 1-based and possibly a half-integer.
        integer(int64), intent(in) :: m    !! how many elements survived.
        real(real64), intent(in) :: a      !! the plotting-position constant.
        real(real64), intent(in) :: den    !! `m + 1 - 2a`, formed once by the caller.
        logical, intent(in) :: filliben    !! .true. for the median rank.
        real(real64) :: p
        real(real64) :: rm

        rm = real(m, real64)
        if (m == 1_int64) then
            ! **Every rule meets at exactly 1/2 when one element survives**, and it is written out
            ! rather than left to the arithmetic because it is the documented degenerate answer.
            ! `(1-a)/(2-2a)` is 1/2 mathematically for any `a`, but it is a quotient of two
            ! rounded numbers, and one ulp of error there is the difference between a score of
            ! exactly 0 and a score of -1e-16 -- which is what a caller would see and disbelieve.
            p = 0.5_real64
        else if (filliben) then
            ! Filliben's median rank. The two END positions are separate closed forms rather than
            ! the interior formula evaluated there, which is what makes this Filliben's statistic
            ! rather than an approximation to it. A tie spanning position 1 has a midrank above 1
            ! and so takes the interior form, which is both correct and monotone.
            if (r == 1.0_real64) then
                p = 1.0_real64 - 0.5_real64 ** (1.0_real64 / rm)
            else if (r == rm) then
                p = 0.5_real64 ** (1.0_real64 / rm)
            else
                p = (r - 0.3175_real64) / (rm + 0.365_real64)
            end if
        else
            p = (r - a) / den
        end if
    end function nscore_position

    !> The midranks of `v(1:m)`: each run of equal values gets the mean of the positions it spans.
    !!
    !! **This is not a second ranking implementation.** The expensive half of ranking is the sort,
    !! and that sort is `pf_argsort` -- shared with every other ordering in this library. What is
    !! here is one walk over the sorted order, in `real64` because a midrank is a half-integer and
    !! `pf_rank`'s output is deliberately an integer array (see feature_pandas_S4.md's F1).
    subroutine midranks(v, m, r, threads)
        real(real64), intent(in) :: v(:)              !! the values, in any order.
        integer(int64), intent(in) :: m               !! how many are live.
        real(real64), intent(out) :: r(:)             !! `r(i)` is `v(i)`'s midrank, 1-based.
        integer, intent(in), optional :: threads      !! passed straight to `pf_argsort`.
        integer(int64), allocatable :: perm(:)
        integer(int64) :: i, lo
        real(real64) :: mid

        if (m <= 0_int64) return
        call pf_argsort(v(1:m), perm, threads=threads)
        i = 1_int64
        do while (i <= m)
            ! The run of equal values starting at sorted position `i`. Ties MUST share a rank --
            ! that is what a midrank is -- and giving each its own position instead would make
            ! Spearman depend on the sort's tie-breaking, which is a reproducibility defect
            ! rather than a rounding one.
            lo = i
            do while (i < m)
                if (v(perm(i + 1_int64)) /= v(perm(lo))) exit
                i = i + 1_int64
            end do
            mid = 0.5_real64 * real(lo + i, real64)
            do while (lo <= i)
                r(perm(lo)) = mid
                lo = lo + 1_int64
            end do
            i = i + 1_int64
        end do
    end subroutine midranks

    ! ==================================================================================
    ! The real64 cores
    ! ==================================================================================

    module procedure cov_f64
        real(real64), allocatable :: kx(:), ky(:), kw(:)
        real(real64) :: mx, my, sxx, sxy, syy, w_sum, w_sq, denom
        integer(int64) :: m, nnull, nnan
        integer :: dd
        logical :: freq

        call stats_weight_kind("pf_cov", weight_type, freq)
        dd = 1
        if (present(ddof)) dd = ddof
        call pair_compact(x, y, "pf_cov", is_valid, weights, kx, ky, kw, m, nnull, nnan)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        c = stats_nan()
        if (present(ok)) ok = .false.
        if (m == 0_int64) return

        call stats_pair_moments(kx, ky, kw, m, mx, my, sxx, sxy, syy, w_sum, w_sq)
        if (w_sum <= 0.0_real64) return
        ! **The same two denominators `pf_variance` uses, and that is the requirement rather than
        ! a convenience.** `pf_cov(x, x, ddof=d)` IS `pf_variance(x, ddof=d)` by definition, so
        ! offering only the reliability convention here would make the two disagree for any caller
        ! who asked the variance for frequency weights. A FREQUENCY weight of 3 says the value
        ! occurred three times, so `ddof` is charged against `sum(w)`; a RELIABILITY weight says it
        ! is that much more precise, so it is charged against Kish's effective size
        ! `sum(w)**2 / sum(w**2)`. Unweighted, `w_sq` is `w_sum`, both collapse to `n - ddof`, and
        ! the argument cannot be observed at all.
        if (freq) then
            denom = w_sum - real(dd, real64)
        else
            denom = w_sum - real(dd, real64) * w_sq / w_sum
        end if
        if (denom <= 0.0_real64) return
        c = sxy / denom
        if (present(ok)) ok = (c == c)
    end procedure cov_f64

    module procedure corr_f64
        real(real64), allocatable :: kx(:), ky(:), kw(:), rx(:), ry(:)
        real(real64) :: mx, my, sxx, sxy, syy, w_sum, w_sq, den
        integer(int64) :: m, nnull, nnan
        logical :: spearman

        call corr_method("pf_corr", method, present(weights), spearman)
        call pair_compact(x, y, "pf_corr", is_valid, weights, kx, ky, kw, m, nnull, nnan)
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        r = stats_nan()
        if (present(ok)) ok = .false.
        if (m == 0_int64) return

        if (spearman) then
            ! Pearson over midranks, which is the definition rather than an approximation to it.
            ! The ranks are taken over the SURVIVING pairs, not over the original arrays: ranking
            ! first and dropping pairs afterwards would leave gaps in both rank vectors and give a
            ! correlation of two populations neither sample has.
            allocate(rx(m), ry(m))
            call midranks(kx, m, rx)
            call midranks(ky, m, ry)
            call stats_pair_moments(rx, ry, kw, m, mx, my, sxx, sxy, syy, w_sum, w_sq)
        else
            call stats_pair_moments(kx, ky, kw, m, mx, my, sxx, sxy, syy, w_sum, w_sq)
        end if
        ! No `ddof` anywhere: the one in the covariance and the two in the standard deviations
        ! cancel exactly, which is why this procedure does not offer the argument at all.
        if (sxx <= 0.0_real64 .or. syy <= 0.0_real64) return
        ! **The two perfect correlations are answered exactly, and this is arithmetic rather than a
        ! special case.** When all three centred sums coincide the expression `sxy/sqrt(sxx*syy)`
        ! IS 1 -- but computed that way it need not be: `sqrt` is correctly rounded and
        ! `sqrt(a)*sqrt(a)` is not `a`, so `pf_corr(x, x)` came back one ulp below 1 before this
        ! test existed. A clamp would not have fixed it either, since the error is on the low side.
        if (sxy == sxx .and. sxy == syy) then
            r = 1.0_real64
        else if (sxx == syy .and. sxy == -sxx) then
            r = -1.0_real64
        else
            den = sqrt(sxx) * sqrt(syy)
            if (den <= 0.0_real64) return
            r = sxy / den
            ! **The NaN leaves before the clamp, and it is the CLAMP that needs it, not the
            ! comparison.** An infinity in either variable makes all three centred sums NaN (the
            ! screen in `stats_pair_moments`), so `r` is NaN here, and a NaN with `ok = .false.`
            ! is what this procedure reports rather than aborting. But `if (r > 1) r = 1` is a
            ! clamp, and a backend is free to emit it as x86 `minsd`/`maxsd` -- measured under
            ! nagfor from `-O2` upward, and under gfortran at `-O2` -- and those raise
            ! IEEE_INVALID for a QUIET-NaN operand, where the bare comparisons above and below
            ! come out as the quiet `ucomisd`. Under nagfor's default `-ieee=stop` that is a dead
            ! process, and only in an optimised build: a plain `fpm test` never reaches it, and
            ! `fpm test --profile release` is what found it.
            !
            ! `r /= r` rather than `ieee_is_nan`: this module's standing rule, and quiet either way.
            if (r /= r) return
            ! Rounding can still put a near-perfect correlation a few ulp OUTSIDE [-1, 1], which
            ! is startling to meet and never useful.
            if (r > 1.0_real64) r = 1.0_real64
            if (r < -1.0_real64) r = -1.0_real64
        end if
        if (present(ok)) ok = (r == r)
    end procedure corr_f64

    module procedure zscore_f64
        real(real64) :: mu, sd
        integer(int64) :: n, nv, nnull, nnan, i
        logical :: saw_nan, skip, excluded, any_excluded

        n = size(values, kind=int64)
        if (size(z, kind=int64) /= n) &
            error stop "pf_zscore: z has " // trim(stats_i2s(size(z, kind=int64))) // &
                " elements but values has " // trim(stats_i2s(n))
        if (present(out_valid)) then
            if (size(out_valid, kind=int64) /= n) &
                error stop "pf_zscore: out_valid has " // &
                    trim(stats_i2s(size(out_valid, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(n))
        end if
        skip = .true.
        if (present(skipnan)) skip = skipnan

        call stats_mean_sd(values, "pf_zscore", is_valid, ddof, skipnan, mu, sd, nv, nnull, &
            nnan, saw_nan)
        if (present(n_null)) n_null = nnull

        ! One test decides everything below: a standard deviation that is not a positive number
        ! makes EVERY output undefined. That covers the empty population, the single-element one at
        ! the default `ddof = 1`, a constant population, and a NaN surviving under
        ! `skipnan = .false.` -- four data conditions with one answer and no branch each.
        if (.not. (sd > 0.0_real64)) then
            do i = 1_int64, n
                z(i) = stats_nan()
            end do
            if (present(out_valid)) out_valid = .false.
            if (present(ok)) ok = .false.
            return
        end if

        any_excluded = .false.
        do i = 1_int64, n
            excluded = .false.
            if (present(is_valid)) excluded = .not. is_valid(i)
            if (.not. excluded .and. skip) excluded = (values(i) /= values(i))
            if (excluded) then
                any_excluded = .true.
                ! A quiet NaN whether or not `out_valid` was asked for. The design left this
                ! element "default-initialised" when the mask is present; writing the NaN anyway
                ! is strictly safer -- an output element is then never undefined memory, which is
                ! a class nagfor's `-nan` build reports and the standard leaves non-conforming to
                ! read -- and it costs one store on a path that is already excluded.
                z(i) = stats_nan()
                if (present(out_valid)) out_valid(i) = .false.
            else
                z(i) = (values(i) - mu) / sd
                if (present(out_valid)) out_valid(i) = .true.
            end if
        end do
        ! ONE flag with two causes, deliberately: either the whole statistic was undefined (handled
        ! above, and then `n_null` is 0) or a null was written out as a NaN. `n_null` separates
        ! them at no cost, so a second flag would be another thing to document and reset for no new
        ! information.
        if (present(ok)) ok = .not. any_excluded
    end procedure zscore_f64

    module procedure normal_scores_f64
        real(real64), allocatable :: keep(:), keep_w(:), rank(:)
        real(real64) :: a, den
        integer(int64) :: n, m, nnull, nnan, i, k
        logical :: saw_nan, filliben, excluded, any_excluded

        ! The token is resolved BEFORE anything else, including the size checks: an unrecognised
        ! `method=` is misuse and should abort on an empty array exactly as it does on a full one,
        ! rather than depending on whether the population happened to reach the branch that reads
        ! it. That is `pf_quantile`'s rule and the reason its bad-token scenario is reproducible.
        call nscore_method("pf_normal_scores", method, a, filliben)
        n = size(values, kind=int64)
        if (size(s, kind=int64) /= n) &
            error stop "pf_normal_scores: s has " // trim(stats_i2s(size(s, kind=int64))) // &
                " elements but values has " // trim(stats_i2s(n))
        if (present(out_valid)) then
            if (size(out_valid, kind=int64) /= n) &
                error stop "pf_normal_scores: out_valid has " // &
                    trim(stats_i2s(size(out_valid, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(n))
        end if

        call stats_compact(values, "pf_normal_scores", is_valid=is_valid, skipnan=skipnan, &
            keep_x=keep, keep_w=keep_w, n_valid=m, n_null=nnull, n_nan=nnan, saw_nan=saw_nan)
        if (present(n_null)) n_null = nnull

        ! **`saw_nan` is tested here, and `feature_risks.md` Risk-253 is exactly why.** Under
        ! `skipnan = .false.` a surviving NaN sorts to one end of the ordering and poisons no
        ! rank -- the other m-1 elements keep the positions they always had -- so a procedure that
        ! tested only for an empty population would hand back a perfectly ordinary set of scores
        ! from a population the caller explicitly asked to have poisoned, with `ok = .true.`.
        ! One test covers that and the empty population both, and they have the same answer.
        if (m == 0_int64 .or. saw_nan) then
            do i = 1_int64, n
                s(i) = stats_nan()
            end do
            if (present(out_valid)) out_valid = .false.
            if (present(ok)) ok = .false.
            return
        end if

        ! **The ranks are taken over the SURVIVORS, not over the original array** -- Risk-254, and
        ! `pf_corr`'s Spearman path carries the same rule for the same reason. Denominating by
        ! `n` instead would shrink every score toward zero by roughly `m/n`: still ordered, still
        ! centred, still monotone, and wrong only in SCALE, so no distributional check would see
        ! it and only a population with something excluded would show it at all.
        allocate(rank(m))
        call midranks(keep, m, rank, threads)
        den = real(m, real64) + 1.0_real64 - 2.0_real64 * a

        ! One walk in the caller's order, consuming the survivors in the order `stats_compact`
        ! kept them -- which is input order, so the k-th survivor is the k-th element this loop
        ! does not exclude. The exclusion test is `pf_zscore`'s, and it is re-derived rather than
        ! read from a mask because a NaN can only be here at all if `skipnan` excluded it: the
        ! other case returned above.
        any_excluded = .false.
        k = 0_int64
        do i = 1_int64, n
            excluded = .false.
            if (present(is_valid)) excluded = .not. is_valid(i)
            if (.not. excluded) excluded = (values(i) /= values(i))
            if (excluded) then
                any_excluded = .true.
                ! Written whether or not `out_valid` was asked for, as `pf_zscore` does: an output
                ! element is then never undefined memory, which nagfor's `-nan` build reports and
                ! the standard leaves non-conforming to read.
                s(i) = stats_nan()
                if (present(out_valid)) out_valid(i) = .false.
            else
                k = k + 1_int64
                s(i) = pf_probit(nscore_position(rank(k), m, a, den, filliben))
                if (present(out_valid)) out_valid(i) = .true.
            end if
        end do
        if (present(ok)) ok = .not. any_excluded
    end procedure normal_scores_f64

    ! ==================================================================================
    ! The cumulative family
    ! ==================================================================================

    !> The one prefix scan behind `pf_cumsum`, `pf_cumprod`, `pf_cummax` and `pf_cummin`.
    !!
    !! **Four generics, one null rule, one loop.** The rule -- an excluded element yields an
    !! excluded output element and the running value continues past it unchanged -- is the whole
    !! substance of this family, and four copies of it would be four chances to diverge on a case
    !! nothing tests. Only `fold` differs between them.
    !!
    !! `started` rather than a sentinel initial value: the identity for a sum is 0 and for a
    !! product 1, but for a running maximum it would have to be `-huge`, which is a real number a
    !! caller can legitimately hold. A flag costs one branch and cannot be confused with data.
    subroutine cum_scan(values, out, op, what, is_valid, skipnan, out_valid, n_null, n_nan, ok)
        real(real64), intent(in) :: values(:)               !! the population, in scan order.
        real(real64), intent(out) :: out(:)                 !! the running value, same size.
        integer, intent(in) :: op                           !! CUM_SUM, CUM_PROD, CUM_MAX or CUM_MIN.
        character(len=*), intent(in) :: what                !! the public procedure's name.
        logical, intent(in), optional :: is_valid(:)        !! per element: .false. marks a null.
        logical, intent(in), optional :: skipnan            !! .true. excludes a NaN from the scan.
        logical, intent(out), optional :: out_valid(:)      !! per output element: .false. if undefined.
        integer(int64), intent(out), optional :: n_null     !! how many elements were null.
        integer(int64), intent(out), optional :: n_nan      !! how many were excluded as NaN.
        logical, intent(out), optional :: ok                !! .false. when any element was excluded.
        integer(int64) :: n, i, nnull, nnan
        real(real64) :: acc, v
        logical :: skip, started, excluded

        n = size(values, kind=int64)
        if (size(out, kind=int64) /= n) &
            error stop what // ": out has " // trim(stats_i2s(size(out, kind=int64))) // &
                " elements but values has " // trim(stats_i2s(n))
        if (present(out_valid)) then
            if (size(out_valid, kind=int64) /= n) &
                error stop what // ": out_valid has " // &
                    trim(stats_i2s(size(out_valid, kind=int64))) // &
                    " elements but values has " // trim(stats_i2s(n))
        end if
        call stats_check_sizes(n, what, is_valid)
        skip = .true.
        if (present(skipnan)) skip = skipnan

        nnull = 0_int64
        nnan = 0_int64
        started = .false.
        acc = 0.0_real64
        do i = 1_int64, n
            excluded = .false.
            if (present(is_valid)) then
                if (.not. is_valid(i)) then
                    nnull = nnull + 1_int64
                    excluded = .true.
                end if
            end if
            if (.not. excluded .and. skip) then
                ! `x /= x` rather than `ieee_is_nan`: per-element test, and `ieee_is_nan` is a
                ! runtime call on half the compiler fleet.
                if (values(i) /= values(i)) then
                    nnan = nnan + 1_int64
                    excluded = .true.
                end if
            end if
            if (excluded) then
                ! A quiet NaN whether or not `out_valid` was asked for. Writing it anyway means an
                ! output element is never undefined memory -- a class nagfor's `-nan` build
                ! reports and the standard leaves non-conforming to read -- and it costs one store
                ! on a path that has already left the population. **`acc` is deliberately not
                ! touched**: this is the line that decides the family's null rule.
                out(i) = stats_nan()
                if (present(out_valid)) out_valid(i) = .false.
                cycle
            end if
            v = values(i)
            if (.not. started) then
                acc = v
                started = .true.
            else
                select case (op)
                case (CUM_SUM)
                    acc = acc + v
                case (CUM_PROD)
                    acc = acc * v
                case (CUM_MAX)
                    ! **The NaN test is not redundant and a probe found that the hard way.** Under
                    ! `skipnan = .false.` a NaN is a value and must poison every later element, as
                    ! `np.maximum.accumulate` and pandas' `cummax(skipna=False)` both do -- but
                    ! `v > acc` is FALSE for a NaN `v`, so without this the NaN is silently
                    ! discarded and the running maximum sails on. Once `acc` is a NaN it stays
                    ! one, because `v > acc` is false in that direction too.
                    if (v /= v) then
                        acc = v
                    else if (v > acc) then
                        acc = v
                    end if
                case (CUM_MIN)
                    if (v /= v) then
                        acc = v
                    else if (v < acc) then
                        acc = v
                    end if
                end select
            end if
            out(i) = acc
            if (present(out_valid)) out_valid(i) = .true.
        end do
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        ! ONE flag with two causes, and `n_null`/`n_nan` separate them at no cost -- the shape
        ! `pf_zscore` uses. Note what it does NOT report: a NaN kept under `skipnan = .false.`
        ! leaves `ok` .true., because nothing was excluded. The output says so itself.
        if (present(ok)) ok = (nnull + nnan == 0_int64)
    end subroutine cum_scan

    module procedure cumsum_f64
        call cum_scan(values, out, CUM_SUM, "pf_cumsum", is_valid, skipnan, out_valid, n_null, &
            n_nan, ok)
    end procedure cumsum_f64

    module procedure cumprod_f64
        call cum_scan(values, out, CUM_PROD, "pf_cumprod", is_valid, skipnan, out_valid, n_null, &
            n_nan, ok)
    end procedure cumprod_f64

    module procedure cummax_f64
        call cum_scan(values, out, CUM_MAX, "pf_cummax", is_valid, skipnan, out_valid, n_null, &
            n_nan, ok)
    end procedure cummax_f64

    module procedure cummin_f64
        call cum_scan(values, out, CUM_MIN, "pf_cummin", is_valid, skipnan, out_valid, n_null, &
            n_nan, ok)
    end procedure cummin_f64

end submodule parquet_stats_relate ! GCOVR_EXCL_LINE
