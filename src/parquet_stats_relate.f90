!===========================================
! parquet-fortran
!
! Copyright (c) Elmo Tempel
! Licensed under the MIT License - see LICENSE
!===========================================
!> The relational tier of `parquet_stats`: statistics of TWO samples, and of one sample
!! transformed element by element.
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
submodule (parquet_stats) parquet_stats_relate
    implicit none

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
        integer(int64) :: n, i

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
                call stats_check_weight(weights(i), i, what)
                if (weights(i) <= 0.0_real64) cycle
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
        ! The reliability convention, which is `pf_variance`'s default and therefore the one that
        ! makes `pf_cov(x, x)` equal `pf_variance(x)` at the same `ddof`. Unweighted, `w_sq` is
        ! `w_sum` and this collapses to the familiar `n - ddof`.
        denom = w_sum - real(dd, real64) * w_sq / w_sum
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

end submodule parquet_stats_relate ! GCOVR_EXCL_LINE
