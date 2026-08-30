!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! NOT a generated file. The module SPEC that declares everything here IS generated
! (tools/generate_parquet_stats.py), so a signature change means editing that script's template
! text while a body change means editing this file.
!
!> The binning tier of `parquet_stats`: `pf_bucketize` and `pf_histogram`.
!!
!! **`pf_histogram` IS `pf_bucketize` followed by a tally, and that is a property rather than an
!! implementation note.** Both reach `bin_of` for every value, so the count in bin k is exactly
!! the number of codes equal to k over the same arguments -- there is no second edge-search rule
!! that could drift from the first. A test asserts the identity in both edge conventions; keep
!! them sharing `bin_of` and it holds by construction.
!!
!! **The two conventions are mirror images and cover the closed range exactly once.** With
!! `right = .false.` (numpy's, the default) bin k is `[edges(k), edges(k+1))` and the LAST bin
!! closes at the top; with `right = .true.` (pandas' `pd.cut(right=True, include_lowest=True)`)
!! bin k is `(edges(k), edges(k+1)]` and the FIRST bin closes at the bottom. Neither leaves a gap
!! and neither double-counts, so `sum(counts) + n_outside + n_null + n_nan == size(values)` under
!! both -- which is the cheapest whole-family invariant available and is asserted as one.
!!
!! **A NaN reaches no bin under either `skipnan`, and only the ACCOUNTING differs.** Skipped, it
!! is excluded and counted in `n_nan`; kept, it is a value that matches no bin and is counted in
!! `n_outside`. That is not an inconsistency to be tidied away: every comparison against a NaN is
!! false, so "which bin does it join" has one answer, and the argument only decides whether the
!! population was asked the question at all.
submodule (parquet_stats) parquet_stats_bin
    implicit none

contains

    ! ==================================================================================
    ! Shared plumbing
    ! ==================================================================================

    !> Aborts unless `edges` can describe bins at all: two or more, finite-or-infinite, strictly
    !! increasing.
    !!
    !! **Strictly increasing, not merely non-decreasing.** An equal adjacent pair describes a bin
    !! no value can ever reach under either convention, so it is a mistake with no useful reading
    !! -- and admitting it would put an always-zero entry in a histogram that a caller would then
    !! have to explain. An INFINITE outer edge is admitted deliberately: it is how an open-ended
    !! first or last bin is asked for, and it satisfies the ordering test like any other value.
    !!
    !! The NaN test comes first and is separate. A NaN edge fails the ordering test too -- every
    !! comparison against it is false -- but it would be reported as "not increasing", which sends
    !! the reader to the wrong half of their edge array.
    subroutine check_edges(edges, what, nbins)
        real(real64), intent(in) :: edges(:)       !! the bin boundaries to validate.
        character(len=*), intent(in) :: what       !! the public procedure's name, for the message.
        integer(int64), intent(out) :: nbins       !! how many bins they describe.
        integer(int64) :: n, i

        n = size(edges, kind=int64)
        if (n < 2_int64) &
            error stop what // ": edges must hold at least two entries to describe one bin, but " // &
                "holds " // trim(stats_i2s(n))
        do i = 1_int64, n
            ! `x /= x` rather than `ieee_is_nan`: this module's standing rule, because the test is
            ! per element and `ieee_is_nan` is a runtime call on half the compiler fleet.
            if (edges(i) /= edges(i)) &
                error stop what // ": edges(" // trim(stats_i2s(i)) // ") is a NaN"
        end do
        do i = 2_int64, n
            if (.not. (edges(i) > edges(i - 1_int64))) &
                error stop what // ": edges must be strictly increasing, but edges(" // &
                    trim(stats_i2s(i - 1_int64)) // ") is not less than edges(" // &
                    trim(stats_i2s(i)) // ")"
        end do
        nbins = n - 1_int64
    end subroutine check_edges

    !> The 1-based bin `v` falls in, or `0` for a value outside `[edges(1), edges(nbins+1)]`.
    !!
    !! **This is the ONE place an edge convention is applied**, which is what makes `pf_histogram`
    !! and `pf_bucketize` agree by construction rather than by review.
    !!
    !! A NaN answers `0`: the two range tests below are both false for it, so it takes the same
    !! exit an out-of-range value does, with no branch of its own.
    pure function bin_of(v, edges, nbins, right) result(k)
        real(real64), intent(in) :: v              !! the value to classify.
        real(real64), intent(in) :: edges(:)       !! the bin boundaries, already validated.
        integer(int64), intent(in) :: nbins        !! `size(edges) - 1`.
        logical, intent(in) :: right               !! .true. closes each bin on its upper side.
        integer(int64) :: k                        !! the bin, or 0 for none.
        integer(int64) :: lo, hi, mid

        k = 0_int64
        if (.not. (v >= edges(1))) return
        if (.not. (v <= edges(nbins + 1_int64))) return
        ! A binary search over the interior edges, so an edge array with thousands of bins costs
        ! the same handful of comparisons a four-bin one does. `lo` ends as the largest index with
        ! `edges(lo) <= v` (left-closed) or the largest with `edges(lo) < v` (right-closed); the
        ! two searches differ in exactly the one comparison below, which is the whole convention.
        lo = 1_int64
        hi = nbins + 1_int64
        do while (hi - lo > 1_int64)
            mid = lo + (hi - lo) / 2_int64
            if (right) then
                if (edges(mid) < v) then
                    lo = mid
                else
                    hi = mid
                end if
            else
                if (edges(mid) <= v) then
                    lo = mid
                else
                    hi = mid
                end if
            end if
        end do
        k = lo
        ! **No clamp is needed here, and that is worth stating because one looks obviously
        ! required.** The outermost bin has to close on its otherwise-open side -- the top edge
        ! under `right = .false.`, the bottom edge under `right = .true.` -- and the loop bounds
        ! already do it: `lo` starts at 1 and only ever rises, and the loop exits at
        ! `hi - lo == 1` with `hi <= nbins + 1`, so `1 <= lo <= nbins` unconditionally. A guard
        ! against either end would be dead code that looks like the thing making this correct.
    end function bin_of

    ! ==================================================================================
    ! The public bodies
    ! ==================================================================================

    module procedure bucketize_f64
        logical :: skip, use_right
        integer(int64) :: n, nbins, nnull, nnan, nout, i, k

        n = size(values, kind=int64)
        if (size(codes, kind=int64) /= n) &
            error stop "pf_bucketize: codes has " // trim(stats_i2s(size(codes, kind=int64))) // &
                " elements but values has " // trim(stats_i2s(n))
        call stats_check_sizes(n, "pf_bucketize", is_valid)
        call check_edges(edges, "pf_bucketize", nbins)
        skip = .true.
        if (present(skipnan)) skip = skipnan
        use_right = .false.
        if (present(right)) use_right = right

        nnull = 0_int64
        nnan = 0_int64
        nout = 0_int64
        do i = 1_int64, n
            codes(i) = 0_int32
            if (present(is_valid)) then
                if (.not. is_valid(i)) then
                    nnull = nnull + 1_int64
                    cycle
                end if
            end if
            if (skip) then
                if (values(i) /= values(i)) then
                    nnan = nnan + 1_int64
                    cycle
                end if
            end if
            k = bin_of(values(i), edges, nbins, use_right)
            if (k == 0_int64) then
                nout = nout + 1_int64
            else
                ! Safe as int32 without a guard: `k <= nbins < size(edges)`, and an edge array
                ! that large could not have been passed in the first place.
                codes(i) = int(k, int32)
            end if
        end do
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (present(n_outside)) n_outside = nout
        ! ONE flag with three causes, and the three counts separate them exactly -- the same shape
        ! `pf_zscore` uses, and the reason no second flag is offered.
        if (present(ok)) ok = (nnull + nnan + nout == 0_int64)
    end procedure bucketize_f64

    module procedure histogram_f64
        logical :: skip, use_right
        integer(int64) :: n, nbins, nnull, nnan, nout, i, k
        real(real64) :: w

        n = size(values, kind=int64)
        call stats_check_sizes(n, "pf_histogram", is_valid, weights)
        call check_edges(edges, "pf_histogram", nbins)
        if (size(counts, kind=int64) /= nbins) &
            error stop "pf_histogram: counts has " // trim(stats_i2s(size(counts, kind=int64))) // &
                " elements but " // trim(stats_i2s(size(edges, kind=int64))) // &
                " edges describe " // trim(stats_i2s(nbins)) // " bins"
        skip = .true.
        if (present(skipnan)) skip = skipnan
        use_right = .false.
        if (present(right)) use_right = right

        counts = 0.0_real64
        nnull = 0_int64
        nnan = 0_int64
        nout = 0_int64
        w = 1.0_real64
        do i = 1_int64, n
            if (present(is_valid)) then
                if (.not. is_valid(i)) then
                    nnull = nnull + 1_int64
                    cycle
                end if
            end if
            if (skip) then
                if (values(i) /= values(i)) then
                    nnan = nnan + 1_int64
                    cycle
                end if
            end if
            if (present(weights)) then
                ! The same exclusion ORDER the whole module uses: null, then NaN, then weight. A
                ! weight is never examined for an element that has already left the population,
                ! so garbage sitting where a value is null cannot abort the call.
                call stats_check_weight(weights(i), i, "pf_histogram")
                if (weights(i) <= 0.0_real64) cycle
                w = weights(i)
            end if
            k = bin_of(values(i), edges, nbins, use_right)
            if (k == 0_int64) then
                nout = nout + 1_int64
            else
                counts(k) = counts(k) + w
            end if
        end do
        if (present(n_null)) n_null = nnull
        if (present(n_nan)) n_nan = nnan
        if (present(n_outside)) n_outside = nout
        ! A zero-weight element is deliberately absent from all three counts and from `ok`: it was
        ! removed from the POPULATION, exactly as it is everywhere else in this module, so it is
        ! not an element that failed to reach a bin. `pf_count_valid` is what reports it.
        if (present(ok)) ok = (nnull + nnan + nout == 0_int64)
    end procedure histogram_f64

end submodule parquet_stats_bin ! GCOVR_EXCL_LINE
