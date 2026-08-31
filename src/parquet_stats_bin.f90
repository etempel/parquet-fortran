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
!! the total weight of the codes equal to k over the same arguments -- there is no second
!! edge-search rule that could drift from the first. A test asserts the identity in both edge
!! conventions, weighted and not; keep them sharing `bin_of` and the same exclusion order and it
!! holds by construction.
!!
!! **That is why `pf_bucketize` takes `weights` at all**, since a weight cannot change a bin
!! number. It decides MEMBERSHIP: a zero weight removes the element from the population, here as
!! everywhere else in this module, and a bucketize blind to weights would hand that element an
!! ordinary code while the histogram beside it left the element out.
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
        integer(int64) :: n, nbins, nnull, nnan, nout, i, k, wbits

        n = size(values, kind=int64)
        if (size(codes, kind=int64) /= n) &
            error stop "pf_bucketize: codes has " // trim(stats_i2s(size(codes, kind=int64))) // &
                " elements but values has " // trim(stats_i2s(n))
        call stats_check_sizes(n, "pf_bucketize", is_valid, weights)
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
            if (present(weights)) then
                ! **The whole reason this procedure takes weights**: a zero weight removes the
                ! element from the population everywhere else in this module, and without this
                ! branch a bucketize would give it an ordinary bin code while `pf_histogram` over
                ! the same arguments left it out -- breaking the identity the two are documented
                ! to satisfy. The weight is examined last, in the module's usual exclusion order,
                ! so garbage sitting where a value is null cannot abort the call, and it never
                ! scales anything: a code is a bin number.
                ! One integer compare on the happy path; see STATS_W_LIM. The validator is
                ! reached only by a weight that cannot be valid.
                wbits = transfer(weights(i), 0_int64)
                if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then
                    call stats_check_weight(weights(i), i, "pf_bucketize")
                    if (weights(i) <= 0.0_real64) cycle
                else if (wbits == 0_int64) then
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
        ! `pf_zscore` uses, and the reason no second flag is offered. A zero-weighted element is
        ! deliberately in none of the three and leaves this alone, exactly as it does in
        ! `pf_histogram`: it left the POPULATION rather than failing to reach a bin.
        if (present(ok)) ok = (nnull + nnan + nout == 0_int64)
    end procedure bucketize_f64

    module procedure histogram_f64
        logical :: skip, use_right, as_density
        integer(int64) :: n, nbins, nnull, nnan, nout, i, k, wbits
        real(real64) :: w, base

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
        as_density = .false.
        if (present(density)) as_density = density

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
                ! One integer compare on the happy path; see STATS_W_LIM. The validator is
                ! reached only by a weight that cannot be valid.
                wbits = transfer(weights(i), 0_int64)
                if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then
                    call stats_check_weight(weights(i), i, "pf_histogram")
                    if (weights(i) <= 0.0_real64) cycle
                else if (wbits == 0_int64) then
                    cycle
                end if
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
        if (.not. as_density) return

        ! **The normalisation base is what was BINNED, not what was passed**, which is numpy's
        ! rule: a value outside the edges was never counted, so it cannot appear in the total it
        ! is not part of. Summed here rather than accumulated in the loop above so that the two
        ! answers cannot drift -- the base is by construction the sum of the numbers being
        ! divided, whatever the loop did.
        base = sum(counts)
        if (base > 0.0_real64) then
            do k = 1_int64, nbins
                ! Each bin by its OWN width, so an uneven edge spacing is accounted for and
                ! `sum(counts * widths)` is 1. An infinite outer edge gives that bin a density of
                ! exactly 0, which is the honest answer and comes free from the arithmetic.
                counts(k) = counts(k) / ((edges(k + 1_int64) - edges(k)) * base)
            end do
        else
            ! Nothing was binned, so there is no density -- the module's standing rule for an
            ! undefined answer applies, and note it makes an empty DENSITY differ from an empty
            ! HISTOGRAM, which is all zeros with `ok = .true.` and perfectly well defined.
            counts = stats_nan()
            if (present(ok)) ok = .false.
        end if
    end procedure histogram_f64

    !> `pf_bin_edges`: `nbins` equal-width boundaries spanning the population's own range.
    !!
    !! **The strict-increase guarantee is the whole design constraint.** These edges exist to be
    !! handed to `pf_histogram`, which ABORTS on a pair that is not strictly increasing -- so a
    !! degenerate population must not produce edges that abort one call later, and every branch
    !! below ends in a usable array. `ok` reports that the edges do not describe the data's own
    !! range; it never means "unusable".
    !!
    !! There are four such branches, and the range scan's non-finite exclusion is the one that is
    !! easy to leave out: an empty population, a constant one, a span too fine for double
    !! precision to resolve into `nbins` distinct edges, and a population holding an infinity or
    !! (under `skipnan = .false.`) a NaN. All four set `ok = .false.` and all four still return
    !! strictly increasing edges.
    module procedure bin_edges_f64
        real(real64) :: lo, hi, span, e
        integer(int64) :: n, i, k, kept, wbits
        integer :: j
        logical :: skip, fine, nonfinite

        n = size(values, kind=int64)
        if (nbins < 1) &
            error stop "pf_bin_edges: nbins must be at least 1, but is " // &
                trim(stats_i2s(int(nbins, int64)))
        if (size(edges, kind=int64) /= int(nbins, int64) + 1_int64) &
            error stop "pf_bin_edges: edges has " // trim(stats_i2s(size(edges, kind=int64))) // &
                " elements but " // trim(stats_i2s(int(nbins, int64))) // &
                " bins need one more boundary than that"
        call stats_check_sizes(n, "pf_bin_edges", is_valid, weights)
        skip = .true.
        if (present(skipnan)) skip = skipnan

        ! The range is taken over the population `pf_histogram` would BIN, exclusions and all --
        ! otherwise a zero-weighted extreme would stretch the edges over data that never lands in
        ! one. The same three exclusions in the same order as everywhere else in this module.
        kept = 0_int64
        nonfinite = .false.
        lo = 0.0_real64
        hi = 0.0_real64
        if (present(n_null)) n_null = 0_int64
        if (present(n_nan)) n_nan = 0_int64
        do i = 1_int64, n
            if (present(is_valid)) then
                if (.not. is_valid(i)) then
                    if (present(n_null)) n_null = n_null + 1_int64
                    cycle
                end if
            end if
            if (skip) then
                if (values(i) /= values(i)) then
                    if (present(n_nan)) n_nan = n_nan + 1_int64
                    cycle
                end if
            end if
            if (present(weights)) then
                ! One integer compare on the happy path; see STATS_W_LIM. The validator is
                ! reached only by a weight that cannot be valid.
                wbits = transfer(weights(i), 0_int64)
                if (wbits < 0_int64 .or. wbits >= STATS_W_LIM) then
                    call stats_check_weight(weights(i), i, "pf_bin_edges")
                    if (weights(i) <= 0.0_real64) cycle
                else if (wbits == 0_int64) then
                    cycle
                end if
            end if
            ! **A non-finite value joins the population but never the RANGE**, and that is a
            ! contract requirement rather than a policy choice. An infinity reached here would
            ! make `span` infinite (or NaN, with both signs present), every interior edge would
            ! come out infinite, and the repair below cannot separate two infinities -- so the
            ! edges would fail the strict-increase test `pf_histogram` aborts on, which is
            ! precisely what these edges exist to satisfy. A NaN is excluded here even under
            ! `skipnan = .false.`, where it is otherwise a member of the population: it is still
            ! not a number this scan can order. numpy raises `ValueError` on the same input; this
            ! module never aborts on a data condition, so the range is taken over the finite
            ! survivors and `ok = .false.` says the edges do not span the data's own range.
            !
            ! The counter deliberately advances only for a FINITE survivor, so a population that
            ! is entirely non-finite takes the empty fallback below rather than the constant one.
            if (values(i) /= values(i)) then
                nonfinite = .true.
                cycle
            end if
            if (abs(values(i)) > huge(0.0_real64)) then
                nonfinite = .true.
                cycle
            end if
            kept = kept + 1_int64
            if (kept == 1_int64) then
                lo = values(i)
                hi = values(i)
            else
                if (values(i) < lo) lo = values(i)
                if (values(i) > hi) hi = values(i)
            end if
        end do

        ! numpy's two degenerate answers, reproduced rather than invented: an EMPTY population
        ! falls back to [0, 1] and a CONSTANT one widens to [x - 0.5, x + 0.5]. Both are arbitrary
        ! -- there is no range to describe -- which is exactly what `ok = .false.` says.
        fine = .not. nonfinite
        if (kept == 0_int64) then
            lo = 0.0_real64
            hi = 1.0_real64
            fine = .false.
        else if (.not. (hi > lo)) then
            lo = lo - 0.5_real64
            hi = hi + 0.5_real64
            fine = .false.
        end if

        span = hi - lo
        edges(1) = lo
        do j = 2, nbins
            edges(j) = lo + real(j - 1, real64) * span / real(nbins, real64)
        end do
        ! The top boundary is assigned rather than computed, so that the highest value in the
        ! population is never left one ulp outside the last bin by a rounded multiplication.
        edges(nbins + 1) = hi

        ! **The repair, and why it cannot be skipped.** When `nbins` is finer than double
        ! precision can resolve over the range -- reachable with a relative span of 1e-12 and ten
        ! thousand bins, which is an ordinary thing to ask of nearly-constant data -- the spacing
        ! underflows and neighbouring edges come out equal. Nudging each one past its predecessor
        ! keeps the strict-increase contract at the smallest possible cost, and `ok = .false.`
        ! says the result no longer spans exactly what was asked for. (Reaching `+Infinity` here
        ! would need the data to sit within `nbins` ulps of `huge`; such an edge is still strictly
        ! greater than its predecessor, so it satisfies the contract and merely leaves the top bin
        ! unreachable.)
        do k = 2_int64, int(nbins, int64) + 1_int64
            if (.not. (edges(k) > edges(k - 1_int64))) then
                e = nearest(edges(k - 1_int64), 1.0_real64)
                edges(k) = e
                fine = .false.
            end if
        end do
        if (present(ok)) ok = fine
    end procedure bin_edges_f64

end submodule parquet_stats_bin ! GCOVR_EXCL_LINE
