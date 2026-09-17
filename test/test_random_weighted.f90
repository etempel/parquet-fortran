!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> The distributional gate for the weighted draw -- written BEFORE the sampler it will judge.
!>
!> Successive sampling (draw one with probability proportional to weight, remove it, renormalise
!> over the survivors, repeat) has a closed form only for tiny populations, and that is exactly
!> what this suite exploits: at `m = 10` with 3 draws every ordered triple of distinct items can
!> be ENUMERATED, so the reference is exact arithmetic rather than a second sampler. A candidate
!> passes when its empirical position-by-position frequencies sit inside a few standard errors of
!> that enumeration.
!>
!> **Why this suite exists before any weighted sampler does.** A distributional test written after
!> an implementation tends to encode whatever that implementation does; written first it is the
!> specification. It is also the only test that can see the class of defect that once let
!> `pf_random_at` and `pf_random_int_at` alias the same 64 bits: every structural check passed
!> while the distribution was wrong by 36 standard errors. Structure is cheap to satisfy by
!> accident, distribution is not.
!>
!> **Three tests, and the order matters.** The enumeration is checked against a closed form; a
!> reference sampler that is obviously correct is checked against the enumeration; and a sampler
!> that deliberately ignores the weights is required to FAIL. Without the third, a loose tolerance
!> would let the gate pass everything, which is the failure mode a gate cannot afford.
module test_random_weighted

    ! NARROW import, not `use parquet`. The facade would compile just as well and would let a
    ! future test in this file reach the C++ layer without anything saying so; naming the tier
    ! makes that a build error instead. The facade's own re-export of this tier is pinned by
    ! `test_facade_covers_every_layer` (test/test_examples.f90), which is where that claim lives.
    use parquet_sampling
    use parquet_random
    use iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none
    private
    public :: collect_tests_parquet_random_weighted

    !> Population the gate enumerates. `m**3` triples, so this stays small on purpose.
    integer, parameter :: MC = 10
    !> Draws taken per trial; the enumeration is written for exactly three positions.
    integer, parameter :: KD = 3
    !> The seed every arm uses. Any value would do; a fixed one keeps the suite deterministic.
    integer(int64), parameter :: gate_seed = 20260818_int64

    !> Candidate selectors for `gate_worst_z`.
    integer, parameter :: ARM_REFERENCE = 1  !! the linear-scan reference sampler
    integer, parameter :: ARM_UNWEIGHTED = 2 !! the same walk ignoring the weights -- the control
    integer, parameter :: ARM_TREE = 3       !! the library's own `pf_weighted_draw`
    integer, parameter :: ARM_RACE = 4       !! the library's own `pf_weighted_permutation`

contains

    !> Registers every test in the `random_weighted` suite.
    subroutine collect_tests_parquet_random_weighted(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the suite's tests

        testsuite = [ &
            new_unittest("the enumeration reproduces the closed form for equal weights", &
                         test_enumeration_closed_form), &
            new_unittest("a reference successive sampler matches the enumeration", &
                         test_reference_matches_enumeration), &
            new_unittest("the gate rejects a sampler that ignores the weights", &
                         test_gate_rejects_unweighted), &
            new_unittest("pf_weighted_draw matches the enumeration", test_tree_matches_enumeration), &
            new_unittest("a drained sampler returns every item exactly once", test_exhaustion), &
            new_unittest("18 decades of weight still give n distinct items", test_wide_dynamic_range), &
            new_unittest("%reset repeats the same sequence", test_reset_repeats_the_sequence), &
            new_unittest("%reseed equals a freshly built sampler", test_reseed_equals_fresh), &
            new_unittest("%init's stream index is optional and defaults to 0", test_init_default_stream), &
            new_unittest("pf_weighted_subset is k calls to %next", test_subset_identities), &
            new_unittest("zero-weight items come last in seed-dependent order", test_zero_weight_tail), &
            new_unittest("the same coordinates give the same sequence", test_reproducibility), &
            new_unittest("pf_weighted_permutation matches the enumeration", test_race_matches_enumeration), &
            new_unittest("a race returns every item exactly once", test_race_is_a_permutation), &
            new_unittest("the race is bit-identical at every thread count", test_race_thread_identity), &
            new_unittest("zero-weight items come last in the race too", test_race_zero_weight_tail), &
            new_unittest("the two families differ in realization", test_families_differ), &
            new_unittest("the two families are independent at one coordinate", &
                         test_families_independent), &
            new_unittest("an unorderably small weight is treated as zero-weight", &
                         test_subnormal_weight_is_zero), &
            new_unittest("every subset and permutation specific reaches the same worker", &
                         test_weighted_specifics) &
            ]
    end subroutine collect_tests_parquet_random_weighted

    ! ---- The reference: exact, enumerated, no sampling anywhere ----

    !> Exact position-by-position probabilities under successive sampling, by enumeration.
    !!
    !! `p(t, i)` is the probability that draw `t` is item `i`. Every ordered triple of distinct
    !! items is visited and its exact probability attributed to the three positions it fills, so
    !! this is `O(m**3)` exact arithmetic with nothing sampled. It is the reference the whole gate
    !! rests on, which is why `test_enumeration_closed_form` checks it against a case whose answer
    !! is known independently rather than trusting it.
    pure subroutine exact_positions(w, p)
        real(real64), intent(in) :: w(:)        !! the weights; every one strictly positive
        real(real64), intent(out) :: p(:, :)    !! `p(t, i)`, shape `(KD, size(w))`
        real(real64) :: tot, pi_, pij, pijk
        integer :: i, j, k, m

        m = size(w)
        tot = sum(w)
        p = 0.0_real64
        do i = 1, m
            pi_ = w(i) / tot
            do j = 1, m
                if (j == i) cycle
                pij = pi_ * w(j) / (tot - w(i))
                do k = 1, m
                    if (k == i .or. k == j) cycle
                    pijk = pij * w(k) / (tot - w(i) - w(j))
                    p(1, i) = p(1, i) + pijk
                    p(2, j) = p(2, j) + pijk
                    p(3, k) = p(3, k) + pijk
                end do
            end do
        end do
    end subroutine exact_positions

    !> The gate's weight fixture: ten distinct, deliberately NOT exactly representable weights.
    !!
    !! `1 + 1.5*i/7` spans roughly 1.2 to 2.6 and only one of its ten values (`i = 7`, giving 2.5)
    !! lands on a representable weight. That choice is not cosmetic: a fixture of 1.0 and 2.5 makes
    !! a whole class of accumulated-rounding defect invisible, so here the fixture IS part of the
    !! test. Distinct weights also mean every one of the 30 cells carries its own probability, so a
    !! sampler cannot pass by getting a symmetry right.
    pure subroutine gate_weights(w)
        real(real64), intent(out) :: w(:)       !! the weights, filled for `size(w)` items
        integer :: i

        do i = 1, size(w)
            w(i) = 1.0_real64 + 1.5_real64 * real(i, real64) / 7.0_real64
        end do
    end subroutine gate_weights

    ! ---- The candidates ----

    !> Successive sampling by a linear scan over the survivors, correct by construction.
    !!
    !! `O(m)` per draw where the shipped sampler will be `O(log m)`, which is exactly what makes it
    !! the right thing to prove the gate with before that sampler exists: it is slow, obvious, and
    !! independent of whatever the real implementation will do.
    !!
    !! Two details are deliberate. It consumes ONE uniform per draw on the **draw axis**, indexed
    !! by position within the sequence, with the trial carried on the stream axis -- the same
    !! coordinate convention the weighted sampler will use, so the gate exercises the addressing as
    !! well as the distribution. And `target` is clamped strictly below the running total, because
    !! `u` may be as large as `1 - 2**-53` and `u * tot` is a rounded product that can land exactly
    !! on `tot`; without the clamp the scan falls off the end and returns a spent item.
    !!
    !! `weighted = .false.` gives the negative control: the identical walk over survivors with
    !! every weight replaced by one.
    subroutine reference_draw(sd, stream, w, weighted, drawn)
        integer(int64), intent(in) :: sd        !! the seed
        integer(int64), intent(in) :: stream    !! which sequence; one per trial
        real(real64), intent(in) :: w(:)        !! the weights
        logical, intent(in) :: weighted         !! `.false.` ignores `w` -- the negative control
        integer, intent(out) :: drawn(:)        !! the items drawn, in order
        real(real64) :: rem(size(w)), tot, u, target, acc
        integer :: t, i, pick

        if (weighted) then
            rem = w
        else
            rem = 1.0_real64
        end if
        do t = 1, size(drawn)
            tot = sum(rem)
            u = pf_random_at(sd, stream, draw=int(t, int64))
            target = u * tot
            if (target >= tot) target = tot * (1.0_real64 - epsilon(1.0_real64))
            acc = 0.0_real64
            pick = 0
            do i = 1, size(w)
                acc = acc + rem(i)
                if (target < acc) then
                    pick = i
                    exit
                end if
            end do
            drawn(t) = pick
            if (pick > 0) rem(pick) = 0.0_real64
        end do
    end subroutine reference_draw

    ! ---- The gate itself ----

    !> Runs `ntrial` trials of one candidate and reports the worst per-cell deviation, in units of
    !! that cell's own Monte Carlo standard error.
    !!
    !! Scoring each of the 30 cells against `sqrt(p(1-p)/N)` rather than against one global
    !! tolerance is what lets a single threshold serve cells whose probabilities differ by a factor
    !! of two. `ok_shape` reports whether every trial returned three distinct, in-range items --
    !! kept separate from the z-score because a sampler that returns duplicates has failed in a way
    !! no distributional statistic should be asked to express.
    subroutine gate_worst_z(sd, w, exact, arm, ntrial, worst_z, ok_shape)
        integer(int64), intent(in) :: sd            !! the seed
        real(real64), intent(in) :: w(:)            !! the weights
        real(real64), intent(in) :: exact(:, :)     !! `exact(t, i)` from `exact_positions`
        integer, intent(in) :: arm                  !! which candidate: see `ARM_*` below
        integer, intent(in) :: ntrial               !! trials to run
        real(real64), intent(out) :: worst_z        !! worst `|emp - exact| / se` over all cells
        logical, intent(out) :: ok_shape            !! every trial gave distinct, in-range items
        type(pf_weighted_draw) :: d
        real(real64) :: emp(KD, size(w)), se, z, p
        integer :: drawn(KD), full(size(w)), t, i, j
        logical :: got

        emp = 0.0_real64
        ok_shape = .true.
        if (arm == ARM_TREE) call d%init(w, sd)
        do t = 1, ntrial
            select case (arm)
            case (ARM_TREE)
                ! %reseed rather than a fresh %init: it drives the library through the UC3 shape,
                ! and it means every one of these trials also exercises the undo journal.
                call d%reseed(sd, stream=t)
                do i = 1, KD
                    call d%next(drawn(i), got)
                    if (.not. got) drawn(i) = 0
                end do
            case (ARM_RACE)
                ! The race produces a whole permutation; its first KD positions must follow the
                ! same distribution as KD sequential draws, which is what "same distribution,
                ! different realization" means and is asserted nowhere else.
                call pf_weighted_permutation(full, w, sd, stream=int(t, int64))
                drawn = full(1:KD)
            case default
                call reference_draw(sd, int(t, int64), w, arm == ARM_REFERENCE, drawn)
            end select
            do i = 1, KD
                if (drawn(i) < 1 .or. drawn(i) > size(w)) then
                    ok_shape = .false.
                    cycle
                end if
                do j = 1, i - 1
                    if (drawn(j) == drawn(i)) ok_shape = .false.
                end do
                emp(i, drawn(i)) = emp(i, drawn(i)) + 1.0_real64
            end do
        end do
        emp = emp / real(ntrial, real64)
        worst_z = 0.0_real64
        do i = 1, KD
            do j = 1, size(w)
                p = exact(i, j)
                if (p <= 0.0_real64 .or. p >= 1.0_real64) cycle
                se = sqrt(p * (1.0_real64 - p) / real(ntrial, real64))
                z = abs(emp(i, j) - p) / se
                if (z > worst_z) worst_z = z
            end do
        end do
    end subroutine gate_worst_z

    ! ---- Tests ----

    !> The enumeration must reproduce a case whose answer is known without it.
    !!
    !! With every weight equal, successive sampling is a uniform random permutation, so item `i`
    !! occupies position `t` with probability exactly `1/m` for every one of the 30 cells. That is
    !! a closed form the enumeration cannot get right by accident, and checking it first is what
    !! makes the other two tests evidence about the samplers rather than about this subroutine.
    !!
    !! The row sums are asserted too: each position is filled exactly once per trial, so every row
    !! must total 1. A transcription error that swapped two positions would survive the uniform
    !! check and die here.
    subroutine test_enumeration_closed_form(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        real(real64) :: w(MC), p(KD, MC), rowsum
        integer :: t, i

        w = 1.0_real64
        call exact_positions(w, p)
        call check(error, all(abs(p - 1.0_real64 / real(MC, real64)) < 1.0e-12_real64), &
                   "equal weights: every cell must be exactly 1/m")
        if (allocated(error)) return

        call gate_weights(w)
        call exact_positions(w, p)
        do t = 1, KD
            rowsum = sum(p(t, :))
            call check(error, abs(rowsum - 1.0_real64) < 1.0e-12_real64, &
                       "unequal weights: each position's probabilities must sum to 1")
            if (allocated(error)) return
        end do
        do i = 1, MC
            call check(error, p(1, i) > 0.0_real64 .and. p(1, i) < 1.0_real64, &
                       "unequal weights: every first-draw probability must be strictly interior")
            if (allocated(error)) return
        end do
        call check(error, abs(p(1, MC) - w(MC) / sum(w)) < 1.0e-12_real64, &
                   "first draw must be exactly proportional to weight")
    end subroutine test_enumeration_closed_form

    !> The reference sampler must reproduce the enumeration -- this is the gate proving it works.
    subroutine test_reference_matches_enumeration(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: ntrial = 200000
        real(real64) :: w(MC), p(KD, MC), worst_z
        logical :: ok_shape
        character(len=160) :: msg

        call gate_weights(w)
        call exact_positions(w, p)
        call gate_worst_z(gate_seed, w, p, ARM_REFERENCE, ntrial, worst_z, ok_shape)

        call check(error, ok_shape, "every trial must return KD distinct, in-range items")
        if (allocated(error)) return
        write(msg, '(a,f8.3,a)') "reference sampler deviates from exact successive sampling: " // &
            "worst cell z = ", worst_z, " (want < 5)"
        call check(error, worst_z < 5.0_real64, trim(msg))
    end subroutine test_reference_matches_enumeration

    !> The negative control: a sampler ignoring the weights must be REJECTED, and by a wide margin.
    !!
    !! Without this the gate could be passing because its tolerance is loose rather than because
    !! the candidate is right, and nothing in a green run would say which. The unweighted walk is
    !! the smallest wrong sampler that still returns a valid permutation prefix -- distinct items,
    !! in range, every position filled -- so it is precisely the candidate every structural check
    !! waves through. A far smaller trial count is enough because the effect is enormous; the
    !! assertion is on the margin, not merely on failure, so a gate that had lost its power would
    !! be visible here rather than merely borderline.
    subroutine test_gate_rejects_unweighted(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: ntrial = 20000
        real(real64) :: w(MC), p(KD, MC), worst_z
        logical :: ok_shape
        character(len=160) :: msg

        call gate_weights(w)
        call exact_positions(w, p)
        call gate_worst_z(gate_seed, w, p, ARM_UNWEIGHTED, ntrial, worst_z, ok_shape)

        call check(error, ok_shape, "the control must still return KD distinct, in-range items")
        if (allocated(error)) return
        write(msg, '(a,f10.2,a)') "the gate failed to reject an unweighted sampler: " // &
            "worst cell z = ", worst_z, " (want > 20)"
        call check(error, worst_z > 20.0_real64, trim(msg))
    end subroutine test_gate_rejects_unweighted

    !> The whole point of stage 1: the library's sampler faces the same exact reference.
    subroutine test_tree_matches_enumeration(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: ntrial = 200000
        real(real64) :: w(MC), p(KD, MC), worst_z
        logical :: ok_shape
        character(len=160) :: msg

        call gate_weights(w)
        call exact_positions(w, p)
        call gate_worst_z(gate_seed, w, p, ARM_TREE, ntrial, worst_z, ok_shape)

        call check(error, ok_shape, "every trial must return KD distinct, in-range items")
        if (allocated(error)) return
        write(msg, '(a,f8.3,a)') "pf_weighted_draw deviates from exact successive sampling: " // &
            "worst cell z = ", worst_z, " (want < 5)"
        call check(error, worst_z < 5.0_real64, trim(msg))
    end subroutine test_tree_matches_enumeration

    !> A drained sampler returns every item exactly once, then reports exhaustion.
    !!
    !! The fixture's weights are deliberately not exactly representable. At weights 1.0 and 2.5 an
    !! ancestor maintained by subtraction accumulates no error at all, so the defect this test
    !! exists to catch would be invisible -- here the fixture IS the test.
    subroutine test_exhaustion(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 500
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: seen(n), item, k, nout
        logical :: ok

        do k = 1, n
            w(k) = 1.0_real64 + 1.5_real64 * real(k, real64) / 7.0_real64
        end do
        call d%init(w, gate_seed)
        call check(error, d%remaining() == int(n, int64), "a fresh sampler must have n remaining")
        if (allocated(error)) return

        seen = 0
        nout = 0
        do
            call d%next(item, ok)
            if (.not. ok) exit
            nout = nout + 1
            call check(error, item >= 1 .and. item <= n, "every drawn item must be in range")
            if (allocated(error)) return
            seen(item) = seen(item) + 1
        end do
        call check(error, nout == n, "a drained sampler must return exactly n items")
        if (allocated(error)) return
        call check(error, all(seen == 1), "every item must be returned exactly once")
        if (allocated(error)) return
        call check(error, d%remaining() == 0_int64, "%remaining must reach 0")
        if (allocated(error)) return
        call d%next(item, ok)
        call check(error, .not. ok, "%next past exhaustion must report ok = .false.")
        if (allocated(error)) return
        call check(error, d%remaining() == 0_int64, "%remaining must not go negative past exhaustion")
    end subroutine test_exhaustion

    !> Weights spanning eighteen decades still yield exactly n distinct items.
    !!
    !! This is the shape that broke a subtractive tree during design: the root loses contact with
    !! the true remaining weight, the descent lands on spent leaves, and the sampler starts
    !! returning duplicates while every other check still passes.
    subroutine test_wide_dynamic_range(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 400
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: seen(n), item, k, nout
        logical :: ok

        do k = 1, n
            w(k) = 10.0_real64 ** (real(mod(k, 19), real64) - 9.0_real64)
        end do
        call d%init(w, gate_seed, stream=3)
        seen = 0
        nout = 0
        do
            call d%next(item, ok)
            if (.not. ok) exit
            nout = nout + 1
            seen(item) = seen(item) + 1
        end do
        call check(error, nout == n, "18 decades of weight: exactly n items must come back")
        if (allocated(error)) return
        call check(error, all(seen == 1), "18 decades of weight: no duplicate and no omission")
    end subroutine test_wide_dynamic_range

    !> `%reset` restores the tree so exactly that the same sequence comes back element for element.
    subroutine test_reset_repeats_the_sequence(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 200, k = 60
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: a(k), b(k), i
        logical :: ok

        call spread_weights(w)
        call d%init(w, gate_seed, stream=11)
        do i = 1, k
            call d%next(a(i), ok)
        end do
        call d%reset()
        call check(error, d%remaining() == int(n, int64), "%reset must restore %remaining to n")
        if (allocated(error)) return
        do i = 1, k
            call d%next(b(i), ok)
        end do
        call check(error, all(a == b), "%reset must reproduce the same sequence exactly")
    end subroutine test_reset_repeats_the_sequence

    !> A reseeded sampler equals a freshly built one -- what makes the undo journal trustworthy.
    !!
    !! This is the assertion the per-thread-array idiom rests on, since there every sampler is
    !! reseeded rather than rebuilt. It is checked after a PARTIAL drain and after a FULL one: the
    !! full drain is what proves the journal's growth path and the deepest replay, and a
    !! partial-only test would leave both unexercised.
    subroutine test_reseed_equals_fresh(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 200, k = 40
        type(pf_weighted_draw) :: d, fresh
        real(real64) :: w(n)
        integer :: a(k), b(k), i, junk
        logical :: ok

        call spread_weights(w)
        call d%init(w, gate_seed)
        do i = 1, k                          ! a partial sequence ...
            call d%next(junk, ok)
        end do
        call d%reseed(gate_seed, stream=77)
        do i = 1, k
            call d%next(a(i), ok)
        end do

        call fresh%init(w, gate_seed, stream=77)
        do i = 1, k
            call fresh%next(b(i), ok)
        end do
        call check(error, all(a == b), &
                   "after a partial drain, %reseed must equal a freshly built sampler")
        if (allocated(error)) return

        do                                   ! ... and now a full one
            call d%next(junk, ok)
            if (.not. ok) exit
        end do
        call d%reseed(gate_seed, stream=77)
        do i = 1, k
            call d%next(a(i), ok)
        end do
        call check(error, all(a == b), &
                   "after a full drain, %reseed must equal a freshly built sampler")
        if (allocated(error)) return

        call d%reseed(gate_seed, stream=78)
        do i = 1, k
            call d%next(a(i), ok)
        end do
        call check(error, .not. all(a == b), "a different stream must give a different sequence")
    end subroutine test_reseed_equals_fresh

    !> `%init`'s `stream` is optional and defaults to **0**, matching `pf_random_stream%seed`.
    !!
    !! `doc/pages/utilities/random.md` shows only the two-argument form, so the third argument is
    !! easy to miss entirely; this pins that it exists and which sequence omitting it selects.
    !!
    !! **Negative control:** stream 1 must give a different sequence. Without it the test passes
    !! against an `%init` that accepts the argument and discards it.
    subroutine test_init_default_stream(error)
        type(error_type), allocatable, intent(out) :: error  !! test-drive error handle
        type(pf_weighted_draw) :: a, b, c
        real(real64), parameter :: w(6) = [5.0_real64, 1.0_real64, 3.0_real64, &
                                           2.0_real64, 4.0_real64, 1.5_real64]
        integer(int64), parameter :: s = 20260822_int64
        integer :: ia, ib, ic, k
        logical :: oka, okb, okc, differs

        call a%init(w, s)
        call b%init(w, s, 0_int64)
        call c%init(w, s, 1_int64)

        differs = .false.
        do k = 1, size(w)
            call a%next(ia, oka)
            call b%next(ib, okb)
            call c%next(ic, okc)
            call check(error, ia == ib, "%init(w, s) must be %init(w, s, 0): the default stream is 0")
            if (allocated(error)) return
            call check(error, oka .eqv. okb, "%init(w, s) and %init(w, s, 0) must drain alike")
            if (allocated(error)) return
            if (ia /= ic) differs = .true.
        end do

        call check(error, differs, &
            "%init(w, s, 1) drew the same items as %init(w, s), so the stream argument is " // &
            "being ignored and the assertions above prove nothing")
    end subroutine test_init_default_stream

    !> `pf_weighted_subset` is k calls to `%next`, and a prefix of a longer subset.
    subroutine test_subset_identities(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 300, k = 25
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: viaSub(k), viaNext(k), longer(2 * k), i
        integer(int64) :: wide(k)
        logical :: ok

        call spread_weights(w)
        call pf_weighted_subset(viaSub, w, gate_seed, stream=5)
        call d%init(w, gate_seed, stream=5)
        do i = 1, k
            call d%next(viaNext(i), ok)
        end do
        call check(error, all(viaSub == viaNext), &
                   "pf_weighted_subset must equal k calls to %next")
        if (allocated(error)) return

        call pf_weighted_subset(longer, w, gate_seed, stream=5)
        call check(error, all(longer(1:k) == viaSub), &
                   "a subset of size k must be a prefix of one of size 2k")
        if (allocated(error)) return

        call pf_weighted_subset(wide, w, gate_seed, stream=5)
        call check(error, all(int(wide, kind(viaSub)) == viaSub), &
                   "the int64 and int32 subset forms must agree")
    end subroutine test_subset_identities

    !> Zero-weight items come last, exactly once each, in an order that depends on the seed.
    subroutine test_zero_weight_tail(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 60, nz = 20
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: seen(n), order_a(nz), order_b(nz), item, k, nout
        logical :: ok, is_zero(n)

        call spread_weights(w)
        is_zero = .false.
        do k = 1, nz
            w(3 * k) = 0.0_real64            ! scattered, not a contiguous block at the end
            is_zero(3 * k) = .true.
        end do

        call d%init(w, gate_seed)
        seen = 0
        nout = 0
        do
            call d%next(item, ok)
            if (.not. ok) exit
            nout = nout + 1
            seen(item) = seen(item) + 1
            if (nout <= n - nz) then
                call check(error, .not. is_zero(item), &
                           "no zero-weight item may be drawn while a positive weight remains")
                if (allocated(error)) return
            else
                call check(error, is_zero(item), "the tail must hold only zero-weight items")
                if (allocated(error)) return
                order_a(nout - (n - nz)) = item
            end if
        end do
        call check(error, nout == n, "a drained sampler must return every item, zero-weight included")
        if (allocated(error)) return
        call check(error, all(seen == 1), "every item, zero-weight included, must appear exactly once")
        if (allocated(error)) return

        ! "Uniform random order" is untested unless the order is shown to move with the seed.
        call d%reseed(gate_seed, stream=9)
        nout = 0
        do
            call d%next(item, ok)
            if (.not. ok) exit
            nout = nout + 1
            if (nout > n - nz) order_b(nout - (n - nz)) = item
        end do
        call check(error, .not. all(order_a == order_b), &
                   "the zero-weight tail's order must depend on the coordinates")
    end subroutine test_zero_weight_tail

    !> The same coordinates give the same sequence; different ones do not.
    subroutine test_reproducibility(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 150, k = 30
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: a(k), b(k), i
        logical :: ok

        call spread_weights(w)
        call pf_weighted_subset(a, w, gate_seed, stream=2)
        call pf_weighted_subset(b, w, gate_seed, stream=2)
        call check(error, all(a == b), "the same coordinates must give the same sequence")
        if (allocated(error)) return

        call pf_weighted_subset(b, w, gate_seed + 1_int64, stream=2)
        call check(error, .not. all(a == b), "a different seed must give a different sequence")
        if (allocated(error)) return

        call pf_weighted_subset(b, w, gate_seed, stream=3)
        call check(error, .not. all(a == b), "a different stream must give a different sequence")
        if (allocated(error)) return

        ! The int32 and int64 stream forms name the same sequence, not two.
        call d%init(w, gate_seed, stream=2_int64)
        do i = 1, k
            call d%next(b(i), ok)
        end do
        call check(error, all(a == b), "the int32 and int64 stream forms must agree")
    end subroutine test_reproducibility

    !> A shared fixture with distinct, awkward weights and a wide-ish spread.
    pure subroutine spread_weights(w)
        real(real64), intent(out) :: w(:)   !! filled for `size(w)` items
        integer :: i

        do i = 1, size(w)
            w(i) = 0.25_real64 + real(mod(i, 13), real64) / 3.0_real64 + real(i, real64) / 97.0_real64
        end do
    end subroutine spread_weights

    !> The race faces the same exact reference the tree does.
    subroutine test_race_matches_enumeration(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: ntrial = 100000
        real(real64) :: w(MC), p(KD, MC), worst_z
        logical :: ok_shape
        character(len=160) :: msg

        call gate_weights(w)
        call exact_positions(w, p)
        call gate_worst_z(gate_seed, w, p, ARM_RACE, ntrial, worst_z, ok_shape)

        call check(error, ok_shape, "every race must return KD distinct, in-range items")
        if (allocated(error)) return
        write(msg, '(a,f8.3,a)') "pf_weighted_permutation deviates from exact successive " // &
            "sampling: worst cell z = ", worst_z, " (want < 5)"
        call check(error, worst_z < 5.0_real64, trim(msg))
    end subroutine test_race_matches_enumeration

    !> A race returns every item exactly once, whatever the weights.
    subroutine test_race_is_a_permutation(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 500
        real(real64) :: w(n)
        integer :: perm(n), seen(n), i

        call spread_weights(w)
        call pf_weighted_permutation(perm, w, gate_seed, stream=4_int64)
        seen = 0
        do i = 1, n
            call check(error, perm(i) >= 1 .and. perm(i) <= n, "every entry must be a valid item")
            if (allocated(error)) return
            seen(perm(i)) = seen(perm(i)) + 1
        end do
        call check(error, all(seen == 1), "a weighted permutation must contain every item once")
    end subroutine test_race_is_a_permutation

    !> The race is bit-identical at every thread count -- the property that motivates it.
    !!
    !! Asserted at several counts rather than assumed from the construction. `threads=` may only
    !! change how fast the array is filled, never what is in it; an implementation that let the
    !! sort's parallel path reorder equal keys differently would break exactly here.
    subroutine test_race_thread_identity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 4000
        real(real64) :: w(n)
        integer :: base(n), other(n), t
        integer, parameter :: counts(4) = [1, 2, 4, 8]
        character(len=120) :: msg

        call spread_weights(w)
        call pf_weighted_permutation(base, w, gate_seed, stream=1_int64, threads=1)
        do t = 1, size(counts)
            call pf_weighted_permutation(other, w, gate_seed, stream=1_int64, threads=counts(t))
            write(msg, '(a,i0,a)') "the race must be bit-identical at threads=", counts(t), &
                " and threads=1"
            call check(error, all(other == base), trim(msg))
            if (allocated(error)) return
        end do
        call pf_weighted_permutation(other, w, gate_seed, stream=1_int64)
        call check(error, all(other == base), "automatic threading must give the same answer too")
    end subroutine test_race_thread_identity

    !> Zero-weight items come last in the race too, in an order that moves with the coordinates.
    !!
    !! They are held OUT of the race rather than given a sentinel key, and this is the test that
    !! says why that matters: tied sentinel keys would come back in index order, because every sort
    !! in `parquet_sorting` is stable. "Uniform random order" would then be false while every other
    !! assertion here still passed.
    subroutine test_race_zero_weight_tail(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 90, nz = 30
        real(real64) :: w(n)
        integer :: perm(n), a(nz), b(nz), i, k
        logical :: is_zero(n)

        call spread_weights(w)
        is_zero = .false.
        do k = 1, nz
            w(3 * k) = 0.0_real64
            is_zero(3 * k) = .true.
        end do

        call pf_weighted_permutation(perm, w, gate_seed, stream=1_int64)
        do i = 1, n - nz
            call check(error, .not. is_zero(perm(i)), &
                       "no zero-weight item may appear before the tail")
            if (allocated(error)) return
        end do
        do i = n - nz + 1, n
            call check(error, is_zero(perm(i)), "the tail must hold only zero-weight items")
            if (allocated(error)) return
        end do
        a = perm(n - nz + 1:n)
        call check(error, .not. all(a == pack([(i, i = 1, n)], is_zero)), &
                   "the tail must not come back in index order -- that is what a sentinel key " // &
                   "would have given, and it is not a uniform random order")
        if (allocated(error)) return

        call pf_weighted_permutation(perm, w, gate_seed, stream=2_int64)
        b = perm(n - nz + 1:n)
        call check(error, .not. all(a == b), "the tail's order must depend on the coordinates")
    end subroutine test_race_zero_weight_tail

    !> The two families agree in distribution and DIFFER in realization -- asserted, not assumed.
    !!
    !! Without this a later "optimisation" that quietly made one call the other would pass every
    !! other test in this suite. The distributional halves are `test_tree_matches_enumeration` and
    !! `test_race_matches_enumeration`; this is the other half of the claim.
    subroutine test_families_differ(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 200
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: viaRace(n), viaTree(n), seen(n), i, ndiff
        logical :: ok

        call spread_weights(w)
        call pf_weighted_permutation(viaRace, w, gate_seed, stream=7_int64)
        call d%init(w, gate_seed, stream=7_int64)
        do i = 1, n
            call d%next(viaTree(i), ok)
        end do

        seen = 0
        do i = 1, n
            seen(viaTree(i)) = seen(viaTree(i)) + 1
        end do
        call check(error, all(seen == 1), "the drained sampler must still be a permutation")
        if (allocated(error)) return

        ndiff = count(viaRace /= viaTree)
        call check(error, ndiff > n / 4, &
                   "the two families are different realizations and must not agree position " // &
                   "for position; near-agreement means one call is quietly serving both")
    end subroutine test_families_differ

    !> The two weighted families must be INDEPENDENT at matched coordinates, not merely different.
    !!
    !! `test_families_differ` above asserts they disagree, which is a much weaker claim and passed
    !! throughout the period this test exists to close. Both families were driven from the same
    !! `(seed, stream)`, so both read draw 1: the race gives item `i` the key `-log(1-u_i)/w_i`,
    !! smallest when `u_1` is near zero, and the sequential descent scales that same `u_1` into
    !! `[0, total)`, where a near-zero value lands on the leftmost leaf -- item 1. So both chose the
    !! lowest-weight item together and nowhere else. Measured over 500 000 seeds with weights
    !! `1 .. 50`: 264 of the 399 seeds where the race chose item 1 (**66 %**) had the tree choose it
    !! too, against **0.078 %** by chance. Every marginal was clean, which is why nothing saw it.
    !!
    !! **The control arm is what makes this a test rather than a hopeful assertion.** It rebuilds
    !! the pre-fix pair from the public generator alone -- a linear descent on
    !! `pf_random_at(seed, 0, 1)`, and an exponential race over `pf_random_at(seed, 0, i)` -- so it
    !! is coupled by construction. It must fire; a run where BOTH arms look independent proves only
    !! that the fixture is too small to resolve anything, which is exactly how a statistic like this
    !! rots into a pass. See `.claude/rules/testing.md`'s "Assertions" for the
    !! shape, and `feature_risks.md` Risk-123 for what the separation forbids.
    subroutine test_families_independent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 10                    !! small, so item 1 is reached often enough
        integer(int64), parameter :: nseed = 20000_int64 !! enough to resolve a 66 % cell from a 1.8 % one
        type(pf_weighted_draw) :: d
        real(real64) :: w(n), total, u, acc, key, best
        integer(int64) :: s, nrace1, nboth, nctl_a1, nctl_both
        integer :: perm(n), item, i, tree1, race1, ctl_a, ctl_b
        logical :: ok

        do i = 1, n
            w(i) = real(i, real64)
        end do
        total = sum(w)
        nrace1 = 0_int64; nboth = 0_int64; nctl_a1 = 0_int64; nctl_both = 0_int64
        call d%init(w, 1_int64)                         ! %init once; it refuses a second call
        do s = 1_int64, nseed
            call pf_weighted_permutation(perm, w, s)
            race1 = perm(1)
            call d%reseed(s)
            call d%next(item, ok)
            tree1 = item
            if (race1 == 1) then
                nrace1 = nrace1 + 1_int64
                if (tree1 == 1) nboth = nboth + 1_int64
            end if

            ! ---- control: the same two rules, deliberately sharing one uniform axis ----
            u = pf_random_at(s, 0_int64, draw=1_int64) * total
            acc = 0.0_real64
            ctl_a = n
            do i = 1, n
                acc = acc + w(i)
                if (u < acc) then
                    ctl_a = i
                    exit
                end if
            end do
            best = huge(1.0_real64)
            ctl_b = 1
            do i = 1, n
                key = -log(1.0_real64 - pf_random_at(s, 0_int64, draw=int(i, int64))) / w(i)
                if (key < best) then
                    best = key
                    ctl_b = i
                end if
            end do
            if (ctl_b == 1) then
                nctl_a1 = nctl_a1 + 1_int64
                if (ctl_a == 1) nctl_both = nctl_both + 1_int64
            end if
        end do

        ! Vacuity guard first: a cell nothing landed in cannot report either verdict.
        call check(error, nrace1 > 50_int64, "the race must choose item 1 often enough to " // &
                   "resolve the cell; too few means the fixture, not the library, is being tested")
        if (allocated(error)) return
        call check(error, nctl_a1 > 50_int64, "the control race must choose item 1 often enough too")
        if (allocated(error)) return

        ! The control MUST fire -- it is coupled by construction.
        call check(error, real(nctl_both, real64) > 0.25_real64 * real(nctl_a1, real64), &
                   "the deliberately coupled control must show item 1 agreeing far above chance; " // &
                   "if it does not, this test has no power and its verdict below means nothing")
        if (allocated(error)) return

        ! And the real pair must not. Chance is w(1)/total; allow a wide Poisson margin.
        call check(error, real(nboth, real64) < 0.10_real64 * real(nrace1, real64), &
                   "pf_weighted_permutation and pf_weighted_draw must be independent at the same " // &
                   "(seed, stream): item 1 agreeing far above w(1)/sum(w) means the two families " // &
                   "share a uniform again -- see wd_family_label")
    end subroutine test_families_independent

    !> A weight below `wd_min_weight` is treated as zero-weight, on BOTH weighted families.
    !!
    !! **This replaces an error scenario, and the replacement is stronger than what it replaced.**
    !! `pf_weighted_permutation` used to abort on a weight whose key `-log(u)/w` overflowed, and the
    !! scenario proved it by passing a denormal. Two things were wrong with that. A build with
    !! denormals flushed to zero -- `-ffast-math`, or ifx at its own defaults -- classified the same
    !! denormal as zero-weight instead, so the scenario asserted nothing there while passing
    !! elsewhere. And the abort was DRAW-DEPENDENT for the normal weights just above `tiny`, since
    !! whether `-log(u)/w` overflows depends on `u`: identical weights would abort under one seed
    !! and not another.
    !!
    !! Thresholding at the overflow bound removes both. A weight below it cannot be ordered against
    !! weights of order one by any finite sample anyway, so calling it zero-weight is the honest
    !! reading, and the item is still returned -- last, in uniform random order. What this test
    !! asserts is exactly that, and it is fp-model-independent because `wd_min_weight` is a normal
    !! number that nothing flushes.
    subroutine test_subnormal_weight_is_zero(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error carrier
        integer, parameter :: n = 4
        type(pf_weighted_draw) :: d
        real(real64) :: w(n)
        integer :: perm(n), item, i, seen(n)
        logical :: ok

        ! Item 2 is denormal (flushed to zero under fast-math); item 3 is a NORMAL number below the
        ! overflow bound, which no model flushes. Both must be treated the same way.
        ! The subnormal is its bit pattern rather than `1.0e-320_real64`, which ifx reports as
        ! `remark #7920` on every build; the value is the same double either way.
        w = [1.0_real64, transfer(2024_int64, 0.0_real64), 1.0e-307_real64, 1.0_real64]

        call pf_weighted_permutation(perm, w, 20260819_int64)
        seen = 0
        do i = 1, n
            seen(perm(i)) = seen(perm(i)) + 1
        end do
        call check(error, all(seen == 1), &
                   "a weighted permutation must still return every item exactly once")
        if (allocated(error)) return
        call check(error, (perm(3) == 2 .or. perm(3) == 3) .and. (perm(4) == 2 .or. perm(4) == 3), &
                   "both unorderably small weights must land in the zero-weight tail, i.e. in " // &
                   "the last two positions, whatever the floating-point model")
        if (allocated(error)) return

        call d%init(w, 20260819_int64)
        call d%next(item, ok)
        call check(error, item == 1 .or. item == 4, &
                   "the sequential family must draw a real weight first, not an unorderable one")
        if (allocated(error)) return
        call d%next(item, ok)
        call check(error, item == 1 .or. item == 4, &
                   "the sequential family must exhaust the real weights before the tail")
    end subroutine test_subnormal_weight_is_zero

    !> Every specific of `pf_weighted_subset` and `pf_weighted_permutation`, called by name.
    !!
    !! **Ten one-line forwarders, five of which were reached by nothing.** Between them the two
    !! generics cover (result kind) x (stream absent / `int32` / `int64`), and the tests above resolve
    !! to whichever specific their literals happen to select -- which turned out to be the `int32`
    !! result for the subset's base form and for both permutation forms, so the `int64` twins of all
    !! three, and the subset's `int64`-stream arm, ran nowhere. A forwarder that dropped its stream,
    !! or handed the worker the wrong one, produces a different but perfectly plausible answer rather
    !! than a compile error.
    !!
    !! **The base forms are the sharp half.** Each is documented as "stream 0", and the only thing
    !! that can show it is a comparison against an explicit `stream = 0` -- with a different stream
    !! asserted to disagree, or a forwarder ignoring the argument entirely would satisfy both.
    subroutine test_weighted_specifics(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: sd = 20260824_int64
        integer(int64), parameter :: other = 9_int64   !! a stream the default must not coincide with
        real(real64) :: w(6)
        integer(int32) :: s32(4), p32(6)
        integer(int64) :: s64(4), p64(6), want_sub(4), want_perm(6), alt(6)
        integer :: i

        do i = 1, 6
            w(i) = real(i, real64)
        end do

        ! The references, taken through the int64/int64-stream specifics, which the tests above do
        ! exercise. Everything below must reproduce them exactly.
        call pf_weighted_subset(want_sub, w, sd, 0_int64)
        call pf_weighted_permutation(want_perm, w, sd, 0_int64)

        ! Vacuity control: stream 0 and stream `other` must differ, or a forwarder that discarded
        ! its stream would satisfy every equality below.
        call pf_weighted_permutation(alt, w, sd, other)
        call check(error, .not. all(alt == want_perm), &
            "stream 0 and stream 9 give the same permutation on this fixture, so a specific ignoring " // &
            "its stream= would be invisible here")
        if (allocated(error)) return

        ! ---- pf_weighted_subset: two result kinds x three stream forms ----
        call pf_weighted_subset(s32, w, sd)
        call check(error, all(int(s32, int64) == want_sub), "wsub_i32_base is not stream 0")
        if (allocated(error)) return
        call pf_weighted_subset(s32, w, sd, 0_int32)
        call check(error, all(int(s32, int64) == want_sub), "wsub_i32_s32 disagrees with the int64 worker")
        if (allocated(error)) return
        call pf_weighted_subset(s32, w, sd, 0_int64)
        call check(error, all(int(s32, int64) == want_sub), "wsub_i32_s64 disagrees with the int64 worker")
        if (allocated(error)) return
        call pf_weighted_subset(s64, w, sd)
        call check(error, all(s64 == want_sub), "wsub_i64_base is not stream 0")
        if (allocated(error)) return
        call pf_weighted_subset(s64, w, sd, 0_int32)
        call check(error, all(s64 == want_sub), "wsub_i64_s32 disagrees with the int64 worker")
        if (allocated(error)) return
        call pf_weighted_subset(s64, w, sd, 0_int64)
        call check(error, all(s64 == want_sub), "wsub_i64_s64 disagrees with the int64 worker")
        if (allocated(error)) return

        ! ---- pf_weighted_permutation: two result kinds x stream absent / int64 ----
        call pf_weighted_permutation(p32, w, sd)
        call check(error, all(int(p32, int64) == want_perm), "wperm_i32_base is not stream 0")
        if (allocated(error)) return
        call pf_weighted_permutation(p32, w, sd, 0_int64)
        call check(error, all(int(p32, int64) == want_perm), "wperm_i32_s64 disagrees with the int64 worker")
        if (allocated(error)) return
        call pf_weighted_permutation(p64, w, sd)
        call check(error, all(p64 == want_perm), "wperm_i64_base is not stream 0")
        if (allocated(error)) return
        call pf_weighted_permutation(p64, w, sd, 0_int64)
        call check(error, all(p64 == want_perm), "wperm_i64_s64 disagrees with the int64 worker")
    end subroutine test_weighted_specifics

end module test_random_weighted
