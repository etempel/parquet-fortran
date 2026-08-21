!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Tests for `parquet_random`'s DISTRIBUTIONS -- the exponential, and everything Phase 3 adds
!> beyond the uniforms.
!>
!> **A separate suite from `random` because the questions are different in kind.** The uniform
!> tests ask whether a bit pattern is the one the contract names; a distribution test has to ask
!> whether a stream of `real64` values has the right shape, which no bit pattern can answer. Both
!> layers are here, in this order:
!>
!>  1. **Golden vectors**, from `tools/generate_random_golden_vectors.py`'s arbitrary-precision
!>     model. For a `_portable` realisation these are exact bit patterns; for a libm-backed one
!>     they are an arbitrary-precision reference the result must land within a few ulp of, which
!>     is exactly what a "bit-identical for a given libm" promise is worth.
!>  2. **Cross-form agreement** -- tier 0, the stream walk and the bulk fill must agree wherever
!>     the contract says they do, and the word cost must be what `%position` claims.
!>  3. **Distributional gates**, each with a NEGATIVE CONTROL: a deliberately wrong generator the
!>     same gate must reject. A gate with no control passes everything, which is the one failure
!>     mode a gate cannot afford.
!>
!> Everything is in memory, touches no file and writes no process-global state, so the suite runs
!> safely under test-drive's per-test parallelism.
module test_random_dist

    use parquet                              ! deliberately the facade: a dropped re-export must
                                             ! break the build rather than a later assertion
    use test_random_vectors
    use iso_fortran_env, only: int32, int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none
    private
    public :: collect_tests_parquet_random_dist

    !> Seed the distributional gates draw under. Any value would do; a fixed one keeps the suite
    !! deterministic, which is the whole premise of the module being tested.
    integer(int64), parameter :: dist_seed = 20260821_int64

contains

    !> Registers every test in the `random_dist` suite.
    subroutine collect_tests_parquet_random_dist(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the suite's tests

        testsuite = [ &
            new_unittest("golden vectors: both exponential realisations, against their own kind of oracle", &
                         test_exp_golden), &
            new_unittest("the exponential's three tiers agree, on every shape and both realisations", &
                         test_exp_cross_form), &
            new_unittest("the exponential's word cost is 2, taken pair-aligned", test_exp_cost), &
            new_unittest("the two exponential realisations are the same value, not two draws", &
                         test_exp_realisations), &
            new_unittest("the exponential is Exp(1): moments, CDF chi-square, and a rejected control", &
                         test_exp_distribution), &
            new_unittest("the fast exponential is pure: usable in do concurrent, same values", &
                         test_exp_purity), &
            new_unittest("golden vectors: both normal realisations, every branch represented", &
                         test_normal_golden), &
            new_unittest("the normal's tiers: fills agree with tier 0 and DIFFER from the stream walk", &
                         test_normal_cross_form), &
            new_unittest("the normal's stream cost is variable, observed, and rewind-exact", &
                         test_normal_cost), &
            new_unittest("the two normal realisations are independent, with a coupled control", &
                         test_normal_realisations_independent), &
            new_unittest("the normal is N(0,1): four moments, CDF chi-square, and rejected controls", &
                         test_normal_distribution), &
            new_unittest("every ziggurat branch is reached, and each one is the value returned", &
                         test_normal_branches), &
            new_unittest("the fast normal is pure: usable in do concurrent, same values", &
                         test_normal_purity), &
            new_unittest("Gamma has mean and variance equal to its shape, on both sides of 1", &
                         test_gamma_moments), &
            new_unittest("Gamma(1) IS the exponential, tested as a distribution not an identity", &
                         test_gamma_is_exponential_at_one), &
            new_unittest("every Gamma branch is reached, including the shape < 1 boost", &
                         test_gamma_branches), &
            new_unittest("Poisson's pmf is exact for small lambda, and its controls are rejected", &
                         test_poisson_exact_pmf), &
            new_unittest("Poisson crosses algorithms exactly at lambda = 10, and both are correct", &
                         test_poisson_crossover), &
            new_unittest("the Poisson int32 and int64 results agree where both fit", &
                         test_poisson_kinds), &
            new_unittest("every squeeze accepts only what the full test would accept, exactly", &
                         test_squeezes_are_valid) &
            ]
    end subroutine collect_tests_parquet_random_dist

    !> Every row of the exponential golden table, against the oracle appropriate to each column.
    !!
    !! **Three assertions per row, and they are three different claims.** `exp_u_bits` pins which
    !! UNIFORM the coordinate names -- without it the exponential table and the scalar table could
    !! drift onto different draw mappings and each stay self-consistent. `exp_portable_bits` pins
    !! the frozen transform exactly. `exp_ref_bits` pins the libm-backed form to an
    !! arbitrary-precision logarithm within a few ulp, which is the strongest statement available
    !! about a value whose last bit belongs to whichever libm is linked.
    subroutine test_exp_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k
        real(real64) :: u, fast, port, ref
        real(real64) :: worst_fast, worst_port, gap

        call check(error, pf_exp_algorithm == "exp:-log(1-u)/libm+expkey/v1", &
            "pf_exp_algorithm no longer reads exp:-log(1-u)/libm+expkey/v1: the vectors in this suite are the " // &
            "contract for that exact string, so they must be regenerated with it, or the identifier must go back")
        if (allocated(error)) return

        worst_fast = 0.0_real64
        worst_port = 0.0_real64
        do k = 1, n_exp
            u = pf_random_at(exp_seed(k), exp_stream(k), exp_draw(k))
            call check(error, transfer(u, 0_int64) == exp_u_bits(k), &
                "the exponential table and pf_random_at disagree about which uniform this coordinate names")
            if (allocated(error)) return

            port = pf_random_exp_portable_at(exp_seed(k), exp_stream(k), exp_draw(k))
            call check(error, transfer(port, 0_int64) == exp_portable_bits(k), &
                "pf_random_exp_portable_at does not match its golden bit pattern -- the frozen transform is " // &
                "exact contract, so this is a value change, not a rounding difference")
            if (allocated(error)) return

            fast = pf_random_exp_at(exp_seed(k), exp_stream(k), exp_draw(k))
            ref = transfer(exp_ref_bits(k), 0.0_real64)
            gap = ulp_gap(fast, ref)
            worst_fast = max(worst_fast, gap)
            call check(error, gap <= 4.0_real64, &
                "pf_random_exp_at is more than 4 ulp from the arbitrary-precision -log(1-u): a libm may differ " // &
                "in the last bit or two, but not by this much, so the mapping itself is suspect")
            if (allocated(error)) return
            worst_port = max(worst_port, ulp_gap(port, ref))
        end do

        ! The frozen transform's own accuracy, asserted against the same independent reference.
        ! `tools/check_exp_key.f90 --accuracy` measures 2.0 ulp worst case over every binade; this
        ! is the same claim restated where the suite can see it.
        call check(error, worst_port <= 2.5_real64, &
            "pf_random_exp_portable_at is more than 2.5 ulp from the arbitrary-precision reference -- the frozen " // &
            "transform is documented at 2.0 ulp worst case, so either the transform or that claim has moved")
        if (allocated(error)) return

        ! Vacuity guard. A table whose every row agreed to 0 ulp would mean the reference had been
        ! read back out of the implementation rather than derived independently.
        call check(error, worst_fast > 0.0_real64 .or. worst_port > 0.0_real64, &
            "not one of the golden rows differs from the arbitrary-precision reference by even an ulp, which is " // &
            "what a table generated FROM the implementation would look like rather than one generated for it")
    end subroutine test_exp_golden

    !> Tier 0, the bulk fill and the stream walk, on both realisations and every awkward shape.
    !!
    !! **The two realisations are held to different standards here, and the difference is a
    !! measured property of libm rather than a concession.** The portable column must agree bit for
    !! bit across all three tiers, because its logarithm is this library's own and no compiler can
    !! substitute anything for it. The fast column may differ by a couple of ulp between the bulk
    !! fill and the scalar forms, because a compiler is free to serve `log` from a VECTOR libm
    !! inside a loop and a scalar one outside it -- measured on machine B as 21 of 64 values
    !! differing by at most 2 ulp under ifx's default `-fp-model=fast`, and none at all under
    !! gfortran.
    !!
    !! **CHUNK INVARIANCE is exact for both, and is the property that actually matters.** It is
    !! what "the same answer at any thread count" means for a bulk fill, so it is asserted over
    !! every split size rather than at one convenient one.
    subroutine test_exp_cross_form(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: k
        real(real64) :: v(16), w(16), part(16), empty(0), x
        type(pf_random_stream) :: rng
        integer(int32) :: i32
        integer(int64) :: i64
        integer :: n
        real(real64) :: worst

        call pf_random_fill_exp(dist_seed, 11_int64, v)
        call pf_random_fill_exp_portable(dist_seed, 11_int64, w)
        worst = 0.0_real64
        do k = 1_int64, 16_int64
            worst = max(worst, ulp_gap(v(k), pf_random_exp_at(dist_seed, 11_int64, k)))
            call check(error, w(k) == pf_random_exp_portable_at(dist_seed, 11_int64, k), &
                "pf_random_fill_exp_portable element k is not pf_random_exp_portable_at at draw k -- the frozen " // &
                "logarithm has no vector variant to substitute, so this column must agree bit for bit")
            if (allocated(error)) return
        end do
        call check(error, worst <= 4.0_real64, &
            "pf_random_fill_exp differs from pf_random_exp_at by more than 4 ulp: a vectorised libm log may " // &
            "differ in the last bit or two from the scalar one, but a gap this size means the fill is reading " // &
            "different uniforms, not merely rounding them differently")
        if (allocated(error)) return

        ! CHUNK INVARIANCE, on both realisations, at every split size. This is exact everywhere and
        ! is what "the same answer at any thread count" reduces to for a bulk fill.
        do n = 1, 16
            part = 0.0_real64
            call pf_random_fill_exp(dist_seed, 11_int64, part(1:n))
            call check(error, all(part(1:n) == v(1:n)), &
                "a partial pf_random_fill_exp is not a prefix of a longer one, so the fill is not splittable and " // &
                "a threaded caller would get different values depending on the chunking")
            if (allocated(error)) return
            part = 0.0_real64
            call pf_random_fill_exp_portable(dist_seed, 11_int64, part(1:n))
            call check(error, all(part(1:n) == w(1:n)), &
                "a partial pf_random_fill_exp_portable is not a prefix of a longer one")
            if (allocated(error)) return
        end do

        ! An offset start lands where it says it does, on both realisations.
        call pf_random_fill_exp(dist_seed, 11_int64, part(1:3), 5_int64)
        call check(error, all(part(1:3) == v(5:7)), "pf_random_fill_exp starting at draw 5 does not resume there")
        if (allocated(error)) return
        call pf_random_fill_exp_portable(dist_seed, 11_int64, part(1:3), 5_int64)
        call check(error, all(part(1:3) == w(5:7)), "pf_random_fill_exp_portable starting at draw 5 does not resume there")
        if (allocated(error)) return

        ! A zero-sized fill is a defined no-op on both realisations rather than an out-of-bounds walk.
        call pf_random_fill_exp(dist_seed, 11_int64, empty)
        call pf_random_fill_exp_portable(dist_seed, 11_int64, empty)
        call check(error, size(empty) == 0, "a zero-sized exponential fill did not stay zero-sized")
        if (allocated(error)) return

        ! The int32 and int64 stream-index specifics are the same procedure reached two ways.
        i32 = 11_int32
        i64 = 11_int64
        call check(error, pf_random_exp_at(dist_seed, i32) == pf_random_exp_at(dist_seed, i64), &
            "the int32 and int64 specifics of pf_random_exp_at disagree")
        if (allocated(error)) return
        call check(error, pf_random_exp_portable_at(dist_seed, i32) == pf_random_exp_portable_at(dist_seed, i64), &
            "the int32 and int64 specifics of pf_random_exp_portable_at disagree")
        if (allocated(error)) return

        ! Elemental use equals a loop of scalar calls, on both realisations.
        call check(error, all(pf_random_exp_at(dist_seed, [(k, k=1_int64,16_int64)]) == &
                              [(pf_random_exp_at(dist_seed, k), k=1_int64,16_int64)]), &
            "an elemental pf_random_exp_at over a stream vector does not equal the scalar calls")
        if (allocated(error)) return
        call check(error, all(pf_random_exp_portable_at(dist_seed, [(k, k=1_int64,16_int64)]) == &
                              [(pf_random_exp_portable_at(dist_seed, k), k=1_int64,16_int64)]), &
            "an elemental pf_random_exp_portable_at over a stream vector does not equal the scalar calls")
        if (allocated(error)) return

        ! The stream walk is the same grid. Both forms are scalar here, so both are exact.
        call rng%seed(dist_seed, 11_int64)
        do k = 1_int64, 16_int64
            call rng%exp(x)
            call check(error, x == pf_random_exp_at(dist_seed, 11_int64, k), &
                "%exp does not walk the pf_random_exp_at grid")
            if (allocated(error)) return
        end do
        call rng%seed(dist_seed, 11_int64)
        do k = 1_int64, 16_int64
            call rng%exp_portable(x)
            call check(error, x == w(k), "%exp_portable does not walk the pf_random_exp_portable_at grid")
            if (allocated(error)) return
        end do
    end subroutine test_exp_cross_form

    !> The word cost, which is contract because `%position` and `%rewind` are denominated in it.
    !!
    !! **Asserting values alone would not catch a cost change.** Two implementations agreeing on
    !! every value can disagree on how many words they consumed, and that silently changes
    !! everything drawn afterwards on the same stream.
    subroutine test_exp_cost(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        real(real64) :: x
        real(real32) :: r32
        integer(int64) :: p0, p1

        call rng%seed(dist_seed, 4_int64)
        p0 = rng%position()
        call rng%exp(x)
        p1 = rng%position()
        call check(error, p1 - p0 == 2_int64, "%exp did not consume exactly two words")
        if (allocated(error)) return
        call rng%exp_portable(x)
        call check(error, rng%position() - p1 == 2_int64, "%exp_portable did not consume exactly two words")
        if (allocated(error)) return

        ! Pair alignment. After a one-word `%uniform32` the cursor is odd, and an exponential draw
        ! must skip to the next pair rather than straddle one -- which is what keeps it equal to
        ! the coordinate-addressed call at the draw it lands on.
        call rng%seed(dist_seed, 4_int64)
        call rng%uniform32(r32)
        call check(error, rng%position() == 2_int64, "%uniform32 did not consume exactly one word")
        if (allocated(error)) return
        call rng%exp(x)
        call check(error, rng%position() == 5_int64, &
            "%exp taken from an odd position did not align to the next word pair: it must cost one word of " // &
            "alignment plus two of draw, leaving position 5")
        if (allocated(error)) return
        call check(error, x == pf_random_exp_at(dist_seed, 4_int64, 2_int64), &
            "%exp taken from an odd position is not the coordinate-addressed draw 2, so the alignment moved the " // &
            "cursor without moving the value onto the same grid")
        if (allocated(error)) return

        ! A mixed sequence, so the arithmetic is exercised rather than one producer at a time.
        call rng%seed(dist_seed, 4_int64)
        call rng%uniform(x)                     ! 2
        call rng%exp(x)                         ! 2
        call rng%uniform32(r32)                 ! 1  -> cursor now odd
        call rng%exp_portable(x)                ! 1 of alignment + 2
        call check(error, rng%position() == 9_int64, &
            "the word cost of a mixed uniform/exp/uniform32/exp_portable sequence is not 2+2+1+(1+2) = 8 words")
    end subroutine test_exp_cost

    !> The two realisations are ONE value computed two ways, and must not be independent draws.
    !!
    !! **This is the exact opposite of what the normal's two realisations require**, and stating
    !! it here is what stops the contrast being read as an oversight. The normal's Ziggurat and
    !! polar forms consume different numbers of words and must be domain-separated with their own
    !! `pf_random_key` labels, or they couple (`feature_risks.md` Risk-123). The exponential's two
    !! forms read the SAME two words and differ only in which logarithm rounds them, so they must
    !! agree to a couple of ulp -- and a change that domain-separated them would be a defect.
    subroutine test_exp_realisations(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: k
        real(real64) :: fast, port, u, worst
        integer :: identical

        worst = 0.0_real64
        identical = 0
        do k = 1_int64, 20000_int64
            u = pf_random_at(dist_seed, 3_int64, k)
            fast = pf_random_exp_at(dist_seed, 3_int64, k)
            port = pf_random_exp_portable_at(dist_seed, 3_int64, k)
            worst = max(worst, ulp_gap(fast, port))
            if (transfer(fast, 0_int64) == transfer(port, 0_int64)) identical = identical + 1
            ! Both must be the transform of the SAME uniform, which is checkable exactly on the
            ! fast side: exp(-x) recovers 1-u to within rounding.
            call check(error, abs(exp(-fast) - (1.0_real64 - u)) <= 8.0_real64 * epsilon(1.0_real64), &
                "exp(-pf_random_exp_at(...)) does not recover 1 - pf_random_at(...) at the same coordinate, so " // &
                "the exponential is not transforming the uniform the contract says it is")
            if (allocated(error)) return
        end do

        call check(error, worst <= 4.0_real64, &
            "the fast and portable exponentials differ by more than 4 ulp: they are the same value through two " // &
            "logarithms, not two draws, so a gap this size means one of them changed mapping")
        if (allocated(error)) return

        ! Vacuity guard in the other direction: if every value were bit-identical, the two names
        ! would be one procedure and the portable column would be buying nothing.
        call check(error, identical < 20000, &
            "every one of 20000 draws is bit-identical between the two realisations, which would mean the frozen " // &
            "transform and libm never disagree -- they are documented to differ by up to 2 ulp")
        if (allocated(error)) return

        ! And they must not be INDEPENDENT: a domain separation between them would show up as a
        ! near-zero correlation here, where the contract requires near-perfect agreement.
        call check(error, worst < 1.0e6_real64, "unreachable given the check above; kept as a shape guard")
    end subroutine test_exp_realisations

    !> Is it actually Exp(1)? Moments, a chi-square on the transformed CDF, and a control.
    !!
    !! **The control is the load-bearing half.** `Exp(2)` -- the same draws halved -- has the right
    !! shape, the right support and the right monotone CDF, and differs only in scale; a gate that
    !! passes it is testing nothing about the rate. The thresholds below are calibrated so that
    !! the real draws pass comfortably and the control fails by a wide margin.
    subroutine test_exp_distribution(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 400000_int64
        real(real64) :: z_mean, z_var, chi2
        real(real64) :: z_mean_c, chi2_c

        call exp_gate(dist_seed, NDRAW, 1.0_real64, z_mean, z_var, chi2)
        call check(error, abs(z_mean) <= 4.0_real64, &
            "the exponential's sample mean is more than 4 standard errors from 1")
        if (allocated(error)) return
        call check(error, abs(z_var) <= 5.0_real64, &
            "the exponential's sample variance is more than 5 standard errors from 1")
        if (allocated(error)) return
        ! 20 equiprobable CDF cells, 19 degrees of freedom: the 0.999 point is 43.8.
        call check(error, chi2 <= 43.8_real64, &
            "1 - exp(-x) is not uniform over 20 equiprobable cells at the 0.999 level, so the draws are not Exp(1)")
        if (allocated(error)) return

        ! NEGATIVE CONTROL: the same gate applied to Exp(2) draws must reject them. Without this,
        ! a gate loose enough to accept anything would report exactly the same success above.
        call exp_gate(dist_seed, NDRAW, 2.0_real64, z_mean_c, z_var, chi2_c)
        call check(error, abs(z_mean_c) > 10.0_real64 .and. chi2_c > 43.8_real64, &
            "the distributional gate ACCEPTS Exp(2) draws, so it is not measuring the rate at all and its verdict " // &
            "on the real draws above means nothing")
    end subroutine test_exp_distribution

    !> The fast exponential is `pure`, so a `do concurrent` body and a caller's own `pure`
    !! procedure can reach it -- which the portable one cannot, `exp_key` having a `volatile`
    !! local.
    !!
    !! **This is a compile-time property asserted at run time**, which is the only way available:
    !! if `pf_random_exp_at` ever stopped being pure, this file would fail to compile rather than
    !! fail an assertion, and the message below would never print. That is the intended behaviour
    !! and the reason the loop is here at all -- the values it checks are already covered by
    !! `test_exp_cross_form`.
    subroutine test_exp_purity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: j
        real(real64) :: dc(512)

        do concurrent (j = 1:512)
            dc(j) = pf_random_exp_at(dist_seed, int(j, int64))
        end do
        call check(error, all(dc == [(pf_random_exp_at(dist_seed, int(j, int64)), j=1,512)]), &
            "pf_random_exp_at gives different values inside a do concurrent body than outside it")
        if (allocated(error)) return
        call check(error, all(dc >= 0.0_real64), "an exponential draw came back negative")
    end subroutine test_exp_purity


    ! ================================================================================
    ! The normal
    ! ================================================================================

    !> Every row of the normal golden table, against the oracle appropriate to each column.
    !!
    !! **The two columns are asserted to different strengths, and the difference is the algorithms
    !! rather than a preference.** The polar form is IEEE `+ - * /` and `sqrt` over
    !! `parquet_expkey`'s frozen logarithm, so every row is exact bit contract on every platform.
    !! The Ziggurat reaches libm `exp` in its wedge test and libm `log` in its tail, so a path-2 or
    !! path-3 row is bit-identical only for a given libm -- which is what `pf_normal_algorithm`
    !! promises for it. A path-1 row touches no libm at all and is asserted exactly.
    !!
    !! `norm_zig_pairs`/`norm_polar_pairs` are asserted too, because consumption is contract for
    !! the stream walk: two implementations agreeing on every value can disagree on what they
    !! consumed, and everything drawn afterwards then moves.
    subroutine test_normal_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, npath(3)
        integer(int32) :: path
        integer(int64) :: pairs
        real(real64) :: x, want

        call check(error, pf_normal_algorithm == "normal:ziggurat256+polar/libm+expkey/v1", &
            "pf_normal_algorithm no longer reads normal:ziggurat256+polar/libm+expkey/v1: the vectors in this " // &
            "suite are the contract for that exact string, so they must be regenerated with it, or the " // &
            "identifier must go back")
        if (allocated(error)) return

        npath = 0
        do k = 1, n_norm
            call parquet_debug_normal_path(norm_seed(k), norm_stream(k), norm_draw(k), .false., x, path, pairs)
            want = transfer(norm_zig_bits(k), 0.0_real64)
            call check(error, int(path) == norm_zig_path(k), &
                "the Ziggurat took a different branch than its golden vector records for this coordinate")
            if (allocated(error)) return
            call check(error, pairs == norm_zig_pairs(k), &
                "the Ziggurat consumed a different number of word pairs than its golden vector records, which " // &
                "moves every value a stream walk produces afterwards")
            if (allocated(error)) return
            npath(path) = npath(path) + 1
            if (path == 1) then
                call check(error, transfer(x, 0_int64) == norm_zig_bits(k), &
                    "a path-1 Ziggurat draw does not match its golden bit pattern -- that branch reaches no libm " // &
                    "at all, so it is exact contract on every platform and a mismatch is a value change")
            else
                call check(error, ulp_gap(x, want) <= 4.0_real64, &
                    "a path-2 or path-3 Ziggurat draw is more than 4 ulp from its golden vector: those branches " // &
                    "use libm exp/log and so are only bit-identical for a given libm, but not this far off")
            end if
            if (allocated(error)) return

            call check(error, transfer(pf_random_normal_at(norm_seed(k), norm_stream(k), norm_draw(k)), 0_int64) &
                              == transfer(x, 0_int64), &
                "parquet_debug_normal_path does not report the value pf_random_normal_at returns, so its branch " // &
                "and cost census is about some other construction")
            if (allocated(error)) return

            x = pf_random_normal_portable_at(norm_seed(k), norm_stream(k), norm_draw(k))
            call check(error, transfer(x, 0_int64) == norm_polar_bits(k), &
                "pf_random_normal_portable_at does not match its golden bit pattern -- the polar form is exact " // &
                "contract on every platform, so this is a value change, not a rounding difference")
            if (allocated(error)) return
            call parquet_debug_normal_path(norm_seed(k), norm_stream(k), norm_draw(k), .true., x, path, pairs)
            call check(error, pairs == norm_polar_pairs(k), &
                "the polar form consumed a different number of word pairs than its golden vector records")
            if (allocated(error)) return
        end do

        ! Vacuity guard on the TABLE, not on the code: a grid that happened to be all path 1 would
        ! leave the wedge and the tail unpinned, and a change to either would move no golden value.
        call check(error, all(npath > 0), &
            "the golden table does not contain at least one row of every Ziggurat branch, so the wedge and tail " // &
            "are unpinned -- regenerate it, and check the generator's deterministic branch hunt still works")
        if (allocated(error)) return
        call check(error, any(norm_polar_pairs > 2_int64), &
            "no golden row makes the polar form reject a candidate, so its rejection loop could be compiled and " // &
            "never entered while this table still passed")
        if (allocated(error)) return
        ! And the sharper one. A wedge row the test ACCEPTED has the same value and the same cost
        ! whether the wedge test is right or wrong -- the value does not depend on the height drawn
        ! -- so only a row the wedge REJECTED pins that decision at all. Confirmed by mutation:
        ! comparing against zig_f(i+1) instead of zig_f(i-1) makes the wedge accept
        ! unconditionally, and every other assertion in this suite survives it.
        call check(error, any(norm_zig_path /= 3_int32 .and. norm_zig_pairs >= 3_int64), &
            "no golden row makes the Ziggurat's WEDGE reject a candidate, so the accept/reject decision itself " // &
            "is unpinned -- a wedge test that always accepted would pass this table")
    end subroutine test_normal_golden

    !> The bulk fill agrees with tier 0; the stream walk deliberately does NOT.
    !!
    !! **The second half is as load-bearing as the first.** A future change that "fixed" the stream
    !! walk to agree with the coordinate-addressed forms would have to make the fill unsplittable to
    !! do it -- which is the whole reason the two realisations exist -- so this asserts the
    !! disagreement rather than leaving it to a comment.
    subroutine test_normal_cross_form(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: k
        real(real64) :: v(16), w(16), v3(3), empty(0), x
        type(pf_random_stream) :: rng
        integer(int32) :: i32
        integer(int64) :: i64
        integer :: agree

        call pf_random_fill_normal(dist_seed, 9_int64, v)
        call pf_random_fill_normal_portable(dist_seed, 9_int64, w)
        do k = 1_int64, 16_int64
            call check(error, v(k) == pf_random_normal_at(dist_seed, 9_int64, k), &
                "pf_random_fill_normal element k is not pf_random_normal_at at draw k")
            if (allocated(error)) return
            call check(error, w(k) == pf_random_normal_portable_at(dist_seed, 9_int64, k), &
                "pf_random_fill_normal_portable element k is not pf_random_normal_portable_at at draw k")
            if (allocated(error)) return
        end do

        call pf_random_fill_normal(dist_seed, 9_int64, v3)
        call check(error, all(v3 == v(1:3)), "a short pf_random_fill_normal is not a prefix of a long one")
        if (allocated(error)) return
        call pf_random_fill_normal(dist_seed, 9_int64, v3, 5_int64)
        call check(error, all(v3 == v(5:7)), "pf_random_fill_normal starting at draw 5 does not resume where it should")
        if (allocated(error)) return
        call pf_random_fill_normal_portable(dist_seed, 9_int64, v3, 5_int64)
        call check(error, all(v3 == w(5:7)), "pf_random_fill_normal_portable does not resume where it should")
        if (allocated(error)) return

        call pf_random_fill_normal(dist_seed, 9_int64, empty)
        call pf_random_fill_normal_portable(dist_seed, 9_int64, empty)
        call check(error, size(empty) == 0, "a zero-sized normal fill did not stay zero-sized")
        if (allocated(error)) return

        i32 = 9_int32
        i64 = 9_int64
        call check(error, pf_random_normal_at(dist_seed, i32) == pf_random_normal_at(dist_seed, i64), &
            "the int32 and int64 specifics of pf_random_normal_at disagree")
        if (allocated(error)) return
        call check(error, pf_random_normal_portable_at(dist_seed, i32) == pf_random_normal_portable_at(dist_seed, i64), &
            "the int32 and int64 specifics of pf_random_normal_portable_at disagree")
        if (allocated(error)) return

        call check(error, all(pf_random_normal_at(dist_seed, [(k, k=1_int64,16_int64)]) == &
                              [(pf_random_normal_at(dist_seed, k), k=1_int64,16_int64)]), &
            "an elemental pf_random_normal_at over a stream vector does not equal the scalar calls")
        if (allocated(error)) return

        ! The stream walk is a DIFFERENT realisation, by construction. Asserting "they differ" is
        ! weak on its own, so this counts agreements: a handful could be coincidence, sixteen
        ! could not, and zero is what a correct domain separation gives.
        agree = 0
        call rng%seed(dist_seed, 9_int64)
        do k = 1_int64, 16_int64
            call rng%normal(x)
            if (x == v(k)) agree = agree + 1
        end do
        call check(error, agree == 0, &
            "%normal reproduces pf_random_normal_at at the same coordinates, which means the coordinate-addressed " // &
            "form has stopped deriving its own sub-stream -- see normal_zig_label and pf_random_normal_at")
        if (allocated(error)) return
        agree = 0
        call rng%seed(dist_seed, 9_int64)
        do k = 1_int64, 16_int64
            call rng%normal_portable(x)
            if (x == w(k)) agree = agree + 1
        end do
        call check(error, agree == 0, &
            "%normal_portable reproduces pf_random_normal_portable_at at the same coordinates -- see " // &
            "normal_polar_label")
    end subroutine test_normal_cross_form

    !> The stream's word cost is variable, matches what the hook reports, and survives a rewind.
    !!
    !! Checkpoint and restart is the one thing `%position` still guarantees for a variable-cost
    !! producer, so it is asserted directly rather than left to the doc-comment.
    subroutine test_normal_cost(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        integer(int64), parameter :: NW = 600_int64   !! enough that a ~2 %% rejection rate is certain to fire
        real(real64) :: x, first(NW), again(NW)
        real(real32) :: r32
        integer(int64) :: k, p0, saved, costs(NW)
        integer :: distinct

        call rng%seed(dist_seed, 6_int64)
        do k = 1_int64, NW
            p0 = rng%position()
            call rng%normal(first(k))
            costs(k) = rng%position() - p0
            call check(error, costs(k) >= 2_int64 .and. modulo(costs(k), 2_int64) == 0_int64, &
                "a %normal draw consumed an odd number of words, or fewer than the two a single candidate needs")
            if (allocated(error)) return
        end do
        ! Vacuity guard: if every draw cost the same, this fixture never reached the rejection
        ! loop and the "variable" in "variable cost" is untested.
        distinct = count(costs /= costs(1))
        call check(error, distinct > 0, &
            "every %normal draw in this fixture cost the same number of words, so no rejection ever fired and this " // &
            "test has not exercised the variable-cost path it exists for")
        if (allocated(error)) return

        ! Checkpoint and restart. The position is exact even though it cannot be predicted, which
        ! is the whole distinction a variable-cost producer forces.
        call rng%seed(dist_seed, 6_int64)
        do k = 1_int64, 17_int64
            call rng%normal(x)
        end do
        saved = rng%position()
        call rng%seed(dist_seed, 6_int64)
        call rng%rewind(saved)
        do k = 18_int64, NW
            call rng%normal(again(k))
        end do
        call check(error, all(again(18:NW) == first(18:NW)), &
            "a stream rewound to a saved %position does not resume the same sequence of normals, so checkpoint " // &
            "and restart is broken for a variable-cost producer")
        if (allocated(error)) return

        ! Pair alignment, exactly as %exp and %int_range: a normal taken from an odd cursor must
        ! skip one word rather than straddle a pair.
        call rng%seed(dist_seed, 6_int64)
        call rng%uniform32(r32)
        p0 = rng%position()
        call check(error, p0 == 2_int64, "%uniform32 did not consume exactly one word")
        if (allocated(error)) return
        call rng%normal(x)
        call check(error, modulo(rng%position() - 1_int64, 2_int64) == 0_int64, &
            "%normal taken from an odd position did not align to a word pair first")
        if (allocated(error)) return
        ! And the aligned draw is the one the stream's own grid holds at that pair.
        call rng%seed(dist_seed, 6_int64)
        call rng%uniform(x)
        call rng%normal(x)
        call check(error, x == first(2), &
            "a %normal taken after one %uniform is not the second normal of a freshly seeded stream, even though " // &
            "the first normal cost exactly two words in this fixture")
    end subroutine test_normal_cost

    !> Risk-123: the two realisations must be INDEPENDENT, not merely different.
    !!
    !! **A correlation test has no power here, and that is measured rather than assumed.** Running
    !! both realisations over the SAME sub-key -- a deliberate coupling, in which they transform the
    !! very same 64 bits -- and correlating 40000 pairs gives `r = -0.00005`, 0.01 standard errors
    !! from zero. The two algorithms extract different functions of the same bits (the Ziggurat's
    !! sign is bit 8, the polar's is bit 63 of the same word), and linear correlation is blind to
    !! that. Risk-123's own instance was likewise invisible to every marginal test.
    !!
    !! **The statistic that does have power is a joint cell**, and it is chosen from the mechanism:
    !! a Ziggurat draw leaves its immediate-acceptance branch when its first uniform is LARGE, and
    !! a polar candidate is rejected when `|2u-1|` is large -- which is the same event. If the two
    !! shared an axis, "the Ziggurat needed a second candidate" would nearly imply "the polar form
    !! did too". The control arm below builds exactly that coupled pair out of `pf_random_at` and
    !! requires it to fire, so a verdict on the real pair means something.
    subroutine test_normal_realisations_independent(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 200000_int64
        integer(int64) :: k, pairs, n_a, n_ab, n_b, nc_a, nc_ab, nc_b
        integer(int32) :: path
        real(real64) :: x, a1, a2, u1, u2, p_b, p_b_given_a, pc_b, pc_b_given_a

        n_a = 0_int64; n_ab = 0_int64; n_b = 0_int64
        nc_a = 0_int64; nc_ab = 0_int64; nc_b = 0_int64
        do k = 1_int64, NDRAW
            call parquet_debug_normal_path(dist_seed, 5_int64, k, .false., x, path, pairs)
            call parquet_debug_normal_path(dist_seed, 5_int64, k, .true., x, path, pairs)
            ! `path` is now the polar call's (always 1); `pairs` is the polar cost.
            if (pairs > 2_int64) n_b = n_b + 1_int64
            call parquet_debug_normal_path(dist_seed, 5_int64, k, .false., x, path, pairs)
            if (path /= 1_int32) then
                n_a = n_a + 1_int64
                call parquet_debug_normal_path(dist_seed, 5_int64, k, .true., x, path, pairs)
                if (pairs > 2_int64) n_ab = n_ab + 1_int64
            end if

            ! ---- control: the same two rules, deliberately over ONE uniform axis ----
            a1 = pf_random_at(dist_seed, 5_int64, k)
            a2 = pf_random_at(dist_seed, 5_int64, k + NDRAW)
            u1 = (a1 + a1) - 1.0_real64
            u2 = (a2 + a2) - 1.0_real64
            if (u1 * u1 + u2 * u2 >= 1.0_real64) nc_b = nc_b + 1_int64
            ! A Ziggurat leaves its fast branch when the first uniform is large; 0.985 is that
            ! branch's own rate, so this control arm fires about as often as the real one.
            if (a1 >= 0.985_real64) then
                nc_a = nc_a + 1_int64
                if (u1 * u1 + u2 * u2 >= 1.0_real64) nc_ab = nc_ab + 1_int64
            end if
        end do

        ! Vacuity guards first: a cell nothing landed in cannot report either verdict.
        call check(error, n_a > 400_int64, &
            "the Ziggurat left its immediate-acceptance branch too rarely to resolve the joint cell; too few " // &
            "means the fixture, not the library, is being tested")
        if (allocated(error)) return
        call check(error, nc_a > 400_int64, "the control's first arm fired too rarely to resolve its joint cell")
        if (allocated(error)) return

        p_b = real(n_b, real64) / real(NDRAW, real64)
        p_b_given_a = real(n_ab, real64) / real(n_a, real64)
        pc_b = real(nc_b, real64) / real(NDRAW, real64)
        pc_b_given_a = real(nc_ab, real64) / real(nc_a, real64)

        ! The control MUST fire -- it is coupled by construction.
        call check(error, pc_b_given_a - pc_b > 0.3_real64, &
            "the deliberately coupled control does not show its joint cell far above chance; if it does not, " // &
            "this test has no power and its verdict below means nothing")
        if (allocated(error)) return

        ! And the real pair must not. The margin is wide against a coupling that would push the
        ! conditional near 1, and comfortable against the ~1 % sampling error at this cell size.
        call check(error, abs(p_b_given_a - p_b) < 0.08_real64, &
            "the two normal realisations agree in their rejection behaviour far above chance at the same " // &
            "coordinate, which means they are reading the same words again -- see normal_zig_label")
    end subroutine test_normal_realisations_independent

    !> Is it actually N(0,1)? Four moments and a CDF chi-square, on both realisations, with
    !! controls the same gate must reject.
    !!
    !! **The fourth moment is where a mis-walked Ziggurat shows up.** A transcription error in a
    !! layer table, or an off-by-one in the wedge test, usually leaves the mean and variance
    !! looking right and moves the tails; a gate that stopped at two moments would pass it.
    subroutine test_normal_distribution(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 400000_int64
        real(real64) :: z_mean, z_var, z_kurt, chi2

        call normal_gate(dist_seed, NDRAW, .false., 1.0_real64, z_mean, z_var, z_kurt, chi2)
        call check(error, abs(z_mean) <= 4.0_real64, "the Ziggurat normal's mean is more than 4 SE from 0")
        if (allocated(error)) return
        call check(error, abs(z_var) <= 5.0_real64, "the Ziggurat normal's variance is more than 5 SE from 1")
        if (allocated(error)) return
        call check(error, abs(z_kurt) <= 6.0_real64, &
            "the Ziggurat normal's kurtosis is more than 6 SE from 3, which is what a mis-walked layer table " // &
            "looks like when the first two moments survive")
        if (allocated(error)) return
        call check(error, chi2 <= 43.8_real64, &
            "the Ziggurat normal's CDF is not uniform over 20 equiprobable cells at the 0.999 level")
        if (allocated(error)) return

        call normal_gate(dist_seed, NDRAW, .true., 1.0_real64, z_mean, z_var, z_kurt, chi2)
        call check(error, abs(z_mean) <= 4.0_real64, "the polar normal's mean is more than 4 SE from 0")
        if (allocated(error)) return
        call check(error, abs(z_var) <= 5.0_real64, "the polar normal's variance is more than 5 SE from 1")
        if (allocated(error)) return
        call check(error, abs(z_kurt) <= 6.0_real64, "the polar normal's kurtosis is more than 6 SE from 3")
        if (allocated(error)) return
        call check(error, chi2 <= 43.8_real64, "the polar normal's CDF is not uniform over 20 equiprobable cells")
        if (allocated(error)) return

        ! NEGATIVE CONTROLS: the same gate applied to N(0, 1.1**2) draws must reject them. A scale
        ! error is the defect a shape-only test cannot see, and 1.1 is deliberately modest --
        ! a control that is obviously wrong proves the gate rejects obviously wrong things.
        call normal_gate(dist_seed, NDRAW, .false., 1.1_real64, z_mean, z_var, z_kurt, chi2)
        call check(error, abs(z_var) > 10.0_real64 .and. chi2 > 43.8_real64, &
            "the distributional gate ACCEPTS Ziggurat draws scaled by 1.1, so it is not measuring the scale at " // &
            "all and its verdict on the real draws means nothing")
        if (allocated(error)) return
        call normal_gate(dist_seed, NDRAW, .true., 1.1_real64, z_mean, z_var, z_kurt, chi2)
        call check(error, abs(z_var) > 10.0_real64 .and. chi2 > 43.8_real64, &
            "the distributional gate ACCEPTS polar draws scaled by 1.1")
    end subroutine test_normal_distribution

    !> Every Ziggurat branch is reached over an ordinary fixture, and each one returns the value.
    !!
    !! **This is CLAUDE.md's "check WHICH CODE PATH the test actually reaches", made explicit.** The
    !! tail is entered about once in 4000 draws and the wedge about once in 125; a suite that never
    !! counted them would pass identically against a build in which either branch was dead. The
    !! rates are asserted loosely -- the point is that the branch runs and produces the returned
    !! value, not that its frequency is pinned to three digits.
    subroutine test_normal_branches(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 200000_int64
        integer(int64) :: k, pairs, npath(3), maxpairs
        integer(int32) :: path
        real(real64) :: x, tail_sum, tail_min

        npath = 0_int64
        maxpairs = 0_int64
        tail_sum = 0.0_real64
        tail_min = huge(1.0_real64)
        do k = 1_int64, NDRAW
            call parquet_debug_normal_path(dist_seed, 8_int64, k, .false., x, path, pairs)
            npath(path) = npath(path) + 1_int64
            maxpairs = max(maxpairs, pairs)
            if (path == 3_int32) then
                tail_sum = tail_sum + abs(x)
                tail_min = min(tail_min, abs(x))
            end if
        end do

        call check(error, npath(1) > 0_int64, "the Ziggurat's immediate-acceptance branch was never reached")
        if (allocated(error)) return
        call check(error, npath(2) > 100_int64, &
            "the Ziggurat's wedge test was reached fewer than 100 times in 200000 draws, so it is effectively " // &
            "untested -- either the layer table or the acceptance test has changed shape")
        if (allocated(error)) return
        call check(error, npath(3) > 10_int64, &
            "the Ziggurat's tail was reached fewer than 10 times in 200000 draws, so the branch that generates " // &
            "everything beyond 3.65 sigma is effectively untested")
        if (allocated(error)) return
        call check(error, maxpairs > 1_int64, "no draw consumed more than one word pair, so no rejection ever fired")
        if (allocated(error)) return

        ! Every tail draw must actually be in the tail. A tail branch that returned a value from
        ! inside the ziggurat would still give the right marginal distribution to two moments.
        call check(error, tail_min >= 3.6541528853610092_real64, &
            "a draw reported as coming from the tail is smaller in magnitude than zig_r, so the tail branch is " // &
            "not generating from the region it is supposed to")
        if (allocated(error)) return
        ! ... and the mean magnitude of a tail draw is r + 1/r for an exponential tail, about 3.93.
        call check(error, abs(tail_sum / real(npath(3), real64) - 3.928_real64) < 0.30_real64, &
            "the mean magnitude of a tail draw is far from r + 1/r, so the tail's own distribution is wrong even " // &
            "though its support is right")
    end subroutine test_normal_branches

    !> The fast normal is `pure`, so a `do concurrent` body can reach it; the portable one cannot.
    !!
    !! Same shape as `test_exp_purity`: if `pf_random_normal_at` stopped being pure this file would
    !! fail to COMPILE rather than fail an assertion, which is the intended report.
    subroutine test_normal_purity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: j
        real(real64) :: dc(512)

        do concurrent (j = 1:512)
            dc(j) = pf_random_normal_at(dist_seed, int(j, int64))
        end do
        call check(error, all(dc == [(pf_random_normal_at(dist_seed, int(j, int64)), j=1,512)]), &
            "pf_random_normal_at gives different values inside a do concurrent body than outside it")
    end subroutine test_normal_purity

    !> Draws `n` normals scaled by `scale` and reports four test statistics.
    !!
    !! Scaling is what makes the negative control possible: `scale = 1` is the library's own draw,
    !! and anything else has the right shape and the wrong width -- precisely the defect a
    !! shape-only test cannot see.
    subroutine normal_gate(seed, n, portable, scale, z_mean, z_var, z_kurt, chi2)
        integer(int64), intent(in) :: seed      !! the family to draw from
        integer(int64), intent(in) :: n         !! how many draws to take
        logical, intent(in) :: portable         !! which realisation to measure
        real(real64), intent(in) :: scale       !! 1 for the real draws; anything else is a control
        real(real64), intent(out) :: z_mean     !! standard errors of the mean away from 0
        real(real64), intent(out) :: z_var      !! standard errors of the variance away from 1
        real(real64), intent(out) :: z_kurt     !! standard errors of the kurtosis away from 3
        real(real64), intent(out) :: chi2       !! chi-square of the normal CDF over 20 cells
        integer, parameter :: NCELL = 20
        integer(int64) :: k, counts(NCELL), cell
        real(real64) :: x, s, s2, s4, mean, var, kurt, expect, dn, u

        counts = 0_int64
        s = 0.0_real64
        s2 = 0.0_real64
        s4 = 0.0_real64
        do k = 1_int64, n
            if (portable) then
                x = pf_random_normal_portable_at(seed, 44_int64, k) * scale
            else
                x = pf_random_normal_at(seed, 44_int64, k) * scale
            end if
            s = s + x
            s2 = s2 + x * x
            s4 = s4 + x ** 4
            ! Phi(x) is Uniform(0,1) exactly when x is N(0,1), and erf gives Phi in closed form,
            ! so the cell index is a distribution-free restatement of the whole shape.
            u = 0.5_real64 * (1.0_real64 + erf(x / sqrt(2.0_real64)))
            cell = min(int(real(NCELL, real64) * u, int64) + 1_int64, int(NCELL, int64))
            counts(max(cell, 1_int64)) = counts(max(cell, 1_int64)) + 1_int64
        end do
        dn = real(n, real64)
        mean = s / dn
        var = s2 / dn - mean * mean
        kurt = s4 / dn
        ! N(0,1): var(mean) = 1/n, var(s2) = 2/n, var(fourth moment) = 96/n.
        z_mean = mean * sqrt(dn)
        z_var = (var - 1.0_real64) * sqrt(dn / 2.0_real64)
        z_kurt = (kurt - 3.0_real64) * sqrt(dn / 96.0_real64)
        expect = dn / real(NCELL, real64)
        chi2 = sum((real(counts, real64) - expect) ** 2 / expect)
    end subroutine normal_gate


    ! ================================================================================
    ! Gamma and Poisson
    ! ================================================================================

    !> `Gamma(a, 1)` has mean `a` and variance `a`. Checked on both sides of the boost boundary.
    !!
    !! **The boundary is what this exists for.** `shape < 1` goes through a completely different
    !! code path -- one extra uniform, a libm `pow`, and a draw from `Gamma(shape+1)` underneath --
    !! so a test that only looked at `shape >= 1` would leave that half of the domain unexercised
    !! while reporting that Gamma works.
    subroutine test_gamma_moments(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 200000_int64
        real(real64), parameter :: shapes(7) = &
            [0.05_real64, 0.3_real64, 0.999_real64, 1.0_real64, 1.001_real64, 4.5_real64, 250.0_real64]
        type(pf_random_stream) :: rng
        integer :: j
        integer(int64) :: k
        real(real64) :: x, s, s2, dn, mean, var, z_mean, z_var

        dn = real(NDRAW, real64)
        do j = 1, size(shapes)
            call rng%seed(dist_seed, int(j, int64))
            s = 0.0_real64
            s2 = 0.0_real64
            do k = 1_int64, NDRAW
                call rng%gamma(shapes(j), x)
                call check(error, x > 0.0_real64, "a Gamma draw came back at or below 0, which its support excludes")
                if (allocated(error)) return
                s = s + x
                s2 = s2 + x * x
            end do
            mean = s / dn
            var = s2 / dn - mean * mean
            ! Gamma(a): mean a, variance a, and the variance of the sample variance is
            ! `(mu4 - sigma**4)/n` = `(6a**2 + ... )/n`; `9*a**2/n` is a safe over-estimate here.
            z_mean = (mean - shapes(j)) / sqrt(shapes(j) / dn)
            z_var = (var - shapes(j)) / sqrt(9.0_real64 * shapes(j) ** 2 / dn)
            call check(error, abs(z_mean) <= 4.5_real64, &
                "a Gamma draw's sample mean is more than 4.5 standard errors from its shape parameter")
            if (allocated(error)) return
            call check(error, abs(z_var) <= 5.0_real64, &
                "a Gamma draw's sample variance is more than 5 standard errors from its shape parameter")
            if (allocated(error)) return
        end do
    end subroutine test_gamma_moments

    !> `Gamma(1, 1)` is `Exp(1)`, which is an independent oracle rather than a restatement.
    !!
    !! **The two are produced by completely unrelated code** -- one by Marsaglia-Tsang rejection
    !! over a Ziggurat normal, the other by transforming a single uniform -- so agreeing in
    !! distribution is real evidence about both. They cannot be compared value for value, and
    !! should not be: this is a two-sample test, with a deliberately wrong scale as its control.
    subroutine test_gamma_is_exponential_at_one(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 200000_int64
        integer, parameter :: NCELL = 16
        type(pf_random_stream) :: rng
        integer(int64) :: k, counts(NCELL), cell
        real(real64) :: x, expect, chi2, chi2_ctl

        ! Gamma(1) cell counts against the Exp(1) CDF. If Gamma(1) is Exp(1) these are uniform.
        counts = 0_int64
        call rng%seed(dist_seed, 71_int64)
        do k = 1_int64, NDRAW
            call rng%gamma(1.0_real64, x)
            cell = min(int(real(NCELL, real64) * (1.0_real64 - exp(-x)), int64) + 1_int64, int(NCELL, int64))
            counts(cell) = counts(cell) + 1_int64
        end do
        expect = real(NDRAW, real64) / real(NCELL, real64)
        chi2 = sum((real(counts, real64) - expect) ** 2 / expect)
        ! 15 degrees of freedom: the 0.999 point is 37.7.
        call check(error, chi2 <= 37.7_real64, &
            "Gamma(1) is not Exp(1) at the 0.999 level, which means one of two unrelated algorithms is wrong -- " // &
            "compare against test_exp_distribution to see which")
        if (allocated(error)) return

        ! NEGATIVE CONTROL: Gamma(1.2) against the same Exp(1) CDF must be rejected. Without it a
        ! loose gate would report the same success above.
        counts = 0_int64
        call rng%seed(dist_seed, 72_int64)
        do k = 1_int64, NDRAW
            call rng%gamma(1.2_real64, x)
            cell = min(int(real(NCELL, real64) * (1.0_real64 - exp(-x)), int64) + 1_int64, int(NCELL, int64))
            counts(cell) = counts(cell) + 1_int64
        end do
        chi2_ctl = sum((real(counts, real64) - expect) ** 2 / expect)
        call check(error, chi2_ctl > 37.7_real64, &
            "the gate accepts Gamma(1.2) as Exp(1), so it is not measuring the shape at all and its verdict " // &
            "above means nothing")
    end subroutine test_gamma_is_exponential_at_one

    !> Every Gamma branch is reached, and the boost is orthogonal to the acceptance branch.
    subroutine test_gamma_branches(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 100000_int64
        type(pf_random_stream) :: rng
        integer(int64) :: k, npath(4), p0, costs
        integer(int32) :: path
        real(real64) :: x
        integer :: distinct_costs
        integer(int64) :: first_cost

        npath = 0_int64
        call rng%seed(dist_seed, 61_int64)
        do k = 1_int64, NDRAW
            call parquet_debug_gamma_path(rng, 3.0_real64, x, path)
            npath(path) = npath(path) + 1_int64
        end do
        call check(error, npath(1) > 0_int64, "the Gamma squeeze acceptance was never reached at shape 3")
        if (allocated(error)) return
        call check(error, npath(2) > 100_int64, &
            "the Gamma logarithmic acceptance test was reached fewer than 100 times in 100000 draws at shape 3, " // &
            "so the branch that decides the ~5 % of candidates the squeeze declines is effectively untested")
        if (allocated(error)) return
        call check(error, npath(3) == 0_int64 .and. npath(4) == 0_int64, &
            "a shape of 3 reported a boosted path, but the boost applies only below shape 1")
        if (allocated(error)) return

        npath = 0_int64
        call rng%seed(dist_seed, 62_int64)
        do k = 1_int64, NDRAW
            call parquet_debug_gamma_path(rng, 0.4_real64, x, path)
            npath(path) = npath(path) + 1_int64
        end do
        call check(error, npath(1) == 0_int64 .and. npath(2) == 0_int64, &
            "a shape of 0.4 reported an unboosted path, but every draw below shape 1 goes through the boost")
        if (allocated(error)) return
        call check(error, npath(3) > 0_int64 .and. npath(4) > 100_int64, &
            "at shape 0.4 the boost did not reach BOTH acceptance branches -- the two are orthogonal, and a " // &
            "path code that collapsed them would hide whichever one broke")
        if (allocated(error)) return

        ! The word cost is variable, which is contract for the stream.
        call rng%seed(dist_seed, 63_int64)
        p0 = rng%position()
        call rng%gamma(3.0_real64, x)
        first_cost = rng%position() - p0
        distinct_costs = 0
        do k = 2_int64, 400_int64
            p0 = rng%position()
            call rng%gamma(3.0_real64, x)
            costs = rng%position() - p0
            if (costs /= first_cost) distinct_costs = distinct_costs + 1
        end do
        call check(error, distinct_costs > 0, &
            "every Gamma draw in this fixture cost the same number of words, so no rejection ever fired and the " // &
            "variable-cost path is untested")
    end subroutine test_gamma_branches

    !> Poisson's pmf is enumerable exactly for a small mean, so this compares against arithmetic.
    !!
    !! **An exact check beats a statistical one wherever one exists**, and here one does:
    !! `P(k) = exp(-lambda) lambda**k / k!` is computable to machine precision, so the test is a
    !! chi-square against exact cell probabilities rather than against another sampler.
    subroutine test_poisson_exact_pmf(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 400000_int64
        real(real64) :: chi2, chi2_ctl
        integer :: nc, nc_ctl

        call poisson_pmf_chi2(dist_seed, NDRAW, 3.0_real64, 3.0_real64, chi2, nc)
        ! The cells are built from the pmf, so the degrees of freedom come back with them. At
        ! `nc - 1` degrees of freedom the 0.999 point never exceeds 45 for the sizes used here.
        call check(error, chi2 <= chi2_999(nc - 1), &
            "Poisson(3)'s observed counts do not match the exact pmf at the 0.999 level")
        if (allocated(error)) return

        ! NEGATIVE CONTROL: draws from Poisson(3.3) judged against Poisson(3)'s exact pmf must be
        ! rejected. A 10 % error in the mean is modest on purpose -- a control that is obviously
        ! wrong proves only that the gate rejects obviously wrong things.
        call poisson_pmf_chi2(dist_seed, NDRAW, 3.3_real64, 3.0_real64, chi2_ctl, nc_ctl)
        call check(error, chi2_ctl > chi2_999(nc_ctl - 1), &
            "the gate accepts Poisson(3.3) draws as Poisson(3), so it is not resolving a 10 % error in the mean " // &
            "and its verdict above means nothing")
    end subroutine test_poisson_exact_pmf

    !> The crossover is contract: which algorithm runs decides the value, so it is pinned exactly.
    !!
    !! **Both algorithms must be tested, and both are** -- against the same exact pmf, on either
    !! side of the boundary. Forcing the "wrong" algorithm for a given lambda is deliberately not
    !! offered: the choice is a function of lambda alone, so choosing lambda IS how a test reaches
    !! either one, and doing it that way exercises the crossover as it actually ships.
    subroutine test_poisson_crossover(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        integer(int64) :: k, kk, npath(3)
        integer(int32) :: path
        real(real64) :: chi2
        integer :: nc

        ! Just below the crossover: Knuth, every time.
        npath = 0_int64
        call rng%seed(dist_seed, 81_int64)
        do k = 1_int64, 20000_int64
            call parquet_debug_poisson_path(rng, 9.999_real64, kk, path)
            npath(path) = npath(path) + 1_int64
        end do
        call check(error, npath(1) == 20000_int64, &
            "a lambda just below 10 did not take Knuth's product every time, so the crossover has moved -- it is " // &
            "frozen contract, published through pf_poisson_algorithm")
        if (allocated(error)) return

        ! Exactly at it: PTRS, every time, and both of its acceptance branches reached.
        npath = 0_int64
        call rng%seed(dist_seed, 82_int64)
        do k = 1_int64, 20000_int64
            call parquet_debug_poisson_path(rng, 10.0_real64, kk, path)
            npath(path) = npath(path) + 1_int64
        end do
        call check(error, npath(1) == 0_int64, &
            "lambda exactly 10 still took Knuth, but the crossover is `lambda < 10`, so 10 itself belongs to PTRS")
        if (allocated(error)) return
        call check(error, npath(2) > 0_int64 .and. npath(3) > 0_int64, &
            "PTRS reached only one of its two acceptance branches in 20000 draws, so the other is effectively " // &
            "untested at this lambda")
        if (allocated(error)) return

        ! And both algorithms produce the right distribution. Knuth is already covered by
        ! test_poisson_exact_pmf at lambda 3; this is PTRS, on the same exact-pmf machinery.
        call poisson_pmf_chi2(dist_seed, 400000_int64, 12.0_real64, 12.0_real64, chi2, nc)
        call check(error, chi2 <= chi2_999(nc - 1), &
            "Poisson(12) -- which is PTRS, not Knuth -- does not match the exact pmf at the 0.999 level")
    end subroutine test_poisson_crossover

    !> The int32 and int64 results are the same draw, and the int32 one is not silently narrowed.
    subroutine test_poisson_kinds(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: a, b
        integer(int64) :: k, k64, pos_a, pos_b
        integer(int32) :: k32

        call a%seed(dist_seed, 91_int64)
        call b%seed(dist_seed, 91_int64)
        do k = 1_int64, 2000_int64
            call a%poisson(7.0_real64, k32)
            call b%poisson(7.0_real64, k64)
            call check(error, int(k32, int64) == k64, &
                "the int32 and int64 specifics of %poisson gave different counts from the same stream position")
            if (allocated(error)) return
        end do
        pos_a = a%position()
        pos_b = b%position()
        call check(error, pos_a == pos_b, &
            "the int32 and int64 specifics of %poisson consumed different numbers of words, which would make " // &
            "everything drawn afterwards depend on which kind the caller asked for")
        if (allocated(error)) return
        ! And across the crossover, where the two algorithms consume very differently.
        call a%seed(dist_seed, 92_int64)
        call b%seed(dist_seed, 92_int64)
        do k = 1_int64, 500_int64
            call a%poisson(40.0_real64, k32)
            call b%poisson(40.0_real64, k64)
            call check(error, int(k32, int64) == k64, "the two %poisson kinds disagree on the PTRS side")
            if (allocated(error)) return
        end do
        call check(error, a%position() == b%position(), "the two %poisson kinds consumed differently under PTRS")
    end subroutine test_poisson_kinds

    !> Chi-square of `n` Poisson(`lambda`) draws against the EXACT pmf of `lambda_ref`.
    !!
    !! Passing the two means separately is what makes a negative control possible: with
    !! `lambda == lambda_ref` this is the real test, and with them different it is a sampler judged
    !! against the wrong reference, which the same gate must reject.
    !!
    !! **The cells are built around the mean rather than fixed at 0..10, and that is not a detail.**
    !! Fixed low cells put essentially all of a lambda-12 sample into one tail bin, which makes the
    !! statistic nearly blind: perturbing PTRS's fast-acceptance constant `v_r` by 3 % survived
    !! exactly that version of this test. Walking the exact pmf and cutting a cell whenever the
    !! accumulated probability passes `1/NCELL` gives roughly equiprobable cells at any lambda, so
    !! the test has the same power wherever it is pointed.
    subroutine poisson_pmf_chi2(seed, n, lambda, lambda_ref, chi2, ncell_used)
        integer(int64), intent(in) :: seed      !! the family to draw from
        integer(int64), intent(in) :: n         !! how many draws to take
        real(real64), intent(in) :: lambda      !! the mean actually drawn from
        real(real64), intent(in) :: lambda_ref  !! the mean the exact pmf is computed for
        real(real64), intent(out) :: chi2       !! chi-square over the cells built below
        integer, intent(out) :: ncell_used      !! how many cells there were, for the caller's dof
        integer, parameter :: NCELL = 20        !! target cells; the walk gives at most this many
        integer(int64), parameter :: KMAX = 4000_int64
        type(pf_random_stream) :: rng
        integer(int64) :: j, kk, counts(NCELL), edge(NCELL), c
        real(real64) :: p(NCELL), term, acc, dn, expect, tot
        integer :: nc

        ! Exact pmf of the REFERENCE mean, walked into roughly equiprobable cells. `edge(c)` is the
        ! largest count that falls in cell `c`; the last cell always runs to infinity, so the
        ! probabilities sum to 1 exactly however far the walk got.
        term = exp(-lambda_ref)
        acc = 0.0_real64
        tot = 0.0_real64
        nc = 0
        do kk = 0_int64, KMAX
            if (kk > 0_int64) term = term * lambda_ref / real(kk, real64)
            acc = acc + term
            tot = tot + term
            if (acc >= 1.0_real64 / real(NCELL, real64) .and. nc < NCELL - 1) then
                nc = nc + 1
                edge(nc) = kk
                p(nc) = acc
                acc = 0.0_real64
            end if
            if (tot > 1.0_real64 - 1.0e-12_real64) exit
        end do
        nc = nc + 1
        edge(nc) = huge(1_int64)
        p(nc) = 1.0_real64 - sum(p(1:nc - 1))
        ncell_used = nc

        counts(1:nc) = 0_int64
        call rng%seed(seed, 77_int64)
        do j = 1_int64, n
            call rng%poisson(lambda, kk)
            do c = 1_int64, int(nc, int64)
                if (kk <= edge(c)) exit
            end do
            counts(min(c, int(nc, int64))) = counts(min(c, int(nc, int64))) + 1_int64
        end do

        dn = real(n, real64)
        chi2 = 0.0_real64
        do c = 1_int64, int(nc, int64)
            expect = dn * p(c)
            if (expect > 0.0_real64) chi2 = chi2 + (real(counts(c), real64) - expect) ** 2 / expect
        end do
    end subroutine poisson_pmf_chi2

    !> Both rejection samplers' SQUEEZES accept only candidates the full test would accept.
    !!
    !! **This is the check that a distributional gate cannot replace, and the reason is measured.**
    !! A squeeze is a cheap sufficient condition standing in for an expensive exact one, so the
    !! only thing that can go wrong with it is accepting something the exact test would reject --
    !! and the resulting bias is tiny. Perturbing PTRS's `v_r` by 3 % moved the sample mean at
    !! lambda 12 by **zero** at a million draws, and dropping its `0.43` centring offset moved it
    !! by 0.14 %, which is under 5 standard errors at a million and invisible at the sizes a test
    !! suite can afford. Both of those survived every moment and chi-square check in this file.
    !!
    !! Containment is an EXACT property, so it is checked exactly: the hooks re-evaluate the full
    !! logarithmic test on every candidate their squeeze accepted and report whether it agrees.
    !! One disagreement is a defect; there is no threshold and no sampling error.
    !!
    !! **The negative control is the mutation record above**, not an arm of this test: a squeeze
    !! that is correct cannot be made to fail by choosing a fixture, so a control here would have
    !! to be a second, deliberately wrong implementation. What stands in for it is that the two
    !! documented over-aggressive mutations -- Gamma's `0.0331` to `0.005` and PTRS's widened
    !! `v_r` -- both fail this test immediately, which was verified when it was written.
    subroutine test_squeezes_are_valid(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 40000_int64
        real(real64), parameter :: shapes(4) = [0.3_real64, 1.0_real64, 4.0_real64, 90.0_real64]
        real(real64), parameter :: lambdas(4) = [10.0_real64, 12.0_real64, 60.0_real64, 900.0_real64]
        type(pf_random_stream) :: rng
        integer(int64) :: k, kk, nfast
        integer(int32) :: path
        real(real64) :: x
        logical :: ok
        integer :: j

        do j = 1, size(shapes)
            nfast = 0_int64
            call rng%seed(dist_seed, int(300 + j, int64))
            do k = 1_int64, NDRAW
                call parquet_debug_gamma_path(rng, shapes(j), x, path, ok)
                if (path == 1_int32 .or. path == 3_int32) nfast = nfast + 1_int64
                call check(error, ok, &
                    "the Gamma squeeze accepted a candidate the full logarithmic test would have rejected, so it " // &
                    "is not a valid squeeze and every draw at this shape is biased")
                if (allocated(error)) return
            end do
            call check(error, nfast > 0_int64, &
                "the Gamma squeeze never fired at this shape, so this test checked nothing about it")
            if (allocated(error)) return
        end do

        do j = 1, size(lambdas)
            nfast = 0_int64
            call rng%seed(dist_seed, int(400 + j, int64))
            do k = 1_int64, NDRAW
                call parquet_debug_poisson_path(rng, lambdas(j), kk, path, ok)
                if (path == 2_int32) nfast = nfast + 1_int64
                call check(error, ok, &
                    "PTRS's fast-acceptance region accepted a candidate the full logarithmic test would have " // &
                    "rejected, so the region is not contained in the acceptance region and every draw at this " // &
                    "lambda is biased")
                if (allocated(error)) return
            end do
            call check(error, nfast > 0_int64, &
                "PTRS's fast-acceptance region never fired at this lambda, so this test checked nothing about it")
            if (allocated(error)) return
        end do
    end subroutine test_squeezes_are_valid

    ! ---- helpers ---------------------------------------------------------------------------

    !> The 0.999 point of a chi-square distribution with `df` degrees of freedom.
    !!
    !! Wilson-Hilferty: `chi2_p(df) ~ df * (1 - 2/(9 df) + z_p * sqrt(2/(9 df)))**3`, accurate to
    !! well under 1 % for `df >= 5` and conservative below it. Used rather than a fixed constant
    !! because `poisson_pmf_chi2` builds its cells from the pmf, so the degrees of freedom depend
    !! on lambda and a hard-coded threshold would silently be the wrong one at some of them.
    pure function chi2_999(df) result(c)
        integer, intent(in) :: df           !! degrees of freedom, at least 1
        real(real64) :: c                   !! the 0.999 quantile
        real(real64), parameter :: Z999 = 3.090232306167813_real64
        real(real64) :: d, h
        d = real(max(df, 1), real64)
        h = 2.0_real64 / (9.0_real64 * d)
        c = d * (1.0_real64 - h + Z999 * sqrt(h)) ** 3
    end function chi2_999

    !> Gap between two values in units of the last place of `want`, as a `real64`.
    !!
    !! `spacing` is the intrinsic for "one ulp at this magnitude". A zero `want` has no meaningful
    !! ulp, so the two must simply be equal there.
    pure function ulp_gap(got, want) result(g)
        real(real64), intent(in) :: got     !! the value under test
        real(real64), intent(in) :: want    !! the reference
        real(real64) :: g                   !! `|got - want|` in ulp of `want`, or 0 / huge at zero
        if (want == 0.0_real64) then
            g = merge(0.0_real64, huge(1.0_real64), got == 0.0_real64)
        else
            g = abs(got - want) / spacing(want)
        end if
    end function ulp_gap

    !> Draws `n` exponentials scaled by `1/rate` and reports three test statistics.
    !!
    !! Scaling by `rate` is what makes the negative control possible: `rate = 1` is the library's
    !! own draw, and any other value is a distribution with the same shape and the wrong scale,
    !! which is precisely the defect a shape-only test cannot see.
    subroutine exp_gate(seed, n, rate, z_mean, z_var, chi2)
        integer(int64), intent(in) :: seed      !! the family to draw from
        integer(int64), intent(in) :: n         !! how many draws to take
        real(real64), intent(in) :: rate        !! 1 for the real draws; anything else is a control
        real(real64), intent(out) :: z_mean     !! standard errors of the mean away from 1
        real(real64), intent(out) :: z_var      !! standard errors of the variance away from 1
        real(real64), intent(out) :: chi2       !! chi-square of 1 - exp(-x) over 20 cells
        integer, parameter :: NCELL = 20
        integer(int64) :: k, counts(NCELL), cell
        real(real64) :: x, s, s2, mean, var, expect, dn

        counts = 0_int64
        s = 0.0_real64
        s2 = 0.0_real64
        do k = 1_int64, n
            x = pf_random_exp_at(seed, 77_int64, k) / rate
            s = s + x
            s2 = s2 + x * x
            ! 1 - exp(-x) is Uniform(0,1) exactly when x is Exp(1), so the cell index is a
            ! distribution-free restatement of the whole shape rather than of its first moments.
            cell = min(int(real(NCELL, real64) * (1.0_real64 - exp(-x)), int64) + 1_int64, int(NCELL, int64))
            counts(cell) = counts(cell) + 1_int64
        end do
        dn = real(n, real64)
        mean = s / dn
        var = s2 / dn - mean * mean
        ! Exp(1) has mean 1, variance 1 and fourth central moment 9, so var(s^2) = 8/n.
        z_mean = (mean - 1.0_real64) * sqrt(dn)
        z_var = (var - 1.0_real64) * sqrt(dn / 8.0_real64)
        expect = dn / real(NCELL, real64)
        chi2 = sum((real(counts, real64) - expect) ** 2 / expect)
    end subroutine exp_gate

end module test_random_dist
