!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Tests for `parquet_random`'s DISTRIBUTIONS -- the exponential, and everything Phase 3 adds
!> beyond the uniforms -- and for its POINTS ON A SPHERE, which are distributions over directions.
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

    ! NARROW import, not `use parquet`. The facade would compile just as well and would let a
    ! future test in this file reach the C++ layer without anything saying so; naming the tier
    ! makes that a build error instead. The facade's own re-export of this tier is pinned by
    ! `test_facade_covers_every_layer` (test/test_examples.f90), which is where that claim lives.
    use parquet_random
    use parquet_expkey, only: parquet_debug_exp_key
    ! The points-on-a-sphere gates measure what they draw with the pixelisation's equal-area cells and
    ! the two separations, which know nothing of how the draws were made.
    use parquet_healpix, only: pf_vec2pix_ring, pf_angdist
    use parquet_skycoord, only: pf_angdist_deg
    use test_random_vectors
    use iso_fortran_env, only: int32, int64, real32, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none
    private
    public :: collect_tests_parquet_random_dist

    !> Seed the distributional gates draw under. Any value would do; a fixed one keeps the suite
    !! deterministic, which is the whole premise of the module being tested.
    integer(int64), parameter :: dist_seed = 20260821_int64

    !> Standardised magnitude past which the truncated-normal fixtures stop computing.
    !!
    !! `tn_q` and `tn_phi` return a flat 0 or 1 out here, which is both what keeps `x*x` from
    !! overflowing for an infinite bound and the widest window `trunc_gate`'s flat control can be
    !! drawn on -- so the three share the one constant, and a control drawn past a cutoff the
    !! conditional CDF has already saturated is a sample of one cell.
    !!
    !! **37 rather than a rounder 40 because that is the largest magnitude at which both
    !! `erfc(x/sqrt(2))` and `exp(-x*x/2)` are still NORMAL numbers**: at 38 both are subnormal,
    !! and producing one raises `IEEE_UNDERFLOW`, which nagfor reports as a line at program exit
    !! attached to nothing. Nothing is lost by stopping here -- `P(Z > 37)` is about `6e-300`,
    !! below one ulp of any interval mass the gate divides by, so every cell index is the one the
    !! uncut function gives.
    real(real64), parameter :: TN_CUT = 37.0_real64

    !> `pi` and friends for the points-on-a-sphere gates.
    real(real64), parameter :: PI = 3.14159265358979323846264338327950288_real64
    !> `2*pi`.
    real(real64), parameter :: TWO_PI = 2.0_real64 * PI
    !> Radians per degree.
    real(real64), parameter :: DEG = PI / 180.0_real64
    !> The `+z` axis.
    real(real64), parameter :: ZAXIS(3) = [0.0_real64, 0.0_real64, 1.0_real64]
    !> A sphere result against the 60-digit model: 32 ulp of 1, ABSOLUTE, for a component of a unit
    !! vector or a rotation. Measured on the rows the generator pins, the cancellation-free transforms
    !! land within 6 ulp; a cancelling one misses the small-radius and small-`kappa` rows by millions.
    real(real64), parameter :: SPH_TOL = 32.0_real64 * epsilon(1.0_real64)
    !> A sky position against the 60-digit model, in degrees: on `dec`, and on `ra` scaled by `cos(dec)`.
    real(real64), parameter :: SPH_TOL_DEG = 1.0e-12_real64
    !> Two forms that compute one value through one body -- a tier, a kind, a `do concurrent` -- agree to
    !! a few ulp rather than to the bit, since a compiler may round two call sites differently.
    real(real64), parameter :: SPH_SAME = 4.0_real64 * epsilon(1.0_real64)

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
                         test_squeezes_are_valid), &
            new_unittest("the truncated normal is right on each case's own interval, with a uniform control", &
                         test_normal_trunc_distribution), &
            new_unittest("every reachable truncated-normal case is taken, and each threshold decides which", &
                         test_normal_trunc_cases), &
            new_unittest("every truncated draw lands inside its bounds, at an ill-conditioned scale", &
                         test_normal_trunc_containment), &
            new_unittest("both truncated envelopes are exact: the accept probability never exceeds 1", &
                         test_normal_trunc_envelopes), &
            new_unittest("the uniform proposal is right at its WIDEST interval, where a flat draw would show", &
                         test_normal_trunc_case_two_widest), &
            new_unittest("a huge bound and a true infinity are the same truncation", &
                         test_normal_trunc_unbounded_forms), &
            new_unittest("the truncated normal is pure: usable in do concurrent, same values", &
                         test_normal_trunc_purity), &
            new_unittest("the truncated normal's stream cost is variable, observed, and rewind-exact", &
                         test_normal_trunc_cost), &
            new_unittest("a far-out interval reaches the tilted case and terminates, at every scale", &
                         test_normal_trunc_far_tail_terminates), &
            new_unittest("golden vectors: every points-on-a-sphere family, its uniforms exact and its values to 32 ulp", &
                         test_sphere_golden), &
            new_unittest("the sphere producers, their tier-0 forms and their fills are one grid, one block per draw", &
                         test_sphere_tiers_agree), &
            new_unittest("every sphere family is independent of the others and of pf_random_at, with a coupled control", &
                         test_sphere_families_independent), &
            new_unittest("each RA/Dec sphere form is its vector form in the standard frame, and a pole's RA is 0", &
                         test_sphere_radec_is_the_vector_form), &
            new_unittest("directions are uniform: mean, marginals, equal-area pixels, and a rejected control", &
                         test_direction_is_uniform), &
            new_unittest("a disc is uniform over its ring and contained in it, about any centre, at every limit", &
                         test_disc_is_uniform_and_contained), &
            new_unittest("a sky disc stays uniform across the pole and the wrap, and qfeet's flat disc does not", &
                         test_disc_radec_across_the_pole_and_the_wrap), &
            new_unittest("points in a ball or a shell are uniform in volume, with a rejected control", &
                         test_ball_is_uniform_in_volume), &
            new_unittest("the vMF draw: mean cosine, exact CDF, uniform and Gaussian limits, and a control", &
                         test_vmf_moments_and_limits), &
            new_unittest("rotations are proper, orthonormal and Haar-uniform, with a rejected control", &
                         test_rotation_is_haar), &
            new_unittest("every tier-0 sphere form is pure: usable in do concurrent, same values", &
                         test_sphere_purity), &
            new_unittest("the int32 and int64 stream-index specifics of every sphere form agree", &
                         test_sphere_kinds_agree), &
            new_unittest("draw 2**62 is the last a sphere form or fill accepts, and a draw below 1 clamps", &
                         test_sphere_draw_bound), &
            new_unittest("the spare-bit pair keeps its uniforms exact and its integer unbiased", &
                         test_pair_spare) &
            ]
    end subroutine collect_tests_parquet_random_dist

    !> `pf_random_pair_spare_at`: the pair is untouched, the integer is exactly uniform, and the
    !! fallback fires exactly where the 22 spare bits cannot decide.
    !!
    !! **The pair assertion is the load-bearing one.** The whole point of the routine is that a
    !! caller's two uniforms do not move when an integer is taken from the bits their conversions
    !! discard; if they did, every mask point would shift and no distributional test would say so.
    !! It is asserted TO THE BIT against `pf_random_fill_draws` at the same coordinates, which is
    !! where those uniforms are defined.
    !!
    !! The uniformity gate has a control: the rejection rate is required to sit near `w/2**22`,
    !! which fails if the lazy guard were widened into accepting everything (that would bias the
    !! integer while leaving it in range, so a range check alone would pass).
    subroutine test_pair_spare(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 400000_int64
        integer(int64), parameter :: WIDTHS(4) = [2_int64, 7_int64, 501_int64, 1000_int64]
        integer(int64) :: k, j, w, cnt(1000), tot, nfall
        real(real64) :: u1, u2, uv(2), worst, expect, chi2, rate, bound
        logical :: ok
        character(len=160) :: msg

        ! 1. The pair is exactly the block's two uniforms, whatever the integer does.
        worst = 0.0_real64
        do k = 1_int64, 20000_int64
            call pf_random_pair_spare_at(dist_seed, 3_int64, 1_int64, 1000_int64, u1, u2, j, ok, k)
            call pf_random_fill_draws(dist_seed, 3_int64, uv, k + k - 1_int64)
            worst = max(worst, max(abs(u1 - uv(1)), abs(u2 - uv(2))))
            call check(error, j >= 1_int64 .and. j <= 1000_int64, "the spare-bit integer left its range")
            if (allocated(error)) return
        end do
        call check(error, worst == 0.0_real64, &
            "pf_random_pair_spare_at's uniforms are not pf_random_fill_draws' pair to the bit: taking the " // &
            "integer from the spare bits moved the caller's point")
        if (allocated(error)) return

        ! 2. The integer is uniform over the draws the spare bits decided.
        do w = 1_int64, int(size(WIDTHS), int64)
            cnt = 0_int64
            tot = 0_int64
            do k = 1_int64, NDRAW
                call pf_random_pair_spare_at(dist_seed, 5_int64, 1_int64, WIDTHS(w), u1, u2, j, ok, k)
                if (ok) then
                    cnt(j) = cnt(j) + 1_int64
                    tot = tot + 1_int64
                end if
            end do
            expect = real(tot, real64) / real(WIDTHS(w), real64)
            chi2 = sum((real(cnt(1:WIDTHS(w)), real64) - expect) ** 2 / expect)
            write (msg, '(a,i0,a,f12.3)') "the spare-bit integer is not uniform at width ", WIDTHS(w), ": chi2 = ", chi2
            call check(error, chi2 <= chi2_999(int(WIDTHS(w) - 1_int64)), trim(msg))
            if (allocated(error)) return
        end do

        ! 3. A width the 22 bits cannot span never decides; the largest they can always does.
        nfall = 0_int64
        do k = 1_int64, 2000_int64
            call pf_random_pair_spare_at(dist_seed, 7_int64, 1_int64, 4194305_int64, u1, u2, j, ok, k)
            if (.not. ok) nfall = nfall + 1_int64
        end do
        call check(error, nfall == 2000_int64, "a width above 2**22 was decided from 22 bits")
        if (allocated(error)) return
        nfall = 0_int64
        do k = 1_int64, 2000_int64
            call pf_random_pair_spare_at(dist_seed, 7_int64, 1_int64, 4194304_int64, u1, u2, j, ok, k)
            if (.not. ok) nfall = nfall + 1_int64
        end do
        call check(error, nfall == 0_int64, "a width of exactly 2**22 divides the 22 bits evenly and cannot reject")
        if (allocated(error)) return

        ! 4. CONTROL: the rejection rate is not merely small, it is Lemire's EXACT one,
        ! `mod(2**22, w)/2**22`. A lazy guard widened into accepting everything would bias the
        ! integer while leaving it in range, and the uniformity gate above is too weak to catch
        ! that on its own; this pins the threshold itself.
        w = 3000000_int64
        nfall = 0_int64
        do k = 1_int64, NDRAW
            call pf_random_pair_spare_at(dist_seed, 9_int64, 1_int64, w, u1, u2, j, ok, k)
            if (.not. ok) nfall = nfall + 1_int64
        end do
        rate = real(nfall, real64) / real(NDRAW, real64)
        expect = real(mod(4194304_int64, w), real64) / 4194304.0_real64
        bound = 4.0_real64 * sqrt(expect * (1.0_real64 - expect) / real(NDRAW, real64))
        write (msg, '(a,f8.5,a,f8.5,a,f8.5,a)') "the spare-bit rejection rate is ", rate, ", not Lemire's ", &
            expect, " (4 SE = ", bound, ")"
        call check(error, abs(rate - expect) <= bound, trim(msg))
        if (allocated(error)) return

        ! 5. `lo > hi` swaps rather than failing, as pf_random_int_at does.
        call pf_random_pair_spare_at(dist_seed, 11_int64, 40_int64, 10_int64, u1, u2, j, ok, 1_int64)
        call check(error, j >= 10_int64 .and. j <= 40_int64, "a reversed range was not swapped")
    end subroutine test_pair_spare

    !> Every row of the exponential golden table, against the oracle appropriate to each column.
    !!
    !! **Four assertions per row, and they are four different claims.** `exp_u_bits` pins which
    !! UNIFORM the coordinate names -- without it the exponential table and the scalar table could
    !! drift onto different draw mappings and each stay self-consistent. `exp_portable_bits` pins
    !! the frozen transform exactly. `exp_ref_bits` pins the libm-backed form to an
    !! arbitrary-precision logarithm within a few ulp, which is the strongest statement available
    !! about a value whose last bit belongs to whichever libm is linked.
    !!
    !! **And `parquet_debug_exp_key` must BE that frozen transform, applied to `1 - u`.** It is the
    !! only public name `parquet_expkey` exposes for `exp_key`, it exists because that module reaches
    !! no `bind(C)` surface and so cannot use the C++-side debug-hook convention, and until this the
    !! only caller anywhere was `tools/check_exp_key.f90` -- a standalone driver a bare compiler
    !! builds, which `fpm test` never runs. So the hook the sweeps depend on was itself unexercised
    !! by the suite. Asserting the identity rather than re-deriving a value is what makes this a test
    !! of the hook: a wrapper that had drifted onto some other logarithm would satisfy any
    !! self-consistent check of its own output and fails this one.
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

            call check(error, transfer(parquet_debug_exp_key(1.0_real64 - u), 0_int64) == exp_portable_bits(k), &
                "parquet_debug_exp_key(1 - u) is not bit-for-bit pf_random_exp_portable_at, so the public hook " // &
                "the tools/check_exp_key.sh sweeps drive is no longer the transform the library itself uses")
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
        if (allocated(error)) return

        ! The one input whose answer is a whole number rather than a bit pattern, and the top of the
        ! documented domain: `exp_key`'s doc-comment states `u = 1` gives exactly 0, and no golden row
        ! can reach it because a uniform draw is in `[0, 1)` and `1 - u` is therefore never 1.
        call check(error, parquet_debug_exp_key(1.0_real64) == 0.0_real64, &
            "parquet_debug_exp_key(1) is not exactly 0, so -log(1) has stopped being exact at the top of the " // &
            "transform's documented domain")
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

        ! **The same two-way reach for the BULK fills**, which are their own specifics rather than
        ! the elemental scalar under another name: `pf_random_fill_exp` and its portable twin each
        ! have an int32-index forwarder that nothing above can reach, and a forwarder wired to the
        ! wrong worker or losing the sign extension is a silently different stream, not a build
        ! failure. Compared against the int64-index fills taken at the top of this test.
        call pf_random_fill_exp(dist_seed, i32, part(1:3))
        call check(error, all(part(1:3) == v(1:3)), &
            "pf_random_fill_exp from an int32 stream index disagrees with the int64 one")
        if (allocated(error)) return
        call pf_random_fill_exp_portable(dist_seed, i32, part(1:3))
        call check(error, all(part(1:3) == w(1:3)), &
            "pf_random_fill_exp_portable from an int32 stream index disagrees with the int64 one")
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

        ! The bulk fills' own int32-index forwarders, for the reason `test_exp_cross_form` gives at
        ! the same point: they are separate procedures from the elemental scalars checked above.
        call pf_random_fill_normal(dist_seed, i32, v3)
        call check(error, all(v3 == v(1:3)), &
            "pf_random_fill_normal from an int32 stream index disagrees with the int64 one")
        if (allocated(error)) return
        call pf_random_fill_normal_portable(dist_seed, i32, v3)
        call check(error, all(v3 == w(1:3)), &
            "pf_random_fill_normal_portable from an int32 stream index disagrees with the int64 one")
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
    !! **This is `.claude/rules/testing.md`'s "Mutation testing", made explicit.** The
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

    ! ================================================================================
    ! The truncated normal
    ! ================================================================================

    !> The truncated normal has the right shape on an interval from each of its three cases.
    !!
    !! **The negative control is a UNIFORM sample on the same interval**, not a rescaled normal,
    !! because that is precisely what a wrong envelope anchor returns (`feature_risks.md`
    !! Risk-249). A gate that accepted it would be measuring nothing on the narrow intervals where
    !! the uniform proposal runs -- which are the intervals where a truncated normal most resembles
    !! a uniform, and so the ones a careless gate passes blind.
    subroutine test_normal_trunc_distribution(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 200000_int64
        real(real64) :: z_mean, z_var, chi2
        integer :: c
        real(real64) :: lo(3), hi(3)
        character(len=16) :: what(3)

        ! One interval per case: naive (straddling and wide), uniform proposal (a tail sliver),
        ! exponential tilting (a one-sided tail).
        lo = [-1.0_real64, 3.0_real64, 4.0_real64]
        hi = [2.0_real64, 3.2_real64, huge(1.0_real64)]
        what = ["naive          ", "uniform        ", "tilted         "]

        do c = 1, 3
            call trunc_gate(dist_seed, NDRAW, lo(c), hi(c), .false., z_mean, z_var, chi2)
            call check(error, abs(z_mean) <= 4.5_real64, &
                "the truncated normal's mean is more than 4.5 SE from the exact conditional mean, case " // &
                trim(what(c)))
            if (allocated(error)) return
            call check(error, abs(z_var) <= 5.0_real64, &
                "the truncated normal's variance is more than 5 SE from the exact conditional variance, case " // &
                trim(what(c)))
            if (allocated(error)) return
            call check(error, chi2 <= 43.8_real64, &
                "the truncated normal's conditional CDF is not uniform over 20 equiprobable cells at the 0.999 " // &
                "level, case " // trim(what(c)))
            if (allocated(error)) return

            ! NEGATIVE CONTROL: uniform draws on the same interval must be rejected.
            call trunc_gate(dist_seed, NDRAW, lo(c), hi(c), .true., z_mean, z_var, chi2)
            call check(error, chi2 > 43.8_real64, &
                "the gate ACCEPTS a flat sample on the same interval, so it is not measuring the shape at all " // &
                "and its verdict on the real draws means nothing, case " // trim(what(c)))
            if (allocated(error)) return
        end do
    end subroutine test_normal_trunc_distribution

    !> Every reachable case is taken, `path = 4` never is, and each threshold decides its own side.
    !!
    !! **The thresholds are recomputed here from the written specification**, not imported from
    !! `parquet_random`, for the reason `test_generic_stride_aliasing` recomputes its domain tags:
    !! a test borrowing the library's constant cannot tell a wrong constant from a wrong rule.
    subroutine test_normal_trunc_cases(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        integer(int64) :: tries, npath(6)
        integer(int32) :: path
        real(real64) :: x, a, w, sq2pi
        integer :: k

        sq2pi = sqrt(8.0_real64 * atan(1.0_real64))      ! sqrt(2*pi), built rather than quoted
        npath = 0_int64
        call rng%seed(dist_seed, 71_int64)

        ! Six intervals, chosen for the case each should take.
        call probe_path(rng, -2.0_real64, 2.0_real64, npath, path)            ! naive
        call probe_path(rng, -0.2_real64, 0.2_real64, npath, path)            ! uniform, straddling
        call probe_path(rng, 4.0_real64, huge(1.0_real64), npath, path)       ! tilted
        call probe_path(rng, -0.2_real64, -0.05_real64, npath, path)          ! uniform, mirrored
        call probe_path(rng, -huge(1.0_real64), -4.0_real64, npath, path)     ! tilted, mirrored
        call probe_path(rng, 0.0_real64, 0.05_real64, npath, path)            ! uniform, a == 0

        call check(error, npath(1) > 0 .and. npath(2) > 0 .and. npath(3) > 0, &
            "one of the three unmirrored truncated-normal cases was never reached, so the case rule has " // &
            "collapsed onto fewer proposals than it ships with")
        if (allocated(error)) return
        call check(error, npath(5) > 0 .and. npath(6) > 0, &
            "a wholly non-positive interval did not report a MIRRORED path, so either the mirror is gone or " // &
            "the path code no longer records it -- and a lost mirror returns a near-uniform draw")
        if (allocated(error)) return
        call check(error, npath(4) == 0, &
            "a draw reported path 4 (mirrored naive), which cannot occur: naive rejection is reachable only " // &
            "from the straddling arm, and the straddling arm is never mirrored")
        if (allocated(error)) return

        ! The straddling threshold: sqrt(2*pi) wide takes naive, a hair narrower takes uniform.
        ! Both are centred, so only the WIDTH differs between the two calls.
        call rng%seed(dist_seed, 72_int64)
        call parquet_debug_normal_truncated_path(rng, -0.5_real64 * sq2pi * 1.001_real64, &
            0.5_real64 * sq2pi * 1.001_real64, x, path, tries)
        call check(error, path == 1_int32, &
            "an interval wider than sqrt(2*pi) did not take naive rejection, so the straddling threshold moved")
        if (allocated(error)) return
        call parquet_debug_normal_truncated_path(rng, -0.5_real64 * sq2pi * 0.999_real64, &
            0.5_real64 * sq2pi * 0.999_real64, x, path, tries)
        call check(error, path == 2_int32, &
            "an interval narrower than sqrt(2*pi) did not take the uniform proposal, so the straddling " // &
            "threshold moved -- and which proposal runs decides the value, not just the speed")
        if (allocated(error)) return

        ! The tail threshold, at three lower bounds spanning the useful range.
        do k = 1, 3
            a = real(k, real64)
            w = spec_threshold(a)
            call parquet_debug_normal_truncated_path(rng, a, a + w * 1.01_real64, x, path, tries)
            call check(error, path == 3_int32, &
                "a tail interval wider than the derived threshold did not take exponential tilting")
            if (allocated(error)) return
            call parquet_debug_normal_truncated_path(rng, a, a + w * 0.99_real64, x, path, tries)
            call check(error, path == 2_int32, &
                "a tail interval narrower than the derived threshold did not take the uniform proposal, so " // &
                "the library's tn_threshold no longer agrees with the specification recomputed here")
            if (allocated(error)) return
        end do
    end subroutine test_normal_trunc_cases

    !> Every draw lands inside the requested bounds, including where the round trip fights back.
    !!
    !! **This is the test for the clamp in `tn_finish`, and it is not vacuous: it FAILS without
    !! it.** `z` is drawn inside the standardised interval, but `mu + sigma*z` need not round back
    !! inside `[lo, hi]` once `mu` and `sigma` are nowhere near the bounds -- and a draw landing
    !! exactly on a bound is not rare, it is most of them for a tight interval far from `mu`.
    !! Measured with the clamp removed, 300 000 draws per arm: arm 1 put 7 584 below `lo` and
    !! 7 608 above `hi`, arm 2 put 38 566 below `lo`. Mutation: replace either `if` in `tn_finish`
    !! with `if (.false.)` and this test must fail.
    !!
    !! The `lo = 0` arm is the one that matters in practice, and it is deliberately kept even
    !! though it is the weakest of the three: a negative value from a positivity truncation flows
    !! into a `sqrt` or a physical count somewhere else entirely.
    subroutine test_normal_trunc_containment(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 60000_int64
        type(pf_random_stream) :: rng
        integer(int64) :: k, outside, negative, on_edge
        real(real64) :: x, mu, sg, lo, hi, u

        ! Arm 1: bounds chosen independently of mu and sigma, both roaming over decades, so the
        ! standardisation is ill-conditioned in both directions.
        outside = 0_int64
        on_edge = 0_int64
        call rng%seed(dist_seed, 73_int64)
        do k = 1_int64, NDRAW
            call rng%uniform(u)
            mu = (u - 0.5_real64) * 2.0e6_real64
            call rng%uniform(u)
            sg = 10.0_real64 ** ((u - 0.5_real64) * 8.0_real64)
            call rng%uniform(u)
            lo = (u - 0.5_real64) * 2.0e6_real64
            call rng%uniform(u)
            hi = lo + sg * (0.01_real64 + 3.0_real64 * u)
            if (.not. (lo < hi)) cycle
            if (.not. ((lo - mu) / sg < (hi - mu) / sg)) cycle
            call rng%normal_truncated(lo, hi, x, mu=mu, sigma=sg)
            if (x < lo .or. x > hi) outside = outside + 1_int64
            if (x == lo .or. x == hi) on_edge = on_edge + 1_int64
        end do
        call check(error, outside == 0_int64, &
            "a truncated normal returned a value OUTSIDE the interval it was asked for, at an ill-conditioned " // &
            "combination of mu, sigma and bounds -- the de-standardised result is not being clamped back in")
        if (allocated(error)) return
        ! Vacuity guard: the clamp only bites on draws that land on a bound, so a fixture reaching
        ! none of them would pass against a build with no clamp at all.
        call check(error, on_edge > 0_int64, &
            "no draw in arm 1 landed exactly on a bound, so this fixture never reached the rounding case the " // &
            "clamp exists for and its verdict is vacuous")
        if (allocated(error)) return

        ! Arm 2: a tight physical interval a long way from mu, with a small sigma -- so the
        ! standardised bounds are enormous and almost every draw lands on one of them.
        outside = 0_int64
        on_edge = 0_int64
        call rng%seed(dist_seed, 74_int64)
        do k = 1_int64, NDRAW
            call rng%uniform(u)
            mu = (u - 0.5_real64) * 2.0_real64
            sg = 1.0e-9_real64
            call rng%uniform(u)
            lo = 1.0e3_real64 * (1.0_real64 + u)
            hi = lo + 1.0e-7_real64
            call rng%normal_truncated(lo, hi, x, mu=mu, sigma=sg)
            if (x < lo .or. x > hi) outside = outside + 1_int64
            if (x == lo .or. x == hi) on_edge = on_edge + 1_int64
        end do
        call check(error, outside == 0_int64, &
            "a truncated normal on a tight interval far from mu returned a value outside it")
        if (allocated(error)) return
        call check(error, on_edge > 0_int64, &
            "no draw in arm 2 landed exactly on a bound, so this fixture is vacuous")
        if (allocated(error)) return

        ! Arm 3: a positivity truncation, which is what this costs in practice.
        negative = 0_int64
        call rng%seed(dist_seed, 75_int64)
        do k = 1_int64, NDRAW
            call rng%uniform(u)
            mu = (u - 0.5_real64) * 100.0_real64
            call rng%uniform(u)
            sg = 10.0_real64 ** ((u - 0.5_real64) * 6.0_real64)
            call rng%normal_truncated(0.0_real64, huge(1.0_real64), x, mu=mu, sigma=sg)
            if (x < 0.0_real64) negative = negative + 1_int64
        end do
        call check(error, negative == 0_int64, &
            "a truncated normal with lo = 0 returned a NEGATIVE value")
    end subroutine test_normal_trunc_containment

    !> Both rejection envelopes are exact: the accept probability is never above 1.
    !!
    !! Asserted DIRECTLY rather than through a moment, the way `test_squeezes_are_valid` asserts
    !! the Gamma squeeze. An envelope anchored at the wrong point makes the accept test pass for
    !! every candidate over part of the interval, which returns the PROPOSAL -- a uniform, or a
    !! truncated exponential -- and no coarse distributional gate can see the difference
    !! (`feature_risks.md` Risk-249). The formulas are written out here from the specification.
    subroutine test_normal_trunc_envelopes(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NPT = 400
        real(real64) :: a, b, m, z, lam, p, worst
        integer :: j, c
        real(real64) :: los(4), his(4)

        ! The uniform proposal: p(z) = exp((m*m - z*z)/2) with m the point of [a,b] nearest zero.
        los = [-0.3_real64, 0.0_real64, 2.0_real64, -1.2_real64]
        his = [0.9_real64, 0.4_real64, 2.3_real64, -0.7_real64]
        do c = 1, 4
            a = los(c)
            b = his(c)
            m = a
            if (b < 0.0_real64) m = b
            if (a < 0.0_real64 .and. b > 0.0_real64) m = 0.0_real64
            worst = 0.0_real64
            do j = 0, NPT
                z = a + (b - a) * real(j, real64) / real(NPT, real64)
                p = exp(0.5_real64 * (m * m - z * z))
                worst = max(worst, p)
                call check(error, p <= 1.0_real64, &
                    "the uniform proposal's accept probability exceeds 1 somewhere on the interval, so its " // &
                    "envelope is anchored away from the target's maximum and the draw degrades toward flat")
                if (allocated(error)) return
            end do
            call check(error, abs(worst - 1.0_real64) <= 1.0e-12_real64, &
                "the uniform proposal's accept probability never REACHES 1 on the interval, so its envelope is " // &
                "loose: the anchor is not the target's maximum and every draw costs more than it should")
            if (allocated(error)) return
        end do

        ! Exponential tilting: p(z) = exp(-(z - lam)**2 / 2), maximal at z = lam, and lam must
        ! solve lam*lam = a*lam + 1 and lie at or above a.
        do c = 1, 4
            a = real(c - 1, real64) * 2.0_real64
            lam = 0.5_real64 * (a + sqrt(a * a + 4.0_real64))
            call check(error, abs(lam * lam - (a * lam + 1.0_real64)) <= 1.0e-12_real64 * max(1.0_real64, lam * lam), &
                "the tilting rate does not solve lam*lam = a*lam + 1, which is what makes its envelope the " // &
                "tightest exponential one and its accept probability at most 1")
            if (allocated(error)) return
            call check(error, lam >= a, "the tilting rate fell below the lower bound, so the envelope's maximum " // &
                "is outside the support and the accept test is no longer bounded by 1")
            if (allocated(error)) return
            do j = 0, NPT
                z = a + 12.0_real64 * real(j, real64) / real(NPT, real64)
                p = exp(-0.5_real64 * (z - lam) ** 2)
                call check(error, p <= 1.0_real64, &
                    "the tilted proposal's accept probability exceeds 1 somewhere above the lower bound")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_normal_trunc_envelopes

    !> The uniform proposal at the WIDEST interval it ever runs on, where a flat draw would show.
    !!
    !! **Where the previous test is exact, this one is where the distortion is largest.** The
    !! uniform proposal only ever runs on intervals narrower than its threshold, and a truncated
    !! normal on a narrow interval is nearly flat -- so a test on a conveniently tiny interval
    !! cannot tell the two apart. This one sits at 99.9 % of the threshold, and its control is a
    !! flat sample on the same interval, which must be rejected.
    subroutine test_normal_trunc_case_two_widest(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 400000_int64
        type(pf_random_stream) :: rng
        real(real64) :: a, b, x, z_mean, z_var, chi2
        integer(int64) :: tries
        integer(int32) :: path

        a = 0.0_real64
        b = a + spec_threshold(a) * 0.999_real64

        ! It must actually BE the uniform proposal, or this test is measuring another case.
        call rng%seed(dist_seed, 75_int64)
        call parquet_debug_normal_truncated_path(rng, a, b, x, path, tries)
        call check(error, path == 2_int32, &
            "the widest case-2 interval no longer takes the uniform proposal, so this test is not exercising " // &
            "the case it was written for")
        if (allocated(error)) return

        call trunc_gate(dist_seed, NDRAW, a, b, .false., z_mean, z_var, chi2)
        call check(error, chi2 <= 43.8_real64, &
            "the uniform proposal's output is not the truncated normal at the widest interval it runs on")
        if (allocated(error)) return
        call check(error, abs(z_var) <= 5.0_real64, &
            "the uniform proposal's variance is wrong at the widest interval it runs on")
        if (allocated(error)) return

        call trunc_gate(dist_seed, NDRAW, a, b, .true., z_mean, z_var, chi2)
        call check(error, chi2 > 43.8_real64, &
            "the gate ACCEPTS a flat sample at the widest case-2 interval, so it cannot see the one defect " // &
            "the uniform proposal can have -- returning its own proposal unfiltered")
    end subroutine test_normal_trunc_case_two_widest

    !> A `huge()` bound and a true infinity are the same truncation, value for value.
    !!
    !! The case rule and both accept tests are written so that an infinite bound needs no special
    !! case; this asserts that, so a caller need not reach for `ieee_arithmetic` to truncate on one
    !! side. The infinity is built with `ieee_value`, never as an overflowing expression, because
    !! nagfor traps the overflow that would build it arithmetically.
    !!
    !! **The last three arms are the ones the two above cannot reach**, and every one of them
    !! aborted under nagfor before `tn_standardise` and `tn_width` existed: standardising a
    !! `huge()` bound by a `sigma` below one, spanning two of them at once, and centring one on a
    !! `mu` of the opposite sign at the same magnitude. Each is representable only as the infinity
    !! the equivalence promises, and the plain arithmetic reached that infinity by raising
    !! `IEEE_OVERFLOW` on the way. They assert values rather than "it did not abort", so they say
    !! the same thing on a compiler that masks the trap.
    subroutine test_normal_trunc_unbounded_forms(error)
        use ieee_arithmetic, only: ieee_value, ieee_positive_inf, ieee_negative_inf
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        integer :: k
        real(real64) :: x1, x2, pinf, ninf

        pinf = ieee_value(0.0_real64, ieee_positive_inf)
        ninf = ieee_value(0.0_real64, ieee_negative_inf)

        do k = 1, 200
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(2.0_real64, huge(1.0_real64), x1)
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(2.0_real64, pinf, x2)
            call check(error, x1 == x2, &
                "an upper bound of huge(1.0_real64) and one of +Infinity gave different draws at the same " // &
                "coordinates, so one of them is taking a path the other is not")
            if (allocated(error)) return

            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(-huge(1.0_real64), -2.0_real64, x1)
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(ninf, -2.0_real64, x2)
            call check(error, x1 == x2, &
                "a lower bound of -huge(1.0_real64) and one of -Infinity gave different draws at the same " // &
                "coordinates")
            if (allocated(error)) return
        end do

        ! Both-sides-unbounded is an ordinary standard normal, and must accept every candidate.
        call rng%seed(dist_seed, 76_int64)
        call rng%normal_truncated(ninf, pinf, x1)
        call check(error, x1 == x1, "an unbounded truncated normal returned a NaN")
        if (allocated(error)) return

        ! A sigma below one: `huge(1.0_real64)/sigma` is past the top of the range.
        do k = 1, 200
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(0.0_real64, huge(1.0_real64), x1, mu=-0.25_real64, sigma=0.5_real64)
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(0.0_real64, pinf, x2, mu=-0.25_real64, sigma=0.5_real64)
            call check(error, x1 == x2, &
                "a huge() upper bound and +Infinity gave different draws under a sigma below one, where the " // &
                "standardised bound is no longer representable")
            if (allocated(error)) return

            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(-huge(1.0_real64), 0.0_real64, x1, mu=0.25_real64, sigma=0.5_real64)
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(ninf, 0.0_real64, x2, mu=0.25_real64, sigma=0.5_real64)
            call check(error, x1 == x2, &
                "a -huge() lower bound and -Infinity gave different draws under a sigma below one")
            if (allocated(error)) return
        end do

        ! Two huge() bounds at once: the standardised WIDTH is what is not representable here,
        ! and every case rule is decided by comparing it against a threshold below exp(0.5).
        do k = 1, 50
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(-huge(1.0_real64), huge(1.0_real64), x1)
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(ninf, pinf, x2)
            call check(error, x1 == x2, &
                "-huge() to huge() and -Infinity to +Infinity gave different draws, so the untruncated " // &
                "spelling of an untruncated draw is not the untruncated draw")
            if (allocated(error)) return
        end do

        ! A mu a full range from the bound, where the CENTRING is what overflows. `sigma` is large
        ! enough to bring the standardised bound back to 1e8: a bound past about 9e307 makes
        ! `tn_threshold`'s own `a + s` overflow, which is the regime `feature_risks.md` Risk-251
        ! argues is unreachable from bounds a caller would write, and which hangs rather than
        ! aborts where the trap is masked. This arm is about the centring, not about that.
        do k = 1, 50
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(0.0_real64, huge(1.0_real64), x1, &
                                      mu=-1.0e308_real64, sigma=1.0e300_real64)
            call rng%seed(dist_seed, int(k, int64))
            call rng%normal_truncated(0.0_real64, pinf, x2, &
                                      mu=-1.0e308_real64, sigma=1.0e300_real64)
            call check(error, x1 == x2, &
                "a huge() upper bound and +Infinity gave different draws against a mu a full range below " // &
                "them, where the centring itself is not representable")
            if (allocated(error)) return
            call check(error, x1 >= 0.0_real64, &
                "a truncated draw centred a full range below its own interval came back outside it")
            if (allocated(error)) return
        end do
    end subroutine test_normal_trunc_unbounded_forms

    !> The truncated normal is `pure`: usable in a `do concurrent` body, with the same values.
    subroutine test_normal_trunc_purity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: j
        real(real64) :: dc(256), serial(256)
        type(pf_random_stream) :: rng

        do concurrent (j = 1:256)
            block
                type(pf_random_stream) :: local
                call local%seed(dist_seed, int(j, int64))
                call local%normal_truncated(-1.0_real64, 2.5_real64, dc(j))
            end block
        end do
        do j = 1, 256
            call rng%seed(dist_seed, int(j, int64))
            call rng%normal_truncated(-1.0_real64, 2.5_real64, serial(j))
        end do
        call check(error, all(dc == serial), &
            "%normal_truncated gives different values inside a do concurrent body than outside it")
    end subroutine test_normal_trunc_purity

    !> The stream cost is variable, reported exactly by `%position`, and `%rewind` reproduces it.
    subroutine test_normal_trunc_cost(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        integer(int64), parameter :: NW = 400_int64
        real(real64) :: x, first(NW), again(NW)
        integer(int64) :: k, p0, saved, costs(NW)
        integer :: distinct

        ! A tail sliver: the uniform proposal rejects often enough that a variable cost is certain.
        call rng%seed(dist_seed, 77_int64)
        do k = 1_int64, NW
            p0 = rng%position()
            call rng%normal_truncated(1.0_real64, 1.3_real64, first(k))
            costs(k) = rng%position() - p0
            call check(error, costs(k) >= 4_int64 .and. modulo(costs(k), 2_int64) == 0_int64, &
                "a %normal_truncated draw consumed an odd number of words, or fewer than the four a single " // &
                "uniform-proposal candidate needs")
            if (allocated(error)) return
        end do
        distinct = count(costs /= costs(1))
        call check(error, distinct > 0, &
            "every %normal_truncated draw in this fixture cost the same number of words, so no rejection ever " // &
            "fired and this test has not exercised the variable-cost path it exists for")
        if (allocated(error)) return

        ! Checkpoint and restart: the position is exact even though it cannot be predicted.
        call rng%seed(dist_seed, 77_int64)
        do k = 1_int64, 13_int64
            call rng%normal_truncated(1.0_real64, 1.3_real64, x)
        end do
        saved = rng%position()
        do k = 1_int64, NW
            call rng%normal_truncated(1.0_real64, 1.3_real64, again(k))
        end do
        call rng%rewind(saved)
        do k = 1_int64, NW
            call rng%normal_truncated(1.0_real64, 1.3_real64, x)
            call check(error, x == again(k), &
                "%rewind to a saved position did not reproduce the truncated normals drawn from it")
            if (allocated(error)) return
        end do
    end subroutine test_normal_trunc_cost

    !> A far-out interval must reach the tilted case and terminate, not spin in the uniform one.
    !!
    !! **Regression for `feature_risks.md` Risk-251, and it is a HANG rather than a wrong answer**,
    !! which is why it is asserted through the path rather than through a value. The case rule's
    !! threshold is a difference of two nearly-equal large doubles if written directly; formed that
    !! way it comes back around `1e53` for a standardised bound near `1e10`, every wide tail
    !! interval is then sent to the uniform proposal, whose acceptance there underflows to zero,
    !! and the draw never returns. Nothing else in the suite would fail -- it would simply stop.
    !!
    !! The bounds below are the ones that hung: `(lo - mu)/sigma` is about `-1.379e10` with the
    !! interval 2.15 standardised units wide, so the tilted case must take it.
    !!
    !! **This test can only fail where the compiler CONTRACTS, so run it optimised.** Reverting the
    !! threshold to its cancelling form and running `fpm test` unoptimised passes all 29 tests in
    !! this suite: at `-O0` there is no FMA to contract into, the difference evaluates to the 0 it
    !! mathematically is, and the reverted code is genuinely correct. The same revert under
    !! `--profile release` fails here and then hangs. Confirmed by mutation, both ways.
    subroutine test_normal_trunc_far_tail_terminates(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng
        real(real64) :: x, a, w
        integer(int64) :: tries
        integer(int32) :: path
        integer :: k

        call rng%seed(dist_seed, 79_int64)
        call parquet_debug_normal_truncated_path(rng, -8.527880783008039e5_real64, &
            -8.527880780515589e5_real64, x, path, tries, &
            mu=7.432195363441985e5_real64, sigma=1.157446411309802e-4_real64)
        call check(error, path == 6_int32, &
            "a far-out mirrored interval 2.15 standardised units wide did not take the tilted case, so the " // &
            "case threshold has collapsed and the uniform proposal it fell into cannot accept at all")
        if (allocated(error)) return
        call check(error, tries <= 4_int64, &
            "a far-out tilted draw took more than four proposals, which the tilted envelope's acceptance bound " // &
            "does not allow -- the rate is wrong")
        if (allocated(error)) return

        ! The threshold must stay finite, positive and ~1/a across the whole range, not just at
        ! the one bound above. spec_threshold recomputes it independently; both must agree.
        !
        ! **The discrimination arm stops at a = 1e6 because of arithmetic, not caution.** The
        ! threshold behaves as 1/a and one ulp of `a` is about a*2.2e-16, so the two cross near
        ! a = 6.7e7: past that no interval NARROWER than the threshold is representable at all,
        ! `a + 0.95*w` rounds to `a` or beyond it, and the question the arm asks stops having an
        ! answer. Everything above that bound is covered by the wide arm below, which is the only
        ! case that exists out there.
        do k = 0, 12
            a = 10.0_real64 ** (real(k, real64) * 0.5_real64) - 1.0_real64
            w = spec_threshold(a)
            call check(error, w > 0.0_real64 .and. w <= 2.0_real64, &
                "the recomputed tail threshold left (0, 2], so the specification this suite checks the " // &
                "library against is itself wrong")
            if (allocated(error)) return
            ! Just above the threshold must tilt; just below must take the uniform proposal.
            call parquet_debug_normal_truncated_path(rng, a, a + w * 1.05_real64, x, path, tries)
            call check(error, path == 3_int32, &
                "an interval wider than the recomputed threshold did not take exponential tilting")
            if (allocated(error)) return
            call check(error, x >= a, "a tilted draw came back below its lower bound")
            if (allocated(error)) return
            call parquet_debug_normal_truncated_path(rng, a, a + w * 0.95_real64, x, path, tries)
            call check(error, path == 2_int32, &
                "an interval narrower than the recomputed threshold did not take the uniform proposal")
            if (allocated(error)) return
            call check(error, tries <= 20_int64, &
                "a uniform-proposal draw at the threshold took more than twenty proposals, so its acceptance " // &
                "is nowhere near the bound the case rule rests on")
            if (allocated(error)) return
        end do

        ! The wide arm, out where the threshold is below one ulp of the bound: every representable
        ! interval tilts, and must terminate promptly and land inside. This is the regime the hang
        ! lived in. It stops at 1e16 because that is as far as a fixture can go and still assert a
        ! MEANINGFUL interval; the separate overflow of `a*a` above 1.3e154, which `hypot` exists to
        ! prevent, is not reachable from a bound a caller would write and is argued rather than
        ! tested (`feature_risks.md` Risk-251).
        do k = 1, 6
            a = 10.0_real64 ** (real(2 * k + 4, real64))
            call parquet_debug_normal_truncated_path(rng, a, a * 1.000001_real64, x, path, tries)
            call check(error, path == 3_int32, &
                "a far-out interval did not take exponential tilting, which is the only case that can serve " // &
                "it: no narrower interval is representable at that magnitude")
            if (allocated(error)) return
            call check(error, tries <= 8_int64, &
                "a far-out tilted draw took more than eight proposals, so its acceptance rate is not the one " // &
                "the envelope guarantees -- this is how the hang announced itself")
            if (allocated(error)) return
            call check(error, x >= a .and. x <= a * 1.000001_real64, &
                "a far-out draw came back outside its own interval")
            if (allocated(error)) return
        end do
    end subroutine test_normal_trunc_far_tail_terminates

    !> Draws one truncated normal and files its path, for `test_normal_trunc_cases`.
    subroutine probe_path(rng, lo, hi, npath, path)
        type(pf_random_stream), intent(inout) :: rng    !! the stream to advance
        real(real64), intent(in) :: lo                  !! lower bound of the support
        real(real64), intent(in) :: hi                  !! upper bound of the support
        integer(int64), intent(inout) :: npath(6)       !! per-path counters, incremented in place
        integer(int32), intent(out) :: path             !! the path this draw took
        real(real64) :: x
        integer(int64) :: tries
        call parquet_debug_normal_truncated_path(rng, lo, hi, x, path, tries)
        npath(path) = npath(path) + 1_int64
    end subroutine probe_path

    !> The tail arm's case threshold, RECOMPUTED from the specification rather than imported.
    !!
    !! `parquet_random`'s `tn_threshold` is private, and a test that reached for it could not tell
    !! a wrong constant from a wrong rule. This is the same expression written independently from
    !! the derivation: the width at which a uniform envelope anchored at `a` and the tilted
    !! envelope have equal mass.
    !! Both forms cancel nothing: `lam - a` is rationalised to `2/(a + s)`, so this is usable at
    !! the large `a` where the naive transcription of either form collapses.
    pure function spec_threshold(a) result(t)
        real(real64), intent(in) :: a       !! standardised lower bound; `a >= 0`
        real(real64) :: t                   !! the interval width at which the two proposals cross
        real(real64) :: s, gap
        s = hypot(a, 2.0_real64)
        gap = 2.0_real64 / (a + s)          ! exactly `lam - a`, rationalised
        t = exp(0.5_real64 * gap * gap) * gap
    end function spec_threshold

    !> Upper tail `P(Z > x)` of the standard normal, safe for an infinite or `huge` argument.
    !!
    !! Written as `erfc(x/sqrt(2))/2` rather than `1 - Phi(x)` so the far-tail intervals do not
    !! cancel: at `x = 4` the two differ in the fifth significant digit of the answer. The cutoff
    !! at `TN_CUT` keeps `x*x` from overflowing for an infinite bound, which nagfor traps, and
    !! keeps `erfc` out of the subnormal range; beyond it the true value is no longer measurable
    !! against any interval mass this suite uses.
    pure function tn_q(x) result(q)
        real(real64), intent(in) :: x       !! the point to measure the upper tail from
        real(real64) :: q                   !! `P(Z > x)`, in `[0, 1]`
        if (x >= TN_CUT) then
            q = 0.0_real64
        else if (x <= -TN_CUT) then
            q = 1.0_real64
        else
            q = 0.5_real64 * erfc(x / sqrt(2.0_real64))
        end if
    end function tn_q

    !> Standard normal density, safe for an infinite or `huge` argument. See `tn_q`.
    pure function tn_phi(x) result(p)
        real(real64), intent(in) :: x       !! the point to evaluate the density at
        real(real64) :: p                   !! the density there
        if (abs(x) >= TN_CUT) then
            p = 0.0_real64
        else
            p = exp(-0.5_real64 * x * x) / sqrt(8.0_real64 * atan(1.0_real64))
        end if
    end function tn_phi

    !> Draws `n` truncated normals on `[lo, hi]` and reports three test statistics.
    !!
    !! `flat` is what makes the negative control possible: `.false.` is the library's own draw and
    !! `.true.` is a uniform sample on the same interval -- the exact output a wrong envelope
    !! anchor produces, and the one a shape test must reject.
    !!
    !! **The control is drawn on the interval clamped to `TN_CUT`, not on the interval itself.**
    !! A one-sided tail is spelled with a `huge()` bound, and a uniform draw is not defined on an
    !! unbounded interval: taken literally it returns values around 1e308, whose `x*x` overflows
    !! (nagfor traps it) and whose conditional CDF is pinned at 1, so every draw falls in the last
    !! cell and the control degenerates to a sample of one cell. Clamping costs nothing the
    !! chi-square can see -- past `TN_CUT` the CDF is already saturated -- and sharpens the
    !! control into a flat sample over the window the gate can actually resolve.
    !!
    !! The cell index is the EXACT conditional CDF, so the chi-square is distribution-free rather
    !! than a comparison against another sampler.
    subroutine trunc_gate(seed, n, lo, hi, flat, z_mean, z_var, chi2)
        integer(int64), intent(in) :: seed      !! the family to draw from
        integer(int64), intent(in) :: n         !! how many draws to take
        real(real64), intent(in) :: lo          !! lower bound of the support
        real(real64), intent(in) :: hi          !! upper bound of the support
        logical, intent(in) :: flat             !! `.true.` draws the uniform control instead
        real(real64), intent(out) :: z_mean     !! standard errors of the mean from the exact one
        real(real64), intent(out) :: z_var      !! standard errors of the variance from the exact one
        real(real64), intent(out) :: chi2       !! chi-square of the conditional CDF over 20 cells
        integer, parameter :: NCELL = 20
        type(pf_random_stream) :: rng
        integer(int64) :: k, counts(NCELL), cell
        real(real64) :: x, u, s, s2, mean, var, expect, dn
        real(real64) :: qa, qb, mass, emean, evar, m4
        real(real64) :: flo, fhi

        ! The finite window the flat control is drawn on. See the note above; an unclamped
        ! `hi - lo` would also overflow outright were both bounds ever given as `huge()`.
        flo = max(lo, -TN_CUT)
        fhi = min(hi, TN_CUT)

        qa = tn_q(lo)
        qb = tn_q(hi)
        mass = qa - qb
        emean = (tn_phi(lo) - tn_phi(hi)) / mass
        evar = 1.0_real64 + (lo * tn_phi(lo) - hi * tn_phi(hi)) / mass - emean * emean
        ! `lo * tn_phi(lo)` is 0 rather than a NaN for an infinite bound, because tn_phi cuts off.

        counts = 0_int64
        s = 0.0_real64
        s2 = 0.0_real64
        call rng%seed(seed, 78_int64)
        do k = 1_int64, n
            if (flat) then
                call rng%uniform(u)
                x = flo + u * (fhi - flo)
            else
                call rng%normal_truncated(lo, hi, x)
            end if
            s = s + x
            s2 = s2 + x * x
            cell = min(int(real(NCELL, real64) * ((qa - tn_q(x)) / mass), int64) + 1_int64, int(NCELL, int64))
            cell = max(cell, 1_int64)
            counts(cell) = counts(cell) + 1_int64
        end do
        dn = real(n, real64)
        mean = s / dn
        var = s2 / dn - mean * mean
        z_mean = (mean - emean) * sqrt(dn / evar)
        ! The variance of the sample variance needs the fourth central moment, which has no tidy
        ! closed form here; 2*evar**2 is its Gaussian value and is the right order for every
        ! interval used, so this z is a scale-free residual rather than an exact standard error.
        m4 = 2.0_real64 * evar * evar
        z_var = (var - evar) * sqrt(dn / m4)
        expect = dn / real(NCELL, real64)
        chi2 = sum((real(counts, real64) - expect) ** 2 / expect)
    end subroutine trunc_gate

    ! ================================================================================
    ! Points on a sphere
    ! ================================================================================

    !> Every golden row of the points-on-a-sphere tables, against the 60-digit model.
    !!
    !! **Two claims per row, held to two strengths.** The uniforms a row names are pinned bit for bit
    !! against the cipher's raw block on the family's derived key, so the table and the library
    !! agree about which words a coordinate reads. The values are pinned to `SPH_TOL` of the model,
    !! an ABSOLUTE tolerance because a component formed as a sum of products has no relative
    !! accuracy near zero; that catches a changed transform, and the rows are chosen so it catches
    !! the two cancelling ones by a wide margin -- the plain `1 - cos` form misses the `1e-9` disc by
    !! millions of ulp and the plain vMF logarithm puts the `kappa = 1e-30` rows at the far pole.
    subroutine test_sphere_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, j
        integer(int64) :: key, d
        real(real64) :: u1, u2, v(3), want(3), centre(3), radius, inner, ra, dec, r(3, 3), rw(3, 3), scale
        ! `vs`, `ws` and `mu` are named rather than written inline at the calls below: `sph_note`'s
        ! and `pf_random_vmf_at`'s array dummies are explicit-shape, so an expression or a function
        ! result -- a value with no address of its own -- is argument-associated through a temporary,
        ! which ifx reports as `warning (406)` per call under --profile debug. See
        ! .claude/rules/fortran-gotchas.md.
        real(real64) :: vs(3), ws(3), mu(3)
        real(real64) :: worst
        logical :: differs

        call check(error, pf_sphere_algorithm == "sphere:archimedes+frame+vmfinv+cbrt+shoemake/libm/v1", &
            "pf_sphere_algorithm no longer reads sphere:archimedes+frame+vmfinv+cbrt+shoemake/libm/v1: the " // &
            "vectors in this suite are the contract for that exact string, so they must be regenerated with it")
        if (allocated(error)) return

        worst = 0.0_real64
        differs = .false.

        ! ---- pf_random_direction_at and pf_random_radec_at ----
        do k = 1, n_sdir
            d = max(sdir_draw(k), 1_int64)
            key = pf_random_key(sdir_seed(k), sph_label_direction)
            call sph_block_uniforms(key, sdir_stream(k), d, u1, u2)
            call check(error, transfer(u1, 0_int64) == sdir_u1_bits(k) .and. transfer(u2, 0_int64) == sdir_u2_bits(k), &
                "the direction table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            v = pf_random_direction_at(sdir_seed(k), sdir_stream(k), sdir_draw(k))
            want = sph_vec(sdir_v_bits, k)
            call sph_note(v, want, worst, differs)
            call check(error, maxval(abs(v - want)) <= SPH_TOL, &
                "pf_random_direction_at is more than 32 ulp from the 60-digit model, so the transform has changed")
            if (allocated(error)) return
            call check(error, abs(norm2(v) - 1.0_real64) <= 8.0_real64 * epsilon(1.0_real64), &
                "pf_random_direction_at returned a vector whose length is not 1 to 8 ulp")
            if (allocated(error)) return
            call pf_random_radec_at(sdir_seed(k), sdir_stream(k), ra, dec, sdir_draw(k))
            call check(error, sph_deg_gap(ra, dec, sdir_ra_bits(k), sdir_dec_bits(k)) <= SPH_TOL_DEG, &
                "pf_random_radec_at is more than 1e-12 degrees from the 60-digit model")
            if (allocated(error)) return
            call check(error, ra >= 0.0_real64 .and. ra < 360.0_real64 .and. dec >= -90.0_real64 .and. dec <= 90.0_real64, &
                "pf_random_radec_at returned a position outside [0, 360) x [-90, 90]")
            if (allocated(error)) return
        end do

        ! ---- pf_random_disc_at ----
        do k = 1, n_sdisc
            key = pf_random_key(sdisc_seed(k), sph_label_disc)
            call sph_block_uniforms(key, sdisc_stream(k), sdisc_draw(k), u1, u2)
            call check(error, transfer(u1, 0_int64) == sdisc_u1_bits(k) .and. transfer(u2, 0_int64) == sdisc_u2_bits(k), &
                "the disc table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            centre = sph_vec(sdisc_centre_bits, k)
            radius = transfer(sdisc_radius_bits(k), 0.0_real64)
            inner = transfer(sdisc_inner_bits(k), 0.0_real64)
            if (sdisc_inner_given(k)) then
                v = pf_random_disc_at(sdisc_seed(k), sdisc_stream(k), centre, radius, sdisc_draw(k), inner)
            else
                v = pf_random_disc_at(sdisc_seed(k), sdisc_stream(k), centre, radius, sdisc_draw(k))
            end if
            want = sph_vec(sdisc_v_bits, k)
            call sph_note(v, want, worst, differs)
            call check(error, maxval(abs(v - want)) <= SPH_TOL, &
                "pf_random_disc_at is more than 32 ulp from the 60-digit model, so the transform has changed")
            if (allocated(error)) return
        end do

        ! ---- pf_random_disc_radec_at ----
        do k = 1, n_sdrd
            key = pf_random_key(sdrd_seed(k), sph_label_disc)
            call sph_block_uniforms(key, sdrd_stream(k), sdrd_draw(k), u1, u2)
            call check(error, transfer(u1, 0_int64) == sdrd_u1_bits(k) .and. transfer(u2, 0_int64) == sdrd_u2_bits(k), &
                "the disc RA/Dec table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            if (sdrd_inner_given(k)) then
                call pf_random_disc_radec_at(sdrd_seed(k), sdrd_stream(k), transfer(sdrd_ra0_bits(k), 0.0_real64), &
                    transfer(sdrd_dec0_bits(k), 0.0_real64), transfer(sdrd_radius_bits(k), 0.0_real64), ra, dec, &
                    sdrd_draw(k), transfer(sdrd_inner_bits(k), 0.0_real64))
            else
                call pf_random_disc_radec_at(sdrd_seed(k), sdrd_stream(k), transfer(sdrd_ra0_bits(k), 0.0_real64), &
                    transfer(sdrd_dec0_bits(k), 0.0_real64), transfer(sdrd_radius_bits(k), 0.0_real64), ra, dec, &
                    sdrd_draw(k))
            end if
            call check(error, sph_deg_gap(ra, dec, sdrd_ra_bits(k), sdrd_dec_bits(k)) <= SPH_TOL_DEG, &
                "pf_random_disc_radec_at is more than 1e-12 degrees from the 60-digit model")
            if (allocated(error)) return
        end do

        ! ---- pf_random_ball_at ----
        do k = 1, n_sball
            key = pf_random_key(sball_seed(k), sph_label_ball)
            call sph_block_uniforms(key, sball_stream(k), sball_draw(k), u1, u2)
            call check(error, transfer(u1, 0_int64) == sball_u1_bits(k) .and. transfer(u2, 0_int64) == sball_u2_bits(k), &
                "the ball table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            call check(error, transfer(pf_random_at(pf_random_key(sball_seed(k), sph_label_ball_radius), sball_stream(k), &
                sball_draw(k)), 0_int64) == sball_u3_bits(k), &
                "the ball's radius uniform is not pf_random_at on its second label at (key, stream, draw)")
            if (allocated(error)) return
            radius = transfer(sball_radius_bits(k), 0.0_real64)
            inner = transfer(sball_inner_bits(k), 0.0_real64)
            if (sball_inner_given(k)) then
                v = pf_random_ball_at(sball_seed(k), sball_stream(k), radius, sball_draw(k), inner)
            else
                v = pf_random_ball_at(sball_seed(k), sball_stream(k), radius, sball_draw(k))
            end if
            want = sph_vec(sball_p_bits, k)
            scale = max(1.0_real64, radius)
            vs = v / scale
            ws = want / scale
            call sph_note(vs, ws, worst, differs)
            call check(error, maxval(abs(v - want)) <= SPH_TOL * scale, &
                "pf_random_ball_at is more than 32 ulp of its radius from the 60-digit model")
            if (allocated(error)) return
        end do

        ! ---- pf_random_vmf_at ----
        do k = 1, n_svmf
            key = pf_random_key(svmf_seed(k), sph_label_vmf)
            call sph_block_uniforms(key, svmf_stream(k), svmf_draw(k), u1, u2)
            call check(error, transfer(u1, 0_int64) == svmf_u1_bits(k) .and. transfer(u2, 0_int64) == svmf_u2_bits(k), &
                "the vMF table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            mu = sph_vec(svmf_mu_bits, k)
            v = pf_random_vmf_at(svmf_seed(k), svmf_stream(k), mu, &
                                 transfer(svmf_kappa_bits(k), 0.0_real64), svmf_draw(k))
            want = sph_vec(svmf_v_bits, k)
            call sph_note(v, want, worst, differs)
            call check(error, maxval(abs(v - want)) <= SPH_TOL, &
                "pf_random_vmf_at is more than 32 ulp from the 60-digit model, so the transform has changed")
            if (allocated(error)) return
        end do

        ! ---- pf_random_vmf_radec_at ----
        do k = 1, n_svrd
            key = pf_random_key(svrd_seed(k), sph_label_vmf)
            call sph_block_uniforms(key, svrd_stream(k), svrd_draw(k), u1, u2)
            call check(error, transfer(u1, 0_int64) == svrd_u1_bits(k) .and. transfer(u2, 0_int64) == svrd_u2_bits(k), &
                "the vMF RA/Dec table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            call pf_random_vmf_radec_at(svrd_seed(k), svrd_stream(k), transfer(svrd_ra0_bits(k), 0.0_real64), &
                transfer(svrd_dec0_bits(k), 0.0_real64), transfer(svrd_sigma_bits(k), 0.0_real64), ra, dec, svrd_draw(k))
            call check(error, sph_deg_gap(ra, dec, svrd_ra_bits(k), svrd_dec_bits(k)) <= SPH_TOL_DEG, &
                "pf_random_vmf_radec_at is more than 1e-12 degrees from the 60-digit model")
            if (allocated(error)) return
        end do

        ! ---- pf_random_rotation_at ----
        do k = 1, n_srot
            key = pf_random_key(srot_seed(k), sph_label_rotation)
            call sph_block_uniforms(key, srot_stream(k), srot_draw(k), u1, u2)
            call check(error, transfer(u1, 0_int64) == srot_u1_bits(k) .and. transfer(u2, 0_int64) == srot_u2_bits(k), &
                "the rotation table and the cipher disagree about which block this coordinate names")
            if (allocated(error)) return
            call check(error, transfer(pf_random_at(pf_random_key(srot_seed(k), sph_label_rotation_angle), srot_stream(k), &
                srot_draw(k)), 0_int64) == srot_u3_bits(k), &
                "the rotation's third uniform is not pf_random_at on its second label at (key, stream, draw)")
            if (allocated(error)) return
            r = pf_random_rotation_at(srot_seed(k), srot_stream(k), srot_draw(k))
            do j = 1, 3
                rw(1, j) = transfer(srot_r_bits(9 * (k - 1) + 3 * (j - 1) + 1), 0.0_real64)
                rw(2, j) = transfer(srot_r_bits(9 * (k - 1) + 3 * (j - 1) + 2), 0.0_real64)
                rw(3, j) = transfer(srot_r_bits(9 * (k - 1) + 3 * (j - 1) + 3), 0.0_real64)
                call sph_note(r(:, j), rw(:, j), worst, differs)
            end do
            call check(error, maxval(abs(r - rw)) <= SPH_TOL, &
                "pf_random_rotation_at is more than 32 ulp from the 60-digit model, so the construction has changed")
            if (allocated(error)) return
            call check(error, sph_orthonormal_gap(r) <= 1.0e-14_real64, &
                "pf_random_rotation_at returned a matrix that is not a proper rotation to 1e-14")
            if (allocated(error)) return
        end do

        ! Vacuity guard. A table whose every value agreed to the bit would mean the reference had been
        ! read back out of the implementation rather than derived for it.
        call check(error, differs, &
            "not one golden component differs from its 60-digit reference by even an ulp, which is what a table " // &
            "generated FROM the implementation would look like rather than one generated for it")
        if (allocated(error)) return
        call check(error, worst > 0.0_real64, "the worst golden gap is zero, contradicting the guard above")
    end subroutine test_sphere_golden

    !> The stream walk, the bulk fills and the coordinate-addressed forms are one grid.
    !!
    !! **Held to `SPH_SAME`, a few ulp, rather than to the bit.** Each producer and its tier-0 form call
    !! one body, but a compiler may inline it at one site and not the other and round the two
    !! differently (`fortran-gotchas.md`, ifx); a different block is a different point, O(1) away, so
    !! the tolerance loses nothing. **Chunk invariance of a fill is exact**, as it is for every fill
    !! in this module: one code path serves every element however the fill is split.
    subroutine test_sphere_tiers_agree(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: STREAM = 11_int64
        real(real64), parameter :: CENTRE(3) = [0.3_real64, -0.5_real64, 0.8_real64]
        type(pf_random_stream) :: rng, fresh
        integer(int64) :: k, d, s, st
        integer :: n
        real(real64) :: v(3), w(3), r(3, 3), ra, dec, ra2, dec2, x
        real(real32) :: x32
        real(real64) :: whole(3, 16), part(3, 16), empty3(3, 0), fra(16), fdec(16), pra(16), pdec(16)
        real(real64) :: empty(0), empty_dec(0)

        call rng%seed(dist_seed, STREAM)
        do k = 1_int64, 16_int64
            call rng%direction(v)
            call check(error, maxval(abs(v - pf_random_direction_at(dist_seed, STREAM, k))) <= SPH_SAME, &
                "%direction does not walk the pf_random_direction_at grid one block per draw")
            if (allocated(error)) return
            call check(error, rng%position() == 4_int64 * k + 1_int64, &
                "%direction did not cost exactly one block (4 words)")
            if (allocated(error)) return
        end do

        ! Every producer on ONE stream, interleaved, so each has to land on the block the one before it
        ! left: `d` counts the blocks consumed, and every value is compared at its own block.
        call rng%seed(dist_seed, STREAM)
        d = 0_int64
        do k = 1_int64, 4_int64
            d = d + 1_int64
            call rng%radec(ra, dec)
            call pf_random_radec_at(dist_seed, STREAM, ra2, dec2, d)
            call check(error, sph_deg_gap(ra, dec, transfer(ra2, 0_int64), transfer(dec2, 0_int64)) <= SPH_TOL_DEG, &
                "%radec does not walk the pf_random_radec_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%disc(CENTRE, 0.4_real64, v)
            call check(error, maxval(abs(v - pf_random_disc_at(dist_seed, STREAM, CENTRE, 0.4_real64, d))) <= SPH_SAME, &
                "%disc without r_inner does not walk the pf_random_disc_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%disc(CENTRE, 0.4_real64, v, r_inner=0.1_real64)
            call check(error, maxval(abs(v - pf_random_disc_at(dist_seed, STREAM, CENTRE, 0.4_real64, d, 0.1_real64))) &
                <= SPH_SAME, "%disc with r_inner does not walk the pf_random_disc_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%disc_radec(150.0_real64, -89.0_real64, 3.0_real64, ra, dec)
            call pf_random_disc_radec_at(dist_seed, STREAM, 150.0_real64, -89.0_real64, 3.0_real64, ra2, dec2, d)
            call check(error, sph_deg_gap(ra, dec, transfer(ra2, 0_int64), transfer(dec2, 0_int64)) <= SPH_TOL_DEG, &
                "%disc_radec without r_inner_deg does not walk the pf_random_disc_radec_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%disc_radec(150.0_real64, -89.0_real64, 3.0_real64, ra, dec, r_inner_deg=1.0_real64)
            call pf_random_disc_radec_at(dist_seed, STREAM, 150.0_real64, -89.0_real64, 3.0_real64, ra2, dec2, d, &
                                         1.0_real64)
            call check(error, sph_deg_gap(ra, dec, transfer(ra2, 0_int64), transfer(dec2, 0_int64)) <= SPH_TOL_DEG, &
                "%disc_radec with r_inner_deg does not walk the pf_random_disc_radec_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%ball(2.5_real64, v)
            call check(error, maxval(abs(v - pf_random_ball_at(dist_seed, STREAM, 2.5_real64, d))) <= 2.5_real64 * SPH_SAME, &
                "%ball without r_inner does not walk the pf_random_ball_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%ball(2.5_real64, v, r_inner=1.0_real64)
            call check(error, maxval(abs(v - pf_random_ball_at(dist_seed, STREAM, 2.5_real64, d, 1.0_real64))) &
                <= 2.5_real64 * SPH_SAME, "%ball with r_inner does not walk the pf_random_ball_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%vmf(CENTRE, 50.0_real64, v)
            call check(error, maxval(abs(v - pf_random_vmf_at(dist_seed, STREAM, CENTRE, 50.0_real64, d))) <= SPH_SAME, &
                "%vmf does not walk the pf_random_vmf_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%vmf_radec(10.0_real64, 20.0_real64, 1.0_real64, ra, dec)
            call pf_random_vmf_radec_at(dist_seed, STREAM, 10.0_real64, 20.0_real64, 1.0_real64, ra2, dec2, d)
            call check(error, sph_deg_gap(ra, dec, transfer(ra2, 0_int64), transfer(dec2, 0_int64)) <= SPH_TOL_DEG, &
                "%vmf_radec does not walk the pf_random_vmf_radec_at grid")
            if (allocated(error)) return
            d = d + 1_int64
            call rng%rotation(r)
            call check(error, maxval(abs(r - pf_random_rotation_at(dist_seed, STREAM, d))) <= SPH_SAME, &
                "%rotation does not walk the pf_random_rotation_at grid")
            if (allocated(error)) return
        end do
        call check(error, rng%position() == 4_int64 * d + 1_int64, &
            "an interleaved run of sphere producers did not cost exactly one block each")
        if (allocated(error)) return

        ! Block alignment. A `%uniform32` leaves the cursor on word 2; the next sphere producer must
        ! skip to the next block rather than straddle one.
        call rng%seed(dist_seed, STREAM)
        call rng%uniform32(x32)
        call rng%direction(v)
        call check(error, rng%position() == 9_int64, &
            "%direction taken after a %uniform32 did not align to the next block: it must leave position 9")
        if (allocated(error)) return
        call check(error, maxval(abs(v - pf_random_direction_at(dist_seed, STREAM, 2_int64))) <= SPH_SAME, &
            "%direction taken after a %uniform32 is not block 2's direction")
        if (allocated(error)) return
        call rng%seed(dist_seed, STREAM)
        call rng%uniform(x)
        call rng%disc(CENTRE, 0.3_real64, v)
        call check(error, rng%position() == 9_int64, &
            "%disc taken after a %uniform did not align to the next block: it must leave position 9")
        if (allocated(error)) return
        ! The producer read its family's sub-stream, so the raw words of the block it consumed are
        ! still exactly what %uniform finds there.
        call rng%rewind(5_int64)
        call rng%uniform(x)
        call check(error, x == pf_random_at(dist_seed, STREAM, 3_int64), &
            "a %uniform rewound onto the block a sphere producer consumed does not see that block's raw words")
        if (allocated(error)) return

        ! %address hands back what %seed was given, before and after draws, for every seeding form.
        call fresh%address(s, st)
        call check(error, s == 0_int64 .and. st == 0_int64, "%address on a stream never seeded is not (0, 0)")
        if (allocated(error)) return
        call rng%seed(dist_seed)
        call rng%address(s, st)
        call check(error, s == dist_seed .and. st == 0_int64, "%address after %seed(seed) is not (seed, 0)")
        if (allocated(error)) return
        call rng%seed(dist_seed, 7_int32)
        call rng%direction(v)
        call rng%address(s, st)
        call check(error, s == dist_seed .and. st == 7_int64, "%address after %seed(seed, int32 7) is not (seed, 7)")
        if (allocated(error)) return
        call rng%seed(-dist_seed, -3_int64)
        call rng%address(s, st)
        call check(error, s == -dist_seed .and. st == -3_int64, "%address after %seed(-seed, int64 -3) is not (-seed, -3)")
        if (allocated(error)) return

        ! The fills: element k is the scalar draw to a few ulp, and chunking is exact.
        call pf_random_fill_direction(dist_seed, STREAM, whole, 5_int64)
        call pf_random_fill_radec(dist_seed, STREAM, fra, fdec, 5_int64)
        do k = 1_int64, 16_int64
            w = pf_random_direction_at(dist_seed, STREAM, k + 4_int64)
            call check(error, maxval(abs(whole(:, k) - w)) <= 2.0_real64 * SPH_SAME, &
                "pf_random_fill_direction's column k is not pf_random_direction_at at draw+k-1 to a few ulp")
            if (allocated(error)) return
            call pf_random_radec_at(dist_seed, STREAM, ra, dec, k + 4_int64)
            call check(error, sph_deg_gap(fra(k), fdec(k), transfer(ra, 0_int64), transfer(dec, 0_int64)) <= SPH_TOL_DEG, &
                "pf_random_fill_radec's element k is not pf_random_radec_at at draw+k-1")
            if (allocated(error)) return
        end do
        do n = 1, 16
            part = 0.0_real64
            call pf_random_fill_direction(dist_seed, STREAM, part(:, 1:n), 5_int64)
            call check(error, all(part(:, 1:n) == whole(:, 1:n)), &
                "a partial pf_random_fill_direction is not a prefix of a longer one, so the fill is not splittable")
            if (allocated(error)) return
            pra = 0.0_real64
            pdec = 0.0_real64
            call pf_random_fill_radec(dist_seed, STREAM, pra(1:n), pdec(1:n), 5_int64)
            call check(error, all(pra(1:n) == fra(1:n)) .and. all(pdec(1:n) == fdec(1:n)), &
                "a partial pf_random_fill_radec is not a prefix of a longer one")
            if (allocated(error)) return
        end do
        call pf_random_fill_direction(dist_seed, STREAM, part(:, 1:3), 9_int64)
        call check(error, all(part(:, 1:3) == whole(:, 5:7)), "pf_random_fill_direction starting at draw 9 does not resume there")
        if (allocated(error)) return
        call pf_random_fill_radec(dist_seed, STREAM, pra(1:3), pdec(1:3), 9_int64)
        call check(error, all(pra(1:3) == fra(5:7)) .and. all(pdec(1:3) == fdec(5:7)), &
            "pf_random_fill_radec starting at draw 9 does not resume there")
        if (allocated(error)) return
        call pf_random_fill_direction(dist_seed, STREAM, empty3)
        call pf_random_fill_radec(dist_seed, STREAM, empty, empty_dec)
        call check(error, size(empty3, 2) == 0 .and. size(empty) == 0 .and. size(empty_dec) == 0, &
            "a zero-sized sphere fill did not stay zero-sized")
        if (allocated(error)) return

        ! An elemental RA/Dec form over an index array is the scalar calls.
        call pf_random_radec_at(dist_seed, [(k, k=1_int64,16_int64)], pra, pdec)
        do k = 1_int64, 16_int64
            call pf_random_radec_at(dist_seed, k, ra, dec)
            call check(error, sph_deg_gap(pra(k), pdec(k), transfer(ra, 0_int64), transfer(dec, 0_int64)) <= SPH_TOL_DEG, &
                "an elemental pf_random_radec_at over a stream vector does not equal the scalar calls")
            if (allocated(error)) return
        end do
    end subroutine test_sphere_tiers_agree

    !> Every family is independent of every other at one coordinate, and of the raw axis there.
    !!
    !! **A joint test, with the coupling it exists to catch built as a control.** Each family's
    !! output is turned back into the uniform its construction consumed -- the direction's and the
    !! whole-sphere disc's `z`, the ball's direction and radius, the uniform vMF's cosine, the
    !! rotation's `r(3,3)` and its second quaternion angle -- and every pair from different families
    !! goes through a chi-square test of independence on an 8 x 8 table. Two families sharing a label
    !! would read one block, which makes the pair a function of one uniform and the table a diagonal.
    !! The control builds exactly that: the disc as it would be on the direction's label. The
    !! threshold is the 5-sigma point, so 33 tests on one fixed seed cannot fail by chance.
    subroutine test_sphere_families_independent(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NSTAT = 9
        integer, parameter :: NBIN = 8
        integer(int64), parameter :: NDRAW = 40000_int64
        !> The family each statistic belongs to; a pair within one family is not tested.
        integer, parameter :: FAMILY(NSTAT) = [1, 1, 2, 3, 3, 4, 5, 5, 6]
        real(real64), allocatable :: stat(:, :), coupled(:)
        real(real64) :: v(3), p(3), r(3, 3), limit, chi2
        integer(int64) :: k
        integer :: a, b

        allocate(stat(NSTAT, NDRAW), coupled(NDRAW))
        do k = 1_int64, NDRAW
            v = pf_random_direction_at(dist_seed, k)
            stat(1, k) = 0.5_real64 * (v(3) + 1.0_real64)
            stat(2, k) = modulo(atan2(v(2), v(1)) / TWO_PI, 1.0_real64)
            v = pf_random_disc_at(dist_seed, k, ZAXIS, PI)
            stat(3, k) = 0.5_real64 * (1.0_real64 - v(3))
            p = pf_random_ball_at(dist_seed, k, 1.0_real64)
            stat(4, k) = 0.5_real64 * (p(3) / norm2(p) + 1.0_real64)
            stat(5, k) = norm2(p) ** 3
            v = pf_random_vmf_at(dist_seed, k, ZAXIS, 0.0_real64)
            stat(6, k) = 0.5_real64 * (1.0_real64 - v(3))
            r = pf_random_rotation_at(dist_seed, k)
            stat(7, k) = 0.5_real64 * (r(3, 3) + 1.0_real64)
            ! w**2 - z**2 = (r11 + r22)/2 and 2*z*w = (r21 - r12)/2, so this is twice the second
            ! quaternion angle: a function of the third uniform alone.
            stat(8, k) = modulo(atan2(r(2, 1) - r(1, 2), r(1, 1) + r(2, 2)) / TWO_PI, 1.0_real64)
            stat(9, k) = pf_random_at(dist_seed, k)
            ! CONTROL: the whole-sphere disc's uniform as it would be on the DIRECTION's label.
            coupled(k) = pf_random_at(pf_random_key(dist_seed, sph_label_direction), k, 1_int64)
        end do

        limit = chi2_quantile((NBIN - 1) * (NBIN - 1), 5.0_real64)
        chi2 = sph_independence(stat(1, :), coupled, NBIN)
        call check(error, chi2 > 100.0_real64 * limit, &
            "the deliberately coupled control -- a disc on the direction's label -- passes the independence test, " // &
            "so this test has no power and its verdicts below mean nothing")
        if (allocated(error)) return
        do a = 1, NSTAT - 1
            do b = a + 1, NSTAT
                if (FAMILY(a) == FAMILY(b)) cycle
                chi2 = sph_independence(stat(a, :), stat(b, :), NBIN)
                call check(error, chi2 <= limit, &
                    "two points-on-a-sphere families, or a family and pf_random_at, are dependent at one coordinate: " // &
                    "they share a label or a block (feature_risks.md Risk-123)")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_sphere_families_independent

    !> Each RA/Dec form is its vector form read in the standard frame: the same point, not a new draw.
    !!
    !! The reference is converted HERE, by a formula written independently of the library's
    !! (`(cos(dec)cos(ra), cos(dec)sin(ra), sin(dec))`, compared as vectors), so a swapped `atan2`
    !! argument, a sign or a wrap defect shows as a vector 1e-9 away rather than as two conversions
    !! agreeing with each other. The pole rule is asserted exactly: a centre ON a pole with radius 0
    !! is the pole, and the pole's right ascension is 0.
    subroutine test_sphere_radec_is_the_vector_form(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 3000_int64
        real(real64), parameter :: RA0(4) = [150.0_real64, 359.9_real64, 10.0_real64, 283.0_real64]
        real(real64), parameter :: DEC0(4) = [-89.0_real64, 10.0_real64, 20.0_real64, 61.0_real64]
        real(real64) :: v(3), c(3), ra, dec, sigma, kappa, worst
        integer(int64) :: k
        integer :: j

        worst = 0.0_real64
        do k = 1_int64, NDRAW
            j = int(modulo(k, 4_int64)) + 1
            call pf_random_radec_at(dist_seed, k, ra, dec, 3_int64)
            call check(error, ra >= 0.0_real64 .and. ra < 360.0_real64 .and. dec >= -90.0_real64 .and. dec <= 90.0_real64, &
                "pf_random_radec_at returned a position outside [0, 360) x [-90, 90]")
            if (allocated(error)) return
            worst = max(worst, maxval(abs(sph_vec_of(ra, dec) - pf_random_direction_at(dist_seed, k, 3_int64))))

            c = sph_vec_of(RA0(j), DEC0(j))
            call pf_random_disc_radec_at(dist_seed, k, RA0(j), DEC0(j), 3.0_real64, ra, dec, 2_int64, 1.0_real64)
            v = pf_random_disc_at(dist_seed, k, c, 3.0_real64 * DEG, 2_int64, 1.0_real64 * DEG)
            worst = max(worst, maxval(abs(sph_vec_of(ra, dec) - v)))

            sigma = 0.5_real64 * real(j, real64)
            kappa = 1.0_real64 / (sigma * DEG) ** 2
            call pf_random_vmf_radec_at(dist_seed, k, RA0(j), DEC0(j), sigma, ra, dec)
            v = pf_random_vmf_at(dist_seed, k, c, kappa)
            worst = max(worst, maxval(abs(sph_vec_of(ra, dec) - v)))
        end do
        ! 1e-9 degrees is about 1.7e-11 radians; each side is a few ulp, so this is generous and a
        ! wrong frame, a sign or a swapped argument misses it by many orders of magnitude.
        call check(error, worst <= 1.7e-11_real64, &
            "an RA/Dec form is not its vector form read in the standard frame -- they must be the same point to 1e-9 degrees")
        if (allocated(error)) return

        ! The pole rule, exactly: on a pole the offset is zero, so no rounding is involved.
        call pf_random_disc_radec_at(dist_seed, 1_int64, 123.4_real64, 90.0_real64, 0.0_real64, ra, dec)
        call check(error, ra == 0.0_real64 .and. dec == 90.0_real64, &
            "a disc of radius 0 on the north pole is not (0, 90) exactly: a declination of 90 is the pole whatever ra0 " // &
            "says, and the right ascension of a pole is 0 by rule")
        if (allocated(error)) return
        call pf_random_disc_radec_at(dist_seed, 1_int64, 17.0_real64, -90.0_real64, 0.0_real64, ra, dec, 5_int64)
        call check(error, ra == 0.0_real64 .and. dec == -90.0_real64, &
            "a disc of radius 0 on the south pole is not (0, -90) exactly")
        if (allocated(error)) return
        ! A vMF of vanishing width sits within far less than an ulp of the pole, so its declination is
        ! exactly -90 while its right ascension is the meaningless one of a point 1e-154 radians away.
        call pf_random_vmf_radec_at(dist_seed, 1_int64, 17.0_real64, -90.0_real64, 1.0e-170_real64, ra, dec)
        call check(error, dec == -90.0_real64 .and. ra >= 0.0_real64 .and. ra < 360.0_real64, &
            "a vMF of vanishing width on the south pole does not sit on the pole")
    end subroutine test_sphere_radec_is_the_vector_form

    !> Directions are uniform on the sphere: the mean, both marginals, and equal-area pixels.
    !!
    !! **The pixel chi-square is the load-bearing gate**: the 192 equal-area pixels of `nside = 4` see
    !! a non-isotropic construction the marginals can miss. The control keeps each draw's azimuth and
    !! remaps its cosine through `u**2`, which the same gates must reject.
    subroutine test_direction_is_uniform(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 192000_int64
        integer, parameter :: NCELL = 20
        integer(int64) :: k, ipix, zc(NCELL), pc(NCELL), pix(0:191), cz(NCELL), cpix(0:191)
        real(real64) :: v(3), vc(3), mean(3), u, zz, s, ph, se

        zc = 0_int64; pc = 0_int64; pix = 0_int64; cz = 0_int64; cpix = 0_int64
        mean = 0.0_real64
        do k = 1_int64, NDRAW
            v = pf_random_direction_at(dist_seed, 21_int64, k)
            mean = mean + v
            call sph_count(zc, 0.5_real64 * (v(3) + 1.0_real64))
            ph = atan2(v(2), v(1))
            call sph_count(pc, modulo(ph / TWO_PI, 1.0_real64))
            call pf_vec2pix_ring(4_int64, v, ipix)
            pix(ipix) = pix(ipix) + 1_int64
            ! CONTROL: z drawn as u**2 on the same azimuth.
            u = 0.5_real64 * (v(3) + 1.0_real64)
            zz = 2.0_real64 * u * u - 1.0_real64
            s = sqrt((1.0_real64 - zz) * (1.0_real64 + zz))
            vc = [s * cos(ph), s * sin(ph), zz]
            call sph_count(cz, 0.5_real64 * (zz + 1.0_real64))
            call pf_vec2pix_ring(4_int64, vc, ipix)
            cpix(ipix) = cpix(ipix) + 1_int64
        end do
        mean = mean / real(NDRAW, real64)
        se = sqrt(1.0_real64 / (3.0_real64 * real(NDRAW, real64)))
        call check(error, all(abs(mean) <= 4.0_real64 * se), "the mean direction is more than 4 standard errors from 0")
        if (allocated(error)) return
        call check(error, sph_chi2(zc) <= chi2_999(NCELL - 1), "z is not uniform on [-1, 1] at the 0.999 level")
        if (allocated(error)) return
        call check(error, sph_chi2(pc) <= chi2_999(NCELL - 1), "the azimuth is not uniform at the 0.999 level")
        if (allocated(error)) return
        call check(error, sph_chi2(pix) <= chi2_999(191), &
            "directions are not uniform over the 192 equal-area pixels of nside 4 at the 0.999 level")
        if (allocated(error)) return
        call check(error, sph_chi2(cz) > chi2_999(NCELL - 1) .and. sph_chi2(cpix) > chi2_999(191), &
            "the gates ACCEPT directions whose cosine is drawn as u**2, so they measure nothing")
    end subroutine test_direction_is_uniform

    !> A disc is uniform over its cap and contained in it, about any centre, with and without a hole.
    !!
    !! Four centres -- `+z`, a generic one, `-z` and one of length about `2e-300` -- and a radius of 0.4
    !! with and without `r_inner = 0.1`. Every draw must be inside the ring; the cosine of the angle
    !! from the centre must be uniform between the two bounds and the azimuth uniform in a frame the
    !! TEST builds, which has nothing to do with the library's. The control draws the ANGLE uniformly
    !! instead of its cosine, the classic mistake. Then the limits: `radius = 0` is the centre, a
    !! radius of `pi` or above is the whole sphere, and `r_inner = radius` is the circle itself.
    subroutine test_disc_is_uniform_and_contained(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 60000_int64
        integer, parameter :: NCELL = 20
        real(real64), parameter :: RADIUS = 0.4_real64
        real(real64), parameter :: CENTRES(3, 4) = reshape([0.0_real64, 0.0_real64, 1.0_real64, &
            0.3_real64, -0.5_real64, 0.8_real64, 0.0_real64, 0.0_real64, -1.0_real64, &
            1.0e-300_real64, -2.0e-300_real64, 5.0e-301_real64], [3, 4])
        ! The generic centre, `CENTRES(:, 2)`, named rather than written inline at the calls below:
        ! `pf_random_disc_at`'s `centre(3)` and `pf_angdist`'s `vec1(3)` dummies are explicit-shape,
        ! so an array constructor -- a value with no address of its own -- is argument-associated
        ! through a temporary, which ifx reports as `warning (406)` on every call under
        ! --profile debug. See .claude/rules/fortran-gotchas.md.
        real(real64), parameter :: TILTED(3) = [0.3_real64, -0.5_real64, 0.8_real64]
        integer(int64) :: k, outside, tc(NCELL), ac(NCELL), cc(NCELL), pix(0:191), ipix
        real(real64) :: centre(3), cu(3), f1(3), f2(3), v(3), rin, ang, cos_in, cos_out, t, theta_c
        integer :: j, variant

        do j = 1, 4
            centre = CENTRES(:, j)
            cu = centre / maxval(abs(centre))
            cu = cu / norm2(cu)
            call sph_test_frame(cu, f1, f2)
            do variant = 1, 2
                rin = merge(0.0_real64, 0.1_real64, variant == 1)
                cos_in = cos(rin)
                cos_out = cos(RADIUS)
                tc = 0_int64; ac = 0_int64; cc = 0_int64
                outside = 0_int64
                do k = 1_int64, NDRAW
                    if (variant == 1) then
                        v = pf_random_disc_at(dist_seed, 31_int64, centre, RADIUS, k)
                    else
                        v = pf_random_disc_at(dist_seed, 31_int64, centre, RADIUS, k, rin)
                    end if
                    call pf_angdist(cu, v, ang)
                    if (ang > RADIUS + 1.0e-12_real64 .or. ang < rin - 1.0e-12_real64) outside = outside + 1_int64
                    t = (cos_in - dot_product(v, cu)) / (cos_in - cos_out)
                    call sph_count(tc, t)
                    call sph_count(ac, modulo(atan2(dot_product(v, f2), dot_product(v, f1)) / TWO_PI, 1.0_real64))
                    ! CONTROL: the angle, not its cosine, uniform between the bounds.
                    theta_c = rin + t * (RADIUS - rin)
                    call sph_count(cc, (cos_in - cos(theta_c)) / (cos_in - cos_out))
                end do
                call check(error, outside == 0_int64, "pf_random_disc_at placed a point outside its ring")
                if (allocated(error)) return
                call check(error, sph_chi2(tc) <= chi2_999(NCELL - 1), &
                    "the cosine of a disc point's angle from the centre is not uniform between the ring's bounds")
                if (allocated(error)) return
                call check(error, sph_chi2(ac) <= chi2_999(NCELL - 1), &
                    "a disc point's azimuth about the centre is not uniform, so the frame is not orthonormal or not about c")
                if (allocated(error)) return
                call check(error, sph_chi2(cc) > chi2_999(NCELL - 1), &
                    "the gate ACCEPTS a disc whose angle, rather than its cosine, is uniform -- it measures nothing")
                if (allocated(error)) return
            end do
        end do

        ! radius = 0 is the normalised centre. On +z that is exact by construction: the offset is 0,
        ! and 0 times any finite azimuth term adds nothing.
        do k = 1_int64, 50_int64
            v = pf_random_disc_at(dist_seed, k, ZAXIS, 0.0_real64)
            call check(error, all(v == ZAXIS), "a disc of radius 0 about +z did not return +z exactly")
            if (allocated(error)) return
            v = pf_random_disc_at(dist_seed, k, TILTED, 0.0_real64)
            call check(error, maxval(abs(v - TILTED / norm2(TILTED))) <= SPH_SAME, &
                "a disc of radius 0 did not return its normalised centre")
            if (allocated(error)) return
        end do

        ! pi and anything above it is the whole sphere, uniform over equal-area pixels.
        do variant = 1, 2
            pix = 0_int64
            do k = 1_int64, 192000_int64
                v = pf_random_disc_at(dist_seed, 33_int64, TILTED, merge(PI, 4.0_real64, variant == 1), k)
                call pf_vec2pix_ring(4_int64, v, ipix)
                pix(ipix) = pix(ipix) + 1_int64
            end do
            call check(error, sph_chi2(pix) <= chi2_999(191), &
                "a disc of radius pi, or one above it that clamps there, is not uniform over the whole sphere")
            if (allocated(error)) return
        end do

        ! An outer radius past the half turn clamps to it; an INNER one is refused instead, because
        ! clamping it leaves a ring of zero width whose every draw is the antipode, returned silently
        ! for what the caller wrote as an annulus (scenario random_disc_inner_above_half_turn). The
        ! largest legal inner radius is the half turn itself, and that ring IS the antipode alone.
        do k = 1_int64, 50_int64
            v = pf_random_disc_at(dist_seed, 37_int64, ZAXIS, 4.0_real64, k, PI)
            call check(error, maxval(abs(v - [0.0_real64, 0.0_real64, -1.0_real64])) <= SPH_SAME, &
                "a ring whose inner radius is exactly the half turn is not the antipode")
            if (allocated(error)) return
        end do

        ! r_inner = radius is the circle itself.
        do k = 1_int64, 2000_int64
            v = pf_random_disc_at(dist_seed, 35_int64, TILTED, 0.3_real64, k, 0.3_real64)
            call pf_angdist(TILTED, v, ang)
            call check(error, abs(ang - 0.3_real64) <= 1.0e-12_real64, "a ring with r_inner = radius is not the circle itself")
            if (allocated(error)) return
        end do

        ! A tiny disc keeps its size: the plain 1 - cos form would collapse every point onto the centre.
        outside = 0_int64
        ang = 0.0_real64
        do k = 1_int64, 2000_int64
            v = pf_random_disc_at(dist_seed, 36_int64, TILTED, 1.0e-9_real64, k)
            call pf_angdist(TILTED, v, t)
            if (t > 1.0e-9_real64 * (1.0_real64 + 1.0e-6_real64)) outside = outside + 1_int64
            ang = max(ang, t)
        end do
        call check(error, outside == 0_int64 .and. ang > 0.9e-9_real64, &
            "a disc of radius 1e-9 radians does not fill its own radius: its points must reach 0.9e-9 and never pass 1e-9")
    end subroutine test_disc_is_uniform_and_contained

    !> A disc on the sky stays uniform across the pole and across `ra = 0`, and qfeet's flat disc does not.
    !!
    !! Three centres from the design -- `dec0 = 89.5` (the disc crosses the pole), `ra0 = 359.9` (it
    !! straddles the wrap) and qfeet's own `(150, -89)` -- at radii of 3 and 30 degrees. Containment
    !! and the cosine's uniformity are measured with `pf_angdist_deg`, which needs no frame. The
    !! control is qfeet's `get_random_radec_in_radius`: a flat disc of offsets rotated onto the
    !! centre, whose density at the rim exceeds the centre's by `1/cos(radius) - 1` -- invisible at 3
    !! degrees, and rejected by the same gate at 30.
    subroutine test_disc_radec_across_the_pole_and_the_wrap(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 100000_int64
        integer, parameter :: NCELL = 20
        real(real64), parameter :: RA0(3) = [40.0_real64, 359.9_real64, 150.0_real64]
        real(real64), parameter :: DEC0(3) = [89.5_real64, 10.0_real64, -89.0_real64]
        real(real64), parameter :: RADII(2) = [3.0_real64, 30.0_real64]
        integer(int64) :: k, bad, tc(NCELL), qc(NCELL), past_pole, below_wrap, above_wrap
        real(real64) :: ra, dec, ang, cos_r, r, a, dra, ddec
        integer :: j, m

        do m = 1, 2
            cos_r = cos(RADII(m) * DEG)
            do j = 1, 3
                tc = 0_int64; qc = 0_int64
                bad = 0_int64; past_pole = 0_int64; below_wrap = 0_int64; above_wrap = 0_int64
                do k = 1_int64, NDRAW
                    call pf_random_disc_radec_at(dist_seed, 41_int64, RA0(j), DEC0(j), RADII(m), ra, dec, k)
                    if (.not. (ra >= 0.0_real64 .and. ra < 360.0_real64 .and. dec >= -90.0_real64 .and. &
                               dec <= 90.0_real64)) bad = bad + 1_int64
                    ang = pf_angdist_deg(ra, dec, RA0(j), DEC0(j))
                    if (ang > RADII(m) + 1.0e-9_real64) bad = bad + 1_int64
                    call sph_count(tc, (1.0_real64 - cos(ang * DEG)) / (1.0_real64 - cos_r))
                    if (dec > DEC0(j) .and. abs(modulo(ra - RA0(j) + 180.0_real64, 360.0_real64) - 180.0_real64) > 90.0_real64) &
                        past_pole = past_pole + 1_int64
                    if (ra < 1.0_real64) below_wrap = below_wrap + 1_int64
                    if (ra > 359.0_real64) above_wrap = above_wrap + 1_int64
                    ! CONTROL: qfeet's flat disc, whose angle from the centre is exactly this.
                    r = sqrt(pf_random_at(dist_seed, 42_int64, 2_int64 * k - 1_int64)) * RADII(m) * DEG
                    a = TWO_PI * pf_random_at(dist_seed, 42_int64, 2_int64 * k)
                    dra = r * cos(a)
                    ddec = r * sin(a)
                    call sph_count(qc, (1.0_real64 - cos(ddec) * cos(dra)) / (1.0_real64 - cos_r))
                end do
                call check(error, bad == 0_int64, &
                    "pf_random_disc_radec_at returned a position out of range or outside its disc")
                if (allocated(error)) return
                call check(error, sph_chi2(tc) <= chi2_999(NCELL - 1), &
                    "the cosine of the angle from a sky disc's centre is not uniform, so the disc is not uniform on the sphere")
                if (allocated(error)) return
                if (j == 1) then
                    call check(error, past_pole > 0_int64, &
                        "vacuity: no draw of the disc at dec0 = 89.5 crossed the pole, so the pole was not exercised")
                    if (allocated(error)) return
                end if
                if (j == 2) then
                    call check(error, below_wrap > 0_int64 .and. above_wrap > 0_int64, &
                        "vacuity: the disc at ra0 = 359.9 did not land on both sides of ra = 0")
                    if (allocated(error)) return
                end if
                if (m == 2) then
                    call check(error, sph_chi2(qc) > chi2_999(NCELL - 1), &
                        "the gate ACCEPTS qfeet's flat-disc approximation at a 30-degree radius, so it cannot tell " // &
                        "a uniform cap from the distortion it was written to catch")
                    if (allocated(error)) return
                end if
            end do
        end do
    end subroutine test_disc_radec_across_the_pole_and_the_wrap

    !> Points in a ball are uniform in volume: `(r/R)**3` uniform, the direction isotropic, a shell
    !! uniform between its radii, and a zero radius the origin. The control draws `r = R*u`.
    subroutine test_ball_is_uniform_in_volume(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 192000_int64
        integer, parameter :: NCELL = 20
        real(real64), parameter :: RADIUS = 2.5_real64
        real(real64), parameter :: INNER = 1.25_real64
        integer(int64) :: k, ipix, bad, rc(NCELL), sc(NCELL), cc(NCELL), pix(0:191)
        real(real64) :: p(3), rr, u

        rc = 0_int64; sc = 0_int64; cc = 0_int64; pix = 0_int64
        bad = 0_int64
        do k = 1_int64, NDRAW
            p = pf_random_ball_at(dist_seed, 51_int64, RADIUS, k)
            rr = norm2(p)
            if (rr > RADIUS * (1.0_real64 + 1.0e-14_real64)) bad = bad + 1_int64
            u = (rr / RADIUS) ** 3
            call sph_count(rc, u)
            call sph_count(cc, u ** 3)                   ! CONTROL: r = R*u makes (r/R)**3 = u**3
            call pf_vec2pix_ring(4_int64, p, ipix)
            pix(ipix) = pix(ipix) + 1_int64
            p = pf_random_ball_at(dist_seed, 52_int64, RADIUS, k, INNER)
            rr = norm2(p)
            if (rr < INNER * (1.0_real64 - 1.0e-14_real64) .or. rr > RADIUS * (1.0_real64 + 1.0e-14_real64)) &
                bad = bad + 1_int64
            call sph_count(sc, (rr ** 3 - INNER ** 3) / (RADIUS ** 3 - INNER ** 3))
        end do
        call check(error, bad == 0_int64, "pf_random_ball_at placed a point outside its ball or shell")
        if (allocated(error)) return
        call check(error, sph_chi2(rc) <= chi2_999(NCELL - 1), "(r/R)**3 is not uniform, so the ball is not uniform in volume")
        if (allocated(error)) return
        call check(error, sph_chi2(pix) <= chi2_999(191), "a ball point's direction is not isotropic")
        if (allocated(error)) return
        call check(error, sph_chi2(sc) <= chi2_999(NCELL - 1), &
            "(r**3 - r_in**3)/(R**3 - r_in**3) is not uniform, so the shell is not uniform in volume")
        if (allocated(error)) return
        call check(error, sph_chi2(cc) > chi2_999(NCELL - 1), &
            "the gate ACCEPTS a ball whose radius is R*u, so it cannot tell volume from length")
        if (allocated(error)) return
        do k = 1_int64, 20_int64
            p = pf_random_ball_at(dist_seed, k, 0.0_real64)
            call check(error, all(p == 0.0_real64), "a ball of radius 0 did not return the origin")
            if (allocated(error)) return
        end do
    end subroutine test_ball_is_uniform_in_volume

    !> The von Mises-Fisher draw has the right mean at every scale, the exact cosine distribution, the
    !! uniform limit at `kappa = 0` and at vanishing `kappa`, and the Gaussian limit of `sigma_deg`.
    !!
    !! The mean cosine against `coth(kappa) - 1/kappa` at `kappa` from 1e-3 to 1000, where the last
    !! runs the arm on which `exp(-2*kappa)` is exactly 0. The cosine's exact CDF,
    !! `(exp(kappa*(w-1)) - exp(-2*kappa))/(1 - exp(-2*kappa))`, at 1 and 50. `kappa` of 0, 1e-12 and
    !! 1e-30 must all be uniform over equal-area pixels: a `log(1 - x)` written plainly sends every
    !! draw at `kappa = 1e-30` to one pole. And at `sigma_deg = 0.01` the offsets must have the
    !! Rayleigh moments of an isotropic Gaussian of that width, with a width 5 % too large as the
    !! rejected control.
    subroutine test_vmf_moments_and_limits(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 100000_int64
        integer, parameter :: NCELL = 20
        real(real64), parameter :: MU(3) = [0.3_real64, -0.5_real64, 0.8_real64]
        real(real64), parameter :: KAPPAS(4) = [1.0e-3_real64, 1.0_real64, 50.0_real64, 1000.0_real64]
        real(real64), parameter :: TINY_KAPPAS(3) = [0.0_real64, 1.0e-12_real64, 1.0e-30_real64]
        real(real64), parameter :: SIGMA = 0.01_real64
        integer(int64) :: k, ipix, wc(NCELL), pix(0:191)
        real(real64) :: mu_hat(3), v(3), w, sum_w, kappa, want, var, fw, ra, dec, th, s1, s2, c1, c2
        integer :: j

        mu_hat = MU / norm2(MU)
        do j = 1, 4
            kappa = KAPPAS(j)
            sum_w = 0.0_real64
            wc = 0_int64
            do k = 1_int64, NDRAW
                v = pf_random_vmf_at(dist_seed, 61_int64, MU, kappa, k)
                w = dot_product(v, mu_hat)
                sum_w = sum_w + w
                if (j == 2 .or. j == 3) then
                    ! Only where exp(-2*kappa) is a normal number, so no underflow flag is raised.
                    fw = (exp(kappa * (w - 1.0_real64)) - exp(-2.0_real64 * kappa)) / (1.0_real64 - exp(-2.0_real64 * kappa))
                    call sph_count(wc, fw)
                end if
            end do
            want = 1.0_real64 / tanh(kappa) - 1.0_real64 / kappa
            var = 1.0_real64 - 2.0_real64 * want / kappa - want * want
            call check(error, abs(sum_w / real(NDRAW, real64) - want) <= 5.0_real64 * sqrt(var / real(NDRAW, real64)), &
                "the mean cosine of vMF draws is more than 5 standard errors from coth(kappa) - 1/kappa")
            if (allocated(error)) return
            if (j == 2 .or. j == 3) then
                call check(error, sph_chi2(wc) <= chi2_999(NCELL - 1), &
                    "the cosine of vMF draws does not follow its exact CDF at the 0.999 level")
                if (allocated(error)) return
            end if
        end do

        do j = 1, 3
            pix = 0_int64
            do k = 1_int64, 192000_int64
                v = pf_random_vmf_at(dist_seed, 62_int64, MU, TINY_KAPPAS(j), k)
                call pf_vec2pix_ring(4_int64, v, ipix)
                pix(ipix) = pix(ipix) + 1_int64
            end do
            call check(error, sph_chi2(pix) <= chi2_999(191), &
                "vMF draws at kappa 0, 1e-12 or 1e-30 are not uniform over the sphere: the uniform limit has been lost")
            if (allocated(error)) return
        end do

        ! The Gaussian limit of the sky form, and a control 5 % too wide.
        do j = 1, 2
            s1 = 0.0_real64
            s2 = 0.0_real64
            do k = 1_int64, NDRAW
                call pf_random_vmf_radec_at(dist_seed, 63_int64, 10.0_real64, 20.0_real64, &
                                            SIGMA * merge(1.0_real64, 1.05_real64, j == 1), ra, dec, k)
                th = pf_angdist_deg(ra, dec, 10.0_real64, 20.0_real64) / SIGMA
                s1 = s1 + th
                s2 = s2 + th * th
            end do
            c1 = s1 / real(NDRAW, real64)
            c2 = s2 / real(NDRAW, real64)
            ! Rayleigh with unit width: mean sqrt(pi/2), variance 2 - pi/2; E[th**2] = 2, var 4.
            if (j == 1) then
                call check(error, abs(c1 - sqrt(PI / 2.0_real64)) <= 5.0_real64 * sqrt((2.0_real64 - PI / 2.0_real64) &
                    / real(NDRAW, real64)) .and. abs(c2 - 2.0_real64) <= 5.0_real64 * sqrt(4.0_real64 / real(NDRAW, real64)), &
                    "vMF offsets at sigma_deg = 0.01 do not have the Rayleigh moments of a Gaussian of that width")
            else
                call check(error, abs(c1 - sqrt(PI / 2.0_real64)) > 5.0_real64 * sqrt((2.0_real64 - PI / 2.0_real64) &
                    / real(NDRAW, real64)), "the Gaussian-limit gate ACCEPTS a width 5 % too large, so it measures nothing")
            end if
            if (allocated(error)) return
        end do
    end subroutine test_vmf_moments_and_limits

    !> Rotations are Haar-uniform: proper and orthonormal, the image of an axis isotropic, and the
    !! rotation angle distributed as `(1 - cos a)/pi`. The control rotates by a uniform angle about a
    !! fixed axis, which is orthonormal and proper and still wrong.
    subroutine test_rotation_is_haar(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: NDRAW = 40000_int64
        integer, parameter :: NCELL = 20
        integer(int64) :: k, ipix, bad, pix(0:191), ac(NCELL), cc(NCELL)
        real(real64) :: r(3, 3), ang, v(3)

        pix = 0_int64; ac = 0_int64; cc = 0_int64
        bad = 0_int64
        do k = 1_int64, NDRAW
            r = pf_random_rotation_at(dist_seed, 71_int64, k)
            if (sph_orthonormal_gap(r) > 1.0e-14_real64) bad = bad + 1_int64
            v = matmul(r, ZAXIS)
            call pf_vec2pix_ring(4_int64, v, ipix)
            pix(ipix) = pix(ipix) + 1_int64
            ang = acos(max(-1.0_real64, min(1.0_real64, 0.5_real64 * (r(1, 1) + r(2, 2) + r(3, 3) - 1.0_real64))))
            call sph_count(ac, (ang - sin(ang)) / PI)
            ! CONTROL: a uniform angle about a fixed axis.
            ang = PI * pf_random_at(dist_seed, 72_int64, k)
            call sph_count(cc, (ang - sin(ang)) / PI)
        end do
        call check(error, bad == 0_int64, "pf_random_rotation_at returned a matrix that is not a proper rotation to 1e-14")
        if (allocated(error)) return
        call check(error, sph_chi2(pix) <= chi2_999(191), "a random rotation's image of +z is not isotropic")
        if (allocated(error)) return
        call check(error, sph_chi2(ac) <= chi2_999(NCELL - 1), &
            "the rotation angle is not distributed as (1 - cos a)/pi, so the rotations are not Haar-uniform")
        if (allocated(error)) return
        call check(error, sph_chi2(cc) > chi2_999(NCELL - 1), &
            "the gate ACCEPTS rotations by a uniform angle, so it cannot tell Haar measure from a naive draw")
    end subroutine test_rotation_is_haar

    !> Every tier-0 sphere form is `pure`: usable in `do concurrent`, with the values it has outside.
    !!
    !! A compile-time property asserted at run time, as `test_exp_purity` explains: a form that stopped
    !! being pure would fail to compile here.
    subroutine test_sphere_purity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: N = 64
        real(real64), parameter :: C(3) = [0.3_real64, -0.5_real64, 0.8_real64]
        integer :: j
        real(real64) :: dv(3, N), dd(3, N), db(3, N), dm(3, N), dr(3, 3, N)
        real(real64) :: ra(N), dec(N), dra(N), ddec(N), vra(N), vdec(N), ra1, dec1
        real(real64) :: worst, worst_deg

        do concurrent (j = 1:N)
            dv(:, j) = pf_random_direction_at(dist_seed, int(j, int64))
            dd(:, j) = pf_random_disc_at(dist_seed, int(j, int64), C, 0.3_real64, 2_int64, 0.1_real64)
            db(:, j) = pf_random_ball_at(dist_seed, int(j, int64), 2.0_real64, 3_int64, 1.0_real64)
            dm(:, j) = pf_random_vmf_at(dist_seed, int(j, int64), C, 20.0_real64)
            dr(:, :, j) = pf_random_rotation_at(dist_seed, int(j, int64))
            call pf_random_radec_at(dist_seed, int(j, int64), ra(j), dec(j))
            call pf_random_disc_radec_at(dist_seed, int(j, int64), 10.0_real64, 20.0_real64, 2.0_real64, dra(j), ddec(j))
            call pf_random_vmf_radec_at(dist_seed, int(j, int64), 10.0_real64, 20.0_real64, 2.0_real64, vra(j), vdec(j))
        end do
        worst = 0.0_real64
        worst_deg = 0.0_real64
        do j = 1, N
            worst = max(worst, maxval(abs(dv(:, j) - pf_random_direction_at(dist_seed, int(j, int64)))))
            worst = max(worst, maxval(abs(dd(:, j) - pf_random_disc_at(dist_seed, int(j, int64), C, 0.3_real64, 2_int64, &
                                                                        0.1_real64))))
            worst = max(worst, maxval(abs(db(:, j) - pf_random_ball_at(dist_seed, int(j, int64), 2.0_real64, 3_int64, &
                                                                        1.0_real64))) / 2.0_real64)
            worst = max(worst, maxval(abs(dm(:, j) - pf_random_vmf_at(dist_seed, int(j, int64), C, 20.0_real64))))
            worst = max(worst, maxval(abs(dr(:, :, j) - pf_random_rotation_at(dist_seed, int(j, int64)))))
            call pf_random_radec_at(dist_seed, int(j, int64), ra1, dec1)
            worst_deg = max(worst_deg, sph_deg_gap(ra(j), dec(j), transfer(ra1, 0_int64), transfer(dec1, 0_int64)))
            call pf_random_disc_radec_at(dist_seed, int(j, int64), 10.0_real64, 20.0_real64, 2.0_real64, ra1, dec1)
            worst_deg = max(worst_deg, sph_deg_gap(dra(j), ddec(j), transfer(ra1, 0_int64), transfer(dec1, 0_int64)))
            call pf_random_vmf_radec_at(dist_seed, int(j, int64), 10.0_real64, 20.0_real64, 2.0_real64, ra1, dec1)
            worst_deg = max(worst_deg, sph_deg_gap(vra(j), vdec(j), transfer(ra1, 0_int64), transfer(dec1, 0_int64)))
        end do
        call check(error, worst <= SPH_SAME .and. worst_deg <= SPH_TOL_DEG, &
            "a tier-0 sphere form gives different values inside a do concurrent body than outside it")
    end subroutine test_sphere_purity

    !> The `int32` and `int64` stream-index specifics are one procedure reached two ways, for every
    !! name including the fills, at negative indices too -- where a lost sign extension would show.
    subroutine test_sphere_kinds_agree(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int32), parameter :: I32S(4) = [-5_int32, 0_int32, 7_int32, huge(1_int32)]
        real(real64), parameter :: C(3) = [0.3_real64, -0.5_real64, 0.8_real64]
        real(real64) :: worst, worst_deg, ra1, dec1, ra2, dec2, f1(3, 4), f2(3, 4), g1(4), g2(4), h1(4), h2(4)
        integer :: j
        integer(int64) :: i64

        worst = 0.0_real64
        worst_deg = 0.0_real64
        do j = 1, 4
            i64 = int(I32S(j), int64)
            worst = max(worst, maxval(abs(pf_random_direction_at(dist_seed, I32S(j), 2_int64) - &
                                          pf_random_direction_at(dist_seed, i64, 2_int64))))
            worst = max(worst, maxval(abs(pf_random_disc_at(dist_seed, I32S(j), C, 0.5_real64, 2_int64, 0.2_real64) - &
                                          pf_random_disc_at(dist_seed, i64, C, 0.5_real64, 2_int64, 0.2_real64))))
            worst = max(worst, maxval(abs(pf_random_ball_at(dist_seed, I32S(j), 1.0_real64, 2_int64, 0.5_real64) - &
                                          pf_random_ball_at(dist_seed, i64, 1.0_real64, 2_int64, 0.5_real64))))
            worst = max(worst, maxval(abs(pf_random_vmf_at(dist_seed, I32S(j), C, 7.0_real64, 2_int64) - &
                                          pf_random_vmf_at(dist_seed, i64, C, 7.0_real64, 2_int64))))
            worst = max(worst, maxval(abs(pf_random_rotation_at(dist_seed, I32S(j), 2_int64) - &
                                          pf_random_rotation_at(dist_seed, i64, 2_int64))))
            call pf_random_radec_at(dist_seed, I32S(j), ra1, dec1, 2_int64)
            call pf_random_radec_at(dist_seed, i64, ra2, dec2, 2_int64)
            worst_deg = max(worst_deg, sph_deg_gap(ra1, dec1, transfer(ra2, 0_int64), transfer(dec2, 0_int64)))
            call pf_random_disc_radec_at(dist_seed, I32S(j), 10.0_real64, 20.0_real64, 2.0_real64, ra1, dec1, 2_int64, &
                                         1.0_real64)
            call pf_random_disc_radec_at(dist_seed, i64, 10.0_real64, 20.0_real64, 2.0_real64, ra2, dec2, 2_int64, 1.0_real64)
            worst_deg = max(worst_deg, sph_deg_gap(ra1, dec1, transfer(ra2, 0_int64), transfer(dec2, 0_int64)))
            call pf_random_vmf_radec_at(dist_seed, I32S(j), 10.0_real64, 20.0_real64, 2.0_real64, ra1, dec1, 2_int64)
            call pf_random_vmf_radec_at(dist_seed, i64, 10.0_real64, 20.0_real64, 2.0_real64, ra2, dec2, 2_int64)
            worst_deg = max(worst_deg, sph_deg_gap(ra1, dec1, transfer(ra2, 0_int64), transfer(dec2, 0_int64)))
            call pf_random_fill_direction(dist_seed, I32S(j), f1, 3_int64)
            call pf_random_fill_direction(dist_seed, i64, f2, 3_int64)
            worst = max(worst, maxval(abs(f1 - f2)))
            call pf_random_fill_radec(dist_seed, I32S(j), g1, h1, 3_int64)
            call pf_random_fill_radec(dist_seed, i64, g2, h2, 3_int64)
            worst_deg = max(worst_deg, maxval(abs(g1 - g2)), maxval(abs(h1 - h2)))
        end do
        call check(error, worst <= SPH_SAME .and. worst_deg <= SPH_TOL_DEG, &
            "the int32 and int64 stream-index specifics of a sphere form disagree")
        if (allocated(error)) return
        ! And the index reaches the draw at all: streams -5 and 5 are different points.
        call check(error, maxval(abs(pf_random_direction_at(dist_seed, -5_int32) - pf_random_direction_at(dist_seed, 5_int32))) &
            > 1.0e-3_real64, "pf_random_direction_at gives the same point on streams -5 and 5, so the index is not reaching it")
    end subroutine test_sphere_kinds_agree

    !> Draw `2**62` is the last one every sphere form accepts, and a fill may end exactly there;
    !! a draw below 1 clamps to 1. The refusal beyond it is `random_sphere_draw_beyond_2p62`.
    subroutine test_sphere_draw_bound(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: LAST = 4611686018427387904_int64
        real(real64), parameter :: C(3) = [0.3_real64, -0.5_real64, 0.8_real64]
        real(real64) :: v(3), r(3, 3), ra, dec, ra2, dec2, f(3, 3), fra(3), fdec(3)
        logical :: ok

        v = pf_random_direction_at(dist_seed, 3_int64, LAST)
        ok = abs(norm2(v) - 1.0_real64) <= 8.0_real64 * epsilon(1.0_real64)
        v = pf_random_disc_at(dist_seed, 3_int64, C, 0.2_real64, LAST, 0.1_real64)
        ok = ok .and. abs(norm2(v) - 1.0_real64) <= 8.0_real64 * epsilon(1.0_real64)
        v = pf_random_ball_at(dist_seed, 3_int64, 2.0_real64, LAST, 1.0_real64)
        ok = ok .and. norm2(v) >= 1.0_real64 - 1.0e-14_real64 .and. norm2(v) <= 2.0_real64 + 1.0e-14_real64
        v = pf_random_vmf_at(dist_seed, 3_int64, C, 3.0_real64, LAST)
        ok = ok .and. abs(norm2(v) - 1.0_real64) <= 8.0_real64 * epsilon(1.0_real64)
        r = pf_random_rotation_at(dist_seed, 3_int64, LAST)
        ok = ok .and. sph_orthonormal_gap(r) <= 1.0e-14_real64
        call pf_random_radec_at(dist_seed, 3_int64, ra, dec, LAST)
        ok = ok .and. ra >= 0.0_real64 .and. ra < 360.0_real64
        call pf_random_disc_radec_at(dist_seed, 3_int64, 10.0_real64, 20.0_real64, 2.0_real64, ra, dec, LAST)
        ok = ok .and. pf_angdist_deg(ra, dec, 10.0_real64, 20.0_real64) <= 2.0_real64 + 1.0e-9_real64
        call pf_random_vmf_radec_at(dist_seed, 3_int64, 10.0_real64, 20.0_real64, 2.0_real64, ra, dec, LAST)
        ok = ok .and. dec >= -90.0_real64 .and. dec <= 90.0_real64
        call check(error, ok, "a sphere form at draw 2**62, the last it accepts, did not return a valid value")
        if (allocated(error)) return

        call pf_random_fill_direction(dist_seed, 3_int64, f, LAST - 2_int64)
        call check(error, maxval(abs(f(:, 3) - pf_random_direction_at(dist_seed, 3_int64, LAST))) <= 2.0_real64 * SPH_SAME, &
            "a pf_random_fill_direction whose last draw is exactly 2**62 does not end on draw 2**62")
        if (allocated(error)) return
        call pf_random_fill_radec(dist_seed, 3_int64, fra, fdec, LAST - 2_int64)
        call pf_random_radec_at(dist_seed, 3_int64, ra, dec, LAST)
        call check(error, sph_deg_gap(fra(3), fdec(3), transfer(ra, 0_int64), transfer(dec, 0_int64)) <= SPH_TOL_DEG, &
            "a pf_random_fill_radec whose last draw is exactly 2**62 does not end on draw 2**62")
        if (allocated(error)) return

        ! Below 1 clamps to 1, as everywhere in the module.
        call pf_random_radec_at(dist_seed, 3_int64, ra, dec, 0_int64)
        call pf_random_radec_at(dist_seed, 3_int64, ra2, dec2, 1_int64)
        call check(error, sph_deg_gap(ra, dec, transfer(ra2, 0_int64), transfer(dec2, 0_int64)) <= SPH_TOL_DEG .and. &
            maxval(abs(pf_random_direction_at(dist_seed, 3_int64, -9_int64) - pf_random_direction_at(dist_seed, 3_int64))) &
            <= SPH_SAME, "a sphere draw below 1 does not clamp to draw 1")
    end subroutine test_sphere_draw_bound

    ! ---- Points on a sphere: helpers ----

    !> The three `real64` values a flattened bit table holds for row `k`.
    pure function sph_vec(bits, k) result(v)
        integer(int64), intent(in) :: bits(:)       !! three bit patterns per row
        integer, intent(in) :: k                    !! the row
        real(real64) :: v(3)                        !! the row's vector
        v(1) = transfer(bits(3 * (k - 1) + 1), 0.0_real64)
        v(2) = transfer(bits(3 * (k - 1) + 2), 0.0_real64)
        v(3) = transfer(bits(3 * (k - 1) + 3), 0.0_real64)
    end function sph_vec

    !> A sky position as a unit vector, written here independently of the library's conversion.
    pure function sph_vec_of(ra, dec) result(v)
        real(real64), intent(in) :: ra              !! right ascension, degrees
        real(real64), intent(in) :: dec             !! declination, degrees
        real(real64) :: v(3)                        !! the unit vector, standard frame
        v = [cos(dec * DEG) * cos(ra * DEG), cos(dec * DEG) * sin(ra * DEG), sin(dec * DEG)]
    end function sph_vec_of

    !> The two uniforms of draw `d` of `(key, stream)` straight from the cipher: block `d - 1`'s halves.
    subroutine sph_block_uniforms(key, stream, d, u1, u2)
        integer(int64), intent(in) :: key           !! the derived key
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: d             !! the draw
        real(real64), intent(out) :: u1             !! the block's first uniform
        real(real64), intent(out) :: u2             !! the block's second uniform
        integer(int64) :: w0, w1, w2, w3
        call parquet_debug_random_block(key, stream, d - 1_int64, w0, w1, w2, w3)
        u1 = real(ishft(ior(ishft(w1, 32), w0), -11), real64) * 2.0_real64 ** (-53)
        u2 = real(ishft(ior(ishft(w3, 32), w2), -11), real64) * 2.0_real64 ** (-53)
    end subroutine sph_block_uniforms

    !> Records the worst gap between a result and its reference, and whether any differ at all.
    pure subroutine sph_note(got, want, worst, differs)
        real(real64), intent(in) :: got(3)          !! the value under test
        real(real64), intent(in) :: want(3)         !! the reference
        real(real64), intent(inout) :: worst        !! the largest gap so far
        logical, intent(inout) :: differs           !! whether any component so far is not bit-identical
        worst = max(worst, maxval(abs(got - want)))
        if (any(transfer(got, [0_int64]) /= transfer(want, [0_int64]))) differs = .true.
    end subroutine sph_note

    !> The gap between a sky position and a reference given as bit patterns, in degrees: the larger
    !! of the declination gap and the right-ascension gap scaled by `cos(dec)`, across the wrap.
    pure function sph_deg_gap(ra, dec, ra_bits, dec_bits) result(g)
        real(real64), intent(in) :: ra              !! right ascension under test, degrees
        real(real64), intent(in) :: dec             !! declination under test, degrees
        integer(int64), intent(in) :: ra_bits       !! the reference right ascension's bit pattern
        integer(int64), intent(in) :: dec_bits      !! the reference declination's bit pattern
        real(real64) :: g                           !! the gap, degrees
        real(real64) :: ra_ref, dec_ref, dra
        ra_ref = transfer(ra_bits, 0.0_real64)
        dec_ref = transfer(dec_bits, 0.0_real64)
        dra = abs(ra - ra_ref)
        dra = min(dra, 360.0_real64 - dra)
        g = max(abs(dec - dec_ref), dra * cos(dec_ref * DEG))
    end function sph_deg_gap

    !> How far a matrix is from a proper rotation: the worst entry of `r r^T - I`, or of `det - 1`.
    pure function sph_orthonormal_gap(r) result(g)
        real(real64), intent(in) :: r(3, 3)         !! the matrix
        real(real64) :: g                           !! the gap
        real(real64) :: det, m(3, 3)
        integer :: i
        m = matmul(r, transpose(r))
        g = 0.0_real64
        do i = 1, 3
            g = max(g, maxval(abs(m(:, i) - merge(1.0_real64, 0.0_real64, [1, 2, 3] == i))))
        end do
        det = r(1, 1) * (r(2, 2) * r(3, 3) - r(2, 3) * r(3, 2)) - r(1, 2) * (r(2, 1) * r(3, 3) - r(2, 3) * r(3, 1)) &
            + r(1, 3) * (r(2, 1) * r(3, 2) - r(2, 2) * r(3, 1))
        g = max(g, abs(det - 1.0_real64))
    end function sph_orthonormal_gap

    !> Adds a value in `[0, 1)` to one of `size(counts)` equal cells, clamping a rounding edge inward.
    pure subroutine sph_count(counts, u)
        integer(int64), intent(inout) :: counts(:)  !! the cells
        real(real64), intent(in) :: u               !! the value, nominally in `[0, 1)`
        integer :: cell
        cell = int(real(size(counts), real64) * u) + 1
        cell = max(1, min(size(counts), cell))
        counts(cell) = counts(cell) + 1_int64
    end subroutine sph_count

    !> The chi-square of `counts` against equal expectations.
    pure function sph_chi2(counts) result(c)
        integer(int64), intent(in) :: counts(:)     !! observed cell counts
        real(real64) :: c                           !! the statistic
        real(real64) :: expect
        expect = real(sum(counts), real64) / real(size(counts), real64)
        c = sum((real(counts, real64) - expect) ** 2) / expect
    end function sph_chi2

    !> Pearson's chi-square test of independence of two samples in `[0, 1)`, on an `nbin x nbin` table.
    !!
    !! The expectation of each cell is the product of its row and column totals over the sample size,
    !! so the marginals need not be uniform -- two of the statistics that feed it are not.
    pure function sph_independence(a, b, nbin) result(c)
        real(real64), intent(in) :: a(:)            !! the first sample
        real(real64), intent(in) :: b(:)            !! the second, paired with the first
        integer, intent(in) :: nbin                 !! cells per axis
        real(real64) :: c                           !! the statistic, `(nbin-1)**2` degrees of freedom
        integer(int64) :: table(nbin, nbin), rows(nbin), cols(nbin)
        integer :: k, i, j
        real(real64) :: expect
        table = 0_int64
        do k = 1, size(a)
            i = max(1, min(nbin, int(real(nbin, real64) * a(k)) + 1))
            j = max(1, min(nbin, int(real(nbin, real64) * b(k)) + 1))
            table(i, j) = table(i, j) + 1_int64
        end do
        rows = sum(table, dim=2)
        cols = sum(table, dim=1)
        c = 0.0_real64
        do j = 1, nbin
            do i = 1, nbin
                expect = real(rows(i), real64) * real(cols(j), real64) / real(size(a), real64)
                if (expect > 0.0_real64) c = c + (real(table(i, j), real64) - expect) ** 2 / expect
            end do
        end do
    end function sph_independence

    !> The upper quantile of a chi-square with `df` degrees of freedom `z` normal deviates out,
    !! by Wilson-Hilferty, as `chi2_999` is at `z = 3.09`.
    pure function chi2_quantile(df, z) result(q)
        integer, intent(in) :: df                   !! degrees of freedom, at least 1
        real(real64), intent(in) :: z               !! the deviate
        real(real64) :: q                           !! the quantile
        real(real64) :: d, h
        d = real(max(df, 1), real64)
        h = 2.0_real64 / (9.0_real64 * d)
        q = d * (1.0_real64 - h + z * sqrt(h)) ** 3
    end function chi2_quantile

    !> An orthonormal frame perpendicular to a unit vector, built the TEST's way: Gram-Schmidt
    !! against whichever of `x` and `y` is further from it -- not the library's rule.
    pure subroutine sph_test_frame(c, f1, f2)
        real(real64), intent(in) :: c(3)            !! a unit vector
        real(real64), intent(out) :: f1(3)          !! perpendicular to `c`
        real(real64), intent(out) :: f2(3)          !! `c x f1`
        real(real64) :: a(3)
        a = merge([1.0_real64, 0.0_real64, 0.0_real64], [0.0_real64, 1.0_real64, 0.0_real64], abs(c(1)) < abs(c(2)))
        f1 = a - dot_product(a, c) * c
        f1 = f1 / norm2(f1)
        f2 = [c(2) * f1(3) - c(3) * f1(2), c(3) * f1(1) - c(1) * f1(3), c(1) * f1(2) - c(2) * f1(1)]
    end subroutine sph_test_frame

end module test_random_dist
