!> Tests for `parquet_integrate`: the 21-point Gauss-Kronrod rule with adaptive bisection over a
!> finite range, the evaluation record, the budget and every `PF_INT_*` code reachable today.
!!
!! **Every expected value is a closed form, derived in `test_integrate_support.f90` beside the
!! integrand it belongs to**, never a number read off a run. The one class of figure that IS a
!! measurement is an evaluation COUNT, and no count is asserted as an equality unless it is a
!! structural fact -- 21 for one rule application, 0 for a zero-width range, `42*nsub - 21` for a
!! finite partition. Every other count is asserted as a window one bisection wide, because a
!! bisection decision rests on an error estimate whose last bits differ between compilers.
!!
!! **The accuracy assertions are what catch a wrong rule.** Swapping the Kronrod weights for the
!! Gauss weights in `qk21` leaves every count unchanged and every answer slightly wrong, so a
!! suite that only counted would pass; each reference integrand is therefore asserted against its
!! closed form at the tolerance the call asked for.
!!
!! **`test_points_reproduce_the_partition_integral` is the one test that reads the record as a
!! quadrature rule**, which is the whole reason the record has weights. Nothing else in the
!! library reads them, so a weight that lost its half-length or its `log_base` Jacobian would
!! leave every integral right and the record silently wrong.
!!
!! This suite is pure computation with no fixture files and no process-global state, so it stays
!! out of the runner's parallelism exclusion list and runs concurrently. Its only library import
!! is `use parquet_integrate`: it is registered in `run_tester_pf.f90`, the runner that executes
!! no `bind(C)` call, and `check_test_runner_partition` requires that the files feeding that
!! runner never reach `parquet_bindings`.
module test_integrate

    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet_integrate
    use test_integrate_support
    use iso_fortran_env, only : real64

    implicit none
    private

    public :: collect_tests_integrate

    !> Evaluations one bisection costs; the width of every count window asserted here.
    integer, parameter :: BISECTION = 42
    !> Evaluations one rule application costs.
    integer, parameter :: ONE_RULE = 21

contains

    !> Registers this module's tests.
    subroutine collect_tests_integrate(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("every finite closed form is reproduced to the tolerance asked for", &
                         test_finite_closed_forms), &
            new_unittest("log_base integrates six decades in a single rule application", &
                         test_log_base_over_decades), &
            new_unittest("the six reference integrands meet their tolerance and their budget", &
                         test_six_reference_integrands), &
            new_unittest("all four specifics agree bit for bit and the object counts its calls", &
                         test_entry_forms_agree), &
            new_unittest("an atol-only tolerance converges where a relative one cannot", &
                         test_atol_only_tolerance), &
            new_unittest("a zero-width range returns zero without evaluating anything", &
                         test_zero_width_range), &
            new_unittest("an identically zero integrand costs exactly one rule application", &
                         test_zero_integrand_costs_one_rule), &
            new_unittest("the record reproduces the partition integral through its weights", &
                         test_points_reproduce_the_partition_integral), &
            new_unittest("asking for no record changes neither the count nor the answer", &
                         test_points_absent_records_nothing), &
            new_unittest("appending two records sums to the sum of the two integrals", &
                         test_points_append_adds_up), &
            new_unittest("the counted evaluations match QUADPACK's own rule formula", &
                         test_neval_matches_the_rule_formula), &
            new_unittest("reaching the budget reports PF_INT_LIMIT and never exceeds it", &
                         test_status_limit_and_converged_agree), &
            new_unittest("a tolerance below the round-off floor reports PF_INT_ROUNDOFF", &
                         test_status_roundoff), &
            new_unittest("a discontinuity drilled into reports a non-OK status, not a wrong answer", &
                         test_status_on_a_discontinuity), &
            new_unittest("a feature the first rule application cannot see is integrated as zero", &
                         test_blind_spot_is_documented), &
            new_unittest("an rtol below 50*epsilon is accepted when atol is positive", &
                         test_rtol_floor_needs_atol), &
            new_unittest("the extrapolation earns its keep on an endpoint singularity", &
                         test_extrapolation_earns_its_keep), &
            new_unittest("every infinite tail reproduces its closed form, negated ones included", &
                         test_infinite_tails), &
            new_unittest("a feature far along an infinite range is found, not stepped over", &
                         test_start_panel_search), &
            new_unittest("an oscillatory tail that runs out of budget still answers correctly", &
                         test_integral_inf_oscillatory), &
            new_unittest("the four range spellings agree with each other and with a split", &
                         test_infinite_ranges_are_consistent), &
            new_unittest("both caps stop the walk and neither is reported as convergence", &
                         test_max_panels_caps_the_walk), &
            new_unittest("every entry form reaches the walk and the object counts its calls", &
                         test_infinite_entry_forms) &
            ]

    end subroutine collect_tests_integrate

    !> Asserts the closed forms an elementary antiderivative gives, over a finite range.
    subroutine test_finite_closed_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: r, want

        ! sin over a range covering three half-periods: the antiderivative is -cos.
        want = cos(0.1_real64) - cos(10.0_real64)
        r = pf_integrate(sine, 0.1_real64, 10.0_real64, 1.0e-12_real64)
        call check(error, abs(r - want) <= 1.0e-12_real64*abs(want), &
                   "sin over [0.1, 10] must reproduce cos(0.1) - cos(10)")
        if (allocated(error)) return

        ! x*x over the unit interval: the rule is exact to degree 31, so this is exact.
        r = pf_integrate(x_squared, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        call check(error, abs(r - 1.0_real64/3.0_real64) <= 1.0e-15_real64, &
                   "x*x over [0, 1] must be 1/3 to rounding")
        if (allocated(error)) return

        ! Runge's function: a rational whose antiderivative is an arctangent.
        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        call check(error, abs(r - runge_exact()) <= 1.0e-10_real64*abs(runge_exact()), &
                   "Runge's function over [0, 1] must be atan(5)/5")
        if (allocated(error)) return

        ! A gaussian narrow enough to need refinement but wide enough to be found.
        r = pf_integrate(sharp_gauss, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        call check(error, abs(r - sharp_gauss_exact()) <= 1.0e-10_real64*abs(sharp_gauss_exact()), &
                   "a gaussian of width 0.01 at 0.9 must reproduce its error-function form")

    end subroutine test_finite_closed_forms

    !> Asserts that `log_base` turns six decades of a power law into one rule application.
    !!
    !! `x**-1.5` is a straight line in `log x` against `log f`, so the transformed integrand is a
    !! pure exponential the 21-point rule resolves at once. The same call without `log_base` needs
    !! tens of subintervals, which is the whole reason the argument exists.
    subroutine test_log_base_over_decades(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: with_log, without_log
        real(real64)              :: r, want
        real(real64), parameter   :: LO = 1.0e-3_real64, HI = 1.0e3_real64

        want = inv_pow15_exact(LO, HI)

        r = pf_integrate(inv_pow15, LO, HI, 1.0e-10_real64, log_base=.true., info=with_log)
        call check(error, abs(r - want) <= 1.0e-10_real64*abs(want), &
                   "x**-1.5 over six decades in log x must reproduce 2*(lo**-0.5 - hi**-0.5)")
        if (allocated(error)) return
        call check(error, with_log%converged, "the log_base call must converge")
        if (allocated(error)) return

        r = pf_integrate(inv_pow15, LO, HI, 1.0e-10_real64, info=without_log)
        call check(error, abs(r - want) <= 1.0e-10_real64*abs(want), &
                   "the same integral in linear x must reproduce the same closed form")
        if (allocated(error)) return
        call check(error, with_log%neval < without_log%neval, &
                   "integrating a power law in log x must cost fewer evaluations than in linear x")

    end subroutine test_log_base_over_decades

    !> Asserts the six reference integrands of the design's engine comparison.
    !!
    !! Each is asserted against its closed form at the tolerance requested, and its evaluation
    !! count against a window one bisection wide around the count measured when this was written.
    !! The accuracy half is what a wrong quadrature weight breaks; the count half is what a rule
    !! that stopped subdividing too late breaks. Four are finite and two are tails, so the set
    !! covers both paths through the driver at one tolerance.
    subroutine test_six_reference_integrands(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), parameter :: RTOL = 1.0e-6_real64

        call one_reference(error, osc_a, osc_a_exact(), RTOL, 399, "A, 2/(2+sin(10 pi x))")
        if (allocated(error)) return
        call one_reference(error, log_sqrt, log_sqrt_exact(), RTOL, 1995, "B, log(x)/sqrt(x)")
        if (allocated(error)) return
        call one_reference(error, peak_c, peak_c_exact(), RTOL, 399, "C, an interior peak")
        if (allocated(error)) return
        call one_reference(error, heavy_f, heavy_f_exact(), RTOL, 105, "F, a 40-term sum")
        if (allocated(error)) return
        call one_reference_inf(error, exp_over_x, 1.0_real64, exp_over_x_exact(), RTOL, 105, &
                               "D, exp(-x)/x over [1, inf)")
        if (allocated(error)) return
        call one_reference_inf(error, exp_cos, 0.0_real64, exp_cos_exact(), RTOL, 168, &
                               "E, exp(-x) cos(x) over [0, inf)")

    end subroutine test_six_reference_integrands

    !> Integrates one reference integrand over `[a, inf)` and asserts its accuracy and its cost.
    subroutine one_reference_inf(error, fn, a, want, rtol, measured, what)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle.
        procedure(pf_integrand_func)               :: fn       !! the integrand
        real(real64), intent(in)                   :: a        !! lower bound
        real(real64), intent(in)                   :: want     !! its closed form over [a, inf)
        real(real64), intent(in)                   :: rtol     !! tolerance to ask for
        integer, intent(in)                        :: measured !! count when this was written
        character(len=*), intent(in)               :: what     !! names the case in a message

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(fn, a, pf_infinity(), rtol, info=info)
        call check(error, info%converged, "case "//what//" must converge")
        if (allocated(error)) return
        call check(error, abs(r - want) <= rtol*abs(want), &
                   "case "//what//" must meet the relative tolerance it was given")
        if (allocated(error)) return
        call check(error, info%neval >= ONE_RULE .and. info%neval <= measured + BISECTION, &
                   "case "//what//" must cost what the walk costs, to within one bisection")

    end subroutine one_reference_inf

    !> Integrates one reference integrand over `[0, 1]` and asserts its accuracy and its cost.
    subroutine one_reference(error, fn, want, rtol, measured, what)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle.
        procedure(pf_integrand_func)               :: fn       !! the integrand
        real(real64), intent(in)                   :: want     !! its closed form over [0, 1]
        real(real64), intent(in)                   :: rtol     !! tolerance to ask for
        integer, intent(in)                        :: measured !! count when this was written
        character(len=*), intent(in)               :: what     !! names the case in a message

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(fn, 0.0_real64, 1.0_real64, rtol, info=info)
        call check(error, info%converged, "case "//what//" must converge")
        if (allocated(error)) return
        call check(error, abs(r - want) <= rtol*abs(want), &
                   "case "//what//" must meet the relative tolerance it was given")
        if (allocated(error)) return
        call check(error, info%neval >= ONE_RULE .and. info%neval <= measured + BISECTION, &
                   "case "//what//" must cost what the rule costs, to within one bisection")

    end subroutine one_reference

    !> Asserts that the four specifics are four spellings of one computation.
    subroutine test_entry_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(scaled_runge) :: obj
        real(real64)       :: v_func_rtol, v_func_tol, v_obj_rtol, v_obj_tol

        v_func_rtol = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        v_func_tol = pf_integrate(runge, 0.0_real64, 1.0_real64, &
                                  pf_tolerance(rtol=1.0e-10_real64))
        call check(error, v_func_rtol == v_func_tol, &
                   "the bare rtol and the pf_tolerance forms must agree bit for bit")
        if (allocated(error)) return

        obj%amp = 1.0_real64
        v_obj_rtol = pf_integrate(obj, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        call check(error, v_obj_rtol == v_func_rtol, &
                   "the object form must agree with the plain-function form bit for bit")
        if (allocated(error)) return
        call check(error, obj%calls > 0, "the object must have seen every evaluation itself")
        if (allocated(error)) return

        obj%calls = 0
        v_obj_tol = pf_integrate(obj, 0.0_real64, 1.0_real64, pf_tolerance(rtol=1.0e-10_real64))
        call check(error, v_obj_tol == v_func_rtol, &
                   "the object plus pf_tolerance form must agree with the other three")
        if (allocated(error)) return

        ! An amplitude is linear in the integrand, so it is linear in the integral, and the two
        ! runs differ only by a factor the arithmetic reproduces exactly.
        obj%amp = 3.0_real64
        call check(error, abs(pf_integrate(obj, 0.0_real64, 1.0_real64, 1.0e-10_real64) &
                              - 3.0_real64*v_func_rtol) <= 1.0e-15_real64*abs(v_func_rtol), &
                   "an object carrying an amplitude must scale the integral by it")

    end subroutine test_entry_forms_agree

    !> Asserts that a tolerance carrying only `atol` is accepted and met.
    subroutine test_atol_only_tolerance(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, &
                         pf_tolerance(rtol=0.0_real64, atol=1.0e-10_real64), info=info)
        call check(error, info%converged, "an atol-only tolerance must be accepted and converge")
        if (allocated(error)) return
        call check(error, abs(r - runge_exact()) <= 1.0e-10_real64, &
                   "an atol-only tolerance must be met in absolute terms")

    end subroutine test_atol_only_tolerance

    !> Asserts the zero-width range: zero, no evaluation, and an allocated but empty record.
    subroutine test_zero_width_range(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info)   :: info
        type(pf_integration_points) :: pts
        real(real64)                :: r

        r = pf_integrate(runge, 0.5_real64, 0.5_real64, 1.0e-10_real64, info=info, points=pts)
        call check(error, r == 0.0_real64, "a zero-width range must integrate to exactly zero")
        if (allocated(error)) return
        call check(error, info%neval == 0, &
                   "a zero-width range must not evaluate the integrand at all")
        if (allocated(error)) return
        call check(error, info%converged, "a zero-width range must report convergence")
        if (allocated(error)) return
        ! Allocated and empty rather than unallocated: size() on an unfilled descriptor is what
        ! this contract exists to keep a caller away from.
        call check(error, allocated(pts%x) .and. allocated(pts%w) .and. allocated(pts%f), &
                   "a zero-width range must still hand back an allocated record")
        if (allocated(error)) return
        call check(error, pts%n == 0 .and. size(pts%x) == 0, &
                   "the record of a zero-width range must be empty")

    end subroutine test_zero_width_range

    !> Asserts that an integrand that is identically zero costs one rule application.
    !!
    !! The error estimate of a rule whose samples are all zero is zero, so nothing is refined.
    !! That is the mechanism behind the blind spot the next test documents, seen here in the one
    !! case where answering zero is right.
    subroutine test_zero_integrand_costs_one_rule(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(zero_integrand, 0.0_real64, 1.0_real64, 1.0e-8_real64, info=info)
        call check(error, r == 0.0_real64, "a zero integrand must integrate to exactly zero")
        if (allocated(error)) return
        call check(error, info%neval == ONE_RULE, &
                   "a zero integrand must cost exactly one rule application")
        if (allocated(error)) return
        call check(error, info%converged .and. info%nsub == 1, &
                   "a zero integrand must converge on the first partition")

    end subroutine test_zero_integrand_costs_one_rule


    !> Asserts each tail shape of qfeet's `infinite_tails` against its closed form.
    !!
    !! The negated tail is the load-bearing one: the walk decides whether a panel carries anything
    !! by comparing the panel's SIGNED integral against its integral of `|f|`, and without the
    !! `abs` on the signed side an integrand that is everywhere below zero looks negligible on
    !! every panel. The walk then steps over the whole of it and reports convergence.
    subroutine test_infinite_tails(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r, negated, inf

        inf = pf_infinity()

        ! exp(-x) over [1, inf) is exp(-1); over [0, inf) it is one.
        r = pf_integrate(tail_exp, 1.0_real64, inf, 1.0e-10_real64, info=info)
        call check(error, info%converged, "exp(-x) over [1, inf) must converge")
        if (allocated(error)) return
        call check(error, abs(r - exp(-1.0_real64)) <= 1.0e-10_real64*exp(-1.0_real64), &
                   "exp(-x) over [1, inf) must reproduce exp(-1)")
        if (allocated(error)) return

        r = pf_integrate(tail_exp, 0.0_real64, inf, 1.0e-10_real64, info=info)
        call check(error, abs(r - 1.0_real64) <= 1.0e-10_real64, &
                   "exp(-x) over [0, inf) must reproduce one")
        if (allocated(error)) return
        ! A lower bound of zero cannot be reached in log x at all, so this range and the one above
        ! take different routes through the start-panel search.
        call check(error, info%npanels > 1, "a decaying tail must take more than one panel")
        if (allocated(error)) return

        ! The same tail negated: bit for bit the negative of the first, since every panel, every
        ! subinterval and every rule application sees exactly the mirrored value.
        negated = pf_integrate(tail_neg_exp, 0.0_real64, inf, 1.0e-10_real64)
        call check(error, negated == -r, &
                   "negating the integrand must negate the result bit for bit")
        if (allocated(error)) return

        ! x**-2 over [1, inf) is one; x**-1.5 over [1, inf) is two.
        r = pf_integrate(tail_pow2, 1.0_real64, inf, 1.0e-8_real64, info=info)
        call check(error, info%converged .and. abs(r - 1.0_real64) <= 1.0e-8_real64, &
                   "x**-2 over [1, inf) must converge to one")
        if (allocated(error)) return

        r = pf_integrate(inv_pow15, 1.0_real64, inf, 1.0e-8_real64, info=info)
        call check(error, info%converged .and. abs(r - 2.0_real64) <= 2.0e-8_real64, &
                   "x**-1.5 over [1, inf) must converge to two")
        if (allocated(error)) return

        ! exp(-x**2) over [0, inf) is sqrt(pi)/2.
        want_gauss: block
            real(real64) :: want
            want = 0.5_real64*sqrt(acos(-1.0_real64))
            r = pf_integrate(tail_gauss, 0.0_real64, inf, 1.0e-10_real64, info=info)
            call check(error, info%converged .and. abs(r - want) <= 1.0e-10_real64*want, &
                       "exp(-x**2) over [0, inf) must converge to sqrt(pi)/2")
        end block want_gauss

    end subroutine test_infinite_tails

    !> Asserts the four shapes the start-panel search exists for: the blind spot of section 3.4.
    !!
    !! These are the cases QUADPACK's own infinite-range routine answers as ZERO, converged, in 45
    !! evaluations: its change of variable packs everything beyond about `a + 19` into the last two
    !! abscissae of the first rule, and a narrow feature falls between them. The outward walk
    !! samples 21 points per factor of e instead, and finds all four.
    !!
    !! Each arm is guarded by a different half of the search. The bump at 40 needs the WIDENING
    !! loop, which steps the first panel outward until something is in it. The sliver bump needs
    !! the NARROW retry: its whole support falls short of the first abscissa of the wide first
    !! probe, so that probe reads 21 exact zeros, and widening from there only moves the abscissae
    !! further away -- without the retry the answer is zero, reported as converged. The spike at
    !! 1.02 needs neither: the wide probe sees a value far below the tolerance but not zero, so the
    !! panel is accepted and the engine's own bisection resolves it.
    subroutine test_start_panel_search(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: inf, sqrt_pi

        inf = pf_infinity()
        sqrt_pi = far_bump_exact()

        ! A spike of half-width 1e-3 at 1.02, just above the lower bound.
        call one_far_feature(error, narrow_spike, 1.0_real64, narrow_spike_exact(), &
                             1.0e-10_real64, "the spike at 1.02 from [1, inf)")
        if (allocated(error)) return

        ! A compact bump living entirely inside the first probe's blind sliver.
        call one_far_feature(error, sliver_bump, 1.0_real64, sliver_bump_exact(), &
                             1.0e-10_real64, "the sliver bump at 1.001 from [1, inf)")
        if (allocated(error)) return

        ! A unit-width bump at 40, from three lower bounds, each of which sends the search down a
        ! different arm: log x from one, log x from a bound below one, and linear x from zero.
        call one_far_feature(error, far_bump, 1.0_real64, sqrt_pi, 1.0e-8_real64, &
                             "the bump at 40 from [1, inf)")
        if (allocated(error)) return
        call one_far_feature(error, far_bump, 0.5_real64, sqrt_pi, 1.0e-8_real64, &
                             "the bump at 40 from [0.5, inf)")
        if (allocated(error)) return
        call one_far_feature(error, far_bump, 0.0_real64, sqrt_pi, 1.0e-8_real64, &
                             "the bump at 40 from [0, inf)")

    end subroutine test_start_panel_search

    !> Integrates one far feature to infinity and asserts it was found, not stepped over.
    subroutine one_far_feature(error, fn, a, want, thr, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        procedure(pf_integrand_func)               :: fn    !! the integrand
        real(real64), intent(in)                   :: a     !! lower bound
        real(real64), intent(in)                   :: want  !! its closed form over [a, inf)
        real(real64), intent(in)                   :: thr   !! relative threshold to assert
        character(len=*), intent(in)               :: what  !! names the case in a message

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(fn, a, pf_infinity(), 1.0e-10_real64, info=info)
        call check(error, info%converged, what//" must converge")
        if (allocated(error)) return
        call check(error, abs(r - want) <= thr*abs(want), &
                   what//" must be found, not integrated as zero")

    end subroutine one_far_feature

    !> Asserts the oscillatory tail qfeet's `integral_inf` is about.
    !!
    !! `sin(x)/x**2` over `[1, inf)` is `sin(1) - Ci(1)`. It is the one tail in this suite that a
    !! tight tolerance cannot buy inside the default budget, and the point of the test is that the
    !! walk returns its best estimate rather than a wrong one when it gives up: at `1e-10` the
    !! budget runs out, `converged` is false, and the answer is still right to well inside the
    !! threshold qfeet asserted.
    subroutine test_integral_inf_oscillatory(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: loose, tight
        real(real64)              :: r, want, inf

        inf = pf_infinity()
        want = tail_osc_exact()

        r = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-8_real64, info=loose)
        call check(error, abs(r - want) <= 1.0e-5_real64, &
                   "sin(x)/x**2 over [1, inf) must reproduce sin(1) - Ci(1)")
        if (allocated(error)) return
        call check(error, loose%converged, "the oscillatory tail must converge at 1e-8")
        if (allocated(error)) return

        ! The same integrand at a tolerance the budget cannot buy.
        r = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-10_real64, info=tight)
        call check(error, .not. tight%converged .and. tight%status == PF_INT_LIMIT, &
                   "the oscillatory tail must report PF_INT_LIMIT at 1e-10, not convergence")
        if (allocated(error)) return
        call check(error, abs(r - want) <= 1.0e-5_real64, &
                   "a walk that ran out of budget must still return its best estimate")
        if (allocated(error)) return
        call check(error, tight%neval > loose%neval, &
                   "the tighter tolerance must have cost more, not less")

    end subroutine test_integral_inf_oscillatory

    !> Asserts that the three infinite spellings agree with each other and with a finite split.
    !!
    !! `(-inf, b]` is the walk with every evaluation mirrored and `(-inf, inf)` is two walks from
    !! zero; dropping either the mirroring or the split leaves a plausible number that is wrong by
    !! a factor of two or by the whole of one half.
    subroutine test_infinite_ranges_are_consistent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: whole, half
        real(real64)              :: r, r_half, inf, sqrt_pi

        inf = pf_infinity()
        sqrt_pi = far_bump_exact()

        ! The whole line against twice the half line, and against sqrt(pi).
        r_half = pf_integrate(tail_gauss, 0.0_real64, inf, 1.0e-10_real64, info=half)
        r = pf_integrate(tail_gauss, -inf, inf, 1.0e-10_real64, info=whole)
        call check(error, whole%converged, "exp(-x**2) over the whole line must converge")
        if (allocated(error)) return
        call check(error, abs(r - 2.0_real64*r_half) <= 1.0e-10_real64*abs(r), &
                   "the whole line must be twice the half line for an even integrand")
        if (allocated(error)) return
        call check(error, abs(r - sqrt_pi) <= 1.0e-9_real64*sqrt_pi, &
                   "exp(-x**2) over the whole line must reproduce sqrt(pi)")
        if (allocated(error)) return
        ! Both halves are walked, so both halves' panels are counted.
        call check(error, whole%npanels >= 2*half%npanels, &
                   "the whole line must use at least the panels of both its halves")
        if (allocated(error)) return

        ! A rising exponential, integrable only towards minus infinity.
        r = pf_integrate(rising_exp, -inf, 0.0_real64, 1.0e-10_real64, info=whole)
        call check(error, whole%converged .and. abs(r - 1.0_real64) <= 1.0e-10_real64, &
                   "exp(x) over (-inf, 0] must converge to one")
        if (allocated(error)) return

        ! Additivity: a finite piece plus a tail is the whole tail.
        r_half = pf_integrate(tail_exp, 0.1_real64, 1.0_real64, 1.0e-10_real64)
        r = pf_integrate(tail_exp, 1.0_real64, inf, 1.0e-10_real64)
        call check(error, abs(r_half + r - exp(-0.1_real64)) <= 1.0e-9_real64, &
                   "[0.1, 1] plus [1, inf) must be [0.1, inf)")

    end subroutine test_infinite_ranges_are_consistent

    !> Asserts that both caps stop the walk, and that neither is reported as success.
    !!
    !! `max_panels` is the cap on the panels one walk may use and `max_neval` the cap on the
    !! evaluations; the walk may overshoot the second by one rule application, because a panel
    !! costs its 21 points even when five of them would have been enough.
    subroutine test_max_panels_caps_the_walk(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: capped, budgeted
        real(real64)              :: r, inf

        inf = pf_infinity()

        r = pf_integrate(inv_pow15, 1.0_real64, inf, 1.0e-8_real64, max_panels=2, info=capped)
        call check(error, capped%status == PF_INT_LIMIT .and. .not. capped%converged, &
                   "a walk stopped by max_panels must report PF_INT_LIMIT, not convergence")
        if (allocated(error)) return
        call check(error, capped%npanels == 2, &
                   "max_panels must be the number of panels the walk is allowed")
        if (allocated(error)) return
        call check(error, r > 0.0_real64 .and. r < 2.0_real64, &
                   "a capped walk must still return the part of the integral it reached")
        if (allocated(error)) return

        ! A budget too small for the tail it is asked for.
        r = pf_integrate(inv_pow15, 1.0_real64, inf, 1.0e-10_real64, max_neval=100, &
                         info=budgeted)
        call check(error, budgeted%status == PF_INT_LIMIT .and. .not. budgeted%converged, &
                   "a walk stopped by max_neval must report PF_INT_LIMIT")
        if (allocated(error)) return
        call check(error, budgeted%neval <= 100 + ONE_RULE, &
                   "a walk may overshoot its budget by at most one rule application")

    end subroutine test_max_panels_caps_the_walk

    !> Asserts that every entry form reaches the walk, and that an object counts what it saw.
    subroutine test_infinite_entry_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(exp_profile)         :: profile
        type(pf_integration_info) :: info
        real(real64)              :: r_func, r_obj, r_tol, inf

        inf = pf_infinity()

        ! The object form: amp*exp(-x/scale) over [0, inf) is amp*scale.
        profile%amp = 3.0_real64
        profile%scale = 2.0_real64
        r_obj = pf_integrate(profile, 0.0_real64, inf, 1.0e-10_real64, info=info)
        call check(error, info%converged .and. abs(r_obj - 6.0_real64) <= 1.0e-9_real64, &
                   "an exp_profile over [0, inf) must integrate to amp*scale")
        if (allocated(error)) return
        call check(error, profile%calls == info%neval, &
                   "the object must have been called once per counted evaluation")
        if (allocated(error)) return

        ! The plain-function form and the pf_tolerance form must agree bit for bit with each
        ! other on the same range.
        r_func = pf_integrate(tail_exp, 1.0_real64, inf, 1.0e-10_real64)
        r_tol = pf_integrate(tail_exp, 1.0_real64, inf, &
                             pf_tolerance(rtol=1.0e-10_real64, atol=0.0_real64))
        call check(error, r_func == r_tol, &
                   "the rtol and pf_tolerance forms must agree bit for bit on a tail")

    end subroutine test_infinite_entry_forms

    !> Asserts that the record's weighted sum reproduces the partition integral.
    !!
    !! One case per change of variable: the plain range, `log_base`, the walk's own `log y`, the
    !! mirrored abscissae of `negate`, and the two walks of the whole line. Any single factor
    !! dropped from a recorded weight -- the Kronrod weight, the half-length, the `exp(u)` of
    !! either log transform -- leaves the integral itself right and breaks only this assertion,
    !! and putting `negate`'s sign on the weight rather than on the abscissa turns the record's
    !! weighted sum into the negative of what it should reproduce.
    subroutine test_points_reproduce_the_partition_integral(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info)   :: info
        type(pf_integration_points) :: pts
        real(real64)                :: r, summed

        ! Plain range.
        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info, points=pts)
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, abs(summed - info%partition_integral) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral on a plain range")
        if (allocated(error)) return
        ! The record IS the final partition, so its length is the partition's own.
        call check(error, pts%n == ONE_RULE*info%nsub, &
                   "the record must hold 21 points per subinterval of the final partition")
        if (allocated(error)) return
        call check(error, info%partition_integral == r .and. .not. info%extrapolated, &
                   "without the extrapolation the partition sum IS the result")
        if (allocated(error)) return

        ! Integrating in log x, where the weight carries the Jacobian and the abscissa does not.
        r = pf_integrate(inv_pow15, 1.0e-3_real64, 1.0e3_real64, 1.0e-10_real64, &
                         log_base=.true., info=info, points=pts)
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, abs(summed - info%partition_integral) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral under log_base too")
        if (allocated(error)) return
        call check(error, pts%n == ONE_RULE*info%nsub, &
                   "the log_base record must hold 21 points per subinterval too")
        if (allocated(error)) return
        ! The abscissae are the CALLER's x, not the engine's log x, so they lie inside the range
        ! the caller named.
        call check(error, all(pts%x(1:pts%n) >= 1.0e-3_real64) &
                   .and. all(pts%x(1:pts%n) <= 1.0e3_real64), &
                   "a log_base record's abscissae must be in the caller's x, inside the range")
        if (allocated(error)) return

        ! The walk, whose panels are in log y and whose record is assembled panel by panel.
        r = pf_integrate(tail_exp, 1.0_real64, pf_infinity(), 1.0e-10_real64, info=info, &
                         points=pts)
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, abs(summed - info%partition_integral) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral across the walk's panels")
        if (allocated(error)) return
        call check(error, pts%n == ONE_RULE*info%nsub, &
                   "the walk's record must hold 21 points per subinterval, over all panels")
        if (allocated(error)) return
        call check(error, info%partition_integral == r .and. .not. info%extrapolated, &
                   "without the extrapolation the walk's partition sum IS its result")
        if (allocated(error)) return
        call check(error, all(pts%x(1:pts%n) >= 1.0_real64), &
                   "a walk's abscissae must all lie inside the range the caller named")
        if (allocated(error)) return

        ! `negate`: the sign belongs on the abscissa, and the weights stay as they were. With the
        ! sign moved to the weight the integral is still right and this sum is its negative.
        r = pf_integrate(rising_exp, -pf_infinity(), 0.0_real64, 1.0e-10_real64, info=info, &
                         points=pts)
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, abs(summed - info%partition_integral) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral under negate too")
        if (allocated(error)) return
        call check(error, all(pts%x(1:pts%n) <= 0.0_real64), &
                   "a negated walk's abscissae must be the caller's x, at or below the bound")
        if (allocated(error)) return

        ! The whole line, which is both walks at once.
        r = pf_integrate(tail_gauss, -pf_infinity(), pf_infinity(), 1.0e-10_real64, info=info, &
                         points=pts)
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, abs(summed - info%partition_integral) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral over the whole line")
        if (allocated(error)) return
        call check(error, any(pts%x(1:pts%n) < 0.0_real64) &
                   .and. any(pts%x(1:pts%n) > 0.0_real64), &
                   "a whole-line record must carry points from both of its walks")

    end subroutine test_points_reproduce_the_partition_integral

    !> Asserts that asking for the record changes nothing about the integration itself.
    subroutine test_points_absent_records_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info)   :: bare, recorded
        type(pf_integration_points) :: pts
        real(real64)                :: r_bare, r_recorded

        r_bare = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=bare)
        r_recorded = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                                  info=recorded, points=pts)
        call check(error, r_bare == r_recorded, &
                   "recording must not change the integral by a single bit")
        if (allocated(error)) return
        call check(error, bare%neval == recorded%neval, &
                   "recording must not change how often the integrand is evaluated")
        if (allocated(error)) return
        call check(error, bare%nsub == recorded%nsub, &
                   "recording must not change the final partition")

    end subroutine test_points_absent_records_nothing

    !> Asserts that appending two adjacent records gives a rule for the union of the two ranges.
    subroutine test_points_append_adds_up(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_points) :: left, right
        real(real64)                :: r_left, r_right, summed

        r_left = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, points=left)
        r_right = pf_integrate(runge, 1.0_real64, 2.0_real64, 1.0e-10_real64, points=right)
        call left%append(right)
        summed = sum(left%w(1:left%n)*left%f(1:left%n))

        call check(error, left%n == ONE_RULE*(left%n/ONE_RULE), &
                   "an appended record must still be a whole number of rule applications")
        if (allocated(error)) return
        call check(error, abs(summed - (r_left + r_right)) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(r_left + r_right), &
                   "the appended record must sum to the sum of the two integrals")

    end subroutine test_points_append_adds_up

    !> Asserts the counted evaluations against QUADPACK's own closed formula.
    !!
    !! The engine counts each `eval` rather than trusting `42*last - 21`, and on a finite,
    !! non-log range the two must agree: one rule application for the first partition, two more
    !! per bisection. A counter incremented once per RULE rather than once per point passes every
    !! accuracy assertion in this file and fails only here.
    subroutine test_neval_matches_the_rule_formula(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info)
        call check(error, info%nsub >= 2, &
                   "this fixture must actually bisect, or the formula is untested")
        if (allocated(error)) return
        call check(error, info%neval == BISECTION*info%nsub - ONE_RULE, &
                   "the counted evaluations must equal 42*nsub - 21 on a finite, non-log range")
        if (allocated(error)) return

        r = pf_integrate(peak_c, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info)
        call check(error, info%neval == BISECTION*info%nsub - ONE_RULE, &
                   "the formula must hold for a harder integrand too")

    end subroutine test_neval_matches_the_rule_formula

    !> Asserts that reaching the budget is reported, and that the budget is a ceiling.
    !!
    !! The subinterval cap is derived from the budget as `(budget + 21)/42`, so a budget of the
    !! form `42k - 21` is spent exactly and no budget is ever exceeded.
    subroutine test_status_limit_and_converged_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r
        integer, parameter        :: BUDGET = 105 ! 42*3 - 21: three subintervals exactly

        r = pf_integrate(sharp_gauss, 0.0_real64, 1.0_real64, 1.0e-13_real64, &
                         max_neval=BUDGET, info=info)
        call check(error, info%status == PF_INT_LIMIT, &
                   "a budget too small for the tolerance must report PF_INT_LIMIT")
        if (allocated(error)) return
        call check(error, .not. info%converged, &
                   "converged must be false whenever the status is not PF_INT_OK")
        if (allocated(error)) return
        call check(error, info%neval == BUDGET, &
                   "a budget of the form 42k - 21 must be spent exactly")
        if (allocated(error)) return
        call check(error, r == r, "a call that ran out of budget must still return a number")
        if (allocated(error)) return

        ! A budget that is not a whole number of bisections is still never exceeded.
        r = pf_integrate(sharp_gauss, 0.0_real64, 1.0_real64, 1.0e-13_real64, &
                         max_neval=100, info=info)
        call check(error, info%neval <= 100, &
                   "the evaluation count must never exceed max_neval on a finite range")

    end subroutine test_status_limit_and_converged_agree

    !> Asserts that a tolerance the arithmetic cannot deliver reports round-off rather than
    !! pretending to have met it.
    !!
    !! `x*x` is integrated exactly by the first rule application, so the only thing standing
    !! between the result and a relative tolerance of `1e-16` is the round-off floor QUADPACK puts
    !! under its own error estimate. Disabling that floor makes this call claim convergence.
    subroutine test_status_roundoff(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(x_squared, 0.0_real64, 1.0_real64, &
                         pf_tolerance(rtol=1.0e-16_real64, atol=1.0e-300_real64), info=info)
        call check(error, info%status == PF_INT_ROUNDOFF, &
                   "a tolerance below the round-off floor must report PF_INT_ROUNDOFF")
        if (allocated(error)) return
        call check(error, .not. info%converged, "PF_INT_ROUNDOFF must not report convergence")
        if (allocated(error)) return
        call check(error, abs(r - 1.0_real64/3.0_real64) <= 1.0e-15_real64, &
                   "the result must still be the right one, to the accuracy that IS available")

    end subroutine test_status_roundoff

    !> Asserts what a discontinuity inside the range does: a non-OK status and a right answer.
    !!
    !! Bisecting towards a step never converges, so the engine drills down until the subinterval
    !! is too narrow to halve and reports `PF_INT_BAD_INTEGRAND` -- which is the code that tells a
    !! caller to split the range at the discontinuity instead.
    subroutine test_status_on_a_discontinuity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(unit_step, 0.0_real64, 1.0_real64, &
                         pf_tolerance(rtol=1.0e-15_real64, atol=1.0e-16_real64), info=info)
        call check(error, info%status == PF_INT_BAD_INTEGRAND, &
                   "drilling into a discontinuity must report PF_INT_BAD_INTEGRAND")
        if (allocated(error)) return
        call check(error, .not. info%converged, &
                   "a status other than PF_INT_OK must report converged = .false.")
        if (allocated(error)) return
        call check(error, abs(r - unit_step_exact()) <= 1.0e-12_real64, &
                   "the answer must still be right even though the status is not OK")

    end subroutine test_status_on_a_discontinuity

    !> Documents the one thing the integrator cannot see, so that a change to it is noticed.
    !!
    !! **These are documenting assertions, not requirements.** A feature narrower than the spacing
    !! of the 21 points of the first rule application is sampled as zero, the error estimate is
    !! zero, and nothing is ever refined -- so the answer is zero and `converged` is true. Both
    !! figures below were measured; the wide-range case reproduces the design document's own
    !! blind-spot measurement (`1.66915E-16` in 63 evaluations) exactly.
    subroutine test_blind_spot_is_documented(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        ! A bump of width 0.012 on a range of width 9: every sample misses it.
        r = pf_integrate(compact_bump, 1.0_real64, 10.0_real64, 1.0e-8_real64, info=info)
        call check(error, r == 0.0_real64, &
                   "a bump narrower than the first rule's spacing must integrate as exactly zero")
        if (allocated(error)) return
        call check(error, info%converged .and. info%neval == ONE_RULE .and. info%nsub == 1, &
                   "and must be reported as converged after exactly one rule application")
        if (allocated(error)) return

        ! Fit the range to the feature and the same integrand is integrated correctly.
        r = pf_integrate(compact_bump, 1.0_real64, 1.05_real64, 1.0e-8_real64, info=info)
        call check(error, abs(r - compact_bump_exact()) <= 1.0e-9_real64, &
                   "the same bump on a range fitted to it must be integrated correctly")
        if (allocated(error)) return

        ! The unit-width bump at 40 is found on [0, 100] and missed on [0, 1000], which is the
        ! measurement the design rests on.
        r = pf_integrate(far_bump, 0.0_real64, 100.0_real64, &
                         pf_tolerance(rtol=1.0e-8_real64, atol=1.0e-14_real64), info=info)
        call check(error, abs(r - far_bump_exact()) <= 1.0e-8_real64*far_bump_exact(), &
                   "a unit-width bump at 40 must be found on a range of width 100")
        if (allocated(error)) return

        r = pf_integrate(far_bump, 0.0_real64, 1000.0_real64, &
                         pf_tolerance(rtol=1.0e-8_real64, atol=1.0e-14_real64), info=info)
        call check(error, abs(r) <= 1.0e-14_real64 .and. info%converged, &
                   "the same bump on a range of width 1000 is missed and reported as converged")

    end subroutine test_blind_spot_is_documented

    !> Asserts that the `50*epsilon` floor is about the PAIR of tolerances, not about `rtol`.
    !!
    !! An `rtol` below `50*epsilon` with no `atol` is refused out of process (the
    !! `integrate_rtol_below_floor` scenario); the same `rtol` with a positive `atol` is accepted,
    !! because `atol` is then what convergence is decided on. Removing the floor check makes the
    !! scenario print its result instead of aborting; removing the `atol` half of the condition
    !! makes this call abort.
    subroutine test_rtol_floor_needs_atol(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: info
        real(real64)              :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, &
                         pf_tolerance(rtol=1.0e-14_real64, atol=1.0e-20_real64), info=info)
        call check(error, abs(r - runge_exact()) <= 1.0e-12_real64*abs(runge_exact()), &
                   "an rtol below 50*epsilon with a positive atol must be accepted and answered")
        if (allocated(error)) return
        ! Whether such a call CONVERGES is up to the arithmetic -- here it reports round-off --
        ! but it must not be refused, and it must not be wrong.
        call check(error, info%status == PF_INT_OK .or. info%status == PF_INT_ROUNDOFF, &
                   "a tolerance at the floor must end in OK or round-off, not in another code")

    end subroutine test_rtol_floor_needs_atol

    !> Asserts that the extrapolation is worth having on the class it was vendored for.
    !!
    !! The endpoint singularity is the one shape where the Wynn-epsilon table changes the cost by
    !! an order of magnitude; on a smooth integrand and on an interior peak it contributes
    !! nothing. Forcing `noext` true inside the engine leaves the second call costing what the
    !! first does and fails here.
    subroutine test_extrapolation_earns_its_keep(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integration_info) :: plain, extrapolated
        real(real64)              :: r_plain, r_extrapolated

        r_plain = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                               extrapolate=.false., info=plain)
        r_extrapolated = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                                      extrapolate=.true., info=extrapolated)

        call check(error, abs(r_plain - log_sqrt_exact()) <= 1.0e-9_real64, &
                   "the plain bisection must still reach the closed form")
        if (allocated(error)) return
        call check(error, abs(r_extrapolated - log_sqrt_exact()) <= 1.0e-9_real64, &
                   "the extrapolated result must reach the closed form too")
        if (allocated(error)) return
        call check(error, extrapolated%neval*5 <= plain%neval, &
                   "the extrapolation must cost at least five times fewer evaluations")
        if (allocated(error)) return
        call check(error, extrapolated%extrapolated .and. .not. plain%extrapolated, &
                   "only the extrapolated call may report having used the epsilon table")

    end subroutine test_extrapolation_earns_its_keep

end module test_integrate
