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
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_support_flag, &
        ieee_invalid, ieee_underflow

    implicit none
    private

    public :: collect_tests_integrate

    !> Evaluations one bisection costs; the width of every count window asserted here.
    integer, parameter :: BISECTION = 42
    !> Evaluations one rule application costs.
    integer, parameter :: ONE_RULE = 21
    !> `max_neval`'s default, as the module documents it; a walk may overshoot it by one rule.
    integer, parameter :: DEFAULT_BUDGET = 100000
    !> `max_panels`'s default, as the module documents it.
    integer, parameter :: DEFAULT_PANELS = 100

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
            new_unittest("both specifics agree bit for bit and the object counts its calls", &
                         test_entry_forms_agree), &
            new_unittest("an atol-only tolerance converges where a relative one cannot", &
                         test_atol_only_tolerance), &
            new_unittest("an atol-only call converges on an integral that is zero", &
                         test_atol_only_on_a_zero_integral), &
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
            new_unittest("the epsilon table raises no underflow on an ordinary integral", &
                         test_extrapolation_raises_no_underflow), &
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
                         test_infinite_entry_forms), &
            new_unittest("breakpoints find a far feature the unbroken call reports as zero", &
                         test_breakpoints_find_the_far_bump), &
            new_unittest("cutting a smooth range changes neither the answer nor the record", &
                         test_breakpoints_agree_with_the_whole), &
            new_unittest("a breakpoint on an infinite range leaves the walk to the tail piece", &
                         test_breakpoints_on_infinite_ranges), &
            new_unittest("the pieces share the caller's atol instead of each taking all of it", &
                         test_breakpoints_share_atol), &
            new_unittest("an endpoint singularity costs the same at every tolerance", &
                         test_endpoint_singularities_are_cheap), &
            new_unittest("a divergent integral is reported as divergent, not integrated", &
                         test_status_divergent), &
            new_unittest("a non-finite integrand value is reported, not aborted on", &
                         test_non_finite_value_is_reported), &
            new_unittest("a walk and a piece list both stop at a non-finite value", &
                         test_non_finite_value_stops_every_path), &
            new_unittest("the default panel cap clears an algebraic tail at the tightest tolerance", &
                         test_default_panel_cap_clears_an_algebraic_tail), &
            new_unittest("the guide's overshoot bounds hold, and max_neval binds", &
                         test_guide_budget_overshoot_bounds) &
            ]

    end subroutine collect_tests_integrate

    !> Asserts the closed forms an elementary antiderivative gives, over a finite range.
    subroutine test_finite_closed_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64) :: r, want
        logical :: uf_ok, uf_was

        ! sin over a range covering three half-periods: the antiderivative is -cos.
        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            want = cos(0.1_real64) - cos(10.0_real64)
            r = pf_integrate(sine, 0.1_real64, 10.0_real64, 1.0e-12_real64)
            call check(error, abs(r - want) <= 1.0e-12_real64*abs(want), &
                       "sin over [0.1, 10] must reproduce cos(0.1) - cos(10)")
            if (allocated(error)) exit run

            ! x*x over the unit interval: the rule is exact to degree 31, so this is exact.
            r = pf_integrate(x_squared, 0.0_real64, 1.0_real64, 1.0e-10_real64)
            call check(error, abs(r - 1.0_real64/3.0_real64) <= 1.0e-15_real64, &
                       "x*x over [0, 1] must be 1/3 to rounding")
            if (allocated(error)) exit run

            ! Runge's function: a rational whose antiderivative is an arctangent.
            r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64)
            call check(error, abs(r - runge_exact()) <= 1.0e-10_real64*abs(runge_exact()), &
                       "Runge's function over [0, 1] must be atan(5)/5")
            if (allocated(error)) exit run

            ! A gaussian narrow enough to need refinement but wide enough to be found.
            r = pf_integrate(sharp_gauss, 0.0_real64, 1.0_real64, 1.0e-10_real64)
            call check(error, abs(r - sharp_gauss_exact()) <= 1.0e-10_real64*abs(sharp_gauss_exact()), &
                       "a gaussian of width 0.01 at 0.9 must reproduce its error-function form")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_finite_closed_forms

    !> Asserts that `log_base` turns six decades of a power law into one rule application.
    !!
    !! `x**-1.5` is a straight line in `log x` against `log f`, so the transformed integrand is a
    !! pure exponential the 21-point rule resolves at once. The same call without `log_base` needs
    !! tens of subintervals, which is the whole reason the argument exists.
    subroutine test_log_base_over_decades(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: with_log, without_log
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
        ! B's count is upstream `dqags`' own 315, not the 1995 the bisection alone pays: the
        ! extrapolation is on by default and this is the integrand it exists for.
        call one_reference(error, log_sqrt, log_sqrt_exact(), RTOL, 315, "B, log(x)/sqrt(x)")
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
        if (allocated(error)) return

        ! B down the whole ladder. It is the one row of the six whose cost the tolerance moves at
        ! all under the plain bisection -- 1995, 2583, 3129, 3717 -- and the one row the table
        ! flattens: the same 315 at every tolerance, and the same answer, because the table stops
        ! when the accelerated sequence stops moving rather than when the tolerance is met.
        call b_down_the_ladder(error, 1.0e-8_real64)
        if (allocated(error)) return
        call b_down_the_ladder(error, 1.0e-10_real64)
        if (allocated(error)) return
        call b_down_the_ladder(error, 1.0e-12_real64)

    end subroutine test_six_reference_integrands

    !> Asserts case B's accuracy and cost at one tolerance of the ladder.
    subroutine b_down_the_ladder(error, rtol)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        real(real64), intent(in)                   :: rtol  !! tolerance to ask for

        type(pf_integrate_info) :: info
        real(real64)              :: r
        character(len=16)         :: at

        write (at, '(es9.1)') rtol
        r = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, rtol, info=info)
        call check(error, info%converged, "case B must converge at rtol "//trim(at))
        if (allocated(error)) return
        call check(error, abs(r - log_sqrt_exact()) <= rtol*abs(log_sqrt_exact()), &
                   "case B must meet the tolerance it was given at rtol "//trim(at))
        if (allocated(error)) return
        call check(error, info%neval <= 315 + BISECTION, &
                   "case B must still cost what dqags costs at rtol "//trim(at))

    end subroutine b_down_the_ladder

    !> Integrates one reference integrand over `[a, inf)` and asserts its accuracy and its cost.
    subroutine one_reference_inf(error, fn, a, want, rtol, measured, what)
        type(error_type), allocatable, intent(out) :: error    !! test-drive's error handle.
        procedure(pf_integrand_func)               :: fn       !! the integrand
        real(real64), intent(in)                   :: a        !! lower bound
        real(real64), intent(in)                   :: want     !! its closed form over [a, inf)
        real(real64), intent(in)                   :: rtol     !! tolerance to ask for
        integer, intent(in)                        :: measured !! count when this was written
        character(len=*), intent(in)               :: what     !! names the case in a message

        type(pf_integrate_info) :: info
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

        type(pf_integrate_info) :: info
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

    !> Asserts that the two specifics are two spellings of one computation, and that omitting
    !! `atol` is the same call as passing its default of zero.
    subroutine test_entry_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(scaled_runge) :: obj
        real(real64)       :: v_func, v_func_atol, v_obj, v_obj_atol

        v_func = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        v_func_atol = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                                   atol=0.0_real64)
        call check(error, v_func == v_func_atol, &
                   "an absent atol must be the same call as atol=0, bit for bit")
        if (allocated(error)) return

        obj%amp = 1.0_real64
        v_obj = pf_integrate(obj, 0.0_real64, 1.0_real64, 1.0e-10_real64)
        call check(error, v_obj == v_func, &
                   "the object form must agree with the plain-function form bit for bit")
        if (allocated(error)) return
        call check(error, obj%calls > 0, "the object must have seen every evaluation itself")
        if (allocated(error)) return

        obj%calls = 0
        v_obj_atol = pf_integrate(obj, 0.0_real64, 1.0_real64, rtol=1.0e-10_real64, &
                                  atol=0.0_real64)
        call check(error, v_obj_atol == v_func, &
                   "the object form with both tolerances by keyword must agree with the rest")
        if (allocated(error)) return

        ! An amplitude is linear in the integrand, so it is linear in the integral, and the two
        ! runs differ only by a factor the arithmetic reproduces exactly.
        obj%amp = 3.0_real64
        call check(error, abs(pf_integrate(obj, 0.0_real64, 1.0_real64, 1.0e-10_real64) &
                              - 3.0_real64*v_func) <= 1.0e-15_real64*abs(v_func), &
                   "an object carrying an amplitude must scale the integral by it")

    end subroutine test_entry_forms_agree

    !> Asserts that a tolerance carrying only `atol` is accepted and met.
    subroutine test_atol_only_tolerance(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info
        real(real64)              :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, &
                         rtol=0.0_real64, atol=1.0e-10_real64, info=info)
        call check(error, info%converged, "an atol-only tolerance must be accepted and converge")
        if (allocated(error)) return
        call check(error, abs(r - runge_exact()) <= 1.0e-10_real64, &
                   "an atol-only tolerance must be met in absolute terms")

    end subroutine test_atol_only_tolerance

    !> Asserts that an `atol`-only call converges on an integral whose true value is exactly zero.
    !!
    !! **This is the case `rtol` alone cannot state.** `sin` is odd, so its integral over a range
    !! symmetric about the origin is exactly zero, and the convergence test
    !! `abserr <= max(atol, rtol*abs(result))` then reduces to `abserr <= atol`: with `atol = 0`
    !! there is no tolerance any arithmetic could meet, and the run would spend its budget. So the
    !! assertion is that `atol` alone both reaches the engine and decides the outcome.
    !!
    !! Mutation proved: make the engine receive `atol = 0` -- drop the `if (present(atol))` in
    !! `tolerance_of` (`src/parquet_integrate_driver.f90`) -- and this test fails, because the
    !! call is then refused for having no positive tolerance at all.
    subroutine test_atol_only_on_a_zero_integral(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        real(real64), parameter :: PI = acos(-1.0_real64) !! the half-turn, for a symmetric range

        type(pf_integrate_info) :: info
        real(real64)              :: r

        r = pf_integrate(sine, -PI, PI, rtol=0.0_real64, atol=1.0e-10_real64, info=info)
        call check(error, info%converged, &
                   "an atol-only call must converge on an integral that is zero")
        if (allocated(error)) return
        call check(error, info%status == PF_INT_OK, &
                   "an atol-only call on a zero integral must report PF_INT_OK")
        if (allocated(error)) return
        call check(error, abs(r) <= 1.0e-10_real64, &
                   "the integral of an odd function over a symmetric range must be zero to atol")

    end subroutine test_atol_only_on_a_zero_integral

    !> Asserts the zero-width range: zero, no evaluation, and an allocated but empty record.
    subroutine test_zero_width_range(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info)   :: info
        type(pf_integrate_points) :: pts
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

        type(pf_integrate_info) :: info
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

        type(pf_integrate_info) :: info
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
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            inf = pf_infinity()
            sqrt_pi = far_bump_exact()

            ! A spike of half-width 1e-3 at 1.02, just above the lower bound.
            call one_far_feature(error, narrow_spike, 1.0_real64, narrow_spike_exact(), &
                                 1.0e-10_real64, "the spike at 1.02 from [1, inf)")
            if (allocated(error)) exit run

            ! A compact bump living entirely inside the first probe's blind sliver.
            call one_far_feature(error, sliver_bump, 1.0_real64, sliver_bump_exact(), &
                                 1.0e-10_real64, "the sliver bump at 1.001 from [1, inf)")
            if (allocated(error)) exit run

            ! A unit-width bump at 40, from three lower bounds, each of which sends the search down a
            ! different arm: log x from one, log x from a bound below one, and linear x from zero.
            call one_far_feature(error, far_bump, 1.0_real64, sqrt_pi, 1.0e-8_real64, &
                                 "the bump at 40 from [1, inf)")
            if (allocated(error)) exit run
            call one_far_feature(error, far_bump, 0.5_real64, sqrt_pi, 1.0e-8_real64, &
                                 "the bump at 40 from [0.5, inf)")
            if (allocated(error)) exit run
            call one_far_feature(error, far_bump, 0.0_real64, sqrt_pi, 1.0e-8_real64, &
                                 "the bump at 40 from [0, inf)")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_start_panel_search

    !> Integrates one far feature to infinity and asserts it was found, not stepped over.
    subroutine one_far_feature(error, fn, a, want, thr, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        procedure(pf_integrand_func)               :: fn    !! the integrand
        real(real64), intent(in)                   :: a     !! lower bound
        real(real64), intent(in)                   :: want  !! its closed form over [a, inf)
        real(real64), intent(in)                   :: thr   !! relative threshold to assert
        character(len=*), intent(in)               :: what  !! names the case in a message

        type(pf_integrate_info) :: info
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

        type(pf_integrate_info) :: loose, tight, plain
        real(real64)              :: r, r_plain, want, inf

        inf = pf_infinity()
        want = tail_osc_exact()

        r = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-8_real64, info=loose)
        call check(error, abs(r - want) <= 1.0e-5_real64, &
                   "sin(x)/x**2 over [1, inf) must reproduce sin(1) - Ci(1)")
        if (allocated(error)) return
        call check(error, loose%converged, "the oscillatory tail must converge at 1e-8")
        if (allocated(error)) return
        ! A converging walk reaches its answer through the partition, not the table: the
        ! extrapolation is on here and must have changed neither the answer nor the count.
        r_plain = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-8_real64, info=plain, &
                               extrapolate=.false.)
        call check(error, r_plain == r .and. plain%neval == loose%neval &
                   .and. .not. loose%extrapolated, &
                   "the extrapolation must not touch a tail the bisection already converged on")
        if (allocated(error)) return

        ! The same integrand at a tolerance the budget cannot buy.
        r = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-10_real64, info=tight)
        call check(error, .not. tight%converged, &
                   "the oscillatory tail must not report convergence at 1e-10")
        if (allocated(error)) return
        call check(error, tight%neval >= DEFAULT_BUDGET, &
                   "it must not report anything but convergence without having spent the budget")
        if (allocated(error)) return
        ! The code differs between the two paths and both are right about what they saw. The
        ! bisection ran out of evaluations: `PF_INT_LIMIT`. The extrapolation, handed the partial
        ! sums of an integrand that keeps changing sign, reports QUADPACK's own verdict for a
        ! series that will not settle: `PF_INT_DIVERGENT`, whose text is "probably divergent, or
        ! SLOWLY CONVERGENT" -- and a conditionally convergent oscillatory tail is exactly that.
        call check(error, tight%status == PF_INT_DIVERGENT, &
                   "an oscillatory tail the budget cannot buy is the extrapolation's slow case")
        if (allocated(error)) return
        r_plain = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-10_real64, info=plain, &
                               extrapolate=.false.)
        call check(error, plain%status == PF_INT_LIMIT .and. .not. plain%converged, &
                   "without the table the same call is simply out of budget: PF_INT_LIMIT")
        if (allocated(error)) return
        call check(error, abs(r - want) <= 1.0e-5_real64 .and. abs(r_plain - want) <= 1.0e-5_real64, &
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

        type(pf_integrate_info) :: whole, half
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

        type(pf_integrate_info) :: capped, budgeted
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
        type(pf_integrate_info) :: info
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

        ! Passing `atol` explicitly at its default must be the same call, bit for bit, on an
        ! infinite range as on a finite one.
        r_func = pf_integrate(tail_exp, 1.0_real64, inf, 1.0e-10_real64)
        r_tol = pf_integrate(tail_exp, 1.0_real64, inf, &
                             rtol=1.0e-10_real64, atol=0.0_real64)
        call check(error, r_func == r_tol, &
                   "an absent atol must be the same call as atol=0 on a tail, bit for bit")

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

        type(pf_integrate_info)   :: info
        type(pf_integrate_points) :: pts
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

        ! **A partition that outgrows the work arrays' first allocation.** They start at 64
        ! subintervals and DOUBLE, copying what is already there across -- the record included, in
        ! three arrays of 21 rows each. A copy that lost or misplaced a column would leave every
        ! integral right and only this sum wrong, which is the whole reason the check below is a
        ! weighted sum rather than a length. The count is asserted first as a vacuity guard: below
        ! 65 subintervals the arrays never grow and this case is the one above it a second time.
        r = pf_integrate(saw_sqrt, 0.0_real64, 1.0_real64, &
                         rtol=0.0_real64, atol=1.0e-14_real64, info=info, points=pts)
        call check(error, info%nsub > 64, &
                   "the growing-partition fixture must pass the work arrays' first allocation of 64")
        if (allocated(error)) return
        call check(error, pts%n == ONE_RULE*info%nsub, &
                   "a record carried through a growth must hold 21 points per subinterval still")
        if (allocated(error)) return
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, abs(summed - info%partition_integral) &
                   <= 1.0e-12_real64*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral across a work-array growth")
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

        ! An endpoint singularity, where the returned result is the epsilon table's and NOT the
        ! partition sum. The record is still exactly the partition, so `sum(w*f)` reproduces
        ! `partition_integral` as always and `res` no longer -- which is the whole reason
        ! `info%extrapolated` exists and the one case a caller re-weighting the record must know
        ! about.
        r = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info, points=pts)
        summed = sum(pts%w(1:pts%n)*pts%f(1:pts%n))
        call check(error, info%extrapolated, &
                   "log(x)/sqrt(x) must reach its answer through the table, which is the default")
        if (allocated(error)) return
        call check(error, abs(summed - info%partition_integral) &
                   <= 8.0_real64*epsilon(1.0_real64)*abs(info%partition_integral), &
                   "sum(w*f) must reproduce the partition integral on an extrapolated call too")
        if (allocated(error)) return
        call check(error, pts%n == ONE_RULE*info%nsub, &
                   "an extrapolated call's record must still be 21 points per subinterval")
        if (allocated(error)) return
        ! The two differ by the bisection's own remaining error, which is what the table removed.
        call check(error, info%partition_integral /= r, &
                   "an extrapolated result must NOT equal the partition sum it was accelerated from")
        if (allocated(error)) return
        call check(error, abs(r - log_sqrt_exact()) < abs(info%partition_integral - log_sqrt_exact()), &
                   "and the accelerated result must be the closer of the two to the closed form")
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

        type(pf_integrate_info)   :: bare, recorded
        type(pf_integrate_points) :: pts
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

        type(pf_integrate_points) :: left, right
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

        type(pf_integrate_info) :: info
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

        type(pf_integrate_info) :: info
        real(real64)              :: r
        integer, parameter        :: BUDGET = 105 ! 42*3 - 21: three subintervals exactly
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            r = pf_integrate(sharp_gauss, 0.0_real64, 1.0_real64, 1.0e-13_real64, &
                             max_neval=BUDGET, info=info)
            call check(error, info%status == PF_INT_LIMIT, &
                       "a budget too small for the tolerance must report PF_INT_LIMIT")
            if (allocated(error)) exit run
            call check(error, .not. info%converged, &
                       "converged must be false whenever the status is not PF_INT_OK")
            if (allocated(error)) exit run
            call check(error, info%neval == BUDGET, &
                       "a budget of the form 42k - 21 must be spent exactly")
            if (allocated(error)) exit run
            call check(error, r == r, "a call that ran out of budget must still return a number")
            if (allocated(error)) exit run

            ! A budget that is not a whole number of bisections is still never exceeded.
            r = pf_integrate(sharp_gauss, 0.0_real64, 1.0_real64, 1.0e-13_real64, &
                             max_neval=100, info=info)
            call check(error, info%neval <= 100, &
                       "the evaluation count must never exceed max_neval on a finite range")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_status_limit_and_converged_agree

    !> Asserts that a tolerance the arithmetic cannot deliver reports round-off rather than
    !! pretending to have met it.
    !!
    !! `x*x` is integrated exactly by the first rule application, so the only thing standing
    !! between the result and a relative tolerance of `1e-16` is the round-off floor QUADPACK puts
    !! under its own error estimate. Disabling that floor makes this call claim convergence.
    subroutine test_status_roundoff(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info
        real(real64)              :: r

        r = pf_integrate(x_squared, 0.0_real64, 1.0_real64, &
                         rtol=1.0e-16_real64, atol=1.0e-300_real64, info=info)
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

        type(pf_integrate_info) :: info
        real(real64)              :: r

        ! The plain bisection's own verdict: it keeps halving towards the jump until QUADPACK's
        ! "bad behaviour at a point" test fires.
        r = pf_integrate(unit_step, 0.0_real64, 1.0_real64, &
                         rtol=1.0e-15_real64, atol=1.0e-16_real64, info=info, &
                         extrapolate=.false.)
        call check(error, info%status == PF_INT_BAD_INTEGRAND, &
                   "drilling into a discontinuity must report PF_INT_BAD_INTEGRAND")
        if (allocated(error)) return
        call check(error, .not. info%converged, &
                   "a status other than PF_INT_OK must report converged = .false.")
        if (allocated(error)) return
        call check(error, abs(r - unit_step_exact()) <= 1.0e-12_real64, &
                   "the answer must still be right even though the status is not OK")
        if (allocated(error)) return

        ! The extrapolation's verdict on the same integrand, which is the DEFAULT path. A jump is
        ! not a singularity the epsilon table can accelerate, so the table itself stops making
        ! progress and reports it: `PF_INT_NO_CONVERGENCE`, the one code only stage 2 can reach.
        r = pf_integrate(unit_step, 0.0_real64, 1.0_real64, &
                         rtol=1.0e-15_real64, atol=1.0e-16_real64, info=info)
        call check(error, info%status == PF_INT_NO_CONVERGENCE, &
                   "the extrapolation's own failure on a jump must be PF_INT_NO_CONVERGENCE")
        if (allocated(error)) return
        call check(error, .not. info%converged, &
                   "PF_INT_NO_CONVERGENCE must report converged = .false. as well")
        if (allocated(error)) return
        call check(error, info%extrapolated, &
                   "the code is the table's finding, so the table must say it ran")
        if (allocated(error)) return
        ! **Read this bound before loosening it.** The extrapolated answer here is WORSE than the
        ! bisection's -- measured 1.8e-10 against 1.1e-16 -- while `info%abserr` claims 3.6e-15.
        ! An accelerated sequence that was never converging carries no error estimate worth
        ! having, and on this integrand the status is the only half of the answer that is honest.
        call check(error, abs(r - unit_step_exact()) <= 1.0e-8_real64, &
                   "the extrapolated answer must still be within 1e-8 of the closed form")

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

        type(pf_integrate_info) :: info
        real(real64)              :: r
        logical :: uf_ok, uf_was

        ! A bump of width 0.012 on a range of width 9: every sample misses it.
        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            r = pf_integrate(compact_bump, 1.0_real64, 10.0_real64, 1.0e-8_real64, info=info)
            call check(error, r == 0.0_real64, &
                       "a bump narrower than the first rule's spacing must integrate as exactly zero")
            if (allocated(error)) exit run
            call check(error, info%converged .and. info%neval == ONE_RULE .and. info%nsub == 1, &
                       "and must be reported as converged after exactly one rule application")
            if (allocated(error)) exit run

            ! Fit the range to the feature and the same integrand is integrated correctly.
            r = pf_integrate(compact_bump, 1.0_real64, 1.05_real64, 1.0e-8_real64, info=info)
            call check(error, abs(r - compact_bump_exact()) <= 1.0e-9_real64, &
                       "the same bump on a range fitted to it must be integrated correctly")
            if (allocated(error)) exit run

            ! The unit-width bump at 40 is found on [0, 100] and missed on [0, 1000], which is the
            ! measurement the design rests on.
            r = pf_integrate(far_bump, 0.0_real64, 100.0_real64, &
                             rtol=1.0e-8_real64, atol=1.0e-14_real64, info=info)
            call check(error, abs(r - far_bump_exact()) <= 1.0e-8_real64*far_bump_exact(), &
                       "a unit-width bump at 40 must be found on a range of width 100")
            if (allocated(error)) exit run

            r = pf_integrate(far_bump, 0.0_real64, 1000.0_real64, &
                             rtol=1.0e-8_real64, atol=1.0e-14_real64, info=info)
            call check(error, abs(r) <= 1.0e-14_real64 .and. info%converged, &
                       "the same bump on a range of width 1000 is missed and reported as converged")
            if (allocated(error)) exit run

            ! The cure, on the same call the first assertion above showed returning zero: the guide
            ! page prints exactly this, so this assertion is what keeps the page honest.
            r = pf_integrate(compact_bump, 1.0_real64, 10.0_real64, 1.0e-8_real64, &
                             breakpoints=[1.0_real64 + 1.0e-6_real64, 1.02_real64], info=info)
            call check(error, abs(r - compact_bump_exact()) <= 1.0e-9_real64, &
                       "a cut each side of the bump must find it on the very range that missed it")
            if (allocated(error)) exit run
            call check(error, info%converged, "and the cut call must converge")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
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

        type(pf_integrate_info) :: info
        real(real64)              :: r

        r = pf_integrate(runge, 0.0_real64, 1.0_real64, &
                         rtol=1.0e-14_real64, atol=1.0e-20_real64, info=info)
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

        type(pf_integrate_info) :: plain, extrapolated, by_default
        real(real64)              :: r_plain, r_extrapolated, r_default

        r_plain = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                               extrapolate=.false., info=plain)
        r_extrapolated = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                                      extrapolate=.true., info=extrapolated)
        r_default = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=by_default)

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
        if (allocated(error)) return
        ! **What pins the DEFAULT.** Omitting `extrapolate=` must be the same call as passing
        ! `.true.`, to the last bit; nothing else in this suite would notice the default going
        ! back to `.false.`, because every other assertion here is about a better answer or a
        ! smaller count and the plain bisection reaches both eventually.
        call check(error, r_default == r_extrapolated, &
                   "a call that omits extrapolate= must return the extrapolated result exactly")
        if (allocated(error)) return
        call check(error, by_default%neval == extrapolated%neval &
                   .and. by_default%extrapolated .and. by_default%status == extrapolated%status, &
                   "omitting extrapolate= must cost and report what extrapolate=.true. does")

    end subroutine test_extrapolation_earns_its_keep

    !> Asserts that the Wynn-epsilon table raises no underflow on an integral whose own values
    !! raise none.
    !!
    !! Each step of the table forms `1/delta` for three differences, and upstream's table holds
    !! `huge` as a placeholder for the element not yet computed, so the first difference of every
    !! step is about `huge` and its reciprocal is subnormal: every extrapolated call raised
    !! IEEE_UNDERFLOW for a term that cannot change the answer. The engine forms each reciprocal
    !! from a difference capped at `1/tiny` (deviation 14 in `parquet_integrate_engine.f90`).
    !! `log_sqrt` over `[0, 1]` raises nothing in its own arithmetic, and the call must report that
    !! the table ran, or the flag proves nothing. The flag is cleared around the call alone and
    !! restored as `saved .or. raised`, as `test_status_divergent` does for INVALID.
    subroutine test_extrapolation_raises_no_underflow(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info
        real(real64)              :: r
        logical                   :: can_test, saved, raised

        can_test = ieee_support_flag(ieee_underflow, 0.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag(ieee_underflow, saved)
            call ieee_set_flag(ieee_underflow, .false.)
        end if
        r = pf_integrate(log_sqrt, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info)
        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_underflow, raised)
            call ieee_set_flag(ieee_underflow, saved .or. raised)
        end if
        call check(error, info%extrapolated, &
                   "the call must have used the epsilon table, or the flag proves nothing")
        if (allocated(error)) return
        call check(error, abs(r - log_sqrt_exact()) <= 1.0e-9_real64, &
                   "the extrapolated result must reach the closed form")
        if (allocated(error)) return
        call check(error, .not. raised, &
                   "the epsilon table raised IEEE_UNDERFLOW: a reciprocal of the table's huge " // &
                   "placeholder was formed")

    end subroutine test_extrapolation_raises_no_underflow

    !> Asserts that the extrapolation costs the same at every tolerance, where the bisection does
    !! not.
    !!
    !! **The assertion that cannot be met by accident.** An endpoint singularity is the one shape
    !! where the plain bisection's cost grows with the tolerance -- halving the interval next to
    !! the singularity buys a fixed FACTOR of the remaining error rather than a fixed number of
    !! digits -- so it pays more and more for each further digit. The epsilon table extrapolates
    !! that geometric sequence to its limit instead, and reaches full double precision at the
    !! loosest tolerance asked for, so the count at `1e-12` is the count at `1e-6`. Measured on
    !! machine B: `log(x)/sqrt(x)` costs 315 evaluations at every tolerance on this ladder where
    !! the bisection alone costs 1995 rising to 3717; `1/sqrt(x)` costs 231 against 1617 rising
    !! to 3297; `x**-0.9` costs 231 against 8085 rising to 16443.
    !!
    !! The equality of the two counts is what a mutation cannot fake: forcing `noext` true makes
    !! every count grow again, and the growth is what fails here.
    subroutine test_endpoint_singularities_are_cheap(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        call one_singularity(error, inv_sqrt, inv_sqrt_exact(), "1/sqrt(x)")
        if (allocated(error)) return
        call one_singularity(error, mild_pow, mild_pow_exact(), "x**-0.9")
        if (allocated(error)) return
        call one_singularity(error, log_sqrt, log_sqrt_exact(), "log(x)/sqrt(x)")

    end subroutine test_endpoint_singularities_are_cheap

    !> Integrates one endpoint singularity at the loosest and the tightest tolerance of the
    !! ladder, and asserts that the cost did not move.
    subroutine one_singularity(error, fn, want, what)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.
        procedure(pf_integrand_func)               :: fn    !! the integrand
        real(real64), intent(in)                   :: want  !! its closed form over [0, 1]
        character(len=*), intent(in)               :: what  !! names the case in a message

        type(pf_integrate_info) :: loose, tight
        real(real64)              :: r_loose, r_tight

        r_loose = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-6_real64, info=loose)
        r_tight = pf_integrate(fn, 0.0_real64, 1.0_real64, 1.0e-12_real64, info=tight)

        call check(error, loose%converged .and. tight%converged, &
                   what//" must converge at both ends of the tolerance ladder")
        if (allocated(error)) return
        call check(error, loose%extrapolated .and. tight%extrapolated, &
                   what//" must reach its answer through the epsilon table, which is the default")
        if (allocated(error)) return
        ! The loose call is as accurate as the tight one: the table does not stop at the tolerance
        ! it was given, it stops when the accelerated sequence stops moving.
        call check(error, abs(r_loose - want) <= 1.0e-12_real64*abs(want), &
                   what//" at rtol 1e-6 must already be accurate to twelve digits")
        if (allocated(error)) return
        call check(error, abs(r_tight - want) <= 1.0e-12_real64*abs(want), &
                   what//" at rtol 1e-12 must meet the tolerance it was given")
        if (allocated(error)) return
        ! THE assertion. Without the extrapolation this count grows by thousands.
        call check(error, tight%neval <= loose%neval + BISECTION, &
                   what//" must cost no more at rtol 1e-12 than at 1e-6, to within one bisection")
        if (allocated(error)) return
        ! And the absolute level, so that "did not grow" cannot be met by both being enormous.
        call check(error, tight%neval <= 20*ONE_RULE, &
                   what//" must cost at most twenty rule applications in all")

    end subroutine one_singularity

    !> Asserts that an integral which does not exist is reported as divergent rather than answered.
    !!
    !! **The status is the only honest half of this answer.** `x**-1.1` over `[0, 1]` diverges,
    !! and the epsilon table, handed the partial sums of a series that runs away, lands on the
    !! analytic continuation `-1/(p - 1)` -- a finite number, and NEGATIVE for an integrand that
    !! is positive everywhere. A caller who reads the result and not `converged` gets `-10` for
    !! an integral that is `+infinity`. That the sign alone gives it away is luck, not a
    !! guarantee; `converged` is the guarantee.
    !!
    !! **The `extrapolate=.false.` arm is the contrast the default buys.** The plain bisection
    !! keeps halving the interval next to zero, the abscissae go below `1e-280`, `x**-1.1`
    !! overflows to an infinity, and the engine's non-finite screen ends the integration with
    !! `PF_INT_NONFINITE` -- a different answer to the same integral, reached by drilling rather
    !! than by recognising. It is asserted here because it is in process: the screen reports and
    !! does not abort, so the test binary survives what the drill runs into.
    !!
    !! **Surviving it takes more than a status, which is what the `IEEE_INVALID` flag asserts.**
    !! A rule that went on doing arithmetic with the screened infinity forms `Inf - Inf` in its
    !! error estimate: under nagfor's default traps that raise IS the abort the status exists to
    !! replace, and on every other compiler it is only this flag.
    subroutine test_status_divergent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info
        real(real64)              :: r
        logical                   :: can_test, saved, raised

        r = pf_integrate(divergent_pow, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=info)

        call check(error, info%status == PF_INT_DIVERGENT, &
                   "a divergent integral must report PF_INT_DIVERGENT")
        if (allocated(error)) return
        call check(error, .not. info%converged, &
                   "a divergent integral must not be reported as converged")
        if (allocated(error)) return
        ! The NaN screen first and the magnitude second, so neither half reads the other's case.
        call check(error, r == r .and. abs(r) <= huge(1.0_real64), &
                   "the returned estimate must still be a finite number")
        if (allocated(error)) return
        call check(error, info%extrapolated, &
                   "the divergence is the extrapolation's finding, so it must say it ran")
        if (allocated(error)) return
        ! What the table lands on, to the accuracy it claims: the finite part, not the integral.
        call check(error, abs(r - divergent_pow_finite_part()) <= 1.0e-8_real64, &
                   "the table must return the analytic continuation -1/(p-1), which is -10 here")
        if (allocated(error)) return
        ! And it must find that out cheaply: the point of the test is that the engine STOPS.
        call check(error, info%neval <= 20*ONE_RULE, &
                   "divergence must be found in at most twenty rule applications, not drilled for")
        if (allocated(error)) return

        ! The contrast: without the table the same integral is drilled into until the integrand
        ! overflows, and what comes back is the screen's finding rather than the divergence. The
        ! flag is cleared around this call alone and restored as `saved .or. raised`, so a flag
        ! raised elsewhere is neither hidden nor blamed on it; only INVALID is read, because the
        ! fixture's own `x**-1.1` legitimately raises OVERFLOW on the way to the infinity.
        can_test = ieee_support_flag(ieee_invalid, 0.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, saved)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        r = pf_integrate(divergent_pow, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                         extrapolate=.false., info=info)
        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, saved .or. raised)
        end if
        call check(error, .not. raised, &
                   "the drilled integral raised IEEE_INVALID: the engine did arithmetic with the " // &
                   "infinity it screened, which aborts a caller whose traps are unmasked")
        if (allocated(error)) return
        call check(error, info%status == PF_INT_NONFINITE, &
                   "without the extrapolation the drill reaches an overflow, which is reported " // &
                   "as PF_INT_NONFINITE rather than ending the process")
        if (allocated(error)) return
        call check(error, info%neval > 20*ONE_RULE, &
                   "and it must cost far more than the extrapolated arm, which is what the " // &
                   "table is for")

    end subroutine test_status_divergent

    ! ---- breakpoints ------------------------------------------------------------------------

    !> Asserts that naming a far feature's neighbourhood is what makes it visible.
    !!
    !! This is the positive half of `test_blind_spot_is_documented`, and the two are written to be
    !! read together: the same integrand, the same range, the same `atol`, one call without
    !! breakpoints and one with. Without them a unit-width bump at 40 on `[0, 1000]` is below the
    !! `atol` a first rule application can resolve, so the answer is zero and `converged` is true;
    !! with a cut each side of the bump, the piece that contains it gets a rule application of its
    !! own and the bump is found.
    !!
    !! **The `atol` is load-bearing.** With `atol = 0` the same unbroken call subdivides -- the
    !! relative test can never be met by a result of zero -- and finds the bump anyway. The blind
    !! spot needs a tolerance the wrong answer satisfies, which is what an absolute one is.
    subroutine test_breakpoints_find_the_far_bump(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: blind, named
        real(real64)              :: r_blind, r_named
        logical :: uf_ok, uf_was

        ! Extreme but legal inputs underflow inside the library; the flag is put back rather than
        ! left for nagfor to report at exit, unattributed (`.claude/rules/fortran-gotchas.md`).
        uf_ok = ieee_support_flag(ieee_underflow, 0.0_real64)
        if (uf_ok) call ieee_get_flag(ieee_underflow, uf_was)

        run: block
            r_blind = pf_integrate(far_bump, 0.0_real64, 1000.0_real64, &
                                   rtol=1.0e-8_real64, atol=1.0e-14_real64, info=blind)
            call check(error, abs(r_blind) <= 1.0e-14_real64 .and. blind%converged, &
                       "a bump at 40 on [0, 1000] at atol=1e-14 must come back as zero, converged")
            if (allocated(error)) exit run
            call check(error, blind%neval <= 3*ONE_RULE, &
                       "and it must come back that way after a handful of rule applications")
            if (allocated(error)) exit run

            r_named = pf_integrate(far_bump, 0.0_real64, 1000.0_real64, &
                                   rtol=1.0e-8_real64, atol=1.0e-14_real64, &
                                   breakpoints=[30.0_real64, 50.0_real64], info=named)
            call check(error, abs(r_named - far_bump_exact()) <= 1.0e-9_real64, &
                       "cutting the range at 30 and 50 must find the bump and reproduce sqrt(pi)")
            if (allocated(error)) exit run
            call check(error, named%nsub >= 3, &
                       "three pieces must leave at least three subintervals in the partition")

        end block run

        if (uf_ok) call ieee_set_flag(ieee_underflow, uf_was)
    end subroutine test_breakpoints_find_the_far_bump

    !> Asserts that cutting a range the integrand is smooth over changes the answer by nothing.
    !!
    !! Breakpoints are a way of spending evaluations where the caller knows they are needed, never
    !! a change of what is being integrated, so the piecewise sum must agree with the unbroken
    !! call and with the closed form -- in linear `x` and in `log x` alike. The unsorted case is
    !! the assertion the sort exists for: a caller who lists the cuts in any order gets the same
    !! integral, because the driver sorts a copy before it walks them.
    subroutine test_breakpoints_agree_with_the_whole(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: whole, cut, shuffled
        type(pf_integrate_points) :: pts
        real(real64)                :: r_whole, r_cut, r_shuffled, want
        real(real64), parameter     :: LO = 1.0e-3_real64, HI = 1.0e3_real64

        ! Runge on [0, 1], cut into four.
        r_whole = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, info=whole)
        r_cut = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                             breakpoints=[0.25_real64, 0.5_real64, 0.75_real64], info=cut, &
                             points=pts)
        call check(error, abs(r_cut - r_whole) <= 1.0e-12_real64*abs(r_whole), &
                   "four pieces of Runge's function must sum to the unbroken integral")
        if (allocated(error)) return
        call check(error, abs(r_cut - runge_exact()) <= 1.0e-10_real64*abs(runge_exact()), &
                   "and they must sum to atan(5)/5")
        if (allocated(error)) return
        call check(error, cut%converged, "every piece converged, so the sum must report converged")
        if (allocated(error)) return
        call check(error, pts%n == ONE_RULE*cut%nsub, &
                   "the record over all pieces must hold 21 points per subinterval")
        if (allocated(error)) return

        ! The same cuts, listed in the wrong order.
        r_shuffled = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-10_real64, &
                                  breakpoints=[0.75_real64, 0.25_real64, 0.5_real64], &
                                  info=shuffled)
        call check(error, r_shuffled == r_cut, &
                   "breakpoints given unsorted must give bit for bit the sorted answer")
        if (allocated(error)) return
        call check(error, shuffled%neval == cut%neval, &
                   "and must cost exactly what the sorted list cost")
        if (allocated(error)) return

        ! A power law over six decades in log x, cut at two of them.
        want = inv_pow15_exact(LO, HI)
        r_whole = pf_integrate(inv_pow15, LO, HI, 1.0e-10_real64, log_base=.true.)
        r_cut = pf_integrate(inv_pow15, LO, HI, 1.0e-10_real64, log_base=.true., &
                             breakpoints=[1.0e-2_real64, 1.0e2_real64], info=cut)
        call check(error, abs(r_cut - want) <= 1.0e-10_real64*abs(want), &
                   "breakpoints in log x must still reproduce 2*(lo**-0.5 - hi**-0.5)")
        if (allocated(error)) return
        call check(error, abs(r_cut - r_whole) <= 1.0e-10_real64*abs(want), &
                   "and must agree with the same call without them")

    end subroutine test_breakpoints_agree_with_the_whole

    !> Asserts that a breakpoint on an infinite range cuts the finite part and leaves the walk.
    !!
    !! The last piece of `[a, +inf)` reaches the infinity and goes through the outward walk, which
    !! is what `npanels` reports; dropping that piece would leave the finite pieces summing to
    !! something short of the integral, which the closed form catches. On the whole line the
    !! pieces are `(-inf, p1], [p1, p2], [p2, +inf)`, so the split at zero the unbroken call makes
    !! is never reached -- and the answer must be the same either way.
    subroutine test_breakpoints_on_infinite_ranges(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: tail, line
        real(real64)              :: r, inf, sqrt_pi

        inf = pf_infinity()
        sqrt_pi = far_bump_exact()

        r = pf_integrate(tail_exp, 0.0_real64, inf, 1.0e-10_real64, &
                         breakpoints=[1.0_real64, 2.0_real64], info=tail)
        call check(error, abs(r - 1.0_real64) <= 1.0e-10_real64, &
                   "exp(-x) over [0, inf) cut at 1 and 2 must still integrate to one")
        if (allocated(error)) return
        call check(error, tail%converged, "and must converge")
        if (allocated(error)) return
        call check(error, tail%npanels >= 1, &
                   "the piece reaching the infinity must have been walked, so panels were used")
        if (allocated(error)) return

        r = pf_integrate(tail_gauss, -inf, inf, 1.0e-10_real64, &
                         breakpoints=[-1.0_real64, 1.0_real64], info=line)
        call check(error, abs(r - sqrt_pi) <= 1.0e-9_real64*sqrt_pi, &
                   "exp(-x**2) over the whole line cut at -1 and 1 must reproduce sqrt(pi)")
        if (allocated(error)) return
        call check(error, line%converged, "and must converge")
        if (allocated(error)) return
        ! Two walks, one outward from each end: the middle piece is finite and is not walked.
        call check(error, line%npanels >= 3, &
                   "three pieces, two of them walked, must report at least three panels")

    end subroutine test_breakpoints_on_infinite_ranges

    !> Asserts that the pieces SHARE the caller's `atol` rather than each being given all of it.
    !!
    !! With `breakpoints` each piece is integrated to `atol/n_pieces`, so the sum meets `atol`
    !! whenever every piece meets its share -- and that is a THEOREM rather than a measurement:
    !! QUADPACK's loop does not exit until its own error estimate is inside the bound it was
    !! given, so four pieces each inside `atol/4` sum to inside `atol` on any compiler. Giving
    !! each piece the full `atol` instead leaves every piece reporting `PF_INT_OK`, the sum
    !! reporting `converged`, and an error estimate up to `n_pieces` times what was asked for.
    !!
    !! **Which half of this catches the mutation, and which cannot.** The obvious fixture is an
    !! integral that is zero by cancellation -- `sin(2 pi x)` over `[0, 1]`, where `rtol` on the
    !! sum means nothing and `atol` is the whole of the tolerance. It is asserted first, because
    !! that is the case `atol` exists for. It is also blind to the split: one rule application
    !! integrates a quarter sine to 1e-15, so every piece reports the same error estimate whatever
    !! tolerance it is handed, and `atol` replacing `atol/4` changes nothing at all.
    !!
    !! What sees the split is a piece whose accuracy is TOLERANCE-limited: `saw_sqrt`, an
    !! endpoint singularity per tooth, cut at the teeth. There the engine stops at the first
    !! partition inside the bound, so the estimate it returns sits just under the budget it was
    !! given -- and four budgets of `atol` instead of `atol/4` come back, measured, at about 3.6
    !! times the `atol` the caller asked for, with `converged` still true. That is Risk-274's
    !! silent failure exactly, and the summed `abserr` is what shows it: the ACTUAL error stays
    !! inside `atol` in both arms, so a test asserting only the answer would pass over it. It is
    !! also the only half a caller who cannot check the answer has.
    !!
    !! **The saw arm passes `extrapolate=.false.`, and that is what makes it discriminate.** The
    !! Wynn-epsilon table is on by default and accelerates each tooth to about `7e-13` whatever
    !! bound the piece was handed, so with it on the summed `abserr` is SIX ORDERS below `atol` in
    !! both arms and is bit-identical between them -- the assertion cannot see the split at all.
    !! The measurement above was taken when the extrapolation was off by default
    !! (`use_eps = .false.` at commit `ef307eb`, where this test was written); the default was
    !! flipped afterwards and silently blinded the test. With the bisection alone the engine is
    !! tolerance-limited again: `abserr` is `9.1e-7` against an `atol` of `1e-6`, and four
    !! undivided budgets overrun it.
    !!
    !! Mutation proved: drop the `/real(npieces, real64)` from `piece_tol` in `integrate_pieces`
    !! (`src/parquet_integrate_driver.f90`) and the saw arm's `abserr` assertion fails.
    subroutine test_breakpoints_share_atol(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info
        real(real64)              :: r
        real(real64), parameter   :: WAVE_ATOL = 1.0e-8_real64
        real(real64), parameter   :: SAW_ATOL = 1.0e-6_real64

        ! The case atol exists for: an integral that is zero by cancellation. Documenting, not
        ! discriminating -- see the note above.
        r = pf_integrate(full_wave, 0.0_real64, 1.0_real64, &
                         rtol=0.0_real64, atol=WAVE_ATOL, &
                         breakpoints=[0.25_real64, 0.5_real64, 0.75_real64], info=info)
        call check(error, info%converged, &
                   "an atol-only call over four pieces of a full sine wave must converge")
        if (allocated(error)) return
        call check(error, abs(r) <= WAVE_ATOL, &
                   "the integral of sin(2 pi x) over [0, 1] is zero, to within the atol asked for")
        if (allocated(error)) return
        call check(error, info%abserr <= WAVE_ATOL, &
                   "and its summed error estimate must be inside the atol too")
        if (allocated(error)) return

        ! The case that can tell the split from its absence -- with the extrapolation OFF, which
        ! is what leaves the engine tolerance-limited; see the note above.
        r = pf_integrate(saw_sqrt, 0.0_real64, 1.0_real64, &
                         rtol=0.0_real64, atol=SAW_ATOL, &
                         breakpoints=[0.25_real64, 0.5_real64, 0.75_real64], &
                         extrapolate=.false., info=info)
        call check(error, info%converged, &
                   "four endpoint singularities, one per piece, must each reach their share")
        if (allocated(error)) return
        call check(error, info%abserr <= SAW_ATOL, &
                   "the summed error estimate must be inside the atol asked for, not n_pieces " // &
                   "times it -- every piece converged, so this is what the atol/n_pieces split " // &
                   "buys and the only thing that reports it")
        if (allocated(error)) return
        call check(error, abs(r - saw_sqrt_exact()) <= SAW_ATOL, &
                   "and the answer itself must be inside the atol asked for")

    end subroutine test_breakpoints_share_atol

    ! ---- a non-finite integrand value -----------------------------------------------------

    !> Asserts that an integrand returning a NaN ends the INTEGRATION rather than the process.
    !!
    !! **This is the one failure that is not the caller's contract.** Every other refusal in this
    !! module is a bound, a tolerance, a budget or a breakpoint that does not say what it meant,
    !! and each of those aborts; a NaN out of `eval` is the caller's own function misbehaving on
    !! the caller's own data, and a caller sweeping a parameter grid has to be able to keep the
    !! sweep and say which parameter broke. So it is `PF_INT_NONFINITE`, `converged = .false.`
    !! and `info%nonfinite_at`.
    !!
    !! **The negative control is the whole of the second half.** `nonfinite_at` defaults to zero,
    !! which is a point like any other, so a test asserting only the NaN case would pass against
    !! an implementation that set the component for every call, or for none. The clean call
    !! asserts `PF_INT_OK` and a zero, and the two together are what pin it.
    subroutine test_non_finite_value_is_reported(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info, clean
        real(real64)              :: r
        logical                   :: can_test, saved, raised

        ! The NaN must not reach the rule's arithmetic either: an ordered comparison or a `min`
        ! over it raises IEEE_INVALID, which ends the process after all under unmasked traps
        ! (`test_status_divergent` asserts the infinity's half, and says why the flag is read).
        can_test = ieee_support_flag(ieee_invalid, 0.0_real64)
        saved = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, saved)
            call ieee_set_flag(ieee_invalid, .false.)
        end if
        r = pf_integrate(nan_at_half, 0.0_real64, 1.0_real64, 1.0e-8_real64, info=info)
        raised = .false.
        if (can_test) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, saved .or. raised)
        end if
        call check(error, .not. raised, &
                   "a NaN integrand raised IEEE_INVALID inside the engine, which aborts a caller " // &
                   "whose traps are unmasked")
        if (allocated(error)) return

        call check(error, info%status == PF_INT_NONFINITE, &
                   "a non-finite integrand value must report PF_INT_NONFINITE")
        if (allocated(error)) return
        call check(error, .not. info%converged, &
                   "a call that met a non-finite value must not be reported as converged")
        if (allocated(error)) return
        ! `nan_at_half` is NaN exactly on `|x - 0.5| < 0.05`, so the recorded point must be one
        ! the fixture really answers a NaN at -- not merely some number the engine had to hand.
        call check(error, abs(info%nonfinite_at - 0.5_real64) < 0.05_real64, &
                   "nonfinite_at must be a point the integrand actually returned a NaN at")
        if (allocated(error)) return
        ! The screen has to stop the walk of subintervals rather than let it run to the budget:
        ! the first rule application already meets the NaN, so nothing beyond a handful of
        ! applications can be justified.
        call check(error, info%neval <= 2*ONE_RULE, &
                   "the integration must stop at the value, not spend the budget on a NaN")
        if (allocated(error)) return

        ! The negative control.
        r = pf_integrate(runge, 0.0_real64, 1.0_real64, 1.0e-8_real64, info=clean)
        call check(error, clean%status == PF_INT_OK, &
                   "an integrand that never returns a non-finite value must report PF_INT_OK")
        if (allocated(error)) return
        call check(error, clean%nonfinite_at == 0.0_real64, &
                   "nonfinite_at must be left alone when no value was non-finite")

    end subroutine test_non_finite_value_is_reported

    !> Asserts that the outward walk and the piece list both stop at a non-finite value.
    !!
    !! The finite path above reaches the screen through `qagse`'s first rule application. These
    !! two reach it through the walk and through `integrate_pieces`, which are separate loops
    !! with separate exits, and each would otherwise go on asking a broken integrand for values:
    !! the walk until it ran out of panels or budget, the piece list until it had tried every
    !! piece. **The piece count is what asserts the second exit.** Cutting `[0, 1]` at `0.25` and
    !! `0.75` makes three pieces, the NaN lives in the middle one, and a piece list that did not
    !! stop would report three.
    subroutine test_non_finite_value_stops_every_path(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: walked, pieced, searched
        real(real64)              :: r, inf

        inf = pf_infinity()

        r = pf_integrate(nan_at_half, 0.0_real64, inf, 1.0e-8_real64, info=walked)
        call check(error, walked%status == PF_INT_NONFINITE, &
                   "a walk that meets a non-finite value must report PF_INT_NONFINITE")
        if (allocated(error)) return

        ! **The walk's START-PANEL SEARCH is a third exit**, before any panel has been integrated at
        ! all: it probes for the first panel worth taking, and an integrand that stops answering with
        ! numbers while it is still probing has to end the walk there rather than hand a panel it
        ! never accepted to the rest of it. `nan_at_half`'s band lies below every panel such a search
        ! looks at from a positive lower bound, which is why it takes the exit above and not this
        ! one; `nan_near_two`'s lies inside the first. No panel was integrated, so none is counted.
        r = pf_integrate(nan_near_two, 1.0_real64, inf, 1.0e-8_real64, info=searched)
        call check(error, searched%status == PF_INT_NONFINITE, &
                   "a start-panel search that meets a non-finite value must report PF_INT_NONFINITE")
        if (allocated(error)) return
        call check(error, .not. searched%converged .and. searched%npanels == 0, &
                   "and must count no panel: the probes are not panels")
        if (allocated(error)) return
        call check(error, searched%nonfinite_at >= 1.5_real64 .and. searched%nonfinite_at <= 2.5_real64, &
                   "nonfinite_at must be a point inside the band the fixture answers a NaN on")
        if (allocated(error)) return
        call check(error, .not. walked%converged, &
                   "a walk that met a non-finite value must not be reported as converged")
        if (allocated(error)) return
        call check(error, abs(walked%nonfinite_at - 0.5_real64) < 0.05_real64, &
                   "the walk must record a point the integrand really answered a NaN at")
        if (allocated(error)) return
        call check(error, walked%npanels < DEFAULT_PANELS, &
                   "the walk must stop at the value rather than run out its panel cap")
        if (allocated(error)) return

        r = pf_integrate(nan_at_half, 0.0_real64, 1.0_real64, 1.0e-8_real64, &
                         breakpoints=[0.25_real64, 0.75_real64], info=pieced)
        call check(error, pieced%status == PF_INT_NONFINITE, &
                   "a piece that meets a non-finite value must report PF_INT_NONFINITE")
        if (allocated(error)) return
        call check(error, pieced%npanels == 2, &
                   "the piece list must stop at the piece that met the value, having integrated " // &
                   "the one before it and not the one after")

    end subroutine test_non_finite_value_stops_every_path

    !> Asserts that the DEFAULT panel cap is high enough for an algebraic tail at `rtol = 1e-12`.
    !!
    !! `x**-1.5` over `[1, infinity)` is the dearest tail shape the walk converges on at all, and
    !! it is the shape `DEFAULT_MAX_PANELS` is set by: it needs more panels than any other scored
    !! tail, and a cap below what it needs turns an integral the walk can deliver into
    !! `PF_INT_LIMIT` with an answer short of the tolerance. **The `max_panels=50` arm is the
    !! control**, and it is what makes this test about the DEFAULT rather than about the walk:
    !! without it the assertion would pass at any cap the walk happens to clear.
    subroutine test_default_panel_cap_clears_an_algebraic_tail(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: deflt, capped
        real(real64)              :: r, want, inf

        inf = pf_infinity()
        ! `2*(1**-0.5 - 0)`: the closed form's upper term vanishes at infinity. Never
        ! `inv_pow15_exact(1.0_real64, huge(1.0_real64))` -- `huge()` is a number, and nagfor's
        ! `**` overflows on it and aborts the runner (fortran-gotchas.md, the `huge()` sentinel).
        want = 2.0_real64

        r = pf_integrate(inv_pow15, 1.0_real64, inf, 1.0e-12_real64, info=deflt)
        call check(error, deflt%converged, &
                   "an algebraic tail at rtol 1e-12 must converge at the DEFAULT panel cap")
        if (allocated(error)) return
        call check(error, abs(r - want) <= 1.0e-12_real64*abs(want), &
                   "and it must meet the tolerance it reported converging to")
        if (allocated(error)) return
        call check(error, deflt%npanels < DEFAULT_PANELS, &
                   "it must finish inside the cap rather than by reaching it")
        if (allocated(error)) return

        ! The control: the same call at a cap below what this tail needs does NOT converge, which
        ! is what says the assertions above are about the default and not about the shape.
        r = pf_integrate(inv_pow15, 1.0_real64, inf, 1.0e-12_real64, max_panels=50, info=capped)
        call check(error, .not. capped%converged, &
                   "the same tail must NOT converge at a cap below the one the default sets, or " // &
                   "this test would pass whatever the default were")
        if (allocated(error)) return
        call check(error, capped%status == PF_INT_LIMIT, &
                   "a walk stopped by its panel cap must say so")

    end subroutine test_default_panel_cap_clears_an_algebraic_tail

    !> The `pf_integrate` row of the overshoot column in doc/pages/utilities/solvers.md,
    !! "The budget: max_neval": how far past `max_neval` a call may go, on each of the three shapes
    !! of range, and that `max_neval` binds at all.
    !!
    !! **Where the numbers come from, so that none of them is a recorded output.** A panel's
    !! subinterval cap is `(max(budget - neval, 0) + ONE_RULE)/BISECTION` floored at 1
    !! (`run_panel`, `src/parquet_integrate_driver.f90`), so a panel still costs one rule
    !! application with nothing left of the budget; the walk tests the budget before every panel
    !! after the first. On an infinite range `find_start_panel` spends one rule application of its
    !! own before that first panel, and neither it nor the first panel sits behind a budget test --
    !! so an outward walk costs `2*ONE_RULE` whatever `max_neval` says, and can exceed a budget it
    !! has already spent by that much. `(-inf, +inf)` is TWO walks, split at zero
    !! (`integrate_infinite`), and the second is entered whatever the first spent: the first
    !! overshoots by at most its last panel, `ONE_RULE`, and the second then pays its whole
    !! `2*ONE_RULE`. A finite range has no walk and no start probe, so once the budget covers the
    !! one unavoidable rule application it is never exceeded.
    !!
    !! **Two negative controls, because the bound alone is satisfied by doing nothing.** Every case
    !! also asserts the MINIMUM cost the same mechanism forces -- one rule application on a finite
    !! range, two per walk on an infinite one -- so a call that returned at once fails. And each
    !! shape must somewhere in the sweep actually pass its budget (`neval >= budget`), counted and
    !! required non-zero, or the bound was asserted over runs that never tried to exceed anything.
    !!
    !! **`PF_INT_LIMIT` is NOT the discriminator for "the budget bound"**, which is why the counts
    !! key on `neval` instead: `walk_outward` reports `LIMIT` whenever it did not show the tail was
    !! small, which includes stopping on `max_panels` or on the abscissa ceiling with evaluations
    !! still unspent. Measured: a two-sided run at `max_neval = 343` ends `LIMIT` with `neval` 336.
    subroutine test_guide_budget_overshoot_bounds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        type(pf_integrate_info) :: info
        real(real64) :: r, inf
        integer :: budget, seen_finite, seen_one, seen_two
        character(len=190) :: msg
        !> A one-sided infinite range may exceed `max_neval` by one rule application.
        integer, parameter :: ONE_SIDED_OVER = ONE_RULE
        !> `(-inf, +inf)` may exceed it by the first walk's last panel plus the second walk's
        !! two unavoidable rule applications.
        integer, parameter :: TWO_SIDED_OVER = ONE_RULE + 2*ONE_RULE
        !> What a walk cannot avoid spending: the start probe and the first panel.
        integer, parameter :: PER_WALK_MIN = 2*ONE_RULE

        inf = pf_infinity()
        seen_finite = 0
        seen_one = 0
        seen_two = 0

        do budget = ONE_RULE, 400, 7
            ! ---- a finite range: never past the cap -------------------------------------
            r = pf_integrate(osc_a, 0.0_real64, 1.0_real64, 1.0e-13_real64, max_neval=budget, &
                             info=info)
            write(msg, '(a, i0, a, i0)') "finite: max_neval ", budget, " was exceeded, neval ", &
                info%neval
            call check(error, info%neval <= budget, trim(msg))
            if (allocated(error)) return
            write(msg, '(a, i0, a, i0)') "finite: one rule application is unavoidable, so neval " // &
                "must be at least ", ONE_RULE, "; it is ", info%neval
            call check(error, info%neval >= ONE_RULE, trim(msg))
            if (allocated(error)) return
            if (info%neval >= budget) seen_finite = seen_finite + 1

            ! ---- one-sided infinite: one rule application past the cap ------------------
            r = pf_integrate(tail_osc, 1.0_real64, inf, 1.0e-13_real64, max_neval=budget, &
                             info=info)
            write(msg, '(a, i0, a, i0, a, i0)') "one-sided: max_neval ", budget, &
                " may be exceeded by ", ONE_SIDED_OVER, ", neval ", info%neval
            call check(error, info%neval <= budget + ONE_SIDED_OVER, trim(msg))
            if (allocated(error)) return
            write(msg, '(a, i0, a, i0)') "one-sided: a walk cannot spend less than ", &
                PER_WALK_MIN, "; neval is ", info%neval
            call check(error, info%neval >= PER_WALK_MIN, trim(msg))
            if (allocated(error)) return
            if (info%neval >= budget) seen_one = seen_one + 1

            ! ---- two-sided infinite: three, because the second walk pays in full --------
            r = pf_integrate(tail_gauss, -inf, inf, 1.0e-13_real64, max_neval=budget, info=info)
            write(msg, '(a, i0, a, i0, a, i0)') "two-sided: max_neval ", budget, &
                " may be exceeded by ", TWO_SIDED_OVER, ", neval ", info%neval
            call check(error, info%neval <= budget + TWO_SIDED_OVER, trim(msg))
            if (allocated(error)) return
            write(msg, '(a, i0, a, i0)') "two-sided: two walks cannot spend less than ", &
                2*PER_WALK_MIN, "; neval is ", info%neval
            call check(error, info%neval >= 2*PER_WALK_MIN, trim(msg))
            if (allocated(error)) return
            if (info%neval >= budget) seen_two = seen_two + 1
        end do

        ! The vacuity guard: each shape must have reached its budget somewhere in the sweep, or its
        ! bound above was asserted over runs that never tried to exceed anything.
        write(msg, '(a, 3(1x, i0))') "the budget must bind somewhere in the sweep; neval reached " // &
            "it (finite, one-sided, two-sided) times:", seen_finite, seen_one, seen_two
        call check(error, seen_finite > 0 .and. seen_one > 0 .and. seen_two > 0, trim(msg))

    end subroutine test_guide_budget_overshoot_bounds

end module test_integrate
