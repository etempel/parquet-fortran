!> Tests for `parquet_prima`: BOBYQA, LINCOA and COBYLA, their bounds and constraints, their
!! `scale=` argument and BOBYQA's use as a local solver under `pf_minimize_multistart`.
!!
!! **Every expected location is the objective's own algebra**, never a location read off a
!! previous run: the sphere's minimum is its centre, Rosenbrock's is `(1, 1)`, `bad_scaling`'s is
!! `BAD_SCALING_MIN` by construction, and where the minimum lies outside the box the answer is the
!! nearest point of the box, which is again algebra rather than a recorded output. The constrained
!! answers are worked out the same way -- each is a KKT point whose multipliers are written into
!! the test's own comment, or the projection of a centre onto a circle.
!!
!! **No test asserts an exact evaluation count.** A count is a whole-search-path quantity and a
!! reassociated floating-point model moves it by a few; every count assertion here is an upper
!! bound, which a real regression moves by tens or hundreds.
!!
!! **The engine is a transcription of PRIMA at commit `43863c69`**, and the strongest check on it
!! is not in this file: it is the differential run recorded in `feature_optimizer.md` step 23's
!! `Outcome:` paragraph, where 120 minimisations against upstream's own `bobyqa` agreed on every
!! evaluation count and every bit of every answer. What is here is the behaviour this library
!! adds on top -- the refusals, the scaling, the record, the status mapping and the solver object.
!!
!! The abort paths are not here: an `error stop` kills the runner, so each is an out-of-process
!! scenario in `test/error_scenarios.f90` with its wrapper in `test/test_errors.f90`.
module test_prima

    use parquet_prima
    use parquet_optimize, only : pf_minimize_multistart, pf_simplex_solver
    use test_optimize_support
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_get_flag, ieee_set_flag, ieee_usual, ieee_underflow, &
                                             ieee_is_nan
#ifndef __flang__
    ! The halting-mode pair lowers to `feenableexcept`/`fedisableexcept`, which Apple's libc
    ! lacks, so flang on macOS cannot LINK a reference to either (`fortran-gotchas.md`).
    use, intrinsic :: ieee_arithmetic, only : ieee_support_halting, ieee_get_halting_mode, &
                                             ieee_set_halting_mode, ieee_overflow, ieee_invalid, &
                                             ieee_divide_by_zero
#endif

    implicit none
    private

    public :: collect_tests_prima

contains

    !> Collects every test in this suite.
    subroutine collect_tests_prima(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("BOBYQA reaches each closed-form minimiser inside its box", &
                         test_bobyqa_closed_forms), &
            new_unittest("BOBYQA works with no bounds at all", &
                         test_bobyqa_unbounded), &
            new_unittest("every point BOBYQA evaluates lies inside the bounds", &
                         test_bobyqa_stays_inside_the_bounds), &
            new_unittest("a minimum outside the box is answered on the boundary", &
                         test_bobyqa_minimum_outside_the_box), &
            new_unittest("the start point is never moved, and rhobeg shrinks instead", &
                         test_bobyqa_honours_the_start), &
            new_unittest("bounds at +/-huge() are no bounds, and nothing overflows reaching that", &
                         test_bobyqa_bounds_beyond_boundmax), &
            new_unittest("BOBYQA runs the configuration whose geometry step overflows", &
                         test_bobyqa_geometry_step_overflow), &
            new_unittest("BOBYQA runs an objective whose values are near 1e300", &
                         test_bobyqa_huge_objective_values), &
            new_unittest("a scale= run reproduces the hand-scaled objective bit for bit", &
                         test_bobyqa_scale_matches_hand_scaling), &
            new_unittest("scale= reaches a badly conditioned minimum the unscaled run misses", &
                         test_bobyqa_scale_earns_its_place), &
            new_unittest("converged= agrees with info%converged on every exit, per engine", &
                         test_prima_converged_argument_agrees), &
            new_unittest("a budget stops at PF_OPT_LIMIT with the best point so far", &
                         test_bobyqa_budget), &
            new_unittest("ftarget stops at PF_OPT_TARGET as soon as it is met", &
                         test_bobyqa_ftarget), &
            new_unittest("the record holds every evaluation, in the caller's units and in order", &
                         test_bobyqa_history), &
            new_unittest("the plain-function and object forms evaluate identically", &
                         test_bobyqa_forms_agree), &
            new_unittest("npt is honoured, and a fuller model still converges", &
                         test_bobyqa_npt), &
            new_unittest("pf_bobyqa_solver finds both wells under the multistart driver", &
                         test_bobyqa_solver_multistart), &
            new_unittest("pf_bobyqa_solver's options reach the runs it drives", &
                         test_bobyqa_solver_options), &
            new_unittest("LINCOA reaches the KKT point of a quadratic with both kinds of " // &
                         "constraint active", test_lincoa_kkt_point), &
            new_unittest("LINCOA returns the best FEASIBLE point it evaluated, not its last", &
                         test_lincoa_returns_the_filtered_best), &
            new_unittest("LINCOA's plain-function and object forms evaluate identically", &
                         test_lincoa_forms_agree), &
            new_unittest("COBYLA answers on the constraint boundary, and the sign decides which " // &
                         "side", test_cobyla_sign_of_the_constraint), &
            new_unittest("COBYLA honours a linear constraint beside the nonlinear one", &
                         test_cobyla_linear_and_nonlinear), &
            new_unittest("contradictory constraints give PF_OPT_INFEASIBLE and the least " // &
                         "violating point", test_cobyla_infeasible), &
            new_unittest("info%cstrv is measured in the caller's units, feasible or not", &
                         test_cstrv_is_in_the_callers_units), &
            new_unittest("the six calls of the guide's worked example reproduce their answers", &
                         test_guide_examples), &
            new_unittest("a start with coordinates on both bounds keeps them and still converges", &
                         test_bobyqa_start_on_bounds), &
            new_unittest("pf_bobyqa_solver without scale_from_box scales the radius by the box", &
                         test_bobyqa_solver_unscaled), &
            new_unittest("COBYLA's default rhobeg is pulled up by an explicit rhoend", &
                         test_cobyla_rhoend_pulls_rhobeg_up), &
            new_unittest("COBYLA stops on ftarget, and on its budget, through checkexit_con", &
                         test_cobyla_target_and_budget), &
            new_unittest("LINCOA builds the fuller model an npt above 2n+1 asks for", &
                         test_lincoa_larger_npt), &
            new_unittest("a budget too small to finish the initial model stops inside it", &
                         test_budget_below_the_initial_model), &
            new_unittest("LINCOA with no constraints at all minimises over the whole space", &
                         test_lincoa_without_constraints), &
            new_unittest("LINCOA stops on ftarget and on a budget spent in the main loop", &
                         test_lincoa_target_and_budget), &
            new_unittest("LINCOA rescales a model gradient too large to step on directly", &
                         test_lincoa_huge_gradient), &
            new_unittest("COBYLA honours every budget it is given, whatever the search was doing", &
                         test_cobyla_honours_every_budget), &
            new_unittest("LINCOA minimises a strongly non-quadratic objective under a constraint", &
                         test_lincoa_non_quadratic), &
            new_unittest("the guide's overshoot bound holds for all three engines, and binds", &
                         test_guide_budget_overshoot_bounds)]

    end subroutine collect_tests_prima

    !> BOBYQA on three objectives whose minimisers are known in closed form.
    subroutine test_bobyqa_closed_forms(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(4), fmin, lo(4), hi(4)
        type(pf_optimize_info) :: info

        ! `sphere` is the sum of squares about 1, so its minimum 0 is at 1 in every coordinate.
        x = [0.7_real64, -1.3_real64, 2.2_real64, -0.4_real64]
        lo = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-6_real64, &
                   "the sphere's minimiser is 1 in every coordinate; BOBYQA stopped elsewhere")
        if (allocated(error)) return
        call check(error, fmin < 1.0e-12_real64, "the sphere's minimum is 0")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK .and. info%converged, &
                   "a radius-driven stop is PF_OPT_OK")
        if (allocated(error)) return
        call check(error, info%neval <= 300, "the sphere in four variables should not cost 300 evaluations")
        if (allocated(error)) return
        call check(error, info%rho <= 1.0e-8_real64, "the final radius should have reached rhoend")
        if (allocated(error)) return

        ! Rosenbrock in two variables: minimum 0 at (1, 1), both terms vanishing there.
        x(1:2) = [-1.2_real64, 1.0_real64]
        lo(1:2) = -5.0_real64
        hi(1:2) = 10.0_real64
        call pf_minimize_bobyqa(rosenbrock, x(1:2), fmin, lower=lo(1:2), upper=hi(1:2), &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x(1:2) - 1.0_real64)) < 1.0e-6_real64, &
                   "Rosenbrock's minimiser is (1, 1); BOBYQA stopped elsewhere")
        if (allocated(error)) return
        call check(error, info%neval <= 400, "Rosenbrock-2 should not cost 400 evaluations")
        if (allocated(error)) return

        ! A sphere whose minimiser lives in an ALLOCATABLE component, and which counts its own
        ! calls: at one thread the caller's own object is used, so the count comes back.
        block
            type(table_sphere) :: obj
            obj%centre = [0.25_real64, -1.5_real64]
            x(1:2) = [1.0_real64, 1.0_real64]
            lo(1:2) = -3.0_real64
            hi(1:2) = 3.0_real64
            call pf_minimize_bobyqa(obj, x(1:2), fmin, lower=lo(1:2), upper=hi(1:2), &
                                    rhobeg=0.3_real64, rhoend=1.0e-9_real64, info=info)
            call check(error, maxval(abs(x(1:2) - obj%centre)) < 1.0e-7_real64, &
                       "the objective's minimiser is its centre component")
            if (allocated(error)) return
        end block

    end subroutine test_bobyqa_closed_forms

    !> With neither bound present the engine still converges: the bounds are genuinely optional.
    subroutine test_bobyqa_unbounded(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info

        x = [-1.2_real64, 1.0_real64]
        call pf_minimize_bobyqa(rosenbrock, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-8_real64, &
                                info=info)
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-6_real64, &
                   "an unbounded run should still reach Rosenbrock's minimiser")
        if (allocated(error)) return
        call check(error, info%converged, "an unbounded run should converge")

    end subroutine test_bobyqa_unbounded

    !> Not one evaluation happens outside the box, which is what "bound-constrained" has to mean.
    !!
    !! The unconstrained minimum is inside the box, so this is a test of the SEARCH rather than of
    !! the answer: an engine that searched freely and clipped only on return would give the same
    !! `x` and a `history` full of points outside the box.
    !!
    !! **The start and `rhobeg` are chosen so the bounds BIND from the first step.** BOBYQA's
    !! initial interpolation set is the start displaced by `+/-rhobeg` in each coordinate; from
    !! `(0.05, 0.05)` with `rhobeg = 0.5` every negative displacement lands outside `[0, 2]`, so
    !! an unbounded search puts a point outside the box before it has done anything else. An
    !! earlier version of this test started in the middle of the box, where a free search happens
    !! to stay inside anyway and the assertion held for no reason.
    subroutine test_bobyqa_stays_inside_the_bounds(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lo(2), hi(2)
        type(pf_optimize_history) :: record
        integer :: k

        lo = [0.0_real64, 0.0_real64]
        hi = [2.0_real64, 2.0_real64]
        x = [0.05_real64, 0.05_real64]
        call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, history=record)

        call check(error, record%n > 10, &
                   "the record is empty or nearly so: the bound check below went unexercised")
        if (allocated(error)) return
        do k = 1, record%n
            call check(error, all(record%x(:, k) >= lo) .and. all(record%x(:, k) <= hi), &
                       "BOBYQA evaluated the objective outside the bounds it was given")
            if (allocated(error)) return
        end do

    end subroutine test_bobyqa_stays_inside_the_bounds

    !> With the minimum outside the box the answer is the box's own nearest point.
    subroutine test_bobyqa_minimum_outside_the_box(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lo(2), hi(2)
        type(pf_optimize_info) :: info

        ! Rosenbrock's minimiser is (1, 1); this box stops short of it in both coordinates, and
        ! the function decreases towards it along the valley, so the answer sits on the corner.
        lo = [-2.0_real64, -2.0_real64]
        hi = [0.5_real64, 0.25_real64]
        x = [0.0_real64, 0.0_real64]
        call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lo, upper=hi, rhobeg=0.05_real64, &
                                rhoend=1.0e-9_real64, info=info)

        call check(error, all(x >= lo) .and. all(x <= hi), &
                   "the returned point must lie inside the bounds")
        if (allocated(error)) return
        call check(error, abs(x(1) - hi(1)) < 1.0e-6_real64, &
                   "the first coordinate should have been driven onto its upper bound")
        if (allocated(error)) return
        call check(error, abs(fmin - rosenbrock(x)) <= 0.0_real64, &
                   "fmin must be the objective's value at the returned point, exactly")

    end subroutine test_bobyqa_minimum_outside_the_box

    !> The start is used as given, and `rhobeg` shrinks to make room instead.
    !!
    !! PRIMA's own default is the other way round: with a valid `rhobeg` it sets
    !! `honour_x0 = .false.` and MOVES the start away from the bound, warning as it does so. This
    !! library fixes `honour_x0 = .true.` (Q9), so the first point evaluated is the caller's own.
    !! The negative control is exactly that revert: with the start moved, the first record entry
    !! would be `lower + rhobeg` rather than the start.
    subroutine test_bobyqa_honours_the_start(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lo(2), hi(2), start(2)
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record

        lo = [0.0_real64, 0.0_real64]
        hi = [4.0_real64, 4.0_real64]
        ! Well inside the box, but only 0.01 from the lower bound in the first coordinate, and
        ! rhobeg is 0.5 -- fifty times the room available.
        start = [0.01_real64, 2.0_real64]
        x = start
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, info=info, history=record)

        call check(error, record%n > 0, "the record is empty: the start assertion went unexercised")
        if (allocated(error)) return
        call check(error, all(record%x(:, 1) == start), &
                   "the first point evaluated was not the caller's start: the start was moved")
        if (allocated(error)) return
        ! The radius had to come down to the room available, and rhoend came down with it, so the
        ! run ends below the 1e-10 it was asked for rather than at it.
        call check(error, info%rho < 1.0e-10_real64, &
                   "rhobeg was not reduced to the distance from the bound, or rhoend did not follow")

    end subroutine test_bobyqa_honours_the_start

    !> A bound at `+/-huge()` means "no bound", and reaching that verdict costs no arithmetic.
    !!
    !! `huge()` is the sentinel a caller reaches for when the bound argument is not optional in
    !! their own code, and it is a NUMBER: `upper - lower` and `lower/scale` overflow on it. The
    !! overflow delivers the infinity that was meant, so the run looks right under gfortran and ifx
    !! and ends a build that halts on the flag. The bounds are therefore recognised as absent
    !! BEFORE any arithmetic on them.
    !!
    !! **The flag assertion is comparative, against the same run with no bounds at all.** The
    !! engines carry the absent sentinel `+/-BOUNDMAX` through their own arithmetic, and under ifx
    !! that arithmetic raises overflow on an UNBOUNDED run already -- upstream's code, unchanged
    !! here, and nothing this driver can avoid. What the driver owns, and what this pins, is that
    !! naming the bounds explicitly adds no flag the same call without them does not raise.
    subroutine test_bobyqa_bounds_beyond_boundmax(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: free(2), bounded(2), f_free, f_bounded, lo(2), hi(2), sc(2)
        type(pf_optimize_info) :: i_free, i_bounded
        logical :: halting(size(ieee_usual)), saved(size(ieee_usual)), base(size(ieee_usual)), &
                   raised(size(ieee_usual))

        lo = -huge(1.0_real64)
        hi = huge(1.0_real64)
        ! Held off, read and restored in this body, never in a helper: see `traps_can_be_held`.
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)

        ! The baseline: the same call with no bounds, whose flags are the engine's own.
        free = [-1.2_real64, 1.0_real64]
        call pf_minimize_bobyqa(rosenbrock, free, f_free, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=i_free)
        call ieee_get_flag(ieee_usual, base)

        call ieee_set_flag(ieee_usual, .false.)
        bounded = [-1.2_real64, 1.0_real64]
        call pf_minimize_bobyqa(rosenbrock, bounded, f_bounded, lower=lo, upper=hi, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=i_bounded)
        call ieee_get_flag(ieee_usual, raised)

        call check(error, all(raised .eqv. (raised .and. base)), &
            "forming the box from +/-huge() bounds raised a flag the unbounded run does not")
        if (allocated(error)) return

        ! Absent and beyond-BOUNDMAX are the same problem, so they are the same run: not a
        ! tolerance, every bit of it.
        call check(error, all(bounded == free) .and. f_bounded == f_free, &
            "a run inside +/-huge() bounds must be the unbounded run, point for point")
        if (allocated(error)) return
        call check(error, i_bounded%neval, i_free%neval, "and evaluation for evaluation")
        if (allocated(error)) return

        ! A scale below one is the second overflow, and the one the driver's own comment guards
        ! against for ABSENT bounds only: `lower/scale` is `-1e311` before it is anything else.
        ! Its baseline is the same scaled call with no bounds.
        sc = 1.0e-3_real64
        call ieee_set_flag(ieee_usual, .false.)
        free = [-1.2_real64, 1.0_real64]*sc
        call pf_minimize_bobyqa(rosenbrock, free, f_free, scale=sc, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=i_free)
        call ieee_get_flag(ieee_usual, base)

        call ieee_set_flag(ieee_usual, .false.)
        bounded = [-1.2_real64, 1.0_real64]*sc
        call pf_minimize_bobyqa(rosenbrock, bounded, f_bounded, lower=lo, upper=hi, scale=sc, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=i_bounded)
        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif

        call check(error, all(raised .eqv. (raised .and. base)), &
            "dividing a +/-huge() bound by a scale below one raised a flag the same call without " // &
            "bounds does not")
        if (allocated(error)) return
        call check(error, i_bounded%neval, i_free%neval, &
            "and the scaled run is the same run with the bounds named or left out")

    end subroutine test_bobyqa_bounds_beyond_boundmax

    !> The one battery configuration whose geometry step raises IEEE overflow and invalid.
    !!
    !! **This is a reproducer, not a demand that the site keep raising one particular flag.** Inside
    !! `geostep` the engine forms `sqrt(resis/ggfree)` with a `ggfree` of order `1e-300`, and then a
    !! curvature from the result; both are upstream's own arithmetic, and upstream's `bobyqa` trips
    !! the same configuration. WHICH exception that is depends on the build: gfortran raises
    !! overflow and invalid, while ifx flushes the tiny `ggfree` to zero at `-O1` and above and
    !! raises divide-by-zero and invalid instead. Both complete the run; a build that halts on the
    !! flags (nagfor's default) stops there, which is why the halting mode is held off around the
    !! call rather than left to end the runner.
    !!
    !! The fixture is Brown's almost-linear function in twelve variables with `npt = n + 2`, the
    !! bounded second start of the verification battery; it is written out as literals because one
    !! ulp in any of them is a different search path. WHEN THE SITE IS GUARDED, this test asserts
    !! the opposite -- that neither flag is raised -- rather than being deleted.
    !!
    !! **Reaching the site is a property of the TARGET, not of the fixture, so the flag reading is a
    !! SKIP rather than a failure.** One ulp is a different search path and an FMA is one ulp: on a
    !! target that has a fused multiply-add the compiler contracts `a*b + c` and the run walks away
    !! from the `1e-300` curvature this fixture was aimed at. Measured on arm64 macOS under nagfor
    !! 7.2, where the site is unreachable -- not for this fixture and not for any of 32 `npt` and
    !! `rhobeg` variations of it -- while the same compiler at the same version reaches it on
    !! x86-64, whose baseline has no FMA to contract into; see
    !! `fortran-gotchas.md`, "One target contracts to an FMA and another cannot".
    !!
    !! So a skip is the expected reading of a reproducer keyed to one path, and **re-aiming the
    !! literals to suit one machine would only move the skip to the other**; the assertions that
    !! run everywhere are the survival ones.
    subroutine test_bobyqa_geometry_step_overflow(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64), parameter :: START(12) = [ &
            -2.2094910695261749E-01_real64, -1.4343417097974296E+00_real64, -1.1086736317298718E+00_real64, &
            -7.8487723264139020E-01_real64, 1.1911031674552257E+00_real64, -2.5900376879563725E-01_real64, &
            1.6290764657915924E+00_real64, 1.1500802259659770E+00_real64, -4.7741239633290666E-01_real64, &
            -1.1737833857414235E+00_real64, 3.0218687574480985E-01_real64, -1.1373209222859335E+00_real64]
        real(real64), parameter :: LOWER(12) = [ &
            -1.9118292748051833E+00_real64, -2.5439127488266271E+00_real64, -3.1900051975110566E+00_real64, &
            -1.4316137865798613E+00_real64, -2.4965516373033414E-01_real64, -7.8725789500738386E-01_real64, &
            -2.1595744193343647E-02_real64, 1.1204627464155048E-01_real64, -1.8579666574289866E+00_real64, &
            -1.6813163762825152E+00_real64, -1.9167636546384372E+00_real64, -2.0846749011355801E+00_real64]
        real(real64), parameter :: UPPER(12) = [ &
            1.2556333042474619E+00_real64, -3.3071673141360103E-01_real64, -1.5266180813902142E-01_real64, &
            8.3531793129412368E-01_real64, 3.0365078256169835E+00_real64, 2.0959225984271259E+00_real64, &
            2.2273246486332847E+00_real64, 3.0869446073597970E+00_real64, 1.2573249725426199E+00_real64, &
            9.5120302329361595E-01_real64, 2.2632390054702940E+00_real64, -4.1340787099367371E-01_real64]
        real(real64), parameter :: RHOBEG = 4.1781675753547659E-01_real64

        real(real64) :: x(12), fmin, f_start
        type(pf_optimize_info) :: info
        logical :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))
        logical :: underflow_saved

        x = START
        f_start = brown_almost_linear(START)
        ! Held off, read and restored in this body, never in a helper: see `traps_can_be_held`.
        ! The `1e-300` curvature raises underflow as well, which is put back with the rest.
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
        call ieee_get_flag(ieee_underflow, underflow_saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        call pf_minimize_bobyqa(brown_almost_linear, x, fmin, lower=LOWER, upper=UPPER, &
                                rhobeg=RHOBEG, rhoend=1.0e-6_real64, npt=14, max_neval=6000, &
                                info=info)
        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, saved)
        call ieee_set_flag(ieee_underflow, underflow_saved)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif

        ! The survival assertions first, because they hold on every target; whether the site was
        ! reached at all is the question the skip below answers.
        call check(error, fmin < f_start, "the run must still improve on its own start")
        if (allocated(error)) return
        call check(error, all(x >= LOWER) .and. all(x <= UPPER), &
            "and answer inside the bounds it was given")
        if (allocated(error)) return
        call check(error, info%neval > 100, "vacuity guard: the run stopped before the site")
        if (allocated(error)) return
        if (.not. any(raised)) then
            call skip_test(error, "this build's search path does not reach the geometry step's " // &
                "site, so only the survival assertions above ran: see the FMA paragraph in this " // &
                "test's doc-comment before re-aiming the fixture. If the site has instead been " // &
                "GUARDED in the engine, flip this test to assert that NO flag is raised.")
            return
        end if

    end subroutine test_bobyqa_geometry_step_overflow

    !> An objective of order `1e300` runs to an answer, raising overflow inside the model.
    !!
    !! The other half of the same reproducer: every value the objective returns is finite, and the
    !! sums of squares the model forms from them are not. **Scaling the objective is the caller's
    !! job** and `prima.md` says so; what this pins is that the engine still returns a usable point
    !! rather than a NaN, and that a build which halts on the flags meets them here. As above, the
    !! assertion is that SOME exception is raised, not which one.
    !!
    !! **The tolerances are loose on purpose.** A run whose model arithmetic overflows ends on
    !! `PF_OPT_ROUNDOFF` wherever rounding first blocks it, and that point moves with the last bit of
    !! every value the model forms -- by `1.5e-2` in `x` between arm64 and x86-64 under nagfor 7.2,
    !! which contract `a*b + c` differently; see
    !! `fortran-gotchas.md`, "One target contracts to an FMA and another cannot".
    !! What survives that is the DIRECTION of the answer and the SIZE of the improvement, not their
    !! digits, so `x` is asserted against `(1, 1)` at a tenth -- against a start `2.2` away -- and
    !! `fmin` against the value at the start.
    subroutine test_bobyqa_huge_objective_values(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64), parameter :: START(2) = [-1.2_real64, 1.0_real64]

        real(real64) :: x(2), fmin, f_start
        type(pf_optimize_info) :: info
        logical :: halting(size(ieee_usual)), saved(size(ieee_usual)), raised(size(ieee_usual))

        x = START
        f_start = rosenbrock_1e300(START)
        ! Held off, read and restored in this body, never in a helper: see `traps_can_be_held`.
        halting = .false.
        call ieee_get_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) then
            call ieee_get_halting_mode(ieee_usual, halting)
            call ieee_set_halting_mode(ieee_usual, .false.)
        end if
#endif
        call ieee_set_flag(ieee_usual, .false.)
        call pf_minimize_bobyqa(rosenbrock_1e300, x, fmin, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=info)
        call ieee_get_flag(ieee_usual, raised)
        call ieee_set_flag(ieee_usual, saved)
#ifndef __flang__
        if (traps_can_be_held()) call ieee_set_halting_mode(ieee_usual, halting)
#endif

        call check(error, any(raised), &
            "an objective near 1e300 no longer trips the model's arithmetic: re-aim the reproducer")
        if (allocated(error)) return
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-1_real64, &
            "the answer is still Rosenbrock's minimiser, which scaling the values does not move")
        if (allocated(error)) return
        ! Against the value at the START rather than an absolute ceiling: both are figures about
        ! where the run stopped, and only the ratio is a claim about the ENGINE.
        call check(error, fmin >= 0.0_real64 .and. fmin < 1.0e-2_real64*f_start, &
            "and the value there is the scaled zero, not a NaN or the starting value")
        if (allocated(error)) return
        call check(error, info%neval > 50, "vacuity guard: the run stopped before the model site")

    end subroutine test_bobyqa_huge_objective_values

#ifndef __flang__
    !> The processor can turn halting off for every flag in `ieee_usual`.
    !!
    !! nagfor halts on overflow, invalid and divide-by-zero by default, so a test running a
    !! configuration known to raise one would take the whole runner down before it could assert
    !! anything; holding halting off around the call keeps the finding a test result. **That bracket
    !! is written out in each test's own body, and this inquiry is the only part a helper may
    !! carry**: F2018 17.3 restores the halting modes on return from any procedure other than
    !! `ieee_set_halting_mode`, and quietens a flag signalling on entry to a procedure until it
    !! returns. nagfor does both, so a helper that set the modes changed nothing its caller ran
    !! under, and one that read the flags reported none raised (flang does the flag half too). The
    !! flags are restored afterwards rather than left raised: what a test deliberately provoked is
    !! not a finding for whatever runs next on this thread. `ieee_set_halting_mode` does not link
    !! under flang on macOS, which is what the preprocessor guard is for.
    function traps_can_be_held() result(can)
        logical :: can !! `ieee_support_halting` holds for overflow, invalid and divide-by-zero

        can = ieee_support_halting(ieee_overflow) .and. ieee_support_halting(ieee_invalid) &
            .and. ieee_support_halting(ieee_divide_by_zero)

    end function traps_can_be_held
#endif

    !> A `scale=` run and a run on the hand-scaled objective agree bit for bit.
    !!
    !! This is what "the engine minimises `g(y) = f(scale*y)`" has to mean: the same sequence of
    !! points, the same values, and the record back in the caller's units. Bit equality, not a
    !! tolerance -- the two runs are the same arithmetic, so anything else is a defect.
    subroutine test_bobyqa_scale_matches_hand_scaling(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: xs(2), xh(2), fs, fh
        type(pf_optimize_history) :: scaled, hand
        integer :: k

        xs = [3.0_real64, -2.0_real64] * BAD_SCALING_SCALE
        xh = [3.0_real64, -2.0_real64]

        call pf_minimize_bobyqa(bad_scaling, xs, fs, scale=BAD_SCALING_SCALE, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, history=scaled)
        call pf_minimize_bobyqa(bad_scaling_unit, xh, fh, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, history=hand)

        call check(error, scaled%n == hand%n .and. scaled%n > 5, &
                   "the two runs evaluated different numbers of points")
        if (allocated(error)) return
        do k = 1, scaled%n
            call check(error, scaled%f(k) == hand%f(k), &
                       "a scale= run and the hand-scaled run disagree on an objective value")
            if (allocated(error)) return
            call check(error, all(scaled%x(:, k) == BAD_SCALING_SCALE * hand%x(:, k)), &
                       "the recorded points are not the caller's units of the hand-scaled ones")
            if (allocated(error)) return
        end do
        call check(error, fs == fh, "the two runs reached different minima")
        if (allocated(error)) return
        call check(error, all(xs == BAD_SCALING_SCALE * xh), &
                   "the returned points are not the same point in the two units")

    end subroutine test_bobyqa_scale_matches_hand_scaling

    !> `scale=` reaches a badly conditioned minimum inside a budget the unscaled run cannot.
    !!
    !! The negative control is built in: the SAME call without `scale=` is made first, and the
    !! test asserts it does NOT get there. Without that arm the scaled run's success would prove
    !! nothing about the argument.
    subroutine test_bobyqa_scale_earns_its_place(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, start(2)
        type(pf_optimize_info) :: plain_info, scaled_info

        start = [3.0_real64, -2.0_real64] * BAD_SCALING_SCALE

        ! The same radii and the same budget for both arms; the only difference is `scale=`.
        ! Thirty-five evaluations is where the two separate: the scaled run has converged by 33
        ! and the unscaled one is still two coordinate-magnitudes away.
        x = start
        call pf_minimize_bobyqa(bad_scaling, x, fmin, rhobeg=0.1_real64, &
                                rhoend=1.0e-8_real64, max_neval=35, info=plain_info)
        call check(error, plain_info%status == PF_OPT_LIMIT, &
                   "the unscaled run converged inside the budget: the scaled arm proves nothing")
        if (allocated(error)) return
        call check(error, maxval(abs(x - BAD_SCALING_MIN)/BAD_SCALING_SCALE) > 1.0_real64, &
                   "the unscaled run reached the minimum: the scaled arm below proves nothing")
        if (allocated(error)) return

        x = start
        call pf_minimize_bobyqa(bad_scaling, x, fmin, scale=BAD_SCALING_SCALE, &
                                rhobeg=0.1_real64, rhoend=1.0e-8_real64, max_neval=35, &
                                info=scaled_info)
        call check(error, maxval(abs(x - BAD_SCALING_MIN)/BAD_SCALING_SCALE) < 1.0e-10_real64, &
                   "the scaled run should reach the minimum in every coordinate's own units")
        if (allocated(error)) return
        call check(error, fmin < 1.0e-20_real64, "the scaled run should reach the minimum value")
        if (allocated(error)) return
        call check(error, scaled_info%status == PF_OPT_OK .and. scaled_info%neval < 35, &
                   "the scaled run should converge on its own rule, inside the budget")

    end subroutine test_bobyqa_scale_earns_its_place

    !> `converged=` equals `info%converged` on every exit the three PRIMA engines reach, and is set
    !! whether or not `info` was asked for.
    !!
    !! **Every actual is seeded with the WRONG answer before each call**, so a path that never
    !! assigns `converged` is caught rather than passing on a lucky initial value. All three
    !! engines report through one `finish_run`, so the flag and the component come from a single
    !! expression there; what this test pins is that every driver actually FORWARDS the argument,
    !! which is three separate call sites and the place a new engine would forget.
    !!
    !! BOBYQA and LINCOA are exercised through both specifics, the object form and the
    !! plain-function form that forwards to it; COBYLA takes an object only.
    !!
    !! Mutation proved: drop `converged=converged` from any one of the three `finish_run` calls
    !! (`src/parquet_prima_{bobyqa,lincoa,cobyla}.f90`) and this test fails on that engine.
    subroutine test_prima_converged_argument_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64)             :: x(2), x3(3), fmin, lo(3), hi(3), a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info)   :: info
        type(shifted_quadratic)  :: obj
        type(disc_fit)           :: cobj
        logical                  :: flag

        ! ---- BOBYQA: LIMIT, then TARGET, then the object form ----
        lo = -5.0_real64
        hi = 10.0_real64
        x = [-1.2_real64, 1.0_real64]
        flag = .true.
        call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lo(1:2), upper=hi(1:2), &
                                rhobeg=0.5_real64, rhoend=1.0e-10_real64, max_neval=12, &
                                converged=flag, info=info)
        call check(error, info%status == PF_OPT_LIMIT, "bobyqa LIMIT: the fixture must spend its budget")
        if (allocated(error)) return
        call check(error, flag .eqv. info%converged, "bobyqa LIMIT: converged= must equal info%converged")
        if (allocated(error)) return
        call check(error, .not. flag, "bobyqa LIMIT: the flag must be false on a spent budget")
        if (allocated(error)) return

        x = [-1.2_real64, 1.0_real64]
        flag = .true.
        call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lo(1:2), upper=hi(1:2), &
                                rhobeg=0.5_real64, rhoend=1.0e-10_real64, max_neval=12, &
                                converged=flag)
        call check(error, .not. flag, "bobyqa LIMIT: the flag must be cleared with no info= present")
        if (allocated(error)) return

        lo = -5.0_real64
        hi = 5.0_real64
        x3 = [1.0_real64, -2.0_real64, 1.5_real64]
        flag = .false.
        call pf_minimize_bobyqa(sphere, x3, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, ftarget=1.0e-4_real64, converged=flag, info=info)
        call check(error, info%status == PF_OPT_TARGET, "bobyqa TARGET: the fixture must meet its target")
        if (allocated(error)) return
        call check(error, flag .eqv. info%converged, "bobyqa TARGET: converged= must equal info%converged")
        if (allocated(error)) return
        call check(error, flag, "bobyqa TARGET: a met target counts as converged")
        if (allocated(error)) return

        x = 0.0_real64
        flag = .false.
        call pf_minimize_bobyqa(obj, x, fmin, rhobeg=0.3_real64, rhoend=1.0e-9_real64, &
                                converged=flag, info=info)
        call check(error, flag .eqv. info%converged, "bobyqa object: converged= must equal info%converged")
        if (allocated(error)) return

        ! ---- LINCOA: both specifics ----
        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64
        x = 0.0_real64
        flag = .false.
        call pf_minimize_lincoa(sphere, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.3_real64, rhoend=1.0e-9_real64, converged=flag, info=info)
        call check(error, flag .eqv. info%converged, "lincoa function: converged= must equal info%converged")
        if (allocated(error)) return
        call check(error, flag, "lincoa function: this fixture converges")
        if (allocated(error)) return
        x = 0.0_real64
        flag = .false.
        call pf_minimize_lincoa(obj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.3_real64, rhoend=1.0e-9_real64, converged=flag)
        call check(error, flag, "lincoa object: the flag must be set with no info= present")
        if (allocated(error)) return

        ! ---- COBYLA: a converged run, then the infeasible one ----
        x = 0.0_real64
        flag = .false.
        call pf_minimize_cobyla(cobj, x, fmin, converged=flag, info=info)
        call check(error, flag .eqv. info%converged, "cobyla OK: converged= must equal info%converged")
        if (allocated(error)) return

        a_ineq(1, :) = [-1.0_real64, 0.0_real64]
        b_ineq = -2.0_real64
        x = 0.0_real64
        flag = .true.
        call pf_minimize_cobyla(cobj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, converged=flag, info=info)
        call check(error, info%status == PF_OPT_INFEASIBLE, &
                   "cobyla INFEASIBLE: the fixture must report an infeasible answer")
        if (allocated(error)) return
        call check(error, flag .eqv. info%converged, "cobyla INFEASIBLE: converged= must equal info%converged")
        if (allocated(error)) return
        call check(error, .not. flag, "cobyla INFEASIBLE: an infeasible answer is never convergence")
        if (allocated(error)) return
        x = 0.0_real64
        flag = .true.
        call pf_minimize_cobyla(cobj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, converged=flag)
        call check(error, .not. flag, "cobyla INFEASIBLE: the flag must be cleared with no info= present")

    end subroutine test_prima_converged_argument_agrees

    !> A budget too small to converge is `PF_OPT_LIMIT`, not an error, and the best point so far.
    subroutine test_bobyqa_budget(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lo(2), hi(2), fstart
        type(pf_optimize_info) :: info

        lo = -5.0_real64
        hi = 10.0_real64
        x = [-1.2_real64, 1.0_real64]
        fstart = rosenbrock(x)
        call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, max_neval=12, info=info)

        call check(error, info%status == PF_OPT_LIMIT, "a spent budget is PF_OPT_LIMIT")
        if (allocated(error)) return
        call check(error, .not. info%converged, "PF_OPT_LIMIT is not converged")
        if (allocated(error)) return
        call check(error, info%neval <= 12, &
                   "the budget must not be overrun at all")
        if (allocated(error)) return
        call check(error, fmin <= fstart, "the best point so far cannot be worse than the start")
        if (allocated(error)) return
        call check(error, fmin == rosenbrock(x), "fmin must be the value at the returned point")

    end subroutine test_bobyqa_budget

    !> `ftarget` stops the run as soon as a value at or below it is found.
    subroutine test_bobyqa_ftarget(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(3), fmin, lo(3), hi(3)
        type(pf_optimize_info) :: full, targeted

        lo = -5.0_real64
        hi = 5.0_real64

        x = [1.0_real64, -2.0_real64, 1.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, info=full)

        x = [1.0_real64, -2.0_real64, 1.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, ftarget=1.0e-4_real64, info=targeted)

        call check(error, targeted%status == PF_OPT_TARGET, "a met target is PF_OPT_TARGET")
        if (allocated(error)) return
        call check(error, targeted%converged, "PF_OPT_TARGET counts as converged")
        if (allocated(error)) return
        call check(error, fmin <= 1.0e-4_real64, "the target was reported met without being met")
        if (allocated(error)) return
        call check(error, targeted%neval < full%neval, &
                   "a target that fires early must cost fewer evaluations than running to rhoend")

    end subroutine test_bobyqa_ftarget

    !> The record holds one entry per evaluation, in order, in the caller's units.
    subroutine test_bobyqa_history(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lo(2), hi(2)
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        integer :: k

        lo = -3.0_real64
        hi = 3.0_real64
        x = [2.0_real64, -1.0_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.4_real64, &
                                rhoend=1.0e-9_real64, info=info, history=record)

        call check(error, record%n == info%neval, &
                   "the record should hold exactly one entry per evaluation")
        if (allocated(error)) return
        call check(error, size(record%f) == record%n .and. size(record%x, 2) == record%n, &
                   "the record should come back trimmed to the entries in use")
        if (allocated(error)) return
        do k = 1, record%n
            call check(error, record%f(k) == sphere(record%x(:, k)), &
                       "a recorded value is not the objective at the recorded point")
            if (allocated(error)) return
        end do
        call check(error, minval(record%f) == fmin, &
                   "the returned minimum is not the least value in the record")

    end subroutine test_bobyqa_history

    !> The object form and the plain-function form walk the same path.
    subroutine test_bobyqa_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(table_sphere) :: obj
        real(real64) :: xa(3), xb(3), fa, fb, lo(3), hi(3)
        type(pf_optimize_history) :: ra, rb
        integer :: k

        ! `sphere` is the sum of squares about 1, so this centre makes the two the
        ! same function -- and the same arithmetic, which is what bit equality needs.
        obj%centre = [1.0_real64, 1.0_real64, 1.0_real64]
        lo = -4.0_real64
        hi = 4.0_real64
        xa = [1.0_real64, -2.0_real64, 0.5_real64]
        xb = xa

        call pf_minimize_bobyqa(obj, xa, fa, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-9_real64, history=ra)
        call pf_minimize_bobyqa(sphere, xb, fb, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-9_real64, history=rb)

        call check(error, ra%n == rb%n .and. ra%n > 5, "the two forms evaluated different counts")
        if (allocated(error)) return
        do k = 1, ra%n
            call check(error, ra%f(k) == rb%f(k) .and. all(ra%x(:, k) == rb%x(:, k)), &
                       "the object and plain-function forms diverged")
            if (allocated(error)) return
        end do
        call check(error, fa == fb .and. all(xa == xb), "the two forms returned different answers")

    end subroutine test_bobyqa_forms_agree

    !> A fuller interpolation model costs more evaluations per step and still converges.
    subroutine test_bobyqa_npt(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(3), fmin, lo(3), hi(3)
        type(pf_optimize_info) :: lean, full

        lo = -4.0_real64
        hi = 4.0_real64

        x = [1.0_real64, -2.0_real64, 0.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-9_real64, npt=2*3 + 1, info=lean)

        ! (n+1)(n+2)/2 = 10 for n = 3: the largest model BOBYQA accepts.
        x = [1.0_real64, -2.0_real64, 0.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-9_real64, npt=10, info=full)

        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-6_real64, &
                   "a full quadratic model should still reach the sphere's minimiser")
        if (allocated(error)) return
        call check(error, full%neval > lean%neval, &
                   "a model of ten points cannot cost fewer evaluations than one of seven")

    end subroutine test_bobyqa_npt

    !> `pf_bobyqa_solver` under the multistart driver finds both wells of `twin_wells`.
    subroutine test_bobyqa_solver_multistart(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(pf_bobyqa_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)
        type(pf_optimize_info) :: info

        lo = [-2.0_real64, -2.0_real64]
        hi = [2.0_real64, 2.0_real64]
        solver%rhoend = 1.0e-9_real64

        call pf_minimize_multistart(twin_wells, lo, hi, 20260916_int64, x, fmin, nstart=12, &
                                    solver=solver, info=info)

        call check(error, fmin < 1.0e-12_real64, &
                   "twin_wells has two minima of value 0; the driver should reach one")
        if (allocated(error)) return
        call check(error, abs(abs(x(1)) - 1.0_real64) < 1.0e-5_real64 .and. abs(x(2)) < 1.0e-5_real64, &
                   "the answer should be one of the two wells, at (+/-1, 0)")
        if (allocated(error)) return
        call check(error, info%nminima >= 2, &
                   "twelve starts over this box should find both wells, not one")
        if (allocated(error)) return
        call check(error, info%nlimit == 0, "no BOBYQA run here should end on its budget")

    end subroutine test_bobyqa_solver_multistart

    !> The solver object's options reach the runs: a budget of six ends every start on it.
    subroutine test_bobyqa_solver_options(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(pf_bobyqa_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)
        type(pf_optimize_info) :: info

        lo = [-2.0_real64, -2.0_real64]
        hi = [2.0_real64, 2.0_real64]
        solver%max_neval = 6

        call pf_minimize_multistart(twin_wells, lo, hi, 20260916_int64, x, fmin, nstart=5, &
                                    solver=solver, info=info)

        call check(error, info%nlimit == 5, &
                   "a max_neval of six should end every one of the five starts on its budget")
        if (allocated(error)) return
        call check(error, info%neval <= 5 * (6 + 1), &
                   "the driver's total cannot exceed five budgets, soft by one step each")

    end subroutine test_bobyqa_solver_options

    !> LINCOA on a quadratic with one equality and one inequality, both active at the answer.
    !!
    !! Minimise `|x - (1, 2, 3)|^2` subject to `x1 + x2 + x3 = 3` and `x1 >= 1`, written as
    !! `-x1 <= -1`. The KKT point is derived here rather than recorded:
    !!
    !! The unconstrained minimiser `(1, 2, 3)` has coordinate sum `6`, so the equality binds.
    !! Projecting onto the plane gives `(0, 1, 2)`, whose first coordinate is below `1`, so the
    !! inequality binds too. With `x1 = 1` fixed, minimising `(x2-2)^2 + (x3-3)^2` on
    !! `x2 + x3 = 2` projects `(2, 3)` onto that line and gives `(0.5, 1.5)`. So
    !! `x* = (1, 0.5, 1.5)` and `f* = 0 + 2.25 + 2.25 = 4.5`.
    !!
    !! Stationarity confirms both multipliers are non-zero, which is what makes this a test of the
    !! active set rather than of a projection: `grad f = (0, -3, -3)`, and
    !! `(0, -3, -3) + lambda*(1, 1, 1) + mu*(-1, 0, 0) = 0` gives `lambda = 3` and `mu = 3 >= 0`.
    subroutine test_lincoa_kkt_point(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(3), fmin, a_ineq(1, 3), b_ineq(1), a_eq(1, 3), b_eq(1)
        real(real64), parameter :: EXPECTED(3) = [1.0_real64, 0.5_real64, 1.5_real64]
        type(pf_optimize_info) :: info

        a_eq(1, :) = 1.0_real64
        b_eq = 3.0_real64
        a_ineq(1, :) = [-1.0_real64, 0.0_real64, 0.0_real64]
        b_ineq = -1.0_real64
        ! A feasible start: the coordinates sum to 3 and the first is exactly on its bound.
        x = 1.0_real64
        call pf_minimize_lincoa(sphere123, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, a_eq=a_eq, &
                                b_eq=b_eq, rhobeg=0.5_real64, rhoend=1.0e-9_real64, info=info)

        call check(error, maxval(abs(x - EXPECTED)) < 1.0e-6_real64, &
                   "LINCOA stopped away from the KKT point (1, 0.5, 1.5) derived above")
        if (allocated(error)) return
        ! The tolerance is the accuracy the run was asked for, propagated: `x` is right to about
        ! `rhoend`, the gradient there has length `|(0, -3, -3)| = 4.24`, and the equality is
        ! satisfied to about `ctol`, so a few times `1e-8` is the size of the error to expect.
        call check(error, abs(fmin - 4.5_real64) < 1.0e-6_real64, &
                   "the value at the KKT point is 4.5")
        if (allocated(error)) return
        call check(error, info%cstrv <= sqrt(epsilon(1.0_real64)), &
                   "the answer must be feasible; info%cstrv says it is not")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK .and. info%converged, &
                   "a feasible converged run is PF_OPT_OK")
        if (allocated(error)) return
        call check(error, info%neval < 400, "LINCOA needed far more evaluations than expected")

    end subroutine test_lincoa_kkt_point

    !> The point LINCOA returns is the best FEASIBLE point in its record, not its last iterate.
    !!
    !! **LINCOA evaluates infeasible points**, which is upstream's design and worth knowing: the
    !! initial interpolation set is `x0` displaced by `+/-rhobeg` along each coordinate whether
    !! that leaves the region or not, and a geometry-improving step need not be feasible either.
    !! Only the trust-region iterates are feasible by construction. What makes the answer sound is
    !! therefore not the search but the FILTER: every evaluated point is offered to `savefilt`,
    !! and `selectx` chooses from it at the end.
    !!
    !! WHAT THIS FORBIDS: returning the last iterate, or the lowest value regardless of
    !! feasibility. Both are cheaper and both are wrong; the vacuity guards below fail the test if
    !! the record holds no infeasible point with a lower value than the answer, which is exactly
    !! the case that separates the three rules.
    subroutine test_lincoa_returns_the_filtered_best(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, a_ineq(2, 2), b_ineq(2)
        real(real64) :: viol, best_feasible, lowest_infeasible
        real(real64), parameter :: CTOL = sqrt(epsilon(1.0_real64))
        integer :: k, nfeasible, ninfeasible
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record

        ! x1 + x2 <= 1 and -x1 + x2 <= 1: a wedge whose corner is at (0, 1). The minimum of
        ! |x - (1, 2)|^2 over it is that corner -- moving along either edge away from it increases
        ! the distance to (1, 2), which lies beyond the corner in the direction the wedge closes.
        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        a_ineq(2, :) = [-1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64
        x = [0.0_real64, 0.95_real64]
        call pf_minimize_lincoa(dist12, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.5_real64, rhoend=1.0e-9_real64, info=info, &
                                history=record)

        call check(error, maxval(abs(x - [0.0_real64, 1.0_real64])) < 1.0e-6_real64, &
                   "the constrained minimum is the corner (0, 1)")
        if (allocated(error)) return
        call check(error, info%cstrv <= CTOL, "the answer must be feasible")
        if (allocated(error)) return

        nfeasible = 0
        ninfeasible = 0
        best_feasible = huge(1.0_real64)
        lowest_infeasible = huge(1.0_real64)
        do k = 1, record%n
            viol = max(0.0_real64, maxval(matmul(a_ineq, record%x(:, k)) - b_ineq))
            if (viol <= CTOL) then
                nfeasible = nfeasible + 1
                best_feasible = min(best_feasible, record%f(k))
            else
                ninfeasible = ninfeasible + 1
                lowest_infeasible = min(lowest_infeasible, record%f(k))
            end if
        end do

        ! Vacuity guards: without an infeasible point that beats the answer, "best feasible" and
        ! "lowest of all" are the same rule and the test would pass either way.
        call check(error, nfeasible >= 2 .and. ninfeasible >= 1, &
                   "the record must hold feasible AND infeasible points for this test to mean " // &
                   "anything")
        if (allocated(error)) return
        call check(error, lowest_infeasible < best_feasible, &
                   "an infeasible point must beat every feasible one on value, or returning " // &
                   "the lowest value outright would pass this test")
        if (allocated(error)) return
        call check(error, abs(fmin - best_feasible) < 1.0e-12_real64, &
                   "the value returned is not the lowest among the feasible points evaluated")

    end subroutine test_lincoa_returns_the_filtered_best

    !> The object form and the plain-function form of LINCOA run the same search.
    subroutine test_lincoa_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: xf(2), xo(2), fminf, fmino, a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: infof, infoo
        type(shifted_quadratic) :: obj

        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64
        ! `sphere` and `shifted_quadratic` are the same function: the sum of squares about 1.
        xf = 0.0_real64
        call pf_minimize_lincoa(sphere, xf, fminf, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.3_real64, rhoend=1.0e-9_real64, info=infof)
        xo = 0.0_real64
        call pf_minimize_lincoa(obj, xo, fmino, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.3_real64, rhoend=1.0e-9_real64, info=infoo)

        call check(error, all(xf == xo), "the two forms answered different points")
        if (allocated(error)) return
        call check(error, fminf == fmino, "the two forms answered different values")
        if (allocated(error)) return
        call check(error, infof%neval == infoo%neval .and. obj%ncall == infoo%neval, &
                   "the two forms spent different numbers of evaluations")
        if (allocated(error)) return
        ! The minimum of |x - (1, 1)|^2 on x1 + x2 <= 1 is the projection of (1, 1) onto the line,
        ! namely (0.5, 0.5), where the value is 0.5.
        call check(error, maxval(abs(xo - 0.5_real64)) < 1.0e-6_real64 .and. &
                   abs(fmino - 0.5_real64) < 1.0e-6_real64, &
                   "the constrained minimum is (0.5, 0.5) with value 0.5")

    end subroutine test_lincoa_forms_agree

    !> COBYLA on a constraint whose FEASIBLE side is the one the objective dislikes.
    !!
    !! `outside_disc` minimises `|x|^2` subject to `1 - |x|^2 <= 0`, so the feasible region is
    !! everything OUTSIDE the unit circle and the answer sits on the circle with value `1`. Under
    !! SciPy's opposite convention the same constraint function would mean "inside", and the
    !! answer would be the origin with value `0`.
    !!
    !! WHAT THIS FORBIDS: changing the sign convention without renaming the binding
    !! (`feature_optimizer.md` 8). The two answers are `1` and `0`, so a flipped sign cannot pass
    !! this by a tolerance.
    subroutine test_cobyla_sign_of_the_constraint(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info
        type(outside_disc) :: obj

        x = [2.0_real64, 0.5_real64]
        call pf_minimize_cobyla(obj, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-9_real64, info=info)

        call check(error, abs(sum(x**2) - 1.0_real64) < 1.0e-6_real64, &
                   "the answer must lie ON the unit circle; a flipped constraint sign puts it " // &
                   "at the origin instead")
        if (allocated(error)) return
        call check(error, abs(fmin - 1.0_real64) < 1.0e-6_real64, &
                   "the constrained minimum value is 1, not 0")
        if (allocated(error)) return
        call check(error, info%cstrv <= sqrt(epsilon(1.0_real64)), &
                   "the answer must be feasible")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK .and. info%converged, &
                   "a feasible converged run is PF_OPT_OK")

    end subroutine test_cobyla_sign_of_the_constraint

    !> COBYLA with a linear constraint beside the nonlinear one, both active at the answer.
    !!
    !! `disc_fit` minimises `|x - (1, 2)|^2` inside the unit disc; the disc alone would answer
    !! `(1, 2)/sqrt(5)`, whose first coordinate is about `0.447`. Adding `x1 <= 0.3` cuts that off,
    !! so `x1 = 0.3` and, since `2` is above the top of the disc at that abscissa,
    !! `x2 = sqrt(1 - 0.09) = sqrt(0.91)`. Both constraints are active.
    subroutine test_cobyla_linear_and_nonlinear(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, a_ineq(1, 2), b_ineq(1)
        real(real64) :: expected(2)
        type(pf_optimize_info) :: info
        type(disc_fit) :: obj

        expected = [0.3_real64, sqrt(0.91_real64)]
        a_ineq(1, :) = [1.0_real64, 0.0_real64]
        b_ineq = 0.3_real64
        x = 0.0_real64
        call pf_minimize_cobyla(obj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, rhobeg=0.5_real64, &
                                rhoend=1.0e-9_real64, info=info)

        call check(error, maxval(abs(x - expected)) < 1.0e-6_real64, &
                   "the answer is where the disc meets x1 = 0.3")
        if (allocated(error)) return
        call check(error, abs(fmin - sum((expected - [1.0_real64, 2.0_real64])**2)) < 1.0e-8_real64, &
                   "the value is the squared distance from (1, 2) at that point")
        if (allocated(error)) return
        call check(error, info%cstrv <= sqrt(epsilon(1.0_real64)) .and. info%converged, &
                   "the answer must be feasible and the run converged")

    end subroutine test_cobyla_linear_and_nonlinear

    !> Contradictory constraints are not an error: the run returns the least violating point.
    !!
    !! The unit disc and `x1 >= 2` have no point in common. Nothing is printed, `info%status` is
    !! `PF_OPT_INFEASIBLE`, `converged` is false, and `info%cstrv` is the violation at the point
    !! returned -- which must be no worse than the violation at the start, or the run gave back
    !! something worse than it was handed.
    subroutine test_cobyla_infeasible(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, a_ineq(1, 2), b_ineq(1), start_violation
        type(pf_optimize_info) :: info
        type(disc_fit) :: obj

        a_ineq(1, :) = [-1.0_real64, 0.0_real64]
        b_ineq = -2.0_real64
        x = 0.0_real64
        ! At the start the disc is satisfied and `-x1 <= -2` is violated by 2.
        start_violation = 2.0_real64
        call pf_minimize_cobyla(obj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, info=info)

        call check(error, info%status == PF_OPT_INFEASIBLE, &
                   "contradictory constraints must report PF_OPT_INFEASIBLE")
        if (allocated(error)) return
        call check(error, .not. info%converged, "an infeasible answer never counts as converged")
        if (allocated(error)) return
        call check(error, info%cstrv > sqrt(epsilon(1.0_real64)), &
                   "PF_OPT_INFEASIBLE and a feasible cstrv would contradict each other")
        if (allocated(error)) return
        call check(error, info%cstrv <= start_violation, &
                   "the point returned violates the constraints more than the start did")

    end subroutine test_cobyla_infeasible

    !> `info%cstrv` is the violation at the returned point in the CALLER's units, both when the
    !> answer is feasible and when it is not.
    !!
    !! Two arms, because the property has two halves and only the second can fail by a factor.
    !!
    !! **Feasible**: `bad_scaling`'s minimiser is `(1e-3, 1e3)`; an upper bound of `5e-4` on the
    !! first coordinate cuts it off, so the answer sits on that bound, and a bound-active answer
    !! must not be reported as infeasible.
    !!
    !! **Infeasible**: the unit disc and `x1 >= 2` have no point in common, so COBYLA returns the
    !! least-violating point it found and the violation is a real number rather than a rounding
    !! residue. Under `scale = (10, 1)` the bound rows reach the engine divided by `scale`, so the
    !! engine's own violation is a TENTH of the caller's -- and the test computes the caller's
    !! itself, from the returned `x` and the constraints as written, and demands that
    !! `info%cstrv` equal it.
    !!
    !! WHAT THIS FORBIDS: taking `info%cstrv` or the `PF_OPT_INFEASIBLE` verdict from the engine's
    !! own violation rather than recomputing it (`feature_optimizer.md` 8, last entry). The two
    !! differ by `scale` on every bound row, and by each row's gradient length in LINCOA, which
    !! normalises them. The vacuity guard is the third assertion: the bound's violation must be
    !! the largest one, since it is the only one `scale=` rescales.
    subroutine test_cstrv_is_in_the_callers_units(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lo(2), hi(2)
        real(real64) :: bound_violation, disc_violation
        real(real64), parameter :: CTOL = sqrt(epsilon(1.0_real64))
        real(real64), parameter :: SCALED(2) = [10.0_real64, 1.0_real64]
        type(pf_optimize_info) :: info
        type(disc_fit) :: disc

        ! ---- feasible: the answer sits on a bound, under an extreme scale --------------------
        lo = [0.0_real64, 0.0_real64]
        hi = [5.0e-4_real64, 1.0e4_real64]
        x = [2.0e-4_real64, 500.0_real64]
        call pf_minimize_lincoa(bad_scaling, x, fmin, lower=lo, upper=hi, &
                                scale=BAD_SCALING_SCALE, rhobeg=0.1_real64, &
                                rhoend=1.0e-10_real64, info=info)

        ! The bound binds: the free minimiser's first coordinate is 1e-3, twice the bound.
        call check(error, abs(x(1) - hi(1)) < 1.0e-9_real64, &
                   "the first coordinate must sit on its upper bound")
        if (allocated(error)) return
        call check(error, abs(x(2) - BAD_SCALING_MIN(2)) < 1.0e-3_real64, &
                   "the second coordinate is unbounded here and must reach its own minimiser")
        if (allocated(error)) return
        call check(error, info%cstrv <= CTOL .and. info%status /= PF_OPT_INFEASIBLE, &
                   "a feasible point must not be reported as infeasible")
        if (allocated(error)) return

        ! ---- infeasible: the violation is a number, and it is the caller's number ------------
        lo = [2.0_real64, -10.0_real64]
        hi = [50.0_real64, 10.0_real64]
        x = [2.0_real64, 0.0_real64]
        call pf_minimize_cobyla(disc, x, fmin, lower=lo, upper=hi, scale=SCALED, &
                                rhobeg=0.2_real64, rhoend=1.0e-9_real64, info=info)

        bound_violation = max(0.0_real64, maxval(lo - x), maxval(x - hi))
        disc_violation = max(0.0_real64, x(1)**2 + x(2)**2 - 1.0_real64)

        call check(error, info%status == PF_OPT_INFEASIBLE, &
                   "the disc and x1 >= 2 have no point in common")
        if (allocated(error)) return
        ! Vacuity guard: `scale=` rescales the BOUND rows and nothing else, so unless the bound is
        ! the binding violation the two units agree and this arm tests nothing.
        call check(error, bound_violation > disc_violation, &
                   "the bound must be the largest violation, or the engine's units and the " // &
                   "caller's would agree and this test would pass either way")
        if (allocated(error)) return
        call check(error, abs(info%cstrv - bound_violation) <= 1.0e-12_real64*bound_violation, &
                   "info%cstrv must equal the violation computed from the returned x and the " // &
                   "constraints as the caller wrote them")

    end subroutine test_cstrv_is_in_the_callers_units

    !> The six calls of `feature_optimizer.md` 5.9, which the guide page prints.
    !!
    !! Structural figures, not measurements (Q13): each answer is the analytic one worked out on
    !! the page, so the page and the code cannot drift apart without this failing.
    subroutine test_guide_examples(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, lower(2), upper(2)
        real(real64) :: a_ineq(1, 2), b_ineq(1), a_eq(1, 2), b_eq(1)
        real(real64), parameter :: ROOT5 = sqrt(5.0_real64)
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        type(disc_fit) :: disc

        ! 1. unconstrained BOBYQA on Rosenbrock, from the classic start.
        x = [-1.2_real64, 1.0_real64]
        call pf_minimize_bobyqa(rosenbrock, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-8_real64, &
                                info=info, history=record)
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-6_real64 .and. record%n == info%neval, &
                   "call 1 must reach (1, 1) and record every evaluation")
        if (allocated(error)) return

        ! 2. the same with x1 <= 0.5, where the free minimum is out of reach. On the bound the
        !    inner square vanishes at x2 = x1**2 = 0.25, leaving (1 - 0.5)**2 = 0.25.
        x = [-1.2_real64, 1.0_real64]
        lower = -2.0_real64
        upper = [0.5_real64, 2.0_real64]
        call pf_minimize_bobyqa(rosenbrock, x, fmin, lower=lower, upper=upper, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x - [0.5_real64, 0.25_real64])) < 1.0e-6_real64 .and. &
                   info%converged, "call 2 must stop at (0.5, 0.25) with converged true")
        if (allocated(error)) return

        ! 3. LINCOA with x1 + x2 <= 1 and x1 = x2: the equality puts the answer on the diagonal
        !    and the inequality caps it at (0.5, 0.5).
        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64
        a_eq(1, :) = [1.0_real64, -1.0_real64]
        b_eq = 0.0_real64
        x = 0.0_real64
        call pf_minimize_lincoa(dist12, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, a_eq=a_eq, &
                                b_eq=b_eq, rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x - 0.5_real64)) < 1.0e-6_real64, &
                   "call 3 must stop at (0.5, 0.5)")
        if (allocated(error)) return

        ! 4. COBYLA on the same distance inside the unit disc: the nearest point of the circle
        !    to (1, 2) is (1, 2)/sqrt(5), where the distance squared is 6 - 2*sqrt(5).
        !
        !    The POSITION is asserted at 1e-5 and the VALUE at 1e-6, which is the other way round
        !    from how it reads: COBYLA's last stage moves ALONG the circle, where the objective is
        !    flat to second order, so a tangential error of 2e-6 costs 4e-12 in the value and the
        !    linear model cannot see it -- rhoend bounds the trust region, never this. The value is
        !    the tighter claim because it is bounded by the FEASIBILITY tolerance instead: ctol is
        !    sqrt(epsilon), so the answer may sit 7e-9 outside the circle and dip 2e-8 below the
        !    constrained minimum, which is the figure 1e-6 leaves room for.
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x - [1.0_real64/ROOT5, 2.0_real64/ROOT5])) < 1.0e-5_real64, &
                   "call 4 must stop at (1, 2)/sqrt(5)")
        if (allocated(error)) return
        call check(error, abs(fmin - (6.0_real64 - 2.0_real64*ROOT5)) < 1.0e-6_real64, &
                   "and the value there must be 6 - 2*sqrt(5)")
        if (allocated(error)) return

        ! 5. the disc and x1 <= 0.3 together.
        a_ineq(1, :) = [1.0_real64, 0.0_real64]
        b_ineq = 0.3_real64
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x - [0.3_real64, sqrt(0.91_real64)])) < 1.0e-6_real64, &
                   "call 5 must stop where the disc meets x1 = 0.3")
        if (allocated(error)) return

        ! 6. the disc and x1 >= 2, which have no point in common.
        a_ineq(1, :) = [-1.0_real64, 0.0_real64]
        b_ineq = -2.0_real64
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, info=info)
        call check(error, info%status == PF_OPT_INFEASIBLE .and. info%cstrv > 0.0_real64, &
                   "call 6 must report PF_OPT_INFEASIBLE with a positive violation")

    end subroutine test_guide_examples

    !> A start whose coordinates sit exactly ON their bounds is kept there and still converges.
    !!
    !! `pf_minimize_bobyqa` fixes PRIMA's `honour_x0` at `.true.`, so a coordinate already at a
    !! bound stays there and `rhobeg` shrinks instead of the start moving. The engine's
    !! `initxf` then meets `sl(k) == 0` (start at the lower bound) and `su(k) == 0` (start at the
    !! upper bound) and builds that coordinate's two interpolation points on the one side it has
    !! room on -- the arms this test exists to reach. Both are set up here at once: the odd
    !! coordinates start on their lower bound and the even ones on their upper bound.
    !!
    !! The minimiser of `sphere` is 1 in every coordinate, and the box below has it strictly
    !! inside, so the run must leave every bound it started on and reach it.
    subroutine test_bobyqa_start_on_bounds(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(4), fmin, lo(4), hi(4)
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record

        lo = [-1.0_real64, -1.0_real64, -1.0_real64, -1.0_real64]
        hi = [3.0_real64, 3.0_real64, 3.0_real64, 3.0_real64]
        x = [lo(1), hi(2), lo(3), hi(4)]

        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=info, history=record)

        ! The start is the caller's own, unmoved: `honour_x0` shrinks the radius instead.
        call check(error, record%n > 0, "the record must hold the evaluations")
        if (allocated(error)) return
        call check(error, maxval(abs(record%x(:, 1) - [lo(1), hi(2), lo(3), hi(4)])) == 0.0_real64, &
                   "the first point evaluated must be the start, on its bounds, unmoved")
        if (allocated(error)) return
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-6_real64, &
                   "the sphere's minimiser is 1 in every coordinate, strictly inside this box")
        if (allocated(error)) return
        call check(error, fmin < 1.0e-12_real64, "the sphere's minimum is 0")
        if (allocated(error)) return
        call check(error, all(x >= lo) .and. all(x <= hi), "and every bound still holds")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK, "a radius-driven stop is PF_OPT_OK")

    end subroutine test_bobyqa_start_on_bounds

    !> `scale_from_box = .false.` leaves the units alone and scales the radius by the box instead.
    !!
    !! The two arms of `pf_bobyqa_solver` differ in where the box width goes: with
    !! `scale_from_box` the width becomes `scale` and `rhobeg_fraction` is the radius outright;
    !! without it the units stay the caller's and the radius becomes the fraction TIMES the
    !! narrowest width. On a box this test makes deliberately lopsided the two are different
    !! numbers, and the negative control is the same solver with the flag left at its default:
    !! both must find the same well, which is what makes the flag a scaling choice rather than a
    !! different search.
    subroutine test_bobyqa_solver_unscaled(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(pf_bobyqa_solver) :: solver
        real(real64) :: x(2), fmin, x_scaled(2), fmin_scaled, lo(2), hi(2)
        type(pf_optimize_info) :: info, info_scaled

        ! Lopsided on purpose: `minval(hi - lo)` is 0.5 while the other side spans 20.
        lo = [-10.0_real64, 0.75_real64]
        hi = [10.0_real64, 1.25_real64]

        solver%scale_from_box = .false.
        solver%rhobeg_fraction = 0.2_real64
        solver%rhoend = 1.0e-8_real64
        call pf_minimize_multistart(sphere, lo, hi, 20260921_int64, x, fmin, nstart=4, &
                                    solver=solver, info=info)

        call check(error, info%status == PF_OPT_OK, "the unscaled solver must report a clean stop")
        if (allocated(error)) return
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-5_real64, &
                   "the sphere's minimiser is 1 in both coordinates, inside this box")
        if (allocated(error)) return

        ! The negative control: the default arm reaches the same well from the same starts.
        solver%scale_from_box = .true.
        call pf_minimize_multistart(sphere, lo, hi, 20260921_int64, x_scaled, fmin_scaled, &
                                    nstart=4, solver=solver, info=info_scaled)
        call check(error, maxval(abs(x_scaled - 1.0_real64)) < 1.0e-5_real64, &
                   "and so must the box-scaled arm, from the same starts")
        if (allocated(error)) return
        call check(error, abs(fmin - fmin_scaled) < 1.0e-8_real64, &
                   "the two arms differ in units, not in the well they find")

    end subroutine test_bobyqa_solver_unscaled

    !> An explicit `rhoend` with no `rhobeg` pulls COBYLA's default `rhobeg` up to `10*rhoend`.
    !!
    !! Upstream's rule, which `pf_minimize_cobyla` keeps: the default `rhobeg` is
    !! `max(10*rhoend, RHOBEG_DFT)`, so an `rhoend` above a tenth of the default raises it rather
    !! than leaving a final radius larger than the initial one. The observable is `info%rho`,
    !! which cannot have reached a value below the `rhoend` asked for; the negative control is the
    !! same call with a tiny `rhoend`, where the default `rhobeg` governs instead.
    subroutine test_cobyla_rhoend_pulls_rhobeg_up(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(unit_disc) :: disc
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info, info_small

        ! `rhoend` well above a tenth of RHOBEG_DFT (which is one), so `10*rhoend` wins.
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, rhoend=0.5_real64, info=info)
        call check(error, info%neval > 0, "the run must have evaluated the objective")
        if (allocated(error)) return
        call check(error, info%rho >= 0.5_real64, &
                   "a run whose rhoend is 0.5 cannot report a final radius below it")
        if (allocated(error)) return

        ! The negative control: a tiny rhoend leaves the default rhobeg in charge and the run
        ! reaches a far smaller radius.
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, rhoend=1.0e-8_real64, info=info_small)
        call check(error, info_small%rho < info%rho, &
                   "and a tiny rhoend must reach a smaller radius than a coarse one")

    end subroutine test_cobyla_rhoend_pulls_rhobeg_up

    !> COBYLA's own exit tests: `ftarget` stops it early, and `max_neval` stops it on budget.
    !!
    !! These are `checkexit_con`'s two reachable arms -- the constrained twin of the test BOBYQA
    !! already has. `unit_disc` is the squared distance from `(1, 1)` on the unit ball, whose
    !! minimum is at the projection of `(1, 1)` onto the circle and so is
    !! `2*(1 - 1/sqrt(2))**2`, about `0.172`. A target comfortably above that is met partway
    !! through the search, and a budget of a handful of evaluations runs out before the radius
    !! rule fires.
    subroutine test_cobyla_target_and_budget(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(unit_disc) :: disc
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info

        ! A target above the constrained minimum of about 0.172, so the search meets it well
        ! before it converges.
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, ftarget=0.5_real64, rhobeg=0.5_real64, &
                                rhoend=1.0e-8_real64, info=info)
        call check(error, info%status == PF_OPT_TARGET, &
                   "a met ftarget is PF_OPT_TARGET, not a radius-driven stop")
        if (allocated(error)) return
        call check(error, fmin <= 0.5_real64, "and the value returned must meet the target")
        if (allocated(error)) return
        call check(error, info%converged, "a target stop counts as converged")
        if (allocated(error)) return

        ! A budget too small for the radius rule to fire.
        x = 0.0_real64
        call pf_minimize_cobyla(disc, x, fmin, max_neval=8, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, info=info)
        call check(error, info%status == PF_OPT_LIMIT, "an exhausted budget is PF_OPT_LIMIT")
        if (allocated(error)) return
        call check(error, info%neval <= 8, "and the budget must not be overrun")
        if (allocated(error)) return
        call check(error, .not. info%converged, "a budget stop is not convergence")

    end subroutine test_cobyla_target_and_budget

    !> An `npt` above `2n+1` makes LINCOA build the fuller model, and it still converges.
    !!
    !! LINCOA's `inith` has one arm for the `2n+1` points a default run interpolates and another
    !! for the extra points beyond them, which only an explicit `npt` reaches. With `n = 2` the
    !! range is `[n+2, (n+1)(n+2)/2] = [4, 6]`, so `npt = 6` is the fullest model available and
    !! exercises the second arm; the negative control is the same problem at the default `npt`,
    !! which must reach the same KKT point.
    subroutine test_lincoa_larger_npt(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, x_dft(2), fmin_dft
        real(real64) :: a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: info

        ! `sphere` is the sum of squares about 1; the half-plane x1 + x2 <= 1 cuts the minimiser
        ! (1, 1) off, so the constrained minimiser is its projection onto the line, (0.5, 0.5).
        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64

        x = [0.0_real64, 0.0_real64]
        call pf_minimize_lincoa(sphere, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, npt=6, &
                                rhobeg=0.2_real64, rhoend=1.0e-8_real64, info=info)
        call check(error, maxval(abs(x - 0.5_real64)) < 1.0e-6_real64, &
                   "the constrained minimiser is the projection of (1, 1) onto x1 + x2 = 1")
        if (allocated(error)) return
        ! The value is flat to first order in `x` only at an unconstrained minimum; here the
        ! gradient along the active line is nonzero, so the tolerance on `f` is the tolerance on
        ! `x` times that slope rather than the square of it.
        call check(error, abs(fmin - 0.5_real64) < 1.0e-5_real64, &
                   "and the value there is 2*(0.5 - 1)^2 = 0.5")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK, "a radius-driven stop is PF_OPT_OK")
        if (allocated(error)) return

        ! The negative control: the default model reaches the same point.
        x_dft = [0.0_real64, 0.0_real64]
        call pf_minimize_lincoa(sphere, x_dft, fmin_dft, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.2_real64, rhoend=1.0e-8_real64)
        call check(error, maxval(abs(x_dft - 0.5_real64)) < 1.0e-6_real64, &
                   "the default model must reach the same KKT point")

    end subroutine test_lincoa_larger_npt

    !> A budget smaller than the initial model needs stops each engine INSIDE its initialisation.
    !!
    !! `pf_minimize_bobyqa` uses `max_neval` as given rather than raising it to the `n + 3` that
    !! upstream would, so a budget below the interpolation set's size is honoured and the run
    !! ends before the model is complete. That is a different exit path from the one
    !! `a budget stops at PF_OPT_LIMIT with the best point so far` takes, which spends its
    !! budget in the main loop with a finished model: this one leaves the initialisation
    !! routine, which has its own return and its own best-point selection.
    !!
    !! All three engines are driven here because each has its own initialisation and its own
    !! copy of that exit. BOBYQA and LINCOA interpolate `2n+1 = 5` points by default and COBYLA
    !! builds a simplex of `n+1 = 3`, so the budgets below are under each.
    !!
    !! The negative control in every case is `neval`: a run that stopped early must report
    !! FEWER evaluations than the model needed, which is what distinguishes stopping inside the
    !! initialisation from completing it and stopping straight after.
    subroutine test_budget_below_the_initial_model(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(unit_disc) :: disc
        real(real64) :: x(2), fmin, lo(2), hi(2), a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: info

        ! BOBYQA: three evaluations against the five its default model interpolates.
        lo = -5.0_real64
        hi = 5.0_real64
        x = [0.5_real64, -0.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, max_neval=3, info=info)
        call check(error, info%status == PF_OPT_LIMIT, &
                   "BOBYQA must report PF_OPT_LIMIT when the budget runs out in the model")
        if (allocated(error)) return
        call check(error, info%neval <= 3, "and must not overrun the budget it was given")
        if (allocated(error)) return
        call check(error, fmin == sphere(x), "fmin must be the value at the returned point")
        if (allocated(error)) return
        call check(error, .not. info%converged, "an unfinished model is not convergence")
        if (allocated(error)) return

        ! BOBYQA again with an explicit npt above 2n+1, so the budget runs out in the SECOND
        ! of the two loops that fill the initial set -- the one that adds the points beyond
        ! the first 2n+1, which the default-npt run above does not enter.
        x = [0.5_real64, -0.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, npt=6, max_neval=5, info=info)
        call check(error, info%status == PF_OPT_LIMIT, &
                   "BOBYQA must report PF_OPT_LIMIT with npt = 6 and a budget of five")
        if (allocated(error)) return
        call check(error, info%neval <= 5, "and must not overrun that budget either")
        if (allocated(error)) return

        ! And one evaluation further on, so the budget runs out on the SIXTH point rather than
        ! the fifth. The first 2n+1 = 5 points exhaust the first loop exactly, so this is the
        ! only budget that stops in the second one -- which is a separate exit from the first.
        x = [0.5_real64, -0.5_real64]
        call pf_minimize_bobyqa(sphere, x, fmin, lower=lo, upper=hi, rhobeg=0.5_real64, &
                                rhoend=1.0e-10_real64, npt=6, max_neval=6, info=info)
        call check(error, info%status == PF_OPT_LIMIT, &
                   "a budget of exactly npt stops on the last point of the initial set")
        if (allocated(error)) return
        call check(error, info%neval <= 6, "and must not overrun it")
        if (allocated(error)) return

        ! LINCOA: the same, with a constraint so the linear machinery is set up first.
        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64
        x = [0.0_real64, 0.0_real64]
        call pf_minimize_lincoa(sphere, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.2_real64, rhoend=1.0e-10_real64, max_neval=3, info=info)
        call check(error, info%status == PF_OPT_LIMIT, &
                   "LINCOA must report PF_OPT_LIMIT when the budget runs out in the model")
        if (allocated(error)) return
        call check(error, info%neval <= 3, "and must not overrun the budget it was given")
        if (allocated(error)) return

        ! COBYLA: two evaluations against the three vertices of its initial simplex.
        x = [0.0_real64, 0.0_real64]
        call pf_minimize_cobyla(disc, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-10_real64, &
                                max_neval=2, info=info)
        call check(error, info%status == PF_OPT_LIMIT, &
                   "COBYLA must report PF_OPT_LIMIT when the budget runs out in the simplex")
        if (allocated(error)) return
        call check(error, info%neval <= 2, "and must not overrun the budget it was given")

    end subroutine test_budget_below_the_initial_model

    !> LINCOA with every constraint argument absent is a plain unconstrained minimisation.
    !!
    !! Each of `a_ineq`, `b_ineq`, `a_eq`, `b_eq`, `lower` and `upper` is optional, and nothing
    !! refuses a call that passes none: the engine then works with an EMPTY active set, which is
    !! its own path through the active-set machinery rather than a special case bolted on.
    !! `sphere`'s minimiser is 1 in every coordinate and there is nothing to keep it away, so
    !! the answer is the unconstrained one.
    !!
    !! The negative control is the constrained run below it: the same objective with the
    !! half-plane that cuts the minimiser off must stop somewhere else, which is what shows the
    !! constraint-free run was not simply ignoring a constraint it had been given.
    subroutine test_lincoa_without_constraints(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: info

        x = [-0.5_real64, 0.4_real64]
        call pf_minimize_lincoa(sphere, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-8_real64, &
                                info=info)
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-6_real64, &
                   "with no constraints the minimiser is the sphere's own, 1 in each coordinate")
        if (allocated(error)) return
        call check(error, fmin < 1.0e-12_real64, "and the unconstrained minimum is 0")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK, "a radius-driven stop is PF_OPT_OK")
        if (allocated(error)) return
        call check(error, info%cstrv <= 0.0_real64, &
                   "a run with no constraints cannot report a positive violation")
        if (allocated(error)) return

        ! The control: the same objective with a constraint that excludes (1, 1) must stop on it.
        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64
        x = [-0.5_real64, 0.4_real64]
        call pf_minimize_lincoa(sphere, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64)
        call check(error, maxval(abs(x - 0.5_real64)) < 1.0e-6_real64, &
                   "and with the half-plane it must stop at its projection, (0.5, 0.5)")

    end subroutine test_lincoa_without_constraints

    !> LINCOA's own exit tests: `ftarget` met, and a budget spent after the model is built.
    !!
    !! The twin of the BOBYQA and COBYLA cases, on the third engine.
    !! `a budget too small to finish the initial model` covers the exit INSIDE the
    !! initialisation; these two are the exits from the main loop, which is a different site.
    !! `sphere` under `x1 + x2 <= 1` has its constrained minimum `0.5` at `(0.5, 0.5)`, so a
    !! target above that is met partway and a budget above the five-point model but below
    !! convergence runs out in a trust-region step.
    subroutine test_lincoa_target_and_budget(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: info

        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64

        x = [0.0_real64, 0.0_real64]
        call pf_minimize_lincoa(sphere, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                ftarget=0.8_real64, rhobeg=0.2_real64, rhoend=1.0e-10_real64, &
                                info=info)
        call check(error, info%status == PF_OPT_TARGET, &
                   "a met ftarget is PF_OPT_TARGET, not a radius-driven stop")
        if (allocated(error)) return
        call check(error, fmin <= 0.8_real64, "and the value returned must meet the target")
        if (allocated(error)) return

        ! A budget above the 5-point model but far below convergence.
        x = [0.0_real64, 0.0_real64]
        call pf_minimize_lincoa(sphere, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, max_neval=9, &
                                rhobeg=0.2_real64, rhoend=1.0e-12_real64, info=info)
        call check(error, info%status == PF_OPT_LIMIT, "an exhausted budget is PF_OPT_LIMIT")
        if (allocated(error)) return
        call check(error, info%neval > 5 .and. info%neval <= 9, &
                   "the run must get past the five-point model and stop on its budget exactly")

    end subroutine test_lincoa_target_and_budget

    !> A model gradient above `1e12` is rescaled before the trust-region step is taken.
    !!
    !! The engine's step is computed from a rescaled copy of the model when
    !! `maxval(abs(gopt)) > 1e12`, an empirical guard so that the step direction is not lost to
    !! the magnitude of the gradient. `bad_scaling` makes the COORDINATES badly scaled, which is
    !! a different thing and does not reach this; `sphere_1e13` makes the VALUE large, so the
    !! gradient near the start is of order `1e13`.
    !!
    !! Scaling an objective by a positive constant cannot move its minimiser, so the answer must
    !! be the same `(0.5, 0.5)` the unscaled run reaches -- that invariance is the assertion, and
    !! it is what a lost or mis-scaled step would break.
    subroutine test_lincoa_huge_gradient(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: info

        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.0_real64

        x = [0.0_real64, 0.0_real64]
        call pf_minimize_lincoa(sphere_1e13, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.2_real64, rhoend=1.0e-8_real64, max_neval=500, &
                                info=info)
        call check(error, maxval(abs(x - 0.5_real64)) < 1.0e-5_real64, &
                   "scaling the objective by 1e13 cannot move its constrained minimiser")
        if (allocated(error)) return
        call check(error, fmin < 0.5_real64 * 1.0e13_real64 * (1.0_real64 + 1.0e-4_real64), &
                   "and the value there is 1e13 times the unscaled minimum, 0.5")
        if (allocated(error)) return
        call check(error, info%neval > 0, "the run must have evaluated the objective")

    end subroutine test_lincoa_huge_gradient

    !> Every budget is honoured, whichever kind of step the search was taking when it ran out.
    !!
    !! A budget can expire on any of three evaluation sites -- filling the initial simplex,
    !! taking a trust-region step, or taking a geometry step -- and each leaves the engine
    !! through its own exit. Which one a given budget lands on depends on the search path, so
    !! rather than aim at one, this sweeps every budget from three to forty and requires the
    !! same contract of all of them: the count is honoured, the point returned is usable, and
    !! it is no worse than the start.
    !!
    !! That is a stronger statement than any single budget makes, and it is the honest way to
    !! reach a path-dependent exit: the assertion does not depend on WHICH site a particular
    !! budget stops at, so it cannot rot when the arithmetic moves the search by an ulp.
    subroutine test_cobyla_honours_every_budget(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        type(unit_disc) :: disc
        real(real64) :: x(2), fmin, f_start
        type(pf_optimize_info) :: info
        integer :: b
        character(len=64) :: which

        ! `unit_disc` is the squared distance from (1, 1); at the origin, where every run below
        ! starts, that is 2.
        f_start = 2.0_real64

        do b = 3, 40
            write (which, '(a, i0)') "budget ", b
            x = 0.0_real64
            call pf_minimize_cobyla(disc, x, fmin, rhobeg=0.5_real64, rhoend=1.0e-10_real64, &
                                    max_neval=b, info=info)

            call check(error, info%neval <= b, &
                       trim(which)//": the budget must be honoured exactly, never overrun")
            if (allocated(error)) return
            call check(error, .not. ieee_is_nan(fmin), &
                       trim(which)//": the value returned must be a number")
            if (allocated(error)) return
            call check(error, fmin <= f_start, &
                       trim(which)//": the best point found cannot be worse than the start")
            if (allocated(error)) return
            call check(error, info%status == PF_OPT_LIMIT .or. info%status == PF_OPT_OK, &
                       trim(which)//": a run that ends must either converge or hit its limit")
            if (allocated(error)) return
        end do

    end subroutine test_cobyla_honours_every_budget

    !> LINCOA on Rosenbrock under a linear constraint, where the model is a poor local fit.
    !!
    !! Every other LINCOA test here minimises a quadratic, which its quadratic model reproduces
    !! exactly, so the trust-region ratio is essentially always excellent and the radius rule
    !! only ever takes its outer arms. Rosenbrock's curved valley is the opposite case: the
    !! model is a poor fit over the trust region, the ratio lands in the middling band, and the
    !! radius update takes its intermediate arm.
    !!
    !! The minimum of Rosenbrock is `(1, 1)`, where `x1 + x2 = 2`, so the constraint
    !! `x1 + x2 <= 1.5` is active and the answer lies on that line. The assertions are the ones
    !! a curved problem supports -- the constraint holds, the run improves on its start, and it
    !! lands on the active line -- never a digit of a search path.
    subroutine test_lincoa_non_quadratic(error)
        type(error_type), allocatable, intent(out) :: error !! Set on the first failed check.

        real(real64) :: x(2), fmin, f_start, a_ineq(1, 2), b_ineq(1)
        type(pf_optimize_info) :: info

        a_ineq(1, :) = [1.0_real64, 1.0_real64]
        b_ineq = 1.5_real64

        x = [-1.2_real64, 1.0_real64]
        f_start = rosenbrock(x)
        call pf_minimize_lincoa(rosenbrock, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, &
                                rhobeg=0.5_real64, rhoend=1.0e-8_real64, max_neval=2000, &
                                info=info)

        call check(error, fmin < f_start, "the run must improve on its own start")
        if (allocated(error)) return
        ! The answer is accurate to about `rhoend` in the engine's units, so a constraint the
        ! solution sits ON is met to that order rather than exactly; the run reports the
        ! residue it left in `info%cstrv`, and `PF_OPT_OK` is what says it is within `ctol`.
        call check(error, x(1) + x(2) <= 1.5_real64 + 1.0e-6_real64, &
                   "the linear constraint must hold at the point returned, to about rhoend")
        if (allocated(error)) return
        call check(error, info%status == PF_OPT_OK, &
                   "and the run must call that point feasible, not PF_OPT_INFEASIBLE")
        if (allocated(error)) return
        call check(error, abs(x(1) + x(2) - 1.5_real64) < 1.0e-5_real64, &
                   "and it must be ACTIVE: Rosenbrock's minimiser lies beyond it")
        if (allocated(error)) return
        call check(error, fmin == rosenbrock(x), "fmin must be the value at the returned point")
        if (allocated(error)) return
        call check(error, info%neval > 0, "the run must have evaluated the objective")

    end subroutine test_lincoa_non_quadratic

    !> The `BOBYQA, LINCOA, COBYLA` row of the overshoot column in
    !! doc/pages/utilities/solvers.md, "The budget: max_neval": the page says NEVER, for all three.
    !!
    !! **Why never is the right word here.** Every PRIMA evaluation goes through `evaluate` or
    !! `evaluate_fc` (`src/parquet_prima_common.f90`), and the engines test the spent count against
    !! the budget before asking for another point; a vendored driver that wanted one more would get
    !! the `MAXFUN_REACHED` return instead. The three share that machinery, which is why one test
    !! covers all three rather than one per engine.
    !!
    !! **Two negative controls.** Each case asserts the run evaluated its unavoidable start, so a
    !! call that returned at once cannot pass the bound; and each engine must somewhere in the
    !! sweep actually reach its cap, counted through `PF_OPT_LIMIT` and required non-zero.
    subroutine test_guide_budget_overshoot_bounds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error handle.

        !> The fixture's dimension.
        integer, parameter :: N_DIM = 3
        type(pf_optimize_info) :: info
        !> COBYLA's fixture is a CONSTRAINED objective, so it needs an object and its own
        !! dimension: `unit_disc` is the squared distance from `(1, 2)` inside the unit disc.
        type(unit_disc) :: disc
        real(real64) :: x(N_DIM), lo(N_DIM), hi(N_DIM), fmin, x2(2), lo2(2), hi2(2)
        integer :: budget, seen_bob, seen_lin, seen_cob
        character(len=190) :: msg

        seen_bob = 0
        seen_lin = 0
        seen_cob = 0
        lo = -2.0_real64
        hi = 2.0_real64
        lo2 = -2.0_real64
        hi2 = 2.0_real64

        do budget = 4, 300, 3
            x = 0.5_real64
            call pf_minimize_bobyqa(rastrigin, x, fmin, lower=lo, upper=hi, max_neval=budget, &
                                    info=info)
            write(msg, '(a, i0, a, i0)') "BOBYQA must never exceed max_neval ", budget, &
                "; neval is ", info%neval
            call check(error, info%neval <= budget, trim(msg))
            if (allocated(error)) return
            call check(error, info%neval >= 1, "the BOBYQA run must have evaluated something")
            if (allocated(error)) return
            if (info%status == PF_OPT_LIMIT) seen_bob = seen_bob + 1

            x = 0.5_real64
            call pf_minimize_lincoa(rastrigin, x, fmin, lower=lo, upper=hi, max_neval=budget, &
                                    info=info)
            write(msg, '(a, i0, a, i0)') "LINCOA must never exceed max_neval ", budget, &
                "; neval is ", info%neval
            call check(error, info%neval <= budget, trim(msg))
            if (allocated(error)) return
            call check(error, info%neval >= 1, "the LINCOA run must have evaluated something")
            if (allocated(error)) return
            if (info%status == PF_OPT_LIMIT) seen_lin = seen_lin + 1

            x2 = 0.5_real64
            call pf_minimize_cobyla(disc, x2, fmin, lower=lo2, upper=hi2, max_neval=budget, &
                                    info=info)
            write(msg, '(a, i0, a, i0)') "COBYLA must never exceed max_neval ", budget, &
                "; neval is ", info%neval
            call check(error, info%neval <= budget, trim(msg))
            if (allocated(error)) return
            call check(error, info%neval >= 1, "the COBYLA run must have evaluated something")
            if (allocated(error)) return
            if (info%status == PF_OPT_LIMIT) seen_cob = seen_cob + 1
        end do

        write(msg, '(a, 3(1x, i0))') "each engine's budget must bind somewhere in the sweep; " // &
            "(bobyqa, lincoa, cobyla) bound:", seen_bob, seen_lin, seen_cob
        call check(error, seen_bob > 0 .and. seen_lin > 0 .and. seen_cob > 0, trim(msg))

    end subroutine test_guide_budget_overshoot_bounds

end module test_prima
