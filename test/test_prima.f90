!> Tests for `parquet_prima`: BOBYQA, its bounds, its `scale=` argument and its use as a local
!! solver under `pf_minimize_multistart`.
!!
!! **Every expected location is the objective's own algebra**, never a location read off a
!! previous run: the sphere's minimum is its centre, Rosenbrock's is `(1, 1)`, `bad_scaling`'s is
!! `BAD_SCALING_MIN` by construction, and where the minimum lies outside the box the answer is the
!! nearest point of the box, which is again algebra rather than a recorded output.
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
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use iso_fortran_env, only : real64, int64

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
            new_unittest("a scale= run reproduces the hand-scaled objective bit for bit", &
                         test_bobyqa_scale_matches_hand_scaling), &
            new_unittest("scale= reaches a badly conditioned minimum the unscaled run misses", &
                         test_bobyqa_scale_earns_its_place), &
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
                         test_bobyqa_solver_options)]

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
        call check(error, info%neval <= 12 + 1, &
                   "the budget is soft by at most one step, not by more")
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

end module test_prima
