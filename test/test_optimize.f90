!> Tests for `parquet_optimize`'s local engines: Brent on a bracket and the Nelder-Mead simplex.
!!
!! **No test asserts an exact evaluation count.** A count is a whole-search-path quantity: a
!! floating-point model that reassociates arithmetic flips one accept/reject decision and moves the
!! total by a few, and qfeet measured 357, 359 and 360 for one example under gfortran, ifx and
!! `-Ofast`. Every count assertion here is an upper bound paired with the minimum actually reached,
!! which is what a real regression moves by tens.
!!
!! **Every expected location and value is derived, never recorded.** The minimisers come from the
!! objectives' own algebra (`test_optimize_support.f90` states each one beside its function), and
!! the line fit's slope, intercept and residual sum of squares are the exact least-squares
!! solution, written as the arithmetic that produces them.
!!
!! The abort paths are not here: an `error stop` kills the runner, so each is an out-of-process
!! scenario in `test/error_scenarios.f90` with its wrapper in `test/test_errors.f90`.
module test_optimize

    use parquet_optimize
    use test_optimize_support
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_get_flag, ieee_set_flag, &
                                             ieee_support_flag, ieee_invalid

    implicit none
    private

    public :: collect_tests_optimize

contains

    !> Collects every test in this suite.
    subroutine collect_tests_optimize(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("Brent finds each closed-form minimiser on its bracket", &
                         test_scalar_closed_forms), &
            new_unittest("a minimiser at zero converges at the default tolerance", &
                         test_scalar_zero_minimiser), &
            new_unittest("a looser tol gives a looser answer, and tol=0 the arithmetic's own floor", &
                         test_scalar_tolerance), &
            new_unittest("a scalar budget stops at PF_OPT_LIMIT with the best point so far", &
                         test_scalar_budget), &
            new_unittest("the scalar record holds every evaluation, in order", &
                         test_scalar_history), &
            new_unittest("the simplex reaches the minimum from a start point and a step", &
                         test_simplex_amoeba), &
            new_unittest("the object form counts its own evaluations and agrees with info", &
                         test_simplex_object_form), &
            new_unittest("atol alone converges where a fractional tolerance cannot", &
                         test_simplex_atol_only), &
            new_unittest("one variable, an already-converged simplex and an impossible budget", &
                         test_simplex_edge_cases), &
            new_unittest("the simplex reproduces the exact least-squares line", &
                         test_simplex_line_fit), &
            new_unittest("the final spread is the convergence test that fired", &
                         test_simplex_spread), &
            new_unittest("the plain-function and object forms evaluate identically", &
                         test_simplex_forms_agree), &
            new_unittest("a symmetric straddle converges at neither vertex being the minimum", &
                         test_simplex_straddle), &
            new_unittest("a record comes back trimmed to the records in use", &
                         test_history_is_trimmed), &
            new_unittest("pf_simplex_solver runs the simplex inside the driver's box", &
                         test_simplex_solver_object), &
            new_unittest("DE reaches every one of the four reference minima", &
                         test_de_reference_functions), &
            new_unittest("DE converges on the spread test with no target given", &
                         test_de_spread_alone), &
            new_unittest("a small generation budget stops DE at PF_OPT_LIMIT", &
                         test_de_generation_budget), &
            new_unittest("DE crosses a NaN region and still finds the minimum outside it", &
                         test_de_nan_region), &
            new_unittest("an objective with an allocatable component is cloned per thread", &
                         test_de_clones_the_objective), &
            new_unittest("DE is a function of its seed and of nothing else", &
                         test_de_seed_decides_the_run), &
            new_unittest("the final population has np individuals, all inside the box", &
                         test_de_population), &
            new_unittest("polish lowers the value and adds its evaluations to the count", &
                         test_de_polish), &
            new_unittest("polish minimises inside the box and answers inside it", &
                         test_de_polish_stays_in_the_box), &
            new_unittest("a mutant outside the box lands between its parent and the bound", &
                         test_de_out_of_box_rule), &
            new_unittest("max_gen at huge(1) runs generations rather than stopping at once", &
                         test_de_max_gen_ceiling), &
            new_unittest("no finite value anywhere is PF_OPT_NONFINITE, the box centre and +Inf", &
                         test_de_nothing_finite), &
            new_unittest("the multistart driver ends every start on a stationary point", &
                         test_multistart_basins), &
            new_unittest("the lowest start index wins, whatever the thread count", &
                         test_multistart_tie_rule), &
            new_unittest("xtol decides how many minima count as distinct", &
                         test_multistart_xtol), &
            new_unittest("the default merge radius counts one minimum on a one-basin objective", &
                         test_multistart_default_xtol), &
            new_unittest("the solver object's own budget reaches every local run", &
                         test_multistart_solver_options), &
            new_unittest("no finite start is PF_OPT_NONFINITE, the box centre and +Inf", &
                         test_multistart_nothing_finite), &
            new_unittest("NaN starts are counted and skipped, and the best finite start wins", &
                         test_multistart_some_nan_starts) &
            ]

    end subroutine collect_tests_optimize

    !> Brent reaches each objective's own minimiser: interior, at an end, and on a flat function.
    subroutine test_scalar_closed_forms(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x, fmin
        type(pf_optimize_info) :: info

        ! (x - 1)**2 on a bracket straddling its minimiser.
        call pf_minimize_scalar(quad1d, -5.0_real64, 5.0_real64, x, fmin, info=info)
        call check(error, x, quad1d_min(), thr=1.0e-7_real64)
        if (allocated(error)) return
        call check(error, fmin, 0.0_real64, thr=1.0e-14_real64)
        if (allocated(error)) return
        call check(error, info%converged, "an interior minimum should report converged")
        if (allocated(error)) return
        call check(error, info%neval <= 60, "Brent should not need dozens of evaluations here")
        if (allocated(error)) return
        call check(error, info%neval > 2, "Brent should have moved past its first two points")
        if (allocated(error)) return

        ! (x - 2)**2 with a different minimiser, so a hard-coded 1 would fail.
        call pf_minimize_scalar(one_dim, 0.0_real64, 5.0_real64, x, fmin, info=info)
        call check(error, x, one_dim_min(), thr=1.0e-7_real64)
        if (allocated(error)) return

        ! The minimiser lies OUTSIDE the bracket, so the answer sits on the near end: Brent
        ! narrows towards `a` and stops within its own tolerance of it.
        call pf_minimize_scalar(one_dim, 3.0_real64, 9.0_real64, x, fmin, info=info)
        call check(error, x < 3.0_real64 + 1.0e-6_real64, &
            "a minimum outside the bracket should be reported at the near end")
        if (allocated(error)) return
        call check(error, x >= 3.0_real64, "the answer must stay inside the bracket")
        if (allocated(error)) return

        ! A flat objective: every point is a minimum, and the run still terminates and reports one.
        call pf_minimize_scalar(constant_one, -1.0_real64, 1.0_real64, x, fmin, info=info)
        call check(error, fmin, 1.0_real64, thr=0.0_real64)
        if (allocated(error)) return
        call check(error, x >= -1.0_real64 .and. x <= 1.0_real64, &
            "a flat objective should still answer inside the bracket")
        if (allocated(error)) return
        call check(error, info%converged, "a flat objective should report converged")

    end subroutine test_scalar_closed_forms

    !> A minimiser AT ZERO converges, at the default tolerance and at an explicit `tol = 0`.
    !!
    !! The relative part of the effective tolerance, `sqrt(epsilon)*abs(x)`, vanishes as `x`
    !! approaches zero, so without the bracket-width floor the stopping test chases a target that
    !! keeps receding: the run spends its whole budget and reports `PF_OPT_LIMIT` although its
    !! answer is excellent. `sphere` is used as a one-variable objective -- `sum(x**2)` over a
    !! one-element array -- because its minimiser is exactly zero, which is the case in question.
    subroutine test_scalar_zero_minimiser(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x, fmin
        type(pf_optimize_info) :: info

        call pf_minimize_scalar(origin_sphere, -1.0_real64, 1.0_real64, x, fmin, info=info)
        call check(error, info%converged, &
            "a minimum at zero must meet the stopping test, not run out of budget")
        if (allocated(error)) return
        call check(error, info%status, PF_OPT_OK, "and its status is convergence")
        if (allocated(error)) return
        call check(error, info%neval <= 40, &
            "converging on x**2 should cost a few dozen evaluations, not the whole budget")
        if (allocated(error)) return
        call check(error, abs(x) <= 1.0e-7_real64, "and the answer is still the minimiser")
        if (allocated(error)) return

        ! An explicit `tol = 0` asks for as much accuracy as the arithmetic allows, which is what
        ! the floor supplies; it must not be the one spelling that cannot stop.
        call pf_minimize_scalar(origin_sphere, -1.0_real64, 1.0_real64, x, fmin, tol=0.0_real64, &
                                info=info)
        call check(error, info%converged, "tol = 0 must converge on the same minimum")
        if (allocated(error)) return
        call check(error, abs(x) <= 1.0e-7_real64, "and reach it")

    end subroutine test_scalar_zero_minimiser

    !> `tol` is honoured, and `tol = 0` means the arithmetic's own floor rather than no tolerance.
    subroutine test_scalar_tolerance(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x_loose, x_tight, fmin
        type(pf_optimize_info) :: loose, tight
        real(real64), parameter :: TOL = 1.0e-4_real64

        call pf_minimize_scalar(quad1d, -5.0_real64, 5.0_real64, x_loose, fmin, tol=TOL, info=loose)
        call check(error, abs(x_loose - quad1d_min()) <= 3.0_real64*TOL, &
            "a requested tol should bound the distance to the minimiser")
        if (allocated(error)) return

        call pf_minimize_scalar(quad1d, -5.0_real64, 5.0_real64, x_tight, fmin, tol=0.0_real64, &
                                info=tight)
        call check(error, abs(x_tight - quad1d_min()) <= 1.0e-7_real64, &
            "tol = 0 should reach the arithmetic's own floor")
        if (allocated(error)) return

        ! The looser request must not cost more than the tighter one: that is what asking for
        ! less accuracy buys, and it is the assertion a tol that is read and then ignored fails.
        call check(error, loose%neval <= tight%neval, &
            "a looser tol should not cost more evaluations than a tighter one")

    end subroutine test_scalar_tolerance

    !> A budget too small to converge stops at `PF_OPT_LIMIT` with the best point so far.
    subroutine test_scalar_budget(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x, fmin, x_full, fmin_full
        type(pf_optimize_info) :: info, full

        call pf_minimize_scalar(quad1d, -5.0_real64, 5.0_real64, x, fmin, max_neval=4, info=info)
        call check(error, info%status, PF_OPT_LIMIT, "a spent budget should report PF_OPT_LIMIT")
        if (allocated(error)) return
        call check(error, .not. info%converged, "a spent budget is not convergence")
        if (allocated(error)) return
        call check(error, info%neval <= 5, "the budget is soft by at most one engine step")
        if (allocated(error)) return

        ! The point returned is the best SEEN, not a fresh guess: the unbudgeted run from the same
        ! bracket can only do better.
        call pf_minimize_scalar(quad1d, -5.0_real64, 5.0_real64, x_full, fmin_full, info=full)
        call check(error, fmin >= fmin_full, "the budgeted run cannot beat the unbudgeted one")
        if (allocated(error)) return
        call check(error, fmin, quad1d(reshape([x], [1])), thr=0.0_real64)

    end subroutine test_scalar_budget

    !> The record holds every evaluation, in order, and its values belong to its points.
    subroutine test_scalar_history(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x, fmin
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        integer :: k
        logical :: values_match

        call pf_minimize_scalar(quartic, -2.0_real64, 2.0_real64, x, fmin, info=info, &
                                history=record)

        call check(error, record%n, info%neval, "the record should hold one entry per evaluation")
        if (allocated(error)) return
        call check(error, size(record%f), record%n, "the record should come back trimmed")
        if (allocated(error)) return
        call check(error, size(record%x, 1), 1, "a scalar record has one row")
        if (allocated(error)) return
        call check(error, size(record%x, 2), record%n, "the record's points should be trimmed too")
        if (allocated(error)) return

        values_match = .true.
        do k = 1, record%n
            if (record%f(k) /= quartic(record%x(:, k))) values_match = .false.
        end do
        call check(error, values_match, "every recorded value should belong to its recorded point")
        if (allocated(error)) return

        ! The returned answer is the best entry in the record -- nothing better was seen and
        ! discarded.
        call check(error, fmin, minval(record%f), thr=0.0_real64)
        if (allocated(error)) return

        ! The minimiser is a stationary point of the quartic, which is the closed form available
        ! for a function whose root has no tidy decimal.
        call check(error, abs(quartic_derivative(x)) <= 1.0e-7_real64, &
            "the quartic's minimiser should solve 4*(x - 1/2)**3 + 2*x = 0")

    end subroutine test_scalar_history

    !> The simplex reaches the minimum from a start point and a step, and reports its budget.
    subroutine test_simplex_amoeba(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info

        x = [0.0_real64, -5.0_real64]
        call pf_minimize_simplex(shifted_norm, x, fmin, step=[2.5_real64, 2.5_real64], &
                                 ftol=1.0e-2_real64, info=info)
        call check(error, norm2(x - 1.0_real64) <= 1.0e-4_real64, &
            "the simplex should reach the objective's own minimiser")
        if (allocated(error)) return
        call check(error, fmin >= 0.0_real64, "a norm cannot be negative")
        if (allocated(error)) return
        call check(error, fmin <= 1.0e-7_real64, "the value at the minimum should be near zero")
        if (allocated(error)) return
        call check(error, info%converged, "the simplex should have converged")
        if (allocated(error)) return
        call check(error, info%neval <= 500, "this fit should not cost hundreds of evaluations")
        if (allocated(error)) return
        call check(error, info%niter > 0, "convergence here takes at least one iteration")
        if (allocated(error)) return

        ! An impossible budget: the starting simplex must be evaluated before the budget can be
        ! tested at all, so the count cannot fall below npar+1 whatever the caller asked for.
        x = [0.0_real64, -5.0_real64]
        call pf_minimize_simplex(shifted_norm, x, fmin, step=[2.5_real64, 2.5_real64], &
                                 ftol=1.0e-2_real64, max_neval=1, info=info)
        call check(error, info%neval, 3, "neval cannot fall below npar+1 whatever max_neval says")
        if (allocated(error)) return
        call check(error, .not. info%converged, "an exhausted budget should report not converged")
        if (allocated(error)) return
        call check(error, info%status, PF_OPT_LIMIT, "an exhausted budget is PF_OPT_LIMIT")

    end subroutine test_simplex_amoeba

    !> The object form updates its own components and agrees with `info` on the count.
    subroutine test_simplex_object_form(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info
        type(shifted_quadratic) :: obj

        obj%shift = 10.0_real64
        obj%ncall = 0
        x = [0.0_real64, -5.0_real64]

        call pf_minimize_simplex(obj, x, fmin, step=[2.5_real64, 2.5_real64], &
                                 ftol=1.0e-10_real64, info=info)

        call check(error, info%converged, "the object form should have converged")
        if (allocated(error)) return
        call check(error, norm2(x - 1.0_real64) <= 1.0e-4_real64, &
            "the object form should reach the same minimiser")
        if (allocated(error)) return
        call check(error, fmin, obj%shift, thr=1.0e-8_real64)
        if (allocated(error)) return
        ! `eval` updates a component of the caller's own object, which the caller reads afterwards.
        call check(error, obj%ncall, info%neval, &
            "the objective should have counted exactly the evaluations info reports")

    end subroutine test_simplex_object_form

    !> `atol` alone converges an objective whose minimum value is zero, which `ftol` alone cannot.
    subroutine test_simplex_atol_only(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info
        type(shifted_quadratic) :: obj

        ! A fractional spread over values that are all near zero never falls below a positive
        ! ftol, so ftol = 0 with a positive atol is the only pair that can converge here.
        obj%shift = 0.0_real64
        obj%ncall = 0
        x = [5.0_real64, -3.0_real64]

        call pf_minimize_simplex(obj, x, fmin, step=[0.5_real64, 0.5_real64], ftol=0.0_real64, &
                                 atol=1.0e-10_real64, info=info)

        call check(error, info%converged, "atol alone should have converged")
        if (allocated(error)) return
        call check(error, fmin < 1.0e-8_real64, "atol should have driven the value close to zero")
        if (allocated(error)) return
        call check(error, norm2(x - 1.0_real64) <= 1.0e-4_real64, &
            "atol convergence should still reach the minimiser")

    end subroutine test_simplex_atol_only

    !> One variable, an already-converged simplex, and a budget below the starting simplex.
    subroutine test_simplex_edge_cases(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(1), fmin
        type(pf_optimize_info) :: info

        ! One variable is the shape most likely to be broken by an off-by-one.
        x = [5.0_real64]
        call pf_minimize_simplex(one_dim, x, fmin, step=[0.5_real64], ftol=1.0e-10_real64, &
                                 info=info)
        call check(error, info%converged, "a one-variable fit should have converged")
        if (allocated(error)) return
        call check(error, x(1), one_dim_min(), thr=1.0e-5_real64)
        if (allocated(error)) return
        call check(error, fmin, 0.0_real64, thr=1.0e-9_real64)
        if (allocated(error)) return

        ! A simplex whose vertices already agree converges without a single iteration, so the only
        ! evaluations are the npar+1 spent on the starting simplex itself.
        x = [0.0_real64]
        call pf_minimize_simplex(constant_one, x, fmin, step=[7.0_real64], ftol=1.0e-6_real64, &
                                 info=info)
        call check(error, info%converged, "a constant objective should converge immediately")
        if (allocated(error)) return
        call check(error, info%neval, 2, "immediate convergence costs exactly npar+1 evaluations")
        if (allocated(error)) return
        call check(error, info%niter, 0, "immediate convergence takes no iteration")
        if (allocated(error)) return

        ! Every optional argument omitted: the bare call is legal because ftol is required.
        x = [5.0_real64]
        call pf_minimize_simplex(one_dim, x, fmin, [0.5_real64], 1.0e-10_real64)
        call check(error, x(1), one_dim_min(), thr=1.0e-5_real64)

    end subroutine test_simplex_edge_cases

    !> The simplex reproduces the exact least-squares line through the five points.
    subroutine test_simplex_line_fit(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info
        type(line_fit) :: fit

        fit%ncall = 0
        x = [1.0_real64, 0.0_real64]

        call pf_minimize_simplex(fit, x, fmin, step=[0.2_real64, 0.2_real64], &
                                 ftol=1.0e-12_real64, info=info)

        ! The reference is the normal-equations solution, not a recorded run.
        call check(error, x(1), line_fit_slope, thr=1.0e-6_real64)
        if (allocated(error)) return
        call check(error, x(2), line_fit_intercept, thr=1.0e-6_real64)
        if (allocated(error)) return
        call check(error, fmin, line_fit_rss, thr=1.0e-9_real64)
        if (allocated(error)) return
        call check(error, fit%ncall, info%neval, "the fit should have counted every evaluation")
        if (allocated(error)) return
        call check(error, info%neval <= 400, "a two-parameter line fit is cheap")

    end subroutine test_simplex_line_fit

    !> `info%spread` is the convergence test that actually fired.
    subroutine test_simplex_spread(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin
        type(pf_optimize_info) :: info
        real(real64), parameter :: ATOL = 1.0e-10_real64

        ! Converged on atol: the reported spread is the absolute spread the test compared, so it
        ! must be below the tolerance that ended the run. A spread left at its default 0, or
        ! carrying some other quantity, fails here.
        x = [5.0_real64, -3.0_real64]
        call pf_minimize_simplex(sphere, x, fmin, step=[0.5_real64, 0.5_real64], ftol=0.0_real64, &
                                 atol=ATOL, info=info)
        call check(error, info%converged, "the atol run should have converged")
        if (allocated(error)) return
        call check(error, info%spread < ATOL, &
            "an atol-converged run must report a spread below that atol")
        if (allocated(error)) return
        call check(error, info%spread >= 0.0_real64, "a spread is a magnitude")
        if (allocated(error)) return

        ! Stopped on the budget instead: the spread is whatever the simplex had reached, and it
        ! must be ABOVE the tolerance that did not fire.
        x = [5.0_real64, -3.0_real64]
        call pf_minimize_simplex(sphere, x, fmin, step=[0.5_real64, 0.5_real64], ftol=0.0_real64, &
                                 atol=ATOL, max_neval=8, info=info)
        call check(error, .not. info%converged, "an eight-evaluation budget cannot converge here")
        if (allocated(error)) return
        call check(error, info%spread >= ATOL, &
            "a run that stopped on its budget has a spread its tolerance would have rejected")

    end subroutine test_simplex_spread

    !> The plain-function and object forms are the same engine over the same arithmetic.
    subroutine test_simplex_forms_agree(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x_fun(2), x_obj(2), f_fun, f_obj
        type(pf_optimize_history) :: h_fun, h_obj
        type(shifted_quadratic) :: obj
        logical :: same

        ! `sphere` and a zero-shift `shifted_quadratic` compute the same expression, so the two
        ! specifics must walk the same path, evaluation for evaluation.
        obj%shift = 0.0_real64
        obj%ncall = 0

        x_fun = [5.0_real64, -3.0_real64]
        call pf_minimize_simplex(sphere, x_fun, f_fun, step=[0.5_real64, 0.5_real64], &
                                 ftol=0.0_real64, atol=1.0e-10_real64, history=h_fun)

        x_obj = [5.0_real64, -3.0_real64]
        call pf_minimize_simplex(obj, x_obj, f_obj, step=[0.5_real64, 0.5_real64], &
                                 ftol=0.0_real64, atol=1.0e-10_real64, history=h_obj)

        call check(error, h_fun%n, h_obj%n, "the two forms should evaluate the same number of times")
        if (allocated(error)) return

        same = all(h_fun%f == h_obj%f) .and. all(h_fun%x == h_obj%x)
        call check(error, same, "the two forms should record identical evaluations, bit for bit")
        if (allocated(error)) return
        call check(error, f_fun == f_obj .and. all(x_fun == x_obj), &
            "the two forms should return identical answers, bit for bit")
        if (allocated(error)) return
        call check(error, obj%ncall, h_obj%n, "the object counted what the record holds")

    end subroutine test_simplex_forms_agree

    !> A simplex straddling the minimum symmetrically converges at neither vertex being it.
    subroutine test_simplex_straddle(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(1), fmin
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        real(real64) :: tied_sum
        integer :: k, ntied

        ! (x - 1)**2 is symmetric about its minimiser, so a simplex whose two vertices sit equally
        ! far either side of it has EQUAL values: the spread is exactly zero, the test fires, and
        ! neither vertex is the answer. This is the example the guide page uses to say what
        ! `converged` means, and it is why `info%converged` is documented as "the engine's own
        ! stopping rule fired", never "a minimum was found".
        x = [5.0_real64]
        call pf_minimize_simplex(quad1d, x, fmin, step=[0.5_real64], ftol=1.0e-10_real64, &
                                 info=info, history=record)

        call check(error, info%converged, "the straddle case should report converged")
        if (allocated(error)) return
        call check(error, info%spread, 0.0_real64, thr=0.0_real64)
        if (allocated(error)) return

        ! Both vertices sit one unit either side of the minimiser, so both values are exactly 1 --
        ! the closed form, not a recorded number.
        call check(error, fmin, 1.0_real64, thr=0.0_real64)
        if (allocated(error)) return
        call check(error, abs(x(1) - quad1d_min()), 1.0_real64, thr=0.0_real64)
        if (allocated(error)) return

        ! The record holds both of them, and their midpoint is the minimiser neither reached.
        ntied = 0
        tied_sum = 0.0_real64
        do k = 1, record%n
            if (record%f(k) == fmin) then
                ntied = ntied + 1
                tied_sum = tied_sum + record%x(1, k)
            end if
        end do
        call check(error, ntied, 2, "the straddle should leave exactly two vertices tied")
        if (allocated(error)) return
        call check(error, 0.5_real64*tied_sum, quad1d_min(), thr=0.0_real64)

    end subroutine test_simplex_straddle

    !> A record comes back trimmed: its arrays are exactly as long as the records in use.
    subroutine test_history_is_trimmed(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin
        type(pf_optimize_history) :: record
        type(pf_optimize_info) :: info

        ! The record grows geometrically, so an untrimmed one would be longer than `n` on almost
        ! every run; a caller must be able to use `size()` without knowing that.
        x = [5.0_real64, -3.0_real64]
        call pf_minimize_simplex(sphere, x, fmin, step=[0.5_real64, 0.5_real64], ftol=0.0_real64, &
                                 atol=1.0e-10_real64, info=info, history=record)

        call check(error, allocated(record%f), "a record must come back allocated")
        if (allocated(error)) return
        call check(error, allocated(record%x), "a record's points must come back allocated")
        if (allocated(error)) return
        call check(error, size(record%f), record%n, "the values should be trimmed to n")
        if (allocated(error)) return
        call check(error, size(record%x, 2), record%n, "the points should be trimmed to n")
        if (allocated(error)) return
        call check(error, size(record%x, 1), size(x), "the record has one row per variable")
        if (allocated(error)) return
        call check(error, record%n, info%neval, "every evaluation should be recorded")

    end subroutine test_history_is_trimmed

    !> `pf_simplex_solver` carries its options and derives its step from the driver's box.
    subroutine test_simplex_solver_object(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64) :: x(2), fmin, lower(2), upper(2)
        type(pf_optimize_info) :: info
        type(pf_simplex_solver) :: solver
        type(shifted_quadratic) :: obj

        lower = [-4.0_real64, -4.0_real64]
        upper = [6.0_real64, 6.0_real64]

        ! The defaults are a valid pair as they stand: atol is positive, so the solver converges
        ! where the entry point's own bare form would have nothing to fire on.
        obj%shift = 0.0_real64
        obj%ncall = 0
        x = [5.0_real64, -3.0_real64]
        call solver%run(obj, x, fmin, lower, upper, info)

        call check(error, info%converged, "the default solver options should converge")
        if (allocated(error)) return
        call check(error, norm2(x - 1.0_real64) <= 1.0e-3_real64, &
            "the solver should reach the objective's minimiser")
        if (allocated(error)) return
        call check(error, obj%ncall, info%neval, "the solver should report the evaluations made")
        if (allocated(error)) return

        ! An option set on the object reaches the run: a budget of five cannot converge, and the
        ! solver must report that rather than silently using its own default.
        solver%max_neval = 5
        obj%ncall = 0
        x = [5.0_real64, -3.0_real64]
        call solver%run(obj, x, fmin, lower, upper, info)
        call check(error, info%status, PF_OPT_LIMIT, &
            "a max_neval set on the solver object should reach the run")
        if (allocated(error)) return
        call check(error, info%neval <= 8, "the budget is soft by at most one engine step")

    end subroutine test_simplex_solver_object

    !> DE reaches the reference minimum of each of the four functions the design was measured on.
    !!
    !! Every bound below is an UPPER BOUND on the evaluation count, generous by a factor of about
    !! two, never a measurement: a floating-point model that reassociates `a + F*(b - c)` flips one
    !! selection and moves the whole path, so an exact count would pin one toolchain. What a real
    !! regression moves is the order of magnitude, and that is what these catch. The minimum
    !! actually reached is asserted beside each, which is the half a loose bound cannot fake.
    subroutine test_de_reference_functions(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x2(2), x5(5), x10(10), fmin
        real(real64) :: lo2(2), hi2(2), lo5(5), hi5(5), lo10(10), hi10(10)
        real(real64), parameter :: target = 1.0e-8_real64

        lo2 = -5.0_real64
        hi2 = 5.0_real64
        call pf_minimize_de(rosenbrock, lo2, hi2, 42_int64, x2, fmin, ftarget=target, info=info)
        call check(error, info%status, PF_OPT_TARGET, "Rosenbrock in 2 variables should reach the target")
        if (allocated(error)) return
        call check(error, fmin <= target, "the target is a bound on the value, not a suggestion")
        if (allocated(error)) return
        call check(error, info%neval <= 4000, "Rosenbrock-2 should cost a few thousand evaluations")
        if (allocated(error)) return
        call check(error, maxval(abs(x2 - 1.0_real64)) < 1.0e-3_real64, &
            "Rosenbrock's minimiser is (1, 1), by inspection of its two squares")
        if (allocated(error)) return

        lo10 = -5.0_real64
        hi10 = 5.0_real64
        call pf_minimize_de(sphere, lo10, hi10, 42_int64, x10, fmin, np=100, ftarget=target, &
                            max_gen=20000, info=info)
        call check(error, info%status, PF_OPT_TARGET, "the 10-variable sphere should reach the target")
        if (allocated(error)) return
        call check(error, info%neval <= 200000, "the sphere in 10 variables should cost under 2e5 evaluations")
        if (allocated(error)) return
        call check(error, maxval(abs(x10 - 1.0_real64)) < 1.0e-3_real64, &
            "the sphere's minimiser is 1 in every coordinate")
        if (allocated(error)) return

        call pf_minimize_de(rosenbrock, lo10, hi10, 42_int64, x10, fmin, np=100, ftarget=target, &
                            max_gen=20000, info=info)
        call check(error, info%status, PF_OPT_TARGET, "Rosenbrock in 10 variables should reach the target")
        if (allocated(error)) return
        call check(error, info%neval <= 400000, "Rosenbrock-10 should cost under 4e5 evaluations")
        if (allocated(error)) return

        ! Rastrigin in five variables has about 11**5 local minima, so this is the case a local
        ! engine and a multistart driver both fail; a low CR is what gets DE through it (3.2).
        lo5 = -5.12_real64
        hi5 = 5.12_real64
        call pf_minimize_de(rastrigin, lo5, hi5, 42_int64, x5, fmin, np=50, cr=0.2_real64, &
                            ftarget=target, max_gen=20000, info=info)
        call check(error, info%status, PF_OPT_TARGET, "Rastrigin in 5 variables should reach the target")
        if (allocated(error)) return
        call check(error, info%neval <= 40000, "Rastrigin-5 should cost tens of thousands of evaluations")
        if (allocated(error)) return
        call check(error, maxval(abs(x5)) < 1.0e-3_real64, &
            "Rastrigin's global minimiser is the origin, where every cosine is 1")

    end subroutine test_de_reference_functions

    !> With no target the run ends on the population's value spread, and says so.
    !!
    !! The objective is SHIFTED away from zero on purpose: the fractional test compares the spread
    !! with the size of the values themselves, and on an objective whose minimum is zero it can
    !! never fire, however tight the population gets. That is the same trap the simplex page
    !! states, and this is the case it does work on.
    subroutine test_de_spread_alone(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(shifted_quadratic) :: obj
        real(real64) :: x(3), fmin, lo(3), hi(3)

        obj%shift = 10.0_real64
        lo = -4.0_real64
        hi = 6.0_real64
        call pf_minimize_de(obj, lo, hi, 5_int64, x, fmin, info=info)

        call check(error, info%status, PF_OPT_OK, "the spread test should be what ends this run")
        if (allocated(error)) return
        call check(error, info%converged, "PF_OPT_OK is a converged run")
        if (allocated(error)) return
        call check(error, fmin, 10.0_real64, thr=1.0e-4_real64)
        if (allocated(error)) return
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-2_real64, &
            "the shifted quadratic's minimiser is 1 in every coordinate")
        if (allocated(error)) return
        call check(error, obj%ncall, info%neval, &
            "at threads = 1 the caller's own object is what gets evaluated, so its counter must agree")

    end subroutine test_de_spread_alone

    !> A generation budget too small to converge is PF_OPT_LIMIT, not an error.
    subroutine test_de_generation_budget(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, x, fmin, max_gen=3, info=info)

        call check(error, info%status, PF_OPT_LIMIT, "three generations cannot converge on Rosenbrock")
        if (allocated(error)) return
        call check(error, .not. info%converged, "running out of budget is not convergence")
        if (allocated(error)) return
        call check(error, info%niter, 3, "the run should have spent exactly its three generations")
        if (allocated(error)) return
        ! The starting population plus three generations of one trial each.
        call check(error, info%neval, 4*20, "np = max(20, 10n) here, and each generation costs np")
        if (allocated(error)) return
        call check(error, fmin < 1.0e30_real64, "the best point so far still comes back")

    end subroutine test_de_generation_budget

    !> A NaN region inside the box is "outside my domain", counted, never selected, and never
    !! carried into an ordered comparison.
    !!
    !! **This is the behaviour that separates the population tier from the local one.** The same
    !! objective aborts `pf_minimize_simplex`, because every simplex decision is a comparison of
    !! values; here a non-finite trial simply loses its selection. The vacuity guard is
    !! `info%nonfinite`: without it the test would pass just as well on an objective the population
    !! never left the finite part of.
    !!
    !! **The IEEE_INVALID assertion is what makes the two NaN guards testable at all.** Deleting
    !! either of them -- the `is_finite_quiet` screen on the selection, or the `+Infinity`
    !! substitution that keeps a non-finite incumbent out of `minloc` and `maxval` -- changes no
    !! answer this test could otherwise see, because IEEE makes every ordered comparison against a
    !! NaN false and the right individual wins anyway. What changes is that `<=`, `minsd` and
    !! `maxsd` then SIGNAL on a NaN operand: invisible under gfortran and ifx, fatal under nagfor's
    !! default `-ieee=stop`, which would abort the whole runner on a build this machine cannot run.
    !! Reading the flag turns that into an ordinary assertion here. Both mutations were confirmed
    !! to fail this test and nothing else. The screens themselves raise it too when written as
    !! `ieee_is_finite` over an array, which gfortran vectorises into a signalling compare, so this
    !! test fails under `--profile release` on that spelling and passes under the default profile.
    subroutine test_de_nan_region(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x(2), fmin, lo(2), hi(2)
        logical :: flags_work, was_raised, raised

        lo = -5.0_real64
        hi = 5.0_real64

        ! Restoring the saved value cannot itself abort under nagfor: with halting on, which is its
        ! default, the process would already have gone down when the flag was first raised, so
        ! `was_raised` can only be true on a build that does not halt.
        flags_work = ieee_support_flag(ieee_invalid, 0.0_real64)
        was_raised = .false.
        if (flags_work) then
            call ieee_get_flag(ieee_invalid, was_raised)
            call ieee_set_flag(ieee_invalid, .false.)
        end if

        call pf_minimize_de(nan_corner, lo, hi, 3_int64, x, fmin, ftarget=1.0e-10_real64, info=info)

        raised = .false.
        if (flags_work) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, was_raised)
        end if

        call check(error, info%nonfinite > 0, &
            "vacuity guard: the population never reached the NaN region, so nothing was screened")
        if (allocated(error)) return
        call check(error, info%status, PF_OPT_TARGET, "the minimum outside the region is reachable")
        if (allocated(error)) return
        call check(error, maxval(abs(x)) < 1.0e-4_real64, &
            "nan_corner's minimiser is the origin, by inspection of its sum of squares")
        if (allocated(error)) return
        call check(error, .not. raised, &
            "a NaN reached an ordered comparison: IEEE_INVALID was raised, which is a silent flag " // &
            "under this compiler and a fatal trap under nagfor's default -ieee=stop")

    end subroutine test_de_nan_region

    !> An objective whose parameters live in an allocatable component survives being cloned.
    !!
    !! The clone is `allocate(slot%obj, source=f)`, which must DEEP-copy `centre` into every
    !! thread's own object. A shallow clone either crashes or -- much worse -- leaves a thread
    !! reading an unallocated or shared centre and returning a confident wrong minimum, which is
    !! why the component is the thing the answer depends on. The threaded answer is compared with
    !! the serial one bit for bit, and the caller's own object is checked afterwards.
    subroutine test_de_clones_the_objective(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: serial_info, threaded_info
        type(table_sphere) :: obj
        real(real64) :: xs(3), xt(3), fs, ft, lo(3), hi(3)

        obj%centre = [0.25_real64, -1.5_real64, 2.75_real64]
        lo = -4.0_real64
        hi = 4.0_real64

        call pf_minimize_de(obj, lo, hi, 9_int64, xs, fs, max_gen=200, atol=1.0e-12_real64, &
                            info=serial_info)
        call pf_minimize_de(obj, lo, hi, 9_int64, xt, ft, max_gen=200, atol=1.0e-12_real64, &
                            threads=4, info=threaded_info)

        call check(error, all(xt == xs), "four threads must give the serial point, bit for bit")
        if (allocated(error)) return
        call check(error, ft == fs, "four threads must give the serial value, bit for bit")
        if (allocated(error)) return
        call check(error, threaded_info%neval, serial_info%neval, &
            "the thread count must not change how many evaluations the run needs")
        if (allocated(error)) return
        call check(error, serial_info%status, PF_OPT_OK, "the run should end on its own tolerance")
        if (allocated(error)) return
        call check(error, maxval(abs(xs - obj%centre)) < 1.0e-5_real64, &
            "the minimiser is the centre the allocatable component holds")
        if (allocated(error)) return
        call check(error, allocated(obj%centre), "the caller's own object must come back intact")
        if (allocated(error)) return
        call check(error, size(obj%centre), 3, "and with its component the size it went in at")

    end subroutine test_de_clones_the_objective

    !> The same seed repeats a run exactly; a different seed gives a different one.
    subroutine test_de_seed_decides_the_run(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: xa(2), xb(2), xc(2), fa, fb, fc, lo(2), hi(2)

        lo = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_de(rosenbrock, lo, hi, 1_int64, xa, fa, max_gen=12, info=info)
        call pf_minimize_de(rosenbrock, lo, hi, 1_int64, xb, fb, max_gen=12, info=info)
        call pf_minimize_de(rosenbrock, lo, hi, 2_int64, xc, fc, max_gen=12, info=info)

        call check(error, all(xb == xa), "one seed must repeat its run bit for bit")
        if (allocated(error)) return
        call check(error, fb == fa, "and its value with it")
        if (allocated(error)) return
        call check(error, any(xc /= xa), &
            "vacuity guard: two seeds gave the same point, so the seed reaches nothing")

    end subroutine test_de_seed_decides_the_run

    !> The final population comes back with `np` individuals, every one inside the box.
    subroutine test_de_population(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        real(real64), allocatable :: pop(:,:)
        real(real64) :: x(2), fmin, lo(2), hi(2)
        integer :: j

        lo = [-5.0_real64, -3.0_real64]
        hi = [4.0_real64, 6.0_real64]
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, x, fmin, np=17, max_gen=25, &
                            population=pop, history=record, info=info)

        call check(error, size(pop, 1), 2, "one row per variable")
        if (allocated(error)) return
        call check(error, size(pop, 2), 17, "one column per individual, and np was asked for")
        if (allocated(error)) return
        do j = 1, 2
            call check(error, all(pop(j,:) >= lo(j)) .and. all(pop(j,:) <= hi(j)), &
                "every trial point is clipped to the box, so the population never leaves it")
            if (allocated(error)) return
        end do

        ! The record is one entry per generation plus the starting population's own best.
        call check(error, record%n, info%niter + 1, &
            "DE records the best of each generation, and of the population it started from")
        if (allocated(error)) return
        call check(error, record%f(record%n), fmin, &
            "the last record is the best point the run ended on")

    end subroutine test_de_population

    !> `polish` runs the simplex from the best individual: a lower value, more evaluations.
    subroutine test_de_polish(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: plain, polished
        real(real64) :: xa(2), xb(2), fa, fb, lo(2), hi(2)

        ! Few enough generations that DE is still some way off, so the simplex has work to do.
        lo = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, xa, fa, max_gen=6, info=plain)
        call pf_minimize_de(rosenbrock, lo, hi, 42_int64, xb, fb, max_gen=6, polish=.true., &
                            info=polished)

        call check(error, fb < fa, "polishing from the best individual must not make it worse")
        if (allocated(error)) return
        call check(error, polished%neval > plain%neval, &
            "the simplex's own evaluations are counted in the total")
        if (allocated(error)) return
        call check(error, polished%niter, plain%niter, &
            "polishing adds no generation: niter counts DE's own")

    end subroutine test_de_polish

    !> `polish` minimises the objective seen through the box, so its answer is inside the box.
    !!
    !! The simplex itself is unbounded, so polishing an objective whose unconstrained minimum lies
    !! OUTSIDE the box would walk out of it and report a point and a value the box forbids -- with
    !! `PF_OPT_OK`, since nothing in the run says otherwise. `sphere` is centred at `(1, 1)` and the
    !! box starts at `2`, so the box minimum is the corner `(2, 2)` where the value is `2` and the
    !! free minimum is two units outside.
    subroutine test_de_polish_stays_in_the_box(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x(2), plain(2), fmin, fplain, lo(2), hi(2)

        lo = 2.0_real64
        hi = 3.0_real64
        call pf_minimize_de(sphere, lo, hi, 3_int64, x, fmin, np=8, max_gen=6, polish=.true., &
                            info=info)
        call pf_minimize_de(sphere, lo, hi, 3_int64, plain, fplain, np=8, max_gen=6)

        call check(error, all(x >= lo) .and. all(x <= hi), &
            "the point a polished run reports must lie inside the box it was given")
        if (allocated(error)) return
        call check(error, fmin, 2.0_real64, thr=1.0e-6_real64)
        if (allocated(error)) return
        ! The value reported is the value AT the point reported -- the assertion that fails when a
        ! projection is applied to the point and not to the value.
        call check(error, fmin == sphere(x), &
            "fmin must be the objective's value at the x that comes back")
        if (allocated(error)) return
        call check(error, fmin <= fplain, &
            "and polishing inside the box must not be worse than not polishing at all")

    end subroutine test_de_polish_stays_in_the_box

    !> A mutant component outside the box is put half way between its parent and the bound.
    !!
    !! **The observable is that no individual ever sits exactly ON a bound.** Clipping assigns the
    !! bound itself, so a run pressed against one leaves individuals bit-equal to it; the midpoint
    !! of a parent strictly inside the box and the bound never is the bound. The box starts two
    !! units above `sphere`'s minimiser, so the population crowds into the lower corner and the
    !! rule is exercised on nearly every trial.
    subroutine test_de_out_of_box_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        real(real64), allocatable :: pop(:,:)
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = 2.0_real64
        hi = 3.0_real64
        call pf_minimize_de(sphere, lo, hi, 11_int64, x, fmin, np=16, max_gen=25, population=pop)

        call check(error, minval(abs(pop(1,:) - lo(1))) < 1.0e-2_real64 .and. &
                          minval(abs(pop(2,:) - lo(2))) < 1.0e-2_real64, &
            "vacuity guard: the population never reached the bound, so no out-of-box rule ran")
        if (allocated(error)) return
        call check(error, count(pop(1,:) == lo(1)) + count(pop(2,:) == lo(2)), 0, &
            "an individual exactly on a bound is what clipping leaves and the midpoint cannot")
        if (allocated(error)) return
        call check(error, all(pop >= 2.0_real64) .and. all(pop <= 3.0_real64), &
            "and every individual is still inside the box")

    end subroutine test_de_out_of_box_rule

    !> `max_gen = huge(1)` is a budget, not a way to stop before the first generation.
    !!
    !! The default evaluation budget is `np*(max_gen + 1)`, and forming `max_gen + 1` in default
    !! integer overflows at `huge(1)`: the budget comes out at or below zero and the very first
    !! budget test ends the run with the starting population's best point.
    subroutine test_de_max_gen_ceiling(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -5.0_real64
        hi = 5.0_real64
        call pf_minimize_de(sphere, lo, hi, 7_int64, x, fmin, np=12, max_gen=huge(1), info=info)

        call check(error, info%niter > 1, "a run at the largest max_gen must run generations")
        if (allocated(error)) return
        call check(error, info%neval > 12, "and pay for more than its starting population")
        if (allocated(error)) return
        call check(error, info%converged, "the spread test is what ends it, not the budget")
        if (allocated(error)) return
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-3_real64, &
            "and it reaches sphere's minimiser at 1 in every coordinate")

    end subroutine test_de_max_gen_ceiling

    !> An objective that is NaN everywhere ends the run without a point to report.
    subroutine test_de_nothing_finite(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = [-5.0_real64, -3.0_real64]
        hi = [4.0_real64, 6.0_real64]
        call pf_minimize_de(always_nan, lo, hi, 42_int64, x, fmin, np=8, info=info)

        call check(error, info%status, PF_OPT_NONFINITE, "not one finite value is its own status")
        if (allocated(error)) return
        call check(error, .not. info%converged, "and it is not convergence")
        if (allocated(error)) return
        call check(error, info%nonfinite, 8, "every individual of the starting population counted")
        if (allocated(error)) return
        call check(error, .not. ieee_is_nan(fmin), &
            "the value must be +Infinity, never a NaN a caller's comparison would trap on")
        if (allocated(error)) return
        call check(error, fmin > huge(1.0_real64), "and it must be +Infinity, not merely large")
        if (allocated(error)) return
        call check(error, .not. ieee_is_nan(info%spread), &
            "the spread must be +Infinity too, never the NaN that Inf - Inf gives")
        if (allocated(error)) return
        call check(error, info%spread > huge(1.0_real64), "and the spread must be +Infinity")
        if (allocated(error)) return
        call check(error, all(x == 0.5_real64*(lo + hi)), "the point reported is the box's centre")

    end subroutine test_de_nothing_finite

    !> Every start of a multistart run ends on a stationary point, and several basins are found.
    !!
    !! **The reference is Rastrigin's own gradient**, `2x + 20 pi sin(2 pi x)`, evaluated at each
    !! start's answer -- not a list of locations read off a run. Its minima are near the integer
    !! points but NOT at them (`test_optimize_support.f90` derives the displacement), so a test
    !! asserting integer coordinates would be asserting something false.
    subroutine test_multistart_basins(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        real(real64) :: x(2), fmin, lo(2), hi(2)
        integer :: k

        lo = -5.12_real64
        hi = 5.12_real64
        call pf_minimize_multistart(rastrigin, lo, hi, 11_int64, x, fmin, nstart=20, info=info, &
                                    history=record)

        call check(error, record%n, 20, "the record holds every start's own minimum, before merging")
        if (allocated(error)) return
        do k = 1, record%n
            call check(error, maxval(abs(rastrigin_gradient(record%x(:,k)))) < 1.0e-3_real64, &
                "every local run should end on a zero of the gradient, not part way down a slope")
            if (allocated(error)) return
        end do

        call check(error, info%nminima >= 3, &
            "vacuity guard: twenty starts over this box all landed in one basin, so nothing was counted")
        if (allocated(error)) return
        call check(error, info%nminima <= 20, "no more distinct minima than there were starts")
        if (allocated(error)) return
        call check(error, info%nlimit, 0, "every local run had budget enough to converge")
        if (allocated(error)) return
        call check(error, info%niter, 20, "niter is the number of starts")
        if (allocated(error)) return

        ! Rastrigin's global minimum is exactly 0 at the origin: every cosine is 1 there and the
        ! -10 cos terms cancel the 10n.
        call check(error, maxval(abs(x)) < 1.0e-5_real64, "the best start should reach the origin")
        if (allocated(error)) return
        call check(error, fmin, 0.0_real64, thr=1.0e-9_real64)

    end subroutine test_multistart_basins

    !> The winning start is the lowest-valued one, and on a tie the lowest INDEX.
    !!
    !! Asserted against the record rather than against a location: `x` must be, bit for bit, the
    !! record entry `minloc` picks. That is the rule a "first thread to find a better value wins"
    !! implementation breaks, and it breaks it invisibly -- every such answer is a valid local
    !! minimum of the double well, so only this comparison can tell.
    subroutine test_multistart_tie_rule(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        real(real64) :: x(2), fmin, lo(2), hi(2)
        integer :: kbest

        lo = -3.0_real64
        hi = 3.0_real64
        call pf_minimize_multistart(twin_wells, lo, hi, 5_int64, x, fmin, nstart=16, info=info, &
                                    history=record)

        kbest = minloc(record%f, 1)
        call check(error, all(x == record%x(:,kbest)), &
            "the point returned must be the record entry with the lowest value and lowest index")
        if (allocated(error)) return
        call check(error, fmin == record%f(kbest), "and its value")
        if (allocated(error)) return

        ! The double well's two minima are at x(1) = -1 and x(1) = +1, equal by symmetry; the
        ! vacuity guard is that the starts really did find both, or there would be no tie to break.
        call check(error, any(record%x(1,:) < 0.0_real64) .and. any(record%x(1,:) > 0.0_real64), &
            "vacuity guard: every start landed in the same well, so no tie was ever in play")
        if (allocated(error)) return
        call check(error, maxval(abs(abs(record%x(1,:)) - 1.0_real64)) < 1.0e-4_real64, &
            "both wells sit at x(1) = +/-1, by inspection of (x(1)**2 - 1)**2")
        if (allocated(error)) return
        call check(error, fmin < 1.0e-8_real64, "and the value there is zero")

    end subroutine test_multistart_tie_rule

    !> `xtol` is what decides whether two starts in one basin count once or twice.
    subroutine test_multistart_xtol(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: merged, separate
        real(real64) :: x(2), fmin, lo(2), hi(2)

        ! One basin, twelve starts: every run ends within about 1e-5 of the same point.
        lo = -3.0_real64
        hi = 3.0_real64
        call pf_minimize_multistart(sphere, lo, hi, 5_int64, x, fmin, nstart=12, &
                                    xtol=1.0e-2_real64, info=merged)
        call pf_minimize_multistart(sphere, lo, hi, 5_int64, x, fmin, nstart=12, &
                                    xtol=0.0_real64, info=separate)

        call check(error, merged%nminima, 1, &
            "a merge radius wider than the spread of the answers makes them one minimum")
        if (allocated(error)) return
        call check(error, separate%nminima > merged%nminima, &
            "vacuity guard: with xtol = 0 the same runs must count separately, or xtol reaches nothing")
        if (allocated(error)) return
        call check(error, separate%neval, merged%neval, &
            "xtol decides only the counting; the runs themselves are the same")

    end subroutine test_multistart_xtol

    !> The DEFAULT merge radius counts basins: one objective with one basin counts one minimum.
    !!
    !! `nminima` is only a count of distinct minima where the merge radius exceeds the local
    !! solver's own accuracy: a radius tighter than that splits one basin's answers into several
    !! and the count becomes a count of starts. The sphere has exactly one minimum, so any count
    !! above one here is that failure and nothing else.
    subroutine test_multistart_default_xtol(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        real(real64) :: x(2), fmin, lo(2), hi(2)

        lo = -3.0_real64
        hi = 3.0_real64
        call pf_minimize_multistart(sphere, lo, hi, 5_int64, x, fmin, nstart=12, info=info)

        call check(error, info%nminima, 1, &
            "twelve starts on a single-basin objective are one minimum at the default xtol")
        if (allocated(error)) return
        call check(error, maxval(abs(x - 1.0_real64)) < 1.0e-4_real64, &
            "and the basin they all found is the sphere's own minimiser")

    end subroutine test_multistart_default_xtol

    !> The solver object's options reach every local run, `max_neval` included.
    subroutine test_multistart_solver_options(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(pf_simplex_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)

        solver%max_neval = 5
        lo = -3.0_real64
        hi = 3.0_real64
        call pf_minimize_multistart(sphere, lo, hi, 5_int64, x, fmin, nstart=12, solver=solver, &
                                    info=info)

        call check(error, info%nlimit, 12, "a budget of five cannot converge, so every start runs out")
        if (allocated(error)) return
        call check(error, info%status, PF_OPT_LIMIT, "and the driver says so when no start converged")
        if (allocated(error)) return
        call check(error, .not. info%converged, "which is not convergence")
        if (allocated(error)) return
        ! Three starting vertices, then at most one more engine step before the budget is tested.
        call check(error, info%neval <= 12*8, &
            "the solver's own budget must reach the runs, or they would each cost thousands")

    end subroutine test_multistart_solver_options

    !> No start coming back finite ends the run without a point to report, as it does for DE.
    !!
    !! `pf_simplex_solver` aborts on a NaN before the driver sees one, so the case is reached
    !! through a solver that screens nothing (`unscreened_solver`).
    subroutine test_multistart_nothing_finite(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(unscreened_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2)
        logical :: flags_work, was_raised, raised

        lo = [-5.0_real64, -3.0_real64]
        hi = [4.0_real64, 6.0_real64]

        ! The flag bracket is `test_de_nan_region`'s, written out in place for the same reason: a
        ! helper procedure would have a flag signalling on its entry restored on its return.
        flags_work = ieee_support_flag(ieee_invalid, 0.0_real64)
        was_raised = .false.
        if (flags_work) then
            call ieee_get_flag(ieee_invalid, was_raised)
            call ieee_set_flag(ieee_invalid, .false.)
        end if

        call pf_minimize_multistart(always_nan, lo, hi, 42_int64, x, fmin, nstart=6, solver=solver, &
                                    info=info)

        raised = .false.
        if (flags_work) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, was_raised)
        end if

        call check(error, info%status, PF_OPT_NONFINITE, "no finite start is its own status")
        if (allocated(error)) return
        call check(error, .not. info%converged, "and it is not convergence")
        if (allocated(error)) return
        call check(error, info%nonfinite, 6, "every start counted")
        if (allocated(error)) return
        call check(error, info%nminima, 0, "and no minimum found")
        if (allocated(error)) return
        call check(error, .not. ieee_is_nan(fmin), &
            "the value must be +Infinity, never a NaN a caller's comparison would trap on")
        if (allocated(error)) return
        call check(error, fmin > huge(1.0_real64), "and it must be +Infinity, not merely large")
        if (allocated(error)) return
        call check(error, .not. ieee_is_nan(info%spread), &
            "the spread must be +Infinity too, never the NaN that Inf - Inf gives")
        if (allocated(error)) return
        call check(error, info%spread > huge(1.0_real64), "and the spread must be +Infinity")
        if (allocated(error)) return
        call check(error, all(x == 0.5_real64*(lo + hi)), "the point reported is the box's centre")
        if (allocated(error)) return
        call check(error, .not. raised, &
            "counting the NaN starts raised IEEE_INVALID, which is a silent flag under this " // &
            "compiler and a fatal trap under nagfor's default -ieee=stop")

    end subroutine test_multistart_nothing_finite

    !> NaN starts are counted and passed over, and the lowest finite start is the answer.
    !!
    !! The case between `test_multistart_basins` (every start finite) and
    !! `test_multistart_nothing_finite` (none), and the only one reaching the driver's `+Infinity`
    !! substitution and its per-start screen. `unscreened_solver` leaves each start where it is, so
    !! the record's columns ARE the starts, and `nan_corner` re-evaluated on each is the reference.
    !! Eight Latin-hypercube strata of width 1.25 over `[-5, 5]` guarantee the mix for any seed:
    !! strata 7 and 8 lie wholly above 2, so at least two starts are NaN, and at most three starts
    !! per coordinate lie in strata 6 to 8, so at least two have both coordinates below 2.
    !!
    !! The IEEE_INVALID assertion is `test_de_nan_region`'s, for the same reason, and its bracket is
    !! written out in place for the reason `test_multistart_nothing_finite` gives.
    subroutine test_multistart_some_nan_starts(error)
        type(error_type), allocatable, intent(out) :: error !! set on the first failed assertion
        type(pf_optimize_info) :: info
        type(pf_optimize_history) :: record
        type(unscreened_solver) :: solver
        real(real64) :: x(2), fmin, lo(2), hi(2), fk, fbest
        integer :: k, nnan
        logical :: flags_work, was_raised, raised

        lo = -5.0_real64
        hi = 5.0_real64

        flags_work = ieee_support_flag(ieee_invalid, 0.0_real64)
        was_raised = .false.
        if (flags_work) then
            call ieee_get_flag(ieee_invalid, was_raised)
            call ieee_set_flag(ieee_invalid, .false.)
        end if

        call pf_minimize_multistart(nan_corner, lo, hi, 7_int64, x, fmin, nstart=8, solver=solver, &
                                    info=info, history=record)

        raised = .false.
        if (flags_work) then
            call ieee_get_flag(ieee_invalid, raised)
            call ieee_set_flag(ieee_invalid, was_raised)
        end if

        call check(error, record%n, 8, "one record per start")
        if (allocated(error)) return

        ! Only a value `ieee_is_nan` has passed reaches the ordered comparison.
        nnan = 0
        fbest = huge(1.0_real64)
        do k = 1, record%n
            fk = nan_corner(record%x(:,k))
            if (ieee_is_nan(fk)) then
                nnan = nnan + 1
            else if (fk < fbest) then
                fbest = fk
            end if
        end do

        call check(error, nnan >= 2 .and. nnan <= 6, &
            "vacuity guard: eight strata over [-5, 5] must give both NaN and finite starts")
        if (allocated(error)) return
        call check(error, info%nonfinite, nnan, "every NaN start counted, and nothing else")
        if (allocated(error)) return
        call check(error, info%status /= PF_OPT_NONFINITE, "a finite start exists, so a minimum does")
        if (allocated(error)) return
        call check(error, fmin == fbest, "the lowest finite start is the answer")
        if (allocated(error)) return
        call check(error, fmin == nan_corner(x), "and x is the start that value came from")
        if (allocated(error)) return
        call check(error, info%nminima >= 1, "a finite start is at least one minimum")
        if (allocated(error)) return
        call check(error, .not. raised, &
            "a NaN start reached an ordered comparison or a signalling screen: IEEE_INVALID was " // &
            "raised, which is a silent flag under this compiler and a fatal trap under nagfor's " // &
            "default -ieee=stop")

    end subroutine test_multistart_some_nan_starts

end module test_optimize
