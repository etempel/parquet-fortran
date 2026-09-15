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
    use iso_fortran_env, only : real64

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
                         test_simplex_solver_object) &
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

end module test_optimize
