!> The objectives `test_optimize.f90` and `test/error_scenarios.f90` share, as module procedures
!> and as `pf_objective` extensions.
!!
!! **Every callback here is a module procedure or a type-bound procedure, never an internal one.**
!! flang SIGSEGVs before the callee runs when an internal procedure is passed as an actual
!! argument, and gfortran implements one with a trampoline that makes the binary demand an
!! executable stack. That is the whole library's callback convention, so the tests have to be
!! written the way the guide tells a user to write them.
!!
!! **The minimiser of every objective here is stated beside it, with its derivation**, so that no
!! test asserts a location or a value that was read off a run.
!!
!! Its only library import is `use parquet_optimize`: a scenario and a test must be able to name
!! the same objective without either of them reaching the facade.
module test_optimize_support

    use parquet_optimize, only : pf_objective, pf_constrained_objective
    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf

    implicit none
    private

    public :: shifted_quadratic, line_fit, unit_disc, table_sphere
    public :: rosenbrock, sphere, quad1d, quad1d_min, one_dim, one_dim_min
    public :: constant_one, shifted_norm, quartic, quartic_derivative
    public :: always_nan, always_inf, nan_beyond_two
    public :: rastrigin, rastrigin_gradient, twin_wells, nan_corner

    !> Sum of squares about `1` plus a shift, counting its own evaluations.
    !!
    !! Minimum `shift` at `x = 1` in every coordinate, by inspection: the sum of squares is
    !! non-negative and vanishes only there. qfeet's `shifted_quadratic`, carried over so that its
    !! `amoeba` cases migrate unchanged.
    type, extends(pf_objective) :: shifted_quadratic
        real(real64) :: shift = 0.0_real64 !! added to every value, so the minimum need not be zero
        integer      :: ncall = 0          !! evaluations, written by `eval` and read by the caller
    contains
        procedure :: eval => shifted_quadratic_eval !! Evaluates the shifted quadratic.
    end type shifted_quadratic

    !> Sum of squared residuals about a straight line, counting its own evaluations.
    !!
    !! The least-squares line through the five points is exact and is derived in
    !! `line_fit_slope`/`line_fit_intercept` below, so the test asserts a reference rather than a
    !! recorded output. qfeet's `line_fit`, and the guide page's objective-object example.
    type, extends(pf_objective) :: line_fit
        real(real64) :: xd(5) = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64] !! abscissae
        real(real64) :: yd(5) = [2.1_real64, 3.9_real64, 6.2_real64, 7.8_real64, 10.1_real64] !! ordinates
        integer      :: ncall = 0 !! evaluations, written by `eval` and read by the caller
    contains
        procedure :: eval => line_fit_eval !! Sum of squared residuals about a straight line.
    end type line_fit

    !> A constrained objective, existing only so the refusal at every unconstrained entry point
    !! has something to refuse.
    !!
    !! Squared distance from the all-ones point, restricted to the unit ball
    !! `c(x) = sum(x**2) - 1 <= 0`. **Written for ANY number of variables on purpose**: a scenario
    !! that handed a two-variable objective to the one-variable engine would abort on an
    !! out-of-bounds read rather than on the refusal, and would keep passing with the refusal
    !! removed. No engine in `parquet_optimize` honours it; `pf_minimize_cobyla` will.
    type, extends(pf_constrained_objective) :: unit_disc
    contains
        procedure :: eval => unit_disc_eval                   !! Squared distance from `(1, 2)`.
        procedure :: n_constraints => unit_disc_count         !! One constraint.
        procedure :: constraints => unit_disc_constraints     !! The disc, as `c(x) <= 0`.
    end type unit_disc

    !> A sphere whose centre lives in an ALLOCATABLE component, for the per-thread clone test.
    !!
    !! The clone the population engines make is `allocate(slot%obj, source=f)`, a sourced
    !! allocation: an allocatable component must be DEEP-copied into every clone, not shared and
    !! not left unallocated. A shallow clone segfaults or, worse, reads another thread's centre and
    !! returns a plausible wrong minimum -- which is why the component is what the answer depends
    !! on rather than a counter nobody checks.
    type, extends(pf_objective) :: table_sphere
        real(real64), allocatable :: centre(:) !! the minimiser; deep-copied into every clone
    contains
        procedure :: eval => table_sphere_eval !! Squared distance from `centre`.
    end type table_sphere

    !> `2 pi`, shared by `rastrigin` and its gradient so the two cannot drift apart.
    real(real64), parameter :: RASTRIGIN_TWO_PI = 2.0_real64*acos(-1.0_real64)

    !> Slope of the exact least-squares line through `line_fit`'s five points.
    !!
    !! `Sxy/Sxx` with `xd` centred on `3`: `Sxy = 19.9`, `Sxx = 10`, so the slope is `1.99`
    !! exactly. Written as the quotient rather than as `1.99` so the derivation is in the source.
    real(real64), parameter, public :: line_fit_slope = 19.9_real64/10.0_real64

    !> Intercept of that same line: `mean(yd) - slope*mean(xd) = 6.02 - 1.99*3 = 0.05`.
    real(real64), parameter, public :: line_fit_intercept = 6.02_real64 - line_fit_slope*3.0_real64

    !> Residual sum of squares at that line.
    !!
    !! The five residuals are `0.06, -0.13, 0.18, -0.21, 0.10`, whose squares sum to `0.107`.
    real(real64), parameter, public :: line_fit_rss = 0.107_real64

contains

    !> Evaluates the shifted quadratic and counts the call.
    function shifted_quadratic_eval(this, x) result(f)
        class(shifted_quadratic), intent(inout) :: this !! the objective, shift and counter
        real(real64), intent(in)                :: x(:) !! the point
        real(real64)                            :: f    !! objective value at `x`

        f = sum((x - 1.0_real64)**2) + this%shift
        this%ncall = this%ncall + 1

    end function shifted_quadratic_eval

    !> Sum of squared residuals about a straight line, and counts the call.
    function line_fit_eval(this, x) result(f)
        class(line_fit), intent(inout) :: this !! the objective, data and counter
        real(real64), intent(in)       :: x(:) !! slope and intercept
        real(real64)                   :: f    !! residual sum of squares

        f = sum((this%yd - (x(1)*this%xd + x(2)))**2)
        this%ncall = this%ncall + 1

    end function line_fit_eval

    !> Squared distance from the all-ones point, in any number of variables.
    function unit_disc_eval(this, x) result(f)
        class(unit_disc), intent(inout) :: this !! the objective
        real(real64), intent(in)        :: x(:) !! the point
        real(real64)                    :: f    !! squared distance from the all-ones point

        f = sum((x - 1.0_real64)**2)

    end function unit_disc_eval

    !> How many constraint values `constraints` fills.
    function unit_disc_count(this) result(m)
        class(unit_disc), intent(in) :: this !! the objective
        integer                      :: m    !! one

        m = 1

    end function unit_disc_count

    !> The unit ball as `c(x) <= 0`.
    subroutine unit_disc_constraints(this, x, c)
        class(unit_disc), intent(inout) :: this !! the objective
        real(real64), intent(in)        :: x(:) !! the point
        real(real64), intent(out)       :: c(:) !! exactly `n_constraints()` values

        c(1) = sum(x**2) - 1.0_real64

    end subroutine unit_disc_constraints

    !> Rosenbrock's function; minimum `0` at `(1, 1)`, by inspection of its two squares.
    function rosenbrock(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (1.0_real64 - x(1))**2 + 100.0_real64*(x(2) - x(1)**2)**2

    end function rosenbrock

    !> Sum of squares about `1`; minimum `0` at `x = 1` in every coordinate.
    function sphere(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = sum((x - 1.0_real64)**2)

    end function sphere

    !> One-variable quadratic `(x - 1)**2`; minimum `0` at `x = 1`.
    function quad1d(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (x(1) - 1.0_real64)**2

    end function quad1d

    !> One-variable quadratic `(x - 2)**2`; minimum `0` at `x = 2`.
    function one_dim(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (x(1) - 2.0_real64)**2

    end function one_dim

    !> A constant objective, so every simplex vertex has the same value.
    !!
    !! `0.0*sum(x)` keeps `x` used, so no compiler may drop the argument.
    function constant_one(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! always `1`

        f = 1.0_real64 + 0.0_real64*sum(x)

    end function constant_one

    !> Scaled Euclidean distance from `1`; minimum `0` at `x = 1`, and NOT smooth there.
    !!
    !! qfeet's `func_to_minimize`. The kink at the minimum is the point: a simplex handles it and
    !! a model-based solver would not.
    function shifted_norm(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = norm2(x - 1.0_real64)*1.0e-3_real64

    end function shifted_norm

    !> `(x - 1/2)**4 + x**2`, the guide page's one-variable example.
    !!
    !! Its stationary points solve `4*(x - 1/2)**3 + 2*x = 0`, which has one real root; the tests
    !! assert that root's defining equation rather than a decimal read off a run.
    function quartic(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (x(1) - 0.5_real64)**4 + x(1)**2

    end function quartic

    !> Derivative of `quartic`, so a test can assert its minimiser is a stationary point.
    pure function quartic_derivative(x) result(d)
        real(real64), intent(in) :: x !! the point
        real(real64)             :: d !! `4*(x - 1/2)**3 + 2*x`

        d = 4.0_real64*(x - 0.5_real64)**3 + 2.0_real64*x

    end function quartic_derivative

    !> An objective that is NaN everywhere, for the non-finite abort.
    function always_nan(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! a quiet NaN

        f = ieee_value(1.0_real64, ieee_quiet_nan) + 0.0_real64*sum(x)

    end function always_nan

    !> An objective that is `+Infinity` everywhere, for the non-finite abort.
    function always_inf(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! `+Infinity`

        f = ieee_value(1.0_real64, ieee_positive_inf) + 0.0_real64*sum(x)

    end function always_inf

    !> Finite for `|x| <= 2` and NaN outside it, with its MINIMISER outside: `x = 3`.
    !!
    !! That placement is the whole point. A starting simplex inside the finite region evaluates
    !! cleanly, so the run begins normally; the simplex then walks towards `(3, 3)` and the first
    !! trial point that crosses the boundary returns the NaN. An engine screening only its
    !! STARTING simplex, as qfeet's does, cannot see that -- which is why the port screens every
    !! value at the evaluation site instead.
    function nan_beyond_two(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! squares about `3` inside the region, NaN outside it

        if (maxval(abs(x)) > 2.0_real64) then
            f = ieee_value(1.0_real64, ieee_quiet_nan)
        else
            f = sum((x - 3.0_real64)**2)
        end if

    end function nan_beyond_two

    !> Squared distance from a centre held in an allocatable component.
    function table_sphere_eval(this, x) result(f)
        class(table_sphere), intent(inout) :: this !! the objective and its centre
        real(real64), intent(in)           :: x(:) !! the point
        real(real64)                       :: f    !! squared distance from `this%centre`

        f = sum((x - this%centre)**2)

    end function table_sphere_eval

    !> Rastrigin's function, `10n + sum(x**2 - 10 cos(2 pi x))`.
    !!
    !! The global minimum is `0` at the ORIGIN, exactly, and `[-5.12, 5.12]**n` holds `11**n` local
    !! minima -- which is what makes it the function a multistart driver cannot exhaust and a
    !! population engine can.
    !!
    !! **Its other minima are NEAR the integer points, not at them**, so "the value there is
    !! `sum(k**2)`" is false and no test may assert it: the stationarity condition
    !! `2x + 20 pi sin(2 pi x) = 0` has its root near `k` displaced by about `-k/(1 + 20 pi**2)`,
    !! which puts the minimum beside `k = 1` at `0.99496` rather than at `1`. The derived reference
    !! a test CAN assert is that condition itself, which `rastrigin_gradient` returns.
    function rastrigin(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = 10.0_real64*size(x) + sum(x**2 - 10.0_real64*cos(RASTRIGIN_TWO_PI*x))

    end function rastrigin

    !> The exact gradient of `rastrigin`: `2x + 20 pi sin(2 pi x)`, coordinate by coordinate.
    !!
    !! Every local minimum of `rastrigin` is a zero of this, so a test asserting that a run ended
    !! on one is asserting a derived property of the function rather than a location read off a
    !! previous run.
    pure function rastrigin_gradient(x) result(g)
        real(real64), intent(in) :: x(:)     !! the point
        real(real64)             :: g(size(x)) !! the gradient at `x`

        g = 2.0_real64*x + 10.0_real64*RASTRIGIN_TWO_PI*sin(RASTRIGIN_TWO_PI*x)

    end function rastrigin_gradient

    !> A double well, `(x(1)**2 - 1)**2 + x(2)**2`, with TWO minima of exactly equal value.
    !!
    !! `0` at `(-1, 0)` and at `(1, 0)`, by inspection: both terms are non-negative and both vanish
    !! there. The two are equal by the function's own symmetry in `x(1)`, not by arithmetic that
    !! happens to agree, which is what a test of the tie rule needs.
    function twin_wells(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (x(1)**2 - 1.0_real64)**2 + x(2)**2

    end function twin_wells

    !> Squares about the origin, NaN wherever any coordinate exceeds 2.
    !!
    !! The minimum `0` at the origin is well away from the NaN region, so a population engine that
    !! treats a non-finite value as "outside my domain" finds it while still meeting the region:
    !! over `[-5, 5]` more than half of each coordinate's range is inside the NaN half-space, so a
    !! spread population cannot avoid it. A local engine would abort here instead, which is the
    !! difference the guide page draws between the two tiers.
    function nan_corner(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! squares about the origin, or NaN

        if (any(x > 2.0_real64)) then
            f = ieee_value(1.0_real64, ieee_quiet_nan)
        else
            f = sum(x**2)
        end if

    end function nan_corner

    !> The minimiser of `quad1d`.
    pure function quad1d_min() result(x)
        real(real64) :: x !! the minimiser of `quad1d`

        x = 1.0_real64

    end function quad1d_min

    !> The minimiser of `one_dim`.
    pure function one_dim_min() result(x)
        real(real64) :: x !! the minimiser of `one_dim`

        x = 2.0_real64

    end function one_dim_min

end module test_optimize_support
