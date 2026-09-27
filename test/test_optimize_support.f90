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

    use parquet_optimize, only : pf_objective, pf_constrained_objective, pf_local_solver, &
                                 pf_optimize_info, pf_objective_func
    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf

    implicit none
    private

    public :: shifted_quadratic, line_fit, unit_disc, table_sphere, function_objective
    public :: rosenbrock, rosenbrock_1e300, brown_almost_linear, sphere_1e13
    public :: sphere, origin_sphere, quad1d, quad1d_min, one_dim, one_dim_min
    public :: constant_one, shifted_norm, quartic, quartic_derivative
    public :: always_nan, always_inf, nan_beyond_two
    public :: rastrigin, rastrigin_gradient, twin_wells, nan_corner
    public :: bad_scaling, bad_scaling_unit, BAD_SCALING_SCALE, BAD_SCALING_MIN
    public :: disc_fit, outside_disc, dist12, sphere123, SPHERE123_CENTRE
    public :: negative_count_disc, nan_constraint_disc, nan_value_disc, unscreened_solver
    public :: counted_hash

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

    !> A plain function behind the object interface, for the tests asserting that an engine's
    !! object form and plain-function form walk one path, evaluation for evaluation.
    !!
    !! **The object calls the SAME function the plain form is given**, through a pointer the
    !! compiler cannot resolve, as the engines' own adapter for the plain form does. An object
    !! writing that function's arithmetic out again does not round alike under an optimiser
    !! (`fortran-gotchas.md`, "Two loops written to mirror each other do not round alike"):
    !! gfortran at `--profile release` on arm64 fuses `sphere` and a `sum((x - centre)**2)` into
    !! different multiply-adds, and the two searches part at the first last-bit difference.
    type, extends(pf_objective) :: function_objective
        procedure(pf_objective_func), nopass, pointer :: fn => null() !! the plain function it calls
        integer :: ncall = 0 !! evaluations, written by `eval` and read by the caller
    contains
        procedure :: eval => function_objective_eval !! Calls `fn` and counts the call.
    end type function_objective

    !> An objective whose value depends on WHEN it is called, not where: a hash of its own call
    !! count, in `[0, 1000)`.
    !!
    !! **It exists so that a simplex cannot converge on it while every value stays finite.** A
    !! shrinking simplex converges when the spread of the values at its vertices falls below the
    !! tolerance; here re-evaluating the same point gives a different number, so the spread never
    !! settles and the run can only end on its budget. An objective that merely oscillates finely in
    !! `x` would not do: Nelder-Mead contracts geometrically, so within a few hundred iterations the
    !! vertices coincide to the last bit and the spread collapses to zero whatever the shape.
    !!
    !! The counter is a COMPONENT, so the object is reentrant: the multistart driver clones the
    !! objective once per thread and each clone counts its own calls.
    type, extends(pf_objective) :: counted_hash
        integer(int64) :: ncall = 0_int64 !! evaluations so far, written by `eval`
    contains
        procedure :: eval => counted_hash_eval !! A hash of the call count, never the point.
    end type counted_hash

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

    !> The worked constrained objective: squared distance from `(1, 2)`, restricted to the unit
    !! disc.
    !!
    !! Two variables exactly, unlike `unit_disc`: this one exists to reproduce the guide's own
    !! examples, whose answers are worked out on the page in two dimensions. The constrained
    !! minimum is the point of the unit circle nearest `(1, 2)`, which is `(1, 2)/sqrt(5)`.
    type, extends(pf_constrained_objective) :: disc_fit
    contains
        procedure :: eval => disc_fit_eval               !! Squared distance from `(1, 2)`.
        procedure :: n_constraints => disc_fit_count     !! One constraint.
        procedure :: constraints => disc_fit_constraints !! The unit disc, as `c(x) <= 0`.
    end type disc_fit

    !> Squared distance from the origin, restricted to the OUTSIDE of the unit disc.
    !!
    !! The constraint-sign test. `c(x) = 1 - |x|^2 <= 0` keeps the search OUT of the disc, which
    !! is the harder side: the unconstrained minimum, the origin, is infeasible, so the answer is
    !! on the circle with value `1`. Write the constraint the other way round -- SciPy's
    !! convention, `c(x) >= 0` feasible -- and the same solver returns the origin with value `0`,
    !! which is what the test asserts against.
    type, extends(pf_constrained_objective) :: outside_disc
    contains
        procedure :: eval => outside_disc_eval               !! Squared distance from the origin.
        procedure :: n_constraints => outside_disc_count     !! One constraint.
        procedure :: constraints => outside_disc_constraints !! Outside the unit disc, `c <= 0`.
    end type outside_disc

    !> A constrained objective whose `n_constraints` answers a negative number.
    !!
    !! Exists only so `pf_minimize_cobyla`'s refusal has something to refuse. A negative count
    !! would either make `allocate(c(m))` fail or silently produce a zero-length array, and the
    !! caller would never learn which.
    type, extends(outside_disc) :: negative_count_disc
    contains
        procedure :: n_constraints => negative_count              !! A negative number, on purpose.
    end type negative_count_disc

    !> A constrained objective whose `constraints` returns a NaN.
    !!
    !! Exists only so the constraint screen has something to catch. Upstream's `moderatec` would
    !! clamp the value and carry on, and the NaN would then decide which point COBYLA's filter
    !! keeps.
    type, extends(outside_disc) :: nan_constraint_disc
    contains
        procedure :: constraints => nan_constraint                !! Returns a NaN.
    end type nan_constraint_disc

    !> A constrained objective whose `eval` returns a NaN.
    !!
    !! The twin of `nan_constraint_disc` on the other screen: `evaluate_fc` checks the objective
    !! value before it looks at the constraints at all, and that arm is reachable only from a
    !! constrained objective, since an unconstrained one goes through `evaluate` instead.
    type, extends(outside_disc) :: nan_value_disc
    contains
        procedure :: eval => nan_value                            !! Returns a NaN.
    end type nan_value_disc

    !> A local solver that evaluates its start once and hands back whatever the objective said.
    !!
    !! It screens nothing, as a caller's own `pf_local_solver` extension is free not to: with
    !! `always_nan` every start comes back NaN, which is the only route to the multistart driver's
    !! `PF_OPT_NONFINITE` case, since `pf_simplex_solver` aborts on the first NaN itself.
    type, extends(pf_local_solver) :: unscreened_solver
    contains
        procedure :: run => unscreened_run !! Evaluates the start once and reports that value.
    end type unscreened_solver

    !> The centre of `sphere123`, and its unconstrained minimiser.
    real(real64), parameter :: SPHERE123_CENTRE(3) = [1.0_real64, 2.0_real64, 3.0_real64]

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

    !> The characteristic magnitude of each coordinate of `bad_scaling`, its `scale=` argument.
    real(real64), parameter :: BAD_SCALING_SCALE(2) = [1.0e-3_real64, 1.0e3_real64]

    !> The minimiser of `bad_scaling`, by construction: each term vanishes there.
    real(real64), parameter :: BAD_SCALING_MIN(2) = BAD_SCALING_SCALE

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

    !> A hash of the call count, in `[0, 1000)`: finite, bounded, and never the same twice running.
    function counted_hash_eval(self, x) result(f)
        class(counted_hash), intent(inout) :: self !! the objective, which counts its own calls
        real(real64), intent(in)           :: x(:) !! the point, which the value ignores
        real(real64)                       :: f    !! the hash of `self%ncall`

        ! Knuth's multiplicative constant, in int64 so the product cannot overflow, then folded to
        ! [0, 1000). `x` is referenced so that no compiler may drop the argument.
        self%ncall = self%ncall + 1_int64 + int(0.0_real64*sum(x), int64)
        f = real(modulo(self%ncall*2654435761_int64, 1000_int64), real64)

    end function counted_hash_eval

    !> Evaluates the shifted quadratic and counts the call.
    function shifted_quadratic_eval(self, x) result(f)
        class(shifted_quadratic), intent(inout) :: self !! the objective, shift and counter
        real(real64), intent(in)                :: x(:) !! the point
        real(real64)                            :: f    !! objective value at `x`

        f = sum((x - 1.0_real64)**2) + self%shift
        self%ncall = self%ncall + 1

    end function shifted_quadratic_eval

    !> Calls the plain function the object holds, and counts the call.
    function function_objective_eval(self, x) result(f)
        class(function_objective), intent(inout) :: self !! the objective, its function and counter
        real(real64), intent(in)                 :: x(:) !! the point
        real(real64)                             :: f    !! `fn(x)`

        f = self%fn(x)
        self%ncall = self%ncall + 1

    end function function_objective_eval

    !> Sum of squared residuals about a straight line, and counts the call.
    function line_fit_eval(self, x) result(f)
        class(line_fit), intent(inout) :: self !! the objective, data and counter
        real(real64), intent(in)       :: x(:) !! slope and intercept
        real(real64)                   :: f    !! residual sum of squares

        f = sum((self%yd - (x(1)*self%xd + x(2)))**2)
        self%ncall = self%ncall + 1

    end function line_fit_eval

    !> Squared distance from the all-ones point, in any number of variables.
    function unit_disc_eval(self, x) result(f)
        class(unit_disc), intent(inout) :: self !! the objective
        real(real64), intent(in)        :: x(:) !! the point
        real(real64)                    :: f    !! squared distance from the all-ones point

        f = sum((x - 1.0_real64)**2)

    end function unit_disc_eval

    !> How many constraint values `constraints` fills.
    function unit_disc_count(self) result(m)
        class(unit_disc), intent(in) :: self !! the objective
        integer                      :: m    !! one

        m = 1

    end function unit_disc_count

    !> The unit ball as `c(x) <= 0`.
    subroutine unit_disc_constraints(self, x, c)
        class(unit_disc), intent(inout) :: self !! the objective
        real(real64), intent(in)        :: x(:) !! the point
        real(real64), intent(out)       :: c(:) !! exactly `n_constraints()` values

        c(1) = sum(x**2) - 1.0_real64

    end subroutine unit_disc_constraints

    !> Squared distance from `(1, 2)`.
    function disc_fit_eval(self, x) result(f)
        class(disc_fit), intent(inout) :: self !! the objective
        real(real64), intent(in)       :: x(:) !! the point
        real(real64)                   :: f    !! squared distance from `(1, 2)`

        f = dist12(x)

    end function disc_fit_eval

    !> How many constraint values `constraints` fills.
    function disc_fit_count(self) result(m)
        class(disc_fit), intent(in) :: self !! the objective
        integer                     :: m    !! one

        m = 1

    end function disc_fit_count

    !> The unit disc as `c(x) <= 0`.
    subroutine disc_fit_constraints(self, x, c)
        class(disc_fit), intent(inout) :: self !! the objective
        real(real64), intent(in)       :: x(:) !! the point
        real(real64), intent(out)      :: c(:) !! exactly `n_constraints()` values

        c(1) = x(1)**2 + x(2)**2 - 1.0_real64

    end subroutine disc_fit_constraints

    !> Squared distance from `(1, 2)`, as a plain function: the same objective for LINCOA, which
    !! refuses a constrained object.
    function dist12(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! squared distance from `(1, 2)`

        f = (x(1) - 1.0_real64)**2 + (x(2) - 2.0_real64)**2

    end function dist12

    !> Squared distance from the origin.
    function outside_disc_eval(self, x) result(f)
        class(outside_disc), intent(inout) :: self !! the objective
        real(real64), intent(in)           :: x(:) !! the point
        real(real64)                       :: f    !! squared distance from the origin

        f = sum(x**2)

    end function outside_disc_eval

    !> How many constraint values `constraints` fills.
    function outside_disc_count(self) result(m)
        class(outside_disc), intent(in) :: self !! the objective
        integer                         :: m    !! one

        m = 1

    end function outside_disc_count

    !> The OUTSIDE of the unit disc as `c(x) <= 0`: feasible where `|x| >= 1`.
    subroutine outside_disc_constraints(self, x, c)
        class(outside_disc), intent(inout) :: self !! the objective
        real(real64), intent(in)           :: x(:) !! the point
        real(real64), intent(out)          :: c(:) !! exactly `n_constraints()` values

        c(1) = 1.0_real64 - sum(x**2)

    end subroutine outside_disc_constraints

    !> A negative constraint count, which no engine may accept.
    function negative_count(self) result(m)
        class(negative_count_disc), intent(in) :: self !! the objective
        integer                                :: m    !! a negative number, on purpose

        m = -1

    end function negative_count

    !> A constraint value that is a NaN.
    subroutine nan_constraint(self, x, c)
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        class(nan_constraint_disc), intent(inout) :: self !! the objective
        real(real64), intent(in)                  :: x(:) !! the point
        real(real64), intent(out)                 :: c(:) !! one NaN

        c(1) = ieee_value(1.0_real64, ieee_quiet_nan) + sum(x)*0.0_real64

    end subroutine nan_constraint

    !> A NaN objective value, formed with `ieee_value` so no fixture arithmetic raises a flag.
    function nan_value(self, x) result(f)
        use, intrinsic :: ieee_arithmetic, only : ieee_value, ieee_quiet_nan
        class(nan_value_disc), intent(inout) :: self !! the objective
        real(real64), intent(in)             :: x(:) !! the point
        real(real64)                         :: f    !! a NaN

        f = ieee_value(1.0_real64, ieee_quiet_nan) + sum(x)*0.0_real64

    end function nan_value

    !> `sphere` scaled by `1e13`, so the model GRADIENT exceeds the engines' `1e12` threshold.
    !!
    !! `bad_scaling` makes the COORDINATES badly scaled; this makes the VALUE large, which is a
    !! different thing and reaches the gradient-rescaling arm of LINCOA's trust-region step.
    !! The minimiser is unchanged at 1 in every coordinate, since scaling by a positive constant
    !! does not move it.
    function sphere_1e13(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! `1e13` times `sphere(x)`

        f = 1.0e13_real64*sum((x - 1.0_real64)**2)

    end function sphere_1e13

    !> One evaluation at the start, reported as the minimum with no finiteness screen.
    subroutine unscreened_run(self, f, x, fmin, lower, upper, info)
        class(unscreened_solver), intent(in) :: self     !! the solver
        class(pf_objective), intent(inout)   :: f        !! the objective
        real(real64), intent(inout)          :: x(:)     !! the start, left where it is
        real(real64), intent(out)            :: fmin     !! the objective's value at `x`, unscreened
        real(real64), intent(in)             :: lower(:) !! the driver's box, not used
        real(real64), intent(in)             :: upper(:) !! the driver's box, not used
        type(pf_optimize_info), intent(out)  :: info     !! one evaluation

        fmin = f%eval(x)
        info%neval = 1

    end subroutine unscreened_run

    !> Squared distance from `SPHERE123_CENTRE`, for the linearly constrained KKT test.
    function sphere123(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! squared distance from `(1, 2, 3)`

        f = sum((x - SPHERE123_CENTRE)**2)

    end function sphere123

    !> Rosenbrock's function; minimum `0` at `(1, 1)`, by inspection of its two squares.
    function rosenbrock(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = (1.0_real64 - x(1))**2 + 100.0_real64*(x(2) - x(1)**2)**2

    end function rosenbrock

    !> Sum of squares about the ORIGIN; minimum `0` at `x = 0` in every coordinate.
    !!
    !! The minimiser is exactly zero, which `sphere`'s is not: a tolerance proportional to `abs(x)`
    !! alone vanishes there, so this is the objective the scalar engine's tolerance floor is about.
    function origin_sphere(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = sum(x**2)

    end function origin_sphere

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

    !> Rosenbrock's function multiplied by `1e300`, whose values fill the top of the exponent range.
    !!
    !! The engines compare a predicted reduction in the OBJECTIVE's units against a radius in the
    !! variables', and they form sums of squares of model gradients; at this magnitude those sums
    !! overflow although every value the objective returns is finite. The minimiser is
    !! Rosenbrock's own, `(1, 1)`: multiplying by a positive constant moves no minimum.
    function rosenbrock_1e300(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! `1e300` times `rosenbrock(x)`

        f = 1.0e300_real64*rosenbrock(x)

    end function rosenbrock_1e300

    !> Brown's almost-linear function, More-Garbow-Hillstrom problem 27, in any dimension.
    !!
    !! The last term is a product of every coordinate, so the model's own geometry step can find
    !! itself with almost no free curvature to work with -- which is the configuration
    !! `BOBYQA runs the configuration whose geometry step overflows` exists to reach.
    function brown_almost_linear(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        real(real64) :: s
        integer :: i, n

        n = size(x)
        f = 0.0_real64
        ! Added in index order, not by the intrinsic: flang's runtime `SUM` compensates its rounding
        ! at `-O0`, and one ulp here puts the reproducer on a search path that misses its site.
        s = 0.0_real64
        do i = 1, n
            s = s + x(i)
        end do
        s = s - real(n + 1, real64)
        do i = 1, n - 1
            f = f + (x(i) + s)**2
        end do
        f = f + (product(x) - 1.0_real64)**2

    end function brown_almost_linear

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
    function table_sphere_eval(self, x) result(f)
        class(table_sphere), intent(inout) :: self !! the objective and its centre
        real(real64), intent(in)           :: x(:) !! the point
        real(real64)                       :: f    !! squared distance from `this%centre`

        f = sum((x - self%centre)**2)

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

    !> A quadratic whose two coordinates differ by six orders of magnitude.
    !!
    !! `((x(1) - 1e-3)/1e-3)**2 + ((x(2) - 1e3)/1e3)**2`: the minimum is `0` at
    !! `BAD_SCALING_MIN = (1e-3, 1e3)`, and in the SCALED variable `y = x/BAD_SCALING_SCALE` it is
    !! the unit-conditioned `(y(1) - 1)**2 + (y(2) - 1)**2`. One trust-region radius has to serve
    !! both coordinates, so this is the shape `scale=` exists for: without it a radius small
    !! enough to resolve `x(1)` cannot move `x(2)` at all.
    function bad_scaling(x) result(f)
        real(real64), intent(in) :: x(:) !! the point
        real(real64)             :: f    !! objective value at `x`

        f = ((x(1) - BAD_SCALING_MIN(1))/BAD_SCALING_SCALE(1))**2 &
            + ((x(2) - BAD_SCALING_MIN(2))/BAD_SCALING_SCALE(2))**2

    end function bad_scaling

    !> `bad_scaling` composed with its own scaling: `g(y) = bad_scaling(BAD_SCALING_SCALE*y)`.
    !!
    !! Written as the composition rather than as the algebraically equal `(y(1)-1)**2 +
    !! (y(2)-1)**2`, because the test that uses it asserts BIT equality between a `scale=` run of
    !! `bad_scaling` and a plain run of this: a hand-simplified form would differ in the last bits
    !! and the test would then be measuring the simplification instead of the scaling.
    !!
    !! **The scaled point is `volatile`, so that it is rounded before `bad_scaling` reads it**, as
    !! the library's own `scale*y` is. Otherwise an optimiser inlines `bad_scaling` here and fuses
    !! the product into its `x - BAD_SCALING_MIN` -- gfortran does at `--profile release` on arm64
    !! -- and the two runs part in the last bit (`fortran-gotchas.md`, "Calling one procedure from
    !! another does not pin its last bit").
    function bad_scaling_unit(y) result(f)
        real(real64), intent(in) :: y(:) !! the point, in scaled units
        real(real64)             :: f    !! `bad_scaling` at `BAD_SCALING_SCALE*y`

        real(real64), volatile :: x(size(y))

        x = BAD_SCALING_SCALE*y
        f = bad_scaling(x)

    end function bad_scaling_unit

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
