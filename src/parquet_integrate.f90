!> Adaptive numerical integration of a function of one `real64` variable: `pf_integrate`, the
!> 21-point Gauss-Kronrod rule with adaptive bisection over a finite range.
!!
!! `parquet_integrate` is an **Arrow-free leaf**: it imports the two INTRINSIC modules
!! `iso_fortran_env` and `ieee_arithmetic`, and no module of this library. An intrinsic module is
!! not a compiled file, not a tier edge and not a footprint entry, so `use parquet_integrate`
!! compiles three Fortran files and never crosses the C++ boundary.
!! `check_parquet_integrate_stays_arrow_free` (tools/check_source_conventions.py) keeps that true.
!!
!! **Nothing in this module prints.** It reads no setting, emits no INFO, NOTE or WARNING, and
!! re-exports no knob; a caller learns what happened from `converged=`, from `info=` and from the
!! `PF_INT_*` status codes. The only output path is `error stop`, for a caller contract that was
!! broken (`pf_integrate: ...` messages, one per row of the guide page's table) and for an
!! integrand that returned a non-finite value.
!!
!! **Thread safety is by construction.** The module has no variable that is not a `parameter`;
!! every work array is a local of the call, and the only state that outlives a call is the
!! caller's own integrand object. Give each thread its own `class(pf_integrand)` object and
!! concurrent integrations are independent. The one `error stop` reachable from inside a parallel
!! region -- the non-finite integrand value -- is taken under a named `critical` so a single
!! thread aborts.
!!
!! **The integrand is an object or a plain function.** An object extending `pf_integrand` carries
!! its parameters as components and may update them from `eval` (a call counter, a cache); a
!! `procedure(pf_integrand_func)` is the parameterless form. Either way the callback must be a
!! MODULE procedure or a type-bound procedure -- never an internal procedure, which flang cannot
!! pass at all and which gfortran implements with an executable stack.
!!
!! **What the integrator cannot see**: a feature of the integrand narrower than the spacing of the
!! 21 points of the first rule application is integrated as zero and reported as converged. That
!! is a property of the input rather than of the code; `points=` shows where the integrand was
!! actually sampled, and naming the feature's neighbourhood is what makes it visible.
!!
!! The engine is QUADPACK's, vendored and reworked; `src/parquet_integrate_engine.f90`'s header
!! carries the attribution and the full list of deviations from the upstream text.
module parquet_integrate

    use iso_fortran_env, only : real64
    use ieee_arithmetic, only : ieee_value, ieee_positive_inf

    implicit none
    private

    public :: pf_integrand, pf_integrand_eval, pf_integrand_func
    public :: pf_tolerance, pf_integration_points, pf_integration_info
    public :: pf_integrate, pf_infinity
    public :: PF_INT_OK, PF_INT_LIMIT, PF_INT_ROUNDOFF, PF_INT_BAD_INTEGRAND
    public :: PF_INT_NO_CONVERGENCE, PF_INT_DIVERGENT

    ! ---- status codes, the values of pf_integration_info%status ----------------------------

    integer, parameter :: PF_INT_OK = 0             !! requested accuracy achieved
    integer, parameter :: PF_INT_LIMIT = 1          !! `max_neval` reached before the tolerance was
    integer, parameter :: PF_INT_ROUNDOFF = 2       !! round-off prevents the requested tolerance
    integer, parameter :: PF_INT_BAD_INTEGRAND = 3  !! extremely bad behaviour at some point
    integer, parameter :: PF_INT_NO_CONVERGENCE = 4 !! round-off in the extrapolation table
    integer, parameter :: PF_INT_DIVERGENT = 5      !! probably divergent, or slowly convergent

    ! ---- internal parameters, not part of the public surface -------------------------------

    !> Points in one application of the 21-point Gauss-Kronrod rule, and so the number of record
    !! slots one subinterval of the final partition occupies.
    integer, parameter :: GK_POINTS = 21
    !> Integrand evaluations one bisection costs: two children of `GK_POINTS` each.
    integer, parameter :: GK_BISECTION = 2*GK_POINTS
    !> Default budget on integrand evaluations, `max_neval`'s default. Continuity with qfeet's
    !! `integrate`, and large enough that reaching it means the integrand, not the caller.
    integer, parameter :: DEFAULT_MAX_NEVAL = 100000
    !> Largest `max_neval` accepted, the `huge(1)/42` the abort message names. Every evaluation
    !! count is `42*limit - 21` for some `limit`, so a budget above this could not be represented
    !! in a default integer. Written as an exact division -- `huge(1)` is not a multiple of 42 --
    !! so that `-Winteger-division` stays quiet about a truncation that is the whole point.
    integer, parameter :: MAX_NEVAL_CEILING = (huge(1) - mod(huge(1), GK_BISECTION))/GK_BISECTION
    !> Subintervals the work arrays are first allocated for. The median call converges in one or
    !! two rule applications, so a call at the default budget must not pay for 2381 of them; the
    !! arrays double from here (`grow_work`) up to the limit the budget implies.
    integer, parameter :: WORK_START = 64
    !> Upper bound on the epsilon table, QUADPACK's `limexp`.
    integer, parameter :: LIMEXP = 50
    !> Characters of caller-supplied `context` an abort message reproduces before eliding with
    !! `...` (`api-conventions.md`, errors and diagnostics).
    integer, parameter :: CONTEXT_CAP = 100

    ! ---- the integrand -----------------------------------------------------------------------

    !> Base type for an integrand that carries its own parameters as components.
    !!
    !! Extend it in a MODULE, add whatever the integrand needs, and implement `eval`. One object
    !! per thread: `eval` may update the object, so two threads sharing one object race.
    type, abstract :: pf_integrand
    contains
        procedure(pf_integrand_eval), deferred :: eval !! Evaluates the integrand at one point.
    end type pf_integrand

    abstract interface

        !> Evaluates a parameterised integrand at one point.
        !!
        !! The passed-object dummy is `intent(inout)` so that an extension may keep a call counter
        !! or a cache; an extension must declare the same intent.
        function pf_integrand_eval(this, x) result(f)
            import :: pf_integrand, real64
            implicit none
            class(pf_integrand), intent(inout) :: this !! the integrand object, which may update
            real(real64), intent(in)           :: x    !! point at which to evaluate
            real(real64)                       :: f    !! integrand value at `x`
        end function pf_integrand_eval

        !> Evaluates a plain, parameterless integrand at one point.
        !!
        !! Must be a module procedure, never an internal one.
        function pf_integrand_func(x) result(f)
            import :: real64
            implicit none
            real(real64), intent(in) :: x !! point at which to evaluate
            real(real64)             :: f !! integrand value at `x`
        end function pf_integrand_func

    end interface

    !> Wraps a plain `pf_integrand_func` as a `pf_integrand`, so the engine has one integrand
    !! shape. Private: the plain-function specifics build one and delegate; callers never see it.
    type, extends(pf_integrand) :: func_integrand
        procedure(pf_integrand_func), nopass, pointer :: fp => null() !! the wrapped function
    contains
        procedure :: eval => func_integrand_eval !! Calls the wrapped function.
    end type func_integrand

    ! ---- tolerances, the record and the outcome ----------------------------------------------

    !> Relative and absolute tolerance together, for the specifics that take one object rather
    !! than a bare `rtol`. At least one of the two must be positive.
    !!
    !! `atol` is what makes an integral whose true value is near zero converge: a relative
    !! tolerance on a result of `1e-30` asks for an accuracy no arithmetic can deliver.
    type :: pf_tolerance
        real(real64) :: rtol = 0.0_real64 !! relative tolerance on the result
        real(real64) :: atol = 0.0_real64 !! absolute tolerance, for an integral near zero
    end type pf_tolerance

    !> Every point of the FINAL PARTITION at which the integrand was evaluated, with the weight
    !! that point carries and the value the integrand returned.
    !!
    !! Filled only when `pf_integrate` is called with `points=`. `sum(w(1:n)*f(1:n))` reproduces
    !! `info%partition_integral` to rounding, so the record is a quadrature rule for the caller's
    !! integrand in the caller's own variable: the weights carry the Kronrod weight, the
    !! half-length of the subinterval and any change-of-variable factor. The record is the final
    !! partition, not a log of every evaluation -- a bisected subinterval's own 21 points are
    !! superseded by its two children's 42, and summing both would count the region twice.
    type :: pf_integration_points
        integer                   :: n = 0 !! points in use; `x`, `w` and `f` are valid in `1:n`
        real(real64), allocatable :: x(:)  !! abscissae, in the caller's `x`
        real(real64), allocatable :: w(:)  !! weights, including every change-of-variable factor
        real(real64), allocatable :: f(:)  !! integrand values at `x`
    contains
        procedure :: append => points_append !! Appends another record to this one.
    end type pf_integration_points

    !> What happened, for a caller who wants more than `converged`.
    type :: pf_integration_info
        integer      :: status = PF_INT_OK              !! one of the `PF_INT_*` codes
        logical      :: converged = .true.              !! `status == PF_INT_OK`
        logical      :: extrapolated = .false.          !! the result came from the epsilon table
        real(real64) :: abserr = 0.0_real64             !! the engine's estimate of `|error|`
        real(real64) :: partition_integral = 0.0_real64 !! plain sum over the final partition
        integer      :: neval = 0                       !! integrand evaluations, counted
        integer      :: nsub = 0                        !! subintervals in the final partition
        integer      :: npanels = 0                     !! walk panels used; 1 for a finite range
    end type pf_integration_info

    ! ---- the public generic --------------------------------------------------------------------

    !> Integrates `f` over `[a, b]`, adaptively, to the tolerance asked for.
    !!
    !! ```
    !! res = pf_integrate(f, a, b, rtol, [max_neval], [converged], [info], [points], [context], &
    !!                    [log_base], [extrapolate])
    !! res = pf_integrate(f, a, b, tol,  [max_neval], [converged], [info], [points], [context], &
    !!                    [log_base], [extrapolate])
    !! ```
    !!
    !! Optional arguments are shown in square brackets, with the comma outside the bracket.
    !! FORD renders no per-argument documentation for a generic with several specifics, so every
    !! argument is named here instead:
    !!
    !! * `f` -- the integrand: a `class(pf_integrand)` object carrying its own parameters, or a
    !!   plain `procedure(pf_integrand_func)` function. Must be a module procedure or a
    !!   type-bound procedure of an object, never an internal procedure.
    !! * `a`, `b` -- the bounds, `real64`. `a == b` returns zero without evaluating the integrand;
    !!   `a > b` aborts. An infinite bound arrives in a later phase and is refused today.
    !! * `rtol` -- relative tolerance, `real64`. Convergence is `abserr <= max(atol, rtol*|res|)`.
    !! * `tol` -- a `pf_tolerance` supplying `rtol` and `atol` together, in place of `rtol`.
    !! * `max_neval` -- optional budget on integrand evaluations, default 100000. Reaching it is
    !!   `PF_INT_LIMIT` and `converged = .false.`, not an error. The count never exceeds it on a
    !!   finite range.
    !! * `converged` -- optional `logical`, `intent(out)`: `info%status == PF_INT_OK`.
    !! * `info` -- optional `type(pf_integration_info)`, `intent(out)`: status, error estimate,
    !!   evaluation count, subinterval count and the plain partition sum.
    !! * `points` -- optional `type(pf_integration_points)`, `intent(out)`: every abscissa, weight
    !!   and value of the final partition. Recorded only when this argument is present.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    !! * `log_base` -- optional `logical`, default false: integrate in `log x`, for a finite range
    !!   spanning many decades. Requires `a > 0` and two finite bounds.
    !! * `extrapolate` -- optional `logical`, default false: use the Wynn-epsilon extrapolation,
    !!   which pays for itself on an endpoint singularity and nowhere else. With it false the
    !!   result is always the partition sum, which is what `points` reproduces exactly.
    !!
    !! There is no optional `real64` argument in the list and there must never be one: an optional
    !! dummy counts against the margin that distinguishes the `rtol` specifics from the `tol`
    !! specifics (`fortran-gotchas.md`), so `atol` travels inside `pf_tolerance` and `abserr`
    !! inside `pf_integration_info`.
    interface pf_integrate

        !> Integrand as an object, tolerance as a bare `rtol`.
        module function integrate_obj_rtol(f, a, b, rtol, max_neval, converged, info, points, &
                                           context, log_base, extrapolate) result(res)
            implicit none
            class(pf_integrand), intent(inout)                  :: f          !! the integrand
            real(real64), intent(in)                            :: a          !! lower bound
            real(real64), intent(in)                            :: b          !! upper bound
            real(real64), intent(in)                            :: rtol       !! relative tolerance
            integer, intent(in), optional                       :: max_neval  !! evaluation budget
            logical, intent(out), optional                      :: converged  !! status is OK
            type(pf_integration_info), intent(out), optional    :: info       !! what happened
            type(pf_integration_points), intent(out), optional  :: points     !! the record
            character(len=*), intent(in), optional              :: context    !! call-site text
            logical, intent(in), optional                       :: log_base   !! integrate in log x
            logical, intent(in), optional                       :: extrapolate !! epsilon table
            real(real64)                                        :: res        !! the integral
        end function integrate_obj_rtol

        !> Integrand as an object, tolerance as a `pf_tolerance`.
        module function integrate_obj_tol(f, a, b, tol, max_neval, converged, info, points, &
                                          context, log_base, extrapolate) result(res)
            implicit none
            class(pf_integrand), intent(inout)                  :: f          !! the integrand
            real(real64), intent(in)                            :: a          !! lower bound
            real(real64), intent(in)                            :: b          !! upper bound
            type(pf_tolerance), intent(in)                      :: tol        !! both tolerances
            integer, intent(in), optional                       :: max_neval  !! evaluation budget
            logical, intent(out), optional                      :: converged  !! status is OK
            type(pf_integration_info), intent(out), optional    :: info       !! what happened
            type(pf_integration_points), intent(out), optional  :: points     !! the record
            character(len=*), intent(in), optional              :: context    !! call-site text
            logical, intent(in), optional                       :: log_base   !! integrate in log x
            logical, intent(in), optional                       :: extrapolate !! epsilon table
            real(real64)                                        :: res        !! the integral
        end function integrate_obj_tol

        !> Integrand as a plain function, tolerance as a bare `rtol`.
        module function integrate_func_rtol(f, a, b, rtol, max_neval, converged, info, points, &
                                            context, log_base, extrapolate) result(res)
            implicit none
            procedure(pf_integrand_func)                        :: f          !! the integrand
            real(real64), intent(in)                            :: a          !! lower bound
            real(real64), intent(in)                            :: b          !! upper bound
            real(real64), intent(in)                            :: rtol       !! relative tolerance
            integer, intent(in), optional                       :: max_neval  !! evaluation budget
            logical, intent(out), optional                      :: converged  !! status is OK
            type(pf_integration_info), intent(out), optional    :: info       !! what happened
            type(pf_integration_points), intent(out), optional  :: points     !! the record
            character(len=*), intent(in), optional              :: context    !! call-site text
            logical, intent(in), optional                       :: log_base   !! integrate in log x
            logical, intent(in), optional                       :: extrapolate !! epsilon table
            real(real64)                                        :: res        !! the integral
        end function integrate_func_rtol

        !> Integrand as a plain function, tolerance as a `pf_tolerance`.
        module function integrate_func_tol(f, a, b, tol, max_neval, converged, info, points, &
                                           context, log_base, extrapolate) result(res)
            implicit none
            procedure(pf_integrand_func)                        :: f          !! the integrand
            real(real64), intent(in)                            :: a          !! lower bound
            real(real64), intent(in)                            :: b          !! upper bound
            type(pf_tolerance), intent(in)                      :: tol        !! both tolerances
            integer, intent(in), optional                       :: max_neval  !! evaluation budget
            logical, intent(out), optional                      :: converged  !! status is OK
            type(pf_integration_info), intent(out), optional    :: info       !! what happened
            type(pf_integration_points), intent(out), optional  :: points     !! the record
            character(len=*), intent(in), optional              :: context    !! call-site text
            logical, intent(in), optional                       :: log_base   !! integrate in log x
            logical, intent(in), optional                       :: extrapolate !! epsilon table
            real(real64)                                        :: res        !! the integral
        end function integrate_func_tol

    end interface pf_integrate

    ! ---- the engine's work arrays, private ---------------------------------------------------

    !> The subinterval lists one `qagse` application keeps, plus the optional record buffer.
    !!
    !! Owned by the driver, passed to the engine, and grown geometrically by `grow_work` rather
    !! than allocated at the size the budget implies: the median call converges in one or two rule
    !! applications, and a fixed allocation at the default budget would first-touch five arrays of
    !! about 2400 elements on every call. `rx`, `rw` and `rf` are `(GK_POINTS, capacity)` and are
    !! allocated only when the caller asked for `points`.
    type :: engine_work
        integer                   :: capacity = 0       !! subintervals the lists can hold
        logical                   :: record = .false.   !! `rx`/`rw`/`rf` are allocated and filled
        real(real64), allocatable :: alist(:)           !! left end points of the subintervals
        real(real64), allocatable :: blist(:)           !! right end points of the subintervals
        real(real64), allocatable :: rlist(:)           !! integral approximations per subinterval
        real(real64), allocatable :: elist(:)           !! error estimates per subinterval
        integer, allocatable      :: iord(:)            !! subintervals ordered by error estimate
        real(real64), allocatable :: rx(:,:)            !! recorded abscissae, per subinterval slot
        real(real64), allocatable :: rw(:,:)            !! recorded weights, per subinterval slot
        real(real64), allocatable :: rf(:,:)            !! recorded values, per subinterval slot
    end type engine_work

    ! ---- the driver's workers, implemented in parquet_integrate_driver.f90 --------------------

    interface

        !> Deep-copies another record onto the end of this one, growing this one to fit.
        module subroutine points_append(this, other)
            implicit none
            class(pf_integration_points), intent(inout) :: this  !! record appended to
            type(pf_integration_points), intent(in)     :: other !! record appended
        end subroutine points_append

        !> Evaluates the wrapped plain function.
        module function func_integrand_eval(this, x) result(f)
            implicit none
            class(func_integrand), intent(inout) :: this !! the wrapper, never updated
            real(real64), intent(in)             :: x    !! point at which to evaluate
            real(real64)                         :: f    !! integrand value at `x`
        end function func_integrand_eval

        !> Aborts with `pf_integrate: <text>`, plus the caller's context when one was given.
        !!
        !! The single `error stop` of this module. Taken under a named `critical` so that one
        !! thread aborts when the call is inside a parallel region: two threads reaching
        !! `ERROR STOP` at once leave the exit status nondeterministic under ifx.
        !! Impure deliberately -- a `pure` guard-only procedure's call is deleted by ifx at `-O0`.
        module subroutine integrate_abort(text, context)
            implicit none
            character(len=*), intent(in)           :: text    !! what went wrong
            character(len=*), intent(in), optional :: context !! caller's call-site text
        end subroutine integrate_abort

        !> Grows a work array set to at least `want` subintervals, geometrically and never past
        !! `limit`, preserving the `1:last` entries already in it.
        module subroutine grow_work(work, want, limit, last)
            implicit none
            type(engine_work), intent(inout) :: work  !! the arrays to grow
            integer, intent(in)              :: want  !! subintervals needed
            integer, intent(in)              :: limit !! cap the budget implies
            integer, intent(in)              :: last  !! entries currently in use
        end subroutine grow_work

    end interface

    ! ---- the vendored engine, implemented in parquet_integrate_engine.f90 ---------------------

    interface

        !> One application of the 21-point Gauss-Kronrod rule over `[a, b]`.
        module subroutine qk21(f, a, b, log_base, res, abserr, resabs, resasc, neval, &
                               rx, rw, rf, context)
            implicit none
            class(pf_integrand), intent(inout)     :: f        !! the integrand
            real(real64), intent(in)               :: a        !! lower limit, in the engine's
                                                               !! variable
            real(real64), intent(in)               :: b        !! upper limit, same variable
            logical, intent(in)                    :: log_base !! evaluate `f(exp(u))*exp(u)`
            real(real64), intent(out)              :: res      !! the Kronrod approximation
            real(real64), intent(out)              :: abserr   !! estimate of `|error|`
            real(real64), intent(out)              :: resabs   !! approximation to the integral of
                                                               !! `|f|`
            real(real64), intent(out)              :: resasc   !! approximation to the integral of
                                                               !! `|f - i/(b-a)|`
            integer, intent(inout)                 :: neval    !! evaluation counter, incremented
            real(real64), intent(out)              :: rx(:)    !! the 21 abscissae, caller's `x`
            real(real64), intent(out)              :: rw(:)    !! the 21 weights, with Jacobian
            real(real64), intent(out)              :: rf(:)    !! the 21 integrand values
            character(len=*), intent(in), optional :: context  !! caller's call-site text
        end subroutine qk21

        !> Adaptive bisection over the finite range `[a, b]`, optionally with the Wynn-epsilon
        !! extrapolation.
        module subroutine qagse(f, a, b, epsabs, epsrel, limit, log_base, extrapolate, work, &
                                res, abserr, neval, ier, last, extrapolated, context)
            implicit none
            class(pf_integrand), intent(inout)     :: f            !! the integrand
            real(real64), intent(in)               :: a            !! lower limit, engine variable
            real(real64), intent(in)               :: b            !! upper limit, engine variable
            real(real64), intent(in)               :: epsabs       !! absolute accuracy requested
            real(real64), intent(in)               :: epsrel       !! relative accuracy requested
            integer, intent(in)                    :: limit        !! cap on subintervals
            logical, intent(in)                    :: log_base     !! evaluate `f(exp(u))*exp(u)`
            logical, intent(in)                    :: extrapolate  !! run the epsilon table
            type(engine_work), intent(inout)       :: work         !! the subinterval lists
            real(real64), intent(out)              :: res          !! the integral
            real(real64), intent(out)              :: abserr       !! estimate of `|error|`
            integer, intent(inout)                 :: neval        !! evaluation counter
            integer, intent(out)                   :: ier          !! QUADPACK's raw status
            integer, intent(out)                   :: last         !! subintervals produced
            logical, intent(out)                   :: extrapolated !! result came from the table
            character(len=*), intent(in), optional :: context      !! caller's call-site text
        end subroutine qagse

        !> Keeps the subinterval list ordered by error estimate, largest first.
        module subroutine qpsrt(limit, last, maxerr, ermax, elist, iord, nrmax)
            implicit none
            integer, intent(in)       :: limit     !! cap on subintervals
            integer, intent(in)       :: last      !! error estimates currently in the list
            integer, intent(inout)    :: maxerr    !! points to the `nrmax`-th largest estimate
            real(real64), intent(out) :: ermax     !! the `nrmax`-th largest estimate
            real(real64), intent(in)  :: elist(:)  !! the error estimates
            integer, intent(inout)    :: iord(:)   !! pointers into `elist`, descending
            integer, intent(inout)    :: nrmax     !! `maxerr = iord(nrmax)`
        end subroutine qpsrt

        !> One step of the Wynn-epsilon extrapolation table.
        module subroutine qelg(n, epstab, res, abserr, res3la, nres)
            implicit none
            integer, intent(inout)      :: n          !! `epstab(n)` holds the new element
            real(real64), intent(inout) :: epstab(:)  !! the epsilon table, `LIMEXP + 2` long
            real(real64), intent(out)   :: res        !! the extrapolated approximation
            real(real64), intent(out)   :: abserr     !! estimate of `|error|`
            real(real64), intent(inout) :: res3la(:)  !! the last three extrapolated results
            integer, intent(inout)      :: nres       !! calls to this routine so far
        end subroutine qelg

    end interface

contains

    !> Positive infinity, for use as a bound of `pf_integrate`.
    !!
    !! A named constant cannot hold an infinity in a constant expression, and building one by
    !! overflowing an expression raises `IEEE_OVERFLOW`, which is fatal under nagfor's default
    !! `-ieee=stop`. `ieee_value` is the only portable route.
    pure function pf_infinity() result(inf)
        real(real64) :: inf !! `+Infinity` as a `real64`

        inf = ieee_value(1.0_real64, ieee_positive_inf)

    end function pf_infinity

end module parquet_integrate
