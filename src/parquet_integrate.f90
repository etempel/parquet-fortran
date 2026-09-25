!> Adaptive numerical integration of a function of one `real64` variable: `pf_integrate`, the
!> 21-point Gauss-Kronrod rule with adaptive bisection over a finite range, an outward walk over
!> the same rule for an infinite one, and the Wynn-epsilon extrapolation over both.
!!
!! `parquet_integrate` is an **Arrow-free leaf**: it imports the two INTRINSIC modules
!! `iso_fortran_env` and `ieee_arithmetic`, and no module of this library. An intrinsic module is
!! not a compiled file, not a tier edge and not a footprint entry, so `use parquet_integrate`
!! compiles three Fortran files and never crosses the C++ boundary.
!! `check_parquet_integrate_stays_arrow_free` (tools/check_source_conventions.py) keeps that true.
!!
!! **Nothing in this module prints.** It reads no setting, emits no INFO, NOTE or WARNING, and
!! re-exports no knob; a caller learns what happened from `converged=`, from `info=` and from the
!! `PF_INT_*` status codes. The only output path is `error stop`, and every one of those is a
!! caller contract that was broken (`pf_integrate: ...` messages, one per row of the guide page's
!! table): a bound, a tolerance, a budget or a breakpoint that does not say what it meant.
!!
!! **What the INTEGRAND does is never a contract violation.** An integrand that returns a NaN or
!! an infinity is the caller's own function misbehaving on the caller's own data, so it ends the
!! integration rather than the process: `PF_INT_NONFINITE`, `converged = .false.`, and
!! `info%nonfinite_at` carrying the point it happened at. A caller sweeping a parameter grid
!! keeps the sweep and can say which parameter broke.
!!
!! **Thread safety is by construction.** The module has no variable that is not a `parameter`;
!! every work array is a local of the call, and the only state that outlives a call is the
!! caller's own integrand object. Give each thread its own `class(pf_integrand)` object and
!! concurrent integrations are independent. The `error stop` paths are all reachable from inside
!! a parallel region -- nothing stops a caller validating badly there -- so each is taken under a
!! named `critical` and a single thread aborts.
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
!! actually sampled, and `breakpoints=` is what makes such a feature visible -- a cut at each side
!! of it puts a rule application on the piece that contains it.
!!
!! **The extrapolation is on unless it is turned off.** `extrapolate=.false.` leaves the plain
!! bisection, whose result is exactly the sum `points=` reproduces. On by default it costs
!! essentially nothing on a smooth integrand -- the same answer, and the same count but for the
!! occasional extra bisection at the tightest tolerances -- and it is what turns an integrable
!! endpoint singularity from a cost that grows with the tolerance into one that does not, and a
!! DIVERGENT integral from a long drill towards the singularity into `PF_INT_DIVERGENT` in a
!! couple of hundred evaluations.
!!
!! The engine is QUADPACK's, vendored and reworked; `src/parquet_integrate_engine.f90`'s header
!! carries the attribution and the full list of deviations from the upstream text.
module parquet_integrate

    use iso_fortran_env, only : real64
    use ieee_arithmetic, only : ieee_value, ieee_positive_inf

    implicit none
    private

    public :: pf_integrand, pf_integrand_eval, pf_integrand_func
    public :: pf_integrate_points, pf_integrate_info
    public :: pf_integrate, pf_infinity
    public :: PF_INT_OK, PF_INT_LIMIT, PF_INT_ROUNDOFF, PF_INT_BAD_INTEGRAND
    public :: PF_INT_NO_CONVERGENCE, PF_INT_DIVERGENT, PF_INT_NONFINITE

    ! ---- status codes, the values of pf_integrate_info%status ----------------------------

    integer, parameter :: PF_INT_OK = 0             !! requested accuracy achieved
    integer, parameter :: PF_INT_LIMIT = 1          !! `max_neval` reached before the tolerance was
    integer, parameter :: PF_INT_ROUNDOFF = 2       !! round-off prevents the requested tolerance
    integer, parameter :: PF_INT_BAD_INTEGRAND = 3  !! extremely bad behaviour at some point
    integer, parameter :: PF_INT_NO_CONVERGENCE = 4 !! round-off in the extrapolation table
    integer, parameter :: PF_INT_DIVERGENT = 5      !! probably divergent, or slowly convergent
    integer, parameter :: PF_INT_NONFINITE = 6      !! the integrand returned a NaN or an infinity

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

    ! ---- the outward walk of an infinite range, qfeet's constants ---------------------------

    !> Default cap on the panels one walk may use, `max_panels`'s default.
    !!
    !! The panel count is what the walk's cost is made of, so this cap is what a caller meets
    !! first on a tail the walk would not otherwise finish. It is set by the dearest tail shape
    !! that DOES finish: an algebraic tail at the tightest tolerance worth asking for --
    !! `x**-1.5` over `[1, infinity)` at `rtol = 1e-12` -- converges on its 56th panel, and a cap
    !! below that returns `PF_INT_LIMIT` with a result short of the tolerance asked for, on an
    !! integral the walk can in fact deliver. Twice qfeet's `default_max_tail_steps`, which clears
    !! that with room to spare and costs nothing on a shape that finishes earlier: a panel beyond
    !! the last one walked is never integrated, so the cap is a ceiling and not a price.
    integer, parameter :: DEFAULT_MAX_PANELS = 100
    !> Width of one walk panel, in natural-log units of `x`, so each panel spans a factor of e.
    !!
    !! qfeet's `tail_step`, and one is where a sweep over 1, 2 and 3 left it. **The cost did not
    !! decide it.** Beside `dqagi`'s count on the three scored tail shapes, at
    !! `rtol = 1e-10, atol = 1e-14`:
    !!
    !! ```
    !! ratio against dqagi          width 1   width 2   width 3
    !! exp(-x)      [1, inf)           0.78      0.93      1.56
    !! x**-1.5      [1, inf)           6.11      3.18      2.16
    !! exp(-x**2)   [0, inf)           0.75      0.97      1.18
    !! geometric mean of the three     1.53      1.42      1.59
    !! ```
    !!
    !! A wider panel covers the same distance in fewer steps, which is cheaper on an algebraic
    !! tail and dearer on an exponential one, and the three means land within twelve per cent of
    !! each other -- width 2 being the cheapest of them, by seven per cent.
    !!
    !! **What decides it is the narrow feature just above the lower bound**, the blind spot the
    !! walk exists to cover. A gaussian spike of half-width `1e-3` centred at `x = 1.02`,
    !! integrated over `[1, infinity)` at `rtol = 1e-10`:
    !!
    !! ```
    !! atol       width 1              width 2              width 3
    !! 0          609, rel 1.3e-14    1113, rel 1.3e-14     147, rel 1.0
    !! 1e-14      609, rel 1.3e-14     105, rel 1.0         147, rel 1.0
    !! 1e-20      609, rel 1.3e-14     105, rel 1.0         147, rel 1.0
    !! 1e-30      609, rel 1.3e-14    1113, rel 1.3e-14     147, rel 1.0
    !! ```
    !!
    !! A relative error of one is the whole integral missed: `5.4e-23` returned where `1.77e-03`
    !! was wanted, `converged` TRUE. **Width 3 loses the spike at every tolerance.** Width 2 loses
    !! it for any `atol` at or above about `1e-22`, which is every setting a caller would
    !! plausibly pass, and finds it only when `atol` is effectively zero -- and then at 1113
    !! evaluations against width 1's 609. Width 1 finds it at every setting. A silent wrong answer
    !! on the shape this walk was built for is not worth seven per cent, so the width stays at one.
    !!
    !! **The failure is abrupt rather than gradual, because the protection is exactly one
    !! bisection deep.** At every width the first rule application over the start panel misses
    !! the spike completely -- 18 of its 21 abscissae read an exact zero and the other three read
    !! `1e-21` and smaller -- and QUADPACK does not return on that: `abserr == resabs`, the error
    !! estimate saturated at the integral of `|f|`, which says the rule learned nothing, and it
    !! blocks `qagse`'s early exit. That buys one bisection and no more, since the test guards a
    !! panel's FIRST application only. Below it the loop exits on `errsum <= errbnd`, and
    !! `errbnd` is `max(atol, rtol*|area|)`: on a panel whose area is `5e-23` the `rtol` term is
    !! nothing, so `atol` IS the bound, and any ordinary `atol` ends the bisection right there.
    !! That is the whole of the `atol` column above. At width 1 the one bisection yields `x` in
    !! `[1, 1.65]`, whose 21 abscissae are close enough together that one lands on the spike:
    !! `errsum` jumps from `1.1e-22` to `9.1e-05`, far above any bound, and the bisection
    !! converges onto it. At width 2 it yields `[1, 2.72]` -- width 1's whole panel, read with the
    !! guard already spent -- where `errsum` stays at `1.1e-22` and only an `atol` below that
    !! keeps the walk going.
    !!
    !! **The suite protects this, but not through the test one would expect.** Setting the width
    !! to 2 fails `integral_inf_oscillatory`, not `start_panel_search`, even though the latter
    !! integrates exactly this spike: it uses the bare-`rtol` spelling, so its `atol` is zero and
    !! width 2 survives it. `start_panel_search` catches width 3. Anyone adding a blind-spot
    !! fixture should pass a non-zero `atol` if the width is what it means to pin.
    !!
    !! **The width is a `parameter` and not an argument**, so re-running the sweep means
    !! recompiling: copy `src/parquet_integrate*.f90` outside the repository, edit this line and
    !! build the three files with a driver of your own -- this module imports only
    !! `iso_fortran_env` and `ieee_arithmetic`, so it needs nothing else. The header of
    !! `bench/benchmark_integrate.sh` says the same, and that benchmark's `walk` mode is what
    !! reports the rows above.
    real(real64), parameter :: TAIL_STEP = 1.0_real64
    !> Multiple of `epsilon` below which a panel's contribution is indistinguishable from
    !! accumulated rounding. qfeet's `roundoff_factor`: not a tolerance, and fixed on purpose.
    real(real64), parameter :: ROUNDOFF_FACTOR = 8.0_real64
    !> Lower bound below which the search for a first panel works in absolute rather than
    !! relative steps. qfeet's `start_min_a`.
    real(real64), parameter :: START_MIN_A = 1.0_real64
    !> Factor applied to a small lower bound when the first panel comes back negligible.
    real(real64), parameter :: START_MULT_A = 1.1_real64
    !> Offset applied, in `log x`, to a large lower bound when the first panel is negligible.
    real(real64), parameter :: START_ADD_A = 0.1_real64
    !> Factor by which the search widens a negligible panel, in `log x`.
    real(real64), parameter :: START_WIDEN = 3.0_real64
    !> Cap on attempts to find a starting panel that is not negligible. qfeet's
    !! `max_start_steps`: 30 widenings by a factor of three reach `exp(33)` from `x = 1`.
    integer, parameter :: MAX_START_STEPS = 30
    !> Largest `log x` a panel bound may take. `exp` of anything above `log(huge(1.0_real64))`,
    !! which is 709.78, is an infinity, and the integrand would then be asked for a value at a
    !! point that is not a number. Written as a literal because a constant expression may not
    !! call `log`; the margin below 709.78 is deliberate.
    real(real64), parameter :: LOG_X_CEILING = 709.0_real64

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
        function pf_integrand_eval(self, x) result(f)
            import :: pf_integrand, real64
            implicit none
            class(pf_integrand), intent(inout) :: self !! the integrand object, which may update
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

    !> The two tolerances as one object, for the driver to carry down to the engine and to divide
    !! among the pieces of a `breakpoints` call. At least one of the two must be positive.
    !!
    !! PRIVATE: a caller passes `rtol` and `atol` as separate arguments of `pf_integrate`. The type
    !! exists because the piecewise path integrates each of `n` pieces to `atol/n`, so the pair
    !! travels together through the driver, the walk and the engine.
    !!
    !! `atol` is what makes an integral whose true value is near zero converge: a relative
    !! tolerance on a result of `1e-30` asks for an accuracy no arithmetic can deliver.
    type :: tolerance_pair
        real(real64) :: rtol = 0.0_real64 !! relative tolerance on the result
        real(real64) :: atol = 0.0_real64 !! absolute tolerance, for an integral near zero
    end type tolerance_pair

    !> Every point of the FINAL PARTITION at which the integrand was evaluated, with the weight
    !! that point carries and the value the integrand returned.
    !!
    !! Filled only when `pf_integrate` is called with `points=`. `sum(w(1:n)*f(1:n))` reproduces
    !! `info%partition_integral` to rounding, so the record is a quadrature rule for the caller's
    !! integrand in the caller's own variable: the weights carry the Kronrod weight, the
    !! half-length of the subinterval and any change-of-variable factor. The record is the final
    !! partition, not a log of every evaluation -- a bisected subinterval's own 21 points are
    !! superseded by its two children's 42, and summing both would count the region twice.
    type :: pf_integrate_points
        integer                   :: n = 0 !! points in use; `x`, `w` and `f` are valid in `1:n`
        real(real64), allocatable :: x(:)  !! abscissae, in the caller's `x`
        real(real64), allocatable :: w(:)  !! weights, including every change-of-variable factor
        real(real64), allocatable :: f(:)  !! integrand values at `x`
    contains
        procedure :: append => points_append !! Appends another record to this one.
    end type pf_integrate_points

    !> What happened, for a caller who wants more than `converged`.
    !!
    !! `nonfinite_at` is the one component that means nothing on its own: it is the point the
    !! integrand returned a NaN or an infinity at, and it is defined only when `status` is
    !! `PF_INT_NONFINITE`. On every other status it stays zero, which is a point like any other
    !! and so must never be read as "no bad value" -- the status is what says that.
    type :: pf_integrate_info
        integer      :: status = PF_INT_OK              !! one of the `PF_INT_*` codes
        logical      :: converged = .true.              !! `status == PF_INT_OK`
        logical      :: extrapolated = .false.          !! the result came from the epsilon table
        real(real64) :: abserr = 0.0_real64             !! the engine's estimate of `|error|`
        real(real64) :: partition_integral = 0.0_real64 !! plain sum over the final partition
        integer      :: neval = 0                       !! integrand evaluations, counted
        integer      :: nsub = 0                        !! subintervals in the final partition
        integer      :: npanels = 0                     !! walk panels used; 1 for a finite range
        real(real64) :: nonfinite_at = 0.0_real64      !! where the integrand returned a non-finite
                                                        !! value; read only on `PF_INT_NONFINITE`
    end type pf_integrate_info

    !> The first non-finite integrand value a call met, carried out of the rule rather than
    !! aborted on.
    !!
    !! Private, and threaded through the engine as one `intent(inout)` argument in place of the
    !! `context` the screen used to abort with. `seen` false means every evaluation so far
    !! returned a finite number; once it is true the engine stops at its next check and the driver
    !! turns it into `PF_INT_NONFINITE`. The FIRST such point is kept, not the last: it is the one
    !! a caller can reason about, since everything after it may be a consequence.
    type :: bad_value
        logical      :: seen = .false.  !! an evaluation returned a NaN or an infinity
        real(real64) :: x = 0.0_real64  !! the first such point, in the caller's `x`
    end type bad_value

    ! ---- the public generic --------------------------------------------------------------------

    !> Integrates `f` over `[a, b]`, adaptively, to the tolerance asked for.
    !!
    !! ```
    !! res = pf_integrate(f, a, b, rtol, [atol], [max_neval], [log_base], [extrapolate], &
    !!                    [max_panels], [breakpoints], [converged], [info], [points], [context])
    !! ```
    !!
    !! Either bound may be an infinity, which is how the four ranges are spelled:
    !!
    !! ```fortran
    !! r = pf_integrate(f, 0.0_real64, 1.0_real64, 1.0e-8_real64)        ! [0, 1]
    !! r = pf_integrate(f, 1.0_real64, pf_infinity(), 1.0e-8_real64)     ! [1, +inf)
    !! r = pf_integrate(f, -pf_infinity(), 0.0_real64, 1.0e-8_real64)    ! (-inf, 0]
    !! r = pf_integrate(f, -pf_infinity(), pf_infinity(), 1.0e-8_real64) ! (-inf, +inf)
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
    !!   `a > b` aborts. Either may be `pf_infinity()` or its negative, and an infinite range is
    !!   answered by an outward walk rather than by a change of variable: the walk looks for a
    !!   first panel the integrand is not negligible on and then steps outward a factor of e at a
    !!   time, so a feature far along the range is found rather than fallen between two abscissae.
    !!   Both bounds the same infinity aborts.
    !! * `rtol` -- relative tolerance, `real64`, required. Convergence is
    !!   `abserr <= max(atol, rtol*|res|)`. An `rtol` below `50*epsilon` needs a positive `atol`.
    !! * `atol` -- optional absolute tolerance, `real64`, default 0. It is what makes an integral
    !!   whose true value is near zero converge, since a relative tolerance on a result of `1e-30`
    !!   asks for an accuracy no arithmetic can deliver. At least one of the two must be positive.
    !! * `max_neval` -- optional budget on integrand evaluations, default 100000. Reaching it is
    !!   `PF_INT_LIMIT` and `converged = .false.`, not an error. The count never exceeds it on a
    !!   finite range, with or without `breakpoints`, provided the budget covers one rule
    !!   application per piece (21 evaluations each).
    !! * `converged` -- optional `logical`, `intent(out)`: `info%status == PF_INT_OK`.
    !! * `info` -- optional `type(pf_integrate_info)`, `intent(out)`: status, error estimate,
    !!   evaluation count, subinterval count, the plain partition sum, and -- on
    !!   `PF_INT_NONFINITE` alone -- the point the integrand returned a non-finite value at.
    !! * `points` -- optional `type(pf_integrate_points)`, `intent(out)`: every abscissa, weight
    !!   and value of the final partition. Recorded only when this argument is present.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    !! * `log_base` -- optional `logical`, default false: integrate in `log x`, for a finite range
    !!   spanning many decades. Requires `a > 0` and two finite bounds.
    !! * `extrapolate` -- optional `logical`, DEFAULT TRUE: use the Wynn-epsilon extrapolation.
    !!   It is what makes an integrable endpoint singularity affordable -- the bisection alone
    !!   pays more and more as the tolerance tightens, while the table's cost does not grow at
    !!   all -- and on a smooth integrand, an interior peak or a polynomial it leaves the answer
    !!   alone and the count all but alone, because the table is not consulted until the
    !!   bisection has begun dividing the same end of the range over and over. It also earns the two status
    !!   codes only it can reach: a divergent integral is reported as `PF_INT_DIVERGENT` in a
    !!   couple of hundred evaluations, where the bisection alone would drill towards the
    !!   singularity until the integrand overflows. Pass `.false.` when the returned result must
    !!   BE the partition sum -- the sum `points` reproduces exactly -- rather than an
    !!   acceleration of it; `info%extrapolated` says which of the two was returned.
    !! * `max_panels` -- optional cap on the panels ONE walk may use, default 100, counting the
    !!   first panel the search accepted. Reaching it is `PF_INT_LIMIT`, not an error. Present on
    !!   a finite range aborts; on `(-inf, +inf)`, which is two walks, it caps each of them.
    !! * `breakpoints` -- optional rank-1 `real64` of interior points at which the range is cut.
    !!   Each piece is integrated on its own, by the path its own bounds select, and the results,
    !!   error estimates and records are concatenated in order. This is how a caller NAMES a
    !!   feature the first rule application would not sample -- the one thing that turns the blind
    !!   spot below into a right answer. The breakpoints are sorted here, must be finite, distinct
    !!   and strictly inside the range, and may be given on an infinite range, where the pieces
    !!   are `(-inf, p1], [p1, p2], ..., [pn, +inf)` and no split at zero is made. Each piece is
    !!   integrated to `rtol` of itself and to `atol/n_pieces`, so the sum meets both tolerances
    !!   whenever every piece does; `converged` is the conjunction.
    !!
    !! An optional scalar `real64` such as `atol` is safe here because the two specifics differ in
    !! their FIRST argument -- a data object against a procedure -- exactly as `pf_find_root`'s do,
    !! so no later dummy is asked to tell them apart.
    interface pf_integrate

        !> Integrand as an object carrying its own parameters.
        module function integrate_obj(f, a, b, rtol, atol, max_neval, log_base, extrapolate, &
                                      max_panels, breakpoints, converged, info, points, context) result(res)
            implicit none
            class(pf_integrand), intent(inout)                  :: f          !! the integrand
            real(real64), intent(in)                            :: a          !! lower bound
            real(real64), intent(in)                            :: b          !! upper bound
            real(real64), intent(in)                            :: rtol       !! relative tolerance
            real(real64), intent(in), optional                  :: atol       !! absolute tolerance
            integer, intent(in), optional                       :: max_neval  !! evaluation budget; at most huge(1)/42
            logical, intent(in), optional                       :: log_base   !! integrate in log x
            logical, intent(in), optional                       :: extrapolate !! epsilon table
            integer, intent(in), optional                       :: max_panels !! walk panel cap
            real(real64), intent(in), optional                  :: breakpoints(:) !! interior cuts
            logical, intent(out), optional                      :: converged  !! status is OK
            type(pf_integrate_info), intent(out), optional    :: info       !! what happened
            type(pf_integrate_points), intent(out), optional  :: points     !! the record
            character(len=*), intent(in), optional              :: context    !! call-site text
            real(real64)                                        :: res        !! the integral
        end function integrate_obj

        !> Integrand as a plain, parameterless function.
        module function integrate_func(f, a, b, rtol, atol, max_neval, log_base, extrapolate, &
                                       max_panels, breakpoints, converged, info, points, context) result(res)
            implicit none
            procedure(pf_integrand_func)                        :: f          !! the integrand
            real(real64), intent(in)                            :: a          !! lower bound
            real(real64), intent(in)                            :: b          !! upper bound
            real(real64), intent(in)                            :: rtol       !! relative tolerance
            real(real64), intent(in), optional                  :: atol       !! absolute tolerance
            integer, intent(in), optional                       :: max_neval  !! evaluation budget; at most huge(1)/42
            logical, intent(in), optional                       :: log_base   !! integrate in log x
            logical, intent(in), optional                       :: extrapolate !! epsilon table
            integer, intent(in), optional                       :: max_panels !! walk panel cap
            real(real64), intent(in), optional                  :: breakpoints(:) !! interior cuts
            logical, intent(out), optional                      :: converged  !! status is OK
            type(pf_integrate_info), intent(out), optional    :: info       !! what happened
            type(pf_integrate_points), intent(out), optional  :: points     !! the record
            character(len=*), intent(in), optional              :: context    !! call-site text
            real(real64)                                        :: res        !! the integral
        end function integrate_func

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
            class(pf_integrate_points), intent(inout) :: this  !! record appended to
            type(pf_integrate_points), intent(in)     :: other !! record appended
        end subroutine points_append

        !> Evaluates the wrapped plain function.
        module function func_integrand_eval(self, x) result(f)
            implicit none
            class(func_integrand), intent(inout) :: self !! the wrapper, never updated
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
        module subroutine qk21(f, a, b, log_base, negate, res, abserr, resabs, resasc, neval, &
                               rx, rw, rf, bad)
            implicit none
            class(pf_integrand), intent(inout)     :: f        !! the integrand
            real(real64), intent(in)               :: a        !! lower limit, in the engine's
                                                               !! variable
            real(real64), intent(in)               :: b        !! upper limit, same variable
            logical, intent(in)                    :: log_base !! evaluate `f(exp(u))*exp(u)`
            logical, intent(in)                    :: negate   !! evaluate at `-x`, recording `-x`
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
            type(bad_value), intent(inout)         :: bad      !! set on a non-finite value
        end subroutine qk21

        !> Adaptive bisection over the finite range `[a, b]`, optionally with the Wynn-epsilon
        !! extrapolation.
        module subroutine qagse(f, a, b, epsabs, epsrel, limit, log_base, negate, extrapolate, &
                                work, res, abserr, defabs, neval, ier, last, extrapolated, bad, &
                                context)
            implicit none
            class(pf_integrand), intent(inout)     :: f            !! the integrand
            real(real64), intent(in)               :: a            !! lower limit, engine variable
            real(real64), intent(in)               :: b            !! upper limit, engine variable
            real(real64), intent(in)               :: epsabs       !! absolute accuracy requested
            real(real64), intent(in)               :: epsrel       !! relative accuracy requested
            integer, intent(in)                    :: limit        !! cap on subintervals
            logical, intent(in)                    :: log_base     !! evaluate `f(exp(u))*exp(u)`
            logical, intent(in)                    :: negate       !! evaluate at `-x`
            logical, intent(in)                    :: extrapolate  !! run the epsilon table
            type(engine_work), intent(inout)       :: work         !! the subinterval lists
            real(real64), intent(out)              :: res          !! the integral
            real(real64), intent(out)              :: abserr       !! estimate of `|error|`
            real(real64), intent(out)              :: defabs       !! the FIRST rule application's
                                                                   !! approximation to the integral
                                                                   !! of `|f|` over `[a, b]`, which
                                                                   !! is what the walk's round-off
                                                                   !! floor is measured against
            integer, intent(inout)                 :: neval        !! evaluation counter
            integer, intent(out)                   :: ier          !! QUADPACK's raw status
            integer, intent(out)                   :: last         !! subintervals produced
            logical, intent(out)                   :: extrapolated !! result came from the table
            type(bad_value), intent(inout)         :: bad          !! set on a non-finite value
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

end module parquet_integrate ! GCOVR_EXCL_LINE
