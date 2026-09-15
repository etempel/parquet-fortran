!> Minimisation of a function of one or many variables: Brent's method on a bracket and the
!> Nelder-Mead simplex from a start point and a step, over one objective type.
!!
!! `parquet_optimize` is an **Arrow-free entry module**: no module in its Fortran graph imports
!! `parquet_bindings`, so `use parquet_optimize` never crosses the C++ boundary.
!! `check_parquet_optimize_stays_arrow_free` (`tools/check_source_conventions.py`) is what keeps
!! that true. It is not a leaf -- the population tier imports `parquet_sampling` for the
!! Latin-hypercube designs of `pf_minimize_de` and `pf_minimize_multistart` -- but this stage
!! imports only the two INTRINSIC modules `iso_fortran_env` and `ieee_arithmetic`, which are not
!! compiled files, not tier edges and not footprint entries.
!!
!! **The objective carries its own parameters.** It is either an object extending `pf_objective`,
!! whose `eval` may update the object (a counter, a cache), or a plain
!! `procedure(pf_objective_func)`. Both forms reach every entry point through one generic, and the
!! object form is the one the library's other callbacks use (`pf_integrand`,
!! `parquet_group_reducer`). **Every callback must be a module procedure or a type-bound procedure
!! of a `class(...)` dummy**: flang SIGSEGVs before the callee runs when an internal procedure is
!! passed as an actual argument, and gfortran gives such a procedure a trampoline that makes the
!! binary demand an executable stack.
!!
!! **This module is silent.** Nothing here prints on any path, at any verbosity: an engine that
!! stops early reports `PF_OPT_LIMIT` through `info` and returns the best point it found, and a
!! caller mistake is an `error stop` with the entry point's name, not a warning. The one emitter
!! anything in this module can reach is `parquet_clamp_to_affinity`'s once-per-process notice,
!! which belongs to the population tier's `threads=`; that is why the module re-exports the
!! verbosity and message-stream pair when that tier lands.
!!
!! **`info%converged` means the engine's own stopping rule fired, not that a minimum was found.**
!! A local solver stops where its test is met, which on a function with several basins is the
!! nearest one; the guide page states this in full and the two-tier story is what
!! `pf_minimize_de` and `pf_minimize_multistart` exist for.
!!
!! **A non-finite objective value aborts a local engine.** `pf_minimize_scalar` and
!! `pf_minimize_simplex` refuse a NaN or an infinity from `eval` rather than carrying it into a
!! comparison, because every simplex decision is a `min`/`max`/`minloc` over the values and nagfor
!! traps on those instructions with a NaN operand. The population engines treat a non-finite value
!! as "outside my domain" instead, never select it, and count it in `info%nonfinite`.
!!
!! **Nothing here is process-global.** No entry point reads a setting; there is no `optimize_*`
!! knob, and there never will be one for a quantity that changes the ANSWER. Every entry point
!! holds no state between calls, so separate minimisations may run concurrently from inside a
!! caller's own parallel region.
module parquet_optimize

    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan

    implicit none
    private

    public :: pf_objective, pf_objective_eval, pf_objective_func
    public :: pf_constrained_objective, pf_constraint_count, pf_constraint_eval
    public :: pf_optimize_info, pf_optimize_history
    public :: pf_local_solver, pf_local_run, pf_simplex_solver
    public :: pf_minimize_scalar, pf_minimize_simplex
    public :: PF_OPT_OK, PF_OPT_LIMIT, PF_OPT_TARGET
    public :: PF_OPT_NONFINITE, PF_OPT_ROUNDING, PF_OPT_INFEASIBLE

    ! ---- status codes, the values of pf_optimize_info%status ---------------------------------
    !
    ! Declared ABOVE pf_optimize_info because that type's `status` component initialises from
    ! PF_OPT_OK: a named constant in an initialisation expression must already be declared, and
    ! the reverse order is a compile error rather than a forward reference. The same order is why
    ! src/parquet_integrate.f90 carries its PF_INT_* block above pf_integration_info.

    integer, parameter :: PF_OPT_OK = 0          !! the engine's own convergence test was met
    integer, parameter :: PF_OPT_LIMIT = 1       !! `max_neval`, `max_gen` or `max_iter` came first
    integer, parameter :: PF_OPT_TARGET = 2      !! `ftarget` reached (the population tier)
    integer, parameter :: PF_OPT_NONFINITE = 3   !! no finite value anywhere (the population tier)
    integer, parameter :: PF_OPT_ROUNDING = 4    !! rounding blocks any further model improvement
    integer, parameter :: PF_OPT_INFEASIBLE = 5  !! `cstrv > ctol` at the returned point

    !> Longest caller `context=` text carried into an abort message before it is cut.
    integer, parameter :: CONTEXT_CAP = 100

    !> Default evaluation budget for `pf_minimize_scalar`.
    integer, parameter :: SCALAR_MAX_NEVAL = 500

    !> Default evaluation budget for `pf_minimize_simplex`; qfeet's `minimize` default.
    integer, parameter :: SIMPLEX_MAX_NEVAL = 5000

    ! ---- the objective ------------------------------------------------------------------------

    !> Base type for an objective carrying its own parameters as components.
    !!
    !! Extend it, add the parameters as components, and implement `eval`. The passed-object dummy
    !! is `intent(inout)`, so `eval` may keep a call counter or a cache; an extension must declare
    !! the same intent, since an extension repeats whatever the abstract interface declares.
    type, abstract :: pf_objective
    contains
        procedure(pf_objective_eval), deferred :: eval !! Value at one point; may update the object.
    end type pf_objective

    abstract interface

        !> The objective of an object: one value at one point.
        !!
        !! The object is `intent(inout)` so an extension may keep a counter or a cache. A local
        !! engine aborts on a NaN or an infinity returned here rather than comparing it.
        function pf_objective_eval(this, x) result(f)
            import :: pf_objective, real64
            implicit none
            class(pf_objective), intent(inout) :: this !! the objective object
            real(real64), intent(in)           :: x(:) !! the point
            real(real64)                       :: f    !! objective value at `x`
        end function pf_objective_eval

        !> A plain, parameterless objective.
        !!
        !! Must be a MODULE procedure: an internal procedure of the main program cannot be passed
        !! as an actual argument under flang, and gfortran's trampoline for one makes the binary
        !! demand an executable stack.
        function pf_objective_func(x) result(f)
            import :: real64
            implicit none
            real(real64), intent(in) :: x(:) !! the point
            real(real64)             :: f    !! objective value at `x`
        end function pf_objective_func

    end interface

    !> An objective with nonlinear constraints `c(x) <= 0`, for `pf_minimize_cobyla`.
    !!
    !! Declared here rather than in `parquet_prima` so that EVERY entry point of both modules can
    !! `select type` on its objective and refuse one carrying constraints it would not honour. It
    !! extends `pf_objective`, so the language accepts one everywhere; without the refusal an
    !! unconstrained engine would minimise it without its constraints and the answer would look
    !! fine. `pf_minimize_cobyla` is the only entry point that accepts one.
    type, abstract, extends(pf_objective) :: pf_constrained_objective
    contains
        procedure(pf_constraint_count), deferred :: n_constraints !! How many values `constraints` fills.
        procedure(pf_constraint_eval), deferred  :: constraints   !! `c(x)`; feasible where every `c <= 0`.
    end type pf_constrained_objective

    abstract interface

        !> How many constraint values the object fills; read once per call.
        function pf_constraint_count(this) result(m)
            import :: pf_constrained_objective
            implicit none
            class(pf_constrained_objective), intent(in) :: this !! the objective
            integer                                     :: m    !! the number of constraints
        end function pf_constraint_count

        !> The constraint values at one point; `c(i) <= 0` is feasible.
        !!
        !! The binding must fill exactly `n_constraints()` values. Filling fewer leaves the tail of
        !! `c` undefined and the engine reads it as a violation or a satisfaction at random, which
        !! the language cannot detect.
        subroutine pf_constraint_eval(this, x, c)
            import :: pf_constrained_objective, real64
            implicit none
            class(pf_constrained_objective), intent(inout) :: this !! the objective
            real(real64), intent(in)                       :: x(:) !! the point
            real(real64), intent(out)                      :: c(:) !! exactly `n_constraints()` values
        end subroutine pf_constraint_eval

    end interface

    ! ---- what happened, and the optional record ----------------------------------------------

    !> What happened, for a caller who wants more than `converged`.
    !!
    !! Every component carries a default initialiser, so a fresh one reads as a converged run of
    !! no cost and a component an engine does not fill is zero rather than undefined.
    type :: pf_optimize_info
        integer      :: status = PF_OPT_OK  !! one of the `PF_OPT_*` codes
        logical      :: converged = .true.  !! `status` is `PF_OPT_OK` or `PF_OPT_TARGET`
        integer      :: neval = 0           !! objective evaluations, every thread counted
        integer      :: niter = 0           !! simplex iterations, DE generations, starts
        integer      :: nonfinite = 0       !! non-finite values seen (the population tier)
        real(real64) :: spread = 0.0_real64 !! final value spread: simplex vertices or population
        real(real64) :: rho = 0.0_real64    !! final trust-region radius (`parquet_prima`)
        real(real64) :: cstrv = 0.0_real64  !! constraint violation at `x` (`parquet_prima`)
        integer      :: nminima = 0         !! distinct local minima found (multistart)
        integer      :: nlimit = 0          !! local runs that ended on `PF_OPT_LIMIT` (multistart)
    end type pf_optimize_info

    !> A record of evaluations: every one for a local engine, the best of each generation for DE,
    !! every local minimum before deduplication for the multistart driver.
    !!
    !! Grown geometrically and **trimmed to `n` on output**, as `pf_integration_points` is, so a
    !! caller reads `x(:, 1:n)` and `f(1:n)` without knowing how it was filled. Costs memory only
    !! when the caller asks for it.
    type :: pf_optimize_history
        integer                   :: n = 0 !! records in use
        real(real64), allocatable :: x(:,:) !! points, one per column
        real(real64), allocatable :: f(:)   !! objective values
    contains
        procedure :: append => history_append !! Appends one record, growing geometrically.
    end type pf_optimize_history

    ! ---- the local solver the multistart driver runs ------------------------------------------

    !> A local solver the multistart driver runs from each start.
    !!
    !! Options are components and there is one deferred `run`, so a user configures the engine by
    !! setting a component and the driver needs to know nothing about it. `parquet_optimize`
    !! provides `pf_simplex_solver`; `parquet_prima` provides `pf_bobyqa_solver`. The object is
    !! `intent(in)` inside `run`, so one instance serves every thread of a threaded driver.
    type, abstract :: pf_local_solver
    contains
        procedure(pf_local_run), deferred :: run !! Minimises from `x`, within the driver's box.
    end type pf_local_solver

    abstract interface

        !> One local minimisation from `x`.
        !!
        !! `lower`/`upper` are the driver's box, which a bounded engine honours and an unbounded
        !! one treats as the start region only.
        subroutine pf_local_run(this, obj, x, fmin, lower, upper, info)
            import :: pf_local_solver, pf_objective, pf_optimize_info, real64
            implicit none
            class(pf_local_solver), intent(in)  :: this     !! the solver and its options
            class(pf_objective), intent(inout)  :: obj      !! the objective (this thread's clone)
            real(real64), intent(inout)         :: x(:)     !! start in, minimum out
            real(real64), intent(out)           :: fmin     !! value at `x`
            real(real64), intent(in)            :: lower(:) !! the driver's box
            real(real64), intent(in)            :: upper(:) !! the driver's box
            type(pf_optimize_info), intent(out) :: info     !! this run's outcome
        end subroutine pf_local_run

    end interface

    !> Nelder-Mead as a local solver for the multistart driver.
    !!
    !! The defaults are a valid pair as they stand (`atol` positive), which the entry point's own
    !! bare form is not: `pf_minimize_simplex` requires `ftol` positionally, exactly as qfeet's
    !! `minimize` does, so that "at least one of ftol and atol must be positive" cannot be reached
    !! by omitting an argument.
    type, extends(pf_local_solver) :: pf_simplex_solver
        real(real64) :: step_fraction = 0.1_real64 !! `step = step_fraction*(upper - lower)`
        real(real64) :: ftol = 0.0_real64          !! as `pf_minimize_simplex`, which requires it
        real(real64) :: atol = 1.0e-10_real64      !! as `pf_minimize_simplex`
        integer      :: max_neval = 5000           !! as `pf_minimize_simplex`
    contains
        procedure :: run => simplex_solver_run !! Calls `pf_minimize_simplex` with these options.
    end type pf_simplex_solver

    ! ---- private: the plain-function wrapper --------------------------------------------------

    !> Wraps a plain `procedure(pf_objective_func)` as an objective object.
    !!
    !! The four specifics of the two generics differ only in how the objective arrives, so each
    !! plain-function form wraps its argument here and delegates to the object form. That is
    !! qfeet's own shape (`func_objective` in `../qfeet/src/minimization.f90`) and it keeps one
    !! engine per algorithm rather than two.
    type, extends(pf_objective) :: func_objective
        procedure(pf_objective_func), nopass, pointer :: fun => null() !! the wrapped function
    contains
        procedure :: eval => func_objective_eval !! Calls the wrapped function.
    end type func_objective

    ! ---- the entry points ---------------------------------------------------------------------
    !
    ! One generic per engine with two specifics, told apart by the first dummy: a data object
    ! against a procedure. Each specific is written in the fully restated `module subroutine`
    ! form, with its own `implicit none` -- gfortran gives a `procedure(...)` dummy an implicit
    ! interface in the abbreviated submodule form.

    !> Minimises a function of ONE variable on a bracket, by Brent's method.
    !!
    !! Golden-section steps with successive parabolic interpolation, terminating when the bracket
    !! is within twice the effective tolerance of `x`. `f` is an object extending `pf_objective`
    !! or a plain `procedure(pf_objective_func)`; either is evaluated at a one-element array.
    !!
    !! Arguments: `f` the objective; `a` and `b` the bracket ends, finite with `a < b`; `x` the
    !! minimiser and `fmin` its value, both `intent(out)` scalars; `tol` the absolute tolerance on
    !! `x`, default `0` meaning "as accurately as the arithmetic allows"; `max_neval` the
    !! evaluation budget, default 500; `info` the outcome; `history` every `(x, f)` in order; and
    !! `context` text appended to any abort message.
    interface pf_minimize_scalar

        !> Brent's method with the objective as an object.
        module subroutine minimize_scalar_obj(f, a, b, x, fmin, tol, max_neval, info, history, context)
            implicit none
            class(pf_objective), intent(inout)                 :: f         !! the objective
            real(real64), intent(in)                           :: a         !! bracket, lower end
            real(real64), intent(in)                           :: b         !! bracket, upper end
            real(real64), intent(out)                          :: x         !! the minimiser found
            real(real64), intent(out)                          :: fmin      !! value at `x`
            real(real64), intent(in), optional                 :: tol       !! tolerance on `x`
            integer, intent(in), optional                      :: max_neval !! evaluation budget
            type(pf_optimize_info), intent(out), optional      :: info      !! what happened
            type(pf_optimize_history), intent(out), optional   :: history   !! every evaluation
            character(len=*), intent(in), optional             :: context   !! call-site text
        end subroutine minimize_scalar_obj

        !> Brent's method with the objective as a plain module procedure.
        module subroutine minimize_scalar_func(f, a, b, x, fmin, tol, max_neval, info, history, context)
            implicit none
            procedure(pf_objective_func)                       :: f         !! the objective
            real(real64), intent(in)                           :: a         !! bracket, lower end
            real(real64), intent(in)                           :: b         !! bracket, upper end
            real(real64), intent(out)                          :: x         !! the minimiser found
            real(real64), intent(out)                          :: fmin      !! value at `x`
            real(real64), intent(in), optional                 :: tol       !! tolerance on `x`
            integer, intent(in), optional                      :: max_neval !! evaluation budget
            type(pf_optimize_info), intent(out), optional      :: info      !! what happened
            type(pf_optimize_history), intent(out), optional   :: history   !! every evaluation
            character(len=*), intent(in), optional             :: context   !! call-site text
        end subroutine minimize_scalar_func

    end interface pf_minimize_scalar

    !> Minimises a function of one or many variables by the Nelder-Mead simplex, from a start
    !! point and a per-coordinate step.
    !!
    !! Ported step for step from `../qfeet/src/minimization.f90`: reflection at `-1`, expansion at
    !! `2`, contraction at `0.5`, the shrink onto the best vertex, and the value-spread test in the
    !! branch form that avoids `0/0`. The starting simplex is `x` and, for each coordinate `i`, `x`
    !! with `step(i)` added to coordinate `i`.
    !!
    !! Arguments: `f` the objective; `x` the start point in and the best point out; `fmin` its
    !! value; `step` the per-coordinate offset, no element zero or NaN; `ftol` the fractional
    !! tolerance on the value spread, REQUIRED as qfeet has it; `atol` the absolute tolerance,
    !! default `0`, at least one of the two positive; `max_neval` the budget, default 5000; then
    !! `info`, `history` and `context`.
    interface pf_minimize_simplex

        !> Nelder-Mead with the objective as an object.
        module subroutine minimize_simplex_obj(f, x, fmin, step, ftol, atol, max_neval, info, &
                                               history, context)
            implicit none
            class(pf_objective), intent(inout)               :: f         !! the objective
            real(real64), intent(inout)                      :: x(:)      !! start in, minimum out
            real(real64), intent(out)                        :: fmin      !! value at `x`
            real(real64), intent(in)                         :: step(:)   !! offset per coordinate
            real(real64), intent(in)                         :: ftol      !! fractional tolerance
            real(real64), intent(in), optional               :: atol      !! absolute tolerance
            integer, intent(in), optional                    :: max_neval !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info      !! what happened
            type(pf_optimize_history), intent(out), optional :: history   !! every evaluation
            character(len=*), intent(in), optional           :: context   !! call-site text
        end subroutine minimize_simplex_obj

        !> Nelder-Mead with the objective as a plain module procedure.
        module subroutine minimize_simplex_func(f, x, fmin, step, ftol, atol, max_neval, info, &
                                                history, context)
            implicit none
            procedure(pf_objective_func)                     :: f         !! the objective
            real(real64), intent(inout)                      :: x(:)      !! start in, minimum out
            real(real64), intent(out)                        :: fmin      !! value at `x`
            real(real64), intent(in)                         :: step(:)   !! offset per coordinate
            real(real64), intent(in)                         :: ftol      !! fractional tolerance
            real(real64), intent(in), optional               :: atol      !! absolute tolerance
            integer, intent(in), optional                    :: max_neval !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info      !! what happened
            type(pf_optimize_history), intent(out), optional :: history   !! every evaluation
            character(len=*), intent(in), optional           :: context   !! call-site text
        end subroutine minimize_simplex_func

    end interface pf_minimize_simplex

    ! ---- the shared workers, implemented in parquet_optimize_support.f90 -----------------------
    !
    ! Declared here rather than contained in this module: a module-contained private procedure
    ! called from a submodule fails to link under gfortran, so every worker a submodule reaches is
    ! a separate module procedure with its interface in this specification part.

    interface

        !> Aborts with `<entry point>: <text>`, plus the caller's context when one was given.
        !!
        !! The single `error stop` of this module. Taken under a named `critical` so that ONE
        !! thread aborts when the call is inside a parallel region -- a local engine run from the
        !! multistart driver's threads is exactly that -- since two threads reaching `ERROR STOP`
        !! at once leave the exit status nondeterministic under ifx. Impure deliberately: a `pure`
        !! guard-only procedure's call is deleted by ifx at `-O0`.
        module subroutine optimize_abort(entry_point, text, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            character(len=*), intent(in)           :: text        !! what went wrong
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine optimize_abort

        !> Refuses an objective carrying nonlinear constraints this engine would not honour.
        !!
        !! Every entry point but `pf_minimize_cobyla` calls it first. Without it a
        !! `pf_constrained_objective` is accepted everywhere the language accepts a
        !! `pf_objective`, and the constraints are silently dropped.
        module subroutine refuse_constrained(entry_point, f, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            class(pf_objective), intent(in)        :: f           !! the caller's objective
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine refuse_constrained

        !> Validates the number of variables: at least one.
        module subroutine validate_size(entry_point, n, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            integer, intent(in)                    :: n           !! variables the caller asked for
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine validate_size

        !> Validates one tolerance: finite and not negative.
        module subroutine validate_tolerance(entry_point, name, value, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            character(len=*), intent(in)           :: name        !! the argument's own name
            real(real64), intent(in)               :: value       !! what the caller passed
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine validate_tolerance

        !> Validates an evaluation budget: positive, and not above `huge(1)/2`.
        !!
        !! The upper bound is what lets every count in `pf_optimize_info` be a default `integer`:
        !! each is bounded by a budget, and a budget below `huge(1)/2` cannot overflow one.
        module subroutine validate_budget(entry_point, max_neval, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            integer, intent(in)                    :: max_neval   !! what the caller passed
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine validate_budget

        !> Appends one record to a history, growing it geometrically.
        module subroutine history_append(this, x, f)
            implicit none
            class(pf_optimize_history), intent(inout) :: this !! the record to extend
            real(real64), intent(in)                  :: x(:) !! the point
            real(real64), intent(in)                  :: f    !! objective value at `x`
        end subroutine history_append

        !> Trims a history to the records actually in use, as `pf_integration_points` is trimmed.
        module subroutine history_trim(this)
            implicit none
            type(pf_optimize_history), intent(inout) :: this !! the record to trim
        end subroutine history_trim

        !> Calls the wrapped plain function.
        module function func_objective_eval(this, x) result(f)
            implicit none
            class(func_objective), intent(inout) :: this !! the wrapper
            real(real64), intent(in)             :: x(:) !! the point
            real(real64)                         :: f    !! objective value at `x`
        end function func_objective_eval

    end interface

    ! ---- the engines' own workers --------------------------------------------------------------

    interface

        !> `pf_simplex_solver%run`: derives `step` from the box and calls `pf_minimize_simplex`.
        module subroutine simplex_solver_run(this, obj, x, fmin, lower, upper, info)
            implicit none
            class(pf_simplex_solver), intent(in) :: this     !! the solver and its options
            class(pf_objective), intent(inout)   :: obj      !! the objective
            real(real64), intent(inout)          :: x(:)     !! start in, minimum out
            real(real64), intent(out)            :: fmin     !! value at `x`
            real(real64), intent(in)             :: lower(:) !! the driver's box
            real(real64), intent(in)             :: upper(:) !! the driver's box
            type(pf_optimize_info), intent(out)  :: info     !! this run's outcome
        end subroutine simplex_solver_run

    end interface

end module parquet_optimize
