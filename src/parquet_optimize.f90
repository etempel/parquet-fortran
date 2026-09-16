!> Minimisation of a function of one or many variables: Brent's method on a bracket, the
!> Nelder-Mead simplex from a start point and a step, differential evolution over a box, and a
!> multistart driver that runs a local solver from a Latin hypercube of starts.
!!
!! `parquet_optimize` is an **Arrow-free entry module**: no module in its Fortran graph imports
!! `parquet_bindings`, so `use parquet_optimize` never crosses the C++ boundary.
!! `check_parquet_optimize_stays_arrow_free` (`tools/check_source_conventions.py`) is what keeps
!! that true. It is not a leaf: `pf_minimize_de` and `pf_minimize_multistart` lay their starts out
!! on a Latin hypercube, so the module imports `parquet_sampling` for `pf_random_perm_at` and,
!! with it, the argsort tier and `parquet_settings_base`.
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
!! which `threads=` passes through; that is why the module re-exports the verbosity and
!! message-stream pair.
!!
!! **`info%converged` means the engine's own stopping rule fired, not that a minimum was found.**
!! A local solver stops where its test is met, which on a function with several basins is the
!! nearest one; the guide page states this in full and the two-tier story is what
!! `pf_minimize_de` and `pf_minimize_multistart` exist for.
!!
!! **A non-finite objective value aborts a local engine.** `pf_minimize_scalar` and
!! `pf_minimize_simplex` refuse a NaN or an infinity from `eval` rather than carrying it into a
!! comparison, because every simplex decision is a `min`/`max`/`minloc` over the values and nagfor
!! traps on those instructions with a NaN operand. `pf_minimize_de` treats a non-finite value as
!! "outside my domain" instead, never selects it, and counts it in `info%nonfinite`;
!! `pf_minimize_multistart` inherits whichever policy its local solver has, so with the shipped
!! `pf_simplex_solver` a NaN inside the box aborts the run.
!!
!! **The answer does not depend on `threads`.** Every draw `pf_minimize_de` and
!! `pf_minimize_multistart` make is addressed by `(seed, generation, individual)` or
!! `(seed, start)` and nothing is drawn outside that scope; every decision that combines
!! individuals or starts is element-wise or lowest-index. So for a given library version and build
!! the result is a function of `seed`, the arguments and the objective, and not of the thread
!! count, the schedule or the machine's timing. What is NOT promised is cross-compiler or
!! cross-version bit equality: `a + F*(b - c)` is an FMA-contractable shape, and one ulp in a
!! trial vector can flip one selection and thereafter the whole path.
!!
!! **Nothing here is process-global except one test-only counter.** No entry point reads a
!! setting; there is no `optimize_*` knob, and there never will be one for a quantity that changes
!! the ANSWER. Every entry point holds no state between calls, so separate minimisations may run
!! concurrently from inside a caller's own parallel region. The one exception is
!! `parquet_debug_optimize_threads_used`, a saved counter the threaded regions write and only
!! tests read (`CLAUDE.md`'s NAG `-thread_safe` note enumerates such variables).
module parquet_optimize

    use iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan
    use parquet_random, only : pf_random_key, pf_random_at, pf_random_stream
    use parquet_sampling, only : pf_random_perm_at
    use parquet_settings_base, only : parquet_clamp_to_affinity, &
        parquet_set_verbosity, parquet_get_verbosity, &
        parquet_set_message_stream, parquet_get_message_stream

    implicit none
    private

    public :: pf_objective, pf_objective_eval, pf_objective_func
    public :: pf_constrained_objective, pf_constraint_count, pf_constraint_eval
    public :: pf_optimize_info, pf_optimize_history
    public :: pf_local_solver, pf_local_run, pf_simplex_solver
    public :: pf_minimize_scalar, pf_minimize_simplex
    public :: pf_minimize_de, pf_minimize_multistart
    public :: parquet_debug_optimize_threads_used
    public :: PF_OPT_OK, PF_OPT_LIMIT, PF_OPT_TARGET
    public :: PF_OPT_NONFINITE, PF_OPT_ROUNDING, PF_OPT_INFEASIBLE
    !
    ! ---- Re-exported from parquet_settings_base ----
    !
    ! **A module re-exports, get and set, every knob its own code reads.** This module reads no
    ! knob at all -- `threads=` is an argument and is never resolved from a setting -- but the
    ! population tier's thread clamp can emit one notice per process, and the output pair is what
    ! silences it. Without these a program whose only import is `use parquet_optimize` could quiet
    ! that notice only through `use parquet_settings`, which reaches `parquet_bindings` and drags
    ! the whole Arrow stack back into a build this module exists to keep clear of it.
    public :: parquet_set_verbosity, parquet_get_verbosity
    public :: parquet_set_message_stream, parquet_get_message_stream

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

    !> Default generation budget for `pf_minimize_de`.
    integer, parameter :: DE_MAX_GEN = 1000

    !> Smallest population `pf_minimize_de` accepts: DE/rand/1 needs an individual plus three
    !! others distinct from it and from each other.
    integer, parameter :: DE_MIN_NP = 4

    !> Default differential weight `F` of `pf_minimize_de`.
    real(real64), parameter :: DE_F_WEIGHT = 0.8_real64

    !> Default crossover probability `CR` of `pf_minimize_de`.
    real(real64), parameter :: DE_CR = 0.9_real64

    !> Default fractional tolerance on the population's value spread.
    real(real64), parameter :: DE_FTOL = 1.0e-6_real64

    !> Step `polish=` gives the simplex, as a fraction of each coordinate's width.
    real(real64), parameter :: DE_POLISH_STEP = 1.0e-3_real64

    !> Default merge radius of `pf_minimize_multistart`, as a fraction of each coordinate's width.
    real(real64), parameter :: MULTISTART_XTOL = 1.0e-6_real64

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

    !> Holds one thread's own clone of the caller's objective.
    !!
    !! **A shared array allocated BEFORE the region, one slot per thread, indexed by
    !! `omp_get_thread_num() + 1`** -- the shape `materialize_marked_parallel` uses. Never a
    !! `private()` copy (gfortran leaves the allocatable component's descriptor garbage) and never
    !! block-local inside the region (ifx's privatization scaffolding segfaults on a type with an
    !! allocatable component). The clone is made by `allocate(slots(t)%obj, source=f)`, a sourced
    !! allocation of the polymorphic dummy, so the caller's dynamic type and every component come
    !! with it; what a clone then accumulates is discarded at the end of the call.
    type :: objective_slot
        class(pf_objective), allocatable :: obj !! this thread's clone of the caller's objective
    end type objective_slot

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

    !> Minimises a function of one or many variables over a box by differential evolution
    !! (DE/rand/1/bin), from a seed rather than a start point.
    !!
    !! A global engine: it keeps a population spread over the whole box, so unlike the two local
    !! engines it is not confined to the basin it started in. The initial population is a Latin
    !! hypercube -- one stratum of `np` per coordinate, permuted independently -- so every
    !! coordinate is covered evenly however small `np` is.
    !!
    !! **The answer does not depend on `threads`.** Every draw is addressed by
    !! `(seed, generation, individual)`, and the selection is element-wise with the best taken by
    !! `minloc`, so a run at any thread count and any schedule reaches the same point by the same
    !! path. Cross-compiler and cross-version equality is NOT promised.
    !!
    !! Arguments: `f` the objective; `lower` and `upper` the box, finite with `lower < upper` in
    !! every coordinate; `seed` the run's seed; `x` the best point found and `fmin` its value, both
    !! `intent(out)`; `np` the population size, default `max(20, 10n)` and at least 4; `f_weight`
    !! the differential weight, default `0.8`, in `(0, 2]`; `cr` the crossover probability, default
    !! `0.9`, in `[0, 1]`; `ftol` and `atol` the fractional and absolute tolerances on the
    !! population's value spread, default `1e-6` and `0`, at least one positive; `ftarget` a value
    !! to stop at; `max_gen` the generation budget, default 1000; `max_neval` the evaluation
    !! budget, default `np*(max_gen + 1)`; `threads` the team the population is evaluated on,
    !! default 1; `polish` to finish with `pf_minimize_simplex` from the best individual, default
    !! false; then `info`, `history` (the best of each generation), `population` (the final
    !! population, one individual per column) and `context`.
    interface pf_minimize_de

        !> Differential evolution with the objective as an object.
        module subroutine minimize_de_obj(f, lower, upper, seed, x, fmin, np, f_weight, cr, ftol, &
                                          atol, ftarget, max_gen, max_neval, threads, polish, &
                                          info, history, population, context)
            implicit none
            class(pf_objective), intent(inout)               :: f          !! the objective
            real(real64), intent(in)                         :: lower(:)   !! the box, lower corner
            real(real64), intent(in)                         :: upper(:)   !! the box, upper corner
            integer(int64), intent(in)                       :: seed       !! the run's seed
            real(real64), intent(out)                        :: x(:)       !! the best point found
            real(real64), intent(out)                        :: fmin       !! value at `x`
            integer, intent(in), optional                    :: np         !! population size
            real(real64), intent(in), optional               :: f_weight   !! differential weight
            real(real64), intent(in), optional               :: cr         !! crossover probability
            real(real64), intent(in), optional               :: ftol       !! fractional tolerance
            real(real64), intent(in), optional               :: atol       !! absolute tolerance
            real(real64), intent(in), optional               :: ftarget    !! stop at this value
            integer, intent(in), optional                    :: max_gen    !! generation budget
            integer, intent(in), optional                    :: max_neval  !! evaluation budget
            integer, intent(in), optional                    :: threads    !! evaluation team size
            logical, intent(in), optional                    :: polish     !! finish with a simplex
            type(pf_optimize_info), intent(out), optional    :: info       !! what happened
            type(pf_optimize_history), intent(out), optional :: history    !! best of each generation
            real(real64), allocatable, intent(out), optional :: population(:,:) !! final population
            character(len=*), intent(in), optional           :: context    !! call-site text
        end subroutine minimize_de_obj

        !> Differential evolution with the objective as a plain module procedure.
        module subroutine minimize_de_func(f, lower, upper, seed, x, fmin, np, f_weight, cr, ftol, &
                                           atol, ftarget, max_gen, max_neval, threads, polish, &
                                           info, history, population, context)
            implicit none
            procedure(pf_objective_func)                     :: f          !! the objective
            real(real64), intent(in)                         :: lower(:)   !! the box, lower corner
            real(real64), intent(in)                         :: upper(:)   !! the box, upper corner
            integer(int64), intent(in)                       :: seed       !! the run's seed
            real(real64), intent(out)                        :: x(:)       !! the best point found
            real(real64), intent(out)                        :: fmin       !! value at `x`
            integer, intent(in), optional                    :: np         !! population size
            real(real64), intent(in), optional               :: f_weight   !! differential weight
            real(real64), intent(in), optional               :: cr         !! crossover probability
            real(real64), intent(in), optional               :: ftol       !! fractional tolerance
            real(real64), intent(in), optional               :: atol       !! absolute tolerance
            real(real64), intent(in), optional               :: ftarget    !! stop at this value
            integer, intent(in), optional                    :: max_gen    !! generation budget
            integer, intent(in), optional                    :: max_neval  !! evaluation budget
            integer, intent(in), optional                    :: threads    !! evaluation team size
            logical, intent(in), optional                    :: polish     !! finish with a simplex
            type(pf_optimize_info), intent(out), optional    :: info       !! what happened
            type(pf_optimize_history), intent(out), optional :: history    !! best of each generation
            real(real64), allocatable, intent(out), optional :: population(:,:) !! final population
            character(len=*), intent(in), optional           :: context    !! call-site text
        end subroutine minimize_de_func

    end interface pf_minimize_de

    !> Runs a local solver from a Latin hypercube of starts over a box and reports the best minimum
    !! and how many distinct ones were found.
    !!
    !! The cheap global method for an objective with a handful of basins: a local engine converges
    !! to whichever basin it starts in, so enough starts spread evenly over the box find every
    !! basin wide enough to catch one. On a function with thousands of local minima it is not a
    !! substitute for `pf_minimize_de`, and the guide page says which is which.
    !!
    !! **The starts depend on `seed`, `nstart` and the box, and on nothing else** -- not on the
    !! solver and not on `threads` -- so two runs differing only in their solver explore the same
    !! basins. The best is the lowest value with the lowest start index winning a tie.
    !!
    !! Arguments: `f` the objective; `lower` and `upper` the box; `seed` the run's seed; `x` the
    !! best point found and `fmin` its value, both `intent(out)`; `nstart` the number of starts,
    !! default `max(10, 2n)`; `solver` the local engine and its options, default
    !! `pf_simplex_solver()`; `xtol` the merge radius as a fraction of each coordinate's width,
    !! default `1e-6`; `threads` the team the starts run on, default 1; then `info` (whose
    !! `nminima` counts the distinct minima and whose `nlimit` counts the runs that ran out of
    !! budget), `history` (every start's own minimum, before merging) and `context`.
    interface pf_minimize_multistart

        !> The multistart driver with the objective as an object.
        module subroutine minimize_multistart_obj(f, lower, upper, seed, x, fmin, nstart, solver, &
                                                  xtol, threads, info, history, context)
            implicit none
            class(pf_objective), intent(inout)               :: f        !! the objective
            real(real64), intent(in)                         :: lower(:) !! the box, lower corner
            real(real64), intent(in)                         :: upper(:) !! the box, upper corner
            integer(int64), intent(in)                       :: seed     !! the run's seed
            real(real64), intent(out)                        :: x(:)     !! the best point found
            real(real64), intent(out)                        :: fmin     !! value at `x`
            integer, intent(in), optional                    :: nstart   !! how many starts
            class(pf_local_solver), intent(in), optional     :: solver   !! the local engine
            real(real64), intent(in), optional               :: xtol     !! merge radius, as a fraction
            integer, intent(in), optional                    :: threads  !! team the starts run on
            type(pf_optimize_info), intent(out), optional    :: info     !! what happened
            type(pf_optimize_history), intent(out), optional :: history  !! every start's minimum
            character(len=*), intent(in), optional           :: context  !! call-site text
        end subroutine minimize_multistart_obj

        !> The multistart driver with the objective as a plain module procedure.
        module subroutine minimize_multistart_func(f, lower, upper, seed, x, fmin, nstart, solver, &
                                                   xtol, threads, info, history, context)
            implicit none
            procedure(pf_objective_func)                     :: f        !! the objective
            real(real64), intent(in)                         :: lower(:) !! the box, lower corner
            real(real64), intent(in)                         :: upper(:) !! the box, upper corner
            integer(int64), intent(in)                       :: seed     !! the run's seed
            real(real64), intent(out)                        :: x(:)     !! the best point found
            real(real64), intent(out)                        :: fmin     !! value at `x`
            integer, intent(in), optional                    :: nstart   !! how many starts
            class(pf_local_solver), intent(in), optional     :: solver   !! the local engine
            real(real64), intent(in), optional               :: xtol     !! merge radius, as a fraction
            integer, intent(in), optional                    :: threads  !! team the starts run on
            type(pf_optimize_info), intent(out), optional    :: info     !! what happened
            type(pf_optimize_history), intent(out), optional :: history  !! every start's minimum
            character(len=*), intent(in), optional           :: context  !! call-site text
        end subroutine minimize_multistart_func

    end interface pf_minimize_multistart

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

    ! ---- the population tier's shared workers -------------------------------------------------

    interface

        !> Validates a box: `lower`, `upper` and `x` the same size, every bound finite, every
        !! lower bound below its upper bound.
        module subroutine validate_box(entry_point, lower, upper, n, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            real(real64), intent(in)               :: lower(:)    !! the box, lower corner
            real(real64), intent(in)               :: upper(:)    !! the box, upper corner
            integer, intent(in)                    :: n           !! `size(x)`, the variable count
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine validate_box

        !> Validates a `threads=` request and lowers it to what this process's affinity allows.
        !!
        !! **The one place either module resolves a thread count**, and the only call that can
        !! print: `parquet_clamp_to_affinity` warns once per process when the mask is narrower than
        !! the request. The count is never read from a setting -- `threads=` is the whole story --
        !! and an absent argument is 1, so the default is serial and the caller's own objective is
        !! what gets evaluated.
        module subroutine resolve_threads(entry_point, threads, nt, context)
            implicit none
            character(len=*), intent(in)           :: entry_point !! the generic the caller named
            integer, intent(in), optional          :: threads     !! what the caller asked for
            integer, intent(out)                   :: nt          !! the team size actually to open
            character(len=*), intent(in), optional :: context     !! caller's call-site text
        end subroutine resolve_threads

        !> Lays `m` points out on a Latin hypercube over the box, one point per column.
        !!
        !! One independent permutation of the `m` strata per coordinate, with a uniform position
        !! inside the stratum: `design(j, i)` is a function of `(seed, j, i)` alone, so the design
        !! is the same at every thread count and the same for every solver the caller then runs
        !! over it. `pf_random_perm_at` is `pure elemental` and reads no setting.
        module subroutine latin_hypercube(seed, lower, upper, design)
            implicit none
            integer(int64), intent(in) :: seed        !! the design's seed
            real(real64), intent(in)   :: lower(:)    !! the box, lower corner
            real(real64), intent(in)   :: upper(:)    !! the box, upper corner
            real(real64), intent(out)  :: design(:,:) !! `(size(lower), m)`; one point per column
        end subroutine latin_hypercube

        !> Records the team size a threaded region actually opened, for the debug hook below.
        module subroutine record_team_size(n)
            implicit none
            integer, intent(in) :: n !! threads the region opened; 1 on the serial arm
        end subroutine record_team_size

        !> Team size the last threaded region of this module actually opened; 1 with no OpenMP,
        !! at `threads = 1`, or before any threaded call.
        !!
        !! **Test-only, and the only thing that can fail when `threads=` is dropped.** The answer
        !! is bit-identical at every thread count by construction, so a count that is validated,
        !! clamped and then never reaches a `num_threads` clause passes every assertion a test
        !! could make about `x`, `fmin`, `info` or `population`. This reports what the region
        !! ACHIEVED -- `omp_get_num_threads()` from inside it -- not what was resolved, so a team
        !! the runtime declined to grant is visible too.
        !!
        !! It is saved process-global state, so a suite that reads it must be excluded from
        !! test-drive's per-test parallelism (`suite_is_safe_to_parallelize`).
        module function parquet_debug_optimize_threads_used() result(n)
            implicit none
            integer :: n !! threads the last threaded region opened; 1 means serial
        end function parquet_debug_optimize_threads_used

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
