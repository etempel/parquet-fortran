!> Powell's derivative-free methods, from PRIMA: BOBYQA for bounds, LINCOA for linear constraints
!> and COBYLA for nonlinear ones.
!!
!! **What this tier is.** `parquet_optimize` carries the general-purpose engines and the types;
!! this module carries the ones vendored from PRIMA (Reference Implementation for Powell's
!! methods with Modernization and Amelioration, Zaikun Zhang, BSD-3-Clause), reworked to this
!! repository's rules. It imports `parquet_optimize` and nothing else, so it reaches no reader, no
!! writer and no Arrow symbol, and a program whose only import is `use parquet_prima` links
!! without the Arrow stack (`check_parquet_prima_stays_arrow_free`).
!!
!! **Which engine to reach for.** The constraints decide. Bounds only, or none:
!! `pf_minimize_bobyqa`. Linear constraints as well: `pf_minimize_lincoa`. A constraint that is not
!! linear in `x`: `pf_minimize_cobyla`, which is the only one that reads a
!! `pf_constrained_objective` -- the other two refuse one rather than drop its constraints.
!!
!! BOBYQA and LINCOA build a quadratic model from the values they have already paid for and take a
!! trust-region step in it, which costs far fewer evaluations than the simplex on a smooth problem
!! of more than two or three variables. COBYLA models the objective and every constraint LINEARLY
!! over a simplex, so it is the cheapest to set up and the slowest to converge; reach for it when a
!! constraint leaves no alternative.
!!
!! All three are LOCAL methods -- each converges to the basin it starts in -- so on a multi-modal
!! problem BOBYQA goes inside `pf_minimize_multistart` as a `pf_bobyqa_solver`, or
!! `pf_minimize_de` does the global search. `doc/pages/utilities/prima.md` has the comparison and
!! the worked examples.
!!
!! **Silence, as everywhere in this tier.** Nothing here prints. What PRIMA reports through
!! `iprint` is reported through `info=`, and a caller mistake is an `error stop` carrying the
!! entry point, the message and the caller's `context`.
!!
!! **Threads.** Every procedure here is reentrant: the vendored engines hold no `save` variable,
!! open no parallel region and draw no random number, so a `pf_bobyqa_solver` is safe under
!! `pf_minimize_multistart`'s threads, one clone of the objective per thread. The one shared thing
!! is the abort, which enters the same named critical as `parquet_optimize`'s.
!!
!! **Attribution.** The engines are derived from PRIMA at commit `43863c69`; each engine file
!! carries the BSD-3-Clause text and the list of what was changed, and README's License section
!! carries the same notice. The algorithms are M. J. D. Powell's.
module parquet_prima

    use iso_fortran_env, only : real64
    use parquet_optimize, only : pf_objective, pf_objective_eval, pf_objective_func, &
        pf_constrained_objective, pf_constraint_count, pf_constraint_eval, &
        pf_optimize_info, pf_optimize_history, pf_local_solver, pf_local_run, &
        PF_OPT_OK, PF_OPT_LIMIT, PF_OPT_TARGET, PF_OPT_NONFINITE, PF_OPT_ROUNDING, &
        PF_OPT_INFEASIBLE

    implicit none
    private

    public :: pf_minimize_bobyqa, pf_minimize_lincoa, pf_minimize_cobyla
    public :: pf_bobyqa_solver
    !
    ! ---- Re-exported from parquet_optimize ----
    !
    ! An objective written for `pf_minimize_bobyqa`, and a program that reads `info` back, need
    ! `pf_objective` and `pf_optimize_info` as much as they need the entry point. Without these a
    ! caller would write `use parquet_prima` and `use parquet_optimize` side by side for every
    ! program, and the guide would have to explain why. The names are the same names: one
    ! `pf_objective` exists, declared once in `parquet_optimize`.
    public :: pf_objective, pf_objective_eval, pf_objective_func
    public :: pf_constrained_objective, pf_constraint_count, pf_constraint_eval
    public :: pf_optimize_info, pf_optimize_history
    public :: pf_local_solver, pf_local_run
    public :: PF_OPT_OK, PF_OPT_LIMIT, PF_OPT_TARGET
    public :: PF_OPT_NONFINITE, PF_OPT_ROUNDING, PF_OPT_INFEASIBLE

    ! ---- the plain-function adapter, shared by the driver submodules ---------------------------

    !> Wraps a plain `procedure(pf_objective_func)` as an objective object.
    !!
    !! Private: the type is an implementation detail of "the function form delegates to the object
    !! form", not API. `parquet_optimize` has one of these for its own specifics and keeps it
    !! private too, so this module declares its own rather than widening that one.
    !!
    !! It is declared HERE, in the spec, rather than in a driver submodule, because two of the
    !! three driver submodules need it and a submodule cannot see a sibling's declarations. Its
    !! `eval` is a separate module procedure for the reason `.claude/rules/code-style.md` gives: a
    !! private procedure contained directly in a module and called only from its submodules does
    !! not link under gfortran.
    type, extends(pf_objective) :: prima_func_objective
        procedure(pf_objective_func), nopass, pointer :: fun => null() !! the wrapped function
    contains
        procedure :: eval => prima_func_objective_eval !! Calls the wrapped function.
    end type prima_func_objective

    interface

        !> Calls the wrapped plain function.
        module function prima_func_objective_eval(this, x) result(f)
            implicit none
            class(prima_func_objective), intent(inout) :: this !! the wrapper
            real(real64), intent(in)                   :: x(:) !! the point
            real(real64)                               :: f    !! objective value at `x`
        end function prima_func_objective_eval

    end interface

    ! ---- local-solver object, for pf_minimize_multistart ---------------------------------------

    !> BOBYQA as a local solver for `pf_minimize_multistart`.
    !!
    !! Hand one of these to the driver's `solver=` and every start is refined by BOBYQA instead of
    !! by the simplex. The driver clones the objective per thread and gives each run the box as
    !! its bounds, which BOBYQA honours -- so unlike `pf_simplex_solver`, a run never leaves the
    !! box it started in.
    !!
    !! The defaults scale the problem from that box, which is what makes the object usable without
    !! reading the BOBYQA paper first: with `scale_from_box` the engine works in `y = x/(upper -
    !! lower)`, where every coordinate spans one unit, so `rhobeg_fraction` is a fraction of the
    !! box and the same pair of radii suits a problem whose coordinates differ by many orders of
    !! magnitude. Set `scale_from_box = .false.` when the box is a search region rather than a
    !! statement about magnitudes, and the radii are then in the caller's own units.
    type, extends(pf_local_solver) :: pf_bobyqa_solver
        logical      :: scale_from_box = .true.       !! `scale = upper - lower`
        real(real64) :: rhobeg_fraction = 0.1_real64  !! initial radius, as a fraction of the box; at most `0.5`
        real(real64) :: rhoend = 1.0e-6_real64        !! final radius, in the same units
        integer      :: max_neval = 0                 !! evaluation budget; `0` means `500*n`
    contains
        procedure :: run => bobyqa_solver_run         !! Calls `pf_minimize_bobyqa` with these.
    end type pf_bobyqa_solver

    ! ---- pf_minimize_bobyqa ---------------------------------------------------------------------

    !> Minimises a smooth function of several variables, with bounds, without derivatives.
    !!
    !! Powell's BOBYQA: a quadratic model interpolating `npt` points is minimised inside a trust
    !! region and inside the bounds, the model is updated with the value at the new point, and the
    !! trust-region radius falls from `rhobeg` to `rhoend`. The returned `x` is accurate to about
    !! `rhoend` in the engine's units, so `rhoend` is the accuracy asked for and `rhobeg` is "about
    !! a tenth of the greatest expected change to a variable" (PRIMA's own advice).
    !!
    !! **It is a local method.** It converges to the minimum of the basin it starts in, and
    !! `info%converged` means its stopping rule fired, not that the minimum is global. For a
    !! function with several basins, run it from a Latin hypercube of starts through
    !! `pf_minimize_multistart` with a `pf_bobyqa_solver`, or search globally with
    !! `pf_minimize_de` first.
    !!
    !! **`scale=` is the argument to reach for when the coordinates have different magnitudes.**
    !! One trust-region radius governs every coordinate, so a problem in which one variable is of
    !! order `1e-9` and another of order `1e6` cannot be served by a single `rhobeg` without it.
    !! With `scale=` the engine minimises `g(y) = f(scale*y)`, `rhobeg`, `rhoend` and `info%rho`
    !! are in `y`, and `x` and `history` come back in the caller's units.
    !!
    !! Aborts rather than adjusts: where PRIMA revises an invalid argument and warns, this refuses
    !! with a message (`doc/pages/utilities/prima.md`). The one adjustment kept is PRIMA's
    !! `honour_x0` reduction of `rhobeg` when the start lies within `rhobeg` of a bound -- the
    !! start is never moved.
    interface pf_minimize_bobyqa

        !> BOBYQA with the objective as an object.
        module subroutine minimize_bobyqa_obj(f, x, fmin, lower, upper, rhobeg, rhoend, npt, &
                                              scale, ftarget, max_neval, info, history, context)
            implicit none
            class(pf_objective), intent(inout)               :: f          !! the objective
            real(real64), intent(inout)                      :: x(:)       !! start in, minimum out
            real(real64), intent(out)                        :: fmin       !! value at `x`
            real(real64), intent(in), optional               :: lower(:)   !! bounds; absent is none
            real(real64), intent(in), optional               :: upper(:)   !! bounds; absent is none
            real(real64), intent(in), optional               :: rhobeg     !! initial radius
            real(real64), intent(in), optional               :: rhoend     !! final radius
            integer, intent(in), optional                    :: npt        !! interpolation points
            real(real64), intent(in), optional               :: scale(:)   !! per-coordinate scale
            real(real64), intent(in), optional               :: ftarget    !! stop at this value
            integer, intent(in), optional                    :: max_neval  !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info       !! what happened
            type(pf_optimize_history), intent(out), optional :: history    !! every evaluation
            character(len=*), intent(in), optional           :: context    !! call-site text
        end subroutine minimize_bobyqa_obj

        !> BOBYQA with the objective as a plain module procedure.
        module subroutine minimize_bobyqa_func(f, x, fmin, lower, upper, rhobeg, rhoend, npt, &
                                               scale, ftarget, max_neval, info, history, context)
            implicit none
            procedure(pf_objective_func)                     :: f          !! the objective
            real(real64), intent(inout)                      :: x(:)       !! start in, minimum out
            real(real64), intent(out)                        :: fmin       !! value at `x`
            real(real64), intent(in), optional               :: lower(:)   !! bounds; absent is none
            real(real64), intent(in), optional               :: upper(:)   !! bounds; absent is none
            real(real64), intent(in), optional               :: rhobeg     !! initial radius
            real(real64), intent(in), optional               :: rhoend     !! final radius
            integer, intent(in), optional                    :: npt        !! interpolation points
            real(real64), intent(in), optional               :: scale(:)   !! per-coordinate scale
            real(real64), intent(in), optional               :: ftarget    !! stop at this value
            integer, intent(in), optional                    :: max_neval  !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info       !! what happened
            type(pf_optimize_history), intent(out), optional :: history    !! every evaluation
            character(len=*), intent(in), optional           :: context    !! call-site text
        end subroutine minimize_bobyqa_func

    end interface pf_minimize_bobyqa

    ! ---- pf_minimize_lincoa ---------------------------------------------------------------------

    !> Minimises a smooth function of several variables subject to linear constraints, without
    !! derivatives.
    !!
    !! Powell's LINCOA, the same quadratic model as `pf_minimize_bobyqa` with an active-set
    !! trust-region step: the step is taken inside the constraints rather than projected onto them
    !! afterwards, so the iterates it accepts stay feasible and the point it answers with is the
    !! best FEASIBLE point it evaluated, feasible to `ctol`.
    !!
    !! **It does evaluate infeasible points on the way.** The initial interpolation set is the start
    !! displaced by `+/-rhobeg` in each coordinate, feasible or not, and a geometry step need not be
    !! feasible either; what makes the answer sound is the filter that keeps only feasible points as
    !! candidates. So the objective must be defined wherever `rhobeg` can reach from a feasible
    !! point -- `pf_minimize_bobyqa` is the engine that asks about nothing outside its bounds.
    !!
    !! **The constraints are arrays**: `a_ineq*x <= b_ineq` and `a_eq*x = b_eq`, one ROW per
    !! constraint and one column per variable, with `lower`/`upper` folded in as two more
    !! inequalities each. Nonlinear constraints do not belong here -- a `pf_constrained_objective`
    !! is refused, because minimising it without its constraints would return a confident answer
    !! from the wrong region; `pf_minimize_cobyla` is the engine that reads one.
    !!
    !! **The start point must be feasible**, and is refused when it is not (PRIMA relaxes the
    !! right-hand sides to admit an infeasible start and warns, which this tier has no channel for
    !! and would make `info%cstrv` a measurement against constraints the caller did not write).
    !! `x = 0` satisfies any homogeneous system, which is why the examples start there.
    !!
    !! `info%cstrv` is the largest violation at the returned point, measured against the caller's
    !! own constraints in the caller's own units, and `info%status` is `PF_OPT_INFEASIBLE` exactly
    !! when it exceeds `ctol`. The arguments it shares with `pf_minimize_bobyqa` -- `x`, `fmin`,
    !! `lower`, `upper`, `rhobeg`, `rhoend`, `npt`, `scale`, `ftarget`, `max_neval`, `info`,
    !! `history`, `context` -- mean what they mean there.
    interface pf_minimize_lincoa

        !> LINCOA with the objective as an object.
        module subroutine minimize_lincoa_obj(f, x, fmin, a_ineq, b_ineq, a_eq, b_eq, lower, &
                                              upper, rhobeg, rhoend, npt, scale, ctol, ftarget, &
                                              max_neval, info, history, context)
            implicit none
            class(pf_objective), intent(inout)               :: f            !! the objective
            real(real64), intent(inout)                      :: x(:)         !! start in, minimum out
            real(real64), intent(out)                        :: fmin         !! value at `x`
            real(real64), intent(in), optional               :: a_ineq(:, :) !! `a_ineq*x <= b_ineq`
            real(real64), intent(in), optional               :: b_ineq(:)    !! `a_ineq*x <= b_ineq`
            real(real64), intent(in), optional               :: a_eq(:, :)   !! `a_eq*x = b_eq`
            real(real64), intent(in), optional               :: b_eq(:)      !! `a_eq*x = b_eq`
            real(real64), intent(in), optional               :: lower(:)     !! bounds; absent is none
            real(real64), intent(in), optional               :: upper(:)     !! bounds; absent is none
            real(real64), intent(in), optional               :: rhobeg       !! initial radius
            real(real64), intent(in), optional               :: rhoend       !! final radius
            integer, intent(in), optional                    :: npt          !! interpolation points
            real(real64), intent(in), optional               :: scale(:)     !! per-coordinate scale
            real(real64), intent(in), optional               :: ctol         !! feasibility tolerance
            real(real64), intent(in), optional               :: ftarget      !! stop at this value
            integer, intent(in), optional                    :: max_neval    !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info         !! what happened
            type(pf_optimize_history), intent(out), optional :: history      !! every evaluation
            character(len=*), intent(in), optional           :: context      !! call-site text
        end subroutine minimize_lincoa_obj

        !> LINCOA with the objective as a plain module procedure.
        module subroutine minimize_lincoa_func(f, x, fmin, a_ineq, b_ineq, a_eq, b_eq, lower, &
                                               upper, rhobeg, rhoend, npt, scale, ctol, ftarget, &
                                               max_neval, info, history, context)
            implicit none
            procedure(pf_objective_func)                     :: f            !! the objective
            real(real64), intent(inout)                      :: x(:)         !! start in, minimum out
            real(real64), intent(out)                        :: fmin         !! value at `x`
            real(real64), intent(in), optional               :: a_ineq(:, :) !! `a_ineq*x <= b_ineq`
            real(real64), intent(in), optional               :: b_ineq(:)    !! `a_ineq*x <= b_ineq`
            real(real64), intent(in), optional               :: a_eq(:, :)   !! `a_eq*x = b_eq`
            real(real64), intent(in), optional               :: b_eq(:)      !! `a_eq*x = b_eq`
            real(real64), intent(in), optional               :: lower(:)     !! bounds; absent is none
            real(real64), intent(in), optional               :: upper(:)     !! bounds; absent is none
            real(real64), intent(in), optional               :: rhobeg       !! initial radius
            real(real64), intent(in), optional               :: rhoend       !! final radius
            integer, intent(in), optional                    :: npt          !! interpolation points
            real(real64), intent(in), optional               :: scale(:)     !! per-coordinate scale
            real(real64), intent(in), optional               :: ctol         !! feasibility tolerance
            real(real64), intent(in), optional               :: ftarget      !! stop at this value
            integer, intent(in), optional                    :: max_neval    !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info         !! what happened
            type(pf_optimize_history), intent(out), optional :: history      !! every evaluation
            character(len=*), intent(in), optional           :: context      !! call-site text
        end subroutine minimize_lincoa_func

    end interface pf_minimize_lincoa

    ! ---- pf_minimize_cobyla ---------------------------------------------------------------------
    !
    ! One specific rather than two, unlike the other entry points of this tier: a plain function
    ! cannot carry constraints, so `f` is a `pf_constrained_objective` and there is no function
    ! form to distinguish it from (`feature_optimizer.md` 5.2).

    interface

        !> Minimises a function of several variables subject to nonlinear constraints, without
        !! derivatives.
        !!
        !! Powell's COBYLA: linear models of the objective AND of every constraint over a simplex of
        !! `n + 1` points, a trust-region step found by linear programming, and a merit function
        !! that weighs value against violation. It is the only engine here that reads nonlinear
        !! constraints, and the slowest to converge of the three -- a linear model carries no
        !! curvature, so reach for it when a constraint leaves no alternative rather than by
        !! preference.
        !!
        !! **The constraints come from the object**, in PRIMA's sign convention: `f%n_constraints()`
        !! says how many values `f%constraints(x, c)` fills, and a point is feasible where every
        !! `c(i) <= 0`. **SciPy's COBYLA uses the opposite convention**, `c(x) >= 0`, so a program
        !! ported from `scipy.optimize.minimize(method="COBYLA")` negates its constraint functions
        !! once -- otherwise it gets a valid-looking answer from the wrong region. The binding must
        !! fill exactly `n_constraints()` values; the language cannot check it.
        !!
        !! Linear constraints may be given as arrays beside them, in the same form
        !! `pf_minimize_lincoa` takes, and `lower`/`upper` are folded in as two more inequalities
        !! each. There is no `npt`: the simplex has `n + 1` vertices and no choice about it.
        !!
        !! **An infeasible start is normal here** -- COBYLA drives towards feasibility rather than
        !! requiring it -- and so is an infeasible ANSWER: `info%status` is `PF_OPT_INFEASIBLE`
        !! when `info%cstrv` exceeds `ctol` at the end, the run returns the least-violating point
        !! it found, and nothing is printed. `info%cstrv` is measured against the caller's own
        !! constraints in the caller's own units.
        !!
        !! The arguments it shares with `pf_minimize_bobyqa` -- `x`, `fmin`, `lower`, `upper`,
        !! `rhobeg`, `rhoend`, `scale`, `ftarget`, `max_neval`, `info`, `history`, `context` --
        !! mean what they mean there.
        module subroutine pf_minimize_cobyla(f, x, fmin, a_ineq, b_ineq, a_eq, b_eq, lower, &
                                             upper, rhobeg, rhoend, scale, ctol, ftarget, &
                                             max_neval, info, history, context)
            implicit none
            class(pf_constrained_objective), intent(inout)   :: f            !! objective and constraints
            real(real64), intent(inout)                      :: x(:)         !! start in, minimum out
            real(real64), intent(out)                        :: fmin         !! value at `x`
            real(real64), intent(in), optional               :: a_ineq(:, :) !! `a_ineq*x <= b_ineq`
            real(real64), intent(in), optional               :: b_ineq(:)    !! `a_ineq*x <= b_ineq`
            real(real64), intent(in), optional               :: a_eq(:, :)   !! `a_eq*x = b_eq`
            real(real64), intent(in), optional               :: b_eq(:)      !! `a_eq*x = b_eq`
            real(real64), intent(in), optional               :: lower(:)     !! bounds; absent is none
            real(real64), intent(in), optional               :: upper(:)     !! bounds; absent is none
            real(real64), intent(in), optional               :: rhobeg       !! initial radius
            real(real64), intent(in), optional               :: rhoend       !! final radius
            real(real64), intent(in), optional               :: scale(:)     !! per-coordinate scale
            real(real64), intent(in), optional               :: ctol         !! feasibility tolerance
            real(real64), intent(in), optional               :: ftarget      !! stop at this value
            integer, intent(in), optional                    :: max_neval    !! evaluation budget
            type(pf_optimize_info), intent(out), optional    :: info         !! what happened
            type(pf_optimize_history), intent(out), optional :: history      !! every evaluation
            character(len=*), intent(in), optional           :: context      !! call-site text
        end subroutine pf_minimize_cobyla

    end interface

    ! ---- the solver binding, implemented in the same submodule ---------------------------------

    interface

        !> `pf_bobyqa_solver%run`: derives the scale and the radii from the box and calls
        !! `pf_minimize_bobyqa`.
        module subroutine bobyqa_solver_run(this, obj, x, fmin, lower, upper, info)
            implicit none
            class(pf_bobyqa_solver), intent(in) :: this     !! the solver and its options
            class(pf_objective), intent(inout)  :: obj      !! the objective
            real(real64), intent(inout)         :: x(:)     !! start in, minimum out
            real(real64), intent(out)           :: fmin     !! value at `x`
            real(real64), intent(in)            :: lower(:) !! the driver's box
            real(real64), intent(in)            :: upper(:) !! the driver's box
            type(pf_optimize_info), intent(out) :: info     !! this run's outcome
        end subroutine bobyqa_solver_run

    end interface

end module parquet_prima ! GCOVR_EXCL_LINE
