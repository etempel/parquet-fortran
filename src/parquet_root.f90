!> Root finding in one `real64` variable: `pf_find_root`, Brent's method on a bracket, with the
!> bracket widened first under a growth policy the caller states.
!!
!! `parquet_root` is an **Arrow-free leaf**: it imports the two INTRINSIC modules
!! `iso_fortran_env` and `ieee_arithmetic`, and no module of this library. An intrinsic module is
!! not a compiled file, not a tier edge and not a footprint entry, so `use parquet_root` compiles
!! two Fortran files and never crosses the C++ boundary. `check_parquet_root_stays_arrow_free`
!! (tools/check_source_conventions.py) keeps that true.
!!
!! **Nothing in this module prints.** It reads no setting, emits no INFO, NOTE or WARNING, and
!! re-exports no knob; a caller learns what happened from `converged=`, from `info=` and from the
!! `PF_ROOT_*` status codes. The only output path is `error stop`, and every one of those is a
!! caller contract that was broken (`pf_find_root: ...` messages, one per row of the guide page's
!! table), or a function that returned a NaN.
!!
!! **A missing sign change is a status, not an error.** A bracket whose two ends have the same
!! sign -- with no expansion asked for, or with the expansion used up -- returns
!! `PF_ROOT_NO_BRACKET`. A caller that answers it by retrying with a wider policy needs an answer
!! it can act on, not an abort.
!!
!! **An infinity is a sign; a NaN is an error.** `+/-Infinity` from the function has a sign and
!! takes part in the sign test like any other value, and a bracket with an infinite end is bisected
!! rather than interpolated through until that end becomes finite. A NaN has no sign, so there is
!! no answer to return, and it aborts. `pf_integrate` reports the same event as a status instead
!! (`PF_INT_BAD_VALUE`); the difference is deliberate, because a quadrature can say where the bad
!! value was and let a sweep carry on, while a root finder has no sign to carry on from.
!!
!! **No evaluation order overflows.** A bracket may lie anywhere in the finite range and the
!! function may return values of any magnitude, and nagfor's default `-ieee=stop` ends the process
!! on the first `IEEE_OVERFLOW`, `IEEE_DIVIDE_BY_ZERO` or `IEEE_INVALID`. So an expansion never
!! forms an end beyond its limit, the bracket's width never passes `huge`, and the interpolation
!! step is formed from operands that are harmless wherever it is not taken: an optimiser may form
!! a guarded expression before its guard (`fortran-gotchas.md`).
!!
!! **Thread safety is by construction.** The module has no variable that is not a `parameter`;
!! every work variable is a local of the call, and the only state that outlives a call is the
!! caller's own function object. Give each thread its own `class(pf_rootfun)` object and
!! concurrent solves are independent. The `error stop` is taken under a named `critical`, so one
!! thread aborts.
!!
!! **The function is an object or a plain function.** An object extending `pf_rootfun` carries its
!! parameters as components and may update them from `eval` (a call counter, a cache); a
!! `procedure(pf_root_func)` is the parameterless form. Either way the callback must be a MODULE
!! procedure or a type-bound procedure -- never an internal procedure, which flang cannot pass at
!! all and which gfortran implements with an executable stack.
!!
!! The method is Brent's `zeroin`, written here rather than vendored;
!! `src/parquet_root_solve.f90`'s header lists where it departs from `zeroin.f`.
module parquet_root

    use iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite, ieee_is_nan

    implicit none
    private

    public :: pf_rootfun, pf_rootfun_eval, pf_root_func
    public :: pf_bracket_expansion, pf_root_info, pf_root_history
    public :: pf_find_root
    public :: PF_EXPAND_NONE, PF_EXPAND_UP, PF_EXPAND_DOWN, PF_EXPAND_BOTH
    public :: PF_ROOT_OK, PF_ROOT_LIMIT, PF_ROOT_NO_BRACKET

    ! ---- status codes, the values of pf_root_info%status ------------------------------------
    !
    ! Declared ABOVE pf_root_info because that type's `status` component initialises from
    ! PF_ROOT_OK, and a named constant in an initialisation expression must already be declared.

    integer, parameter :: PF_ROOT_OK = 0         !! the tolerance was met, or a value was exactly zero
    integer, parameter :: PF_ROOT_LIMIT = 1      !! `max_neval` was spent first
    integer, parameter :: PF_ROOT_NO_BRACKET = 2 !! no sign change: expansion used up, or none asked

    ! ---- growth policies, the values of pf_bracket_expansion%mode ---------------------------

    integer, parameter :: PF_EXPAND_NONE = 0 !! the given bracket must already contain a sign change
    integer, parameter :: PF_EXPAND_UP = 1   !! hold `a`, grow `b` outward
    integer, parameter :: PF_EXPAND_DOWN = 2 !! hold `b`, grow `a` outward
    integer, parameter :: PF_EXPAND_BOTH = 3 !! grow both ends outward about the midpoint

    ! ---- internal parameters, not part of the public surface -------------------------------

    !> Default budget on function evaluations, `max_neval`'s default, the expansion's included.
    !! Doubling a bracket from `1e-11` past `1` takes 37 tries, and a bracketed solve rarely
    !! needs more than a few tens, so reaching it means the function, not the caller.
    integer, parameter :: DEFAULT_MAX_NEVAL = 200
    !> Characters of caller-supplied `context` an abort message reproduces before eliding with
    !! `...` (`api-conventions.md`, errors and diagnostics).
    integer, parameter :: CONTEXT_CAP = 100
    !> `rtol`'s default and its floor, `4*epsilon`: SciPy's `brentq` default. A smaller `rtol` is
    !! raised to it rather than refused.
    real(real64), parameter :: RTOL_FLOOR = 4.0_real64*epsilon(1.0_real64)

    ! ---- the function ------------------------------------------------------------------------

    !> Base type for a function that carries its own parameters as components.
    !!
    !! Extend it in a MODULE, add whatever the function needs, and implement `eval`. One object
    !! per thread: `eval` may update the object, so two threads sharing one object race.
    type, abstract :: pf_rootfun
    contains
        procedure(pf_rootfun_eval), deferred :: eval !! Returns f at one point.
    end type pf_rootfun

    abstract interface

        !> Evaluates a parameterised function at one point.
        !!
        !! The passed-object dummy is `intent(inout)` so that an extension may keep a call counter
        !! or a cache; an extension must declare the same intent.
        function pf_rootfun_eval(this, x) result(y)
            import :: pf_rootfun, real64
            implicit none
            class(pf_rootfun), intent(inout) :: this !! the function object, which may update
            real(real64), intent(in)         :: x    !! where to evaluate
            real(real64)                     :: y    !! f(x)
        end function pf_rootfun_eval

        !> Evaluates a plain, parameterless function at one point.
        !!
        !! Must be a module procedure, never an internal one.
        function pf_root_func(x) result(y)
            import :: real64
            implicit none
            real(real64), intent(in) :: x !! where to evaluate
            real(real64)             :: y !! f(x)
        end function pf_root_func

    end interface

    !> Wraps a plain `pf_root_func` as a `pf_rootfun`, so the solver has one function shape.
    !! Private: the plain-function specific builds one and delegates; callers never see it.
    type, extends(pf_rootfun) :: func_rootfun
        procedure(pf_root_func), nopass, pointer :: fp => null() !! the wrapped function
    contains
        procedure :: eval => func_rootfun_eval !! Calls the wrapped function.
    end type func_rootfun

    ! ---- the growth policy, the outcome and the record ---------------------------------------

    !> How `pf_find_root` widens a bracket whose two ends have the same sign.
    !!
    !! The default object expands not at all, so a call with no `expand=` is the strict contract
    !! of a bracketed method. Each try takes the width `w = b - a` and moves the ends `mode`
    !! names: `PF_EXPAND_UP` puts `b` at `a + factor*w`, `PF_EXPAND_DOWN` puts `a` at
    !! `b - factor*w`, and `PF_EXPAND_BOTH` puts the ends `factor*w/2` either side of the midpoint,
    !! moving and evaluating the lower end first. The expansion stops at the FIRST sign change it
    !! sees, so growing a small bracket upward from `a` finds the smallest root above `a` that a
    !! probe steps over.
    !!
    !! No end goes past its limit, and the width never passes `huge(1.0_real64)`: a move that
    !! would is cut short there, an end already there stays put without being evaluated again, and
    !! a try that can move neither end ends the expansion as `max_tries` does.
    type :: pf_bracket_expansion
        integer      :: mode = PF_EXPAND_NONE           !! which ends move; one of `PF_EXPAND_*`
        real(real64) :: factor = 2.0_real64             !! the width multiplies by this; above 1
        integer      :: max_tries = 64                  !! expansion tries before giving up
        real(real64) :: lower_limit = -huge(1.0_real64) !! `a` never goes below this
        real(real64) :: upper_limit = huge(1.0_real64)  !! `b` never goes above this
    end type pf_bracket_expansion

    !> What happened, for a caller who wants more than `converged`.
    !!
    !! Every component carries a default initialiser. **`froot` is how a pole is told from a
    !! root**: a sign change across a pole is converged on exactly as one across a root is, and
    !! `froot` is then huge or infinite where at a root it is near zero.
    type :: pf_root_info
        integer      :: status = PF_ROOT_OK     !! one of the `PF_ROOT_*` codes
        logical      :: converged = .true.      !! `status == PF_ROOT_OK`
        integer      :: neval = 0               !! function evaluations, the expansion's included
        integer      :: niter = 0               !! Brent iterations, after the bracket was found
        integer      :: nexpand = 0             !! expansion tries that evaluated something
        real(real64) :: froot = 0.0_real64      !! f at the returned point
        real(real64) :: bracket_lo = 0.0_real64 !! low end of the bracket solved on, or last tried
        real(real64) :: bracket_hi = 0.0_real64 !! high end of the bracket solved on, or last tried
    end type pf_root_info

    !> Every evaluation one call made, in order: the two ends, each expansion probe and each
    !! Brent step.
    !!
    !! Filled only when `pf_find_root` is called with `history=`. Grown geometrically and
    !! **trimmed to `n` on output**, as `pf_optimize_history` and `pf_integration_points` are, so
    !! `x(1:n)` and `f(1:n)` are the whole record and `size(x) == n`.
    type :: pf_root_history
        integer                   :: n = 0 !! records in use; `x` and `f` are valid in `1:n`
        real(real64), allocatable :: x(:)  !! every point evaluated, in order
        real(real64), allocatable :: f(:)  !! the value returned there
    contains
        procedure :: append => root_history_append !! Appends one record, growing geometrically.
    end type pf_root_history

    ! ---- the public generic -------------------------------------------------------------------
    !
    ! Two specifics, told apart by the first dummy: a data object against a procedure. Each is
    ! written in the fully restated `module subroutine` form with its own `implicit none`.

    !> Finds a root of `f` in `[a, b]` by Brent's method, widening the bracket first if asked to.
    !!
    !! ```
    !! call pf_find_root(f, a, b, x, [expand], [tol], [rtol], [max_neval], [converged], [info], &
    !!                   [history], [context])
    !! ```
    !!
    !! Optional arguments are shown in square brackets, with the comma outside the bracket.
    !! FORD renders no per-argument documentation for a generic with several specifics, so every
    !! argument is named here instead:
    !!
    !! * `f` -- the function: a `class(pf_rootfun)` object carrying its own parameters, or a
    !!   plain `procedure(pf_root_func)` function. Must be a module procedure or a type-bound
    !!   procedure of an object, never an internal procedure.
    !! * `a`, `b` -- the bracket, `real64`: both finite, `a < b`, and `b - a` finite. Their values
    !!   need not differ in sign when `expand` asks for an expansion.
    !! * `x` -- `real64`, `intent(out)`: the root. Set on every path that returns: the converged
    !!   point on `PF_ROOT_OK`, the best point reached on `PF_ROOT_LIMIT`, and the end of the last
    !!   bracket with the smaller `|f|` on `PF_ROOT_NO_BRACKET`.
    !! * `expand` -- optional `pf_bracket_expansion`, the growth policy; default no expansion.
    !! * `tol` -- optional absolute tolerance on `x`, `real64`, default 0.
    !! * `rtol` -- optional relative tolerance on `x`, `real64`, default and floor `4*epsilon`; a
    !!   smaller value is raised to the floor. The search stops when the bracket around the root
    !!   is no wider than `2*t`, with `t = 2*epsilon*|x| + max(tol, rtol*|x|)/2`, so `x` is within
    !!   `2*t` of the root: about eight units of `epsilon*|x|` under the defaults.
    !! * `max_neval` -- optional budget on function evaluations, the expansion's included, default
    !!   200. Reaching it is `PF_ROOT_LIMIT`, not an error.
    !! * `converged` -- optional `logical`, `intent(out)`: `info%status == PF_ROOT_OK`.
    !! * `info` -- optional `type(pf_root_info)`, `intent(out)`: the status, the counts, `f` at
    !!   `x`, and the bracket solved on.
    !! * `history` -- optional `type(pf_root_history)`, `intent(out)`: every evaluation, in order.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    interface pf_find_root

        !> The function as an object.
        module subroutine find_root_obj(f, a, b, x, expand, tol, rtol, max_neval, converged, info, &
                                        history, context)
            implicit none
            class(pf_rootfun), intent(inout)                 :: f         !! the function
            real(real64), intent(in)                         :: a         !! bracket, lower end
            real(real64), intent(in)                         :: b         !! bracket, upper end
            real(real64), intent(out)                        :: x         !! the root
            type(pf_bracket_expansion), intent(in), optional :: expand    !! the growth policy
            real(real64), intent(in), optional               :: tol       !! absolute tolerance
            real(real64), intent(in), optional               :: rtol      !! relative tolerance
            integer, intent(in), optional                    :: max_neval !! evaluation budget; a
                                                                          !! bracketed solve takes
                                                                          !! tens, never huge(1)
            logical, intent(out), optional                   :: converged !! status is OK
            type(pf_root_info), intent(out), optional        :: info      !! what happened
            type(pf_root_history), intent(out), optional     :: history   !! every evaluation
            character(len=*), intent(in), optional           :: context   !! call-site text
        end subroutine find_root_obj

        !> The function as a plain module procedure.
        module subroutine find_root_func(f, a, b, x, expand, tol, rtol, max_neval, converged, info, &
                                         history, context)
            implicit none
            procedure(pf_root_func)                          :: f         !! the function
            real(real64), intent(in)                         :: a         !! bracket, lower end
            real(real64), intent(in)                         :: b         !! bracket, upper end
            real(real64), intent(out)                        :: x         !! the root
            type(pf_bracket_expansion), intent(in), optional :: expand    !! the growth policy
            real(real64), intent(in), optional               :: tol       !! absolute tolerance
            real(real64), intent(in), optional               :: rtol      !! relative tolerance
            integer, intent(in), optional                    :: max_neval !! evaluation budget; a
                                                                          !! bracketed solve takes
                                                                          !! tens, never huge(1)
            logical, intent(out), optional                   :: converged !! status is OK
            type(pf_root_info), intent(out), optional        :: info      !! what happened
            type(pf_root_history), intent(out), optional     :: history   !! every evaluation
            character(len=*), intent(in), optional           :: context   !! call-site text
        end subroutine find_root_func

    end interface pf_find_root

    ! ---- the binding targets, implemented in parquet_root_solve.f90 --------------------------

    interface

        !> Appends one `(x, f)` record, growing the arrays geometrically.
        module subroutine root_history_append(this, x, f)
            implicit none
            class(pf_root_history), intent(inout) :: this !! record appended to
            real(real64), intent(in)              :: x    !! the point evaluated
            real(real64), intent(in)              :: f    !! the value returned there
        end subroutine root_history_append

        !> Evaluates the wrapped plain function.
        module function func_rootfun_eval(this, x) result(y)
            implicit none
            class(func_rootfun), intent(inout) :: this !! the wrapper, never updated
            real(real64), intent(in)           :: x    !! where to evaluate
            real(real64)                       :: y    !! f(x)
        end function func_rootfun_eval

    end interface

end module parquet_root ! GCOVR_EXCL_LINE
