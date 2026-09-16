!> `pf_minimize_bobyqa`: the validation, the scaling, the call into the vendored engine and the
!> translation of PRIMA's exit code into a `pf_optimize_info`.
!!
!! **This file replaces PRIMA's own driver** `fortran/bobyqa/bobyqa.f90` and the part of
!! `fortran/common/preproc.f90` that it calls. Upstream reads an absent or invalid argument,
!! substitutes a default, and warns: `rhobeg` and `rhoend` are swapped if reversed, `npt` is
!! clamped into range, a start too close to a bound is moved, a bound wider than `BOUNDMAX` is
!! treated as absent. Here an absent argument gets upstream's default and an INVALID one is
!! refused with the message of `feature_optimizer.md` 5.6, because this library has no channel
!! for a warning and a silently adjusted argument is how a caller comes to believe they asked for
!! something they did not.
!!
!! **The one adjustment kept** is PRIMA's `honour_x0` reduction of `rhobeg`. BOBYQA requires the
!! start to be at least `rhobeg` from every inactive bound; upstream can satisfy that by moving
!! the start or by shrinking `rhobeg`, and this library fixes `honour_x0` at `.true.` (Q9), so the
!! start is never moved and `rhobeg` shrinks instead. A coordinate already at a bound stays there.
!! The radius the run ended at comes back in `info%rho`.
submodule (parquet_prima) parquet_prima_bobyqa

    use parquet_prima_common, only : prima_state
    use parquet_prima_bobyqb, only : bobyqb
    use parquet_prima_linalg, only : prima_abort, is_finite, is_nan, trueloc, &
        ZERO, ONE, TWO, HALF, TEN, TENTH, EPS, BOUNDMAX, &
        RHOBEG_DFT, RHOEND_DFT, FTARGET_DFT, MAXFUN_DIM_DFT, &
        SMALL_TR_RADIUS, FTARGET_ACHIEVED, TRSUBP_FAILED, MAXFUN_REACHED, MAXTR_REACHED, &
        NAN_INF_X, NAN_INF_F, NAN_INF_MODEL, NO_SPACE_BETWEEN_BOUNDS, DAMAGING_ROUNDING

    implicit none

    !> The generic every abort message here names.
    character(len=*), parameter :: EP = "pf_minimize_bobyqa"

    !> One text for every way the pair of radii can be wrong, as 5.6 specifies it.
    character(len=*), parameter :: RADII_TEXT = &
        "rhobeg and rhoend must be finite and positive with rhoend <= rhobeg"

    !> Upstream's `ETA1_DFT`: below this reduction ratio the trust-region radius contracts.
    real(real64), parameter :: ETA1_DFT = TENTH
    !> Upstream's `ETA2_DFT`: above this reduction ratio it expands.
    real(real64), parameter :: ETA2_DFT = 0.7_real64
    !> Upstream's `GAMMA1_DFT`: the contraction factor.
    real(real64), parameter :: GAMMA1_DFT = HALF
    !> Upstream's `GAMMA2_DFT`: the expansion factor.
    real(real64), parameter :: GAMMA2_DFT = TWO

    !> Wraps a plain `procedure(pf_objective_func)` as an objective object.
    !!
    !! `parquet_optimize` has one of these for its own specifics and keeps it private, so this
    !! module declares its own rather than widening that one: the type is an implementation
    !! detail of "the function form delegates to the object form", not API.
    type, extends(pf_objective) :: prima_func_objective
        procedure(pf_objective_func), nopass, pointer :: fun => null() !! the wrapped function
    contains
        procedure :: eval => prima_func_objective_eval !! Calls the wrapped function.
    end type prima_func_objective

contains

    !> Calls the wrapped plain function.
    function prima_func_objective_eval(this, x) result(f)
        class(prima_func_objective), intent(inout) :: this !! the wrapper
        real(real64), intent(in)                   :: x(:) !! the point
        real(real64)                               :: f    !! objective value at `x`

        f = this%fun(x)

    end function prima_func_objective_eval

    module procedure minimize_bobyqa_obj

        integer :: n, npt_use, maxfun_use, prima_info, nf, status
        real(real64) :: rhobeg_use, rhoend_use, rhobeg_in, ftarget_use, fout
        real(real64), allocatable :: sc(:), lo(:), hi(:), y(:)
        logical, allocatable :: at_lower(:), at_upper(:)
        type(prima_state) :: st

        n = size(x)
        call refuse_bad_call(f, n, x, lower, upper, rhobeg, rhoend, npt, scale, max_neval, context)

        ! ---- the caller's units to the engine's ------------------------------------------------
        allocate(sc(n))
        sc = ONE
        if (present(scale)) sc = scale
        allocate(y(n))
        y = x / sc
        allocate(lo(n), hi(n))
        ! An absent bound is BOUNDMAX in the ENGINE's units, never the caller's divided by the
        ! scale: `BOUNDMAX/sc` overflows to +Infinity for any scale below one, and an infinite
        ! bound puts an infinity into `su - sl` inside the engine.
        lo = -BOUNDMAX
        hi = BOUNDMAX
        if (present(lower)) lo = lower / sc
        if (present(upper)) hi = upper / sc
        y = max(lo, min(hi, y))

        ! ---- upstream's defaults, for the arguments the caller left out ------------------------
        if (present(rhobeg)) then
            rhobeg_use = rhobeg
        else
            rhobeg_use = max(EPS, min(RHOBEG_DFT, minval(hi - lo) / 4.0_real64))
        end if
        if (present(rhoend)) then
            rhoend_use = rhoend
        else
            rhoend_use = max(EPS, min((RHOEND_DFT / RHOBEG_DFT) * rhobeg_use, RHOEND_DFT))
        end if
        ftarget_use = FTARGET_DFT
        if (present(ftarget)) ftarget_use = ftarget
        maxfun_use = MAXFUN_DIM_DFT * n
        if (present(max_neval)) maxfun_use = max_neval
        npt_use = 2 * n + 1
        if (present(npt)) npt_use = npt

        ! ---- honour_x0: shrink rhobeg rather than move the start (Q9) --------------------------
        !
        ! WHAT THIS FORBIDS: restoring PRIMA's default, which moves the start towards the interior
        ! when it sits within `rhobeg` of a bound. A silently moved start is a different problem
        ! from the one the caller posed, and this tier has no channel to say so. The test
        ! `the start point is never moved, and rhobeg shrinks instead` asserts the first recorded
        ! point is the caller's own; `prima_start_outside_bounds` covers the refusal above.
        rhobeg_in = rhobeg_use
        at_lower = is_finite(lo) .and. y - lo <= EPS * max(ONE, abs(lo))
        at_upper = is_finite(hi) .and. y - hi >= -EPS * max(ONE, abs(hi))
        y(trueloc(at_lower)) = lo(trueloc(at_lower))
        y(trueloc(at_upper)) = hi(trueloc(at_upper))
        rhobeg_use = max(EPS, minval([rhobeg_use, &
            y(trueloc(.not. at_lower)) - lo(trueloc(.not. at_lower)), &
            hi(trueloc(.not. at_upper)) - y(trueloc(.not. at_upper))]))
        if (rhobeg_in - rhobeg_use > EPS * max(ONE, rhobeg_in)) then
            ! Only a real reduction drags `rhoend` down with it, and in the same proportion, so
            ! that a run whose radius was halved still asks for half the accuracy it was told to.
            rhoend_use = max(EPS, min((rhoend_use / rhobeg_in) * rhobeg_use, rhoend_use))
        end if
        rhobeg_use = max(rhobeg_use, EPS)
        rhoend_use = min(max(rhoend_use, EPS), rhobeg_use)

        ! ---- the run ---------------------------------------------------------------------------
        st%scale = sc
        st%entry_point = EP
        if (present(context)) st%context = context
        st%want_history = present(history)

        call bobyqb(f, st, maxfun_use, npt_use, ETA1_DFT, ETA2_DFT, ftarget_use, GAMMA1_DFT, &
                    GAMMA2_DFT, rhobeg_use, rhoend_use, lo, hi, y, nf, fout, prima_info)

        x = sc * y
        fmin = fout

        ! ---- what happened ---------------------------------------------------------------------
        select case (prima_info)
        case (SMALL_TR_RADIUS)
            status = PF_OPT_OK
        case (FTARGET_ACHIEVED)
            status = PF_OPT_TARGET
        case (MAXFUN_REACHED, MAXTR_REACHED)
            status = PF_OPT_LIMIT
        case default
            ! TRSUBP_FAILED, DAMAGING_ROUNDING and NAN_INF_MODEL are the rounding failures 5.6
            ! names. NAN_INF_X, NAN_INF_F and NO_SPACE_BETWEEN_BOUNDS cannot arrive: a non-finite
            ! value aborts in `evaluate` and the bounds are refused above. They land here rather
            ! than in an arm of their own so that `status` is defined whatever the engine returns.
            status = PF_OPT_ROUNDING
        end select

        if (present(info)) then
            info%status = status
            info%converged = (status == PF_OPT_OK .or. status == PF_OPT_TARGET)
            info%neval = st%neval
            info%niter = st%niter
            info%rho = st%rho
        end if

        if (present(history)) then
            history%n = st%record%n
            if (st%record%n > 0) then
                history%x = st%record%x(:, 1:st%record%n)
                history%f = st%record%f(1:st%record%n)
            else
                allocate(history%x(n, 0), history%f(0))
            end if
        end if

    end procedure minimize_bobyqa_obj

    ! The FULLY RESTATED form, not `module procedure minimize_bobyqa_func`: in the abbreviated
    ! form gfortran 15 gives the `procedure(pf_objective_func)` dummy an implicit interface and
    ! refuses the pointer assignment below with "Explicit interface required for 'f'".
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

        type(prima_func_objective) :: obj !! wraps the plain function as an objective object

        obj%fun => f
        call minimize_bobyqa_obj(obj, x, fmin, lower, upper, rhobeg, rhoend, npt, scale, ftarget, &
                                 max_neval, info, history, context)

    end subroutine minimize_bobyqa_func

    module procedure bobyqa_solver_run

        real(real64) :: sc(size(x)), rhobeg_use
        integer :: maxfun_use

        if (this%scale_from_box) then
            ! Every coordinate spans one unit in `y`, so the radii are fractions of the box.
            sc = upper - lower
            rhobeg_use = this%rhobeg_fraction
        else
            sc = ONE
            rhobeg_use = this%rhobeg_fraction * minval(upper - lower)
        end if
        maxfun_use = this%max_neval
        if (maxfun_use <= 0) maxfun_use = MAXFUN_DIM_DFT * size(x)

        call minimize_bobyqa_obj(obj, x, fmin, lower=lower, upper=upper, rhobeg=rhobeg_use, &
                                 rhoend=this%rhoend, scale=sc, max_neval=maxfun_use, info=info)

    end procedure bobyqa_solver_run

    !> Refuses every call `feature_optimizer.md` 5.6 says is refused, before anything is allocated.
    !!
    !! Impure by nature: it exists to abort (`api-conventions.md`), and ifx deletes a `pure`
    !! guard-only subroutine at `-O0`.
    subroutine refuse_bad_call(f, n, x, lower, upper, rhobeg, rhoend, npt, scale, max_neval, &
                               context)
        class(pf_objective), intent(in)        :: f          !! the caller's objective
        integer, intent(in)                    :: n          !! `size(x)`
        real(real64), intent(in)               :: x(:)       !! the start point
        real(real64), intent(in), optional     :: lower(:)   !! bounds
        real(real64), intent(in), optional     :: upper(:)   !! bounds
        real(real64), intent(in), optional     :: rhobeg     !! initial radius
        real(real64), intent(in), optional     :: rhoend     !! final radius
        integer, intent(in), optional          :: npt        !! interpolation points
        real(real64), intent(in), optional     :: scale(:)   !! per-coordinate scale
        integer, intent(in), optional          :: max_neval  !! evaluation budget
        character(len=*), intent(in), optional :: context    !! call-site text

        real(real64) :: rhobeg_test, rhoend_test
        integer :: npt_max

        ! pf_constrained_objective EXTENDS pf_objective, so the language accepts one here. An
        ! engine that cannot read its constraints would minimise it without them and return a
        ! confident answer from the wrong region.
        !
        ! WHAT THIS FORBIDS: a new entry point in this module without this `select type`. Only
        ! `pf_minimize_cobyla` may accept one, and only because it reads the constraints. The
        ! scenario `prima_bobyqa_constraints_not_honoured` is what holds it here.
        select type (f)
        class is (pf_constrained_objective)
            call abort_here("this engine does not honour nonlinear constraints; " // &
                "use pf_minimize_cobyla")
        end select

        if (n < 1) call abort_here("at least one variable is required")
        if (any(is_nan(x))) call abort_here("the start point must not contain NaN")

        if (present(scale)) then
            if (size(scale) /= n) call abort_here("scale and x must have the same size")
            if (.not. all(is_finite(scale))) call abort_here("scale must be finite and positive")
            if (any(scale <= ZERO)) call abort_here("scale must be finite and positive")
        end if

        if (present(lower)) then
            if (size(lower) /= n) call abort_here("lower, upper and x must have the same size")
        end if
        if (present(upper)) then
            if (size(upper) /= n) call abort_here("lower, upper and x must have the same size")
        end if
        if (present(lower) .and. present(upper)) then
            if (any(upper - lower <= TWO * EPS)) call abort_here( &
                "every upper bound must exceed its lower bound by more than 2*epsilon")
        end if
        ! The start is never moved (Q9), so a start outside the bounds is the caller's mistake
        ! rather than something to project away.
        if (present(lower)) then
            if (any(x < lower)) call abort_here("the start point must lie within the bounds")
        end if
        if (present(upper)) then
            if (any(x > upper)) call abort_here("the start point must lie within the bounds")
        end if

        if (present(rhobeg) .or. present(rhoend)) then
            rhobeg_test = ONE
            rhoend_test = ZERO
            if (present(rhobeg)) rhobeg_test = rhobeg
            if (present(rhoend)) rhoend_test = rhoend
            ! Finiteness FIRST, and each test on its own line: `.or.` does not short-circuit, and
            ! an ordered comparison against a NaN signals IEEE_INVALID even where it answers.
            if (.not. is_finite(rhobeg_test)) call abort_here(RADII_TEXT)
            if (.not. is_finite(rhoend_test)) call abort_here(RADII_TEXT)
            if (rhobeg_test <= ZERO) call abort_here(RADII_TEXT)
            if (present(rhoend)) then
                if (rhoend_test <= ZERO) call abort_here(RADII_TEXT)
            end if
            if (rhoend_test > rhobeg_test) call abort_here(RADII_TEXT)
        end if

        if (present(npt)) then
            npt_max = (n + 1) * (n + 2) / 2
            if (npt < n + 2) call abort_here("npt must be in [n+2, (n+1)(n+2)/2]")
            if (npt > npt_max) call abort_here("npt must be in [n+2, (n+1)(n+2)/2]")
        end if

        if (present(max_neval)) then
            if (max_neval < 1) call abort_here("max_neval must be positive")
            if (max_neval > huge(1) / 2) call abort_here("max_neval must not exceed huge(1)/2")
        end if

    contains

        !> Aborts with this entry point and the caller's context, whether or not one was given.
        subroutine abort_here(text)
            character(len=*), intent(in) :: text !! what went wrong

            if (present(context)) then
                call prima_abort(EP, text, context)
            else
                call prima_abort(EP, text)
            end if

        end subroutine abort_here

    end subroutine refuse_bad_call

end submodule parquet_prima_bobyqa
