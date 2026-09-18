!> `pf_minimize_bobyqa`: the scaling, the call into the vendored engine and the translation of
!> PRIMA's exit code into a `pf_optimize_info`.
!!
!! **This file replaces PRIMA's own driver** `fortran/bobyqa/bobyqa.f90` and the part of
!! `fortran/common/preproc.f90` that it calls. Upstream reads an absent or invalid argument,
!! substitutes a default, and warns: `rhobeg` and `rhoend` are swapped if reversed, `npt` is
!! clamped into range, a start too close to a bound is moved, a bound wider than `BOUNDMAX` is
!! treated as absent. Here an absent argument gets upstream's default and an INVALID one is
!! refused with the message of `feature_optimizer.md` 5.6, because this library has no channel
!! for a warning and a silently adjusted argument is how a caller comes to believe they asked for
!! something they did not. The refusals themselves are `refuse_bad_call` in
!! `parquet_prima_common`, shared with the other two drivers so that one set of abort sites serves
!! all three.
!!
!! **Where this driver's defaults differ from upstream's**, beyond those refusals:
!!
!! 1. The default `rhobeg` is `max(EPS, min(1, minval(hi - lo)/4))`. Upstream's is `1`, or
!!    `max(10*rhoend, 1)` when only `rhoend` was given, reduced to a quarter of the narrowest width
!!    only when it exceeds half of it. The two part when the narrowest width is between 2 and 4,
!!    and whenever `rhoend` alone is given.
!! 2. `maxfun` is used as given; upstream raises one below `n + 3` to `n + 3`. A budget below `npt`
!!    therefore ends the run with `PF_OPT_LIMIT` after that many evaluations.
!! 3. `npt` is used as given; upstream resets it when `npt >= maxfun`.
!! 4. `rhoend` is silently lowered to a `rhobeg` this driver defaulted below it. An explicit pair
!!    in the wrong order is refused; a defaulted `rhobeg` cannot be, since the caller did not write
!!    it.
!!
!! The `epsilon` floors on the two radii are upstream's own, and this is the one driver of the
!! three that applies them -- `pf_minimize_lincoa` and `pf_minimize_cobyla` say so in their headers.
!!
!! **The one adjustment kept** is PRIMA's `honour_x0` reduction of `rhobeg`. BOBYQA requires the
!! start to be at least `rhobeg` from every inactive bound; upstream can satisfy that by moving
!! the start or by shrinking `rhobeg`, and this library fixes `honour_x0` at `.true.` (Q9), so the
!! start is never moved and `rhobeg` shrinks instead. A coordinate already at a bound stays there.
!! The radius the run ended at comes back in `info%rho`.
submodule (parquet_prima) parquet_prima_bobyqa

    use parquet_prima_common, only : prima_state, refuse_bad_call, finish_run, &
        bounds_in_engine_units
    use parquet_prima_bobyqb, only : bobyqb
    use parquet_prima_linalg, only : is_finite, trueloc, prima_abort, &
        ZERO, ONE, HALF, EPS, ETA1_DFT, ETA2_DFT, GAMMA1_DFT, GAMMA2_DFT, &
        RHOBEG_DFT, RHOEND_DFT, FTARGET_DFT, MAXFUN_DIM_DFT

    implicit none

    !> The generic every abort message here names.
    character(len=*), parameter :: EP = "pf_minimize_bobyqa"

contains

    ! The adapter every driver submodule of this module uses; declared in the spec, implemented
    ! here (`.claude/rules/code-style.md`: a separate module procedure is implemented in exactly
    ! one descendant of the module that declares its interface).
    module procedure prima_func_objective_eval

        f = this%fun(x)

    end procedure prima_func_objective_eval

    module procedure minimize_bobyqa_obj

        integer :: n, npt_use, maxfun_use, prima_info, nf
        real(real64) :: rhobeg_use, rhoend_use, rhobeg_in, ftarget_use, fout
        real(real64), allocatable :: sc(:), lo(:), hi(:), y(:)
        logical, allocatable :: at_lower(:), at_upper(:)
        type(prima_state) :: st

        n = size(x)
        call refuse_bad_call(EP, f, x, allow_constrained=.false., lower=lower, upper=upper, &
                             rhobeg=rhobeg, rhoend=rhoend, npt=npt, scale=scale, &
                             max_neval=max_neval, context=context)

        ! ---- the caller's units to the engine's ------------------------------------------------
        allocate(sc(n))
        sc = ONE
        if (present(scale)) sc = scale
        allocate(y(n))
        y = x / sc
        allocate(lo(n), hi(n))
        ! An absent bound -- and one at or beyond `BOUNDMAX` in the engine's units, which means the
        ! same thing -- is `+/-BOUNDMAX` in the ENGINE's units, never the caller's divided by the
        ! scale: `BOUNDMAX/sc` overflows to +Infinity for any scale below one, and an infinite
        ! bound puts an infinity into `su - sl` inside the engine.
        call bounds_in_engine_units(sc, lo, hi, lower, upper)
        y = max(lo, min(hi, y))

        ! ---- upstream's defaults, for the arguments the caller left out ------------------------
        !
        ! BOBYQA's model needs two DISTINCT points per coordinate inside the bounds, so `rhobeg`
        ! may not exceed half the narrowest width; upstream reduces a larger one to a quarter of
        ! that width and warns. Refused here with every other argument upstream adjusts: from a
        ! start ON a bound the two initial points of that coordinate coincide, and the run ends
        ! after a handful of evaluations, at the start, reporting rounding error. The widths are
        ! the engine's, so `pf_bobyqa_solver` meets the same rule as a `rhobeg_fraction` above
        ! `0.5`, whichever way it scales.
        if (present(rhobeg)) then
            if (rhobeg > HALF * minval(hi - lo)) call abort_here( &
                "rhobeg must not exceed half the narrowest distance between the bounds")
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
        !
        ! No `cstrv`: BOBYQA honours its bounds at every point it evaluates, so a bound violation
        ! is not a thing the caller has to be told about and `PF_OPT_INFEASIBLE` cannot arise.
        call finish_run(prima_info, st, n, info, history)

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

end submodule parquet_prima_bobyqa
