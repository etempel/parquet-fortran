!> PRIMA's shared workers above the linear algebra, and the two procedures this library writes
!> itself: the evaluation of the caller's objective, the exit tests, the trust-region radius and
!> ratio rules, the base-point shift, the bound projection, the one-dimensional grid search, the
!> constrained solvers' filter, and the validation every `parquet_prima` entry point performs.
!!
!! **Provenance.** Derived from PRIMA (Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`: `fortran/common/evaluate.f90`, `checkexit.f90`, `ratio.f90`,
!! `redrho.f90`, `xinbd.f90`, `shiftbase.f90`, `univar.f90` and `selectx.f90`. The licence text and
!! the list of deviations shared by every file of this tier are in `parquet_prima_linalg`, which
!! also carries the kinds, the constants, the exit codes, the IEEE predicates and the abort.
!!
!! **A bug found here is checked against upstream before it is fixed here** -- with three
!! exceptions, `evaluate`, `evaluate_fc` and `refuse_bad_call`, which are this library's own and
!! are described at the procedure.
!!
!! **Why the validation lives here.** `refuse_bad_call` is not PRIMA's: it replaces
!! `preproc.f90`, which adjusts an invalid argument and warns where this refuses it
!! (`feature_optimizer.md` 5.6). It sits in this module rather than in a driver submodule because
!! all three entry points share it -- one set of abort sites, so the error scenarios of 9.4 cover
!! `pf_minimize_lincoa` and `pf_minimize_cobyla` through the same code they cover
!! `pf_minimize_bobyqa` through, and a message cannot drift between the three.
!!
!! **What is not vendored here.** `moderatex`, `moderatef` and `moderatec`: upstream replaces a
!! NaN coordinate by zero and a NaN or huge value by a large finite one and carries on, which is
!! its "moderated extreme barrier". This library refuses instead (`feature_optimizer.md` Q7), so a
!! non-finite value never reaches the model and PRIMA's `NAN_INF_F` cannot arise. `history.f90`:
!! upstream's `xhist`/`fhist` ring buffer with its `PRIMA_MAX_HIST_MEM_MB` cap is replaced by the
!! optional `pf_optimize_history` record, appended at the single point every evaluation passes
!! through -- `evaluate` and `evaluate_fc` -- and bounded by the evaluation budget instead.
!! `shiftbase_qint` is UOBYQA's and is not here (Q15). Upstream's `evaluatefc` is replaced by
!! `evaluate_fc`, which screens rather than moderates.
module parquet_prima_common

    use, intrinsic :: iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
    use parquet_optimize, only : pf_objective, pf_constrained_objective, pf_optimize_history, &
        pf_optimize_info, &
        PF_OPT_OK, PF_OPT_LIMIT, PF_OPT_TARGET, PF_OPT_ROUNDING, PF_OPT_INFEASIBLE
    use parquet_prima_linalg, only : sum, ZERO, HALF, QUART, INFO_DFT, NAN_INF_X, NAN_INF_F, &
        FTARGET_ACHIEVED, MAXFUN_REACHED, TENTH, prima_abort, is_nan, is_inf, is_posinf, &
        is_neginf, is_finite, REALMAX, BOUNDMAX, ONE, TWO, TEN, EPS, inprod, matprod, outprod, &
        trueloc, linspace, int, maximum, FUNCMAX, CONSTRMAX, &
        SMALL_TR_RADIUS, FTARGET_ACHIEVED, MAXFUN_REACHED, MAXTR_REACHED

    implicit none
    private

    public :: prima_state, FUNC_WITH_ARGS
    public :: evaluate, evaluate_fc, checkexit, redrat, redrho, xinbd, shiftbase, interval_max
    public :: savefilt, selectx, isbetter
    public :: refuse_bad_call, caller_violation, finish_run, bounds_in_engine_units

    interface checkexit
        module procedure checkexit, checkexit_con
    end interface checkexit

    interface isbetter
        module procedure isbetter10, isbetter01
    end interface isbetter

    !> Everything a vendored engine needs that is not mathematics.
    !!
    !! One object travels beside the objective through every engine procedure that can evaluate
    !! or abort, in place of upstream's `procedure(OBJ) :: calfun` dummy. It carries the four
    !! things upstream had no room for: the caller's scaling (`feature_optimizer.md` Q26), the
    !! wording an abort must carry, the optional evaluation record, and the running count.
    type :: prima_state
        !> Characteristic magnitude per coordinate. The engines work in `y` and the objective is
        !! evaluated at `scale*y`, so `rhobeg`, `rhoend` and the returned `rho` are in `y` units
        !! while `x` and the record are in the caller's. All ones when the caller gave no `scale`.
        real(real64), allocatable :: scale(:)
        !> The generic the caller named, for an abort message.
        character(len=:), allocatable :: entry_point
        !> The caller's call-site text, unallocated when they gave none.
        character(len=:), allocatable :: context
        !> Whether the caller asked for the evaluation record.
        logical :: want_history = .false.
        !> Every evaluation, in the caller's units, when `want_history`.
        type(pf_optimize_history) :: record
        !> Objective evaluations made, every one counted.
        integer :: neval = 0
        !> The trust-region radius the run ended at, for `info%rho`; in the engine's `y` units.
        real(real64) :: rho = 0.0_real64
        !> Trust-region iterations performed, for `info%niter`.
        integer :: niter = 0
    end type prima_state

    abstract interface
        !> A function of one variable carrying its parameters in a vector, upstream's
        !! `FUNC_WITH_ARGS`; what `interval_max` maximises.
        function FUNC_WITH_ARGS(x, args) result(f)
            import :: real64
            real(real64), intent(in) :: x       !! the variable
            real(real64), intent(in) :: args(:) !! the parameters, whatever the caller packed
            real(real64)             :: f       !! the value
        end function FUNC_WITH_ARGS
    end interface

contains

    !> Evaluates the caller's objective at one point and records it.
    !!
    !! **This procedure is this library's, not upstream's.** PRIMA's `evaluatef` calls a
    !! `procedure(OBJ)` and then applies the moderated extreme barrier: a NaN coordinate becomes
    !! zero, a NaN or huge value becomes `FUNCMAX`, and the search carries on against a value the
    !! objective never returned. This library refuses instead (`feature_optimizer.md` Q7), for the
    !! reason the guide gives: a moderated value is indistinguishable from a real one in the
    !! answer, so a bug in the objective is reported as a minimum.
    !!
    !! The engines work in scaled coordinates `y`; the objective is always evaluated at
    !! `scale*y`, and that is what the record keeps, so a caller reading `history` back never
    !! sees the engine's units.
    !!
    !! Not `pure`: `eval` may update the objective (a counter, a cache), and the abort is an
    !! `error stop`.
    subroutine evaluate(obj, st, y, f)
        class(pf_objective), intent(inout) :: obj  !! the objective (this thread's own)
        type(prima_state), intent(inout)   :: st   !! scaling, wording, record and count
        real(real64), intent(in)           :: y(:) !! the point, in the engine's scaled units
        real(real64), intent(out)          :: f    !! objective value at `scale*y`

        real(real64) :: x(size(y))

        x = st%scale * y
        f = obj%eval(x)
        st%neval = st%neval + 1

        ! Q7: the value never reaches the model unscreened. `is_finite` is false for a NaN and for
        ! either infinity, which is exactly the set 5.6 refuses; testing `f /= f` would see the
        ! NaN and miss the infinities, and the objective call dominates the cost either way.
        if (.not. ieee_is_finite(f)) then
            call state_abort(st, "the objective returned a non-finite value")
        end if

        if (st%want_history) call st%record%append(x, f)

    end subroutine evaluate


    !> Evaluates the caller's constrained objective at one point and records it.
    !!
    !! **This procedure is this library's, not upstream's**, and it is `evaluate`'s sibling:
    !! upstream's `evaluatefc` moderates a NaN or huge value and a NaN coordinate and carries on,
    !! and this refuses (`feature_optimizer.md` Q7). The refusal covers the constraint values as
    !! well as the objective, which is the abort 5.6 words `a constraint returned a non-finite
    !! value` -- without it a NaN constraint would enter COBYLA's filter, where it is compared,
    !! and decide which point is returned.
    !!
    !! `c` is sized by the caller, always `f%n_constraints()` values, and is left untouched when
    !! that count is zero. Only the objective value goes into the record: `pf_optimize_history`
    !! carries `x` and `f` (5.1), and the violation the caller reads is `info%cstrv` at the
    !! returned point.
    subroutine evaluate_fc(obj, st, y, f, c)
        class(pf_constrained_objective), intent(inout) :: obj  !! the objective (this thread's own)
        type(prima_state), intent(inout)               :: st   !! scaling, wording, record and count
        real(real64), intent(in)                       :: y(:) !! the point, in the engine's units
        real(real64), intent(out)                      :: f    !! objective value at `scale*y`
        real(real64), intent(out)                      :: c(:) !! the nonlinear constraints there

        real(real64) :: x(size(y))

        x = st%scale * y
        f = obj%eval(x)
        st%neval = st%neval + 1
        if (.not. ieee_is_finite(f)) then
            call state_abort(st, "the objective returned a non-finite value")
        end if

        if (size(c) > 0) then
            call obj%constraints(x, c)
            if (.not. all(ieee_is_finite(c))) then
                call state_abort(st, "a constraint returned a non-finite value")
            end if
        end if

        if (st%want_history) call st%record%append(x, f)

    end subroutine evaluate_fc

    !> Aborts from inside an engine, carrying the caller's wording.
    !!
    !! The two evaluation procedures differ only in what they screen, and both have to reproduce
    !! the entry point and the optional context exactly; one place to get that right.
    subroutine state_abort(st, text)
        type(prima_state), intent(in) :: st   !! the run's wording
        character(len=*), intent(in)  :: text !! what went wrong

        if (allocated(st%context)) then
            call prima_abort(st%entry_point, text, st%context)
        else
            call prima_abort(st%entry_point, text)
        end if

    end subroutine state_abort

    !> Whether the solver should stop, and why; upstream's `checkexit` for an unconstrained problem.
    !!
    !! Returns `INFO_DFT` to carry on. The `NAN_INF_X` and `NAN_INF_F` arms are upstream's safety
    !! net and cannot fire here: the start is validated finite, the steps are bounded, and a
    !! non-finite value aborts in `evaluate` before it can be tested.
    function checkexit(maxfun, nf, f, ftarget, x) result(info)
        ! Common modules

        integer, intent(in) :: maxfun          !! the evaluation budget
        integer, intent(in) :: nf              !! evaluations made so far
        real(real64), intent(in) :: f          !! the value just computed
        real(real64), intent(in) :: ftarget    !! the caller's target value
        real(real64), intent(in) :: x(:)       !! the point just evaluated

        integer :: info                        !! PRIMA's exit code, `INFO_DFT` to continue

        info = INFO_DFT  ! Default info, indicating that the solver should not exit.

        ! Although X should not contain NaN unless there is a bug, we include the following for security.
        ! X can be Inf, as finite + finite can be Inf numerically.
        if (any(is_nan(x) .or. is_inf(x))) then
            info = NAN_INF_X
        end if

        ! Although NAN_INF_F should not happen unless there is a bug, we include the following for security.
        if (is_nan(f) .or. is_posinf(f)) then
            info = NAN_INF_F
        end if

        if (f <= ftarget) then
            info = FTARGET_ACHIEVED
        end if

        if (nf >= maxfun) then
            info = MAXFUN_REACHED
        end if

    end function checkexit

    !> The reduction ratio of a trust-region step, upstream's `redrat`.
    !!
    !! Handles the infinities and NaNs a ratio of two differences can produce: a NaN numerator, a
    !! non-positive or NaN denominator, and both infinite with either sign.
    function redrat(ared, pred, rshrink) result(ratio)
        real(real64), intent(in) :: ared       !! the actual reduction
        real(real64), intent(in) :: pred       !! the predicted reduction
        real(real64), intent(in) :: rshrink    !! the ratio at or below which the radius shrinks

        real(real64) :: ratio                  !! the reduction ratio, never NaN

        if (is_nan(ared)) then
            ! This should not happen in unconstrained problems due to the moderated extreme barrier.
            ratio = -REALMAX
        elseif (is_nan(pred) .or. pred <= 0) then
            ! The trust-region subproblem solver fails in this rare case. Instead of terminating as Powell's
            ! original code does, we set RATIO as follows so that the solver may continue to progress.
            if (ared > 0) then
                ! The trial point will be accepted, but the trust-region radius will be shrunk if RSHRINK>0.
                ratio = HALF * rshrink
            else
                ! Set ratio to a large negative number to signify a bad trust-region step, so that the
                ! solver will check whether to take a geometry step or reduce RHO.
                ratio = -REALMAX
            end if
        elseif (is_posinf(pred) .and. is_posinf(ared)) then
            ratio = ONE  ! ARED/PRED = NaN if calculated directly.
        elseif (is_posinf(pred) .and. is_neginf(ared)) then
            ratio = -REALMAX  ! ARED/PRED = NaN if calculated directly.
        else
            ratio = ared / pred
        end if

    end function redrat

    !> The reduced trust-region radius, upstream's `redrho`.
    !!
    !! The scheme NEWUOA, BOBYQA and LINCOA share: a tenth while far from the end, the end itself
    !! once close, and the geometric mean between.
    function redrho(rho_in, rhoend) result(rho)
        real(real64), intent(in) :: rho_in    !! the current radius
        real(real64), intent(in) :: rhoend    !! the final radius asked for

        real(real64) :: rho                   !! the reduced radius, above `rhoend` until it reaches it

        real(real64) :: rho_ratio

        rho_ratio = rho_in / rhoend

        if (rho_ratio > 250.0_real64) then
            rho = TENTH * rho_in
        else if (rho_ratio <= 16.0_real64) then
            rho = rhoend
        else
            rho = sqrt(rho_ratio) * rhoend  !rho = sqrt(rho_in * rhoend)
        end if

    end function redrho

    !> `xbase + step`, projected into the bounds, upstream's `xinbd`.
    !!
    !! `sl` and `su` are the bounds relative to `xbase`. A step that reaches a relative bound puts
    !! the coordinate exactly on the absolute bound, so a bound-active point is bit-exact rather
    !! than within rounding of the bound.
    function xinbd(xbase, step, xl, xu, sl, su) result(x)
        ! Common modules

        real(real64), intent(in) :: xbase(:)    !! the base point, inside the bounds
        real(real64), intent(in) :: step(:)     !! the step from `xbase`
        real(real64), intent(in) :: xl(:)       !! lower bounds
        real(real64), intent(in) :: xu(:)       !! upper bounds
        real(real64), intent(in) :: sl(:)       !! `xl - xbase`
        real(real64), intent(in) :: su(:)       !! `xu - xbase`

        real(real64) :: x(size(xbase))          !! the projected point

        integer :: n
        real(real64) :: s(size(xbase))

        n = int(size(xbase))

        s = max(sl, min(su, step))
        x = max(xl, min(xu, xbase + s))
        x(trueloc(s <= sl)) = xl(trueloc(s <= sl))
        x(trueloc(s >= su)) = xu(trueloc(s >= su))

    end function xinbd

    !> Shifts the base point to `xbase + xopt` and updates the model, upstream's `shiftbase`.
    !!
    !! Section 7 of the NEWUOA paper. `pq` and `zmat` are unchanged by the shift; `bmat` and `hq`
    !! are not.
    subroutine shiftbase(kopt, xbase, xpt, zmat, bmat, pq, hq, idz)
        ! Common modules

        integer, intent(in) :: kopt                  !! which interpolation point becomes the new base
        real(real64), intent(in) :: pq(:)            !! the model's implicit Hessian weights
        real(real64), intent(in) :: zmat(:, :)       !! the factorisation's Z
        integer, intent(in), optional :: idz         !! the factorisation's sign split; absent in BOBYQA, which is `idz = 1`

        real(real64), intent(inout) :: bmat(:, :)    !! the factorisation's B, updated in place
        real(real64), intent(inout) :: hq(:, :)      !! the model's explicit Hessian, updated
        real(real64), intent(inout) :: xbase(:)      !! the base point, shifted in place
        real(real64), intent(inout) :: xpt(:, :)     !! the interpolation set, shifted in place

        integer :: idz_loc
        integer :: k
        integer :: n
        integer :: npt
        real(real64) :: bymat(size(xbase), size(xbase))
        !real(real64) :: htol
        real(real64) :: qxoptq
        real(real64) :: sxpt(size(xpt, 2))
        real(real64) :: v(size(xbase))
        real(real64) :: vxopt(size(xbase), size(xbase))
        real(real64) :: xopt(size(xbase))
        real(real64) :: xoptsq
        real(real64) :: xptxav(size(xpt, 1), size(xpt, 2))
        real(real64) :: ymat(size(xpt, 1), size(xpt, 2))
        real(real64) :: yzmat(size(xbase), size(zmat, 2))
        real(real64) :: yzmat_c(size(xbase), size(zmat, 2))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Read IDZ, which is absent from BOBYQA, being equivalent to IDZ = 1.
        idz_loc = 1
        if (present(idz)) then
            idz_loc = idz
        end if

        ! Read XOPT.
        xopt = xpt(:, kopt)
        xoptsq = inprod(xopt, xopt)

        ! Update BMAT. See (7.11)--(7.12) of the NEWUOA paper and the elaborations around.
        ! XPTXAV corresponds to XPT - XAV in the NEWUOA paper, with XAV = (X0 + XOPT)/2.
        xptxav = xpt - HALF * spread(xopt, dim=2, ncopies=npt)
        ! MATLAB: xptxav = xpt - xopt/2  % xopt should be a column! Implicit expansion
        !sxpt = matprod(xopt, xptxav)
        sxpt = matprod(xopt, xpt) - HALF * xoptsq  ! This one seems to work better numerically.

        ! First, make the changes to BMAT that do not depend on ZMAT.
        qxoptq = QUART * xoptsq
        do k = 1, npt
            ymat(:, k) = sxpt(k) * xptxav(:, k) + qxoptq * xopt
        end do
        ! MATLAB: ymat = xptxav .* sxpt + qxoptq * xopt  % sxpt should be a row, xopt should be a column
        !ymat(:, kopt) = HALF * xoptsq * xopt ! This makes no difference according to a test on 20220406
        bymat = matprod(bmat(:, 1:npt), transpose(ymat))  ! BMAT(:, 1:NPT) is not updated yet.
        bmat(:, npt + 1:npt + n) = bmat(:, npt + 1:npt + n) + (bymat + transpose(bymat))
        ! Then the revisions of BMAT that depend on ZMAT are calculated.
        yzmat = matprod(ymat, zmat)
        yzmat_c = yzmat
        yzmat_c(:, 1:idz_loc - 1) = -yzmat(:, 1:idz_loc - 1)  ! IDZ_LOC is usually small. So this assignment is cheap.
        bmat(:, npt + 1:npt + n) = bmat(:, npt + 1:npt + n) + matprod(yzmat, transpose(yzmat_c))
        bmat(:, 1:npt) = bmat(:, 1:npt) + matprod(yzmat_c, transpose(zmat))

        ! Update the quadratic model. Note that PQ remains unchanged. For HQ, see (7.14) of the NEWUOA paper.
        !v = matprod(xptxav, pq)  ! Vector V in (7.14) of the NEWUOA paper
        v = matprod(xpt, pq) - HALF * sum(pq) * xopt ! This one seems to work better numerically.
        vxopt = outprod(v, xopt)  ! MATLAB: vxopt = v * xopt';  % v and xopt should be both columns
        hq = (vxopt + transpose(vxopt)) + hq !call r2update(hq, ONE, xopt, v)
        !call symmetrize(hq)  ! Do this if the update above does not ensure symmetry.

        ! The following instructions complete the shift of XBASE.
        xbase = xbase + xopt
        xpt = xpt - spread(xopt, dim=2, ncopies=npt)
        xpt(:, kopt) = ZERO
        ! MATLAB: xpt = xpt - xopt; xpt(:, kopt) = 0;  % xopt should be a column! Implicit expansion

    end subroutine shiftbase

    !> An approximate maximiser of a function of one variable on an interval, upstream's
    !> `interval_max`.
    !!
    !! Evaluates on an even grid, takes the best point and refines it by the maximiser of the
    !! quadratic through that point and its two neighbours, unless the best point is an end.
    function interval_max(fun, lb, ub, args, grid_size) result(x)
        procedure(FUNC_WITH_ARGS) :: fun
        real(real64), intent(in) :: lb         !! lower end of the interval
        real(real64), intent(in) :: ub         !! upper end of the interval
        real(real64), intent(in) :: args(:)    !! parameters passed through to `fun`
        integer, intent(in) :: grid_size       !! how many grid points

        real(real64) :: x                      !! the approximate maximiser

        integer :: k
        integer :: kopt
        real(real64) :: fgrid(grid_size)
        real(real64) :: fopt
        real(real64) :: fnext
        real(real64) :: fprev
        real(real64) :: step
        real(real64) :: xgrid(grid_size)

        if (ub <= lb) then
            x = lb
            return
        end if

        xgrid = linspace(lb, ub, grid_size)
        fgrid = [(fun(xgrid(k), args), k=1, grid_size)]
        ! MATLAB: fgrid = arrayfun(@(x) fun(x, args), xgrid(1:grid_size));  % Same shape as `xgrid`

        if (all(is_nan(fgrid))) then
            x = lb
            return
        end if

        kopt = int(maxloc(fgrid, mask=(.not. is_nan(fgrid)), dim=1))
        fopt = fgrid(kopt)
        ! MATLAB: [fopt, kopt] = min(fgrid, [], 'omitnan');

        if (kopt == 1) then
            x = lb
        elseif (kopt == grid_size) then
            x = ub
        else
            fprev = fgrid(kopt - 1)
            fnext = fgrid(kopt + 1)
            step = ZERO
            if (abs(fprev - fnext) > 0) then
                step = HALF * ((fnext - fprev) / (fopt + fopt - fprev - fnext))
            end if
            if (is_finite(step) .and. abs(step) > 0) then
                x = lb + (ub - lb) * (real(kopt - 1, real64) + step) / real(grid_size - 1, real64)
                ! N.B.: 1. XGRID(KOPT) = LB + (UB-LB)*(KOPT - 1)/(GRID_SIZE -1)
                ! 2. XGRID(KOPT-1) <= X <= XGRID(KOPT+1), as X maximizes the quadratic interpolant.
            else
                x = xgrid(kopt)
            end if
        end if

    end function interval_max

    ! ---- What LINCOA and COBYLA add: the constrained exit test and Powell's filter -------------
    !
    ! Everything above is what BOBYQA reaches (`feature_optimizer.md` 12.1 allows a phase to extend
    ! what an earlier one built).

    !> Whether the solver should stop, and why; upstream's `checkexit` for a constrained problem.
    !!
    !! `checkexit`'s constrained specific. It differs from the unconstrained one in the target test
    !! alone: a value at or below `ftarget` stops the run only at a point that is feasible to
    !! `ctol`, so a target cannot be claimed from outside the feasible region.
    !!
    !! The `NAN_INF_X` and `NAN_INF_F` arms are upstream's safety net and cannot fire here: the
    !! start is validated finite, the steps are bounded, and a non-finite value or constraint aborts
    !! in `evaluate_fc` before it can be tested.
    function checkexit_con(maxfun, nf, cstrv, ctol, f, ftarget, x) result(info)
        ! Common modules

        integer, intent(in) :: maxfun          !! the evaluation budget
        integer, intent(in) :: nf              !! evaluations made so far
        real(real64), intent(in) :: cstrv      !! the constraint violation at the point just evaluated
        real(real64), intent(in) :: ctol       !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: f          !! the value just computed
        real(real64), intent(in) :: ftarget    !! the caller's target value
        real(real64), intent(in) :: x(:)       !! the point just evaluated

        integer :: info                        !! PRIMA's exit code, `INFO_DFT` to continue

        info = INFO_DFT   ! Default info, indicating that the solver should not exit.

        ! Although X should not contain NaN unless there is a bug, we include the following for security.
        ! X can be Inf, as finite + finite can be Inf numerically.
        if (any(is_nan(x) .or. is_inf(x))) then
            info = NAN_INF_X
        end if

        ! Although NAN_INF_F should not happen unless there is a bug, we include the following for security.
        if (is_nan(f) .or. is_posinf(f) .or. is_nan(cstrv) .or. is_posinf(cstrv)) then
            info = NAN_INF_F
        end if

        if (cstrv <= ctol .and. f <= ftarget) then
            info = FTARGET_ACHIEVED
        end if

        if (nf >= maxfun) then
            info = MAXFUN_REACHED
        end if

    end function checkexit_con

    !> Offers one point to the filter of nondominated `(f, cstrv)` pairs; upstream's `savefilt`.
    !!
    !! The filter is what LINCOA and COBYLA return from: a run that ends infeasible must still hand
    !! back the best point it saw, and "best" on a constrained problem is not a single number. A
    !! pair is kept when no pair already in the filter is better in both value and violation, and
    !! every pair the new one dominates is dropped -- so the filter holds only the trade-off
    !! frontier.
    !!
    !! When it is full the pair with the largest `cweight*cstrv + f` goes, which is the only place
    !! `cweight` is used: it says how many units of value one unit of violation is worth, and
    !! PRIMA's default `1e8` makes feasibility dominate.
    !!
    !! A point whose value or violation is a NaN is not offered by this tier -- `evaluate_fc` aborts
    !! first -- so upstream's screen below is unreachable and kept only as upstream has it.
    subroutine savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt, constr, confilt)
        real(real64), intent(in) :: cstrv                         !! the point's constraint violation
        real(real64), intent(in) :: ctol                          !! the feasibility tolerance
        real(real64), intent(in) :: cweight  !! how heavily violation weighs against value when the filter is full
        real(real64), intent(in) :: f                             !! the point's objective value
        real(real64), intent(in) :: x(:)                          !! the point
        real(real64), intent(in), optional :: constr(:)  !! the constraint values at `x`; COBYLA keeps them, LINCOA does not

        integer, intent(inout) :: nfilt                           !! how many pairs the filter holds, updated
        real(real64), intent(inout) :: cfilt(:)                   !! the filter's violations, updated
        real(real64), intent(inout) :: ffilt(:)                   !! the filter's values, updated
        real(real64), intent(inout) :: xfilt(:, :)                !! the filter's points, one per column, updated
        real(real64), intent(inout), optional :: confilt(:, :)    !! the filter's constraint values, updated when `constr` is given

        integer :: index_to_keep(size(ffilt))
        integer :: kworst
        integer :: m
        integer :: maxfilt
        integer :: n
        logical :: keep(nfilt)
        real(real64) :: cfilt_shifted(size(ffilt))
        real(real64) :: cref
        real(real64) :: fref
        real(real64) :: phi(size(ffilt))
        real(real64) :: phimax

        if (present(constr)) then
            m = int(size(constr))
        else
            m = 0
        end if
        n = int(size(x))
        maxfilt = int(size(ffilt))

        ! Return immediately if any column of XFILT is better than X. Note that ISBETTER checks "strictly
        ! better", handling NaN/Inf properly, but we need "non-strictly better" here, allowing equality.
        ! This is why we need to supplement ISBETTER with (FFILT <= F .AND. CFILT <= CSTRV).
        if (any(isbetter(ffilt(1:nfilt), cfilt(1:nfilt), f, cstrv, ctol)) .or. &
            & any(ffilt(1:nfilt) <= f .and. cfilt(1:nfilt) <= cstrv)) then
            return
        end if

        ! Decide which columns of XFILT to keep.
        keep = (.not. isbetter(f, cstrv, ffilt(1:nfilt), cfilt(1:nfilt), ctol))

        ! If NFILT == MAXFILT and X is not better than any column of XFILT, then we remove the worst column
        ! of XFILT according to the merit function PHI = FFILT + CWEIGHT * MAX(CFILT - CTOL, ZERO).
        if (count(keep) == maxfilt) then  ! In this case, NFILT = SIZE(KEEP) = COUNT(KEEP) = MAXFILT > 0.
            cfilt_shifted = max(cfilt - ctol, ZERO)
            if (cweight <= 0) then
                phi = ffilt
            elseif (is_posinf(cweight)) then
                phi = cfilt_shifted
                ! We should not use CFILT here; if MAX(CFILT_SHIFTED) is attained at multiple indices, then
                ! we will check FFILT to exhaust the remaining degree of freedom.
            else
                phi = max(ffilt, -REALMAX) + cweight * cfilt_shifted
                ! MAX(FFILT, -REALMAX) makes sure that PHI will not contain NaN (unless there is a bug).
            end if
            ! We select X to maximize PHI. In case there are multiple maximizers, we take the one with the
            ! largest CSTRV_SHIFTED; if there are more than one choices, we take the one with the largest F;
            ! if there are several candidates, we take the one with the largest CSTRV; if the last comparison
            ! still leads to more than one possibilities, then they are equally bad and we choose the first.
            ! N.B.:
            ! 1. This process is the opposite of selecting KOPT in SELECTX.
            ! 2. In finite-precision arithmetic, PHI_1 == PHI_2 and CSTRV_SHIFTED_1 == CSTRV_SHIFTED_2 do
            ! not ensure that F_1 == F_2!
            phimax = maxval(phi)
            cref = maxval(cfilt_shifted, mask=(phi >= phimax))
            fref = maxval(ffilt, mask=(cfilt_shifted >= cref))
            kworst = int(maxloc(cfilt, mask=(ffilt >= fref), dim=1))
            ! MATLAB: cmax = max(cfilt(ffilt >= fref)); kworst = find(ffilt >= fref & ~(cfilt < cmax), 1,'first');
            if (kworst < 1 .or. kworst > size(keep)) then  ! For security. Should not happen.
                kworst = 1
            end if
            keep(kworst) = .false.
        end if

        nfilt = int(count(keep))
        index_to_keep(1:nfilt) = trueloc(keep)
        xfilt(:, 1:nfilt) = xfilt(:, index_to_keep(1:nfilt))
        ffilt(1:nfilt) = ffilt(index_to_keep(1:nfilt))
        cfilt(1:nfilt) = cfilt(index_to_keep(1:nfilt))
        if (present(confilt) .and. present(constr)) then
            confilt(:, 1:nfilt) = confilt(:, index_to_keep(1:nfilt))
        end if

        nfilt = nfilt + 1
        xfilt(:, nfilt) = x
        ffilt(nfilt) = f
        cfilt(nfilt) = cstrv
        if (present(confilt) .and. present(constr)) then
            confilt(:, nfilt) = constr
        end if

    end subroutine savefilt

    !> Which point of a filter to return; upstream's `selectx`.
    !!
    !! The rule Powell's constrained solvers end on. Violations are shifted by `ctol` so that
    !! anything feasible counts as zero, and the point minimising `f + cweight*cstrv` is taken, with
    !! the reference values chosen so that a history of nothing but moderated or infinite values
    !! still selects something. Ties go to the lowest violation, then to the lowest value, then to
    !! the earliest index -- so the answer does not depend on the order the filter happens to be in.
    function selectx(fhist, chist, cweight, ctol) result(kopt)
        real(real64), intent(in) :: cweight     !! how heavily violation weighs against value
        real(real64), intent(in) :: chist(:)    !! their violations
        real(real64), intent(in) :: ctol        !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: fhist(:)    !! the values to choose among, normally a filter rather than a history

        integer :: kopt                         !! the index chosen

        integer :: nhist
        real(real64) :: chist_shifted(size(fhist))
        real(real64) :: cmin
        real(real64) :: cref
        real(real64) :: fref
        real(real64) :: phi(size(fhist))
        real(real64) :: phimin

        nhist = int(size(fhist))

        ! We select X among the points with F < FREF and CSTRV < CREF.
        ! Do NOT use F <= FREF, because F == FREF (FUNCMAX or REALMAX) may mean F == INF in practice!
        if (any(fhist < FUNCMAX .and. chist < CONSTRMAX)) then
            fref = FUNCMAX
            cref = CONSTRMAX
        elseif (any(fhist < REALMAX .and. chist < CONSTRMAX)) then
            fref = REALMAX
            cref = CONSTRMAX
        elseif (any(fhist < FUNCMAX .and. chist < REALMAX)) then
            fref = FUNCMAX
            cref = REALMAX
        else
            fref = REALMAX
            cref = REALMAX
        end if

        if (.not. any(fhist < fref .and. chist < cref)) then
            kopt = nhist
        else
            ! Shift the constraint violations by CTOL, so that CSTRV <= CTOL is regarded as no violation.
            chist_shifted = max(chist - ctol, ZERO)
            ! CMIN is the minimal shifted constraint violation attained in the history.
            cmin = minval(chist_shifted, mask=(fhist < fref))
            ! We consider only the points whose shifted constraint violations are at most the CREF below.
            ! N.B.: Without taking MAX(EPS, .), CREF would be 0 if CMIN = 0. In that case, asking for
            ! CSTRV_SHIFTED < CREF would be WRONG!
            cref = max(EPS, TWO * cmin)
            ! We use the following PHI as our merit function to select X.
            if (cweight <= 0) then
                phi = fhist
            elseif (is_posinf(cweight)) then
                phi = chist_shifted
                ! We should not use CHIST here; if MIN(CHIST_SHIFTED) is attained at multiple indices, then
                ! we will check FHIST to exhaust the remaining degree of freedom.
            else
                phi = max(fhist, -REALMAX) + cweight * chist_shifted
                ! MAX(FHIST, -REALMAX) makes sure that PHI will not contain NaN (unless there is a bug).
            end if
            ! We select X to minimize PHI subject to F < FREF and CSTRV_SHIFTED <= CREF (see the comments
            ! above for the reason of taking "<" and "<=" in these two constraints). In case there are
            ! multiple minimizers, we take the one with the least CSTRV_SHIFTED; if there are more than one
            ! choices, we take the one with the least F; if there are several candidates, we take the one
            ! with the least CSTRV; if the last comparison still leads to more than one possibilities, then
            ! they are equally good and we choose the first.
            ! N.B.:
            ! 1. This process is the opposite of selecting KWORST in SAVEFILT.
            ! 2. In finite-precision arithmetic, PHI_1 == PHI_2 and CSTRV_SHIFTED_1 == CSTRV_SHIFTED_2 do
            ! not ensure that F_1 == F_2!
            phimin = minval(phi, mask=(fhist < fref .and. chist_shifted <= cref))
            cref = minval(chist_shifted, mask=(fhist < fref .and. phi <= phimin))
            fref = minval(fhist, mask=(chist_shifted <= cref))
            kopt = int(minloc(chist, mask=(fhist <= fref), dim=1))
            ! MATLAB: cmin = min(chist(fhist <= fref)); kopt = find(fhist <= fref & ~(chist > cmin), 1,'first');
        end if

    end function selectx

    !> Whether `(f1, c1)` is strictly better than `(f2, c2)`; upstream's `isbetter00`.
    !!
    !! Better means: not worse in either coordinate and better in one, with everything feasible to
    !! `ctol` treated as equally feasible, and a NaN in either coordinate never better.
    function isbetter00(f1, c1, f2, c2, ctol) result(is_better)
        real(real64), intent(in) :: f1      !! the first value
        real(real64), intent(in) :: c1      !! the first violation
        real(real64), intent(in) :: f2      !! the second value
        real(real64), intent(in) :: c2      !! the second violation
        real(real64), intent(in) :: ctol    !! the feasibility tolerance

        logical :: is_better                !! whether the first pair dominates the second

        real(real64) :: cref

        is_better = .false.
        ! Even though NaN/+Inf should not occur in FC1 or FC2 due to the moderated extreme barrier, for
        ! security and robustness, the code below does not make this assumption.
        is_better = is_better .or. (any(is_nan([f2, c2]) .or. is_posinf([f2, c2])) .and. .not. &
            & any(is_nan([f1, c1]) .or. is_posinf([f1, c1])))
        is_better = is_better .or. (f1 < f2 .and. c1 <= c2)
        is_better = is_better .or. (f1 <= f2 .and. c1 < c2)
        ! If C1 <= CTOL and C2 is significantly larger/worse than CTOL, i.e., C2 > MAX(CTOL, CREF),
        ! then FC1 is better than FC2 as long as F1 < REALMAX. Normally CREF >= CTOL so MAX(CTOL, CREF)
        ! is indeed CREF. However, this may not be true if CTOL > 1E-1*CONSTRMAX.
        cref = TEN * max(EPS, min(ctol, 1.0E-2_real64 * CONSTRMAX))  ! The MIN avoids overflow.
        is_better = is_better .or. (f1 < REALMAX .and. c1 <= ctol .and. (c2 > max(ctol, cref) .or. is_nan(c2)))

    end function isbetter00

    !> `isbetter00` of each of many pairs against one; upstream's `isbetter10`.
    function isbetter10(f1, c1, f2, c2, ctol) result(is_better)
        real(real64), intent(in) :: f1(:)       !! the values
        real(real64), intent(in) :: c1(:)       !! their violations
        real(real64), intent(in) :: f2          !! the value compared against
        real(real64), intent(in) :: c2          !! the violation compared against
        real(real64), intent(in) :: ctol        !! the feasibility tolerance

        logical, allocatable :: is_better(:)    !! element by element, whether the first pair dominates the second

        integer :: i
        integer :: nfc

        nfc = int(size(f1))

        allocate(is_better(int(nfc, int64)))
        is_better = [(isbetter00(f1(i), c1(i), f2, c2, ctol), i=1, nfc)]

    end function isbetter10

    !> `isbetter00` of one pair against each of many; upstream's `isbetter01`.
    function isbetter01(f1, c1, f2, c2, ctol) result(is_better)
        real(real64), intent(in) :: f1          !! the value
        real(real64), intent(in) :: c1          !! its violation
        real(real64), intent(in) :: f2(:)       !! the values compared against
        real(real64), intent(in) :: c2(:)       !! their violations
        real(real64), intent(in) :: ctol        !! the feasibility tolerance

        logical, allocatable :: is_better(:)    !! element by element, whether the first pair dominates the second

        integer :: i
        integer :: nfc

        nfc = int(size(f2))

        allocate(is_better(int(nfc, int64)))
        is_better = [(isbetter00(f1, c1, f2(i), c2(i), ctol), i=1, nfc)]

    end function isbetter01

    ! ---- the bounds, in the units the engines work in -------------------------------------------

    !> The caller's bounds in the engine's units: `bound/scale`, or `+/-BOUNDMAX` where absent.
    !!
    !! **This procedure is this library's, not upstream's**, and it is where upstream's rule that a
    !! bound at or beyond `BOUNDMAX` is no bound at all is applied -- before any arithmetic on the
    !! bound rather than after. Two overflows live at that arithmetic: `upper - lower` for a box of
    !! `+/-huge()`, and `bound/scale` for a large bound and a scale below one, the case each
    !! driver's own comment guards against for ABSENT bounds only. Both deliver the infinity that
    !! was meant, so gfortran and ifx look correct while a build that halts on the flag stops on a
    !! documented call: a `huge()` sentinel is a NUMBER, and centring, scaling or differencing it
    !! overflows.
    !!
    !! A NaN bound never reaches here: `refuse_bad_call` refuses one, which is where this library
    !! parts from upstream, and it runs before any driver forms these.
    subroutine bounds_in_engine_units(sc, lo, hi, lower, upper)
        real(real64), intent(in)           :: sc(:)    !! the per-coordinate scale, finite and positive
        real(real64), intent(out)          :: lo(:)    !! lower bounds as the engine will see them
        real(real64), intent(out)          :: hi(:)    !! upper bounds as the engine will see them
        real(real64), intent(in), optional :: lower(:) !! the caller's bounds; absent is none
        real(real64), intent(in), optional :: upper(:) !! the caller's bounds; absent is none

        integer :: j

        lo = -BOUNDMAX
        hi = BOUNDMAX
        do j = 1, size(sc)
            if (present(lower)) then
                if (.not. bound_is_absent(lower(j), sc(j))) lo(j) = lower(j) / sc(j)
            end if
            if (present(upper)) then
                if (.not. bound_is_absent(upper(j), sc(j))) hi(j) = upper(j) / sc(j)
            end if
        end do

    end subroutine bounds_in_engine_units

    !> Whether one bound is "no bound" once divided by its scale, decided without dividing.
    !!
    !! `abs(b) >= BOUNDMAX*min(sc, 1)` is the whole test: at a scale of one or more it reads
    !! `abs(b) >= BOUNDMAX`, since dividing can only shrink the bound, and below one it is `b/sc`
    !! compared without the division. Where the answer is `.false.` the quotient is below `BOUNDMAX`
    !! in magnitude and therefore finite.
    !!
    !! **The product is formed from `min(sc, ONE)` rather than guarded by `sc < ONE`**, because a
    !! guard does not keep an optimiser from forming what it guards: nagfor's `--profile release`
    !! computed `BOUNDMAX*sc` ahead of the `sc < ONE` test and trapped on the overflow for any scale
    !! above four (`.claude/rules/fortran-gotchas.md`). Here no evaluation order can overflow, and
    !! `min` is quiet because `sc` is finite.
    pure function bound_is_absent(b, sc) result(absent)
        real(real64), intent(in) :: b      !! one of the caller's bounds, not a NaN
        real(real64), intent(in) :: sc     !! that coordinate's scale, finite and positive
        logical                  :: absent !! `b/sc` is at or beyond `BOUNDMAX`

        absent = abs(b) >= BOUNDMAX * min(sc, ONE)

    end function bound_is_absent

    !> One bound held inside `[-BOUNDMAX, BOUNDMAX]`, so that a difference of two cannot overflow.
    !!
    !! Used by `refuse_bad_call` for its width test, and only after its own NaN refusal: `min` and
    !! `max` compile to instructions that raise IEEE invalid on a NaN operand.
    elemental function clamp_to_boundmax(v) result(w)
        real(real64), intent(in) :: v !! one of the caller's bounds, not a NaN
        real(real64)             :: w !! `v`, or the sentinel it is at or beyond

        w = min(BOUNDMAX, max(-BOUNDMAX, v))

    end function clamp_to_boundmax

    ! ---- the validation that replaces PRIMA's preproc.f90 --------------------------------------

    !> Refuses every call `feature_optimizer.md` 5.6 says is refused, before anything is allocated.
    !!
    !! **This procedure is this library's, not upstream's.** It stands in for `preproc.f90`, which
    !! reads an invalid argument, substitutes a default and warns: `rhobeg` and `rhoend` swapped if
    !! reversed, `npt` clamped into range, `ctol` reset if negative, a bound wider than `BOUNDMAX`
    !! treated as absent. There is no channel for a warning in this tier, and a silently adjusted
    !! argument is how a caller comes to believe they asked for something they did not -- so every
    !! one of those is an abort here, with the exact text 5.6 gives.
    !!
    !! **One procedure for all three entry points**, which is what makes the error scenarios of
    !! 9.4 cover `pf_minimize_lincoa` and `pf_minimize_cobyla`: the site a scenario exercises is
    !! this one whichever generic the caller named, and the message it asserts cannot drift
    !! between the three. Arguments no engine of the three takes are simply absent --
    !! `pf_minimize_cobyla` passes no `npt`, `pf_minimize_bobyqa` no `ctol` or constraint arrays.
    !!
    !! Impure by nature: it exists to abort (`api-conventions.md`), and ifx deletes a `pure`
    !! guard-only subroutine at `-O0`.
    subroutine refuse_bad_call(ep, f, x, allow_constrained, lower, upper, rhobeg, rhoend, npt, &
                               scale, ctol, a_ineq, b_ineq, a_eq, b_eq, max_neval, context)
        character(len=*), intent(in)           :: ep         !! the generic the caller named
        class(pf_objective), intent(in)        :: f          !! the caller's objective
        real(real64), intent(in)               :: x(:)       !! the start point
        logical, intent(in)                    :: allow_constrained !! only `pf_minimize_cobyla`
        real(real64), intent(in), optional     :: lower(:)   !! bounds
        real(real64), intent(in), optional     :: upper(:)   !! bounds
        real(real64), intent(in), optional     :: rhobeg     !! initial radius
        real(real64), intent(in), optional     :: rhoend     !! final radius
        integer, intent(in), optional          :: npt        !! interpolation points
        real(real64), intent(in), optional     :: scale(:)   !! per-coordinate scale
        real(real64), intent(in), optional     :: ctol       !! feasibility tolerance
        real(real64), intent(in), optional     :: a_ineq(:, :) !! `a_ineq*x <= b_ineq`
        real(real64), intent(in), optional     :: b_ineq(:)  !! `a_ineq*x <= b_ineq`
        real(real64), intent(in), optional     :: a_eq(:, :) !! `a_eq*x = b_eq`
        real(real64), intent(in), optional     :: b_eq(:)    !! `a_eq*x = b_eq`
        integer, intent(in), optional          :: max_neval  !! evaluation budget
        character(len=*), intent(in), optional :: context    !! call-site text

        !> One text for every way the pair of radii can be wrong, as 5.6 specifies it.
        character(len=*), parameter :: RADII_TEXT = &
            "rhobeg and rhoend must be finite and positive with rhoend <= rhobeg"

        real(real64) :: rhobeg_test, rhoend_test
        integer :: n, npt_max

        n = size(x)

        ! pf_constrained_objective EXTENDS pf_objective, so the language accepts one at every
        ! entry point. An engine that cannot read its constraints would minimise it without them
        ! and return a confident answer from the wrong region.
        !
        ! WHAT THIS FORBIDS: a new entry point in this tier that passes `allow_constrained` true
        ! without reading the constraints. Only `pf_minimize_cobyla` may, and only because it
        ! does. The scenarios `prima_bobyqa_constraints_not_honoured` and
        ! `prima_lincoa_constraints_not_honoured` are what hold it here.
        if (.not. allow_constrained) then
            select type (f)
            class is (pf_constrained_objective)
                call abort_here("this engine does not honour nonlinear constraints; " // &
                    "use pf_minimize_cobyla")
            end select
        end if

        if (n < 1) call abort_here("at least one variable is required")
        if (any(is_nan(x))) call abort_here("the start point must not contain NaN")
        ! An infinity in the start is refused for the same reason the NaN is: the driver would
        ! clamp it to `BOUNDMAX`, evaluate the objective at a number of order `1e307`, and report a
        ! value that overflows there as the objective's fault.
        if (.not. all(is_finite(x))) call abort_here("the start point must be finite")

        if (present(scale)) then
            if (size(scale) /= n) call abort_here("scale and x must have the same size")
            if (.not. all(is_finite(scale))) call abort_here("scale must be finite and positive")
            if (any(scale <= ZERO)) call abort_here("scale must be finite and positive")
        end if

        if (present(lower)) then
            if (size(lower) /= n) call abort_here("lower, upper and x must have the same size")
            ! Upstream treats a NaN bound as an absent one. Refused here, with every other
            ! silently-adjusted argument: a bound the caller wrote and the engine dropped is a
            ! different problem from the one they posed, and each engine fails differently on it --
            ! BOBYQA and LINCOA carry the NaN into the objective and then blame the objective for
            ! the value it gives back, while COBYLA drops the bound and reports `info%cstrv` as a
            ! NaN beside `PF_OPT_OK`, since `cstrv > ctol` is false for a NaN.
            if (any(is_nan(lower))) call abort_here("the bounds must not contain NaN")
        end if
        if (present(upper)) then
            if (size(upper) /= n) call abort_here("lower, upper and x must have the same size")
            if (any(is_nan(upper))) call abort_here("the bounds must not contain NaN")
        end if
        if (present(lower) .and. present(upper)) then
            ! Measured on the bounds AS THE ENGINE WILL SEE THEM: a magnitude at or beyond
            ! `BOUNDMAX` is "no bound" (`bound_in_engine_units`), so `+/-huge()` is a box of width
            ! `REALMAX/2` here rather than the overflow that forming `upper - lower` on the
            ! caller's own numbers would be -- an overflow that ends a build halting on it, on
            ! input this procedure exists to pass.
            if (any(clamp_to_boundmax(upper) - clamp_to_boundmax(lower) <= TWO * EPS)) &
                call abort_here("every upper bound must exceed its lower bound by more than 2*epsilon")
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

        if (present(ctol)) then
            if (.not. is_finite(ctol)) call abort_here( &
                "ctol must be a finite, non-negative number")
            if (ctol < ZERO) call abort_here("ctol must be a finite, non-negative number")
        end if

        call check_linear(a_ineq, b_ineq, "a_ineq", "b_ineq")
        call check_linear(a_eq, b_eq, "a_eq", "b_eq")

        ! A binding that answers a negative count would make `allocate(c(m))` fail or silently
        ! produce a zero-length array, and the caller would never learn which.
        select type (f)
        class is (pf_constrained_objective)
            if (f%n_constraints() < 0) call abort_here("n_constraints must be non-negative")
        end select

        if (present(max_neval)) then
            if (max_neval < 1) call abort_here("max_neval must be positive")
            if (max_neval > huge(1) / 2) call abort_here("max_neval must not exceed huge(1)/2")
        end if

    contains

        !> Refuses a malformed linear-constraint pair, whichever of the two the caller gave.
        !!
        !! One matrix without its right-hand side is a shape mismatch, not an absent constraint:
        !! the caller wrote one half of a pair and the other half is what they forgot.
        subroutine check_linear(a, b, aname, bname)
            real(real64), intent(in), optional :: a(:, :) !! the constraint matrix
            real(real64), intent(in), optional :: b(:)    !! its right-hand side
            character(len=*), intent(in)       :: aname   !! `a_ineq` or `a_eq`
            character(len=*), intent(in)       :: bname   !! `b_ineq` or `b_eq`

            integer :: i
            character(len=:), allocatable :: shape_text

            if (.not. (present(a) .or. present(b))) return
            shape_text = aname//" must have size(x) columns and size("//bname//") rows"
            if (.not. present(a)) call abort_here(shape_text)
            if (.not. present(b)) call abort_here(shape_text)
            if (size(a, 2) /= n) call abort_here(shape_text)
            if (size(a, 1) /= size(b)) call abort_here(shape_text)
            ! PRIMA's ZERO_LINEAR_CONSTRAINT: upstream drops such a row and warns. A row of zeros
            ! is either `0 <= b` (no constraint at all) or `0 <= b < 0` (an infeasible problem
            ! stated by accident), and neither is what the caller meant to write.
            do i = 1, size(a, 1)
                if (all(a(i, :) == ZERO)) call abort_here( &
                    "a linear constraint must not have an all-zero row")
            end do

        end subroutine check_linear

        !> Aborts with this entry point and the caller's context, whether or not one was given.
        subroutine abort_here(text)
            character(len=*), intent(in) :: text !! what went wrong

            if (present(context)) then
                call prima_abort(ep, text, context)
            else
                call prima_abort(ep, text)
            end if

        end subroutine abort_here

    end subroutine refuse_bad_call

    ! ---- what the caller reads back ------------------------------------------------------------

    !> The constraint violation at one point, in the CALLER's units and against the CALLER's own
    !> constraints.
    !!
    !! **This procedure is this library's, not upstream's**, and it is what makes `info%cstrv`
    !! comparable with the numbers the caller wrote. The engines work in `y = x/scale` and measure
    !! violation there, against constraints folded and normalised for their own use -- LINCOA
    !! divides every row by its gradient length, which rescales the violation with it. Recomputing
    !! here is what `feature_optimizer.md` 6.5 item 12 asks for, and it is also what PRIMA's own
    !! MATLAB post-processing does after its `scale` option.
    !!
    !! PRIMA's L-infinity form: the largest of zero, the bound violations, the inequality
    !! violations, the absolute equality residuals and the nonlinear constraint values. `maximum`
    !! rather than `maxval`, so that a NaN propagates rather than being skipped -- though a
    !! non-finite constraint value aborts in `evaluate_fc` long before it could arrive here.
    function caller_violation(x, lower, upper, a_ineq, b_ineq, a_eq, b_eq, c) result(cstrv)
        real(real64), intent(in)           :: x(:)         !! the point, in the caller's units
        real(real64), intent(in), optional :: lower(:)     !! bounds
        real(real64), intent(in), optional :: upper(:)     !! bounds
        real(real64), intent(in), optional :: a_ineq(:, :) !! `a_ineq*x <= b_ineq`
        real(real64), intent(in), optional :: b_ineq(:)    !! `a_ineq*x <= b_ineq`
        real(real64), intent(in), optional :: a_eq(:, :)   !! `a_eq*x = b_eq`
        real(real64), intent(in), optional :: b_eq(:)      !! `a_eq*x = b_eq`
        real(real64), intent(in), optional :: c(:)         !! nonlinear constraint values at `x`

        real(real64) :: cstrv                              !! the largest violation, never negative

        cstrv = ZERO
        if (present(lower)) cstrv = maximum([cstrv, lower - x])
        if (present(upper)) cstrv = maximum([cstrv, x - upper])
        if (present(a_ineq) .and. present(b_ineq)) then
            cstrv = maximum([cstrv, matprod(a_ineq, x) - b_ineq])
        end if
        if (present(a_eq) .and. present(b_eq)) then
            cstrv = maximum([cstrv, abs(matprod(a_eq, x) - b_eq)])
        end if
        if (present(c)) cstrv = maximum([cstrv, c])

    end function caller_violation

    !> Fills the caller's `info` and `history` from the state and PRIMA's exit code.
    !!
    !! **This procedure is this library's, not upstream's.** The mapping of 5.6 lives here once
    !! rather than three times: `SMALL_TR_RADIUS` is `PF_OPT_OK`, `FTARGET_ACHIEVED` is
    !! `PF_OPT_TARGET`, `MAXFUN_REACHED` and `MAXTR_REACHED` are `PF_OPT_LIMIT`, and everything
    !! else -- `TRSUBP_FAILED`, `DAMAGING_ROUNDING`, `NAN_INF_MODEL` -- is `PF_OPT_ROUNDING`.
    !! `NAN_INF_X`, `NAN_INF_F` and `NO_SPACE_BETWEEN_BOUNDS` cannot arrive: a non-finite value
    !! aborts in `evaluate` and the bounds are refused before the engine sees them. They land in
    !! the default arm rather than one of their own so that `status` is defined whatever the
    !! engine returns.
    !!
    !! **`cstrv` decides last**, and it decides in the caller's units: a constrained run whose
    !! recomputed violation exceeds `ctol` is `PF_OPT_INFEASIBLE` whatever the engine thought.
    !!
    !! WHAT THIS FORBIDS: taking the verdict from the engine's own violation. Under `scale=` the
    !! engine measures in `y` and against constraints it normalised for its own use, so its number
    !! and the caller's differ by whatever `scale` is -- and a status read from one, reported
    !! beside a `cstrv` read from the other, would contradict itself. What stays scaled is the
    !! FILTER inside the engine, which can keep a slightly infeasible point it would otherwise
    !! discard when a bound is active and `scale` is extreme; that is why the verdict is taken
    !! here rather than there (`feature_optimizer.md` 8, last entry). The test
    !! `a bound-active scaled problem comes back feasible in the caller's units`
    !! (`test/test_prima.f90`) is what holds it.
    subroutine finish_run(prima_info, st, n, info, history, cstrv, ctol)
        integer, intent(in)              :: prima_info !! PRIMA's exit code
        type(prima_state), intent(in)    :: st         !! the run's radius, count and record
        integer, intent(in)              :: n          !! how many variables
        type(pf_optimize_info), intent(out), optional    :: info    !! what happened
        type(pf_optimize_history), intent(out), optional :: history !! every evaluation
        real(real64), intent(in), optional :: cstrv    !! violation in the caller's units
        real(real64), intent(in), optional :: ctol     !! the feasibility tolerance

        integer :: status

        select case (prima_info)
        case (SMALL_TR_RADIUS)
            status = PF_OPT_OK
        case (FTARGET_ACHIEVED)
            status = PF_OPT_TARGET
        case (MAXFUN_REACHED, MAXTR_REACHED)
            status = PF_OPT_LIMIT
        case default
            status = PF_OPT_ROUNDING
        end select

        if (present(cstrv) .and. present(ctol)) then
            if (cstrv > ctol) status = PF_OPT_INFEASIBLE
        end if

        if (present(info)) then
            info%status = status
            info%converged = (status == PF_OPT_OK .or. status == PF_OPT_TARGET)
            info%neval = st%neval
            info%niter = st%niter
            info%rho = st%rho
            if (present(cstrv)) info%cstrv = cstrv
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

    end subroutine finish_run

end module parquet_prima_common
