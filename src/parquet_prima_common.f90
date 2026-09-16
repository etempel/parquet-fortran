!> PRIMA's shared workers above the linear algebra: the evaluation of the caller's objective,
!> the exit test, the trust-region radius and ratio rules, the base-point shift, the bound
!> projection and the one-dimensional grid search.
!!
!! **Provenance.** Derived from PRIMA (Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`: `fortran/common/evaluate.f90`, `checkexit.f90`, `ratio.f90`,
!! `redrho.f90`, `xinbd.f90`, `shiftbase.f90` and `univar.f90`. The licence text and the list of
!! deviations shared by every file of this tier are in `parquet_prima_linalg`, which also carries
!! the kinds, the constants, the exit codes, the IEEE predicates and the abort.
!!
!! **A bug found here is checked against upstream before it is fixed here** -- with one exception,
!! `evaluate`, which is this library's own and is described at the procedure.
!!
!! **What is not vendored here.** `moderatex`, `moderatef` and `moderatec`: upstream replaces a
!! NaN coordinate by zero and a NaN or huge value by a large finite one and carries on, which is
!! its "moderated extreme barrier". This library refuses instead (`feature_optimizer.md` Q7), so a
!! non-finite value never reaches the model and PRIMA's `NAN_INF_F` cannot arise. `history.f90`:
!! upstream's `xhist`/`fhist` ring buffer with its `PRIMA_MAX_HIST_MEM_MB` cap is replaced by the
!! optional `pf_optimize_history` record, appended at the single point every evaluation passes
!! through -- `evaluate` -- and bounded by the evaluation budget instead. `checkexit_con`,
!! `evaluatefc`, `shiftbase_qint` (UOBYQA's) and `selectx.f90`'s filter are not here: the first
!! two arrive with COBYLA, and the last two belong to solvers this phase does not build.
module parquet_prima_common

    use, intrinsic :: iso_fortran_env, only : real64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_finite
    use parquet_optimize, only : pf_objective, pf_optimize_history
    use parquet_prima_linalg, only : ZERO, HALF, QUART, INFO_DFT, NAN_INF_X, NAN_INF_F, &
        FTARGET_ACHIEVED, MAXFUN_REACHED, TENTH, prima_abort, is_nan, is_inf, is_posinf, &
        is_neginf, is_finite, REALMAX, ONE, EPS, inprod, matprod, outprod, trueloc, linspace

    implicit none
    private

    public :: prima_state, FUNC_WITH_ARGS
    public :: evaluate, checkexit, redrat, redrho, xinbd, shiftbase, interval_max

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
            if (allocated(st%context)) then
                call prima_abort(st%entry_point, "the objective returned a non-finite value", &
                    st%context)
            else
                call prima_abort(st%entry_point, "the objective returned a non-finite value")
            end if
        end if

        if (st%want_history) call st%record%append(x, f)

    end subroutine evaluate


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

end module parquet_prima_common
