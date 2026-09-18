!> `pf_minimize_cobyla`: the validation, the folding of the caller's bounds and linear constraints
!> into the one form COBYLA works with, the scaling, the first evaluation, the call into the
!> vendored engine and the translation of PRIMA's exit code into a `pf_optimize_info`.
!!
!! **This file replaces PRIMA's own driver** `fortran/cobyla/cobyla.f90` -- its `get_lincon`
!! included -- and the part of `fortran/common/preproc.f90` that it calls. Upstream reads an
!! absent or invalid argument, substitutes a default, and warns. Here an absent argument gets
!! upstream's default and an INVALID one is refused with the message of `feature_optimizer.md`
!! 5.6, through the shared `refuse_bad_call`.
!!
!! **Where this driver's defaults differ from upstream's**, beyond those refusals:
!!
!! 1. The radii are used as given. Upstream floors both for every solver
!!    (`rhobeg = max(rhobeg, EPS)`, `rhoend = min(max(rhoend, EPS), rhobeg)`), and
!!    `pf_minimize_bobyqa`'s driver does; here a pair below `epsilon` is honoured, which is what
!!    lets a problem stated in very small units converge at all.
!! 2. `maxfun` is used as given; upstream raises one below `n + 2` to `n + 2`. COBYLA takes no
!!    `npt`, so upstream's reset of that has no counterpart here.
!! 3. `rhoend` is silently lowered to a `rhobeg` this driver defaulted below it, and a `rhoend`
!!    above `1` with no `rhobeg` beside it is refused although the default `rhobeg` would have been
!!    `max(10*rhoend, 1)` and so above it.
!!
!! **How the constraints are laid out.** COBYLA sees one vector `constr` of `m = m_lcon + m_nlcon`
!! values, all in PRIMA's sign convention `c <= 0`: the linear half first -- the caller's bounds,
!! equalities and inequalities folded into `amat^T y <= bvec`, evaluated inside the engine -- and
!! the nonlinear half after it, from the caller's object through `evaluate_fc`. An equality
!! becomes two inequalities. Unlike LINCOA, the rows are NOT normalised to unit gradient length:
!! COBYLA does not require it and upstream does not do it.
!!
!! **An infeasible start is normal**, and is not refused: COBYLA drives towards feasibility rather
!! than assuming it, which is the whole difference between its trust-region subproblem and
!! LINCOA's. The engine needs the value and the constraints AT the start point before its first
!! iteration, so this file evaluates them, and that evaluation is counted and recorded like any
!! other.
submodule (parquet_prima) parquet_prima_cobyla

    use, intrinsic :: iso_fortran_env, only : int64
    use parquet_prima_common, only : prima_state, refuse_bad_call, caller_violation, finish_run, &
        evaluate_fc, bounds_in_engine_units
    use parquet_prima_cobylb, only : cobylb
    use parquet_prima_linalg, only : eye, trueloc, matprod, &
        ZERO, ONE, TEN, EPS, BOUNDMAX, &
        ETA1_DFT, ETA2_DFT, GAMMA1_DFT, GAMMA2_DFT, &
        RHOBEG_DFT, RHOEND_DFT, FTARGET_DFT, MAXFUN_DIM_DFT, CTOL_DFT, CWEIGHT_DFT, MAXFILT_DFT

    implicit none

    !> The generic every abort message here names.
    character(len=*), parameter :: EP = "pf_minimize_cobyla"

contains

    module procedure pf_minimize_cobyla

        integer :: n, m_nlcon, m_lcon, maxfun_use, maxfilt_use, prima_info, nf
        real(real64) :: rhobeg_use, rhoend_use, ctol_use, ftarget_use, fout, cstrv_engine
        real(real64), allocatable :: sc(:), lo(:), hi(:), y(:)
        real(real64), allocatable :: ain(:, :), bin(:), aeq(:, :), beq(:)
        real(real64), allocatable :: amat(:, :), bvec(:), constr(:)
        type(prima_state) :: st

        n = size(x)
        ! `allow_constrained` is true here and nowhere else in this tier: this is the one engine
        ! that reads the object's constraints rather than dropping them.
        call refuse_bad_call(EP, f, x, allow_constrained=.true., lower=lower, upper=upper, &
                             rhobeg=rhobeg, rhoend=rhoend, scale=scale, ctol=ctol, &
                             a_ineq=a_ineq, b_ineq=b_ineq, a_eq=a_eq, b_eq=b_eq, &
                             max_neval=max_neval, context=context)
        m_nlcon = f%n_constraints()

        ! ---- the caller's units to the engine's ------------------------------------------------
        !
        ! As in `pf_minimize_lincoa`: the engine minimises `g(y) = f(scale*y)`, so the COLUMNS of
        ! each constraint matrix are scaled and the right-hand sides are not (6.5 item 12).
        allocate(sc(n))
        sc = ONE
        if (present(scale)) sc = scale
        allocate(y(n))
        y = x / sc
        allocate(lo(n), hi(n))
        ! An absent bound -- and one at or beyond `BOUNDMAX` in the engine's units, which means the
        ! same thing -- is `+/-BOUNDMAX` in the ENGINE's units, never the caller's divided by the
        ! scale: `BOUNDMAX/sc` overflows to +Infinity for any scale below one.
        call bounds_in_engine_units(sc, lo, hi, lower, upper)
        call scale_rows(a_ineq, b_ineq, sc, ain, bin)
        call scale_rows(a_eq, b_eq, sc, aeq, beq)
        call build_lincon(lo, hi, ain, bin, aeq, beq, amat, bvec)
        m_lcon = size(bvec)

        ! ---- upstream's defaults, for the arguments the caller left out ------------------------
        if (present(rhobeg)) then
            rhobeg_use = rhobeg
        else if (present(rhoend)) then
            ! Upstream's rule, as in `pf_minimize_lincoa`: an explicit `rhoend` pulls the default
            ! `rhobeg` up with it. `rhoend` is finite and positive here.
            rhobeg_use = max(TEN * rhoend, RHOBEG_DFT)
        else
            rhobeg_use = RHOBEG_DFT
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
        ctol_use = CTOL_DFT
        if (present(ctol)) ctol_use = ctol
        maxfilt_use = min(max(MAXFILT_DFT, 1), max(maxfun_use, 1))

        ! ---- the value and the constraints at the start ----------------------------------------
        !
        ! `cobylb` takes `[x, f, constr]` at the start point already filled; upstream's driver does
        ! the same. The linear half is computed rather than asked for, which is why the objective
        ! is evaluated through `evaluate_fc` with only the nonlinear tail handed to it.
        st%scale = sc
        st%entry_point = EP
        if (present(context)) st%context = context
        st%want_history = present(history)

        allocate(constr(m_lcon + m_nlcon))
        ! `matprod`, not `matmul`: `cobylb` computes this same row at this same point with the
        ! hand-written loop, and the two can differ in the last bit -- which would put a point
        ! into the filter under one violation and compare it under another.
        constr(1:m_lcon) = matprod(y, amat) - bvec
        call evaluate_fc(f, st, y, fout, constr(m_lcon + 1:m_lcon + m_nlcon))

        call cobylb(f, st, maxfilt_use, maxfun_use, amat, bvec, ctol_use, CWEIGHT_DFT, ETA1_DFT, &
                    ETA2_DFT, ftarget_use, GAMMA1_DFT, GAMMA2_DFT, rhobeg_use, rhoend_use, &
                    constr, fout, y, nf, cstrv_engine, prima_info)

        x = sc * y
        fmin = fout

        ! ---- what happened ---------------------------------------------------------------------
        !
        ! The violation is recomputed in the CALLER's units against the CALLER's own constraints.
        ! The nonlinear half comes from the engine's own `constr`, whose tail is `c(x)` at the
        ! returned point and is already in the caller's units -- the objective is only ever
        ! evaluated at `scale*y`.
        call finish_run(prima_info, st, n, info, history, &
                        cstrv=caller_violation(x, lower, upper, a_ineq, b_ineq, a_eq, b_eq, &
                                               constr(m_lcon + 1:m_lcon + m_nlcon)), &
                        ctol=ctol_use)

    end procedure pf_minimize_cobyla

    !> Scales the columns of a constraint matrix, or leaves both halves empty.
    !!
    !! `a_ineq` and `b_ineq` are optional and validated as a pair, so either both are present or
    !! neither is. The engine's arrays are always allocated -- with zero rows when the caller gave
    !! no constraints of that kind -- because the folding below reads their extents.
    subroutine scale_rows(a, b, sc, a_out, b_out)
        real(real64), intent(in), optional     :: a(:, :)     !! the caller's matrix, one row each
        real(real64), intent(in), optional     :: b(:)        !! the caller's right-hand sides
        real(real64), intent(in)               :: sc(:)       !! the per-coordinate scale
        real(real64), allocatable, intent(out) :: a_out(:, :) !! the matrix in engine units
        real(real64), allocatable, intent(out) :: b_out(:)    !! the right-hand sides, unchanged

        integer :: i

        if (present(a) .and. present(b)) then
            allocate(a_out(size(a, 1, kind=int64), size(a, 2, kind=int64)), &
                     b_out(size(b, kind=int64)))
            do i = 1, size(a, 1)
                a_out(i, :) = a(i, :) * sc
            end do
            b_out = b
        else
            allocate(a_out(0_int64, size(sc, kind=int64)), b_out(0_int64))
        end if

    end subroutine scale_rows

    !> Folds bounds, equalities and inequalities into COBYLA's `amat^T y <= bvec`.
    !!
    !! Upstream's `get_lincon` from `cobyla.f90`, less its warnings. A bound `lo <= y <= hi`
    !! becomes `-y <= -lo` and `y <= hi`; an equality `aeq y = beq` becomes `-aeq y <= -beq` and
    !! `aeq y <= beq`. Only the nontrivial bounds take a column. Unlike LINCOA's, this form is not
    !! normalised and no row is dropped -- an all-zero row is refused before the call.
    subroutine build_lincon(lo, hi, ain, bin, aeq, beq, amat, bvec)
        real(real64), intent(in) :: lo(:)     !! lower bounds, in engine units
        real(real64), intent(in) :: hi(:)     !! upper bounds, in engine units
        real(real64), intent(in) :: ain(:, :) !! inequality matrix, in engine units
        real(real64), intent(in) :: bin(:)    !! inequality right-hand sides
        real(real64), intent(in) :: aeq(:, :) !! equality matrix, in engine units
        real(real64), intent(in) :: beq(:)    !! equality right-hand sides
        real(real64), allocatable, intent(out) :: amat(:, :) !! one constraint per column
        real(real64), allocatable, intent(out) :: bvec(:)    !! the right-hand sides

        integer :: n, m_lcon, mxl, mxu
        integer, allocatable :: ixl(:), ixu(:)
        real(real64) :: idmat(size(lo), size(lo))

        n = size(lo)
        mxl = count(lo > -BOUNDMAX)
        mxu = count(hi < BOUNDMAX)
        m_lcon = mxl + mxu + 2 * size(beq) + size(bin)

        ixl = trueloc(lo > -BOUNDMAX)
        ixu = trueloc(hi < BOUNDMAX)

        allocate(amat(n, m_lcon), bvec(m_lcon))
        idmat = eye(n)
        amat = reshape(shape=shape(amat), source= &
            [-idmat(:, ixl), idmat(:, ixu), -transpose(aeq), transpose(aeq), transpose(ain)])
        bvec = [-lo(ixl), hi(ixu), -beq, beq, bin]

    end subroutine build_lincon

end submodule parquet_prima_cobyla
