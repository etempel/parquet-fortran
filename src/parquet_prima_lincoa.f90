!> `pf_minimize_lincoa`: the validation, the folding of the caller's constraints into the one form
!> LINCOA works with, the scaling, the call into the vendored engine, and the translation of
!> PRIMA's exit code into a `pf_optimize_info`.
!!
!! **This file replaces PRIMA's own driver** `fortran/lincoa/lincoa.f90` -- its `get_lincon`
!! included -- and the part of `fortran/common/preproc.f90` that it calls. Upstream reads an
!! absent or invalid argument, substitutes a default, and warns. Here an absent argument gets
!! upstream's default and an INVALID one is refused with the message of `feature_optimizer.md`
!! 5.6, through the shared `refuse_bad_call`, because this library has no channel for a warning.
!!
!! **Where this driver's defaults differ from upstream's**, beyond those refusals:
!!
!! 1. The radii are used as given. Upstream floors both for every solver
!!    (`rhobeg = max(rhobeg, EPS)`, `rhoend = min(max(rhoend, EPS), rhobeg)`), and
!!    `pf_minimize_bobyqa`'s driver does; here a pair below `epsilon` is honoured, which is what
!!    lets a problem stated in very small units converge at all.
!! 2. `maxfun` is used as given; upstream raises one below `n + 3` to `n + 3`, and resets `npt`
!!    when `npt >= maxfun`. A budget below `npt` therefore ends the run with `PF_OPT_LIMIT` after
!!    that many evaluations.
!! 3. `rhoend` is silently lowered to a `rhobeg` this driver defaulted below it, and a `rhoend`
!!    above `1` with no `rhobeg` beside it is refused although the default `rhobeg` would have been
!!    `max(10*rhoend, 1)` and so above it.
!!
!! **The two warnings upstream raises from this driver are made unreachable rather than silenced**
!! (6.5 item 2): a linear constraint with an all-zero gradient is refused by `refuse_bad_call`
!! where upstream drops it and warns, and an infeasible start is refused here where upstream
!! relaxes the right-hand sides to admit it and warns. The second refusal is what keeps
!! `info%cstrv` meaningful: a relaxed `bvec` would make the engine's own feasibility a statement
!! about constraints the caller never wrote.
!!
!! **LINCOA works with ONE set of inequalities**, `amat^T y <= bvec`, each row scaled to unit
!! gradient length. `build_lincon` below folds the caller's bounds, equalities and inequalities
!! into that form, exactly as upstream's `get_lincon` does; an equality becomes two inequalities.
!! The caller's own arrays travel beside it, because the violation the engine reports is measured
!! against those rather than against the folded form.
submodule (parquet_prima) parquet_prima_lincoa

    use, intrinsic :: iso_fortran_env, only : int64
    use parquet_prima_common, only : prima_state, refuse_bad_call, caller_violation, finish_run, &
        bounds_in_engine_units
    use parquet_prima_lincob, only : lincob
    use parquet_prima_linalg, only : sum, prima_abort, is_finite, eye, trueloc, maximum, norm, &
        ZERO, ONE, TWO, TEN, EPS, BOUNDMAX, MAXPOW10, &
        ETA1_DFT, ETA2_DFT, GAMMA1_DFT, GAMMA2_DFT, &
        RHOBEG_DFT, RHOEND_DFT, FTARGET_DFT, MAXFUN_DIM_DFT, CTOL_DFT, CWEIGHT_DFT, MAXFILT_DFT, &
        SMALL_TR_RADIUS, FTARGET_ACHIEVED, MAXFUN_REACHED, MAXTR_REACHED

    implicit none

    !> The generic every abort message here names.
    character(len=*), parameter :: EP = "pf_minimize_lincoa"

contains

    module procedure minimize_lincoa_obj

        integer :: n, npt_use, maxfun_use, maxfilt_use, prima_info, nf
        real(real64) :: rhobeg_use, rhoend_use, ctol_use, ftarget_use, fout, cstrv_engine
        real(real64), allocatable :: sc(:), lo(:), hi(:), y(:)
        real(real64), allocatable :: ain(:, :), bin(:), aeq(:, :), beq(:)
        real(real64), allocatable :: amat(:, :), bvec(:)
        type(prima_state) :: st

        n = size(x)
        call refuse_bad_call(EP, f, x, allow_constrained=.false., lower=lower, upper=upper, &
                             rhobeg=rhobeg, rhoend=rhoend, npt=npt, scale=scale, ctol=ctol, &
                             a_ineq=a_ineq, b_ineq=b_ineq, a_eq=a_eq, b_eq=b_eq, &
                             max_neval=max_neval, context=context)

        ! ---- the caller's units to the engine's ------------------------------------------------
        !
        ! The engine minimises `g(y) = f(scale*y)` over `y = x/scale`, so a constraint
        ! `sum_j a(i,j) x(j) <= b(i)` becomes `sum_j (a(i,j) scale(j)) y(j) <= b(i)`: the COLUMNS
        ! of the matrix are scaled and the right-hand side is not (6.5 item 12).
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
        y = max(lo, min(hi, y))
        call scale_rows(a_ineq, b_ineq, sc, ain, bin)
        call scale_rows(a_eq, b_eq, sc, aeq, beq)

        ! ---- upstream's defaults, for the arguments the caller left out ------------------------
        !
        ! LINCOA takes PRIMA's plain defaults; the bound-aware `rhobeg` of `preproc` is BOBYQA's
        ! alone, because LINCOA treats a bound as one more linear constraint rather than as a box
        ! the trust region must fit inside.
        if (present(rhobeg)) then
            rhobeg_use = rhobeg
        else if (present(rhoend)) then
            ! Upstream's rule: an explicit `rhoend` pulls the default `rhobeg` up with it, so that
            ! asking for more accuracy than the default radius allows does not silently give a
            ! run with `rhoend` almost equal to `rhobeg`. `rhoend` is finite and positive here --
            ! `refuse_bad_call` has seen it.
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
        npt_use = 2 * n + 1
        if (present(npt)) npt_use = npt
        ctol_use = CTOL_DFT
        if (present(ctol)) ctol_use = ctol
        ! `cweight` and `maxfilt` keep PRIMA's defaults and are not arguments (6.5): a knob nobody
        ! has asked to turn is not an argument until someone does.
        maxfilt_use = min(max(MAXFILT_DFT, 1), max(maxfun_use, 1))

        call refuse_infeasible_start(y, lo, hi, ain, bin, aeq, beq, rhoend_use, context)
        call build_lincon(lo, hi, ain, bin, aeq, beq, amat, bvec)

        ! ---- the run ---------------------------------------------------------------------------
        st%scale = sc
        st%entry_point = EP
        if (present(context)) st%context = context
        st%want_history = present(history)

        call lincob(f, st, maxfilt_use, maxfun_use, npt_use, aeq, ain, amat, beq, bin, bvec, &
                    ctol_use, CWEIGHT_DFT, ETA1_DFT, ETA2_DFT, ftarget_use, GAMMA1_DFT, &
                    GAMMA2_DFT, rhobeg_use, rhoend_use, lo, hi, y, nf, cstrv_engine, fout, &
                    prima_info)

        x = sc * y
        fmin = fout

        ! ---- what happened ---------------------------------------------------------------------
        !
        ! The violation is recomputed in the CALLER's units against the CALLER's own constraints,
        ! never read from `cstrv_engine`: the engine measured it in `y` against rows it normalised
        ! to unit gradient length, which is a different number (6.5 item 12).
        call finish_run(prima_info, st, n, converged=converged, info=info, history=history, &
                        cstrv=caller_violation(x, lower, upper, a_ineq, b_ineq, a_eq, b_eq), &
                        ctol=ctol_use)

    end procedure minimize_lincoa_obj

    ! The FULLY RESTATED form, not `module procedure minimize_lincoa_func`: in the abbreviated
    ! form gfortran 15 gives the `procedure(pf_objective_func)` dummy an implicit interface and
    ! refuses the pointer assignment below with "Explicit interface required for 'f'".
    module subroutine minimize_lincoa_func(f, x, fmin, a_ineq, b_ineq, a_eq, b_eq, lower, &
                                           upper, rhobeg, rhoend, npt, scale, ctol, ftarget, &
                                           max_neval, converged, info, history, context)
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
        integer, intent(in), optional                    :: max_neval    !! evaluation budget; at most huge(1)/2
        logical, intent(out), optional                   :: converged !! the run's own rule fired
        type(pf_optimize_info), intent(out), optional    :: info         !! what happened
        type(pf_optimize_history), intent(out), optional :: history      !! every evaluation
        character(len=*), intent(in), optional           :: context      !! call-site text

        type(prima_func_objective) :: obj !! wraps the plain function as an objective object

        obj%fun => f
        call minimize_lincoa_obj(obj, x, fmin, a_ineq=a_ineq, b_ineq=b_ineq, a_eq=a_eq, &
                                 b_eq=b_eq, lower=lower, upper=upper, rhobeg=rhobeg, &
                                 rhoend=rhoend, npt=npt, scale=scale, ctol=ctol, &
                                 ftarget=ftarget, max_neval=max_neval, &
                                 converged=converged, info=info, &
                                 history=history, context=context)

    end subroutine minimize_lincoa_func

    !> Scales the columns of a constraint matrix, or leaves both halves unallocated-as-empty.
    !!
    !! `a_ineq` and `b_ineq` are optional and validated as a pair, so either both are present or
    !! neither is. The engine's arrays are always allocated -- with zero rows when the caller gave
    !! no constraints of that kind -- because `lincob` reads their extents.
    subroutine scale_rows(a, b, sc, a_out, b_out)
        real(real64), intent(in), optional     :: a(:, :)  !! the caller's matrix, one row each
        real(real64), intent(in), optional     :: b(:)     !! the caller's right-hand sides
        real(real64), intent(in)               :: sc(:)    !! the per-coordinate scale
        real(real64), allocatable, intent(out) :: a_out(:, :) !! the matrix in engine units
        real(real64), allocatable, intent(out) :: b_out(:) !! the right-hand sides, unchanged

        integer :: i

        if (present(a) .and. present(b)) then
            ! GCOVR_EXCL_START -- gcov attribution artifact: the count for this two-line
            ! `allocate` lands on its continuation line, so the first line reads as unhit while
            ! the loop below it, which no branch separates from it, shows a positive count.
            allocate(a_out(size(a, 1, kind=int64), size(a, 2, kind=int64)), &
                     b_out(size(b, kind=int64)))
            ! GCOVR_EXCL_STOP
            do i = 1, size(a, 1)
                a_out(i, :) = a(i, :) * sc
            end do
            b_out = b
        else
            allocate(a_out(0_int64, size(sc, kind=int64)), b_out(0_int64))
        end if

    end subroutine scale_rows

    !> Refuses a start that does not satisfy the linear constraints.
    !!
    !! **The predicate is upstream's own**, the one `get_lincon` uses to decide whether to warn:
    !! a violation is ignored when it is below `1e-6 * rhoend` times the gradient's length, which
    !! is the size at which upstream's relaxation of `bvec` is rounding rather than a change of
    !! problem. Above it, upstream widens the constraint and prints a warning; here it is refused,
    !! for the reason the file header gives.
    !!
    !! WHAT THIS FORBIDS: accepting an infeasible start by relaxing `bvec`, as upstream does.
    !! `bvec` is what the engine's trust-region step keeps its iterates inside and what the filter
    !! measures a candidate against, so a relaxed `bvec` does not merely admit the start -- it
    !! moves the whole feasible region, and every later `info%cstrv` would be measured against the
    !! moved one. The scenario `prima_lincoa_infeasible_start` is what holds
    !! this here.
    subroutine refuse_infeasible_start(y, lo, hi, ain, bin, aeq, beq, rhoend_use, context)
        real(real64), intent(in) :: y(:)        !! the start, in engine units
        real(real64), intent(in) :: lo(:)       !! lower bounds, in engine units
        real(real64), intent(in) :: hi(:)       !! upper bounds, in engine units
        real(real64), intent(in) :: ain(:, :)   !! inequality matrix, in engine units
        real(real64), intent(in) :: bin(:)      !! inequality right-hand sides
        real(real64), intent(in) :: aeq(:, :)   !! equality matrix, in engine units
        real(real64), intent(in) :: beq(:)      !! equality right-hand sides
        real(real64), intent(in) :: rhoend_use  !! the final radius, which sets the tolerance
        character(len=*), intent(in), optional :: context !! call-site text

        !> 5.6's text for this refusal.
        character(len=*), parameter :: TEXT = "the start point must satisfy the linear constraints"

        real(real64) :: smallx, ain_norm(size(bin)), aeq_norm(size(beq))
        logical :: bad

        smallx = TEN**max(-6, -MAXPOW10) * rhoend_use
        ain_norm = sqrt(sum(ain**2, dim=2))
        aeq_norm = sqrt(sum(aeq**2, dim=2))
        bad = any(y + smallx < lo) .or. any(y - smallx > hi) &
              .or. any(abs(matmul(aeq, y) - beq) > smallx * aeq_norm) &
              .or. any(matmul(ain, y) - bin > smallx * ain_norm)
        if (bad) then
            if (present(context)) then
                call prima_abort(EP, TEXT, context)
            else
                call prima_abort(EP, TEXT)
            end if
        end if

    end subroutine refuse_infeasible_start

    !> Folds bounds, equalities and inequalities into LINCOA's single `amat^T y <= bvec`.
    !!
    !! Upstream's `get_lincon`, less its relaxation of `bvec` (refused above) and its warnings.
    !! A bound `lo <= y <= hi` becomes `-y <= -lo` and `y <= hi`; an equality `aeq y = beq`
    !! becomes `-aeq y <= -beq` and `aeq y <= beq`. Only the nontrivial bounds and the rows with a
    !! non-zero gradient take a column, and every row of the result is normalised to unit gradient
    !! length, which LINCOA relies on. The start point does not appear: upstream's `get_lincon`
    !! uses it to relax `bvec`, and that relaxation is refused above rather than performed.
    subroutine build_lincon(lo, hi, ain, bin, aeq, beq, amat, bvec)
        real(real64), intent(in) :: lo(:)     !! lower bounds, in engine units
        real(real64), intent(in) :: hi(:)     !! upper bounds, in engine units
        real(real64), intent(in) :: ain(:, :) !! inequality matrix, in engine units
        real(real64), intent(in) :: bin(:)    !! inequality right-hand sides
        real(real64), intent(in) :: aeq(:, :) !! equality matrix, in engine units
        real(real64), intent(in) :: beq(:)    !! equality right-hand sides
        real(real64), allocatable, intent(out) :: amat(:, :) !! one constraint per column
        real(real64), allocatable, intent(out) :: bvec(:)    !! the right-hand sides

        integer :: n, m, mxl, mxu, meq, mineq
        integer, allocatable :: ixl(:), ixu(:), ieq(:), iineq(:)
        real(real64) :: idmat(size(lo), size(lo))
        real(real64) :: aeq_norm(size(beq)), ain_norm(size(bin))
        real(real64), allocatable :: anorm(:)

        n = size(lo)
        aeq_norm = sqrt(sum(aeq**2, dim=2))
        ain_norm = sqrt(sum(ain**2, dim=2))
        mxl = count(lo > -BOUNDMAX)
        mxu = count(hi < BOUNDMAX)
        meq = count(aeq_norm > ZERO)
        mineq = count(ain_norm > ZERO)
        m = mxl + mxu + 2 * meq + mineq

        ixl = trueloc(lo > -BOUNDMAX)
        ixu = trueloc(hi < BOUNDMAX)
        ieq = trueloc(aeq_norm > ZERO)
        iineq = trueloc(ain_norm > ZERO)

        allocate(amat(n, m), bvec(m))
        idmat = eye(n)
        amat = reshape(shape=shape(amat), source= &
            [-idmat(:, ixl), idmat(:, ixu), -transpose(aeq(ieq, :)), transpose(aeq(ieq, :)), &
             transpose(ain(iineq, :))])
        bvec = [-lo(ixl), hi(ixu), -beq(ieq), beq(ieq), bin(iineq)]

        ! Each constraint is normalised to a gradient of unit length, which is essential for
        ! LINCOA: the bound rows are unit already, so only the tail is divided.
        anorm = [aeq_norm(ieq), aeq_norm(ieq), ain_norm(iineq)]
        if (m > mxl + mxu) then
            amat(:, mxl + mxu + 1:m) = amat(:, mxl + mxu + 1:m) / spread(anorm, dim=1, ncopies=n)
            bvec(mxl + mxu + 1:m) = bvec(mxl + mxu + 1:m) / anorm
        end if

    end subroutine build_lincon

end submodule parquet_prima_lincoa
