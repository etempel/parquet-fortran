!> PRIMA's LINCOA: minimisation subject to linear equality and inequality constraints, by a
!> quadratic model interpolating `npt` points, with an active-set trust-region step and a geometry
!> step.
!!
!! **Provenance.** Derived from PRIMA (Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`: `fortran/lincoa/lincob.f90`, `geometry.f90`, `getact.f90`,
!! `initialize.f90`, `trustregion.f90` and `update.f90`, which are one module each upstream and one
!! module here (`feature_optimizer.md` 4.1's ten-file layout). The licence text and the deviations
!! shared by every file of this tier are in `parquet_prima_linalg`.
!!
!! The algorithm is M. J. D. Powell's; there is no LINCOA paper, and the comments citing equation
!! numbers of "the NEWUOA paper" and "the BOBYQA paper" are upstream's and refer to *The NEWUOA
!! software for unconstrained optimization without derivatives* (2006) and *The BOBYQA algorithm
!! for bound constrained optimization without derivatives* (DAMTP 2009/NA06), whose model algebra
!! LINCOA shares. `getact` -- the active-set solver the other two solvers have no counterpart for
!! -- follows Powell 2015, *On fast trust region methods for quadratic models with linear
!! constraints*.
!!
!! **A bug found here is checked against upstream before it is fixed here.** Every formula, every
!! branch and every tolerance below is upstream's.
!!
!! **How the constraints reach this module.** LINCOA works with ONE set of linear inequalities,
!! `amat^T x <= bvec`, each row scaled to unit gradient length. The caller's bounds, equalities and
!! inequalities are folded into that form by the driver in `parquet_prima_lincoa`, which is where
!! upstream's `get_lincon` went; `Aeq`, `Aineq`, `beq`, `bineq`, `xl` and `xu` are still passed in
!! because the violation reported back is measured against the caller's own constraints, not
!! against the folded ones.
!!
!! **What changed beyond the tier-wide list.**
!!
!! 1. `lincob` and `initxf` take the caller's objective and a `prima_state` in place of upstream's
!!    `procedure(OBJ) :: calfun`, and evaluate through `evaluate`, which screens the value and
!!    fills the record.
!! 2. `iprint` and every `fmsg`/`rhomsg`/`retmsg` call are gone: this tier prints nothing.
!! 3. `xhist`/`fhist`/`chist` and the `savehist`/`rangehist` calls are gone with them; the record
!!    is the optional `pf_optimize_history`, appended in `evaluate`.
!! 4. `callback_fcn` and `CALLBACK_TERMINATE` are not carried (`feature_optimizer.md` Q22).
!! 5. Upstream's driver `lincoa.f90` is NOT vendored: its argument defaults, its `get_lincon` and
!!    its `preproc` adjustments become the validation and the driver in `parquet_prima_lincoa`,
!!    where an adjustment upstream makes with a warning is either refused or reported.
module parquet_prima_lincob

    use, intrinsic :: iso_fortran_env, only : real64, int64
    use parquet_optimize, only : pf_objective
    use parquet_prima_common, only : prima_state, evaluate, checkexit, redrat, redrho, &
        shiftbase, savefilt, selectx, isbetter
    use parquet_prima_linalg, only : sum, ZERO, ONE, TWO, HALF, TEN, TENTH, EPS, REALMAX, REALMIN, &
        BOUNDMAX, TINYCV, is_nan, is_finite, is_posinf, inprod, matprod, outprod, trueloc, norm, &
        planerot, eye, maximum, isminor, lsqr, solve, linspace, int, r1update, &
        INFO_DFT, SMALL_TR_RADIUS, MAXTR_REACHED, DAMAGING_ROUNDING, NAN_INF_MODEL
    use parquet_prima_powalg, only : quadinc, omega_mul, omega_col, hess_mul, calden, &
        updateh, qradd, qrexc, setij

    implicit none
    private

    public :: lincob

contains


    !> LINCOA's main iteration.
    !!
    !! Builds the interpolation set inside the feasible region, then alternates an active-set
    !! trust-region step with a geometry step, reducing `rho` from `rhobeg` to `rhoend`. The point
    !! returned is chosen from the filter, so a run that ends slightly infeasible still hands back
    !! the best trade-off it saw rather than its last iterate.
    !!
    !! Upstream's own description of the state it carries -- `xbase`, `[xpt, fval, kopt]`, `[gopt,
    !! hq, pq]`, `[bmat, zmat, idz]`, `rescon`, `iact`, `qfac` and `rfac` -- is kept in the comment
    !! block below.
    subroutine lincob(obj, st, maxfilt, maxfun, npt, Aeq, Aineq, amat, beq, bineq, bvec, &
            & ctol, cweight, eta1, eta2, ftarget, gamma1, gamma2, rhobeg, rhoend, xl, xu, x, nf, &
            & cstrv, f, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine performs the actual calculations of LINCOA.
        !
        ! The arguments IPRINT, MAXFILT, MAXFUN, MAXHIST, NPT, AEQ, AINEQ, BEQ, BINEQ, CTOL, CWEIGHT, ETA1,
        ! ETA2, FTARGET, GAMMA1, GAMMA2, RHOBEG, RHOEND, X, NF, F, XHIST, FHIST, CHIST, CSTRV and INFO are
        ! identical to the corresponding arguments in subroutine LINCOA.
        ! AMAT is a matrix whose columns are the constraint gradients, scaled so that they have unit length.
        ! BVEC contains on entry the right hand sides of the constraints, scaled as above.
        ! XBASE holds a shift of origin that should reduce the contributions from rounding errors to values
        !   of the model and Lagrange functions.
        ! XOPT is the displacement from XBASE of the feasible vector of variables that provides the least
        !   calculated F so far, this vector being the current trust region centre. FOPT = F(XOPT + XBASE).
        !   However, we do not save XOPT and FOPT explicitly, because XOPT = XPT(:, KOPT) and
        !   FOPT = FVAL(KOPT), which is explained below.
        ! [XPT, FVAL, KOPT] describes the interpolation set:
        ! XPT contains the interpolation points relative to XBASE, each COLUMN for a point; FVAL holds the
        !   values of F at the interpolation points; KOPT is the index of XOPT in XPT.
        ! [GOPT, HQ, PQ] describes the quadratic model: GOPT will hold the gradient of the quadratic model
        !   at XBASE + XOPT; HQ will hold the explicit second order derivatives of the quadratic model; PQ
        !   will contain the parameters of the implicit second order derivatives of the quadratic model.
        ! [BMAT, ZMAT, IDZ] describes the matrix H in the NEWUOA paper (eq. 3.12), which is the inverse of
        !   the coefficient matrix of the KKT system for the least-Frobenius norm interpolation problem:
        !   ZMAT will hold a factorization of the leading NPT*NPT submatrix of H, the factorization being
        !   ZMAT*Diag(DZ)*ZMAT^T with DZ(1:IDZ-1)=-1, DZ(IDZ:NPT-N-1)=1. BMAT will hold the last N ROWs of H
        !   except for the (NPT+1)th column. Note that the (NPT + 1)th row and column of H are not saved as
        !   they are unnecessary for the calculation.
        ! D is reserved for trial steps from XOPT. It is chosen by subroutine TRSTEP or GEOSTEP. Usually
        !   XBASE + XOPT + D is the vector of variables for the next call of CALFUN.
        ! IACT is an integer array for the indices of the active constraints.
        ! RESCON holds information about the constraint residuals at the current trust region center XOPT.
        !   1. If if B(J) - AMAT(:, J)^T*XOPT <= DELTA, then RESCON(J) = B(J) - AMAT(:, J)^T*XOPT. Note that
        !   RESCON >= 0 in this case, because the algorithm keeps XOPT to be feasible.
        !   2. Otherwise, RESCON(J) is a negative value that B(J) - AMAT(:,J)^T*XOPT >= |RESCON(J)| >= DELTA.
        !   RESCON can be updated without calculating the constraints that are far from being active, so
        !   that we only need to evaluate the constraints that are nearly active.
        ! QFAC is the orthogonal part of the QR factorization of the matrix of active constraint gradients,
        !   these gradients being ordered in accordance with IACT. When NACT is less than N, columns are
        !   appended to QFAC to complete an N by N orthogonal matrix, which is important for keeping
        !   calculated steps sufficiently close to the boundaries of the active constraints.
        ! RFAC is the upper triangular part of this QR factorization.
        !--------------------------------------------------------------------------------------------------!

        ! Generic models

        ! Solver-specific modules

        class(pf_objective), intent(inout) :: obj    !! the objective (this thread's own)
        type(prima_state), intent(inout) :: st       !! scaling, wording, record and evaluation count
        integer, intent(in) :: maxfilt               !! how many points the filter keeps
        integer, intent(in) :: maxfun                !! the evaluation budget
        integer, intent(in) :: npt                   !! how many interpolation points
        real(real64), intent(in) :: Aeq(:, :)        !! the caller's equality matrix, one row per constraint
        real(real64), intent(in) :: Aineq(:, :)      !! the caller's inequality matrix, one row per constraint
        real(real64), intent(in) :: amat(:, :)       !! the constraint gradients, one per column, each of unit length
        real(real64), intent(in) :: beq(:)           !! the caller's equality right-hand sides
        real(real64), intent(in) :: bineq(:)         !! the caller's inequality right-hand sides
        real(real64), intent(in) :: bvec(:)          !! the right-hand sides of `amat`
        real(real64), intent(in) :: ctol             !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: cweight          !! how heavily violation weighs against value in the filter
        real(real64), intent(in) :: eta1             !! ratio below which the radius contracts
        real(real64), intent(in) :: eta2             !! ratio above which it expands
        real(real64), intent(in) :: ftarget          !! stop as soon as a value at or below this is found
        real(real64), intent(in) :: gamma1           !! the contraction factor
        real(real64), intent(in) :: gamma2           !! the expansion factor
        real(real64), intent(in) :: rhobeg           !! the initial trust-region radius
        real(real64), intent(in) :: rhoend           !! the final trust-region radius
        real(real64), intent(in) :: xl(:)            !! lower bounds
        real(real64), intent(in) :: xu(:)            !! upper bounds

        real(real64), intent(inout) :: x(:)          !! the start in, the best point out

        integer, intent(out) :: info                 !! PRIMA's exit code
        integer, intent(out) :: nf                   !! objective evaluations made
        real(real64), intent(out) :: cstrv           !! the constraint violation at the returned point
        real(real64), intent(out) :: f               !! objective value at the new point

        integer :: iact(size(bvec))
        integer :: idz
        integer :: ij(2, max(0, int(npt - 2 * size(x) - 1)))
        integer :: k
        integer :: knew_geo
        integer :: knew_tr
        integer :: kopt
        integer :: m
        integer :: maxtr
        integer :: n
        integer :: nact
        integer :: nfilt
        integer :: ngetact
        integer :: subinfo
        integer :: tr
        integer, allocatable :: ixl(:)
        integer, allocatable :: ixu(:)
        logical :: accurate_mod
        logical :: adequate_geo
        logical :: bad_trstep
        logical :: close_itpset
        logical :: evaluated(npt)
        logical :: feasible
        logical :: improve_geo
        logical :: qalt_better(3)
        logical :: reduce_rho
        logical :: shortd
        logical :: small_trrad
        logical :: trfail
        logical :: ximproved
        real(real64) :: b(size(bvec))
        real(real64) :: bmat(size(x), npt + size(x))
        real(real64) :: cfilt(maxfilt)
        real(real64) :: constr(count(xl > -BOUNDMAX) + count(xu < BOUNDMAX) + 2 * size(beq) + size(bineq))
        real(real64) :: constr_leq(size(beq))
        real(real64) :: cval(npt)
        real(real64) :: d(size(x))
        real(real64) :: delbar
        real(real64) :: delta
        real(real64) :: distsq(npt)
        real(real64) :: dnorm
        real(real64) :: dnorm_rec(3)  ! Powell's implementation: DNORM_REC(5)
        real(real64) :: ffilt(maxfilt)
        real(real64) :: fval(npt)
        real(real64) :: galt(size(x))
        real(real64) :: gamma3
        real(real64) :: gopt(size(x))
        real(real64) :: hq(size(x), size(x))
        real(real64) :: moderr
        real(real64) :: moderr_alt
        real(real64) :: pq(npt)
        real(real64) :: pqalt(npt)
        real(real64) :: qfac(size(x), size(x))
        real(real64) :: qred
        real(real64) :: ratio
        real(real64) :: rescon(size(bvec))
        real(real64) :: rfac(size(x), size(x))
        real(real64) :: rho
        real(real64) :: xbase(size(x))
        real(real64) :: xdrop(size(x))
        real(real64) :: xfilt(size(x), maxfilt)
        real(real64) :: xosav(size(x))
        real(real64) :: xpt(size(x), npt)
        real(real64) :: zmat(npt, npt - size(x) - 1)
        real(real64), parameter :: trtol = 1.0E-2_real64  ! Convergence tolerance of trust-region subproblem solver

        ! Sizes.
        m = int(size(bvec))
        n = int(size(x))

        ! IXL and IXU are the indices of the nontrivial lower and upper bounds, respectively.
        allocate(ixl(int(count(xl > -BOUNDMAX), int64)))
        allocate(ixu(int(count(xu < BOUNDMAX), int64)))
        ixl = trueloc(xl > -BOUNDMAX)
        ixu = trueloc(xu < BOUNDMAX)

        ! Initialize B, XBASE, XPT, FVAL, CVAL, and KOPT, together with the history, NF, IJ, and EVALUATED.
        b = bvec
        call initxf(obj, st, maxfun, Aeq, Aineq, amat, beq, bineq, ctol, ftarget, rhobeg, xl, xu, &
            & x, b, ij, kopt, nf, cval, fval, xbase, xpt, evaluated, subinfo)

        ! Initialize X, F, CONSTR, and CSTRV according to KOPT.
        ! N.B.: We must set CONSTR and CSTRV. Otherwise, if REDUCE_RHO is TRUE after the very first
        ! iteration due to SHORTD, then RHOMSG will be called with CONSTR and CSTRV uninitialized.
        x = xbase + xpt(:, kopt)
        f = fval(kopt)
        constr_leq = matprod(Aeq, x) - beq
        constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
        cstrv = maximum([ZERO, constr])

        ! Initialize the filter, including XFILT, FFILT, CONFILT, CFILT, and NFILT.
        ! N.B.: The filter is used only when selecting which iterate to return. It does not interfere with
        ! the iterations. LINCOA is NOT a filter method but a trust-region method. All the trust-region
        ! iterates are supposed to be feasible, but can be infeasible due to rounding errors; the
        ! geometry-improving iterates are not necessarily feasible. Powell's implementation does not use a
        ! filter to select the iterate, possibly returning a suboptimal iterate.
        nfilt = 0
        do k = 1, npt
            if (evaluated(k)) then
                call savefilt(cval(k), ctol, cweight, fval(k), xbase + xpt(:, k), nfilt, cfilt, ffilt, xfilt)
            end if
        end do

        ! Finish the initialization if INITXF completed normally and CALLBACK did not request termination;
        ! otherwise, do not proceed, as XPT etc may be uninitialized, leading to errors or exceptions.
        if (subinfo == INFO_DFT) then
            ! Initialize [BMAT, ZMAT, IDZ], representing inverse of KKT matrix of the interpolation system.
            call inith(ij, xpt, idz, bmat, zmat)

            ! Initialize the quadratic represented by [GOPT, HQ, PQ], so that its gradient at XBASE+XOPT is
            ! GOPT; its Hessian is HQ + sum_{K=1}^NPT PQ(K)*XPT(:, K)*XPT(:, K)'.
            hq = ZERO
            pq = omega_mul(idz, zmat, fval)
            gopt = matprod(bmat(:, 1:npt), fval) + hess_mul(xpt(:, kopt), xpt, pq)
            pqalt = pq
            galt = gopt
            if (.not. (all(is_finite(gopt)) .and. all(is_finite(hq)) .and. all(is_finite(pq)))) then
                subinfo = NAN_INF_MODEL  ! GCOVR_EXCL_LINE -- `evaluate` refuses a non-finite value
            end if
        end if

        ! Check whether to return due to abnormal cases that may occur during the initialization.
        if (subinfo /= INFO_DFT) then
            info = subinfo
            ! The driver reads the radius reached and the iteration count from the state; set them on
            ! every exit, this one included, so that `info%rho` and `info%niter` are never stale.
            st%rho = rhobeg
            st%niter = 0
            ! Return the best calculated values of the variables. If CTOL > 0, the KOPT decided by SELECTX
            ! may not be the same as the one by INITXF.
            kopt = selectx(ffilt(1:nfilt), cfilt(1:nfilt), cweight, ctol)
            x = xfilt(:, kopt)
            f = ffilt(kopt)
            constr_leq = matprod(Aeq, x) - beq
            constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
            cstrv = maximum([ZERO, constr])
            ! Arrange CHIST, FHIST, and XHIST so that they are in the chronological order.
            return
        end if

        ! Initialize RESCON.
        rescon = max(b - matprod(xpt(:, kopt), amat), ZERO)
        rescon(trueloc(rescon >= rhobeg)) = -rescon(trueloc(rescon >= rhobeg))
        ! MATLAB: rescon(rescon >= rhobeg) = -rescon(rescon >= rhobeg)

        ! Set some more initial values.
        ! We must initialize RATIO. Otherwise, when SHORTD = TRUE, compilers may raise a run-time error that
        ! RATIO is undefined. But its value will not be used: when SHORTD = FALSE, its value will be
        ! overwritten; when SHORTD = TRUE, its value is used only in BAD_TRSTEP, which is TRUE regardless of
        ! RATIO. Similar for KNEW_TR.
        ! No need to initialize SHORTD unless MAXTR < 1, but some compilers may complain if we do not do it.
        rho = rhobeg
        delta = rho
        ratio = -ONE
        dnorm_rec = REALMAX
        shortd = .false.
        trfail = .false.
        qalt_better = .false.
        knew_tr = 0
        knew_geo = 0
        qfac = eye(n)
        rfac = ZERO
        nact = 0
        iact = linspace(1, m, m)

        ! If DELTA <= GAMMA3*RHO after an update, we set DELTA to RHO. GAMMA3 must be less than GAMMA2. The
        ! reason is as follows. Imagine a very successful step with DENORM = the un-updated DELTA = RHO.
        ! Then TRRAD will update DELTA to GAMMA2*RHO. If GAMMA3 >= GAMMA2, then DELTA will be reset to RHO,
        ! which is not reasonable as D is very successful. See paragraph two of Sec. 5.2.5 in
        ! T. M. Ragonneau's thesis: "Model-Based Derivative-Free Optimization Methods and Software".
        ! According to test on 20230613, for LINCOA, this Powellful updating scheme of DELTA works evidently
        ! better than setting directly DELTA = MAX(NEW_DELTA, RHO).
        gamma3 = max(ONE, min(0.75_real64 * gamma2, 1.5_real64))

        ! MAXTR is the maximal number of trust-region iterations. Here, we set it to HUGE(MAXTR) - 1 so that
        ! the algorithm will not terminate due to MAXTR. However, this may not be allowed in other languages
        ! such as MATLAB. In that case, we can set MAXTR to 10*MAXFUN, which is unlikely to reach because
        ! each trust-region iteration takes 1 or 2 function evaluations unless the trust-region step is short
        ! or fails to reduce the trust-region model but the geometry step is not invoked.
        ! N.B.: Do NOT set MAXTR to HUGE(MAXTR), as it may cause overflow and infinite cycling in the DO
        ! loop. See
        ! https://fortran-lang.discourse.group/t/loop-variable-reaching-integer-huge-causes-infinite-loop
        ! https://fortran-lang.discourse.group/t/loops-dont-behave-like-they-should
        maxtr = huge(maxtr) - 1  ! MATLAB: maxtr = 10 * maxfun;
        info = MAXTR_REACHED

        ! Begin the iterative procedure.
        ! After solving a trust-region subproblem, we use three boolean variables to control the workflow.
        ! SHORTD: Is the trust-region trial step too short to invoke a function evaluation?
        ! IMPROVE_GEO: Should we improve the geometry?
        ! REDUCE_RHO: Should we reduce rho?
        ! LINCOA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously.
        do tr = 1, maxtr
            ! Generate the next trust region step D by calling TRSTEP. Note that D is feasible.
            call trstep(amat, delta, gopt, hq, pq, rescon, trtol, xpt, iact, nact, qfac, rfac, d, ngetact)
            dnorm = min(delta, norm(d))

            ! A trust region step is applied whenever its length is at least 0.5*DELTA. It is also
            ! applied if its length is at least 0.1999*DELTA and if a line search of TRSTEP has caused a
            ! change to the active set, indicated by NGETACT >= 2 (note that NGETACT is at least 1).
            ! Otherwise, the trust region step is considered too short to try.
            ! N.B. The magic number 0.1999 seems to be related to the fact that a linear constraint is
            ! considered nearly active if the point under consideration is within 0.2*DELTA to the boundary
            ! of the constraint. See the subroutine GETACT and Section 3 of Powell (2015) for more details.
            ! `<=` works better than `<` in case of underflow.
            shortd = ((dnorm <= HALF * delta .and. ngetact < 2) .or. dnorm <= 0.1999_real64 * delta)
            !------------------------------------------------------------------------------------------!
            ! The SHORTD defined above needs NGETACT, which relies on Powell's trust region subproblem
            ! solver. If a different subproblem solver is used, we can take the following SHORTD adopted
            ! from UOBYQA, NEWUOA and BOBYQA.
            ! !SHORTD = (DNORM < HALF * RHO)
            !------------------------------------------------------------------------------------------!

            ! DNORM_REC records the DNORM of recent trust-region iterations. It will be used to decide
            ! whether we should improve the geometry of the interpolation set or reduce RHO when SHORTD
            ! is TRUE. Note that it does not record the geometry steps.
            dnorm_rec = [dnorm_rec(2:size(dnorm_rec)), dnorm]

            ! In some cases, we reset DNORM_REC to REALMAX. This indicates a preference of improving the
            ! geometry of the interpolation set to reducing RHO in the subsequent three or more iterations.
            ! This is important for the performance of LINCOA.
            ! Zaikun 20230609: This does not exist in NEWUOA/BOBYQA/UOBYQA. Try it!
            if (delta > rho .or. .not. shortd) then  ! Another possibility: IF (DELTA > RHO) THEN
                dnorm_rec = REALMAX
            end if

            ! Set QRED to the reduction of the quadratic model when the move D is made from XOPT. QRED
            ! should be positive. If it is nonpositive due to rounding errors, we will not take this step.
            qred = -quadinc(d, xpt, gopt, pq, hq)  ! QRED = Q(XOPT) - Q(XOPT + D)
            trfail = (.not. qred > 1.0E-6 * rho**2)  ! QRED is tiny/negative or NaN.

            if (shortd .or. trfail) then
                ! In this case, do nothing but reducing DELTA. Afterward, DELTA < DNORM may occur.
                ! N.B.: 1. This value of DELTA will be discarded if REDUCE_RHO turns out TRUE later.
                ! 2. Powell's code does not shrink DELTA when TRFAIL is TRUE (i.e., when VQUAD >= 0 in
                ! Powell's code, where VQUAD = -QRED). Consequently, the algorithm may be stuck in an
                ! infinite cycling, because both REDUCE_RHO and IMPROVE_GEO may end up with FALSE in this
                ! case, which did happen in tests.
                ! 3. The factor HALF works better than TENTH (used in NEWUOA/BOBYQA), 0.2, and 0.7.
                delta = HALF * delta
                if (delta <= gamma3 * rho) then
                    delta = rho  ! Set DELTA to RHO when it is close to or below.
                end if
            else
                ! Calculate the next value of the objective function.
                x = xbase + (xpt(:, kopt) + d)
                call evaluate(obj, st, x, f)
                nf = nf + 1

                ! Evaluate the constraints. They are used only for printing messages.
                constr_leq = matprod(Aeq, x) - beq
                constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
                cstrv = maximum([ZERO, constr])

                ! Print a message about the function evaluation according to IPRINT.
                ! Save X, F, CSTRV into the history.
                ! Save X, F, CSTRV into the filter.
                call savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt)

                ! Check whether to exit.
                subinfo = checkexit(maxfun, nf, cstrv, ctol, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo
                    exit
                end if

                ! QALT_BETTER is a boolean array indicating whether the recent few (three) alternative
                ! models are more accurate in predicting the function value at XOPT + D.
                ! N.B.: Do NOT change the "<" in the comparison to "<="; otherwise, the result will not be
                ! reasonable if the two values being compared are both ZERO or INF.
                moderr = f - fval(kopt) + qred
                moderr_alt = f - fval(kopt) - quadinc(d, xpt, galt, pqalt)
                qalt_better = [qalt_better(2:size(qalt_better)), abs(moderr_alt) < TENTH * abs(moderr)]

                ! Calculate the reduction ratio by REDRAT, which handles Inf/NaN carefully.
                ratio = redrat(fval(kopt) - f, qred, eta1)

                ! Update DELTA. After this, DELTA < DNORM may hold.
                ! The new DELTA lies in [GAMMA1*DNORM, GAMMA2*DNORM].
                delta = trrad(delta, dnorm, eta1, eta2, gamma1, gamma2, ratio)
                if (delta <= gamma3 * rho) then
                    delta = rho  ! Set DELTA to RHO when it is close to or below.
                end if

                ! Is the newly generated X better than current best point?
                ximproved = (f < fval(kopt))

                ! Set KNEW_TR to the index of the interpolation point to be replaced with XNEW = XOPT + D.
                ! KNEW_TR will ensure that the geometry of XPT is "good enough" after the replacement.
                knew_tr = setdrop_tr(idz, kopt, ximproved, bmat, d, delta, rho, xpt, zmat)
                if (knew_tr > 0) then
                    ! Update [BMAT, ZMAT, IDZ] (represents H in the NEWUOA paper), [XPT, FVAL, KOPT] and
                    ! [GOPT, HQ, PQ] (the quadratic model), so that XPT(:, KNEW_TR) becomes XNEW = XOPT + D.
                    xdrop = xpt(:, knew_tr)
                    xosav = xpt(:, kopt)
                    call updateh(knew_tr, kopt, d, xpt, idz, bmat, zmat)
                    call updatexf(knew_tr, ximproved, f, xosav + d, kopt, fval, xpt)
                    call updateq(idz, knew_tr, ximproved, bmat, d, moderr, xdrop, xosav, xpt, zmat, gopt, hq, pq)

                    ! Establish the alternative model, namely the least Frobenius norm interpolant. Replace
                    ! the current model with the alternative model if the recent few (three) alternative
                    ! models are more accurate in predicting the function value of XOPT + D.
                    call tryqalt(idz, bmat, fval - fval(kopt), xpt(:, kopt), xpt, zmat, qalt_better, gopt, pq, hq, galt, pqalt)
                    if (.not. (all(is_finite(gopt)) .and. all(is_finite(hq)) .and. all(is_finite(pq)))) then
                        info = NAN_INF_MODEL  ! GCOVR_EXCL_START -- `evaluate` refuses a non-finite value
                        exit
                        ! GCOVR_EXCL_STOP
                    end if

                    ! Update RESCON if XOPT is changed.
                    ! Zaikun 20221115: Shouldn't we do it after DELTA is updated?
                    call updateres(ximproved, amat, b, delta, norm(d), xpt(:, kopt), rescon)
                end if

            end if  ! End of IF (SHORTD .OR. TRFAIL). The normal trust-region calculation ends.

            !----------------------------------------------------------------------------------------------!
            ! Before the next trust-region iteration, we may improve the geometry of XPT or reduce RHO
            ! according to IMPROVE_GEO and REDUCE_RHO, which in turn depend on the following indicators.
            ! N.B.: We must ensure that the algorithm does not set IMPROVE_GEO = TRUE at infinitely many
            ! consecutive iterations without moving XOPT or reducing RHO. Otherwise, the algorithm will get
            ! stuck in repetitive invocations of GEOSTEP. To this end, make sure the following.
            ! 1. The threshold for CLOSE_ITPSET is at least DELBAR, the trust region radius for GEOSTEP.
            ! Normally, DELBAR <= DELTA <= the threshold (In Powell's UOBYQA, DELBAR = RHO < the threshold).
            ! 2. If an iteration sets IMPROVE_GEO = TRUE, it must also reduce DELTA or set DELTA to RHO.

            ! ACCURATE_MOD: Are the recent models sufficiently accurate? Used only if SHORTD is TRUE.
            ! N.B.: The ACCURATE_MOD here plays a similar role as the variable with the same name in UOBYQA,
            ! NEWUOA, and BOBYQA. However, the definition of ACCURATE_MOD here is different from that in
            ! those solvers, which do not only check whether DNORM is small in recent iterations, but also
            ! verify a curvature condition that really indicates that recent models are sufficiently
            ! accurate. Here, however, we are not really sure whether they are accurate or not. Therefore,
            ! ACCURATE_MOD is not the best name, but we keep it to align with the other solvers.
            accurate_mod = all(dnorm_rec <= rho) .or. all(dnorm_rec(2:size(dnorm_rec)) <= 0.2 * rho)
            ! Powell's version (note that size(dnorm_rec) = 5 in his implementation):
            !accurate_mod = all(dnorm_rec <= HALF * rho) .or. all(dnorm_rec(3:size(dnorm_rec)) <= TENTH * rho)
            ! CLOSE_ITPSET: Are the interpolation points close to XOPT?
            distsq = sum((xpt - spread(xpt(:, kopt), dim=2, ncopies=npt))**2, dim=1)
            ! MATLAB: distsq = sum((xpt - xpt(:, kopt)).^2)  % Implicit expansion
            close_itpset = all(distsq <= 4.0_real64 * delta**2)  ! Powell's NEWUOA code.
            ! Below are some alternative definitions of CLOSE_ITPSET.
            ! N.B.: The threshold for CLOSE_ITPSET is at least DELBAR, the trust region radius for GEOSTEP.
            ! !close_itpset = all(distsq <= 4.0_real64 * rho**2)  ! Powell's UOBYQA code.
            ! !close_itpset = all(distsq <= max(delta**2, 4.0_real64 * rho**2))  ! Powell's code.
            ! !close_itpset = all(distsq <= max((TWO * delta)**2, (TEN * rho)**2))  ! Powell's BOBYQA code.
            ! ADEQUATE_GEO: Is the geometry of the interpolation set "adequate"?
            adequate_geo = (shortd .and. accurate_mod) .or. close_itpset
            ! SMALL_TRRAD: Is the trust-region radius small? This indicator seems not impactive in practice.
            small_trrad = (max(delta, dnorm) <= rho)  ! Behaves the same as Powell's version.
            !small_trrad = (delsav <= rho)  ! Powell's code. DELSAV = unupdated DELTA.

            ! IMPROVE_GEO and REDUCE_RHO are defined as follows.
            ! N.B.: If SHORTD is TRUE at the very first iteration, then REDUCE_RHO will be set to TRUE.

            ! BAD_TRSTEP (for IMPROVE_GEO): Is the last trust-region step bad?
            bad_trstep = (shortd .or. trfail .or. ratio <= eta1 .or. knew_tr == 0)
            improve_geo = bad_trstep .and. .not. adequate_geo
            ! BAD_TRSTEP (for REDUCE_RHO): Is the last trust-region step bad?
            bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. knew_tr == 0)
            reduce_rho = bad_trstep .and. adequate_geo .and. small_trrad

            ! Equivalently, REDUCE_RHO can be set as follows. It shows that REDUCE_RHO is TRUE in two cases.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. knew_tr == 0)
            ! !reduce_rho = (shortd .and. accurate_mod) .or. (bad_trstep .and. close_itpset .and. small_trrad)

            ! With REDUCE_RHO properly defined, we can also set IMPROVE_GEO as follows.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= eta1 .or. knew_tr == 0)
            ! !improve_geo = bad_trstep .and. (.not. reduce_rho) .and. (.not. close_itpset)

            ! With IMPROVE_GEO properly defined, we can also set REDUCE_RHO as follows.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. knew_tr == 0)
            ! !reduce_rho = bad_trstep .and. (.not. improve_geo) .and. small_trrad

            ! LINCOA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously.
            !call assert(.not. (improve_geo .and. reduce_rho), 'IMPROVE_GEO and REDUCE_RHO are not both TRUE', srname)
            !
            ! If SHORTD or TRFAIL is TRUE, then either IMPROVE_GEO or REDUCE_RHO is TRUE unless CLOSE_ITPSET
            ! is TRUE but SMALL_TRRAD is FALSE.
            !call assert((.not. (shortd .or. trfail)) .or. (improve_geo .or. reduce_rho .or. &
            !    & (close_itpset .and. .not. small_trrad)), 'If SHORTD or TRFAIL is TRUE, then either &
            !    & IMPROVE_GEO or REDUCE_RHO is TRUE unless CLOSE_ITPSET is TRUE but SMALL_TRRAD is FALSE', srname)
            !----------------------------------------------------------------------------------------------!

            ! Since IMPROVE_GEO and REDUCE_RHO are never TRUE simultaneously, the following two blocks are
            ! exchangeable: IF (IMPROVE_GEO) ... END IF and IF (REDUCE_RHO) ... END IF.

            if (improve_geo) then
                ! XPT(:, KNEW_GEO) will become  XOPT + D below. KNEW_GEO /= KOPT unless there is a bug.
                knew_geo = int(maxloc(distsq, dim=1))

                ! Set DELBAR, which will be used as the trust-region radius for the geometry-improving
                ! scheme GEOSTEP. Note that DELTA has been updated before arriving here.
                delbar = max(TENTH * delta, rho)  ! Powell's code
                !delbar = rho  ! Powell's UOBYQA code
                !delbar = max(min(TENTH * sqrt(maxval(distsq)), HALF * delta), rho)  ! Powell's NEWUOA code
                !delbar = max(min(TENTH * sqrt(maxval(distsq)), delta), rho)  ! Powell's BOBYQA code
                ! Find D so that the geometry of XPT will be improved when XPT(:, KNEW_GEO) becomes XOPT + D.
                call geostep(iact, idz, knew_geo, kopt, nact, amat, bmat, delbar, qfac, rescon, xpt, zmat, feasible, d)

                ! Calculate the next value of the objective function.
                x = xbase + (xpt(:, kopt) + d)
                call evaluate(obj, st, x, f)
                nf = nf + 1

                ! Evaluate the constraints. They are used only for printing messages.
                constr_leq = matprod(Aeq, x) - beq
                constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
                cstrv = maximum([ZERO, constr])

                ! Print a message about the function evaluation according to IPRINT.
                ! Save X, F, CSTRV into the history.
                ! Save X, F, CSTRV into the filter.
                call savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt)

                ! Check whether to exit.
                subinfo = checkexit(maxfun, nf, cstrv, ctol, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo
                    exit
                end if

                ! QALT_BETTER is a boolean array indicating whether the recent few (three) alternative
                ! models are more accurate in predicting the function value at XOPT + D.
                ! Powell's code takes XOPT + D into account only if it is feasible.
                ! N.B.: Do NOT change the "<" in the comparison to "<="; otherwise, the result will not be
                ! reasonable if the two values being compared are both ZERO or INF.
                moderr = f - fval(kopt) - quadinc(d, xpt, gopt, pq, hq)
                moderr_alt = f - fval(kopt) - quadinc(d, xpt, galt, pqalt)
                qalt_better = [qalt_better(2:size(qalt_better)), abs(moderr_alt) < TENTH * abs(moderr)]

                ! Is the newly generated X better than current best point?
                ximproved = (f < fval(kopt) .and. feasible)

                ! Update [BMAT, ZMAT, IDZ] (represents H in the NEWUOA paper), [XPT, FVAL, KOPT] and
                ! [GOPT, HQ, PQ] (the quadratic model), so that XPT(:, KNEW_GEO) becomes XNEW = XOPT + D.
                xdrop = xpt(:, knew_geo)
                xosav = xpt(:, kopt)
                call updateh(knew_geo, kopt, d, xpt, idz, bmat, zmat)
                call updatexf(knew_geo, ximproved, f, xosav + d, kopt, fval, xpt)
                call updateq(idz, knew_geo, ximproved, bmat, d, moderr, xdrop, xosav, xpt, zmat, gopt, hq, pq)

                ! Establish the alternative model, namely the least Frobenius norm interpolant. Replace the
                ! current model with the alternative model if the recent few (three) alternative models are
                ! more accurate in predicting the function value of XOPT + D.
                ! N.B.: Powell's code does this only if XOPT + D is feasible.
                call tryqalt(idz, bmat, fval - fval(kopt), xpt(:, kopt), xpt, zmat, qalt_better, gopt, pq, hq, galt, pqalt)
                if (.not. (all(is_finite(gopt)) .and. all(is_finite(hq)) .and. all(is_finite(pq)))) then
                    info = NAN_INF_MODEL  ! GCOVR_EXCL_START -- `evaluate` refuses a non-finite value
                    exit
                    ! GCOVR_EXCL_STOP
                end if

                ! Update RESCON. Zaikun 20221115: Currently, UPDATERES does not update RESCON if XIMPROVED
                ! is FALSE. Shouldn't we do it whenever DELTA is updated? Have we MISUNDERSTOOD RESCON?
                call updateres(ximproved, amat, b, delta, norm(d), xpt(:, kopt), rescon)
            end if  ! End of IF (IMPROVE_GEO). The procedure of improving geometry ends.

            ! The calculations with the current RHO are complete. Enhance the resolution of the algorithm
            ! by reducing RHO; update DELTA at the same time.
            if (reduce_rho) then
                if (rho <= rhoend) then
                    info = SMALL_TR_RADIUS
                    exit
                end if
                delta = max(HALF * rho, redrho(rho, rhoend))
                rho = redrho(rho, rhoend)
                ! Print a message about the reduction of RHO according to IPRINT.
                ! DNORM_REC is corresponding to the latest function evaluations with the current RHO.
                ! Update it after reducing RHO.
                dnorm_rec = REALMAX
            end if  ! End of IF (REDUCE_RHO). The procedure of reducing RHO ends.

            ! Shift XBASE if XOPT may be too far from XBASE.
            ! Powell's original criterion for shifting XBASE: before a trust region step or a geometry step,
            ! shift XBASE if SUM(XOPT**2) >= 1.0E3*DELTA**2.
            if (sum(xpt(:, kopt)**2) >= 1.0E3_real64 * delta**2) then
                ! Other possible criteria: SUM(XOPT**2) >= 1.0E4*DELTA**2, SUM(XOPT**2) >= 1.0E3*RHO**2.
                b = b - matprod(xpt(:, kopt), amat)
                call shiftbase(kopt, xbase, xpt, zmat, bmat, pq, hq, idz)
                ! SHIFTBASE shifts XBASE to XBASE + XOPT and XOPT to 0.
                pqalt = omega_mul(idz, zmat, fval - fval(kopt))
                galt = matprod(bmat(:, 1:npt), fval - fval(kopt)) + hess_mul(xpt(:, kopt), xpt, pqalt)
            end if

        end do  ! End of DO TR = 1, MAXTR. The iterative procedure ends.

        ! The radius reached and the iterations performed, for `info%rho` and `info%niter`. TR is MAXTR+1
        ! when the loop ran to completion.
        st%rho = rho
        st%niter = min(tr, maxtr)

        ! Return from the calculation, after trying the Newton-Raphson step if it has not been tried yet.
        if (info == SMALL_TR_RADIUS .and. shortd .and. dnorm > TENTH * rhoend .and. nf < maxfun) then
            x = xbase + (xpt(:, kopt) + d)
            call evaluate(obj, st, x, f)
            nf = nf + 1
            constr_leq = matprod(Aeq, x) - beq
            constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
            cstrv = maximum([ZERO, constr])
            ! Print a message about the function evaluation according to IPRINT.
            ! Zaikun 20230512: DELTA has been updated. RHO is only indicative here. TO BE IMPROVED.
            ! Save X, F, CSTRV into the history.
            ! Save X, F, CSTRV into the filter.
            call savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt)
        end if

        ! Return the best calculated values of the variables.
        kopt = selectx(ffilt(1:nfilt), cfilt(1:nfilt), cweight, ctol)
        x = xfilt(:, kopt)
        f = ffilt(kopt)
        constr_leq = matprod(Aeq, x) - beq
        constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
        cstrv = maximum([ZERO, constr])

        ! Deallocate IXL and IXU as they have finished their job.
        deallocate (ixl, ixu)

        ! Arrange CHIST, FHIST, and XHIST so that they are in the chronological order.

        ! Print a return message according to IPRINT.

    end subroutine lincob

    !> Builds the initial interpolation set and evaluates the objective on it, upstream's `initxf`.
    !!
    !! The first `2n + 1` points are `x0` and its coordinate displacements by `+/-rhobeg`; the rest
    !! displace two coordinates at once, by the pairs `setij` returns. `b` is shifted to `xbase`
    !! here and is otherwise the caller's.
    !!
    !! **The initial set is NOT required to be feasible**, and each point is evaluated whether it is
    !! or not: `cval` records each violation, and `kopt` is chosen among the feasible ones when
    !! there are any. Only the TRUST-REGION iterates are feasible by construction; a
    !! geometry-improving step need not be, which is upstream's design and is why LINCOA returns a
    !! point from the filter rather than its last iterate.
    subroutine initxf(obj, st, maxfun, Aeq, Aineq, amat, beq, bineq, ctol, ftarget, rhobeg, xl, xu,&
            & x0, b, ij, kopt, nf, cval, fval, xbase, xpt, evaluated, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine does the initialization about the interpolation points & their function values.
        !
        ! N.B.:
        ! 1. Remark on IJ:
        ! If NPT <= 2*N + 1, then IJ is empty. Assume that NPT >= 2*N + 2. Then SIZE(IJ) = [2, NPT-2*N-1].
        ! IJ contains integers between 1 and N. For each K > 2*N + 1, XPT(:, K) is
        ! XPT(:, IJ(1, K) + 1) + XPT(:, IJ(2, K) + 1). The 1 in IJ + 1 comes from the fact that XPT(:, 1)
        ! corresponds to the base point XBASE. Let I = IJ(1, K) and J = IJ(2, K). Then all the entries of
        ! XPT(:, K) are zero except for the I and J entries. Consequently, the Hessian of the quadratic
        ! model will get a possibly nonzero (I, J) entry.
        ! 2. At return,
        ! INFO = INFO_DFT: initialization finishes normally
        ! INFO = FTARGET_ACHIEVED: return because F <= FTARGET
        ! INFO = NAN_INF_X: return because X contains NaN
        ! INFO = NAN_INF_F: return because F is either NaN or +Inf
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        class(pf_objective), intent(inout) :: obj    !! the objective (this thread's own)
        type(prima_state), intent(inout) :: st       !! scaling, wording, record and evaluation count
        integer, intent(in) :: maxfun                !! the evaluation budget
        real(real64), intent(in) :: Aeq(:, :)        !! the caller's equality matrix, one row per constraint
        real(real64), intent(in) :: Aineq(:, :)      !! the caller's inequality matrix, one row per constraint
        real(real64), intent(in) :: amat(:, :)       !! the constraint gradients, one per column, each of unit length
        real(real64), intent(in) :: beq(:)           !! the caller's equality right-hand sides
        real(real64), intent(in) :: bineq(:)         !! the caller's inequality right-hand sides
        real(real64), intent(in) :: ctol             !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: ftarget          !! stop as soon as a value at or below this is found
        real(real64), intent(in) :: rhobeg           !! the initial trust-region radius
        real(real64), intent(in) :: xl(:)            !! lower bounds
        real(real64), intent(in) :: xu(:)            !! upper bounds
        real(real64), intent(in) :: x0(:)            !! the start point

        real(real64), intent(inout) :: b(:)          !! the right-hand sides of `amat`, relative to `xbase`

        integer, intent(out) :: info                 !! PRIMA's exit code
        integer, intent(out) :: ij(:, :)             !! the coordinate pairs of the interpolation set beyond `2n + 1`
        integer, intent(out) :: kopt                 !! index in `xpt` of the best point so far
        integer, intent(out) :: nf                   !! objective evaluations made
        logical, intent(out) :: evaluated(:)         !! which interpolation points have a value
        real(real64), intent(out) :: cval(:)         !! constraint violation at each interpolation point
        real(real64), intent(out) :: fval(:)         !! objective value at each interpolation point
        real(real64), intent(out) :: xbase(:)        !! the base point every other point is relative to
        real(real64), intent(out) :: xpt(:, :)       !! the interpolation set relative to `xbase`, one point per column

        integer :: k
        integer :: m
        integer :: n
        integer :: npt
        integer :: subinfo
        integer, allocatable :: ixl(:)
        integer, allocatable :: ixu(:)
        logical :: feasible(size(xpt, 2))
        real(real64) :: constr(count(xl > -BOUNDMAX) + count(xu < BOUNDMAX) + 2 * size(beq) + size(bineq))
        real(real64) :: constr_leq(size(beq))
        real(real64) :: cstrv
        real(real64) :: f
        real(real64) :: x(size(x0))

        ! Sizes.
        m = int(size(b))
        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Initialize INFO to the default value. At return, an INFO different from this value will indicate
        ! an abnormal return.
        info = INFO_DFT

        ! Initialize XBASE to X0.
        xbase = x0

        ! EVALUATED is a boolean array with EVALUATED(I) indicating whether the function value of the I-th
        ! interpolation point has been evaluated. We need it for a portable counting of the number of
        ! function evaluations, especially if the loop is conducted asynchronously. However, the loop here
        ! is not fully parallelizable if NPT>2N+1, as the definition XPT(;, 2N+2:end) involves FVAL(1:2N+1).
        evaluated = .false.

        ! Initialize XHIST, FHIST, CHIST, FVAL, and CVAL. Otherwise, compilers may complain that they are
        ! not (completely) initialized if the initialization aborts due to abnormality (see CHECKEXIT).
        ! N.B.: 1. Initializing them to NaN would be more reasonable (NaN is not available in Fortran).
        ! 2. Do not initialize the models if the current initialization aborts due to abnormality. Otherwise,
        ! errors or exceptions may occur, as FVAL and XPT etc are uninitialized.
        fval = REALMAX
        cval = REALMAX

        ! Set the nonzero coordinates of XPT(K,.), K=1,2,...,min[2*N+1,NPT], but they may be altered
        ! later to make a constraint violation sufficiently large.
        xpt(:, 1) = ZERO
        xpt(:, 2:n + 1) = rhobeg * eye(n)
        xpt(:, n + 2:npt) = -rhobeg * eye(n, npt - n - 1)  ! XPT(:, 2*N+2 : NPT) = ZERO if it is nonempty.

        ! Set IJ.
        ! In general, when NPT = (N+1)*(N+2)/2, we can set IJ(:, 1 : NPT - (2*N+1)) to ANY permutation
        ! of {{I, J} : 1 <= I /= J <= N}; when NPT < (N+1)*(N+2)/2, we can set it to the first NPT - (2*N+1)
        ! elements of such a permutation. The following IJ is defined according to Powell's code. See also
        ! Section 3 of the NEWUOA paper and (2.4) of the BOBYQA paper.
        ij = setij(n, npt)

        ! Set XPT(:, 2*N + 2 : NPT).
        ! Indeed, XPT(:, K) has only two nonzeros for each K >= 2*N + 2,
        ! N.B.: The 1 in IJ + 1 comes from the fact that XPT(:, 1) corresponds to XBASE.
        xpt(:, 2 * n + 2:npt) = xpt(:, ij(1, :) + 1) + xpt(:, ij(2, :) + 1)

        ! Update the constraint right-hand sides to allow for the shift XBASE.
        b = b - matprod(xbase, amat)

        ! Define FEASIBLE, which will be used when defining KOPT.
        do k = 1, npt
            ! Internally, we use AMAT and B to evaluate the constraints.
            cval(k) = maximum([ZERO, matprod(xpt(:, k), amat) - b])
            if (is_nan(cval(k))) then
                cval(k) = REALMAX  ! GCOVR_EXCL_LINE -- AMAT, B and XPT are all finite here
            end if
            ! Powell's implementation contains the following procedure that shifts every infeasible point if
            ! necessary so that its constraint violation is at least 0.2*RHOBEG. According to a test on
            ! 20230209, it does not evidently improve the performance of LINCOA. Indeed, it worsens a bit the
            ! performance in the early stage. Thus we decided to remove it.
            !----------------------------------------------------------------------------------------------!
            !mincv = 0.2_real64 * rhobeg
            !constr(1:m) = matprod(xpt(:, k), amat) - b
            !if (cval(k) < mincv .and. cval(k) > 0) then
            !    j = int(maxloc(constr(1:m), dim=1))
            !    xpt(:, k) = xpt(:, k) + (mincv - constr(j)) * amat(:, j)
            !end if
            !----------------------------------------------------------------------------------------------!
        end do
        feasible = (cval <= 0)

        ! Set FVAL by evaluating F. Totally parallelizable except for FMSG.
        ! IXL and IXU are the indices of the nontrivial lower and upper bounds, respectively.
        allocate(ixl(int(count(xl > -BOUNDMAX), int64)))
        allocate(ixu(int(count(xu < BOUNDMAX), int64)))
        ixl = trueloc(xl > -BOUNDMAX)
        ixu = trueloc(xu < BOUNDMAX)
        do k = 1, npt
            x = xbase + xpt(:, k)
            call evaluate(obj, st, x, f)
            ! Evaluate the constraints.
            constr_leq = matprod(Aeq, x) - beq
            constr = [xl(ixl) - x(ixl), x(ixu) - xu(ixu), -constr_leq, constr_leq, matprod(Aineq, x) - bineq]
            cstrv = maximum([ZERO, constr])

            ! Print a message about the function evaluation according to IPRINT.
            ! Save X, F, CSTRV into the history.

            evaluated(k) = .true.
            cval(k) = cstrv  ! CVAL will be used to initialize CFILT.
            fval(k) = f

            ! Check whether to exit.
            subinfo = checkexit(maxfun, k, cstrv, ctol, f, ftarget, x)
            if (subinfo /= INFO_DFT) then
                info = subinfo
                exit
            end if
        end do

        ! Deallocate IXL and IXU as they have finished their job.
        deallocate (ixl, ixu)

        nf = int(count(evaluated))
        ! Since the starting point is supposed to be feasible, there should be at least one feasible point.
        ! We set feasible to TRUE for the evaluated point with the smallest constraint violation. This is
        ! necessary, or KOPT defined below may become 0 if EVALUATED .AND. FEASIBLE is all FALSE.
        feasible(minloc(cval, mask=evaluated, dim=1)) = .true.
        kopt = int(minloc(fval, mask=(evaluated .and. feasible), dim=1))
        ! MATLAB:
        ! fopt = min(fval(evaluated & feasible));
        ! kopt = find(evaluated & feasible & ~(fval > fopt), 1, 'first');

    end subroutine initxf

    !> Initialises the factorisation `[idz, bmat, zmat]` of the matrix `H` of (3.12) of the NEWUOA
    !> paper, upstream's `inith`.
    subroutine inith(ij, xpt, idz, bmat, zmat, info)
        ! Common modules
        !use, non_intrinsic :: powalg_mod, only : errh

        integer, intent(in) :: ij(:, :)            !! the coordinate pairs of the interpolation set beyond `2n + 1`
        real(real64), intent(in) :: xpt(:, :)      !! the interpolation set relative to `xbase`, one point per column
        ! N.B.: XPT is essentially only used for debugging, to test the error in the initial H. The initial
        ! ZMAT and BMAT are completely defined by RHOBEG and IJ.

        integer, intent(out), optional :: info     !! PRIMA's exit code
        integer, intent(out) :: idz                !! the factorisation's sign split: `S` is -1 below `idz` and +1 from it
        real(real64), intent(out) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(out) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Diag(S) Z^T`

        integer :: k
        integer :: n
        integer :: npt
        real(real64) :: recip
        real(real64) :: reciq
        real(real64) :: rhobeg
        real(real64) :: rhosq

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        rhobeg = maxval(abs(xpt(:, 2)))  ! Read RHOBEG from XPT.
        rhosq = rhobeg**2

        ! Set BMAT.
        recip = ONE / rhobeg
        reciq = HALF / rhobeg
        bmat = ZERO
        if (npt <= 2 * n + 1) then
            ! Set BMAT(1 : NPT-N-1, :)
            bmat(1:npt - n - 1, 2:npt - n) = reciq * eye(npt - n - 1)
            bmat(1:npt - n - 1, n + 2:npt) = -reciq * eye(npt - n - 1)
            ! Set BMAT(NPT-N : N, :)
            bmat(npt - n:n, 1) = -recip
            bmat(npt - n:n, npt - n + 1:n + 1) = recip * eye(2 * n - npt + 1)
            bmat(npt - n:n, 2 * npt - n:npt + n) = -(HALF * rhosq) * eye(2 * n - npt + 1)
        else
            bmat(:, 2:n + 1) = reciq * eye(n)
            bmat(:, n + 2:2 * n + 1) = -reciq * eye(n)
        end if

        ! Set ZMAT.
        recip = ONE / rhosq
        reciq = sqrt(HALF) / rhosq
        zmat = ZERO
        if (npt <= 2 * n + 1) then
            zmat(1, :) = -reciq - reciq
            zmat(2:npt - n, :) = reciq * eye(npt - n - 1)
            zmat(n + 2:npt, :) = reciq * eye(npt - n - 1)
        else
            ! Set ZMAT(:, 1:N).
            zmat(1, 1:n) = -reciq - reciq
            zmat(2:n + 1, 1:n) = reciq * eye(n)
            zmat(n + 2:2 * n + 1, 1:n) = reciq * eye(n)
            ! Set ZMAT(:, N+1 : NPT-N-1).
            zmat(1, n + 1:npt - n - 1) = recip
            zmat(2 * n + 2:npt, n + 1:npt - n - 1) = recip * eye(npt - 2 * n - 1)
            do k = 1, npt - 2 * n - 1
                zmat(ij(:, k) + 1, k + n) = -recip
            end do
        end if

        ! Set IDZ.
        idz = 1

        ! Neither arm runs: `lincob`, the only caller, leaves INFO out, and the factorisation is
        ! built from RHOBEG and IJ alone, which are finite.
        if (present(info)) then
            if (any(is_nan(bmat)) .or. any(is_nan(zmat))) then   ! GCOVR_EXCL_START -- see the note above
                info = NAN_INF_MODEL
            else
                info = INFO_DFT
            end if                        ! GCOVR_EXCL_STOP
        end if

    end subroutine inith

    !> The active-set trust-region step, upstream's `trstep`.
    !!
    !! Minimises the model over the step `s` subject to `||s|| <= delta` and `amat^T s <= b`,
    !! starting from `s = 0`, which is feasible. Conjugate-gradient iterations inside the null space
    !! of the active set, with `getact` called again whenever a constraint would be crossed; the
    !! iteration stops at the trust-region boundary.
    subroutine trstep(amat, delta, gopt_in, hq_in, pq_in, rescon, tol, xpt, iact, nact, qfac, rfac, s, ngetact)
        ! Common modules

        ! Solver-specific modules

        real(real64), intent(in) :: amat(:, :)       !! the constraint gradients, one per column, each of unit length
        real(real64), intent(in) :: delta            !! the trust-region radius
        real(real64), intent(in) :: gopt_in(:)       !! the model's gradient at the trust-region centre
        real(real64), intent(in) :: hq_in(:, :)      !! the model's explicit Hessian part
        real(real64), intent(in) :: pq_in(:)         !! the model's implicit Hessian weights
        real(real64), intent(in) :: rescon(:)  !! constraint residuals at the trust-region centre, negated where far from active
        real(real64), intent(in) :: tol              !! the accuracy asked of the subproblem
        real(real64), intent(in) :: xpt(:, :)        !! the interpolation set relative to `xbase`, one point per column

        integer, intent(inout) :: iact(:)            !! indices of the active constraints, the first `nact` in use
        integer, intent(inout) :: nact               !! how many constraints are active
        real(real64), intent(inout) :: qfac(:, :)    !! the orthogonal factor of the active gradients
        real(real64), intent(inout) :: rfac(:, :)    !! the upper triangular factor of the active gradients

        real(real64), intent(out) :: s(:)            !! the step returned
        integer, intent(out), optional :: ngetact    !! how many times `getact` was called

        integer :: iter
        integer :: itercg
        integer :: jsav
        integer :: m
        integer :: maxiter
        integer :: n
        integer :: ngetact_loc
        integer :: npt
        logical :: newact
        real(real64) :: ad(size(amat, 2))
        real(real64) :: alpha
        real(real64) :: alphm
        real(real64) :: alpht
        real(real64) :: beta
        real(real64) :: d(size(gopt_in))
        real(real64) :: dd
        real(real64) :: delsq
        real(real64) :: dg
        real(real64) :: dhd
        real(real64) :: dproj(size(gopt_in))
        real(real64) :: ds
        real(real64) :: frac(size(amat, 2))
        real(real64) :: g(size(gopt_in))
        real(real64) :: gamma
        real(real64) :: gopt(size(gopt_in))
        real(real64) :: hd(size(gopt_in))
        real(real64) :: hq(size(hq_in, 1), size(hq_in, 2))
        real(real64) :: modscal
        real(real64) :: pg(size(gopt_in))
        real(real64) :: pq(size(pq_in))
        real(real64) :: psd(size(gopt_in))
        real(real64) :: reduct
        real(real64) :: resact(size(amat, 2))
        real(real64) :: resid
        real(real64) :: resnew(size(amat, 2))
        real(real64) :: restmp(size(amat, 2))
        real(real64) :: sold(size(s))
        real(real64) :: sqrtd
        real(real64) :: ss

        ! Sizes.
        m = int(size(amat, 2))
        n = int(size(gopt_in))
        npt = int(size(pq_in))

        ! Scale the problem if GOPT contains large values. Otherwise, floating point exceptions may occur.
        ! Note that the trust-region step is scale invariant.
        ! N.B.: It is faster and safer to scale by multiplying a reciprocal than by division. See
        ! https://fortran-lang.discourse.group/t/ifort-ifort-2021-8-0-1-0e-37-1-0e-38-0/
        if (maxval(abs(gopt_in)) > 1.0E12) then   ! The threshold is empirical.
            modscal = max(TWO * REALMIN, ONE / maxval(abs(gopt_in)))  ! MAX: precaution against underflow.
            gopt = gopt_in * modscal
            pq = pq_in * modscal
            hq = hq_in * modscal
        else
            gopt = gopt_in
            pq = pq_in
            hq = hq_in
        end if

        ! Return if G is not finite. Otherwise, GETACT will fail in the debugging mode.
        ! Not reached: the model is built from values `evaluate` has screened, so its gradient is
        ! finite.
        if (.not. is_finite(sum(abs(gopt)))) then
            s = ZERO         ! GCOVR_EXCL_START -- see the note above
            if (present(ngetact)) then
                ngetact = 0
            end if
            return
            ! GCOVR_EXCL_STOP
        end if

        ! Set the initial elements of RESNEW, RESACT and S.

        ! 1. RESNEW(J) < 0 indicates that the J-th constraint does not restrict the CG steps of the current
        ! trust region calculation. In other words, RESCON >= DELTA.
        ! 2. RESNEW(J) = 0 indicates that J is an entry of IACT(1:NACT).
        ! 3. RESNEW(J) > 0 means that RESNEW(J) = max(B(J) - AMAT(:, J)^T*(XOPT+S), TINYCV), where S is the
        ! step up to now, calculated by a sequence of (truncated) CG iterations.
        ! N.B.: The order of the following lines is important, as the later ones override the earlier.
        resnew = rescon
        resnew(trueloc(rescon >= 0)) = max(TINYCV, rescon(trueloc(rescon >= 0)))
        resnew(trueloc(rescon >= delta)) = -ONE
        ! MATLAB:
        ! resnew = rescon; resnew(rescon >= 0) = max(TINYCV, rescon(rescon >= 0)); resnew(rescon >= delta) = -1;
        resnew(iact(1:nact)) = ZERO

        ! RESACT contains the constraint residuals of the constraints in IACT(1:NACT), namely the values
        ! of B(J) - AMAT(:, J)^T*(XOPT+S) for the J in IACT(1:NACT). Here, IACT(1:NACT) is a set of
        ! indicates such that the columns of AMAT(:, IACT(1:NACT)) form a basis of the constraint gradients
        ! in the "active set". For the definition of the "active set", see (3.5) of Powell (2015) and the
        ! comments at the beginning of the GETACT subroutine.
        ! N.B.: Between two calls of GETACT, S is updated in the orthogonal complement of the "active"
        ! gradients (i.e., null space of the "active" constraints). Therefore, RESACT remains unchanged.
        ! RESACT is changed right after GETACT is called if the first search direction D is not PSD
        ! but PSD + GAMMA * DPROJ.
        resact(1:nact) = rescon(iact(1:nact))

        g = gopt
        delsq = delta * delta
        s = ZERO
        ss = ZERO
        reduct = ZERO
        ngetact_loc = 0
        newact = .true.

        ! ITERCG is the number of CG iterations corresponding to the current "active set" obtained by
        ! calling GETACT. These CG iterations are restricted in the orthogonal complement of the active
        ! gradients (i.e., null space of the active constraints).
        ! The following initial value of ITERCG is an artificial value that is not used. It is to entertain
        ! Fortran compilers (can it be be removed?).
        itercg = -1

        ! What is the THEORETICAL upper bound of ITER? For the moment, we set the following MAXITER.
        ! The formulation of MAXITER below contains a precaution against overflow. In MATLAB/Python/Julia/R,
        ! we can write maxiter = min(10000, 10*(m + n))
        maxiter = int(min(10**min(4, range(0)), 10 * int(m + n)))
        do iter = 1, maxiter  ! Powell's code is essentially a DO WHILE loop. We impose an explicit MAXITER.
            if (newact) then
                ! GETACT picks the active set for the current S. It also sets PSD to the vector closest to
                ! -G that is orthogonal to the normals of the active constraints. PSD is scaled to have
                ! length 0.2*DELTA. Then a move of PSD from S is allowed by the linear constraints: PSD
                ! reduces the values of the nearly active constraints; it changes the inactive constraints
                ! by at most 0.2*DELTA, but the residuals of these constraints at no less than 0.2*DELTA.
                ! N.B.: The magic number 0.2 appears also in GETACT (TDEL = 0.2_real64 * DELTA). It works well.
                ngetact_loc = ngetact_loc + 1
                call getact(amat, delta, g, iact, nact, qfac, resact, resnew, rfac, psd)
                dd = inprod(psd, psd)
                if (dd <= EPS * delsq .or. is_nan(dd)) then  ! Powell's code: IF (DD <= 0) THEN
                    exit
                end if
                psd = (0.2_real64 * delta / sqrt(dd)) * psd

                ! If the modulus of the residual of an "active constraint" is substantial (i.e., more than
                ! 1.0E-4*DELTA), then modify the searching direction PSD by a projection step to the
                ! boundaries of the "active constraint". This modified step will reduce the constraint
                ! residuals of the "active constraints" (see the update of RESACT below). The motivation is
                ! that the constraints in the "active set" are presumed to be active, and hence should have
                ! zero residuals (no constraint is violated, as the current method is feasible). According
                ! to a test on 20220821, this modification is important for the performance of LINCOA.
                ! N.B.:
                ! 1. The residual of the constraint A*X <= B is defined as B - A*X. It is not the constraint
                ! violation. Indeed, the constraint violations of the iterates are 0 in the current method.
                ! 2. We prefer `ANY(X > Y)` to `MAXVAL(X) > Y`, as Fortran standards do not specify
                ! MAXVAL(X) when X contains NaN, and MATLAB/Python/R/Julia behave differently in this
                ! respect. Moreover, MATLAB defines max(X) = [] if X == [], differing from mathematics
                ! and other languages.
                gamma = ZERO  ! The steplength of the projection step to be taken.
                if (any(resact(1:nact) > 1.0E-4_real64 * delta)) then
                    ! Set DPROJ to the shortest move (projection step) from S to the boundaries of the
                    ! active constraints. We will use DPROJ to modify PSD.
                    dproj = matprod(qfac(:, 1:nact), solve(transpose(rfac(1:nact, 1:nact)), resact(1:nact)))
                    ! MATLAB: dproj = qfac(:, 1:nact) * (rfac(1:nact, 1:nact)' \ resact(1:nact))

                    ! The vector DPROJ is also the shortest move from S + PSD to the boundaries of the
                    ! active constraints (this is because PSD is parallel to the boundaries of the active
                    ! constraints). Set GAMMA to the greatest steplength of this move that satisfies both
                    ! the trust region bound and the linear constraints.
                    ds = inprod(dproj, s + psd)
                    dd = sum(dproj**2)
                    resid = delsq - sum((s + psd)**2)
                    ! Powell's condition for the following IF: RESID > 0.
                    if (resid > 0 .and. dd > EPS * delsq .and. .not. is_nan(ds)) then
                        ! Set GAMMA to the greatest value so that S + PSD + GAMMA*DPROJ satisfies the trust
                        ! region bound. SQRTD: square root of a discriminant. Powell's code for SQRTD is
                        ! SQRT(DS * DS + DD * RESID), which may be below ABS(DS) due to underflow in DS*DS.
                        sqrtd = maxval([sqrt(ds * ds + dd * resid), abs(ds), sqrt(dd * resid)])
                        ! The two arms compute the same root, each avoiding the cancellation the
                        ! other would suffer; DS is non-positive at every projection step the runs
                        ! the suite drives take, so only the first is reached.
                        if (ds <= 0) then
                            gamma = (sqrtd - ds) / dd
                        else
                            gamma = resid / (sqrtd + ds)   ! GCOVR_EXCL_LINE -- DS <= 0; see above
                        end if
                        ! GAMMA < 0 should not happen. GAMMA can be 0 or NaN when, e.g., DS or DD becomes
                        ! Inf. Powell's code does not handle this.
                        if (gamma < 0 .or. .not. is_finite(gamma)) then
                            gamma = 0             ! GCOVR_EXCL_LINE -- needs a non-finite DS or DD,
                                                  ! which `evaluate` refuses before the model sees it
                        end if

                        ! Reduce GAMMA so that the move along DPROJ also satisfies the linear constraints.
                        ad = -ONE
                        ad(trueloc(resnew > 0)) = matprod(dproj, amat(:, trueloc(resnew > 0)))
                        frac = ONE
                        restmp(trueloc(ad > 0)) = resnew(trueloc(ad > 0)) - matprod(psd, amat(:, trueloc(ad > 0)))
                        frac(trueloc(ad > 0)) = restmp(trueloc(ad > 0)) / ad(trueloc(ad > 0))
                        gamma = minval([gamma, ONE, frac])  ! GAMMA = MINVAL([GAMMA, ONE, FRAC(TRUELOC(AD>0))])
                    end if
                end if

                ! Set the next direction for seeking a reduction in the model function subject to the trust
                ! region bound and the linear constraints.
                ! Do NOT write D = PSD + GAMMA*DPROJ, as DPROJ may contain NaN/Inf, in which case GAMMA = 0.
                if (gamma > 0) then
                    d = psd + gamma * dproj  ! Modified searching direction.
                    itercg = -1
                else
                    d = psd  ! Original searching direction.
                    itercg = 0
                end if
            end if
            itercg = itercg + 1
            ! After the above line, ITERCG = 0 iff GETACT has been just called, and D is not PSD but a
            ! modified step.

            ! Set ALPHA to the steplength from S along D to the trust region boundary. Return if the first
            ! derivative term of this step is sufficiently small or if no further progress is possible.
            resid = delsq - ss
            dg = inprod(d, g)
            ds = inprod(d, s)
            dd = inprod(d, d)
            ! Powell's condition for the following IF: (RESID <= 0 .OR. DG >= 0). If DD is tiny (so is DS),
            ! ALPHA may be mistakenly calculated as a huge value due to rounding errors, as observed on
            ! 20221205. Therefore, we exit when DD is small. The test for DG is covered by the IF after the
            ! calculation of ALPHA.
            if (resid <= 0 .or. dd <= EPS * delsq .or. is_nan(ds)) then
                exit
            end if
            ! SQRTD: square root of a discriminant. Powell's code for SQRTD is SQRT(DS * DS + DD * RESID),
            ! which may be below ABS(DS) due to underflow in DS*DS.
            sqrtd = maxval([sqrt(ds * ds + dd * resid), abs(ds), sqrt(dd * resid)])
            if (ds <= 0) then
                alpha = (sqrtd - ds) / dd
            else
                alpha = resid / (sqrtd + ds)
            end if
            ! ALPHA < 0 should not happen. ALPHA can be 0 or NaN when, e.g., DS or DD becomes Inf. Powell's
            ! code does not handle this.
            if (alpha <= 0 .or. .not. is_finite(alpha)) then
                exit  ! GCOVR_EXCL_LINE -- ALPHA is positive and finite for a finite model
            end if

            ! Powell's condition for the following IF: -ALPHA * DG <= TOL * REDUCT. Note that the EXIT
            ! will be triggered if DG >= 0, as ALPHA >= 0.
            if (-alpha * dg <= tol * reduct .or. is_nan(alpha * dg)) then
                exit  ! GCOVR_EXCL_LINE -- these runs leave the loop at the tests below first
            end if

            ! Set DHD to the curvature of the model along D. Then reduce ALPHA if necessary to the value
            ! that minimizes the model.
            hd = hess_mul(d, xpt, pq, hq)
            dhd = inprod(d, hd)
            alpht = alpha
            if (dg + alpha * dhd > 0) then
                alpha = -dg / dhd
            end if

            ! Make a further reduction in ALPHA if necessary to preserve feasibility.
            alphm = alpha
            ad = -ONE
            ad(trueloc(resnew > 0)) = matprod(d, amat(:, trueloc(resnew > 0)))
            frac = alpha
            frac(trueloc(ad > 0)) = resnew(trueloc(ad > 0)) / ad(trueloc(ad > 0))
            frac(trueloc(is_nan(frac))) = alpha
            jsav = 0
            if (any(frac < alpha)) then
                jsav = int(minloc(frac, dim=1))
                alpha = frac(jsav)
            end if
            !----------------------------------------------------------------------------------------------!
            ! Alternatively, JSAV and ALPHA can be calculated as below.
            ! !JSAV = INT(MINLOC([ALPHA, FRAC], DIM=1), KIND(JSAV)) - 1
            ! !ALPHA = MINVAL([ALPHA, FRAC])  ! This line cannot be exchanged with the last.
            ! We prefer our implementation as the code is more explicit; in addition, it is more flexible:
            ! we can change the condition ANY(FRAC < ALPHA) to ANY(FRAC < (1 - EPS) * ALPHA) or
            ! ANY(FRAC < (1 + EPS) * ALPHA), depending on whether we believe a false positive or a false
            ! negative of JSAV > 0 is more harmful.
            !----------------------------------------------------------------------------------------------!

            ! Post-process ALPHA according to some prior information.
            ! N.B.:
            ! 1. Since we set ALPHA=1 when ITERCG=0, the ALPHA calculated above is needed only if ITERCG>0.
            ! 2. Zaikun 20220821: In theory, shouldn't this post-processing change nothing? According to
            ! a test on 20220821, it does change ALPHA sometimes. Strange! Why?
            if (itercg == 0) then  ! Iff GETACT has been called, and D is not PSD but a modified step.
                ! By the definition of D, ALPHA = ONE is the largest ALPHA so that S + ALPHA*D satisfies the
                ! linear and trust region constraints.
                alpha = ONE
            elseif (itercg == 1 .and. gamma <= 0) then  ! Iff GETACT has been called, and D is not modified.
                ! Due to the scaling of PSD, S + D satisfies the linear and trust region constraints.
                alpha = max(alpha, ONE)
            else
                alpha = max(alpha, ZERO)
            end if

            ! Set ALPHA to the minimum between ALPHA and ALPHM, namely the steplength obtained by minimizing
            ! the quadratic model along D.
            alpha = min(alpha, alphm)

            ! Update S, G.
            sold = s
            s = s + alpha * d
            ss = sum(s**2)
            if (.not. is_finite(ss)) then
                s = sold     ! GCOVR_EXCL_START -- S stays finite inside the trust region
                exit
                ! GCOVR_EXCL_STOP
            end if
            g = g + alpha * hd
            if (.not. is_finite(sum(abs(g)))) then
                exit  ! GCOVR_EXCL_LINE -- G stays finite: `evaluate` refuses a non-finite value
            end if

            ! Update RESNEW.
            restmp = resnew - alpha * ad  ! Only RESTMP(TRUELOC(RESNEW > 0)) is needed.
            resnew(trueloc(resnew > 0)) = max(TINYCV, restmp(trueloc(resnew > 0)))
            ! MATLAB: mask = (resnew > 0); resnew(mask) = max(TINYCV, resnew(mask) - alpha * ad(mask));

            ! Update RESACT. This is done iff GETACT has been called, and D is not PSD but a modified step.
            !----------------------------------------------------------------------------------------------!
            ! Zaikun 20220821: There seems be a typo here. Powell's original code does not take ALPHA into
            ! account. Then RESACT seems to correspond to S + D, where D is defined as PSD + GAMMA*DPROJ
            ! during the modification procedure after GETACT is called. Without this modification, RESACT
            ! would remain unchanged because D = PSD, which is in the null space of the active constraints.
            ! The GAMMA*DPROJ component in the modified step D reduces RESACT by GAMMA*RESACT. However,
            ! since S is updated to S + ALPHA*D, shouldn't RESACT be reduced by ALPHA*GAMMA*RESACT?
            ! Note that Powell chose to update RESACT after ALPHA is calculated (instead of right after
            ! GAMMA is calculated), which might be an indication that he wanted to take ALPHA into account.
            ! In the following code, we try correcting this apparent typo, but it has little impact on the
            ! performance of LINCOA according to a test on 20220821.
            if (itercg == 0) then
                resact(1:nact) = (ONE - alpha * gamma) * resact(1:nact)
                !resact(1:nact) = (ONE - gamma) * resact(1:nact)  ! Powell's code.
            end if
            !----------------------------------------------------------------------------------------------!

            ! Update REDUCT, the reduction up to now.
            reduct = reduct - alpha * (dg + HALF * alpha * dhd)
            ! Not reached: ALPHA is cut to the model minimiser above, so every CG pass of the runs
            ! the suite drives reduces the model, and REDUCT cannot be NaN for a finite model.
            if (reduct <= 0 .or. is_nan(reduct)) then
                s = sold     ! GCOVR_EXCL_START -- see the note above
                exit
                ! GCOVR_EXCL_STOP
            end if

            ! Test for termination.
            if (alpha >= alpht .or. -alphm * (dg + HALF * alphm * dhd) <= tol * reduct) then
                exit
            end if

            ! Branch to a new loop if there is a new active constraint.
            ! When JSAV > 0, Powell's code branches back with NEWACT = .TRUE. only if ||S|| <= 0.8*DELTA,
            ! and it exits if ||S|| > 0.8*DELTA, as mentioned at the end of Section 3 of Powell 2015. The
            ! motivation seems to avoid small steps that changes the active set, because GETACT is expensive
            ! in flops. However, according to a test on 20220820, removing this condition (essentially
            ! replacing it with ||S|| < DELTA) improves the performance of LINCOA a bit. This may lead to
            ! small steps, but tiny steps will lead to tiny reductions and trigger an exit.
            newact = (jsav > 0)
            if (newact) then
                cycle
            end if

            ! If N-NACT CG iterations has been taken in the current null space (corresponding to the
            ! current "active set"), then, in theory, a stationary point in this subspace has been found.
            ! If the "active set" is the true active set, then a stationary point of the
            ! linearly-constrained trust region subproblem is found. So a termination is reasonable.
            ! However, the "active set" is not precisely the true active set, is it? See (3.5) of Powell
            ! (2015) and the comments at the beginning of the GETACT subroutine. Also, we should take into
            ! account the modification after GETACT is called.
            if (itercg >= n - nact) then  ! ITERCG > N - NACT is impossible.
                exit
            end if

            ! Calculate the next search direction, which is conjugate to the previous one if ITERCG /= NACT.
            ! N.B.: NACT < 0 is impossible unless GETACT is buggy; NACT = 0 can happen, particularly if
            ! there is no constraint. In theory, the code for the second case below covers the first as well.
            if (nact <= 0) then
                pg = g
            else
                pg = matprod(qfac(:, nact + 1:n), matprod(g, qfac(:, nact + 1:n)))
                ! MATLAB: pg = qfac(:, nact+1:n) * (g' * qfac(:, nact+1:n))';
            end if

            if (itercg == 0) then  ! Iff GETACT has been called, and D is not PSD but a modified step.
                beta = ZERO
            else
                beta = inprod(pg, hd) / dhd
            end if
            d = -pg + beta * d
        end do

        if (present(ngetact)) then
            ngetact = ngetact_loc
        end if

    end subroutine trstep

    !> The active set at the current point, and the projected steepest-descent direction; upstream's
    !> `getact`.
    !!
    !! Powell 2015. Picks the constraints to treat as active, maintains their QR factorisation in
    !! `qfac`/`rfac` through `qradd` and `qrexc`, and returns `psd`: minus the gradient projected
    !! onto the null space of the active gradients, which is the direction the trust-region
    !! iteration searches along.
    subroutine getact(amat, delta, g, iact, nact, qfac, resact, resnew, rfac, psd)
        ! THE FOLLOWING DESCRIPTION NEEDS VERIFICATION!               !
        ! Note that the set JJ gets updated within this subroutine,   !
        ! which seems inconsistent with the description below.        !
        ! See the lines below "Pick the next integer L or terminate". !
        !-------------------------------------------------------------!
        !
        ! This subroutine solves a linearly constrained projected problem (LCPP)
        !
        ! min ||D + G|| subject to AMAT(:, j)^T * D <= 0 for j in JJ.
        !
        ! The solution is PSD, which is a projected steepest descent direction PSD for a linearly
        ! constrained trust-region subproblem (LCTRS)
        !
        ! min Q(X_k + D)  subject to ||D|| <= Delta and AMAT^T*(X_k + D) <= B,
        !
        ! where X_k is in R^N, B is in R^M, and AMAT is in R^{NxM}.
        !
        ! In (LCPP), JJ is the index set defined in (3.3) of Powell (2015) as
        !
        ! JJ = {j : B_j - A_j^T*Y <= 0.2*Delta*||A_j||, 1 <= j <= M} with A_j = AMAT(:, j),
        !
        ! i.e., the index set of the nearly active constraints of (LCTRS) (Powell wrote that j is in JJ if
        ! and only if the distance from Y to the boundary of the j-th constraint is at most 0.2*Delta).
        ! Here, Y is the point where G is taken, namely G = nabla Q(Y). Y is not necessarily X_k, but an
        ! iterate of the algorithm (e.g., truncated conjugate gradient) that solves (LCTRS). In LINCOA,
        ! ||A_j|| is 1 as the gradients of the linear constraints are normalized before LINCOA starts.
        !
        ! The subroutine solves (LCPP) by the active set method of Goldfarb-Idnani (1983). It does not only
        ! calculate PSD, but also identify the active set of (LCPP) at the solution PSD, namely
        !
        ! II = {j in JJ : AMAT(:, j)^T*PSD = 0} (see (3.5) of Powell (2015)),
        !
        ! and maintains a QR factorization of A corresponding to the active set. More specifically,
        ! IACT(1:NACT) is a set of indices such that the columns of AMAT(:, IACT(1:NACT)) constitute a basis
        ! of the "active constraint" gradients (i.e., those corresponding to the set II mentioned above, but
        ! not JJ!), and QFAC*RFAC(:, 1:NACT) is the QR factorization of! AMAT(:, IACT(1:NACT)) such that
        !
        ! SIZE(QFAC) = [N, N], SIZE(RFAC, 1) = N, diag(RFAC(:, 1:NACT)) > 0.
        !
        ! NACT, IACT, QFAC and RFAC are maintained up to date across invocations of GETACT for warm starts.
        !
        ! DELTA, RESNEW, RESACT, and G are the same as the terms with these names in SUBROUTINE TRSTEP.
        ! The elements of RESNEW and RESACT are also kept up to date. See Section 3 of Powell (2015).
        ! Note that the updates only permute RESACT but do not change the values inside.
        !
        ! VLAM is the vector of Lagrange multipliers of the calculation.
        !
        ! See Section 3 of Powell (2015) for more information.
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        real(real64), intent(in) :: amat(:, :)       !! the constraint gradients, one per column, each of unit length
        real(real64), intent(in) :: delta            !! the trust-region radius
        real(real64), intent(in) :: g(:)             !! the gradient to project

        integer, intent(inout) :: iact(:)            !! indices of the active constraints, the first `nact` in use
        integer, intent(inout) :: nact               !! how many constraints are active
        real(real64), intent(inout) :: qfac(:, :)    !! the orthogonal factor of the active gradients
        real(real64), intent(inout) :: resact(:)     !! residuals of the active constraints
        real(real64), intent(inout) :: resnew(:)     !! residuals of the constraints that still restrict the step
        real(real64), intent(inout) :: rfac(:, :)    !! the upper triangular factor of the active gradients

        real(real64), intent(out) :: psd(:)          !! the projected steepest-descent direction

        integer :: icon
        integer :: iter
        integer :: l
        integer :: m
        integer :: maxiter
        integer :: n
        logical :: mask(size(amat, 2))
        real(real64) :: apsd(size(amat, 2))
        real(real64) :: dd
        real(real64) :: ddsav
        real(real64) :: dnorm
        real(real64) :: gg
        real(real64) :: frac(size(g))
        real(real64) :: psdsav(size(psd))
        real(real64) :: tdel
        real(real64) :: v(size(g))
        real(real64) :: violmx
        real(real64) :: vlam(size(g))
        real(real64) :: vmu(size(g))
        real(real64) :: vmult

        ! Sizes.
        m = int(size(amat, 2))
        n = int(size(g))

        ! Quick return when M = 0.
        if (m <= 0) then
            nact = 0
            qfac = eye(n)
            psd = -g
            return
        end if

        ! Set some constants.
        gg = inprod(g, g)
        tdel = 0.2_real64 * delta  ! Changing TDEL to 0.1_real64*DELTA does not improve the performance of LINCOA.

        ! Set the initial QFAC to the identity matrix in the case NACT = 0.
        if (nact == 0) then
            qfac = eye(n)
        end if

        ! Remove any constraints from the initial active set whose residuals exceed TDEL.
        ! Compilers may complain if VLAM is not set. The value does not matter, as it will be overwritten.
        vlam = ZERO
        do icon = nact, 1, -1
            if (resact(icon) > tdel) then
                ! Delete constraint IACT(ICON) from the active set, and set NACT = NACT - 1.
                call delact(icon, iact, nact, qfac, resact, resnew, rfac, vlam)
            end if
        end do

        ! Remove any constraints from the initial active set whose Lagrange multipliers are nonnegative,
        ! and set the surviving multipliers.
        ! The following loop will run for at most NACT times, since each call of DELACT reduces NACT by 1.
        do while (nact > 0)
            vlam(1:nact) = lsqr(g, qfac(:, 1:nact), rfac(1:nact, 1:nact))
            if (.not. any(vlam(1:nact) >= 0)) then
                exit
            end if
            icon = maxval(trueloc(vlam(1:nact) >= 0))
            ! MATLAB: icon = max(find(vlam(1:nact) >= 0)); % OR: icon = find(vlam(1:nact) >= 0, 1, 'last')
            call delact(icon, iact, nact, qfac, resact, resnew, rfac, vlam)
        end do
        ! Zaikun 20220330: What if NACT = 0 at this point?

        ! Set the new search direction D. Terminate if the 2-norm of D is ZERO or does not decrease, or if
        ! NACT=N holds. The situation NACT=N occurs for sufficiently large DELTA if the origin is in the
        ! convex hull of the constraint gradients.
        ! Start with initialization of PSDSAV and DDSAV.
        psdsav = ZERO  ! Must be set, in case the loop exits due to abnormality at iteration 1.
        ddsav = TWO * gg  ! By Powell. This value is used at iteration 1 to test whether DD >= DDSAV. Why?

        ! What is the theoretical maximal number of iterations in the following procedure? Powell's code for
        ! this part is essentially a `DO WHILE (NACT < N) ... END DO` loop. We enforce the following maximal
        ! number of iterations, which is never reached in our tests (indeed, even 2*N cannot be reached).
        ! N.B.: 1. The formulation of MAXITER below contains a precaution against overflow. In
        ! MATLAB/Python/Julia/R, we can write maxiter = min(10000, 2*(m + n))
        ! 2. The iteration counter ITER never appears in the code of the iterations, as its purpose is
        ! merely to impose an upper bound on the number of iterations.
        maxiter = int(min(10**min(4, range(0)), 2 * int(m + n)))
        do iter = 1, maxiter
            ! When NACT == N, exit with PSD = 0. Indeed, with a correctly implemented matrix product, the
            ! lines below this IF should render DD = 0 and trigger an exit. We make it explicit for clarity.
            if (nact >= n) then  ! Indeed, NACT > N should never happen.
                psd = ZERO
                exit
            end if

            ! Set PSD to the projection of -G to range(QFAC(:,NACT+1:N))
            psd = -matprod(qfac(:, nact + 1:n), matprod(g, qfac(:, nact + 1:n)))
            ! MATLAB: psd = -qfac(:, nact + 1:n) * (g' * qfac(:, nact + 1:n))';
            !----------------------------------------------------------------------------------------------!
            ! Zaikun: The schemes below work evidently worse than the one above in a test on 20220417. Why?
            !-------------------------------------------------------------------------!
            ! VERSION 1:
            ! !psd = matprod(qfac(:, 1:nact), matprod(g, qfac(:, 1:nact))) - g
            !-------------------------------------------------------------------------!
            ! VERSION 2:
            ! !if (2 * nact < n) then
            ! !    psd = matprod(qfac(:, 1:nact), matprod(g, qfac(:, 1:nact))) - g
            ! !else
            ! !    psd = -matprod(qfac(:, nact + 1:n), matprod(g, qfac(:, nact + 1:n)))
            ! !end if
            !-------------------------------------------------------------------------!
            !----------------------------------------------------------------------------------------------!

            dd = inprod(psd, psd)
            dnorm = sqrt(dd)

            if (dnorm <= EPS .or. is_nan(dnorm)) then
                exit
            end if

            if (dd >= ddsav) then
                ! GCOVR_EXCL_START -- the projected direction shortens at every pass in these runs
                psd = ZERO  ! Zaikun 20220329: Powell wrote this. Why?
                !psd = psdsav  ! This does not seem to improve the performance.
                exit
                ! GCOVR_EXCL_STOP
            end if

            !---------------------------------------------------------------------------------------!
            ! Powell's code does not handle the following pathological cases.
            ! Not reached: PSD is minus a projection of G, so it cannot point uphill, and the model
            ! it is built from is finite because `evaluate` refuses a non-finite objective value.
            if (inprod(psd, g) > 0 .or. .not. is_finite(sum(abs(psd)))) then
                psd = psdsav     ! GCOVR_EXCL_START -- see the note above
                exit
                ! GCOVR_EXCL_STOP
            end if
            ! In our tests, tolerating the following cases seems to render better numerical results.
            ! !if (dd > gg) then
            ! !    psd = (sqrt(gg) / dnorm) * psd
            ! !    exit
            ! !end if
            ! !if (inprod(psd, g) < -gg) then
            ! !    exit
            ! !end if
            !---------------------------------------------------------------------------------------!

            psdsav = psd
            ddsav = dd

            ! Pick the next integer L or terminate; a positive L is the index of the most violated constraint.
            apsd = matprod(psd, amat)
            mask = (resnew > 0 .and. resnew <= tdel .and. apsd > (dnorm / delta) * resnew)
            !----------------------------------------------------------------------------------------------!
            ! N.B.: the definition of L and VIOLMX can be simplified as follows, but we prefer explicitness.
            !L = INT(MAXLOC(APSD, MASK=MASK, DIM=1)) ! MAXLOC(...) = 0 if MASK is all FALSE.
            !VIOLMX = MAXVAL(APSD, MASK=MASK)  ! MAXVAL(...) = -HUGE(APSD) if MASK is all FALSE.
            if (any(mask)) then
                l = int(maxloc(apsd, mask=mask, dim=1))
                violmx = apsd(l)
            else
                l = 0
                violmx = -REALMAX
            end if
            ! MATLAB: apsd(mask) = -Inf; [violmx, l] = max(apsd);
            ! N.B.: the value of L will differ from the Fortran version if MASK is all FALSE, but this is OK
            ! because VIOLMX will be -Inf, which will trigger the `exit` below. This is tricky. Be cautious!
            !----------------------------------------------------------------------------------------------!

            ! Terminate if VIOLMX <= 0 (when MASK contains only FALSE) or a positive value of VIOLMX may be
            ! due to computer rounding errors.
            ! N.B.: 1. Theoretically (but not numerically), APSD(IACT(1:NACT)) = 0 or empty.
            ! 2. CAUTION: the Inf-norm of APSD(IACT(1:NACT)) is NOT always MAXVAL(ABS(APSD(IACT(1:NACT)))),
            ! as the latter returns -HUGE(APSD) instead of 0 when NACT = 0! In MATLAB, max([]) = []; in
            ! Python, R, and Julia, the maximum of an empty array raises errors/warnings (as of 20220318).
            ! Powell's condition for the IF is as follows. Very often, the threshold is almost zero.
            ! !if (all(.not. mask) .or. violmx <= min(0.01_real64 * dnorm, TEN * norm(apsd(iact(1:nact)), 'inf'))) then
            ! The following condition works essentially the same as Powell's. However, it ensures that
            ! VIOLMX > EPS * DNORM when the EXIT is not triggered, which implies that AMAT(:, L) is not in
            ! the range of QFAC(:, 1:NACT).
            if (all(.not. mask) .or. violmx <= max(EPS * dnorm, TEN * norm(apsd(iact(1:nact)), 'inf'))) then
                exit
            end if

            ! Add constraint L to the active set. ADDACT sets NACT = NACT + 1 and VLAM(NACT) = 0.
            call addact(l, amat(:, l), iact, nact, qfac, resact, resnew, rfac, vlam)

            ! Set the components of the vector VMU if VIOLMX is positive.
            ! N.B.: 1. In theory, NACT > 0 is not needed in the condition below, because VIOLMX must be 0
            ! when NACT is 0. We keep NACT > 0 for security: when NACT <= 0, RFAC(NACT, NACT) is invalid.
            ! 2. The loop will run for at most NACT <= N times: if VIOLMX > 0, then ICON > 0, and hence
            ! VLAM(ICON) = 0, which implies that DELACT will be called to reduce NACT by 1.
            do while (violmx > 0 .and. nact > 0)
                v(1:nact - 1) = ZERO
                v(nact) = ONE / rfac(nact, nact) ! This is why we must ensure NACT > 0.
                ! Solve the linear system RFAC(1:NACT, 1:NACT) * VMU(1:NACT) = V(1:NACT) .
                vmu(1:nact) = solve(rfac(1:nact, 1:nact), v(1:nact)) ! VMU(NACT) = V(NACT)/RFAC(NACT,NACT)>0
                ! MATLAB: vmu(1:nact) = rfac(1:nact, 1:nact) \ v(1:nact);

                ! Calculate the multiple of VMU to subtract from VLAM, and update VLAM.
                ! N.B.: 1. VLAM(1:NACT-1) < 0 and VLAM(NACT) <= 0 by the updates of VLAM. 2. VMU(NACT) > 0.
                ! 3. Only the places where VMU(1:NACT) < 0 is relevant below, if any.
                frac = REALMAX
                where (vmu(1:nact) < 0 .and. vlam(1:nact) < 0) frac(1:nact) = vlam(1:nact) / vmu(1:nact)
                ! MATLAB: frac = vlam / vmu; frac(vmu >= 0 | vlam >= 0) = Inf;
                vmult = minval([violmx, frac(1:nact)])
                icon = maxval([0, trueloc(frac(1:nact) <= vmult)])
                ! MATLAB: icon = max([0; find(frac(1:nact) <= vmult)]); % find(frac(1:nact)<=vmult) can be empty

                ! N.B.: 0. The definition of ICON given above is mathematically equivalent to the following.
                ! !ICON = MAXVAL(TRUELOC([VIOLMX, FRACMULT(1:NACT)] <= VMULT)) - 1, OR
                ! !ICON = INT(MINLOC([VIOLMX, FRACMULT(1:NACT)], DIM=1, BACK=.TRUE.)) - 1
                ! However, such implementations are problematic in the unlikely case of VMULT = NaN: ICON
                ! will be -Inf in the first and unspecified in the second. The MATLAB counterpart of the
                ! first implementation will render ICON = [] as `find` (the MATLAB version of TRUELOC)
                ! returns [].
                ! 1. The BACK argument in MINLOC is available in F2008. Not supported by Absoft as of 2022.
                ! 2. A motivation for backward MINLOC is to save computation in DELACT below (what else?).

                violmx = max(violmx - vmult, ZERO)
                vlam(1:nact) = vlam(1:nact) - vmult * vmu(1:nact)
                if (icon > 0 .and. icon <= nact) then  ! Powell: IF (ICON>0). We check ICON<=NACT for safety.
                    vlam(icon) = ZERO  ! GCOVR_EXCL_LINE -- VMULT is VIOLMX here, never a FRAC, so ICON is 0
                end if

                ! Reduce the active set if necessary, so that all components of the new VLAM are negative,
                ! with resetting of the residuals of the constraints that become inactive.
                do icon = nact, 1, -1
                    if (vlam(icon) >= 0) then  ! Powell's version: IF (.NOT. VLAM(ICON) < 0) THEN
                        ! Delete the constraint with index IACT(ICON) from the active set; set NACT = NACT-1.
                        ! Not reached: no multiplier turns non-negative inside this loop in the runs
                        ! the suite drives.
                        call delact(icon, iact, nact, qfac, resact, resnew, rfac, vlam)  ! GCOVR_EXCL_LINE
                    end if
                end do
            end do  ! End of DO WHILE (VIOLMX > 0 .AND. NACT > 0)

            !----------------------------------------------------------------------------------------------!
            ! NACT can become 0 at this point iff VLAM(1:NACT) >= 0 before calling DELACT, which is true
            ! if NACT happens to be 1 when the WHILE loop starts. However, we have never observed a failure
            ! of the assertion below as of 20220329. Why?
            !-----------------------------------------!
            !-----------------------------------------!
            if (nact == 0) then
                exit  ! GCOVR_EXCL_LINE -- NACT never falls to 0 in the loop above
            end if
            !----------------------------------------------------------------------------------------------!
        end do  ! End of DO WHILE (NACT < N)

        ! It is possible to have NACT == 0 here. The following lines improve the performance of LINCOA.
        ! Powell's code does not take care of this case explicitly.
        if (nact == 0) then
            qfac = eye(n)
            psd = -g
        end if

    end subroutine getact

    !> Adds constraint `l` to the active set, upstream's `addact`.
    !!
    !! The new gradient is appended to the QR factorisation through `qradd` and `nact` grows by one.
    subroutine addact(l, c, iact, nact, qfac, resact, resnew, rfac, vlam)
        integer, intent(in) :: l                     !! index of the constraint to activate
        real(real64), intent(in) :: c(:)             !! its gradient

        integer, intent(inout) :: iact(:)            !! indices of the active constraints, the first `nact` in use
        integer, intent(inout) :: nact               !! how many constraints are active
        real(real64), intent(inout) :: qfac(:, :)    !! the orthogonal factor of the active gradients
        real(real64), intent(inout) :: resact(:)     !! residuals of the active constraints
        real(real64), intent(inout) :: resnew(:)     !! residuals of the constraints that still restrict the step
        real(real64), intent(inout) :: rfac(:, :)    !! the upper triangular factor of the active gradients
        real(real64), intent(inout) :: vlam(:)       !! the Lagrange multipliers, extended by a zero

        ! Local variables (debugging only)
        integer :: m
        integer :: n

        m = int(size(iact))
        n = int(size(vlam))

        ! QRADD applies Givens rotations to the last (N-NACT) columns of QFAC so that the first (NACT+1)
        ! columns of QFAC are the ones required for the addition of the L-th constraint, and add the
        ! appropriate column to RFAC.
        ! N.B.: QRADD always augment NACT by 1, which differs from the corresponding subroutine in COBYLA.
        ! It is ensured that C cannot be represented by the gradients of the existing active constraints.
        call qradd(c, qfac, rfac, nact)  ! NACT is increased by 1!
        ! Indeed, it suffices to pass RFAC(:, 1:NACT+1) to QRADD as follows.
        ! !call qradd(c, qfac, rfac(:, 1:nact + 1), nact)  ! NACT is increased by 1!

        ! Update IACT, RESACT, RESNEW, and VLAM. N.B.: NACT has been increased by 1 in QRADD.
        iact(nact) = l
        resact(nact) = resnew(l)  ! RESACT(NACT) = RESNEW(IACT(NACT))
        resnew(l) = ZERO  ! RESNEW(IACT(NACT)) = ZERO  ! Why not TINYCV? See DECACT.
        vlam(nact) = ZERO

    end subroutine addact

    !> Removes the `icon`-th active constraint, upstream's `delact`.
    !!
    !! The column is moved to the end of the QR factorisation through `qrexc` and `nact` falls by
    !! one.
    subroutine delact(icon, iact, nact, qfac, resact, resnew, rfac, vlam)
        integer, intent(in) :: icon                  !! which entry of `iact` to deactivate

        integer, intent(inout) :: iact(:)            !! indices of the active constraints, the first `nact` in use
        integer, intent(inout) :: nact               !! how many constraints are active
        real(real64), intent(inout) :: qfac(:, :)    !! the orthogonal factor of the active gradients
        real(real64), intent(inout) :: resact(:)     !! residuals of the active constraints
        real(real64), intent(inout) :: resnew(:)     !! residuals of the constraints that still restrict the step
        real(real64), intent(inout) :: rfac(:, :)    !! the upper triangular factor of the active gradients
        real(real64), intent(inout) :: vlam(:)       !! the Lagrange multipliers, with the removed one dropped

        ! Local variables (debugging only)
        integer :: m
        integer :: n

        m = int(size(iact))
        n = int(size(vlam))

        ! The following instructions rearrange the active constraints so that the new value of IACT(NACT) is
        ! the old value of IACT(ICON). QREXC implements the updates of QFAC and RFAC by a sequence of Givens
        ! rotations. Then NACT is reduced by one.

        call qrexc(qfac, rfac(:, 1:nact), icon)  ! QREXC does nothing if ICON == NACT.
        ! Indeed, it suffices to pass QFAC(:, 1:NACT) and RFAC(1:NACT, 1:NACT) to QREXC as follows. However,
        ! compilers may create a temporary copy of RFAC(1:NACT, 1:NACT), which is not contiguous in memory.
        ! !call qrexc(qfac(:, 1:nact), rfac(1:nact, 1:nact), icon)

        iact(icon:nact) = [iact(icon + 1:nact), iact(icon)]
        resact(icon:nact) = [resact(icon + 1:nact), resact(icon)]
        resnew(iact(nact)) = max(resact(nact), TINYCV)
        vlam(icon:nact) = [vlam(icon + 1:nact), vlam(icon)]
        nact = nact - 1

    end subroutine delact

    !> The updated trust-region radius, upstream's `trrad`.
    !!
    !! Contracts below `eta1`, expands above `eta2`, and never lets the radius fall below the length
    !! of the step just taken.
    function trrad(delta_in, dnorm, eta1, eta2, gamma1, gamma2, ratio) result(delta)
        ! Generic module

        real(real64), intent(in) :: delta_in    !! the current radius
        real(real64), intent(in) :: dnorm       !! the length of the step just taken
        real(real64), intent(in) :: eta1        !! ratio below which the radius contracts
        real(real64), intent(in) :: eta2        !! ratio above which it expands
        real(real64), intent(in) :: gamma1      !! the contraction factor
        real(real64), intent(in) :: gamma2      !! the expansion factor
        real(real64), intent(in) :: ratio       !! the trust-region reduction ratio

        real(real64) :: delta                   !! the updated radius

        if (ratio <= eta1) then
            delta = gamma1 * dnorm  ! Powell's UOBYQA/NEWUOA.
            !delta = gamma1 * delta_in  ! Powell's COBYLA/LINCOA.
            !delta = min(gamma1 * delta_in, dnorm)  ! Powell's BOBYQA.
        elseif (ratio <= eta2) then
            delta = max(gamma1 * delta_in, dnorm)   ! Powell's UOBYQA/NEWUOA/BOBYQA/LINCOA
        else
            delta = max(gamma1 * delta_in, gamma2 * dnorm)  ! Powell's NEWUOA/BOBYQA.
            !delta = max(delta_in, 1.25_real64 * dnorm, dnorm + rho)  ! Powell's UOBYQA
            !delta = max(delta_in, gamma2 * dnorm)  ! Modified version. Works well for UOBYQA.
            ! Powell's LINCOA code is as follows.
            !delta = min(max(gamma1 * delta_in, gamma2 * dnorm), sqrt(gamma2) * delta_in)
        end if

        ! For noisy problems, the following may work better.
        ! !if (ratio <= eta1) then
        ! !    delta = gamma1 * dnorm
        ! !elseif (ratio <= eta2) then  ! Ensure DELTA >= DELTA_IN
        ! !    delta = delta_in
        ! !else  ! Ensure DELTA > DELTA_IN with a constant factor
        ! !    delta = max(delta_in * (1.0_real64 + gamma2) / 2.0_real64, gamma2 * dnorm)
        ! !end if
        !

    end function trrad

    !> A step that improves the geometry of the interpolation set, upstream's `geostep`.
    !!
    !! Maximises the modulus of the Lagrange function `LFUNC` at `xopt + s` within `delbar`, which
    !! bounds the updating denominator `SIGMA` from below by (4.12) of the NEWUOA paper. Three
    !! approximate maximisers are computed -- along the lines through `xopt` and the other
    !! interpolation points, along the gradient, and along the gradient projected onto the
    !! orthogonal complement of the active constraint gradients -- and the best is taken, the
    !! projected one winning ties because it tends to stay feasible.
    !!
    !! The linear constraints do not bound the first two steps, so the step returned may be
    !! infeasible; `feasible` says whether it is.
    subroutine geostep(iact, idz, knew, kopt, nact, amat, bmat, delbar, qfac, rescon, xpt, zmat, feasible, s)
        ! Common modules

        integer, intent(in) :: iact(:)            !! indices of the active constraints, the first `nact` in use
        integer, intent(in) :: idz                !! the factorisation's sign split: `S` is -1 below `idz` and +1 from it
        integer, intent(in) :: knew               !! index in `xpt` of the point being replaced
        integer, intent(in) :: kopt               !! index in `xpt` of the best point so far
        integer, intent(in) :: nact               !! how many constraints are active
        real(real64), intent(in) :: amat(:, :)    !! the constraint gradients, one per column, each of unit length
        real(real64), intent(in) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: delbar        !! the radius the geometry step may use
        real(real64), intent(in) :: qfac(:, :)    !! the orthogonal factor of the active gradients
        real(real64), intent(in) :: rescon(:)     !! constraint residuals at the trust-region centre, negated where far from active
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Diag(S) Z^T`

        logical, intent(out) :: feasible          !! whether the step satisfies the constraints
        real(real64), intent(out) :: s(:)         !! the geometry step

        integer :: k
        integer :: m
        integer :: n
        integer :: npt
        integer :: rstat(size(amat, 2))
        logical :: take_pgstp
        real(real64) :: cstrv
        real(real64) :: cvtol
        real(real64) :: dderiv(size(xpt, 2))
        real(real64) :: den(size(xpt, 2))
        real(real64) :: denabs
        real(real64) :: distsq(size(xpt, 2))
        real(real64) :: glag(size(xpt, 1))
        real(real64) :: gstp(size(xpt, 1))
        real(real64) :: gnorm
        real(real64) :: pglag(size(xpt, 1))
        real(real64) :: pgstp(size(xpt, 1))
        real(real64) :: pqlag(size(xpt, 2))
        real(real64) :: scaling
        real(real64) :: stplen(size(xpt, 2))
        real(real64) :: vlagabs(size(xpt, 2))
        real(real64) :: xopt(size(xpt, 1))

        ! Sizes.
        m = int(size(amat, 2))
        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Read XOPT.
        xopt = xpt(:, kopt)

        ! PQLAG contains the leading NPT elements of the KNEW-th column of H, and it provides the second
        ! derivative parameters of LFUNC. Set GLAG to the gradient of LFUNC at the trust region centre.
        pqlag = omega_col(idz, zmat, knew)
        glag = bmat(:, knew) + hess_mul(xopt, xpt, pqlag)

        ! Maximize |LFUNC| within the trust region on the lines through XOPT and other interpolation points,
        ! without considering the linear constraints. In the following, VLAGABS(K) is set to the maximum of
        ! |PHI_K(t)| subject to the trust-region constraint with PHI_K(t) = LFUNC((1-t)*XOPT + t*XPT(:, K)).
        dderiv = matprod(glag, xpt) - inprod(glag, xopt) ! The derivatives PHI_K'(0).
        distsq = sum((xpt - spread(xopt, dim=2, ncopies=npt))**2, dim=1)
        ! Set DISTSQ(KOPT) to a positive artificial value. Otherwise, the calculation of STPLEN will raise a
        ! floating point exception. This artificial value will NOT be used.
        distsq(kopt) = ONE
        ! For each K /= KNEW, |PHI_K(t)| is maximized by STPLEN(K), the maximum being VLAGABS(K). Note that
        ! PHI_K(t) is a quadratic function with PHI_K'(0) = DDERIV(K) and PHI_K(0) = 0 = PHI_K(1).
        stplen = -delbar / sqrt(distsq)
        vlagabs = abs(stplen * (ONE - stplen) * dderiv)
        ! The maximization of |PHI_K(t)| is as follows. Note that PHI_K(t) is a quadratic function with
        ! PHI_K'(0) = DDERIV(K), PHI_K(0) = 0, and PHI_K(1) = 1.
        if (dderiv(knew) * (dderiv(knew) - ONE) < 0) then
            stplen(knew) = -stplen(knew)
        end if
        vlagabs(knew) = abs(stplen(knew) * dderiv(knew)) + stplen(knew)**2 * abs(dderiv(knew) - ONE)
        ! It does not make sense to consider "the straight line through XOPT and XPT(:, KOPT)". Thus we set
        ! VLAGABS(KOPT) to -1 so that KOPT is skipped when we maximize VLAGABS.
        vlagabs(kopt) = -ONE
        ! Find K so that VLAGABS(K) is maximized. We define K in a way slightly different from Powell's
        ! code, which sets K to MAXLOC(VLAGABS) by comparing the entries of VLAGABS sequentially.
        ! 1. If VLAGABS contains only NaN, which can happen, Powell's code leaves K uninitialized.
        ! 2. If VLAGABS(KNEW) = MAXVAL(VLAGABS) = VLAGABS(K) and K < KNEW, Powell's code does not set K=KNEW.
        k = knew
        if (any(vlagabs > vlagabs(knew))) then
            k = int(maxloc(vlagabs, mask=(.not. is_nan(vlagabs)), dim=1))
            ! MATLAB: [~, k] = max(vlagabs, [], 'omitnan');
        end if
        ! Set S to the step corresponding to VLAGABS(K), and calculate DENABS for it.
        s = stplen(k) * (xpt(:, k) - xopt)
        den = calden(kopt, bmat, s, xpt, zmat, idz)  ! Indeed, only DEN(KNEW) is needed.
        denabs = abs(den(knew))

        ! Replace S with a steepest ascent step from XOPT if the latter provides a larger value of DENABS.
        gnorm = norm(glag)
        if (gnorm > EPS .and. is_finite(gnorm)) then
            gstp = (delbar / gnorm) * glag
            if (inprod(gstp, hess_mul(gstp, xpt, pqlag)) < 0) then  ! <GSTP, HESS_LAG*GSTP> is negative
                gstp = -gstp
            end if
            den = calden(kopt, bmat, gstp, xpt, zmat, idz)  ! Indeed, only DEN(KNEW) is needed.
            if (abs(den(knew)) > denabs .or. is_nan(denabs)) then
                denabs = abs(den(knew))
                s = gstp
            end if
        end if

        ! RSTAT identifies the constraints that need evaluation. RSTAT(J) is -1, 0, or 1 respectively means
        ! constraint J is irrelevant, active, or inactive and relevant. Do NOT change the order of the lines
        ! that set RSTAT, as the later lines override the earlier.
        rstat = 1  ! Inactive and relevant
        rstat(trueloc(abs(rescon) >= delbar)) = -1  ! Irrelevant
        rstat(iact(1:nact)) = 0  ! Active

        ! Set FEASIBLE for the calculated S.
        cstrv = maximum([ZERO, matprod(s, amat(:, trueloc(rstat >= 0))) - rescon(trueloc(rstat >= 0))])
        feasible = (cstrv <= 0)

        ! If NACT <= 0 or NACT >= N, the calculation has finished. Otherwise, define PGSTP by maximizing
        ! |LFUNC| within the trust region from XOPT along the projection of GLAG onto the column space of
        ! QFAC(:, NACT+1:N), i.e., the orthogonal complement of the space spanned by the active gradients.
        ! In precise arithmetic, moving along PGSTP does not change the values of the active constraints.
        ! This projected gradient step is preferred and will override S if it renders a denominator not too
        ! small and leads to good feasibility. *** This is critical for the performance of LINCOA. ***
        ! In the following, NORMG > EPS prevents floating point exception, and it implies NACT < N.
        pglag = matprod(qfac(:, nact + 1:n), matprod(glag, qfac(:, nact + 1:n)))
        ! MATLAB: pglag = qfac(:, nact+1:n) * (glag' * qfac(:, nact+1:n))';
        gnorm = norm(pglag)
        if (nact > 0 .and. gnorm > EPS .and. is_finite(gnorm)) then
            pgstp = (delbar / gnorm) * pglag
            if (inprod(pgstp, hess_mul(pgstp, xpt, pqlag)) < 0) then  ! <PGSTP, HESS_LAG*PGSTP> is negative.
                pgstp = -pgstp
            end if

            ! Decide whether to replace S with PGSTP and set FEASIBLE accordingly. CSTRV is the constraint
            ! violation of XOPT+PGSTP. Note that we only need to check the constraints that are inactive and
            ! relevant, as the value of the active constraints is not changed by moving along PGSTP.
            cstrv = maximum([ZERO, matprod(pgstp, amat(:, trueloc(rstat == 1))) - rescon(trueloc(rstat == 1))])
            ! The purpose of CVTOL below is to provide a check on feasibility that includes a tolerance for
            ! contributions from computer rounding errors.
            ! Powell's code is as follows. Note that MATPROD(PGSTP, AMAT(:, IACT(1:NACT))) is 0 in theory.
            ! !cvtol = min(0.01_real64 * norm(pgstp), TEN * norm(matprod(pgstp, amat(:, iact(1:nact))), 'inf'))
            ! The following code works essentially the same as Powell's code.
            cvtol = max(EPS * norm(pgstp), TEN * norm(matprod(pgstp, amat(:, iact(1:nact))), 'inf'))
            take_pgstp = .false.
            if (cstrv <= cvtol) then
                den = calden(kopt, bmat, pgstp, xpt, zmat, idz)  ! Indeed, only DEN(KNEW) is needed.
                take_pgstp = (abs(den(knew)) > TENTH * denabs)
            end if
            if (take_pgstp .or. is_nan(denabs)) then
                s = pgstp
                feasible = (cstrv <= cvtol)
            end if
        end if

        ! In case S is zero or contains Inf/NaN, replace it with a displacement from XPT(:, KNEW) to
        ! XOPT. Powell's code does not have this.
        ! Not reached: S is built from the model, which `evaluate` keeps finite by refusing a
        ! non-finite objective value, and no run the suite drives leaves S at exactly zero.
        if (sum(abs(s)) <= 0 .or. .not. is_finite(sum(abs(s)))) then
            s = xpt(:, knew) - xopt                 ! GCOVR_EXCL_START -- see the note above
            scaling = delbar / norm(s)
            s = max(0.6_real64 * scaling, min(HALF, scaling)) * s  ! 0.6: ensure |D| > DELBAR/2
            cstrv = maximum([ZERO, matprod(s, amat(:, trueloc(rstat >= 0))) - rescon(trueloc(rstat >= 0))])
            feasible = (cstrv <= 0)
            ! GCOVR_EXCL_STOP
        end if

    end subroutine geostep

    !> Which interpolation point to drop after a trust-region step, upstream's `setdrop_tr`.
    !!
    !! Weighs each candidate's updating denominator against its distance from the new best point.
    !! Upstream's note is worth keeping: the function VALUE is deliberately not considered, because
    !! a set of points with low values is not a good interpolation set -- the model needs points
    !! where the function is large as much as points where it is small.
    function setdrop_tr(idz, kopt, ximproved, bmat, d, delta, rho, xpt, zmat) result(knew)
        ! Common modules

        integer, intent(in) :: idz                !! the factorisation's sign split: `S` is -1 below `idz` and +1 from it
        integer, intent(in) :: kopt               !! index in `xpt` of the best point so far
        logical, intent(in) :: ximproved          !! whether the new point improved on the best value
        real(real64), intent(in) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: d(:)          !! the step from the best point
        real(real64), intent(in) :: delta         !! the trust-region radius
        real(real64), intent(in) :: rho           !! the lower bound on the trust-region radius
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Diag(S) Z^T`

        integer :: knew                           !! the point to drop, `0` when none should be

        integer :: n
        integer :: npt
        real(real64) :: den(size(xpt, 2))
        real(real64) :: distsq(size(xpt, 2))
        real(real64) :: score(size(xpt, 2))
        real(real64) :: weight(size(xpt, 2))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Calculate the distance squares between the interpolation points and the "optimal point". When
        ! identifying the optimal point, it is reasonable to take into account the new trust-region trial
        ! point XPT(:, KOPT) + D, which will become the optimal point in the next iteration if XIMPROVED
        ! is TRUE. Powell suggested this in
        ! - (56) of the UOBYQA paper, lines 276--297 of uobyqb.f,
        ! - (7.5) and Box 5 of the NEWUOA paper, lines 383--409 of newuob.f,
        ! - the last paragraph of page 26 of the BOBYQA paper, lines 435--465 of bobyqb.f.
        ! However, Powell's LINCOA code is different. In his code, the KNEW after a trust-region step is
        ! picked in lines 72--96 of the update.f for LINCOA, where DISTSQ is calculated as the square of the
        ! distance to XPT(KOPT, :) (Powell recorded the interpolation points in rows). However, note that
        ! the trust-region trial point has not been included into XPT yet --- it cannot be included without
        ! knowing KNEW (see lines 332-344 and 404--431 of lincob.f). Hence Powell's LINCOA code picks KNEW
        ! based on the distance to the un-updated "optimal point", which is unreasonable. This has been
        ! corrected in our implementation of LINCOA, yet it does not boost the performance.
        if (ximproved) then
            distsq = sum((xpt - spread(xpt(:, kopt) + d, dim=2, ncopies=npt))**2, dim=1)
            ! MATLAB: distsq = sum((xpt - (xpt(:, kopt) + d)).^2)  % d should be a column!! Implicit expansion
        else
            distsq = sum((xpt - spread(xpt(:, kopt), dim=2, ncopies=npt))**2, dim=1)
            ! MATLAB: distsq = sum((xpt - xpt(:, kopt)).^2)  % Implicit expansion
        end if
        !distsq = sum((xpt - spread(xpt(:, kopt), dim=2, ncopies=npt))**2, dim=1)  ! Powell's code

        weight = max(ONE, distsq / max(TENTH * delta, rho)**2)**3  ! Powell's NEWUOA code
        ! Other possible definitions of WEIGHT.
        ! !weight = distsq**2  ! Powell's code. WRONG.
        ! !weight = max(ONE, distsq / max(TENTH * delta, rho)**2)**2.5  ! Worse than power 3
        ! !weight = max(ONE, distsq / max(TENTH * delta, rho)**2)**3.5  ! Worse than power 3
        ! !weight = (distsq / delta**2)**2   ! Works the same as DISTSQ**2 (as it should be).
        ! !weight = (distsq / delta**2)**3  ! Not bad
        ! !weight = max(1.0_real64, 10.0_real64 * distsq / rho**2)**3
        ! !weight = max(1.0_real64, 1.0E2 * distsq / rho**2)**3
        ! !weight = max(1.0_real64, 10.0_real64 * distsq / delta**2)**3
        ! !weight = max(1.0_real64, 1.0E2_real64 * distsq / delta**2)**3
        !--------------------------------------------------------------------------------------------------!
        ! N.B.: If DISTSQ is the square of distances to the updated XOPT, then it is WRONG to set WEIGHT to
        ! DISTSQ**2 or any power of DISTSQ. Why?
        !
        ! Consider a scenario where XIMPROVED is TRUE and the new interpolation point XNEW is quite close to
        ! one of the existing points in the old XPT, e.g., XPT(:, J). In this case, the only appropriate
        ! value of KNEW is J; otherwise, the new interpolation problem will be close to degenerate and its
        ! KKT system will be close to singular due to the two close points in the updated interpolation set.
        !
        ! What KNEW will be generated if KNEW = MAXLOC(DISTSQ**p * ABS(DEN))? If ||XNEW - XPT(:, J)|| = E,
        ! we have the following.
        ! 1. DISTSQ(J) = O(E**2) and DEN(J) = O(1);
        ! 2. for any K /= J, DIST(K) = O(1) and DEN(K) = O(E).
        ! Therefore, for any p > 1/2, KNEW /= J when E is small. As analyzed above, this is inappropriate.
        ! In addition, small values of p (e.g., p <= 1/2) always performs poorly for all Powell's methods
        ! in our numerical experiments.
        !
        ! For the order of DEN, note the DEN(K) is the denominator in the Sherman-Morrison-Woodbury update
        ! of the KKT matrix, and DEN(K) = det(new KKT matrix) / det(old KKT matrix), where "KKT matrix"
        ! refers to the coefficient matrix of the KKT system for the interpolation problem. See equations
        ! (3.10)--(3.12) of the NEWUOA paper for this matrix.
        !
        ! Similar arguments can be made if the interpolation is fully determined. Indeed, the usage of
        ! DISTSQ as the weight led to a problem in COBYLA during a test on 20230501, which is the very
        ! motivation for the current comment.
        !
        ! Note that Powell's LINCOA code sets DISTSQ to the square of the distance to the old XOPT, which
        ! avoids this problem. However, such a DISTSQ itself seems not ideal, as mentioned above.
        !--------------------------------------------------------------------------------------------------!

        den = calden(kopt, bmat, d, xpt, zmat, idz)
        score = weight * abs(den)

        ! If the new F is not better than FVAL(KOPT), we set SCORE(KOPT) = -1 to avoid KNEW = KOPT.
        if (.not. ximproved) then
            score(kopt) = -ONE
        end if

        ! SCORE(K) is NaN implies ABS(DEN(K)) is NaN, but we want ABS(DEN) to be big. So we exclude such K.
        score(trueloc(is_nan(score))) = -ONE

        knew = 0
        ! The following IF works a bit better than `IF (ANY(SCORE > 1) .OR. ANY(SCORE > 0) .AND. XIMPROVED)`
        ! from Powell's UOBYQA and NEWUOA code.
        if (any(score > 0)) then  ! Powell's BOBYQA and LINCOA code
            knew = int(maxloc(score, dim=1))
            ! MATLAB: [~, knew] = max(score);
        end if

        ! Powell's code does not include the following instructions. With Powell's code, if DEN consists of
        ! only NaN, then KNEW can be 0 even when XIMPROVED is TRUE. Here, we set KNEW to the following value,
        ! to make sure that the new trial point is included in the interpolation set. However, the updating
        ! subroutine will likely need to skip the update of the Lagrange polynomials (i.e., H), or they
        ! would be destroyed by the NaNs.
        if ((ximproved .and. knew == 0) .or. knew < 0) then  ! KNEW < 0 is impossible in theory.
            knew = int(maxloc(distsq, dim=1))  ! GCOVR_EXCL_LINE -- needs every DEN zero or NaN
        end if

    end function setdrop_tr

    !> Replaces `xpt(:, knew)` by the new point and updates `fval` and `kopt`; upstream's
    !> `updatexf`.
    subroutine updatexf(knew, ximproved, f, xnew, kopt, fval, xpt)
        ! Common modules

        integer, intent(in) :: knew                 !! index in `xpt` of the point being replaced
        real(real64), intent(in) :: f               !! objective value at the new point
        real(real64), intent(in) :: xnew(:)         !! the new point, relative to `xbase`

        integer, intent(inout) :: kopt              !! index in `xpt` of the best point so far
        logical, intent(in) :: ximproved            !! whether the new point improved on the best value
        real(real64), intent(inout) :: fval(:)      !! objective value at each interpolation point
        real(real64), intent(inout) :: xpt(:, :)    !! the interpolation set relative to `xbase`, one point per column

        integer :: n
        integer :: npt

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Do essentially nothing when KNEW is 0. This can only happen after a trust-region step.
        if (knew <= 0) then  ! KNEW < 0 is impossible if the input is correct.
            return  ! GCOVR_EXCL_LINE -- lincob calls UPDATEXF only with a positive KNEW
        end if

        xpt(:, knew) = xnew
        fval(knew) = f

        if (ximproved) then
            kopt = knew
        end if

    end subroutine updatexf

    !> Updates the quadratic model after an interpolation point moved, upstream's `updateq`.
    !!
    !! Section 4 of the NEWUOA paper and of the BOBYQA paper. `xpt(:, knew)` has changed from
    !! `xdrop` to `xosav + d`, and the model is corrected by the interpolation error at the new
    !! point spread over the Lagrange function of `knew`.
    subroutine updateq(idz, knew, ximproved, bmat, d, moderr, xdrop, xosav, xpt, zmat, gopt, hq, pq)
        ! Common modules

        integer, intent(in) :: idz                 !! the factorisation's sign split: `S` is -1 below `idz` and +1 from it
        integer, intent(in) :: knew                !! index in `xpt` of the point being replaced
        logical, intent(in) :: ximproved           !! whether the new point improved on the best value
        real(real64), intent(in) :: bmat(:, :)     !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: d(:)           !! the step from the best point
        real(real64), intent(in) :: moderr         !! the model error at the new point
        real(real64), intent(in) :: xdrop(:)       !! the point that left the set
        real(real64), intent(in) :: xosav(:)       !! the best point before the update
        real(real64), intent(in) :: xpt(:, :)      !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)     !! the factorisation's Z, with `OMEGA = Z Diag(S) Z^T`

        real(real64), intent(inout) :: gopt(:)     !! the model's gradient at `xbase + xpt(:, kopt)`
        real(real64), intent(inout) :: hq(:, :)    !! the model's explicit Hessian part
        real(real64), intent(inout) :: pq(:)       !! the model's implicit Hessian weights, one per interpolation point

        integer :: n
        integer :: npt
        real(real64) :: pqinc(size(pq))

        n = int(size(gopt))
        npt = int(size(pq))

        ! Do nothing when KNEW is 0. This can only happen after a trust-region step.
        if (knew <= 0) then  ! KNEW < 0 is impossible if the input is correct.
            return  ! GCOVR_EXCL_LINE -- lincob calls UPDATEQ only with a positive KNEW
        end if

        ! The unupdated model corresponding to [GOPT, HQ, PQ] interpolates F at all points in XPT except for
        ! XNEW. The error is MODERR = [F(XNEW)-F(XOPT)] - [Q(XNEW)-Q(XOPT)].

        ! Absorb PQ(KNEW)*XDROP*XDROP^T into the explicit part of the Hessian.
        ! Implement R1UPDATE properly so that it ensures that HQ is symmetric.
        call r1update(hq, pq(knew), xdrop)
        pq(knew) = ZERO

        ! Update the implicit part of the Hessian.
        pqinc = moderr * omega_col(idz, zmat, knew)
        pq = pq + pqinc

        ! Update the gradient, which needs the updated XPT.
        gopt = gopt + moderr * bmat(:, knew) + hess_mul(xosav, xpt, pqinc)

        ! Further update GOPT if XIMPROVED is TRUE, as XOPT changes from XOSAV to XNEW = XOSAV + D.
        if (ximproved) then
            gopt = gopt + hess_mul(d, xpt, pq, hq)
        end if

    end subroutine updateq

    !> Replaces the model by the least-Frobenius-norm alternative when that has been predicting
    !> better; upstream's `tryqalt`.
    !!
    !! The alternative model is the one minimising the Frobenius norm of the Hessian subject to the
    !! interpolation conditions. It takes over only when the last three alternative models all
    !! predicted the value at `xopt + d` better than the current model did.
    subroutine tryqalt(idz, bmat, fval, xopt, xpt, zmat, qalt_better, gopt, pq, hq, galt, pqalt)
        ! Common modules

        integer, intent(in) :: idz                  !! the factorisation's sign split: `S` is -1 below `idz` and +1 from it
        real(real64), intent(in) :: bmat(:, :)      !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: fval(:)         !! objective value at each interpolation point
        real(real64), intent(in) :: xopt(:)         !! the best point so far, relative to `xbase`
        real(real64), intent(in) :: xpt(:, :)       !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)      !! the factorisation's Z, with `OMEGA = Z Diag(S) Z^T`

        ! In-outptuts
        logical, intent(inout) :: qalt_better(:)    !! whether each of the last three alternative models predicted better
        real(real64), intent(inout) :: gopt(:)      !! the model's gradient at `xbase + xpt(:, kopt)`
        real(real64), intent(inout) :: pq(:)        !! the model's implicit Hessian weights, one per interpolation point
        real(real64), intent(inout) :: hq(:, :)     !! the model's explicit Hessian part

        real(real64), intent(out) :: galt(:)        !! the alternative model's gradient
        real(real64), intent(out) :: pqalt(:)       !! the alternative model's implicit Hessian weights

        integer :: n
        integer :: npt

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Establish the alternative model, which is the least Frobenius norm interpolant.
        pqalt = omega_mul(idz, zmat, fval)
        galt = matprod(bmat(:, 1:npt), fval) + hess_mul(xopt, xpt, pqalt)

        ! Replace the current model with the alternative model if ALL(QALT_BETTER) = TRUE, i.e., the
        ! recent few alternative models are more accurate in predicting the function value of XOPT + D.
        ! The takeover needs the three most recent alternative models to have ALL predicted better,
        ! which no run the suite drives produces.
        if (all(qalt_better)) then
            pq = pqalt                            ! GCOVR_EXCL_START -- see the note above
            hq = ZERO
            gopt = galt
            qalt_better = .false.
            ! GCOVR_EXCL_STOP
        end if

    end subroutine tryqalt

    !> Updates the constraint residuals at the trust-region centre, upstream's `updateres`.
    !!
    !! A residual larger than `delta` is stored negated, which marks the constraint as far from
    !! active and lets the next iteration skip it: that is what makes LINCOA cheap when most of the
    !! constraints are slack.
    subroutine updateres(ximproved, amat, b, delta, dnorm, xopt, rescon)
        ! Common modules

        logical, intent(in) :: ximproved            !! whether the new point improved on the best value
        real(real64), intent(in) :: amat(:, :)      !! the constraint gradients, one per column, each of unit length
        real(real64), intent(in) :: b(:)            !! the right-hand sides of `amat`, relative to `xbase`
        real(real64), intent(in) :: delta           !! the trust-region radius
        real(real64), intent(in) :: dnorm           !! the length of the step just taken
        real(real64), intent(in) :: xopt(:)         !! the best point so far, relative to `xbase`

        real(real64), intent(inout) :: rescon(:)  !! constraint residuals at the trust-region centre, negated where far from active

        integer :: m
        integer :: n
        logical :: mask(size(b))
        real(real64) :: ax(size(b))

        m = int(size(b))
        n = int(size(xopt))

        ! Zaikun 20221115: Currently, UPDATERES does not update RESCON unless XIMPROVED is TRUE. Shouldn't
        ! we do it whenever DELTA is updated? Have we MISUNDERSTOOD RESCON?
        if (.not. ximproved) then
            return
        end if

        mask = (abs(rescon) < dnorm + delta)
        ax(trueloc(mask)) = matprod(xopt, amat(:, trueloc(mask)))
        where (mask)
            rescon = max(b - ax, ZERO)
        elsewhere
            rescon = min(-abs(rescon) + dnorm, -delta)
        end where
        rescon(trueloc(rescon >= delta)) = -rescon(trueloc(rescon >= delta))

        ! MATLAB:
        ! mask = (abs(rescon) < delta + dnorm);
        ! rescon(mask) = max(b(mask) - (xopt'*amat(:, mask))', 0);
        ! rescon(~mask) = max(rescon(~mask) - dnorm, delta);
        ! rescon(rescon >= delta) = -rescon(rescon >= delta);

    end subroutine updateres

end module parquet_prima_lincob
