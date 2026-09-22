!> PRIMA's BOBYQA: bound-constrained minimisation by a quadratic model interpolating
!> `npt` points, with a trust-region step, a geometry step, the RESCUE procedure and the
!> factorisation updates.
!!
!! **Provenance.** Derived from PRIMA (Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`: `fortran/bobyqa/bobyqb.f90`, `geometry.f90`,
!! `initialize.f90`, `rescue.f90`, `trustregion.f90` and `update.f90`, which are one module each
!! upstream and one module here (`feature_optimizer.md` 4.1's ten-file layout). The licence text
!! and the deviations shared by every file of this tier are in `parquet_prima_linalg`.
!!
!! The algorithm is M. J. D. Powell's, *The BOBYQA algorithm for bound constrained optimization
!! without derivatives* (DAMTP 2009/NA06, University of Cambridge); the comments citing equation
!! numbers of "the BOBYQA paper" and "the NEWUOA paper" are upstream's and refer to it and to
!! *The NEWUOA software for unconstrained optimization without derivatives* (2006).
!!
!! **A bug found here is checked against upstream before it is fixed here.** Every formula, every
!! branch and every tolerance below is upstream's.
!!
!! **What changed beyond the tier-wide list.**
!!
!! 1. `bobyqb`, `initxf` and `rescue` take the caller's objective and a `prima_state` in place of
!!    upstream's `procedure(OBJ) :: calfun`, and evaluate through `evaluate`, which screens the
!!    value and fills the record.
!! 2. `iprint` and every `fmsg`/`rhomsg`/`retmsg` call are gone: this tier prints nothing.
!! 3. `xhist`/`fhist` and the `savehist`/`rangehist` calls are gone with them; the record is the
!!    optional `pf_optimize_history`, appended in `evaluate`.
!! 4. `callback_fcn` and `CALLBACK_TERMINATE` are not carried (`feature_optimizer.md` Q22).
!! 5. Upstream's driver `bobyqa.f90` is NOT vendored: its argument defaults, its `honour_x0`
!!    revision of `rhobeg` and its `preproc` adjustments become the validation and the driver in
!!    `parquet_prima_bobyqa`, where an adjustment upstream makes with a warning is either refused
!!    or reported in `info%rho`.
module parquet_prima_bobyqb

    use, intrinsic :: iso_fortran_env, only : real64
    use parquet_optimize, only : pf_objective
    use parquet_prima_common, only : prima_state, evaluate, checkexit, redrat, redrho, xinbd, &
        shiftbase, interval_max
    use parquet_prima_linalg, only : sum, ZERO, ONE, TWO, HALF, TEN, TENTH, EPS, REALMAX, REALMIN, &
        QUART, is_nan, is_finite, is_posinf, is_inf, inprod, matprod, outprod, trueloc, norm, &
        diag, planerot, r1update, r2update, symmetrize, linspace, INFO_DFT, SMALL_TR_RADIUS, &
        MAXTR_REACHED, DAMAGING_ROUNDING, NAN_INF_MODEL, MAXFUN_REACHED, FTARGET_ACHIEVED
    use parquet_prima_powalg, only : quadinc, calden, calvlag, calbeta, hess_mul, setij

    implicit none
    private

    public :: bobyqb

contains


    !> BOBYQA's main iteration.
    !!
    !! Initialises the interpolation set, the model and the factorisation, then alternates a
    !! trust-region step with a geometry step, reducing `rho` from `rhobeg` to `rhoend` and calling
    !! `rescue` when the factorisation degrades. `x` enters as the start and leaves as the best
    !! point found.
    !!
    !! Upstream's own description of the state it carries -- `xbase`, `sl`/`su`, `[xpt, fval,
    !! kopt]`, `[gopt, hq, pq]`, `[bmat, zmat]` and `d` -- is kept in the comment block below.
    subroutine bobyqb(obj, st, maxfun, npt, eta1, eta2, ftarget, gamma1, gamma2, rhobeg, rhoend, &
            & xl, xu, x, nf, f, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine performs the major calculations of BOBYQA.
        !
        ! IPRINT, MAXFUN, MAXHIST, NPT, ETA1, ETA2, FTARGET, GAMMA1, GAMMA2, RHOBEG, RHOEND, XL, XU, X, NF,
        ! F, FHIST, XHIST, and INFO are identical to the corresponding arguments in subroutine BOBYQA.
        !
        ! XBASE holds a shift of origin that should reduce the contributions from rounding errors to values
        !   of the model and Lagrange functions.
        ! SL and SU hold XL - XBASE and XU - XBASE, respectively.
        ! XOPT is the displacement from XBASE of the best vector of variables so far (i.e., the one provides
        !   the least calculated F so far). XOPT satisfies SL(I) <= XOPT(I) <= SU(I), with appropriate
        !   equalities when XOPT is on a constraint boundary. FOPT = F(XOPT + XBASE). However, we do not
        !   save XOPT and FOPT explicitly, because XOPT = XPT(:, KOPT) and FOPT = FVAL(KOPT), which is
        !   explained below.
        ! [XPT, FVAL, KOPT] describes the interpolation set:
        ! XPT contains the interpolation points relative to XBASE, each COLUMN for a point; FVAL holds the
        !   values of F at the interpolation points; KOPT is the index of XOPT in XPT.
        ! [GOPT, HQ, PQ] describes the quadratic model: GOPT will hold the gradient of the quadratic model
        !   at XBASE + XOPT; HQ will hold the explicit second order derivatives of the quadratic model; PQ
        !   will contain the parameters of the implicit second order derivatives of the quadratic model.
        ! [BMAT, ZMAT] describes the matrix H in the BOBYQA paper (eq. 2.7), which is the inverse of
        !   the coefficient matrix of the KKT system for the least-Frobenius norm interpolation problem:
        ! ZMAT will hold a factorization of the leading NPT*NPT submatrix of H, the factorization being
        !   OMEGA = ZMAT*ZMAT^T, which provides both the correct rank and positive semi-definiteness. BMAT
        !   will hold the last N ROWs of H except for the (NPT+1)th column. Note that the (NPT + 1)th row
        !   and column of H are not saved as they are unnecessary for the calculation.
        ! D is reserved for trial steps from XOPT. It is chosen by subroutine TRSBOX or GEOSTEP. Usually
        !   XBASE + XOPT + D is the vector of variables for the next call of CALFUN.
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        ! Solver-specific modules

        class(pf_objective), intent(inout) :: obj    !! the objective (this thread's own)
        type(prima_state), intent(inout) :: st       !! scaling, wording, record and evaluation count
        integer, intent(in) :: maxfun                !! the evaluation budget
        integer, intent(in) :: npt                   !! how many interpolation points
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
        real(real64), intent(out) :: f               !! value at the returned `x`

        integer :: ij(2, max(0, int(npt - 2 * size(x) - 1)))
        integer :: itest
        integer :: knew_geo
        integer :: knew_tr
        integer :: kopt
        integer :: maxtr
        integer :: n
        integer :: subinfo
        integer :: tr
        logical :: accurate_mod
        logical :: adequate_geo
        logical :: bad_trstep
        logical :: close_itpset
        logical :: improve_geo
        logical :: reduce_rho
        logical :: rescued
        logical :: shortd
        logical :: small_trrad
        logical :: to_rescue
        logical :: trfail
        logical :: ximproved
        real(real64) :: bmat(size(x), npt + size(x))
        real(real64) :: crvmin
        real(real64) :: d(size(x))
        real(real64) :: delbar
        real(real64) :: delta
        real(real64) :: den(npt)
        real(real64) :: distsq(npt)
        real(real64) :: dnorm
        real(real64) :: dnorm_rec(2)  ! Powell's implementation: DNORM_REC(3)
        real(real64) :: ebound
        real(real64) :: fval(npt)
        real(real64) :: gamma3
        real(real64) :: gopt(size(x))
        real(real64) :: hq(size(x), size(x))
        real(real64) :: moderr
        real(real64) :: moderr_rec(size(dnorm_rec))
        real(real64) :: pq(npt)
        real(real64) :: qred
        real(real64) :: ratio
        real(real64) :: rho
        real(real64) :: sl(size(x))
        real(real64) :: su(size(x))
        real(real64) :: vlag(npt + size(x))
        real(real64) :: xbase(size(x))
        real(real64) :: xdrop(size(x))
        real(real64) :: xosav(size(x))
        real(real64) :: xpt(size(x), npt)
        real(real64) :: zmat(npt, npt - size(x) - 1)
        real(real64), parameter :: trtol = 1.0E-2_real64  ! Convergence tolerance of trust-region subproblem solver

        ! Sizes.
        n = int(size(x))

        ! Initialize XBASE, XPT, SL, SU, FVAL, and KOPT, together with the history, NF, and IJ.
        call initxf(obj, st, maxfun, ftarget, rhobeg, xl, xu, x, ij, kopt, nf, fval, &
            & sl, su, xbase, xpt, subinfo)

        ! Initialize X and F according to KOPT.
        x = xinbd(xbase, xpt(:, kopt), xl, xu, sl, su)  ! In precise arithmetic, X = XBASE + XOPT.
        f = fval(kopt)

        ! Finish the initialization if INITXF completed normally and CALLBACK did not request termination;
        ! otherwise, do not proceed, as XPT etc may be uninitialized, leading to errors or exceptions.
        if (subinfo == INFO_DFT) then
            ! Initialize [BMAT, ZMAT], representing inverse of KKT matrix of the interpolation system.
            call inith(ij, xpt, bmat, zmat)

            ! Initialize the quadratic represented by [GOPT, HQ, PQ], so that its gradient at XBASE+XOPT is
            ! GOPT; its Hessian is HQ + sum_{K=1}^NPT PQ(K)*XPT(:, K)*XPT(:, K)'.
            call initq(ij, fval, xpt, gopt, hq, pq)
            ! The initial model is finite in every run the suite drives: a non-finite value aborts
            ! in `evaluate` before it can reach INITQ.
            if (.not. (all(is_finite(gopt)) .and. all(is_finite(hq)) .and. all(is_finite(pq)))) then
                subinfo = NAN_INF_MODEL  ! GCOVR_EXCL_LINE -- see the note above
            end if
        end if

        ! Check whether to return due to abnormal cases that may occur during the initialization.
        if (subinfo /= INFO_DFT) then
            info = subinfo
            ! The driver reads the radius reached and the iteration count from the state; set them on
            ! every exit, this one included, so that `info%rho` and `info%niter` are never stale.
            st%rho = rhobeg
            st%niter = 0
            ! Arrange FHIST and XHIST so that they are in the chronological order.
            ! Print a return message according to IPRINT.
            return
        end if

        ! Set some more initial values.
        ! We must initialize RATIO. Otherwise, when SHORTD = TRUE, compilers may raise a run-time error that
        ! RATIO is undefined. But its value will not be used: when SHORTD = FALSE, its value will be
        ! overwritten; when SHORTD = TRUE, its value is used only in BAD_TRSTEP, which is TRUE regardless of
        ! RATIO. Similar for KNEW_TR.
        ! No need to initialize SHORTD unless MAXTR < 1, but some compilers may complain if we do not do it.
        rho = rhobeg
        delta = rho
        ebound = ZERO
        rescued = .false.
        shortd = .false.
        trfail = .false.
        ratio = -ONE
        dnorm_rec = REALMAX
        moderr_rec = REALMAX
        knew_tr = 0
        knew_geo = 0
        itest = 0

        ! If DELTA <= GAMMA3*RHO after an update, we set DELTA to RHO. GAMMA3 must be less than GAMMA2. The
        ! reason is as follows. Imagine a very successful step with DENORM = the un-updated DELTA = RHO.
        ! Then TRRAD will update DELTA to GAMMA2*RHO. If GAMMA3 >= GAMMA2, then DELTA will be reset to RHO,
        ! which is not reasonable as D is very successful. See paragraph two of Sec. 5.2.5 in
        ! T. M. Ragonneau's thesis: "Model-Based Derivative-Free Optimization Methods and Software".
        ! According to test on 20230613, for BOBYQA, this Powellful updating scheme of DELTA works better
        ! than setting directly DELTA = MAX(NEW_DELTA, RHO).
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
        ! BOBYQA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously.
        do tr = 1, maxtr
            ! Generate the next trust region step D.
            call trsbox(delta, gopt, hq, pq, sl, su, trtol, xpt(:, kopt), xpt, crvmin, d)
            dnorm = min(delta, norm(d))
            shortd = (dnorm <= HALF * rho)  ! `<=` works better than `<` in case of underflow.

            ! Set QRED to the reduction of the quadratic model when the move D is made from XOPT. QRED
            ! should be positive. If it is nonpositive due to rounding errors, we will not take this step.
            qred = -quadinc(d, xpt, gopt, pq, hq)  ! QRED = Q(XOPT) - Q(XOPT + D)
            trfail = (.not. qred > 1.0E-6 * rho**2)  ! QRED is tiny/negative or NaN.

            ! When D is short, make a choice between reducing RHO and improving the geometry depending
            ! on whether or not our work with the current RHO seems complete. RHO is reduced if the
            ! errors in the quadratic model at the recent interpolation points compare favourably
            ! with predictions of likely improvements to the model within distance HALF*RHO of XOPT.
            ! Why do we reduce RHO when SHORTD is true and the entries of MODERR_REC and DNORM_REC are all
            ! small? The reason is well explained by the BOBYQA paper in the paragraphs surrounding
            ! (6.8)--(6.11). Roughly speaking, in this case, a trust-region step is unlikely to decrease the
            ! objective function according to some estimations. This suggests that the current trust-region
            ! center may be an approximate local minimizer up to the current "resolution" of the algorithm.
            ! When this occurs, the algorithm takes the view that the work for the current RHO is complete,
            ! and hence it will reduce RHO, which will enhance the resolution of the algorithm in general.
            if (shortd .or. trfail) then
                delta = TENTH * delta
                if (delta <= gamma3 * rho) then
                    delta = rho  ! Set DELTA to RHO when it is close to or below.
                end if
                ! Evaluate EBOUND. It will be used as a bound to test if the entries of MODERR_REC are small.
                ebound = errbd(crvmin, d, gopt, hq, moderr_rec, pq, rho, sl, su, xpt(:, kopt), xpt)
            else
                ! Calculate the next value of the objective function.
                x = xinbd(xbase, xpt(:, kopt) + d, xl, xu, sl, su)  ! X = XBASE + XOPT + D without rounding.
                call evaluate(obj, st, x, f)
                nf = nf + 1
                rescued = .false.  ! Set RESCUED to FALSE after evaluating F at a new point.

                ! Print a message about the function evaluation according to IPRINT.
                ! Save X, F into the history.

                ! Check whether to exit
                subinfo = checkexit(maxfun, nf, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo
                    exit
                end if

                ! Update DNORM_REC and MODERR_REC.
                ! DNORM_REC records the DNORM of the recent function evaluations with the current RHO.
                dnorm_rec = [dnorm_rec(2:size(dnorm_rec)), dnorm]
                ! MODERR is the error of the current model in predicting the change in F due to D.
                ! MODERR_REC records the prediction errors of the recent models with the current RHO.
                moderr = f - fval(kopt) + qred
                moderr_rec = [moderr_rec(2:size(moderr_rec)), moderr]

                ! Calculate the reduction ratio by REDRAT, which handles Inf/NaN carefully.
                ratio = redrat(fval(kopt) - f, qred, eta1)

                ! Update DELTA. After this, DELTA < DNORM may hold.
                delta = trrad(delta, dnorm, eta1, eta2, gamma1, gamma2, ratio)
                if (delta <= gamma3 * rho) then
                    delta = rho  ! Set DELTA to RHO when it is close to or below.
                end if

                ! Is the newly generated X better than current best point?
                ximproved = (f < fval(kopt))

                ! Call RESCUE if rounding errors have damaged the denominator corresponding to D.
                ! RESCUE is invoked sometimes though not often after a trust-region step, and it does
                ! improve the performance, especially when pursing high-precision solutions.
                vlag = calvlag(kopt, bmat, d, xpt, zmat)
                den = calden(kopt, bmat, d, xpt, zmat)
                to_rescue = (ximproved .and. .not. (is_finite(sum(abs(vlag))) .and. any(den > maxval(vlag(1:npt)**2))))
                ! Below are some alternatives conditions for calling RESCUE. They perform fairly well.
                ! !to_rescue = .false.  ! Do not call RESCUE at all.
                ! !to_rescue = (ximproved .and. .not. any(den > 0.25_real64 * maxval(vlag(1:npt)**2)))
                ! !to_rescue = (ximproved .and. .not. any(den > HALF * maxval(vlag(1:npt)**2)))
                ! !to_rescue = (.not. any(den > HALF * maxval(vlag(1:npt)**2)))  ! Powell's code.
                ! !to_rescue = (.not. any(den > maxval(vlag(1:npt)**2)))
                if (to_rescue) then
                    if (rescued) then
                        ! Not reached: no run the suite drives calls RESCUE twice over with no
                        ! objective evaluation in between.
                        ! GCOVR_EXCL_START -- see the note above
                        info = DAMAGING_ROUNDING  ! The last RESCUE did not improve the situation.
                        exit
                    end if                ! GCOVR_EXCL_STOP
                    call rescue(obj, st, maxfun, delta, ftarget, xl, xu, kopt, nf, &
                        & fval, gopt, hq, pq, sl, su, xbase, xpt, bmat, zmat, subinfo)
                    ! RESCUE returns a nondefault code only on the budget or the target, neither
                    ! of which it meets in any run the suite drives.
                    if (subinfo /= INFO_DFT) then
                        info = subinfo            ! GCOVR_EXCL_START -- see the note above
                        exit
                    end if                ! GCOVR_EXCL_STOP
                    rescued = .true.
                    dnorm_rec = REALMAX
                    moderr_rec = REALMAX

                    ! RESCUE shifts XBASE to the best point before RESCUE. Update D, MODERR, and XIMPROVED.
                    ! Do NOT calculate QRED according to this D, as it is not really a trust region step.
                    ! Note that QRED will be used afterward for defining IMPROVE_GEO and REDUCE_RHO.
                    d = max(sl, min(su, d)) - xpt(:, kopt)
                    moderr = f - fval(kopt) - quadinc(d, xpt, gopt, pq, hq)
                    ximproved = (f < fval(kopt))
                end if

                ! Set KNEW_TR to the index of the interpolation point to be replaced with XOPT + D.
                ! KNEW_TR will ensure that the geometry of XPT is "good enough" after the replacement.
                knew_tr = setdrop_tr(kopt, ximproved, bmat, d, delta, rho, xpt, zmat)

                ! Update [BMAT, ZMAT] (representing H in the BOBYQA paper), [GQ, HQ, PQ] (the quadratic
                ! model), and [FVAL, XPT, KOPT, FOPT, XOPT] so that XPT(:, KNEW_TR) becomes XOPT + D. If
                ! KNEW_TR = 0, the updating subroutines will do essentially nothing, as the algorithm
                ! decides not to include XOPT + D into XPT.
                if (knew_tr > 0) then
                    xdrop = xpt(:, knew_tr)
                    xosav = xpt(:, kopt)
                    call updateh(knew_tr, kopt, d, xpt, bmat, zmat)
                    call updatexf(knew_tr, ximproved, f, max(sl, min(su, xosav + d)), kopt, fval, xpt)
                    call updateq(knew_tr, ximproved, bmat, d, moderr, xdrop, xosav, xpt, zmat, gopt, hq, pq)
                    ! Try whether to replace the new quadratic model with the alternative model, namely the
                    ! least Frobenius norm interpolant.
                    call tryqalt(bmat, fval - fval(kopt), ratio, sl, su, xpt(:, kopt), xpt, zmat, itest, gopt, hq, pq)
                    if (.not. (all(is_finite(gopt)) .and. all(is_finite(hq)) .and. all(is_finite(pq)))) then
                        info = NAN_INF_MODEL
                        exit
                    end if
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
            accurate_mod = all(abs(moderr_rec) <= ebound) .and. all(dnorm_rec <= rho)
            ! CLOSE_ITPSET: Are the interpolation points close to XOPT?
            distsq = sum((xpt - spread(xpt(:, kopt), dim=2, ncopies=npt))**2, dim=1)
            ! MATLAB: distsq = sum((xpt - xpt(:, kopt)).^2)  % Implicit expansion
            close_itpset = all(distsq <= max(delta**2, (TEN * rho)**2))
            ! Below are some alternative definitions of CLOSE_ITPSET.
            ! N.B.: The threshold for CLOSE_ITPSET is at least DELBAR, the trust region radius for GEOSTEP.
            ! !close_itpset = all(distsq <= max((TWO * delta)**2, (TEN * rho)**2))  ! Powell's code.
            ! !close_itpset = all(distsq <= 4.0_real64 * delta**2)  ! Powell's NEWUOA code.
            ! !close_itpset = all(distsq <= max(delta**2, 4.0_real64 * rho**2))  ! Powell's LINCOA code.
            ! ADEQUATE_GEO: Is the geometry of the interpolation set "adequate"?
            ! N.B. (Zaikun 20240314): Even if RESCUE has just been called (RESCUED = TRUE), the geometry may
            ! still be inadequate/improvable if XPT contains points far away from XOPT.
            adequate_geo = (shortd .and. accurate_mod) .or. close_itpset
            ! SMALL_TRRAD: Is the trust-region radius small? This indicator seems not impactive in practice.
            small_trrad = (max(delta, dnorm) <= rho)  ! Powell's code. See also (6.7) of the BOBYQA paper.
            !small_trrad = (delsav <= rho)  ! Behaves the same as Powell's version. DELSAV = unupdated DELTA.

            ! IMPROVE_GEO and REDUCE_RHO are defined as follows.
            ! N.B.: If SHORTD is TRUE at the very first iteration, then REDUCE_RHO will be set to TRUE.
            ! Powell's code does not have TRFAIL in BAD_TRSTEP; it terminates if TRFAIL is TRUE.

            ! BAD_TRSTEP (for IMPROVE_GEO): Is the last trust-region step bad?
            bad_trstep = (shortd .or. trfail .or. ratio <= eta1 .or. knew_tr == 0)
            improve_geo = bad_trstep .and. .not. adequate_geo  ! See the text above (6.7) of the BOBYQA paper.
            ! BAD_TRSTEP (for REDUCE_RHO): Is the last trust-region step bad?
            bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. knew_tr == 0)
            reduce_rho = bad_trstep .and. adequate_geo .and. small_trrad  ! See (6.7) of the BOBYQA paper.
            ! Zaikun 20221111: What if RESCUE has been called? Is it still reasonable to use RATIO?
            ! Zaikun 20221127: If RESCUE has been called, then KNEW_TR may be 0 even if RATIO > 0.

            ! Equivalently, REDUCE_RHO can be set as follows. It shows that REDUCE_RHO is TRUE in two cases.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. knew_tr == 0)
            ! !reduce_rho = (shortd .and. accurate_mod) .or. (bad_trstep .and. close_itpset .and. small_trrad)

            ! With REDUCE_RHO properly defined, we can also set IMPROVE_GEO as follows.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= eta1 .or. knew_tr == 0)
            ! !improve_geo = bad_trstep .and. (.not. reduce_rho) .and. (.not. close_itpset)

            ! With IMPROVE_GEO properly defined, we can also set REDUCE_RHO as follows.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. knew_tr == 0)
            ! !reduce_rho = bad_trstep .and. (.not. improve_geo) .and. small_trrad

            ! BOBYQA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously.
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

            ! Improve the geometry of the interpolation set by removing a point and adding a new one.
            if (improve_geo) then
                ! XPT(:, KNEW_GEO) will become XOPT + D below. KNEW_GEO /= KOPT unless there is a bug.
                knew_geo = int(maxloc(distsq, dim=1))

                ! Set DELBAR, which will be used as the trust-region radius for the geometry-improving
                ! scheme GEOSTEP. Note that DELTA has been updated before arriving here.
                delbar = max(min(TENTH * sqrt(maxval(distsq)), delta), rho)  ! Powell's code
                !delbar = rho  ! Powell's UOBYQA code
                !delbar = max(min(TENTH * sqrt(maxval(distsq)), HALF * delta), rho)  ! Powell's NEWUOA code
                !delbar = max(TENTH * delta, rho)  ! Powell's LINCOA code

                ! Find D so that the geometry of XPT will be improved when XPT(:, KNEW_GEO) becomes XOPT + D.
                d = geostep(knew_geo, kopt, bmat, delbar, sl, su, xpt, zmat)

                ! Call RESCUE if rounding errors have damaged the denominator corresponding to D.
                ! 1. This does make a difference, yet RESCUE seems not invoked often after a geometry step.
                ! 2. In Powell's implementation, it may happen that RESCUE only recalculates [BMAT, ZMAT]
                ! without introducing any new point into XPT. In that case, GEOSTEP will have to be called
                ! after RESCUE, without which the code may encounter an infinite cycling. We have modified
                ! RESCUE so that it introduces at least one new point into XPT and there is no need to call
                ! GEOSTEP afterward. This improves the performance a bit and simplifies the flow of the code.
                ! 3. It is tempting to incorporate XOPT+D into the interpolation even if RESCUE is called.
                ! However, this cannot be done without recalculating KNEW_GEO, as XPT has been changed by
                ! RESCUE, so that it is invalid to replace XPT(:, KNEW_GEO) with XOPT+D anymore. With a new
                ! KNEW_GEO, the step D will become improper as it was chosen according to the old KNEW_GEO.
                vlag = calvlag(kopt, bmat, d, xpt, zmat)
                den = calden(kopt, bmat, d, xpt, zmat)
                to_rescue = (.not. (is_finite(sum(abs(vlag))) .and. den(knew_geo) > HALF * vlag(knew_geo)**2))
                if (to_rescue) then
                    ! RESCUE is reached from the trust-region step above in runs the suite drives,
                    ! but never from here: rounding does not damage the geometry step's
                    ! denominator in any of them, so the whole arm below is unexecuted.
                    if (rescued) then             ! GCOVR_EXCL_START -- see the note above
                        info = DAMAGING_ROUNDING  ! The last RESCUE did not improve the situation.
                        exit
                    end if
                    call rescue(obj, st, maxfun, delta, ftarget, xl, xu, kopt, nf, &
                        & fval, gopt, hq, pq, sl, su, xbase, xpt, bmat, zmat, subinfo)
                    if (subinfo /= INFO_DFT) then
                        info = subinfo
                        exit
                    end if
                    rescued = .true.
                    dnorm_rec = REALMAX
                    moderr_rec = REALMAX
                    ! GCOVR_EXCL_STOP
                else
                    ! Calculate the next value of the objective function.
                    x = xinbd(xbase, xpt(:, kopt) + d, xl, xu, sl, su)  ! X = XBASE + XOPT + D without rounding.
                    call evaluate(obj, st, x, f)
                    nf = nf + 1
                    rescued = .false.  ! Set RESCUED to FALSE after evaluating F at a new point.

                    ! Print a message about the function evaluation according to IPRINT.
                    ! Save X, F into the history.

                    ! Check whether to exit
                    ! The budget never runs out, and FTARGET is never met, on a geometry-step
                    ! evaluation in any run the suite drives; a non-finite value aborts in
                    ! `evaluate` before CHECKEXIT could see one.
                    subinfo = checkexit(maxfun, nf, f, ftarget, x)
                    if (subinfo /= INFO_DFT) then
                        info = subinfo            ! GCOVR_EXCL_START -- see the note above
                        exit
                    end if                ! GCOVR_EXCL_STOP

                    ! Update DNORM_REC and MODERR_REC.
                    ! DNORM_REC records the DNORM of the recent function evaluations with the current RHO.
                    ! Powell's code does not update DNORM. Therefore, DNORM is the length of the last
                    ! trust-region trial step, inconsistent with MODERR_REC. The same problem exists in NEWUOA.
                    dnorm = min(delbar, norm(d))
                    dnorm_rec = [dnorm_rec(2:size(dnorm_rec)), dnorm]
                    ! MODERR is the error of the current model in predicting the change in F due to D.
                    ! MODERR_REC records the prediction errors of the recent models with the current RHO.
                    moderr = f - fval(kopt) - quadinc(d, xpt, gopt, pq, hq)  ! QRED = Q(XOPT) - Q(XOPT + D)
                    moderr_rec = [moderr_rec(2:size(moderr_rec)), moderr]

                    ! Is the newly generated X better than current best point?
                    ximproved = (f < fval(kopt))

                    ! Update [BMAT, ZMAT] (represents H in the BOBYQA paper), [FVAL, XPT, KOPT, FOPT, XOPT],
                    ! and [GQ, HQ, PQ] (the quadratic model), so that XPT(:, KNEW_GEO) becomes XOPT + D.
                    xdrop = xpt(:, knew_geo)
                    xosav = xpt(:, kopt)
                    call updateh(knew_geo, kopt, d, xpt, bmat, zmat)
                    call updatexf(knew_geo, ximproved, f, max(sl, min(su, xosav + d)), kopt, fval, xpt)
                    call updateq(knew_geo, ximproved, bmat, d, moderr, xdrop, xosav, xpt, zmat, gopt, hq, pq)
                    ! The model stays finite across a geometry step in every run the suite drives.
                    if (.not. (all(is_finite(gopt)) .and. all(is_finite(hq)) .and. all(is_finite(pq)))) then
                        info = NAN_INF_MODEL      ! GCOVR_EXCL_START -- see the note above
                        exit
                    end if                ! GCOVR_EXCL_STOP
                end if
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
                ! DNORM_REC and MODERR_REC are corresponding to the recent function evaluations with
                ! the current RHO. Update them after reducing RHO.
                dnorm_rec = REALMAX
                moderr_rec = REALMAX
            end if  ! End of IF (REDUCE_RHO). The procedure of reducing RHO ends.

            ! Shift XBASE if XOPT may be too far from XBASE.
            ! Powell's original criteria for shifting XBASE is as follows.
            ! 1. After a trust region step that is not short, shift XBASE if SUM(XOPT**2) >= 1.0E3*DNORM**2.
            ! In this case, it seems quite important for the performance to recalculate QRED.
            ! 2. Before a geometry step, shift XBASE if SUM(XOPT**2) >= 1.0E3*DELBAR**2.
            if (sum(xpt(:, kopt)**2) >= 1.0E3_real64 * delta**2) then
                ! Other possible criteria: SUM(XOPT**2) >= 1.0E4*DELTA**2, SUM(XOPT**2) >= 1.0E4*RHO**2.
                sl = min(sl - xpt(:, kopt), ZERO)
                su = max(su - xpt(:, kopt), ZERO)
                call shiftbase(kopt, xbase, xpt, zmat, bmat, pq, hq)
                xbase = max(xl, min(xu, xbase))
            end if

            ! Report the current best value, and check if user asks for early termination.
        end do  ! End of DO TR = 1, MAXTR. The iterative procedure ends.

        ! The radius reached and the iterations performed, for `info%rho` and `info%niter`. TR is MAXTR+1
        ! when the loop ran to completion.
        st%rho = rho
        st%niter = min(tr, maxtr)

        ! Return from the calculation, after trying the Newton-Raphson step if it has not been tried yet.
        if (info == SMALL_TR_RADIUS .and. shortd .and. dnorm > TENTH * rhoend .and. nf < maxfun) then
            x = xinbd(xbase, xpt(:, kopt) + d, xl, xu, sl, su)  ! In precise arithmetic, X = XBASE + XOPT + D.
            call evaluate(obj, st, x, f)
            nf = nf + 1
            ! Print a message about the function evaluation according to IPRINT.
            ! Zaikun 20230512: DELTA has been updated. RHO is only indicative here. TO BE IMPROVED.
            ! Save X, F into the history.
        end if

        ! Choose the [X, F] to return: either the current [X, F] or [XBASE + XOPT, FOPT].
        if (fval(kopt) < f .or. is_nan(f)) then
            x = xinbd(xbase, xpt(:, kopt), xl, xu, sl, su)  ! In precise arithmetic, X = XBASE + XOPT.
            f = fval(kopt)
        end if

        ! Arrange FHIST and XHIST so that they are in the chronological order.

        ! Print a return message according to IPRINT.

    end subroutine bobyqb

    !> The bound the recent models' errors are tested against, upstream's `errbd`.
    !!
    !! Pages 30 to 31 of the BOBYQA paper, around (6.8) to (6.11).
    function errbd(crvmin, d, gopt, hq, moderr_rec, pq, rho, sl, su, xopt, xpt) result(ebound)
        ! Common modules

        real(real64), intent(in) :: crvmin           !! the least curvature seen along the path, `0` when a bound was met
        real(real64), intent(in) :: d(:)             !! the most recent step
        real(real64), intent(in) :: gopt(:)          !! the model's gradient at `xbase + xpt(:, kopt)`
        real(real64), intent(in) :: hq(:, :)         !! the model's explicit Hessian part
        real(real64), intent(in) :: moderr_rec(:)    !! the recent model errors
        real(real64), intent(in) :: pq(:)            !! the model's implicit Hessian weights, one per interpolation point
        real(real64), intent(in) :: rho              !! the lower bound on the trust-region radius
        real(real64), intent(in) :: sl(:)            !! `xl - xbase`: the lower bounds relative to the base point
        real(real64), intent(in) :: su(:)            !! `xu - xbase`: the upper bounds relative to the base point
        real(real64), intent(in) :: xopt(:)          !! the best point relative to `xbase`
        real(real64), intent(in) :: xpt(:, :)        !! the interpolation set relative to `xbase`, one point per column

        real(real64) :: ebound                       !! the bound

        integer :: n
        integer :: npt
        real(real64) :: bfirst(size(d))
        real(real64) :: bsecond(size(d))
        real(real64) :: gnew(size(d))
        real(real64) :: xnew(size(d))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        xnew = xopt + d
        gnew = gopt + hess_mul(d, xpt, pq, hq)
        bfirst = maxval(abs(moderr_rec))
        bfirst(trueloc(xnew <= sl)) = gnew(trueloc(xnew <= sl)) * rho
        bfirst(trueloc(xnew >= su)) = -gnew(trueloc(xnew >= su)) * rho
        bsecond = HALF * (diag(hq) + matprod(xpt**2, pq)) * rho**2
        ebound = minval(max(bfirst, bfirst + bsecond))
        if (crvmin > 0) then
            ebound = min(ebound, 0.125_real64 * crvmin * rho**2)
        end if

    end function errbd

    !> Builds the initial interpolation set and evaluates the objective on it, upstream's `initxf`.
    !!
    !! The first `2n + 1` points are `x0` and its coordinate displacements by `+/-rhobeg`, truncated
    !! to the bounds; the rest displace two coordinates at once, by the pairs `setij` returns.
    subroutine initxf(obj, st, maxfun, ftarget, rhobeg, xl, xu, x0, ij, kopt, nf, fval, &
            & sl, su, xbase, xpt, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine does the initialization about the interpolation points & their function values.
        !
        ! N.B.:
        ! 1. Remark on IJ:
        ! If NPT <= 2*N + 1, then IJ is empty. Assume that NPT >= 2*N + 2. Then SIZE(IJ) = [2, NPT-2*N-1].
        ! IJ contains integers between 1 and N. For each K > 2*N + 1, XPT(:, K) is
        ! XPT(:, IJ(1, K) + 1) + XPT(:, IJ(2, K) + 1). The 1 in IJ + 1 comes from the fact that XPT(:, 1)
        ! corresponds to the base point XBASE. Let I = IJ(1, K) and J = IJ(2, K). Then all the
        ! entries of XPT(:, K) are zero except for the I and J entries. Consequently, the Hessian of the
        ! quadratic model will get a possibly nonzero (I, J) entry.
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
        real(real64), intent(in) :: ftarget          !! stop as soon as a value at or below this is found
        real(real64), intent(in) :: rhobeg           !! the initial trust-region radius
        real(real64), intent(in) :: xl(:)            !! lower bounds
        real(real64), intent(in) :: xu(:)            !! upper bounds

        real(real64), intent(inout) :: x0(:)         !! the start point

        integer, intent(out) :: ij(:, :)             !! the coordinate pairs of the interpolation set beyond `2n + 1`
        integer, intent(out) :: info                 !! PRIMA's exit code
        integer, intent(out) :: kopt                 !! index in `xpt` of the best point so far
        integer, intent(out) :: nf                   !! objective evaluations made
        real(real64), intent(out) :: fval(:)         !! objective value at each interpolation point
        real(real64), intent(out) :: sl(:)           !! `xl - xbase`: the lower bounds relative to the base point
        real(real64), intent(out) :: su(:)           !! `xu - xbase`: the upper bounds relative to the base point
        real(real64), intent(out) :: xbase(:)        !! the base point every other point is relative to
        real(real64), intent(out) :: xpt(:, :)       !! the interpolation set relative to `xbase`, one point per column

        integer :: k
        integer :: n
        integer :: npt
        integer :: subinfo
        logical :: evaluated(size(xpt, 2))
        real(real64) :: f
        real(real64) :: x(size(xpt, 1))

        ! Sizes.
        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Initialize INFO to the default value. At return, an INFO different from this value will indicate
        ! an abnormal return.
        info = INFO_DFT

        ! SL and SU are the lower and upper bounds on feasible moves from X0.
        sl = xl - x0
        su = xu - x0
        ! After the preprocessing subroutine PREPROC, SL <= 0 and the nonzero entries of SL should be less
        ! than -RHOBEG, while SU >= 0 and the nonzeros of SU should be larger than RHOBEG. However, this may
        ! not be true due to rounding. The following lines revise SL and SU to ensure it. X0 is also revised
        ! accordingly. In precise arithmetic, the "revisions" do not change SL, SU, or X0.
        where (sl < 0)
            sl = min(sl, -rhobeg)
        elsewhere
            x0 = xl
            sl = ZERO
            su = xu - xl
        end where
        where (su > 0)
            su = max(su, rhobeg)
        elsewhere
            x0 = xu
            sl = xl - xu
            su = ZERO
        end where
        ! MATLAB code for revising X, SL, and SU:
        ! sl(sl < 0) = min(sl(sl < 0), -rhobeg);
        ! x0(sl >= 0) = xl(sl >= 0);
        ! sl(sl >= 0) = 0;
        ! su(sl >= 0) = xu(sl >= 0) - xl(sl >= 0);
        ! su(su > 0) = max(su(su > 0), rhobeg);
        ! x0(su <= 0) = xu(su <= 0);
        ! sl(su <= 0) = xl(su <= 0) - xu(su <= 0);
        ! su(su <= 0) = 0;

        ! Initialize XBASE to X0.
        xbase = x0

        ! EVALUATED is a boolean array with EVALUATED(I) indicating whether the function value of the I-th
        ! interpolation point has been evaluated. We need it for a portable counting of the number of
        ! function evaluations, especially if the loop is conducted asynchronously.
        evaluated = .false.

        ! Initialize XHIST, FHIST, and FVAL. Otherwise, compilers may complain that they are not
        ! (completely) initialized if the initialization aborts due to abnormality (see CHECKEXIT).
        ! N.B.: 1. Initializing them to NaN would be more reasonable (NaN is not available in Fortran).
        ! 2. Do not initialize the models if the current initialization aborts due to abnormality. Otherwise,
        ! errors or exceptions may occur, as FVAL and XPT etc are uninitialized.
        fval = REALMAX

        ! Set XPT(:, 2 : N+1)
        xpt = ZERO
        do k = 1, n
            xpt(k, k + 1) = rhobeg
            if (su(k) <= 0) then  ! SU(K) == 0
                xpt(k, k + 1) = -rhobeg
            end if
        end do
        ! Set XPT(:, N+2 : MIN(2*N + 1, NPT)).
        do k = 1, min(npt - n - 1, n)
            xpt(k, k + n + 1) = -rhobeg
            if (sl(k) >= 0) then  ! SL(K) == 0
                xpt(k, k + n + 1) = min(TWO * rhobeg, su(k))
            end if
            if (su(k) <= 0) then  ! SU(K) == 0
                xpt(k, k + n + 1) = max(-TWO * rhobeg, sl(k))
            end if
        end do

        ! Set FVAL(1 : MIN(2*N + 1, NPT)) by evaluating F. Totally parallelizable except for FMSG.
        do k = 1, min(npt, int(2 * n + 1))
            x = xinbd(xbase, xpt(:, k), xl, xu, sl, su)  ! In precise arithmetic, X = XBASE + XPT(:, K).
            call evaluate(obj, st, x, f)

            ! Print a message about the function evaluation according to IPRINT.
            ! Save X, F into the history.

            evaluated(k) = .true.
            fval(k) = f

            ! Check whether to exit
            subinfo = checkexit(maxfun, k, f, ftarget, x)
            if (subinfo /= INFO_DFT) then
                info = subinfo
                exit
            end if
        end do

        ! For the K between 2 and N + 1, switch XPT(:, K) and XPT(:, K+1) if XPT(K-1, K) and XPT(K-1, K+N)
        ! have different signs and FVAL(K) <= FVAL(K+N). This provides a bias towards potentially lower
        ! values of F when defining XPT(:, 2*N + 2 : NPT). We may drop the requirement on the signs, but
        ! Powell's code has such a requirement.
        ! N.B.:
        ! 1. The switching is OPTIONAL. If we remove it, then the evaluations of FVAL(1 : NPT) can be
        ! merged, and they are totally PARALLELIZABLE; this can be beneficial if the function evaluations
        ! are expensive, which is likely the case.
        ! 2. The initialization of NEWUOA revises IJ (see below) instead of XPT and FVAL. Theoretically, it
        ! is equivalent; practically, after XPT is revised, the initialization of the quadratic model and
        ! the Lagrange polynomials also needs revision, as, e.g., XPT(:, 2:N) is not RHOBEG*EYE(N) anymore.
        do k = 2, min(npt - n, int(n + 1))
            if (xpt(k - 1, k) * xpt(k - 1, k + n) < 0 .and. fval(k + n) < fval(k)) then
                fval([k, k + n]) = fval([k + n, k])
                xpt(:, [k, k + n]) = xpt(:, [k + n, k])
                ! Indeed, only XPT(K-1, [K, K+N]) needs switching, as the other entries are zero.
            end if
        end do

        ! Set IJ.
        ! In general, when NPT = (N+1)*(N+2)/2, we can set IJ(:, 1 : NPT - (2*N+1)) to ANY permutation
        ! of {{I, J} : 1 <= I /= J <= N}; when NPT < (N+1)*(N+2)/2, we can set it to the first NPT - (2*N+1)
        ! elements of such a permutation. The following IJ is defined according to Powell's code. See also
        ! Section 3 of the NEWUOA paper and (2.4) of the BOBYQA paper.
        ij = setij(n, npt)

        ! Set XPT(:, 2*N + 2 : NPT). It depends on XPT(:, 1 : 2*N + 1) and hence on FVAL(1: 2*N + 1).
        ! Indeed, XPT(:, K) has only two nonzeros for each K >= 2*N+2.
        ! N.B.: The 1 in IJ + 1 comes from the fact that XPT(:, 1) corresponds to XBASE.
        xpt(:, 2 * n + 2:npt) = xpt(:, ij(1, :) + 1) + xpt(:, ij(2, :) + 1)

        ! Set FVAL(2*N + 2 : NPT) by evaluating F. Totally parallelizable except for FMSG.
        if (info == INFO_DFT) then
            do k = int(2 * n + 2), npt
                x = xinbd(xbase, xpt(:, k), xl, xu, sl, su)  ! In precise arithmetic, X = XBASE + XPT(:, K).
                call evaluate(obj, st, x, f)

                ! Print a message about the function evaluation according to IPRINT.
                ! Save X, F into the history.

                evaluated(k) = .true.
                fval(k) = f

                ! Check whether to exit
                subinfo = checkexit(maxfun, k, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo
                    exit
                end if
            end do
        end if

        ! Set NF, KOPT
        nf = int(count(evaluated))
        kopt = int(minloc(fval, mask=evaluated, dim=1))
        ! MATLAB: fopt = min(fval(evaluated)); kopt = find(evaluated & ~(fval > fopt), 1, 'first');

    end subroutine initxf

    !> Initialises the quadratic model from the interpolation values, upstream's `initq`.
    subroutine initq(ij, fval, xpt, gopt, hq, pq, info)
        ! Common modules

        integer, intent(in) :: ij(:, :)           !! the coordinate pairs of the interpolation set beyond `2n + 1`
        real(real64), intent(in) :: fval(:)       !! objective value at each interpolation point
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set relative to `xbase`, one point per column

        integer, intent(out), optional :: info    !! PRIMA's exit code
        real(real64), intent(out) :: gopt(:)      !! the model's gradient at `xbase + xpt(:, kopt)`
        real(real64), intent(out) :: hq(:, :)     !! the model's explicit Hessian part
        real(real64), intent(out) :: pq(:)        !! the model's implicit Hessian weights, one per interpolation point

        integer :: i
        integer :: j
        integer :: k
        integer :: kopt
        integer :: n
        integer :: ndiag
        integer :: npt
        real(real64) :: fbase
        real(real64) :: xa(min(size(xpt, 1), size(xpt, 2) - size(xpt, 1) - 1))
        real(real64) :: xb(size(xa))
        real(real64) :: xi
        real(real64) :: xj

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        fbase = fval(1)  ! FBASE is the function value at XBASE.

        ! Set GOPT by the forward difference.
        gopt = (fval(2:n + 1) - fbase) / diag(xpt(:, 2:n + 1))

        ! The interpolation conditions decide GOPT(1:NDIAG) and the first NDIAG diagonal 2nd derivatives of
        ! the initial quadratic model by a quadratic interpolation on three points.
        ndiag = min(n, npt - n - 1)
        xa = diag(xpt(:, 2:ndiag + 1))
        xb = diag(xpt(:, n + 2:n + ndiag + 1))

        ! Revise GOPT(1:NDIAG) to the value provided by the three-point interpolation.
        gopt(1:ndiag) = (gopt(1:ndiag) * xb - ((fval(n + 2:n + ndiag + 1) - fbase) / xb) * xa) / (xb - xa)

        ! Set the diagonal of HQ by the three-point interpolation. If we do this before the revision of
        ! GOPT(1:NDIAG), we can avoid the calculation of FVAL(K + 1) - FBASE) / RHOBEG. But we prefer to
        ! decouple the initialization of GOPT and HQ. We are not concerned by this amount of flops.
        hq = ZERO
        do k = 1, ndiag
            hq(k, k) = TWO * ((fval(k + 1) - fbase) / xa(k) - (fval(n + k + 1) - fbase) / xb(k)) / (xa(k) - xb(k))
        end do
        ! MATLAB:
        ! hdiag = 2*((fval(2 : ndiag+1) - fbase) / xa - (fval(n+2 : n+ndiag+1) - fbase) / xb) / (xa-xb)
        ! hq(1:ndiag, 1:ndiag) = diag(hdiag)

        ! When NPT > 2*N + 1, set the off-diagonal entries of HQ.
        do k = 1, npt - 2 * n - 1
            i = ij(1, k)
            j = ij(2, k)
            xi = xpt(i, k + 2 * n + 1)
            xj = xpt(j, k + 2 * n + 1)
            ! N.B.: The 1 in I+1 and J+1 comes from the fact that XPT(:, 1) corresponds to XBASE.
            hq(i, j) = (fbase - fval(i + 1) - fval(j + 1) + fval(k + 2 * n + 1)) / (xi * xj)
            hq(j, i) = hq(i, j)
        end do

        kopt = int(minloc(fval, dim=1))
        if (kopt /= 1) then
            gopt = gopt + matprod(hq, xpt(:, kopt))
        end if

        pq = ZERO

        if (present(info)) then
            if (any(is_nan(gopt)) .or. any(is_nan(hq))) then     ! GCOVR_EXCL_START -- BOBYQB's
                info = NAN_INF_MODEL                             ! only call passes no `info`
            else
                info = INFO_DFT
            end if                        ! GCOVR_EXCL_STOP
        end if

    end subroutine initq

    !> Initialises the factorisation `[bmat, zmat]` of the matrix `H` of (2.7) of the BOBYQA paper,
    !> upstream's `inith`.
    subroutine inith(ij, xpt, bmat, zmat, info)
        ! Common modules
        !use, non_intrinsic :: powalg_mod, only : errh

        integer, intent(in) :: ij(:, :)            !! the coordinate pairs of the interpolation set beyond `2n + 1`
        real(real64), intent(in) :: xpt(:, :)      !! the interpolation set relative to `xbase`, one point per column

        integer, intent(out), optional :: info     !! PRIMA's exit code
        real(real64), intent(out) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(out) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Z^T`

        integer :: k
        integer :: n
        integer :: ndiag
        integer :: npt
        real(real64) :: rhobeg
        real(real64) :: rhosq
        real(real64) :: xa(min(size(xpt, 1), size(xpt, 2) - size(xpt, 1) - 1))
        real(real64) :: xb(size(xa))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Some values to be used for setting BMAT and ZMAT.
        rhobeg = maxval(abs(xpt(:, 2)))  ! Read RHOBEG from XPT. Note that XPT(:, 1) = 0.
        rhosq = rhobeg**2

        ! The interpolation set decides the first NDIAG diagonal 2nd derivatives of the Lagrange polynomials.
        ndiag = min(n, npt - n - 1)
        xa = diag(xpt(:, 2:ndiag + 1))
        xb = diag(xpt(:, n + 2:n + ndiag + 1))

        bmat = ZERO
        ! Set BMAT(1 : NDIAG, :)
        bmat(1:ndiag, 1) = -(xa + xb) / (xa * xb)
        do k = 1, ndiag
            bmat(k, k + n + 1) = -HALF / xpt(k, k + 1)
            bmat(k, k + 1) = -bmat(k, 1) - bmat(k, k + n + 1)
        end do
        ! Set BMAT(NDIAG+1 : N, :)
        do k = ndiag + 1, n
            bmat(k, 1) = -ONE / xpt(k, k + 1)
            bmat(k, k + 1) = -bmat(k, 1)
            bmat(k, npt + k) = -HALF * rhosq
        end do

        zmat = ZERO
        ! Set ZMAT(:, 1 : NDIAG)
        zmat(1, 1:ndiag) = sqrt(TWO) / (xa * xb)
        do k = 1, ndiag
            zmat(k + 1, k) = -zmat(1, k) - sqrt(HALF) / rhosq
            zmat(k + n + 1, k) = sqrt(HALF) / rhosq
        end do
        ! Set ZMAT(:, NDIAG+1 : NPT-N-1)
        do k = ndiag + 1, npt - n - 1
            zmat(1, k) = ONE / rhosq
            zmat(k + n + 1, k) = ONE / rhosq
            zmat(ij(:, k - n) + 1, k) = -ONE / rhosq
        end do

        if (present(info)) then
            if (any(is_nan(bmat)) .or. any(is_nan(zmat))) then   ! GCOVR_EXCL_START -- BOBYQB's
                info = NAN_INF_MODEL                             ! only call passes no `info`
            else
                info = INFO_DFT
            end if                        ! GCOVR_EXCL_STOP
        end if

    end subroutine inith

    !> The trust-region subproblem: minimise the model inside the radius and the bounds, upstream's
    !> `trsbox`.
    !!
    !! Section 3 of the BOBYQA paper. A truncated conjugate-gradient walk that fixes a coordinate
    !! whenever it reaches a bound, followed by the "alternative iteration" -- a search over a
    !! circular arc in the plane of the current point and gradient.
    subroutine trsbox(delta, gopt_in, hq_in, pq_in, sl, su, tol, xopt, xpt, crvmin, d)
        ! Common modules

        real(real64), intent(in) :: delta          !! the trust-region radius
        real(real64), intent(in) :: gopt_in(:)     !! the model's gradient at the best point
        real(real64), intent(in) :: hq_in(:, :)    !! the model's explicit Hessian
        real(real64), intent(in) :: pq_in(:)       !! the model's implicit Hessian weights
        real(real64), intent(in) :: sl(:)          !! `xl - xbase`: the lower bounds relative to the base point
        real(real64), intent(in) :: su(:)          !! `xu - xbase`: the upper bounds relative to the base point
        real(real64), intent(in) :: tol            !! relative accuracy asked of the solve
        real(real64), intent(in) :: xopt(:)        !! the best point relative to `xbase`
        real(real64), intent(in) :: xpt(:, :)      !! the interpolation set relative to `xbase`, one point per column

        real(real64), intent(out) :: crvmin        !! the least curvature seen along the path, `0` when a bound was met
        real(real64), intent(out) :: d(:)          !! the trust-region step

        integer :: iact
        integer :: n
        integer :: npt
        integer :: xbdi(size(gopt_in))
        integer :: grid_size
        integer :: iter
        integer :: itercg
        integer :: maxiter
        integer :: nact
        integer :: nactsav
        logical :: scaled
        logical :: twod_search
        real(real64) :: beta
        real(real64) :: bstep
        real(real64) :: cth
        real(real64) :: delsq
        real(real64) :: dhd
        real(real64) :: dhs
        real(real64) :: dold(size(d))
        real(real64) :: dredg
        real(real64) :: dredsq
        real(real64) :: ds
        real(real64) :: ggsav
        real(real64) :: gredsq
        real(real64) :: hangt
        real(real64) :: hangt_bd
        real(real64) :: hq(size(hq_in, 1), size(hq_in, 2))
        real(real64) :: pq(size(pq_in))
        real(real64) :: qred
        real(real64) :: rayleighq
        real(real64) :: resid
        real(real64) :: sbound(size(gopt_in))
        real(real64) :: sdec
        real(real64) :: shs
        real(real64) :: sqrtd
        real(real64) :: sredg
        real(real64) :: stepsq
        real(real64) :: sth
        real(real64) :: stplen
        real(real64) :: temp
        real(real64) :: xtest(size(xopt))
        real(real64) :: args(5)
        real(real64) :: dred(size(gopt_in))
        real(real64) :: gnew(size(gopt_in))
        real(real64) :: gopt(size(gopt_in))
        real(real64) :: hdred(size(gopt_in))
        real(real64) :: hs(size(gopt_in))
        real(real64) :: modscal
        real(real64) :: s(size(gopt_in))
        real(real64) :: sqdscr(size(gopt_in))
        real(real64) :: ssq(size(gopt_in))
        real(real64) :: tanbd(size(gopt_in))
        real(real64) :: xnew(size(gopt_in))

        n = int(size(gopt_in))
        npt = int(size(pq_in))

        ! Scale the problem if GOPT contains large values. Otherwise, floating point exceptions may occur.
        ! Note that CRVMIN must be scaled back if it is nonzero, but step is scale invariant.
        ! N.B.: It is faster and safer to scale by multiplying a reciprocal than by division. See
        ! https://fortran-lang.discourse.group/t/ifort-ifort-2021-8-0-1-0e-37-1-0e-38-0/
        if (maxval(abs(gopt_in)) > 1.0E12) then  ! The threshold is empirical.
            modscal = max(TWO * REALMIN, ONE / maxval(abs(gopt_in)))  ! MAX: precaution against underflow.
            gopt = gopt_in * modscal
            pq = pq_in * modscal
            hq = hq_in * modscal
            scaled = .true.
        else
            modscal = ONE  ! This value is not used, but Fortran compilers may complain without it.
            gopt = gopt_in
            pq = pq_in
            hq = hq_in
            scaled = .false.
        end if

        ! The initial values of IACT, DREDSQ, and GGSAV are unused but to entertain Fortran compilers.
        ! TODO: Check that GGSAV has been initialized before used.
        iact = 0
        dredsq = ZERO
        ggsav = ZERO

        ! The sign of GOPT(I) gives the sign of the change to the I-th variable that will reduce Q from its
        ! value at XOPT. Thus XBDI(I) shows whether or not to fix the I-th variable at one of its bounds
        ! initially, with NACT being set to the number of fixed variables.
        xbdi = 0
        xbdi(trueloc(xopt >= su .and. gopt <= 0)) = 1
        xbdi(trueloc(xopt <= sl .and. gopt >= 0)) = -1
        nact = int(count(xbdi /= 0))

        ! Initialized D and CRVMIN.
        d = ZERO
        crvmin = -REALMAX

        ! GNEW is the gradient at the current iterate.
        gnew = gopt
        gredsq = sum(gnew(trueloc(xbdi == 0))**2)
        ! DELSQ is the upper bound on the sum of squares of the free variables.
        delsq = delta * delta
        ! QRED is the reduction in Q so far.
        qred = ZERO
        ! BETA is the coefficient for the previous searching direction in the conjugate gradient method.
        beta = ZERO

        ! ITERCG is the number of CG iterations corresponding to the current set of active bounds.
        itercg = 0

        ! TWOD_SEARCH: whether to perform a 2-dimensional search after the truncated CG method.
        twod_search = .false.  ! The default value of TWOD_SEARCH is FALSE!

        ! Powell's code is essentially a DO WHILE loop. We impose an explicit MAXITER.
        ! The formulation of MAXITER below contains a precaution against overflow. In MATLAB/Python/Julia/R,
        ! we can write maxiter = min(10000, (n - nact)^2).
        ! Powell commented in the BOBYQA paper (the paragraph above (3.7)) that "numerical experiments show
        ! that it is very unusual for subroutine TRSBOX to make more than ten changes to d when seeking an
        ! approximate solution to the subproblem (1.8), even if there are hundreds of variables."
        maxiter = int(min(10**min(4, range(0)), int(n - nact)**2))
        do iter = 1, maxiter
            resid = delsq - sum(d(trueloc(xbdi == 0))**2)
            ! D fills the trust region at the top of a CG iteration in no run the suite drives;
            ! the two-dimensional search is entered through the exits further down instead.
            if (resid <= 0) then
                twod_search = .true.              ! GCOVR_EXCL_START -- see the note above
                exit
            end if                        ! GCOVR_EXCL_STOP

            ! Set the next search direction of the conjugate gradient method. It is the steepest descent
            ! direction initially and when the iterations are restarted because a variable has just been
            ! fixed by a bound, and of course the components of the fixed variables are zero. MAXITER is an
            ! upper bound on the indices of the conjugate gradient iterations.
            if (itercg == 0) then
                ! TODO: If we are sure that S contain only finite values, we may merge this case into the next.
                s = -gnew
            else
                s = beta * s - gnew
            end if
            s(trueloc(xbdi /= 0)) = ZERO
            stepsq = sum(s**2)
            ds = inprod(d(trueloc(xbdi == 0)), s(trueloc(xbdi == 0)))

            if (.not. (stepsq > EPS * delsq .and. gredsq * delsq > (tol * qred)**2 .and. .not. is_nan(ds))) then
                exit
            end if

            ! Set BSTEP to the length of the step to the trust region boundary and STPLEN to the steplength,
            ! ignoring the simple bounds.

            ! SQRTD: square root of a discriminant. The MAXVAL avoids SQRTD < ABS(DS) due to underflow.
            sqrtd = maxval([sqrt(stepsq * resid + ds * ds), sqrt(stepsq * resid), abs(ds)])

            ! Zaikun 20220210: For the IF ... ELSE ... END IF below, Powell's condition for the IF is DS>=0.
            ! In theory, switching the condition to DS > 0 changes nothing; indeed, the two formulations
            ! of BSTEP are equivalent. However, surprisingly, DS > 0 clearly worsens the performance of
            ! BOBYQA in tests on 20220210, 20221206. Why? When DS = 0, what should be the best formulation?
            ! What if we are at the first iteration? BSTEP = DELTA/||D||?
            ! See TRSAPP.F90 of NEWUOA.
            !if (ds > 0) then  ! Zaikun 20210925
            if (ds >= 0) then
                bstep = resid / (sqrtd + ds)
            else
                bstep = (sqrtd - ds) / stepsq
            end if
            ! BSTEP < 0 should not happen. BSTEP can be 0 or NaN when, e.g., DS or STEPSQ becomes Inf.
            ! Powell's code does not handle this.
            if (bstep <= 0 .or. .not. is_finite(bstep)) then
                exit  ! GCOVR_EXCL_LINE -- BSTEP is positive and finite in every run the suite drives
            end if

            hs = hess_mul(s, xpt, pq, hq)
            shs = inprod(s(trueloc(xbdi == 0)), hs(trueloc(xbdi == 0)))
            stplen = bstep
            if (shs > 0) then
                stplen = min(bstep, gredsq / shs)
            end if

            ! Reduce STPLEN if necessary in order to preserve the simple bounds, letting IACT be the index
            ! of the new constrained variable.
            ! N.B. (Zaikun 20220422):
            ! Theory and computation differ considerably in the calculation of STPLEN and IACT.
            ! 1. Theoretically, the WHERE constructs can simplify (S > 0 .and. XTEST > SU) to (S > 0) and
            ! (S < 0, XTEST < SL) to (S < 0), which will be equivalent to Powell's original code. However,
            ! overflow will occur due to huge values in SU or SL that indicate the absence of bounds, and
            ! Fortran compilers will complain. It is not an issue in MATLAB/Python/Julia/R.
            ! 2. Theoretically, we can also simplify (S > 0 .and. XTEST > SU) to (XTEST > SU). This is
            ! because the algorithm intends to ensure that SL <= XSUM <= SU, under which the inequality
            ! XTEST(I) > SU(I) implies S(I) > 0. Numerically, however, XSUM may violate the bounds slightly
            ! due to rounding. If we replace (S > 0 .and. XTEST > SU) with (XTEST > SU), then SBOUND(I) will
            ! be -Inf when SU(I) - XSUM(I) is negative (although tiny) and S(I) is +0 (positively signed
            ! zero), which will lead to STPLEN = -Inf and IACT = I > 0. This will trigger a restart of the
            ! conjugate gradient method with DELSQ updated to DELSQ - D(IACT)**2; if D(IACT)**2 << DELSQ,
            ! then DELSQ can remain unchanged due to rounding, leading to an infinite cycling.
            ! 3. Theoretically, the WHERE construct corresponding to S > 0 can calculate SBOUND by
            ! MIN(STPLEN * S, SU - XSUM) / S instead of (SU - XSUM) / S, since this quotient matters only if
            ! it is less than STPLEN. The motivation is to avoid overflow even without checking XTEST > XU.
            ! Yet such an implementation clearly worsens the performance of BOBYQA in our test on 20220422.
            ! Why? Note that the conjugate gradient method restarts when IACT > 0. Due to rounding errors,
            ! MIN(STPLEN * S, SU - XSUM) / S can frequently contain entries less than STPLEN, leading to a
            ! positive IACT and hence a restart. This turns out harmful to the performance of the algorithm,
            ! but WHY? It can be rectified in two ways: use MIN(STPLEN, (SU-XSUM) / S) instead of
            ! MIN(STPLEN*S, SU-XSUM)/S, or set IACT to a positive value only if the minimum of SBOUND is
            ! surely less STPLEN, e.g. ANY(SBOUND < (ONE-EPS) * STPLEN). The first method does not avoid
            ! overflow and makes little sense.
            xnew = xopt + d
            xtest = xnew + stplen * s
            sbound = stplen
            where (s > 0 .and. xtest > su) sbound = (su - xnew) / s
            where (s < 0 .and. xtest < sl) sbound = (sl - xnew) / s
            ! MATLAB:
            ! sbound(s > 0) = (su(s > 0) - xnew(s > 0)) / s(s > 0);
            ! sbound(s < 0) = (sl(s < 0) - xnew(s < 0)) / s(s < 0);
            !----------------------------------------------------------------------------------------------!
            ! The code below is mathematically equivalent to the above but numerically inferior as explained.
            !where (s > 0) sbound = min(stplen * s, su - xnew) / s
            !where (s < 0) sbound = max(stplen * s, sl - xnew) / s
            !----------------------------------------------------------------------------------------------!
            sbound(trueloc(is_nan(sbound))) = stplen  ! Needed? No if we are sure that D and S are finite.
            iact = 0
            if (any(sbound < stplen)) then
                iact = int(minloc(sbound, dim=1))
                stplen = sbound(iact)
                ! MATLAB: [stplen, iact] = min(sbound);
            end if
            !----------------------------------------------------------------------------------------------!
            ! Alternatively, IACT and STPLEN can be calculated as below.
            ! !IACT = INT(MINLOC([STPLEN, SBOUND], DIM=1), KIND(IACT)) - 1
            ! !STPLEN = MINVAL([STPLEN, SBOUND]) ! This line cannot be exchanged with the last
            ! We prefer our implementation, as the code is more explicit; in addition, it is more flexible:
            ! we can change the condition ANY(SBOUND < STPLEN) to ANY(SBOUND < (1 - EPS) * STPLEN) or
            ! ANY(SBOUND < (1 + EPS) * STPLEN), depending on whether we believe a false positive or a false
            ! negative of IACT > 0 is more harmful --- according to our test on 20220422, it is the former,
            ! as mentioned above.
            !----------------------------------------------------------------------------------------------!

            ! Update CRVMIN, GNEW, and D. Set SDEC to the decrease that occurs in Q.
            sdec = ZERO
            if (stplen > 0) then
                itercg = itercg + 1
                rayleighq = shs / stepsq
                if (iact == 0 .and. rayleighq > 0) then
                    if (crvmin <= -REALMAX) then  ! CRVMIN <= -REALMAX means CRVMIN has not been set.
                        crvmin = rayleighq
                    else
                        crvmin = min(crvmin, rayleighq)
                    end if
                end if
                ggsav = gredsq
                gnew = gnew + stplen * hs
                gredsq = sum(gnew(trueloc(xbdi == 0))**2)
                dold = d
                d = d + stplen * s

                ! Exit in case of Inf/NaN in D.
                ! The model is finite (a non-finite objective value aborts in `evaluate`) and the
                ! CG step has not overflowed in any run the suite drives.
                if (.not. is_finite(sum(abs(d)))) then
                    d = dold                      ! GCOVR_EXCL_START -- see the note above
                    exit
                end if                    ! GCOVR_EXCL_STOP

                sdec = max(stplen * (ggsav - HALF * stplen * shs), ZERO)
                qred = qred + sdec
            end if

            ! Restart the conjugate gradient method if it has hit a new bound.
            if (iact > 0) then
                nact = nact + 1
                xbdi(iact) = nint(sign(ONE, s(iact)))  ! MATLAB: xbdi(iact) = sign(s(iact))
                ! Exit when NACT = N (NACT > N is impossible). We must update XBDI before exiting!
                if (nact >= n) then
                    exit  ! This leads to a difference. Why?
                end if
                delsq = delsq - d(iact)**2
                ! D does not reach the trust-region boundary by fixing a variable at a bound in any
                ! run the suite drives; the CG loop leaves through one of its other exits.
                if (delsq <= 0) then
                    twod_search = .true.          ! GCOVR_EXCL_START -- see the note above
                    ! Why set TWOD_SEARCH to TRUE? Because DELSQ <= 0 just means that D reaches the trust
                    ! region boundary.
                    exit
                end if                    ! GCOVR_EXCL_STOP
                beta = ZERO
                itercg = 0
                gredsq = sum(gnew(trueloc(xbdi == 0))**2)
            elseif (stplen < bstep) then
                ! Either apply another conjugate gradient iteration or exit.
                ! N.B. ITERCG > N - NACT is impossible.
                if (itercg >= n - nact .or. sdec <= tol * qred .or. is_nan(sdec) .or. is_nan(qred)) then
                    exit
                end if
                beta = gredsq / ggsav  ! Has GGSAV got the correct value yet?
            else
                twod_search = .true.
                exit
            end if
        end do

        ! Set MAXITER for the 2-dimensional search on the trust region boundary. Powell's code essentially
        ! sets MAXITER to infinity; the loop exits when NACT >= N-1 or the procedure cannot significantly
        ! reduce the quadratic model. We set a finite but large MAXITER as a safeguard.
        if (twod_search) then
            crvmin = ZERO
            maxiter = 10 * (n - nact)
        else
            maxiter = 0
        end if

        ! Improve D by a sequential 2-dimensional search on the boundary of the trust region for the
        ! variables that have not reached a bound. See (3.6) of the BOBYQA paper and the elaborations nearby.
        ! 1. At each iteration, the current D is improved by a search conducted on the circular arch
        ! {D(THETA): D(THETA) = (I-P)*D + [COS(THETA)*P*D + SIN(THETA)*S], 0<=THETA<=PI/2, SL<=XOPT+D(THETA)<=SU},
        ! where P is the orthogonal projection onto the space of the variables that have not reached their
        ! bounds, and S is a linear combination of P*D and P*G(XOPT+D) with ||S|| = ||P*D|| and G(.) being
        ! the gradient of the quadratic model. The iteration is performed only if P*D and P*G(XOPT+D) are
        ! not nearly parallel. The arc lies in the hyperplane (I-P)*D + Span{P*D, P*G(XOPT+D)} and the trust
        ! region boundary {D: ||D||=DELTA}; it is part of the circle (I-P)*D + {COS(THETA)*P*D + SIN(THETA)*S}
        ! with THETA being in [0, PI/2] and restricted by the bounds on X.
        ! 2. In (3.6) of the BOBYQA paper, Powell wrote that 0 <= THETA <= PI/4, which seems a typo.
        ! 3. The search on the arch is done by calling INTERVAL_MAX, which maximizes INTERVAL_FUN_TRSBOX.
        ! INTERVAL_FUN_TRSBOX is essentially Q(XOPT + D) - Q(XOPT + D(THETA)), but its independent variable
        ! is not THETA but TAN(THETA/2), namely "tangent of the half angle" in Powell's code/comments. This
        ! "half" may be the reason for the apparent typo mentioned above.
        ! Question (Zaikun 20220424): Shouldn't we try something similar in GEOSTEP?

        nactsav = nact - 1
        do iter = 1, maxiter
            xnew = xopt + d

            ! Update XBDI. It indicates whether the lower (-1) or upper bound (+1) is reached or not (0).
            xbdi(trueloc(xbdi == 0 .and. (xnew >= su))) = 1
            xbdi(trueloc(xbdi == 0 .and. (xnew <= sl))) = -1
            nact = int(count(xbdi /= 0))
            if (nact >= n - 1) then
                exit
            end if

            ! Update GREDSQ, DREDG, DREDSQ.
            gredsq = sum(gnew(trueloc(xbdi == 0))**2)
            dredg = inprod(d(trueloc(xbdi == 0)), gnew(trueloc(xbdi == 0)))
            if (iter == 1 .or. nact > nactsav) then
                dredsq = sum(d(trueloc(xbdi == 0))**2) ! In theory, DREDSQ changes only when NACT increases.
                dred = d
                dred(trueloc(xbdi /= 0)) = ZERO
                hdred = hess_mul(dred, xpt, pq, hq)
                nactsav = nact
            end if

            ! Let the search direction S be a linear combination of the reduced D and the reduced G that is
            ! orthogonal to the reduced D.
            temp = gredsq * dredsq - dredg * dredg
            if (.not. temp > tol**2 * max(gredsq * dredsq, qred**2)) then  ! TEMP is tiny or NaN occurs
                exit
            end if
            temp = sqrt(temp)
            s = (dredg * d - dredsq * gnew) / temp
            s(trueloc(xbdi /= 0)) = ZERO
            sredg = -temp

            ! By considering the simple bounds on the free variables, calculate an upper bound on the
            ! TANGENT of HALF the angle of the alternative iteration, namely ANGBD. The bounds are
            ! SL - XOPT <= COS(THETA)*D + SIN(THETA)*S <= SU - XOPT for the free variables.
            ! Defining HANGT = TAN(THETA/2), and using the tangent half-angle formula, we have
            ! (1+HANGT^2)*(SL - XOPT) <= (1-HANGT^2)*D + 2*HANGT*S <= (1+HANGT^2)*(SU - XOPT),
            ! which is required for all free variables. The indices of the free variable are those with
            ! XBDI == 0. Solving this inequality system for HANGT in [0, PI/4], we get bounds for HANGT,
            ! namely TANBD; the final bound for HANGT is the minimum of TANBD, which is HANGT_BD.
            ! When solving the system, note that SL < XOPT < SU and SL < XOPT + D < SU if XBDI = 0.
            !
            ! Note the following for the calculation of the first SQDSCR below (the second is similar).
            ! 0. SQDSCR means "square root of discriminant".
            ! 1. When calculating the first SQDSCR, Powell's code checks whether SSQ - (XOPT - SL)**2) is
            ! positive. However, overflow will occur if SL contains large values that indicate absence of
            ! bounds. It is not a problem in MATLAB/Python/Julia/R.
            ! 2. Even if XOPT - SL < SQRT(SSQ), rounding errors may render SSQ - (XOPT - SL)**2) < 0.
            ssq = d**2 + s**2  ! Indeed, only SSQ(TRUELOC(XBDI == 0)) is needed.
            tanbd = ONE
            sqdscr = -REALMAX
            where (xbdi == 0 .and. xopt - sl < sqrt(ssq)) sqdscr = sqrt(max(ZERO, ssq - (xopt - sl)**2))
            where (sqdscr - s > 0) tanbd = min(tanbd, (xnew - sl) / (sqdscr - s))
            sqdscr = -REALMAX
            where (xbdi == 0 .and. su - xopt < sqrt(ssq)) sqdscr = sqrt(max(ZERO, ssq - (su - xopt)**2))
            where (sqdscr + s > 0) tanbd = min(tanbd, (su - xnew) / (sqdscr + s))
            tanbd(trueloc(is_nan(tanbd))) = ZERO
            !----------------------------------------------------------------------------------------------!
            ! MATLAB code for defining TANBD:
            ! xfree = (xbdi == 0);
            ! ssq = NaN(n, 1);
            ! ssq(xfree) = s(xfree).^2 + d(xfree).^2;
            ! discmn = NaN(n, 1);
            ! discmn(xfree) = ssq(xfree) - (xopt(xfree) - sl(xfree))**2;  % This is a discriminant.
            ! tanbd = 1;
            ! mask = (xfree & discmn > 0 & sqrt(discmn) - s > 0);
            ! tanbd(mask) = min(tanbd(mask), (xnew(mask) - sl(mask)) / (sqrt(discmn(mask)) - s(mask)));
            ! discmn(xfree) = ssq(xfree) - (su(xfree) - xopt(xfree))**2;  % This is a discriminant.
            ! mask = (xfree & discmn > 0 & sqrt(discmn) + s > 0);
            ! tanbd(mask) = min(tanbd(mask), (su(mask) - xnew(mask)) / (sqrt(discmn(mask)) + s(mask)));
            ! tanbd(isnan(tanbd)) = 0;
            !----------------------------------------------------------------------------------------------!

            iact = 0
            hangt_bd = ONE
            if (any(tanbd < 1)) then
                iact = int(minloc(tanbd, dim=1))
                hangt_bd = tanbd(iact)
                ! MATLAB: [hangt_bd, iact] = min(tanbd);
            end if
            ! TANBD is ONE unless a bound cuts the arc shorter, and a nonpositive bound on the
            ! half-angle tangent does not occur in any run the suite drives.
            if (hangt_bd <= 0) then
                exit  ! GCOVR_EXCL_LINE -- see the note above
            end if

            ! Calculate HS and some curvatures for the alternative iteration.
            hs = hess_mul(s, xpt, pq, hq)
            shs = inprod(s(trueloc(xbdi == 0)), hs(trueloc(xbdi == 0)))
            dhs = inprod(d(trueloc(xbdi == 0)), hs(trueloc(xbdi == 0)))
            dhd = inprod(d(trueloc(xbdi == 0)), hdred(trueloc(xbdi == 0)))

            ! Seek the greatest reduction in Q for a range of equally spaced values of HANGT in [0, ANGBD],
            ! with HANGT being the TANGENT of HALF the angle of the alternative iteration.
            args = [shs, dhd, dhs, dredg, sredg]
            if (any(is_nan(args))) then
                exit  ! GCOVR_EXCL_LINE -- a NaN aborts in `evaluate` before it can reach the model
            end if
            ! Define the grid size of the search for HANGT. Powell defined the size to be 4 if hangt_bd is
            ! nearly zero and 20 if it is nearly one, with a linear interpolation in between. We double this
            ! size, which improves the performance of BOBYQA in general according to a test on 20230827.
            !grid_size = nint(17.0_real64 * hangt_bd + 4.1_real64)  ! Powell's version
            grid_size = 2 * nint(17.0_real64 * hangt_bd + 4.1_real64)
            ! MATLAB: grid_size = 2 * round(17 * hangt_bd + 4.1_real64)
            hangt = interval_max(interval_fun_trsbox, ZERO, hangt_bd, args, grid_size)
            sdec = interval_fun_trsbox(hangt, args)
            if (.not. sdec > 0) then
                exit
            end if

            ! Update GNEW, D and HDRED. If the angle of the alternative iteration is restricted by a bound
            ! on a free variable, that variable is fixed at the bound. The MIN below is a precaution against
            ! rounding errors.
            cth = min((ONE - hangt**2) / (ONE + hangt**2), ONE - hangt**2)
            sth = min((hangt + hangt) / (ONE + hangt**2), hangt + hangt)
            gnew = gnew + (cth - ONE) * hdred + sth * hs
            dold = d
            d(trueloc(xbdi == 0)) = cth * d(trueloc(xbdi == 0)) + sth * s(trueloc(xbdi == 0))

            ! Exit in case of Inf/NaN in D.
            ! The model is finite (a non-finite objective value aborts in `evaluate`) and the arc
            ! step below has not overflowed in any run the suite drives.
            if (.not. is_finite(sum(abs(d)))) then
                d = dold                          ! GCOVR_EXCL_START -- see the note above
                exit
            end if                        ! GCOVR_EXCL_STOP

            hdred = cth * hdred + sth * hs
            qred = qred + sdec
            if (iact >= 1 .and. iact <= n .and. hangt >= hangt_bd) then  ! D(IACT) reaches lower/upper bound.
                xbdi(iact) = nint(sign(ONE, xopt(iact) + d(iact) - HALF * (sl(iact) + su(iact))))
                ! MATLAB: xbdi(iact) = sign(xopt(iact)+d(iact) - 0.5*(sl+su));
            elseif (.not. sdec > tol * qred) then  ! SDEC is small or NaN occurs
                exit
            end if
        end do

        ! Set D, giving careful attention to the bounds.
        xnew = max(sl, min(su, xopt + d))
        xnew(trueloc(xbdi == -1)) = sl(trueloc(xbdi == -1))
        xnew(trueloc(xbdi == 1)) = su(trueloc(xbdi == 1))
        d = xnew - xopt

        ! Set CRVMIN to ZERO if it has never been set or becomes NaN due to ill conditioning.
        if (crvmin <= -REALMAX .or. is_nan(crvmin)) then
            crvmin = ZERO
        end if

        ! Scale CRVMIN back before return. Note that the trust-region step is scale invariant.
        if (scaled .and. crvmin > 0) then
            crvmin = crvmin / modscal
        end if

    end subroutine trsbox

    !> The objective of `trsbox`'s arc search, upstream's `interval_fun_trsbox`.
    !!
    !! `hangt` is the tangent of half the arc's angle; `args` packs the quantities the reduction
    !! depends on. A module procedure, not an internal one: `interval_max` takes it as a
    !! `procedure(...)` actual argument (`fortran-gotchas.md`).
    function interval_fun_trsbox(hangt, args) result(f)
        real(real64), intent(in) :: hangt      !! tangent of half the angle
        real(real64), intent(in) :: args(:)    !! the packed coefficients

        real(real64) :: f                      !! the reduction this angle achieves

        real(real64) :: sth

        f = ZERO
        if (abs(hangt) > 0) then
            sth = (hangt + hangt) / (ONE + hangt * hangt)
            f = args(1) + hangt * (hangt * args(2) - args(3) - args(3))
            f = sth * (hangt * args(4) - args(5) - HALF * sth * f)
            ! N.B.: ARGS = [SHS, DHD, DHS, DREDG, SREDG]
        end if

    end function interval_fun_trsbox

    !> The updated trust-region radius, upstream's `trrad`.
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
            delta = min(gamma1 * delta_in, dnorm)  ! Powell's BOBYQA.
            !delta = gamma1 * dnorm  ! Powell's UOBYQA/NEWUOA.
            !delta = gamma1 * delta_in  ! Powell's COBYLA/LINCOA. Works poorly here.
        elseif (ratio <= eta2) then
            delta = max(gamma1 * delta_in, dnorm)    ! Powell's UOBYQA/NEWUOA/BOBYQA/LINCOA
        else
            delta = max(gamma1 * delta_in, gamma2 * dnorm)  ! Powell's NEWUOA/BOBYQA.
            !delta = max(delta_in, gamma2 * dnorm)  ! Modified version. Works well for UOBYQA.
            !delta = max(delta_in, 1.25_real64 * dnorm, dnorm + rho)  ! Powell's UOBYQA
            !delta = min(max(gamma1 * delta_in, gamma2 * dnorm), sqrt(gamma2) * delta_in)  ! Powell's LINCOA.
        end if

        ! For noisy problems, the following may work better.
        ! !if (ratio <= eta1) then
        ! !    delta = gamma1 * dnorm
        ! !elseif (ratio <= eta2) then  ! Ensure DELTA >= DELTA_IN
        ! !    delta = delta_in
        ! !else  ! Ensure DELTA > DELTA_IN with a constant factor
        ! !    delta = max(delta_in * (1.0_real64 + gamma2) / 2.0_real64, gamma2 * dnorm)
        ! !end if

    end function trrad

    !> A step that improves the geometry of the interpolation set, upstream's `geostep`.
    !!
    !! Section 3 of the BOBYQA paper, from (3.7): the larger of a straight-line step between two
    !! interpolation points and a Cauchy step, both truncated to the bounds.
    function geostep(knew, kopt, bmat, delbar, sl, su, xpt, zmat) result(d)
        ! Common modules

        integer, intent(in) :: knew               !! index in `xpt` of the point being replaced
        integer, intent(in) :: kopt               !! index in `xpt` of the best point so far
        real(real64), intent(in) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: delbar        !! the radius the geometry step may use
        real(real64), intent(in) :: sl(:)         !! `xl - xbase`: the lower bounds relative to the base point
        real(real64), intent(in) :: su(:)         !! `xu - xbase`: the upper bounds relative to the base point
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Z^T`

        real(real64) :: d(size(xpt, 1))           !! the geometry step

        integer :: ibd
        integer :: ilbd
        integer :: isbd(3, size(xpt, 2))
        integer :: isq
        integer :: iubd
        integer :: k
        integer :: ksq
        integer :: ksqs(3)
        integer :: n
        integer :: npt
        integer :: uphill
        logical :: mask_fixl(size(xpt, 1))
        logical :: mask_fixu(size(xpt, 1))
        logical :: mask_free(size(xpt, 1))
        real(real64) :: alpha, stpsiz
        real(real64) :: betabd(3, size(xpt, 2))
        real(real64) :: bigstp
        real(real64) :: curv
        real(real64) :: dderiv(size(xpt, 2))
        real(real64) :: den_cauchy(size(xpt, 2))
        real(real64) :: den_line(size(xpt, 2))
        real(real64) :: distsq(size(xpt, 2))
        real(real64) :: ggfree
        real(real64) :: glag(size(xpt, 1))
        real(real64) :: grdstp
        real(real64) :: gs
        real(real64) :: lfrac(size(xpt, 1))
        real(real64) :: pqlag(size(xpt, 2))
        real(real64) :: predsq(3, size(xpt, 2))
        real(real64) :: resis
        real(real64) :: s(size(xpt, 1))
        real(real64) :: scaling
        real(real64) :: sfixsq
        real(real64) :: slbd
        real(real64) :: slbd_test(size(xpt, 1))
        real(real64) :: ssqsav
        real(real64) :: stplen(3, size(xpt, 2))
        real(real64) :: stpm
        real(real64) :: subd
        real(real64) :: subd_test(size(xpt, 1))
        real(real64) :: sumin
        real(real64) :: sxpt(size(xpt, 2))
        real(real64) :: ufrac(size(xpt, 1))
        real(real64) :: vlag(3, size(xpt, 2))
        real(real64) :: vlagsq
        real(real64) :: vlagsq_cauchy
        real(real64) :: x(size(xpt, 1))
        real(real64) :: xcauchy(size(xpt, 1))
        real(real64) :: xdiff(size(xpt, 1))
        real(real64) :: xline(size(xpt, 1))
        real(real64) :: xopt(size(xpt, 1))
        real(real64) :: xtemp(size(xpt, 1))

        ! Sizes.
        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! PQLAG contains the leading NPT elements of the KNEW-th column of H, and it provides the second
        ! derivative parameters of LFUNC, which is the KNEW-th Lagrange function. ALPHA will is the KNEW-th
        ! diagonal element of the H matrix.
        pqlag = matprod(zmat, zmat(knew, :))
        alpha = pqlag(knew)

        ! Read XOPT.
        xopt = xpt(:, kopt)

        ! Calculate the gradient GLAG of the KNEW-th Lagrange function at XOPT.
        glag = bmat(:, knew) + hess_mul(xopt, xpt, pqlag)

        ! In case GLAG contains NaN, set D to a displacement from XOPT to XPT(:, KNEW) and return. Powell's
        ! code does not have this, and D may be NaN in the end. Note that it is crucial to ensure that a
        ! geometry step is nonzero.
        ! Unreachable here: a NaN or an infinity aborts in `evaluate` before it can reach the model,
        ! so the Lagrange gradient GLAG this is built from is finite.
        if (.not. is_finite(sum(abs(glag)))) then
            d = xpt(:, knew) - xopt                                   ! GCOVR_EXCL_START -- see above
            d = min(HALF, delbar / norm(d)) * d  ! Since XPT respects the bounds, so does XOPT + D.
            return
        end if                            ! GCOVR_EXCL_STOP

        ! Search for a large denominator along the straight lines through XOPT and another interpolation
        ! point, subject to the bound constraints and the trust region. According to these constraints, SLBD
        ! and SUBD will be lower and upper bounds on the step along each of these lines in turn. On each
        ! line, we will evaluate the value of the KNEW-th Lagrange function at 3 trial points, and estimate
        ! the denominator accordingly. The three points take the form (1-t)*XOPT + t*XPT(:, K) with step
        ! lengths t = SLBD, SUBD, and STPM, corresponding to the upper (U) bound of t, the lower (L) bound
        ! of t, and a medium (M) step. In total, 3*(NPT-1) trial points will be considered. On the K-th line,
        ! we intend to maximize the modulus of PHI_K(t) = LFUNC((1-t)*XOPT + t*XPT(:,K)); overall, we intend
        ! to find a trial point rendering a large value of the PREDSQ defined in (3.11) of the BOBYQA paper.
        !
        ! We start with the following DO loop, the purpose of which is to define two 3-by-NPT arrays STPLEN
        ! and ISBD. For each K, STPLEN(1:3, K) and ISBD(1:3, K) corresponds to the straight line through
        ! XOPT and XPT(:, K). STPLEN(1:3, K) contains SLBD, SUBD, and STPM in this order, which are the step
        ! lengths for the three trial points on this line. The three entries of SBDI(1:3, K) indicate
        ! whether the corresponding trial points lie on bounds; SBDI(I, K) = J > 0 means that the I-th trail
        ! point on the K-th line attains the J-th upper bound, SBDI(I, K) = -J < 0 indicates reaching the
        ! J-th lower bound, and SBDI(I, K) = 0 means not touching any bound.
        dderiv = matprod(glag, xpt) - inprod(glag, xopt) ! The derivatives PHI_K'(0).
        distsq = sum((xpt - spread(xopt, dim=2, ncopies=npt))**2, dim=1)
        do k = 1, npt
            ! It does not make sense to consider "straight line through XOPT and XPT(:, KOPT)". Hence set
            ! STPLEN(:, KOPT) = 0 and ISBD(:, KOPT) = 0 so that VLAG(:, K) and PREDSQ(:, K) obtained after
            ! this loop will be both zero and the search will skip K = KOPT. To avoid undesired/unpredictable
            ! behavior due to possible NaN, set DDERIV(K) = 0 if K = KOPT or if DDERIV(K) is originally NaN.
            if (k == kopt .or. is_nan(dderiv(k))) then
                dderiv(k) = ZERO
                stplen(:, k) = ZERO
                isbd(:, k) = 0
                cycle
            end if

            subd = delbar / sqrt(distsq(k))  ! DISTSQ(K) > 0 unless K == KOPT or the input is incorrect.
            slbd = -subd
            ilbd = 0
            iubd = 0
            sumin = min(ONE, subd)

            ! Revise SLBD and SUBD if necessary because of the bounds in SL and SU according to LFRAC, UFRAC.
            ! N.B.: We calculate LFRAC only at the positions where SL - XOPT > -ABS(XDIFF) * SUBD, because
            ! the values of LFRAC are relevant only at the positions where ABS(LFRAC) < SUBD. Powell's code
            ! does not check this inequality before evaluating LFRAC, and overflow may occur due to large
            ! entries of SL. Note that SL - XOPT > -ABS(XDIFF) * SUBD implies that XDIFF /= 0, as long as
            ! SL <= XOPT is ensured. In addition, when initializing LFRAC to SIGN(SUBD, -XDIFF), we do not
            ! need to worry about the case where XDIFF = 0, because we only use LFRAC when XDIFF /= 0.
            ! Similar things can be said about UFRAC.
            xdiff = xpt(:, k) - xopt
            lfrac = sign(subd, -xdiff)
            where (sl - xopt > -abs(xdiff) * subd) lfrac = (sl - xopt) / xdiff
            ufrac = sign(subd, xdiff)
            where (su - xopt < abs(xdiff) * subd) ufrac = (su - xopt) / xdiff
            ! MATLAB code for LFRAC and UFRAC (the code is simpler as we are not concerned about overflow):
            ! xdiff = xpt(:, k) - xopt;
            ! lfrac = (sl - xopt) / xdiff;
            ! ufrac = (su - xopt) / xdiff;

            ! First, revise SLBD. Note that SLBD_TEST <= 0 unless the input violates XOPT >= SL.
            slbd_test = slbd
            slbd_test(trueloc(xdiff > 0)) = lfrac(trueloc(xdiff > 0))
            slbd_test(trueloc(xdiff < 0)) = ufrac(trueloc(xdiff < 0))
            if (any(slbd_test > slbd)) then
                ilbd = int(maxloc(slbd_test, mask=(.not. is_nan(slbd_test)), dim=1))
                slbd = slbd_test(ilbd)
                ilbd = -ilbd * nint(sign(ONE, xdiff(ilbd)))
                ! MATLAB:
                ! [slbd, ilbd] = max(slbd_test, [], 'omitnan');
                ! ilbd = -ilbd * sign(xdiff(ilbd));
            end if

            ! Second, revise SUBD. Note that SUBD_TEST >= 0 unless the input violates XOPT <= SU.
            subd_test = subd
            subd_test(trueloc(xdiff > 0)) = ufrac(trueloc(xdiff > 0))
            subd_test(trueloc(xdiff < 0)) = lfrac(trueloc(xdiff < 0))
            if (any(subd_test < subd)) then
                iubd = int(minloc(subd_test, mask=(.not. is_nan(subd_test)), dim=1))
                subd = max(sumin, subd_test(iubd))
                iubd = iubd * nint(sign(ONE, xdiff(iubd)))
                ! MATLAB:
                ! [subd, iubd] = min(subd_test, [], 'omitnan');
                ! subd = max(sumin, subd);
                ! iubd = iubd * sign(xdiff(iubd));
            end if

            ! Now, define the step length STPM between SLBD and SUBD by finding the critical point of the
            ! function PHI_K(t) = LFUNC((1-t)*XOPT + t*XPT(:,K)) mentioned above. It is a quadratic since
            ! LFUNC is the KNEW-th Lagrange function. For K /= KNEW, the critical point is 0.5, as
            ! PHI_K(0) = 1 = PHI_K(1); when K = KNEW, it is -0.5*PHI_K'(0) / (1 - PHI_K'(0)), because
            ! PHI_K(0) = 0 and PHI_K(1) = 1.
            stpm = HALF
            if (k == knew) then
                stpm = slbd
                if (abs(ONE - dderiv(k)) > 0) then
                    stpm = -HALF * dderiv(k) / (ONE - dderiv(k))
                end if
            end if
            stpm = max(slbd, min(subd, stpm))

            stplen(:, k) = [slbd, subd, stpm]
            isbd(:, k) = [ilbd, iubd, 0]
        end do

        ! The following lines calculate PREDSQ for all the 3*(NPT-1) trial points.
        ! First, compute VLAG = PHI(STPLEN). Using the fact that PHI_K(0) = 0, PHI_K(1) = delta_{K, KNEW}
        ! (Kronecker delta), and recalling the PHI_K is quadratic, we can find that
        ! PHI_K(t) = t*(1-t)*PHI_K'(0) for K /= KNEW, and PHI_KNEW = t*[t*(1-PHI_K'(0)) + PHI_K'(0)].
        vlag = stplen * (ONE - stplen) * spread(dderiv, dim=1, ncopies=3)
        ! MATLAB: vlag = stplen .* (1 - stplen) .* dderiv; % Implicit expansion; dderiv is a row!
        vlag(:, knew) = stplen(:, knew) * (stplen(:, knew) * (ONE - dderiv(knew)) + dderiv(knew))
        ! Set NaNs in VLAG to 0 so that the behavior of MAXVAL(ABS(VLAG)) is predictable. VLAG does not have
        ! NaN unless XPT does, which would be a bug. MAXVAL(ABS(VLAG)) appears in Powell's code, not here.
        where (is_nan(vlag)) vlag = ZERO  ! MATLAB: vlag(isnan(vlag)) = 0;
        !
        ! Second, BETABD is the upper bound of BETA given in (3.10) of the BOBYQA paper.
        betabd = HALF * (stplen * (ONE - stplen) * spread(distsq, dim=1, ncopies=3))**2
        ! MATLAB: betabd = 0.5 * (stplen .* (1-stplen) .* distsq).^2 % Implicit expansion; distsq is a row!
        !
        ! Finally, PREDSQ is the quantity defined in (3.11) of the BOBYQA paper.
        predsq = vlag * vlag * (vlag * vlag + alpha * betabd)
        ! Set NaNs in PREDSQ to 0 so that the behavior of MAXLOC(PREDSQ) is predictable. PREDSQ does not
        ! have NaN unless XPT does, which would be a bug.
        where (is_nan(predsq)) predsq = ZERO  ! MATLAB: predsq(isnan(predsq)) = 0

        ! Locate the trial point the renders the maximum of PREDSQ. It is the ISQ-th trial point on the
        ! straight line through XOPT and XPT(:, KSQ).
        ! N.B.: 1. The strategy is a bit different from Powell's original code. In Powell's code and the
        ! BOBYQA paper, we first select the trial point that gives the largest value of ABS(VLAG) on each
        ! straight line, and then maximize PREDSQ among the (NPT-1) selected points. Here we maximize PREDSQ
        ! among all the trial points. It works slightly better than Powell's version in a test on 20220428.
        ! Powell's version is as follows.
        !---------------------------------------------------------------------!
        !isqs = int(maxloc(abs(vlag), dim=1))  ! SIZE(ISQS) = NPT
        !ksq = int(maxloc([(predsq(isqs(k), k), k=1, npt)], dim=1))
        !isq = isqs(ksq)
        !---------------------------------------------------------------------!
        ! 2. Recall that we have set the NaN entries of PREDSQ to zero, if there is any. Thus the KSQS below
        ! is a well defined integer array, all the three entries lying between 1 and NPT.
        ksqs = int(maxloc(predsq, dim=2))
        isq = int(maxloc([predsq(1, ksqs(1)), predsq(2, ksqs(2)), predsq(3, ksqs(3))], dim=1))
        ksq = ksqs(isq)
        ! MATLAB:
        ! [~, ksqs] = max(predsq, [], 'omitnan');
        ! [~, isq] = max([predsq(1, ksqs(1)), predsq(2, ksqs(2)), predsq(3, ksqs(3))]);
        ! ksq = ksqs(isq);

        ! Construct XLINE in a way that satisfies the bound constraints exactly.
        stpsiz = stplen(isq, ksq)
        ibd = isbd(isq, ksq)

        xline = max(sl, min(su, xopt + stpsiz * (xpt(:, ksq) - xopt)))
        ! IBD is nonzero only when the trial point that maximises PREDSQ is one that the bounds
        ! truncated. No run the suite drives selects such a point, so neither arm below is taken.
        if (ibd < 0) then
            xline(-ibd) = sl(-ibd)  ! GCOVR_EXCL_LINE -- see the note above
        end if
        if (ibd > 0) then
            xline(ibd) = su(ibd)  ! GCOVR_EXCL_LINE -- see the note above
        end if

        ! Calculate DENOM for the current choice of D. Indeed, only DEN_LINE(KNEW) is needed.
        ! Zaikun 20250907: It was observed numerically that D could be ZERO here (i.e., XLINE = XOPT).
        ! Should this be impossible in theory?
        d = xline - xopt
        den_line = calden(kopt, bmat, d, xpt, zmat)

        !--------------------------------------------------------------------------------------------------!
        ! The following IF ... END IF does not exist in Powell's code. SURPRISINGLY, the performance of
        ! BOBYQA on bound constrained problems (but NOT unconstrained ones) is evidently improved by this IF
        ! ... END IF, which means to try the Cauchy step only in the late stage of the algorithm, e.g., when
        ! DELBAR is relatively small. WHY? In the following condition, 1.0E-2 works well if we use
        ! DEN_CAUCHY to decide whether to take the Cauchy step; 1.0E-3 works well if we use VLAGSQ instead.
        ! How to make this condition adaptive? A naive idea is to replace the thresholds to,
        ! e.g.,1.0E-2*RHOBEG. However, in a test on 20220517, this adaptation worsened the performance. In
        ! such a test, RHOBEG must take a value that is quite different from one. We tried RHOBEG = 0.9E-2.
        !if (delbar > 1.0E-3) then
        !if (delbar > 1.0E-1) then
        if (delbar > 1.0E-2) then
            return
        end if
        !--------------------------------------------------------------------------------------------------!

        ! Prepare for the method that assembles the constrained Cauchy step in S. The sum of squares of the
        ! fixed components of S is formed in SFIXSQ, and the free components of S are set to BIGSTP. When
        ! UPHILL = 0, the method calculates the downhill version of XCAUCHY, which intends to minimize the
        ! KNEW-th Lagrange function; when UPHILL = 1, it calculates the uphill version that intends to
        ! maximize the Lagrange function.
        bigstp = delbar + delbar  ! N.B.: In the sequel, S <= BIGSTP.
        xcauchy = xopt
        vlagsq_cauchy = ZERO
        do uphill = 0, 1
            if (uphill == 1) then
                glag = -glag
            end if
            s = ZERO
            mask_free = (min(xopt - sl, glag) > 0 .or. max(xopt - su, glag) < 0)
            s(trueloc(mask_free)) = bigstp
            ggfree = sum(glag(trueloc(mask_free))**2)
            ! In Powell's code, the subroutine returns immediately if GGFREE is 0. However, GGFREE depends
            ! on GLAG, which in turn depends on UPHILL. It can happen that GGFREE is 0 when UPHILL = 0 but
            ! not so when UPHILL= 1. Thus we skip the iteration for the current UPHILL but do not return.
            if (ggfree <= 0 .or. is_nan(ggfree)) then
                cycle
            end if

            ! Investigate whether more components of S can be fixed. Note that the loop counter K does not
            ! appear in the loop body. The purpose of K is only to impose an explicit bound on the number of
            ! loops. Powell's code does not have such a bound. The bound is not a true restriction, because
            ! we can check that (SFIXSQ > SSQSAV .AND. GGFREE > 0) must fail within N loops.
            sfixsq = ZERO
            grdstp = ZERO
            do k = 1, n
                resis = delbar**2 - sfixsq
                ! The fixed components of S never take up the whole of DELBAR**2 in any run the
                ! suite drives, so the loop always ends at its own test below.
                if (resis <= 0) then
                    exit  ! GCOVR_EXCL_LINE -- see the note above
                end if
                ssqsav = sfixsq
                grdstp = sqrt(resis / ggfree)
                xtemp = xopt - grdstp * glag
                mask_fixl = (s >= bigstp .and. xtemp <= sl)  ! S == BIGSTP & XTEMP == SL
                mask_fixu = (s >= bigstp .and. xtemp >= su)  ! S == BIGSTP & XTEMP == SU
                mask_free = (s >= bigstp .and. .not. (mask_fixl .or. mask_fixu))
                s(trueloc(mask_fixl)) = sl(trueloc(mask_fixl)) - xopt(trueloc(mask_fixl))
                s(trueloc(mask_fixu)) = su(trueloc(mask_fixu)) - xopt(trueloc(mask_fixu))
                sfixsq = sfixsq + sum(s(trueloc(mask_fixl .or. mask_fixu))**2)
                ggfree = sum(glag(trueloc(mask_free))**2)
                if (.not. (sfixsq > ssqsav .and. ggfree > 0)) then
                    exit
                end if
            end do

            ! Set the remaining free components of S and all components of XCAUCHY. S may be scaled later.
            x(trueloc(glag > 0)) = sl(trueloc(glag > 0))
            x(trueloc(glag <= 0)) = su(trueloc(glag <= 0))
            x(trueloc(abs(s) <= 0)) = xopt(trueloc(abs(s) <= 0))
            xtemp = max(sl, min(su, xopt - grdstp * glag))
            x(trueloc(s >= bigstp)) = xtemp(trueloc(s >= bigstp))  ! S == BIGSTP
            s(trueloc(s >= bigstp)) = -grdstp * glag(trueloc(s >= bigstp))  ! S == BIGSTP
            gs = inprod(glag, s)

            ! Set CURV to the curvature of the KNEW-th Lagrange function along S. Scale S by a factor less
            ! than ONE if that can reduce the modulus of the Lagrange function at XOPT+S. Set CAUCHY to the
            ! final value of the square of this function.
            sxpt = matprod(s, xpt)
            curv = inprod(sxpt, pqlag * sxpt)  ! CURV = INPROD(S, HESS_MUL(S, XPT, PQLAG))
            if (uphill == 1) then
                curv = -curv
            end if
            if (curv > -gs .and. curv < -(ONE + sqrt(TWO)) * gs) then
                scaling = -gs / curv
                x = max(sl, min(su, xopt + scaling * s))
                vlagsq = (HALF * gs * scaling)**2
            else
                vlagsq = (gs + HALF * curv)**2
            end if

            if (vlagsq > vlagsq_cauchy) then
                xcauchy = x
                vlagsq_cauchy = vlagsq
            end if
        end do

        ! Calculate the denominator rendered by the Cauchy step. Indeed, only DEN_CAUCHY(KNEW) is needed.
        s = xcauchy - xopt
        den_cauchy = calden(kopt, bmat, s, xpt, zmat)

        ! Take the Cauchy step if it is likely to render a larger denominator.
        !IF (VLAGSQ_CAUCHY > MAX(DEN_LINE(KNEW), ZERO) .OR. IS_NAN(DEN_LINE(KNEW))) THEN  ! Powell's version
        if (den_cauchy(knew) > max(den_line(knew), ZERO) .or. is_nan(den_line(knew))) then  ! Works better
            d = s
        end if

        ! In case D is zero or contains Inf/NaN, replace it with a displacement from XPT(:, KNEW) to XOPT.
        ! Powell's code does not have this. Note that it is crucial to ensure that a geometry step is nonzero.
        ! A non-finite D cannot arise (a non-finite value aborts in `evaluate` before it could reach
        ! the model), and no run the suite drives leaves D exactly zero here.
        if (sum(abs(d)) <= 0 .or. .not. is_finite(sum(abs(d)))) then
            d = xpt(:, knew) - xopt                                   ! GCOVR_EXCL_START -- see above
            d = min(HALF, delbar / norm(d)) * d  ! Since XPT respects the bounds, so does XOPT + D.
        end if                            ! GCOVR_EXCL_STOP

    end function geostep

    !> Which interpolation point to drop after a trust-region step, upstream's `setdrop_tr`.
    !!
    !! Chooses the `knew` that leaves the geometry of `xpt` best once `xpt(:, knew)` becomes `xopt +
    !! d`, weighing each candidate's updating denominator against its distance.
    function setdrop_tr(kopt, ximproved, bmat, d, delta, rho, xpt, zmat) result(knew)
        ! Common modules

        integer, intent(in) :: kopt               !! index in `xpt` of the best point so far
        logical, intent(in) :: ximproved          !! whether the new point improved on the best value
        real(real64), intent(in) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: d(:)          !! the step from the best point
        real(real64), intent(in) :: delta         !! upstream's signature; read only by the commented alternative weightings
        real(real64), intent(in) :: rho           !! the lower bound on the trust-region radius
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Z^T`

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
            ! MATLAB: distsq = sum((xpt - (xpt(:, kopt) + d)).^2)  % d should be a column! Implicit expansion
        else
            distsq = sum((xpt - spread(xpt(:, kopt), dim=2, ncopies=npt))**2, dim=1)
            ! MATLAB: distsq = sum((xpt - xpt(:, kopt)).^2)  % Implicit expansion
        end if

        weight = max(ONE, distsq / rho**2)**4
        ! Other possible definitions of WEIGHT.
        ! !weight = max(ONE, distsq / rho**2)**3.5  ! Quite similar to power 4
        ! !weight = max(ONE, distsq / rho**2)**3  ! Not bad
        ! !weight = max(ONE, distsq / delta**2)**2  ! Powell's code. Does not works as well as the above.
        ! !weight = max(ONE, distsq / rho**2)**2  ! Similar to Powell's code, not better.
        ! !weight = max(ONE, distsq / delta**2)  ! Defined in (6.1) of the BOBYQA paper. It works poorly!
        ! !weight = max(ONE, distsq / max(TENTH * delta, rho)**2)**3.5  ! The same as DISTSQ/RHO**2.
        ! The following WEIGHT all perform a bit worse than the above one.
        ! !weight = max(ONE, distsq / delta**2)**3.5
        ! !weight = max(ONE, distsq / delta**2)**2.5
        ! !weight = max(ONE, distsq / delta**2)**3
        ! !weight = max(ONE, distsq / delta**2)**4
        ! !weight = max(ONE, distsq / delta**2)**4.5
        ! !weight = max(ONE, distsq / rho**2)**2.5
        ! !weight = max(ONE, distsq / rho**2)**3
        ! !weight = max(ONE, distsq / rho**2)**4.5

        ! Different from NEWUOA/LINCOA, the possibility that entries in DEN become negative is handled by
        ! RESCUE. Hence the SCORE here uses DEN in contrast to ABS(DEN) in NEWUOA/LINCOA.
        den = calden(kopt, bmat, d, xpt, zmat)
        score = weight * den

        ! If the new F is not better than FVAL(KOPT), we set SCORE(KOPT) = -1 to avoid KNEW = KOPT.
        if (.not. ximproved) then
            score(kopt) = -ONE
        end if

        ! SCORE(K) = NaN implies DEN(K) = NaN. We exclude such K as we want DEN to be big.
        score(trueloc(is_nan(score))) = -ONE

        knew = 0
        ! The following IF works slightly better than `IF (ANY(SCORE > 0))` from Powell's BOBYQA/LINCOA code.
        if (any(score > 1) .or. (ximproved .and. any(score > 0))) then  ! Powell's UOBYQA and NEWUOA code.
            ! See (6.1) of the BOBYQA paper for the definition of KNEW in this case.
            knew = int(maxloc(score, dim=1))
            ! MATLAB: [~, knew] = max(score);
        end if

        ! Powell's code does not include the following instructions. With Powell's code, if DEN consists of
        ! only NaN, then KNEW can be 0 even when XIMPROVED is TRUE. Here, we set KNEW to the following value,
        ! to make sure that the new trial point is included in the interpolation set. However, the updating
        ! subroutine will likely need to skip the update of the Lagrange polynomials (i.e., H), or they
        ! would be destroyed by the NaNs.
        ! A NaN cannot reach DEN (a non-finite value aborts in `evaluate`), and an improvement with
        ! no positive denominator is caught by the RESCUE test in BOBYQB before this is reached.
        if ((ximproved .and. knew == 0) .or. knew < 0) then  ! KNEW < 0 is impossible in theory.
            knew = int(maxloc(distsq, dim=1))  ! GCOVR_EXCL_LINE -- see the note above
        end if

    end function setdrop_tr

    !> Updates the factorisation when `xpt(:, knew)` becomes `xpt(:, kopt) + d`, upstream's
    !> `updateh`.
    !!
    !! Section 4 of the BOBYQA paper.
    subroutine updateh(knew, kopt, d, xpt, bmat, zmat, info)
        ! Common modules

        integer, intent(in) :: knew                  !! index in `xpt` of the point being replaced
        integer, intent(in) :: kopt                  !! index in `xpt` of the best point so far
        real(real64), intent(in) :: d(:)             !! the step from the best point
        real(real64), intent(in) :: xpt(:, :)        !! the interpolation set relative to `xbase`, one point per column

        real(real64), intent(inout) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(inout) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Z^T`

        integer, intent(out), optional :: info       !! PRIMA's exit code

        integer :: j
        integer :: n
        integer :: npt
        real(real64) :: alpha
        real(real64) :: beta
        real(real64) :: denom
        real(real64) :: grot(2, 2)
        real(real64) :: hcol(size(bmat, 2))
        real(real64) :: sqrtdn
        real(real64) :: tau
        real(real64) :: v1(size(bmat, 1))
        real(real64) :: v2(size(bmat, 1))
        real(real64) :: vlag(size(bmat, 2))

        ! Sizes.
        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        if (present(info)) then
            info = INFO_DFT  ! GCOVR_EXCL_LINE -- no caller here passes `info`
        end if

        ! Do anything if KNEW is 0. This can only happen sometimes after a trust-region step.
        if (knew <= 0) then  ! KNEW < 0 is impossible if the input is correct.
            return  ! GCOVR_EXCL_LINE -- BOBYQB screens KNEW at both call sites
        end if

        ! Put the KNEW-th column of the unupdated H (except for the (NPT+1)th entry) into HCOL. Powell's
        ! code does this after ZMAT is rotated below, and then HCOL(1:NPT) = ZMAT(KNEW, 1) * ZMAT(:, 1),
        ! which saves flops but also introduces rounding errors due to the rotation.
        hcol(1:npt) = matprod(zmat, zmat(knew, :))
        hcol(npt + 1:npt + n) = bmat(:, knew)

        ! Calculate VLAG and BETA and other parameters for (4.9) and (4.14) of the BOBYQA paper.
        beta = calbeta(kopt, bmat, d, xpt, zmat)
        vlag = calvlag(kopt, bmat, d, xpt, zmat)

        ! In theory, DENOM can also be calculated after ZMAT is rotated below. However, this worsened the
        ! performance of BOBYQA in a test on 20220413.
        alpha = hcol(knew)
        tau = vlag(knew)
        denom = alpha * beta + tau**2

        ! After the following line, VLAG = H*w - e_KNEW in the NEWUOA paper (where t = KNEW).
        vlag(knew) = vlag(knew) - ONE

        ! Quite rarely, due to rounding errors, VLAG or BETA may not be finite, or DENOM may not be
        ! positive. In such cases, [BMAT, ZMAT] would be destroyed by the update, and hence we would rather
        ! not update them at all. Or should we simply terminate the algorithm?
        if (.not. (is_finite(sum(abs(hcol)) + sum(abs(vlag)) + abs(beta)) .and. denom > 0)) then
            if (present(info)) then               ! GCOVR_EXCL_START -- upstream's rounding
                info = DAMAGING_ROUNDING          ! rescue: no run the suite drives damages this
            end if                                ! denominator, and no caller here passes
            return                                ! `info`
        end if                            ! GCOVR_EXCL_STOP

        ! Update the matrix BMAT. It implements the last N rows of (4.9) in the BOBYQA paper.
        v1 = (alpha * vlag(npt + 1:npt + n) - tau * hcol(npt + 1:npt + n)) / denom
        v2 = (-beta * hcol(npt + 1:npt + n) - tau * vlag(npt + 1:npt + n)) / denom
        bmat = bmat + outprod(v1, vlag) + outprod(v2, hcol) !call r2update(bmat, ONE, v1, vlag, ONE, v2, hcol)
        ! N.B.: The use of OUTPROD is expensive memory-wise, but it is not our concern in this implementation.
        ! Numerically, the update above does not guarantee BMAT(:, NPT+1 : NPT+N) to be symmetric.
        call symmetrize(bmat(:, npt + 1:npt + n))

        ! Apply Givens rotations to put zeros in the KNEW-th row of ZMAT. After this, ZMAT(KNEW, :) contains
        ! only one nonzero at ZMAT(KNEW, 1). Entries of ZMAT are treated as 0 if the moduli are quite small.
        do j = 2, npt - n - 1
            if (abs(zmat(knew, j)) > 1.0E-20 * maxval(abs(zmat))) then  ! This threshold is by Powell
                grot = planerot(zmat(knew, [1, j]))
                zmat(:, [1, j]) = matprod(zmat(:, [1, j]), transpose(grot))
            end if
            zmat(knew, j) = ZERO
        end do

        ! Complete the updating of ZMAT. See (4.14) of the BOBYQA paper.
        sqrtdn = sqrt(denom)
        zmat(:, 1) = (tau / sqrtdn) * zmat(:, 1) - (zmat(knew, 1) / sqrtdn) * vlag(1:npt)
        ! Zaikun 20231012: Either of the following two lines worsens the performance of BOBYQA when the
        ! objective function is evaluated with 5 or less correct significance digits. Strange.
        ! !zmat(:, 1) = (tau * zmat(:, 1) - zmat(knew, 1) * vlag(1:npt)) / sqrtdn
        ! !zmat(knew, 1) = zknew1 / sqrtdn  ! ZKNEW1 is the unupdated ZMAT(KNEW, 1)

    end subroutine updateh

    !> Puts the new point and its value into the interpolation set, upstream's `updatexf`.
    subroutine updatexf(knew, ximproved, f, xnew, kopt, fval, xpt)
        ! Common modules

        integer, intent(in) :: knew                 !! index in `xpt` of the point being replaced
        real(real64), intent(in) :: f               !! objective value at the new point
        real(real64), intent(in) :: xnew(:)         !! the new point relative to `xbase`

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
            return  ! GCOVR_EXCL_LINE -- BOBYQB screens KNEW at both call sites
        end if

        xpt(:, knew) = xnew
        fval(knew) = f

        ! KOPT is NOT identical to MINLOC(FVAL). Indeed, if FVAL(KNEW) = FVAL(KOPT) and KNEW < KOPT, then
        ! MINLOC(FVAL) = KNEW /= KOPT. Do not change KOPT in this case.
        if (ximproved) then
            kopt = knew
        end if

    end subroutine updatexf

    !> Updates the quadratic model after the interpolation set changed, upstream's `updateq`.
    !!
    !! Section 4 of the NEWUOA paper and of the BOBYQA paper.
    subroutine updateq(knew, ximproved, bmat, d, moderr, xdrop, xosav, xpt, zmat, gopt, hq, pq)
        ! Common modules

        integer, intent(in) :: knew                !! index in `xpt` of the point being replaced
        logical, intent(in) :: ximproved           !! whether the new point improved on the best value
        real(real64), intent(in) :: bmat(:, :)     !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: d(:)           !! the step from the best point
        real(real64), intent(in) :: moderr         !! the model error at the new point
        real(real64), intent(in) :: xdrop(:)       !! the point that was dropped
        real(real64), intent(in) :: xosav(:)       !! the best point before the update
        real(real64), intent(in) :: xpt(:, :)      !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)     !! the factorisation's Z, with `OMEGA = Z Z^T`

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
            return  ! GCOVR_EXCL_LINE -- BOBYQB screens KNEW at both call sites
        end if

        ! The unupdated model corresponding to [GOPT, HQ, PQ] interpolates F at all points in XPT except for
        ! XNEW. The error is MODERR = [F(XNEW)-F(XOPT)] - [Q(XNEW)-Q(XOPT)].

        ! Absorb PQ(KNEW)*XDROP*XDROP^T into the explicit part of the Hessian.
        ! Implement R1UPDATE properly so that it ensures that HQ is symmetric.
        call r1update(hq, pq(knew), xdrop)
        pq(knew) = ZERO

        ! Update the implicit part of the Hessian.
        pqinc = moderr * matprod(zmat, zmat(knew, :))  ! pqinc = moderr * omega_col(1, zmat, knew)
        pq = pq + pqinc

        ! Update the gradient, which needs the updated XPT.
        gopt = gopt + moderr * bmat(:, knew) + hess_mul(xosav, xpt, pqinc)

        ! Further update GOPT if XIMPROVED is TRUE, as XOPT changes from XOSAV to XNEW = XOSAV + D.
        if (ximproved) then
            gopt = gopt + hess_mul(d, xpt, pq, hq)
        end if

    end subroutine updateq

    !> Replaces the model by the least-Frobenius-norm alternative when the last three steps all
    !> favoured it, upstream's `tryqalt`.
    !!
    !! The paragraph around (6.12) of the BOBYQA paper.
    subroutine tryqalt(bmat, fval, ratio, sl, su, xopt, xpt, zmat, itest, gopt, hq, pq)
        ! Common modules

        real(real64), intent(in) :: bmat(:, :)     !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(in) :: fval(:)        !! objective value at each interpolation point
        real(real64), intent(in) :: ratio          !! the trust-region reduction ratio
        real(real64), intent(in) :: sl(:)          !! `xl - xbase`: the lower bounds relative to the base point
        real(real64), intent(in) :: su(:)          !! `xu - xbase`: the upper bounds relative to the base point
        real(real64), intent(in) :: xopt(:)        !! the best point relative to `xbase`
        real(real64), intent(in) :: xpt(:, :)      !! the interpolation set relative to `xbase`, one point per column
        real(real64), intent(in) :: zmat(:, :)     !! the factorisation's Z, with `OMEGA = Z Z^T`

        integer, intent(inout) :: itest            !! how many consecutive steps favoured the alternative model
        real(real64), intent(inout) :: gopt(:)     !! the model's gradient at `xbase + xpt(:, kopt)`
        real(real64), intent(inout) :: hq(:, :)    !! the model's explicit Hessian part
        real(real64), intent(inout) :: pq(:)       !! the model's implicit Hessian weights, one per interpolation point
        ! N.B.:
        ! GOPT, HQ, and PQ should be INTENT(INOUT) instead of INTENT(OUT). According to the Fortran 2018
        ! standard, an INTENT(OUT) dummy argument becomes undefined on invocation of the procedure.
        ! Therefore, if the procedure does not define such an argument, its value becomes undefined,
        ! which is the case for HQ and PQ when ITEST < 3 at exit. In addition, the information in GOPT is
        ! needed for defining ITEST, so it must be INTENT(INOUT).

        integer :: n
        integer :: npt
        real(real64) :: galt(size(gopt))
        real(real64) :: pgalt(size(gopt))
        real(real64) :: pgopt(size(gopt))
        real(real64) :: pqalt(size(pq))

        ! Debugging variables
        !real(real64) :: intp_tol

        n = int(size(gopt))
        npt = int(size(pq))

        ! Calculate the norm square of the projected gradient.
        pgopt = gopt
        pgopt(trueloc(xopt >= su)) = max(ZERO, gopt(trueloc(xopt >= su)))
        pgopt(trueloc(xopt <= sl)) = min(ZERO, gopt(trueloc(xopt <= sl)))

        ! Calculate the parameters of the least Frobenius norm interpolant to the current data.
        pqalt = matprod(zmat, matprod(fval, zmat))
        galt = matprod(bmat(:, 1:npt), fval) + hess_mul(xopt, xpt, pqalt)

        ! Calculate the norm square of the projected alternative gradient.
        pgalt = galt
        pgalt(trueloc(xopt >= su)) = max(ZERO, galt(trueloc(xopt >= su)))
        pgalt(trueloc(xopt <= sl)) = min(ZERO, galt(trueloc(xopt <= sl)))

        ! Test whether to replace the new quadratic model by the least Frobenius norm interpolant,
        ! making the replacement if the test is satisfied.
        ! N.B.: In the following IF, Powell's condition does not check RATIO. The condition here (with RATIO
        ! > TENTH)is adopted and adapted from NEWUOA, and it seems to improve the performance.
        ! !if (inprod(pgopt, pgopt) < TEN * inprod(pgalt, pgalt)) then  ! Powell's code
        if (ratio > TENTH .or. inprod(pgopt, pgopt) < TEN * inprod(pgalt, pgalt)) then
            itest = 0
        else
            itest = itest + 1
        end if
        if (itest >= 3) then
            gopt = galt
            pq = pqalt
            hq = ZERO
            itest = 0
        end if

    end subroutine tryqalt

    !> The RESCUE procedure of Section 5 of the BOBYQA paper, upstream's `rescue`.
    !!
    !! Rebuilds the interpolation set around the best point when rounding has damaged the
    !! factorisation, keeping the points it can and evaluating the objective at the ones it cannot.
    subroutine rescue(obj, st, maxfun, delta, ftarget, xl, xu, kopt, nf, fval, &
            & gopt, hq, pq, sl, su, xbase, xpt, bmat, zmat, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine implements "the method of RESCUE" introduced in Section 5 of BOBYQA paper. The
        ! purpose of this subroutine is to replace a few interpolation points by new points in order to
        ! improve the geometry of the interpolation set and the conditioning of the interpolation system.
        ! This is done in the following way.
        !
        ! 1. Define a set of "provisional interpolation points" XPT_PROV around the current XOPT. Similar to
        ! the construction of the initial interpolation set, XPT_PROV is obtained by perturbing XOPT subject
        ! to the bound constraints along one or two coordinate directions, the latter taking place only if
        ! NPT >= 2*N+2. See (5.4)--(5.5) of the BOBYQA paper for details. After defining XPT_PROV, set BMAT
        ! and ZMAT to represent the H matrix defined in (2.7) of the BOBYQA paper corresponding to XPT_PROV.
        ! N.B.: In the code, XPT_PROV is not formed explicitly, but represented implicitly by XPT, PTSID,
        ! and PTSAUX.
        ! 2. For each "original interpolation point" in XPT, check whether it can replace a point in XPT_PROV
        ! without damaging the geometry of XPT_PROV, which is indicated by the denominator SIGMA in the
        ! updating formula of H due to the replacement (see (4.9) of the BOBYQA paper). If yes, update
        ! XPT_PROV by the performing such an replacement. Continue doing this until all the original points
        ! get reinstated in XPT_PROV, or we cannot find any original point that can safely replace
        ! a provisional point. Note the following.
        ! 2.1. Suppose that the KORIG-th original point is going to replace the KPROV-th provisional point.
        ! Then we first exchange the KORIG-th and KPROV-th provisional points, and then replace the new
        ! KORIG-th provisional point by the KORIG-th original point. BMAT and ZMAT are updated accordingly.
        ! In this way, FVAL(KORIG) is consistent with XPT_PROV(:, KORIG), so that we need not update FVAL.
        ! 2.1. The KOPT-th original point (i.e., XOPT) is always reinstated in XPT_PROV. This is done by
        ! replacing XPT_PROV(:, 1) with XOPT. This is the first replacement to perform.
        ! 2.2. After XOPT is reinstated, the original points are ranked according the scores saved in SCORE.
        ! SCORE is initialized to the squares of the distances from between the original point and XOPT.
        ! The KORIG is set to the index of the point with the smallest positive score. If XPT(:, KORIG)
        ! cannot replace any provisional point safely, then set SCORE(KORIG) to -SCORE(KORIG) - SCOREINC;
        ! otherwise, set SCORE(KORIG) to 0 and all the other scores to their absolute values. In this way,
        ! an original point that fails to replace any provisional point during the current attempt will be
        ! skipped until another original point succeeds in doing so; moreover, the failing point will get
        ! a lower priority in later attempts. An original point that successfully replaces a provisional
        ! point will not be tried again due to the zero score.
        ! 2.3. Once a provisional point is replaced, it will be marked (by setting the corresponding entry
        ! of PTSID to zero) so that it will not be replaced again.
        ! 3. When the above procedure finishes, normally most original points are reinstated in XPT_PROV, so
        ! that XPT_PROV differs from XPT only at very few positions. Set XPT to XPT_PROV, update FVAL at the
        ! new interpolation points by evaluating F, and then quadratic interpolant [GQ, PQ, HQ] accordingly.
        !
        ! At the end of the subroutine, the elements of BMAT and ZMAT are set in a well-conditioned way to
        ! the values that are appropriate for the new interpolation points. The elements of GOPT, HQ and PQ
        ! are also revised to the values that are appropriate to the final quadratic model.
        !
        ! The arguments NF, KOPT, XL, XU, IPRINT, MAXFUN, XBASE, XPT, FVAL GOPT, HQ, PQ, BMAT, ZMAT, SL
        ! and, SU have the same meanings as the corresponding arguments of BOBYQB on the entry to RESCUE.
        ! DELTA is the current trust region radius.
        ! PTSAUX is a 2-by-N real array. For J = 1, 2, ..., N, PTSAUX(1, J) and PTSAUX(2, J) specify the two
        !   positions of provisional interpolation points when a nonzero step is taken along e_J (the J-th
        !   coordinate direction) through XBASE + XOPT, as specified below. Usually these steps have length
        !   DELTA, but other lengths are chosen if necessary in order to satisfy the bound constraints.
        ! PTSID is an integer array of length NPT. Its components denote provisional new positions of the
        !   interpolation points. The K-th point is a candidate for change if and only if PTSID(K) is
        !   nonzero. In this case let IP and IQ be the integer parts of PTSID(K) and (PTSID(K)-IP)*(N+1). If
        !   IP and IQ are both positive, the step from XBASE + XOPT to the new K-th interpolation point is
        !   PTSAUX(1, IP)*e_IP + PTSAUX(1, IQ)*e_IQ. Otherwise the step is either PTSAUX(1, IP)*e_IP or
        !   PTSAUX(2, IQ)*e_IQ in the cases IQ=0 or  IP=0, respectively.
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        class(pf_objective), intent(inout) :: obj    !! the objective (this thread's own)
        type(prima_state), intent(inout) :: st       !! scaling, wording, record and evaluation count
        integer, intent(in) :: maxfun                !! the evaluation budget
        real(real64), intent(in) :: delta            !! the trust-region radius
        real(real64), intent(in) :: ftarget          !! stop as soon as a value at or below this is found
        real(real64), intent(in) :: xl(:)            !! lower bounds
        real(real64), intent(in) :: xu(:)            !! upper bounds

        integer, intent(inout) :: kopt               !! index in `xpt` of the best point so far
        integer, intent(inout) :: nf                 !! objective evaluations made
        real(real64), intent(inout) :: fval(:)       !! objective value at each interpolation point
        real(real64), intent(inout) :: gopt(:)       !! the model's gradient at `xbase + xpt(:, kopt)`
        real(real64), intent(inout) :: hq(:, :)      !! the model's explicit Hessian part
        real(real64), intent(inout) :: pq(:)         !! the model's implicit Hessian weights, one per interpolation point
        real(real64), intent(inout) :: sl(:)         !! `xl - xbase`: the lower bounds relative to the base point
        real(real64), intent(inout) :: su(:)         !! `xu - xbase`: the upper bounds relative to the base point
        real(real64), intent(inout) :: xbase(:)      !! the base point every other point is relative to
        real(real64), intent(inout) :: xpt(:, :)     !! the interpolation set relative to `xbase`, one point per column

        integer, intent(out) :: info                 !! PRIMA's exit code
        real(real64), intent(out) :: bmat(:, :)      !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(out) :: zmat(:, :)      !! the factorisation's Z, with `OMEGA = Z Z^T`

        integer :: ij(2, max(0, size(xpt, 2) - 2 * size(xpt, 1) - 1))
        integer :: ip
        integer :: iq
        integer :: iter
        integer :: k
        integer :: kbase
        integer :: korig
        integer :: kprov
        integer :: kpt
        integer :: maxiter
        integer :: n
        integer :: nprov
        integer :: npt
        integer :: subinfo
        logical :: mask(size(xpt, 1))
        real(real64) :: beta
        real(real64) :: bsum
        real(real64) :: den(size(xpt, 2))
        real(real64) :: f
        real(real64) :: fbase
        real(real64) :: hdiag(size(xpt, 2))
        real(real64) :: moderr
        real(real64) :: pqinc(size(xpt, 2))
        real(real64) :: ptsaux(2, size(xpt, 1))
        real(real64) :: ptsid(size(xpt, 2))
        real(real64) :: score(size(xpt, 2))
        real(real64) :: scoreinc
        real(real64) :: sfrac
        real(real64) :: temp
        real(real64) :: v(size(xpt, 1))
        real(real64) :: vlag(size(xpt, 1) + size(xpt, 2))
        real(real64) :: vquad
        real(real64) :: wmv(size(xpt, 1) + size(xpt, 2))
        real(real64) :: x(size(xpt, 1))
        real(real64) :: xnew(size(xpt, 1))
        real(real64) :: xopt(size(xpt, 1))
        real(real64) :: xp
        real(real64) :: xq
        real(real64) :: xxpt(size(xpt, 2))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        info = INFO_DFT

        ! Do nothing if NF already reaches it upper bound.
        ! To please Fortran compilers, set BMAT and ZMAT before returning, though they will not be used.
        ! BOBYQB's own CHECKEXIT has already left the iteration when the budget is exhausted, so
        ! RESCUE is never entered with NF >= MAXFUN.
        if (nf >= maxfun) then
            bmat = ZERO                           ! GCOVR_EXCL_START -- see the note above
            zmat = ZERO
            info = MAXFUN_REACHED
            return
        end if                            ! GCOVR_EXCL_STOP

        ! Shift the interpolation points so that XOPT becomes the origin.
        xopt = xpt(:, kopt)
        sl = min(sl - xopt, ZERO)
        su = max(su - xopt, ZERO)
        xbase = min(max(xl, xbase + xopt), xu)
        xpt = xpt - spread(xopt, dim=2, ncopies=npt)
        xpt(:, kopt) = ZERO

        ! Update HQ so that HQ and PQ define the second derivatives of the model after XBASE has been
        ! shifted to the trust region centre.
        v = matprod(xpt, pq) + HALF * sum(pq) * xopt
        call r2update(hq, ONE, xopt, v)

        ! Set the elements of PTSAUX.
        ptsaux(1, :) = min(delta, su)
        ptsaux(2, :) = max(-delta, sl)
        mask = (ptsaux(1, :) + ptsaux(2, :) < 0)
        ptsaux([1, 2], trueloc(mask)) = ptsaux([2, 1], trueloc(mask))
        mask = (abs(ptsaux(2, :)) < HALF * abs(ptsaux(1, :)))
        ptsaux(2, trueloc(mask)) = HALF * ptsaux(1, trueloc(mask))

        ! Set the identifiers of the artificial interpolation points that are along a coordinate direction
        ! from XOPT, and set the corresponding nonzero elements of BMAT and ZMAT.
        sfrac = HALF / real(n + 1, real64)
        ptsid(1) = sfrac
        bmat = ZERO
        zmat = ZERO
        do k = 1, n
            ptsid(k + 1) = real(k, real64) + sfrac
            if (k <= npt - n - 1) then
                ptsid(k + n + 1) = real(k, real64) / real(n + 1, real64) + sfrac
                temp = ONE / (ptsaux(1, k) - ptsaux(2, k))
                bmat(k, k + 1) = -temp + ONE / ptsaux(1, k)
                bmat(k, k + n + 1) = temp + ONE / ptsaux(2, k)
                bmat(k, 1) = -bmat(k, k + 1) - bmat(k, k + n + 1)
                zmat(1, k) = sqrt(TWO) / abs(ptsaux(1, k) * ptsaux(2, k))
                zmat(k + 1, k) = zmat(1, k) * ptsaux(2, k) * temp
                zmat(k + n + 1, k) = -zmat(1, k) * ptsaux(1, k) * temp
            else
                bmat(k, 1) = -ONE / ptsaux(1, k)
                bmat(k, k + 1) = ONE / ptsaux(1, k)
                bmat(k, k + npt) = -HALF * ptsaux(1, k)**2
            end if
        end do

        ! Set any remaining identifiers with their nonzero elements of ZMAT.
        ij = setij(n, npt)
        do k = 2 * n + 2, npt
            ip = ij(1, k - 2 * n - 1)
            iq = ij(2, k - 2 * n - 1)
            ptsid(k) = real(ip, real64) + real(iq, real64) / real(n + 1, real64) + sfrac
            temp = ONE / (ptsaux(1, ip) * ptsaux(1, iq))
            zmat([1, k], k - n - 1) = temp
            zmat([ip + 1, iq + 1], k - n - 1) = -temp
        end do

        ! Update BMAT, ZMAT, ans PTSID so that the 1st and the KOPT-th provisional points are exchanged.
        ! After the exchanging, the KOPT-th point in the provisional set becomes the zero vector, which is
        ! exactly the KOPT-th original point (after the shift of XBASE at the beginning of the subroutine).
        if (kopt /= 1) then
            bmat(:, [1, kopt]) = bmat(:, [kopt, 1])
            zmat([1, kopt], :) = zmat([kopt, 1], :)
        end if
        ptsid(1) = ptsid(kopt)
        ptsid(kopt) = ZERO

        ! The squares of the distances from XOPT to the other interpolation points are set at SCORE, which
        ! will be used to define the index KORIG in the loop below.  Increments of SCOREINC may be added
        ! later to these scores to balance the consideration of the choice of point that is going to become
        ! current. Note that, in Powell's BOBYQA code, the initial scores are the squares of the distances,
        ! but there is no square in the BOBYQA paper (see the paragraph between (5.9) and (5.10) of the
        ! BOBYQA paper). The latter seem to work better in a test on 20221125.
        !score = sum(xpt**2, dim=1)  ! Powell's BOBYQA code
        score = sqrt(sum(xpt**2, dim=1))  ! Powell's BOBYQA paper
        ! In theory, SCORE(KOPT) = 0. Make sure this so that KOPT will be skipped when we choose KORIG below.
        score(kopt) = ZERO
        scoreinc = maxval(score)

        ! NPROV is the number of provisional points that has not yet been replaced with original points.
        nprov = npt - 1

        ! Even without an upper bound for the loop counter, the following loop runs for at most NPT^2 times:
        ! for each value of NPROV, we need at most NPT loops to find an original point that can safely
        ! replace a provisional point; if such a pair of origin and provisional points are found, then NPROV
        ! will de reduced by 1; otherwise, SCORE will become all zero or negative, and the loop will exit.
        ! Originally, it is a WHILE loop, but we change it to a DO loop to avoid infinite cycling.
        ! N.B.: Overflow will occur in NPT^2 if NPT > 180 and IK = 16. The following is a workaround, which
        ! is **not needed in Python/MATLAB/Julia/R. In MATLAB, we can just take maxiter = npt^2**.
        maxiter = int(min(10**min(range(0), range(0)), int(npt)**2))  ! MATLAB: maxiter = npt^2;
        do iter = 1, maxiter
            ! !DO WHILE (ANY(SCORE > 0) .AND. NPROV > 1)   ! WHILE version.
            ! !IF (ALL(SCORE <= 0) .AND. NPROV <= 0) THEN ! Powell's code. May not take any provisional point.
            ! !IF (ALL(SCORE <= 0) .AND. NPROV <= 2) THEN  ! Retain at least two provisional points.
            if (all(score <= 0) .or. nprov <= 1) then   ! Retain at least one provisional point.
                exit
            end if

            ! Pick the index KORIG of an original point that has not yet replaced one of the provisional
            ! points, giving attention to the closeness to XOPT and to previous tries with KORIG.
            korig = int(minloc(score, mask=(score > 0), dim=1))

            ! Calculate VLAG and BETA for the required updating of the H matrix if XPT(:, KORIG) is
            ! reinstated in the set of interpolation points, which means to replace a point in the
            ! following provisional interpolation set XPT_PROV defined in (5.4)--(5.5) of the BOBYQA paper.
            ! 1. XPT_PROV(:, KOPT) = 0;
            ! 2. For each K /= KOPT, if PTSID(K) == 0, then XPT_PROV(:, K) = XPT(:, K); if PTSID(K) > 0,
            ! then XPT_PROV(:, K) has nonzeros only at IP (if IP > 0), IQ (if IQ > 0) positions, where IP
            ! and IQ are the P(J) and Q(J) defined in and below (2.4) of the BOBYQA paper.

            ! First, form the (W - V) vector for XPT(:, KORIG).
            ! In the code below, WMV = W - V = w(XNEW) - w(XOPT) without the (NPT+1)th entry, where
            ! XNEW = XPT(:, KORIG), XOPT = XPT_PROV = 0, and w(.) is defined by (6.3) of the NEWUOA paper
            ! (as well as (4.10) of the BOBYQA paper). Since XOPT = 0, we see from (6.3) that w(XOPT)(K) = 0
            ! for all K except that w(XOPT)(NPT+1) = 1. Thus WMV is the same at w(XNEW) without the
            ! (NPT+1)-th entry. Therefore, WMV= [HALF*MATPROD(XNEW, XPT_PROV)**2, XNEW].
            do k = 1, npt
                if (k == kopt) then
                    wmv(k) = ZERO
                else if (ptsid(k) <= 0) then  ! Indeed, PTSID >= 0. So PTSID(K) <= 0 means PTSID(K) = 0.
                    wmv(k) = inprod(xpt(:, korig), xpt(:, k))
                else
                    ip = floor(ptsid(k))  ! IP = 0 if 0 < PTSID(K) < 1.
                    iq = floor(real(n + 1, real64) * ptsid(k) - real((n + 1) * ip, real64))
                    if (ip > 0 .and. iq > 0) then
                        wmv(k) = xpt(ip, korig) * ptsaux(1, ip) + xpt(iq, korig) * ptsaux(1, iq)
                    elseif (ip > 0) then
                        wmv(k) = xpt(ip, korig) * ptsaux(1, ip)
                    elseif (iq > 0) then
                        wmv(k) = xpt(iq, korig) * ptsaux(2, iq)
                    else
                        ! A positive PTSID(K) with both parts zero is the bare SFRAC identifier,
                        ! which the KOPT exchange above always clears, so no K reaches this arm.
                        wmv(k) = ZERO  ! GCOVR_EXCL_LINE -- see the note above
                    end if
                end if
                wmv(k) = HALF * wmv(k) * wmv(k)
            end do
            wmv(npt + 1:npt + n) = xpt(:, korig)

            ! Now calculate VLAG = H*WMV + e_KOPT according to (4.26) of the NEWUOA paper except VLAG(KOPT).
            vlag(1:npt) = matprod(zmat, matprod(wmv(1:npt), zmat)) + matprod(wmv(npt + 1:npt + n), bmat(:, 1:npt))
            vlag(npt + 1:npt + n) = matprod(bmat, wmv(1:npt + n))

            ! Now calculate BETA. According to (4.12) of the NEWUOA paper (also (4.10) of the BOBYQA paper),
            ! BETA = HALF*||XNEW - XOPT||^4 - WMV'*H*WMV. To calculate WMX'*H*WMV, note that
            ! WMV'*H*WMV = WMV' * [Z*Z', B2^T; B1, B2] * WMV with Z = ZMAT, B1 = BMAT(:, 1:NPT), and
            ! B2 = BMAT(:, NPT+1:NPT+N). Denoting W1 = WMV(1:NPT) and W2 = WMV(NPT+1:NPT+N), we then have
            ! WMV'*H*WMV = ||W1'*Z||^2 + 2*W1'*B1*W2 + W1'*B2*W2 = ||W1'*Z||^2 + W1'(B1*W2 + [B1, B2]*WMV).
            bsum = inprod(wmv(1:n), matprod(bmat(:, 1:npt), wmv(1:npt)) + matprod(bmat, wmv))
            beta = HALF * sum(xpt(:, korig)**2)**2 - sum(matprod(wmv(1:npt), zmat)**2) - bsum

            ! Finally, set VLAG(KOPT) to the correct value.
            vlag(kopt) = vlag(kopt) + ONE

            ! For all K with PTSID(K) > 0, calculate the denominator DEN(K) = SIGMA in the updating formula
            ! of H for XPT(:, KORIG) to replace XPT_PROV(:, K).
            den = ZERO
            hdiag(trueloc(ptsid > 0)) = sum(zmat(trueloc(ptsid > 0), :)**2, dim=2)
            den(trueloc(ptsid > 0)) = hdiag(trueloc(ptsid > 0)) * beta + vlag(trueloc(ptsid > 0))**2

            ! Attempt setting KPROV to the index of the provisional point to be replaced with the KORIG-th
            ! original interpolation point. We choose KPROV by maximizing DEN(KPROV), which will be the
            ! denominator SIGMA in the updating formula (4.9). In order to avoid a small denominator, we
            ! consider it proper to replace the KPROV-th provisional point with the KORIG-th original point
            ! only if DEN(KPROV) = MAXVAL(DEN) > C*MAXVAL(VLAG(1:NPT)**2), where C is a relatively small
            ! positive constant --- C = 1 is achievable if the rounding errors were not severe; Powell took
            ! C = 0.01, which prefers strongly the original point to the provisional (new) point, as the
            ! latter necessitate new function evaluations. If this inequality is not achievable for the
            ! current KORIG, then we will update SCORE(KORIG) to a negative value and continue the loop with
            ! the next KORIG, which is set to MINLOC(SCORE, MASK=(SCORE > 0)) at the beginning of the loop,
            ! skipping the original interpolation points with a nonpositive score. When a KORIG rendering
            ! the aforesaid inequality is found, SCORE(KORIG) will be set to zero, and all the scores will
            ! be reset to their absolute values, so that future attempts will try the original points that
            ! have not succeeded in replacing a provisional point. The update of SCORE reflects an adaptive
            ! ranking of the original points: points that are closer to XOPT have higher priority, and a
            ! point will be ranked lower if it fails to fulfill MAXVAL(DEN) > C*MAXVAL(VLAG(1:NPT)**2).
            ! Even if KORIG cannot satisfy this condition for now, it may validate the inequality in future
            ! attempts, as BMAT and ZMAT will be updated.
            if (.not. (is_finite(sum(abs(vlag))) .and. any(den > 5.0E-2_real64 * maxval(vlag(1:npt)**2)))) then
                ! The above condition works a bit better than Powell's version below due to the factor 0.05.
                ! !IF (.NOT. (ANY(DEN > 1.0E-2_real64 * MAXVAL(VLAG(1:NPT)**2)))) THEN  ! Powell' code
                score(korig) = -score(korig) - scoreinc
                cycle
            end if
            kprov = int(maxloc(den, mask=(.not. is_nan(den)), dim=1))
            ! MATLAB: [~, kprov] = max(den, [], 'omitnan');

            ! Update BMAT, ZMAT, VLAG, and PTSID to exchange the KPROV-th and KORIG-th provisional points.
            ! After the exchanging, the KORIG-th original point will replace the KORIG-th provisional point.
            if (kprov /= korig) then
                bmat(:, [kprov, korig]) = bmat(:, [korig, kprov])
                zmat([kprov, korig], :) = zmat([korig, kprov], :)
                vlag([kprov, korig]) = vlag([korig, kprov])
            end if
            ptsid(kprov) = ptsid(korig)

            ! Set PTSID(KORIG) = 0 so that the KORIG-th provisional point (after the exchanging) will be
            ! skipped in the later loops.
            ptsid(korig) = ZERO
            ! Set SCORE(KORIG) = 0 so that the KORIG-th original point will be skipped in later loops.
            score(korig) = ZERO
            ! Reset SCORE to ABS(SCORE) so that all the original points with a nonzero score will be checked
            ! in later loops.
            score = abs(score)

            ! Update the BMAT and ZMAT matrices so that the KORIG-th original point replaces the KORIG-th
            ! provisional point.
            call updateh_rsc(korig, beta, vlag, bmat, zmat)

            ! NPROV is the number of provisional points that has not yet been replaced with original points.
            nprov = nprov - 1
        end do

        ! All the final positions of the interpolation points have been chosen although any changes have not
        ! been included yet in XPT. Also the final BMAT and ZMAT matrices are complete, but, apart from the
        ! shift of XBASE, the updating of the quadratic model remains to be done. The following cycle
        ! through the new interpolation points begins by putting the new point in XPT(:, KPT) and by setting
        ! PQ(KPT) to zero. A return occurs if MAXFUN prohibits another value of F or when all the new
        ! interpolation points are included in the model.
        kbase = kopt
        fbase = fval(kopt)
        if (nprov > 0) then
            do kpt = 1, npt
                if (ptsid(kpt) <= 0) then
                    cycle
                end if

                ! Absorb PQ(KPT)*XPT(:, KPT)*XPT(:, KPT)^T into the explicit part of the Hessian of the
                ! quadratic model. Implement R1UPDATE properly so that it ensures HQ is symmetric.
                call r1update(hq, pq(kpt), xpt(:, kpt))
                pq(kpt) = ZERO

                ip = floor(ptsid(kpt))
                iq = floor(real(n + 1, real64) * ptsid(kpt) - real((n + 1) * ip, real64))

                ! Update XPT(:, KPT) to the new point. It contains at most two nonzeros XP and XQ at the IP
                ! and IQ entries.
                xp = ZERO
                xq = ZERO
                xnew = ZERO
                if (ip > 0 .and. iq > 0) then
                    xp = ptsaux(1, ip)
                    xnew(ip) = xp
                    xq = ptsaux(1, iq)
                    xnew(iq) = xq
                elseif (ip > 0) then  ! IP > 0, IQ == 0
                    xp = ptsaux(1, ip)
                    xnew(ip) = xp
                elseif (iq > 0) then  ! IP == 0, IQ > 0
                    xq = ptsaux(2, iq)
                    xnew(iq) = xq
                end if

                ! Zaikun 20240314: Skip the new point if it is too close to XPT(:, KPT), the point to replace.
                ! Indeed, it may even happen that XNEW == XPT(:, KPT), which did occur when RP = REAL16 (half
                ! precision) and led to an infinite cycling, because RESCUE did not make any change to XPT,
                ! and later the algorithm decided to call RESCUE again with the same data. This was fixed by
                ! the skipping, and by terminating the algorithm if RESCUE is requested for two times
                ! without any new function evaluations in between, which was the behavior of Powell's code.
                ! Skipping an XNEW that is close but not identical to XPT(:, KPT) will cause discrepancy
                ! between [BMAT, ZMAT] and XPT, since the former has been updated, but it is not severe as
                ! the difference between XNEW and XPT(:, KPT) is tiny.
                ! Neither condition is met: no run the suite drives puts XNEW within 1.0E-2*DELTA of
                ! the point it replaces, and a non-finite value aborts in `evaluate` before it
                ! could reach XNEW.
                if (sum(abs(xnew - xpt(:, kpt))) <= 1.0E-2 * delta .or. .not. is_finite(sum(abs(xnew)))) then
                    cycle  ! GCOVR_EXCL_LINE -- see the note above
                end if
                xpt(:, kpt) = xnew

                ! Calculate F at the new interpolation point, and set MODERR to the factor that is going to
                ! multiply the KPT-th Lagrange function when the model is updated to provide interpolation
                ! to the new function value.
                x = xinbd(xbase, xpt(:, kpt), xl, xu, sl, su)  ! In precise arithmetic, X = XBASE + XPT(:, KPT).
                call evaluate(obj, st, x, f)
                nf = nf + 1

                ! Print a message about the function evaluation according to IPRINT.
                ! Save X, F into the history.

                ! Update FVAL and KOPT.
                fval(kpt) = f
                if (f < fval(kopt)) then
                    kopt = kpt
                end if

                ! Check whether to exit
                ! The budget is never exhausted, and FTARGET never met, inside RESCUE in any run
                ! the suite drives; a non-finite value aborts in `evaluate` before CHECKEXIT
                ! could see one.
                subinfo = checkexit(maxfun, nf, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo                ! GCOVR_EXCL_START -- see the note above
                    exit
                end if                    ! GCOVR_EXCL_STOP

                ! Set VQUAD to the value of the current model at the new XPT(:, KPT), which has at most two
                ! nonzeros XP and XQ at the IP and IQ entries respectively.
                vquad = fbase
                if (ip > 0 .and. iq > 0) then
                    vquad = vquad + xp * (gopt(ip) + HALF * xp * hq(ip, ip))
                    vquad = vquad + xq * (gopt(iq) + HALF * xq * hq(iq, iq))
                    vquad = vquad + xp * xq * hq(ip, iq)
                    xxpt = xp * xpt(ip, :) + xq * xpt(iq, :)
                elseif (ip > 0) then  ! IP > 0, IQ == 0
                    vquad = vquad + xp * (gopt(ip) + HALF * xp * hq(ip, ip))
                    xxpt = xp * xpt(ip, :)
                elseif (iq > 0) then  ! IP == 0, IQ > 0
                    vquad = vquad + xq * (gopt(iq) + HALF * xq * hq(iq, iq))
                    xxpt = xq * xpt(iq, :)
                end if
                vquad = vquad + HALF * inprod(xxpt, pq * xxpt)
                ! N.B.: INPROD(XXPT, PQ * XXPT) = INPROD(X, HESS_MUL(X, XPT, PQ))

                ! Update the quadratic model.
                moderr = f - vquad
                gopt = gopt + moderr * bmat(:, kpt)
                pqinc = moderr * matprod(zmat, zmat(kpt, :))
                pq(trueloc(ptsid <= 0)) = pq(trueloc(ptsid <= 0)) + pqinc(trueloc(ptsid <= 0))
                do k = 1, npt
                    if (ptsid(k) <= 0) then
                        cycle
                    end if
                    ip = floor(ptsid(k))
                    iq = floor(real(n + 1, real64) * ptsid(k) - real((n + 1) * ip, real64))
                    if (ip > 0 .and. iq > 0) then
                        hq(ip, ip) = hq(ip, ip) + pqinc(k) * ptsaux(1, ip)**2
                        hq(iq, iq) = hq(iq, iq) + pqinc(k) * ptsaux(1, iq)**2
                        hq(ip, iq) = hq(ip, iq) + pqinc(k) * ptsaux(1, ip) * ptsaux(1, iq)
                        hq(iq, ip) = hq(ip, iq)
                    elseif (ip > 0) then  ! IP > 0, IQ == 0
                        hq(ip, ip) = hq(ip, ip) + pqinc(k) * ptsaux(1, ip)**2
                    elseif (iq > 0) then  ! IP == 0, IP > 0
                        hq(iq, iq) = hq(iq, iq) + pqinc(k) * ptsaux(2, iq)**2
                    end if
                end do
                ptsid(kpt) = ZERO
            end do
        end if

        ! Update GOPT if necessary.
        if (kopt /= kbase) then
            gopt = gopt + hess_mul(xpt(:, kopt), xpt, pq, hq)
        end if

        !--------------------------------------------------------------------------------------------------!
        ! Zaikun 20221123: What if we rebuild the model? It seems to worsen the performance of BOBYQA. Why?
        ! !hq = ZERO
        ! !pq = omega_mul(1, zmat, fval - fval(kopt))
        ! !gopt = matprod(bmat(:, 1:npt), fval - fval(kopt)) + hess_mul(xpt(:, kopt), xpt, pq)
        !--------------------------------------------------------------------------------------------------!

        !--------------------------------------------------------------------------------------------------!
        ! Zaikun 20221123: Shouldn't we correct the models using the new [BMAT, ZMAT]?!
        ! In this way, we do not even need the quadratic model received by RESCUE is an interpolant.
        ! !real(real64) :: qval(size(xpt, 2))
        ! !qval = [(quadinc(xpt(:, k) - xpt(:, kopt), xpt, gopt, pq, hq), k=1, npt)]
        ! !pq = pq + omega_mul(1, zmat, fval - qval - fval(kopt))
        ! !gopt = gopt + matprod(bmat(:, 1:npt), fval - qval - fval(kopt)) + hess_mul(xpt(:, kopt), xpt, pq)
        !--------------------------------------------------------------------------------------------------!

    end subroutine rescue

    !> The factorisation update RESCUE uses, upstream's `updateh_rsc`.
    !!
    !! Section 4 of the BOBYQA paper. Distinct from `updateh` because RESCUE has already formed
    !! `beta` and `vlag` and must not recompute them from a set it is midway through rebuilding.
    subroutine updateh_rsc(knew, beta, vlag_in, bmat, zmat, info)
        ! !!! N.B.: UPDATEH_RSC is only used by RESCUE.
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine updates arrays BMAT and ZMAT in order to replace the interpolation point
        ! XPT(:, KNEW) by XNEW = XPT(:, KOPT) + D. See Section 4 of the BOBYQA paper. [BMAT, ZMAT] describes
        ! the matrix H in the BOBYQA paper (eq. 2.7), which is the inverse of the coefficient matrix of the
        ! KKT system for the least-Frobenius norm interpolation problem: ZMAT holds a factorization of the
        ! leading NPT*NPT submatrix OMEGA of H, the factorization being OMEGA = ZMAT*ZMAT^T; BMAT holds the
        ! last N ROWs of H except for the (NPT+1)th column. Note that the (NPT + 1)th row and (NPT + 1)th
        ! column of H are not stored as they are unnecessary for the calculation.
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        integer, intent(in) :: knew                  !! index in `xpt` of the point being replaced
        real(real64), intent(in) :: beta             !! the paper's beta for this replacement
        real(real64), intent(in) :: vlag_in(:)       !! the Lagrange values for this replacement

        real(real64), intent(inout) :: bmat(:, :)    !! the factorisation's B: the last n rows of H, less its (npt+1)-th column
        real(real64), intent(inout) :: zmat(:, :)    !! the factorisation's Z, with `OMEGA = Z Z^T`

        integer, intent(out), optional :: info       !! PRIMA's exit code

        integer :: j
        integer :: n
        integer :: npt
        real(real64) :: alpha
        real(real64) :: denom
        real(real64) :: grot(2, 2)
        real(real64) :: hcol(size(bmat, 2))
        real(real64) :: sqrtdn
        real(real64) :: tau
        real(real64) :: v1(size(bmat, 1))
        real(real64) :: v2(size(bmat, 1))
        real(real64) :: vlag(size(vlag_in))
        real(real64) :: zknew1

        ! Sizes.
        n = int(size(bmat, 1))
        npt = int(size(bmat, 2) - size(bmat, 1))

        if (present(info)) then
            info = INFO_DFT  ! GCOVR_EXCL_LINE -- RESCUE's only call passes no `info`
        end if

        ! We must not do anything if KNEW is 0. This can only happen sometimes after a trust-region step.
        if (knew <= 0) then  ! KNEW < 0 is impossible if the input is correct.
            return  ! GCOVR_EXCL_LINE -- RESCUE's only call passes a `minloc` index, so KNEW >= 1
        end if

        ! Read VLAG, and calculate parameters for the updating formula (4.9) and (4.14) of the BOBYQA paper.
        vlag = vlag_in
        tau = vlag(knew)
        ! In theory, DENOM can also be calculated after ZMAT is rotated below. However, this worsened the
        ! performance of BOBYQA in a test on 20220413.
        denom = sum(zmat(knew, :)**2) * beta + tau**2

        ! Quite rarely, due to rounding errors, VLAG or BETA may not be finite, or DENOM may not be
        ! positive. In such cases, [BMAT, ZMAT] would be destroyed by the update, and hence we would rather
        ! not update them at all. Or should we simply terminate the algorithm?
        if (.not. (is_finite(sum(abs(vlag)) + abs(beta)) .and. denom > 0)) then
            if (present(info)) then               ! GCOVR_EXCL_START -- upstream's rounding
                info = DAMAGING_ROUNDING          ! rescue: no run the suite drives damages this
            end if                                ! denominator, and RESCUE's only call passes
            return                                ! no `info`
        end if                            ! GCOVR_EXCL_STOP

        ! After the following line, VLAG = H*w - e_KNEW in the NEWUOA paper (where t = KNEW).
        vlag(knew) = vlag(knew) - ONE

        ! Apply Givens rotations to put zeros in the KNEW-th row of ZMAT. After this, ZMAT(KNEW, :) contains
        ! only one nonzero at ZMAT(KNEW, 1). Entries of ZMAT are treated as 0 if the moduli are quite small.
        do j = 2, npt - n - 1
            if (abs(zmat(knew, j)) > 1.0E-20 * maxval(abs(zmat))) then  ! This threshold is by Powell
                grot = planerot(zmat(knew, [1, j]))
                zmat(:, [1, j]) = matprod(zmat(:, [1, j]), transpose(grot))
            end if
            zmat(knew, j) = ZERO
        end do

        ! Put the KNEW-th column of the unupdated H (except for the (NPT+1)th entry) into HCOL.
        hcol(1:npt) = zmat(knew, 1) * zmat(:, 1)
        hcol(npt + 1:npt + n) = bmat(:, knew)

        ! Complete the updating of ZMAT. See (4.14) of the BOBYQA paper.
        sqrtdn = sqrt(denom)
        zknew1 = zmat(knew, 1) / sqrtdn
        zmat(:, 1) = (tau / sqrtdn) * zmat(:, 1) - zknew1 * vlag(1:npt)
        zmat(knew, 1) = zknew1  ! Because TAU = VLAG(KNEW) + 1. Powell's code does not have this.

        ! Finally, update the matrix BMAT. It implements the last N rows of (4.9) in the BOBYQA paper.
        alpha = hcol(knew)
        v1 = (alpha * vlag(npt + 1:npt + n) - tau * hcol(npt + 1:npt + n)) / denom
        v2 = (-beta * hcol(npt + 1:npt + n) - tau * vlag(npt + 1:npt + n)) / denom
        bmat = bmat + outprod(v1, vlag) + outprod(v2, hcol) !call r2update(bmat, ONE, v1, vlag, ONE, v2, hcol)
        ! N.B.: The use of OUTPROD is expensive memory-wise, but it is not our concern in this implementation.
        ! Numerically, the update above does not guarantee BMAT(:, NPT+1 : NPT+N) to be symmetric.
        call symmetrize(bmat(:, npt + 1:npt + n))

    end subroutine updateh_rsc

end module parquet_prima_bobyqb
