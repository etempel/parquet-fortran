!> PRIMA's COBYLA: minimisation subject to nonlinear as well as linear constraints, by linear
!> models of the objective and every constraint over a simplex of `n + 1` points.
!!
!! **Provenance.** Derived from PRIMA (Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`: `fortran/cobyla/cobylb.f90`, `geometry.f90`,
!! `initialize.f90`, `trustregion.f90` and `update.f90`, which are one module each upstream and one
!! module here (`feature_optimizer.md` 4.1's ten-file layout). The licence text and the deviations
!! shared by every file of this tier are in `parquet_prima_linalg`.
!!
!! The algorithm is M. J. D. Powell's, *A direct search optimization method that models the
!! objective and constraint functions by linear interpolation* (1994); the comments citing equation
!! numbers of "the COBYLA paper" are upstream's and refer to it. SciPy 1.16 replaced its Fortran 77
!! COBYLA with a translation of this same code.
!!
!! **A bug found here is checked against upstream before it is fixed here.** Every formula, every
!! branch and every tolerance below is upstream's.
!!
!! **Constraint values are `c(x) <= 0` where feasible** -- PRIMA's convention and this library's
!! (`feature_optimizer.md` Q16), the opposite of SciPy's. The linear constraints reach the module
!! as `amat^T x <= bvec` and are evaluated here rather than through the objective; the nonlinear
!! ones come from the caller's `pf_constrained_objective` through `evaluate_fc`, and `constr`
!! carries the two halves in that order.
!!
!! **What changed beyond the tier-wide list.**
!!
!! 1. `cobylb` and `initxfc` take the caller's objective and a `prima_state` in place of upstream's
!!    `procedure(OBJCON) :: calcfc`, and evaluate through `evaluate_fc`, which screens the value
!!    AND the constraint values and fills the record.
!! 2. `moderatec` is gone with the rest of the moderated extreme barrier: upstream clamps a
!!    non-finite or enormous constraint value and carries on, and this refuses it in `evaluate_fc`
!!    (Q7). Upstream's note that it moderates the linear constraints to match what `evaluate` did
!!    to the nonlinear ones is replaced at each of its four sites, since neither half is moderated
!!    here.
!! 3. `iprint` and every `fmsg`/`rhomsg`/`retmsg`/`cpenmsg` call are gone: this tier prints
!!    nothing.
!! 4. `xhist`/`fhist`/`chist`/`conhist` and the `savehist`/`rangehist` calls are gone with them;
!!    the record is the optional `pf_optimize_history`, appended in `evaluate_fc`.
!! 5. `callback_fcn` and `CALLBACK_TERMINATE` are not carried (Q22).
!! 6. `trstlp_sub` carries the ONE `call validate` in this tier's whole vendored subset (6.5 item
!!    3): the active-set invariant `icon > 0`, which upstream checks on every call rather than
!!    under `DEBUGGING`. It is kept, as an abort through `prima_abort` carrying this library's
!!    wording. No caller input can reach it, so it is the one abort in this tier with no error
!!    scenario.
!! 7. Upstream's driver `cobyla.f90` is NOT vendored: its argument defaults, its folding of the
!!    bounds and linear constraints into `amat`/`bvec` and its `preproc` adjustments become the
!!    validation and the driver in `parquet_prima_cobyla`, where an adjustment upstream makes with
!!    a warning is either refused or reported.
module parquet_prima_cobylb

    use, intrinsic :: iso_fortran_env, only : real64, int64
    use parquet_optimize, only : pf_constrained_objective
    use parquet_prima_common, only : prima_state, evaluate_fc, checkexit, redrat, redrho, &
        savefilt, selectx, isbetter
    use parquet_prima_linalg, only : sum, ZERO, ONE, TWO, HALF, TEN, TENTH, EPS, REALMAX, REALMIN, &
        is_nan, is_posinf, is_finite, inprod, matprod, outprod, trueloc, norm, eye, inv, &
        maximum, isminor, lsqr, linspace, int, prima_abort, &
        INFO_DFT, SMALL_TR_RADIUS, MAXTR_REACHED, DAMAGING_ROUNDING
    use parquet_prima_powalg, only : qradd, qrexc

    implicit none
    private

    public :: cobylb

contains


    !> COBYLA's main iteration.
    !!
    !! Builds a simplex of `n + 1` points, fits a linear model to the objective and to every
    !! constraint over it, and takes a trust-region step from the best vertex by solving a linear
    !! programme (`trstlp`); a geometry step replaces a vertex when the simplex degenerates. The
    !! merit function is `f + cpen*cstrv`, and `cpen` itself is raised as the run proceeds until the
    !! predicted reduction is positive.
    !!
    !! The point returned is chosen from the filter, so a run that ends infeasible still hands back
    !! the best trade-off it saw rather than its last iterate.
    subroutine cobylb(obj, st, maxfilt, maxfun, amat, bvec, ctol, cweight, eta1, eta2, ftarget, &
            & gamma1, gamma2, rhobeg, rhoend, constr, f, x, nf, cstrv, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine performs the actual calculations of COBYLA.
        !
        ! IPRINT, MAXFILT, MAXFUN, MAXHIST, CTOL, CWEIGHT, ETA1, ETA2, FTARGET, GAMMA1, GAMMA2, RHOBEG,
        ! RHOEND, X, NF, F, XHIST, FHIST, CHIST, CONHIST, CSTRV, INFO and CALLBACK are identical to the corresponding
        ! arguments in subroutine COBYLA.
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        ! Solver-specific modules

        class(pf_constrained_objective), intent(inout) :: obj    !! the objective and its constraints (this thread's own)
        type(prima_state), intent(inout) :: st                   !! scaling, wording, record and evaluation count
        integer, intent(in) :: maxfilt                           !! how many points the filter keeps
        integer, intent(in) :: maxfun                            !! the evaluation budget
        real(real64), intent(in) :: amat(:, :)                   !! the linear constraint gradients, one per column
        real(real64), intent(in) :: bvec(:)                      !! the right-hand sides of `amat`
        real(real64), intent(in) :: ctol                         !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: cweight                      !! how heavily violation weighs against value in the filter
        real(real64), intent(in) :: eta1                         !! ratio below which the radius contracts
        real(real64), intent(in) :: eta2                         !! ratio above which it expands
        real(real64), intent(in) :: ftarget                      !! stop as soon as a feasible value at or below this is found
        real(real64), intent(in) :: gamma1                       !! the contraction factor
        real(real64), intent(in) :: gamma2                       !! the expansion factor
        real(real64), intent(in) :: rhobeg                       !! the initial trust-region radius
        real(real64), intent(in) :: rhoend                       !! the final trust-region radius

        ! On entry, [X, F, CONSTR] = [X0, F(X0), CONSTR(X0)]
        real(real64), intent(inout) :: constr(:)                 !! the constraint values at the new point
        real(real64), intent(inout) :: f                         !! objective value at the new point
        real(real64), intent(inout) :: x(:)                      !! the start in, the best point out

        integer, intent(out) :: info                             !! PRIMA's exit code
        integer, intent(out) :: nf                               !! objective evaluations made
        real(real64), intent(out) :: cstrv                       !! the constraint violation at the returned point

        integer :: j
        integer :: jdrop_geo
        integer :: jdrop_tr
        integer :: kopt
        integer :: m
        integer :: m_lcon
        integer :: maxtr
        integer :: n
        integer :: nfilt
        integer :: subinfo
        integer :: tr
        logical :: bad_trstep
        logical :: adequate_geo
        logical :: evaluated(size(x) + 1)
        logical :: improve_geo
        logical :: reduce_rho
        logical :: shortd
        logical :: trfail
        logical :: ximproved
        real(real64) :: A(size(x), size(constr)) ! A contains the approximate gradient for the constraints
        real(real64) :: actrem
        real(real64) :: cfilt(min(max(maxfilt, 1), maxfun))
        real(real64) :: confilt(size(constr), size(cfilt))
        real(real64) :: conmat(size(constr), size(x) + 1)
        real(real64) :: cpen  ! Penalty parameter for constraint in merit function (PARMU in Powell's code)
        real(real64) :: cval(size(x) + 1)
        real(real64) :: d(size(x))
        real(real64) :: delbar
        real(real64) :: delta
        real(real64) :: distsq(size(x) + 1)
        real(real64) :: dnorm
        real(real64) :: ffilt(size(cfilt))
        real(real64) :: fval(size(x) + 1)
        real(real64) :: g(size(x))
        real(real64) :: gamma3
        real(real64) :: prerec  ! Predicted reduction in constraint violation
        real(real64) :: preref  ! Predicted reduction in objective Function
        real(real64) :: prerem  ! Predicted reduction in merit function
        real(real64) :: ratio  ! Reduction ratio: ACTREM/PREREM
        real(real64) :: rho
        real(real64) :: sim(size(x), size(x) + 1)
        real(real64) :: simi(size(x), size(x))
        real(real64) :: xfilt(size(x), size(cfilt))
        ! CPENMIN is the minimum of the penalty parameter CPEN for the L-infinity constraint violation in
        ! the merit function. Note that CPENMIN = 0 in Powell's implementation, which allows CPEN to be 0.
        ! Here, we take CPENMIN > 0 so that CPEN is always positive. This avoids the situation where PREREM
        ! becomes 0 when PREREF = 0 = CPEN. It brings two advantages as follows.
        ! 1. If the trust-region subproblem solver works correctly and the trust-region center is not
        ! optimal for the subproblem, then PREREM > 0 is guaranteed. This is because, in theory, PREREC >= 0
        ! and MAX(PREREC, PREREF) > 0 , and the definition of CPEN in GETCPEN ensures that PREREM > 0.
        ! 2. There is no need to revise ACTREM and PREREM when CPEN = 0 and F = FVAL(N+1) as in lines
        ! 312--314 of Powell's cobylb.f code. Powell's code revises ACTREM to CVAL(N + 1) - CSTRV and PREREM
        ! to PREREC in this case, which is crucial for feasibility problems.
        real(real64), parameter :: cpenmin = EPS

        m_lcon = int(size(bvec))
        m = int(size(constr))
        n = int(size(x))

        ! Initialize SIM, SIMI, FVAL, CONMAT, and CVAL, together with the history, NF, and EVALUATED.
        ! After the initialization, SIM(:, N+1) holds the vertex of the initial simplex with the smallest
        ! function value (regardless of the constraint violation), and SIM(:, 1:N) holds the displacements
        ! from the other vertices to SIM(:, N+1). FVAL, CONMAT, and CVAL hold the function values,
        ! constraint values, and constraint violations on the vertices in the order corresponding to SIM.
        call initxfc(obj, st, maxfun, amat, bvec, constr, ctol, f, ftarget, rhobeg, x, nf, &
            & conmat, cval, fval, sim, simi, evaluated, subinfo)

        ! Initialize the filter, including XFILT, FFILT, CONFILT, CFILT, and NFILT.
        ! N.B.: The filter is used only when selecting which iterate to return. It does not interfere with
        ! the iterations. COBYLA is NOT a filter method but a trust-region method based on an L-infinity
        ! merit function. Powell's implementation does not use a filter to select the iterate, possibly
        ! returning a suboptimal iterate.
        call initfilt(conmat, ctol, cweight, cval, fval, sim, evaluated, nfilt, cfilt, confilt, ffilt, xfilt)

        ! Check whether to return due to abnormal cases that may occur during the initialization.
        if (subinfo /= INFO_DFT) then
            info = subinfo
            ! The driver reads the radius reached and the iteration count from the state; set them on
            ! every exit, this one included, so that `info%rho` and `info%niter` are never stale.
            st%rho = rhobeg
            st%niter = 0
            ! Return the best calculated values of the variables.
            ! N.B. SELECTX and FINDPOLE choose X by different standards. One cannot replace the other.
            kopt = selectx(ffilt(1:nfilt), cfilt(1:nfilt), cweight, ctol)
            x = xfilt(:, kopt)
            f = ffilt(kopt)
            constr = confilt(:, kopt)
            cstrv = cfilt(kopt)
            ! Arrange CHIST, CONHIST, FHIST, and XHIST so that they are in the chronological order.
            ! Print a return message according to IPRINT.
            return
        end if

        ! Set some more initial values.
        ! We must initialize ACTREM and PREREM. Otherwise, when SHORTD = TRUE, compilers may raise a
        ! run-time error that they are undefined. But their values will not be used: when SHORTD = FALSE,
        ! they will be overwritten; when SHORTD = TRUE, the values are used only in BAD_TRSTEP, which is
        ! TRUE regardless of ACTREM or PREREM. Similar for PREREC, PREREF, PREREM, RATIO, and JDROP_TR.
        ! No need to initialize SHORTD unless MAXTR < 1, but some compilers may complain if we do not do it.
        ! Our initialization of CPEN differs from Powell's in two ways. First, we use the ratio defined in
        ! (13) of Powell's COBYLA paper to initialize CPEN. Second, we impose CPEN >= CPENMIN > 0. Powell's
        ! code simply initializes CPEN to 0.
        rho = rhobeg
        delta = rhobeg
        cpen = max(cpenmin, min(1.0E3_real64, fcratio(conmat, fval)))  ! Powell's code: CPEN = ZERO
        prerec = -REALMAX
        preref = -REALMAX
        prerem = -REALMAX
        actrem = -REALMAX
        shortd = .false.
        trfail = .false.
        ratio = -ONE
        jdrop_tr = 0
        jdrop_geo = 0

        ! If DELTA <= GAMMA3*RHO after an update, we set DELTA to RHO. GAMMA3 must be less than GAMMA2. The
        ! reason is as follows. Imagine a very successful step with DENORM = the un-updated DELTA = RHO.
        ! Then TRRAD will update DELTA to GAMMA2*RHO. If GAMMA3 >= GAMMA2, then DELTA will be reset to RHO,
        ! which is not reasonable as D is very successful. See paragraph two of Sec. 5.2.5 in
        ! T. M. Ragonneau's thesis: "Model-Based Derivative-Free Optimization Methods and Software".
        ! According to test on 20230613, for COBYLA, this Powellful updating scheme of DELTA works slightly
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
        ! SHORTD - Is the trust-region trial step too short to invoke a function evaluation?
        ! IMPROVE_GEO - Will we improve the model after the trust-region iteration? If yes, a geometry step
        ! will be taken, corresponding to the "Branch (Delta)" in the COBYLA paper.
        ! REDUCE_RHO - Will we reduce rho after the trust-region iteration?
        ! COBYLA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously.
        do tr = 1, maxtr
            ! Increase the penalty parameter CPEN, if needed, so that PREREM = PREREF + CPEN * PREREC > 0.
            ! This is the first (out of two) update of CPEN, where CPEN increases or remains the same.
            ! N.B.: CPEN and the merit function PHI = FVAL + CPEN*CVAL are used at three places only.
            ! 1. In FINDPOLE/UPDATEPOLE, deciding the optimal vertex of the current simplex.
            ! 2. After the trust-region trial step, calculating the reduction radio.
            ! 3. In GEOSTEP, deciding the direction of the geometry step.
            ! They do not appear explicitly in the trust-region subproblem, though the trust-region center
            ! (i.e., the current optimal vertex) is defined by them.
            cpen = getcpen(amat, bvec, conmat, cpen, cval, delta, fval, rho, sim, simi)

            ! Switch the best vertex of the current simplex to SIM(:, N + 1).
            call updatepole(cpen, conmat, cval, fval, sim, simi, subinfo)
            ! Check whether to exit due to damaging rounding in UPDATEPOLE.
            if (subinfo == DAMAGING_ROUNDING) then
                info = subinfo
                exit  ! Better action to take? Geometry step, or simply continue?
            end if

            ! Does the interpolation set have adequate geometry? It affects IMPROVE_GEO and REDUCE_RHO.
            adequate_geo = all(sum(sim(:, 1:n)**2, dim=1) <= 4.0_real64 * delta**2)

            ! Calculate the linear approximations to the objective and constraint functions.
            ! N.B.: TRSTLP accesses A mostly by columns, so it is more reasonable to save A instead of A^T.
            ! Zaikun 2023108: According to a test on 2023108, calculating G and A(:, M_LCON+1:M) by solving
            ! the linear systems SIM^T*G = FVAL(1:N)-FVAL(N+1) and SIM^T*A = CONMAT(:, 1:N)-CONMAT(:, N+1)
            ! does not seem to improve or worsen the performance of COBYLA in terms of the number of function
            ! evaluations. The system was solved by SOLVE in LINALG_MOD based on a QR factorization of SIM
            ! (not necessarily a good algorithm). No preconditioning or scaling was used.
            g = matprod(fval(1:n) - fval(n + 1), simi)
            A(:, 1:m_lcon) = amat
            A(:, m_lcon + 1:m) = transpose(matprod(conmat(m_lcon + 1:m, 1:n) &
                & - spread(conmat(m_lcon + 1:m, n + 1), dim=2, ncopies=n), simi))
            ! MATLAB: A(:, m_lcon+1:m) = simi'*(conmat(m_lcon+1:m, 1:n) - conmat(m_lcon+1:m, n+1))'
            ! with implicit expansion for the subtraction

            ! Calculate the trust-region trial step D. Note that D does NOT depend on CPEN.
            d = trstlp(A, -conmat(:, n + 1), delta, g)
            dnorm = min(delta, norm(d))

            ! Is the trust-region trial step short? Note that we compare DNORM with RHO, not DELTA.
            ! Powell's code essentially defines SHORTD by SHORTD = (DNORM < HALF * RHO). In our tests,
            ! TENTH seems to work better than HALF or QUART, especially for linearly constrained problems.
            ! Note that LINCOA has a slightly more sophisticated way of defining SHORTD, taking into account
            ! whether D causes a change to the active set. Should we try the same here?
            shortd = (dnorm <= TENTH * rho)  ! `<=` works better than `<` in case of underflow.

            ! Predict the change to F (PREREF) and to the constraint violation (PREREC) due to D.
            ! We have the following in precise arithmetic. They may fail to hold due to rounding errors.
            ! 1. PREREC is the reduction of the L-infinity violation of the linearized constraints achieved
            ! by D. It is nonnegative in theory; it is 0 iff CONMAT(1:M, N+1) <= 0, namely the trust-region
            ! center satisfies the constraints.
            ! 2. PREREF may be negative or 0, but it should be positive when PREREC = 0 and SHORTD is FALSE.
            ! 3. Due to 2, in theory, MAXIMUM([PREREC, PREREF]) > 0 if SHORTD is FALSE.
            preref = -inprod(d, g)  ! Can be negative.
            prerec = cval(n + 1) - maximum([ZERO, conmat(:, n + 1) + matprod(d, A)])

            ! Evaluate PREREM, which is the predicted reduction in the merit function.
            ! In theory, PREREM >= 0 and it is 0 iff CPEN = 0 = PREREF. This may not be true numerically.
            prerem = preref + cpen * prerec
            trfail = (.not. prerem > 1.0E-6 * min(cpen, ONE) * rho)  ! PREREM is tiny/negative or NaN.

            if (shortd .or. trfail) then
                ! Reduce DELTA if D is short or D fails to render PREREM > 0. The latter can happen due to
                ! rounding errors. This seems important for performance.
                delta = TENTH * delta
                if (delta <= gamma3 * rho) then
                    delta = rho  ! Set DELTA to RHO when it is close to or below.
                end if
            else
                ! Calculate the next value of the objective and constraint functions.
                ! If X is close to one of the points in the interpolation set, then we do not evaluate the
                ! objective and constraints at X, assuming them to have the values at the closest point.
                ! N.B.: If this happens, do NOT include X into the filter, as F and CONSTR are inaccurate.
                x = sim(:, n + 1) + d
                distsq(n + 1) = sum((x - sim(:, n + 1))**2)
                distsq(1:n) = [(sum((x - (sim(:, n + 1) + sim(:, j)))**2), j=1, n)]  ! Implied do-loop
                ! MATLAB: distsq(1:n) = sum((x - (sim(:,1:n) + sim(:, n+1)))**2, 1)  % Implicit expansion
                j = int(minloc(distsq, dim=1))
                if (distsq(j) <= (1.0E-4 * rhoend)**2) then
                    f = fval(j)
                    constr = conmat(:, j)
                    cstrv = cval(j)
                else
                    ! Evaluate the objective and constraints at X, taking care of possible Inf/NaN values.
                    constr(1:m_lcon) = matprod(x, amat) - bvec  ! Linear constraints
                    call evaluate_fc(obj, st, x, f, constr(m_lcon + 1:m))  ! Nonlinear constraints
                    ! Neither half is moderated here: a non-finite value aborts in `evaluate_fc`, so the
                    ! violation below is the true one (`feature_optimizer.md` Q7).
                    cstrv = maximum([ZERO, constr])
                    nf = nf + 1
                    ! Save X, F, CONSTR, CSTRV into the history.
                    ! Save X, F, CONSTR, CSTRV into the filter.
                    call savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt, constr, confilt)
                end if

                ! Print a message about the function/constraint evaluation according to IPRINT.

                ! Evaluate ACTREM, which is the actual reduction in the merit function.
                actrem = (fval(n + 1) + cpen * cval(n + 1)) - (f + cpen * cstrv)

                ! Calculate the reduction ratio by REDRAT, which handles Inf/NaN carefully.
                ratio = redrat(actrem, prerem, eta1)

                ! Update DELTA. After this, DELTA < DNORM may hold.
                ! N.B.: 1. Powell's code uses RHO as the trust-region radius and updates it as follows.
                ! Reduce RHO to GAMMA1*RHO if ADEQUATE_GEO is TRUE and either SHORTD is TRUE or RATIO < ETA1,
                ! and then revise RHO to RHOEND if its new value is not more than GAMMA3*RHOEND; RHO remains
                ! unchanged in all other cases; in particular, RHO is never increased.
                ! 2. Our implementation uses DELTA as the trust-region radius, while using RHO as a lower
                ! bound for DELTA. DELTA is updated in a way that is typical for trust-region methods, and
                ! it is revised to RHO if its new value is not more than GAMMA3*RHO. RHO reflects the current
                ! resolution of the algorithm; its update is essentially the same as the update of RHO in
                ! Powell's code (see the definition of REDUCE_RHO below). Our implementation aligns with
                ! UOBYQA/NEWUOA/BOBYQA/LINCOA and improves the performance of COBYLA.
                ! 3. The same as Powell's code, we do not reduce RHO unless ADEQUATE_GEO is TRUE. This is
                ! also how Powell updated RHO in UOBYQA/NEWUOA/BOBYQA/LINCOA. What about we also use
                ! ADEQUATE_GEO == TRUE as a prerequisite for reducing DELTA? The argument would be that the
                ! bad (small) value of RATIO may be because of a bad geometry (and hence a bad model) rather
                ! than an improperly large DELTA, and it might be good to try improving the geometry first
                ! without reducing DELTA. However, according to a test on 20230206, it does not improve the
                ! performance if we skip the update of DELTA when ADEQUATE_GEO is FALSE and RATIO < 0.1.
                ! Therefore, we choose to update DELTA without checking ADEQUATE_GEO.
                delta = trrad(delta, dnorm, eta1, eta2, gamma1, gamma2, ratio)
                if (delta <= gamma3 * rho) then
                    delta = rho  ! Set DELTA to RHO when it is close to or below.
                end if

                ! Is the newly generated X better than current best point?
                ximproved = (actrem > 0)  ! If ACTREM is NaN, then XIMPROVED should & will be FALSE.

                ! Set JDROP_TR to the index of the vertex to be replaced with X. JDROP_TR = 0 means there
                ! is no good point to replace, and X will not be included into the simplex; in this case,
                ! the geometry of the simplex likely needs improvement, which will be handled below.
                jdrop_tr = setdrop_tr(ximproved, d, delta, rho, sim, simi)

                ! Update SIM, SIMI, FVAL, CONMAT, and CVAL so that SIM(:, JDROP_TR) is replaced with D.
                ! UPDATEXFC does nothing if JDROP_TR == 0, as the algorithm decides to discard X.
                call updatexfc(jdrop_tr, constr, cpen, cstrv, d, f, conmat, cval, fval, sim, simi, subinfo)
                ! Check whether to exit due to damaging rounding in UPDATEXFC.
                if (subinfo == DAMAGING_ROUNDING) then
                    info = subinfo
                    exit  ! Better action to take? Geometry step, or a RESCUE as in BOBYQA?
                end if

                ! Check whether to exit due to MAXFUN, FTARGET, etc.
                subinfo = checkexit(maxfun, nf, cstrv, ctol, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo
                    exit
                end if
            end if  ! End of IF (SHORTD .OR. TRFAIL). The normal trust-region calculation ends.

            !----------------------------------------------------------------------------------------------!
            ! Before the next trust-region iteration, we possibly improve the geometry of simplex or reduce
            ! RHO according to IMPROVE_GEO and REDUCE_RHO. Now we decide these indicators.
            ! N.B.: We must ensure that the algorithm does not set IMPROVE_GEO = TRUE at infinitely many
            ! consecutive iterations without moving SIM(:, N+1) or reducing RHO. Otherwise, the algorithm
            ! will get stuck in repetitive invocations of GEOSTEP. This is ensured by the following facts.
            ! 1. If an iteration sets IMPROVE_GEO = TRUE, it must also reduce DELTA or set DELTA to RHO.
            ! 2. If SIM(:, N+1) and RHO remains unchanged, then ADEQUATE_GEO will become TRUE after at
            ! most N invocations of GEOSTEP.

            ! BAD_TRSTEP: Is the last trust-region step bad?
            bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. jdrop_tr == 0)
            ! IMPROVE_GEO: Should we take a geometry step to improve the geometry of the interpolation set?
            improve_geo = (bad_trstep .and. .not. adequate_geo)
            ! REDUCE_RHO: Should we enhance the resolution by reducing RHO?
            reduce_rho = (bad_trstep .and. adequate_geo .and. max(delta, dnorm) <= rho)

            ! COBYLA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously.
            ! !call assert(.not. (improve_geo .and. reduce_rho), 'IMPROVE_GEO or REDUCE_RHO are not both TRUE', srname)

            ! If SHORTD or TRFAIL is TRUE, then either IMPROVE_GEO or REDUCE_RHO is TRUE unless ADEQUATE_GEO
            ! is TRUE and MAX(DELTA, DNORM) > RHO.
            ! !call assert((.not. (shortd .or. trfail)) .or. (improve_geo .or. reduce_rho .or. &
            ! !    & (adequate_geo .and. max(delta, dnorm) > rho)), 'If SHORTD or TRFAIL is TRUE, then &
            ! !    & either IMPROVE_GEO or REDUCE_RHO is TRUE unless ADEQUATE_GEO is TRUE and MAX(DELTA, DNORM) > RHO', srname)
            !----------------------------------------------------------------------------------------------!

            ! Comments on BAD_TRSTEP:
            ! 1. Powell's definition of BAD_TRSTEP is as follows. The one used above seems to work better,
            ! especially for linearly constrained problems due to the factor TENTH (= ETA1).
            ! !bad_trstep = (shortd .or. actrem <= 0 .or. actrem < TENTH * prerem .or. jdrop_tr == 0)
            ! Besides, Powell did not check PREREM > 0 in BAD_TRSTEP, which is reasonable to do but has
            ! little impact upon the performance.
            ! 2. NEWUOA/BOBYQA/LINCOA would define BAD_TRSTEP, IMPROVE_GEO, and REDUCE_RHO as follows. Two
            ! different thresholds are used in BAD_TRSTEP. It outperforms Powell's version.
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= eta1 .or. jdrop_tr == 0)
            ! !improve_geo = bad_trstep .and. .not. adequate_geo
            ! !bad_trstep = (shortd .or. trfail .or. ratio <= 0 .or. jdrop_tr == 0)
            ! !reduce_rho = bad_trstep .and. adequate_geo .and. max(delta, dnorm) <= rho
            ! 3. Theoretically, JDROP_TR > 0 when ACTREM > 0 (guaranteed by RATIO > 0). However, in Powell's
            ! implementation, JDROP_TR may be 0 even RATIO > 0 due to NaN. The modernized code has rectified
            ! this in the function SETDROP_TR. After this rectification, we can indeed simplify the
            ! definition of BAD_TRSTEP by removing the condition JDROP_TR == 0. We retain it for robustness.

            ! Comments on REDUCE_RHO:
            ! When SHORTD is TRUE, UOBYQA/NEWUOA/BOBYQA/LINCOA all set REDUCE_RHO to TRUE if the recent
            ! models are sufficiently accurate according to certain criteria. See the paragraph around (37)
            ! in the UOBYQA paper and the discussions about Box 14 in the NEWUOA paper. This strategy is
            ! crucial for the performance of the solvers. However, as of 20221111, we have not managed to
            ! make it work in COBYLA. As in NEWUOA, we recorded the errors of the recent models, and set
            ! REDUCE_RHO to true if they are small (e.g., ALL(ABS(MODERR_REC) <= 0.1 * MAXVAL(ABS(A))*RHO) or
            ! ALL(ABS(MODERR_REC) <= RHO**2)) when SHORTD is TRUE. It made little impact on the performance.

            ! Since COBYLA never sets IMPROVE_GEO and REDUCE_RHO to TRUE simultaneously, the following
            ! two blocks are exchangeable: IF (IMPROVE_GEO) ... END IF and IF (REDUCE_RHO) ... END IF.

            ! Improve the geometry of the simplex by removing a point and adding a new one.
            ! If the current interpolation set has adequate geometry, then we skip the geometry step.
            ! The code has a small difference from Powell's original code here: If the current geometry
            ! is adequate, then we will continue with a new trust-region iteration; however, at the
            ! beginning of the iteration, CPEN may be updated, which may alter the pole point SIM(:, N+1)
            ! by UPDATEPOLE; the quality of the interpolation point depends on SIM(:, N + 1), meaning
            ! that the same interpolation set may have good or bad geometry with respect to different
            ! "poles"; if the geometry turns out bad with the new pole, the original COBYLA code will
            ! take a geometry step, yet our code here will NOT do it but continue to take a trust-region
            ! step. The argument is this: even if the geometry step is not skipped in the first place, the
            ! geometry may turn out bad again after the pole is altered due to an update to CPEN; should
            ! we take another geometry step in that case? If no, why should we do it here? Indeed, this
            ! distinction makes no practical difference for CUTEst problems with at most 100 variables
            ! and 5000 constraints, while the algorithm framework is simplified.
            if (improve_geo .and. .not. all(sum(sim(:, 1:n)**2, dim=1) <= 4.0_real64 * delta**2)) then
                ! Before the geometry step, UPDATEPOLE has been called either implicitly by UPDATEXFC or
                ! explicitly after CPEN is updated, so that SIM(:, N + 1) is the optimal vertex.

                ! Decide a vertex to drop from the simplex. It will be replaced with SIM(:, N + 1) + D to
                ! improve the geometry of the simplex.
                ! N.B.: 1. COBYLA never sets JDROP_GEO = N + 1.
                ! 2. The following JDROP_GEO comes from UOBYQA/NEWUOA/BOBYQA/LINCOA.
                ! 3. In Powell's original algorithm, the geometry of the simplex is considered acceptable
                ! iff the distance between any vertex and the pole is at most 2.1*DELTA, and the distance
                ! between any vertex and the opposite face of the simplex is at least 0.25*DELTA, as
                ! specified in (14) of the COBYLA paper. Correspondingly, JDROP_GEO is set to the index of
                ! the vertex with the largest distance to the pole provided that the distance is larger than
                ! 2.1*DELTA, or the vertex with the smallest distance to the opposite face of the simplex,
                ! in which case the distance must be less than 0.25*DELTA, as the current simplex does not
                ! have acceptable geometry (see (15)--(16) of the COBYLA paper). Once JDROP_GEO is set, the
                ! algorithm replaces SIM(:, JDROP_GEO) with D specified in (17) of the COBYLA paper, which
                ! is orthogonal to the face opposite to SIM(:, JDROP_GEO) and has a length of 0.5*DELTA,
                ! intending to improve the geometry of the simplex as per (14).
                ! 4. Powell's geometry-improving procedure outlined above has an intrinsic flaw: it may lead
                ! to infinite cycling, as was observed in a test on 20240320. In this test, the geometry-
                ! improving point introduced in the previous iteration was replaced with the trust-region
                ! trial point in the current iteration, which was then replaced with the same geometry-
                ! improving point in the next iteration, and so on. In this process, the simplex alternated
                ! between two configurations, neither of which had acceptable geometry. Thus RHO was never
                ! reduced, leading to infinite cycling. (N.B.: Our implementation uses DELTA as the trust
                ! region radius, with RHO being its lower bound. When the infinite cycling occurred in this
                ! test, DELTA = RHO and it could not be reduced due to the requirement that DELTA >= RHO.)
                jdrop_geo = int(maxloc(sum(sim(:, 1:n)**2, dim=1), dim=1))

                ! Calculate the geometry step D.
                delbar = HALF * delta
                d = geostep(jdrop_geo, amat, bvec, conmat, cpen, cval, delbar, fval, simi)

                ! Calculate the next value of the objective and constraint functions.
                ! If X is close to one of the points in the interpolation set, then we do not evaluate the
                ! objective and constraints at X, assuming them to have the values at the closest point.
                ! N.B.:
                ! 1. If this happens, do NOT include X into the filter, as F and CONSTR are inaccurate.
                ! 2. In precise arithmetic, the geometry improving step ensures that the distance between X
                ! and any interpolation point is at least DELBAR, yet X may be close to them due to
                ! rounding. In an experiment with single precision on 20240317, X = SIM(:, N+1) occurred.
                x = sim(:, n + 1) + d
                distsq(n + 1) = sum((x - sim(:, n + 1))**2)
                distsq(1:n) = [(sum((x - (sim(:, n + 1) + sim(:, j)))**2), j=1, n)]  ! Implied do-loop
                ! MATLAB: distsq(1:n) = sum((x - (sim(:,1:n) + sim(:, n+1)))**2, 1)  % Implicit expansion
                j = int(minloc(distsq, dim=1))
                if (distsq(j) <= (1.0E-4 * rhoend)**2) then
                    f = fval(j)
                    constr = conmat(:, j)
                    cstrv = cval(j)
                else
                    ! Evaluate the objective and constraints at X, taking care of possible Inf/NaN values.
                    constr(1:m_lcon) = matprod(x, amat) - bvec  ! Linear constraints
                    call evaluate_fc(obj, st, x, f, constr(m_lcon + 1:m))  ! Nonlinear constraints
                    ! Neither half is moderated here: a non-finite value aborts in `evaluate_fc`, so the
                    ! violation below is the true one (`feature_optimizer.md` Q7).
                    cstrv = maximum([ZERO, constr])
                    nf = nf + 1
                    ! Save X, F, CONSTR, CSTRV into the history.
                    ! Save X, F, CONSTR, CSTRV into the filter.
                    call savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt, constr, confilt)
                end if

                ! Print a message about the function/constraint evaluation according to IPRINT.
                ! Update SIM, SIMI, FVAL, CONMAT, and CVAL so that SIM(:, JDROP_GEO) is replaced with D.
                call updatexfc(jdrop_geo, constr, cpen, cstrv, d, f, conmat, cval, fval, sim, simi, subinfo)
                ! Check whether to exit due to damaging rounding in UPDATEXFC.
                if (subinfo == DAMAGING_ROUNDING) then
                    info = subinfo
                    exit  ! Better action to take? Geometry step, or simply continue?
                end if

                ! Check whether to exit due to MAXFUN, FTARGET, etc.
                subinfo = checkexit(maxfun, nf, cstrv, ctol, f, ftarget, x)
                if (subinfo /= INFO_DFT) then
                    info = subinfo
                    exit
                end if
            end if  ! End of IF (IMPROVE_GEO). The procedure of improving geometry ends.

            ! The calculations with the current RHO are complete. Enhance the resolution of the algorithm
            ! by reducing RHO; update DELTA and CPEN at the same time.
            if (reduce_rho) then
                if (rho <= rhoend) then
                    info = SMALL_TR_RADIUS
                    exit
                end if
                delta = max(HALF * rho, redrho(rho, rhoend))
                rho = redrho(rho, rhoend)
                ! The second (out of two) update of CPEN, where CPEN decreases or remains the same.
                ! Powell's code: CPEN = MIN(CPEN, FCRATIO(FVAL, CONMAT)), which may set CPEN to 0.
                cpen = max(cpenmin, min(cpen, fcratio(conmat, fval)))
                ! Print a message about the reduction of RHO according to IPRINT.
                ! Switch the best vertex of the current simplex to SIM(:, N + 1).
                call updatepole(cpen, conmat, cval, fval, sim, simi, subinfo)
                ! Check whether to exit due to damaging rounding in UPDATEPOLE.
                if (subinfo == DAMAGING_ROUNDING) then
                    info = subinfo
                    exit  ! Better action to take? Geometry step, or simply continue?
                end if
            end if  ! End of IF (REDUCE_RHO). The procedure of reducing RHO ends.

        end do  ! End of DO TR = 1, MAXTR. The iterative procedure ends.

        ! The radius reached and the iterations performed, for `info%rho` and `info%niter`. TR is MAXTR+1
        ! when the loop ran to completion.
        st%rho = rho
        st%niter = min(tr, maxtr)

        ! Return from the calculation, after trying the last trust-region step if it has not been tried yet.
        ! Ensure that D has not been updated after SHORTD == TRUE occurred, or the code below is incorrect.
        x = sim(:, n + 1) + d
        if (info == SMALL_TR_RADIUS .and. shortd .and. norm(x - sim(:, n + 1)) > 1.0E-3_real64 * rhoend .and. nf < maxfun) then
            constr(1:m_lcon) = matprod(x, amat) - bvec  ! Linear constraints
            call evaluate_fc(obj, st, x, f, constr(m_lcon + 1:m))  ! Nonlinear constraints
            ! Neither half is moderated here: a non-finite value aborts in `evaluate_fc`, so the
            ! violation below is the true one (`feature_optimizer.md` Q7).
            cstrv = maximum([ZERO, constr])
            nf = nf + 1
            ! Save X, F, CONSTR, CSTRV into the history.
            ! Save X, F, CONSTR, CSTRV into the filter.
            call savefilt(cstrv, ctol, cweight, f, x, nfilt, cfilt, ffilt, xfilt, constr, confilt)
            ! Print a message about the function/constraint evaluation according to IPRINT.
            ! Zaikun 20230512: DELTA has been updated. RHO is only indicative here. TO BE IMPROVED.
        end if

        ! Return the best calculated values of the variables.
        ! N.B. SELECTX and FINDPOLE choose X by different standards. One cannot replace the other.
        kopt = selectx(ffilt(1:nfilt), cfilt(1:nfilt), max(cpen, cweight), ctol)
        x = xfilt(:, kopt)
        f = ffilt(kopt)
        constr = confilt(:, kopt)
        cstrv = cfilt(kopt)

        ! Arrange CHIST, CONHIST, FHIST, and XHIST so that they are in the chronological order.

        ! Print a return message according to IPRINT.

    end subroutine cobylb

    !> Builds the initial simplex and evaluates the objective and the constraints on it, upstream's
    !> `initxfc`.
    !!
    !! The simplex is `x0` and its `n` coordinate displacements by `rhobeg`, with a displacement
    !! reversed where that keeps the vertex on the better side. `sim` holds the displacements and
    !! the best vertex; `simi` is the inverse of the displacement block.
    subroutine initxfc(obj, st, maxfun, amat, bvec, constr0, ctol, f0, ftarget, rhobeg, x0, nf, &
            & conmat, cval, fval, sim, simi, evaluated, info)
        !--------------------------------------------------------------------------------------------------!
        ! This subroutine does the initialization concerning X, function values, and constraints.
        !--------------------------------------------------------------------------------------------------!

        ! Common modules

        class(pf_constrained_objective), intent(inout) :: obj    !! the objective and its constraints (this thread's own)
        type(prima_state), intent(inout) :: st                   !! scaling, wording, record and evaluation count
        integer, intent(in) :: maxfun                            !! the evaluation budget
        real(real64), intent(in) :: amat(:, :)                   !! the linear constraint gradients, one per column
        real(real64), intent(in) :: bvec(:)                      !! the right-hand sides of `amat`
        real(real64), intent(in) :: constr0(:)                   !! the constraint values at the start point
        real(real64), intent(in) :: ctol                         !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: f0                           !! the objective value at the start point
        real(real64), intent(in) :: ftarget                      !! stop as soon as a feasible value at or below this is found
        real(real64), intent(in) :: rhobeg                       !! the initial trust-region radius
        real(real64), intent(in) :: x0(:)                        !! the start point

        integer, intent(out) :: info                             !! PRIMA's exit code
        integer, intent(out) :: nf                               !! objective evaluations made
        logical, intent(out) :: evaluated(:)                     !! which simplex vertices have a value
        real(real64), intent(out) :: conmat(:, :)  !! the constraint values at each simplex vertex, one vertex per column
        real(real64), intent(out) :: cval(:)                     !! the constraint violation at each simplex vertex
        real(real64), intent(out) :: fval(:)                     !! objective value at each simplex vertex
        real(real64), intent(out) :: sim(:, :)  !! the simplex: `n` displacement vectors, then the best vertex itself
        real(real64), intent(out) :: simi(:, :)                  !! the inverse of `sim(:, 1:n)`

        integer :: j
        integer :: k
        integer :: m
        integer :: m_lcon
        integer :: n
        integer :: subinfo
        real(real64) :: constr(size(conmat, 1))
        real(real64) :: cstrv
        real(real64) :: f
        real(real64) :: x(size(x0))

        m_lcon = int(size(bvec))
        m = int(size(conmat, 1))
        n = int(size(sim, 1))

        ! Initialize INFO to the default value. At return, an INFO different from this value will indicate
        ! an abnormal return.
        info = INFO_DFT

        ! Initialize the simplex. It will be revised during the initialization.
        sim = eye(n, n + 1) * rhobeg
        sim(:, n + 1) = x0

        ! Initialize the matrix SIMI. This initial value will be discarded at the end of the initialization.
        ! If we do not do this, compilers may complain if we return due to CHECKEXIT before SIMI is set.
        simi = eye(n) / rhobeg

        ! EVALUATED(J) = TRUE iff the function/constraint of SIM(:, J) has been evaluated.
        evaluated = .false.

        ! Initialize XHIST, FHIST, CHIST, CONHIST, FVAL, CVAL, and CONMAT. Otherwise, compilers may complain
        !that they are not (completely) initialized if the initialization aborts due to abnormality (see
        !CHECKEXIT).
        ! N.B.: 1. Initializing them to NaN would be more reasonable (NaN is not available in Fortran).
        ! 2. Do not initialize the models if the current initialization aborts due to abnormality. Otherwise,
        ! errors or exceptions may occur, as FVAL and XPT etc are uninitialized.
        fval = REALMAX
        cval = REALMAX
        conmat = REALMAX

        do k = 1, n + 1
            x = sim(:, n + 1)
            ! We will evaluate F corresponding to SIM(:, J).
            if (k == 1) then
                j = n + 1
                f = f0
                constr = constr0
            else
                j = k - 1
                x(j) = x(j) + rhobeg
                constr(1:m_lcon) = matprod(x, amat) - bvec  ! Linear constraints.
                call evaluate_fc(obj, st, x, f, constr(m_lcon + 1:m))  ! Nonlinear constraints.
                ! Neither half is moderated here: a non-finite value aborts in `evaluate_fc`, so the
                ! violation below is the true one (`feature_optimizer.md` Q7).
            end if
            cstrv = maximum([ZERO, constr])

            ! Print a message about the function/constraint evaluation according to IPRINT.
            ! Save X, F, CONSTR, CSTRV into the history.

            ! Save F, CONSTR, and CSTRV to FVAL, CONMAT, and CVAL respectively. This must be done before
            ! checking whether to exit. If exit, FVAL, CONMAT, and CVAL will define FFILT, CONFILT, and
            ! CFILT, which will define the returned X, F, CONSTR, and CSTRV.
            evaluated(j) = .true.
            fval(j) = f
            conmat(:, j) = constr
            cval(j) = cstrv

            ! Check whether to exit.
            subinfo = checkexit(maxfun, k, cstrv, ctol, f, ftarget, x)
            if (subinfo /= INFO_DFT) then
                info = subinfo
                exit
            end if

            ! Exchange the new vertex of the initial simplex with the optimal vertex if necessary.
            ! This is the ONLY part that is essentially non-parallel.
            if (j <= n .and. fval(j) < fval(n + 1)) then
                fval([j, n + 1]) = fval([n + 1, j])
                cval([j, n + 1]) = cval([n + 1, j])
                conmat(:, [j, n + 1]) = conmat(:, [n + 1, j])
                sim(:, n + 1) = x
                sim(j, 1:j) = -rhobeg  ! SIM(:, 1:N) is lower triangular.
            end if
        end do

        nf = int(count(evaluated))

        if (all(evaluated)) then
            ! Initialize SIMI to the inverse of SIM(:, 1:N).
            simi = inv(sim(:, 1:n))
        end if

    end subroutine initxfc

    !> Seeds the filter from the initial simplex, upstream's `initfilt`.
    !!
    !! Separate from `initxfc` for upstream's two reasons: the history may not exist to seed from,
    !! and the evaluation loop is then easier to parallelise.
    subroutine initfilt(conmat, ctol, cweight, cval, fval, sim, evaluated, nfilt, cfilt, confilt, ffilt, xfilt)
        ! Common modules

        real(real64), intent(in) :: conmat(:, :)        !! the constraint values at each simplex vertex, one vertex per column
        real(real64), intent(in) :: ctol                !! the violation at or below which a point counts as feasible
        real(real64), intent(in) :: cweight             !! how heavily violation weighs against value in the filter
        real(real64), intent(in) :: cval(:)             !! the constraint violation at each simplex vertex
        real(real64), intent(in) :: fval(:)             !! objective value at each simplex vertex
        real(real64), intent(in) :: sim(:, :)           !! the simplex: `n` displacement vectors, then the best vertex itself
        logical, intent(in) :: evaluated(:)             !! which simplex vertices have a value

        integer, intent(inout) :: nfilt                 !! how many pairs the filter holds
        real(real64), intent(inout) :: cfilt(:)         !! the filter's violations
        real(real64), intent(inout) :: confilt(:, :)    !! the filter's constraint values
        real(real64), intent(inout) :: ffilt(:)         !! the filter's values
        real(real64), intent(inout) :: xfilt(:, :)      !! the filter's points, one per column

        integer :: i
        integer :: m
        integer :: maxfilt
        integer :: n
        real(real64) :: x(size(sim, 1))

        m = int(size(conmat, 1))
        n = int(size(sim, 1))
        maxfilt = int(size(ffilt))

        nfilt = 0
        do i = 1, n + 1
            if (evaluated(i)) then
                if (i <= n) then
                    x = sim(:, i) + sim(:, n + 1)
                else
                    x = sim(:, i)  ! I == N+1
                end if
                call savefilt(cval(i), ctol, cweight, fval(i), x, nfilt, cfilt, ffilt, xfilt, conmat(:, i), confilt)
            end if
        end do

    end subroutine initfilt

    !> The trust-region step, by linear programming; upstream's `trstlp`.
    !!
    !! Two stages. The first finds the shortest step of length at most `delta` that minimises the
    !! greatest violation of `A^T d <= b`; if that step is strictly shorter than `delta`, the second
    !! uses the remaining freedom to minimise `g^T d` without increasing any violation.
    !!
    !! A degeneracy can prevent `d` from reaching `delta`, which is rare and not an error.
    function trstlp(A, b, delta, g) result(d)
        ! Common modules

        real(real64), intent(in) :: A(:, :)    !! the constraint gradients, one per column
        real(real64), intent(in) :: b(:)       !! the right-hand sides
        real(real64), intent(in) :: delta      !! the trust-region radius
        real(real64), intent(in) :: g(:)       !! the linear model's gradient

        real(real64) :: d(size(A, 1))          !! the step

        integer :: i
        integer :: iact(size(b) + 1)
        integer :: m
        integer :: n
        integer :: nact
        real(real64) :: A_aug(size(A, 1), size(A, 2) + 1)
        real(real64) :: b_aug(size(b) + 1)
        real(real64) :: modscal
        real(real64) :: vmultc(size(b) + 1)
        real(real64) :: z(size(d), size(d))

        m = int(size(A, 2))
        n = int(size(A, 1))

        ! Form A_aug and B_aug. This allows the gradient of the objective function to be regarded as the
        ! gradient of a constraint in the second stage.
        A_aug = reshape([A, g], [n, m + 1])  ! MATLAB: A_aug = [A, g];
        b_aug = [b, ZERO]  ! MATLAB: b_aug = [b; 0];

        ! Scale the problem if A_aug contains large values. Otherwise, floating point exceptions may occur.
        ! Note that the trust-region step is scale invariant.
        ! N.B.: It is faster and safer to scale by multiplying a reciprocal than by division. See
        ! https://fortran-lang.discourse.group/t/ifort-ifort-2021-8-0-1-0e-37-1-0e-38-0/
        do i = 1, m + 1  ! Note that SIZE(A, 2) = SIZE(B) = M + 1 /= M.
            if (maxval(abs(A_aug(:, i))) > 1.0E12) then
                modscal = max(TWO * REALMIN, ONE / maxval(abs(A_aug(:, i)))) ! MAX: avoid underflow.
                A_aug(:, i) = A_aug(:, i) * modscal
                b_aug(i) = b_aug(i) * modscal
            end if
        end do

        ! Stage 1: minimize the l_infinity constraint violation of the linearized constraints.
        call trstlp_sub(iact(1:m), nact, 1, A_aug(:, 1:m), b_aug(1:m), delta, d, vmultc(1:m), z)

        ! Stage 2: minimize the linearized objective without increasing the l_infinity constraint violation.
        call trstlp_sub(iact, nact, 2, A_aug, b_aug, delta, d, vmultc, z)

    end function trstlp

    !> One stage of `trstlp`, upstream's `trstlp_sub`.
    !!
    !! The two stages differ in five places, which upstream lists: what is initialised (stage 2
    !! inherits from stage 1), whether the violation is updated or held, how `sdirn` is defined,
    !! what `optnew` measures, and the cap on `step`.
    subroutine trstlp_sub(iact, nact, stage, A, b, delta, d, vmultc, z)
        ! Common modules

        integer, intent(in) :: stage                !! 1 or 2
        real(real64), intent(in) :: A(:, :)         !! the constraint gradients, one per column
        real(real64), intent(in) :: b(:)            !! the right-hand sides
        real(real64), intent(in) :: delta           !! the trust-region radius

        integer, intent(inout) :: iact(:)           !! indices of the active constraints
        integer, intent(inout) :: nact              !! how many are active
        real(real64), intent(inout) :: d(:)         !! the step, updated in place
        real(real64), intent(inout) :: vmultc(:)    !! the Lagrange multipliers of the constraints
        real(real64), intent(inout) :: z(:, :)      !! the orthogonal factor of the active gradients

        integer :: icon
        integer :: iter
        integer :: k
        integer :: m
        integer :: maxiter
        integer :: mcon
        integer :: n
        integer :: nactold
        integer :: nactsav
        integer :: nfail
        real(real64) :: cviol
        !real(real64) :: cvold
        real(real64) :: cvsabs(size(b))
        real(real64) :: cvshift(size(b))
        real(real64) :: dd
        real(real64) :: dnew(size(d))
        real(real64) :: dold(size(d))
        real(real64) :: frac
        real(real64) :: fracmult(size(vmultc))
        real(real64) :: optnew
        real(real64) :: optold
        real(real64) :: sd
        real(real64) :: sdirn(size(d))
        real(real64) :: sqrtd
        real(real64) :: ss
        real(real64) :: step
        real(real64) :: vmultd(size(vmultc))
        real(real64) :: zdasav(size(z, 2))
        real(real64) :: zdota(size(z, 2))

        mcon = int(size(A, 2))
        n = int(size(A, 1))

        ! Initialization according to STAGE.
        if (stage == 1) then
            iact = linspace(1, mcon, mcon)  ! MATLAB: iact = (1:mcon);  % Row vector
            ! N.B.: 1. The MATLAB version of LINSPACE returns a row vector. Take a transpose if needed.
            ! 2. In MATLAB, linspace(1, mcon, mcon) can also be written as (1:mcon).
            nact = 0
            d = ZERO
            cviol = maximum([ZERO, -b])
            vmultc = cviol + b
            z = eye(n)
            if (mcon == 0 .or. cviol <= 0) then
                ! Check whether a quick return is possible. Make sure the In-outputs have been initialized.
                return
            end if

            if (all(is_nan(b))) then
                return
            else
                icon = int(maxloc(-b, mask=(.not. is_nan(b)), dim=1))
                ! MATLAB: [~, icon] = max(b, [], 'omitnan');
            end if
            m = mcon
            sdirn = ZERO
        else
            if (inprod(d, d) >= delta**2) then
                ! Check whether a quick return is possible.
                return
            end if

            iact(mcon) = mcon
            vmultc(mcon) = ZERO
            m = mcon - 1
            icon = mcon

            ! In Powell's code, stage 2 uses the ZDOTA and CVIOL calculated by stage 1. Here we re-calculate
            ! them so that they need not be passed from stage 1 to 2, and hence the coupling is reduced.
            cviol = maximum([ZERO, matprod(d, A(:, 1:m)) - b(1:m)])
        end if
        zdota(1:nact) = [(inprod(z(:, k), A(:, iact(k))), k=1, nact)]
        ! MATLAB: zdota(1:nact) = sum(z(:, 1:nact) .* A(:, iact(1:nact)), 1);  % Row vector

        ! More initialization.
        optold = REALMAX
        nactold = nact
        nfail = 0

        !----------------------------------------------------------------------------------------------!
        ! Zaikun 20211011: VMULTD is computed from scratch at each iteration, but VMULTC is inherited.
        !----------------------------------------------------------------------------------------------!

        ! Powell's code can encounter infinite cycling, which did happen when testing the following CUTEst
        ! problems: DANWOODLS, GAUSS1LS, GAUSS2LS, GAUSS3LS, KOEBHELB, TAX13322, TAXR13322. Indeed, in all
        ! these cases, Inf/NaN appear in D due to extremely large values in A (up to 10^219). To resolve
        ! this, we set the maximal number of iterations to MAXITER, and terminate if Inf/NaN occurs in D.
        ! The formulation of MAXITER below contains a precaution against overflow. In MATLAB/Python/Julia/R,
        ! we can write maxiter = min(10000, 100*max(m, n))
        maxiter = int(min(10**min(4, range(0)), 100 * int(max(m, n))))
        do iter = 1, maxiter
            if (stage == 1) then
                optnew = cviol
            else
                optnew = inprod(d, A(:, mcon))
            end if

            ! End the current stage of the calculation if 3 consecutive iterations have either failed to
            ! reduce the best calculated value of the objective function or to increase the number of active
            ! constraints since the best value was calculated. This strategy prevents cycling, but there is
            ! a remote possibility that it will cause premature termination.
            if (optnew < optold .or. nact > nactold) then
                nactold = nact
                nfail = 0
            else
                nfail = nfail + 1
            end if
            optold = min(optold, optnew)
            if (nfail == 3) then
                exit
            end if

            ! If ICON exceeds NACT, then we add the constraint with index IACT(ICON) to the active set.
            if (icon > nact) then
                zdasav(1:nact) = zdota(1:nact)
                nactsav = nact
                call qradd(A(:, iact(icon)), z, zdota, nact)  ! QRADD may update NACT to NACT + 1.
                ! Indeed, it suffices to pass ZDOTA(1:MIN(N, NACT+1)) to QRADD as follows.
                ! !call qradd(A(:, iact(icon)), z, zdota(1:min(n, nact + 1)), nact)

                if (nact == nactsav + 1) then
                    ! N.B.: It is problematic to index arrays using [NACT, ICON] when NACT == ICON.
                    ! Zaikun 20211012: Why should VMULTC(NACT) = 0?
                    if (nact /= icon) then
                        vmultc([icon, nact]) = [vmultc(nact), ZERO]
                        iact([icon, nact]) = iact([nact, icon])
                    else
                        vmultc(nact) = ZERO
                    end if
                else
                    ! Zaikun 20211011:
                    ! 1. VMULTD is calculated from scratch for the first time (out of 2) in one iteration.
                    ! 2. NOTE that IACT has not been updated to replace IACT(NACT) with IACT(ICON). Thus
                    ! A(:, IACT(1:NACT)) is the UNUPDATED version before QRADD (Z(:, 1:NACT) remains the
                    ! same before and after QRADD). Therefore, if we supply ZDOTA to LSQR (as Rdiag) as
                    ! Powell did, we should use the UNUPDATED version, namely ZDASAV.
                    vmultd(1:nact) = lsqr(A(:, iact(1:nact)), A(:, iact(icon)), z(:, 1:nact), zdasav(1:nact))
                    if (.not. any(vmultd(1:nact) > 0 .and. iact(1:nact) <= m)) then
                        ! N.B.: This can be triggered by NACT == 0 (among other possibilities)! This is
                        ! important, because NACT will be used as an index in the sequel.
                        exit
                    end if
                    ! VMULTD(NACT+1:MCON) is not used, but we have to initialize it in Fortran, or compilers
                    ! complain about the WHERE construct below (another solution: restrict WHERE to 1:NACT).
                    vmultd(nact + 1:mcon) = -ONE  ! SIZE(VMULTD) = MCON

                    ! Revise the Lagrange multipliers. The revision is not applicable to VMULTC(NACT + 1:M).
                    fracmult = REALMAX
                    where (vmultd > 0 .and. iact <= m) fracmult = vmultc / vmultd
                    ! MATLAB: mask = (vmultd > 0 & iact <= m); fracmult(mask) = vmultc(mask) / vmultd(mask);
                    ! Only the places with VMULTD > 0 and IACT <= M is relevant blow, if any.
                    frac = minval(fracmult(1:nact))  ! FRACMULT(NACT+1:MCON) may contain garbage.
                    vmultc(1:nact) = max(ZERO, vmultc(1:nact) - frac * vmultd(1:nact))

                    ! Reorder the active constraints so that the one to be replaced is at the end of the list.
                    ! Exit if the new value of ZDOTA(NACT) is not acceptable. Powell's condition for the
                    ! following IF: .NOT. ABS(ZDOTA(NACT)) > 0. Note that it is different from
                    ! 'ABS(ZDOTA(NACT) <= 0)', as ZDOTA(NACT) can be NaN.
                    ! N.B.: We cannot arrive here with NACT == 0, which should have triggered an exit above.
                    if (is_nan(zdota(nact)) .or. abs(zdota(nact)) <= EPS**2) then
                        exit
                    end if
                    vmultc([icon, nact]) = [ZERO, frac]  ! VMULTC([ICON, NACT]) is valid as ICON > NACT.
                    iact([icon, nact]) = iact([nact, icon])
                end if

                ! In stage 2, ensure that the objective continues to be treated as the last active constraint.
                ! Zaikun 20211011, 20211111: Is it guaranteed for stage 2 that IACT(NACT-1) = MCON when
                ! IACT(NACT) /= MCON??? If not, then how does the following procedure ensure that MCON is
                ! the last of IACT(1:NACT)?
                if (stage == 2 .and. iact(nact) /= mcon) then
                    if (nact <= 1) then
                        ! We must exit, as NACT-1 is used as an index below. Powell's code does not have this.
                        exit
                    end if
                    call qrexc(A(:, iact(1:nact)), z, zdota(1:nact), nact - 1)
                    ! Indeed, it suffices to pass Z(:, 1:NACT) to QREXC as follows.
                    ! !call qrexc(A(:, iact(1:nact)), z(:, 1:nact), zdota(1:nact), nact - 1)
                    iact([nact - 1, nact]) = iact([nact, nact - 1])
                    vmultc([nact - 1, nact]) = vmultc([nact, nact - 1])
                end if
                ! Zaikun 20211117: It turns out that the last few lines do not guarantee IACT(NACT) == N in
                ! stage 2; the following test cannot be passed. IS THIS A BUG?!
                ! !call assert(iact(nact) == mcon .or. stage == 1, 'IACT(NACT) == MCON in stage 2', srname)

                ! Powell's code does not have the following. It avoids subsequent floating point exceptions.
                !------------------------------------------------------------------------------------------!
                if (is_nan(zdota(nact)) .or. abs(zdota(nact)) <= EPS**2) then
                    exit
                end if
                !------------------------------------------------------------------------------------------!

                ! Set SDIRN to the direction of the next change to the current vector of variables.
                ! Usually during stage 1 the vector SDIRN gives a search direction that reduces all the
                ! active constraint violations by one simultaneously.
                if (stage == 1) then
                    sdirn = sdirn - ((inprod(sdirn, A(:, iact(nact))) + ONE) / zdota(nact)) * z(:, nact)
                else
                    sdirn = -(ONE / zdota(nact)) * z(:, nact)
                    ! SDIRN = Z(:, NACT)/(A(:,IACT(NACT))^T*Z(:, NACT))
                    ! SDIRN^T*A(:, IACT(NACT)) = 1, SDIRN is orthogonal to A(:, IACT(1:NACT-1)) and is
                    ! parallel to Z(:, NACT).
                end if
            else  ! ICON <= NACT
                ! Delete the constraint with the index IACT(ICON) from the active set, which is done by
                ! reordering IACT(ICONT:NACT) into [IACT(ICON+1:NACT), IACT(ICON)] by pairwise exchanges
                ! and then reduce NACT to NACT - 1. In theory, ICON > 0.
                if (.not. (icon > 0)) then
                    ! Upstream's one `call validate` in this subset, kept as 6.5 item 3 asks. In theory
                    ! ICON > 0 always; upstream checks it on every call rather than under DEBUGGING, and
                    ! so does this. No caller input can reach it, so it has no error scenario -- it is
                    ! an invariant of the active-set bookkeeping, and reaching it means a defect here.
                    call prima_abort('pf_minimize_cobyla', &
                        'the trust-region subproblem lost its active set (internal invariant icon > 0)')
                end if
                call qrexc(A(:, iact(1:nact)), z, zdota(1:nact), icon)  ! QREXC does nothing if ICON==NACT.
                ! Indeed, it suffices to pass Z(:, 1:NACT) to QREXC as follows.
                ! !call qrexc(A(:, iact(1:nact)), z(:, 1:nact), zdota(1:nact), icon)
                iact(icon:nact) = [iact(icon + 1:nact), iact(icon)]
                vmultc(icon:nact) = [vmultc(icon + 1:nact), vmultc(icon)]
                nact = nact - 1

                ! Powell's code does not have the following. It avoids subsequent exceptions.
                !------------------------------------------------------------------------------------------!
                ! Zaikun 20221212: In theory, NACT > 0 in stage 2, as the objective function should always
                ! be considered as an "active constraint" --- more precisely, IACT(NACT) = MCON. However,
                ! looking at the code, I cannot see why in stage 2 NACT must be positive after the reduction
                ! above. It did happen in stage 1 that NACT became 0 after the reduction --- this is
                ! extremely rare, and it was never observed until 20221212, after almost one year of
                ! random tests. Maybe NACT is theoretically positive even in stage 1?
                if (stage == 2 .and. nact <= 0) then
                    exit  ! If this case ever occurs, we have to exit, as NACT is used as an index below.
                end if
                if (nact > 0) then
                    if (is_nan(zdota(nact)) .or. abs(zdota(nact)) <= EPS**2) then
                        exit
                    end if
                end if
                !------------------------------------------------------------------------------------------!

                ! Set SDIRN to the direction of the next change to the current vector of variables.
                if (stage == 1) then
                    sdirn = sdirn - inprod(sdirn, z(:, nact + 1)) * z(:, nact + 1)
                    ! SDIRN is orthogonal to Z(:, NACT+1)
                else
                    sdirn = -(ONE / zdota(nact)) * z(:, nact)
                    ! SDIRN = Z(:, NACT)/(A(:,IACT(NACT))^T*Z(:, NACT))
                    ! SDIRN^T*A(:, IACT(NACT)) = 1, SDIRN is orthogonal to A(:, IACT(1:NACT-1)) and is
                    ! parallel to Z(:, NACT).
                end if
            end if

            ! Calculate the step to the trust region boundary or take the step that reduces CVIOL to 0.
            !----------------------------------------------------------------------------------------------!
            ! The following calculation of STEP is adopted from NEWUOA/BOBYQA/LINCOA. It seems to improve
            ! the performance of COBYLA. We also found that removing the precaution about underflows is
            ! beneficial to the overall performance of COBYLA --- the underflows are harmless anyway.
            dd = delta**2 - inprod(d, d)
            ss = inprod(sdirn, sdirn)
            sd = inprod(sdirn, d)
            if (dd <= 0 .or. ss <= EPS * delta**2 .or. is_nan(sd)) then
                exit
            end if
            ! SQRTD: square root of a discriminant. The MAXVAL avoids SQRTD < ABS(SD) due to underflow.
            sqrtd = maxval([sqrt(ss * dd + sd**2), abs(sd), sqrt(ss * dd)])
            if (sd > 0) then
                step = dd / (sqrtd + sd)
            else
                step = (sqrtd - sd) / ss
            end if
            ! STEP < 0 should not happen. STEP can be 0 or NaN when, e.g., SD or SS becomes Inf.
            if (step <= 0 .or. .not. is_finite(step)) then
                exit
            end if
            ! Powell's approach and comments are as follows.
            !----------------------------------------------------------------!
            ! The two statements below that include the factor EPS prevent
            ! some harmless underflows that occurred in a test calculation
            ! (Zaikun: here, EPS is the machine epsilon; Powell's original
            ! code used 1.0E-6, and Powell's code was written in SINGLE
            ! PRECISION). Further, we skip the step if it could be zero within
            ! a reasonable tolerance for computer rounding errors.
            !
            ! !dd = delta**2 - sum(d**2, mask=(abs(d) >= EPS * delta))
            ! !ss = inprod(sdirn, sdirn)
            ! !if (dd  <= 0) then
            ! !    exit
            ! !end if
            ! !sd = inprod(sdirn, d)
            ! !if (abs(sd) >= EPS * sqrt(ss * dd)) then
            ! !    step = dd / (sqrt(ss * dd + sd**2) + sd)
            ! !else
            ! !    step = dd / (sqrt(ss * dd) + sd)
            ! !end if
            !----------------------------------------------------------------!
            !----------------------------------------------------------------------------------------------!

            if (stage == 1) then
                if (isminor(cviol, step)) then
                    exit
                end if
                step = min(step, cviol)
            end if

            ! Set DNEW to the new variables if STEP is the steplength, and reduce CVIOL to the corresponding
            ! maximum residual if stage 1 is being done.
            dnew = d + step * sdirn
            if (stage == 1) then
                !cvold = cviol
                cviol = maximum([ZERO, matprod(dnew, A(:, iact(1:nact))) - b(iact(1:nact))])
                ! N.B.: CVIOL will be used when calculating VMULTD(NACT+1 : MCON).
            end if

            ! Zaikun 20211011:
            ! 1. VMULTD is computed from scratch for the second (out of 2) time in one iteration.
            ! 2. VMULTD(1:NACT) and VMULTD(NACT+1:MCON) are calculated separately with no coupling.
            ! 3. VMULTD will be calculated from scratch again in the next iteration.
            ! Set VMULTD to the VMULTC vector that would occur if D became DNEW. A device is included to
            ! force VMULTD(K)=ZERO if deviations from this value can be attributed to computer rounding
            ! errors. First calculate the new Lagrange multipliers.
            vmultd(1:nact) = -lsqr(A(:, iact(1:nact)), dnew, z(:, 1:nact), zdota(1:nact))
            if (stage == 2) then
                vmultd(nact) = max(ZERO, vmultd(nact))  ! This seems never activated.
            end if
            ! Complete VMULTD by finding the new constraint residuals. (Powell wrote "Complete VMULTC ...")
            cvshift = cviol - (matprod(dnew, A(:, iact)) - b(iact))  ! Only CVSHIFT(nact+1:mcon) is needed.
            cvsabs = matprod(abs(dnew), abs(A(:, iact))) + abs(b(iact)) + cviol
            cvshift(trueloc(isminor(cvshift, cvsabs))) = ZERO
            ! MATLAB: cvshift(isminor(cvshift, cvsabs)) = 0;
            vmultd(nact + 1:mcon) = cvshift(nact + 1:mcon)

            ! Calculate the fraction of the step from D to DNEW that will be taken.
            fracmult = REALMAX
            where (vmultd < 0) fracmult = vmultc / (vmultc - vmultd)
            ! MATLAB: mask = (vmultd < 0); fracmult(mask) = vmultc(mask) / (vmultc(mask) - vmultd(mask));
            ! Only the places with VMULTD < 0 is relevant below, if any.
            icon = int(minloc([ONE, fracmult], dim=1) - 1)
            frac = minval([ONE, fracmult])
            ! MATLAB: [frac, icon] = min([1, fracmult]); icon = icon - 1

            ! Update D, VMULTC and CVIOL.
            dold = d
            d = (ONE - frac) * d + frac * dnew
            vmultc = max(ZERO, (ONE - frac) * vmultc + frac * vmultd)
            ! Exit in case of Inf/NaN in D or VMULTC.
            if (.not. (is_finite(sum(abs(d))) .and. is_finite(sum(abs(vmultc))))) then
                d = dold  ! Should we restore also IACT, NACT, VMULTC, and Z?
                exit
            end if

            if (stage == 1) then
                !cviol = (ONE - frac) * cvold + frac * cviol  ! Powell's version
                ! In theory, CVIOL = MAXVAL([MATPROD(D, A) - B, ZERO]), yet the CVIOL updated as above
                ! can be quite different from this value if A has huge entries (e.g., > 1E20).
                cviol = maximum([ZERO, matprod(d, A) - b])
            end if

            if (icon < 1 .or. icon > mcon) then
                ! In Powell's code, the condition is ICON == 0. Indeed, ICON < 0 cannot hold unless
                ! FRACMULT contains only NaN, which should not happen; ICON > MCON should never occur.
                exit
            end if
        end do

    end subroutine trstlp_sub

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
            delta = max(gamma1 * delta_in, dnorm)  ! Powell's UOBYQA/NEWUOA/BOBYQA/LINCOA
        else
            delta = max(gamma1 * delta_in, gamma2 * dnorm)  ! Powell's NEWUOA/BOBYQA.
            !delta = max(delta_in, gamma2 * dnorm)  ! Modified version. Works well for UOBYQA.
            ! For noise-free CUTEst problems of <= 100 variables, Powell's version works slightly better
            ! than the modified one.
            !delta = max(delta_in, 1.25_real64 * dnorm, dnorm + rho)  ! Powell's UOBYQA.
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

    !> A step that improves the geometry of the simplex, upstream's `geostep`.
    !!
    !! Equations (15) to (17) of the COBYLA paper: a step of length `delbar` along the direction
    !! `simi` says is worst determined, with the sign chosen to reduce the linearised merit
    !! function.
    function geostep(jdrop, amat, bvec, conmat, cpen, cval, delbar, fval, simi) result(d)
        ! Common modules

        integer, intent(in) :: jdrop                !! which simplex vertex is replaced
        real(real64), intent(in) :: amat(:, :)      !! the linear constraint gradients, one per column
        real(real64), intent(in) :: bvec(:)         !! the right-hand sides of `amat`
        real(real64), intent(in) :: conmat(:, :)    !! the constraint values at each simplex vertex, one vertex per column
        real(real64), intent(in) :: cpen            !! the penalty on violation in the merit function `f + cpen*cstrv`
        real(real64), intent(in) :: cval(:)         !! the constraint violation at each simplex vertex
        real(real64), intent(in) :: delbar          !! the radius the geometry step may use
        real(real64), intent(in) :: fval(:)         !! objective value at each simplex vertex
        real(real64), intent(in) :: simi(:, :)      !! the inverse of `sim(:, 1:n)`

        real(real64) :: d(size(simi, 1))            !! the geometry step

        integer :: m
        integer :: m_lcon
        integer :: n
        real(real64) :: A(size(simi, 1), size(conmat, 1))
        real(real64) :: cvnd
        real(real64) :: cvpd
        real(real64) :: g(size(simi, 1))

        m_lcon = int(size(bvec))
        m = int(size(conmat, 1))
        n = int(size(simi, 1))

        ! SIMI(JDROP, :) is a vector perpendicular to the face of the simplex to the opposite of vertex
        ! JDROP. Set D to the vector in this direction and with length DELBAR.
        d = simi(jdrop, :)
        d = delbar * (d / norm(d))

        ! The code below chooses the direction of D according to an approximation of the merit function.
        ! See (17) of the COBYLA paper and  line 225 of Powell's cobylb.f.

        ! Calculate the coefficients of the linear approximations to the objective and constraint functions.
        ! N.B.: CONMAT and SIMI have been updated after the last trust-region step, but G and A have not.
        ! So we cannot pass G and A from outside.
        g = matprod(fval(1:n) - fval(n + 1), simi)
        A(:, 1:m_lcon) = amat
        A(:, m_lcon + 1:m) = transpose(matprod(conmat(m_lcon + 1:m, 1:n) &
            & - spread(conmat(m_lcon + 1:m, n + 1), dim=2, ncopies=n), simi))
        ! MATLAB: A(:, m_lcon+1:m) = simi'*(conmat(m_lcon+1:m, 1:n) - conmat(m_lcon+1:m, n+1))'
        ! with implicit expansion for the subtraction
        ! CVPD and CVND are the predicted constraint violation of D and -D by the linear models.
        cvpd = maximum([ZERO, conmat(:, n + 1) + matprod(d, A)])
        cvnd = maximum([ZERO, conmat(:, n + 1) - matprod(d, A)])
        ! Take -D if the linear models predict that its merit function value is lower.
        if (-inprod(d, g) + cpen * cvnd < inprod(d, g) + cpen * cvpd) then
            d = -d
        end if

    end function geostep

    !> Which simplex vertex to replace after a trust-region step, upstream's `setdrop_tr`.
    !!
    !! Equations (19) to (22) of the COBYLA paper: the vertex whose removal leaves the simplex best
    !! conditioned, weighing each one's distance from the best vertex against its contribution to
    !! `simi`. When the step improved the merit function the answer is never zero, and never `n+1`.
    function setdrop_tr(ximproved, d, delta, rho, sim, simi) result(jdrop)
        ! Common modules

        logical, intent(in) :: ximproved          !! whether the new point improved on the merit function
        real(real64), intent(in) :: d(:)          !! the step from the best vertex
        real(real64), intent(in) :: delta         !! the trust-region radius
        real(real64), intent(in) :: rho           !! the lower bound on the trust-region radius
        real(real64), intent(in) :: sim(:, :)     !! the simplex: `n` displacement vectors, then the best vertex itself
        real(real64), intent(in) :: simi(:, :)    !! the inverse of `sim(:, 1:n)`

        integer :: jdrop                          !! the vertex to replace, `0` when none should be

        integer :: n
        real(real64) :: distsq(size(sim, 2))
        real(real64) :: weight(size(sim, 2))
        real(real64) :: score(size(sim, 2))
        real(real64) :: simid(size(simi, 1))
        !real(real64) :: sigbar(size(sim, 1))
        !real(real64) :: veta(size(sim, 1))
        !real(real64) :: vsig(size(sim, 1))

        n = int(size(sim, 1))

        !--------------------------------------------------------------------------------------------------!
        ! The following code is Powell's scheme for defining JDROP.
        !--------------------------------------------------------------------------------------------------!
        !  JDROP = 0 by default. It cannot be removed, as JDROP may not be set below in some cases (e.g.,
        !  when XIMPROVED == FALSE, MAXVAL(ABS(SIMID)) <= 1, and MAXVAL(VETA) <= EDGMAX).
        !jdrop = 0
        !
        !  SIMID(J) is the value of the J-th Lagrange function at D. It is the counterpart of VLAG in UOBYQA
        !  and DEN in NEWUOA/BOBYQA/LINCOA, but it excludes the value of the (N+1)-th Lagrange function.
        !simid = matprod(simi, d)
        !if (any(abs(simid) > 1) .or. (ximproved .and. any(.not. is_nan(simid)))) then
        !    jdrop = int(maxloc(abs(simid), mask=(.not. is_nan(simid)), dim=1))
        !    ! MATLAB: [~, jdrop] = max(simid, [], 'omitnan');
        !end if
        !
        !  VETA(J) is the distance from the J-th vertex of the simplex to the best vertex, taking the trial
        !  point SIM(:, N+1) + D into account.
        !if (ximproved) then
        !    veta = sqrt(sum((sim(:, 1:n) - spread(d, dim=2, ncopies=n))**2, dim=1))
        !    ! MATLAB: veta = sqrt(sum((sim(:, 1:n) - d).^2));  % d should be a column! Implicit expansion
        !else
        !    veta = sqrt(sum(sim(:, 1:n)**2, dim=1))
        !end if
        !
        !  VSIG(J) (J=1, .., N) is the Euclidean distance from vertex J to the opposite face of the simplex.
        !vsig = ONE / sqrt(sum(simi**2, dim=2))
        !sigbar = abs(simid) * vsig
        !
        !  The following JDROP will overwrite the previous one if its premise holds. FACTOR_DELTA = 1.1
        !  and FACTOR_ALPHA = 0.25.
        !mask = (veta > factor_delta * delta .and. (sigbar >= factor_alpha * delta .or. sigbar >= vsig))
        !if (any(mask)) then
        !    jdrop = int(maxloc(veta, mask=mask, dim=1))
        !    ! MATLAB: etamax = max(veta(mask)); jdrop = find(mask & ~(veta < etamax), 1, 'first');
        !end if
        !
        !  Powell's code does not include the following instructions. With Powell's code, if SIMID consists
        !  of only NaN, then JDROP can be 0 even when XIMPROVED == TRUE (i.e., D reduces the merit function).
        !  With the following code, JDROP cannot be 0 when XIMPROVED == TRUE, unless VETA is all NaN, which
        !  should not happen if X0 does not contain NaN, the trust-region/geometry steps never contain NaN,
        !  and we exit once encountering an iterate containing Inf (due to overflow).
        !if (ximproved .and. jdrop <= 0) then  ! Write JDROP <= 0 instead of JDROP == 0 for robustness.
        !    jdrop = int(maxloc(veta, mask=(.not. is_nan(veta)), dim=1))
        !    ! MATLAB: [~, jdrop] = max(veta, [], 'omitnan');
        !end if
        !--------------------------------------------------------------------------------------------------!
        ! Powell's scheme ends here.
        !--------------------------------------------------------------------------------------------------!

        ! The following definition of JDROP is inspired by SETDROP_TR in UOBYQA/NEWUOA/BOBYQA/LINCOA.
        ! It is simpler and works better than Powell's scheme. Note that we allow JDROP to be N+1 if
        ! IMPROVEX is TRUE, whereas Powell's code does not.
        ! See also (4.1) of Scheinberg-Toint-2010: Self-Correcting Geometry in Model-Based Algorithms for
        ! Derivative-Free Unconstrained Optimization, which refers to the strategy here as the "combined
        ! distance/poisedness criteria".

        ! DISTQ(J) is the square of the distance from the J-th vertex of the simplex to the "best" point so
        ! far, taking the trial point SIM(:, N+1) + D into account.
        if (ximproved) then
            distsq(1:n) = sum((sim(:, 1:n) - spread(d, dim=2, ncopies=n))**2, dim=1)
            ! MATLAB: distsq = sum((sim(:, 1:n) - d).^2);  % d should be a column! Implicit expansion
            distsq(n + 1) = sum(d**2)
        else
            distsq(1:n) = sum(sim(:, 1:n)**2, dim=1)
            distsq(n + 1) = ZERO
        end if

        weight = max(ONE, distsq / max(rho, TENTH * delta)**2)  ! Similar to Powell's NEWUOA code
        ! Other possible definitions of WEIGHT.
        ! !weight = distsq  ! Similar to Powell's LINCOA code, but WRONG. See comments in LINCOA/geometry.f90.
        ! !weight = max(ONE, 25.0_real64 * distsq / delta**2)  ! Similar to Powell's BOBYQA code, works well
        ! !weight = max(ONE, TEN * distsq / delta**2)
        ! !weight = max(ONE, 1.0E2_real64 * distsq / delta**2)
        ! !weight = max(ONE, distsq / rho**2)  ! Similar to Powell's UOBYQA

        ! If 1 <= J <= N, SIMID(J) is the value of the J-th Lagrange function at D; the value of the
        ! (N+1)-th Lagrange function is 1 - SUM(SIMID). [SIMID, 1 - SUM(SIMID)] is the counterpart of
        ! VLAG in UOBYQA and DEN in NEWUOA/BOBYQA/LINCOA.
        simid = matprod(simi, d)
        score = weight * abs([simid, ONE - sum(simid)])

        ! If XIMPROVED = FALSE (D does not render a better X), set SCORE(N+1) = -1 to avoid JDROP = N+1.
        if (.not. ximproved) then
            score(n + 1) = -ONE
        end if

        ! SCORE(J) is NaN implies SIMID(J) is NaN, but we want ABS(SIMID) to be big. So we exclude such J.
        score(trueloc(is_nan(score))) = -ONE

        jdrop = 0
        ! The following IF works a bit better than `IF (ANY(SCORE > 1) .OR. ANY(SCORE > 0) .AND. XIMPROVED)`
        ! from Powell's UOBYQA and NEWUOA code.
        if (any(score > 0)) then  ! Powell's BOBYQA and LINCOA code
            jdrop = int(maxloc(score, dim=1))
            ! MATLAB: [~, jdrop] = max(score);
        end if

        if ((ximproved .and. jdrop == 0) .or. jdrop < 0) then  ! JDROP < 0 is impossible in theory.
            jdrop = int(maxloc(distsq, dim=1))
        end if

    end function setdrop_tr

    !> Replaces one simplex vertex by the new point, upstream's `updatexfc`.
    !!
    !! `sim`, `simi`, `fval`, `conmat` and `cval` are updated together; `info` reports
    !! `DAMAGING_ROUNDING` when `simi` can no longer be trusted as the inverse.
    subroutine updatexfc(jdrop, constr, cpen, cstrv, d, f, conmat, cval, fval, sim, simi, info)
        ! Common modules

        integer, intent(in) :: jdrop                   !! which simplex vertex is replaced
        real(real64), intent(in) :: constr(:)          !! the constraint values at the new point
        real(real64), intent(in) :: cpen               !! the penalty on violation in the merit function `f + cpen*cstrv`
        real(real64), intent(in) :: cstrv              !! the constraint violation at the returned point
        real(real64), intent(in) :: d(:)               !! the step from the best vertex
        real(real64), intent(in) :: f                  !! objective value at the new point

        real(real64), intent(inout) :: conmat(:, :)    !! the constraint values at each simplex vertex, one vertex per column
        real(real64), intent(inout) :: cval(:)         !! the constraint violation at each simplex vertex
        real(real64), intent(inout) :: fval(:)         !! objective value at each simplex vertex
        real(real64), intent(inout) :: sim(:, :)       !! the simplex: `n` displacement vectors, then the best vertex itself
        real(real64), intent(inout) :: simi(:, :)      !! the inverse of `sim(:, 1:n)`

        integer, intent(out) :: info                   !! PRIMA's exit code

        integer :: m
        integer :: n
        real(real64) :: erri
        real(real64) :: erri_test
        real(real64) :: sim_old(size(sim, 1), size(sim, 2))
        real(real64) :: simi_jdrop(size(simi, 2))
        real(real64) :: simi_old(size(simi, 1), size(simi, 2))
        real(real64) :: simi_test(size(simi, 1), size(simi, 2))
        real(real64) :: simid(size(simi, 1))
        real(real64) :: sum_simi(size(simi, 2))
        real(real64), parameter :: itol = ONE

        m = int(size(constr))
        n = int(size(sim, 1))

        ! Do nothing when JDROP is 0. This can only happen after a trust-region step.
        if (jdrop <= 0) then  ! JDROP < 0 is impossible if the input is correct.
            info = INFO_DFT  ! INFO must be set, as it is an output!
            return
        end if

        sim_old = sim
        simi_old = simi
        ! N.B.: The use of OUTPROD is expensive memory-wise, but it is not our concern in this implementation.
        if (jdrop <= n) then
            sim(:, jdrop) = d
            simi_jdrop = simi(jdrop, :) / inprod(simi(jdrop, :), d)
            simi = simi - outprod(matprod(simi, d), simi_jdrop)
            simi(jdrop, :) = simi_jdrop
        else  ! JDROP = N+1
            sim(:, n + 1) = sim(:, n + 1) + d
            sim(:, 1:n) = sim(:, 1:n) - spread(d, dim=2, ncopies=n)
            simid = matprod(simi, d)
            sum_simi = sum(simi, dim=1)
            simi = simi + outprod(simid, sum_simi / (ONE - sum(simid)))
        end if

        ! Check whether SIMI is a poor approximation to the inverse of SIM(:, 1:N).
        ! Calculate SIMI from scratch if the current one is damaged by rounding errors.
        erri = maximum(abs(matprod(simi, sim(:, 1:n)) - eye(n)))  ! MAXIMUM(X) returns NaN if X contains NaN
        if (erri > TENTH * itol .or. is_nan(erri)) then
            simi_test = inv(sim(:, 1:n))
            erri_test = maximum(abs(matprod(simi_test, sim(:, 1:n)) - eye(n)))
            if (erri_test < erri .or. (is_nan(erri) .and. .not. is_nan(erri_test))) then
                simi = simi_test
                erri = erri_test
            end if
        end if

        ! If SIMI is satisfactory, then update FVAL, CONMAT, CVAL, and the pole position. Otherwise, restore
        ! SIM and SIMI, and return with INFO = DAMAGING_ROUNDING.
        if (erri <= itol) then
            fval(jdrop) = f
            conmat(:, jdrop) = constr
            cval(jdrop) = cstrv
            ! Switch the best vertex to the pole position SIM(:, N+1) if it is not there already.
            call updatepole(cpen, conmat, cval, fval, sim, simi, info)
        else  ! ERRI > ITOL or ERRI is NaN
            info = DAMAGING_ROUNDING
            sim = sim_old
            simi = simi_old
        end if

    end subroutine updatexfc

    !> Moves the best vertex to the pole position, upstream's `updatepole`.
    !!
    !! Powell's "pole position" is `sim(:, n+1)`, the vertex the displacements are measured from.
    !! Which vertex is best depends on `cpen`, so this runs again whenever `cpen` changes.
    !!
    !! Upstream's two notes are worth keeping. Applying this twice with two penalties is the same as
    !! applying it once with the second, in exact arithmetic but not in floating point. And where
    !! Powell's code gave up when `simi` was no longer a usable inverse, this recomputes it from
    !! scratch first and only then returns `DAMAGING_ROUNDING`.
    subroutine updatepole(cpen, conmat, cval, fval, sim, simi, info)
        ! Common modules

        real(real64), intent(in) :: cpen               !! the penalty on violation in the merit function `f + cpen*cstrv`

        real(real64), intent(inout) :: conmat(:, :)    !! the constraint values at each simplex vertex, one vertex per column
        real(real64), intent(inout) :: cval(:)         !! the constraint violation at each simplex vertex
        real(real64), intent(inout) :: fval(:)         !! objective value at each simplex vertex
        real(real64), intent(inout) :: sim(:, :)       !! the simplex: `n` displacement vectors, then the best vertex itself
        real(real64), intent(inout) :: simi(:, :)      !! the inverse of `sim(:, 1:n)`

        integer, intent(out) :: info                   !! PRIMA's exit code

        integer :: jopt
        integer :: m
        integer :: n
        real(real64) :: erri
        real(real64) :: erri_test
        real(real64) :: sim_jopt(size(sim, 1))
        real(real64) :: sim_old(size(sim, 1), size(sim, 2))
        real(real64) :: simi_old(size(simi, 1), size(simi, 2))
        real(real64) :: simi_test(size(simi, 1), size(simi, 2))
        real(real64), parameter :: itol = ONE

        m = int(size(conmat, 1))
        n = int(size(sim, 1))

        ! INFO must be set, as it is an output.
        info = INFO_DFT

        ! Identify the optimal vertex of the current simplex.
        jopt = findpole(cpen, cval, fval)

        ! Switch the best vertex to the pole position SIM(:, N+1) if it is not there already, and update
        ! SIMI. Before the update, save a copy of SIM and SIMI. If the update is unsuccessful due to
        ! damaging rounding errors, we restore them and return with INFO = DAMAGING_ROUNDING.
        sim_old = sim
        simi_old = simi
        if (jopt >= 1 .and. jopt <= n) then
            ! Unless there is a bug in FINDPOLE, it is guaranteed that JOPT >= 1.
            ! When JOPT == N + 1, there is nothing to switch; in addition, SIMI(JOPT, :) will be illegal.
            sim(:, n + 1) = sim(:, n + 1) + sim(:, jopt)
            sim_jopt = sim(:, jopt)
            sim(:, jopt) = ZERO
            sim(:, 1:n) = sim(:, 1:n) - spread(sim_jopt, dim=2, ncopies=n)
            ! MATLAB: sim(:, 1:n) = sim(:, 1:n) - sim_jopt; % sim_jopt should be a column! Implicit expansion
            ! The above update is equivalent to multiply SIM(:, 1:N) from the right side by a matrix whose
            ! JOPT-th row is [-1, -1, ..., -1], while all the other rows are the same as those of the
            ! identity matrix. It is easy to check that the inverse of this matrix is itself. Therefore,
            ! SIMI should be updated by a multiplication with this matrix (i.e., its inverse) from the left
            ! side, as is done in the following line. The JOPT-th row of the updated SIMI is minus the sum
            ! of all rows of the original SIMI, whereas all the other rows remain unchanged.
            simi(jopt, :) = -sum(simi, dim=1)  ! Must ensure that 1 <= JOPT <= N!
        end if

        ! Check whether SIMI is a poor approximation to the inverse of SIM(:, 1:N).
        ! Calculate SIMI from scratch if the current one is damaged by rounding errors.
        erri = maximum(abs(matprod(simi, sim(:, 1:n)) - eye(n)))  ! MAXIMUM(X) returns NaN if X contains NaN
        if (erri > TENTH * itol .or. is_nan(erri)) then
            simi_test = inv(sim(:, 1:n))
            erri_test = maximum(abs(matprod(simi_test, sim(:, 1:n)) - eye(n)))
            if (erri_test < erri .or. (is_nan(erri) .and. .not. is_nan(erri_test))) then
                simi = simi_test
                erri = erri_test
            end if
        end if

        ! If SIMI is satisfactory, then update FVAL, CONMAT, and CVAL. Otherwise, restore SIM and SIMI, and
        ! return with INFO = DAMAGING_ROUNDING.
        if (erri <= itol) then
            if (jopt >= 1 .and. jopt <= n) then
                fval([jopt, n + 1]) = fval([n + 1, jopt])
                conmat(:, [jopt, n + 1]) = conmat(:, [n + 1, jopt])
                cval([jopt, n + 1]) = cval([n + 1, jopt])
            end if
        else  ! ERRI > ITOL or ERRI is NaN
            info = DAMAGING_ROUNDING
            sim = sim_old
            simi = simi_old
        end if

    end subroutine updatepole

    !> Which vertex minimises the merit function `f + cpen*cstrv`, upstream's `findpole`.
    function findpole(cpen, cval, fval) result(jopt)
        ! Common modules

        real(real64), intent(in) :: cpen       !! the penalty on violation in the merit function `f + cpen*cstrv`
        real(real64), intent(in) :: cval(:)    !! the constraint violation at each simplex vertex
        real(real64), intent(in) :: fval(:)    !! objective value at each simplex vertex

        integer :: jopt                        !! the index of the best vertex

        integer :: n
        real(real64) :: phi(size(cval))
        real(real64) :: phimin

        ! Size
        n = int(size(fval) - 1)

        ! Identify the optimal vertex of the current simplex.
        jopt = int(size(fval))  ! We use N + 1 as the default value of JOPT.
        phi = fval + cpen * cval
        phimin = minval(phi)
        ! Essentially, JOPT = MINLOC(PHI). However, we keep JOPT = N + 1 unless there is a strictly better
        ! choice. When there are multiple choices, we choose the JOPT with the smallest value of CVAL.
        if (phimin < phi(jopt) .or. any(cval < cval(jopt) .and. phi <= phi(jopt))) then
            jopt = int(minloc(cval, mask=(phi <= phimin), dim=1))
            ! MATLAB: cmin = min(cval(phi <= phimin)); jopt = find(phi <= phimin & cval <= cmin, 1, 'first');
        end if

    end function findpole

    !> The penalty parameter that makes the predicted reduction positive, upstream's `getcpen`.
    !!
    !! The discussion around equation (9) of the COBYLA paper: `cpen` is raised until `prerem =
    !! preref + cpen*prerec > 0`, i.e. until the merit function actually predicts progress.
    function getcpen(amat, bvec, conmat_in, cpen_in, cval_in, delta, fval_in, rho, sim_in, simi_in) result(cpen)
        ! Common modules

        ! Solver-specific modules

        real(real64), intent(in) :: amat(:, :)         !! the linear constraint gradients, one per column
        real(real64), intent(in) :: bvec(:)            !! the right-hand sides of `amat`
        real(real64), intent(in) :: conmat_in(:, :)    !! the constraint values before the update
        real(real64), intent(in) :: cpen_in            !! the penalty before the update
        real(real64), intent(in) :: cval_in(:)         !! the violations before the update
        real(real64), intent(in) :: delta              !! the trust-region radius
        real(real64), intent(in) :: fval_in(:)         !! the values before the update
        real(real64), intent(in) :: rho                !! the lower bound on the trust-region radius
        real(real64), intent(in) :: sim_in(:, :)       !! the simplex before the update
        real(real64), intent(in) :: simi_in(:, :)      !! its inverse before the update

        real(real64) :: cpen                           !! the penalty to use

        integer :: info
        integer :: iter
        integer :: m
        integer :: m_lcon
        integer :: n
        real(real64) :: A(size(sim_in, 1), size(conmat_in, 1))
        real(real64) :: conmat(size(conmat_in, 1), size(conmat_in, 2))
        real(real64) :: cval(size(cval_in))
        real(real64) :: d(size(sim_in, 1))
        real(real64) :: fval(size(fval_in))
        real(real64) :: g(size(sim_in, 1))
        real(real64) :: prerec
        real(real64) :: preref
        real(real64) :: sim(size(sim_in, 1), size(sim_in, 2))
        real(real64) :: simi(size(simi_in, 1), size(simi_in, 2))

        m_lcon = int(size(bvec))
        m = int(size(conmat, 1))
        n = int(size(sim, 1))

        ! Copy the inputs.
        conmat = conmat_in
        cpen = cpen_in
        cval = cval_in
        fval = fval_in
        sim = sim_in
        simi = simi_in

        ! Initialize INFO, PREREF, and PREREC, which are needed in the postconditions.
        info = INFO_DFT
        preref = ZERO
        prerec = ZERO

        ! Increase CPEN if necessary to ensure PREREM > 0. Branch back for the next loop if this change
        ! alters the optimal vertex of the current simplex. Note the following.
        ! 1. In each loop, CPEN is changed only if PREREC > 0 > PREREF, in which case PREREM is guaranteed
        ! positive after the update. Note that PREREC >= 0 and MAX(PREREC, PREREF) > 0 in theory. If this
        ! holds numerically as well, then CPEN is not changed only if PREREC = 0 or PREREF >= 0, in which
        ! case PREREM is currently positive, explaining why CPEN needs no update.
        ! 2. Even without an upper bound for the loop counter, the loop can occur at most N+1 times. This is
        ! because the update of CPEN does not decrease CPEN, and hence it can make vertex J (J <= N) become
        ! the new optimal vertex only if CVAL(J) is less than CVAL(N+1), which can happen at most N times.
        ! See the paragraph below (9) in the COBYLA paper. After the "correct" optimal vertex is found,
        ! one more loop is needed to calculate CPEN, and hence the loop can occur at most N+1 times.
        do iter = 1, n + 1
            ! Switch the best vertex of the current simplex to SIM(:, N + 1).
            call updatepole(cpen, conmat, cval, fval, sim, simi, info)
            ! Check whether to exit due to damaging rounding in UPDATEPOLE.
            if (info == DAMAGING_ROUNDING) then
                exit
            end if

            ! Calculate the linear approximations to the objective and constraint functions.
            g = matprod(fval(1:n) - fval(n + 1), simi)
            A(:, 1:m_lcon) = amat
            A(:, m_lcon + 1:m) = transpose(matprod(conmat(m_lcon + 1:m, 1:n) &
                & - spread(conmat(m_lcon + 1:m, n + 1), dim=2, ncopies=n), simi))
            ! MATLAB: A(:, m_lcon+1:m) = simi'*(conmat(m_lcon+1:m, 1:n) - conmat(m_lcon+1:m, n+1))'
            ! with implicit expansion for the subtraction

            ! Calculate the trust-region trial step D. Note that D does NOT depend on CPEN.
            d = trstlp(A, -conmat(:, n + 1), delta, g)

            ! Predict the change to F (PREREF) and to the constraint violation (PREREC) due to D.
            preref = -inprod(d, g)  ! Can be negative.
            prerec = cval(n + 1) - maximum([ZERO, conmat(:, n + 1) + matprod(d, A)])

            if (.not. (prerec > 0 .and. preref < 0)) then  ! PREREC <= 0 or PREREF >= 0 or either is NaN.
                exit
            end if

            ! Powell's code defines BARMU = -PREREF / PREREC, and CPEN is increased to 2*BARMU if and
            ! only if it is currently less than 1.5*BARMU, a very "Powellful" scheme. In our implementation,
            ! however, we set CPEN directly to the maximum between its current value and 2*BARMU while
            ! handling possible overflow. This simplifies the scheme without worsening the performance.
            cpen = max(cpen, min(-TWO * (preref / prerec), REALMAX))

            if (findpole(cpen, cval, fval) == n + 1) then
                exit
            end if
        end do

    end function getcpen

    !> The ratio between the typical change in `f` and the typical change in the constraints,
    !> upstream's `fcratio`.
    !!
    !! Equations (12) and (13) of Section 3 of the COBYLA paper; it sets the scale `cpen` starts at.
    function fcratio(conmat, fval) result(r)
        real(real64), intent(in) :: conmat(:, :)    !! the constraint values at each simplex vertex, one vertex per column
        real(real64), intent(in) :: fval(:)         !! objective value at each simplex vertex

        real(real64) :: r                           !! the ratio

        real(real64) :: cmax(size(conmat, 1))
        real(real64) :: cmin(size(conmat, 1))
        real(real64) :: denom
        real(real64) :: fmax
        real(real64) :: fmin

        ! N.B.: In the original version of COBYLA, Powell proposed the ratio for constraints in the form of
        ! CONSTR(X) >= 0, but the constraints we consider here are CONSTR(X) <= 0. Hence we need to change
        ! the sign of the constraints before defining CMIN and CMAX.
        cmin = minval(-conmat, dim=2)
        cmax = maxval(-conmat, dim=2)
        fmin = minval(fval)
        fmax = maxval(fval)
        r = ZERO
        if (any(cmin < HALF * cmax) .and. fmin < fmax) then
            denom = minval(max(cmax, ZERO) - cmin, mask=(cmin < HALF * cmax))
            ! Powell mentioned the following alternative in Section 4 of his COBYLA paper. According to a
            ! test on 20230610, it does not make much difference to the performance.
            ! !denom = maxval(max(cmax, ZERO) - cmin, mask=(cmin < HALF * cmax))
            r = (fmax - fmin) / denom
        end if

    end function fcratio

end module parquet_prima_cobylb
