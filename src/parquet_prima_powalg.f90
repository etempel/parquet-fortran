!> PRIMA's `powalg_mod`: the algebra of Powell's quadratic interpolation model -- the model
!> increment, the Hessian product, the Lagrange values, the updating denominator and the
!> interpolation-set index table.
!!
!! **Provenance.** Derived from PRIMA (Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`: `fortran/common/powalg.f90`. The licence text and the
!! deviations shared by every file of this tier are in `parquet_prima_linalg`.
!!
!! **A bug found here is checked against upstream before it is fixed here.** Every formula below
!! is upstream's, and the comments naming equations of the NEWUOA and BOBYQA papers are upstream's
!! too; they are what makes this file checkable against the papers rather than only against the
!! code.
!!
!! **What is not vendored here.** `errh`, `errquad`, `omega_inprod`, `calvlag_qint` and
!! `quadinc_ghv`: UOBYQA's, or reached only from upstream's `assert`s. `setij` loses its optional
!! `sorting_direction`: no solver here passes one, and the sort it would need went with it.
!!
!! **Two `updateh`s exist in this tier, and they are not the same procedure.** The one below is
!! NEWUOA's and LINCOA's, which carries `idz` -- the sign split of the `ZMAT` factorisation. BOBYQA
!! has its own in `parquet_prima_bobyqb`, without `idz`, because the RESCUE method of section 5 of
!! the BOBYQA paper removes the need for it (equivalently, `idz = 1` throughout). Upstream keeps
!! them apart the same way, in `powalg.f90` and `bobyqa/update.f90`.
module parquet_prima_powalg

    use, intrinsic :: iso_fortran_env, only : real64
    use parquet_prima_linalg, only : sum, ZERO, HALF, ONE, EPS, REALMAX, inprod, matprod, outprod, &
        trueloc, is_finite, DAMAGING_ROUNDING, INFO_DFT, &
        diag, planerot, symmetrize, hypotenuse, isminor, norm

    implicit none
    private

    public :: quadinc, hess_mul, omega_mul, omega_col, calvlag, calbeta, calden, setij
    public :: qradd, qrexc, updateh

    interface quadinc
        module procedure quadinc_d0
    end interface quadinc

    interface calvlag
        module procedure calvlag_lfqint
    end interface calvlag

    interface qradd
        module procedure qradd_Rdiag, qradd_Rfull
    end interface qradd

    interface qrexc
        module procedure qrexc_Rdiag, qrexc_Rfull
    end interface qrexc

contains


    !> `Q(d) - Q(0)` for the quadratic model, upstream's `quadinc_d0`.
    !!
    !! The Hessian is Powell's split form: an explicit part `hq` and an implicit part `pq` over the
    !! interpolation set, `HESSIAN = hq + sum_k pq(k) xpt(:,k) xpt(:,k)^T`.
    function quadinc_d0(d, xpt, gq, pq, hq) result(qinc)
        real(real64), intent(in) :: d(:)                  !! the step
        real(real64), intent(in) :: xpt(:, :)             !! the interpolation set, one point per column
        real(real64), intent(in) :: gq(:)                 !! the model's gradient at the base point
        real(real64), intent(in) :: pq(:)                 !! implicit Hessian weights
        real(real64), intent(in), optional :: hq(:, :)    !! explicit Hessian part; absent means zero

        real(real64) :: qinc                              !! the model increment

        ! Local variable
        integer :: n
        integer :: npt
        real(real64) :: dxpt(size(pq))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        !--------------------------------------------------------------------------------------------------!
        ! The following is Powell's scheme in LINCOA.
        ! !! First-order term and explicit second-order term
        ! !qinc = ZERO
        ! !do j = 1, n
        ! !    qinc = qinc + d(j) * gq(j)
        ! !    do i = 1, j
        ! !        t = d(i) * d(j)
        ! !        if (i == j) then
        ! !            t = HALF * t
        ! !        end if
        ! !        if (present(hq)) then
        ! !            qinc = qinc + t * hq(i, j)
        ! !        end if
        ! !    end do
        ! !end do
        ! !
        ! !! Implicit second-order term
        ! !dxpt = matprod(d, xpt)
        ! !do i = 1, npt
        ! !    qinc = qinc + HALF * pq(i) * dxpt(i) * dxpt(i)  ! In BOBYQA, it is QINC - HALF * PQ(I) * DXPT(I)**2.
        ! !end do
        !--------------------------------------------------------------------------------------------------!

        !--------------------------------------------------------------------------------------------------!
        ! The following is a loop-free implementation, which should be applied in MATLAB/Python/R/Julia.
        ! N.B.: INPROD(DXPT, PQ * DXPT) = INPROD(D, HESS_MUL(D, XPT, PQ))
        !--------------------------------------------------------------------------------------------------!
        dxpt = matprod(d, xpt)
        if (present(hq)) then
            qinc = inprod(d, gq + HALF * matprod(hq, d)) + HALF * inprod(dxpt, pq * dxpt)
        else
            qinc = inprod(d, gq) + HALF * inprod(dxpt, pq * dxpt)
        end if
        ! MATLAB:
        ! if nargin >= 5
        !     qinc = d'*(gq + 0.5*hq*d) + 0.5*dxpt'*(pq*dxpt);
        ! else
        !     qinc = d'*gq + 0.5*dxpt'*(pq*dxpt);
        ! end
        !--------------------------------------------------------------------------------------------------!

    end function quadinc_d0

    !> `HESSIAN*x` for the model's split Hessian, upstream's `hess_mul`.
    function hess_mul(x, xpt, pq, hq) result(y)
        real(real64), intent(in) :: x(:)                  !! the vector
        real(real64), intent(in) :: xpt(:, :)             !! the interpolation set, one point per column
        real(real64), intent(in) :: pq(:)                 !! implicit Hessian weights
        real(real64), intent(in), optional :: hq(:, :)    !! explicit Hessian part; absent means zero

        real(real64) :: y(size(x))                        !! the product

        integer :: j
        integer :: n
        integer :: npt

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        !--------------------------------------------------------------------------------!
        !----------! y = matprod(hq, x) + matprod(xpt, pq * matprod(x, xpt)) !-----------!
        !--------------------------------------------------------------------------------!
        y = matprod(xpt, pq * matprod(x, xpt))
        if (present(hq)) then
            do j = 1, n
                y = y + hq(:, j) * x(j)
            end do
        end if

    end function hess_mul

    !> `OMEGA*x`, upstream's `omega_mul`.
    !!
    !! `OMEGA` is the leading `npt`-by-`npt` block of the matrix `H` of (3.12) of the NEWUOA paper,
    !! held in factored form as `zmat` with a sign split at `idz`.
    function omega_mul(idz, zmat, x) result(y)
        integer, intent(in) :: idz                !! the sign split: column `i` counts negative while `i < idz`
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z
        real(real64), intent(in) :: x(:)          !! the vector

        real(real64) :: y(size(zmat, 1))          !! the product

        real(real64) :: xz(size(zmat, 2))

        xz = matprod(x, zmat)
        xz(1:idz - 1) = -xz(1:idz - 1)
        y = matprod(zmat, xz)

    end function omega_mul

    !> `vlag = H*w` for a step `d` from `xpt(:, kref)`, upstream's `calvlag_lfqint`.
    !!
    !! Equation (4.25) of the NEWUOA paper. Normally called with `kref = kopt`, the best
    !! interpolation point and the trust region's centre.
    function calvlag_lfqint(kref, bmat, d, xpt, zmat, idz) result(vlag)
        ! Common modules

        integer, intent(in) :: kref                          !! which interpolation point the step starts from
        real(real64), intent(in) :: bmat(:, :)               !! the factorisation's B
        real(real64), intent(in) :: d(:)                     !! the step
        real(real64), intent(in) :: xpt(:, :)                !! the interpolation set
        real(real64), intent(in) :: zmat(:, :)               !! the factorisation's Z
        integer, intent(in), optional :: idz                 !! the sign split; absent in BOBYQA, which is `idz = 1`

        real(real64) :: vlag(size(xpt, 1) + size(xpt, 2))    !! the Lagrange values

        integer :: idz_loc
        integer :: n
        integer :: npt
        real(real64) :: wcheck(size(zmat, 1))
        real(real64) :: xref(size(xpt, 1))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Read IDZ, which is not present in BOBYQA, being equivalent to IDZ = 1.
        idz_loc = 1
        if (present(idz)) then
            idz_loc = idz
        end if

        xref = xpt(:, kref)  ! Read XREF.

        ! Set WCHECK to the first NPT entries of (w-v) for w and v in (4.10) and (4.24) of the NEWUOA paper.
        wcheck = matprod(d, xpt)
        wcheck = wcheck * (HALF * wcheck + matprod(xref, xpt))

        ! The following two lines set VLAG to H*(w-v).
        vlag(1:npt) = omega_mul(idz_loc, zmat, wcheck) + matprod(d, bmat(:, 1:npt))
        vlag(npt + 1:npt + n) = matprod(bmat, [wcheck, d])
        ! The following line is equivalent to the above one, but handles WCHECK and D separately.
        ! !vlag(npt + 1:npt + n) = matprod(bmat(:, 1:npt), wcheck) + matprod(bmat(:, npt + 1:npt + n), d)

        ! The following line sets VLAG(KREF) to the correct value.
        vlag(kref) = vlag(kref) + ONE

    end function calvlag_lfqint

    !> `beta` for a step `d` from `xpt(:, kref)`, upstream's `calbeta`.
    !!
    !! Equations (4.12) and (4.26) of the NEWUOA paper. The expression is Powell's rather than the
    !! algebraically equal one, because his contracts the rounding error by a factor of `||d||^2`;
    !! upstream's comment inside the body says why in full.
    function calbeta(kref, bmat, d, xpt, zmat, idz) result(beta)
        ! Common modules

        integer, intent(in) :: kref               !! which interpolation point the step starts from
        real(real64), intent(in) :: bmat(:, :)    !! the factorisation's B
        real(real64), intent(in) :: d(:)          !! the step
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z
        integer, intent(in), optional :: idz      !! the sign split; absent in BOBYQA, which is `idz = 1`

        real(real64) :: beta                      !! the paper's beta

        integer :: idz_loc
        integer :: n
        integer :: npt
        real(real64) :: dsq
        real(real64) :: dvlag
        real(real64) :: dxref
        real(real64) :: vlag(size(xpt, 1) + size(xpt, 2))
        real(real64) :: wcheck(size(zmat, 1))
        real(real64) :: wmv(size(xpt, 1) + size(xpt, 2))
        real(real64) :: wvlag
        real(real64) :: xref(size(xpt, 1))
        real(real64) :: xrefsq

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Read IDZ, which is absent from BOBYQA, being equivalent to IDZ = 1.
        idz_loc = 1
        if (present(idz)) then
            idz_loc = idz
        end if

        xref = xpt(:, kref)  ! Read XREF.

        !--------------------------------------------------------------------------------------------------!
        ! N.B.: When checking the NEWUOA paper, note that the paper takes KREF = KOPT and XREF = XOPT.
        !--------------------------------------------------------------------------------------------------!

        ! Set WCHECK to the first NPT entries of (w-v) for w and v in (4.10) and (4.24) of the NEWUOA paper.
        wcheck = matprod(d, xpt)
        wcheck = wcheck * (HALF * wcheck + matprod(xref, xpt))

        ! WMV is the vector (w-v) for w and v in (4.10) and (4.24) of the NEWUOA paper.
        wmv = [wcheck, d]
        ! The following two lines set VLAG to H*(w-v).
        vlag(1:npt) = omega_mul(idz_loc, zmat, wcheck) + matprod(d, bmat(:, 1:npt))
        vlag(npt + 1:npt + n) = matprod(bmat, wmv)
        ! The following line is equivalent to the above one, but handles WCHECK and D separately.
        ! !VLAG(NPT + 1:NPT + N) = MATPROD(BMAT(:, 1:NPT), WCHECK) + MATPROD(BMAT(:, NPT + 1:NPT + N), D)

        ! Set BETA = HALF*||XREF + D||^4 - (W-V)'*H*(W-V) - [XREF'*(X+XREF)]^2 + HALF*||XREF||^4. See
        ! equations (4.10), (4.12), (4.24), and (4.26) of the NEWUOA paper.
        dxref = inprod(d, xref)
        dsq = inprod(d, d)
        xrefsq = inprod(xref, xref)
        dvlag = inprod(d, vlag(npt + 1:npt + n))
        wvlag = inprod(wcheck, vlag(1:npt))
        beta = dxref**2 + dsq * (xrefsq + dxref + dxref + HALF * dsq) - dvlag - wvlag
        !---------------------------------------------------------------------------------------------------!
        ! The last line is equivalent to either of the following lines, but performs better numerically.
        ! !BETA = DXREF**2 + DSQ * (XREFSQ + DXREF + DXREF + HALF * DSQ) - INPROD(VLAG, WMV)  ! not good
        ! !BETA = DXREF**2 + DSQ * (XREFSQ + DXREF + DXREF + HALF * DSQ) - WVLAG - DVLAG  ! bad
        !---------------------------------------------------------------------------------------------------!

        ! N.B.:
        ! 1. Mathematically, the following two quantities are equal:
        ! DXREF**2 + DSQ * (XREFSQ + DXREF + DXREF + HALF * DSQ) ,
        ! HALF * (INPROD(X, X)**2 + INPROD(XREF, XREF)**2) - INPROD(X, XREF)**2 with X = XREF + D.
        ! However, the first (by Powell) is a better numerical scheme. According to the first formulation,
        ! this quantity is in the order of ||D||^2*||XREF||^2 if ||XREF|| >> ||D||, which is normally the case.
        ! However, each term in the second formulation has an order of ||XREF||^4. Thus much cancellation
        ! will occur in the second formulation. In addition, the first formulation contracts the rounding
        ! error in (XREFSQ + DXREF + DXREF + HALF * DSQ) by a factor of ||D||^2, which is typically small.
        ! 2. We can evaluate INPROD(VLAG, WMV) as INPROD(VLAG(1:NPT), WCHECK) + INPROD(VLAG(NPT+1:NPT+N),D)
        ! if it is desirable to handle WCHECK and D separately due to their significantly different magnitudes.

        ! The following line sets VLAG(KREF) to the correct value if we intend to output VLAG.
        ! !VLAG(KREF) = VLAG(KREF) + ONE

    end function calbeta

    !> The updating denominators for a step `d` from `xpt(:, kref)`, upstream's `calden`.
    !!
    !! `den(k)` is the `sigma` of (4.12) of the NEWUOA paper that would result from replacing
    !! `xpt(:, k)` by `xpt(:, kref) + d`; it is the denominator of the update (4.11) of `H`, so the
    !! geometry step picks the `k` that maximises it.
    function calden(kref, bmat, d, xpt, zmat, idz) result(den)
        ! Common modules

        integer, intent(in) :: kref               !! which interpolation point the step starts from
        real(real64), intent(in) :: bmat(:, :)    !! the factorisation's B
        real(real64), intent(in) :: d(:)          !! the step
        real(real64), intent(in) :: xpt(:, :)     !! the interpolation set
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z
        integer, intent(in), optional :: idz      !! the sign split; absent in BOBYQA, which is `idz = 1`

        real(real64) :: den(size(xpt, 2))         !! one denominator per interpolation point

        integer :: idz_loc
        integer :: n
        integer :: npt
        real(real64) :: beta
        real(real64) :: hdiag(size(xpt, 2))
        real(real64) :: vlag(size(xpt, 1) + size(xpt, 2))

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! Read IDZ, which is absent from BOBYQA, being equivalent to IDZ = 1.
        idz_loc = 1
        if (present(idz)) then
            idz_loc = idz
        end if

        hdiag = -sum(zmat(:, 1:idz_loc - 1)**2, dim=2) + sum(zmat(:, idz_loc:size(zmat, 2))**2, dim=2)
        vlag = calvlag(kref, bmat, d, xpt, zmat, idz_loc)
        beta = calbeta(kref, bmat, d, xpt, zmat, idz_loc)
        den = hdiag * beta + vlag(1:npt)**2

    end function calden

    !> The index table of the interpolation set, upstream's `setij`.
    !!
    !! `ij(:, k)` is the pair `[p, q]` of (2.4) of the BOBYQA paper for point `k + 2n + 1`: which
    !! two coordinates that point displaces. Empty when `npt <= 2n + 1`.
    function setij(n, npt) result(ij)
        integer, intent(in) :: n                     !! the dimension
        integer, intent(in) :: npt                   !! how many interpolation points
        integer :: ij(2, max(0, npt - 2 * n - 1))    !! the coordinate pairs, two rows by `npt - 2n - 1` columns
        integer :: k
        integer :: ell(max(0, npt - 2 * n - 1))

        ell = int([(k, k=n, npt - n - 2)] / n)  ! The ell below (2.4) of the BOBYQA paper.
        ij(1, :) = [(k, k=n, npt - n - 2)] - n * ell + 1
        ij(2, :) = modulo(ij(1, :) + ell - 1, n) + 1  ! MODULO(K-1, N) + 1 = K-N for K in [N+1, 2N]
        ! MATLAB: (N.B.: Fortran MODULO == MATLAB `mod`, Fortran MOD == MATLAB `rem`)
        ! ell = floor((n : npt-n-2) / n);
        ! ij(1, :) = (n : npt-n-2) - n*ell + 1;
        ! ij(2, :) = mod(ij(1, :) + ell - 1, n) + 1;  % mod(k-1,n) + 1 = k-n for k in [n+1,2n]
        ! if nargin >= 3
        ! end

    end function setij

    ! ---- What LINCOA and COBYLA add: the QR updates and the factorisation update ---------------
    !
    ! Everything above is what BOBYQA reaches (`feature_optimizer.md` 12.1 allows a phase to extend
    ! what an earlier one built).

    !> Column `k` of `OMEGA`, upstream's `omega_col`.
    !!
    !! `OMEGA` is the leading `npt`-by-`npt` block of the matrix `H` of equation (3.12) of the
    !! NEWUOA paper, held as `ZMAT*Diag(S)*ZMAT^T` with `S(1:idz-1) = -1` and the rest `+1`.
    function omega_col(idz, zmat, k) result(y)
        integer, intent(in) :: idz                !! the factorisation's sign split
        integer, intent(in) :: k                  !! which column
        real(real64), intent(in) :: zmat(:, :)    !! the factorisation's Z

        real(real64) :: y(size(zmat, 1))          !! that column of `OMEGA`

        real(real64) :: zk(size(zmat, 2))

        zk = zmat(k, :)
        zk(1:idz - 1) = -zk(1:idz - 1)
        y = matprod(zmat, zk)

    end function omega_col

    !> Appends a column to a QR factorisation held as `Q` and the diagonal of `R`; upstream's
    !> `qradd_Rdiag`, COBYLA's form.
    !!
    !! The new column is added while full column rank is kept: if `c` is outside the range of `A`
    !! the matrix becomes `[A, c]` and `n` grows by one; if it is inside, `c` replaces the last
    !! column instead and `n` is unchanged. Upstream's comment records that Powell's own code always
    !! drops the LAST column in that second case, and that a snippet meant for the general case can
    !! never be reached -- kept as upstream has it.
    subroutine qradd_Rdiag(c, Q, Rdiag, n)  ! Used in COBYLA
        real(real64), intent(in) :: c(:)           !! the column to append, of length m

        integer, intent(inout) :: n                !! columns in the factorisation; may be increased by one
        real(real64), intent(inout) :: Q(:, :)     !! the orthonormal columns, updated in place
        real(real64), intent(inout) :: Rdiag(:)    !! the diagonal of R, updated in place

        integer :: k
        integer :: m
        integer :: nsave
        real(real64) :: cq(size(Q, 2))
        real(real64) :: cqa(size(Q, 2))
        real(real64) :: G(2, 2)
        !------------------------------------------------------------!
        !------------------------------------------------------------!

        m = int(size(Q, 2))

        nsave = n  ! Needed for debugging (only).

        ! As in Powell's COBYLA, CQ is set to 0 at the positions with CQ being negligible as per ISMINOR.
        ! This may not be the best choice if the subroutine is used in other contexts, e.g., LINCOA.
        cq = matprod(c, Q)
        cqa = matprod(abs(c), abs(Q))
        cq(trueloc(isminor(cq, cqa))) = ZERO  ! MATLAB: cq(isminor(cq, cqa)) = zero

        ! Update Q so that the columns of Q(:, N+2:M) are orthogonal to C. This is done by applying a 2D
        ! Givens rotation to Q(:, [K, K+1]) from the right to zero C'*Q(:, K+1) out for K = N+1, ..., M-1
        ! in the reverse order. Nothing will be done if N >= M-1.
        do k = m - 1, n + 1, -1
            if (abs(cq(k + 1)) > 0) then
                ! Powell wrote CQ(K+1) /= 0 instead of ABS(CQ(K+1)) > 0. The two differ if CQ(K+1) is NaN.
                ! If we apply the rotation below when CQ(K+1) = 0, then CQ(K) will get updated to |CQ(K)|.
                G = planerot(cq([k, k + 1]))
                Q(:, [k, k + 1]) = matprod(Q(:, [k, k + 1]), transpose(G))
                cq(k) = hypotenuse(cq(k), cq(k + 1))  !cq(k) = sqrt(cq(k)**2 + cq(k + 1)**2)
            end if
        end do

        ! Augment N by 1 if C is not in range(A).
        ! The two IFs cannot be merged as Fortran may evaluate CQ(N+1) even if N>=M, leading to a SEGFAULT.
        if (n < m) then
            ! Powell's condition for the following IF: CQ(N+1) /= 0.
            if (abs(cq(n + 1)) > EPS**2 .and. .not. isminor(cq(n + 1), cqa(n + 1))) then
                n = n + 1
            end if
        end if

        ! Update RDIAG so that RDIAG(N) = CQ(N) = INPROD(C, Q(:, N)). Note that N may have been augmented.
        ! Zaikun 20230903: Different from QRADD_RFULL, Powell did not maintain the positiveness of RDIAG.
        if (n >= 1 .and. n <= m) then  ! Indeed, N > M should not happen unless the input is wrong.
            Rdiag(n) = cq(n)  ! Indeed, RDIAG(N) = INPROD(C, Q(:, N))
        end if

    end subroutine qradd_Rdiag

    !> Appends a column to a QR factorisation held as `Q` and the full `R`; upstream's
    !> `qradd_Rfull`, LINCOA's form.
    !!
    !! Unlike `qradd_Rdiag` this always appends and always increases `n` by one: in LINCOA `c` is
    !! known not to lie in the column space of `A`.
    subroutine qradd_Rfull(c, Q, R, n)  ! Used in LINCOA
        real(real64), intent(in) :: c(:)          !! the column to append, of length m

        integer, intent(inout) :: n               !! columns in the factorisation, increased by one
        real(real64), intent(inout) :: Q(:, :)    !! the orthonormal columns, updated in place
        real(real64), intent(inout) :: R(:, :)    !! the upper triangle, updated in place

        integer :: k
        integer :: m
        real(real64) :: cq(size(Q, 2))
        real(real64) :: G(2, 2)
        !------------------------------------------------------------!
        !------------------------------------------------------------!

        m = int(size(Q, 1))

        cq = matprod(c, Q)

        ! Update Q so that the columns of Q(:, N+2:M) are orthogonal to C. This is done by applying a 2D
        ! Givens rotation to Q(:, [K, K+1]) from the right to zero C'*Q(:, K+1) out for K = N+1, ..., M-1.
        ! Nothing will be done if N >= M-1.
        do k = m - 1, n + 1, -1
            if (abs(cq(k + 1)) > 0) then  ! Powell: IF (ABS(CQ(K + 1)) > 1.0D-20 * ABS(CQ(K))) THEN
                G = planerot(cq([k, k + 1]))
                Q(:, [k, k + 1]) = matprod(Q(:, [k, k + 1]), transpose(G))
                cq(k) = sqrt(cq(k)**2 + cq(k + 1)**2)
            end if
        end do

        R(1:n, n + 1) = matprod(c, Q(:, 1:n))

        ! Maintain the positiveness of the diagonal entries of R.
        if (cq(n + 1) < 0) then
            Q(:, n + 1) = -Q(:, n + 1)
        end if
        R(n + 1, n + 1) = abs(cq(n + 1))

        n = n + 1

    end subroutine qradd_Rfull

    !> Moves column `i` of a QR factorisation to the end; upstream's `qrexc_Rdiag`, COBYLA's form.
    !!
    !! The factorisation is updated to one of the matrix whose columns `i` to `n` have been
    !! rearranged to `i+1, ..., n, i`. This is how the constrained solvers drop a constraint from
    !! the active set without refactorising.
    subroutine qrexc_Rdiag(A, Q, Rdiag, i)  ! Used in COBYLA
        real(real64), intent(in) :: A(:, :)        !! the matrix the factorisation belongs to, m by n

        real(real64), intent(inout) :: Q(:, :)     !! the orthonormal columns, updated in place
        real(real64), intent(inout) :: Rdiag(:)    !! the diagonal of R, updated in place
        integer, intent(in) :: i                   !! the column to move to the end

        integer :: k
        integer :: m
        integer :: n
        real(real64) :: G(2, 2)
        !------------------------------------------------------------!
        !------------------------------------------------------------!

        m = int(size(A, 1))
        n = int(size(A, 2))

        if (i <= 0 .or. i >= n) then
            ! Only I == N is really needed, as 1 <= I <= N unless the input is wrong.
            return                                ! GCOVR_EXCL_LINE -- COBYLA drops an interior
                                                  ! column, so 1 <= i < n at every call
        end if

        ! Let R be the upper triangular matrix in the QR factorization, namely R = Q^T*A.
        ! For each K, find the Givens rotation G with G*R([K, K+1], :) = [HYPT, 0], and update Q(:, [K,K+1])
        ! to Q(:, [K, K+1])*G^T. Then R = Q^T*A is an upper triangular matrix as long as A(:, [K, K+1]) is
        ! updated to A(:, [K+1, K]). Indeed, this new upper triangular matrix can be obtained by first
        ! updating R([K, K+1], :) to G*R([K, K+1], :) and then exchanging its columns K and K+1; at the same
        ! time, entries K and K+1 of R's diagonal RDIAG become [HYPT, -(RDIAG(K+1) / HYPT) * RDIAG(K)].
        ! After this is done for each K = 1, ..., N-1, we obtain the QR factorization of the matrix that
        ! rearranges columns [I, I+1, ..., N] of A as [I+1, ..., N, I].
        ! Powell's code, however, is slightly different: before everything, he first exchanged columns K and
        ! K+1 of Q (as well as rows K and K+1 of R). This makes sure that the entires of the update RDIAG
        ! are all positive if it is the case for the original RDIAG.
        ! Zaikun 20230903: It turns out that Powell's code does not ensure that the original RDIAG is
        ! positive (see QRADD_RDIAG), and hence the updated RDIAG may contain negative values.
        do k = i, n - 1
            G = planerot([Rdiag(k + 1), inprod(Q(:, k), A(:, k + 1))])
            Q(:, [k, k + 1]) = matprod(Q(:, [k + 1, k]), transpose(G))
            ! Powell's code updates RDIAG in the following way:
            ! !HYPT = SQRT(RDIAG(K + 1)**2 + INPROD(Q(:, K), A(:, K + 1))**2)
            ! !RDIAG([K, K + 1]) = [HYPT, (RDIAG(K + 1) / HYPT) * RDIAG(K)]
            ! Note that RDIAG(N) inherits all rounding in RDIAG(I:N-1) and Q(:, I:N-1) and hence contain
            ! significant errors. Thus we may modify Powell's code to set only RDIAG(K) = HYPT here and then
            ! calculate RDIAG(N) by an inner product after the loop. Nevertheless, we simply calculate RDIAG
            ! from scratch we do below.
        end do

        ! Calculate RDIAG(I:N) from scratch.
        Rdiag(i:n - 1) = [(inprod(Q(:, k), A(:, k + 1)), k=i, n - 1)]
        ! MATLAB: Rdiag(i:n-1) = sum(Q(:, i:n-1) .* A(:, i+1:n), 1);  % Row vector
        Rdiag(n) = inprod(Q(:, n), A(:, i))  ! Calculate RDIAG(N) from scratch. See the comments above.

    end subroutine qrexc_Rdiag

    !> Moves column `i` of a QR factorisation to the end; upstream's `qrexc_Rfull`, LINCOA's form.
    !!
    !! As `qrexc_Rdiag`, but with the full upper triangle rather than its diagonal, and without
    !! needing the matrix itself.
    subroutine qrexc_Rfull(Q, R, i)  ! Used in LINCOA
        integer, intent(in) :: i                  !! the column to move to the end

        real(real64), intent(inout) :: Q(:, :)    !! the orthonormal columns, updated in place
        real(real64), intent(inout) :: R(:, :)    !! the upper triangle, updated in place

        integer :: k
        integer :: m
        integer :: n
        real(real64) :: G(2, 2)
        real(real64) :: hypt
        !------------------------------------------------------------!
        !------------------------------------------------------------!

        m = int(size(Q, 1))
        n = int(size(R, 2))

        if (i <= 0 .or. i >= n) then
            ! Only I == N is really needed, as 1 <= I <= N unless the input is wrong.
            return
        end if

        ! For each K, find the Givens rotation G with G*R([K, K+1], K+1) = [HYPT, 0]. Then make two updates.
        ! First, update Q(:, [K, K+1]) to Q(:, [K, K+1])*G^T, and R([K, K+1], :) to G*R[K+1, K], :), which
        ! keeps Q*R unchanged and maintains the orthogonality of Q's columns. Second, exchange columns K and
        ! K+1 of R. Then R becomes upper triangular, and the new product Q*R exchanges columns K and K+1 of
        ! the original one. After this is done for each K = 1, ..., N-1, we obtain the QR factorization of
        ! the matrix that rearranges columns [I, I+1, ..., N] of A as [I+1, ..., N, I].
        ! Powell's code, however, is slightly different: before everything, he first exchanged columns K and
        ! K+1 of Q as well as rows K and K+1 of R. This makes sure that the diagonal entries of the updated
        ! R are all positive if it is the case for the original R.
        ! LINCOA's calls all arrive with `i == n` -- the constraint it drops is the last of the
        ! active set -- so the quick return above answers every one and this loop, which
        ! rearranges the columns before it, does not run.
        do k = i, n - 1                           ! GCOVR_EXCL_START -- see the note above
            G = planerot(R([k + 1, k], k + 1))
            ! HYPT must be calculated before R is updated.
            hypt = hypotenuse(R(k + 1, k + 1), R(k, k + 1)) !hypt = sqrt(R(k, k + 1)**2 + R(k + 1, k + 1)**2)

            ! Update Q(:, [K, K+1]).
            Q(:, [k, k + 1]) = matprod(Q(:, [k + 1, k]), transpose(G))

            ! Update R([K, K+1], :).
            R([k, k + 1], k:n) = matprod(G, R([k + 1, k], k:n))
            R(1:k + 1, [k, k + 1]) = R(1:k + 1, [k + 1, k])
            ! N.B.: The above two lines implement the following while noting that R is upper triangular.
            ! !R([K, K + 1], :) = MATPROD(G, R([K + 1, K], :))  ! No need for R([K, K+1], 1:K-1) = 0
            ! !R(:, [K, K + 1]) = R(:, [K + 1, K])  ! No need for R(K+2:, [K, K+1]) = 0

            ! Revise R([K, K+1], K). Changes nothing in theory but seems good for the practical performance.
            R([k, k + 1], k) = [hypt, ZERO]

            !----------------------------------------------------------------------------------------------!
            ! The following code performs the update without exchanging columns K and K+1 of Q or rows K and
            ! K+1 of R beforehand. If the diagonal entries of the original R are positive, then all the
            ! updated ones become negative.
            !
            ! !G = planerot(R([k, k + 1], k + 1))
            ! !hypt = hypotenuse(R(k + 1, k + 1), R(k, k + 1)) !hypt = sqrt(R(k, k + 1)**2 + R(k + 1, k + 1)**2)
            ! !
            ! !Q(:, [k, k + 1]) = matprod(Q(:, [k, k + 1]), transpose(G))
            ! !
            ! !R([k, k + 1], k:n) = matprod(G, R([k, k + 1], k:n))
            ! !R(1:k + 1, [k, k + 1]) = R(1:k + 1, [k + 1, k])
            ! !R([k, k + 1], k) = [hypt, ZERO]
            !----------------------------------------------------------------------------------------------!
        end do
        ! GCOVR_EXCL_STOP

    end subroutine qrexc_Rfull

    !> Replaces one interpolation point in the factorisation `[bmat, zmat, idz]`; upstream's
    !> `updateh`, NEWUOA's and LINCOA's form.
    !!
    !! Section 4 of the NEWUOA paper. `xpt(:, knew)` is replaced by `xpt(:, kref) + d`, and `H` --
    !! the inverse of the KKT system of the least-Frobenius-norm interpolation problem, equation
    !! (3.12) -- is updated with it. `zmat` factorises the leading `npt`-by-`npt` block as
    !! `ZMAT*Diag(S)*ZMAT^T`, `S` negative below `idz`; `bmat` holds the last `n` rows of `H` less
    !! its `(npt+1)`-th column.
    !!
    !! BOBYQA's own `updateh` in `parquet_prima_bobyqb` is a different procedure: it has no `idz`.
    !!
    !! Upstream's note on `kref` is worth keeping: in exact arithmetic the update does not depend on
    !! it, but computing `vlag` and `beta` from `xpt(:, knew)` instead was observed on 2022-04-12 to
    !! put significant error into `H`.
    subroutine updateh(knew, kref, d, xpt, idz, bmat, zmat, info)
        ! Common modules

        integer, intent(in) :: knew                  !! index in `xpt` of the point being replaced
        integer, intent(in) :: kref                  !! the reference point the step is taken from, normally the best one
        real(real64), intent(in) :: d(:)             !! the step from `xpt(:, kref)` to the new point
        real(real64), intent(in) :: xpt(:, :)        !! the interpolation set, one point per column

        integer, intent(inout) :: idz                !! the factorisation's sign split, updated
        real(real64), intent(inout) :: bmat(:, :)    !! the factorisation's B, updated in place
        real(real64), intent(inout) :: zmat(:, :)    !! the factorisation's Z, updated in place

        integer, intent(out), optional :: info  !! `DAMAGING_ROUNDING` when the update went non-finite; absent means do not report

        integer :: j
        integer :: ja
        integer :: jb
        integer :: jl
        integer :: n
        integer :: npt
        real(real64) :: alpha
        real(real64) :: beta
        real(real64) :: denom
        real(real64) :: grot(2, 2)
        real(real64) :: hcol(size(bmat, 2))
        real(real64) :: scala
        real(real64) :: scalb
        real(real64) :: sqrtdn
        real(real64) :: tau
        real(real64) :: temp
        real(real64) :: tempa
        real(real64) :: tempb
        real(real64) :: v1(size(bmat, 1))
        real(real64) :: v2(size(bmat, 1))
        real(real64) :: vlag(size(bmat, 2))

        ! Debugging variables
        !real(real64) :: beta_test
        !real(real64) :: tol
        !real(real64), allocatable :: vlag_test(:)
        !real(real64), allocatable :: xpt_test(:, :)

        n = int(size(xpt, 1))
        npt = int(size(xpt, 2))

        ! LINCOA, this procedure's only caller in the tier, does not pass `info` and screens
        ! `knew` itself before calling, so neither arm below runs. BOBYQA's own `updateh`, in
        ! `parquet_prima_bobyqb`, is a different procedure and does pass one.
        if (present(info)) then
            info = INFO_DFT                       ! GCOVR_EXCL_LINE -- no caller passes `info`
        end if

        ! We must not do anything if KNEW is 0. This can only happen sometimes after a trust-region step.
        if (knew <= 0) then  ! KNEW < 0 is impossible if the input is correct.
            return                                ! GCOVR_EXCL_LINE -- `knew > 0` at every call
        end if

        ! Set the first NPT components of HCOL to the leading elements of the KNEW-th column of H. Powell's
        ! code does this after ZMAT is rotated blow, which saves flops but also introduces rounding errors.
        hcol(1:npt) = omega_col(idz, zmat, knew)
        hcol(npt + 1:npt + n) = bmat(:, knew)

        ! Calculate VLAG and BETA according to D.
        ! VLAG contains the components of the vector H*w of the updating formula (4.11) in the NEWUOA paper,
        ! and BETA holds the value of the parameter that has this name.
        ! N.B.: Powell's original comments mention that VLAG is "the vector THETA*WCHECK + e_b of the
        ! updating formula (6.11)", which does not match the published version of the NEWUOA paper.
        vlag = calvlag(kref, bmat, d, xpt, zmat, idz)
        beta = calbeta(kref, bmat, d, xpt, zmat, idz)  ! Nonnegative in precise arithmetic.

        ! Calculate the parameters of the updating formula (4.18)--(4.20) in the NEWUOA paper.
        alpha = hcol(knew)  ! Nonnegative in precise arithmetic.
        tau = vlag(knew)  ! Nonzero due to the definition of KNEW.
        denom = alpha * beta + tau**2  ! Positive in precise arithmetic.

        ! After the following line, VLAG = H*w - e_KNEW in the NEWUOA paper (where t = KNEW).
        vlag(knew) = vlag(knew) - ONE

        ! Quite rarely, due to rounding errors, VLAG or BETA may not be finite, and ABS(DENOM) may not be
        ! positive. In such cases, [BMAT, ZMAT] would be destroyed by the update, and hence we would rather
        ! not update them at all. Or should we simply terminate the algorithm?
        if (.not. (is_finite(sum(abs(hcol)) + sum(abs(vlag)) + abs(beta)) .and. abs(denom) > 0)) then
            if (present(info)) then               ! GCOVR_EXCL_START -- upstream's rounding
                info = DAMAGING_ROUNDING          ! rescue: a non-finite value aborts in
            end if                                ! `evaluate` before it can reach the model,
            return                                ! and no caller here passes `info`
        end if                            ! GCOVR_EXCL_STOP

        ! Update the matrix BMAT. It implements the last N rows of (4.11) in the NEWUOA paper.
        v1 = (alpha * vlag(npt + 1:npt + n) - tau * hcol(npt + 1:npt + n)) / denom
        v2 = (-beta * hcol(npt + 1:npt + n) - tau * vlag(npt + 1:npt + n)) / denom
        bmat = bmat + outprod(v1, vlag) + outprod(v2, hcol) !call r2update(bmat, ONE, v1, vlag, ONE, v2, hcol)
        ! N.B.: The use of OUTPROD is expensive memory-wise, but it is not our concern in this implementation.
        ! Numerically, the update above does not guarantee BMAT(:, NPT+1 : NPT+N) to be symmetric.
        call symmetrize(bmat(:, npt + 1:npt + n))

        ! Apply Givens rotations to put zeros in the KNEW-th row of ZMAT and set JL. After this,
        ! ZMAT(KNEW, :) contains at most two nonzero entries ZMAT(KNEW, 1) and ZMAT(KNEW, JL), one
        ! corresponding to all the columns of ZMAT that has a coefficient -1 in the factorization of
        ! OMEGA (if any), and the other corresponding to all the columns with +1. In specific,
        ! 1. If IDZ = 1 (all coefficients are +1 for the columns of ZMAT in the factorization of OMEGA ) or
        ! NPT - N (all the coefficients are -1), then JL = 1, and ZMAT(KNEW, 1) is L2-norm of ZMAT(KNEW, :);
        ! 2. If 2 <= IDZ <= NPT - N -1, then JL = IDZ, and ZMAT(KNEW, 1) is L2-norm of ZMAT(KNEW, 1 : IDZ-1),
        ! while ZMAT(KNEW, JL) is L2 norm of ZMAT(KNEW, IDZ : NPT-N-1).
        ! See (4.15)--(4.17) of the NEWUOA paper and the elaboration around them.
        jl = 1  ! In the loop below, if 2 <= J < IDZ, then JL = 1; if IDZ < J <= NPT-N-1, then JL = IDZ.
        do j = 2, npt - n - 1
            ! `idz` is the sign split of the ZMAT factorisation, and it only ever leaves 1 when
            ! `denom < 0` at the foot of this procedure -- see the exclusion there. While it
            ! stays 1 this arm cannot be selected, since the loop starts at j = 2.
            if (j == idz) then
                jl = idz  ! GCOVR_EXCL_START -- `idz` stays 1; see the note above, and below
                cycle
            end if                        ! GCOVR_EXCL_STOP

            ! Powell's condition in NEWUOA/LINCOA for the IF ... THEN below: IF (ZMAT(KNEW, J) /= 0) THEN
            ! A possible alternative: IF (ABS(ZMAT(KNEW, J)) > 1.0E-20 * ABS(ZMAT(KNEW, JL))) THEN
            if (abs(zmat(knew, j)) > 1.0E-20 * maxval(abs(zmat))) then  ! Threshold comes from Powell's BOBYQA
                ! Multiply a Givens rotation to ZMAT from the right so that ZMAT(KNEW, [JL,J]) becomes [*,0].
                grot = planerot(zmat(knew, [jl, j]))  ! MATLAB: grot = planerot(zmat(knew, [jl, j])')
                zmat(:, [jl, j]) = matprod(zmat(:, [jl, j]), transpose(grot))
            end if
            zmat(knew, j) = ZERO
        end do

        sqrtdn = sqrt(abs(denom))

        if (jl == 1) then
            ! Complete the updating of ZMAT when there is only 1 nonzero in ZMAT(KNEW, :) after the rotation.
            ! This is the normal case, as IDZ = 1 in precise arithmetic; it also covers the rare case that
            ! IDZ = NPT-N, meaning that OMEGA = -ZMAT*ZMAT^T. See (4.18) of the NEWUOA paper for details.
            ! Note that (4.18) updates Z_{NPT-N-1}, but the code here updates ZMAT(:, 1). Correspondingly,
            ! we implicitly update S_1 to SIGN(DENOM)*S_1 according to (4.18). If IDZ = NPT-N before the
            ! update, then IDZ is reduced by 1, and we need to switch ZMAT(:, 1) and ZMAT(:, IDZ) to maintain
            ! that S_J = -1 iff 1 <= J < IDZ, which is done after the END IF together with another case.

            !----------------------------------------------------------------------------------------------!
            ! Up to now, TEMPA = ZMAT(KNEW, 1) if IDZ = 1 and TEMPA = -ZMAT(KNEW, 1) if IDZ >= 2. However,
            ! according to (4.18) of the NEWUOA paper, TEMPB should always be ZMAT(KNEW, 1)/SQRTDN
            ! regardless of IDZ. Therefore, the following definition of TEMPB is inconsistent with (4.18).
            ! This is probably a BUG. See also Lemma 4 and (5.13) of Powell's paper "On updating the inverse
            ! of a KKT matrix". However, the inconsistency is hardly observable in practice, because JL = 1
            ! implies IDZ = 1 in precise arithmetic.
            !--------------------------------------------!
            ! !tempb = tempa/sqrtdn
            ! !tempa = tau/sqrtdn
            !--------------------------------------------!
            ! Here is the corrected version (only TEMPB is changed).
            tempa = tau / sqrtdn
            tempb = zmat(knew, 1) / sqrtdn
            !----------------------------------------------------------------------------------------------!

            ! The following line updates ZMAT(:, 1) according to (4.18) of the NEWUOA paper.
            zmat(:, 1) = tempa * zmat(:, 1) - tempb * vlag(1:npt)

            !----------------------------------------------------------------------------------------------!
            ! Zaikun 20220411: The update of IDZ is decoupled from the update of ZMAT, located after END IF.
            !----------------------------------------------------------------------------------------------!
            ! The following six lines from Powell's NEWUOA code are obviously problematic --- SQRTDN is
            ! always nonnegative. According to (4.18) of the NEWUOA paper, "SQRTDN < 0" and "SQRTDN >= 0"
            ! below should be both revised to "DENOM < 0". See also the corresponding part of the LINCOA
            ! code. Note that the NEWUOA paper uses SIGMA to denote DENOM. Check also Lemma 4 and (5.13) of
            ! Powell's paper "On updating the inverse of a KKT matrix". Note that the BOBYQA code does not
            ! have this part, as it does not have IDZ at all.
            ! !if (idz == 1 .and. sqrtdn < 0) then
            ! !    idz = 2
            ! !end if
            ! !if (idz >= 2 .and. sqrtdn >= 0) then
            ! !    reduce_idz = .true.
            ! !end if
            ! This is the corrected version, copied from LINCOA.
            ! !if (denom < 0) then
            ! !    if (idz == 1) then
            ! !        idz = 2
            ! !    else
            ! !        reduce_idz = .true.
            ! !    end if
            ! !end if
            !----------------------------------------------------------------------------------------------!
        else
            ! Complete the updating of ZMAT in the alternative case: ZMAT(KNEW, :) has 2 nonzeros. See (4.19)
            ! and (4.20) of the NEWUOA paper.
            ! First, set JA and JB so that ZMAT(: [JA, JB]) corresponds to [Z_1, Z_2] in (4.19) when BETA>=0,
            ! and corresponds to [Z2, Z1] in (4.20) when BETA<0. In this way, the update of ZMAT(:, [JA, JB])
            ! follows the same scheme regardless of BETA. Indeed, since S_1 = 1 and S_2 = -1 in (4.19)-(4.20)
            ! as elaborated above the equations, ZMAT(:, [1, JL]) always correspond to [Z_2, Z_1].
            ! This whole arm needs ZMAT(KNEW, :) to have TWO nonzeros, which happens only when
            ! `jl /= 1` -- that is, when `idz` has left 1. It has not in any run the suite
            ! drives; see the exclusion on the `denom < 0` revision below, which is the only
            ! thing that moves it.
            ! GCOVR_EXCL_START -- needs idz > 1; see the note above
            if (beta >= 0) then  ! ZMAT(:, [JA, JB]) corresponds to [Z_1, Z_2] in (4.19)
                ja = jl
                jb = 1
            else  ! ZMAT(:, [JA, JB]) corresponds to [Z_2, Z_1] in (4.20)
                ja = 1
                jb = jl
            end if
            ! Now update ZMAT(:, [ja, jb]) according to (4.19)--(4.20) of the NEWUOA paper.
            temp = zmat(knew, jb) / denom
            !tempa = temp * beta
            !tempb = temp * tau
            tempa = (beta / denom) * zmat(knew, jb)
            tempb = (tau / denom) * zmat(knew, jb)
            temp = zmat(knew, ja)
            scala = ONE / sqrt(abs(beta) * temp**2 + tau**2)  ! 1/SQRT(ZETA) in (4.19)-(4.20) of NEWUOA paper
            scalb = scala * sqrtdn
            zmat(:, ja) = scala * (tau * zmat(:, ja) - temp * vlag(1:npt))
            zmat(:, jb) = scalb * (zmat(:, jb) - tempa * hcol(1:npt) - tempb * vlag(1:npt))
            ! GCOVR_EXCL_STOP

            !----------------------------------------------------------------------------------------------!
            ! Zaikun 20220411: The update of IDZ is decoupled from the update of ZMAT, located after END IF.
            !----------------------------------------------------------------------------------------------!
            ! If and only if DENOM < 0, IDZ will be revised according to the sign of BETA.
            ! See (4.19)--(4.20) of the NEWUOA paper.
            ! !if (denom < 0) then
            ! !    if (beta < 0) then
            ! !        idz = idz + 1
            ! !    else
            ! !        reduce_idz = .true.
            ! !    end if
            ! !end if
            !----------------------------------------------------------------------------------------------!
        end if

        !--------------------------------------------------------------------------------------------------!
        ! Zaikun 20220411: The update of IDZ is decoupled from the update of ZMAT, located right below.
        !--------------------------------------------------------------------------------------------------!
        ! IDZ is reduced in the following case. Then exchange ZMAT(:, 1) and ZMAT(:, IDZ).
        ! !if (reduce_idz) then
        ! !    idz = idz - 1
        ! !    if (idz > 1) then
        ! !        zmat(:, [1, idz]) = zmat(:, [idz, 1])
        ! !    end if
        ! !end if
        !--------------------------------------------------------------------------------------------------!

        ! According to (4.18) and (4.19)--(4.20) of the NEWUOA paper, the coefficients {S_J} need update iff
        ! DENOM < 0, in which case one of the S_J will flip the sign when multiplied by SIGN(DENOM), leading
        ! to an increase of IDZ (if S_J flipped from 1 to -1) or a decrease (if S_J flipped from -1 to 1).
        ! `denom` is positive in exact arithmetic -- it is `alpha*beta + tau**2` with `alpha`
        ! and `beta` nonnegative -- and goes negative only under the rounding upstream guards
        ! against above. It has not done so in any run the suite drives, which is why `idz`
        ! stays 1 and the arms keyed on it, here and above, do not run.
        if (denom < 0) then
            ! GCOVR_EXCL_START -- `denom > 0`; see the note above
            if (idz == 1 .or. (idz < npt - n .and. beta < 0)) then  ! (4.18), (4.20) of the NEWUOA paper
                idz = idz + 1
            elseif (idz == npt - n .or. (idz > 1 .and. beta >= 0)) then  ! (4.18), (4.19) of the NEWUOA paper
                idz = idz - 1
                ! Exchange ZMAT(:, 1) and ZMAT(:. IDZ) if IDZ > 1. Why? No matter whether the update is
                ! given by (4.18) (IDZ = NPT-N) or (4.19) (1 < IDZ < NPT-N and BETA >= 0), we have S_1 = +1
                ! and S_{IDZ} = -1 at this moment (unless IDZ = 1). Thus we need to exchange ZMAT(:, 1) with
                ! ZMAT(:, IDZ) and implicitly S_1 with S_IDZ to maintain that S_J = -1 iff 1 <= J < IDZ.
                ! Note that, in the case of (4.18), ZMAT(:, 1) (and implicitly S_1) rather than
                ! ZMAT(:, NPT-N-1) was updated by the code above.
                if (idz > 1) then
                    zmat(:, [1, idz]) = zmat(:, [idz, 1])
                end if                    ! GCOVR_EXCL_STOP
            end if
        end if

    end subroutine updateh

end module parquet_prima_powalg
