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
!! **What is not vendored here.** `qradd` and `qrexc` (LINCOA's QR updates), `updateh` and `errh`
!! (NEWUOA's and LINCOA's factorisation update -- BOBYQA has its own, in `parquet_prima_bobyqb`),
!! `errquad`, `omega_col`, `omega_inprod`, `calvlag_qint` and `quadinc_ghv` (UOBYQA's, or reached
!! only from upstream's `assert`s). `setij` loses its optional `sorting_direction`: BOBYQA never
!! passes one, and the sort it would need went with it.
module parquet_prima_powalg

    use, intrinsic :: iso_fortran_env, only : real64
    use parquet_prima_linalg, only : ZERO, HALF, ONE, EPS, REALMAX, inprod, matprod, outprod, &
        trueloc, is_finite, DAMAGING_ROUNDING, INFO_DFT

    implicit none
    private

    public :: quadinc, hess_mul, omega_mul, calvlag, calbeta, calden, setij

    interface quadinc
        module procedure quadinc_d0
    end interface quadinc

    interface calvlag
        module procedure calvlag_lfqint
    end interface calvlag

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

end module parquet_prima_powalg
