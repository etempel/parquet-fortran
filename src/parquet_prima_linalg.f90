!> The base module of the vendored PRIMA tier: the fixed kinds, PRIMA's named constants and
!> exit codes, the IEEE predicates its `infnan` layer provided, the one abort every engine here
!> reaches, and PRIMA's `linalg_mod` -- the small dense linear algebra Powell's methods are built
!> on.
!!
!! **Provenance.** Derived from PRIMA (Reference Implementation for Powell's methods with
!! Modernization and Amelioration, Zaikun Zhang, `https://github.com/libprima/prima`),
!! BSD-3-Clause, at commit `43863c69`. The procedures here come from that tree's
!! `fortran/common/linalg.f90`; the constants from `fortran/common/consts.F90` and `ppf.h`, the
!! codes from `fortran/common/infos.f90`, and the predicates from `fortran/common/infnan.F90`,
!! `huge.F90` and `inf.F90`.
!!
!! BSD-3-Clause, as carried by the upstream repository:
!!
!! Redistribution and use in source and binary forms, with or without modification, are permitted
!! provided that the following conditions are met: (1) Redistributions of source code must retain
!! the above copyright notice, this list of conditions and the following disclaimer.
!! (2) Redistributions in binary form must reproduce the above copyright notice, this list of
!! conditions and the following disclaimer in the documentation and/or other materials provided
!! with the distribution. (3) Neither the name of the copyright holder nor the names of its
!! contributors may be used to endorse or promote products derived from this software without
!! specific prior written permission. THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
!! CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
!! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN
!! NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
!! INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
!! PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
!! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
!! LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
!! THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
!!
!! **There is no upstream sync path: this is a fork, pinned at that commit.** The header of every
!! file of this tier records it, as `parquet_integrate_engine.f90` records QUADPACK's, because
!! nothing automated will ever tell you that upstream has moved. Re-vendoring is a deliberate act:
!! re-run `feature_optimizer.md` 3.4's audit at the new commit first, since every count in it is
!! commit-specific.
!!
!! **A bug found here is checked against upstream before it is fixed here.** The arithmetic is a
!! transcription: the Givens rotation's branch structure, the scaled p-norm, the rank-one and
!! rank-two updates and the bubble sort are upstream's, and a difference from upstream is a defect
!! in this file unless it is one of the deviations below.
!!
!! **Deviations from the upstream text**, shared by every `parquet_prima_*` engine file and listed
!! once here:
!!
!! 1. `RP` is `real64` and `IK` is the default `integer`, fixed. `ppf.h`, the six `.F90` files and
!!    every `#if` are gone, and `consts.F90`'s constants are the `parameter`s below. This
!!    repository compiles one precision and `fpm.toml` runs cpp over every file, so a header of
!!    per-precision macros has nothing to select and could not be included.
!! 2. `fprint.f90`, `message.f90`, `string.f90` and `iprint` are not vendored: this tier prints
!!    nothing. Whatever PRIMA reports through `iprint` is reported here through `info=`.
!! 3. Every `call assert` is deleted -- upstream's own development instrumentation, compiled out
!!    unless `PRIMA_DEBUGGING = 1`. Every `call validate` is kept, as an abort through
!!    `prima_abort` carrying the message this library's callers see.
!! 4. `pintrf.f90`'s `calfun`/`calcfc` are replaced by `pf_objective`/`pf_constrained_objective`
!!    (`parquet_prima_common`), and `moderatef`/`moderatex`/`moderatec` by the refusal of a
!!    non-finite value: it never reaches the model.
!! 5. `memory.F90`'s `safealloc` is plain `allocate` with `int64` extents; `huge.F90`, `inf.F90`
!!    and `infnan.F90` are the `ieee_arithmetic` predicates below.
!! 6. Only what the three vendored solvers reach is vendored: about half of `linalg_mod` and a
!!    third of `powalg_mod`, and of `common` only the files 6.5 names. Whatever NEWUOA and UOBYQA
!!    alone use is not here and is not to be added (Q15). `qr`, `istriu`, `istril` and `isbanded`
!!    are private, because nothing outside this module calls them once upstream's `call assert`
!!    sites are gone. `norm` keeps upstream's generic name over two specifics -- the Euclidean
!!    norm and the named one -- of which `norm_2` is the one name in this file that upstream does
!!    not have, because upstream's `p_norm` takes an exponent this one does not.
!! 7. Every procedure and dummy carries its `!>`/`!!`; upstream's rationale comments are kept as
!!    plain `!` blocks, its `!====! Calculation starts !====!` decoration is not, and every line
!!    is inside 132 columns with no `/` immediately followed by `*` (cpp runs over this file).
!!
!! **Every public name here is upstream's**, unprefixed: `inprod`, `trueloc`, `ZERO`, `EPS`.
!! That is deliberate -- the value of a vendored engine is that it can be diffed against the
!! commit it came from -- and it is why `choosing-a-module.md` puts the `parquet_prima_*` engine
!! modules outside the stability promise and the guide tells nobody to import them. Everything in
!! this tier imports them with an `only:` list.
!!
!! **Why the constants are here rather than in `parquet_prima_common`.** `parquet_prima_common`'s
!! own procedures (`xinbd`, `shiftbase`, `evaluate`, `interval_max`, `history`) call the linear
!! algebra below, so this module has to compile first, and Fortran has no circular `use`. The
!! ten-file layout has no room for a separate constants module, so the tier's floor is here.
module parquet_prima_linalg

    use, intrinsic :: iso_fortran_env, only : real64, int64
    use, intrinsic :: ieee_arithmetic, only : ieee_is_nan, ieee_is_finite

    implicit none
    private

    public :: ZERO, ONE, TWO, HALF, QUART, TEN, TENTH
    public :: PI, EPS, REALMIN, REALMAX, BOUNDMAX
    public :: MAXPOW10, FUNCMAX, CONSTRMAX, TINYCV
    public :: RHOBEG_DFT, RHOEND_DFT, FTARGET_DFT, MAXFUN_DIM_DFT
    public :: CTOL_DFT, CWEIGHT_DFT, MAXFILT_DFT
    public :: ETA1_DFT, ETA2_DFT, GAMMA1_DFT, GAMMA2_DFT
    public :: INFO_DFT, SMALL_TR_RADIUS, FTARGET_ACHIEVED
    public :: TRSUBP_FAILED, MAXFUN_REACHED, MAXTR_REACHED
    public :: NAN_INF_X, NAN_INF_F, NAN_INF_MODEL
    public :: NO_SPACE_BETWEEN_BOUNDS, DAMAGING_ROUNDING
    public :: is_nan, is_finite, is_inf, is_posinf, is_neginf
    public :: prima_abort
    public :: inprod, matprod, outprod, r1update, r2update, symmetrize
    public :: diag, planerot, hypotenuse, trueloc, norm, linspace, smat_mul_vec, int
    public :: eye, inv, solve, lsqr, isminor, maximum

    ! ---- PRIMA's named constants (`common/consts.F90`) ----

    !> PRIMA's `ZERO`.
    real(real64), parameter :: ZERO = 0.0_real64
    !> PRIMA's `ONE`.
    real(real64), parameter :: ONE = 1.0_real64
    !> PRIMA's `TWO`.
    real(real64), parameter :: TWO = 2.0_real64
    !> PRIMA's `HALF`.
    real(real64), parameter :: HALF = 0.5_real64
    !> PRIMA's `QUART`.
    real(real64), parameter :: QUART = 0.25_real64
    !> PRIMA's `TEN`.
    real(real64), parameter :: TEN = 10.0_real64
    !> PRIMA's `TENTH`.
    real(real64), parameter :: TENTH = 0.1_real64
    !> PRIMA's `PI`, to more digits than `real64` carries.
    real(real64), parameter :: PI = &
        3.1415926535897932384626433832795028841971693993751058209749445923078_real64
    !> PRIMA's `EPS`: the relative spacing of the model's arithmetic.
    real(real64), parameter :: EPS = epsilon(0.0_real64)
    !> PRIMA's `REALMIN`: the smallest positive normal magnitude.
    real(real64), parameter :: REALMIN = tiny(0.0_real64)
    !> PRIMA's `REALMAX`: the largest finite magnitude.
    real(real64), parameter :: REALMAX = huge(0.0_real64)
    !> PRIMA's `BOUNDMAX`: a bound at or beyond this magnitude is treated as absent.
    real(real64), parameter :: BOUNDMAX = QUART * REALMAX
    !> PRIMA's `MAXPOW10`: the decimal exponent range of the working precision.
    integer, parameter :: MAXPOW10 = range(0.0_real64)
    !> PRIMA's `HALF_MAXPOW10`.
    integer, parameter :: HALF_MAXPOW10 = floor(real(MAXPOW10) / 2.0)
    !> PRIMA's `FUNCMAX`: the magnitude beyond which upstream moderates an objective value.
    !!
    !! Nothing here moderates (deviation 4), so this is used only where the constrained solvers
    !! compare against it -- `selectx`'s reference values in `parquet_prima_common`.
    real(real64), parameter :: FUNCMAX = TEN**max(4, min(30, HALF_MAXPOW10))
    !> PRIMA's `CONSTRMAX`: `FUNCMAX`'s counterpart for a constraint value.
    real(real64), parameter :: CONSTRMAX = FUNCMAX
    !> PRIMA's `TINYCV`: the positive value LINCOA gives a residual that is zero but not active.
    real(real64), parameter :: TINYCV = TEN**max(-60, -MAXPOW10)

    !> PRIMA's `RHOBEG_DFT`: the default initial trust-region radius.
    real(real64), parameter :: RHOBEG_DFT = ONE
    !> PRIMA's `RHOEND_DFT`: the default final trust-region radius.
    real(real64), parameter :: RHOEND_DFT = 1.0e-6_real64
    !> PRIMA's `FTARGET_DFT`: no target, i.e. one no finite value can reach.
    real(real64), parameter :: FTARGET_DFT = -REALMAX
    !> PRIMA's `MAXFUN_DIM_DFT`: the default evaluation budget is this times the dimension.
    integer, parameter :: MAXFUN_DIM_DFT = 500
    !> PRIMA's `CTOL_DFT`: a point whose constraint violation is at or below this is feasible.
    real(real64), parameter :: CTOL_DFT = sqrt(EPS)
    !> PRIMA's `CWEIGHT_DFT`: how heavily the filter weighs violation against value.
    real(real64), parameter :: CWEIGHT_DFT = TEN**min(8, MAXPOW10)
    !> PRIMA's `MIN_MAXFILT`: the shortest filter upstream recommends.
    integer, parameter :: MIN_MAXFILT = 200
    !> PRIMA's `MAXFILT_DFT`: how many points the constrained solvers' filter keeps.
    integer, parameter :: MAXFILT_DFT = 10 * MIN_MAXFILT
    !> PRIMA's `ETA1_DFT`: below this reduction ratio the trust-region radius contracts.
    real(real64), parameter :: ETA1_DFT = TENTH
    !> PRIMA's `ETA2_DFT`: above this reduction ratio it expands.
    real(real64), parameter :: ETA2_DFT = 0.7_real64
    !> PRIMA's `GAMMA1_DFT`: the contraction factor.
    real(real64), parameter :: GAMMA1_DFT = HALF
    !> PRIMA's `GAMMA2_DFT`: the expansion factor.
    real(real64), parameter :: GAMMA2_DFT = TWO

    ! ---- PRIMA's exit codes (`common/infos.f90`) ----

    !> PRIMA's `INFO_DFT`: the solver should not exit yet.
    integer, parameter :: INFO_DFT = 0
    !> PRIMA's `SMALL_TR_RADIUS`: the trust-region radius reached `rhoend`, its normal exit.
    integer, parameter :: SMALL_TR_RADIUS = 0
    !> PRIMA's `FTARGET_ACHIEVED`: a value at or below the caller's target was found.
    integer, parameter :: FTARGET_ACHIEVED = 1
    !> PRIMA's `TRSUBP_FAILED`: the trust-region subproblem solver failed.
    integer, parameter :: TRSUBP_FAILED = 2
    !> PRIMA's `MAXFUN_REACHED`: the evaluation budget ran out.
    integer, parameter :: MAXFUN_REACHED = 3
    !> PRIMA's `MAXTR_REACHED`: the trust-region iteration cap ran out.
    integer, parameter :: MAXTR_REACHED = 20
    !> PRIMA's `NAN_INF_X`: a non-finite point was reached.
    integer, parameter :: NAN_INF_X = -1
    !> PRIMA's `NAN_INF_F`: a non-finite objective value was returned.
    integer, parameter :: NAN_INF_F = -2
    !> PRIMA's `NAN_INF_MODEL`: the interpolation model went non-finite.
    integer, parameter :: NAN_INF_MODEL = -3
    !> PRIMA's `NO_SPACE_BETWEEN_BOUNDS`: some `upper - lower` is below `2*rhobeg`.
    integer, parameter :: NO_SPACE_BETWEEN_BOUNDS = 6
    !> PRIMA's `DAMAGING_ROUNDING`: rounding errors make the model unimprovable.
    integer, parameter :: DAMAGING_ROUNDING = 7

    ! ---- The generic interfaces upstream declares ----

    interface matprod
        module procedure matprod12, matprod21, matprod22
    end interface matprod

    interface r1update
        module procedure r1_sym, r1
    end interface r1update

    interface r2update
        module procedure r2_sym, r2
    end interface r2update

    interface linspace
        module procedure linspace_r, linspace_i
    end interface linspace

    interface eye
        module procedure eye1, eye2
    end interface eye

    interface lsqr
        module procedure lsqr_Rdiag, lsqr_Rfull
    end interface lsqr

    interface isminor
        module procedure isminor0, isminor1
    end interface isminor

    interface maximum
        module procedure maximum1, maximum2
    end interface maximum

    interface norm
        module procedure norm_2, named_norm_vec
    end interface norm

    interface int
        module procedure logical_to_int
    end interface int

contains

    !> Aborts the whole run from inside a vendored PRIMA engine.
    !!
    !! The tier's one fatal exit. It carries the same shape as `parquet_optimize`'s own abort --
    !! the entry point the caller named, what went wrong, and the caller's `context` capped to 100
    !! characters -- and it enters the SAME named critical, `parquet_optimize_abort`. That name is
    !! shared deliberately: a `pf_bobyqa_solver` runs under `pf_minimize_multistart`'s threads, so
    !! an abort raised here and one raised in `parquet_optimize` can be reached concurrently by
    !! different threads of one team, and two threads reaching `error stop` at once leave the exit
    !! status nondeterministic under ifx. One critical across both modules is what makes exactly
    !! one of them win.
    !!
    !! Impure by nature; every caller is a validation site, never a hot path.
    subroutine prima_abort(entry_point, text, context)
        character(len=*), intent(in)           :: entry_point !! the generic the caller named
        character(len=*), intent(in)           :: text        !! what went wrong
        character(len=*), intent(in), optional :: context     !! caller's call-site text

        !> Cap on caller-supplied context, as `parquet_optimize` uses (`api-conventions.md`).
        integer, parameter :: CONTEXT_CAP = 100
        character(len=:), allocatable :: msg

        msg = entry_point//": "//text
        if (present(context)) then
            if (len_trim(context) > CONTEXT_CAP) then
                msg = msg//" (context: "//context(1:CONTEXT_CAP)//"...)"
            else
                msg = msg//" (context: "//trim(context)//")"
            end if
        end if

        !$omp critical (parquet_optimize_abort)
        error stop msg
        !$omp end critical (parquet_optimize_abort)

    end subroutine prima_abort

    !> True where `x` is a NaN, PRIMA's `is_nan`.
    elemental function is_nan(x) result(y)
        real(real64), intent(in) :: x  !! the value
        logical                  :: y  !! `x` is a NaN

        y = ieee_is_nan(x)

    end function is_nan

    !> True where `x` is finite, PRIMA's `is_finite`.
    elemental function is_finite(x) result(y)
        real(real64), intent(in) :: x  !! the value
        logical                  :: y  !! `x` is neither infinite nor a NaN

        y = ieee_is_finite(x)

    end function is_finite

    !> True where `x` is an infinity of either sign, PRIMA's `is_inf`.
    elemental function is_inf(x) result(y)
        real(real64), intent(in) :: x  !! the value
        logical                  :: y  !! `x` is `+Infinity` or `-Infinity`

        y = (.not. ieee_is_finite(x)) .and. (.not. ieee_is_nan(x))

    end function is_inf

    !> True where `x` is `+Infinity`, PRIMA's `is_posinf`.
    elemental function is_posinf(x) result(y)
        real(real64), intent(in) :: x  !! the value
        logical                  :: y  !! `x` is `+Infinity`

        y = is_inf(x) .and. x > 0.0_real64

    end function is_posinf

    !> True where `x` is `-Infinity`, PRIMA's `is_neginf`.
    elemental function is_neginf(x) result(y)
        real(real64), intent(in) :: x  !! the value
        logical                  :: y  !! `x` is `-Infinity`

        y = is_inf(x) .and. x < 0.0_real64

    end function is_neginf


    !> The inner product `z = x^T y`, upstream's `inprod`.
    !!
    !! Written as a loop rather than `dot_product` for the reason upstream gives: the two differ in
    !! finite-precision arithmetic, and Powell's methods were verified against the loop.
    function inprod(x, y) result(z)
        real(real64), intent(in) :: x(:)    !! first vector
        real(real64), intent(in) :: y(:)    !! second vector, the same length
        real(real64) :: z                   !! the inner product
        integer :: i

        z = ZERO
        do i = 1, int(size(x))
            z = z + x(i) * y(i)
        end do

    end function inprod

    !> The matrix product of a row vector and a matrix, upstream's `matprod12`.
    !!
    !! `x` is an m-vector read as a row and `y` an m-by-n matrix, so `z` is an n-vector. A loop, not
    !! `matmul`, for the reason `inprod` gives.
    function matprod12(x, y) result(z)
        real(real64), intent(in) :: x(:)       !! the row, of length m
        real(real64), intent(in) :: y(:, :)    !! the matrix, m by n
        real(real64) :: z(size(y, 2))          !! the product, of length n
        integer :: j

        do j = 1, int(size(y, 2))
            ! When interfaced with MATLAB, the following seems more efficient than a loop, which is strange
            ! since inprod itself is implemented by a loop. This may depend on the machine (e.g., cache
            ! size), compiler, compiling options, and MATLAB version.
            z(j) = inprod(x, y(:, j))
        end do

    end function matprod12

    !> The matrix product of a matrix and a column vector, upstream's `matprod21`.
    function matprod21(x, y) result(z)
        real(real64), intent(in) :: x(:, :)    !! the matrix, m by n
        real(real64), intent(in) :: y(:)       !! the column, of length n
        real(real64) :: z(size(x, 1))          !! the product, of length m
        integer :: j

        z = ZERO
        do j = 1, int(size(x, 2))
            z = z + x(:, j) * y(j)
        end do

    end function matprod21

    !> The matrix product of two matrices, upstream's `matprod22`.
    function matprod22(x, y) result(z)
        real(real64), intent(in) :: x(:, :)          !! the left matrix, m by p
        real(real64), intent(in) :: y(:, :)          !! the right matrix, p by n
        real(real64) :: z(size(x, 1), size(y, 2))    !! the product, m by n
        integer :: i, j

        z = ZERO
        do j = 1, int(size(y, 2))
            do i = 1, int(size(x, 2))
                z(:, j) = z(:, j) + x(:, i) * y(i, j)
            end do
        end do

    end function matprod22

    !> The outer product `z = x y^T`, upstream's `outprod`.
    function outprod(x, y) result(z)
        real(real64), intent(in) :: x(:)       !! first vector, of length m
        real(real64), intent(in) :: y(:)       !! second vector, of length n
        real(real64) :: z(size(x), size(y))    !! the product, m by n
        integer :: i

        do i = 1, int(size(y))
            z(:, i) = x * y(i)
        end do

    end function outprod

    !> The rank-one update `A = A + alpha*(x y^T)`, upstream's `r1`.
    subroutine r1(A, alpha, x, y)
        real(real64), intent(in) :: alpha         !! the scalar weight
        real(real64), intent(in) :: x(:)          !! the left vector, of length m
        real(real64), intent(in) :: y(:)          !! the right vector, of length n
        real(real64), intent(inout) :: A(:, :)    !! the matrix, updated in place

        ! N.B.: The use of OUTPROD is expensive memory-wise, but it is not our concern in this implementation.
        A = A + outprod(alpha * x, y)
        !A = A + alpha * outprod(x, y)

    end subroutine r1

    !> The symmetric rank-one update `A = A + alpha*(x x^T)`, upstream's `r1_sym`.
    !!
    !! Only the lower triangle is computed and the result is mirrored, so the answer is exactly
    !! symmetric rather than symmetric up to rounding.
    subroutine r1_sym(A, alpha, x)
        real(real64), intent(in) :: alpha         !! the scalar weight
        real(real64), intent(in) :: x(:)          !! the vector
        real(real64), intent(inout) :: A(:, :)    !! the square matrix, updated in place
        integer :: n, j

        n = int(size(x))

        ! Only update the LOWER TRIANGULAR part of A.
        do j = 1, n
            A(j:n, j) = A(j:n, j) + alpha * x(j:n) * x(j)
        end do
        call symmetrize(A) ! Copy A(LOWER_TRI) to A(UPPER_TRI).

        ! For some reason, A + alpha*outprod(x,x), A + (outprod(alpha*x, x) + outprod(x, alpha*x))/2,
        ! A + symmetrize(x, alpha*x), or A + sign(alpha) * outprod(sqrt(|alpha|) * x, sqrt(|alpha|) * x)
        ! does not work as well as the above lines in NEWUOA, where SYMMETRIZE should copy A(LOWER_TRI)
        ! to A(UPPER_TRI) rather than set A = (A'+A)/2. When X is rather small or large, calculating
        ! OUTPROD(X, X) can be a bad idea, even though it guarantees symmetry in finite-precision arithmetic.

    end subroutine r1_sym

    !> The rank-two update `A = A + alpha*(x y^T) + beta*(u v^T)`, upstream's `r2`.
    subroutine r2(A, alpha, x, y, beta, u, v)
        real(real64), intent(in) :: alpha         !! weight of the first term
        real(real64), intent(in) :: beta          !! weight of the second term
        real(real64), intent(in) :: x(:)          !! left vector of the first term
        real(real64), intent(in) :: y(:)          !! right vector of the first term
        real(real64), intent(in) :: u(:)          !! left vector of the second term
        real(real64), intent(in) :: v(:)          !! right vector of the second term
        real(real64), intent(inout) :: A(:, :)    !! the matrix, updated in place

        ! N.B.: The use of OUTPROD is expensive memory-wise, but it is not our concern in this implementation.
        A = A + outprod(alpha * x, y) + outprod(beta * u, v)
        !A = A + (alpha * outprod(x, y) + beta * outprod(u, v))

    end subroutine r2

    !> The symmetric rank-two update `A = A + alpha*(x y^T + y x^T)`, upstream's `r2_sym`.
    subroutine r2_sym(A, alpha, x, y)
        real(real64), intent(in) :: alpha         !! the scalar weight
        real(real64), intent(in) :: x(:)          !! the first vector
        real(real64), intent(in) :: y(:)          !! the second vector
        real(real64), intent(inout) :: A(:, :)    !! the square matrix, updated in place
        integer :: n, j

        n = int(size(x))

        do j = 1, n
            A(j:n, j) = A(j:n, j) + alpha * x(j:n) * y(j) + alpha * y(j:n) * x(j)
        end do
        call symmetrize(A)  ! Copy A(LOWER_TRI) to A(UPPER_TRI).

        ! For some reason, A = A + ALPHA * (OUTPROD(X, Y) + OUTPROD(Y, X)) does not work as well as the
        ! above lines for NEWUOA, where SYMMETRIZE should copy A(LOWER_TRI) to A(UPPER_TRI), although
        ! ALPHA*( X*Y^T + Y*X^T) is guaranteed symmetric even in floating-point arithmetic.

    end subroutine r2_sym

    !> Makes `A` exactly symmetric, upstream's `symmetrize`.
    !!
    !! For a matrix that IS symmetric in exact arithmetic and asymmetric only through rounding or
    !! noise: the lower triangle is copied over the upper.
    subroutine symmetrize(A)
        real(real64), intent(inout) :: A(:, :)    !! the square matrix, symmetrised in place
        integer :: j

        ! A is symmetrized by copying A(LOWER_TRI) to A(UPPER_TRI).
        ! N.B.: The following assumes that A(LOWER_TRI) has been properly defined.
        do j = 1, int(size(A, 1))
            A(1:j - 1, j) = A(j, 1:j - 1)
        end do

    end subroutine symmetrize

    !> The `k`-th diagonal of a matrix, upstream's `diag`.
    !!
    !! `k = 0` (the default) is the main diagonal, `k > 0` is above it and `k < 0` below. When `|k|`
    !! exceeds the matrix, the result is a zero-length array.
    function diag(A, k) result(D)
        real(real64), intent(in) :: A(:, :)    !! the matrix
        integer, intent(in), optional :: k     !! which diagonal; default 0, the main one
        real(real64), allocatable :: D(:)      !! the diagonal, as a rank-one array
        integer :: dlen
        integer :: i
        integer :: k_loc

        if (present(k)) then
            k_loc = k
        else
            k_loc = 0
        end if

        ! DLEN is the length of D. We allow |K| to exceed the number of rows/columns in A.
        dlen = max(0, int(min(size(A, 1), size(A, 2)) - abs(k_loc)))
        allocate(D(int(dlen, int64)))
        if (k_loc >= 0) then
            D = [(A(i, i + k_loc), i=1, dlen)]
        else
            D = [(A(i - k_loc, i), i=1, dlen)]
        end if

    end function diag

    !> The 2-by-2 Givens rotation `G` with `G*x` in the direction of the first axis, upstream's
    !> `planerot`.
    !!
    !! Continuous in `x` except at the origin, and orthogonal for every input: where MATLAB returns
    !! a NaN matrix (a NaN in `x`, or `x` all infinite) this returns the identity or a matrix of
    !! `+/-sqrt(2)/2` instead, because the callers rely on `G` being a rotation.
    function planerot(x) result(G)
        real(real64), intent(in) :: x(:)    !! the 2-vector to rotate onto the first axis
        real(real64) :: G(2, 2)             !! the rotation, 2 by 2
        real(real64) :: c
        real(real64) :: s
        real(real64) :: r
        real(real64) :: t
        real(real64) :: u

        ! Define C = X(1) / R and S = X(2) / R with R = HYPOT(X(1), X(2)). Handle Inf/NaN, over/underflow.
        if (any(is_nan(x))) then
            ! In this case, MATLAB sets G to NaN(2, 2). We refrain from doing so to keep G orthogonal.
            c = ONE
            s = ZERO
        elseif (all(is_inf(x))) then
            ! In this case, MATLAB sets G to NaN(2, 2). We refrain from doing so to keep G orthogonal.
            c = sign(1 / sqrt(2.0_real64), x(1))
            s = sign(1 / sqrt(2.0_real64), x(2))
        elseif (abs(x(1)) <= 0 .and. abs(x(2)) <= 0) then ! X(1) == 0 == X(2).
            c = ONE
            s = ZERO
        elseif (abs(x(2)) <= EPS * abs(x(1))) then
            ! N.B.:
            ! 0. With <= instead of <, this case covers X(1) == 0 == X(2), which is treated above separately
            ! to avoid the confusing SIGN(., 0) (see 1).
            ! 1. SIGN(A, 0) = ABS(A) in Fortran but sign(0) = 0 in MATLAB, Python, Julia, and R!
            ! 2. Taking SIGN(X(1)) into account ensures the continuity of G with respect to X except at 0.
            c = sign(ONE, x(1))  ! MATLAB: c = sign(x(1))
            s = ZERO
        elseif (abs(x(1)) <= EPS * abs(x(2))) then
            ! N.B.: SIGN(A, X) = ABS(A) * sign of X /= A * sign of X ! Therefore, it is WRONG to define G
            ! as SIGN(RESHAPE([ZERO, -ONE, ONE, ZERO], [2, 2]), X(2)). This mistake was committed on
            ! 20211206 and took a whole day to debug! NEVER use SIGN on arrays unless you are really sure.
            c = ZERO
            s = sign(ONE, x(2))  ! MATLAB: s = sign(x(2))
        else
            ! Here is the normal case. It implements the Givens rotation in a stable & continuous way as in:
            ! Bindel, D., Demmel, J., Kahan, W., and Marques, O. (2002). On computing Givens rotations
            ! reliably and efficiently. ACM Transactions on Mathematical Software (TOMS), 28(2), 206-238.
            ! N.B.: 1. Modern compilers compute SQRT(REALMIN) and SQRT(REALMAX/2.1) at compilation time.
            ! 2. The direct calculation without involving T and U seems to work better; use it if possible.
            if (all(abs(x) > sqrt(REALMIN) .and. abs(x) < sqrt(REALMAX / 2.1_real64))) then
                ! Do NOT use HYPOTENUSE here; the best implementation for one may be suboptimal for the other
                r = norm(x)
                c = x(1) / r
                s = x(2) / r
            elseif (abs(x(1)) > abs(x(2))) then
                t = x(2) / x(1)
                u = maxval([ONE, abs(t), sqrt(ONE + t**2)])  ! MAXVAL: precaution against rounding error.
                u = sign(u, x(1))  ! MATLAB: u = sign(x(1))*sqrt(ONE + t**2)
                c = ONE / u
                s = t / u
            else
                t = x(1) / x(2)
                u = maxval([ONE, abs(t), sqrt(ONE + t**2)])  ! MAXVAL: precaution against rounding error.
                u = sign(u, x(2))  ! MATLAB: u = sign(x(2))*sqrt(ONE + t**2)
                c = t / u
                s = ONE / u
            end if
        end if

        G = reshape([c, -s, s, c], [2, 2])  ! MATLAB: G = [c, s; -s, c]

    end function planerot

    !> The Euclidean norm of a vector, upstream's `p_norm` at its default exponent.
    !!
    !! Computed on a scaled copy when the plain sum of squares overflows or underflows, so a
    !! vector whose square is not representable still gets its norm. `SCALMIN` and `SCALMAX` are
    !! bounded so that dividing by `scaling` cannot itself overflow -- and so that a compiler
    !! evaluating `1/scaling` first cannot produce a NaN, which upstream records happening with
    !! `flang -ffast-math`.
    !!
    !! **The one procedure here whose name is not upstream's.** Upstream's `p_norm` takes an
    !! optional exponent and answers the 0-, 1-, p- and infinity-norms from the same body; nothing
    !! in this tier passes one, so only this branch is vendored, and a procedure called `p_norm`
    !! that refuses a `p` would be worse than a renamed one. With the other branches went
    !! upstream's one `call validate` in this file -- an argument check with no argument left to
    !! check.
    function norm_2(x) result(y)
        real(real64), intent(in) :: x(:)  !! the vector
        real(real64)             :: y     !! the Euclidean norm

        real(real64) :: maxabs
        real(real64) :: scaling
        real(real64) :: scalmax
        real(real64) :: scalmin

        ! SIZE(X) = 0 would make MAXVAL(ABS(X)) equal -HUGE(X); the appended zero is upstream's
        ! guard against it.
        maxabs = maxval([abs(x), ZERO])

        if (size(x) == 0) then
            y = ZERO
        else if (.not. all(is_finite(x))) then
            ! A NaN in X makes Y a NaN; an infinity in X makes it +Infinity.
            y = sum(abs(x))
        else if (maxabs <= 0) then
            ! Only reached when X holds no NaN, so MAXABS = 0 does mean X is all zero.
            y = ZERO
        else
            y = sqrt(sum(x**2))
            if (is_posinf(y) .or. y <= 0) then
                scalmin = real(radix(ZERO), real64)**max(minexponent(ZERO) - 1, 1 - maxexponent(ZERO))
                scalmax = real(radix(ZERO), real64)**min(maxexponent(ZERO) - 1, 1 - minexponent(ZERO))
                scaling = min(max(maxabs, scalmin), scalmax)
                y = scaling * sqrt(sum((x / scaling)**2))
            end if
        end if

    end function norm_2

    !> A named norm of a vector, upstream's `named_norm_vec`.
    !!
    !! `'inf'` is the largest magnitude and `'fro'` the Euclidean norm, which is also what any
    !! other name gets. A NaN anywhere gives a NaN and an infinity gives `+Infinity`, before the
    !! name is even looked at, which is what makes the two norms agree on a degenerate vector.
    !!
    !! Upstream lowercases and strips the name through `string_mod` first. That module is not
    !! vendored (deviation 2: it exists for the printing layer), and every call site in this tier
    !! passes a lower-case literal, so the name is matched as it arrives.
    function named_norm_vec(x, nname) result(y)
        real(real64), intent(in)     :: x(:)   !! the vector
        character(len=*), intent(in) :: nname  !! `'inf'` or `'fro'`
        real(real64)                 :: y      !! the norm

        if (size(x) == 0) then
            y = ZERO
        else if (.not. all(is_finite(x))) then
            ! If X contains NaN, then Y is NaN. Otherwise, Y is Inf when X contains +/-Inf.
            y = sum(abs(x))
        else if (.not. any(abs(x) > 0)) then
            ! The following is incorrect without checking the last case, as X may be all NaN.
            y = ZERO
        else
            select case (nname)
            case ('inf')
                ! If SIZE(X) = 0, then MAXVAL(ABS(X)) = -HUGE(X); since we have handled such a
                ! case above, it is OK to write Y = MAXVAL(ABS(X)) here, but we append a 0 for
                ! robustness.
                y = maxval([abs(x), ZERO])
            case default
                y = norm_2(x)
            end select
        end if

    end function named_norm_vec

    !> The product of a packed symmetric matrix and a vector, upstream's `smat_mul_vec`.
    !!
    !! The upper triangle of the matrix is stored column by column in `smatv`, which is how Powell's
    !! methods carry the quadratic model's Hessian.
    function smat_mul_vec(smatv, x) result(y)
        real(real64), intent(in) :: smatv(:)    !! the matrix's upper triangle, packed column by column
        real(real64), intent(in) :: x(:)        !! the vector
        real(real64) :: y(size(x))              !! the product
        integer :: ih
        integer :: n
        integer :: j

        n = int(size(x))

        do j = 1, n
            ih = (j - 1) * j / 2
            y(j) = inprod(smatv(ih + 1:ih + j), x(1:j))
            y(1:j - 1) = y(1:j - 1) + x(j) * smatv(ih + 1:ih + j - 1)
        end do

    end function smat_mul_vec

    !> The indices where a logical array is true, in ascending order; upstream's `trueloc`.
    !!
    !! Fortran has no logical indexing, so `y(trueloc(mask))` is how upstream writes what MATLAB,
    !! Python, Julia and R write `y(mask)`.
    function trueloc(x) result(loc)
        logical, intent(in) :: x(:)       !! the mask
        integer, allocatable :: loc(:)    !! the indices where `x` is true
        integer :: n

        allocate(loc(int(count(x), int64)))
        n = int(size(x))
        loc = pack(linspace(1, n, n), mask=x)

    end function trueloc

    !> `n` evenly spaced values from `xstart` to `xstop`, upstream's `linspace_r`.
    !!
    !! The endpoints are set exactly rather than accumulated, so the last value is `xstop` however
    !! the spacing rounds.
    function linspace_r(xstart, xstop, n) result(x)
        real(real64), intent(in) :: xstart    !! the first value
        real(real64), intent(in) :: xstop     !! the last value
        integer, intent(in) :: n              !! how many values
        real(real64) :: x(max(n, 0))          !! the values
        integer :: i
        integer :: nm
        real(real64) :: xunit

        if (n <= 0) then ! Quick return when N <= 0.
            return
        end if

        nm = n - 1

        if (n == 1 .or. (xstart <= xstop .and. xstop <= xstart)) then
            x = xstop
        elseif (abs(xstart) <= abs(xstop) .and. abs(xstop) <= abs(xstart)) then
            xunit = xstop / real(nm, real64)
            x = xunit * real([(i, i=-nm, nm, 2)], real64)
            if (modulo(nm, 2) == 0) then
                x(1 + nm / 2) = ZERO
            end if
        else
            xunit = (xstop - xstart) / real(nm, real64)
            x = xstart + xunit * real([(i, i=0, nm)], real64)
        end if

        if (n >= 1) then ! Indeed, N < 1 cannot happen due to the quick return when N <= 0.
            x(1) = xstart
            x(n) = xstop
        end if

    end function linspace_r

    !> `n` evenly spaced integers from `xstart` to `xstop`, upstream's `linspace_i`.
    function linspace_i(xstart, xstop, n) result(x)
        integer, intent(in) :: xstart    !! the first value
        integer, intent(in) :: xstop     !! the last value
        integer, intent(in) :: n         !! how many values
        integer :: x(max(n, 0))          !! the values

        x = nint(linspace_r(real(xstart, real64), real(xstop, real64), n))  ! Rounded to the closest integer.

    end function linspace_i

    !> `1` for true and `0` for false, upstream's `logical_to_int` behind the generic `int`.
    pure elemental function logical_to_int(x) result(y)
        logical, intent(in) :: x    !! the logical
        integer :: y                !! 1 or 0

        y = merge(tsource=1, fsource=0, mask=x)

    end function logical_to_int

    ! ---- What LINCOA and COBYLA add: the factorisations and the two constrained tests ----------
    !
    ! Everything above is what BOBYQA reaches. The rest of PRIMA's `linalg_mod` that this tier
    ! needs arrives here, with the two solvers that call it (`feature_optimizer.md` 12.1 allows a
    ! phase to extend what an earlier one built).

    !> `sqrt(x1**2 + x2**2)` without overflowing or underflowing, upstream's `hypotenuse`.
    !!
    !! The final `min`/`max` is upstream's: without it rounding can put the answer outside
    !! `[max(|x1|, |x2|), |x1| + |x2|]`, which the Givens rotations built on it rely on.
    function hypotenuse(x1, x2) result(r)
        real(real64), intent(in) :: x1    !! one side
        real(real64), intent(in) :: x2    !! the other side
        real(real64) :: r                 !! the hypotenuse
        real(real64) :: y(2)

        if (.not. is_finite(x1)) then
            r = abs(x1)
        elseif (.not. is_finite(x2)) then
            r = abs(x2)
        else
            y = abs([x1, x2])
            y = [minval(y), maxval(y)]
            if (y(1) > sqrt(REALMIN) .and. y(2) < sqrt(REALMAX / 2.1_real64)) then
                r = sqrt(sum(y**2))
            elseif (y(2) > 0) then
                r = y(2) * sqrt((y(1) / y(2))**2 + ONE)
            else
                r = ZERO
            end if
            ! Without the following line, R > Y(1) + Y(2) or R < Y(2) may happen due to rounding errors.
            r = min(sum(y), max(y(2), r))
        end if

    end function hypotenuse

    !> The `n`-by-`n` identity matrix, upstream's `eye1` behind the generic `eye`.
    function eye1(n) result(x)
        integer, intent(in) :: n                   !! the order
        real(real64) :: x(max(n, 0), max(n, 0))    !! the identity matrix
        integer :: i

        if (size(x, 1) * size(x, 2) > 0) then
            x = ZERO
            do i = 1, int(min(size(x, 1), size(x, 2)))
                x(i, i) = ONE
            end do
        end if

    end function eye1

    !> The `m`-by-`n` matrix with ones on the main diagonal, upstream's `eye2`.
    !!
    !! Rectangular as well as square: COBYLA builds its initial simplex from `eye(n, n + 1)`.
    function eye2(m, n) result(x)
        integer, intent(in) :: m                   !! rows
        integer, intent(in) :: n                   !! columns
        real(real64) :: x(max(m, 0), max(n, 0))    !! the matrix
        integer :: i

        if (size(x, 1) * size(x, 2) > 0) then
            x = ZERO
            do i = 1, int(min(size(x, 1), size(x, 2)))
                x(i, i) = ONE
            end do
        end if

    end function eye2

    !> Whether `A` is banded within the given bandwidths, to a tolerance; upstream's `isbanded`.
    function isbanded(A, lwidth, uwidth, tol) result(is_banded)
        real(real64), intent(in) :: A(:, :)          !! the matrix
        integer, intent(in) :: lwidth                !! how many sub-diagonals may be non-zero
        integer, intent(in) :: uwidth                !! how many super-diagonals may be non-zero
        real(real64), intent(in), optional :: tol    !! tolerance, absolute or relative to `maxval(abs(A))`; default zero
        logical :: is_banded                         !! whether every entry outside the band is within `tol` of zero
        integer :: i
        integer :: m
        integer :: n
        real(real64) :: tol_loc

        tol_loc = ZERO
        if (present(tol)) then
            tol_loc = max(tol, tol * maxval(abs(A)))
        end if
        if (is_nan(tol_loc)) then
            tol_loc = ZERO
        end if

        m = int(size(A, 1))
        n = int(size(A, 2))

        is_banded = .true.
        do i = 1, n
            is_banded = (all(abs(A(i + lwidth + 1:m, i)) <= tol_loc) .and. all(abs(A(1:i - uwidth - 1, i)) <= tol_loc))
            if (.not. is_banded) then
                exit
            end if
        end do

    end function isbanded

    !> Whether `A` is lower triangular to a tolerance, upstream's `istril`.
    function istril(A, tol) result(is_tril)
        real(real64), intent(in) :: A(:, :)          !! the matrix
        real(real64), intent(in), optional :: tol    !! tolerance; default zero
        logical :: is_tril                           !! whether the strict upper triangle is within `tol` of zero
        integer :: width
        real(real64) :: tol_loc

        if (present(tol)) then
            tol_loc = tol
        else
            tol_loc = ZERO
        end if
        width = int(max(0, size(A, 1) - 1))
        is_tril = isbanded(A, width, 0, tol_loc)

    end function istril

    !> Whether `A` is upper triangular to a tolerance, upstream's `istriu`.
    function istriu(A, tol) result(is_triu)
        real(real64), intent(in) :: A(:, :)          !! the matrix
        real(real64), intent(in), optional :: tol    !! tolerance; default zero
        logical :: is_triu                           !! whether the strict lower triangle is within `tol` of zero
        integer :: width
        real(real64) :: tol_loc

        if (present(tol)) then
            tol_loc = tol
        else
            tol_loc = ZERO
        end if
        width = int(max(0, size(A, 2) - 1))
        is_triu = isbanded(A, 0, width, tol_loc)

    end function istriu

    !> The QR factorisation of `A`, optionally with column pivoting; upstream's `qr`.
    !!
    !! Givens rotations applied to the transpose, so the work is on columns throughout. With `P`
    !! present the columns are pivoted by descending remaining norm and `A(:, P) = Q*R`; without it
    !! `A = Q*R`.
    !!
    !! Upstream's guard on the first line tests `R` twice where it means `P`, so a call asking for
    !! the permutation alone would return with `P` unset. Kept as upstream has it: no call in this
    !! tier takes that form, and a silent divergence from the commit is worse than a dead branch.
    subroutine qr(A, Q, R, P)
        real(real64), intent(in) :: A(:, :)               !! the matrix to factorise
        real(real64), intent(out), optional :: Q(:, :)    !! the orthonormal columns
        real(real64), intent(out), optional :: R(:, :)    !! the upper triangle
        integer, intent(out), optional :: P(:)            !! the column permutation; its presence is what turns pivoting on
        logical :: pivot
        integer :: i
        integer :: j
        integer :: k
        integer :: m
        integer :: n
        real(real64) :: G(2, 2)
        real(real64) :: Q_loc(size(A, 1), size(A, 1))
        real(real64) :: T(size(A, 2), size(A, 1))

        if (.not. (present(Q) .or. present(R) .or. present(R))) then
            return
        end if

        m = int(size(A, 1))
        n = int(size(A, 2))

        pivot = (present(P))
        Q_loc = eye(m)
        T = transpose(A) ! T is the transpose of R. We consider T in order to work on columns.
        if (pivot) then
            P = linspace(1, n, n)
        end if

        do j = 1, n
            if (pivot) then
                k = int(maxloc(sum(T(j:n, j:m)**2, dim=2), dim=1))
                if (k > 1 .and. k <= n - j + 1) then
                    k = k + j - 1
                    P([j, k]) = P([k, j])
                    T([j, k], :) = T([k, j], :)
                end if
            end if
            do i = m, j + 1, -1
                G = transpose(planerot(T(j, [j, i])))
                T(j, [j, i]) = [hypotenuse(T(j, j), T(j, i)), ZERO]  !T(j, [j, i]) = [sqrt(T(j, j)**2 + T(j, i)**2), ZERO]
                T(j + 1:n, [j, i]) = matprod(T(j + 1:n, [j, i]), G)
                Q_loc(:, [j, i]) = matprod(Q_loc(:, [j, i]), G)
            end do
        end do

        if (present(Q)) then
            Q = Q_loc(:, 1:size(Q, 2))
        end if
        if (present(R)) then
            R = transpose(T(:, 1:size(R, 1)))
        end if

    end subroutine qr

    !> The inverse of a small invertible matrix, upstream's `inv`.
    !!
    !! Naive by intent, as upstream says: forward or back substitution when `A` is triangular --
    !! which is the case COBYLA invokes, on its simplex -- and otherwise through `qr`. Not for
    !! general use, and not a pseudo-inverse.
    function inv(A) result(B)
        real(real64), intent(in) :: A(:, :)          !! the matrix
        real(real64) :: B(size(A, 1), size(A, 1))    !! its inverse
        integer :: P(size(A, 1))
        integer :: InvP(size(A, 1))
        integer :: i
        integer :: n
        real(real64) :: Q(size(A, 1), size(A, 1))
        real(real64) :: R(size(A, 1), size(A, 1))

        n = int(size(A, 1))

        if (n <= 0) then ! Of course, N < 0 should never happen.
            return
        end if

        if (istril(A)) then
            ! This case is invoked in COBYLA.
            R = transpose(A) ! Take transpose to work on columns.
            B = ZERO
            do i = 1, n
                B(i, i) = ONE / R(i, i)
                B(1:i - 1, i) = -matprod(B(1:i - 1, 1:i - 1), R(1:i - 1, i) / R(i, i))
            end do
            B = transpose(B)
        elseif (istriu(A)) then
            B = ZERO
            do i = 1, n
                B(i, i) = ONE / A(i, i)
                B(1:i - 1, i) = -matprod(B(1:i - 1, 1:i - 1), A(1:i - 1, i) / A(i, i))
            end do
        else
            ! This is NOT the best algorithm for the inverse, but since the QR subroutine is available ...
            call qr(A, Q, R, P)
            R = transpose(R) ! Take transpose to work on columns.
            B = ZERO
            do i = n, 1, -1
                B(:, i) = (Q(:, i) - matprod(B(:, i + 1:n), R(i + 1:n, i))) / R(i, i)
            end do
            InvP(P) = linspace(1, n, n) ! The inverse permutation
            B = transpose(B(:, InvP))
        end if

    end function inv

    !> The solution of `A*x = b` for a small invertible `A`, upstream's `solve`.
    !!
    !! Substitution when `A` is triangular -- LINCOA's case, on the `R` of its active-set
    !! factorisation -- and otherwise through `qr`.
    function solve(A, b) result(x)
        real(real64), intent(in) :: A(:, :)    !! the square matrix
        real(real64), intent(in) :: b(:)       !! the right-hand side
        real(real64) :: x(size(A, 2))          !! the solution
        integer :: P(size(A, 1))
        integer :: i
        integer :: n
        real(real64) :: Q(size(A, 1), size(A, 1))
        real(real64) :: R(size(A, 1), size(A, 2))

        n = int(size(A, 1))

        if (n <= 0) then ! Of course, N < 0 should never happen.
            return
        end if

        ! Zaikun 20220527: With the following code, Huawei Bisheng flang 2.1.0, Arm Fortran Compiler 23.1,
        ! and AOCC 5.1 flang, which raise a false positive error about out-bound subscripts when invoked
        ! with the -Mbounds flag. See https://github.com/flang-compiler/flang/issues/1238
        if (istril(A)) then
            do i = 1, n
                x(i) = (b(i) - inprod(A(i, 1:i - 1), x(1:i - 1))) / A(i, i) ! INPROD = 0 if I == 1.
            end do
        elseif (istriu(A)) then ! This case is invoked in LINCOA.
            do i = n, 1, -1
                x(i) = (b(i) - inprod(A(i, i + 1:n), x(i + 1:n))) / A(i, i) ! INPROD = 0 if I == N.
            end do
        else
            ! This is NOT a good algorithm for linear systems, but since the QR subroutine is available ...
            call qr(A, Q, R, P)
            x = matprod(b, Q)
            do i = n, 1, -1
                x(i) = (x(i) - inprod(R(i, i + 1:n), x(i + 1:n))) / R(i, i) ! INPROD = 0 if I == N.
            end do
            x(P) = x ! Handle the permutation.
        end if

    end function solve

    !> The least-squares solution of `A*x = b` from an externally supplied `Q` and the diagonal of
    !> `R`; upstream's `lsqr_Rdiag`, COBYLA's form.
    !!
    !! COBYLA carries the factorisation of its active constraint gradients as `Q` (its `z`) and the
    !! diagonal `Rdiag` (its `zdota`), so the factorisation is never recomputed. The `isminor` test
    !! is Powell's: a component whose departure from zero is attributable to rounding is set to
    !! zero, which is what keeps the active set from drifting.
    function lsqr_Rdiag(A, b, Q, Rdiag) result(x)
        real(real64), intent(in) :: A(:, :)               !! the matrix, m by n
        real(real64), intent(in) :: b(:)                  !! the right-hand side, of length m
        real(real64), intent(in), optional :: Q(:, :)     !! the factorisation's orthonormal columns; computed here when absent
        real(real64), intent(in), optional :: Rdiag(:)    !! the diagonal of R; computed from `Q` and `A` when absent
        real(real64) :: x(size(A, 2))                     !! the least-squares solution
        logical :: pivot
        integer :: i
        integer :: j
        integer :: m
        integer :: n
        integer :: P(size(A, 2))
        integer :: rank
        real(real64) :: Q_loc(size(A, 1), min(size(A, 1), size(A, 2)))
        real(real64) :: Rdiag_loc(min(size(A, 1), size(A, 2)))
        real(real64) :: y(size(b))
        real(real64) :: yq
        real(real64) :: yqa

        m = int(size(A, 1))
        n = int(size(A, 2))

        if (n <= 0) then ! Of course, N < 0 should never happen.
            return
        end if

        if (present(Q)) then
            Q_loc = Q(:, 1:size(Q_loc, 2))
            if (present(Rdiag)) then
                Rdiag_loc = Rdiag
            else
                Rdiag_loc = [(inprod(Q_loc(:, i), A(:, i)), i=1, min(m, n))]
                ! MATLAB: Rdiag_loc = sum(Q_loc(:, 1:min(m,n)) .* A(:, 1:min(m,n)), 1); % Row vector
            end if
            rank = min(m, n)
            pivot = .false.
        else
            call qr(A, Q=Q_loc, P=P)
            Rdiag_loc = [(inprod(Q_loc(:, i), A(:, P(i))), i=1, min(m, n))]
            ! MATLAB: Rdiag_loc = sum(Q_loc(:, 1:min(m,n)) .* A(:, P(1:min(m,n))), 1); % Row vector
            rank = maxval([0, trueloc(abs(Rdiag_loc) > 0)])
            pivot = .true.
        end if

        x = ZERO
        y = b ! Local copy of B; B is INTENT(IN) and should not be modified.

        do i = rank, 1, -1
            if (pivot) then
                j = P(i)
            else
                j = i
            end if
            ! The following IF comes from Powell. It forces X(J) = 0 if deviations from this value can be
            ! attributed to computer rounding errors. This is a favorable choice in the context of COBYLA.
            yq = inprod(y, Q_loc(:, i))
            yqa = inprod(abs(y), abs(Q_loc(:, i)))
            if (isminor(yq, yqa)) then
                x(j) = ZERO
            else
                x(j) = yq / Rdiag_loc(i)
                y = y - x(j) * A(:, j)
            end if
        end do

        !  Postconditions
        !if (DEBUGGING) then
        ! ! The following test cannot be passed.
        ! !call assert(norm(matprod(b - matprod(A, x), A)) <= max(tol, tol * norm(matprod(b, A))), &
        ! ! & 'A*X is the projection of B to the column space of A', srname)
        !end if

    end function lsqr_Rdiag

    !> The least-squares solution of `A*x = b` from an externally supplied economy QR factorisation;
    !> upstream's `lsqr_Rfull`, LINCOA's form.
    !!
    !! The back substitution is written as the explicit double loop upstream keeps rather than the
    !! `inprod` form beside it, for the reason upstream records: it works slightly better in LINCOA.
    function lsqr_Rfull(b, Q, R) result(x)
        real(real64), intent(in) :: b(:)       !! the right-hand side, of length m
        real(real64), intent(in) :: Q(:, :)    !! the orthonormal columns, m by n
        real(real64), intent(in) :: R(:, :)    !! the upper triangle, n by n, non-singular
        real(real64) :: x(size(R, 2))          !! the least-squares solution
        integer :: i
        integer :: j
        integer :: m
        integer :: n

        m = int(size(Q, 1))
        n = int(size(R, 2))

        if (n <= 0) then ! Of course, N < 0 should never happen.
            return
        end if

        x = matprod(b, Q)
        do i = n, 1, -1
            do j = i + 1, n
                x(i) = x(i) - R(i, j) * x(j)
            end do
            x(i) = x(i) / R(i, i)
        end do
        !--------------------------------------------------------------------------------------------------!
        ! The following is equivalent to the above, yet the above version works slightly better in LINCOA.
        ! !do i = n, 1, -1
        ! !    x(i) = (inprod(Q(:, i), b) - inprod(R(i, i + 1:n), x(i + 1:n))) / R(i, i)
        ! !end do
        !--------------------------------------------------------------------------------------------------!

    end function lsqr_Rfull

    !> Whether `x` is negligible beside `ref` in this arithmetic, upstream's `isminor0`.
    !!
    !! Powell's test, and exact arithmetic is not what it answers: it is true when adding a tenth of
    !! `x` to `|ref|` changes nothing that adding a fifth does not, i.e. when `x`'s departure from
    !! zero is attributable to rounding. The sensitivity `TENTH` is Powell's.
    pure function isminor0(x, ref) result(is_minor)
        real(real64), intent(in) :: x      !! the value under test
        real(real64), intent(in) :: ref    !! the reference magnitude
        logical :: is_minor                !! whether `x` is negligible beside `ref`
        real(real64), parameter :: sensitivity = TENTH
        real(real64) :: refa
        real(real64) :: refb

        refa = abs(ref) + sensitivity * abs(x)
        refb = abs(ref) + TWO * sensitivity * abs(x)
        is_minor = (abs(ref) >= refa .or. refa >= refb)

    end function isminor0

    !> `isminor0` element by element, upstream's `isminor1`.
    function isminor1(x, ref) result(is_minor)
        real(real64), intent(in) :: x(:)      !! the values
        real(real64), intent(in) :: ref(:)    !! the reference magnitudes, the same length
        logical :: is_minor(size(x))          !! the element-wise verdict
        integer :: i

        is_minor = [(isminor0(x(i), ref(i)), i=1, int(size(x)))]

    end function isminor1

    !> `maxval(x)`, but a NaN anywhere in `x` gives a NaN; upstream's `maximum1`.
    !!
    !! F2018 leaves `maxval` over a NaN unspecified, and this repository's own rules say why that
    !! matters: under nagfor at `-O4` such a reduction aborts the process. Upstream detects the NaN
    !! through `sum(abs(x))` rather than `any(is_nan(x))`, which is one pass instead of two.
    !!
    !! The constrained solvers compare violations with it, where a NaN must propagate rather than be
    !! quietly skipped.
    function maximum1(x) result(y)
        real(real64), intent(in) :: x(:)    !! the values
        real(real64) :: y                   !! the largest, or a NaN if any element is one
        real(real64) :: nan_test

        !y = merge(tsource=sum(x), fsource=maxval(x), mask=any(is_nan(x)))
        nan_test = sum(abs(x)) ! 1. Assume: X has NaN iff NAN_TEST = NaN. 2. Avoid enormous calls to IS_NAN
        y = merge(tsource=nan_test, fsource=maxval(x), mask=is_nan(nan_test))

    end function maximum1

    !> `maximum1` over a matrix, upstream's `maximum2`.
    function maximum2(x) result(y)
        real(real64), intent(in) :: x(:, :)    !! the values
        real(real64) :: y                      !! the largest, or a NaN if any element is one
        real(real64) :: nan_test

        !y = merge(tsource=sum(x), fsource=maxval(x), mask=any(is_nan(x)))
        nan_test = sum(abs(x)) ! 1. Assume: X has NaN iff NAN_TEST = NaN. 2. Avoid enormous calls to IS_NAN
        y = merge(tsource=nan_test, fsource=maxval(x), mask=is_nan(nan_test))

    end function maximum2

end module parquet_prima_linalg
