!> The discrete cosine and sine transforms of a `real64` sequence whose length is a power of two:
!> `pf_dct` and `pf_dst` (type II) with `pf_idct` and `pf_idst` (their exact inverses), and
!> `pf_is_pow2` and `pf_next_pow2` for choosing that length.
!!
!! `parquet_transform` is an **Arrow-free leaf**: it imports the INTRINSIC module `iso_fortran_env`
!! and, of this library, `parquet_constants` alone, a leaf of parameters that imports nothing
!! itself (`check_parquet_constants_stays_leaf`). An intrinsic module is not a compiled file, not a
!! tier edge and not a footprint entry, so `use parquet_transform` compiles three Fortran files and
!! never crosses the C++ boundary. `check_parquet_transform_stays_arrow_free`
!! (tools/check_source_conventions.py) keeps that true.
!!
!! **The convention is scipy's, factor of two included.** For a sequence `x` of length `n`, with
!! indices from 0 in the formulas (the arrays themselves are 1-based, `x(j+1)` holding `x[j]`):
!!
!! ```
!! pf_dct:   y[k] = 2 * sum_{j=0}^{n-1} x[j] * cos(pi*k*(2j+1) / (2n)),       k = 0 .. n-1
!! pf_idct:  x[k] = (y[0] + 2 * sum_{j=1}^{n-1} y[j] * cos(pi*j*(2k+1) / (2n))) / (2n)
!! pf_dst:   y[k] = 2 * sum_{j=0}^{n-1} x[j] * sin(pi*(k+1)*(2j+1) / (2n)),   k = 0 .. n-1
!! pf_idst:  x[k] = ((-1)**k * y[n-1]
!!                   + 2 * sum_{j=0}^{n-2} y[j] * sin(pi*(j+1)*(2k+1) / (2n))) / (2n)
!! ```
!!
!! which are `scipy.fft.dct(x)`, `idct(y)`, `dst(x)` and `idst(y)` under their defaults, type 2.
!! **The two inverses differ only in their unpaired term**: the cosine transform's is the constant
!! `y[0]`, the sine transform's is the top frequency `y[n-1]`, which alternates in sign.
!!
!! `norm="ortho"` scales the coefficients by `sqrt(1/(4n))` at ONE index and by `sqrt(1/(2n))`
!! elsewhere, scipy's `norm='ortho'`: the transform is then orthonormal, and the inverse is its
!! transpose. **That index is `k = 0` for the cosine pair and `k = n-1` for the sine pair** -- the
!! opposite end, because the two transforms carry their unpaired frequency at opposite ends. Every
!! internal consistency test passes under a wrong convention, so `test/test_transform.f90` pins
!! this one against values scipy produced. **The factor of two is load-bearing**: the Improved
!! Sheather-Jones bandwidth rule squares `y(2:)/2`, dividing it back out, and a transform without
!! it would put that rule's every bandwidth out by a constant factor.
!!
!! **A spectrum does not carry across from one pair to the other unchanged**: `pf_dct` coefficient
!! `k` and `pf_dst` coefficient `k` are different frequencies, `k` and `k+1`. Code that transforms
!! with `pf_dct` and inverts with `pf_idst` moves the coefficients down one position and drops the
!! top frequency; the guide page's "Composing the two transforms" writes that out.
!!
!! **A length that is not a power of two is an `error stop`**, not a slow path. Zero-padding a
!! sequence up to a legal length is not a fix: the transform of a padded sequence is not a padded
!! transform, and its coefficients mean something else. Choose the length before building the
!! sequence; `pf_next_pow2` gives the smallest legal one at or above a count.
!!
!! **There is no status code and no `info` type**, unlike `parquet_root` and `parquet_integrate`:
!! a transform has no budget, no convergence and no partial success. Nothing in this module prints
!! and it reads no setting; its only output path is `error stop`, for a caller contract that was
!! broken.
!!
!! **No value is screened.** The transform only multiplies and adds, and never compares a data
!! value. A NaN anywhere in the input makes every output value NaN and raises none of the three
!! exceptions a build can stop on; `IEEE_INEXACT` is raised by the arithmetic itself, whatever the
!! input. An infinity makes output values infinite or NaN and can raise `IEEE_INVALID` on the way,
!! and a sequence large enough that `n` times its largest magnitude approaches `huge` can overflow;
!! under nagfor's default `-ieee=stop` either ends the program.
!!
!! **Thread safety is by construction.** The module has no variable that is not a `parameter`, and
!! each call allocates its own workspace, so concurrent calls on distinct arrays are independent.
!! The `error stop` is taken under a named `critical`, so one thread aborts.
!!
!! The engine is a complex radix-2 FFT of length `n`, private to this module, and there is exactly
!! one of it: the sine pair is the cosine pair applied to a sign-alternated sequence and read
!! backwards, not a second transform. `src/parquet_transform_core.f90`'s header says how all four
!! are built on it.
module parquet_transform

    use iso_fortran_env, only : real64

    implicit none
    private

    public :: pf_dct, pf_idct
    public :: pf_dst, pf_idst
    public :: pf_is_pow2, pf_next_pow2

    ! ---- internal parameters, not part of the public surface -------------------------------

    !> Characters of caller-supplied `context` an abort message reproduces before eliding with
    !! `...` (`api-conventions.md`, errors and diagnostics).
    integer, parameter :: CONTEXT_CAP = 100
    !> The largest power of two a default integer holds, `2**30` for a 32-bit default: the ceiling
    !! of `pf_next_pow2`. Formed from `digits` so that it follows the default kind.
    integer, parameter :: POW2_MAX = 2**(digits(1) - 1)

    ! ---- the transforms ---------------------------------------------------------------------
    !
    ! One specific each, a rank-1 `real64` sequence. The generic names leave room for another kind
    ! or rank without changing what a caller writes. Each specific is written in the fully restated
    ! `module subroutine` form with its own `implicit none`.

    !> The discrete cosine transform of type II of `x`, into `y`: scipy's `dct(x)`.
    !!
    !! ```
    !! call pf_dct(x, y, [norm], [context])
    !! ```
    !!
    !! Optional arguments are shown in square brackets, with the comma outside the bracket. The
    !! arguments:
    !!
    !! * `x` -- `real64`, the sequence. Its length must be a positive power of two.
    !! * `y` -- `real64`, `intent(out)`: the coefficients, `y(k+1)` holding the formula's `y[k]`.
    !!   Must have the size of `x`, and must not overlap it.
    !! * `norm` -- optional token: `"none"`, the default, for the unnormalised transform with its
    !!   factor of two, or `"ortho"` for the orthonormal one. Matched case-insensitively; any other
    !!   token aborts.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    interface pf_dct

        !> The rank-1 `real64` form.
        module subroutine dct_r64(x, y, norm, context)
            implicit none
            real(real64), intent(in)               :: x(:)    !! the sequence, of a power-of-two length
            real(real64), intent(out)              :: y(:)    !! the coefficients, the size of `x`
            character(len=*), intent(in), optional :: norm    !! `"none"` (default) or `"ortho"`
            character(len=*), intent(in), optional :: context !! call-site text for an abort message
        end subroutine dct_r64

    end interface pf_dct

    !> The inverse of `pf_dct`, into `x`: the sequence whose transform is `y`, scipy's `idct(y)`.
    !!
    !! ```
    !! call pf_idct(y, x, [norm], [context])
    !! ```
    !!
    !! Under `"none"` this is the type-III transform divided by `2n`; under `"ortho"` it is the
    !! orthonormal type-III transform, the transpose of `pf_dct`'s. The arguments:
    !!
    !! * `y` -- `real64`, the coefficients. Their number must be a positive power of two.
    !! * `x` -- `real64`, `intent(out)`: the sequence. Must have the size of `y`, and must not
    !!   overlap it.
    !! * `norm` -- optional token, as for `pf_dct`. Name the one the coefficients were made with:
    !!   `pf_idct` then gives back the sequence `pf_dct` transformed, to rounding.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    interface pf_idct

        !> The rank-1 `real64` form.
        module subroutine idct_r64(y, x, norm, context)
            implicit none
            real(real64), intent(in)               :: y(:)    !! the coefficients, a power-of-two count
            real(real64), intent(out)              :: x(:)    !! the sequence, the size of `y`
            character(len=*), intent(in), optional :: norm    !! `"none"` (default) or `"ortho"`
            character(len=*), intent(in), optional :: context !! call-site text for an abort message
        end subroutine idct_r64

    end interface pf_idct

    ! ---- the sine transforms ----------------------------------------------------------------
    !
    ! One specific each, the same shape as the cosine pair above. They are built ON that pair --
    ! the type-II sine transform of `x` is the type-II cosine transform of `x` with every
    ! odd-indexed value negated, read backwards -- so there is one engine here, not two, and the
    ! scipy convention is inherited rather than restated.

    !> The discrete sine transform of type II of `x`, into `y`: scipy's `dst(x)`.
    !!
    !! ```
    !! call pf_dst(x, y, [norm], [context])
    !! ```
    !!
    !! Optional arguments are shown in square brackets, with the comma outside the bracket. The
    !! arguments:
    !!
    !! * `x` -- `real64`, the sequence. Its length must be a positive power of two.
    !! * `y` -- `real64`, `intent(out)`: the coefficients, `y(k+1)` holding the formula's `y[k]`.
    !!   Must have the size of `x`, and must not overlap it.
    !! * `norm` -- optional token: `"none"`, the default, for the unnormalised transform with its
    !!   factor of two, or `"ortho"` for the orthonormal one. Matched case-insensitively; any other
    !!   token aborts. **Under `"ortho"` the exceptional coefficient is the LAST**, at `k = n-1`,
    !!   where `pf_dct`'s is the first, at `k = 0`.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    interface pf_dst

        !> The rank-1 `real64` form.
        module subroutine dst_r64(x, y, norm, context)
            implicit none
            real(real64), intent(in)               :: x(:)    !! the sequence, of a power-of-two length
            real(real64), intent(out)              :: y(:)    !! the coefficients, the size of `x`
            character(len=*), intent(in), optional :: norm    !! `"none"` (default) or `"ortho"`
            character(len=*), intent(in), optional :: context !! call-site text for an abort message
        end subroutine dst_r64

    end interface pf_dst

    !> The inverse of `pf_dst`, into `x`: the sequence whose transform is `y`, scipy's `idst(y)`.
    !!
    !! ```
    !! call pf_idst(y, x, [norm], [context])
    !! ```
    !!
    !! Under `"none"` this is the type-III sine transform divided by `2n`; under `"ortho"` it is the
    !! orthonormal type-III sine transform, the transpose of `pf_dst`'s. The arguments:
    !!
    !! * `y` -- `real64`, the coefficients. Their number must be a positive power of two.
    !! * `x` -- `real64`, `intent(out)`: the sequence. Must have the size of `y`, and must not
    !!   overlap it.
    !! * `norm` -- optional token, as for `pf_dst`. Name the one the coefficients were made with:
    !!   `pf_idst` then gives back the sequence `pf_dst` transformed, to rounding.
    !! * `context` -- optional text appended to any abort message, to identify the call site.
    !!   Capped at 100 characters.
    interface pf_idst

        !> The rank-1 `real64` form.
        module subroutine idst_r64(y, x, norm, context)
            implicit none
            real(real64), intent(in)               :: y(:)    !! the coefficients, a power-of-two count
            real(real64), intent(out)              :: x(:)    !! the sequence, the size of `y`
            character(len=*), intent(in), optional :: norm    !! `"none"` (default) or `"ortho"`
            character(len=*), intent(in), optional :: context !! call-site text for an abort message
        end subroutine idst_r64

    end interface pf_idst

    ! ---- choosing a length ------------------------------------------------------------------

    interface

        !> `.true.` when `n` is a positive power of two -- 1, 2, 4, 8 and so on, the lengths
        !! `pf_dct` and `pf_idct` accept.
        !!
        !! Zero and every negative `n` give `.false.`; nothing aborts, so this is `pure`.
        pure module function pf_is_pow2(n) result(is_pow2)
            implicit none
            integer, intent(in) :: n       !! the candidate length, any value
            logical             :: is_pow2 !! `n` is a positive power of two
        end function pf_is_pow2

        !> The smallest power of two at or above `n`: the legal transform length for at least `n`
        !! values, to choose BEFORE the sequence is built. Padding a sequence already built up to
        !! this length is no substitute (see the module header).
        !!
        !! `n` must lie between 1 and `2**30`, the largest power of two a default integer holds;
        !! anything else is an `error stop`. Impure deliberately: it can abort, and every abort in
        !! this module is taken under one named `critical`.
        module function pf_next_pow2(n) result(p)
            implicit none
            integer, intent(in) :: n !! the count to cover, from 1 to `2**30`
            integer             :: p !! the smallest power of two that is at least `n`
        end function pf_next_pow2

    end interface

end module parquet_transform
