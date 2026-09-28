!> Small, self-contained numeric, text and path helpers for programs built on this library: total
!> arithmetic and angle handling, the standard normal distribution and its quantile function,
!> ASCII case folding, value-to-text rendering, and POSIX path joining and splitting.
!!
!! `parquet_utils` is a **leaf**: it imports the two INTRINSIC modules `iso_fortran_env` and
!! `ieee_arithmetic` and, of this library, `parquet_constants` alone -- a leaf of parameters that
!! imports nothing itself (`check_parquet_constants_stays_leaf`) -- and nothing else, not even
!! `parquet_settings_base`. An intrinsic module is not a compiled file, not a tier edge and not a
!! footprint entry, and `parquet_constants` adds one file with no edge of its own, so the property
!! that promise protects is untouched: `use parquet_utils` compiles two Fortran files and never
!! crosses the C++ boundary. `check_parquet_utils_stays_arrow_free`
!! (tools/check_source_conventions.py) is what keeps that true.
!!
!! **That leaf status is load-bearing rather than tidy.** `parquet_settings_base` carried its own
!! private ASCII fold with a doc-comment explaining that it could not call `parquet_core`'s copy
!! without creating a circular dependency. A module below everything is what removes that cycle, so
!! anything added here must not import a sibling other than `parquet_constants`, which imports
!! nothing -- the cycle returns the moment it does.
!!
!! **Every procedure in this module is total: nothing validates, nothing aborts, nothing prints.**
!! `min_width` grows the result rather than refusing it, Python's join rules have no error case,
!! taking a path apart has none either, and a `fmt` the I/O runtime rejects yields asterisks rather
!! than killing the process. There is no `error stop` anywhere in this file and no entry point has
!! a precondition a caller can violate. That is also why the module needs no out-of-process error
!! scenarios: it has no error paths to run in one.
!!
!! **`pure` on every procedure is what enforces most of that, on every build.** Fortran forbids a
!! `pure` procedure from printing, from any other external I/O and from executing a `STOP`, so
!! "nothing prints" and "nothing stops" are compile-time facts rather than promises. It does NOT
!! forbid `ERROR STOP` -- that third of the claim rests on there being none in the file, which is
!! the one part a future edit can break silently. A new procedure here is declared `pure` too.
!!
!! **Every allocatable result is ALLOCATED on every path, zero-length where the answer is empty.**
!! `pf_dirname("cat.parquet")`, `pf_path_ext("cat")` and `pf_join_path` over a zero-size array all
!! return an allocated, zero-length string, never an unallocated one -- so `len(result)` is the
!! only thing a caller ever has to test. An `intent(out)` allocatable is deallocated on entry, so a
!! path that merely failed to assign would hand back a variable whose `len` is undefined.
!!
!! **Every PATH argument, and `suffix`, has its TRAILING blanks trimmed and its LEADING blanks
!! preserved.** A `character(len=*)` actual is blank-padded and nothing can distinguish padding
!! from intent, so trailing blanks cannot be honoured; a leading blank is a legal filename
!! character and is kept. A caller who genuinely means a trailing blank builds the string with
!! `pf_split_path` plus concatenation.
!!
!! **`pf_to_lower`/`pf_to_upper` are the exception and do not trim**: the copy form is exactly
!! `len(s)` long and the in-place form cannot alter its variable's length at all, which is the
!! whole point of folding a fixed-length key in place. Do not widen the rule above to cover them.
!!
!! **Everything text-producing is a SUBROUTINE with a `character(len=:), allocatable, intent(out)`
!! result**, never a function returning a deferred-length character. GCC PR113797 makes the hidden
!! length variable of such a function unreliable under concurrency; this project measured 562670 of
!! 2000000 concurrent calls corrupted as a function and 0 as a subroutine.
!!
!! **Paths are POSIX and purely lexical.** The separator is `/` on every platform, and no `.` or
!! `..` is ever resolved: `a/b/../c` is only `a/c` when `b` is not a symlink, so a lexical answer
!! would be wrong in exactly the cases where it matters. Nothing here touches the filesystem, which
!! is what keeps every procedure `pure` and testable without fixtures. The reference is CPython's
!! `posixpath`, and `tools/generate_path_reference.py` emits the values the tests assert, so
!! "we follow Python" is checked rather than claimed.
!!
!! **Every numeric helper computes in the KIND it was handed, and none widens behind your back.**
!! `pf_safe_div`, the four `pf_wrap_*`, `pf_deg2rad`/`pf_rad2deg`, `pf_cross_product`, `pf_probit`
!! and `pf_norm_cdf`/`pf_norm_sf`/`pf_norm_pdf` each have a `real32` and a `real64` specific, and
!! the `real32` one does `real32` arithmetic throughout. That
!! is what lets `pf_safe_div` promise it returns *exactly* what `a/b` returns, which a form widening
!! to `real64` and narrowing back could not. A caller wanting `real64` accuracy passes `real64`.
!!
!! **The normal family is TOTAL like everything else here: `pf_probit` answers `-Infinity` at 0,
!! `+Infinity` at 1 and a quiet NaN outside `[0, 1]`**, rather than validating its argument. It is
!! accurate to about 3 ulp over the whole range, including into the subnormal tail, and
!! `tools/generate_probit_reference.py` is the 50-digit oracle that says so rather than a claim in
!! this comment. See `pf_probit`'s own doc-comment for how it is computed and why the two branches
!! are written on `erf` and on `log(Phi)` rather than both on `Phi`.
!!
!! **Both wrapping ranges are HALF-OPEN -- `[0, 360)` and `[-180, 180)` -- and the guard that keeps
!! them so is load-bearing rather than defensive.** `modulo` is the whole computation, and `modulo`
!! can return the divisor itself when the true result is a rounding below it: measured on gfortran
!! 15.2, `modulo(-1.0e-30, 360.0)` is exactly `360.0` in both kinds, and
!! `modulo(x + 180, 360) - 180` is exactly `180.0` for `x` one ulp below `-180`. Each wrap therefore
!! ends with one comparison folding that case back to the low end. Both ranges being half-open is
!! also what makes every wrap **idempotent**: wrapping an already-wrapped angle changes nothing.
!!
!! Guide: `doc/pages/utilities/utils.md`. Tests: `test/test_utils.f90` (suite name `utils`).
module parquet_utils
    use, intrinsic :: iso_fortran_env, only: int32, int64, real32, real64
    use, intrinsic :: ieee_arithmetic, only: ieee_value, ieee_quiet_nan, &
        ieee_positive_inf, ieee_negative_inf
    use parquet_constants, only: PF_PI, PF_TWOPI, PF_RAD_PER_DEG, PF_DEG_PER_RAD
    implicit none
    private

    public :: pf_safe_div
    public :: pf_probit, pf_norm_cdf, pf_norm_sf, pf_norm_pdf
    public :: pf_wrap_deg, pf_wrap_180, pf_wrap_rad, pf_wrap_pi
    public :: pf_deg2rad, pf_rad2deg
    public :: pf_cross_product
    public :: pf_to_lower, pf_to_upper
    public :: pf_to_str
    public :: pf_from_str
    public :: pf_join_path
    public :: pf_dirname, pf_basename, pf_path_ext, pf_path_stem
    public :: pf_split_path, pf_path_add_suffix

    !> Pi, and a turn of it, in `real32`, for the `real32` wraps. The `real64` specifics use
    !! `parquet_constants`' own; these are those narrowed, so each is the nearest `real32` to pi
    !! rather than pi truncated to a shorter decimal literal. Private: the named constants are
    !! `parquet_constants`' to publish, and there in `real64` only.
    real(real32), parameter :: PI_R32 = real(PF_PI, real32)
    real(real32), parameter :: TWOPI_R32 = real(PF_TWOPI, real32)
    !> The two angle-conversion factors in `real32`, narrowed from `real64` for the same reason.
    real(real32), parameter :: DEG2RAD_R32 = real(PF_RAD_PER_DEG, real32)
    real(real32), parameter :: RAD2DEG_R32 = real(PF_DEG_PER_RAD, real32)

    !> The constants the normal family needs, in each kind. Written to more digits than either
    !! kind can hold so that the compiler rounds them, and the `real32` ones narrowed from the
    !! `real64` ones for the reason `PI_R32` is -- each is then the nearest `real32` to the true
    !! value rather than a shorter decimal truncation of it.
    real(real64), parameter :: SQRT2_R64 = 1.414213562373095048801689_real64
    real(real64), parameter :: INV_SQRT2_R64 = 0.7071067811865475244008444_real64
    !> `sqrt(2/pi)`, which is `phi(x)*Phi(x)**(-1)` times `erfc_scaled` in the tail step.
    real(real64), parameter :: SQRT_2_OVER_PI_R64 = 0.7978845608028653558798921_real64
    !> `1/sqrt(2*pi)`, the standard normal density's normalisation.
    real(real64), parameter :: INV_SQRT_2PI_R64 = 0.3989422804014326779399461_real64
    !> `2/sqrt(pi)`, which is `erf`'s own derivative factor.
    real(real64), parameter :: TWO_OVER_SQRTPI_R64 = 1.128379167095512573896159_real64
    !> `log(2*pi)`, which the tail's starting estimate needs.
    real(real64), parameter :: LN_2PI_R64 = 1.837877066409345483560659_real64
    real(real32), parameter :: INV_SQRT2_R32 = real(INV_SQRT2_R64, real32)
    real(real32), parameter :: SQRT2_R32 = real(SQRT2_R64, real32)
    real(real32), parameter :: SQRT_2_OVER_PI_R32 = real(SQRT_2_OVER_PI_R64, real32)
    real(real32), parameter :: INV_SQRT_2PI_R32 = real(INV_SQRT_2PI_R64, real32)
    real(real32), parameter :: TWO_OVER_SQRTPI_R32 = real(TWO_OVER_SQRTPI_R64, real32)
    real(real32), parameter :: LN_2PI_R32 = real(LN_2PI_R64, real32)

    !> Where `pf_probit` stops solving on `erf` and starts solving on `log(Phi)`, as a value of
    !! `q = min(p, 1-p)`.
    !!
    !! **Not a tuning knob and not a fitted number: it is where each branch's starting estimate is
    !! good enough for its own fixed step count**, which is what the sweep found.
    !! Below it the series start is too far out for two Halley steps to close; above it the tail's
    !! asymptotic start is too far out for three. Moving it in either direction costs accuracy at
    !! the edge it moves toward, and the golden-vector test is what reports that.
    real(real64), parameter :: PROBIT_QSPLIT_R64 = 0.1_real64
    real(real32), parameter :: PROBIT_QSPLIT_R32 = 0.1_real32

    !> The Maclaurin coefficients of `erfinv`, `sqrt(pi)/2 * [1, pi/12, 7pi**2/480, ...]`.
    !!
    !! **Derived, not fitted and not copied**: they are the series reversion of `erf`'s own
    !! Maclaurin series, so each has a closed form and `tools/generate_probit_reference.py`
    !! re-derives all six by reversion in `--self-test` and compares them with these literals. That
    !! matters for provenance as well as for correctness -- this library is BSD-3 and a published
    !! minimax coefficient set is not.
    !!
    !! **They set only the SPEED of `pf_probit`, never its answer.** The refinement that follows
    !! converges from any starting point; six terms is what makes two Halley steps enough.
    real(real64), parameter :: ERFINV_C_R64(6) = [ &
        0.8862269254527580136490837_real64, 0.2320136665346544935535341_real64, &
        0.1275561753055979582539997_real64, 0.08655212924154753372964179_real64, &
        0.06495961774538541338201467_real64, 0.05173128198461637411263189_real64]
    !> The same six, narrowed one at a time rather than as `real(ERFINV_C_R64, real32)` --
    !! nagfor warns "Loss of accuracy in double-real conversion" once per element for the
    !! whole-array form and says nothing for this one (`fortran-gotchas.md`).
    real(real32), parameter :: ERFINV_C_R32(6) = [ &
        real(ERFINV_C_R64(1), real32), real(ERFINV_C_R64(2), real32), &
        real(ERFINV_C_R64(3), real32), real(ERFINV_C_R64(4), real32), &
        real(ERFINV_C_R64(5), real32), real(ERFINV_C_R64(6), real32)]

    !> The path separator. POSIX `/` on every platform, deliberately -- see the module header.
    character(len=1), parameter :: PATH_SEP = "/"
    !> The extension marker.
    character(len=1), parameter :: EXT_SEP = "."
    !> Scratch width for one `pf_to_str` rendering. A `fmt` producing more than this many
    !! characters is reported the same way any other rejected `fmt` is, as a run of asterisks.
    integer, parameter :: TO_STR_BUF = 512

    ! ---- Total arithmetic ----

    !> Divides `a` by `b`, returning exactly what `a/b` returns, but WITHOUT evaluating the
    !! division when `b` is zero -- and therefore without raising an IEEE exception.
    !!
    !! `pf_safe_div(a, b)` is `a/b` whenever `b` is non-zero, `+/-Infinity` when `b` is zero and `a`
    !! is not, and a quiet NaN when both are zero. `pure elemental`, so it applies to whole arrays.
    !!
    !! **The point is the FLAGS, not the values.** Dividing by a quantity that is legitimately zero
    !! for part of a dataset -- a bin nothing fell into, a denominator counting something that did
    !! not happen -- already yields NaN or Infinity, and both are meaningful answers there
    !! ("undefined" and "unsatisfiable") that a `real64` Parquet column stores perfectly well. What
    !! the plain division ALSO does is raise `IEEE_DIVIDE_BY_ZERO` and `IEEE_INVALID`, so a run
    !! whose only division by zero was expected still ends with the runtime's floating-point
    !! warnings -- which then hide any exception worth knowing about. This returns the same value by
    !! construction rather than by arithmetic, so the numbers are bit-for-bit what they were and the
    !! flags stay clear.
    !!
    !! **It is deliberately NOT a "return zero when the denominator is zero" helper.** That would
    !! silently turn an unsatisfiable request into an unremarkable one, which is the failure this
    !! exists to prevent rather than to cause.
    !!
    !! **A NaN operand is passed through to the division**, which is where it belongs: `b` is then
    !! not zero, so the guard does not fire and `a/b` is a NaN by the ordinary rules.
    interface pf_safe_div
        module procedure pf_safe_div_r32 !! `real(real32)` numerator and denominator.
        module procedure pf_safe_div_r64 !! `real(real64)` numerator and denominator.
    end interface pf_safe_div

    ! ---- The standard normal distribution ----

    !> The probit function: the quantile function of the standard normal, `Phi**(-1)(p)`.
    !!
    !! `pf_probit(p)` is the `z` for which `pf_norm_cdf(z)` is `p`. `pure elemental`, so it applies
    !! to whole arrays, and it computes in the kind it was handed.
    !!
    !! **Total, like everything else in this module: nothing validates and nothing aborts.**
    !! `pf_probit(0.0)` is `-Infinity`, `pf_probit(1.0)` is `+Infinity`, and any `p` outside
    !! `[0, 1]` -- or a NaN -- gives a quiet NaN. The NaN is screened with an equality test before
    !! any ordered comparison is reached, because `<` and `>` are signalling comparisons that raise
    !! `IEEE_INVALID` on a quiet NaN; the infinities are built with `ieee_value` rather than by
    !! dividing, for the same reason.
    !!
    !! **Two exact properties and one near one, each of which has a test.** `pf_probit(0.5)` is
    !! exactly `0` (a positive zero). The result is antisymmetric about `p = 0.5`, because the
    !! magnitude is computed from `q = min(p, 1-p)` and the sign applied afterwards -- so
    !! `pf_probit(p) == -pf_probit(1-p)` holds BIT FOR BIT wherever `1-p` is itself exact, which
    !! is every `p` at or above `1/2` and every dyadic `p`, and to the accuracy below elsewhere.
    !!
    !! **The near one is monotonicity, and the exception is worth knowing.** The result rises with
    !! `p` everywhere except within one ulp of the branch seam, where the two branches meet at
    !! their own accuracy rather than at the last bit: walking consecutive doubles across it steps
    !! backwards by one ulp at two points out of four hundred. No two-branch approximation can do
    !! better without the branches agreeing exactly, and a real seam defect -- a wrong threshold,
    !! a branch reached with the wrong argument -- is not one ulp but millions, which is what the
    !! test's bound separates.
    !!
    !! **Accurate to about 3 ulp over the whole range**, including into the subnormal tail, where
    !! `p = 5e-324` still gives roughly `-38.5` rather than `-Infinity`. The expectations come from
    !! a 50-digit `mpmath` oracle (`tools/generate_probit_reference.py`), never from another
    !! library's double-precision answer.
    !!
    !! **Two branches, and each is written to avoid the cancellation the other would suffer.**
    !! Near the middle it solves `erf(w) = 1 - 2q` for `w` and returns `-sqrt(2)*w`: `erf` is ODD,
    !! so `erf(w) - y` is a difference of two quantities that both vanish as `q` approaches `1/2`,
    !! and the relative accuracy of a small answer survives. Solving `Phi(x) = q` there instead
    !! would subtract two quantities near `1/2` and lose every digit of it. In the tail it solves
    !! `log(Phi(x)) = log(q)` on `erfc_scaled`, where nothing underflows and the subtraction is
    !! between two logarithms rather than between two nearly equal probabilities.
    !!
    !! **The starting estimates decide only the cost.** The tail's iteration is Newton on a
    !! concave increasing function, whose tangent lies above it, so the first step lands at or
    !! beyond the root from ANY start and every step after that is monotone; the remaining steps
    !! are Halley for the extra order. The step counts are fixed rather than iterated to
    !! convergence, so the answer does not depend on the data and the procedure has no loop whose
    !! trip count a caller can observe.
    interface pf_probit
        module procedure pf_probit_r32 !! `real(real32)` probability.
        module procedure pf_probit_r64 !! `real(real64)` probability.
    end interface pf_probit

    !> The standard normal distribution function, `Phi(z)` -- the probability of drawing at most
    !! `z`.
    !!
    !! `0.5*erfc(-z/sqrt(2))`, so the tail is the intrinsic's rather than a subtraction's.
    !! `pure elemental`. Exactly `0.5` at `z = 0`, exactly `0` at `-Infinity`, exactly `1` at
    !! `+Infinity`, and a quiet NaN on a NaN. Nothing validates and nothing aborts.
    !!
    !! **The NaN is screened here rather than left to `erfc`**, because nagfor's `ERFC` answers
    !! `0` for one -- a probability, in range and silent. See the comment above the bodies.
    !!
    !! **Relative accuracy in the far tail degrades as `z*z`**, and that is a property of taking a
    !! `real64` argument rather than something an implementation can avoid: a half-ulp rounding in
    !! `z/sqrt(2)` moves `erfc` by roughly `2*z*z` ulp. Around `z = -30` the answer is good to a
    !! few hundred ulp, which is still 12 significant digits of a probability near `1e-198`.
    !!
    !! **Use `pf_norm_sf` rather than `1 - pf_norm_cdf(z)` for an upper-tail probability.** Above
    !! about `z = 5` the subtraction has no significant digits left to give, which is exactly the
    !! range an upper tail is asked about.
    interface pf_norm_cdf
        module procedure pf_norm_cdf_r32 !! `real(real32)` quantile.
        module procedure pf_norm_cdf_r64 !! `real(real64)` quantile.
    end interface pf_norm_cdf

    !> The standard normal survival function, `1 - Phi(z)` -- the probability of drawing more than
    !! `z`.
    !!
    !! `0.5*erfc(z/sqrt(2))`, computed as the upper tail in its own right and **never** as
    !! `1 - pf_norm_cdf(z)`, which is the whole reason this exists: at `z = 6` the subtraction
    !! keeps about nine digits and by `z = 9` it keeps none, while this stays accurate until the
    !! result underflows. `pure elemental`.
    !!
    !! Exactly `0.5` at `z = 0`, exactly `1` at `-Infinity`, exactly `0` at `+Infinity`, and a
    !! quiet NaN on a NaN -- screened here rather than left to `erfc`, for the reason
    !! `pf_norm_cdf` gives. `pf_norm_sf(z)` is `pf_norm_cdf(-z)` for every `z`, and it carries the
    !! same `z*z` widening of relative error in the far tail.
    interface pf_norm_sf
        module procedure pf_norm_sf_r32 !! `real(real32)` quantile.
        module procedure pf_norm_sf_r64 !! `real(real64)` quantile.
    end interface pf_norm_sf

    !> The standard normal density, `phi(z) = exp(-z*z/2)/sqrt(2*pi)`.
    !!
    !! `pure elemental`. `phi(0)` is `0.3989422804014327`, the density is exactly `0` at either
    !! infinity, and a NaN gives a quiet NaN. Nothing validates and nothing aborts.
    !!
    !! **It underflows to zero rather than clamping**, at about `|z| = 38.6` in `real64` and
    !! `|z| = 13.3` in `real32`. That is a gradual underflow, which sets `IEEE_UNDERFLOW` and is
    !! not a halting exception on any supported compiler; it is a real answer, not a failure.
    interface pf_norm_pdf
        module procedure pf_norm_pdf_r32 !! `real(real32)` quantile.
        module procedure pf_norm_pdf_r64 !! `real(real64)` quantile.
    end interface pf_norm_pdf

    ! ---- Angles ----

    !> Reduces an angle in DEGREES to the half-open range `[0, 360)`.
    !!
    !! Correct for any finite input and any number of turns -- `pf_wrap_deg(-400.0)` is `320.0` --
    !! which the conditional `if (x < 0.0) x = x + 360.0` written at a call site is not. `pure
    !! elemental`. A non-finite input yields a non-finite result. See the module header for why the
    !! range is half-open and what the closing comparison in each of these is for.
    interface pf_wrap_deg
        module procedure pf_wrap_deg_r32 !! `real(real32)` angle in degrees.
        module procedure pf_wrap_deg_r64 !! `real(real64)` angle in degrees.
    end interface pf_wrap_deg

    !> Reduces an angle in DEGREES to the half-open range `[-180, 180)`, the form a *difference*
    !! of two angles wants.
    !!
    !! `[-180, 180)` rather than `(-180, 180]` so that this is `pf_wrap_deg` shifted, with the same
    !! half-open convention and the same idempotence: `pf_wrap_180(180.0)` is `-180.0`, and wrapping
    !! that again leaves it there. `pure elemental`.
    interface pf_wrap_180
        module procedure pf_wrap_180_r32 !! `real(real32)` angle in degrees.
        module procedure pf_wrap_180_r64 !! `real(real64)` angle in degrees.
    end interface pf_wrap_180

    !> Reduces an angle in RADIANS to the half-open range `[0, 2*pi)`. The radian counterpart of
    !! `pf_wrap_deg`; see it, and the module header, for the rules. `pure elemental`.
    !!
    !! **The radian forms are approximate in a way the degree forms are not**: 360 is exact in
    !! binary and `2*pi` is not, so the reduction of a large angle carries the rounding of the
    !! stored `2*pi`. Wrap in degrees where the input is in degrees.
    interface pf_wrap_rad
        module procedure pf_wrap_rad_r32 !! `real(real32)` angle in radians.
        module procedure pf_wrap_rad_r64 !! `real(real64)` angle in radians.
    end interface pf_wrap_rad

    !> Reduces an angle in RADIANS to the half-open range `[-pi, pi)`. The radian counterpart of
    !! `pf_wrap_180`, and it carries the same approximation note as `pf_wrap_rad`. `pure elemental`.
    interface pf_wrap_pi
        module procedure pf_wrap_pi_r32 !! `real(real32)` angle in radians.
        module procedure pf_wrap_pi_r64 !! `real(real64)` angle in radians.
    end interface pf_wrap_pi

    !> Degrees to radians, `pure elemental`. The multiplication is by the nearest value of the
    !! argument's own kind to `pi/180`; nothing is widened.
    interface pf_deg2rad
        module procedure pf_deg2rad_r32 !! `real(real32)` angle in degrees.
        module procedure pf_deg2rad_r64 !! `real(real64)` angle in degrees.
    end interface pf_deg2rad

    !> Radians to degrees, `pure elemental`. The inverse of `pf_deg2rad`, with the same rule.
    !!
    !! **The pair does not round-trip bit-for-bit**, and no test may assert that it does: `pi/180`
    !! and `180/pi` are each rounded, so the two multiplications compose to a factor a rounding away
    !! from one. That is a property of binary floating point, not of these procedures.
    interface pf_rad2deg
        module procedure pf_rad2deg_r32 !! `real(real32)` angle in radians.
        module procedure pf_rad2deg_r64 !! `real(real64)` angle in radians.
    end interface pf_rad2deg

    ! ---- Vectors ----

    !> The cross product `a x b` of two 3-vectors.
    !!
    !! `pure`, total, and the only procedure in this module taking or returning an array. It exists
    !! here rather than in `parquet_healpix` because it has no HEALPix content whatever: a consumer
    !! holding unit vectors from `pf_ang2vec` should not have to compile a pixelisation tier for
    !! nine lines of arithmetic.
    !!
    !! **Both arguments must be of size 3, and this is NOT checked** -- a check would need an error
    !! path, which this module does not have. The dummy is declared `dimension(3)`, so a
    !! shorter actual is a compile-time error wherever the compiler can see the shape and undefined
    !! behaviour where it cannot; passing an assumed-shape slice of the wrong length is the one way
    !! to get that wrong.
    interface pf_cross_product
        module procedure pf_cross_product_r32 !! `real(real32)` vectors.
        module procedure pf_cross_product_r64 !! `real(real64)` vectors.
    end interface pf_cross_product

    ! ---- Case folding ----

    !> Lowercases every ASCII `A`-`Z`, leaving every other byte untouched.
    !!
    !! ASCII only, with no option to widen it: a byte outside `A`-`Z` is copied through unchanged,
    !! so UTF-8 text passes through byte-identical rather than being corrupted the way a
    !! byte-at-a-time table fold corrupts a multi-byte character.
    !!
    !! Two forms. `call pf_to_lower(s, out)` allocates `out` to `len(s)` and writes the folded copy
    !! into it; `call pf_to_lower(text)` folds a `character(len=*)` variable in place, which is what
    !! most call sites want since they already hold a short key in a fixed-length variable.
    interface pf_to_lower
        module procedure pf_to_lower_copy    !! `(s, out)`: folded copy into an allocatable result.
        module procedure pf_to_lower_inplace !! `(text)`: folds a fixed-length variable in place.
    end interface pf_to_lower

    !> Uppercases every ASCII `a`-`z`, leaving every other byte untouched. Identical in shape and
    !! rules to `pf_to_lower`; see that interface for the ASCII-only guarantee and the two forms.
    interface pf_to_upper
        module procedure pf_to_upper_copy    !! `(s, out)`: folded copy into an allocatable result.
        module procedure pf_to_upper_inplace !! `(text)`: folds a fixed-length variable in place.
    end interface pf_to_upper

    ! ---- Value to text ----

    !> Renders one value as text: `call pf_to_str(value, res [, min_width] [, fmt] [, pad])`.
    !!
    !! Five specifics -- `integer(int32)`, `integer(int64)`, `real(real32)`, `real(real64)` and
    !! `logical` -- all with the same optional tail, so there is one signature to learn. `value` is
    !! the value to render and `res` the `character(len=:), allocatable` result; `min_width` is a
    !! **minimum** field width, `fmt` a Fortran format specification including its parentheses, and
    !! `pad` the single character `min_width` pads with.
    !!
    !! **`min_width` is a minimum, never an exact width.** A value needing more characters gets
    !! them: `pf_to_str(123456, res, min_width=4)` is `"123456"`, not a truncation and not an
    !! abort. `min_width <= 0` is the same as omitting it. This is what leaves the module with no
    !! error paths at all.
    !!
    !! **`fmt` renders and `min_width` then pads what it produced**, in that order, so the two
    !! never fight. A `fmt` the I/O runtime rejects yields a run of asterisks -- Fortran's own
    !! overflow marker, and what `pf_str` in `parquet_logging` already does. **The length of that
    !! run is deliberately unspecified and must not be asserted**: the compilers disagree about it,
    !! because some reject a type-mismatched edit descriptor through `iostat` and others write
    !! asterisks themselves and report success.
    !!
    !! **Exactly one case puts padding between the sign and the digits: an INTEGER padded with
    !! `"0"`.** `pf_to_str(-7, res, min_width=5)` is `"-0007"`, because zeros inserted after an
    !! integer's sign do not change the value it reads back as. Everything else pads in front of
    !! the sign -- `pad=" "` gives `"   -7"`, `pad="x"` gives `"xxx-7"`, and `pad="0"` on a
    !! **real** gives `"0000-3.5"`, since there a leading zero run is alignment rather than part of
    !! the number.
    !!
    !! **`pad` defaults to `"0"` for the two integer specifics** -- the zero-padded file counter is
    !! the case this exists for -- **and to `" "` for the real and logical ones**, where
    !! zero-padding is almost never meant. `fmt` defaults to `'(i0)'` and `'(g0)'` respectively.
    !!
    !! **A `logical` renders as `true` or `false`**, lowercase and unabbreviated, so that the
    !! default is a valid TOML boolean and a caller assembling a config file needs no `fmt`.
    !! **This deliberately disagrees with `pf_str` in `parquet_logging`, which renders `T`/`F`**,
    !! and both are in scope at once under `use parquet`. The two have different destinations: a
    !! log column, and text something else will read back. Pass `fmt='(l1)'` for Fortran's own
    !! spelling.
    !!
    !! **The default rendering of a real is not portable and no test may pin it**: `(g0)` gives
    !! `3.1400000000000001` on one compiler and `3.140000000000000` on another for the same
    !! `3.14_real64`. Callers needing specific text pass `fmt`.
    interface pf_to_str
        module procedure pf_to_str_i32 !! `integer(int32)`; `fmt` defaults to `'(i0)'`, `pad` to `"0"`.
        module procedure pf_to_str_i64 !! `integer(int64)`; `fmt` defaults to `'(i0)'`, `pad` to `"0"`.
        module procedure pf_to_str_r32 !! `real(real32)`; `fmt` defaults to `'(g0)'`, `pad` to `" "`.
        module procedure pf_to_str_r64 !! `real(real64)`; `fmt` defaults to `'(g0)'`, `pad` to `" "`.
        module procedure pf_to_str_log !! `logical`; renders `true`/`false`, `pad` defaults to `" "`.
    end interface pf_to_str

    ! ---- Text to value ----

    !> Reads one value back out of text: `call pf_from_str(text, value, ok)`.
    !!
    !! The inverse of `pf_to_str`, with five specifics over the same types -- `integer(int32)`,
    !! `integer(int64)`, `real(real32)`, `real(real64)` and `logical`. `text` is the text to read,
    !! `value` receives the number and `ok` says whether it could be read at all.
    !!
    !! **`value` is NOT assigned when `ok` is `.false.`, and must not be read then.** Assigning a
    !! zero instead would hand a caller who forgets to test `ok` a plausible wrong number for
    !! `"abc"` -- which is the exact failure this parser exists to prevent. Leaving it undefined
    !! means a checking build (nagfor's `-nan`, or `-C=undefined`) reports that caller instead of
    !! agreeing with them.
    !!
    !! **It is STRICT, and that is the whole reason it exists rather than a `read`.** A
    !! list-directed `read(text, *, iostat=)` rejects `"5abc"` and `"3.9"` as you would hope, and
    !! accepts **`"5 6"` with `iostat == 0`, yielding 5** -- so an ID with a stray space silently
    !! becomes a different, plausible ID. `parquet_settings`' own environment parser documents the
    !! same trap and checks the digits by hand for the same reason. Here the shape is checked
    !! first and the `read` runs only once it is known to be sound.
    !!
    !! **What each specific accepts**, after leading and trailing blanks are trimmed (a
    !! `character` array element is blank-padded by construction, so trailing blanks cannot carry
    !! meaning) and with **no embedded blank anywhere**:
    !!
    !! * an integer: an optional `+` or `-`, then one or more digits, then nothing else;
    !! * a real: a Fortran real literal -- an optional sign, then digits with an optional `.` and
    !!   optional fractional digits (`"12"`, `"12."`, `".5"` all read), then an optional exponent
    !!   `e`/`E`/`d`/`D` with its own optional sign and one or more digits. No kind suffix
    !!   (`"1.0_real64"`), no `q` exponent, and **no `"nan"` or `"inf"`**: neither is a Fortran
    !!   literal, and text reading `nan` in a data file is far more often a missing-value
    !!   placeholder than a deliberate NaN;
    !! * a logical: `true`, `false`, `t`, `f`, `1` or `0`, case-insensitively, and nothing else.
    !!   That set is exactly what this library's two renderers produce -- `pf_to_str` writes
    !!   `true`/`false` and `pf_str` in `parquet_logging` writes `T`/`F` -- plus the `1`/`0` a
    !!   shell or a CSV produces. `on`/`off`/`yes`/`no` are deliberately not accepted.
    !!
    !! **A value the target kind cannot represent is not readable into it**, so `"1e300"` reads
    !! into a `real(real64)` and not into a `real(real32)`, and `"2147483648"` reads into an
    !! `integer(int64)` and not into an `integer(int32)`. **Rounding is not overflow**: `"0.1"`
    !! and `"1e-300"` read into a `real32` as the nearest value it has, exactly as any other
    !! narrowing in this library rounds silently.
    interface pf_from_str
        module procedure pf_from_str_i32 !! `integer(int32)`; optional sign then digits.
        module procedure pf_from_str_i64 !! `integer(int64)`; optional sign then digits.
        module procedure pf_from_str_r32 !! `real(real32)`; a Fortran real literal.
        module procedure pf_from_str_r64 !! `real(real64)`; a Fortran real literal.
        module procedure pf_from_str_log !! `logical`; `true`/`false`/`t`/`f`/`1`/`0`.
    end interface pf_from_str

    ! ---- Path joining ----

    !> Joins path components with exactly one `/` between them, following CPython's
    !! `posixpath.join` rules exactly.
    !!
    !! Available for two to five scalar components -- `call pf_join_path(a, b [, c] [, d] [, e],
    !! path)` -- and for an array, `call pf_join_path(parts, path)`.
    !!
    !! Three rules, applied left to right, and they are the whole specification:
    !!
    !! * a component beginning with `/` **restarts** the path, discarding everything before it, so
    !!   `pf_join_path("a", "/b", p)` is `"/b"` and there is no second procedure to remember for
    !!   the absolute case;
    !! * when the accumulated path is empty or already ends with `/`, the next component is
    !!   appended directly, so `pf_join_path("a/", "b", p)` is `"a/b"` and `pf_join_path("./", "b",
    !!   p)` is `"./b"`;
    !! * otherwise exactly one `/` is inserted.
    !!
    !! **An empty final component still gets its separator: `pf_join_path("a", "", p)` is `"a/"`,
    !! not `"a"`.** This is Python's behaviour and is kept deliberately, but it is the one rule that
    !! surprises people and it bites in a plausible pattern -- `pf_join_path(dir, name, out)` where
    !! `name` happens to be empty yields a directory-looking path rather than `dir` itself.
    !!
    !! Interior duplicate separators are preserved (`"a//b"` stays `"a//b"`); what the rules
    !! guarantee is that no *second* separator is ever added. Nothing is normalised: `"a/b/../c"`
    !! comes back unchanged.
    !!
    !! **Two Fortran-specific extensions of the Python contract**, since `posixpath.join()` with no
    !! arguments raises `TypeError` and cannot be consulted: the array form over a **zero-size**
    !! array returns an allocated, zero-length string -- the identity of the fold, so a caller
    !! accumulating components needs no special case -- and over a **one-element** array returns
    !! that component, trimmed.
    interface pf_join_path
        module procedure pf_join_path_2    !! `(path1, path2, path)`.
        module procedure pf_join_path_3    !! `(path1, path2, path3, path)`.
        module procedure pf_join_path_4    !! `(path1, path2, path3, path4, path)`.
        module procedure pf_join_path_5    !! `(path1, path2, path3, path4, path5, path)`.
        module procedure pf_join_path_many !! `(parts, path)`: an array of components.
    end interface pf_join_path

contains

    ! ================================================================================
    ! Total arithmetic
    ! ================================================================================

    !> `a/b` when `b` is non-zero, and the IEEE value the division would have produced otherwise.
    !! See `pf_safe_div`.
    pure elemental function pf_safe_div_r32(a, b) result(res)
        real(real32), intent(in) :: a !! the numerator.
        real(real32), intent(in) :: b !! the denominator; zero is expected here, not exceptional.
        real(real32) :: res !! `a/b`; `+/-Infinity` when `b == 0` and `a /= 0`, NaN when both are 0.

        if (b /= 0.0_real32) then
            res = a / b
        else if (a > 0.0_real32) then
            res = ieee_value(res, ieee_positive_inf)
        else if (a < 0.0_real32) then
            res = ieee_value(res, ieee_negative_inf)
        else
            ! Both zero, or `a` is a NaN: a quiet NaN either way, and the NaN case reaches here
            ! only because a NaN compares false against both bounds above.
            res = ieee_value(res, ieee_quiet_nan)
        end if
    end function pf_safe_div_r32

    !> `a/b` when `b` is non-zero, and the IEEE value the division would have produced otherwise.
    !! See `pf_safe_div`.
    pure elemental function pf_safe_div_r64(a, b) result(res)
        real(real64), intent(in) :: a !! the numerator.
        real(real64), intent(in) :: b !! the denominator; zero is expected here, not exceptional.
        real(real64) :: res !! `a/b`; `+/-Infinity` when `b == 0` and `a /= 0`, NaN when both are 0.

        if (b /= 0.0_real64) then
            res = a / b
        else if (a > 0.0_real64) then
            res = ieee_value(res, ieee_positive_inf)
        else if (a < 0.0_real64) then
            res = ieee_value(res, ieee_negative_inf)
        else
            res = ieee_value(res, ieee_quiet_nan)
        end if
    end function pf_safe_div_r64

    ! ================================================================================
    ! The standard normal distribution
    ! ================================================================================
    !
    ! The three forward functions are one intrinsic call each. `pf_probit` is the work, and its
    ! two branches, its step counts and the seam between them were all chosen by a sweep.
    ! What the code below must preserve, whatever is tuned:
    !
    !   * the NaN screen is an EQUALITY test and comes before every ordered comparison;
    !   * the magnitude is computed from `q = min(p, 1-p)` and the sign applied last, which is
    !     what makes the result antisymmetric rather than approximately so;
    !   * the central branch iterates on `erf`, not on `Phi`;
    !   * the tail branch iterates on `log(Phi)` through `erfc_scaled`, and its first step is
    !     Newton (globally convergent here) before Halley takes over.

    !> `erfinv(y)` to a few parts in 1e4 over `|y| <= 0.8`, from its Maclaurin series.
    !!
    !! The starting estimate for `pf_probit`'s central branch, and **only** a starting estimate:
    !! the Halley steps that follow converge from anywhere, so this decides how many of them are
    !! needed and nothing else. Horner in `y*y`, which is what makes it odd in `y` exactly.
    pure function erfinv_series_r64(y) result(w)
        real(real64), intent(in) :: y !! `erf`'s value, `1 - 2q`; meant for `|y| <= 0.8`.
        real(real64) :: w !! the approximate `erfinv(y)`.
        real(real64) :: y2
        integer :: k

        y2 = y * y
        w = ERFINV_C_R64(size(ERFINV_C_R64))
        do k = size(ERFINV_C_R64) - 1, 1, -1
            w = w * y2 + ERFINV_C_R64(k)
        end do
        w = w * y
    end function erfinv_series_r64

    !> `erfinv(y)` in `real32`. See `erfinv_series_r64`.
    pure function erfinv_series_r32(y) result(w)
        real(real32), intent(in) :: y !! `erf`'s value, `1 - 2q`; meant for `|y| <= 0.8`.
        real(real32) :: w !! the approximate `erfinv(y)`.
        real(real32) :: y2
        integer :: k

        y2 = y * y
        w = ERFINV_C_R32(size(ERFINV_C_R32))
        do k = size(ERFINV_C_R32) - 1, 1, -1
            w = w * y2 + ERFINV_C_R32(k)
        end do
        w = w * y
    end function erfinv_series_r32

    !> The standard normal quantile function. See `pf_probit`.
    pure elemental function pf_probit_r64(p) result(res)
        real(real64), intent(in) :: p !! the probability; outside `[0, 1]` gives a NaN, not an abort.
        real(real64) :: res !! `Phi**(-1)(p)`; `-/+Infinity` at 0 and 1, a quiet NaN outside `[0, 1]`.
        real(real64) :: q, y, w, f, u, r, t, x, sc, h, rr, lq
        integer :: k

        ! The NaN screen is first, and it is an EQUALITY test on purpose: `<` and `>` are
        ! signalling comparisons and raise IEEE_INVALID on a quiet NaN, which is fatal under
        ! nagfor's default -ieee=stop. `p /= p` is quiet and true only for a NaN.
        if (p /= p) then
            res = ieee_value(res, ieee_quiet_nan)
            return
        end if
        ! `p` is a number from here, so every ordered comparison below raises nothing.
        if (p == 0.5_real64) then
            res = 0.0_real64          ! exact, and a POSITIVE zero: the sign rule below would
            return                    ! otherwise hand back -0.0 for the one input where it shows.
        end if
        if (p <= 0.0_real64) then
            if (p == 0.0_real64) then
                res = ieee_value(res, ieee_negative_inf)
            else
                res = ieee_value(res, ieee_quiet_nan)
            end if
            return
        end if
        if (p >= 1.0_real64) then
            if (p == 1.0_real64) then
                res = ieee_value(res, ieee_positive_inf)
            else
                res = ieee_value(res, ieee_quiet_nan)
            end if
            return
        end if
        ! 0 < p < 1 and p /= 1/2. The magnitude comes from the SMALLER tail and the sign is
        ! applied at the end, which is what makes the result exactly antisymmetric.
        if (p < 0.5_real64) then
            q = p
        else
            q = 1.0_real64 - p
        end if

        if (q >= PROBIT_QSPLIT_R64) then
            ! Central: solve erf(w) = y for w >= 0. `1 - 2q` is exact for q >= 1/4 (Sterbenz) and
            ! loses nothing above the seam either.
            y = 1.0_real64 - 2.0_real64 * q
            w = erfinv_series_r64(y)
            do k = 1, 2
                ! Halley on f = erf(w) - y, with f' = (2/sqrt(pi))*exp(-w*w) and f'' = -2w*f',
                ! so the second-order term costs one multiply rather than another transcendental.
                f = erf(w) - y
                u = f / (TWO_OVER_SQRTPI_R64 * exp(-w * w))
                w = w - u / (1.0_real64 + u * w)
            end do
            x = -SQRT2_R64 * w
        else
            ! Tail: solve log(Phi(x)) = log(q). With t = -x/sqrt(2) >= 0 and sc = erfc_scaled(t),
            ! log(Phi(x)) is log(0.5*sc) - x*x/2 and phi(x)/Phi(x) is sqrt(2/pi)/sc, so nothing
            ! underflows however small q is -- which is what carries this into the subnormals.
            lq = log(q)
            r = -2.0_real64 * lq
            ! The starting estimate inverts q ~ phi(t)/t once. `r - log(r) - log(2*pi)` is
            ! positive for every q below the seam: it increases with r and is already 1.24 at
            ! r = -2*log(0.1), so it needs no guard before the square root.
            t = sqrt(r - log(r) - LN_2PI_R64)
            x = -t
            ! One Newton step first. log(Phi) is concave and increasing, so its tangent lies above
            ! it and this lands at or beyond the root from any start; every step after it is
            ! monotone. That is what makes a FIXED step count safe rather than merely measured.
            sc = erfc_scaled(-x * INV_SQRT2_R64)
            h = log(0.5_real64 * sc) - 0.5_real64 * x * x - lq
            x = x - h * sc / SQRT_2_OVER_PI_R64
            do k = 1, 2
                sc = erfc_scaled(-x * INV_SQRT2_R64)
                h = log(0.5_real64 * sc) - 0.5_real64 * x * x - lq
                rr = SQRT_2_OVER_PI_R64 / sc                      ! phi(x)/Phi(x)
                x = x - 2.0_real64 * h / (2.0_real64 * rr + h * (x + rr))
            end do
        end if

        if (p < 0.5_real64) then
            res = x
        else
            res = -x
        end if
    end function pf_probit_r64

    !> The standard normal quantile function in `real32`. See `pf_probit`.
    !!
    !! The same scheme with one fewer tail step: 24 bits of mantissa are reached by the Newton
    !! step plus one Halley step, where `real64` needs two. The central branch keeps both, because
    !! a single Halley step there lands at about 1e-7 relative, which is `real32`'s own precision
    !! rather than comfortably inside it.
    pure elemental function pf_probit_r32(p) result(res)
        real(real32), intent(in) :: p !! the probability; outside `[0, 1]` gives a NaN, not an abort.
        real(real32) :: res !! `Phi**(-1)(p)`; `-/+Infinity` at 0 and 1, a quiet NaN outside `[0, 1]`.
        real(real32) :: q, y, w, f, u, r, t, x, sc, h, rr, lq
        integer :: k

        if (p /= p) then
            res = ieee_value(res, ieee_quiet_nan)
            return
        end if
        if (p == 0.5_real32) then
            res = 0.0_real32
            return
        end if
        if (p <= 0.0_real32) then
            if (p == 0.0_real32) then
                res = ieee_value(res, ieee_negative_inf)
            else
                res = ieee_value(res, ieee_quiet_nan)
            end if
            return
        end if
        if (p >= 1.0_real32) then
            if (p == 1.0_real32) then
                res = ieee_value(res, ieee_positive_inf)
            else
                res = ieee_value(res, ieee_quiet_nan)
            end if
            return
        end if
        if (p < 0.5_real32) then
            q = p
        else
            q = 1.0_real32 - p
        end if

        if (q >= PROBIT_QSPLIT_R32) then
            y = 1.0_real32 - 2.0_real32 * q
            w = erfinv_series_r32(y)
            do k = 1, 2
                f = erf(w) - y
                u = f / (TWO_OVER_SQRTPI_R32 * exp(-w * w))
                w = w - u / (1.0_real32 + u * w)
            end do
            x = -SQRT2_R32 * w
        else
            lq = log(q)
            r = -2.0_real32 * lq
            t = sqrt(r - log(r) - LN_2PI_R32)
            x = -t
            sc = erfc_scaled(-x * INV_SQRT2_R32)
            h = log(0.5_real32 * sc) - 0.5_real32 * x * x - lq
            x = x - h * sc / SQRT_2_OVER_PI_R32
            sc = erfc_scaled(-x * INV_SQRT2_R32)
            h = log(0.5_real32 * sc) - 0.5_real32 * x * x - lq
            rr = SQRT_2_OVER_PI_R32 / sc
            x = x - 2.0_real32 * h / (2.0_real32 * rr + h * (x + rr))
        end if

        if (p < 0.5_real32) then
            res = x
        else
            res = -x
        end if
    end function pf_probit_r32

    ! The NaN screen in the four `erfc` bodies below is NOT redundant, however much it looks it.
    ! **nagfor's `ERFC` returns 0 for a quiet NaN, and its `ERF` returns 1** (measured, 7.2 on
    ! arm64; gfortran and flang both propagate). Without the screen `pf_norm_cdf(NaN)` is a
    ! probability of exactly zero on the maintainer's default compiler -- in range, plausible, and
    ! silent. `fortran-gotchas.md` carries the rule. `pf_norm_pdf` needs no screen because it goes
    ! through arithmetic and `exp`, which propagate a NaN on every compiler measured, and its test
    ! is what would say otherwise.

    !> `Phi(z)`, the standard normal distribution function. See `pf_norm_cdf`.
    pure elemental function pf_norm_cdf_r32(z) result(res)
        real(real32), intent(in) :: z !! the quantile.
        real(real32) :: res !! `Phi(z)`, in `[0, 1]`.

        if (z /= z) then
            res = ieee_value(res, ieee_quiet_nan)
        else
            res = 0.5_real32 * erfc(-z * INV_SQRT2_R32)
        end if
    end function pf_norm_cdf_r32

    !> `Phi(z)`, the standard normal distribution function. See `pf_norm_cdf`.
    pure elemental function pf_norm_cdf_r64(z) result(res)
        real(real64), intent(in) :: z !! the quantile.
        real(real64) :: res !! `Phi(z)`, in `[0, 1]`.

        if (z /= z) then
            res = ieee_value(res, ieee_quiet_nan)
        else
            res = 0.5_real64 * erfc(-z * INV_SQRT2_R64)
        end if
    end function pf_norm_cdf_r64

    !> `1 - Phi(z)`, the standard normal survival function. See `pf_norm_sf`.
    pure elemental function pf_norm_sf_r32(z) result(res)
        real(real32), intent(in) :: z !! the quantile.
        real(real32) :: res !! `1 - Phi(z)`, in `[0, 1]`; NOT formed by that subtraction.

        if (z /= z) then
            res = ieee_value(res, ieee_quiet_nan)
        else
            res = 0.5_real32 * erfc(z * INV_SQRT2_R32)
        end if
    end function pf_norm_sf_r32

    !> `1 - Phi(z)`, the standard normal survival function. See `pf_norm_sf`.
    pure elemental function pf_norm_sf_r64(z) result(res)
        real(real64), intent(in) :: z !! the quantile.
        real(real64) :: res !! `1 - Phi(z)`, in `[0, 1]`; NOT formed by that subtraction.

        if (z /= z) then
            res = ieee_value(res, ieee_quiet_nan)
        else
            res = 0.5_real64 * erfc(z * INV_SQRT2_R64)
        end if
    end function pf_norm_sf_r64

    !> `phi(z)`, the standard normal density. See `pf_norm_pdf`.
    pure elemental function pf_norm_pdf_r32(z) result(res)
        real(real32), intent(in) :: z !! the quantile.
        real(real32) :: res !! `exp(-z*z/2)/sqrt(2*pi)`; underflows to 0 in the far tail.

        res = INV_SQRT_2PI_R32 * exp(-0.5_real32 * z * z)
    end function pf_norm_pdf_r32

    !> `phi(z)`, the standard normal density. See `pf_norm_pdf`.
    pure elemental function pf_norm_pdf_r64(z) result(res)
        real(real64), intent(in) :: z !! the quantile.
        real(real64) :: res !! `exp(-z*z/2)/sqrt(2*pi)`; underflows to 0 in the far tail.

        res = INV_SQRT_2PI_R64 * exp(-0.5_real64 * z * z)
    end function pf_norm_pdf_r64

    ! ================================================================================
    ! Angles
    ! ================================================================================
    !
    ! Every one of the eight wraps is `modulo` plus one comparison. The comparison is not
    ! defensive: `modulo` returns the divisor itself when the true result rounds up to it, so
    ! without it the advertised range would be closed at the top on some inputs. Measured values
    ! are in the module header.

    !> An angle in degrees reduced to `[0, 360)`. See `pf_wrap_deg`.
    pure elemental function pf_wrap_deg_r32(angle) result(res)
        real(real32), intent(in) :: angle !! an angle in degrees, of any magnitude and sign.
        real(real32) :: res !! the same direction, in `[0, 360)`.

        res = modulo(angle, 360.0_real32)
        if (res >= 360.0_real32) res = 0.0_real32
    end function pf_wrap_deg_r32

    !> An angle in degrees reduced to `[0, 360)`. See `pf_wrap_deg`.
    pure elemental function pf_wrap_deg_r64(angle) result(res)
        real(real64), intent(in) :: angle !! an angle in degrees, of any magnitude and sign.
        real(real64) :: res !! the same direction, in `[0, 360)`.

        res = modulo(angle, 360.0_real64)
        if (res >= 360.0_real64) res = 0.0_real64
    end function pf_wrap_deg_r64

    !> An angle in degrees reduced to `[-180, 180)`. See `pf_wrap_180`.
    pure elemental function pf_wrap_180_r32(angle) result(res)
        real(real32), intent(in) :: angle !! an angle in degrees, of any magnitude and sign.
        real(real32) :: res !! the same direction, in `[-180, 180)`.

        res = modulo(angle + 180.0_real32, 360.0_real32) - 180.0_real32
        if (res >= 180.0_real32) res = -180.0_real32
    end function pf_wrap_180_r32

    !> An angle in degrees reduced to `[-180, 180)`. See `pf_wrap_180`.
    pure elemental function pf_wrap_180_r64(angle) result(res)
        real(real64), intent(in) :: angle !! an angle in degrees, of any magnitude and sign.
        real(real64) :: res !! the same direction, in `[-180, 180)`.

        res = modulo(angle + 180.0_real64, 360.0_real64) - 180.0_real64
        if (res >= 180.0_real64) res = -180.0_real64
    end function pf_wrap_180_r64

    !> An angle in radians reduced to `[0, 2*pi)`. See `pf_wrap_rad`.
    pure elemental function pf_wrap_rad_r32(angle) result(res)
        real(real32), intent(in) :: angle !! an angle in radians, of any magnitude and sign.
        real(real32) :: res !! the same direction, in `[0, 2*pi)`.

        res = modulo(angle, TWOPI_R32)
        if (res >= TWOPI_R32) res = 0.0_real32
    end function pf_wrap_rad_r32

    !> An angle in radians reduced to `[0, 2*pi)`. See `pf_wrap_rad`.
    pure elemental function pf_wrap_rad_r64(angle) result(res)
        real(real64), intent(in) :: angle !! an angle in radians, of any magnitude and sign.
        real(real64) :: res !! the same direction, in `[0, 2*pi)`.

        res = modulo(angle, PF_TWOPI)
        if (res >= PF_TWOPI) res = 0.0_real64
    end function pf_wrap_rad_r64

    !> An angle in radians reduced to `[-pi, pi)`. See `pf_wrap_pi`.
    pure elemental function pf_wrap_pi_r32(angle) result(res)
        real(real32), intent(in) :: angle !! an angle in radians, of any magnitude and sign.
        real(real32) :: res !! the same direction, in `[-pi, pi)`.

        res = modulo(angle + PI_R32, TWOPI_R32) - PI_R32
        if (res >= PI_R32) res = -PI_R32
    end function pf_wrap_pi_r32

    !> An angle in radians reduced to `[-pi, pi)`. See `pf_wrap_pi`.
    pure elemental function pf_wrap_pi_r64(angle) result(res)
        real(real64), intent(in) :: angle !! an angle in radians, of any magnitude and sign.
        real(real64) :: res !! the same direction, in `[-pi, pi)`.

        res = modulo(angle + PF_PI, PF_TWOPI) - PF_PI
        if (res >= PF_PI) res = -PF_PI
    end function pf_wrap_pi_r64

    !> Degrees to radians. See `pf_deg2rad`.
    pure elemental function pf_deg2rad_r32(angle) result(res)
        real(real32), intent(in) :: angle !! an angle in degrees.
        real(real32) :: res !! the same angle in radians.

        res = angle * DEG2RAD_R32
    end function pf_deg2rad_r32

    !> Degrees to radians. See `pf_deg2rad`.
    pure elemental function pf_deg2rad_r64(angle) result(res)
        real(real64), intent(in) :: angle !! an angle in degrees.
        real(real64) :: res !! the same angle in radians.

        res = angle * PF_RAD_PER_DEG
    end function pf_deg2rad_r64

    !> Radians to degrees. See `pf_rad2deg`.
    pure elemental function pf_rad2deg_r32(angle) result(res)
        real(real32), intent(in) :: angle !! an angle in radians.
        real(real32) :: res !! the same angle in degrees.

        res = angle * RAD2DEG_R32
    end function pf_rad2deg_r32

    !> Radians to degrees. See `pf_rad2deg`.
    pure elemental function pf_rad2deg_r64(angle) result(res)
        real(real64), intent(in) :: angle !! an angle in radians.
        real(real64) :: res !! the same angle in degrees.

        res = angle * PF_DEG_PER_RAD
    end function pf_rad2deg_r64

    ! ================================================================================
    ! Vectors
    ! ================================================================================

    !> The cross product of two 3-vectors. See `pf_cross_product`.
    pure function pf_cross_product_r32(a, b) result(res)
        real(real32), intent(in), dimension(3) :: a !! the left operand.
        real(real32), intent(in), dimension(3) :: b !! the right operand.
        real(real32), dimension(3) :: res !! `a x b`.

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)
    end function pf_cross_product_r32

    !> The cross product of two 3-vectors. See `pf_cross_product`.
    pure function pf_cross_product_r64(a, b) result(res)
        real(real64), intent(in), dimension(3) :: a !! the left operand.
        real(real64), intent(in), dimension(3) :: b !! the right operand.
        real(real64), dimension(3) :: res !! `a x b`.

        res(1) = a(2) * b(3) - a(3) * b(2)
        res(2) = a(3) * b(1) - a(1) * b(3)
        res(3) = a(1) * b(2) - a(2) * b(1)
    end function pf_cross_product_r64

    ! ================================================================================
    ! Case folding
    ! ================================================================================

    !> Folds one byte to lower case if it is an ASCII `A`-`Z`, and returns it unchanged otherwise.
    pure function lower_byte(c) result(res)
        character(len=1), intent(in) :: c !! the byte to fold.
        character(len=1) :: res !! `c` lowercased if it was an ASCII capital, else `c` itself.
        integer :: code

        code = iachar(c)
        if (code >= iachar("A") .and. code <= iachar("Z")) then
            res = achar(code - iachar("A") + iachar("a"))
        else
            res = c
        end if
    end function lower_byte

    !> Folds one byte to upper case if it is an ASCII `a`-`z`, and returns it unchanged otherwise.
    pure function upper_byte(c) result(res)
        character(len=1), intent(in) :: c !! the byte to fold.
        character(len=1) :: res !! `c` uppercased if it was an ASCII lower-case letter, else `c`.
        integer :: code

        code = iachar(c)
        if (code >= iachar("a") .and. code <= iachar("z")) then
            res = achar(code - iachar("a") + iachar("A"))
        else
            res = c
        end if
    end function upper_byte

    !> Writes a lowercased copy of `s` into a freshly allocated `out` of the same length.
    pure subroutine pf_to_lower_copy(s, out)
        character(len=*), intent(in) :: s !! text to fold; every byte is preserved except ASCII `A`-`Z`.
        character(len=:), allocatable, intent(out) :: out !! the folded copy, allocated to `len(s)`.
        integer :: i

        allocate (character(len=len(s)) :: out)
        do i = 1, len(s)
            out(i:i) = lower_byte(s(i:i))
        end do
    end subroutine pf_to_lower_copy

    !> Lowercases `text` in place, leaving its length and every non-`A`-`Z` byte unchanged.
    pure subroutine pf_to_lower_inplace(text)
        character(len=*), intent(inout) :: text !! text folded in place; trailing blanks stay blanks.
        integer :: i

        do i = 1, len(text)
            text(i:i) = lower_byte(text(i:i))
        end do
    end subroutine pf_to_lower_inplace

    !> Writes an uppercased copy of `s` into a freshly allocated `out` of the same length.
    pure subroutine pf_to_upper_copy(s, out)
        character(len=*), intent(in) :: s !! text to fold; every byte is preserved except ASCII `a`-`z`.
        character(len=:), allocatable, intent(out) :: out !! the folded copy, allocated to `len(s)`.
        integer :: i

        allocate (character(len=len(s)) :: out)
        do i = 1, len(s)
            out(i:i) = upper_byte(s(i:i))
        end do
    end subroutine pf_to_upper_copy

    !> Uppercases `text` in place, leaving its length and every non-`a`-`z` byte unchanged.
    pure subroutine pf_to_upper_inplace(text)
        character(len=*), intent(inout) :: text !! text folded in place; trailing blanks stay blanks.
        integer :: i

        do i = 1, len(text)
            text(i:i) = upper_byte(text(i:i))
        end do
    end subroutine pf_to_upper_inplace

    ! ================================================================================
    ! Value to text
    ! ================================================================================

    !> Pads `text` on the left to at least `min_width` characters.
    !!
    !! **Exactly one case puts the padding between the sign and the digits: an INTEGER padded with
    !! `"0"`.** `-7` at `min_width=5` is `"-0007"` there, because inserting zeros immediately after
    !! the sign of an integer does not change the value it reads back as. Every other combination
    !! pads in front of the sign, including `pad=" "` (`"   -7"`), any other character
    !! (`"xxx-7"`), and `"0"` on a real -- where a leading zero run is alignment rather than part
    !! of the number, so it goes outside the sign like any other pad.
    pure subroutine pad_left(text, min_width, padc, integer_value, res)
        character(len=*), intent(in) :: text !! the rendered text, used verbatim.
        integer, intent(in) :: min_width !! minimum total width; `<= len(text)` copies `text` through.
        character(len=1), intent(in) :: padc !! the padding character.
        logical, intent(in) :: integer_value !! `.true.` only from the two integer specifics.
        character(len=:), allocatable, intent(out) :: res !! `text`, padded; always allocated.
        integer :: nadd
        logical :: signed

        nadd = min_width - len(text)
        if (nadd <= 0) then
            res = text
            return
        end if
        ! Nested rather than `.and.`-ed: Fortran does not short-circuit, so a single
        ! `if (len(text) > 0 .and. text(1:1) == "-")` would evaluate `text(1:1)` on an empty string.
        signed = .false.
        if (len(text) > 0) signed = (text(1:1) == "-" .or. text(1:1) == "+")
        if (signed .and. integer_value .and. padc == "0") then
            res = text(1:1)//repeat(padc, nadd)//text(2:)
        else
            res = repeat(padc, nadd)//text
        end if
    end subroutine pad_left

    !> The result returned for a `fmt` the I/O runtime rejects. A run of asterisks, Fortran's own
    !! overflow marker; its length is not part of the contract and no test may assert one.
    pure subroutine bad_format_result(res)
        character(len=:), allocatable, intent(out) :: res !! a short run of asterisks.

        res = repeat("*", 3)
    end subroutine bad_format_result

    !> Whether a `write` that reported an error nevertheless left the runtime's OVERFLOW MARKER
    !! in `buf`, in which case that marker is the result to keep.
    !!
    !! **A nonzero `iostat` does not always mean the format was rejected.** A value too wide for
    !! its edit descriptor is not an error in the standard -- the runtime fills the field with
    !! asterisks and reports success -- but ifx's `-check output_conversion`, which `-check all`
    !! and so `--profile debug` turn on, reports `iostat = 63` for exactly that case. The text is
    !! rendered either way. Taking the `iostat` alone would therefore make a diagnostic build
    !! return a different string from a plain one: `pf_to_str(12345, s, fmt='(i2)')` gives `**`
    !! normally and, without this, `bad_format_result`'s `***` under `-check all`.
    !!
    !! The discriminator cannot be the code number -- they are per-compiler (ifx 63 for the
    !! conversion and 62 for a rejected format, gfortran 5006, flang 1005, nagfor 125) and nothing
    !! portable tells them apart. **It is the ASTERISK, and specifically not "the buffer is
    !! non-empty".** That earlier rule was measured on ifx 2026.1 and gfortran 15.2, where a
    !! rejected format renders nothing at all, and generalised from those two -- and flang 22.1.8
    !! is a third behaviour neither of them has: it reports the rejection through `iostat` and
    !! still leaves PARTIAL TEXT behind, `1` for `pf_to_str(1, s, fmt='(nonsense)')` and
    !! `377600000000000000000` for the `real64` form. Under the non-empty rule those were returned
    !! as if they were renderings, so a rejected format silently produced a plausible wrong string
    !! instead of asterisks.
    !!
    !! Keying on the marker is narrower and says what it means: the ONLY nonzero-`iostat` case this
    !! function exists to accept is the field overflow, and an asterisk is what defines that case.
    !! It also keeps a compound format working -- `'("x=",i2)'` renders `x=**`, which is text a
    !! whole-buffer "all asterisks" test would have rejected. Partial text carrying no asterisk is
    !! a format that got part-way and failed, which is exactly what must fall through to
    !! `bad_format_result`.
    !!
    !! **Callers must blank `buf` before the write.** It is an uninitialised local otherwise, and
    !! this would read whatever the stack held.
    pure logical function rendered_ok(ios, buf) result(ok)
        integer, intent(in) :: ios !! the `iostat` the write reported.
        character(len=*), intent(in) :: buf !! the buffer it wrote into, blanked beforehand.

        ok = ios == 0
        if (.not. ok) ok = index(buf, "*") > 0
    end function rendered_ok

    !> Applies the optional `min_width`/`pad` tail to already-rendered text.
    !!
    !! Factored out because all five specifics share it exactly; `min_width` absent or `<= 0` and
    !! the text is copied through unchanged.
    pure subroutine finish_to_str(text, res, min_width, pad, default_pad, integer_value)
        character(len=*), intent(in) :: text !! the rendered text.
        character(len=:), allocatable, intent(out) :: res !! the padded result; always allocated.
        integer, intent(in), optional :: min_width !! caller's minimum width, if any.
        character(len=1), intent(in), optional :: pad !! caller's padding character, if any.
        character(len=1), intent(in) :: default_pad !! this specific's default padding character.
        logical, intent(in) :: integer_value !! `.true.` only from the two integer specifics; see `pad_left`.
        character(len=1) :: padc

        if (.not. present(min_width)) then
            res = text
            return
        end if
        if (min_width <= 0) then
            res = text
            return
        end if
        padc = default_pad
        if (present(pad)) padc = pad
        call pad_left(text, min_width, padc, integer_value, res)
    end subroutine finish_to_str

    !> Renders one `integer(int32)` as text. See the `pf_to_str` interface for the full contract.
    pure subroutine pf_to_str_i32(value, res, min_width, fmt, pad)
        integer(int32), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(i0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `"0"`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(i0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, "0", .true.)
    end subroutine pf_to_str_i32

    !> Renders one `integer(int64)` as text. See the `pf_to_str` interface for the full contract.
    pure subroutine pf_to_str_i64(value, res, min_width, fmt, pad)
        integer(int64), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(i0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `"0"`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(i0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, "0", .true.)
    end subroutine pf_to_str_i64

    !> Renders one `real(real32)` as text. See the `pf_to_str` interface for the full contract, and
    !! note that the default `(g0)` rendering is not portable between compilers.
    pure subroutine pf_to_str_r32(value, res, min_width, fmt, pad)
        real(real32), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(g0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `" "`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(g0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, " ", .false.)
    end subroutine pf_to_str_r32

    !> Renders one `real(real64)` as text. See the `pf_to_str` interface for the full contract, and
    !! note that the default `(g0)` rendering is not portable between compilers.
    pure subroutine pf_to_str_r64(value, res, min_width, fmt, pad)
        real(real64), intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification with parentheses; default `'(g0)'`.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `" "`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        buf = ''
        if (present(fmt)) then
            write (buf, fmt, iostat=ios) value
        else
            write (buf, '(g0)', iostat=ios) value
        end if
        if (.not. rendered_ok(ios, buf)) then
            call bad_format_result(res)
            return
        end if
        call finish_to_str(trim(buf), res, min_width, pad, " ", .false.)
    end subroutine pf_to_str_r64

    !> Renders one `logical` as `true` or `false` -- the TOML spelling, deliberately unlike
    !! `pf_str` in `parquet_logging`, which renders `T`/`F`. See the `pf_to_str` interface.
    pure subroutine pf_to_str_log(value, res, min_width, fmt, pad)
        logical, intent(in) :: value !! the value to render.
        character(len=:), allocatable, intent(out) :: res !! the rendered text; always allocated.
        integer, intent(in), optional :: min_width !! minimum field width; a longer value keeps its length.
        character(len=*), intent(in), optional :: fmt !! format specification; no default -- `true`/`false` is written directly.
        character(len=1), intent(in), optional :: pad !! left-padding character; default `" "`.
        character(len=TO_STR_BUF) :: buf
        integer :: ios

        if (present(fmt)) then
            buf = ''
            write (buf, fmt, iostat=ios) value
            if (.not. rendered_ok(ios, buf)) then
                call bad_format_result(res)
                return
            end if
            call finish_to_str(trim(buf), res, min_width, pad, " ", .false.)
        else if (value) then
            call finish_to_str("true", res, min_width, pad, " ", .false.)
        else
            call finish_to_str("false", res, min_width, pad, " ", .false.)
        end if
    end subroutine pf_to_str_log

    ! ================================================================================
    ! Text to value
    ! ================================================================================

    !> The bounds of `text` with its leading and trailing blanks removed, as an index pair.
    !!
    !! Returned as indices rather than as a trimmed copy on purpose: `pf_from_str` is called once
    !! per ROW by `parquet_table`'s `%parse_column`, and a `character(len=:), allocatable` copy
    !! there would be one allocation per element -- the shape `check_no_per_element_string_alloc`
    !! exists to keep out of this library. A `text(lo:hi)` section costs nothing.
    !!
    !! `hi < lo` means the text was empty or all blanks, which every specific refuses.
    pure subroutine text_span(text, lo, hi)
        character(len=*), intent(in) :: text !! the text to measure.
        integer, intent(out) :: lo !! index of the first non-blank character.
        integer, intent(out) :: hi !! index of the last non-blank character; `< lo` when there is none.

        hi = len_trim(text)
        lo = 1
        do while (lo <= hi)
            if (text(lo:lo) /= " ") exit
            lo = lo + 1
        end do
    end subroutine text_span

    !> `.true.` when every character of `s` is an ASCII digit, and there is at least one.
    pure logical function all_digits(s) result(res)
        character(len=*), intent(in) :: s !! the span to test; may be zero length.
        integer :: k

        res = len(s) > 0
        do k = 1, len(s)
            if (s(k:k) < "0" .or. s(k:k) > "9") then
                res = .false.
                return
            end if
        end do
    end function all_digits

    !> `.true.` when `s` is an optional sign followed by one or more digits and nothing else.
    !!
    !! This is the check that makes the parser strict: `read` accepts `"5 6"`, and this does not.
    pure logical function integer_shape_ok(s) result(res)
        character(len=*), intent(in) :: s !! the trimmed span.
        integer :: first

        first = 1
        if (len(s) >= 1) then
            if (s(1:1) == "+" .or. s(1:1) == "-") first = 2
        end if
        res = len(s) >= first
        if (res) res = all_digits(s(first:))
    end function integer_shape_ok

    !> `.true.` when `s` is a Fortran real literal with no blank, no kind suffix and no exponent
    !! letter other than `e`/`E`/`d`/`D`.
    !!
    !! The grammar, and it is the whole specification:
    !! `[+|-] ( digits [ "." [digits] ] | "." digits ) [ (e|E|d|D) [+|-] digits ]`.
    !! An integer with no point and no exponent reads as a real, which is what a column of counts
    !! written as text needs.
    pure logical function real_shape_ok(s) result(res)
        character(len=*), intent(in) :: s !! the trimmed span.
        integer :: p, dot, expo, n

        res = .false.
        n = len(s)
        p = 1
        if (p <= n) then
            if (s(p:p) == "+" .or. s(p:p) == "-") p = p + 1
        end if
        ! The exponent letter, if any: it ends the significand and starts the exponent.
        expo = 0
        do dot = p, n
            if (index("eEdD", s(dot:dot)) > 0) then
                expo = dot
                exit
            end if
        end do
        if (expo == 0) then
            if (.not. significand_ok(s(p:n))) return
        else
            if (.not. significand_ok(s(p:expo - 1))) return
            dot = expo + 1
            if (dot <= n) then
                if (s(dot:dot) == "+" .or. s(dot:dot) == "-") dot = dot + 1
            end if
            if (.not. all_digits(s(min(dot, n + 1):n))) return
        end if
        res = .true.
    end function real_shape_ok

    !> `.true.` when `s` is `digits`, `digits.`, `digits.digits` or `.digits` -- the significand of
    !! a real literal, with its sign and exponent already removed.
    !!
    !! **The head and the tail are each tested only when they are NON-EMPTY, and the tail is taken
    !! with an explicit upper bound.** That shape is load-bearing under nagfor and must not be
    !! tidied back into the obvious `if (dot == 1) … else if (dot == len(s)) … else …` chain.
    !!
    !! nagfor 7.2 gets `LEN` of a substring whose lower bound is an EXPRESSION wrong -- for a
    !! `character(len=4)` and `d = 2`, `len(s(d+1:))` answers 4 where 2 is correct and
    !! `len(s(d+3:))` answers 6 where 0 is -- and at `-O2` and above the wrong length becomes a
    !! range assumption the optimiser folds a later comparison against. The previous version formed
    !! `s(dot + 1:)`, which is empty for `"12."`, and then asked `dot == len(s)`; nagfor answered
    !! `.false.` to that comparison while printing both operands as 2, took the `else` arm, and
    !! refused every trailing-point literal. `"0."` is exactly what nagfor's own `(g0)` renders
    !! `0.0_real64` as, so a `%format_column` / `%parse_column` round trip refused its own output.
    !!
    !! Silent in the worst way, too: under `invalid="null"` a perfectly good row would have been
    !! marked missing rather than aborting. gfortran, and nagfor at `-O0`, are unaffected -- which
    !! is why only `fpm test --profile release` under nagfor could see it.
    pure logical function significand_ok(s) result(res)
        character(len=*), intent(in) :: s !! the significand span.
        integer :: dot, n

        res = .false.
        n = len(s)
        dot = index(s, ".")
        if (dot == 0) then
            res = all_digits(s)
            return
        end if
        if (dot > 1) then
            if (.not. all_digits(s(1:dot - 1))) return   ! "12." and "1.5"
        end if
        if (dot < n) then
            if (index(s(dot + 1:n), ".") > 0) return     ! a second point: "1.2.3"
            if (.not. all_digits(s(dot + 1:n))) return   ! ".5" and "1.5"
        end if
        res = dot > 1 .or. dot < n                       ! "." alone is not a significand
    end function significand_ok

    !> Reads one `integer(int32)`. See the `pf_from_str` interface for the full contract.
    pure subroutine pf_from_str_i32(text, value, ok)
        character(len=*), intent(in) :: text !! the text to read.
        integer(int32), intent(out) :: value !! the value read; NOT assigned when `ok` is `.false.`.
        logical, intent(out) :: ok !! `.true.` when the text was read; `.false.` leaves `value` unset.
        integer :: lo, hi, ios

        call text_span(text, lo, hi)
        ok = hi >= lo
        if (ok) ok = integer_shape_ok(text(lo:hi))
        if (.not. ok) return
        ! The shape is sound, so the only failure left is a value too large for the kind -- which
        ! every compiler in this project's fleet reports through `iostat` rather than wrapping.
        read (text(lo:hi), *, iostat=ios) value
        ok = ios == 0
    end subroutine pf_from_str_i32

    !> Reads one `integer(int64)`. See the `pf_from_str` interface for the full contract.
    pure subroutine pf_from_str_i64(text, value, ok)
        character(len=*), intent(in) :: text !! the text to read.
        integer(int64), intent(out) :: value !! the value read; NOT assigned when `ok` is `.false.`.
        logical, intent(out) :: ok !! `.true.` when the text was read; `.false.` leaves `value` unset.
        integer :: lo, hi, ios

        call text_span(text, lo, hi)
        ok = hi >= lo
        if (ok) ok = integer_shape_ok(text(lo:hi))
        if (.not. ok) return
        read (text(lo:hi), *, iostat=ios) value
        ok = ios == 0
    end subroutine pf_from_str_i64

    !> Reads one `real(real32)`. See the `pf_from_str` interface for the full contract.
    !!
    !! **The overflow test is needed on top of `iostat`, because the compilers disagree about
    !! which of the two reports it.** `"1e300"` into a `real32` gives `iostat = 215` and an
    !! untouched variable under nagfor 7.2, and `iostat = 0` with `Infinity` under gfortran 15.2 --
    !! both measured. Taking either mechanism alone would make this procedure answer differently
    !! on the two compilers; taking both makes the CONTRACT compiler-independent even though the
    !! mechanism is not. The same split is what `rendered_ok` above exists for, one direction over.
    !!
    !! An ordering comparison is safe here where it would not be on caller data: `real_shape_ok`
    !! admits only a decimal literal, and no decimal literal reads back as a NaN -- so `value` is
    !! finite or infinite and never the NaN that would raise `IEEE_INVALID` on `>`. Widening the
    !! shape to accept `"nan"` would break that, which is one reason it does not.
    pure subroutine pf_from_str_r32(text, value, ok)
        character(len=*), intent(in) :: text !! the text to read.
        real(real32), intent(out) :: value !! the value read; NOT assigned when `ok` is `.false.`.
        logical, intent(out) :: ok !! `.true.` when the text was read; `.false.` leaves `value` unset.
        integer :: lo, hi, ios

        call text_span(text, lo, hi)
        ok = hi >= lo
        if (ok) ok = real_shape_ok(text(lo:hi))
        if (.not. ok) return
        read (text(lo:hi), *, iostat=ios) value
        ok = ios == 0
        if (ok) ok = .not. (value > huge(value) .or. value < -huge(value))
    end subroutine pf_from_str_r32

    !> Reads one `real(real64)`. See `pf_from_str_r32` for why the overflow test is there as well
    !! as the `iostat`, and the `pf_from_str` interface for the full contract.
    pure subroutine pf_from_str_r64(text, value, ok)
        character(len=*), intent(in) :: text !! the text to read.
        real(real64), intent(out) :: value !! the value read; NOT assigned when `ok` is `.false.`.
        logical, intent(out) :: ok !! `.true.` when the text was read; `.false.` leaves `value` unset.
        integer :: lo, hi, ios

        call text_span(text, lo, hi)
        ok = hi >= lo
        if (ok) ok = real_shape_ok(text(lo:hi))
        if (.not. ok) return
        read (text(lo:hi), *, iostat=ios) value
        ok = ios == 0
        if (ok) ok = .not. (value > huge(value) .or. value < -huge(value))
    end subroutine pf_from_str_r64

    !> Reads one `logical`. See the `pf_from_str` interface for the accepted spellings.
    !!
    !! No `read` at all: the six accepted tokens are matched directly, which is both faster and
    !! stricter than list-directed input, whose `logical` form accepts `.TRUE.`, a bare `T`
    !! followed by arbitrary trailing text, and more besides.
    pure subroutine pf_from_str_log(text, value, ok)
        character(len=*), intent(in) :: text !! the text to read.
        logical, intent(out) :: value !! the value read; NOT assigned when `ok` is `.false.`.
        logical, intent(out) :: ok !! `.true.` when the text was read; `.false.` leaves `value` unset.
        integer :: lo, hi
        character(len=5) :: tok

        call text_span(text, lo, hi)
        ok = hi >= lo
        if (ok) ok = hi - lo + 1 <= len(tok)
        if (.not. ok) return
        tok = text(lo:hi)
        call pf_to_lower(tok)
        select case (trim(tok))
        case ("true", "t", "1")
            value = .true.
        case ("false", "f", "0")
            value = .false.
        case default
            ok = .false.
        end select
    end subroutine pf_from_str_log

    ! ================================================================================
    ! Path joining
    ! ================================================================================

    !> `.true.` when `p` ends with the path separator. **Total on a zero-length argument**, which
    !! is the whole reason it exists: the obvious `len(p) > 0 .and. p(len(p):len(p)) == "/"` is not
    !! safe, because Fortran does not guarantee short-circuit evaluation and the substring is then
    !! `p(0:0)`. That is the defect this module's ancestor carried.
    pure function ends_with_sep(p) result(res)
        character(len=*), intent(in) :: p !! the text to test.
        logical :: res !! `.true.` only when `p` is non-empty and its last character is `/`.

        res = .false.
        if (len(p) > 0) res = (p(len(p):len(p)) == PATH_SEP)
    end function ends_with_sep

    !> `.true.` when `p` begins with the path separator, i.e. when it is an absolute path. Total on
    !! a zero-length argument, for the reason given on `ends_with_sep`.
    pure function starts_with_sep(p) result(res)
        character(len=*), intent(in) :: p !! the text to test.
        logical :: res !! `.true.` only when `p` is non-empty and its first character is `/`.

        res = .false.
        if (len(p) > 0) res = (p(1:1) == PATH_SEP)
    end function starts_with_sep

    !> Joins two already-trimmed components, applying the three `pf_join_path` rules once.
    !!
    !! This is the whole algorithm; every scalar arity folds over it, and the array form re-derives
    !! the same walk in two passes so it can size its result before filling it.
    pure subroutine join_pair(a, b, out)
        character(len=*), intent(in) :: a !! the accumulated path so far, trailing blanks already gone.
        character(len=*), intent(in) :: b !! the component to append, trailing blanks already gone.
        character(len=:), allocatable, intent(out) :: out !! the joined path; always allocated.

        if (starts_with_sep(b)) then
            out = b
        else if (len(a) == 0 .or. ends_with_sep(a)) then
            out = a//b
        else
            out = a//PATH_SEP//b
        end if
    end subroutine join_pair

    !> Joins two path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_2(path1, path2, path)
        character(len=*), intent(in) :: path1 !! first component; trailing blanks trimmed, leading blanks kept.
        character(len=*), intent(in) :: path2 !! second component; an absolute one restarts the path.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.

        call join_pair(path1(1:len_trim(path1)), path2(1:len_trim(path2)), path)
    end subroutine pf_join_path_2

    !> Joins three path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_3(path1, path2, path3, path)
        character(len=*), intent(in) :: path1 !! first component.
        character(len=*), intent(in) :: path2 !! second component.
        character(len=*), intent(in) :: path3 !! third component.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.
        character(len=:), allocatable :: acc

        call pf_join_path_2(path1, path2, acc)
        call join_pair(acc, path3(1:len_trim(path3)), path)
    end subroutine pf_join_path_3

    !> Joins four path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_4(path1, path2, path3, path4, path)
        character(len=*), intent(in) :: path1 !! first component.
        character(len=*), intent(in) :: path2 !! second component.
        character(len=*), intent(in) :: path3 !! third component.
        character(len=*), intent(in) :: path4 !! fourth component.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.
        character(len=:), allocatable :: acc

        call pf_join_path_3(path1, path2, path3, acc)
        call join_pair(acc, path4(1:len_trim(path4)), path)
    end subroutine pf_join_path_4

    !> Joins five path components. See the `pf_join_path` interface for the rules.
    pure subroutine pf_join_path_5(path1, path2, path3, path4, path5, path)
        character(len=*), intent(in) :: path1 !! first component.
        character(len=*), intent(in) :: path2 !! second component.
        character(len=*), intent(in) :: path3 !! third component.
        character(len=*), intent(in) :: path4 !! fourth component.
        character(len=*), intent(in) :: path5 !! fifth component.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated.
        character(len=:), allocatable :: acc

        call pf_join_path_4(path1, path2, path3, path4, acc)
        call join_pair(acc, path5(1:len_trim(path5)), path)
    end subroutine pf_join_path_5

    !> Joins every element of `parts` in order. See the `pf_join_path` interface for the rules and
    !! for the two zero- and one-element cases Python cannot be asked about.
    !!
    !! Sized in one pass and filled in a second, rather than reallocating per component: the
    !! obvious accumulate-as-you-go loop copies the whole prefix each time, which is fine at five
    !! components and quadratic at a thousand.
    pure subroutine pf_join_path_many(parts, path)
        character(len=*), intent(in) :: parts(:) !! the components, in order; each is trailing-trimmed.
        character(len=:), allocatable, intent(out) :: path !! the joined path; always allocated, zero-length for a zero-size array.
        integer :: k, n, ln, total, pos, first
        logical :: is_empty, ends_sep

        n = size(parts)
        if (n == 0) then
            path = ""
            return
        end if

        ! An absolute component discards every component before it (posixpath.join's first rule),
        ! so both walks start at the LAST absolute one instead of resetting when they reach it.
        ! Pass 1 could reset in place; pass 2 could not. The buffer is sized for what SURVIVES, so
        ! writing a prefix that is about to be discarded stores past the end of it -- and the
        ! answer still comes out right, because those bytes are then overwritten. Only a
        ! bounds-checking build reports it (--profile debug and --profile nagdeb both do; a plain
        ! `fpm test`, and CI, do not). Starting at `first` removes the reset from both loops, which
        ! is why neither has an "absolute" case at all. See feature_risks.md Risk-3.
        first = 1
        do k = n, 1, -1
            ln = len_trim(parts(k))
            if (ln > 0) then
                if (parts(k) (1:1) == PATH_SEP) then
                    first = k
                    exit
                end if
            end if
        end do

        ! Pass 1 -- the final length, by walking the fold without building anything.
        total = 0
        is_empty = .true.
        ends_sep = .false.
        do k = first, n
            ln = len_trim(parts(k))
            if (is_empty .or. ends_sep) then
                total = total + ln
                if (ln > 0) then
                    is_empty = .false.
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                end if
            else
                total = total + 1 + ln
                if (ln > 0) then
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                else
                    ends_sep = .true.
                end if
            end if
        end do

        allocate (character(len=total) :: path)

        ! Pass 2 -- the identical walk, writing. `pos` therefore tracks `total` exactly, and the
        ! final `pos` equals `total` on every input.
        pos = 0
        is_empty = .true.
        ends_sep = .false.
        do k = first, n
            ln = len_trim(parts(k))
            if (is_empty .or. ends_sep) then
                if (ln > 0) then
                    path(pos + 1:pos + ln) = parts(k) (1:ln)
                    pos = pos + ln
                    is_empty = .false.
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                end if
            else
                path(pos + 1:pos + 1) = PATH_SEP
                pos = pos + 1
                if (ln > 0) then
                    path(pos + 1:pos + ln) = parts(k) (1:ln)
                    pos = pos + ln
                    ends_sep = (parts(k) (ln:ln) == PATH_SEP)
                else
                    ends_sep = .true.
                end if
            end if
        end do
    end subroutine pf_join_path_many

    ! ================================================================================
    ! Taking a path apart
    ! ================================================================================

    !> Locates the two positions every path procedure derives its answer from: the last separator,
    !! and the extension dot if there is one.
    !!
    !! One scanner for five entry points, so the `.bashrc` and `/a.b/c` rules live in one place and
    !! no single-piece query allocates anything it was not asked for.
    !!
    !! `dot_pos` implements `posixpath.splitext`, which skips **all** leading dots of the basename:
    !! an extension exists only when there is a non-dot character between the last separator and
    !! the dot. That is why `.bashrc` has none and `a.tar.gz` has `.gz`.
    pure subroutine scan_path(p, sep_pos, dot_pos)
        character(len=*), intent(in) :: p !! the path, with trailing blanks already removed.
        integer, intent(out) :: sep_pos !! index of the last `/`, or 0 when there is none.
        integer, intent(out) :: dot_pos !! index of the extension dot, or 0 when there is no extension.
        integer :: i, n

        n = len(p)

        sep_pos = 0
        do i = n, 1, -1
            if (p(i:i) == PATH_SEP) then
                sep_pos = i
                exit
            end if
        end do

        dot_pos = 0
        do i = n, sep_pos + 1, -1
            if (p(i:i) == EXT_SEP) then
                dot_pos = i
                exit
            end if
        end do
        if (dot_pos == 0) return

        do i = sep_pos + 1, dot_pos - 1
            if (p(i:i) /= EXT_SEP) return
        end do
        dot_pos = 0
    end subroutine scan_path

    !> Builds the directory part from a scanned path, following `posixpath.dirname`.
    !!
    !! Not simply `p(1:sep_pos-1)`: Python keeps a head that is entirely separators (`"/x"` gives
    !! `"/"`, not `""`) and otherwise strips every trailing separator (`"a//b"` gives `"a"`).
    pure subroutine dirname_from_scan(p, sep_pos, dir)
        character(len=*), intent(in) :: p !! the path, with trailing blanks already removed.
        integer, intent(in) :: sep_pos !! index of the last `/`, from `scan_path`.
        character(len=:), allocatable, intent(out) :: dir !! the directory part; always allocated.
        integer :: i, j
        logical :: all_sep

        if (sep_pos == 0) then
            dir = ""
            return
        end if

        all_sep = .true.
        do i = 1, sep_pos
            if (p(i:i) /= PATH_SEP) then
                all_sep = .false.
                exit
            end if
        end do
        if (all_sep) then
            dir = p(1:sep_pos)
            return
        end if

        j = sep_pos
        do while (j > 0)
            if (p(j:j) /= PATH_SEP) exit
            j = j - 1
        end do
        dir = p(1:j)
    end subroutine dirname_from_scan

    !> Everything before the last separator, following `posixpath.dirname`.
    !!
    !! `"/data/run3/cat.parquet"` gives `"/data/run3"`, `"myfile.txt"` gives `""`, `"/x"` gives
    !! `"/"` and `"/a/b/"` gives `"/a/b"`. Purely lexical: nothing is normalised and the filesystem
    !! is never consulted.
    pure subroutine pf_dirname(path, dir)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: dir !! the directory part; always allocated, zero-length when there is none.
        integer :: n, sep_pos, dot_pos

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        call dirname_from_scan(path(1:n), sep_pos, dir)
    end subroutine pf_dirname

    !> Everything after the last separator, following `posixpath.basename`.
    !!
    !! `"/data/run3/cat.parquet"` gives `"cat.parquet"` and `"/a/b/"` gives `""` -- a trailing
    !! separator means an empty basename, and is not read as "the caller meant `b`".
    pure subroutine pf_basename(path, base)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: base !! the final component; always allocated, zero-length when there is none.
        integer :: n, sep_pos, dot_pos

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        base = path(sep_pos + 1:n)
    end subroutine pf_basename

    !> The extension, **including its leading dot**, following `posixpath.splitext(path)[1]`.
    !!
    !! `"cat.parquet"` gives `".parquet"`, `"a.tar.gz"` gives `".gz"` (the last dot only), and both
    !! `".bashrc"` and `"/a.b/c"` give `""` -- a leading dot marks a hidden file rather than a
    !! suffix, and a dot in a *directory* component is not an extension.
    !!
    !! The dot is included so that `stem // "_stat" // ext` rebuilds a name correctly, and so that
    !! the empty case composes without leaving a stray dot behind.
    pure subroutine pf_path_ext(path, ext)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: ext !! the extension with its dot; zero-length when there is none.
        integer :: n, sep_pos, dot_pos

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        if (dot_pos == 0) then
            ext = ""
        else
            ext = path(dot_pos:n)
        end if
    end subroutine pf_path_ext

    !> The basename with its extension removed, i.e.
    !! `posixpath.splitext(posixpath.basename(path))[0]`.
    !!
    !! `"/data/cat.parquet"` gives `"cat"`, `"a.tar.gz"` gives `"a.tar"`, `".bashrc"` gives
    !! `".bashrc"` and `"/a/b/"` gives `""`.
    !!
    !! **This is not `pathlib.Path.stem`**, which normalises a trailing separator before splitting
    !! and does not read a trailing dot as an extension -- the two disagree on `"/a/b/"` and on
    !! `"a."`. The lexical rule above is the one implemented, so `pathlib` is not a second opinion
    !! to check against.
    pure subroutine pf_path_stem(path, stem)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: stem !! the basename without its extension; always allocated.
        integer :: n, sep_pos, dot_pos, last

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        last = n
        if (dot_pos > 0) last = dot_pos - 1
        stem = path(sep_pos + 1:last)
    end subroutine pf_path_stem

    !> Splits a path into its directory, stem and extension in one call and one scan.
    !!
    !! ```fortran
    !! call pf_split_path(infile, dir, stem, ext)
    !! call pf_join_path(dir, stem // "_stat" // ext, outfile)
    !! ```
    !!
    !! Each piece is exactly what `pf_dirname`, `pf_path_stem` and `pf_path_ext` return
    !! individually; this form costs one scan instead of three and is the escape hatch for a caller
    !! who needs to recombine the pieces with text this module would otherwise have trimmed.
    pure subroutine pf_split_path(path, dir, stem, ext)
        character(len=*), intent(in) :: path !! the path; trailing blanks trimmed, leading blanks kept.
        character(len=:), allocatable, intent(out) :: dir !! the directory part; always allocated.
        character(len=:), allocatable, intent(out) :: stem !! the basename without its extension; always allocated.
        character(len=:), allocatable, intent(out) :: ext !! the extension with its dot; always allocated.
        integer :: n, sep_pos, dot_pos, last

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        call dirname_from_scan(path(1:n), sep_pos, dir)
        last = n
        if (dot_pos > 0) then
            last = dot_pos - 1
            ext = path(dot_pos:n)
        else
            ext = ""
        end if
        stem = path(sep_pos + 1:last)
    end subroutine pf_split_path

    !> Inserts `suffix` immediately before the extension, keeping the directory and the extension.
    !!
    !! `pf_path_add_suffix("/data/myfile.txt", "_stat", out)` gives `"/data/myfile_stat.txt"`. It
    !! composes on every shape the table in the guide covers: a name with no extension gives
    !! `"myfile_stat"`, `"a.tar.gz"` gives `"a.tar_stat.gz"`, and `".bashrc"` gives
    !! `".bashrc_stat"` because a leading dot is not an extension.
    !!
    !! **A path ending in a separator has an empty stem, so `"/a/b/"` gives `"/a/b/_stat"`.** That
    !! is the lexically consistent answer and the one the four single-piece procedures imply;
    !! reading the trailing separator as "the caller meant `b`" would be exactly the normalisation
    !! this module refuses to do everywhere else.
    !!
    !! **`suffix` is trimmed like every other text argument.** A `character(len=16)` variable
    !! holding `"_stat"` is blank-padded, and copying it verbatim would produce
    !! `myfile_stat     .txt` from a call site that looks correct. A caller who genuinely needs a
    !! trailing blank uses `pf_split_path` and ordinary concatenation, which preserves the padding
    !! of a fixed-length variable exactly.
    pure subroutine pf_path_add_suffix(path, suffix, out)
        character(len=*), intent(in) :: path !! the path to rename, e.g. `"/data/myfile.txt"`.
        character(len=*), intent(in) :: suffix !! the text to insert before the extension, e.g. `"_stat"`; trailing blanks trimmed.
        character(len=:), allocatable, intent(out) :: out !! the rebuilt path; always allocated.
        integer :: n, sep_pos, dot_pos, last

        n = len_trim(path)
        call scan_path(path(1:n), sep_pos, dot_pos)
        last = n
        if (dot_pos > 0) last = dot_pos - 1
        ! The literal prefix up to and including the last separator is kept rather than rebuilt
        ! through pf_dirname + pf_join_path, so an interior "a//b.txt" is not silently collapsed.
        out = path(1:sep_pos)//path(sep_pos + 1:last)//suffix(1:len_trim(suffix))//path(last + 1:n)
    end subroutine pf_path_add_suffix

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module parquet_utils ! GCOVR_EXCL_LINE
