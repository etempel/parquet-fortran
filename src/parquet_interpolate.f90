!> Interpolation of tabulated `real64` data: `pf_interp_1d`, an interpolant built once over a table
!> of abscissae and ordinates and evaluated anywhere, and `pf_interp`, the one-shot form.
!!
!! `parquet_interpolate` is an **Arrow-free leaf**: it imports the two INTRINSIC modules
!! `iso_fortran_env` and `ieee_arithmetic`, and no module of this library. An intrinsic module is
!! not a compiled file, not a tier edge and not a footprint entry, so `use parquet_interpolate`
!! compiles three Fortran files and never crosses the C++ boundary.
!! `check_parquet_interpolate_stays_arrow_free` (tools/check_source_conventions.py) keeps that true.
!!
!! **Nothing in this module prints.** It reads no setting, emits no INFO, NOTE or WARNING, and
!! re-exports no knob. The only output path is `error stop`, and every one of those is a caller
!! contract that was broken: a table `%init` cannot build an interpolant from, a token it does not
!! know, or an evaluation of an object that was never built. The guide page lists every message.
!!
!! **Sharing is by construction.** The module has no variable that is not a `parameter`. `%init`
!! and `%clear` are the only bindings that write an object; `%eval` is `pure elemental` and only
!! reads it, so one object nobody writes may be evaluated from any number of threads at once, and
!! two objects never share anything. The aborts of `%init` are each taken under a named
!! `critical`, so a caller building objects inside a parallel region aborts once.
!!
!! **A NaN query answers NaN, quietly, under every policy.** Each evaluator screens it with
!! `xq /= xq` before any other comparison: the clamp a `min`/`max` pair would compile to raises
!! IEEE_INVALID on a NaN, which aborts a program running under nagfor's default `-ieee=stop`.
!!
!! **Outside the table, the object's `outside=` policy decides**, fixed when it is built:
!! `"clamp"` (the default) answers the value at the nearest end, `"extrapolate"` continues the end
!! segment's polynomial, and `"nan"` answers a quiet NaN.
!!
!! A table may be given ascending or descending; it is stored ascending, which is the only order the
!! workers see. The `"pchip"` method and the `"not_a_knot"` and `"clamped"` end conditions are
!! recognised and refused with a message saying they are not available yet.
module parquet_interpolate

    use iso_fortran_env, only : real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_finite, ieee_is_nan

    implicit none
    private

    public :: pf_interp_1d, pf_interp
    public :: parquet_debug_interp_force_search

    ! ---- private codes: what an object stores for each token --------------------------------

    integer, parameter :: M_LINEAR = 1       !! `method="linear"`
    integer, parameter :: M_CUBIC = 2        !! `method="cubic"`
    integer, parameter :: M_PCHIP = 3        !! `method="pchip"`, not available yet
    integer, parameter :: O_CLAMP = 1        !! `outside="clamp"`
    integer, parameter :: O_EXTRAPOLATE = 2  !! `outside="extrapolate"`
    integer, parameter :: O_NAN = 3          !! `outside="nan"`
    integer, parameter :: B_NATURAL = 1      !! `bc="natural"`
    integer, parameter :: B_NOT_A_KNOT = 2   !! `bc="not_a_knot"`, not available yet
    integer, parameter :: B_CLAMPED = 3      !! `bc="clamped"`, not available yet

    ! ---- internal parameters, not part of the public surface -------------------------------

    !> Characters of caller-supplied `context` an abort message reproduces before eliding with
    !! `...` (`api-conventions.md`, errors and diagnostics).
    integer, parameter :: CONTEXT_CAP = 100
    !> Characters of a caller's unknown token an abort message reproduces before eliding with `...`.
    integer, parameter :: TOKEN_CAP = 100
    !> How far from even a table may be and still be bracketed by arithmetic, as a fraction of one
    !! step spread over the whole table: every spacing must lie within `UNIFORM_TOL_FRACTION*h/n` of
    !! the first spacing `h`, which keeps the accumulated drift of an accepted table below a fifth of
    !! a step. It decides only how FAST a bracket is found, never which one: the arithmetic guess is
    !! corrected against the knots before it is used (`interp_bracket`), so an object answers the same
    !! bits on either path. `parquet_debug_interp_force_search` makes both paths testable.
    real(real64), parameter :: UNIFORM_TOL_FRACTION = 0.1_real64

    ! ---- the one-dimensional interpolant -----------------------------------------------------

    !> A one-dimensional interpolant over tabulated data: build it once with `%init`, evaluate it
    !! anywhere with `%eval`.
    !!
    !! Every component is private. An object holds its own copy of the table, stored ascending, and
    !! the per-knot coefficients its method needs. Intrinsic assignment copies it deeply; there is no
    !! finalizer, since every allocatable component frees itself when the object goes out of scope.
    type :: pf_interp_1d
        private
        real(real64), allocatable :: x(:)   !! abscissae, stored strictly increasing
        real(real64), allocatable :: y(:)   !! ordinates, in the same order
        real(real64), allocatable :: d(:)   !! per-knot coefficients: second derivatives for a cubic
                                            !! spline; unallocated for `linear`
        integer      :: n = 0               !! points in the table; 0 until `%init`. A default integer:
                                            !! a table longer than `huge(1)` points is not an input
                                            !! this module is for
        integer      :: method = 0          !! `M_LINEAR` or `M_CUBIC`
        integer      :: outside = 0         !! `O_CLAMP`, `O_EXTRAPOLATE` or `O_NAN`
        logical      :: reversed = .false.  !! the caller's table was descending
        logical      :: uniform = .false.   !! evenly spaced: the bracket is guessed by arithmetic
        real(real64) :: step = 0.0_real64   !! mean knot spacing, `(x(n) - x(1))/(n - 1)`
    contains
        procedure :: init => interp_1d_init          !! Stores the table and builds the coefficients.
        procedure :: eval => interp_1d_eval          !! Value at a point; `pure elemental`.
        procedure :: is_initialised => interp_1d_ready !! `.true.` once built; never aborts.
        procedure :: clear => interp_1d_clear        !! Releases the table; the object may be rebuilt.
    end type pf_interp_1d

    ! ---- the bindings, implemented in parquet_interpolate_1d.f90 ----------------------------

    interface

        !> Builds the interpolant over the table `(x, y)`.
        !!
        !! ```
        !! call c%init(x, y, [method], [bc], [slopes], [outside], [is_valid], [context])
        !! ```
        !!
        !! `x` must be strictly increasing or strictly decreasing and finite, `y` finite and the same
        !! size; `is_valid`, when given, drops the points it marks `.false.` first, and the survivors
        !! must still number at least two. `method` is `"linear"` or `"cubic"` (the default: a C2
        !! cubic spline), `bc` the spline's end condition, `"natural"` (the default), and `outside`
        !! what a point beyond the table answers: `"clamp"` (the default), `"extrapolate"` or `"nan"`.
        !! Tokens are matched without regard to case. `slopes` belongs to the `"clamped"` end
        !! condition, which is not available yet, and is refused. `context` is appended to any abort
        !! message. A second `%init` on a built object replaces its table.
        module subroutine interp_1d_init(this, x, y, method, bc, slopes, outside, is_valid, context)
            implicit none
            class(pf_interp_1d), intent(out)       :: this       !! the interpolant to build
            real(real64), intent(in)               :: x(:)       !! abscissae, strictly monotonic
            real(real64), intent(in)               :: y(:)       !! ordinates, one per abscissa
            character(len=*), intent(in), optional :: method     !! `"linear"` or `"cubic"`
            character(len=*), intent(in), optional :: bc         !! the spline's end condition
            real(real64), intent(in), optional     :: slopes(:)  !! end slopes for `bc="clamped"`
            character(len=*), intent(in), optional :: outside    !! `"clamp"`, `"extrapolate"` or `"nan"`
            logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
            character(len=*), intent(in), optional :: context    !! call-site text for abort messages
        end subroutine interp_1d_init

        !> The interpolant's value at `xq`: `pure elemental`, so an array of queries answers an array.
        !!
        !! A NaN query answers NaN. A query outside the table answers what the object's `outside=`
        !! policy says; a query on a knot answers that knot's ordinate exactly. Aborts on an object
        !! that was never built.
        pure elemental module function interp_1d_eval(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this !! the interpolant
            real(real64), intent(in)        :: xq   !! the query point
            real(real64)                    :: v    !! the interpolated value
        end function interp_1d_eval

        !> `.true.` once `%init` has built the object and `%clear` has not released it since.
        pure module function interp_1d_ready(this) result(ok)
            implicit none
            class(pf_interp_1d), intent(in) :: this !! the interpolant
            logical                         :: ok   !! the object can be evaluated
        end function interp_1d_ready

        !> Releases the table and returns the object to its unbuilt state; harmless on a fresh one.
        pure module subroutine interp_1d_clear(this)
            implicit none
            class(pf_interp_1d), intent(inout) :: this !! the interpolant to release
        end subroutine interp_1d_clear

    end interface

    ! ---- the one-shot generic, implemented in parquet_interpolate_1d.f90 ---------------------

    !> Interpolates the table `(x, y)` at `xq` in one call, building and discarding an interpolant.
    !!
    !! ```
    !! yq = pf_interp(x, y, xq, [method], [bc], [slopes], [outside], [is_valid], [context])
    !! ```
    !!
    !! Optional arguments are shown in square brackets, with the comma outside the bracket. FORD
    !! renders no per-argument documentation for a generic with several specifics, so every argument
    !! is named here instead:
    !!
    !! * `x`, `y` -- the table, rank-1 `real64` of one size: `x` strictly increasing or strictly
    !!   decreasing and finite, `y` finite.
    !! * `xq` -- the query: a scalar, answered by a scalar, or a rank-1 array, answered by an array of
    !!   its size.
    !! * `method` -- `"linear"`, or `"cubic"` (the default), a C2 cubic spline.
    !! * `bc` -- the cubic spline's end condition, `"natural"` (the default). Refused with `"linear"`.
    !! * `slopes` -- end slopes for the `"clamped"` end condition, which is not available yet; refused.
    !! * `outside` -- what a query beyond the table answers: `"clamp"` (the default, the end value),
    !!   `"extrapolate"` (the end segment continued) or `"nan"`.
    !! * `is_valid` -- rank-1 `logical` of the table's size; a point marked `.false.` is dropped.
    !! * `context` -- text appended to any abort message, capped at 100 characters.
    !!
    !! The answer is bit for bit what `%init` with the same arguments followed by `%eval` gives,
    !! because that is how it is computed; its abort messages begin `pf_interp: `. Building the
    !! interpolant is the expensive half, so a table queried more than a handful of times wants a
    !! `pf_interp_1d` object instead.
    interface pf_interp

        !> One query point.
        module function interp_1d_oneshot_scalar(x, y, xq, method, bc, slopes, outside, is_valid, &
                                                 context) result(yq)
            implicit none
            real(real64), intent(in)               :: x(:)        !! abscissae, strictly monotonic
            real(real64), intent(in)               :: y(:)        !! ordinates, one per abscissa
            real(real64), intent(in)               :: xq          !! the query point
            character(len=*), intent(in), optional :: method      !! `"linear"` or `"cubic"`
            character(len=*), intent(in), optional :: bc          !! the spline's end condition
            real(real64), intent(in), optional     :: slopes(:)   !! end slopes for `bc="clamped"`
            character(len=*), intent(in), optional :: outside     !! the out-of-range policy
            logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
            character(len=*), intent(in), optional :: context     !! call-site text
            real(real64)                           :: yq          !! the interpolated value
        end function interp_1d_oneshot_scalar

        !> An array of query points.
        module function interp_1d_oneshot_array(x, y, xq, method, bc, slopes, outside, is_valid, &
                                                context) result(yq)
            implicit none
            real(real64), intent(in)               :: x(:)        !! abscissae, strictly monotonic
            real(real64), intent(in)               :: y(:)        !! ordinates, one per abscissa
            real(real64), intent(in)               :: xq(:)       !! the query points
            character(len=*), intent(in), optional :: method      !! `"linear"` or `"cubic"`
            character(len=*), intent(in), optional :: bc          !! the spline's end condition
            real(real64), intent(in), optional     :: slopes(:)   !! end slopes for `bc="clamped"`
            character(len=*), intent(in), optional :: outside     !! the out-of-range policy
            logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
            character(len=*), intent(in), optional :: context     !! call-site text
            real(real64)                           :: yq(size(xq)) !! the interpolated values
        end function interp_1d_oneshot_array

    end interface pf_interp

    ! ---- the test-only hook, implemented in parquet_interpolate_1d.f90 ------------------------

    !> Test-only. Makes an object find every bracket by bisection, and reports whether it was
    !! finding them by arithmetic until now.
    !!
    !! An evenly spaced table is bracketed by arithmetic and every other one by bisection, and the
    !! two must answer the same bits; this is what lets one test evaluate the same table through both
    !! paths. Public only because the flag it clears is a private component; no library code calls
    !! it, and there is no way to switch the arithmetic path back on short of `%init`.
    interface parquet_debug_interp_force_search

        !> The one-dimensional object's form.
        module subroutine interp_1d_force_search(this, was_uniform)
            implicit none
            type(pf_interp_1d), intent(inout) :: this        !! the interpolant to switch
            logical, intent(out)              :: was_uniform !! it was bracketing by arithmetic
        end subroutine interp_1d_force_search

    end interface parquet_debug_interp_force_search

    ! ---- messages and tokens, implemented in parquet_interpolate_core.f90 --------------------

    interface

        !> Aborts with `<entry>: <text>`, plus the caller's context when one was given.
        !!
        !! The single `error stop` of every entry point that validates. Taken under a named
        !! `critical` so that one thread aborts when a caller builds objects inside a parallel
        !! region: two threads reaching `ERROR STOP` at once leave the exit status nondeterministic
        !! under ifx. Impure deliberately -- a `pure` guard-only procedure's call is deleted by ifx at
        !! `-O0`.
        module subroutine interp_abort(entry, text, context)
            implicit none
            character(len=*), intent(in)           :: entry   !! the entry point, e.g. `pf_interp`
            character(len=*), intent(in)           :: text    !! what went wrong
            character(len=*), intent(in), optional :: context !! caller's call-site text
        end subroutine interp_abort

        !> An integer as text, left-justified in a 24-character buffer the caller trims.
        pure module function interp_i2s(n) result(text)
            implicit none
            integer, intent(in) :: n    !! the integer
            character(len=24)   :: text !! its decimal digits, blank-padded
        end function interp_i2s

        !> Folds a caller's token to lower case for matching against the names this module knows.
        !!
        !! ASCII only. The buffer is one character longer than `TOKEN_CAP`, so a token longer than
        !! that can never trim down to a known name.
        module subroutine interp_fold_token(token, folded)
            implicit none
            character(len=*), intent(in)          :: token  !! the caller's token
            character(len=TOKEN_CAP + 1), intent(out) :: folded !! its lower-case form
        end subroutine interp_fold_token

        !> A caller's token in double quotes for an abort message, capped at `TOKEN_CAP` characters.
        module subroutine interp_quote(token, text)
            implicit none
            character(len=*), intent(in)               :: token !! the caller's token
            character(len=:), allocatable, intent(out) :: text  !! `"token"`, elided when long
        end subroutine interp_quote

    end interface

    ! ---- validation, implemented in parquet_interpolate_core.f90 ------------------------------

    interface

        !> `.true.` if `x` is strictly increasing or strictly decreasing, and which of the two.
        !!
        !! A NaN fails both orderings, and is screened before any comparison is made: an ordered
        !! comparison against a NaN raises IEEE_INVALID. A repeated value fails both as well.
        module function interp_is_monotonic(x, ascending) result(ok)
            implicit none
            real(real64), intent(in) :: x(:)      !! at least two abscissae
            logical, intent(out)     :: ascending !! `.true.` when increasing; meaningful when `ok`
            logical                  :: ok        !! strictly monotonic, with no NaN
        end function interp_is_monotonic

        !> Measures whether an ascending table is evenly spaced enough to bracket by arithmetic.
        !!
        !! Every spacing must lie within `UNIFORM_TOL_FRACTION*h/n` of the first spacing `h`. The
        !! mean spacing is returned either way.
        module subroutine interp_uniform_step(x, uniform, step)
            implicit none
            real(real64), intent(in)  :: x(:)    !! at least two abscissae, strictly increasing
            logical, intent(out)      :: uniform !! evenly spaced to the tolerance
            real(real64), intent(out) :: step    !! `(x(n) - x(1))/(n - 1)`
        end subroutine interp_uniform_step

    end interface

    ! ---- the bracket search, implemented in parquet_interpolate_core.f90 ---------------------

    interface

        !> The segment `k` in `[1, n-1]` with `x(k) <= xq < x(k+1)`, or `n-1` when `xq` is `x(n)`.
        !!
        !! `xq` must lie inside `[x(1), x(n)]`; the evaluators decide what a point outside answers
        !! before they bracket. An evenly spaced table has its bracket GUESSED by arithmetic and the
        !! guess then walked to the segment the inequality names, so the two paths answer the same
        !! segment for every query; any other table is bisected.
        pure module function interp_bracket(x, n, uniform, step, xq) result(k)
            implicit none
            real(real64), intent(in) :: x(:)    !! abscissae, strictly increasing
            integer, intent(in)      :: n       !! points in `x`, at least two
            logical, intent(in)      :: uniform !! guess the bracket by arithmetic
            real(real64), intent(in) :: step    !! the mean spacing, used when `uniform`
            real(real64), intent(in) :: xq      !! the query, inside the table
            integer                  :: k       !! the segment's left knot
        end function interp_bracket

    end interface

    ! ---- the tridiagonal solver and the spline, implemented in parquet_interpolate_core.f90 ---

    interface

        !> Solves a tridiagonal system by the Thomas algorithm, without pivoting.
        !!
        !! Row `i` reads `a(i)*u(i-1) + b(i)*u(i) + c(i)*u(i+1) = r(i)`; `a(1)` and `c(n)` are not
        !! read. Only for a strictly diagonally dominant system, which the spline systems are (the
        !! header of parquet_interpolate_core.f90 says why): such a system has no zero pivot.
        pure module subroutine interp_thomas(a, b, c, r, u)
            implicit none
            real(real64), intent(in)  :: a(:) !! sub-diagonal, `a(i)` in row `i`
            real(real64), intent(in)  :: b(:) !! diagonal
            real(real64), intent(in)  :: c(:) !! super-diagonal, `c(i)` in row `i`
            real(real64), intent(in)  :: r(:) !! right-hand side
            real(real64), intent(out) :: u(:) !! the solution
        end subroutine interp_thomas

        !> The second derivatives `m` of the cubic spline through `(x, y)` under the end condition
        !! `bc`.
        pure module subroutine interp_spline_coeffs(x, y, bc, m)
            implicit none
            real(real64), intent(in)  :: x(:) !! abscissae, strictly increasing, at least two
            real(real64), intent(in)  :: y(:) !! ordinates
            integer, intent(in)       :: bc   !! `B_NATURAL`
            real(real64), intent(out) :: m(:) !! second derivatives at the knots
        end subroutine interp_spline_coeffs

    end interface

    ! ---- the segment polynomials, implemented in parquet_interpolate_core.f90 ----------------

    interface

        !> The straight line through `(x0, y0)` and `(x1, y1)` at `xq`, which may lie beyond them.
        pure module function interp_linear_seg_value(x0, x1, y0, y1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: xq !! the point
            real(real64)             :: v  !! the line's value there
        end function interp_linear_seg_value

        !> One cubic-spline segment, in second-derivative form, at `xq`, which may lie beyond it.
        pure module function interp_cubic_seg_value(x0, x1, y0, y1, m0, m1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: m0 !! the second derivative at `x0`
            real(real64), intent(in) :: m1 !! the second derivative at `x1`
            real(real64), intent(in) :: xq !! the point
            real(real64)             :: v  !! the cubic's value there
        end function interp_cubic_seg_value

    end interface

end module parquet_interpolate
