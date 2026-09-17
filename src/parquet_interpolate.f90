!> Interpolation of tabulated `real64` data: `pf_interp_1d`, an interpolant built once over a table
!> of abscissae and ordinates and evaluated, differentiated and integrated anywhere; `pf_interp_2d`,
!> the same over values tabulated on a rectilinear grid; and `pf_interp`, the one-shot form of both.
!!
!! `parquet_interpolate` is an **Arrow-free leaf**: it imports the two INTRINSIC modules
!! `iso_fortran_env` and `ieee_arithmetic`, and no module of this library. An intrinsic module is
!! not a compiled file, not a tier edge and not a footprint entry, so `use parquet_interpolate`
!! compiles four Fortran files and never crosses the C++ boundary.
!! `check_parquet_interpolate_stays_arrow_free` (tools/check_source_conventions.py) keeps that true.
!!
!! **Nothing in this module prints.** It reads no setting, emits no INFO, NOTE or WARNING, and
!! re-exports no knob. The only output path is `error stop`, and every one of those is a caller
!! contract that was broken: a table `%init` cannot build an interpolant from, a token it does not
!! know, or an evaluation of an object that was never built. The guide page lists every message.
!!
!! **Sharing is by construction.** The module has no variable that is not a `parameter`. `%init`
!! and `%clear` are the only bindings that write an object; `%eval`, `%derivative` and `%integral`
!! are `pure` and only read it, so one object nobody writes may be evaluated from any number of
!! threads at once, and two objects never share anything. The aborts of `%init` are each taken
!! under a named `critical`, so a caller building objects inside a parallel region aborts once.
!!
!! **A NaN query answers NaN, quietly, under every policy.** Each evaluator screens it with
!! `xq /= xq` before any other comparison: the clamp a `min`/`max` pair would compile to raises
!! IEEE_INVALID on a NaN, which aborts a program running under nagfor's default `-ieee=stop`.
!!
!! **Outside the table, the object's `outside=` policy decides**, fixed when it is built:
!! `"clamp"` (the default) answers the value at the nearest end, a derivative of zero and an
!! integral that is flat beyond the table; `"extrapolate"` continues the end segment's polynomial
!! for all three; and `"nan"` answers a quiet NaN for a point beyond the table and for an integral
!! with a limit there. On a grid the policy applies to each coordinate on its own: `"clamp"` moves
!! each coordinate beyond the grid to its nearest grid line, `"extrapolate"` continues the nearest
!! cell's polynomial, and `"nan"` answers a NaN when either coordinate lies beyond the grid.
!!
!! A table may be given ascending or descending, and so may each axis of a grid; each is stored
!! ascending, which is the only order the workers see, with the ordinates, the values along that
!! axis and the end slopes of `bc="clamped"` reversed with it.
module parquet_interpolate

    use iso_fortran_env, only : real64, int64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf

    implicit none
    private

    public :: pf_interp_1d, pf_interp_2d, pf_interp
    public :: parquet_debug_interp_force_search

    ! ---- private codes: what an object stores for each token --------------------------------

    integer, parameter :: M_LINEAR = 1       !! `method="linear"`
    integer, parameter :: M_CUBIC = 2        !! `method="cubic"`
    integer, parameter :: M_PCHIP = 3        !! `method="pchip"`
    integer, parameter :: O_CLAMP = 1        !! `outside="clamp"`
    integer, parameter :: O_EXTRAPOLATE = 2  !! `outside="extrapolate"`
    integer, parameter :: O_NAN = 3          !! `outside="nan"`
    integer, parameter :: B_NATURAL = 1      !! `bc="natural"`
    integer, parameter :: B_NOT_A_KNOT = 2   !! `bc="not_a_knot"`
    integer, parameter :: B_CLAMPED = 3      !! `bc="clamped"`

    ! ---- internal parameters, not part of the public surface -------------------------------

    !> Characters of caller-supplied `context` an abort message reproduces before eliding with
    !! `...` (`api-conventions.md`, errors and diagnostics).
    integer, parameter :: CONTEXT_CAP = 100
    !> The longest token this module reads, in characters, and so the longest a known name can be. A
    !! longer one is refused as unknown before it is folded; an abort message reproduces this many of
    !! its characters before eliding with `...`.
    integer, parameter :: TOKEN_CAP = 100
    !> How far from even a table may be and still be bracketed by arithmetic, as a fraction of its mean
    !! step: every knot must lie within `UNIFORM_TOL_FRACTION` of a step of its place on the straight
    !! line through the table's two ends. A knot's position is tested rather than a spacing, because
    !! the rounding of a table formed as `start + (i - 1)*step` sits on each knot's own magnitude and
    !! does not accumulate, while its spacings can differ from one another by far more than such a
    !! tolerance spread over a long table. It decides only how FAST a bracket is found, never which one:
    !! the arithmetic guess is corrected against the knots before it is used (`interp_bracket`), so an
    !! object answers the same bits on either path. `parquet_debug_interp_force_search` makes both
    !! paths testable.
    real(real64), parameter :: UNIFORM_TOL_FRACTION = 0.1_real64
    !> The widest knot spacing a cubic spline accepts, `2**511`. Its segment formula squares a spacing,
    !! and the square of this one, `2**1022`, is below `huge()`; `linear` and `pchip` square none and
    !! have no such limit.
    real(real64), parameter :: SPLINE_MAX_SPACING = scale(1.0_real64, 511)

    ! ---- the one-dimensional interpolant -----------------------------------------------------

    !> A one-dimensional interpolant over tabulated data: build it once with `%init`, then evaluate,
    !! differentiate and integrate it anywhere.
    !!
    !! Every component is private. An object holds its own copy of the table, stored ascending, and
    !! the per-knot coefficients its method needs. Intrinsic assignment copies it deeply; there is no
    !! finalizer, since every allocatable component frees itself when the object goes out of scope.
    type :: pf_interp_1d
        private
        real(real64), allocatable :: x(:)   !! abscissae, stored strictly increasing
        real(real64), allocatable :: y(:)   !! ordinates, in the same order
        real(real64), allocatable :: d(:)   !! per-knot coefficients: second derivatives for a cubic
                                            !! spline, slopes for `pchip`; unallocated for `linear`
        integer      :: n = 0               !! points in the table; 0 until `%init`. A default integer:
                                            !! a table longer than `huge(1)` points is not an input
                                            !! this module is for
        integer      :: method = 0          !! `M_LINEAR`, `M_CUBIC` or `M_PCHIP`
        integer      :: outside = 0         !! `O_CLAMP`, `O_EXTRAPOLATE` or `O_NAN`
        logical      :: reversed = .false.  !! the caller's table was descending
        logical      :: uniform = .false.   !! evenly spaced: the bracket is guessed by arithmetic
        real(real64) :: step = 0.0_real64   !! mean knot spacing, `(x(n) - x(1))/(n - 1)`
    contains
        procedure :: init => interp_1d_init          !! Stores the table and builds the coefficients.
        ! No specific of `eval` is elemental: a generic holding an elemental specific beside a rank-1 one is
        ! resolved by the order of its specifics under gfortran (`fortran-gotchas.md`), so each rank has its own.
        procedure, private :: eval_rank0 => interp_1d_eval       !! `%eval` at one point.
        procedure, private :: eval_rank1 => interp_1d_eval_rank1 !! `%eval` over a rank-1 array of points.
        procedure, private :: eval_rank2 => interp_1d_eval_rank2 !! `%eval` over a rank-2 array of points.
        procedure, private :: eval_rank3 => interp_1d_eval_rank3 !! `%eval` over a rank-3 array of points.
        procedure, private :: eval_rank4 => interp_1d_eval_rank4 !! `%eval` over a rank-4 array of points.
        procedure, private :: eval_rank5 => interp_1d_eval_rank5 !! `%eval` over a rank-5 array of points.
        procedure, private :: eval_rank6 => interp_1d_eval_rank6 !! `%eval` over a rank-6 array of points.
        procedure, private :: eval_rank7 => interp_1d_eval_rank7 !! `%eval` over a rank-7 array of points.
        generic :: eval => eval_rank0, eval_rank1, eval_rank2, eval_rank3, eval_rank4, eval_rank5, eval_rank6, &
                           eval_rank7                !! Value at a point, or at every point of an array; `pure`.
        procedure :: derivative => interp_1d_derivative !! First or second derivative; `pure elemental`.
        procedure :: integral => interp_1d_integral  !! Definite integral over `[a, b]`; `pure`.
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
        !! must still number at least two, or four for `bc="not_a_knot"`. `method` is `"linear"`,
        !! `"cubic"` (the default: a C2 cubic spline) or `"pchip"` (a C1 cubic that keeps monotone
        !! data monotone). `bc` is the cubic spline's end condition: `"natural"` (the default, zero
        !! curvature at both ends), `"not_a_knot"` (a continuous third derivative at the second and
        !! the second-to-last point) or `"clamped"`, whose first derivatives at the ends are
        !! `slopes(1)` at the table's first point and `slopes(2)` at its last, in the order given.
        !! `outside` says what a point beyond the table answers: `"clamp"` (the default),
        !! `"extrapolate"` or `"nan"`. Tokens are matched without regard to case. `context` is
        !! appended to any abort message. A second `%init` on a built object replaces its table.
        module subroutine interp_1d_init(this, x, y, method, bc, slopes, outside, is_valid, context)
            implicit none
            class(pf_interp_1d), intent(out)       :: this       !! the interpolant to build
            real(real64), intent(in)               :: x(:)       !! abscissae, strictly monotonic
            real(real64), intent(in)               :: y(:)       !! ordinates, one per abscissa
            character(len=*), intent(in), optional :: method     !! `"linear"`, `"cubic"` or `"pchip"`
            character(len=*), intent(in), optional :: bc         !! the spline's end condition
            real(real64), intent(in), optional     :: slopes(:)  !! the two end slopes for `bc="clamped"`
            character(len=*), intent(in), optional :: outside    !! `"clamp"`, `"extrapolate"` or `"nan"`
            logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
            character(len=*), intent(in), optional :: context    !! call-site text for abort messages
        end subroutine interp_1d_init

        !> The interpolant's value at `xq`: `pure`.
        !!
        !! A NaN query answers NaN. A query outside the table answers what the object's `outside=`
        !! policy says; a query on a knot answers that knot's ordinate exactly. Aborts on an object
        !! that was never built. `%eval` over an array of queries of rank 1 to 7 reaches the specific
        !! of that rank, which answers each query this answer, bit for bit.
        pure module function interp_1d_eval(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this !! the interpolant
            real(real64), intent(in)        :: xq   !! the query point
            real(real64)                    :: v    !! the interpolated value
        end function interp_1d_eval

        !> The interpolant's value at every point of a rank-1 array of queries, in their order: what
        !! `%eval` answers at each point alone, bit for bit.
        !!
        !! The search for each query's segment starts from the segment the query before it fell in, so
        !! queries in ascending or descending order find theirs in a comparison or two on a table of
        !! any length, and queries in no order pay a few comparisons more than a bisection; an evenly
        !! spaced table is bracketed by arithmetic either way. The search state lives in the call, not
        !! in the object, which stays shareable. Aborts on an object that was never built, even for an
        !! array holding no query.
        pure module function interp_1d_eval_rank1(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this  !! the interpolant
            real(real64), intent(in)        :: xq(:) !! the query points
            real(real64), allocatable       :: v(:)  !! the interpolated value at each, in order
        end function interp_1d_eval_rank1

        !> `%eval` over a rank-2 array of queries: the value at each, in the array's shape, searched in
        !! array element order as over a rank-1 array.
        pure module function interp_1d_eval_rank2(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this     !! the interpolant
            real(real64), intent(in)        :: xq(:, :) !! the query points
            real(real64), allocatable       :: v(:, :)  !! the interpolated value at each
        end function interp_1d_eval_rank2

        !> `%eval` over a rank-3 array of queries: the value at each, in the array's shape, searched in
        !! array element order as over a rank-1 array.
        pure module function interp_1d_eval_rank3(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this     !! the interpolant
            real(real64), intent(in)        :: xq(:, :, :) !! the query points
            real(real64), allocatable       :: v(:, :, :)  !! the interpolated value at each
        end function interp_1d_eval_rank3

        !> `%eval` over a rank-4 array of queries: the value at each, in the array's shape, searched in
        !! array element order as over a rank-1 array.
        pure module function interp_1d_eval_rank4(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this     !! the interpolant
            real(real64), intent(in)        :: xq(:, :, :, :) !! the query points
            real(real64), allocatable       :: v(:, :, :, :)  !! the interpolated value at each
        end function interp_1d_eval_rank4

        !> `%eval` over a rank-5 array of queries: the value at each, in the array's shape, searched in
        !! array element order as over a rank-1 array.
        pure module function interp_1d_eval_rank5(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this     !! the interpolant
            real(real64), intent(in)        :: xq(:, :, :, :, :) !! the query points
            real(real64), allocatable       :: v(:, :, :, :, :)  !! the interpolated value at each
        end function interp_1d_eval_rank5

        !> `%eval` over a rank-6 array of queries: the value at each, in the array's shape, searched in
        !! array element order as over a rank-1 array.
        pure module function interp_1d_eval_rank6(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this     !! the interpolant
            real(real64), intent(in)        :: xq(:, :, :, :, :, :) !! the query points
            real(real64), allocatable       :: v(:, :, :, :, :, :)  !! the interpolated value at each
        end function interp_1d_eval_rank6

        !> `%eval` over a rank-7 array of queries: the value at each, in the array's shape, searched in
        !! array element order as over a rank-1 array.
        pure module function interp_1d_eval_rank7(this, xq) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this     !! the interpolant
            real(real64), intent(in)        :: xq(:, :, :, :, :, :, :) !! the query points
            real(real64), allocatable       :: v(:, :, :, :, :, :, :)  !! the interpolated value at each
        end function interp_1d_eval_rank7

        !> The interpolant's first or second derivative at `xq`: `pure elemental`.
        !!
        !! ```
        !! dv = c%derivative(xq, [order])
        !! ```
        !!
        !! `order` is 1 (the default) or 2. The derivative is taken on the segment `%eval` evaluates
        !! `xq` on, so at an interior knot it is the right-hand segment's, and at the last knot the
        !! last segment's: the difference shows where the interpolant is not smooth enough to have
        !! that derivative -- a `"linear"` slope everywhere, a `"pchip"` curvature. A NaN query
        !! answers NaN. Beyond the table `"clamp"` answers zero, `"extrapolate"` the end segment's
        !! derivative and `"nan"` a NaN. Aborts on an object that was never built, and on an `order`
        !! other than 1 or 2.
        pure elemental module function interp_1d_derivative(this, xq, order) result(v)
            implicit none
            class(pf_interp_1d), intent(in) :: this  !! the interpolant
            real(real64), intent(in)        :: xq    !! the query point
            integer, intent(in), optional   :: order !! 1 (the default) or 2
            real(real64)                    :: v     !! the derivative there
        end function interp_1d_derivative

        !> The definite integral of the interpolant from `a` to `b`: `pure`.
        !!
        !! Exact for the interpolant, which is a polynomial on every segment. `b < a` negates, and
        !! `a == b` answers zero. A NaN limit answers NaN. A limit beyond the table follows the
        !! object's policy: `"clamp"` integrates the end value as a constant beyond each end,
        !! `"extrapolate"` the end segment's polynomial continued, and `"nan"` answers a NaN. The work
        !! grows with the number of knots between the limits. Aborts on an object that was never
        !! built.
        pure module function interp_1d_integral(this, a, b) result(s)
            implicit none
            class(pf_interp_1d), intent(in) :: this !! the interpolant
            real(real64), intent(in)        :: a    !! the lower limit
            real(real64), intent(in)        :: b    !! the upper limit
            real(real64)                    :: s    !! the integral
        end function interp_1d_integral

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

    ! ---- the two-dimensional interpolant -----------------------------------------------------

    !> A two-dimensional interpolant over values tabulated on a rectilinear grid: build it once with
    !! `%init`, then evaluate it anywhere.
    !!
    !! The grid is the outer product of two strictly monotonic axes, each with a spacing of its own,
    !! and `z(i, j)` is the value at `(x(i), y(j))`. Every component is private. An object holds its
    !! own copy of the grid, each axis stored ascending, and for the bicubic spline three tables of
    !! second derivatives the size of the grid. Intrinsic assignment copies it deeply; there is no
    !! finalizer.
    type :: pf_interp_2d
        private
        real(real64), allocatable :: x(:)        !! the grid lines along `x`, stored strictly increasing
        real(real64), allocatable :: y(:)        !! the grid lines along `y`, stored strictly increasing
        real(real64), allocatable :: z(:, :)     !! the values, `z(i, j)` at `(x(i), y(j))`
        real(real64), allocatable :: zxx(:, :)   !! second derivatives along `x` of every column of `z`;
                                                 !! unallocated for `linear`
        real(real64), allocatable :: zyy(:, :)   !! second derivatives along `y` of every row of `z`;
                                                 !! unallocated for `linear`
        real(real64), allocatable :: zxxyy(:, :) !! second derivatives along `x` of every column of `zyy`;
                                                 !! unallocated for `linear`
        integer      :: nx = 0                   !! grid lines along `x`; 0 until `%init`. A default integer,
                                                 !! since an axis longer than `huge(1)` lines is not an
                                                 !! input this module is for
        integer      :: ny = 0                   !! grid lines along `y`
        integer      :: method = 0               !! `M_LINEAR` or `M_CUBIC`
        integer      :: outside = 0              !! `O_CLAMP`, `O_EXTRAPOLATE` or `O_NAN`
        logical      :: x_reversed = .false.     !! the caller's `x` was descending
        logical      :: y_reversed = .false.     !! the caller's `y` was descending
        logical      :: x_uniform = .false.      !! `x` evenly spaced: its bracket is guessed by arithmetic
        logical      :: y_uniform = .false.      !! `y` evenly spaced: its bracket is guessed by arithmetic
        real(real64) :: x_step = 0.0_real64      !! mean spacing along `x`
        real(real64) :: y_step = 0.0_real64      !! mean spacing along `y`
    contains
        procedure :: init => interp_2d_init            !! Stores the grid and builds the tables.
        procedure :: eval => interp_2d_eval            !! Value at a point; `pure elemental`.
        procedure :: is_initialised => interp_2d_ready !! `.true.` once built; never aborts.
        procedure :: clear => interp_2d_clear          !! Releases the grid; the object may be rebuilt.
    end type pf_interp_2d

    ! ---- the bindings, implemented in parquet_interpolate_2d.f90 ----------------------------

    interface

        !> Builds the interpolant over the values `z` on the grid `x` by `y`.
        !!
        !! ```
        !! call g%init(x, y, z, [method], [bc], [outside], [context])
        !! ```
        !!
        !! `z(i, j)` is the value at `(x(i), y(j))`, so `z` is shaped `(size(x), size(y))`. Each axis
        !! must be strictly increasing or strictly decreasing and finite, with a spacing of its own,
        !! and must hold at least two lines, or four for `bc="not_a_knot"`; every value must be finite.
        !! `method` is `"linear"`, which is bilinear interpolation on each cell, or `"cubic"` (the
        !! default), the bicubic spline: the cubic spline along each axis in turn, whose restriction to
        !! any grid line is the one-dimensional spline of that line. `bc` is its end condition on every
        !! edge, `"natural"` (the default) or `"not_a_knot"`. `outside` says what a point beyond the
        !! grid answers, for each coordinate on its own: `"clamp"` (the default), `"extrapolate"` or
        !! `"nan"`. Tokens are matched without regard to case. `context` is appended to any abort
        !! message. A second `%init` on a built object replaces its grid.
        module subroutine interp_2d_init(this, x, y, z, method, bc, outside, context)
            implicit none
            class(pf_interp_2d), intent(out)       :: this    !! the interpolant to build
            real(real64), intent(in)               :: x(:)    !! the grid lines along `x`, strictly monotonic
            real(real64), intent(in)               :: y(:)    !! the grid lines along `y`, strictly monotonic
            real(real64), intent(in)               :: z(:, :) !! the values, shaped `(size(x), size(y))`
            character(len=*), intent(in), optional :: method  !! `"linear"` or `"cubic"`
            character(len=*), intent(in), optional :: bc      !! the spline's end condition
            character(len=*), intent(in), optional :: outside !! `"clamp"`, `"extrapolate"` or `"nan"`
            character(len=*), intent(in), optional :: context !! call-site text for abort messages
        end subroutine interp_2d_init

        !> The interpolant's value at `(xq, yq)`: `pure elemental`, so two conforming arrays of
        !! coordinates answer an array.
        !!
        !! A NaN coordinate answers NaN. A coordinate beyond the grid answers what the object's
        !! `outside=` policy says, each coordinate on its own; a query on a grid node answers that
        !! node's value exactly. Aborts on an object that was never built.
        pure elemental module function interp_2d_eval(this, xq, yq) result(v)
            implicit none
            class(pf_interp_2d), intent(in) :: this !! the interpolant
            real(real64), intent(in)        :: xq   !! the query's `x`
            real(real64), intent(in)        :: yq   !! the query's `y`
            real(real64)                    :: v    !! the interpolated value
        end function interp_2d_eval

        !> `.true.` once `%init` has built the object and `%clear` has not released it since.
        pure module function interp_2d_ready(this) result(ok)
            implicit none
            class(pf_interp_2d), intent(in) :: this !! the interpolant
            logical                         :: ok   !! the object can be evaluated
        end function interp_2d_ready

        !> Releases the grid and returns the object to its unbuilt state; harmless on a fresh one.
        pure module subroutine interp_2d_clear(this)
            implicit none
            class(pf_interp_2d), intent(inout) :: this !! the interpolant to release
        end subroutine interp_2d_clear

    end interface

    ! ---- the one-shot generic, implemented in parquet_interpolate_1d.f90 and _2d.f90 ------------

    !> Interpolates the table `(x, y)` at `xq`, or the grid values `z` at `(xq, yq)`, in one call,
    !! building and discarding an interpolant.
    !!
    !! ```
    !! yq = pf_interp(x, y, xq, [method], [bc], [slopes], [outside], [is_valid], [context])
    !! zq = pf_interp(x, y, z, xq, yq, [method], [bc], [outside], [context])
    !! ```
    !!
    !! Optional arguments are shown in square brackets, with the comma outside the bracket. FORD
    !! renders no per-argument documentation for a generic with several specifics, so every argument
    !! is named here instead:
    !!
    !! * `x`, `y` -- in one dimension, the table, rank-1 `real64` of one size: `x` strictly increasing
    !!   or strictly decreasing and finite, `y` finite. In two dimensions, the grid lines along each
    !!   axis, each strictly increasing or strictly decreasing and finite.
    !! * `z` -- in two dimensions only, the values, rank-2 `real64` shaped `(size(x), size(y))`,
    !!   `z(i, j)` at `(x(i), y(j))`, every one finite.
    !! * `xq` -- in one dimension, the query: a scalar, answered by a scalar, or a rank-1 array,
    !!   answered by an array of its size.
    !! * `xq`, `yq` -- in two dimensions, the query's coordinates: two scalars, answered by a scalar,
    !!   or two rank-1 arrays of one size, answered by an array of that size.
    !! * `method` -- `"linear"`; `"cubic"` (the default), a C2 cubic spline; or, in one dimension
    !!   only, `"pchip"`, a C1 cubic that keeps monotone data monotone.
    !! * `bc` -- the cubic spline's end condition: `"natural"` (the default), `"not_a_knot"` or, in one
    !!   dimension only, `"clamped"`. Refused with any other method.
    !! * `slopes` -- in one dimension only, rank-1 `real64` of two elements, the first derivatives at
    !!   the table's first and last point for `bc="clamped"`; required with it and refused without it.
    !! * `outside` -- what a query beyond the table answers: `"clamp"` (the default, the end value),
    !!   `"extrapolate"` (the end segment continued) or `"nan"`; in two dimensions, for each
    !!   coordinate on its own.
    !! * `is_valid` -- in one dimension only, rank-1 `logical` of the table's size; a point marked
    !!   `.false.` is dropped.
    !! * `context` -- text appended to any abort message, capped at 100 characters.
    !!
    !! The answer is bit for bit what `%init` with the same arguments followed by `%eval` gives,
    !! because that is how it is computed; its abort messages begin `pf_interp: `. Building the
    !! interpolant is the expensive half, so a table queried more than a handful of times wants a
    !! `pf_interp_1d` or `pf_interp_2d` object instead.
    interface pf_interp

        !> One query point.
        module function interp_1d_oneshot_scalar(x, y, xq, method, bc, slopes, outside, is_valid, &
                                                 context) result(yq)
            implicit none
            real(real64), intent(in)               :: x(:)        !! abscissae, strictly monotonic
            real(real64), intent(in)               :: y(:)        !! ordinates, one per abscissa
            real(real64), intent(in)               :: xq          !! the query point
            character(len=*), intent(in), optional :: method      !! `"linear"`, `"cubic"` or `"pchip"`
            character(len=*), intent(in), optional :: bc          !! the spline's end condition
            real(real64), intent(in), optional     :: slopes(:)   !! end slopes for `bc="clamped"`
            character(len=*), intent(in), optional :: outside     !! the out-of-range policy
            logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
            character(len=*), intent(in), optional :: context     !! call-site text
            real(real64)                           :: yq          !! the interpolated value
        end function interp_1d_oneshot_scalar

        !> An array of query points. The result is allocatable rather than shaped `size(xq)`: ifx builds
        !! the caller's temporary for an explicit-shape result on the stack, where a large `xq` overflows
        !! it, on an OpenMP worker's small stack first.
        module function interp_1d_oneshot_array(x, y, xq, method, bc, slopes, outside, is_valid, &
                                                context) result(yq)
            implicit none
            real(real64), intent(in)               :: x(:)        !! abscissae, strictly monotonic
            real(real64), intent(in)               :: y(:)        !! ordinates, one per abscissa
            real(real64), intent(in)               :: xq(:)       !! the query points
            character(len=*), intent(in), optional :: method      !! `"linear"`, `"cubic"` or `"pchip"`
            character(len=*), intent(in), optional :: bc          !! the spline's end condition
            real(real64), intent(in), optional     :: slopes(:)   !! end slopes for `bc="clamped"`
            character(len=*), intent(in), optional :: outside     !! the out-of-range policy
            logical, intent(in), optional          :: is_valid(:) !! `.false.` drops that point
            character(len=*), intent(in), optional :: context     !! call-site text
            real(real64), allocatable              :: yq(:)       !! the interpolated values, one per query
        end function interp_1d_oneshot_array

        !> One query point on a grid.
        module function interp_2d_oneshot_scalar(x, y, z, xq, yq, method, bc, outside, context) result(zq)
            implicit none
            real(real64), intent(in)               :: x(:)    !! the grid lines along `x`, strictly monotonic
            real(real64), intent(in)               :: y(:)    !! the grid lines along `y`, strictly monotonic
            real(real64), intent(in)               :: z(:, :) !! the values, shaped `(size(x), size(y))`
            real(real64), intent(in)               :: xq      !! the query's `x`
            real(real64), intent(in)               :: yq      !! the query's `y`
            character(len=*), intent(in), optional :: method  !! `"linear"` or `"cubic"`
            character(len=*), intent(in), optional :: bc      !! the spline's end condition
            character(len=*), intent(in), optional :: outside !! the out-of-range policy
            character(len=*), intent(in), optional :: context !! call-site text
            real(real64)                           :: zq      !! the interpolated value
        end function interp_2d_oneshot_scalar

        !> An array of query points on a grid, given as two arrays of coordinates of one size. The result
        !! is allocatable for the reason the one-dimensional array form gives.
        module function interp_2d_oneshot_array(x, y, z, xq, yq, method, bc, outside, context) result(zq)
            implicit none
            real(real64), intent(in)               :: x(:)          !! the grid lines along `x`
            real(real64), intent(in)               :: y(:)          !! the grid lines along `y`
            real(real64), intent(in)               :: z(:, :)       !! the values, shaped `(size(x), size(y))`
            real(real64), intent(in)               :: xq(:)         !! the queries' `x`
            real(real64), intent(in)               :: yq(:)         !! the queries' `y`, one per `xq`
            character(len=*), intent(in), optional :: method        !! `"linear"` or `"cubic"`
            character(len=*), intent(in), optional :: bc            !! the spline's end condition
            character(len=*), intent(in), optional :: outside       !! the out-of-range policy
            character(len=*), intent(in), optional :: context       !! call-site text
            real(real64), allocatable              :: zq(:)         !! the interpolated values, one per query
        end function interp_2d_oneshot_array

    end interface pf_interp

    ! ---- the test-only hook, implemented in parquet_interpolate_1d.f90 and _2d.f90 --------------

    !> Test-only. Makes an object find every bracket by bisection, and reports whether it was
    !! finding them by arithmetic until now.
    !!
    !! An evenly spaced table is bracketed by arithmetic and every other one by bisection, and the
    !! two must answer the same bits; this is what lets one test evaluate the same table through both
    !! paths. A grid decides each axis on its own and reports each. Public only because the flags it
    !! clears are private components; no library code calls it, and there is no way to switch the
    !! arithmetic path back on short of `%init`.
    interface parquet_debug_interp_force_search

        !> The one-dimensional object's form.
        module subroutine interp_1d_force_search(this, was_uniform)
            implicit none
            type(pf_interp_1d), intent(inout) :: this        !! the interpolant to switch
            logical, intent(out)              :: was_uniform !! it was bracketing by arithmetic
        end subroutine interp_1d_force_search

        !> The two-dimensional object's form, reporting each axis.
        module subroutine interp_2d_force_search(this, x_was_uniform, y_was_uniform)
            implicit none
            type(pf_interp_2d), intent(inout) :: this          !! the interpolant to switch
            logical, intent(out)              :: x_was_uniform !! `x` was bracketed by arithmetic
            logical, intent(out)              :: y_was_uniform !! `y` was bracketed by arithmetic
        end subroutine interp_2d_force_search

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

        !> A real as text in scientific form with four significant digits and a three-digit exponent,
        !! `1.000E+160`, left-justified in a 24-character buffer the caller trims.
        pure module function interp_r2s(v) result(text)
            implicit none
            real(real64), intent(in) :: v    !! the value
            character(len=24)        :: text !! its digits, blank-padded
        end function interp_r2s

        !> Folds a caller's token to lower case for matching against the names this module knows.
        !!
        !! ASCII only. A token whose trimmed length exceeds `TOKEN_CAP` folds to blanks, which match
        !! no name, so its caller refuses it as unknown whatever its first characters are -- a token
        !! with blanks inside it included, which a fold keeping its first `TOKEN_CAP` characters could
        !! trim down to a known name.
        module subroutine interp_fold_token(token, folded)
            implicit none
            character(len=*), intent(in)          :: token  !! the caller's token
            character(len=TOKEN_CAP), intent(out) :: folded !! its lower-case form, or blanks
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
        !! comparison against a NaN raises IEEE_INVALID. A repeated value fails both as well. An
        !! infinity passes, and can only be the first or the last value of a strictly monotonic `x`.
        module function interp_is_monotonic(x, ascending) result(ok)
            implicit none
            real(real64), intent(in) :: x(:)      !! at least two abscissae
            logical, intent(out)     :: ascending !! `.true.` when increasing; meaningful when `ok`
            logical                  :: ok        !! strictly monotonic, with no NaN
        end function interp_is_monotonic

        !> `.true.` when no value of `v` is a NaN or an infinity.
        !!
        !! Judged from each value's bits: an all-ones exponent field is exactly a NaN or an infinity,
        !! and testing it is integer arithmetic, which raises no IEEE flag and compiles to a loop both
        !! gfortran and ifx vectorise. This is the whole-table screen of every `%init`, where a
        !! per-element `ieee_is_finite` is a runtime call under ifx.
        pure module function interp_all_finite(v) result(ok)
            implicit none
            real(real64), intent(in) :: v(:) !! the values
            logical                  :: ok   !! every one is finite
        end function interp_all_finite

        !> Measures whether an ascending table is evenly spaced enough to bracket by arithmetic.
        !!
        !! Every knot must lie within `UNIFORM_TOL_FRACTION` of a step of its place on the straight
        !! line through the two ends. The mean spacing is returned either way.
        module subroutine interp_uniform_step(x, uniform, step)
            implicit none
            real(real64), intent(in)  :: x(:)    !! at least two abscissae, strictly increasing
            logical, intent(out)      :: uniform !! evenly spaced to the tolerance
            real(real64), intent(out) :: step    !! `(x(n) - x(1))/(n - 1)`
        end subroutine interp_uniform_step

        !> The widest spacing of an ascending table, measured only when the table's span exceeds
        !! `SPLINE_MAX_SPACING`, since no spacing can exceed the span; zero otherwise.
        pure module function interp_widest_spacing(x) result(widest)
            implicit none
            real(real64), intent(in) :: x(:)   !! at least two abscissae, strictly increasing, finite
            real(real64)             :: widest !! the widest spacing, or zero when the span is within the limit
        end function interp_widest_spacing

        !> The narrowest spacing of an ascending table.
        pure module function interp_narrowest_spacing(x) result(narrowest)
            implicit none
            real(real64), intent(in) :: x(:)      !! at least two abscissae, strictly increasing
            real(real64)             :: narrowest !! the narrowest spacing
        end function interp_narrowest_spacing

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

        !> The segment `interp_bracket` names, searched from segment `k0` outward: `k0` itself, then the
        !! segment after it or the one before, then a bisection of the part of the table on the query's
        !! side of those.
        !!
        !! `xq` must lie inside `[x(1), x(n)]` and `k0` in `[1, n-1]`. The segment the inequality names
        !! is unique, so the answer is `interp_bracket`'s whatever `k0` is; only the work depends on it.
        pure module function interp_bracket_near(x, n, k0, xq) result(k)
            implicit none
            real(real64), intent(in) :: x(:) !! abscissae, strictly increasing
            integer, intent(in)      :: n    !! points in `x`, at least two
            integer, intent(in)      :: k0   !! the segment to search from
            real(real64), intent(in) :: xq   !! the query, inside the table
            integer                  :: k    !! the segment's left knot
        end function interp_bracket_near

    end interface

    ! ---- the tridiagonal solver and the per-knot coefficients, in parquet_interpolate_core.f90 --

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
        !!
        !! `s1` and `sn` are the first derivatives at `x(1)` and `x(n)` that `B_CLAMPED` imposes, in
        !! the stored ascending order; the other end conditions do not read them. `B_NOT_A_KNOT`
        !! needs at least four points, the others two.
        pure module subroutine interp_spline_coeffs(x, y, bc, s1, sn, m)
            implicit none
            real(real64), intent(in)  :: x(:) !! abscissae, strictly increasing
            real(real64), intent(in)  :: y(:) !! ordinates
            integer, intent(in)       :: bc   !! `B_NATURAL`, `B_NOT_A_KNOT` or `B_CLAMPED`
            real(real64), intent(in)  :: s1   !! the slope at `x(1)`, for `B_CLAMPED`
            real(real64), intent(in)  :: sn   !! the slope at `x(n)`, for `B_CLAMPED`
            real(real64), intent(out) :: m(:) !! second derivatives at the knots
        end subroutine interp_spline_coeffs

        !> The slopes `d` at the knots of the shape-preserving piecewise cubic Hermite interpolant
        !! through `(x, y)`: Fritsch and Carlson's monotone slopes with Butland's weighted harmonic
        !! mean inside, and a three-point estimate held to the data's shape at each end.
        pure module subroutine interp_pchip_slopes(x, y, d)
            implicit none
            real(real64), intent(in)  :: x(:) !! abscissae, strictly increasing, at least two
            real(real64), intent(in)  :: y(:) !! ordinates
            real(real64), intent(out) :: d(:) !! first derivatives at the knots
        end subroutine interp_pchip_slopes

    end interface

    ! ---- the segment polynomials, implemented in parquet_interpolate_core.f90 ----------------

    interface

        !> The end segment's polynomial continued beyond the table, at `xq`.
        !!
        !! The segment from `x0` to `x1` is expanded about its end knot -- `x1` for the table's last
        !! segment continued above it, `x0` for its first continued below -- as
        !! `y + e*(d + t*(c2 + t*c3))`, with `e` the query's distance from that knot, `t` the same in
        !! widths of the segment, `d` the slope at the knot and `c2` and `c3` in units of a slope. Each
        !! coefficient is formed from differences of the segment's own data, so a straight segment has
        !! exact zeros beyond its slope, and nothing formed in the fraction of the segment's own origin
        !! is left to cancel however far away `xq` lies. `k0` and `k1` are second derivatives for
        !! `M_CUBIC` and slopes for `M_PCHIP`; `M_LINEAR` reads neither.
        pure module function interp_end_value(method, x0, x1, y0, y1, k0, k1, above, xq) result(v)
            implicit none
            integer, intent(in)      :: method !! `M_LINEAR`, `M_CUBIC` or `M_PCHIP`
            real(real64), intent(in) :: x0     !! the segment's left knot
            real(real64), intent(in) :: x1     !! the segment's right knot
            real(real64), intent(in) :: y0     !! the ordinate at `x0`
            real(real64), intent(in) :: y1     !! the ordinate at `x1`
            real(real64), intent(in) :: k0     !! the method's coefficient at `x0`
            real(real64), intent(in) :: k1     !! the method's coefficient at `x1`
            logical, intent(in)      :: above  !! continued above `x1`, rather than below `x0`
            real(real64), intent(in) :: xq     !! the point, beyond that knot
            real(real64)             :: v      !! the polynomial's value there
        end function interp_end_value

        !> The first (`order` 1) or second (`order` 2) derivative of the end segment's polynomial
        !! continued beyond the table, at `xq`, in the form `interp_end_value` describes.
        pure module function interp_end_derivative(method, x0, x1, y0, y1, k0, k1, above, xq, order) result(v)
            implicit none
            integer, intent(in)      :: method !! `M_LINEAR`, `M_CUBIC` or `M_PCHIP`
            real(real64), intent(in) :: x0     !! the segment's left knot
            real(real64), intent(in) :: x1     !! the segment's right knot
            real(real64), intent(in) :: y0     !! the ordinate at `x0`
            real(real64), intent(in) :: y1     !! the ordinate at `x1`
            real(real64), intent(in) :: k0     !! the method's coefficient at `x0`
            real(real64), intent(in) :: k1     !! the method's coefficient at `x1`
            logical, intent(in)      :: above  !! continued above `x1`, rather than below `x0`
            real(real64), intent(in) :: xq     !! the point, beyond that knot
            integer, intent(in)      :: order  !! 1 or 2
            real(real64)             :: v      !! the derivative there
        end function interp_end_derivative

        !> The integral of the end segment's polynomial continued beyond the table, from its end knot to
        !! `xq`, in the form `interp_end_value` describes: negative for a positive polynomial below the
        !! table, where `xq` lies below the knot.
        pure module function interp_end_integral(method, x0, x1, y0, y1, k0, k1, above, xq) result(v)
            implicit none
            integer, intent(in)      :: method !! `M_LINEAR`, `M_CUBIC` or `M_PCHIP`
            real(real64), intent(in) :: x0     !! the segment's left knot
            real(real64), intent(in) :: x1     !! the segment's right knot
            real(real64), intent(in) :: y0     !! the ordinate at `x0`
            real(real64), intent(in) :: y1     !! the ordinate at `x1`
            real(real64), intent(in) :: k0     !! the method's coefficient at `x0`
            real(real64), intent(in) :: k1     !! the method's coefficient at `x1`
            logical, intent(in)      :: above  !! continued above `x1`, rather than below `x0`
            real(real64), intent(in) :: xq     !! the upper limit; the end knot is the lower
            real(real64)             :: v      !! the integral
        end function interp_end_integral

        !> The straight line through `(x0, y0)` and `(x1, y1)` at `xq` between them.
        pure module function interp_linear_seg_value(x0, x1, y0, y1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: xq !! the point
            real(real64)             :: v  !! the line's value there
        end function interp_linear_seg_value

        !> The slope (`order` 1) or the curvature (`order` 2, zero) of the straight line through
        !! `(x0, y0)` and `(x1, y1)`.
        pure module function interp_linear_seg_derivative(x0, x1, y0, y1, order) result(v)
            implicit none
            real(real64), intent(in) :: x0    !! the segment's left knot
            real(real64), intent(in) :: x1    !! the segment's right knot
            real(real64), intent(in) :: y0    !! the ordinate at `x0`
            real(real64), intent(in) :: y1    !! the ordinate at `x1`
            integer, intent(in)      :: order !! 1 or 2
            real(real64)             :: v     !! the derivative
        end function interp_linear_seg_derivative

        !> The integral from `x0` to `xq` of the straight line through `(x0, y0)` and `(x1, y1)`.
        pure module function interp_linear_seg_integral(x0, x1, y0, y1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot, the lower limit
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: xq !! the upper limit, inside the segment
            real(real64)             :: v  !! the integral
        end function interp_linear_seg_integral

        !> One cubic-spline segment, in second-derivative form, at `xq` inside it.
        !!
        !! The curvature term is a product of both fractions of the segment, each measured from its own
        !! knot, so it vanishes to the bits beside either knot however large the second derivatives are;
        !! the left knot itself answers `y0` directly (the header of parquet_interpolate_core.f90 says
        !! why).
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

        !> The first (`order` 1) or second (`order` 2) derivative of one cubic-spline segment at `xq`
        !! inside it.
        pure module function interp_cubic_seg_derivative(x0, x1, y0, y1, m0, m1, xq, order) result(v)
            implicit none
            real(real64), intent(in) :: x0    !! the segment's left knot
            real(real64), intent(in) :: x1    !! the segment's right knot
            real(real64), intent(in) :: y0    !! the ordinate at `x0`
            real(real64), intent(in) :: y1    !! the ordinate at `x1`
            real(real64), intent(in) :: m0    !! the second derivative at `x0`
            real(real64), intent(in) :: m1    !! the second derivative at `x1`
            real(real64), intent(in) :: xq    !! the point
            integer, intent(in)      :: order !! 1 or 2
            real(real64)             :: v     !! the derivative there
        end function interp_cubic_seg_derivative

        !> The integral from `x0` to `xq` of one cubic-spline segment, `xq` inside it.
        pure module function interp_cubic_seg_integral(x0, x1, y0, y1, m0, m1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot, the lower limit
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: m0 !! the second derivative at `x0`
            real(real64), intent(in) :: m1 !! the second derivative at `x1`
            real(real64), intent(in) :: xq !! the upper limit
            real(real64)             :: v  !! the integral
        end function interp_cubic_seg_integral

        !> One cubic Hermite segment, given its end ordinates and end slopes, at `xq` inside it.
        pure module function interp_hermite_seg_value(x0, x1, y0, y1, d0, d1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: d0 !! the slope at `x0`
            real(real64), intent(in) :: d1 !! the slope at `x1`
            real(real64), intent(in) :: xq !! the point
            real(real64)             :: v  !! the cubic's value there
        end function interp_hermite_seg_value

        !> The first (`order` 1) or second (`order` 2) derivative of one cubic Hermite segment at
        !! `xq` inside it.
        pure module function interp_hermite_seg_derivative(x0, x1, y0, y1, d0, d1, xq, order) result(v)
            implicit none
            real(real64), intent(in) :: x0    !! the segment's left knot
            real(real64), intent(in) :: x1    !! the segment's right knot
            real(real64), intent(in) :: y0    !! the ordinate at `x0`
            real(real64), intent(in) :: y1    !! the ordinate at `x1`
            real(real64), intent(in) :: d0    !! the slope at `x0`
            real(real64), intent(in) :: d1    !! the slope at `x1`
            real(real64), intent(in) :: xq    !! the point
            integer, intent(in)      :: order !! 1 or 2
            real(real64)             :: v     !! the derivative there
        end function interp_hermite_seg_derivative

        !> The integral from `x0` to `xq` of one cubic Hermite segment, `xq` inside it.
        pure module function interp_hermite_seg_integral(x0, x1, y0, y1, d0, d1, xq) result(v)
            implicit none
            real(real64), intent(in) :: x0 !! the segment's left knot, the lower limit
            real(real64), intent(in) :: x1 !! the segment's right knot
            real(real64), intent(in) :: y0 !! the ordinate at `x0`
            real(real64), intent(in) :: y1 !! the ordinate at `x1`
            real(real64), intent(in) :: d0 !! the slope at `x0`
            real(real64), intent(in) :: d1 !! the slope at `x1`
            real(real64), intent(in) :: xq !! the upper limit
            real(real64)             :: v  !! the integral
        end function interp_hermite_seg_integral

    end interface

end module parquet_interpolate
