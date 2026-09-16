---
title: Interpolation of tabulated data with parquet_interpolate
---

`parquet_interpolate` reads a value off a table of numbers at any point: build an interpolant from
a table of abscissae and ordinates once, then evaluate, differentiate or integrate it wherever you
need. It reaches no reader, no writer and no setting: `use parquet_interpolate` compiles four
Fortran files and nothing of the Arrow stack. `use parquet` brings it in too, so nothing here needs
a second import. See [Choosing a module](../operating/choosing-a-module.html) for what each entry
module costs.

Three methods: straight lines between neighbouring points; a cubic spline — a smooth curve whose
slope and curvature are continuous across every point; and PCHIP, a cubic that keeps monotone data
monotone and never overshoots it. The table is `real64`, ascending or descending, and what a query
beyond its ends answers is a choice you make once, when you build. Values tabulated on a grid are
interpolated the same way, bilinearly or with a bicubic spline: see
[Two dimensions](#two-dimensions).

## Quick example

```fortran
program interpolation_example
    use parquet_interpolate
    use iso_fortran_env, only : real64
    implicit none

    type(pf_interp_1d) :: curve, cubic, shape, line
    real(real64) :: x(6), y(6)
    integer :: i

    ! A table: y = x**2 at x = 1, 2, ..., 6.
    x = [(real(i, real64), i = 1, 6)]
    y = x*x

    call curve%init(x, y)                        ! a natural cubic spline, the default
    call cubic%init(x, y, bc="not_a_knot")       ! a cubic spline that reproduces any cubic
    call shape%init(x, y, method="pchip")        ! a cubic that never overshoots the data
    call line%init(x, y, method="linear")

    print *, curve%eval(2.5_real64)              ! 6.2237 -- not the parabola's 6.25; see below
    print *, cubic%eval(2.5_real64)              ! 6.25
    print *, shape%eval(2.5_real64)              ! 6.2396
    print *, line%eval(2.5_real64)               ! 6.5
    print *, curve%eval([1.5_real64, 5.5_real64])   ! 2.3421 and 30.3421, in one call
    print *, curve%eval(99.0_real64)             ! 36.0: beyond the table, the last ordinate

    print *, cubic%derivative(2.5_real64)        ! 5.0, the slope
    print *, cubic%derivative(2.5_real64, 2)     ! 2.0, the curvature
    print *, cubic%integral(1.0_real64, 6.0_real64)   ! 71.667, the area under the curve

    ! One call, no object.
    print *, pf_interp(x, y, 2.5_real64, method="linear")   ! 6.5
end program interpolation_example
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call c%init(x, y, [method], [bc], [slopes], [outside], [is_valid], [context])
v  = c%eval(xq)
dv = c%derivative(xq, [order])
s  = c%integral(a, b)
ok = c%is_initialised()
call c%clear()
yq = pf_interp(x, y, xq, [method], [bc], [slopes], [outside], [is_valid], [context])

call g%init(x, y, z, [method], [bc], [outside], [context])
v  = g%eval(xq, yq)
zq = pf_interp(x, y, z, xq, yq, [method], [bc], [outside], [context])
```

## Building an interpolant

`call c%init(x, y)` copies the table, computes what the method needs, and leaves `c` ready to
evaluate. Your arrays are not modified, and the object keeps its own copy, so they may change or go
away afterwards.

The table has to be one that can be interpolated, and `%init` checks that it is:

- `x` and `y` are the same size;
- there are at least two points, or four for `bc="not_a_knot"`;
- `x` is strictly increasing or strictly decreasing — no repeated value, and no NaN;
- every `x` and every `y` is finite.

Anything else aborts, with a message naming the rule; [What aborts](#what-aborts) lists each.

**A descending table works as it stands.** It is stored ascending, and answers exactly what the
same table given ascending answers, bit for bit — a table generated from decreasing redshift, or
read back to front, needs no flipping at the call site.

**`is_valid=` drops points before anything is built.** Pass a mask the size of the table, and the
points it marks `.false.` are left out as though they were never there, together with whatever
they hold — a NaN in a dropped point is not an error. The points that survive must still meet the
rules above. This is how a column with nulls reaches an interpolant:

```fortran
call c%init(x, y, is_valid=valid)
```

**A second `%init` replaces the table**, and there is nothing to free in between. `%clear` releases
the table early and leaves the object unbuilt; `%is_initialised()` reports whether an object can be
evaluated. Evaluating one that cannot is a fatal error, not a defined value.

Tokens are matched without regard to case: `method="Linear"` is `method="linear"`.

## Choosing a method

**`method="linear"`** joins neighbouring points with straight lines. It reproduces a straight line
exactly and never overshoots the data, which makes it the right choice for a coarse or noisy table,
where a curve through the points would invent structure between them. Its slope jumps at every
point.

**`method="cubic"`**, the default, is a cubic spline: one cubic per segment, joined so that the
value, the slope and the curvature are all continuous across every point. On smooth data it is far
more accurate than straight lines through the same table. On data that turn sharply between widely
or unevenly spaced points it overshoots, where straight lines would not. How it behaves at the two
ends of the table is its end condition, the next section.

**`method="pchip"`** is a piecewise cubic Hermite interpolant whose slopes are chosen to preserve the
data's shape: between two points where the data rise, it rises; where two neighbouring ordinates
are equal, it is flat; and on every segment it stays between that segment's two ordinates, so it
never overshoots a peak or a trough. Its slope is continuous; its curvature is not. It is the
method for a relation known to be monotone — a cumulative distribution, distance against redshift
— where a spline's overshoot would break the ordering. Each slope depends only on the points
beside it, so a change to one ordinate moves the interpolant only from the second point before it
to the second point after it.

The same step, `y = 0, 0, 0, 1, 1, 1` at `x = 1, 2, ..., 6`, read off under each:

| `x` | `"linear"` | `"cubic"` | `"pchip"` |
|---|---|---|---|
| 2.5 | 0 | -0.1023 | 0 |
| 3.25 | 0.25 | 0.2159 | 0.1562 |
| 3.5 | 0.5 | 0.5 | 0.5 |
| 3.75 | 0.75 | 0.7841 | 0.8438 |
| 4.5 | 1 | 1.1023 | 1 |

The spline dips below the first step and rises above the second; PCHIP stays between them, and
flat where the data are.

## End conditions

A cubic spline's system has two equations fewer than it has unknowns, and `bc=` supplies them, one
at each end. It is accepted with `method="cubic"` only.

**`bc="natural"`**, the default, sets the spline's curvature to zero at both ends. It reproduces a
straight line exactly, and it does **not** reproduce a parabola — on `y = x**2` at `x = 1..6` it
answers `6.2237` at `x = 2.5` rather than `6.25`, because a parabola's curvature is not zero at the
ends and the difference spreads inward from them.

**`bc="not_a_knot"`** makes the third derivative continuous across the second point and the
second-to-last, so the first two segments are one cubic and so are the last two. It reproduces any
cubic exactly — the quick example's `6.25` — and is the default of scipy's `CubicSpline`. It needs
at least four points: with four, it is the one cubic through all of them.

**`bc="clamped"`** sets the first derivative at each end to a value you give, as
`slopes=[s_first, s_last]`: `slopes(1)` at the first point of the table as you pass it and
`slopes(2)` at its last — for a descending table, the first point is the largest `x` — and after
`is_valid` has dropped what it drops. It reproduces any cubic whose end slopes you give exactly.
`slopes` is required with `bc="clamped"` and refused with anything else.

Where the table's own ends matter less than its middle, tabulate beyond the range you care about:
on an evenly spaced table, an end condition's influence falls off by a factor of about four with
each point into the table.

## Evaluating

`c%eval(xq)` is `pure` and `elemental`: a scalar query answers a scalar, and an array of any shape
answers an array of the same shape, so a whole column is interpolated in one call. Being `pure`, it
may be called from your own `pure` procedures.

**A query on a point of the table answers that point's ordinate exactly**, under every method and
every policy, the last point included.

**`c%derivative(xq, [order])`** is the interpolant's own first derivative (`order=1`, the default)
or second (`order=2`), `pure` and `elemental` in the same way. It is the derivative of the piece
`%eval` evaluates, so on a point of the table it is the next segment's, and on the last point the
last segment's. That only shows where the interpolant has no such derivative across the point: the
slope of `"linear"` and the curvature of `"pchip"`.

**`c%integral(a, b)`** is the interpolant's definite integral from `a` to `b`, exact for the
interpolant, since it is a polynomial on every segment. It is `pure`, not `elemental`; `b < a`
negates it and `a == b` answers zero. Its work grows with the number of points between the limits.

**Finding the segment** a query falls in is a bisection, except on an evenly spaced table, where it
is arithmetic and cheaper. `%init` detects an even table by itself; there is nothing to declare.
Both routes answer the same bits for every query, because the arithmetic answer is checked against
the table before it is used: a table that is only nearly even is never evaluated on the wrong
segment.

## Outside the table

`outside=` decides what a query beyond either end answers. It is fixed when the object is built:

| `outside=` | `%eval` beyond the table | `%derivative` there | `%integral` with a limit there |
|---|---|---|---|
| `"clamp"` (the default) | the ordinate at the nearer end | zero | the end ordinate, integrated as a constant beyond the end |
| `"extrapolate"` | the end segment's polynomial, continued | the end segment's, continued | the end segment's polynomial, integrated |
| `"nan"` | a quiet NaN | a quiet NaN | a quiet NaN |

Clamping is the default because a cubic leaves the data fast. On the quick example's table,
extrapolating the natural spline to `x = 99` answers about `-3.4e5` where the parabola is `9801` —
the wrong sign, not merely an inaccurate value. Extrapolate when you mean to, and only a little
beyond the table. `"nan"` is for a pipeline that filters its results afterwards: an out-of-range
answer then cannot pass for data.

**A NaN query answers NaN** from `%eval` and `%derivative`, and a NaN limit from `%integral`, under
every policy and raising no IEEE flag, so a column holding NaNs can be interpolated as it stands.

**An infinite query or limit is beyond the table in the direction of its sign.** `"clamp"` answers
the end ordinate and a zero derivative, and integrates the end ordinate out to the infinity, which
is an infinity of that ordinate's sign, or zero where the ordinate is zero. `"nan"` answers a NaN.
`"extrapolate"` answers whatever the end polynomial gives at an infinity, which is an infinity or a
NaN. A NaN made by arithmetic on an infinity — that one, or an integral from one infinity to the
other whose two ends extend with opposite signs — raises IEEE_INVALID, which ends a program built
with that trap enabled, as nagfor builds are by default.

## Two dimensions

`pf_interp_2d` interpolates values tabulated on a rectilinear grid: `x` holds the grid lines along
one axis and `y` those along the other, each strictly increasing or strictly decreasing with a
spacing of its own, and `z(i, j)` is the value at `(x(i), y(j))`, so `z` is shaped
`(size(x), size(y))`.

```fortran
type(pf_interp_2d) :: surface, plane
real(real64) :: gx(5), gy(4), gz(5, 4)
integer :: i, j

! A grid: z = x**2 * y at x = 0, 0.5, ..., 2 and y = 0, 1, 2, 3.
gx = [(0.5_real64*real(i - 1, real64), i = 1, 5)]
gy = [(real(j - 1, real64), j = 1, 4)]
do j = 1, 4
    gz(:, j) = gx**2*gy(j)
end do

call surface%init(gx, gy, gz, bc="not_a_knot")
call plane%init(gx, gy, gz, method="linear")

print *, surface%eval(0.75_real64, 1.5_real64)     ! 0.84375, which is x**2 * y there
print *, plane%eval(0.75_real64, 1.5_real64)       ! 0.9375
print *, surface%eval([0.25_real64, 1.25_real64], [0.5_real64, 2.5_real64])   ! 0.03125 and 3.90625
print *, surface%eval(3.0_real64, 1.5_real64)      ! 6.0: x is beyond the grid, and clamped to 2
```

`g%eval(xq, yq)` is `pure` and `elemental` in both coordinates: two scalars answer a scalar, and two
arrays of one shape answer an array of that shape. A query on a grid node answers that node's
value exactly.

**`method="linear"`** is bilinear interpolation: on each cell, linear along each axis. It
reproduces any `a + b*x + c*y + d*x*y` exactly and never leaves the range of a cell's four
values.

**`method="cubic"`**, the default, is the bicubic spline: the cubic spline along each axis in turn,
with the end condition `bc` on every edge of the grid, `"natural"` (the default) or `"not_a_knot"`,
which needs at least four grid lines along each axis. On any grid line it is the one-dimensional
spline of that line's values, and it reproduces whatever that spline reproduces along both axes at
once: under `"natural"`, anything straight along each axis, as bilinear interpolation does; under
`"not_a_knot"`, any polynomial of degree three or less in each coordinate, such as `x**2 * y` above.

**`outside=` applies to each coordinate on its own.** `"clamp"` moves a coordinate beyond the grid
onto the nearer grid line, `"extrapolate"` continues the polynomial of the nearest cell, and `"nan"`
answers a quiet NaN when either coordinate lies beyond the grid. A NaN coordinate answers NaN under
every policy, as a NaN query does in one dimension.

An axis given descending works as it stands, with `z` in the same order as that axis: the grid is
stored ascending and answers exactly what the same grid given ascending answers.

`method="pchip"` and `bc="clamped"` are refused on a grid. PCHIP's slopes depend on the data, so
interpolating along one axis and then the other gives a different surface for each order, and a
clamped surface would need a slope at every point of every edge. A grid object evaluates only; it
has no `%derivative` or `%integral`, and `%init` takes no `is_valid=`.

`%init` keeps its own copy of the grid and, for the bicubic spline, three more tables of the same
size. An evaluation then costs a bracket search along each axis — a bisection, or arithmetic along
an evenly spaced axis — and, for the bicubic spline, five cubic segments.

## One call, no object

```fortran
yq = pf_interp(x, y, xq, method="linear")
zq = pf_interp(x, y, z, xq, yq, method="linear")
```

`pf_interp` builds an interpolant, evaluates it at `xq` — a scalar, or a rank-1 array — and
discards it; given a grid's values `z`, it evaluates at `(xq, yq)`, two scalars or two rank-1 arrays
of one size. It takes the same optional arguments as `%init` and answers exactly what the object
answers, bit for bit, because that is how it computes; its abort messages begin `pf_interp:`
instead of `pf_interp_1d%init:` or `pf_interp_2d%init:`. The build is the expensive half, and this form repeats it on every
call, so a table queried more than a handful of times wants an object instead. There is no
one-shot derivative or integral: build an object.

## Threads

Only `%init` and `%clear` write an object. `%eval`, `%derivative` and `%integral` only read one, so
a built object may be used by every thread of a parallel region at once, and two objects never
share anything.

**Build one object before the region and share it** — the usual case:

```fortran
call curve%init(x, y)
!$omp parallel do default(shared) private(i)
do i = 1, n
    z(i) = curve%eval(redshift(i))
end do
!$omp end parallel do
```

**When each thread needs its own table, give each thread its own slot** in an array allocated
before the region, indexed by the thread number:

```fortran
allocate (per_thread(omp_get_max_threads()))
!$omp parallel do default(shared) private(i, t)
do i = 1, n
    t = omp_get_thread_num() + 1
    call per_thread(t)%init(x, table(:, i))
    z(i) = per_thread(t)%eval(query(i))
end do
!$omp end parallel do
```

Do not list a `pf_interp_1d` or a `pf_interp_2d` in a `private()` clause, where gfortran starts each
thread's copy from garbage rather than from an unbuilt object, and do not declare one in a `block`
inside the region, which ifx does not support for a type like these. `pf_interp` called from the loop is safe as
well: its object is local to the call. See [Thread safety](../operating/thread-safety.html) for how
this sits beside the rest of the library.

## What aborts

`%init` and `pf_interp` check their arguments in the order below, and the first failure ends the
program with `error stop` and a message that begins with the entry point, `pf_interp_1d%init:` or
`pf_interp:`. `context=` appends your own text to it, capped at 100 characters.

| Refused | Message |
|---|---|
| `x` and `y` of different sizes | `x and y differ in size: <nx> and <ny>` |
| an `is_valid` of a different size | `is_valid has <m> elements for <n> points` |
| an unknown `method` | `unknown method "<token>"; expected "linear", "cubic" or "pchip"` |
| `bc` with `method="linear"` or `method="pchip"` | `bc applies only to method "cubic"` |
| an unknown `bc` | `unknown bc "<token>"; expected "natural", "not_a_knot" or "clamped"` |
| `bc="clamped"` without `slopes` | `bc "clamped" needs slopes` |
| `slopes` without `bc="clamped"` | `slopes apply only to bc "clamped"` |
| `slopes` of a size other than two | `slopes must hold exactly 2 values, one per end; got <m>` |
| a NaN or an infinite slope | `slopes must be finite` |
| an unknown `outside` | `unknown outside "<token>"; expected "clamp", "extrapolate" or "nan"` |
| fewer than two points left after `is_valid`, or four for `bc="not_a_knot"` | `at least <k> points are needed for method "<method>"; got <n>`, the method followed by ` with bc "not_a_knot"` where that is the reason |
| an `x` not strictly monotonic, or holding a NaN | `x must be strictly increasing or strictly decreasing` |
| an infinite `x` | `x must be finite` |
| a NaN or an infinite `y` | `y must be finite` |

A grid is checked in its own order, below, with messages that begin `pf_interp_2d%init:`, or
`pf_interp:` for the one-shot form:

| Refused | Message |
|---|---|
| a `z` not shaped `(size(x), size(y))` | `z must be shaped (size(x), size(y)): got (<a>, <b>) for (<nx>, <ny>)` |
| an unknown `method` | `unknown method "<token>"; expected "linear" or "cubic"` |
| `method="pchip"` | `method "pchip" is not offered in two dimensions` |
| `bc` with `method="linear"` | `bc applies only to method "cubic"` |
| an unknown `bc` | `unknown bc "<token>"; expected "natural" or "not_a_knot"` |
| `bc="clamped"` | `bc "clamped" is not offered in two dimensions` |
| an unknown `outside` | `unknown outside "<token>"; expected "clamp", "extrapolate" or "nan"` |
| fewer than two grid lines along `x`, then along `y`, or four for `bc="not_a_knot"` | `at least <k> points are needed along x for method "<method>"; got <n>`, or `along y`, the method followed by ` with bc "not_a_knot"` where that is the reason |
| an `x` not strictly monotonic or holding a NaN, then a `y` | `x must be strictly increasing or strictly decreasing`, or the same of `y` |
| an infinite `x`, then an infinite `y` | `x must be finite`, or `y must be finite` |
| a NaN or an infinite value in `z` | `z must be finite` |

`pf_interp` given two arrays of coordinates of different sizes aborts with
`pf_interp: xq and yq differ in size: <a> and <b>`, before it builds anything.

Evaluating, differentiating or integrating an object that was never built aborts with
`pf_interp_1d%eval: the interpolant is not initialised`, or the same with `%derivative`,
`%integral` or `pf_interp_2d%eval`, and a derivative of any order but 1 or 2 with
`pf_interp_1d%derivative: order must be 1 or 2`. Those are `pure`, so their messages carry no
context. Nothing a query or a limit can be — a NaN, an infinity, a point far outside the table or
the grid — is an error.

## Moving from qfeet's `spline_type`

The linear and natural-spline arithmetic is qfeet's, so a migrated call answers the same values to
rounding, and the mapping is mechanical:

| qfeet | `parquet_interpolate` |
|---|---|
| `use interpolation, only: spline_type` | `use parquet_interpolate, only: pf_interp_1d` |
| `type(spline_type) :: s` | `type(pf_interp_1d) :: s` |
| `call s%init(xa, ya)` | `call s%init(x, y)`, the same natural cubic spline |
| `s%evaluate(x)` | `s%eval(x)` |
| `s%evaluate_linear(x)` | a second object built with `method="linear"`, or `pf_interp(xa, ya, x, method="linear")` |
| `s%evaluate(x, extrapolate=.true.)` | an object built with `outside="extrapolate"`, then `s%eval(x)` |
| a `real(rsp)` table or query | widened to `real64` at the call site |
| `s%is_initialised()` | `s%is_initialised()` |
| a finalizer releasing the table | nothing to call; `%clear` releases it early if you want |

What else differs: nothing is logged before an abort, and an infinite value in the table is refused
as a NaN is.
