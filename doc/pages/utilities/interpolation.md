---
title: Interpolation of tabulated data with parquet_interpolate
---

`parquet_interpolate` reads a value off a table of numbers at any point: build an interpolant from
a table of abscissae and ordinates once, then evaluate it wherever you need. It reaches no reader,
no writer and no setting: `use parquet_interpolate` compiles three Fortran files and nothing of the
Arrow stack. `use parquet` brings it in too, so nothing here needs a second import. See
[Choosing a module](../operating/choosing-a-module.html) for what each entry module costs.

Two methods: straight lines between neighbouring points, and a cubic spline — a smooth curve whose
slope and curvature are continuous across every point. The table is `real64`, ascending or
descending, and what a query beyond its ends answers is a choice you make once, when you build.

## Quick example

```fortran
program interpolation_example
    use parquet_interpolate
    use iso_fortran_env, only : real64
    implicit none

    type(pf_interp_1d) :: curve, line
    real(real64) :: x(6), y(6)
    integer :: i

    ! A table: y = x**2 at x = 1, 2, ..., 6.
    x = [(real(i, real64), i = 1, 6)]
    y = x*x

    call curve%init(x, y)                    ! a natural cubic spline, the default
    call line%init(x, y, method="linear")

    print *, curve%eval(2.5_real64)          ! 6.2237 -- not the parabola's 6.25; see below
    print *, line%eval(2.5_real64)           ! 6.5
    print *, curve%eval([1.5_real64, 5.5_real64])   ! 2.3421 and 30.3421, in one call
    print *, curve%eval(99.0_real64)         ! 36.0: beyond the table, the last ordinate

    ! One call, no object.
    print *, pf_interp(x, y, 2.5_real64, method="linear")   ! 6.5
end program interpolation_example
```

Signatures on this page show optional arguments in square brackets, with the comma outside the
bracket:

```fortran
call c%init(x, y, [method], [bc], [slopes], [outside], [is_valid], [context])
v  = c%eval(xq)
ok = c%is_initialised()
call c%clear()
yq = pf_interp(x, y, xq, [method], [bc], [slopes], [outside], [is_valid], [context])
```

## Building an interpolant

`call c%init(x, y)` copies the table, computes what the method needs, and leaves `c` ready to
evaluate. Your arrays are not modified, and the object keeps its own copy, so they may change or go
away afterwards.

The table has to be one that can be interpolated, and `%init` checks that it is:

- `x` and `y` are the same size;
- there are at least two points;
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

## Linear or a cubic spline

**`method="linear"`** joins neighbouring points with straight lines. It reproduces a straight line
exactly and never overshoots the data, which makes it the right choice for a coarse or noisy table,
where a curve through the points would invent structure between them.

**`method="cubic"`**, the default, is a cubic spline: one cubic per segment, joined so that the
value, the slope and the curvature are all continuous across every point. On smooth data it is far
more accurate than straight lines through the same table. On data that turn sharply between widely
or unevenly spaced points it can overshoot, where straight lines would not.

**The end condition, `bc="natural"`**, is the default and the one on offer: the spline's curvature
is zero at both ends of the table. It reproduces a straight line exactly, and it does **not**
reproduce a parabola — on `y = x**2` at `x = 1..6` it answers `6.2237` at `x = 2.5` rather than
`6.25`, because a parabola's curvature is not zero at the ends and the difference spreads inward
from them. Where that matters, tabulate beyond the range you care about.

The tokens `method="pchip"`, `bc="not_a_knot"` and `bc="clamped"` (with `slopes=`) are reserved:
`%init` recognises each and refuses it with a message.

## Evaluating

`c%eval(xq)` is `pure` and `elemental`: a scalar query answers a scalar, and an array of any shape
answers an array of the same shape, so a whole column is interpolated in one call. Being `pure`, it
may be called from your own `pure` procedures.

**A query on a point of the table answers that point's ordinate exactly**, under every method and
every policy, the last point included.

**Finding the segment** a query falls in is a bisection, except on an evenly spaced table, where it
is arithmetic and cheaper. `%init` detects an even table by itself; there is nothing to declare.
Both routes answer the same bits for every query, because the arithmetic answer is checked against
the table before it is used: a table that is only nearly even is never evaluated on the wrong
segment.

## Outside the table

`outside=` decides what a query beyond either end answers. It is fixed when the object is built:

| `outside=` | A query beyond the table answers |
|---|---|
| `"clamp"` (the default) | the ordinate at the nearer end |
| `"extrapolate"` | the end segment's line or cubic, continued |
| `"nan"` | a quiet NaN |

Clamping is the default because a cubic leaves the data fast. On the quick example's table,
extrapolating to `x = 99` answers about `-3.4e5` where the parabola is `9801` — the wrong sign, not
merely an inaccurate value. Extrapolate when you mean to, and only a little beyond the table.
`"nan"` is for a pipeline that filters its results afterwards: an out-of-range answer then cannot
pass for data.

**A NaN query answers NaN** under every policy, and raises no IEEE flag, so a column holding NaNs
can be interpolated as it stands.

**An infinite query is beyond the table in the direction of its sign.** `"clamp"` and `"nan"`
answer it as they answer any other such query. `"extrapolate"` answers whatever the end polynomial
gives at an infinity, which is an infinity or a NaN — and a NaN made that way raises IEEE_INVALID,
which ends a program built with that trap enabled, as nagfor builds are by default.

## One call, no object

```fortran
yq = pf_interp(x, y, xq, method="linear")
```

`pf_interp` builds an interpolant, evaluates it at `xq` — a scalar, or a rank-1 array — and
discards it. It takes the same optional arguments as `%init` and answers exactly what the object
answers, bit for bit, because that is how it computes; its abort messages begin `pf_interp:`
instead of `pf_interp_1d%init:`. The build is the expensive half, and this form repeats it on every
call, so a table queried more than a handful of times wants an object instead.

## Threads

Only `%init` and `%clear` write an object. `%eval` only reads one, so a built object may be
evaluated by every thread of a parallel region at once, and two objects never share anything.

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

Do not list a `pf_interp_1d` in a `private()` clause, where gfortran starts each thread's copy from
garbage rather than from an unbuilt object, and do not declare one in a `block` inside the region,
which ifx does not support for a type like this one. `pf_interp` called from the loop is safe as
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
| `method="pchip"` | `method "pchip" is not available yet` |
| `bc` with `method="linear"` | `bc applies only to method "cubic"` |
| an unknown `bc` | `unknown bc "<token>"; expected "natural", "not_a_knot" or "clamped"` |
| `bc="not_a_knot"` or `bc="clamped"` | `bc "<name>" is not available yet` |
| `slopes` | `slopes apply only to bc "clamped"` |
| an unknown `outside` | `unknown outside "<token>"; expected "clamp", "extrapolate" or "nan"` |
| fewer than two points left after `is_valid` | `at least 2 points are needed for method "<method>"; got <n>` |
| an `x` not strictly monotonic, or holding a NaN | `x must be strictly increasing or strictly decreasing` |
| an infinite `x` | `x must be finite` |
| a NaN or an infinite `y` | `y must be finite` |

Evaluating an object that was never built aborts with
`pf_interp_1d%eval: the interpolant is not initialised`. `%eval` is `pure`, so that message carries
no context. Nothing a query can be — a NaN, an infinity, a point far outside the table — is an
error.

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
