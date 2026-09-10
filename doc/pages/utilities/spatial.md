---
title: Spatial neighbour search with pf_spatial_index
---

`pf_spatial_index` answers "which points are near this one?" over plain coordinate arrays. It is a
uniform grid: the points are bucketed into cells once, and every later query walks only the cells
its search region can reach. Building is roughly one pass over the data, and a query costs
something close to the number of points it actually returns rather than the size of the catalogue.

Everything here is reachable from `use parquet`. The narrow import is `use parquet_spatial`, which
compiles fifteen Fortran files and never reaches this library's C++ bindings. That is a statement
about the *Fortran* graph and not about linking: `link` is a package-level key in `fpm.toml`, so
every import of this package still compiles the C++ wrapper and still links `-larrow`. See
[Choosing a module](../operating/choosing-a-module.html) for the per-module file counts and what the
guarantee does and does not cover.

Signatures below are written with optional arguments in **square brackets** —
`%build(x, y, [z,] radius=, [cell=])` means `z` and `cell=` may be omitted. Runnable examples never
use brackets.

## Building an index

`radius=` is mandatory, must be greater than zero, and is not a limit on what you may later ask
for — it is the hint the cell size is chosen from. Give the radius your queries will actually use,
or a list of them. Querying at a very different radius is still answered correctly, but a bulk
query re-tunes the index before sweeping and says so once; see
[Keeping an index current](#keeping-an-index-current).

```fortran
use parquet
type(pf_spatial_index) :: sx
real(real64) :: x(n), y(n), z(n)

call sx%build(x, y, z, radius=0.05_real64)              ! 3D
call sx%build(x, y, radius=0.05_real64)                 ! 2D: omit z
call sx%build(x, y, z, radius=[0.01_real64, 0.05_real64, 0.2_real64])
call sx%build(x, y, z, radius=0.3_real64, observer=o, los=zred)   ! for the line-of-sight queries
```

A list is folded into one effective radius, `sum(r**3)/sum(r**2)`, which is the value the cost
model wants — the cubic term is the points a query tests and the quadratic one is how often. Read
it back with `%effective_radius()`. `observer=` and `los=` serve
[cylinders along the line of sight](#cylinders-along-the-line-of-sight) and nothing else.

**The cell size is measured, not guessed.** A cost model gives a starting bracket and a
deterministic probe then counts, for a handful of candidate cell sizes, how many cells a
representative query would visit and how many points it would distance-test. The candidate with the
least predicted work wins. The probe costs a fraction of the build it is part of, so it always
runs.

Pass `cell=` to override it entirely — the probe is then skipped and your value is used, subject to
one clamp described under [limitations](#limitations).

### Every coordinate, radius and query point must be finite

**A NaN or an infinity is refused, wherever it enters**, with a message naming the entry point and
the argument — `%build`, `%build_sky` (`ra`; `dec` is already covered by its `[-90, 90]` test) and
`%rebuild` check the coordinates, `%rebuild_for` and every bulk query check the radii, and
`%within`, `%within_segment`, `%within_cylinder`, `%within_cone`, `%nearest`, `%within_sky` and
`%nearest_sky` check the point they are given.

This is a refusal rather than a quiet answer because there is nothing to answer: a cell index is
`int((v - lo) * inv)`, and neither that conversion nor the `min`/`max` that clamp a walk to the
grid has a meaning for a NaN. Under a compiler running with the IEEE traps unmasked — nagfor's
default — reaching either one terminates the process with a bare arithmetic exception instead;
under the rest, the search silently answers from a garbage cell.

### Borrowing the caller's arrays

By default the index copies the coordinates, so you may do anything you like with the originals
afterwards. `copy=.false.` stores pointers instead:

```fortran
real(real64), allocatable, target :: x(:), y(:), z(:)
call sx%build(x, y, z, radius=0.05_real64, copy=.false.)
```

Three conditions come with it, all of them real:

- The arrays must have the `TARGET` attribute and must outlive the index.
- They must be **contiguous**. A strided section is refused at build time rather than diagnosed
  later, because nothing later can diagnose it.
- You must not change their contents while the index is in use — the index holds cell assignments
  derived from the values it saw.

`%rebuild` is refused on such an index, since there is no stored copy for it to compare against.

### Metadata

```fortran
n = sx%size()                     ! how many points
h = sx%cell_size()                ! the cell side chosen, ready to pass back as cell=
c = sx%cells()                    ! how many cells the grid has
d = sx%ndim()                     ! 2 or 3
ok = sx%is_built()
ok = sx%is_periodic()
mt = sx%metric()                  ! PF_METRIC_EUCLIDEAN or PF_METRIC_SKY
r = sx%effective_radius()
call sx%grid(nx, ny, nz)          ! cells along each axis
call sx%cell_sides(cx, cy, cz)    ! the cell side per axis, as the grid uses it
call sx%clear()                   ! release everything
```

## Ball search

`%within` fills a buffer you own and returns the **true** count, even when that exceeds the
buffer — so you can size and retry rather than guess.

```fortran
integer(int64) :: found(64)
real(real64) :: d(64)
integer(int64) :: m

m = sx%within([0.5_real64, 0.5_real64, 0.5_real64], 0.05_real64, found)
m = sx%within(p, 0.05_real64, found, dist=d)     ! distances too
m = sx%count_within(p, 0.05_real64)              ! no buffer at all
```

`%count_within` is the form to reach for when you want a local density and not the rows themselves;
on the sky it is `%count_within_sky(ra, dec, radius_deg, [r_inner_deg])`. Both take `r_inner=` /
`r_inner_deg=` and answer exactly what the buffer-filling form would have returned.

The query point is a 2- or 3-element array and must match the rank the index was built with. The
output buffer may be `integer(int32)` or `integer(int64)`; the indices are rows of the arrays you
built from, in the caller's own order, whether or not the index copied them.

Optional arguments are shown in square brackets below; they are ordinary Fortran optionals, not
literal syntax.

### An annulus, with `r_inner=`

`r_inner=` puts a hole in the ball, so a point qualifies when `r_inner <= d <= r`:

```fortran
m = sx%within(p, 0.05_real64, found, r_inner=0.02_real64)
m = sx%count_within(p, 0.05_real64, r_inner=0.02_real64)
```

**Both bounds are inclusive.** A point sitting exactly on the inner surface therefore belongs to
the annulus and to the inner ball alike, so "annulus = outer ball minus inner ball" holds
everywhere except on that surface itself. `r_inner = 0` is exactly the plain ball, and an
`r_inner` larger than the outer radius is refused rather than returning nothing.

It is available on every form that takes a radius — the bulk sweeps, and the sky queries as
`r_inner_deg=`. The one restriction is on `%pairs_within` and `%pairs_within_sky`, where it must be
a **scalar** even when the outer radius is a vector; see [Limitations](#limitations).

### Ordered results, with `sorted=`

`sorted=.true.` returns the rows by increasing distance, with **ties broken by ascending row
index** so that the order is the same on every machine:

```fortran
m = sx%within(p, 0.05_real64, found, dist=d, sorted=.true.)
call sx%all_within(0.05_real64, offsets, neighbours, sorted=.true.)   ! orders within each row
```

Two things to know. It needs the distances even when `dist=` is absent, so asking for it without
wanting them still pays for them. And a buffer shorter than `m` keeps the rows the walk found
*first*, and `sorted=` orders only those — it does not make a short buffer hold the **nearest**
rows. [`%nearest`](#the-k-nearest-neighbours) is the query that does that.

## Every point's neighbours at once

Three bulk forms sweep the whole catalogue, allocate their output once, and thread internally.

```fortran
integer(int64), allocatable :: offsets(:), neighbours(:), i(:), j(:), counts(:)

call sx%all_within(0.05_real64, offsets, neighbours)    ! CSR: row k is neighbours(offsets(k):offsets(k+1)-1)
call sx%pairs_within(0.05_real64, i, j)                 ! edge list: each pair once, i < j
call sx%count_all_within(0.05_real64, counts)           ! counts only, length n
```

Each takes **either one radius or one per point**, the latter in the caller's row order — so a
column read off a `parquet_table` can be passed straight in:

```fortran
call sx%all_within(link_length, offsets, neighbours)    ! link_length(n): one per point
```

`%all_within` reports a point as its own neighbour; `%pairs_within` does not, since a point is not
a pair with itself. (A positive `r_inner=` excludes the point from its own list too — its distance
to itself is zero, which is below any inner radius.)

All three take `r_inner=` and the two list-producing ones take `sorted=`:

```fortran
call sx%all_within(0.05_real64, offsets, neighbours, r_inner=0.02_real64, sorted=.true.)
call sx%count_all_within(0.05_real64, counts, r_inner=0.02_real64)
call sx%pairs_within(0.05_real64, i, j, r_inner=0.02_real64)
```

The per-point pair forms take one further argument, `combine=`, which chooses how two different
radii decide a pair — see [choosing the rule](#choosing-the-rule-with-combine) below. It composes
with `r_inner=`: a pair qualifies when it is at least the inner radius away and satisfies the rule.

On a sky index these three are spelled `%all_within_sky`, `%pairs_within_sky` and
`%count_all_within_sky`, and take their radii in degrees — see
[search on the sky](#search-on-the-sky).

### Directed and symmetric are different questions

With a single radius the distinction does not arise. With **one radius per point** it does, and the
three forms answer differently on purpose:

**`%all_within` and `%count_all_within` are directed.** Row `k`'s list holds the points within
`radius(k)` of it. If `k` has a large radius and `q` a small one, `q` can appear in `k`'s list
without `k` appearing in `q`'s. That asymmetry is what a per-row search radius *means* — a linking
length that varies per object is a statement about that object.

**`%pairs_within` is symmetric.** A pair qualifies when **either** ball reaches the other,
`d(i,j) <= max(radius(i), radius(j))`, so it does not matter which endpoint would have been doing
the searching. Each unordered pair appears exactly once, always with `i < j`.

**So `sum(counts)` does not size a `%pairs_within` result** when the radii vary. For a single
radius the identity `size(i) == (sum(counts) - n) / 2` holds, and it is easy to carry that
assumption across without noticing.

### Choosing the rule, with `combine=`

`d <= max(r_i, r_j)` is one of four rules `%pairs_within` and `%pairs_within_sky` offer, and it is
the default. Name another with `combine=`:

```fortran
call sx%pairs_within(link_length, i, j, combine=PF_LINK_MEAN)
call sky%pairs_within_sky(link_deg, i, j, combine=PF_LINK_MIN)
```

| `combine=` | a pair qualifies when | in words |
|---|---|---|
| `PF_LINK_MAX` (default) | `d <= max(r_i, r_j)` | either ball reaches the other point |
| `PF_LINK_MIN` | `d <= min(r_i, r_j)` | both balls reach the other point |
| `PF_LINK_MEAN` | `d <= (r_i + r_j)/2` | the arithmetic mean of the two lengths reaches the other point |
| `PF_LINK_SUM` | `d <= r_i + r_j` | the two balls touch or overlap |

The argument is offered on the **per-point** forms only. With one radius for every point the first
three are the same rule, and the fourth is the same call at twice the radius.

`PF_LINK_MIN` and `PF_LINK_MEAN` accept subsets of the default, so either could also be had by
filtering that result. **`PF_LINK_SUM` could not** — it is wider than the default, so the pairs it
adds are not in that list to be filtered out of. It is also the one rule that sweeps further than
the radii you gave it: the searcher has to reach a partner up to `r_i + r_j` away, so it walks
`2*r_i`, which in three dimensions is eight times the volume. `bench/benchmark_spatial.sh` with
`MODE=combine` times all four on your own data; `PF_LINK_MIN` costs least and `PF_LINK_SUM` costs
several times the default.

Two consequences of that wider walk are worth knowing. The index is re-tuned against the doubled
radius, so `%effective_radius()` reports it afterwards. And on a sky index every angular radius
must be at most 45 degrees under this rule, so that the doubled walk stays inside the 90-degree
ceiling every sky query has.

## Segment, cylinder and cone

Three shapes around an axis, sharing one cell walk and differing only at the ends.

```fortran
m = sx%within_segment(p1, p2, r, found)              ! capsule: distance to the segment <= r
m = sx%within_cylinder(p1, p2, r, found)             ! flat ends
m = sx%within_cone(p1, p2, r1, r2, found)            ! truncated cone; r1 == r2 is the cylinder
```

| shape | a point qualifies when | ends |
|---|---|---|
| `%within_segment` | its distance to the **segment** is at most `r` | round |
| `%within_cylinder` | it lies between the end planes **and** within `r` of the axis | flat |
| `%within_cone` | it lies between the end planes **and** within `r1 + t*(r2-r1)` of the axis | flat |

with `t` the point's position along the axis, 0 at `p1` and 1 at `p2`. **A point just beyond an end
is inside the capsule and outside the cylinder** — that is the whole difference between the first
two. `dist=` reports the distance to the segment for `%within_segment` and the perpendicular
distance to the axis for the other two.

The cone is the general routine and the other two are it, restricted. It is also the better-
motivated shape for a search outward from an observer: a fixed angular aperture subtends a
transverse extent that grows linearly with distance, so what such a selection sweeps out is a cone
rather than a cylinder.

A zero-length axis (`p1 == p2`) reduces to a ball — of radius `r`, or of `max(r1, r2)` for the
cone. That is the documented answer rather than an error.

### Where on the axis each point sits

All three take two further outputs, and `sorted=`:

```fortran
real(real64) :: foot(3, 64), along(64)

m = sx%within_segment(p1, p2, r, found, dist=d, axis_point=foot, axis_t=along, sorted=.true.)
```

- **`axis_point`** is shaped `(ndim, m)` — `(1:3, :)` on a 3D index, `(1:2, :)` on a 2D one — and
  column `k` is **the point `dist(k)` was measured from**. For the capsule that is the closest
  point on the *segment*, not the projection onto the infinite line: a point beyond `p2` has its
  distance measured from `p2` itself, and `p2` is what comes back. For the cylinder and the cone
  the distinction never arises, since both reject anything falling outside the ends.
- **`axis_t`** is where that point sits along the axis, **normalised to `[0, 1]`** — 0 at `p1`, 1
  at `p2`, and never outside, so a point before `p1` reports exactly 0. Multiply by
  `norm(p2 - p1)` for a length in the index's own coordinate units.

The two are redundant by construction — `axis_point == p1 + axis_t*(p2 - p1)` — and both are
offered because a caller usually wants one or the other and never wants to reconstruct it. A
zero-length axis reports `p1` and 0 for every point.

Every output buffer, `axis_point`'s second extent and `axis_t`'s length included, truncates at the
same place: the shortest one you pass. (The one exception is a zero-length axis, where the shape is
a ball and every foot is `p1`, so each buffer is simply filled to its own extent.) `m` is still the
true count, so this is a size-and-retry rather than an error.

## Cylinders along the line of sight

A redshift-survey group finder links two galaxies when they are close **across** the line of sight
and close **along** it, with two different lengths, because the two separations mean different
things: the transverse one is a distance, the parallel one is mostly a velocity. `%pairs_within_los`
and `%within_los` answer that criterion inside the index, on a 3D Euclidean, non-periodic index
built over Cartesian coordinates about an observer:

```fortran
call sx%build(x, y, z, radius=r_hint, observer=o, los=zred)
call sx%pairs_within_los(b_perp, b_par, i, j, combine=PF_LINK_MEAN, dperp=dperp, dpar=dpar)
m = sx%within_los(p, b_perp, b_par, found, los_p=z_p, dist=dn, sorted=.true.)
```

The full signatures are `%build(x, y, z, radius=, [cell=,] [box_lo=,] [box_hi=,] [copy=,]
[observer=,] [los=])`, `%pairs_within_los(b_perp, b_par, i, j, [combine=,] [threads=,] [dperp=,]
[dpar=])` and `%within_los(p, b_perp, b_par, found, [los_p=,] [dist=,] [dperp=,] [dpar=,]
[sorted=])`.

With `D` the distance from the observer and `n` the unit vector towards a point, a pair `i, j`
qualifies when both of

```
d_perp = |n_i - n_j| * (D_i + D_j) / 2  <=  b_perp
d_par  = |los_i - los_j|                <=  b_par
```

hold. `d_perp` is the chord between the two directions at the pair's mean distance. `d_par` is the
difference of a **parallel coordinate**, which is `los=` when `%build` was given one and `D` itself
otherwise.

**The two lengths are in two units, and the units need not agree.** `b_perp` and `dperp=` are in the
coordinates' units. `b_par` and `dpar=` are in `los=`'s units, whatever those are: `los=` is any
per-point value in row order, most usefully the redshift itself over comoving coordinates, so that
a parallel linking length is a redshift interval — `v / c` for a velocity `v` — while the
transverse one stays a comoving length. Without `los=` both are in the coordinates' units. `los=` is
always copied, whatever `copy=` says. `%rebuild` then takes `los=` with the new values and is
refused without it, and is refused with it on an index built without one. `observer=` is the
observer's position, default the origin, and is fixed at `%build` too.

**`b_perp` is a radius and `b_par` a half-length**, both separations measured from the point, so the
region a point's partners can occupy is `2 * b_perp` across and `2 * b_par` long — exactly as
`radius=` bounds a ball. Both take one value or one per point, in row order.

`combine=` chooses how two points' lengths decide a pair, with the four rules of
[choosing the rule](#choosing-the-rule-with-combine) read on a cylinder:

| `combine=` | a pair qualifies when |
|---|---|
| `PF_LINK_MAX` (default) | it lies in `i`'s cylinder **or** in `j`'s: the union of the two |
| `PF_LINK_MIN` | it lies in both cylinders |
| `PF_LINK_MEAN` | `d_perp <= (b_perp_i + b_perp_j)/2` and `d_par <= (b_par_i + b_par_j)/2` |
| `PF_LINK_SUM` | `d_perp <= b_perp_i + b_perp_j` and `d_par <= b_par_i + b_par_j` |

The default is the **union**, not the componentwise maximum of the lengths: when two points' aspect
ratios differ, a pair can be within the larger transverse length and within the larger parallel
length while lying in neither point's cylinder, and it is not reported. `dperp=` and `dpar=` return
the two separations per pair, in their two units. The scalar form takes no `combine=`, for the
reason `%pairs_within`'s does not.

`%within_los` is the same cylinder about one point `p`, with `p`'s lengths only. `los_p=` is `p`'s
own parallel coordinate, required exactly when the index carries `los=`. Its `dist=` is the
**normalised** measure `max(d_perp / b_perp, d_par / b_par)` — 1 on the cylinder's surface, below 1
inside — and `sorted=.true.` orders by it, ties by ascending row index, so the point nearest in the
cylinder's own sense of near comes first. Both lengths must be positive here, since the measure
divides by them. Every buffer truncates at the shortest one passed, and `m` is still the true
count.

**How the library finds the candidates.** About each point the walk is the cylinder itself: the
cells along that point's own line of sight, `2 * b_perp` across, and in distance from the observer
exactly the range the stored points within `b_par` of its `los` occupy — so a survey's far end,
where a redshift interval spans less distance than at its near end, walks a shorter cylinder.
Without `los=` that range is `b_par` either side. The walked cylinder is padded by the little a
partner's own line of sight can carry it outside the point's — a factor `2 D_far / (D + D_far)` on
the radius and `b_perp**2 / (2 D)` at the near end, both computed per point — so no pair is missed,
and the exact test above then keeps the cylinder and discards the rest. A point so close to the
observer that its padded cylinder would be longer than its covering ball is wide walks that ball
instead. The cylinder walk tests a small multiple of the pairs it keeps where a ball walk tests
hundreds to over a thousand times as many; on a sparse survey the cell the cells-per-point cap
allows is far wider than the cylinder, so the time saved is a fraction of that, and
`bench/benchmark_spatial.sh` with `MODE=los` measures both on your own data. **Give `%build` `radius = b_perp`**: the cell follows the
cylinder's cross-section. `L`, how fast the distance from the observer changes with `los`, is still
measured from the data at `%build` — the steepest slope over pairs of points at least a
hundred-thousandth of `los`'s range apart, with the spread `g` of pairs closer than that — and is
what the two warnings below read. Without `los=`, `L` is exactly one and `g` zero.

Two things are said rather than refused. A `los=` that is not a function of the distance from the
observer, or is noisy at small separations — a spectroscopic redshift as `los=` over coordinates
placed by a photometric one, say — makes `L` far above the catalogue-wide slope, and `%build` warns
that every line-of-sight walk will be that wide. A `b_par` whose walk would span the catalogue's
whole depth is what a parallel length given in the coordinates' units against a redshift `los=`
looks like, and `%pairs_within_los` warns. A constant `los=`, a NaN in it, a point sitting on the
observer, and a 2D, periodic or sky index are refused; see [Limitations](#limitations).

### A redshift survey: comoving coordinates, the redshift as `los=`

Galaxies with `ra` and `dec` in radians and a redshift `zred`, placed at their comoving distance by
the caller's own cosmology. The library never sees the cosmology, only three coordinate arrays and
one `los=` array:

```fortran
use parquet_spatial, only: pf_spatial_index, PF_LINK_MEAN
real(real64), allocatable :: x(:), y(:), z(:), zred(:), b_perp(:), b_par(:), dperp(:), dpar(:)
integer(int64), allocatable :: i(:), j(:)
type(pf_spatial_index) :: sx

do k = 1, n
    d = comoving_distance(zred(k))                   ! the caller's cosmology, Mpc/h
    x(k) = d * cos(dec(k)) * cos(ra(k))
    y(k) = d * cos(dec(k)) * sin(ra(k))
    z(k) = d * sin(dec(k))
end do
b_perp = 0.3_real64                                  ! Mpc/h: the coordinates' units
b_par = 1000.0_real64 / 299792.458_real64            ! 1000 km/s as a redshift interval

call sx%build(x, y, z, radius=b_perp, los=zred)      ! observer at the origin
call sx%pairs_within_los(b_perp, b_par, i, j, combine=PF_LINK_MEAN, dperp=dperp, dpar=dpar)
```

`dperp` comes back in Mpc/h and `dpar` in redshift, so `dpar * c` is each pair's velocity difference
in km/s. The slope the library measured is the survey's `dD/dz` at its nearest redshift; that one
number is the whole conversion between the two units, and a `b_par` given in Mpc/h by mistake is
what the second warning above catches. The same cylinder about one position, nearest first in the
cylinder's own measure, is

```fortran
m = sx%within_los(p, 0.3_real64, b_par(1), found, los_p=z_p, dist=dn, sorted=.true.)
```

with `z_p` the redshift of `p`.

## Search on the sky

A sky index stores `(ra, dec)` in degrees as unit vectors and answers by angular radius.

```fortran
type(pf_spatial_index) :: sky
real(real64) :: ra(n), dec(n), sep(64)

call sky%build_sky(ra, dec, radius_deg=1.0_real64)
m = sky%within_sky(120.5_real64, -35.0_real64, 1.0_real64, found)
m = sky%within_sky(120.5_real64, -35.0_real64, 1.0_real64, found, dist_deg=sep)
```

**Angles in, angles out.** `dist_deg` is an angular separation in degrees; you never see the chord
the index works in internally. The conversion is exact rather than approximate — an angle maps to
the chord `2*sin(theta/2)`, which is strictly increasing, so a Euclidean ball of that radius in
unit-vector space selects exactly the points within that angle. There is no pole special case and
no wrap at 0h, because a sphere has neither.

**A zero radius is a knife edge, and not the way to find one catalogue entry.** The query point is
not compared as `(ra, dec)`: it is converted to a unit vector by the same expression `%build_sky`
applied to the catalogue. Two evaluations of that expression are not required to agree to the last
bit, and on some compilers they do not — measured on ifx 2026.1, in a bulk loop versus as a scalar
they differ by 1–2 ulp under `-O0`, which leaves a catalogue point about `1e-14` degrees away from
its own `(ra, dec)`. So `%within_sky(ra(k), dec(k), 0.0)` may find nothing, and whether it does
varies with the compiler and the optimisation level. Nothing is lost by asking for a radius instead:
`1e-9` degrees is 3.6 microarcseconds, far below any radius an observation implies and five orders
of magnitude above that error. The Euclidean `%within` has no such caveat — it compares the stored
coordinates themselves, so `r = 0` there does find the point.

### Sky queries in bulk

The same three self-join forms, with radii in degrees:

```fortran
call sky%all_within_sky(1.0_real64, offsets, neighbours)     ! CSR: every object's neighbours
call sky%pairs_within_sky(1.0_real64, i, j)                  ! every close pair once, i < j
call sky%count_all_within_sky(1.0_real64, counts)            ! counts only
```

Each takes one angular radius or one per point, threads with `threads=`, and behaves exactly as its
Euclidean twin — including the [directed and symmetric](#directed-and-symmetric-are-different-questions)
split, which survives the change of units unchanged: the chord is strictly increasing in the angle,
so `max(chord_i, chord_j)` is the chord of `max(deg_i, deg_j)` and a pair qualifies when the wider
of the two apertures reaches the other object.

`combine=` works here too, and **its rules are stated on the angles, in degrees** — `sep <=
(deg_i + deg_j)/2` for `PF_LINK_MEAN`, not the mean of the two chords the index works in
internally. For `PF_LINK_MAX` and `PF_LINK_MIN` the distinction is empty, since the larger angle is
the larger chord. For the other two it is not: the chord is concave in the angle, so a rule applied
to chords would answer a slightly different question, and the difference grows with the radius.

What each is for, since the three answer quite different questions: `%all_within_sky` is the
catalogue self-match; `%pairs_within_sky` is what a group finder or a duplicate-source search
wants; and `%count_all_within_sky` gives a local surface density per object without materialising
the neighbour lists, which on a crowded field is the difference between a length-n array and one
that does not fit in memory.

**The metric belongs to the index, and mixing them aborts.** `%within` on a sky index and
`%within_sky` on a Euclidean one are both refused, rather than quietly answering in the wrong
units — as are `%all_within` on a sky index and `%all_within_sky` on a Euclidean one, in both
directions. That guard is the reason the metric is stored at build time instead of being passed
per call: `%all_within(0.02)` on a sky index would otherwise be answered in chords to someone who
meant degrees, and 0.02 chords is about 1.15 degrees — a plausible number, wrong by a factor of 57.

Two further notes. **Every entry point on a sky index takes and returns degrees, without
exception** — `%rebuild_for` included, and it is subject to the same 90-degree ceiling as
`%build_sky`. So `%effective_radius()` and `%rebuild_for` are a matched pair: a radius read from
one can be handed straight back to the other. The only values in unit-vector space are
`%cell_size()` and `cell=`, which are a matched pair of their own for the same reason.

And `%build_sky` has no `copy=`: the stored coordinates are unit vectors it computes, so there is
nothing of yours to borrow.

### Choosing a backend

A sky index can be backed by either of two spatial structures, chosen when you build it:

```fortran
call sky%build_sky(ra, dec, radius_deg=1.0_real64)                         ! the 3D grid, the default
call sky%build_sky(ra, dec, radius_deg=1.0_real64, backend=PF_SKY_HEALPIX) ! the HEALPix pixelisation
```

Everything after that is unchanged. All six sky operations — `%within_sky`, `%all_within_sky`,
`%pairs_within_sky`, `%count_all_within_sky`, `%nearest_sky` and `%kth_distance_sky` — keep their
signatures, their units and their results. **A backend decides only how the candidate set is
narrowed, never what the answer is**, and the two are required to agree element for element under
`sorted=.true.`, which is what the test suite asserts.

**What the choice trades.** `PF_SKY_GRID3D` buckets the unit vectors in a cube. A sphere occupies
a zero-thickness shell of that cube, so most of the grid's cells can never hold a point — and
because the number of cells is capped (see [Limitations](#limitations)), the cell cannot shrink to
match a small query radius. `PF_SKY_HEALPIX` partitions the sphere itself, so every pixel is
occupied and the same cap buys far more usable resolution. It tests **substantially fewer points
per hit** as a result, at every radius.

Fewer candidates is not automatically less time. A HEALPix query enumerates the pixels a disc
touches, which costs a fixed amount per query that the grid's integer cell arithmetic does not pay
— so the pixelisation gives its candidate saving back at small radii and keeps it at large ones.

**How clustered the field is matters more than anything else here**, and it is the one part of the
choice worth reasoning about rather than measuring. A cell of the 3D grid that happens to contain a
cluster is tested in full, however little of it the query disc reaches; a disc's pixels can exclude
most of that same cluster. So on a **clustered** catalogue — which most real sky data is — the
pixelisation wins at and above the radius the index was tuned for, by a comfortable margin, and on
a **uniform** one the grid tends to stay ahead at every radius. That is a direction, not a
guarantee: **the crossover also depends on the machine, the radius and the catalogue size**, which
is why no number for it appears here. Measure your own with

```bash
MODE=backend bench/benchmark_spatial.sh
MODE=backend DIST=clustered bench/benchmark_spatial.sh
```

which builds one catalogue both ways, checks the two answers agree, and prints the ratio across a
radius sweep. Run both fixtures: the clustered one is the one that resembles a real field.

**`nside=` is the HEALPix counterpart of `cell=`.** It forces the resolution and skips the tuner,
must be a power of two, and is still subject to the pixels-per-point cap. `cell=` is refused on a
HEALPix index and `nside=` on a 3D-grid one, rather than either being quietly ignored.

**Reporting an index's shape.** `%backend()` answers `PF_SKY_GRID3D` or `PF_SKY_HEALPIX`;
`%nside()` and `%npix()` answer the HEALPix resolution and pixel count, or zero on a grid index.
`%cells()` answers the bucket count on both — cells on one, pixels on the other — while
`%cell_size()`, `%cell_sides()` and `%grid()` answer zero on a HEALPix index, because they describe
a grid it does not have and `%cell_size()`'s contract is that it can be passed back as `cell=`.

**One visible difference, and it is in the ordering.** Without `sorted=`, results come back in the
order the walk produced them — cell order on the grid, pixel order under HEALPix. These are
different orders over the same set. That order was never a contract on either backend (it depends
on a tuned cell size, and so on the machine), but it is worth knowing before switching a program
between the two: **the two backends return the same set, and the same order only when you ask for
`sorted=.true.`** The same applies to `%pairs_within_sky`, whose pair list is emitted in walk
order; the pairs are the same pairs, each still exactly once with `i < j`.

Finally, `%rebuild_for` re-tunes whichever backend the index has — a cell side for one, a HEALPix
resolution for the other — and never changes which one that is. Call `%build_sky` again to switch.

## The k nearest neighbours

`%nearest` answers the other question a spatial index is asked: not "what is within `r`" but "which
`k` are closest".

```fortran
integer(int64) :: found(20)
real(real64) :: d(20)

m = sx%nearest(p, 20, found)                 ! nearest first, always
m = sx%nearest(p, 20, found, dist=d)
m = sky%nearest_sky(120.5_real64, -35.0_real64, 20, found, dist_deg=sep)
```

Results are **always ordered by increasing distance**, ties by ascending row index. `m` is
`min(k, %size())`, so asking for more neighbours than the catalogue holds returns them all rather
than failing, and a buffer shorter than `m` gets the *nearest* `m` — the ordering happens before
the copy, which is the difference from `%within(sorted=.true.)`.

The result is exact. A ball grows until it holds at least `k` points, after which the `k` nearest
are exactly the `k` smallest of what that ball returned — every point closer than the `k`-th is
inside the ball by construction. The starting radius comes from the density the index measured when
it was built, and each miss rescales from the count it actually saw, so on ordinary data the
expansion converges in one or two rounds.

The query point need not be one of the catalogue's own: a point coincident with a stored row simply
finds it at distance zero.

### Every point's k-th neighbour distance

```fortran
real(real64), allocatable :: dk(:)

call sx%kth_distance(5, dk)               ! length n, in the caller's row order
call sky%kth_distance_sky(5, dk)          ! in degrees
```

For each point, the distance to its `k`-th nearest **other** point — self excluded, which is the
whole reason the call exists: an adaptive-kernel density estimator wants the `k`-th neighbour, and
counting the point itself shifts every bandwidth by one rank. `k` must be at most `%size() - 1`,
checked up front rather than reported per row.

This is cheaper than the loop of `%nearest` you would otherwise write, because the sweep runs in
the index's own stored order: consecutive points are spatially adjacent, so the radius that
converged for one seeds the next. That changes only how many rounds the expansion takes and never
an answer. It threads like the other bulk forms.

## Connected components, and Friends-of-Friends

`pf_connected_components` labels the connected components of an undirected graph given as an edge
list — which is exactly what `%pairs_within` returns, so a **Friends-of-Friends group finder is
these two calls**:

```fortran
integer(int64), allocatable :: i(:), j(:), labels(:), sizes(:)
integer(int64) :: ncomp

call sx%pairs_within(link_length, i, j)
call pf_connected_components(i, j, sx%size(), labels, ncomp=ncomp, sizes=sizes, min_size=2)
```

With a linking length that varies per object, `combine=` decides what "linked" means for two
objects carrying different lengths — the default links them when either length reaches, and
[choosing the rule](#choosing-the-rule-with-combine) sets out the four:

```fortran
call sx%pairs_within(link_length, i, j, combine=PF_LINK_MEAN)
```

- `i`, `j` are the edge list. Neither direction nor `i < j` is required, and duplicate edges and
  self-loops are harmless. Every endpoint must name a vertex in `1..nvert`, though — one outside
  that range is an error rather than an ignored edge, since it almost always means `nvert` and the
  edge list came from different catalogues.
- **`nvert` is required and must not be derived from the edge list.** An isolated vertex never
  appears in an edge list, so taking the vertex count from `maxval` would silently drop every
  trailing isolated point and return a `labels` array shorter than the catalogue.
- `labels` comes back length `nvert`. A vertex in a qualifying component gets a label in
  `1..ncomp`; **everything else gets 0**.
- `min_size` (default 1) is the smallest component that earns a label. The default is the textbook
  reading, in which a singleton *is* a connected component: every vertex is labelled, and
  `sum(sizes)` is `nvert`.
- `sizes` is length `ncomp`, in label order.

**That is why the example above passes `min_size = 2`.** In a group catalogue a galaxy with no
neighbours is not a group of one, it is a field galaxy — so a group finder asks for the threshold,
and `labels > 0` is then the mask that selects group members while `ncomp` is the number of groups
anyone would quote. It is the caller who says so, because which components count is a domain
question rather than a graph one.

**Labels are assigned by ascending vertex index of first appearance**, so the qualifying component
containing the lowest-numbered vertex is 1, and so on. That is a contract rather than an
implementation detail: without it the numbering would fall out of the union-find's internal choices
and the same catalogue could come back numbered differently on another compiler.

It is a module procedure rather than a type-bound one, and it has no spatial content at all — a
union-find over an edge list is pure graph work. It lives here so that a group finder needs one
`use` rather than two, and because this module is a much smaller import than anywhere else it could
sensibly go.

## Periodic boundaries

Give both `box_lo=` and `box_hi=` and the index wraps at the box faces, using the minimum-image
convention:

```fortran
call sx%build(x, y, z, radius=0.05_real64, &
              box_lo=[0.0_real64, 0.0_real64, 0.0_real64], &
              box_hi=[l, l, l])
```

The grid is tiled to fit the box exactly, so the cell side is adjusted slightly from whatever was
chosen or asked for. Points are not moved: a coordinate outside the box is mapped into it for
cell assignment only, and comes back to you as the row it always was.

**The search radius may not exceed half the box on any axis.** Beyond that the minimum image is
ambiguous — a point can be its own neighbour through two images — so the answer does not exist
rather than being inaccurate, and the build refuses.

## Two dimensions

Omit `z` and everything above works unchanged, with query points given as two coordinates. The
index is a 3D grid one cell deep, which costs a small constant factor against a purpose-built 2D
grid and saves the library a second implementation of every query.

## Keeping an index current

The index holds cell assignments derived from the coordinates it saw, so it is stale the moment
those change. Two calls address that.

```fortran
call sx%rebuild(x, y, z, rebuilt=changed)     ! validate, and rebuild only if the data moved
call sx%rebuild_for(0.2_real64)               ! re-tune the cell size, same points
```

**`%rebuild` is cheap to call in a loop.** It compares the stored copy against what you pass,
element by element, and returns without doing anything when they are identical; the optional
`rebuilt=` tells you which happened. The comparison is exact and never a checksum, because a
collision would mean silently answering about the old positions.

**`%rebuild` is Euclidean-only.** A sky index stores the unit vectors it computed rather than the
`(ra, dec)` you passed, so there is nothing for it to compare your arrays against; calling
`%rebuild` on one is refused, and the way to rebuild it is to call `%build_sky` again.
`%rebuild_for`, which re-tunes without looking at coordinates at all, works on both.

A bulk query also **re-tunes itself** when the radius it has been given would choose a very
different cell from the one the index was built with, and says so once per index. Silence it with
`parquet_set_spatial_rebuild_warning(.false.)`.

## Threading and settings

Every bulk form threads internally. Single queries do not, and **neither does building**: `%build`
and `%build_sky` take no `threads=` at all, because the bucketing sort behind them reaches its grid
through a grouped path that never opens a team. Do not size a build around threading.

```fortran
call sx%all_within(0.05_real64, offsets, neighbours, threads=8)
```

Omitting `threads=` resolves automatically: the process thread count, clamped to the number of
processors actually available, and serial when the call is already inside a parallel region.
`parquet_set_spatial_threads(n)` lowers that automatic answer process-wide — but an explicit
`threads=` on the call itself still wins, so the setting is a default rather than a ceiling. See
[Settings](../operating/settings.html).

An index is safe to **share across threads for single queries** once built — `%within`,
`%count_within`, `%nearest` and their kin take it as read-only and never re-tune it.

**A bulk query is not read-only, and that is the one easy mistake here.** It may re-tune the index
before it sweeps (see [Keeping an index current](#keeping-an-index-current) above), so two threads
calling `%all_within`, `%pairs_within` or `%count_all_within` on the *same* index at once can both
decide to rebuild and race on the same arrays — a corrupted heap rather than a wrong number, and
nothing detects it. Call a bulk form from one thread at a time; it threads internally anyway, which
is where its parallelism is meant to come from. Building, `%rebuild` and `%rebuild_for` mutate the
index for the same reason and must likewise not run concurrently with anything else on it.

`bench/benchmark_spatial.sh` measures build, single-query and bulk throughput, the tuner's accuracy
against a swept optimum, and thread scaling, if you want numbers for your own machine and data.

## Limitations

- **An axis-shaped query is refused on a periodic index.** Under the minimum image an axis longer
  than the box wraps onto itself, so a point can be near the shape through more than one image and
  the ball search's half-box guard has no equivalent. Only `%within` and the bulk forms are
  periodic-aware.
- **There is no cross-match against a second catalogue.** Every bulk form here is a *self*-join:
  it queries the index with the index's own points. Matching catalogue B against an index built
  over catalogue A means a loop of `%within_sky`, which works and threads if you write the loop,
  but has no single call.
- **An angular radius above 90 degrees is refused.** The chord mapping stays exact all the way to
  180, but past a hemisphere the ball covers most of the catalogue and the grid has nothing left to
  prune — that is not a neighbour search, and refusing says so where returning everything slowly
  would not.
- **The number of cells is capped** at 0.3 per point — the figure the coarsening warning names when
  it fires. A very small `cell=` is
  coarsened to stay under it, with a warning: below that ceiling the bucketing keeps a fast path
  that a finer grid would lose, which costs more than the finer cells save. **The same cap applies
  to a HEALPix sky index as a pixel count**, so `nside` is bounded by the catalogue size and an
  explicit `nside=` is coarsened the same way, with the same warning. It costs a pixelisation far
  less than it costs the grid, because every pixel is on the sphere while most of the grid's cells
  are not — but on a small catalogue it still floors the resolution well above what a small query
  radius would want.
- **`combine=PF_LINK_SUM` on the sky needs every angular radius to be at most 45 degrees.** The
  rule's searcher walks twice its own radius, and 90 degrees is the ceiling every sky query has, so
  the halved limit is that ceiling seen from the other end rather than a second policy. The
  Euclidean form has no such limit.
- **`combine=` is offered on the per-point pair forms only.** With one radius for every point three
  of the four rules coincide, and `PF_LINK_SUM` is the same call at twice the radius, so a
  selector there would be four names for two answers.
- **`r_inner=` is scalar-only on `%pairs_within` and `%pairs_within_sky`.** Everywhere else it
  takes one value or one per point. With per-point inner radii a pair would qualify when it lies in
  *i*'s annulus **or** in *j*'s, and the union of two different annuli is not an annulus — which
  breaks the ranking that makes each pair be emitted from exactly one endpoint, and would leave the
  edge list quietly incomplete. Passing one is refused rather than approximated.
- **`%nearest` on a periodic index cannot look past half the box.** A periodic ball beyond `L/2` is
  undefined rather than merely imprecise, so a `k` that cannot be reached inside that radius is an
  error naming `k` and the box, not a shorter answer.
- **The line-of-sight queries need a 3D, Euclidean, non-periodic index**, and `observer=`/`los=`
  are refused anywhere else: a 2D index has no depth, a periodic one no observer the minimum image
  could agree on, a sky index no distance. A point sitting on the observer has no line of sight and
  is refused too — at `%build` when `los=` is given, at the query otherwise — as are a constant
  `los=` and a NaN in it. A `los=` that is not a function of the distance from the observer is
  accepted with a warning, and the walk is then wide.
- **The index does not know its coordinates have moved.** Nothing detects a mutated array behind a
  `copy=.false.` index, and with `copy=.true.` only `%rebuild` looks. A stale index returns wrong
  answers silently, so call `%rebuild` after anything that may have changed the data.
