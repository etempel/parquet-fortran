---
title: Spatial neighbour search with pf_spatial_index
---

`pf_spatial_index` answers "which points are near this one?" over plain coordinate arrays. It is a
uniform grid: the points are bucketed into cells once, and every later query walks only the cells
its search region can reach. Building is roughly one pass over the data, and a query costs
something close to the number of points it actually returns rather than the size of the catalogue.

Everything here is reachable from `use parquet`. The narrow import is `use parquet_spatial`, which
compiles nine Fortran files and never reaches this library's C++ bindings — so it pulls in neither
Arrow nor Parquet, and a program that only wants neighbour search need not have them.

Signatures below are written with optional arguments in **square brackets** —
`%build(x, y, [z,] radius=, [cell=])` means `z` and `cell=` may be omitted. Runnable examples never
use brackets.

## Building an index

`radius=` is mandatory, and it is not a limit on what you may later ask for — it is the hint the
cell size is chosen from. Give the radius your queries will actually use, or a list of them.

```fortran
use parquet
type(pf_spatial_index) :: sx
real(real64) :: x(n), y(n), z(n)

call sx%build(x, y, z, radius=0.05_real64)              ! 3D
call sx%build(x, y, radius=0.05_real64)                 ! 2D: omit z
call sx%build(x, y, z, radius=[0.01_real64, 0.05_real64, 0.2_real64])
```

A list is folded into one effective radius, `sum(r**3)/sum(r**2)`, which is the value the cost
model wants — the cubic term is the points a query tests and the quadratic one is how often. Read
it back with `%effective_radius()`.

**The cell size is measured, not guessed.** A cost model gives a starting bracket and a
deterministic probe then counts, for a handful of candidate cell sizes, how many cells a
representative query would visit and how many points it would distance-test. The candidate with the
least predicted work wins. The probe costs a fraction of the build it is part of, so it always
runs; you can see how many candidates it evaluated with `parquet_debug_spatial_probe_count()`.

Pass `cell=` to override it entirely — the probe is then skipped and your value is used, subject to
one clamp described under [limitations](#limitations).

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

The query point is a 2- or 3-element array and must match the rank the index was built with. The
output buffer may be `integer(int32)` or `integer(int64)`; the indices are rows of the arrays you
built from, in the caller's own order, whether or not the index copied them.

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
a pair with itself.

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
assumption across without noticing. Two other conventions are common for per-point radii and
neither is what `%pairs_within` gives — `d <= min(r_i, r_j)` (both must agree) and
`d <= r_i + r_j` (the balls touch) — but both are subsets of it, so either can be had by filtering
the result.

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

Two further notes. `%effective_radius()` comes back in **degrees**, because that is what you gave
it, while `%cell_size()` and `cell=` are both in unit-vector space — they are a matched pair, so a
value read from one can be fed back into the other. And `%build_sky` has no `copy=`: the stored
coordinates are unit vectors it computes, so there is nothing of yours to borrow.

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

A bulk query also **re-tunes itself** when the radius it has been given would choose a very
different cell from the one the index was built with, and says so once per index. Silence it with
`parquet_set_spatial_rebuild_warning(.false.)`.

## Threading and settings

Every bulk form threads internally; single queries do not.

```fortran
call sx%all_within(0.05_real64, offsets, neighbours, threads=8)
```

Omitting `threads=` resolves automatically: the process thread count, clamped to the number of
processors actually available, and serial when the call is already inside a parallel region.
`parquet_set_spatial_threads(n)` sets a process-wide cap; see
[Settings](../operating/settings.html).

An index is safe to **share for reading** across threads once built. Building, `%rebuild` and
`%rebuild_for` mutate it and must not run concurrently with anything else on the same object.

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
- **The number of cells is capped** at a fraction of the number of points. A very small `cell=` is
  coarsened to stay under it, with a warning: below that ceiling the bucketing keeps a fast path
  that a finer grid would lose, which costs more than the finer cells save.
- **There is no k-nearest-neighbour query.** A uniform grid answers a fixed-radius question
  directly; `k` nearest needs an expanding shell, which is a different algorithm.
- **The index does not know its coordinates have moved.** Nothing detects a mutated array behind a
  `copy=.false.` index, and with `copy=.true.` only `%rebuild` looks. A stale index returns wrong
  answers silently, so call `%rebuild` after anything that may have changed the data.
