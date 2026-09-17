---
title: Random points and geometry on the sphere with parquet_sphere
---

`parquet_sphere` draws random points uniformly inside a region of the sky — a polygon given by its
right ascensions and declinations, one HEALPix pixel, or a list of pixels — and carries the RA/Dec
geometry such work needs: conversion between positions and unit vectors in a declination frame you
name, offsetting a position by a separation at a position angle, and the Fibonacci grid of
quasi-uniform directions. Every draw is addressed by `(seed, i, [draw])` exactly as
[`pf_random_at`](random.html) is, so a mock catalogue comes out the same under any OpenMP schedule
and any thread count.

```fortran
use parquet_sphere
use iso_fortran_env, only : int64, real64

type(pf_healpix_grid) :: grid
type(pf_sky_polygon) :: footprint
integer(int64), allocatable :: mask(:)
integer(int64) :: k, idx(1000)
real(real64) :: ra(1000), dec(1000)

! A mock catalogue over a HEALPix mask: 1000 points, uniform over the listed pixels.
call grid%init(256_int64, PF_HP_NEST)
mask = [(k, k = 4096_int64, 4199_int64)]
call pf_random_fill_mask_radec(grid, 42_int64, 1_int64, mask, ra, dec)

! A survey footprint written by its vertices, crossing ra = 0 as 350 .. 370.
call footprint%init([350.0_real64, 370.0_real64, 370.0_real64, 350.0_real64], &
                    [-10.0_real64, -10.0_real64, 5.0_real64, 5.0_real64])
idx = [(k, k = 1_int64, 1000_int64)]
call footprint%random_at(42_int64, idx, ra, dec)       ! row k is stream k
```

Signatures below are written with optional arguments in **square brackets** —
`pf_radec2vec(ra, dec, vec, [frame])` means `frame` may be omitted. The brackets are not Fortran, and
a block containing them is a description rather than a runnable example.

`use parquet_sphere` compiles 15 of this library's Fortran files — `parquet_random`,
`parquet_healpix`, `parquet_utils` and its own four — and never reaches this library's reader, its
writer or its C++ bindings. The module re-exports `pf_healpix_grid`, `pf_random_stream` and the four
HEALPix selectors (`PF_HP_RING`, `PF_HP_NEST`, `PF_HP_DEC_NORTH`, `PF_HP_DEC_SOUTH`), so one import
is enough to call everything on this page. See [Choosing a module](../operating/choosing-a-module.html)
for the whole table. Everything here is also available through `use parquet`.

## Positions, vectors, and where the frame enters

```fortran
call pf_radec2vec(ra, dec, vec, [frame])      ! degrees -> unit vector
call pf_vec2radec(vec, ra, dec, [frame])      ! any nonzero vector -> degrees, ra in [0, 360)
```

Two declination conventions are in live use, and they send the same number to opposite hemispheres:
the standard `theta = pi/2 - dec`, and a mirrored `theta = pi/2 + dec`
([HEALPix](healpix.html#two-conventions-to-get-right) explains why that matters). **A conversion
between a position and a vector has to know which one it is in**, so these two take `frame=`, with
`PF_HP_DEC_NORTH` — the standard convention — as the default and `PF_HP_DEC_SOUTH` for the mirrored
one. Any other value stops the program.

Both are **total**, like `pf_angdist_deg`. A NaN argument gives NaN results without raising a
floating-point flag; a declination outside `[-90, 90]` is read as the direction it names; a vector
need not have unit length, and one as small as `[1e-300, 0, 1e-300]` is still a direction; an
infinite component is read as the direction of the infinite components alone; the zero vector gives
`(0, 0)`. Two rules make the poles exact: **a declination of exactly ±90 is the pole `(0, 0, ±1)`**,
whatever the right ascension, and **the right ascension of a pole is 0**. `pf_vec2radec` takes the
declination as `atan2(z, hypot(x, y))`, never `asin(z)`, which loses half its digits near a pole.

**Nothing else on this page takes a frame, or needs one.** A procedure whose input and output are
both `(ra, dec)` — a polygon, an offset, a position angle, the Fibonacci grid's RA/Dec form — gives
the same answer under either convention: the two differ by a reflection, which maps great circles,
containment and solid angle onto themselves. These compute in the standard frame. A frame enters
again only where a pixel crosses the interface, and there it is the one your `pf_healpix_grid` was
built with.

## Polygons: `pf_sky_polygon`

```fortran
type(pf_sky_polygon) :: poly

call poly%init(ra, dec, [edges])      ! vertices in degrees, in order
inside = poly%contains(ra1, dec1)     ! elemental
call poly%random_at(seed, i, ra1, dec1, [draw])
```

`%init` takes the vertices as two `real(real64)` arrays of one size, at least three, in order; the
orientation does not matter. `%clear()` releases the polygon so `%init` may run again, and `%init` on
a polygon already built stops the program rather than replacing it. `%is_set()`, `%size()` (the
vertex count) and `%edges()` answer on any polygon — 0 and -1 before `%init`.

### Two edge rules

**`PF_EDGE_RADEC`, the default: edges are straight lines in the RA/Dec chart.** An edge between two
vertices at one declination follows that parallel, so a declination band or an RA/Dec box is exactly
what it looks like when written down. **`PF_EDGE_GREAT_CIRCLE`: edges are the minor great-circle
arcs** between consecutive vertices, the shape of a polygon drawn on the sky. The two differ visibly
on a long edge: between two vertices at declination 40 and 60 degrees apart in right ascension, the
great circle reaches declination 44.1 at its midpoint.

Either way a point is inside by the **even-odd rule**, so a self-intersecting polygon is accepted and
sampled by it. A position on an edge is inside or outside arbitrarily, which no draw can notice.

### Vertices are read as written

A polygon crossing `ra = 0` is written continuously — `350, 370` rather than `350, 10` — because the
vertices are taken as written, and `350, 10` names the band the long way round. A band around the
whole sky is `0, 360, 360, 0` at two declinations, and a polar cap above 80 degrees is the same box
at `80, 80, 90, 90`. Under `PF_EDGE_RADEC` the vertices may span at most 360 degrees of right
ascension. `%contains(ra, dec)` accepts any right ascension and reads it within the polygon's own
range, so `-5`, `355` and `715` are one position; a non-finite argument or a declination outside
`[-90, 90]` is outside.

### Great circles and the hemisphere rule

A great-circle polygon must fit an open hemisphere: **every vertex within 89.9 degrees of the
vertices' mean direction**. A polygon that does not is refused, with the vertex and its angle named,
and the remedy is to split it. Containment projects each position onto the plane tangent at that
mean direction, where every great circle is a straight line. **A position on the far side of the
mean direction's hemisphere is outside before it is projected**, because the projection maps a
direction and its antipode to one point.

### Area, acceptance and the floor

`%area()` is the area in steradians and `%area_deg2()` in square degrees, exact to rounding for a
simple polygon: Green's theorem on `cos(dec) d(dec) d(ra)` for chart edges, a sum of spherical
excesses for great-circle edges. For a self-intersecting polygon it is the signed sum of its lobes,
which is not the area the even-odd rule samples. `%bounds(ra_lo, ra_hi, dec_lo, dec_hi)` is the
vertex box, right ascension as written; a great-circle edge may bulge beyond its declinations.

A draw takes candidates uniformly over a **bounding region** and keeps the first inside: the RA/Dec
box for `PF_EDGE_RADEC` — right ascension uniform, and the sine of the declination uniform, so the
candidates are uniform per unit solid angle — or the cap about the mean direction through the
farthest vertex for `PF_EDGE_GREAT_CIRCLE`. `%acceptance()` is the fraction kept, the area over the
bounding region's area, and a draw averages `1/%acceptance()` candidates.

**`%init` refuses a polygon with an acceptance below 1e-3**, and the message says to split it. A
thin strip running diagonally across its own box is the shape that does this; a handful of boxes
following the strip is the same region at a fraction of the cost. A polygon of zero area is refused
as well. `%area`, `%area_deg2`, `%acceptance`, `%bounds`, `%contains` and every draw stop the program
on a polygon `%init` has not built.

### Drawing points

```fortran
call poly%random_at(seed, i, ra, dec, [draw])       ! pure elemental over i and draw
call poly%random_fill(seed, i, ra, dec, [draw])     ! ra(n), dec(n): element k is draw + k - 1
call poly%random_next(rng, ra, dec)                 ! the next point of a pf_random_stream
```

Every point is uniform **per unit solid angle** inside the polygon, never per unit of the chart, with
`ra` in `[0, 360)`. `%random_at` is elemental, so an index array draws a catalogue in one statement.
`%random_fill` fills two arrays of one size with consecutive draws of one stream, and splits at any
boundary with identical results. `%random_next` takes the next point along a stream: it aligns the
stream to a block, returns `%random_at` at the stream's own seed and index for that block however
many candidates the point took, and advances `%position` by one block, four words.

A draw that rejects 100000 candidates in a row stops the program. At the 1e-3 floor that has a
probability of `exp(-100)`, so it is a defect to report, not a mode to handle.

## Points in HEALPix pixels and masks

```fortran
v = pf_random_pixel_at(grid, seed, i, ipix, [draw])               ! a unit vector inside pixel ipix
call pf_random_pixel_radec_at(grid, seed, i, ipix, ra, dec, [draw])
v = pf_random_mask_at(grid, seed, i, pixels, [draw])              ! uniform over the listed pixels
call pf_random_mask_radec_at(grid, seed, i, pixels, ra, dec, [draw])
call pf_random_fill_mask(grid, seed, i, pixels, v, [draw])        ! v(3, n)
call pf_random_fill_mask_radec(grid, seed, i, pixels, ra, dec, [draw])
call pf_random_pixel_next(grid, rng, ipix, v)                     ! and the three other stream forms
```

Every form takes a built [`pf_healpix_grid`](healpix.html#carrying-the-grid-in-an-object), which
carries the resolution, the numbering scheme a pixel index means, and the declination frame the
`_radec` forms answer in. The vector forms return HEALPix-native unit vectors, as `%pix2vec` does.
`ipix` and the list share one integer kind, `int32` or `int64`, independently of `i`.

A point in a pixel is drawn from the cap of radius `%max_pixrad` about the pixel's centre and kept
when `%vec2pix` names the pixel, which is about **3.4 candidates** a point.
`pf_random_pixel_radec_at` is elemental over `i`, `ipix` and `draw`, so one statement draws a point
in every pixel of a list. **The grid's `nside` must be at most 2²⁴**: beyond that a unit vector can
no longer name a pixel near a pole, and the samplers stop the program rather than bias the draws.

A **mask** is a non-empty list of pixel indices. The pixel is chosen uniformly from the list —
uniform over the union, since every pixel has one area — and **a pixel listed twice counts twice**,
so a list built by repetition is a mask with integer weights. A scalar mask draw checks only the
entry it chooses, so a long list costs nothing per draw; the fills check every entry before drawing
anything. For real-valued weights, choose the pixel yourself and draw in it:

```fortran
real(real64) :: cum(npix_listed), u
integer :: j

cum = pixel_weights                                  ! one weight per listed pixel
do j = 2, size(cum)
    cum(j) = cum(j - 1) + cum(j)
end do
u = cum(size(cum)) * pf_random_at(pf_random_key(seed, my_label), i, draw)
j = findloc(cum > u, .true., dim=1)                  ! the first pixel whose cumulative weight passes u
v = pf_random_pixel_at(grid, seed, i, pixels(j), draw)
```

`pf_random_at` and `pf_random_key` come from `parquet_random`, and `my_label` is any integer of your
own.

## Offsets, position angles and the Fibonacci grid

```fortran
call pf_offset_radec(ra0, dec0, pa_deg, sep_deg, ra, dec)     ! pure elemental
pa = pf_position_angle_deg(ra1, dec1, ra2, dec2)              ! pure elemental
call pf_fibonacci_grid(n, vec, [frame])                       ! vec(3, n)
call pf_fibonacci_grid_radec(n, ra, dec)                      ! ra(n), dec(n)
```

`pf_offset_radec` moves `sep_deg` along the great circle leaving `(ra0, dec0)` at position angle
`pa_deg`, measured from north through east, and `pf_position_angle_deg` recovers that angle, in
`[0, 360)`: both follow astropy's `directional_offset_by` and `position_angle`. **At a pole the local
frame follows the `ra0` you gave**, so an offset from the north pole at position angle `pa` lands at
right ascension `ra0 + 180 - pa`, and one from the south pole at `ra0 + pa`. A separation above 180
continues along the same great circle. `pf_offset_radec` validates — a non-finite argument, a `dec0`
outside `[-90, 90]` and a negative `sep_deg` stop the program — because a centre in the wrong range
gives a plausible wrong point. The position angle is total: a NaN argument gives a NaN, and **a
coincident pair has position angle 0**, including two labels of one pole.

The Fibonacci grid places `n` directions nearly uniformly: point `k` (from 0) has latitude
`asin(1 - 2*(k + 1/2)/n)` and longitude `2*pi*(k + 1/2)/phi`, with `phi` the golden ratio. That is
astropy's `golden_spiral_grid`, the half step in the longitude included, so the two agree to rounding;
neighbours sit about `sqrt(4*pi/n)` radians apart and neither pole is a point. The vector form takes
`frame=`; the RA/Dec form needs none. `n` is `int32` or `int64`, at least 1, and the arrays must hold
exactly `n` points.

## How the draws are addressed

The rules are `parquet_random`'s, and [Random numbers](random.html#points-on-a-sphere) explains
them. What this module adds is how a rejection sampler keeps them.

- **A value is a pure function of `(seed, i, draw)`.** The candidates of draw `d` are draws
  `1, 2, 3, ...` of their own key, `pf_random_key(pf_random_key(seed, <family label>), d)`, on stream
  `i` — so a draw needing twelve candidates moves nothing that any other draw reads, a fill splits
  anywhere, and a stream form takes exactly one block per point.
- **Families are independent.** The polygon, the pixel, the mask's choice and the mask's point each
  have their own label, distinct from every label `parquet_random` uses, so none shares a bit with
  another family or with `pf_random_at` at the same coordinate. A mask's point is not the pixel
  sampler's point for the chosen pixel.
- **The region is not part of the key.** Two polygons sampled at one coordinate transform one
  candidate stream, so overlapping polygons give related points. For independent draws, give each
  region its own seed: `pf_random_key(seed, k)` for the `k`-th.
- **`pf_sky_region_algorithm` names the value contract** of every sampler on this page, for a given
  libm, as `pf_sphere_algorithm` does for the disc they draw candidates from. The conversions, the
  offset and the grid are arithmetic, pinned to an accuracy rather than frozen.

## Thread safety

Everything that reads a polygon or a grid is `pure` over an `intent(in)` object, so one polygon built
before a parallel region serves the whole team, and nothing here holds state a thread could race on.
`%init` and `%clear` change the polygon and must not run while another thread reads it. **Build a
polygon before the region, not in a `block` inside it**: `pf_sky_polygon` has allocatable components,
and ifx cannot privatise such a type in a `block` within a parallel region. Nothing on this page
prints, and nothing reads a setting.

## Limitations

- No polygons with holes — draw in the outer one and reject with the inner one's `%contains` — and
  no unions of polygons; draw a polygon with probability proportional to its `%area()` first.
- No great-circle polygon spanning a hemisphere or more, and no triangulation, clipping or
  intersection of polygons.
- No MOC, Mangle or FITS mask formats, and no boolean map as a mask: `pack` one into a pixel list.
- No `real32` forms, and no `threads=` on the fills; wrap a loop of `%random_at` or of the fills in
  your own parallel region.

## See also

- [Random numbers](random.html#points-on-a-sphere) — directions, discs, balls and rotations, and the
  addressing every draw here follows.
- [Sphere pixelisation with `parquet_healpix`](healpix.html) — the grid these samplers take, and the
  two declination conventions.
- [Spatial neighbour search](spatial.html#search-on-the-sky) — finding the points near a position once
  you have drawn them.
