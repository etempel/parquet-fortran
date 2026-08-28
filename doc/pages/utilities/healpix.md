---
title: Sphere pixelisation with `parquet_healpix`
---

HEALPix divides the sphere into `12*nside**2` pixels of exactly equal area, arranged on rings of
constant latitude. `parquet_healpix` gives you the part of it a program actually needs to hold sky
data on a grid: turning a direction into the pixel that contains it and back, converting between
the two pixel-numbering schemes, and listing the pixels of a disc.

```fortran
use parquet_healpix
use iso_fortran_env, only : int64, real64

integer(int64) :: nside, ipix, nfound
integer(int64) :: listpix(4096)
real(real64) :: theta, phi, vec(3)

nside = 1024_int64
theta = 0.5_real64 * 3.14159265358979_real64 - dec * deg2rad   ! colatitude, radians
phi = ra * deg2rad

call pf_ang2pix_nest(nside, theta, phi, ipix)                  ! which pixel is this direction in?
call pf_pix2vec_nest(nside, ipix, vec)                         ! and where is that pixel's centre?
call pf_query_disc(nside, vec, 0.01_real64, listpix, nfound)   ! which pixels lie within 0.01 rad?
```

`use parquet_healpix` compiles seven of this library's Fortran files and reaches no reader, no
writer and no Arrow — see [Which module do I import?](../operating/choosing-a-module.html) for the
whole table. Everything here is also available through `use parquet`.

## Two conventions to get right

**`theta` is colatitude, measured from the north pole**, so `theta = pi/2 - dec` and `theta = 0` is
the north pole. This module offers no RA/Dec entry point at all, and that is deliberate rather than
an omission: a mirrored convention (`theta = pi/2 + dec`, which puts the *south* pole at `theta = 0`)
is in live use in real projects, it is perfectly self-consistent — a reflection preserves angular
distance, so every separation and every disc comes out the same size — and mixing the two silently
searches the wrong hemisphere. Nothing in a *signature* taking a bare `dec` could tell you which
one it meant — so among the free procedures the conversion stays at your call site, where it is
visible.

An **object** can tell you, because it carries the answer rather than asking for it at each call.
That is what [`pf_healpix_grid`](#carrying-the-grid-in-an-object) below is for: you name the
convention once, when you build the grid, and every RA/Dec call on it reads that. If you work in
RA/Dec at all, use the grid — it is the only place this module offers that layer.

**A pixel index is 0-based; the array `pf_query_disc` fills is 1-based.** Those are two different
things and they are the pair most easily conflated. Pixel indices run `0 .. 12*nside**2 - 1`,
because that is how the pixelisation itself defines them, and this library does not get to renumber
them. The `listpix` buffer is filled from element 1, like every other user-facing array here. So
`listpix(1)` is the first pixel found and its *value* may well be `0`, and indexing your own
per-pixel array by a pixel index means `mydata(listpix(k) + 1)`.

## Carrying the grid in an object

`pf_healpix_grid` holds a resolution, a numbering scheme and a declination convention together, so
that no call has to restate them:

```fortran
use parquet_healpix
type(pf_healpix_grid) :: sky
integer(int64) :: ipix, npix

call sky%init(1024_int64, PF_HP_NEST)          ! nside and scheme; frame defaults to north
call sky%get_npix(npix)                        ! 12582912
call sky%radec2pix(214.25_real64, 52.5_real64, ipix)
```

**`%init` is the only binding that validates, and the only one that can abort.** It refuses an
`nside` that is not a positive power of two or is beyond the kind's ceiling, a `scheme` that is
neither selector, and a `frame` that is neither. After that the object is known good, so the
conversions on it are free to be total — they never abort, exactly as the free procedures never do.
`scheme` is mandatory: it is written once, possibly far from every query, and a grid silently RING
against NEST data would be wrong everywhere with nothing to notice it.

**The scheme being state collapses the `_ring`/`_nest` pairs.** `%ang2pix`, `%pix2ang`, `%vec2pix`
and `%pix2vec` are four bindings covering what takes eight free procedures, and `nside` disappears
from every call. The conversions are elemental, so whole arrays work with no separate array form:

```fortran
call sky%ang2pix(theta(:), phi(:), ipix(:))    ! the same binding, over a million rows
```

`%ang2pix_bulk` and its siblings exist only to carry `threads=`, which an elemental procedure
cannot take. Everything else about them is the free bulk forms' behaviour.

### RA and Dec, in degrees, in a convention you named

This is the layer the object exists for, and the only place this module offers it.

**`PF_HP_DEC_NORTH` is the default**: `theta = pi/2 - dec`, positive declination towards the north
pole, the standard astronomical convention. A grid built without `frame=` uses it, so ordinary
catalogue declinations need nothing said. **If your data uses the mirrored `theta = pi/2 + dec`**,
where positive declination points *south* — as `radec_to_vec_healpix` and the code downstream of it
do — you must say so, once:

```fortran
call sky%init(1024_int64, PF_HP_NEST, frame=PF_HP_DEC_SOUTH)
```

Nothing can detect that for you: both conventions are self-consistent and every result comes out
the right size, just reflected. If a program cares, `%frame()` reports what a grid was given, so one
assertion at start-up settles it.

Every angle in this layer is in **degrees** — right ascension, declination, and a disc radius:
`%radec2pix`, `%pix2radec`, `%radec2vec`, `%vec2radec`, and `%query_disc_radec` with its `_count`
and `_alloc` siblings. Right ascension comes back in `[0, 360)` and declination in `[-90, 90]`.
The radian spellings (`%ang2pix`, `%query_disc`) are the same procedures without the conversion, so
pick by the units you are holding, never by mixing them.

### Resolutions, and comparing two grids

`%at_nside` and `%at_order` derive a grid at another resolution, keeping the scheme and the frame —
which is what running two grids side by side actually wants:

```fortran
tar = sky%at_nside(256_int64)            ! same scheme, same frame, four times coarser
call sky%ud_pix(ipix, tar, ipix_coarse)  ! NEST resolution change, or -1 if either is not NEST
```

`==` and `/=` compare resolution, scheme **and** declination convention. Two grids describing the
same pixelisation but disagreeing about declination are not interchangeable, and the question the
operator is for — "does this file's grid match mine" — wants to know.

### Sizes, in the kind you ask for

`%get_nside` and `%get_npix` are subroutines rather than functions, and that is deliberate: the
kind of the argument you pass selects the answer's kind, which a function could not do. Asking for
a value in a kind too small to hold it **aborts** rather than wrapping — `npix` passes the int32
range at `nside = 16384`.

```fortran
integer(int32) :: n32
integer(int64) :: n64
call sky%get_npix(n64)     ! always fine
call sky%get_npix(n32)     ! aborts if this grid's npix exceeds huge(0_int32)
```

`%order`, `%scheme`, `%frame`, `%is_set`, `%pixarea`, `%resol` and `%max_pixrad` are ordinary
functions — none of them has a kind question.

### What an unbuilt grid does

A `pf_healpix_grid` that `%init` has never run on is usable and reports so rather than crashing:
`%is_set()` is `.false.`, a pixel-valued binding returns `-1`, an angle-valued one returns `-999`
(outside the range of every angle here), and a disc or bulk query aborts naming the binding. **No
floating-point exception is raised on that path either** — the sentinel is answered directly rather
than by dividing by a zero `nside`.

**The free procedures remain the primary API.** The object is sugar over them: every binding
delegates to one, so the answers are identical by construction.

**Pick by what reads better.** The delegation costs a call and nothing more. Measured with
`bench/benchmark_healpix.sh --mode=grid` — each binding against the free procedure it calls, best
of five passes over two million elements at `nside` 1024 — every operation is within 5% of the free
form under gfortran.

Under ifx the same table spreads from 0.66x to 1.09x, and that is worth one paragraph because it
looks like a dispatch cost and is not. ifx collapses a hand-written loop of *elemental* calls into
a single array-wide call through a temporary; it does that to the benchmark's free column and not
to its bound column, because a type-bound call carrying a passed object is not collapsed. So under
ifx the two columns of an elemental row are not the same computation. The absolute figures say so
plainly: on `%pix2ang`, which deviates most, the bound column is 18.58 ns under ifx against 18.54
under gfortran, while the free column is 28.08 against 17.70 — the free column is what moved. And
`%query_disc`, the one operation whose free form is not elemental and so cannot be collapsed,
measures 0.998 under ifx and 1.002 under gfortran.

## The two schemes

Every procedure comes in a `_ring` and a `_nest` form, and `pf_query_disc` takes a `scheme=`
argument selecting between them with `PF_HP_RING` or `PF_HP_NEST`.

**RING** numbers pixels along rings of constant latitude, north to south. Index order follows
latitude, so a declination band is one contiguous range of indices — which is what makes it the
right scheme for storing a survey footprint, or for finding the index range a band occupies without
enumerating it.

**NEST** numbers pixels so that the four pixels covering one pixel of the next-coarser resolution
are consecutive. A resolution change is then a bit shift rather than a computation, which is what
makes it the right scheme for hierarchical work and for keeping nearby pixels nearby in memory.

`pf_ring2nest` and `pf_nest2ring` convert between them at a fixed `nside`. Both are exact integer
arithmetic — no direction is computed and nothing rounds — so a round trip is the identity, and at
`nside = 1` the two schemes coincide.

## Resolution, kinds and limits

`nside` must be a positive power of two. Every procedure is generic over `integer(int32)` and
`integer(int64)`, with `nside`, the pixel index and the count all taking the kind you pass:

- **int32**: `nside` up to **8192**. Above that the pixel index overflows a signed 32-bit integer —
  `12*8192**2` is 805306368, and the next power of two does not fit.
- **int64**: `nside` up to **2**29** (536870912), which is the ceiling the HEALPix ecosystem itself
  uses.

Mixing kinds within one call is a compile error rather than a silent conversion, which is the point
of having both.

## Finding the pixels of a disc

```fortran
call pf_query_disc(nside, vec, radius, listpix, nlist [, scheme] [, inclusive])
```

(Square brackets mark optional arguments; they are not part of the syntax.)

`vec` is the direction of the disc centre and need not be normalised; `radius` is in radians. The
result is `listpix(1:nlist)`.

**`inclusive = .false.` (the default) returns the pixels whose CENTRES lie within `radius`.** That
is an exact specification: any correct implementation returns the same set, and you can check it by
scanning every pixel of the sphere.

**`inclusive = .true.` returns the pixels OVERLAPPING the disc.** Every implementation computes
this as a bounded superset, and the bound this one publishes is: every overlapping pixel is
returned, and no pixel whose centre lies farther than `radius` plus one pixel radius is returned. So
a returned pixel is not necessarily one that overlaps — if you need exactly the overlapping set,
filter the result yourself. That bound is also the honest way to size a search margin, and it is
tighter than the factor-of-1.5 rules of thumb such code often carries.

**Ordering: ascending for `PF_HP_RING`, unspecified for `PF_HP_NEST`.** The walk goes ring by ring
from north to south, so RING indices come out in order for free. The RING-to-NEST conversion
destroys that order, and sorting afterwards would cost more than any caller has needed — so NEST
output is deterministic but carries no promise, and should be compared as a set rather than as a
sequence.

**The buffer is yours and is not resized.** If the disc holds more pixels than `listpix` can take,
the call aborts with a message naming the capacity, the count reached, the `nside` and the radius,
rather than truncating silently. A disc of radius `r` covers roughly `nside**2 * 3 * r**2` pixels
for a small `r`; size generously, and remember that `inclusive = .true.` adds a pixel-wide fringe.

### A limit on very small discs

**A disc smaller than about `1e-8` radians (0.0022 arcsec) cannot be resolved, and returns too many
pixels rather than too few.** Membership is decided against `cos(radius)`, and the distance of that
from 1 is about `radius**2/2` — so the radius is resolved to a fraction `2.2e-16 / radius**2` of
itself: 2e-10 at a milliradian, 6e-3 at 2e-7 rad, and 1 at 1.05e-8, where `cos(radius)` is exactly
1.0 in double precision and the comparison stops distinguishing anything at all.

This is a property of any double-precision disc query rather than of this implementation, and it
only becomes reachable at extreme resolution: a disc a few pixels across is resolvable through
`nside = 2**28` and degenerates at `2**29`. Every `nside` a survey actually uses is many orders of
magnitude away from it.

## Angular separations

```fortran
call pf_angdist(vec1, vec2, dist)
```

The angle between two directions, in radians. Computed as `atan2(|v1 x v2|, v1.v2)` rather than
`acos(v1.v2)`, so it keeps its accuracy at both ends of the range — near zero, where `acos` of a dot
product rounds to exactly 1 and returns 0, and near `pi`. The inputs need not be unit vectors.

**Where you already hold unit vectors and only need a comparison, a squared chord is cheaper than
an angle**: `|v1 - v2|**2 <= (2*sin(r/2))**2` is the same test with no inverse trigonometry in it,
and no per-candidate transcendental at all.

If your positions are RA/Dec in degrees rather than vectors, there is a direct form:

```fortran
d = pf_angdist_deg(ra1, dec1, ra2, dec2)      ! degrees in, degrees out
```

It is `pure elemental`, so it broadcasts over whole arrays — which `pf_angdist` cannot, its
arguments being arrays already — and it is **31% cheaper than converting to vectors and calling
`pf_angdist`**, because working in the frame where only the RA difference survives removes one of
the four sine/cosine pairs.

**This is the one place the module takes a bare `dec`, and it can because the answer does not
depend on which declination convention you mean.** Everywhere else a bare `dec` is refused and
RA/Dec lives on [`pf_healpix_grid`](#carrying-the-grid-in-an-object) with its `frame=`, because
`theta = pi/2 - dec` and the mirrored `theta = pi/2 + dec` send the same number to opposite
hemispheres. Those two differ by a reflection, and a reflection does not change the angle between
two directions, so this procedure gives the same answer under either — and the test suite asserts
that, bit for bit.

Right ascension may be any value: it is folded into one turn internally, so an accumulated hour
angle or a negative RA needs no preparation, and a separation that straddles RA 0 keeps full
precision. Accuracy is within **7.8e-15 degrees** of a 60-digit evaluation across the edge cases —
the seam, both poles, coincident and antipodal positions — and that worst case is at the pole,
where it is `cos(dec)` losing relative precision rather than the formula. `acos` of the dot product
and the haversine were both measured and rejected: the first is 2.0e-07 degrees wrong a millionth
of a degree from the pole, the second 9.5e-07 just inside antipodal.

Like every elemental procedure here it is **total**: it validates nothing and never aborts, so a
NaN argument gives a NaN result rather than an error.

## What is validated and what is not

**`pf_query_disc` validates its arguments and aborts on a bad one** — an `nside` that is not a
positive power of two or is above the ceiling for your integer kind, a negative or NaN radius, a
centre vector holding a NaN or having zero length, a `scheme` that is neither selector, or a
`listpix` too small for the result. Each message names the routine and the offending value.

**The conversions validate nothing and never abort.** `pf_ang2pix_ring`, `pf_ang2pix_nest`,
`pf_pix2ang_*`, `pf_pix2vec_*`, `pf_ring2nest` and `pf_nest2ring` are `pure elemental` and total:
they are the per-element path, so the checks live in the once-per-query entry point instead. The
consequence is worth stating plainly — **`pf_ang2pix_nest(0, theta, phi, ipix)` returns nonsense
rather than complaining**, and so does a pixel index outside `0 .. 12*nside**2 - 1`. Validate an
`nside` you read from a file before you use it.

Being `elemental`, those procedures also accept arrays: pass `theta(:)`, `phi(:)` and `ipix(:)` and
the whole column is converted in one call.

## No floating-point exception is raised

**A program running under `-ffpe-trap=zero,invalid` can call anything here without a guard**, and
that is the reason this module exists rather than a dependency on a library that does raise. Every
square root takes an argument made non-negative by construction, every inverse cosine takes a
clamped one, and the one division is guarded by the branch above it.

The single exception is the one the previous section describes: feeding a NaN angle to a
non-validating conversion propagates it into a floating-point-to-integer conversion, which may
raise `IEEE_INVALID`. Garbage in is outside the promise; the validating entry points reject a NaN
before it can get that far.

## Thread safety

The module holds no state — no saved variables, no cache, no lazy initialisation — so every
procedure is a function of its arguments alone and can be called from any number of threads with
nothing to arrange. The scalar conversions are `pure`, so a loop over them threads with a plain
`!$omp parallel do`.

## Precision near the poles

A ring's latitude is defined by its `z`, and `theta = acos(z)` loses about half its significant
digits when `z` is within an ulp or two of ±1. At `nside = 2**20` the first ring's `z` is
`1 - 3e-13`, so the `theta` that `pf_pix2ang_ring` reports for a pixel there is good to about
`4e-10` rather than to an ulp. Longitude carries no such loss, and neither does the pixel index.

If that matters, work in unit vectors through `pf_pix2vec_ring`/`pf_pix2vec_nest`, which are the
primitive form — `pf_pix2ang_*` is derived from them, not the other way round.

## Directions as vectors

```fortran
call pf_ang2vec(theta, phi, vec)          ! direction -> unit vector
call pf_vec2ang(vec, theta, phi)          ! and back; phi comes out in [0, 2*pi)
call pf_vec2pix_ring(nside, vec, ipix)    ! vector straight to a pixel, either scheme
call pf_vec2pix_nest(nside, vec, ipix)
```

**`vec` never needs normalising, and its components may span any magnitude.** Everything here
divides by the largest component before anything is squared, so `[1e-300, 0, 1e-300]` and
`[1e300, 0, 1e300]` both name the same direction — 45 degrees from the pole — and neither raises a
floating-point exception on the way. That is worth knowing because it is not automatic: squaring
first underflows the one and overflows the other, and the answers you get are wrong by the whole 45
degrees rather than by a rounding.

**`pf_vec2pix_*` is not `pf_ang2pix_*` with an angle computed first**, and that is why it exists:
`pf_ang2pix_*` begins by taking the cosine of `theta`, which a caller holding a unit vector already
has. Going through the angle throws away an inverse tangent and a cosine, and a rounding at each
end.

**Longitude is undefined at a pole and is reported as `0` there**; the zero vector reports
`theta = 0, phi = 0`. Both are documented answers rather than accidents — `atan2(0, 0)` is
prohibited by the Fortran standard and raises an exception on some compilers, so those inputs are
guarded rather than passed through.

### One limit worth knowing before you use vectors at extreme resolution

**Above about `nside = 2**24`, a direction vector can no longer name a pixel near a pole.** A
ring's latitude is its `z`, and near a pole consecutive rings converge: the gap between ring `i`
and ring `i+1` is `(2i+1)/(3*nside**2)`, which at `nside = 2**29` is `3.5e-18` at the first ring
and `1.5e-16` at the sixty-fifth — 0.02 and 0.68 of one ulp of `z = 1`. Two adjacent rings there
are not distinct double-precision numbers, so no implementation can recover the right one from a
unit vector.

`pf_ang2pix_*` is unaffected: it works from `theta`, where the same rings are millions of ulps
apart. So at extreme resolution near a pole, keep angles rather than vectors — and note that every
`nside` a survey actually uses is far below where this begins.

## Grid arithmetic

```fortran
pf_nside2npix(nside)      pf_npix2nside(npix)
pf_nside2order(nside)     pf_order2nside(order)
pf_nside2pixarea(nside)   ! steradians
pf_nside2resol(nside)     ! radians -- the side of the equal-area square
pf_max_pixrad(nside)      ! radians -- centre to farthest corner
pf_pix2ring_ring(nside, ipix)   pf_pix2ring_nest(nside, ipix)   pf_ring2z(nside, iring)
```

All `pure elemental` functions, so each accepts whole arrays, and each takes the integer kind you
pass. Rings are indexed `1 .. 4*nside-1` from the north pole, ring `nside` being the first of the
equatorial belt.

**An argument outside the domain gives `-1`** — `-2` for `pf_ring2z`, whose valid results include
`-1`. That is one rule for the whole family, and it is forced rather than chosen: these are `pure`
procedures, and a `pure` procedure may not contain an `error stop`. So:

```fortran
nside = pf_npix2nside(npix_read_from_a_file)
if (nside < 0) error stop "that file's pixel count is not a HEALPix one"
```

**`pf_npix2nside` is the validity check on a pixel count**, and the `-1` is what makes it one. A
valid count is `12 * 4**order`, so the whole test is integer arithmetic and is exact at every
resolution.

**The int32 and int64 forms have different domains, and the boundary is the API ceiling rather than
an arithmetic accident**: `nside` up to 8192 for int32, up to `2**29` for int64. So
`pf_order2nside(20_int32)` reports `-1` rather than a perfectly representable `1048576` that no
int32 entry point of this module would then accept.

**A caller who ignores the sentinel gets a wrong answer, not an abort.** That is the same trade
`pf_ang2pix_nest(0, ...)` already makes, and it is the price of these being usable on arrays.

## Changing resolution in NEST

```fortran
call pf_ud_pix_nest(ipix, order_in, order_out, ipix_out)
```

In NEST the four pixels covering one pixel of the next-coarser grid are consecutive, so a
resolution change is a bit shift and nothing rounds. Coarsening gives the containing pixel;
**refining gives the LOWEST-indexed of the `4**k` children**, the full set being
`ipix_out .. ipix_out + 4**k - 1`. Equal orders are the identity.

`order_in` and `order_out` take the same integer kind as `ipix` — `pf_nside2order`'s result kind
follows its argument, so the two fit together without a cast. Either order outside `0 .. 29`, or a
negative `ipix`, gives `-1`.

This replaces round-tripping through angles to move between two resolutions, which is what a
program running a coarse and a fine grid side by side would otherwise do. That detour is lossy;
this is exact.

## The three disc forms

```fortran
call pf_query_disc(nside, vec, radius, listpix, nlist [, scheme] [, inclusive])        ! your buffer
call pf_query_disc_alloc(nside, vec, radius, listpix, nlist [, scheme] [, inclusive])  ! its buffer
call pf_query_disc_count(nside, vec, radius, nlist [, scheme] [, inclusive])           ! no buffer
```

All three answer the same question and run the same walk, so they cannot disagree.

**If what you want is the objects near a position rather than the pixels**, this module is the
wrong level: `pf_spatial_index` in [`parquet_spatial`](spatial.html#choosing-a-backend) will
bucket a catalogue by HEALPix pixel for you and answer neighbour, pair and k-nearest queries over
it, with the pixelisation as a `backend=` choice rather than something you assemble yourself.

**`pf_query_disc_alloc` is the one to reach for by default.** `listpix` comes back allocated to
exactly `nlist`, so there is no capacity to get wrong and no abort for getting it wrong. An empty
result allocates a **zero-length** array rather than leaving it unallocated, so `size()` is the
only thing you ever test and an `if (.not. allocated(listpix))` branch written against it is dead
code.

**`pf_query_disc` stays the form for a hot loop**, where allocating per call is the thing you are
avoiding. It is the only one that can abort on a buffer, and the price of that is that sizing the
buffer is yours.

**`pf_query_disc_count` answers the size alone**, walking the disc with its stores switched off. It
touches no array, so nothing can overflow. `scheme` changes nothing about a count and is accepted
only so that one call can be switched between the three forms without editing its arguments.

`pf_query_disc_alloc` counts first and then fills, which is what buys an exact size with no
allocation inside the walk and no over-allocation. The counting pass records where each run of
pixels landed, so the filling pass replays those runs rather than walking the ring geometry a
second time; a disc large enough to exhaust that record simply walks again.

## Comparing angles without computing them

```fortran
c2 = pf_chord2_from_angle(radius)              ! (2*sin(radius/2))**2
if (sum((v1 - v2)**2) <= c2) ...               ! the same test as pf_angdist(v1,v2) <= radius
angle = pf_angle_from_chord2(c2)               ! and back
```

Where you already hold unit vectors, this is the same comparison with **no inverse trigonometry per
candidate** — the chord is strictly increasing in the angle, so ordering by squared chord and
ordering by angle are the same order. Convert the radius once, outside the loop.

**Two warnings, and they compound.** `pf_chord2_from_angle` takes **radians** and returns the
**square** of the chord. `parquet_spatial` carries the same definition internally in the other
units — degrees in, the chord itself out — so a value moved between the two tiers without
converting is wrong by a factor of 57 and then squared. Nothing can catch that for you at compile
time, because the two are both plain `real64`.

`pf_angle_from_chord2` clamps at both ends, so a `chord2` a rounding below 0 or above 4 — which
your own arithmetic can easily produce — gives 0 or pi rather than raising.

## Converting a whole array at once

Every conversion has a `_bulk` form taking arrays and an optional `threads=`:

```fortran
call pf_ang2pix_ring_bulk(nside, theta, phi, ipix [, threads])
call pf_vec2pix_nest_bulk(nside, vec, ipix [, threads])      ! vec is (3, n)
call pf_ang2vec_bulk(theta, phi, vec [, threads])
call pf_pix2vec_ring_bulk(nside, ipix, vec [, threads])
```

and likewise `pf_ang2pix_nest_bulk`, `pf_pix2ang_ring_bulk`, `pf_pix2ang_nest_bulk`,
`pf_vec2pix_ring_bulk`, `pf_pix2vec_nest_bulk` and `pf_vec2ang_bulk`.

**What they buy differs, and it is worth knowing which case you are in.** The scalar
`pf_ang2pix_*` and `pf_pix2ang_*` are `pure elemental`, so you already have an array form — `call
pf_ang2pix_ring(nside, theta, phi, ipix)` compiles today — and you already have threading by
writing your own `!$omp parallel do`. For those four the bulk form adds the threading *discipline*:
a thread count that goes serial inside somebody else's parallel region instead of nesting, and that
is clamped to what your CPU affinity mask actually allows. For the other six, whose scalar form
takes a single `vec(3)`, there is no array form without them.

**The bulk forms validate; the elemental forms they wrap do not.** A bulk call checks `nside`, that
the arrays conform, and that an explicit `threads=` is at least 1, and aborts naming the problem.
That is the opposite of the rule stated under [What is validated and what is
not](#what-is-validated-and-what-is-not), and deliberately: the check is once per array rather than
once per element, and the realistic mistake at a bulk call site is a bad `nside` read from a file,
which a loop of a total procedure would turn into millions of silently wrong pixels.

**The result is identical at every thread count**, bit for bit: each element is a function of its
own inputs alone and there is no accumulation anywhere. A zero-sized array is a defined no-op.

**What `threads=` costs you if you omit it.** Leave it out and the team is chosen for you, from the
work available rather than from the machine: one thread per thousand elements, up to whatever your
CPU affinity mask allows and never more than 64. So an array of 500 runs serial, 10 000 gets ten
threads, and anything from 64 000 upward gets the full team. Two things drove that shape. Threading
a short array is a loss, and a single element count cannot mark where that stops on both a laptop
and a 384-processor node -- but the work *one* thread needs to be worth waking is much the same on
both, so that is what the rule is written in terms of. And the ceiling is real rather than
defensive: on the 384-processor machine these forms were measured on, a 192-thread team costs
**1118 ns per element** on 10 000 elements against 16 ns for a plain serial loop -- a 69x
regression, entirely in libgomp's fork and join -- while 64 threads is at or within noise of the
best figure at every size measured. An explicit `threads=` overrides all of it, including the
ceiling, on the rule that an explicit argument always wins.

Note the spelling: this module's array-with-threads forms end in `_bulk`, where `parquet_random`'s
array forms are spelled `pf_random_fill_*`. Two tiers, two conventions, and neither is going to
change.

## Migrating from `libhealpix`

The names and argument orders follow this library's conventions rather than HEALPix's, so a
migration is a real edit rather than a change of `use` line. Fortran's local renaming keeps most
call sites untouched:

```fortran
! before
use pix_tools, only: ang2pix_nest, pix2ang_ring, angdist
! after
use parquet_healpix, only: ang2pix_nest => pf_ang2pix_nest, &
                           pix2ang_ring => pf_pix2ang_ring, &
                           angdist => pf_angdist
```

`pf_query_disc` is the one that genuinely changes. Its `nest=`/`inclusive=` integer flags become a
`scheme=` selector and a `logical`, and `listpix` becomes 1-based:

```fortran
! before
call query_disc(nside, vec, radius, listpix, nlist, nest=1, inclusive=1)
! after
call pf_query_disc(nside, vec, radius, listpix, nlist, scheme=PF_HP_NEST, inclusive=.true.)
```

Since callers almost always allocate `listpix` 1-based already and loop `do k = 1, nlist`, the base
change usually costs nothing — the interface now says what those loops always assumed.
