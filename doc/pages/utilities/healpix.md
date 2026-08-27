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

`use parquet_healpix` compiles four of this library's Fortran files and reaches no reader, no
writer and no Arrow — see [Which module do I import?](../operating/choosing-a-module.html) for the
whole table. Everything here is also available through `use parquet`.

## Two conventions to get right

**`theta` is colatitude, measured from the north pole**, so `theta = pi/2 - dec` and `theta = 0` is
the north pole. This module offers no RA/Dec entry point at all, and that is deliberate rather than
an omission: a mirrored convention (`theta = pi/2 + dec`, which puts the *south* pole at `theta = 0`)
is in live use in real projects, it is perfectly self-consistent — a reflection preserves angular
distance, so every separation and every disc comes out the same size — and mixing the two silently
searches the wrong hemisphere. Nothing in a signature taking a bare `dec` could tell you which one
it meant, so the conversion stays at your call site, where it is visible.

**A pixel index is 0-based; the array `pf_query_disc` fills is 1-based.** Those are two different
things and they are the pair most easily conflated. Pixel indices run `0 .. 12*nside**2 - 1`,
because that is how the pixelisation itself defines them, and this library does not get to renumber
them. The `listpix` buffer is filled from element 1, like every other user-facing array here. So
`listpix(1)` is the first pixel found and its *value* may well be `0`, and indexing your own
per-pixel array by a pixel index means `mydata(listpix(k) + 1)`.

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
