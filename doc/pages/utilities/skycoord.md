---
title: Celestial coordinate systems with parquet_skycoord
---

`parquet_skycoord` converts sky positions between the celestial coordinate systems a catalogue is
kept in — ICRS, Galactic, ecliptic and supergalactic — and carries the RA/Dec geometry that needs no
coordinate system at all: the angular separation of two positions, the position a separation away
at a position angle, and that position angle back. Every procedure is `pure elemental` over `real64`
degrees, so a whole column converts in one call.

```fortran
use parquet_skycoord
use iso_fortran_env, only : real64

real(real64) :: ra(3), dec(3), l(3), b(3), sep(3)

ra = [266.40499_real64, 10.68458_real64, 83.82208_real64]
dec = [-28.93617_real64, 41.26875_real64, -5.39111_real64]
call pf_icrs2gal(ra, dec, l, b)                                   ! the whole column at once
sep = pf_angdist_deg(ra, dec, 10.68458_real64, 41.26875_real64)   ! each one's distance from M31
```

`use parquet_skycoord` compiles 4 of this library's Fortran files and never reaches this library's
reader, its writer or its C++ bindings. Three of them are its own; the fourth is `parquet_utils`,
the small helpers every tier builds on. See [Choosing a module](../operating/choosing-a-module.html)
for the whole table. Everything here is also available through `use parquet`.

## The systems

Each system has a `PF_COORD_*` selector, used where the system is named at run time, and each is
defined exactly as astropy defines it:

- **`PF_COORD_ICRS`** — the International Celestial Reference System, `(ra, dec)`. Every rotation
  here is referred to it.
- **`PF_COORD_GALACTIC`** — Galactic `(l, b)`, astropy's `Galactic`: its FK5 J2000 definition
  carried into ICRS through the frame bias between the two.
- **`PF_COORD_ECLIPTIC`** — ecliptic longitude and latitude, astropy's `BarycentricMeanEcliptic` at
  its default equinox, J2000: the IAU 2006 mean ecliptic and equinox of J2000, frame bias included,
  nutation not.
- **`PF_COORD_SUPERGALACTIC`** — supergalactic `(sgl, sgb)`, astropy's `Supergalactic`: de
  Vaucouleurs' pole at Galactic `(47.37, 6.32)`, with the north Galactic pole at `sgl = 90`.

**Each rotation is built from the three angles that define it** — the target system's north pole in
the system it is built from, and the target longitude of that system's north pole — so it is
orthonormal by construction, its inverse is its transpose rather than a second set of numbers, and
every matrix is a compile-time constant: no call builds one. The Galactic and ecliptic angles are
referred to ICRS and so carry the ICRS frame bias inside them, which is what makes the answers agree
with astropy's `SkyCoord` to rounding rather than differ from it by some 20 milliarcseconds — an
error too small to look like a bug.

## Converting between systems

```fortran
call pf_icrs2gal(ra, dec, l, b)          ! ICRS -> Galactic
call pf_gal2icrs(l, b, ra, dec)          ! Galactic -> ICRS
call pf_icrs2ecl(ra, dec, elon, elat)    ! ICRS -> ecliptic
call pf_ecl2icrs(elon, elat, ra, dec)    ! ecliptic -> ICRS
call pf_gal2sgal(l, b, sgl, sgb)         ! Galactic -> supergalactic
call pf_sgal2gal(sgl, sgb, l, b)         ! supergalactic -> Galactic
call pf_icrs2sgal(ra, dec, sgl, sgb)     ! ICRS -> supergalactic
call pf_sgal2icrs(sgl, sgb, ra, dec)     ! supergalactic -> ICRS
```

All eight take degrees and give degrees: the output longitude in `[0, 360)`, the output latitude in
`[-90, 90]`. The input longitude may be any value, including one below 0 or beyond a turn. Being
`pure elemental`, each takes scalars or arrays of any rank alike, and a caller converting a column
in parallel wraps the call in their own `!$omp parallel do`.

Supergalactic is defined in Galactic coordinates, so `pf_gal2sgal` is its own rotation rather than
a trip through ICRS; `pf_icrs2sgal` composes the two rotations into one matrix at compile time, so a
conversion from ICRS rotates once and rounds once.

## A system named at run time

```fortran
call pf_sky_convert(lon_in, lat_in, from, to, lon_out, lat_out)
call pf_coord_system_name(system, name)
system = pf_coord_system_from_name(token)
```

`pf_sky_convert` is the same conversion with the two systems given as `PF_COORD_*` selectors — for a
system read out of a configuration file or a column's metadata. For a pair that has a named
procedure it calls that procedure, so the two answer alike to the bit; for a pair that has none —
Galactic and ecliptic, ecliptic and supergalactic, either way round — it applies that pair's own
compile-time matrix, never two rotations in turn. **From a system to itself, the input comes back
unchanged**, by copy and before any arithmetic: a longitude of `-10` stays `-10`, so the `[0, 360)`
range applies to a real conversion only. `pf_sky_convert` is `pure elemental` like the named
procedures.

A coordinate system written as text has a token: `pf_coord_system_name` gives `"icrs"`,
`"galactic"`, `"ecliptic"` or `"supergalactic"` for a selector, and `"unknown"` for
`PF_COORD_UNKNOWN`. `pf_coord_system_from_name` reads one back, ignoring case and the blanks around
it, and understands astropy's frame names too: `icrs`, `galactic` and `supergalactic` are the same
words, and `barycentricmeanecliptic` names the ecliptic. **A token it does not know answers
`PF_COORD_UNKNOWN`** rather than stopping the program, so the caller reports the bad token in its
own words:

```fortran
system = pf_coord_system_from_name(token)
if (system == PF_COORD_UNKNOWN) then
    print '(a)', "unknown coordinate system: " // trim(token)
else
    call pf_sky_convert(lon, lat, system, PF_COORD_ICRS, ra, dec)
end if
```

## This is not the HEALPix declination frame

`parquet_healpix` and `parquet_sphere` name a **declination frame** — `PF_HP_DEC_NORTH` or
`PF_HP_DEC_SOUTH` — wherever a position becomes a unit vector: a sign convention for the vector's
third component, `theta = pi/2 - dec` or the mirrored `theta = pi/2 + dec`
([HEALPix](healpix.html#two-conventions-to-get-right) explains why both are in use). A coordinate
system is a different thing, a rotation of the whole sphere, and nothing on this page takes a frame:
a position is `(lon, lat)` as astronomers write it.

The two do not mix. A reflection and a rotation do not commute, so a declination held in the
mirrored convention converts to the wrong hemisphere's Galactic coordinates — a plausible position,
with no error anywhere. **Negate such a declination before converting it.** The geometry in the next
section needs no such care: a separation, an offset and a position angle are all unchanged by the
reflection.

## Angular separations, offsets and position angles

```fortran
d = pf_angdist_deg(ra1, dec1, ra2, dec2)                   ! degrees in, degrees out
call pf_offset_radec(ra0, dec0, pa_deg, sep_deg, ra, dec)  ! along the great circle
pa = pf_position_angle_deg(ra1, dec1, ra2, dec2)           ! north through east, in [0, 360)
```

None of the three takes a coordinate system: they work in whichever one the positions are written
in, provided both positions are in the same one.

**`pf_angdist_deg`** is the angle between two positions, computed in the frame where only the
difference of the right ascensions survives, as `atan2` of the cross and dot products rather than
`acos` of the dot product — so it keeps its accuracy near 0 and near 180 degrees. It is within
7.8e-15 degrees of a 60-digit evaluation across the seam at right ascension 0, both poles, and
coincident and antipodal pairs, and a right ascension may be any value: it is folded into one turn
first. **A position is exactly zero degrees from itself**, including when the two right ascensions
differ by whole turns and when both positions are one pole named by different right ascensions, so
`dist > 0` excludes self-matches safely. It is appreciably cheaper than converting both positions to
vectors and calling `parquet_healpix`'s `pf_angdist`, because the frame it works in removes one of
the four sine and cosine pairs (`bench/benchmark_healpix.sh --mode=dist` measures both on your
machine).

**`pf_offset_radec`** moves `sep_deg` along the great circle leaving `(ra0, dec0)` at position angle
`pa_deg`, measured from north through east, and **`pf_position_angle_deg`** recovers that angle, in
`[0, 360)`: both follow astropy's `directional_offset_by` and `position_angle`. **At a pole the local
frame follows the `ra0` you gave**, so an offset from the north pole at position angle `pa` lands at
right ascension `ra0 + 180 - pa`, and one from the south pole at `ra0 + pa`. A separation above 180
continues along the same great circle. **A coincident pair has position angle 0**, including two
labels of one pole.

For positions as unit vectors, [`parquet_sphere`](sphere.html#positions-vectors-and-where-the-frame-enters)'s
`pf_radec2vec` and `pf_vec2radec` convert between the two, naming the declination frame at that
interface.

## What is validated and what is not

Every procedure here is **total in its coordinates**, because a column read from a file can carry
nulls and a conversion that stopped the program on one would be of no use on the whole column:

- a NaN coordinate gives NaN results and raises no floating-point flag, so a program running with
  the exceptions unmasked can carry it through; screen the results with the validity mask you
  already hold;
- an infinite coordinate gives NaN results and raises `IEEE_INVALID`, as the sine of an infinite
  angle must;
- a latitude outside `[-90, 90]` is read as the direction it names: `(lon, 100)` is
  `(lon + 180, 80)`;
- a latitude of exactly ±90 is the pole, whatever the longitude, and a result at a pole has
  longitude 0.

What stops the program is a caller mistake that has no sensible reading:

- `pf_sky_convert` with a selector that is not one of the four systems, `PF_COORD_UNKNOWN`
  included, even when `from` and `to` are equal;
- `pf_coord_system_name` with an integer that is neither a system nor `PF_COORD_UNKNOWN`;
- `pf_offset_radec` with a `dec0` outside `[-90, 90]` — a centre beyond a pole turns the local north
  and east around and gives a plausible wrong point — or a negative `sep_deg`. A NaN argument
  gives NaN results.

`pf_coord_system_from_name` never stops the program: text is user data, and an unknown token answers
`PF_COORD_UNKNOWN`.

## Thread safety

The module holds no state: every procedure is `pure` and every matrix a constant, so anything here
may be called from any number of threads at once. Nothing on this page prints, and nothing reads a
setting.

## Limitations

- A conversion rotates a direction and nothing else: no proper motion, parallax, radial velocity,
  aberration or light deflection enters it.
- The ecliptic is the mean ecliptic and equinox of J2000 only, without nutation, and there is no
  argument naming another equinox.
- `real64` only. A `real32` column converts through `real(x, real64)` at the call.

## See also

- [Random points and geometry on the sphere](sphere.html) — positions to unit vectors and back in a
  declination frame you name, the Fibonacci grid, and random points in sky polygons and HEALPix
  masks.
- [Sphere pixelisation with `parquet_healpix`](healpix.html) — the two declination conventions, and
  the separation of two unit vectors.
- [Spatial neighbour search](spatial.html#search-on-the-sky) — the positions near a position, once
  you hold them.
