---
title: Celestial coordinate systems with parquet_skycoord
---

`parquet_skycoord` converts sky positions between the celestial coordinate systems a catalogue is
kept in — ICRS, Galactic, ecliptic, supergalactic and FK5 J2000 — by a named procedure, by two
selectors read at run time, or by a rotation prepared once, and carries the RA/Dec geometry that
needs no coordinate system at all: the angular separation of two positions, the position a
separation away at a position angle, that position angle back, a position moved by its proper
motion, a position as a unit vector, and a field projected onto the plane tangent at its centre. It
also writes positions as sexagesimal text and reads them back, and takes a heliocentric redshift
into the rest frame of the cosmic microwave background and back. Everything works in `real64`
degrees, and every conversion is `pure elemental`, so a whole column converts in one call.

```fortran
use parquet_skycoord
use iso_fortran_env, only : real64

real(real64) :: ra(3), dec(3), l(3), b(3), sep(3)

ra = [266.40499_real64, 10.68458_real64, 83.82208_real64]
dec = [-28.93617_real64, 41.26875_real64, -5.39111_real64]
call pf_icrs2gal(ra, dec, l, b)                                   ! the whole column at once
sep = pf_angdist_deg(ra, dec, 10.68458_real64, 41.26875_real64)   ! each one's distance from M31
```

`use parquet_skycoord` compiles 6 of this library's Fortran files and never reaches this library's
reader, its writer or its C++ bindings. Five of them are its own; the sixth is `parquet_utils`,
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
- **`PF_COORD_FK5`** — FK5 J2000 `(ra, dec)`, astropy's `FK5` at its default equinox. **FK5 here is
  FK5 J2000; precessing to another equinox is not provided.** It is ICRS turned by the frame bias of
  USNO Circular 179 and nothing else, so the two differ by at most 32 milliarcseconds on the sky.

**Each rotation is built from the three angles that define it** — the target system's north pole in
the system it is built from, and the target longitude of that system's north pole — so it is
orthonormal by construction, its inverse is its transpose rather than a second set of numbers, and
every matrix is a compile-time constant: no call builds one. The Galactic, ecliptic and FK5 angles
are referred to ICRS and so carry the ICRS frame bias inside them, which is what makes the answers
agree with astropy's `SkyCoord` to rounding rather than differ from it by some 20 milliarcseconds —
an error too small to look like a bug.

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
call pf_icrs2fk5(ra, dec, ra_fk5, dec_fk5)   ! ICRS -> FK5 J2000
call pf_fk52icrs(ra_fk5, dec_fk5, ra, dec)   ! FK5 J2000 -> ICRS
```

All ten take degrees and give degrees: the output longitude in `[0, 360)`, the output latitude in
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
procedure it calls that procedure, so the two answer alike to within a few ulp — they are two call
sites of one kernel, and a compiler is free to inline it at one and not the other; for a pair that
has none — Galactic and ecliptic, ecliptic and supergalactic, and FK5 J2000 and any system but
ICRS, either way round — it applies that pair's own compile-time matrix, never two rotations in
turn. **From a system to itself, the input comes back unchanged**, by copy and before any
arithmetic: a longitude of `-10` stays `-10`, so the `[0, 360)` range applies to a real
conversion only. `pf_sky_convert` is `pure elemental` like the named procedures.

A coordinate system written as text has a token: `pf_coord_system_name` gives `"icrs"`,
`"galactic"`, `"ecliptic"`, `"supergalactic"` or `"fk5"` for a selector, and `"unknown"` for
`PF_COORD_UNKNOWN`. `pf_coord_system_from_name` reads one back, ignoring case and the blanks around
it, and understands astropy's frame names too: `icrs`, `galactic`, `supergalactic` and `fk5` are the
same words, and `barycentricmeanecliptic` names the ecliptic. **A token it does not know answers
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

## A rotation prepared once

```fortran
type(pf_sky_rotation) :: rot

call rot%init(PF_COORD_ICRS, PF_COORD_GALACTIC)   ! the two systems, read once
call rot%apply(ra, dec, l, b)                     ! elemental over whole columns
```

`pf_sky_rotation` is `pf_sky_convert` with the two selectors read once rather than per position.
`%init(from, to)` checks them and takes the pair's matrix, the one `pf_sky_convert` rotates by;
`%apply` then rotates any number of positions, `pure elemental` like the named procedures and with
their rules. It answers what `pf_sky_convert` answers to within a few units in the last place: the
two call one routine from two places, which a compiler may round differently. From a system to
itself `%apply` hands back its input unchanged, as `pf_sky_convert` does.

`%init` may run again on the same object, replacing the rotation it held, and `%is_init()` says
whether it has run. **`%apply` before any `%init` stops the program**, as does `%init` with a
selector that is not one of the five systems. Once prepared the object is only read, so one built
before a parallel region serves every thread of it.

What the object saves is the reading of two selectors per position, which is small beside a
rotation's sines and cosines: over a column it is a few per cent faster than `pf_sky_convert`, no
more (`bench/benchmark_skycoord.sh` measures both). Its use is one rotation, checked once, carried
through a program whose systems come from its configuration.

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
7.8e-15 degrees of a 60-digit evaluation on the edge cases it is pinned to — the seam at right
ascension 0, both poles, and coincident and antipodal pairs — and within 2.8e-14 degrees over a
sweep of separations from a billionth of a degree to 180, and a right ascension may be any value:
it is folded into one turn first. **A position is exactly zero degrees from itself**, including
when the two right ascensions differ by whole turns and when both positions are one pole named by
different right ascensions, so `dist > 0` excludes self-matches safely. It is appreciably cheaper
than converting both positions to vectors and calling `parquet_healpix`'s `pf_angdist`, because the
frame it works in removes one of the four sine and cosine pairs
(`bench/benchmark_healpix.sh --mode=dist` measures both on your machine).

**`pf_offset_radec`** moves `sep_deg` along the great circle leaving `(ra0, dec0)` at position angle
`pa_deg`, measured from north through east, and **`pf_position_angle_deg`** recovers that angle, in
`[0, 360)`: both follow astropy's `directional_offset_by` and `position_angle`. **At a pole the local
frame follows the `ra0` you gave**, so an offset from the north pole at position angle `pa` lands at
right ascension `ra0 + 180 - pa`, and one from the south pole at `ra0 + pa`. A separation above 180
continues along the same great circle. **A coincident pair has position angle 0**, including two
labels of one pole.

**The position angle of a very close pair carries few digits.** The formula's two terms cancel as
the separation falls, so about one digit goes per decade below a degree: a pair a milliarcsecond
apart carries a handful of digits of position angle and a pair a microarcsecond apart almost none.
That is the quantity, not this implementation — astropy's `position_angle` behaves the same way —
and the separation of the same pair is unaffected.

### Positions as unit vectors

```fortran
call pf_radec2unit(lon, lat, v)     ! v(3) = (cos(lat)cos(lon), cos(lat)sin(lon), sin(lat))
call pf_unit2radec(v, lon, lat)     ! back again, from a vector of any length
```

A position and its unit vector, frame-free like everything else here. `v` need not have unit length
coming back: it is scaled by its largest component first, so a direction of any magnitude reads. A
latitude of exactly ±90 is the pole `(0, 0, ±1)` whatever the longitude says, and a pole reads back
with longitude 0.

These are the same two conversions
[`parquet_sphere`](sphere.html#positions-vectors-and-where-the-frame-enters) publishes as
`pf_radec2vec` and `pf_vec2radec` under `PF_HP_DEC_NORTH`, **to the bit**, and the pair here takes
no frame argument: a declination held in the mirrored convention is negated before it is used, the
rule this module states for its rotations. Use these when you are already in this module — they
cost nothing beyond the six files it compiles — and `parquet_sphere`'s when you want to name the
declination frame at that interface, or are using the rest of that module anyway.

### The tangent plane

```fortran
call pf_radec2tan(ra, dec, ra0, dec0, x, y, [pa_deg])   ! degrees -> standard coordinates
call pf_tan2radec(x, y, ra0, dec0, ra, dec, [pa_deg])   ! and back
```

The gnomonic projection — FITS's `TAN` — of a position onto the plane tangent at a field centre
`(ra0, dec0)`: **`x` east, `y` north, both in degrees**, with a point `sep` degrees from the centre
landing at radius `tan(sep)` in radians written in degrees. A great circle is a straight line on
this plane, which is what makes it the projection a tiling or fibre-positioning pipeline works in.
Both are `pure elemental`, so one call projects a whole target list about one centre.

**`pa_deg` turns the axes**: given, `+y` points along position angle `pa_deg` — north through east,
`pf_offset_radec`'s convention — and `+x` 90 degrees east of it, so a point at position angle
`pa_deg` from the centre lands on `+y` exactly. Omitted, `+y` is north. At a pole the local frame
follows the `ra0` you gave, as `pf_offset_radec` does.

**A position in the far hemisphere has no image**: `x` and `y` are NaN, raising no flag, so screen
with `x /= x`. The boundary is the 90 degrees where the projection's own denominator changes sign;
just inside it the image is correct and enormous, so cut a field of view with `pf_angdist_deg`
rather than with a radius on the plane. A `dec0` outside `[-90, 90]` stops the program, as
`pf_offset_radec`'s centre does.

```fortran
real(real64) :: x(n), y(n)

call pf_radec2tan(ra, dec, field_ra, field_dec, x, y)       ! the whole target list at once
where (x /= x) ...                                          ! anything in the far hemisphere
```

## Proper motion

```fortran
call pf_apply_pm(ra, dec, pm_ra, pm_dec, dt_years, ra_out, dec_out)
```

**`pm_ra` is the proper motion in right ascension times `cos(dec)`**, in mas/yr: Gaia's `pmra`,
astropy's `pm_ra_cosdec`. `pm_dec` is the proper motion in declination, also in mas/yr, and
`dt_years` the interval in the same years, negative to move back. The cosine matters: a motion
written without it moves a position near a pole several times too far in right ascension, and near
the equator, where the cosine is about 1, nothing shows it.

The two rates are the step's components on the tangent plane, east through `pm_ra` and north
through `pm_dec`, and the position moves `hypot(pm_ra, pm_dec) * |dt_years|` along the great circle
leaving it in that direction — the circle `pf_offset_radec` would move it along at the same
position angle. At a pole the motion is read in the local frame of the `ra` given, as
`pf_offset_radec` reads a position angle. No time, or no motion, gives the position back, its right
ascension wrapped into `[0, 360)`, and gives it back whatever the other rate says. `pf_apply_pm` is
`pure elemental`: one call moves a whole catalogue to another epoch.

**It is a step along a great circle, not rigorous space motion.** Parallax, radial velocity and
light travel time do not enter it, and the motion is the one at the starting position, held fixed.
That is the same great circle astropy's `apply_space_motion` follows for a source with neither
distance nor radial velocity, which moves in a straight line through space instead and so covers
`atan(s)` of the arc where this covers `s`, in radians: the two differ by `s**3/3`, about 2
microarcseconds for a step of an arcminute and about 6 milliarcseconds for a quarter of a degree. To
carry a position to a new epoch and back, use the motion at the new epoch for the way back: a great
circle's position angle turns as it crosses the meridians, so the components held at the start no
longer point along it.

A NaN argument — the proper motion of a source that has none in a Gaia column — gives NaN results
without raising a flag, and a `dec` outside `[-90, 90]` stops the program, as `pf_offset_radec`'s
centre does.

## Sexagesimal text

Signatures in this section and the next show optional arguments in square brackets, with the comma
outside the bracket: `pf_ra2str(ra, text, [sep], [precision])` means `sep` and `precision` may be
omitted. The brackets are not Fortran, and a block containing them is a description rather than a
runnable example.

```fortran
call pf_ra2str(ra, text, [sep], [precision])           ! "10:21:30.550"
call pf_dec2str(dec, text, [sep], [precision])         ! "+41:16:09.00"
call pf_radec2str(ra, dec, text, [sep], [precision])   ! "10:21:30.550 +41:16:09.00"
call pf_str2ra(text, ra, ok)
call pf_str2dec(text, dec, ok)
call pf_str2radec(text, ra, dec, ok)
```

A catalogue often keeps its positions as text, and more often as two text columns than as one, so
each angle has its own writer and reader and the pair is the two together.

**`text` is `character(len=:), allocatable`** in all three writers, which allocate it to the length
they need; a fixed-length declaration does not compile. The writers are `pure` but not `elemental`
— an allocatable output cannot be — so a column is written in a loop, where the readers below
take one in a single call.

**Writing.** A right ascension is wrapped into `[0, 360)` and written `hh:mm:ss.sss`, a declination
`+dd:mm:ss.ss` with its sign always, every field below ten zero-padded. **`precision` is the number
of decimals of a declination's arcseconds, 0 to 9, default 2, and a right ascension's seconds carry
one more** — a second of time is 15 arcseconds, so one `precision` gives both angles the same
resolution on the sky, and two columns written separately read like the pair written together. The
seconds are rounded once, to the nearest, ties to even; seconds that round to 60 carry into the
minutes and on into the hours or degrees, and 24 hours wrap to `00`. `sep` goes between the fields:
`":"` (the default) or `" "`, or `"hms"` for the lettered form, `10h21m30.550s` and
`+41d16m09.00s`. The sign is `-` only below zero, so `-0.0` is `+00:00:00.00`, while `-1e-10` is
`-00:00:00.00`; a declination beyond 90 is written as given. A NaN coordinate is the text `nan`, in
its own half of a pair.

The text is what astropy's `Angle.to_string` writes for the same separator and number of decimals,
with `pad` on and, for a declination, `alwayssign`, but in three places: astropy carries seconds
into the next minute from `60 - 10**-p` upward, `p` the decimals written, rather than from where
they round to 60; it writes a right ascension that rounds up to 24 hours as `24:00:00.000` rather
than `00:00:00.000`; and it writes `-0.0` with a `-`.

**Reading.** A reader takes **exactly three fields**, separated by colons (`10:21:30.55`), by blanks
(`10 21 30.55`) or by the unit letters (`10h21m30.55s`, `+41d16m09.0s`: `h` or `d`, then `m`, then a
closing `s`, in either case, with a blank allowed after each letter). Hours have one or two digits
and degrees one to three; minutes and whole seconds have one or two, each below 60; the seconds
take any number of decimals. Only a declination takes a sign, `+` or `-`, which applies to the whole
angle, so `-00:30:00` is -0.5. Blanks around the text are ignored, and a pair is the two angles
separated by blanks, one comma, or both.

**The leading field is bounded too, and may reach its bound only exactly**: hours at most 24 and
degrees at most 90, each with zero minutes and zero seconds there. So `24:00:00` reads as 360 —
nothing is wrapped — while `24:00:00.001` and `25:00:00` do not, and `+90:00:00` reads as the pole,
which is what `pf_dec2str` writes for it, while `+90:00:00.01` and `+91:00:00` do not. A writer
takes a declination beyond a pole and writes it as given; a reader does not take it back.

**Text a reader cannot read sets `ok` to `.false.`** — a bare decimal number, two fields, a signed
right ascension, a field of 60, separators of two styles, a trailing letter too many — and never
stops the program: the text is user data, and a lenient reading would turn a malformed field into a
plausible wrong position. **The outputs are not assigned then, so do not read them.** A column of
decimal degrees held as text is `pf_from_str`'s, in `parquet_utils`. The readers are
`pure elemental`, so a whole column of text reads in one call:

```fortran
character(len=16) :: ra_text(1000)
real(real64) :: ra(1000)
logical :: ok(1000)

call pf_str2ra(ra_text, ra, ok)     ! ok(i) says whether ra(i) was read
```

**Fields.** Between an angle and its text sit its fields:

```fortran
call pf_deg2hms(deg, h, m, s)          ! right ascension -> hours, minutes, seconds of time
call pf_deg2dms(deg, sgn, d, m, s)     ! declination -> sign, degrees, arcminutes, arcseconds
ra = pf_hms2deg(h, m, s)               ! 15*h + m/4 + s/240
dec = pf_dms2deg(sgn, d, m, s)         ! d + m/60 + s/3600, negated for a negative sgn
```

**The sign is its own argument**, because a declination between -1 and 0 has no degrees to carry
it: `-0.5` is `sgn = -1, d = 0, m = 30, s = 0`. `sgn` is -1 below zero and +1 otherwise, for both
zeros alike. **Nothing is rounded**: `s` carries the angle's whole precision, and rounding for
display is the writers' work. The split itself is exact to a rounding of the seconds — an angle
whose whole seconds sit within one rounding of a boundary can split as the next second with a zero
fraction rather than the previous one with a fraction of `0.99999999999997` — and either way the
fields rejoin to the same angle. `pf_deg2hms` wraps its angle into `[0, 360)` first; `pf_deg2dms` splits
a declination beyond 90 as given, up to `huge(d)` degrees, past which no default `integer` holds its
degrees. The two joiners validate and wrap nothing, so `pf_hms2deg(24, 0, 0.0_real64)` is 360.

## The CMB rest frame

```fortran
z_cmb = pf_zhel2zcmb(lon, lat, z_hel, [system], [apex_lon], [apex_lat], [apex_v])
z_hel = pf_zcmb2zhel(lon, lat, z_cmb, [system], [apex_lon], [apex_lat], [apex_v])
```

`pf_zhel2zcmb` takes a heliocentric redshift into the rest frame of the cosmic microwave
background, and `pf_zcmb2zhel` brings one back:

```
1 + z_cmb = (1 + z_hel) / (gamma * (1 - (v/c) * cos(theta)))
```

where `theta` is the angle between the position and the dipole apex **as observed** — the direction
a catalogue holds — `v` the Sun's speed toward the apex and `gamma` the Lorentz factor of the whole
of `v`. Looking toward the apex the CMB-frame redshift is the larger, since the Sun's approach
blueshifts what it observes, and at 90 degrees from the apex the shift is the transverse Doppler
effect alone, `1 + z_cmb = (1 + z_hel) / gamma`. The two procedures are inverses: a redshift through
both comes back to rounding. `system` names the system of `(lon, lat)`, a `PF_COORD_*` selector
defaulting to `PF_COORD_ICRS`, so a catalogue in Galactic coordinates passes them as they are.
**The apex is always Galactic**, whatever `system` says, because that is how every dipole is
published.

**The dipole defaults to Planck 2018 results I** (Aghanim et al. 2020, A&A 641, A1): the apex at
Galactic `(264.021, 48.253)` and `v = 369.82` km/s, with the speed of light 299 792.458 km/s. A
measured dipole changes between papers, so each of the three may be given, alone or together, in
degrees and km/s. Both are `pure elemental` in the position and the redshift alike.

They are total in their coordinates and their redshift: a NaN argument gives a NaN; a redshift at or
below -1 is computed as the formula says rather than refused, and -1 exactly comes back as -1 in
either direction, a zero ratio of wavelengths being zero in every frame; an infinite one comes back
itself; and an `apex_v` of the speed of light or more, which has no Lorentz factor, gives a NaN.

## What is validated and what is not

Every procedure here is **total in its coordinates**, because a column read from a file can carry
nulls and a conversion that stopped the program on one would be of no use on the whole column:

- a NaN coordinate gives NaN results and raises no floating-point flag, so a program running with
  the exceptions unmasked can carry it through; screen the results with the validity mask you
  already hold. A split gives zero integer fields and a NaN `s`, and a writer the text `nan`;
- an infinite coordinate gives NaN results and raises `IEEE_INVALID`, as the sine of an infinite
  angle must, and the fields and text of a NaN;
- a latitude outside `[-90, 90]` is read as the direction it names: `(lon, 100)` is
  `(lon + 180, 80)`;
- a latitude of exactly ±90 is the pole, whatever the longitude, and a result at a pole has
  longitude 0;
- a position in the far hemisphere has no tangent-plane image: `pf_radec2tan` answers NaN there,
  raising no flag.

**An output argument must not be an input argument.** `call pf_icrs2gal(ra, dec, ra, dec)`, to
convert a column in place, is undefined behaviour — an `intent(out)` dummy aliased with an
`intent(in)` one — however plausible its answer looks today. Convert into separate arrays, or into
the same array through a temporary you own.

What stops the program is a caller mistake that has no sensible reading:

- `pf_sky_convert` with a selector that is not one of the five systems, `PF_COORD_UNKNOWN`
  included, even when `from` and `to` are equal, and `pf_zhel2zcmb` and `pf_sky_rotation`'s
  `%init` with such a selector;
- `pf_sky_rotation`'s `%apply` before any `%init`;
- `pf_coord_system_name` with an integer that is neither a system nor `PF_COORD_UNKNOWN`;
- a text writer with a `sep` other than `":"`, `" "` or `"hms"`, or a `precision` outside `[0, 9]`;
- `pf_offset_radec` with a `dec0` outside `[-90, 90]` — a centre beyond a pole turns the local north
  and east around and gives a plausible wrong point — or a negative `sep_deg`, and `pf_apply_pm`
  with a `dec` outside `[-90, 90]`, and `pf_radec2tan` and `pf_tan2radec` with a `dec0` outside it.
  A NaN argument to any of them gives NaN results.

`pf_coord_system_from_name` and the text readers never stop the program: text is user data, so an
unknown token answers `PF_COORD_UNKNOWN`, and text a reader cannot read sets `ok` to `.false.`. The
readers are the one validating layer here — everything else reads what it is given as the direction
it names.

## Thread safety

The module holds no state: every procedure is `pure` and every matrix a constant, so anything here
may be called from any number of threads at once. A `pf_sky_rotation` is state you hold: `%init`
writes it and `%apply` only reads it, so prepare it before a parallel region and apply it from every
thread. Nothing on this page prints, and nothing reads a setting.

## Limitations

- A conversion rotates a direction and nothing else: no proper motion, parallax, radial velocity,
  aberration or light deflection enters it. A proper motion is `pf_apply_pm`'s, as a step along a
  great circle.
- The ecliptic is the mean ecliptic and equinox of J2000 only, without nutation, and FK5 is FK5
  J2000 only: no argument names another equinox, and nothing precesses.
- The text readers take three-field sexagesimal angles in ASCII only: no two-field or decimal-hour
  forms, and no degree, minute or second symbols.
- The CMB rest frame is a boost by the dipole and nothing more, and Planck 2018's is the one dipole
  built in.
- The tangent plane is the gnomonic projection only: no other projection, no distortion terms, no
  WCS header.
- A proper motion cannot be rotated between coordinate systems: `pf_sky_convert` rotates a position,
  not a `(pmra, pmdec)` pair.
- `real64` only. A `real32` column converts through `real(x, real64)` at the call.

## See also

- [Random points and geometry on the sphere](sphere.html) — positions to unit vectors and back in a
  declination frame you name, the Fibonacci grid, and random points in sky polygons and HEALPix
  masks.
- [Sphere pixelisation with `parquet_healpix`](healpix.html) — the two declination conventions, and
  the separation of two unit vectors.
- [Spatial neighbour search](spatial.html#search-on-the-sky) — the positions near a position, once
  you hold them.
