#!/usr/bin/env python3
"""Generate the reference vectors that pin `parquet_skycoord`'s contract.

`parquet_skycoord` rotates sky positions between celestial coordinate systems and carries the
library's frame-free RA/Dec geometry. A wrong rotation is a plausible WRONG ANSWER rather than an
abort -- dropping the ICRS frame bias moves every ecliptic position by about 21 milliarcseconds,
far too little to look like a bug and far too much to be rounding -- so the vectors below are
derived here, from a 60-digit model of each system's DEFINITION, rather than read back out of a
Fortran run, which could only ever confirm that the implementation agrees with itself.

Emitted (COMMITTED to the repository, like every other generator's output):

  test/test_skycoord_vectors.f90  module `test_skycoord_vectors`: the coordinate-system selectors;
                                  rotations between every ordered pair of distinct systems;
                                  offsets and position angles; proper motions; unit vectors;
                                  tangent-plane projections and their inverses; sexagesimal
                                  fields, text written and text read; and CMB-frame redshifts,
                                  both ways.

Usage:  tools/generate_skycoord_reference.py [--check] [--self-test] [--verify-oracle]

  --check          regenerate into memory and compare with the committed file; exit 1 on any
                   difference.
  --self-test      re-derive the published anchors -- the angle table and the selectors against
                   src/parquet_skycoord.f90, the dipole and the speed of light against
                   src/parquet_skycoord_rotate.f90, the ecliptic row's closed form, every row
                   rebuilding its definition, FK5 J2000's pole and its Galactic pair, orthonormality,
                   round trips, the pole rules, the offset's pole convention, the proper motion's
                   closed forms and its reversal, the text's carries, ties and grammar, and the
                   redshift's identities -- and exit 1 if any of them fails.
  --verify-oracle  cross-check the emitted rows against astropy: every rotation through
                   `SkyCoord.transform_to`, every offset through `directional_offset_by` and
                   `separation`, every position angle through `position_angle`, every proper motion
                   through `directional_offset_by` and, where the motion is small, through
                   `apply_space_motion`, every field split through `Longitude.hms` and
                   `Angle.signed_dms`, every text written through `to_string` and every text read
                   through astropy's own parser, and every redshift's angle to the apex through
                   `separation`. Needs the workspace `astro` environment; the two modes above need
                   a bare `python3`, so CI's lint image can run them.

NEVER HAND-EDIT A VECTOR. A contract change is an edit to the model below plus a regeneration.

--------------------------------------------------------------------------------------------
THE SYSTEMS, EACH AS ASTROPY DEFINES IT

Every rotation is `M = Rz(180 - lon0) Ry(90 - pole_lat) Rz(pole_lon)` in astropy's
`rotation_matrix` convention (a rotation of the axes, not of the vector): `(pole_lon, pole_lat)` is
the target system's north pole in the source system and `lon0` the target longitude of the
source's north pole. It maps a source unit vector onto the target's.

  Galactic       astropy's FK5 J2000 -> Galactic -- the pole (192.8594812065348, 27.12825118085622)
                 and lon0 = 122.9319185680026, `galactic.py`'s literals -- composed with ICRS ->
                 FK5 J2000, the frame bias of USNO Circular 179 (eta0 = -19.9 mas, xi0 = 9.1 mas,
                 da0 = -22.9 mas; `Rx(-eta0) Ry(xi0) Rz(da0)`, `_icrs_to_fk5_matrix`).
  Ecliptic       `BarycentricMeanEcliptic` at its default equinox J2000 (TT), which is ERFA's
                 `ecm06` there: `Rx(eps) Rx(-eps) Rz(-psib) Rx(phib) Rz(gamb)` with the IAU 2006
                 Fukushima-Williams bias angles at t = 0, gamb = -0.052928", phib = 84381.412819",
                 psib = -0.041775", and the mean obliquity cancelling exactly. Its three-angle row
                 is therefore closed form: (270 + gamb, 90 - phib, 90 + psib) in degrees.
  Supergalactic  de Vaucouleurs' pole at Galactic (47.37, 6.32), lon0 = 90 (`Rz(90) Ry(90 - 6.32)
                 Rz(47.37)`, `supergalactic_transforms.py`), hung off Galactic as it is defined.
  FK5 J2000      `FK5` at its default equinox J2000: the frame bias alone, `Rx(-eta0) Ry(xi0)
                 Rz(da0)` with USNO Circular 179's three angles above (`icrs_to_fk5`, whose
                 precession from J2000 to J2000 is the identity).

src/parquet_skycoord.f90 carries the Galactic, ecliptic and FK5 rows ICRS-referred -- recovered here
from the composite matrix at 60 digits, so the frame bias is inside them -- and the supergalactic
row Galactic-referred, exactly as defined. `--self-test` holds each of its literals to the double
nearest the model's value.

THE FK5 ROW IS NEVER RECOVERED FROM A DOUBLE-PRECISION MATRIX. The FK5 pole sits 21.88 mas from the
ICRS pole, so a pole longitude and a lon0 recovered from a rounded matrix carry about eight digits;
recovered here from the 60-digit bias matrix, the row rebuilds that matrix to the working precision,
and in doubles the library's three-angle product agrees with astropy to a few ulp.

PROPER MOTION IS A STEP ALONG A GREAT CIRCLE. `pf_apply_pm` resolves `(pm_ra, pm_dec)` -- `pm_ra`
being mu_alpha * cos(dec), Gaia's `pmra` and astropy's `pm_ra_cosdec` -- into a position angle and a
separation `hypot(pm_ra, pm_dec) * |dt|`, half a turn round for a negative interval, and hands both
to `pf_offset_radec`. astropy's `apply_space_motion` moves a source with no distance and no radial
velocity in a straight line through space instead, which follows the same great circle but covers
`atan(s)` of it where this covers `s` (radians); the oracle compares the two through that mapping
where the motion is small enough for nothing else to differ, and every row through
`directional_offset_by`.

WHAT IS EXACT AND WHAT IS NOT

Every value is computed in `decimal` at 60 significant digits plus guard digits and correctly
rounded to double, so `--check` gives the same answer on every host. The library builds its
matrices from the rounded angles in double precision and computes with libm, so the suite asserts
every row to a tolerance on the sky rather than bit for bit.

THE TEXT IS EXACT, AND ITS FIXTURES ARE CHOSEN SO THAT IT CAN BE

The sexagesimal model works on the input double's exact value as a `Fraction`: the fraction of a
degree times the output units in a degree, rounded to nearest with ties to even, then integer
fields. The WRITERS round from that exact product -- the library resolves a tie with a two-product
residual -- so a fixture on a knife edge is a fixture worth having, and several are in the lists
below. The unrounded FIELD SPLITS are different: they take the whole part of a double product, and
a value whose exact product lies within that one rounding of a whole second could legitimately
split either way, so `stable_split` refuses such a fixture rather than freezing a coin toss. The
emitted text is then asserted character for character. Text read back is modelled by a separate
grammar written as regular expressions, so the reader is checked against a second transcription of
its own rules, not against itself.
"""

import argparse
import decimal
import math
import pathlib
import re
import sys
from fractions import Fraction

TOOLS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

import generate_random_golden_vectors as rgv  # noqa: E402
import generate_sphere_reference as sph  # noqa: E402

REPO_ROOT = TOOLS.parent
OUT_PATH = REPO_ROOT / "test" / "test_skycoord_vectors.f90"
SRC_PATH = REPO_ROOT / "src" / "parquet_skycoord.f90"
ROTATE_PATH = REPO_ROOT / "src" / "parquet_skycoord_rotate.f90"

D = decimal.Decimal
DPI = rgv.DPI
dctx = rgv.dctx
dsin_cos = rgv.dsin_cos
datan2 = rgv.datan2
deg2rad = sph.deg2rad
rad2deg = sph.rad2deg
dec_sin_cos = sph.dec_sin_cos
radec_unit = sph.radec_unit
unit_radec = sph.unit_radec
dangle = sph.dangle

# ---------------------------------------------------------------------------------------------
# The contract constants. `self_test` reads every one back from src/parquet_skycoord.f90.
# ---------------------------------------------------------------------------------------------

UNKNOWN = 0
SELECTORS = {"ICRS": 1, "GALACTIC": 2, "ECLIPTIC": 3, "SUPERGALACTIC": 4, "FK5": 5}
ICRS, GAL, ECL, SGAL, FK5 = (SELECTORS[k] for k in ("ICRS", "GALACTIC", "ECLIPTIC", "SUPERGALACTIC", "FK5"))
SYSTEMS = [ICRS, GAL, ECL, SGAL, FK5]

MAS = D(3600000)
ARCSEC = D(3600)

# astropy 8.0.1's own literals, as written in its source. The quotients are formed in the working
# context: at module level the default one would keep 28 digits of them.
GAL_FK5_POLE = (D("192.8594812065348"), D("27.12825118085622"))
GAL_FK5_LON0 = D("122.9319185680026")
with dctx():
    BIAS_ETA0 = D("-19.9") / MAS
    BIAS_XI0 = D("9.1") / MAS
    BIAS_DA0 = D("-22.9") / MAS
    ECL_GAMB = D("-0.052928") / ARCSEC
    ECL_PHIB = D("84381.412819") / ARCSEC
    ECL_PSIB = D("-0.041775") / ARCSEC
SGAL_POLE = (D("47.37"), D("6.32"))
SGAL_LON0 = D(90)

# pf_zhel2zcmb's defaults: Planck 2018 results I (Aghanim et al. 2020, A&A 641, A1), the apex in
# Galactic coordinates, and the SI speed of light, both speeds in km/s. `self_test` reads all four
# back from src/parquet_skycoord_rotate.f90.
CMB_APEX_LON = D("264.021")
CMB_APEX_LAT = D("48.253")
CMB_APEX_V = D("369.82")
C_KMS = D("299792.458")

# ---------------------------------------------------------------------------------------------
# Rotations at 60 digits
# ---------------------------------------------------------------------------------------------


def rot(axis, angle_deg):
    """astropy's `rotation_matrix(angle, axis)`: a rotation of the AXES by `angle_deg` degrees.

    In the working context throughout: even a unary minus rounds a Decimal to the context's
    precision, and outside it that is the default 28 digits."""
    with dctx():
        s, c = dsin_cos(deg2rad(angle_deg))
        z, o = D(0), D(1)
        if axis == "x":
            return [[o, z, z], [z, c, s], [z, -s, c]]
        if axis == "y":
            return [[c, z, -s], [z, o, z], [s, z, c]]
        return [[c, s, z], [-s, c, z], [z, z, o]]


def mm(a, b):
    with dctx():
        return [[a[i][0] * b[0][j] + a[i][1] * b[1][j] + a[i][2] * b[2][j] for j in range(3)] for i in range(3)]


def mt(a):
    return [[a[j][i] for j in range(3)] for i in range(3)]


def mv(m, v):
    with dctx():
        return [m[i][0] * v[0] + m[i][1] * v[1] + m[i][2] * v[2] for i in range(3)]


def identity():
    return [[D(1) if i == j else D(0) for j in range(3)] for i in range(3)]


def three_angle(pole_lon, pole_lat, lon0):
    """`Rz(180 - lon0) Ry(90 - pole_lat) Rz(pole_lon)`: the library's construction, at 60 digits."""
    with dctx():
        return mm(mm(rot("z", 180 - D(lon0)), rot("y", 90 - D(pole_lat))), rot("z", D(pole_lon)))


def row_of(m):
    """The three angles of a rotation: the target pole in the source system (the matrix's third
    ROW), and the target longitude of the source pole (its third COLUMN)."""
    with dctx():
        pole_lon = rad2deg(datan2(m[2][1], m[2][0]))
        if pole_lon < 0:
            pole_lon += 360
        pole_lat = rad2deg(datan2(m[2][2], (m[2][0] * m[2][0] + m[2][1] * m[2][1]).sqrt()))
        lon0 = rad2deg(datan2(m[1][2], m[0][2]))
        if lon0 < 0:
            lon0 += 360
        return pole_lon, pole_lat, lon0


_CACHE = {}


def icrs_to_fk5():
    """ICRS -> FK5 J2000: the frame bias, `Rx(-eta0) Ry(xi0) Rz(da0)` (`_icrs_to_fk5_matrix`)."""
    return mm(mm(rot("x", -BIAS_ETA0), rot("y", BIAS_XI0)), rot("z", BIAS_DA0))


def gal_from_fk5():
    """FK5 J2000 -> Galactic: astropy's own three angles, as `galactic.py` writes them."""
    return three_angle(GAL_FK5_POLE[0], GAL_FK5_POLE[1], GAL_FK5_LON0)


def definition(system):
    """The matrix taking an ICRS unit vector to `system`'s, built from the definition."""
    if system in _CACHE:
        return _CACHE[system]
    with dctx():
        if system == ICRS:
            m = identity()
        elif system == FK5:
            m = icrs_to_fk5()
        elif system == GAL:
            m = mm(gal_from_fk5(), definition(FK5))
        elif system == ECL:
            m = mm(mm(rot("z", -ECL_PSIB), rot("x", ECL_PHIB)), rot("z", ECL_GAMB))
        elif system == SGAL:
            m = mm(gal_to_sgal(), definition(GAL))
        else:
            raise ValueError(system)
    _CACHE[system] = m
    return m


def gal_to_sgal():
    return three_angle(SGAL_POLE[0], SGAL_POLE[1], SGAL_LON0)


def pair_matrix(frm, to):
    """The matrix taking a `frm` unit vector to `to`'s."""
    return mm(definition(to), mt(definition(frm)))


def angle_table():
    """The rows src/parquet_skycoord.f90 carries: name -> (pole_lon, pole_lat, lon0)."""
    return {
        "gal": row_of(definition(GAL)),
        "ecl": row_of(definition(ECL)),
        "sgal": (SGAL_POLE[0], SGAL_POLE[1], SGAL_LON0),
        "fk5": row_of(definition(FK5)),
    }


def convert(frm, to, lon, lat):
    """The library's conversion of one position: the input's unit vector with the pole rule, one
    rotation, and back with the output's rules -- `lon` in `[0, 360)` and a pole's `lon` 0."""
    v = radec_unit(lon, lat)
    return unit_radec(mv(pair_matrix(frm, to), v))


# ---------------------------------------------------------------------------------------------
# Offsets and position angles (moved here from tools/generate_sphere_reference.py with the two
# procedures, unchanged)
# ---------------------------------------------------------------------------------------------


def danint(x):
    """Fortran's `anint`: nearest integer, halves away from zero."""
    with dctx():
        return D(x).to_integral_value(rounding=decimal.ROUND_HALF_UP)


def offset_radec(ra0, dec0, pa, sep):
    """`pf_offset_radec`: `cos(sep)*c + sin(sep)*(cos(pa)*north + sin(pa)*east)`."""
    with dctx():
        sd0, cd0 = dec_sin_cos(dec0)
        sa, ca = dsin_cos(deg2rad(ra0))
        sp, cp = dsin_cos(deg2rad(pa))
        ss, cs = dsin_cos(deg2rad(sep))
        c = [cd0 * ca, cd0 * sa, sd0]
        north = [-(sd0 * ca), -(sd0 * sa), cd0]
        east = [-sa, ca, D(0)]
        v = [cs * c[j] + ss * (cp * north[j] + sp * east[j]) for j in range(3)]
        return unit_radec(v)


def position_angle(ra1, dec1, ra2, dec2):
    """`pf_position_angle_deg`, with its fold, its coincidence rule and the pole rule."""
    with dctx():
        dl = D(ra2) - D(ra1)
        dl = dl - 360 * danint(dl / 360)
        if dec1 == dec2 and (dl == 0 or abs(dec1) == 90.0):
            return D(0)
        sd1, cd1 = dec_sin_cos(dec1)
        sd2, cd2 = dec_sin_cos(dec2)
        sdl, cdl = dsin_cos(deg2rad(dl))
        y = sdl * cd2
        x = cd1 * sd2 - sd1 * cd2 * cdl
        if y == 0 and x == 0:
            return D(0)
        pa = rad2deg(datan2(y, x))
        if pa < 0:
            pa += 360
        if pa >= 360:
            pa = D(0)
        return pa


OFFSET_CENTRES = [(10.0, 20.0), (350.0, -45.0), (0.0, 90.0), (123.4, -90.0), (200.0, 89.999),
                  (75.0, -89.5), (720.5, 0.0)]
OFFSET_PAS = [0.0, 90.0, 180.0, 270.0, 33.3, -45.0, 400.0]
OFFSET_SEPS = [0.0, 1e-6, 0.5, 45.0, 90.0, 179.9, 180.0, 250.0]


def offset_rows():
    rows = []
    n = 0
    for ra0, dec0 in OFFSET_CENTRES:
        for pa in OFFSET_PAS:
            sep = OFFSET_SEPS[n % len(OFFSET_SEPS)]
            n += 1
            ra, dec = offset_radec(D(ra0), dec0, D(pa), D(sep))
            rows.append((ra0, dec0, pa, sep, ra, dec))
    return rows


def pa_rows(offsets):
    """Position angles of the offsets' results seen from their centres, and a few set pairs; a
    pair whose separation is within 0.01 degree of 0 or 180 is kept only when it coincides."""
    pairs = [(r[0], r[1], float(r[4]), float(r[5])) for r in offsets]
    pairs += [
        (10.0, 90.0, 250.0, 90.0),       # two labels of the north pole: coincident, 0 by rule
        (33.0, -12.0, 393.0, -12.0),     # one position written two turns apart: 0 by rule
        (0.0, 90.0, 45.0, 60.0),         # from the pole
        (45.0, 60.0, 0.0, 90.0),         # to the pole: north, 0
        (359.5, 10.0, 0.5, 10.0),        # across ra = 0
        (120.0, -30.0, 100.0, -35.0),
        (10.0, -89.0, 190.0, -89.0),     # across the south pole
    ]
    rows = []
    for ra1, dec1, ra2, dec2 in pairs:
        with dctx():
            sep = dangle(radec_unit(ra1, dec1), radec_unit(ra2, dec2))
            sep_deg = float(rad2deg(sep))
        coincident = position_angle(ra1, dec1, ra2, dec2) == 0 and sep_deg < 1e-30
        if not coincident and (sep_deg < 0.01 or sep_deg > 179.99):
            continue
        pa = position_angle(ra1, dec1, ra2, dec2)
        # How far rounding can move the angle: an ulp or two of a direction, over the separation.
        tol = 1e-11 if coincident else max(1e-11, 16 * 2.2e-16 / math.sin(math.radians(sep_deg)) * 180 / math.pi)
        rows.append((ra1, dec1, ra2, dec2, pa, tol))
    return rows


#: The separation sweep behind `utilities/skycoord.md`'s "within 2.8e-14 degrees over a sweep of
#: separations from a billionth of a degree to 180". That figure was stated on the page with
#: nothing in the repository behind it: `test_angdist_deg_reference` pins 15 EDGE CASES at 1e-12,
#: and the only sweep in the suite compares `pf_angdist_deg` with `pf_angdist` at 1e-11, which is
#: two implementations agreeing rather than either being accurate. These rows are the missing
#: half -- the whole separation range against the model.
#:
#: **Both ends of every pair are rounded to double BEFORE the reference angle is taken.** That is
#: the distinction `test_angdist_deg_reference`'s header draws: a separation of 1e-9 degrees
#: written as the difference of two numbers near 360 carries about 1e-5 relative INPUT error
#: before any formula runs, so a table built the other way round measures binary64 rather than
#: the library, and would pass against a much worse implementation.
SEP_SWEEP_BASES = [(0.0, 0.0), (123.25, 41.5), (359.9, -0.25), (0.0, 89.5), (45.0, 60.0)]
SEP_SWEEP_SEPS = [1e-9, 1e-8, 1e-6, 1e-4, 1e-2, 1.0, 30.0, 90.0, 150.0, 179.99, 180.0]
SEP_SWEEP_PAS = [0.0, 37.0, 143.0]


def sep_rows():
    """(ra1, dec1, ra2, dec2, sep): the exact angle between two directions both exact in double."""
    rows = []
    for ra0, dec0 in SEP_SWEEP_BASES:
        for sep in SEP_SWEEP_SEPS:
            for pa in SEP_SWEEP_PAS:
                far = offset_radec(D(ra0), dec0, D(pa), D(sep))
                ra1, dec1 = float(far[0]), float(far[1])
                with dctx():
                    exact = rad2deg(dangle(radec_unit(ra0, dec0), radec_unit(ra1, dec1)))
                rows.append((ra0, dec0, ra1, dec1, exact))
    return rows


# ---------------------------------------------------------------------------------------------
# Unit vectors and the tangent plane at 60 digits
# ---------------------------------------------------------------------------------------------

#: `(lon, lat)` for the unit-vector pair: both poles, whose vectors are exact; a signed zero
#: longitude; the seam; a latitude past 90, read as the direction it names; and an ordinary
#: position.
VEC_INPUTS = [(0.0, 90.0), (123.4, -90.0), (0.0, 0.0), (359.999, 12.5), (45.0, 100.0),
              (266.40499, -28.93617)]


def vec_rows():
    """(lon, lat, v(3), lon_back, lat_back): the vector, and the position it reads back as."""
    rows = []
    for lon, lat in VEC_INPUTS:
        v = radec_unit(lon, lat)
        back = unit_radec(v)
        rows.append((lon, lat, v, back[0], back[1]))
    return rows


def tangent_frame(ra0, dec0):
    """The tangent point's unit vector and its local north and east, `pf_offset_radec`'s frame."""
    with dctx():
        sd0, cd0 = dec_sin_cos(dec0)
        sa, ca = dsin_cos(deg2rad(ra0))
        return ([cd0 * ca, cd0 * sa, sd0], [-(sd0 * ca), -(sd0 * sa), cd0], [-sa, ca, D(0)])


def radec2tan(ra, dec, ra0, dec0, pa):
    """`pf_radec2tan`: the gnomonic projection about `(ra0, dec0)`, `x` east and `y` north in
    degrees, the axes turned so `+y` lies along position angle `pa`. None where the position is in
    the far hemisphere and has no image."""
    with dctx():
        c, north, east = tangent_frame(ra0, dec0)
        w = radec_unit(ra, dec)
        denom = c[0] * w[0] + c[1] * w[1] + c[2] * w[2]
        if denom <= 0:
            return None
        xi = (east[0] * w[0] + east[1] * w[1] + east[2] * w[2]) / denom
        eta = (north[0] * w[0] + north[1] * w[1] + north[2] * w[2]) / denom
        sp, cp = dsin_cos(deg2rad(pa))
        return rad2deg(xi * cp - eta * sp), rad2deg(xi * sp + eta * cp)


def tan2radec(x, y, ra0, dec0, pa):
    """`pf_tan2radec`: the direction of the point `(x, y)` on the plane tangent at `(ra0, dec0)`."""
    with dctx():
        c, north, east = tangent_frame(ra0, dec0)
        sp, cp = dsin_cos(deg2rad(pa))
        xi = deg2rad(D(x) * cp + D(y) * sp)
        eta = deg2rad(-(D(x) * sp) + D(y) * cp)
        return unit_radec([c[j] + xi * east[j] + eta * north[j] for j in range(3)])


#: Tangent points `(ra0, dec0, pa_deg)`: a mid-latitude field unrotated and at three rotations, a
#: pole-centred field, whose local frame follows `ra0`, a field on the seam and one at the equator.
TAN_CENTRES = [(33.0, 21.0, 0.0), (33.0, 21.0, 37.5), (33.0, 21.0, -90.0), (33.0, 21.0, 180.0),
               (250.0, 90.0, 0.0), (120.0, -90.0, 15.0), (359.9, 0.0, 45.0), (0.0, -12.0, 180.0)]
#: Where the projected positions sit, as `(position angle, separation)` from the tangent point: the
#: centre itself, the four cardinal directions, a very small offset, a wide one, and one past 90
#: degrees, which has no image.
TAN_OFFSETS = [(0.0, 0.0), (0.0, 1.0), (90.0, 1.0), (180.0, 0.5), (270.0, 2.5), (33.3, 1.0e-6),
               (120.0, 80.0), (45.0, 95.0)]


def tan_rows():
    """(ra, dec, ra0, dec0, pa, has_image, x, y, ra_back, dec_back).

    The position is the DOUBLE the offset rounds to, so the model projects what the library is
    handed; the inverse is taken from the emitted `(x, y)` doubles for the same reason, which makes
    it a statement about the inverse rather than a round trip through the forward."""
    rows = []
    for ra0, dec0, pa in TAN_CENTRES:
        for opa, sep in TAN_OFFSETS:
            ra, dec = offset_radec(D(ra0), dec0, D(opa), D(sep))
            ra, dec = float(ra), float(dec)
            xy = radec2tan(ra, dec, ra0, dec0, pa)
            if xy is None:
                rows.append((ra, dec, ra0, dec0, pa, False, 0.0, 0.0, 0.0, 0.0))
            else:
                x, y = float(xy[0]), float(xy[1])
                back = tan2radec(x, y, ra0, dec0, pa)
                rows.append((ra, dec, ra0, dec0, pa, True, x, y, back[0], back[1]))
    return rows


# ---------------------------------------------------------------------------------------------
# Proper motion at 60 digits
# ---------------------------------------------------------------------------------------------


def pm_step(pm_ra, pm_dec, dt):
    """`pf_apply_pm`'s step: the position angle, east through `pm_ra` and north through `pm_dec`, half
    a turn round for a negative `dt`, and the separation `hypot(pm_ra, pm_dec) * |dt|` in degrees,
    both Decimals. No motion at all has position angle 0 by rule: `atan2(0, 0)` is prohibited."""
    with dctx():
        a, d = D(pm_ra), D(pm_dec)
        pa = D(0) if a == 0 and d == 0 else rad2deg(datan2(a, d))
        if dt < 0:
            pa += 180
        return pa, (a * a + d * d).sqrt() * abs(D(dt)) / MAS


def apply_pm(ra, dec, pm_ra, pm_dec, dt):
    """`pf_apply_pm`: the step handed to `pf_offset_radec`, which reads it in the local frame of the
    position -- at a pole, the frame of the `ra` given."""
    pa, sep = pm_step(pm_ra, pm_dec, dt)
    return offset_radec(D(ra), dec, pa, sep)


#: Proper motions `(ra, dec, pm_ra, pm_dec, dt_years)`: a Gaia-sized motion; an east-only one at
#: declination 80, where the `cos(dec)` factor is 5.8; Barnard's star over a century; a negative
#: interval near the south pole; the seam at `ra = 0` crossed eastward and westward; both poles, read
#: in the frame of the `ra` given; no motion, and no time; a right ascension outside `[0, 360)`; a
#: step of 1.5 degrees; one of 20, which a great circle carries where a straight line in space
#: would not; and three at a pole -- a motion landing exactly ON one, a start a nanodegree off one,
#: and a motionless source at one, where the step's length is zero and `sin(s)/s` is its limit.
PM_INPUTS = [
    (10.0, 20.0, 5.0, -3.0, 10.0),
    (100.0, 80.0, 1000.0, 0.0, 10.0),
    (269.4520833, 4.6933647, -801.551, 10362.394, 100.0),
    (0.0, -89.9, 50.0, 50.0, -25.0),
    (359.9999, 0.0, 100.0, 0.0, 36.0),
    (0.0001, 10.0, -100.0, 0.0, 36.0),
    (45.0, 90.0, 100.0, 0.0, 10.0),
    (123.0, -90.0, 0.0, 100.0, 10.0),
    (50.0, 30.0, 0.0, 0.0, 10.0),
    (50.0, 30.0, 7.0, 8.0, 0.0),
    (200.0, -45.0, 300.0, -400.0, -50.0),
    (-30.0, 15.0, 20.0, 10.0, 5.0),
    (80.0, -70.0, -2500.0, -1000.0, 2000.0),
    (150.0, 60.0, 3.0e5, 4.0e5, 144.0),
    (10.0, 89.0, 0.0, 3.6e6, 1.0),
    (200.0, 89.999999999, 1.0, -1.0, 1.0),
    (77.0, 90.0, 0.0, 0.0, 5.0),
]


def pm_rows():
    """(ra, dec, pm_ra, pm_dec, dt, ra_out, dec_out)."""
    rows = []
    for ra, dec, pm_ra, pm_dec, dt in PM_INPUTS:
        ra_out, dec_out = apply_pm(ra, dec, pm_ra, pm_dec, dt)
        if float(ra_out) >= 360.0:
            raise SystemExit("proper motion %r: the output right ascension rounds to 360" % ((ra, dec, pm_ra, pm_dec, dt),))
        rows.append((ra, dec, pm_ra, pm_dec, dt, ra_out, dec_out))
    return rows


# ---------------------------------------------------------------------------------------------
# Sexagesimal fields and text, exact
# ---------------------------------------------------------------------------------------------

STYLE_COLON, STYLE_BLANK, STYLE_LETTERS = 1, 2, 3
STYLES = (STYLE_COLON, STYLE_BLANK, STYLE_LETTERS)
#: `huge(1)` degrees: from here no default integer holds a declination's whole degrees.
DMS_LIMIT = (1 << 31) - 1
HALF = Fraction(1, 2)


def ffloor(q):
    return q.numerator // q.denominator


def fwrap(x):
    """`pf_wrap_deg(x)` as an exact Fraction, for an input every compiler's `modulo` wraps exactly --
    whether it forms `fmod` or `a - floor(a/p)*p` -- which the golden inputs are chosen to be."""
    q = Fraction(x) % 360
    by_fmod = math.fmod(x, 360.0)
    if by_fmod < 0:
        by_fmod += 360.0
    by_floor = x - math.floor(x / 360.0) * 360.0
    for w in (by_fmod, by_floor):
        if (0.0 if w >= 360.0 else w) != float(q) or Fraction(float(q)) != q:
            raise SystemExit("the wrap of %r is not exact on every compiler: choose another input" % x)
    return q


def round_units(frac, per_deg):
    """`frac * per_deg` rounded to the nearest integer, ties to even: the writers' one rounding."""
    x = frac * per_deg
    n = ffloor(x)
    r = x - n
    if r > HALF or (r == HALF and n % 2 == 1):
        n += 1
    return n


def stable_split(frac, per_deg):
    """Whether the library's double `frac * per_deg` has the exact product's whole part."""
    return math.trunc(float(frac) * float(per_deg)) == ffloor(frac * per_deg)


def separators(style, letters):
    if style == STYLE_COLON:
        return ":", ":", ""
    if style == STYLE_BLANK:
        return " ", " ", ""
    return letters


def hms_fields(x):
    """`pf_deg2hms(x)`: `(h, m, s)`, `s` an exact Fraction."""
    w = fwrap(x)
    whole = ffloor(w)
    if not stable_split(w - whole, 240):
        raise SystemExit("pf_deg2hms(%r): a whole second on a knife edge; choose another input" % x)
    secs = w * 240
    total = ffloor(secs)
    return total // 3600, total % 3600 // 60, secs - (total - total % 60)


def dms_fields(x):
    """`pf_deg2dms(x)`: `(sgn, d, m, s)`, `s` an exact Fraction; `sgn` is -1 only below zero."""
    mag = abs(Fraction(x))
    d = ffloor(mag)
    if d >= DMS_LIMIT:
        raise SystemExit("pf_deg2dms(%r): past the integer degrees; not a golden input" % x)
    if not stable_split(mag - d, 3600):
        raise SystemExit("pf_deg2dms(%r): a whole arcsecond on a knife edge; choose another input" % x)
    arcsec = (mag - d) * 3600
    total = ffloor(arcsec)
    return (-1 if x < 0 else 1), d, total // 60, arcsec - (total - total % 60)


def ra_text(x, style, precision):
    """`pf_ra2str(x, text, sep, precision)`: the seconds of time to `precision + 1` decimals."""
    if math.isnan(x):
        return "nan"
    q = precision + 1
    w = fwrap(x)
    whole = ffloor(w)
    per_deg = 240 * 10 ** q
    units = round_units(w - whole, per_deg)
    if units >= per_deg:
        whole += 1
        units -= per_deg
    total = (whole * 240 + units // 10 ** q) % 86400
    a, b, c = separators(style, ("h", "m", "s"))
    return "%02d%s%02d%s%02d.%0*d%s" % (total // 3600, a, total % 3600 // 60, b, total % 60, q, units % 10 ** q, c)


def dec_text(x, style, precision):
    """`pf_dec2str(x, text, sep, precision)`: the arcseconds to `precision` decimals, the sign always."""
    if math.isnan(x) or math.isinf(x):
        return "nan"
    mag = abs(Fraction(x))
    whole = ffloor(mag)
    if whole >= DMS_LIMIT:
        return "nan"
    per_deg = 3600 * 10 ** precision
    units = round_units(mag - whole, per_deg)
    if units >= per_deg:
        whole += 1
        units -= per_deg
    secs = units // 10 ** precision
    a, b, c = separators(style, ("d", "m", "s"))
    text = "%s%02d%s%02d%s%02d" % ("-" if x < 0 else "+", whole, a, secs // 60, b, secs % 60)
    if precision > 0:
        text += ".%0*d" % (precision, units % 10 ** precision)
    return text + c


# The readers' grammar, written a second time as regular expressions: three fields in one style,
# minutes and whole seconds below 60, any number of decimals, blanks around the whole ignored.
_SECONDS = r"(\d{1,2})(\.\d*)?"
_RA_FORMS = [r"(\d{1,2}):(\d{1,2}):" + _SECONDS,
             r"(\d{1,2}) +(\d{1,2}) +" + _SECONDS,
             r"(\d{1,2})[hH] *(\d{1,2})[mM] *" + _SECONDS + r"[sS]"]
_DEC_FORMS = [r"([+-]?)(\d{1,3}):(\d{1,2}):" + _SECONDS,
              r"([+-]?)(\d{1,3}) +(\d{1,2}) +" + _SECONDS,
              r"([+-]?)(\d{1,3})[dD] *(\d{1,2})[mM] *" + _SECONDS + r"[sS]"]


def read_ra(text):
    """`pf_str2ra(text)`: the exact right ascension in degrees, or None where `ok` is false.

    The hours are at most 24, and 24 only with zero minutes and zero seconds: the documented exact
    turn reads as 360 degrees and nothing past it reads at all."""
    t = text.strip(" ")
    for form in _RA_FORMS:
        mt = re.fullmatch(form, t)
        if mt:
            h, m, s, frac = mt.groups()
            if int(m) >= 60 or int(s) >= 60:
                return None
            secs = Fraction(s + (frac or "").rstrip(".") or "0")
            if int(h) > 24 or (int(h) == 24 and (int(m) != 0 or secs != 0)):
                return None
            return 15 * int(h) + Fraction(int(m), 4) + secs / 240
    return None


def read_dec(text):
    """`pf_str2dec(text)`: the exact declination in degrees, or None where `ok` is false.

    The degrees are at most 90, and 90 only with zero minutes and zero seconds: a pole reads back
    from what `pf_dec2str` writes for it, and nothing beyond a pole reads."""
    t = text.strip(" ")
    for form in _DEC_FORMS:
        mt = re.fullmatch(form, t)
        if mt:
            sign, d, m, s, frac = mt.groups()
            if int(m) >= 60 or int(s) >= 60:
                return None
            secs = Fraction(s + (frac or "").rstrip(".") or "0")
            if int(d) > 90 or (int(d) == 90 and (int(m) != 0 or secs != 0)):
                return None
            value = int(d) + Fraction(int(m), 60) + secs / 3600
            return -value if sign == "-" else value
    return None


def read_pair(text):
    """`pf_str2radec(text)`: `(ra, dec)` exactly, or None -- every way of cutting the text into a
    right ascension, blanks and at most one comma, and a declination is tried, and every cut that
    reads must read the same."""
    t = text.strip(" ")
    found = set()
    for i in range(1, len(t)):
        for j in range(i + 1, len(t)):
            if re.fullmatch(r" +| *, *", t[i:j]):
                ra, dec = read_ra(t[:i]), read_dec(t[j:])
                if ra is not None and dec is not None:
                    found.add((ra, dec))
    if len(found) > 1:
        raise SystemExit("the pair %r reads two ways: %r" % (text, found))
    return found.pop() if found else None


# ---------------------------------------------------------------------------------------------
# pf_zhel2zcmb at 60 digits
# ---------------------------------------------------------------------------------------------


def cmb_factor(lon, lat, system, apex=None):
    """`D = gamma (1 - beta cos(theta))`, the Doppler factor exact for the OBSERVED direction.

    `theta` is the angle between the apex and the position as the caller gives it, so
    `1 + z_cmb = (1 + z_hel) / D`. The position is taken into Galactic by the definitions' own
    60-digit matrices; `apex` is `(lon, lat, v)`, Galactic, or the Planck 2018 defaults."""
    alon, alat, av = apex if apex is not None else (CMB_APEX_LON, CMB_APEX_LAT, CMB_APEX_V)
    with dctx():
        w = mv(pair_matrix(system, GAL), radec_unit(lon, lat))
        a = radec_unit(alon, alat)
        cth = w[0] * a[0] + w[1] * a[1] + w[2] * a[2]
        beta = D(av) / C_KMS
        g = 1 / (1 - beta * beta).sqrt()
        return g * (1 - beta * cth)


def zcmb(lon, lat, z, system, apex=None):
    """`pf_zhel2zcmb`: `(1 + z) / D - 1`."""
    with dctx():
        return (1 + D(z)) / cmb_factor(lon, lat, system, apex) - 1


def zhel(lon, lat, z, system, apex=None):
    """`pf_zcmb2zhel`: `(1 + z) D - 1`, the same factor the other way."""
    with dctx():
        return (1 + D(z)) * cmb_factor(lon, lat, system, apex) - 1


def default_boost():
    """`gamma (1 + v/c) - 1`: the redshift a source at the apex with `z_hel = 0` has in the CMB frame.

    At the apex `cos(theta)` is 1 and the two ways of writing the factor agree exactly:
    `1 / (gamma (1 - beta)) = gamma (1 + beta) = sqrt((1 + beta) / (1 - beta))`."""
    with dctx():
        beta = CMB_APEX_V / C_KMS
        return (1 + beta) / (1 - beta * beta).sqrt() - 1


# ---------------------------------------------------------------------------------------------
# Text and redshift rows
# ---------------------------------------------------------------------------------------------

#: Angles whose DOUBLE product with the output units a degree lands on a half unit while the exact
#: product does not, so the tie is not one: the rounding must be decided from the exact product,
#: and a writer resolving such a tie by parity alone prints every one of these ONE UNIT OUT in the
#: last digit. The first two are declinations at `precision = 0` and the next two at
#: `precision = 2`; the right ascensions are at `precision = 0`, 2 and 3 in turn, and are inside
#: `[0, 360)` so that their wrap is exact on every compiler.
KNIFE_EDGE_DEC = [-3.3495833333333334, -3.4231944444444444, 12.281945833333333, 1.472501388888889]
KNIFE_EDGE_RA = [0.23854166666666668, 252.68295208333333, 28.259508125]
#: Right ascensions: the seam and both sides of it, a longitude below 0 and one past two turns,
#: a tie at three decimals (0.9375 s), and seconds of 59.9996 that carry into the hour, and into
#: 24 hours, which wrap to 0.
TEXT_RA_INPUTS = [
    0.0, 155.37729166666667, 359.99999999999994, 359.9999, -15.0, 725.5, 1.0e-9, 180.0,
    90.123456789, 266.40499, 0.00390625, (3600 + 59 * 60 + 59.9996) / 240.0,
    (23 * 3600 + 59 * 60 + 59.9996) / 240.0, (7 * 3600 + 5 * 60 + 3.25) / 240.0, 299.868,
] + KNIFE_EDGE_RA
#: Declinations: a sign between -1 and 0, both poles and one beyond, both zeros, a tie at two
#: decimals (28.125"), 59.9996" carrying into the minute and on into the degree, 59.9994", which
#: carries at two decimals and not at three (where astropy carries it anyway), a value that rounds
#: up to the pole, and one with four digits of degrees.
TEXT_DEC_INPUTS = [
    41.26916666666667, -0.5, -28.93617, 90.0, -90.0, 100.0, -1.0e-10, 0.0, -0.0, 0.0078125,
    (59 * 60 + 59.9996) / 3600.0, -(59.9996 / 3600.0), (59 * 60 + 59.9994) / 3600.0, 12.3456789,
    -45.000000001, 89.99999999999, 1234.5,
] + KNIFE_EDGE_DEC
#: (precision, style): the default, the coarsest and finest precisions, and every style.
TEXT_CASES = [(2, STYLE_COLON), (0, STYLE_BLANK), (3, STYLE_LETTERS), (9, STYLE_COLON)]

HMS_INPUTS = [0.0, 155.37729166666667, 359.99999999999994, -15.0, 725.5, 1.0e-9, 90.123456789,
              266.40499, 299.868, 0.00390625]
DMS_INPUTS = [41.26916666666667, -0.5, -28.93617, 90.0, -90.0, 100.0, -1.0e-10, 0.0, -0.0, 12.3456789,
              -45.000000001, 1.0e6, 2147483646.5]

#: Texts for `pf_str2ra`: every form it reads, and the shapes it must refuse.
READ_RA_TEXTS = [
    "10:21:30.55", "10 21 30.55", "10h21m30.55s", "10H21M30.55S", "10h 21m 30.55s", "  10:21:30.55  ",
    "5:1:2", "00:00:00", "23:59:59.9999999999", "24:00:00", "10:21:30.", "10:21:30.123456789012345678",
    "07:05:03.25", "10  21  30",
    "10:21", "155.3772", "+10:21:30", "-10:21:30", "5 6", "10:60:00", "10:21:60", "10:21 30", "10h21m30",
    "10d21m30s", "10:21:30x", "", "nan", "10:21:30:40", "10:021:30", "100:00:00", "10:21:.5", "1e1:00:00",
    "10::30", "10 : 21 : 30", "10h21m30.5s s",
    "25:00:00", "24:00:00.001", "24:00:01", "24:01:00", "99:00:00", "24h00m00.5s",
]
#: Texts for `pf_str2dec`.
READ_DEC_TEXTS = [
    "+41:16:09.00", "-00:30:00", "41 16 09", "+41d16m09s", "-41D16M09.5S", "+100:00:00", "-89:59:59.999",
    "+05:04:03", "0:0:0", "-0:0:0", "+41d 16m 09.25s", "+90:00:00", "-90:00:00", "+90d00m00.000s",
    "89:59:59.999999",
    "+-41:16:09", "++41:16:09", "+1000:00:00", "41:16", "+41:16:60", "- 41:16:09", "41h16m09s",
    "41:16:09.5.5", "+41:16:09 ", "-", "41:16:09s",
    "+91:00:00", "-91:30:00", "+90:00:00.01", "+90:00:01", "+90:01:00", "-90:00:00.001", "+100:30:00",
    "+90d00m00.5s",
]
#: Texts for `pf_str2radec`.
READ_PAIR_TEXTS = [
    "10:21:30.55 +41:16:09.0", "10:21:30.55,+41:16:09.0", "10:21:30.55 , +41:16:09.0",
    "10 21 30.55 +41 16 09.0", "10h21m30.55s +41d16m09.0s", "10h 21m 30.55s, -41d 16m 09.0s",
    "10:21:30.55 41:16:09", "  23:59:59.9 -00:00:01  ",
    "10:21:30.55", "10:21:30.55,,+41:16:09", "10:21:30.55+41:16:09", "10:21:30.55 +41:16:09 x",
    "+10:21:30.55 +41:16:09", ", 10:21:30 +41:16:09", "10:21:30.55 , , +41:16:09",
]

#: (system, lon, lat): the apex and the antapex in Galactic, and a position in every system.
ZCMB_POSITIONS = [
    (GAL, 264.021, 48.253), (GAL, 84.021, -48.253), (ICRS, 155.0, 41.0), (ICRS, 0.0, 90.0),
    (ICRS, 266.40499, -28.93617), (ECL, 100.0, 20.0), (SGAL, 200.0, -10.0), (GAL, 30.0, 0.0),
    (FK5, 155.0, 41.0),
]
#: Heliocentric redshifts, one at -1 and one below it computed as the formula says.
ZCMB_Z = [0.0, 0.01, 0.5, 3.0, -1.0, -2.0]
#: Explicit dipoles `(apex_lon, apex_lat, apex_v)`: another apex, the defaults written out, no
#: motion at all, and a negative speed, which is motion toward the antapex.
ZCMB_APEXES = [(270.0, 30.0, 600.0), (264.021, 48.253, 369.82), (10.0, -60.0, 0.0), (180.0, 0.0, -300.0)]


def text_rows():
    ras = [(x, p, st, ra_text(x, st, p)) for x in TEXT_RA_INPUTS for p, st in TEXT_CASES]
    decs = [(x, p, st, dec_text(x, st, p)) for x in TEXT_DEC_INPUTS for p, st in TEXT_CASES]
    return ras, decs


def read_rows():
    ras = [(t, read_ra(t)) for t in READ_RA_TEXTS]
    decs = [(t, read_dec(t)) for t in READ_DEC_TEXTS]
    pairs = [(t, read_pair(t)) for t in READ_PAIR_TEXTS]
    return ras, decs, pairs


def zcmb_rows():
    """(system, lon, lat, z, apex or None, z_cmb, z_hel).

    The last two are the same input read both ways: `z_cmb` is `pf_zhel2zcmb` of the row's `z`, and
    `z_hel` is `pf_zcmb2zhel` of that same `z` -- the inverse pinned in its own right rather than
    through a round trip, which a pair of matching errors would survive."""
    rows = [(s, lon, lat, z, None, zcmb(lon, lat, z, s), zhel(lon, lat, z, s))
            for s, lon, lat in ZCMB_POSITIONS for z in ZCMB_Z]
    for apex in ZCMB_APEXES:
        for s, lon, lat in ((ICRS, 155.0, 41.0), (GAL, apex[0], apex[1])):
            rows.append((s, lon, lat, 0.05, apex, zcmb(lon, lat, 0.05, s, apex), zhel(lon, lat, 0.05, s, apex)))
    return rows


# ---------------------------------------------------------------------------------------------
# Rotation rows
# ---------------------------------------------------------------------------------------------

#: Positions converted out of every system into every other. Both poles, twice each with different
#: longitudes (the pole rule makes the pair convert identically); a longitude below 0 and one two
#: turns up; a latitude beyond 90, which names the direction (lon + 180, 180 - lat); two positions
#: a hair from a pole; and ordinary ones.
ROT_INPUTS = [
    (0.0, 0.0), (0.0, 90.0), (123.4, 90.0), (45.0, -90.0), (300.0, -90.0),
    (266.40499, -28.93617), (192.859508, 27.128336), (359.9, 0.1), (-10.0, 30.0), (725.5, -60.0),
    (10.0, 100.0), (300.0, 89.9999), (60.0, -89.99999), (180.0, 45.0), (90.0, -30.0),
    (17.3, 62.1), (233.7, -12.5),
]


def rotation_rows():
    rows = []
    for frm in SYSTEMS:
        for to in SYSTEMS:
            if frm == to:
                continue
            for lon, lat in ROT_INPUTS:
                olon, olat = convert(frm, to, lon, lat)
                if float(olon) >= 360.0:
                    raise SystemExit("row (%d -> %d, %r, %r): the output longitude rounds to 360"
                                     % (frm, to, lon, lat))
                rows.append((frm, to, lon, lat, olon, olat))
    return rows


# ---------------------------------------------------------------------------------------------
# Emission
# ---------------------------------------------------------------------------------------------

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_skycoord_reference.py
! The contract model lives in that script; edit it there, not here. Never hand-edit a vector.
!"""

int_literal = rgv.int_literal
array = rgv.array
bits64 = rgv.bits64


def char_array(name, size_expr, items, width):
    """`character(len=width), parameter :: name(size_expr) = [character(len=width) :: ...]`, each
    text a double-quoted literal on its own line."""
    for t in items:
        if '"' in t or len(t) > width:
            raise SystemExit("text %r cannot be emitted into %s" % (t, name))
    out = ["    character(len=%d), parameter :: %s(%s) = [character(len=%d) :: &" % (width, name, size_expr, width)]
    for n, t in enumerate(items):
        out.append('        "%s"%s' % (t, ", &" if n < len(items) - 1 else "]"))
    return out


def logical_array(name, size_expr, flags):
    return array("logical", name, size_expr, [".true." if f else ".false." for f in flags])


def gen_module():
    rots = rotation_rows()
    offs = offset_rows()
    pas = pa_rows(offs)
    seps = sep_rows()
    pms = pm_rows()
    hms = [(x,) + hms_fields(x) for x in HMS_INPUTS]
    dms = [(x,) + dms_fields(x) for x in DMS_INPUTS]
    txt_ra, txt_dec = text_rows()
    rd_ra, rd_dec, rd_pair = read_rows()
    zrows = zcmb_rows()
    vecs = vec_rows()
    tans = tan_rows()

    L = [BANNER]
    L.append("!> Golden rows for `parquet_skycoord`, derived from a 60-digit model of each system's definition.")
    L.append("!!")
    L.append("!! Reals are stored as `transfer` bit patterns of the correctly rounded double, so nothing is")
    L.append("!! lost to a decimal literal. Every value is pinned to a tolerance the suite states, because")
    L.append("!! the library computes it with libm from matrices built out of rounded angles.")
    L.append("module test_skycoord_vectors")
    L.append("    use iso_fortran_env, only: int64")
    L.append("    implicit none")
    L.append("    public")
    L.append("")
    L.append("    ! ---- The selectors, as src/parquet_skycoord.f90 declares them ----")
    L.append("    integer, parameter :: sc_unknown = %d !! `PF_COORD_UNKNOWN`." % UNKNOWN)
    for name, value in SELECTORS.items():
        L.append("    integer, parameter :: sc_%s = %d !! `PF_COORD_%s`." % (name.lower(), value, name))
    L.append("")

    L.append("    ! ---- Rotations: (from, to, lon, lat) -> (lon, lat), every ordered pair of distinct")
    L.append("    ! ---- systems, the same %d positions converted out of each ----" % len(ROT_INPUTS))
    L.append("    integer, parameter :: n_srot = %d" % len(rots))
    L += array("integer", "srot_from", "n_srot", [str(r[0]) for r in rots])
    L += array("integer", "srot_to", "n_srot", [str(r[1]) for r in rots])
    L += array("integer(int64)", "srot_in_bits", "2 * n_srot", [bits64(x) for r in rots for x in r[2:4]])
    L += array("integer(int64)", "srot_out_bits", "2 * n_srot", [bits64(x) for r in rots for x in r[4:6]])
    L.append("")

    L.append("    ! ---- pf_offset_radec: (ra0, dec0, pa_deg, sep_deg) -> (ra, dec) ----")
    L.append("    integer, parameter :: n_soff = %d" % len(offs))
    L += array("integer(int64)", "soff_in_bits", "4 * n_soff", [bits64(x) for r in offs for x in r[:4]])
    L += array("integer(int64)", "soff_out_bits", "2 * n_soff", [bits64(x) for r in offs for x in r[4:]])
    L.append("")

    L.append("    ! ---- pf_position_angle_deg: (ra1, dec1, ra2, dec2) -> pa, with each row's tolerance ----")
    L.append("    integer, parameter :: n_spa = %d" % len(pas))
    L += array("integer(int64)", "spa_in_bits", "4 * n_spa", [bits64(x) for r in pas for x in r[:4]])
    L += array("integer(int64)", "spa_out_bits", "n_spa", [bits64(r[4]) for r in pas])
    L += array("integer(int64)", "spa_tol_bits", "n_spa", [bits64(r[5]) for r in pas])
    L.append("")

    L.append("    ! ---- pf_angdist_deg: (ra1, dec1, ra2, dec2) -> the exact separation, over a")
    L.append("    ! ---- sweep from 1e-9 to 180 degrees; both ends are exact in double ----")
    L.append("    integer, parameter :: n_sdist = %d" % len(seps))
    L += array("integer(int64)", "sdist_in_bits", "4 * n_sdist", [bits64(x) for r in seps for x in r[:4]])
    L += array("integer(int64)", "sdist_out_bits", "n_sdist", [bits64(r[4]) for r in seps])
    L.append("")

    L.append("    ! ---- pf_apply_pm: (ra, dec, pm_ra, pm_dec, dt_years) -> (ra, dec) ----")
    L.append("    integer, parameter :: n_spm = %d" % len(pms))
    L += array("integer(int64)", "spm_in_bits", "5 * n_spm", [bits64(x) for r in pms for x in r[:5]])
    L += array("integer(int64)", "spm_out_bits", "2 * n_spm", [bits64(x) for r in pms for x in r[5:]])
    L.append("")

    L.append("    ! ---- pf_radec2unit and pf_unit2radec: (lon, lat) -> v(3) -> (lon, lat) ----")
    L.append("    integer, parameter :: n_svec = %d" % len(vecs))
    L += array("integer(int64)", "svec_in_bits", "2 * n_svec", [bits64(x) for r in vecs for x in r[:2]])
    L += array("integer(int64)", "svec_out_bits", "3 * n_svec", [bits64(x) for r in vecs for x in r[2]])
    L += array("integer(int64)", "svec_back_bits", "2 * n_svec", [bits64(x) for r in vecs for x in r[3:]])
    L.append("")

    L.append("    ! ---- pf_radec2tan: (ra, dec, ra0, dec0, pa) -> (x, y), and pf_tan2radec back from")
    L.append("    ! ---- the emitted (x, y); a row without an image has NaN coordinates and zero rows ----")
    L.append("    integer, parameter :: n_stan = %d" % len(tans))
    L += array("integer(int64)", "stan_in_bits", "5 * n_stan", [bits64(x) for r in tans for x in r[:5]])
    L += logical_array("stan_has_image", "n_stan", [r[5] for r in tans])
    L += array("integer(int64)", "stan_out_bits", "2 * n_stan", [bits64(x) for r in tans for x in r[6:8]])
    L += array("integer(int64)", "stan_back_bits", "2 * n_stan", [bits64(x) for r in tans for x in r[8:]])
    L.append("")

    L.append("    ! ---- pf_deg2hms: deg -> (h, m, s), exactly split ----")
    L.append("    integer, parameter :: n_shms = %d" % len(hms))
    L += array("integer(int64)", "shms_in_bits", "n_shms", [bits64(r[0]) for r in hms])
    L += array("integer", "shms_h", "n_shms", [str(r[1]) for r in hms])
    L += array("integer", "shms_m", "n_shms", [str(r[2]) for r in hms])
    L += array("integer(int64)", "shms_s_bits", "n_shms", [bits64(r[3]) for r in hms])
    L.append("")
    L.append("    ! ---- pf_deg2dms: deg -> (sgn, d, m, s), exactly split ----")
    L.append("    integer, parameter :: n_sdms = %d" % len(dms))
    L += array("integer(int64)", "sdms_in_bits", "n_sdms", [bits64(r[0]) for r in dms])
    L += array("integer", "sdms_sgn", "n_sdms", [str(r[1]) for r in dms])
    L += array("integer", "sdms_d", "n_sdms", [str(r[2]) for r in dms])
    L += array("integer", "sdms_m", "n_sdms", [str(r[3]) for r in dms])
    L += array("integer(int64)", "sdms_s_bits", "n_sdms", [bits64(r[4]) for r in dms])
    L.append("")

    L.append("    ! ---- The writers: (angle, precision, style) -> text; style 1 is sep=\":\", 2 sep=\" \", 3 sep=\"hms\" ----")
    L.append("    integer, parameter :: n_stxt_ra = %d" % len(txt_ra))
    L += array("integer(int64)", "stxt_ra_in_bits", "n_stxt_ra", [bits64(r[0]) for r in txt_ra])
    L += array("integer", "stxt_ra_prec", "n_stxt_ra", [str(r[1]) for r in txt_ra])
    L += array("integer", "stxt_ra_style", "n_stxt_ra", [str(r[2]) for r in txt_ra])
    L += char_array("stxt_ra_text", "n_stxt_ra", [r[3] for r in txt_ra], 24)
    L.append("    integer, parameter :: n_stxt_dec = %d" % len(txt_dec))
    L += array("integer(int64)", "stxt_dec_in_bits", "n_stxt_dec", [bits64(r[0]) for r in txt_dec])
    L += array("integer", "stxt_dec_prec", "n_stxt_dec", [str(r[1]) for r in txt_dec])
    L += array("integer", "stxt_dec_style", "n_stxt_dec", [str(r[2]) for r in txt_dec])
    L += char_array("stxt_dec_text", "n_stxt_dec", [r[3] for r in txt_dec], 24)
    L.append("")

    L.append("    ! ---- The readers: text -> (ok, value); a value is 0 where ok is false ----")
    L.append("    integer, parameter :: n_sread_ra = %d" % len(rd_ra))
    L += char_array("sread_ra_text", "n_sread_ra", [r[0] for r in rd_ra], 32)
    L += logical_array("sread_ra_ok", "n_sread_ra", [r[1] is not None for r in rd_ra])
    L += array("integer(int64)", "sread_ra_bits", "n_sread_ra", [bits64(r[1] if r[1] is not None else 0) for r in rd_ra])
    L.append("    integer, parameter :: n_sread_dec = %d" % len(rd_dec))
    L += char_array("sread_dec_text", "n_sread_dec", [r[0] for r in rd_dec], 32)
    L += logical_array("sread_dec_ok", "n_sread_dec", [r[1] is not None for r in rd_dec])
    L += array("integer(int64)", "sread_dec_bits", "n_sread_dec", [bits64(r[1] if r[1] is not None else 0) for r in rd_dec])
    L.append("    integer, parameter :: n_sread_pair = %d" % len(rd_pair))
    L += char_array("sread_pair_text", "n_sread_pair", [r[0] for r in rd_pair], 40)
    L += logical_array("sread_pair_ok", "n_sread_pair", [r[1] is not None for r in rd_pair])
    L += array("integer(int64)", "sread_pair_bits", "2 * n_sread_pair",
               [bits64(v) for r in rd_pair for v in (r[1] if r[1] is not None else (0, 0))])
    L.append("")

    L.append("    ! ---- pf_zhel2zcmb and pf_zcmb2zhel: (lon, lat, z) in a system, the dipole default or given ----")
    L.append("    !> The default dipole as documented -- apex longitude and latitude, degrees Galactic, and speed,")
    L.append("    !! km/s -- and the redshift it gives a source at the apex with `z_hel = 0`, `gamma*(1 + v/c) - 1`.")
    L += array("integer(int64)", "szcmb_default_bits", "3", [bits64(CMB_APEX_LON), bits64(CMB_APEX_LAT), bits64(CMB_APEX_V)])
    L.append("    integer(int64), parameter :: szcmb_boost_bits = %s" % bits64(default_boost()))
    L.append("    integer, parameter :: n_szcmb = %d" % len(zrows))
    L += array("integer", "szcmb_system", "n_szcmb", [str(r[0]) for r in zrows])
    L += array("integer(int64)", "szcmb_in_bits", "3 * n_szcmb", [bits64(v) for r in zrows for v in r[1:4]])
    L += logical_array("szcmb_apex_given", "n_szcmb", [r[4] is not None for r in zrows])
    L += array("integer(int64)", "szcmb_apex_bits", "3 * n_szcmb",
               [bits64(v) for r in zrows for v in (r[4] if r[4] is not None else (0.0, 0.0, 0.0))])
    L += array("integer(int64)", "szcmb_out_bits", "n_szcmb", [bits64(r[5]) for r in zrows])
    L.append("    !> `pf_zcmb2zhel` of the SAME three inputs: the row's `z` read as a CMB-frame redshift.")
    L += array("integer(int64)", "szhel_out_bits", "n_szcmb", [bits64(r[6]) for r in zrows])
    L.append("")
    L.append("    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.")
    L.append("end module test_skycoord_vectors ! GCOVR_EXCL_LINE")
    return "\n".join(L) + "\n"


# ---------------------------------------------------------------------------------------------
# --self-test
# ---------------------------------------------------------------------------------------------

def source_real(src, name):
    """The value of `real(real64), parameter :: <name> = <literal>_real64` in the source, or None."""
    m = re.search(r"\b%s\s*=\s*([0-9]+\.[0-9]*(?:[eEdD][+-]?[0-9]+)?)_real64" % re.escape(name), src)
    return None if m is None else float(m.group(1).replace("d", "e").replace("D", "e"))


def self_test():
    bad = []

    def check(ok, what):
        if not ok:
            bad.append(what)

    def close(got, want, tol, what):
        if not abs(float(got) - float(want)) <= tol:
            bad.append("%s: got %r want %r" % (what, float(got), float(want)))

    def tiny(x):
        with dctx():
            return abs(x) < D(10) ** -70

    src = SRC_PATH.read_text()

    # The selectors.
    m = re.search(r"PF_COORD_UNKNOWN\s*=\s*(-?\d+)\b", src)
    check(m is not None and int(m.group(1)) == UNKNOWN, "PF_COORD_UNKNOWN does not match src/parquet_skycoord.f90")
    for name, value in SELECTORS.items():
        m = re.search(r"PF_COORD_%s\s*=\s*(-?\d+)\b" % name, src)
        check(m is not None and int(m.group(1)) == value, "PF_COORD_%s does not match src/parquet_skycoord.f90" % name)

    # The angle table: each literal must BE the double nearest the model's value.
    table = angle_table()
    for sysname, row in table.items():
        for field, value in zip(("pole_lon", "pole_lat", "lon0"), row):
            name = "skc_%s_%s" % (sysname, field)
            got = source_real(src, name)
            check(got is not None and got == float(value),
                  "%s in src/parquet_skycoord.f90 is %r, not the double nearest the model's %s"
                  % (name, got, +value))

    # The ecliptic row is closed form, which makes it an independent statement about the recovery.
    with dctx():
        ecl = table["ecl"]
        check(tiny(ecl[0] - (270 + ECL_GAMB)) and tiny(ecl[1] - (90 - ECL_PHIB)) and tiny(ecl[2] - (90 + ECL_PSIB)),
              "the ecliptic row is not (270 + gamb, 90 - phib, 90 + psib)")

    # Every row rebuilds its definition; every matrix is orthonormal.
    for sysname, system in (("gal", GAL), ("ecl", ECL), ("fk5", FK5)):
        r = three_angle(*table[sysname])
        d = definition(system)
        check(all(tiny(r[i][j] - d[i][j]) for i in range(3) for j in range(3)),
              "the %s row does not rebuild its definition" % sysname)
    check(all(tiny(a - b) for ra, rb in zip(three_angle(*table["sgal"]), gal_to_sgal()) for a, b in zip(ra, rb)),
          "the supergalactic row does not rebuild its definition")
    for system in SYSTEMS:
        p = mm(definition(system), mt(definition(system)))
        check(all(tiny(p[i][j] - (1 if i == j else 0)) for i in range(3) for j in range(3)),
              "the matrix of system %d is not orthonormal" % system)

    # Round trips, every ordered pair.
    for frm in SYSTEMS:
        for to in SYSTEMS:
            if frm == to:
                continue
            for lon, lat in ((10.0, 20.0), (250.0, -70.0), (359.0, 1.0)):
                a, b = convert(frm, to, lon, lat)
                c, e = convert(to, frm, a, b)
                close(c, lon, 1e-40, "round trip %d -> %d -> %d, lon" % (frm, to, frm))
                close(e, lat, 1e-40, "round trip %d -> %d -> %d, lat" % (frm, to, frm))

    # FK5 J2000 is the frame bias and nothing else: its pole sits 21.88 mas from the ICRS pole, and
    # Galactic reached through it is astropy's FK5-referred definition, exactly.
    with dctx():
        check(abs((90 - table["fk5"][1]) * MAS - D("21.88196")) < D("0.00001"),
              "the FK5 pole is not 21.88 mas from the ICRS pole")
        p = pair_matrix(FK5, GAL)
        g = gal_from_fk5()
        check(all(tiny(p[i][j] - g[i][j]) for i in range(3) for j in range(3)),
              "Galactic taken from FK5 is not astropy's FK5-referred definition")

    # The pole rules: the ICRS pole is (lon0, pole_lat) in each ICRS-referred system, whatever
    # longitude names it, and a pole's longitude comes out 0.
    for sysname, system in (("gal", GAL), ("ecl", ECL), ("fk5", FK5)):
        for lon in (0.0, 123.4):
            a, b = convert(ICRS, system, lon, 90.0)
            close(a, table[sysname][2], 1e-40, "the ICRS pole in %s: lon" % sysname)
            close(b, table[sysname][1], 1e-40, "the ICRS pole in %s: lat" % sysname)
    a, b = unit_radec([D(0), D(0), D(1)])
    check(a == 0, "a pole's longitude is not 0")
    close(b, 90.0, 1e-40, "a pole's latitude")
    # The Galactic pole, converted from its own ICRS row, is at latitude 90.
    a, b = convert(ICRS, GAL, float(table["gal"][0]), float(table["gal"][1]))
    close(b, 90.0, 1e-12, "the Galactic pole from its ICRS position: lat")

    # The offset's pole convention and its inverse.
    ra, dec = offset_radec(D(10), 90.0, D(30), D(5))
    close(ra, 160.0, 1e-12, "offset from the north pole: ra")
    close(dec, 85.0, 1e-12, "offset from the north pole: dec")
    ra, dec = offset_radec(D(10), -90.0, D(30), D(5))
    close(ra, 40.0, 1e-12, "offset from the south pole: ra")
    ra, dec = offset_radec(D(10), 20.0, D(33), D(4))
    close(position_angle(10.0, 20.0, float(ra), float(dec)), 33.0, 1e-10, "the position angle inverts the offset")
    close(position_angle(0.0, 0.0, 0.0, 10.0), 0.0, 1e-15, "due north is 0")
    close(position_angle(0.0, 0.0, 10.0, 0.0), 90.0, 1e-12, "due east is 90")
    check(position_angle(10.0, 90.0, 250.0, 90.0) == 0, "two labels of one pole coincide")

    # The proper motion. `pm_ra` is the rate in right ascension times cos(dec), so an east-only motion
    # at declination 80 is the great circle leaving due east, `tan(dra) = tan(s) / cos(dec0)` and
    # `sin(dec) = sin(dec0) cos(s)`: the right ascension moves 5.8 times as far as the arc. A north-only
    # motion moves the declination by the arc and nothing else. No time, or no motion, leaves the
    # position where it is, its right ascension wrapped.
    with dctx():
        s = D(1000) * 10 / MAS
        ss, cs = dsin_cos(deg2rad(s))
        sd, cd = dsin_cos(deg2rad(D(80)))
        ra, dec = apply_pm(100.0, 80.0, 1000.0, 0.0, 10.0)
        check(tiny(ra - (100 + rad2deg(datan2(ss, cd * cs)))) and tiny(dec - rad2deg(sph.dasin(sd * cs))),
              "an east-only motion at dec 80 is not the great circle leaving due east")
        ra, dec = apply_pm(10.0, 20.0, 0.0, 360.0, 25.0)
        check(tiny(ra - 10) and tiny(dec - (20 + D(360) * 25 / MAS)),
              "a north-only motion does not move the declination by the arc alone")
        for args in ((-10.0, 20.0, 5.0, 3.0, 0.0), (-10.0, 20.0, 0.0, 0.0, 7.0)):
            ra, dec = apply_pm(*args)
            check(tiny(ra - 350) and tiny(dec - 20), "no time or no motion moved the position: %r" % (args,))
        # Forward then back: the motion carried to the new position along its great circle -- the
        # track's position angle there, the same rate -- and a negative interval return to the start.
        # The original components would not: the track turns as it crosses the meridians, and they
        # miss by about s**2 tan(dec), here 1e-3 degrees.
        ra1, dec1 = apply_pm(33.0, 61.0, 4000.0, -2500.0, 150.0)
        rate = (D(4000) ** 2 + D(2500) ** 2).sqrt()
        sp, cp = dsin_cos(deg2rad(position_angle(ra1, dec1, D(33), D(61)) + 180))
        ra2, dec2 = apply_pm(ra1, dec1, rate * sp, rate * cp, -150.0)
        check(tiny(ra2 - 33) and tiny(dec2 - 61), "the motion carried along its great circle did not reverse")
        ra2, dec2 = apply_pm(ra1, dec1, 4000.0, -2500.0, -150.0)
        check(abs(dec2 - 61) + abs(ra2 - 33) > D("1e-4"), "the original components reversed the motion after all")

    # pf_zhel2zcmb's dipole and the speed of light, read back from where they are declared.
    rotate_src = ROTATE_PATH.read_text()
    for name, value in (("skc_c_kms", C_KMS), ("skc_cmb_apex_lon", CMB_APEX_LON),
                        ("skc_cmb_apex_lat", CMB_APEX_LAT), ("skc_cmb_apex_v", CMB_APEX_V)):
        got = source_real(rotate_src, name)
        check(got is not None and got == float(value),
              "%s in src/parquet_skycoord_rotate.f90 is %r, not the documented %s" % (name, got, value))

    # The text: the documented examples, the carries into the minute, the degree and 24 hours, the
    # half-unit on either side, the tie, and the sign of a zero.
    ex_ra, ex_dec = 155.37729166666667, 41.26916666666667
    check(ra_text(ex_ra, STYLE_COLON, 2) == "10:21:30.550", "the right ascension example")
    check(dec_text(ex_dec, STYLE_COLON, 2) == "+41:16:09.00", "the declination example")
    check(ra_text(ex_ra, STYLE_LETTERS, 2) == "10h21m30.550s", "the lettered right ascension")
    check(dec_text(-0.5, STYLE_LETTERS, 2) == "-00d30m00.00s", "the lettered declination, and its sign")
    check(ra_text((23 * 3600 + 59 * 60 + 59.9996) / 240.0, STYLE_COLON, 2) == "00:00:00.000", "24 hours wrap to 0")
    check(dec_text((59 * 60 + 59.9996) / 3600.0, STYLE_COLON, 3) == "+01:00:00.000", "the carry into the degree")
    check(dec_text((59 * 60 + 59.9994) / 3600.0, STYLE_COLON, 3) == "+00:59:59.999", "no carry below the half")
    check(dec_text(0.0078125, STYLE_COLON, 2) == "+00:00:28.12", "a tie rounds to even")
    check(dec_text(-0.0, STYLE_COLON, 2) == "+00:00:00.00", "-0.0 is written with +")
    check(dec_text(-1.0e-10, STYLE_COLON, 2) == "-00:00:00.00", "a negative value that rounds to 0 keeps its -")

    # The grammar: the documented forms read alike, and the documented refusals refuse.
    one = read_ra("10:21:30.55")
    check(one is not None and all(read_ra(t) == one for t in ("10 21 30.55", "10h21m30.55s", "10h 21m 30.55s")),
          "the three separator styles do not read alike")
    check(all(read_ra(t) is None for t in ("5 6", "155.3772", "+10:21:30", "10:21", "10:60:00", "10:21:60")),
          "a documented refusal was read")
    check(read_dec("-00:30:00") == Fraction(-1, 2), "-00:30:00 is not -0.5")
    # The leading field's bound, reached only exactly: the pole and the whole turn read, and the
    # first text past either does not.
    check(read_dec("+90:00:00") == 90 and read_dec("-90:00:00") == -90, "a pole does not read")
    check(read_ra("24:00:00") == 360, "24:00:00 is not 360 degrees")
    check(all(read_dec(t) is None for t in ("+90:00:00.01", "+90:00:01", "+90:01:00", "+91:00:00", "-91:00:00")),
          "a declination past a pole was read")
    check(all(read_ra(t) is None for t in ("24:00:00.001", "24:00:01", "24:01:00", "25:00:00", "99:00:00")),
          "an hour past a whole turn was read")
    check(read_pair("10:21:30.55, +41:16:09") == (one, read_dec("+41:16:09")), "a pair split at a comma")

    # Every golden input, written and read back by the model, lands within half a unit of the last
    # decimal it was written to.
    for x in TEXT_RA_INPUTS:
        for p, st in TEXT_CASES:
            back = read_ra(ra_text(x, st, p))
            gap = abs((back - fwrap(x) + 180) % 360 - 180) * 240
            check(back is not None and gap <= HALF / 10 ** (p + 1), "ra %r at precision %d does not round-trip" % (x, p))
    for x in TEXT_DEC_INPUTS:
        if abs(x) > 90.0:
            continue                    # beyond a pole: a writer writes it, a reader refuses it
        for p, st in TEXT_CASES:
            back = read_dec(dec_text(x, st, p))
            check(back is not None and abs(back - Fraction(x)) * 3600 <= HALF / 10 ** p,
                  "dec %r at precision %d does not round-trip" % (x, p))

    # The redshift: no motion changes nothing; the apex gives the documented boost; and the apex's
    # and the antapex's factors multiply to gamma**2 (1 - beta**2), which is exactly one.
    with dctx():
        z = zcmb(155.0, 41.0, 0.05, ICRS, (270.0, 30.0, 0.0))
        check(tiny(z - D(0.05)), "no motion changed the redshift")
        up = zcmb(CMB_APEX_LON, CMB_APEX_LAT, 0.0, GAL)
        down = zcmb(CMB_APEX_LON + 180, -CMB_APEX_LAT, 0.0, GAL)
        check(tiny(up - default_boost()), "the apex does not give gamma*(1 + v/c) - 1")
        check(tiny((1 + up) * (1 + down) - 1), "the apex's and the antapex's factors do not multiply to one")
        check(up > 0 > down, "the CMB-frame redshift is not the larger toward the apex")
        # NINETY DEGREES FROM THE APEX THE BOOST IS THE TRANSVERSE DOPPLER EFFECT AND NOTHING ELSE:
        # `1 + z_cmb = (1 + z_hel) / gamma`, with no first-order term at all. That is the one place
        # the two ways of writing the factor differ most -- the other reading, with `theta` in the
        # CMB frame, answers `(1 + z_hel) * gamma` -- and it holds for the observed direction
        # whatever the speed is, so it is a statement about the physics rather than about this code.
        beta = CMB_APEX_V / C_KMS
        g = 1 / (1 - beta * beta).sqrt()
        for pa in (D(0), D(90), D(215)):
            # The position is kept exact rather than rounded to a double, so what is tested is the
            # formula and not the 1e-16 of a rounded direction -- and the offset leaves the apex
            # the redshift is measured from, the exact one, not the double nearest it.
            lon, lat = offset_radec(CMB_APEX_LON, CMB_APEX_LAT, pa, D(90))
            check(abs((1 + zcmb(lon, lat, D("0.1"), GAL)) - D("1.1") / g) < D(10) ** -40,
                  "90 degrees from the apex at position angle %s is not the transverse Doppler shift" % pa)
        # The inverse undoes the boost, in every system and for a given dipole too; the tolerance
        # is the working precision's, since the pair is a division followed by its multiplication.
        for system, lon, lat in ZCMB_POSITIONS:
            for z in ZCMB_Z:
                back = zhel(lon, lat, zcmb(lon, lat, z, system), system)
                check(abs(back - D(z)) < D(10) ** -40,
                      "pf_zcmb2zhel does not invert pf_zhel2zcmb at (%r, %r) in system %d" % (lon, lat, system))
        back = zhel(155.0, 41.0, zcmb(155.0, 41.0, D("0.3"), ICRS, ZCMB_APEXES[0]), ICRS, ZCMB_APEXES[0])
        check(abs(back - D("0.3")) < D(10) ** -40, "pf_zcmb2zhel does not invert pf_zhel2zcmb under a given dipole")

    # The unit-vector pair: the poles exact, the round trip, and the direction a latitude past 90 names.
    for lon, lat, v, back_lon, back_lat in vec_rows():
        with dctx():
            check(tiny(v[0] * v[0] + v[1] * v[1] + v[2] * v[2] - 1), "the unit vector of (%r, %r) is not one" % (lon, lat))
        if abs(lat) == 90.0:
            check(v[0] == 0 and v[1] == 0 and abs(v[2]) == 1, "a pole's vector is not exactly (0, 0, +/-1)")
            check(back_lon == 0, "a pole's longitude does not read back as 0")
        elif abs(lat) < 90.0:
            with dctx():
                check(tiny(D(back_lon) - sph.dwrap360(D(lon))) and tiny(D(back_lat) - D(lat)),
                      "the vector of (%r, %r) does not read back as itself" % (lon, lat))
    with dctx():
        lon, lat = unit_radec(radec_unit(45.0, 100.0))
        check(tiny(lon - 225) and tiny(lat - 80), "a latitude of 100 is not the direction (225, 80)")

    # The tangent plane: the definition, the inverse, and the hemisphere that has no image.
    with dctx():
        for pa in (D(0), D("37.5"), D(-90)):
            for sep in (D("1e-6"), D("0.5"), D(10), D(80)):
                # Exact positions throughout, as above: a double would put 1e-16 of the centre's
                # own rounding into `x`, which is the quantity being asserted to be zero.
                ra, dec = offset_radec(D(33), 21.0, pa, sep)
                x, y = radec2tan(ra, dec, D(33), D(21), pa)
                # A point at position angle `pa` lands on `+y` at `tan(sep)`, which is the whole
                # definition of the projection and of what `pa_deg` does to the axes.
                st, ct = dsin_cos(deg2rad(sep))
                check(abs(x) < D(10) ** -40 and abs(y - rad2deg(st / ct)) < D(10) ** -40,
                      "a point at position angle %s and separation %s is not (0, tan(sep))" % (pa, sep))
                a, b = tan2radec(x, y, D(33), D(21), pa)
                check(abs(a - ra) < D(10) ** -40 and abs(b - dec) < D(10) ** -40,
                      "pf_tan2radec does not invert pf_radec2tan at position angle %s" % pa)
    # Beyond 90 degrees there is no image. AT 90 degrees the cosine is zero, which 60 digits of a
    # rounded direction resolve no better than a double does: the boundary is a limit, not a row.
    for sep in (90.001, 120.0, 180.0):
        ra, dec = offset_radec(D(10), 5.0, D(30), D(sep))
        check(radec2tan(float(ra), float(dec), 10.0, 5.0, 0.0) is None,
              "a position %r degrees away has an image" % sep)
    with dctx():
        x, y = radec2tan(D(250), D(90), D(250), D(90), D(0))
        check(abs(x) < D(10) ** -40 and abs(y) < D(10) ** -40, "the tangent point is not the origin")
    return bad


# ---------------------------------------------------------------------------------------------
# --verify-oracle
# ---------------------------------------------------------------------------------------------

def verify_oracle():
    try:
        import astropy
        import astropy.units as u
        from astropy.coordinates import (FK5 as AFK5, ICRS as AICRS, BarycentricMeanEcliptic, Galactic, SkyCoord,
                                         Supergalactic)
    except ImportError as exc:
        sys.stderr.write("generate_skycoord_reference --verify-oracle needs astropy: %s\n" % exc)
        return 2
    bad = []
    counts = {"rotations": 0, "offsets": 0, "separations": 0, "position angles": 0}
    frames = {ICRS: AICRS(), GAL: Galactic(), ECL: BarycentricMeanEcliptic(), SGAL: Supergalactic(),
              FK5: AFK5()}

    def check(ok, what):
        if not ok:
            bad.append(what)

    def sky_gap(lon, lat, lon_ref, lat_ref):
        dl = (lon - lon_ref + 180.0) % 360.0 - 180.0
        return max(abs(lat - lat_ref), abs(dl) * math.cos(math.radians(lat_ref)))

    # Rotations against astropy's transform graph. astropy refuses a latitude beyond 90, so such an
    # input is handed over as the direction it names.
    for frm, to, lon, lat, olon, olat in rotation_rows():
        lon_a, lat_a = lon, lat
        if abs(lat) > 90.0:
            lon_a, lat_a = lon + 180.0, math.copysign(180.0, lat) - lat
        c = SkyCoord(lon_a * u.deg, lat_a * u.deg, frame=frames[frm]).transform_to(frames[to])
        sph_c = c.spherical
        got_lon, got_lat = sph_c.lon.deg, sph_c.lat.deg
        counts["rotations"] += 1
        check(sky_gap(got_lon, got_lat, float(olon), float(olat)) < 1e-11,
              "rotation %d -> %d of (%r, %r): astropy (%r, %r), model (%r, %r)"
              % (frm, to, lon, lat, got_lon, got_lat, float(olon), float(olat)))

    # Offsets and position angles against astropy.
    for ra0, dec0, pa, sep, ra, dec in offset_rows():
        if abs(dec0) == 90.0 and sep > 180.0:
            # astropy's pole branch sets the longitude as though the separation were at most 180, so
            # past the far pole it reports the mirror meridian; the vector form continues correctly.
            continue
        o = SkyCoord(ra0 * u.deg, dec0 * u.deg).directional_offset_by(pa * u.deg, sep * u.deg)
        dra = ((o.ra.deg - float(ra) + 180.0) % 360.0 - 180.0) * math.cos(math.radians(float(dec)))
        # astropy takes the declination as `arcsin`, which loses digits near a pole: an ulp of the
        # sine is `eps/sqrt(2*(1 - |sin(dec)|))` radians there. The model uses `atan2`.
        one_minus = max(1e-300, 1.0 - abs(math.sin(math.radians(float(dec)))))
        dec_tol = 1e-9 + 4.0 * 2.2e-16 / math.sqrt(2.0 * one_minus) * 180.0 / math.pi
        counts["offsets"] += 1
        check(abs(dra) < 1e-9 and abs(o.dec.deg - float(dec)) < dec_tol,
              "offset (%g, %g, %g, %g): astropy (%r, %r), model (%r, %r)"
              % (ra0, dec0, pa, sep, o.ra.deg, o.dec.deg, float(ra), float(dec)))
        if sep <= 180.0:
            # The separation the offset was asked for is the one astropy measures back.
            got = SkyCoord(ra0 * u.deg, dec0 * u.deg).separation(SkyCoord(float(ra) * u.deg, float(dec) * u.deg)).deg
            counts["separations"] += 1
            check(abs(got - sep) < 1e-9, "separation after offset (%g, %g, %g, %g): astropy %r"
                  % (ra0, dec0, pa, sep, got))
    for ra1, dec1, ra2, dec2, pa, tol in pa_rows(offset_rows()):
        if pa == 0 and dec1 == dec2 and abs(dec1) == 90.0:
            continue                               # the pole's coincidence rule is the library's own
        got = SkyCoord(ra1 * u.deg, dec1 * u.deg).position_angle(SkyCoord(ra2 * u.deg, dec2 * u.deg)).deg
        diff = (got - float(pa) + 180.0) % 360.0 - 180.0
        counts["position angles"] += 1
        check(abs(diff) < max(1e-9, 1e3 * tol), "position angle (%g, %g, %g, %g): astropy %r, model %r"
              % (ra1, dec1, ra2, dec2, got, float(pa)))

    verify_proper_motion(check, counts)
    verify_vectors_and_tangent_plane(check, counts)
    verify_text_and_redshift(check, counts, frames)

    if bad:
        sys.stderr.write("generate_skycoord_reference --verify-oracle FAILED (%d):\n" % len(bad))
        for line in bad[:40]:
            sys.stderr.write("  %s\n" % line)
        return 1
    print("generate_skycoord_reference --verify-oracle: astropy %s agrees (%s)"
          % (astropy.__version__, ", ".join("%d %s" % (v, k) for k, v in counts.items())))
    return 0


def verify_proper_motion(check, counts):
    """The proper-motion rows against astropy, two ways.

    * Every row's step through `directional_offset_by`, at the position angle and separation the
      model resolved the motion into: the offset half of `pf_apply_pm`.
    * Every row whose step is at most half a degree through `apply_space_motion`, which reads
      `pm_ra_cosdec` and `pm_dec` itself -- so this is the check on the `cos(dec)` convention and on
      the direction. A source with no distance and no radial velocity moves in a straight line
      through space there, along the same great circle but through `atan(s)` of it where the model
      moves `s` (radians); the comparison takes that mapping, and below half a degree nothing else
      astropy models reaches 1e-10 degrees. A longer step is counted, not compared, and so is a
      start at a pole, where astropy divides `pm_ra_cosdec` by `cos(dec)` and ERFA then drops the
      motion as too fast: the library reads it in the local frame of the `ra` given instead.

    astropy takes a declination as `arcsin`, which loses digits near a pole, so the tolerance widens
    there as it does for the offsets.
    """
    import warnings
    import astropy.units as u
    from astropy.coordinates import SkyCoord
    from astropy.time import Time
    from erfa import ErfaWarning

    for key in ("proper motions", "space motions", "by design: steps past half a degree, great circle only",
                "by design: starts at a pole, great circle only"):
        counts[key] = 0
    epoch = Time(2000.0, format="jyear", scale="tdb")
    for ra, dec, pm_ra, pm_dec, dt, ra_out, dec_out in pm_rows():
        pa, sep = pm_step(pm_ra, pm_dec, dt)
        pa, sep = float(pa), float(sep)
        model = SkyCoord(float(ra_out) * u.deg, float(dec_out) * u.deg)
        one_minus = max(1e-300, 1.0 - abs(math.sin(math.radians(float(dec_out)))))
        pole = 4.0 * 2.2e-16 / math.sqrt(2.0 * one_minus) * 180.0 / math.pi
        o = SkyCoord(ra * u.deg, dec * u.deg).directional_offset_by(pa * u.deg, sep * u.deg)
        counts["proper motions"] += 1
        check(o.separation(model).deg < 1e-9 + pole, "proper motion %r: directional_offset_by (%r, %r), model (%r, %r)"
              % ((ra, dec, pm_ra, pm_dec, dt), o.ra.deg, o.dec.deg, float(ra_out), float(dec_out)))
        if sep > 0.5:
            counts["by design: steps past half a degree, great circle only"] += 1
            continue
        if abs(dec) == 90.0:
            counts["by design: starts at a pole, great circle only"] += 1
            continue
        with warnings.catch_warnings():
            # A source with no distance has one supplied by ERFA, which says so.
            warnings.simplefilter("ignore", ErfaWarning)
            c = SkyCoord(ra * u.deg, dec * u.deg, pm_ra_cosdec=pm_ra * u.mas / u.yr, pm_dec=pm_dec * u.mas / u.yr,
                         obstime=epoch)
            moved = c.apply_space_motion(dt=dt * u.yr)
        along = SkyCoord(ra * u.deg, dec * u.deg).directional_offset_by(
            pa * u.deg, math.degrees(math.atan(math.radians(sep))) * u.deg)
        counts["space motions"] += 1
        check(SkyCoord(moved.ra, moved.dec).separation(along).deg < 1e-10 + pole,
              "proper motion %r: apply_space_motion (%r, %r), the model's great circle through atan (%r, %r)"
              % ((ra, dec, pm_ra, pm_dec, dt), moved.ra.deg, moved.dec.deg, along.ra.deg, along.dec.deg))


def verify_vectors_and_tangent_plane(check, counts):
    """The unit vectors against astropy's own Cartesian representation, and the tangent-plane rows
    against `astropy.wcs`, which is wcslib rather than a second copy of this model.

    The projection is written as a FITS `RA---TAN`/`DEC--TAN` header whose reference point is the
    tangent point, one degree per unit (`CDELT = 1`) and `CRPIX = 1`, so a pixel coordinate IS the
    intermediate world coordinate in degrees; `PC` carries the axis rotation, `R(-pa_deg)`, which
    makes a pixel coordinate `(x, y)` as this module defines it. **`LONPOLE` is 180 at every
    tangent point, the FITS default everywhere except exactly at a pole**, where the standard's
    own default of 0 turns the chart half a turn; 180 is the convention this module states, the
    local frame following the `ra0` given, and it is what `pf_offset_radec` uses.
    """
    import numpy as np
    import astropy.units as u
    from astropy.coordinates import SkyCoord
    from astropy.wcs import WCS

    counts["unit vectors"] = 0
    counts["tangent-plane projections"] = 0
    counts["positions outside the projection"] = 0

    for lon, lat, v, back_lon, back_lat in vec_rows():
        # astropy refuses a latitude beyond 90, so such an input is handed over as the direction it
        # names -- which is what the library reads it as.
        lon_a, lat_a = lon, lat
        if abs(lat) > 90.0:
            lon_a, lat_a = lon + 180.0, math.copysign(180.0, lat) - lat
        c = SkyCoord(lon_a * u.deg, lat_a * u.deg).cartesian
        got = (c.x.value, c.y.value, c.z.value)
        counts["unit vectors"] += 1
        check(max(abs(got[j] - float(v[j])) for j in range(3)) < 1e-15,
              "the unit vector of (%r, %r): astropy %r, model %r" % (lon, lat, got, [float(q) for q in v]))

    for ra, dec, ra0, dec0, pa, has_image, x, y, ra_back, dec_back in tan_rows():
        w = WCS(naxis=2)
        w.wcs.ctype = ["RA---TAN", "DEC--TAN"]
        w.wcs.crval = [ra0, dec0]
        w.wcs.crpix = [1.0, 1.0]
        w.wcs.cdelt = [1.0, 1.0]
        w.wcs.lonpole = 180.0
        r = math.radians(pa)
        w.wcs.pc = [[math.cos(r), math.sin(r)], [-math.sin(r), math.cos(r)]]
        gx, gy = w.wcs_world2pix(ra, dec, 0)
        if not has_image:
            # wcslib reports the far hemisphere as invalid, which numpy delivers as a NaN.
            counts["positions outside the projection"] += 1
            check(not np.isfinite(gx) or not np.isfinite(gy) or abs(gx) + abs(gy) > 1e12,
                  "a position %r degrees from (%r, %r) has a finite image %r" % ((ra, dec), ra0, dec0, (gx, gy)))
            continue
        counts["tangent-plane projections"] += 1
        scale = max(1.0, abs(x), abs(y))
        check(abs(float(gx) - x) < 1e-11 * scale and abs(float(gy) - y) < 1e-11 * scale,
              "pf_radec2tan(%r, %r) about (%r, %r) at pa %r: wcslib (%r, %r), model (%r, %r)"
              % (ra, dec, ra0, dec0, pa, float(gx), float(gy), x, y))
        # And back: wcslib's own inverse of the emitted standard coordinates.
        ga, gd = w.wcs_pix2world(x, y, 0)
        counts["tangent-plane projections"] += 1
        back_ra, back_dec = float(ra_back), float(dec_back)
        dra = ((float(ga) - back_ra + 180.0) % 360.0 - 180.0) * math.cos(math.radians(back_dec))
        check(abs(dra) < 1e-11 and abs(float(gd) - back_dec) < 1e-11,
              "pf_tan2radec(%r, %r) about (%r, %r) at pa %r: wcslib (%r, %r), model (%r, %r)"
              % (x, y, ra0, dec0, pa, float(ga), float(gd), back_ra, back_dec))


def verify_text_and_redshift(check, counts, frames):
    """The sexagesimal rows against astropy's `Angle`, and the redshift rows against a cosine taken
    from astropy's `separation`.

    Three differences are the library's by design and are counted rather than compared:

    * astropy carries seconds into the next minute from `60 - 10**-q` upward, where they round to 60
      only from `60 - 10**-q / 2`; the library carries what rounds to 60, so a row in between is not
      compared;
    * astropy writes a right ascension that rounds up to 24 hours as `24:...`, and the library wraps
      it to `00:...`;
    * astropy signs a zero by `np.sign` (0) or, in text, `-0.0` negative, where the library's sign is
      `pf_deg2dms`'s: -1 below zero, +1 otherwise;
    * where the DOUBLE product of the fraction and the units a degree lands on a half unit that the
      exact product misses, astropy rounds the tie it was handed by parity, and the library rounds
      the exact product -- so the two differ by one unit in the last digit. Those inputs are in the
      lists above on purpose (`KNIFE_EDGE_RA`, `KNIFE_EDGE_DEC`) and are counted here, not compared.

    A field split is compared as the time it adds up to, modulo a day: astropy converts degrees to
    hours by multiplying by a rounded 1/15, which can carry a value a hair below a field boundary
    across it (359.99999999999994 degrees is 24 hours there), where the library splits exactly.
    """
    import astropy.units as u
    from astropy.coordinates import Angle, Longitude, SkyCoord

    for key in ("field splits", "texts written", "texts read", "redshifts", "by design: early carries",
                "by design: 24 h wrapped", "by design: the sign of a zero", "texts astropy refuses",
                "by design: a tie the double product invents"):
        counts[key] = 0
    ra_sep = {STYLE_COLON: ":", STYLE_BLANK: " ", STYLE_LETTERS: "hms"}
    dec_sep = {STYLE_COLON: ":", STYLE_BLANK: " ", STYLE_LETTERS: "dms"}

    def carries_early(seconds, q):
        return 60 - Fraction(1, 10 ** q) <= seconds < 60 - Fraction(1, 2 * 10 ** q)

    def invented_tie(frac, per_deg):
        """Whether the double product lands on a half unit the exact product misses."""
        xd = float(frac) * float(per_deg)
        t = math.trunc(xd)
        return xd - t == 0.5 and frac * per_deg != Fraction(2 * t + 1, 2)

    for x in HMS_INPUTS:
        h, m, s = hms_fields(x)
        a = Longitude(x * u.deg).hms
        gap = ((a.h * 3600 + a.m * 60 + a.s) - (h * 3600 + m * 60 + float(s)) + 43200) % 86400 - 43200
        counts["field splits"] += 1
        check(abs(gap) < 1e-9, "pf_deg2hms(%r): astropy %r, model (%d, %d, %r)" % (x, tuple(a), h, m, float(s)))
    for x in DMS_INPUTS:
        sgn, d, m, s = dms_fields(x)
        a = Angle(x * u.deg).signed_dms
        if x == 0:
            counts["by design: the sign of a zero"] += 1
            continue
        counts["field splits"] += 1
        check(a.sign == sgn and a.d == d and a.m == m and abs(a.s - float(s)) < 1e-6,
              "pf_deg2dms(%r): astropy %r, model (%d, %d, %d, %r)" % (x, tuple(a), sgn, d, m, float(s)))

    txt_ra, txt_dec = text_rows()
    for x, p, st, text in txt_ra:
        secs = fwrap(x) * 240
        if carries_early(secs - 60 * ffloor(secs / 60), p + 1):
            counts["by design: early carries"] += 1
            continue
        if invented_tie(fwrap(x) - ffloor(fwrap(x)), 240 * 10 ** (p + 1)):
            counts["by design: a tie the double product invents"] += 1
            continue
        got = Longitude(x * u.deg).to_string(unit=u.hourangle, sep=ra_sep[st], precision=p + 1, pad=True)
        if got.startswith("24") and text.startswith("00"):
            counts["by design: 24 h wrapped"] += 1
            got = "00" + got[2:]
        counts["texts written"] += 1
        check(got == text, "pf_ra2str(%r, precision=%d, style %d): astropy %r, model %r" % (x, p, st, got, text))
    for x, p, st, text in txt_dec:
        arcsec = abs(Fraction(x)) * 3600
        if carries_early(arcsec - 60 * ffloor(arcsec / 60), p):
            counts["by design: early carries"] += 1
            continue
        if invented_tie(abs(Fraction(x)) - ffloor(abs(Fraction(x))), 3600 * 10 ** p):
            counts["by design: a tie the double product invents"] += 1
            continue
        if x == 0 and math.copysign(1.0, x) < 0:
            counts["by design: the sign of a zero"] += 1
            continue
        got = Angle(x * u.deg).to_string(unit=u.deg, sep=dec_sep[st], precision=p, pad=True, alwayssign=True)
        counts["texts written"] += 1
        check(got == text, "pf_dec2str(%r, precision=%d, style %d): astropy %r, model %r" % (x, p, st, got, text))

    rd_ra, rd_dec, rd_pair = read_rows()
    for rows, unit in ((rd_ra, u.hourangle), (rd_dec, u.deg)):
        for text, want in rows:
            if want is None:
                continue                               # the library's grammar is stricter than astropy's
            try:
                got = Angle(text, unit=unit).degree
            except Exception:
                counts["texts astropy refuses"] += 1
                continue
            counts["texts read"] += 1
            check(abs(got - float(want)) < 1e-12, "reading %r: astropy %r, model %r" % (text, got, float(want)))
    for text, want in rd_pair:
        if want is None:
            continue
        try:
            c = SkyCoord(text, unit=(u.hourangle, u.deg))
        except Exception:
            counts["texts astropy refuses"] += 1
            continue
        counts["texts read"] += 1
        check(abs(c.ra.deg - float(want[0])) < 1e-12 and abs(c.dec.deg - float(want[1])) < 1e-12,
              "reading the pair %r: astropy (%r, %r), model (%r, %r)"
              % (text, c.ra.deg, c.dec.deg, float(want[0]), float(want[1])))

    c_kms = float(C_KMS)
    for system, lon, lat, z, apex, want, want_hel in zcmb_rows():
        alon, alat, av = apex if apex is not None else (float(CMB_APEX_LON), float(CMB_APEX_LAT), float(CMB_APEX_V))
        sep = SkyCoord(lon * u.deg, lat * u.deg, frame=frames[system]).separation(
            SkyCoord(l=alon * u.deg, b=alat * u.deg, frame="galactic"))
        beta = av / c_kms
        # `D = gamma (1 - beta cos(theta))` with `theta` astropy's own separation from the apex.
        d_fac = (1 - beta * math.cos(sep.radian)) / math.sqrt(1 - beta * beta)
        counts["redshifts"] += 2
        check(abs((1 + z) / d_fac - 1 - float(want)) < 1e-13 * max(1.0, abs(float(want))),
              "pf_zhel2zcmb(%r, %r, %r, system %d, apex %r): astropy's angle gives %r, model %r"
              % (lon, lat, z, system, apex, (1 + z) / d_fac - 1, float(want)))
        check(abs((1 + z) * d_fac - 1 - float(want_hel)) < 1e-13 * max(1.0, abs(float(want_hel))),
              "pf_zcmb2zhel(%r, %r, %r, system %d, apex %r): astropy's angle gives %r, model %r"
              % (lon, lat, z, system, apex, (1 + z) * d_fac - 1, float(want_hel)))


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="compare the committed file with a fresh generation; exit 1 on drift")
    ap.add_argument("--self-test", action="store_true",
                    help="re-derive the published anchors; exit 1 if any of them fails")
    ap.add_argument("--verify-oracle", action="store_true",
                    help="cross-check the rows against astropy")
    ap.add_argument("--print-table", action="store_true",
                    help="print the angle table src/parquet_skycoord.f90 carries, at 30 digits")
    args = ap.parse_args()

    if args.print_table:
        with decimal.localcontext(prec=30):
            for sysname, row in angle_table().items():
                print("%-5s %s" % (sysname, "  ".join(str(+x) for x in row)))
        return 0
    if args.self_test:
        bad = self_test()
        if bad:
            print("generate_skycoord_reference.py --self-test FAILED (%d):" % len(bad), file=sys.stderr)
            for line in bad[:40]:
                print("  " + line, file=sys.stderr)
            return 1
        print("generate_skycoord_reference.py --self-test: every anchor reproduced")
        return 0
    if args.verify_oracle:
        return verify_oracle()

    text = gen_module()
    over = [n for n, line in enumerate(text.split("\n"), start=1) if len(line) > 132]
    if over:
        print("generate_skycoord_reference.py: emitted line(s) exceed 132 columns: %s" % over[:10], file=sys.stderr)
        return 1
    if args.check:
        if not OUT_PATH.exists() or OUT_PATH.read_text() != text:
            print("generate_skycoord_reference.py --check: %s differs from a fresh generation."
                  % OUT_PATH.relative_to(REPO_ROOT), file=sys.stderr)
            print("Re-run tools/generate_skycoord_reference.py to regenerate.", file=sys.stderr)
            return 1
        print("generate_skycoord_reference.py --check: %s is current." % OUT_PATH.relative_to(REPO_ROOT))
        return 0
    OUT_PATH.write_text(text)
    print("generate_skycoord_reference.py: wrote %s (%d lines)." % (OUT_PATH.relative_to(REPO_ROOT), text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
