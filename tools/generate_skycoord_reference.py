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
                                  rotations between every ordered pair of distinct systems; and
                                  offsets and position angles.

Usage:  tools/generate_skycoord_reference.py [--check] [--self-test] [--verify-oracle]

  --check          regenerate into memory and compare with the committed file; exit 1 on any
                   difference.
  --self-test      re-derive the published anchors -- the angle table and the selectors against
                   src/parquet_skycoord.f90, the ecliptic row's closed form, every row rebuilding
                   its definition, orthonormality, round trips, the pole rules, and the offset's
                   pole convention -- and exit 1 if any of them fails.
  --verify-oracle  cross-check the emitted rows against astropy: every rotation through
                   `SkyCoord.transform_to`, every offset through `directional_offset_by` and
                   `separation`, every position angle through `position_angle`. Needs the
                   workspace `astro` environment; the two modes above need a bare `python3`, so
                   CI's lint image can run them.

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

src/parquet_skycoord.f90 carries the Galactic and ecliptic rows ICRS-referred -- recovered here from
the composite matrix at 60 digits, so the frame bias is inside them -- and the supergalactic row
Galactic-referred, exactly as defined. `--self-test` holds each of its literals to the double
nearest the model's value.

WHAT IS EXACT AND WHAT IS NOT

Every value is computed in `decimal` at 60 significant digits plus guard digits and correctly
rounded to double, so `--check` gives the same answer on every host. The library builds its
matrices from the rounded angles in double precision and computes with libm, so the suite asserts
every row to a tolerance on the sky rather than bit for bit.
"""

import argparse
import decimal
import math
import pathlib
import re
import sys

TOOLS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

import generate_random_golden_vectors as rgv  # noqa: E402
import generate_sphere_reference as sph  # noqa: E402

REPO_ROOT = TOOLS.parent
OUT_PATH = REPO_ROOT / "test" / "test_skycoord_vectors.f90"
SRC_PATH = REPO_ROOT / "src" / "parquet_skycoord.f90"

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
SELECTORS = {"ICRS": 1, "GALACTIC": 2, "ECLIPTIC": 3, "SUPERGALACTIC": 4}
ICRS, GAL, ECL, SGAL = (SELECTORS[k] for k in ("ICRS", "GALACTIC", "ECLIPTIC", "SUPERGALACTIC"))
SYSTEMS = [ICRS, GAL, ECL, SGAL]

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


def definition(system):
    """The matrix taking an ICRS unit vector to `system`'s, built from the definition."""
    if system in _CACHE:
        return _CACHE[system]
    with dctx():
        if system == ICRS:
            m = identity()
        elif system == GAL:
            bias = mm(mm(rot("x", -BIAS_ETA0), rot("y", BIAS_XI0)), rot("z", BIAS_DA0))
            m = mm(three_angle(GAL_FK5_POLE[0], GAL_FK5_POLE[1], GAL_FK5_LON0), bias)
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


def gen_module():
    rots = rotation_rows()
    offs = offset_rows()
    pas = pa_rows(offs)

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
    for sysname, system in (("gal", GAL), ("ecl", ECL)):
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

    # The pole rules: the ICRS pole is (lon0, pole_lat) in each ICRS-referred system, whatever
    # longitude names it, and a pole's longitude comes out 0.
    for sysname, system in (("gal", GAL), ("ecl", ECL)):
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
    return bad


# ---------------------------------------------------------------------------------------------
# --verify-oracle
# ---------------------------------------------------------------------------------------------

def verify_oracle():
    try:
        import astropy
        import astropy.units as u
        from astropy.coordinates import (ICRS as AICRS, BarycentricMeanEcliptic, Galactic, SkyCoord,
                                         Supergalactic)
    except ImportError as exc:
        sys.stderr.write("generate_skycoord_reference --verify-oracle needs astropy: %s\n" % exc)
        return 2
    bad = []
    counts = {"rotations": 0, "offsets": 0, "separations": 0, "position angles": 0}
    frames = {ICRS: AICRS(), GAL: Galactic(), ECL: BarycentricMeanEcliptic(), SGAL: Supergalactic()}

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

    if bad:
        sys.stderr.write("generate_skycoord_reference --verify-oracle FAILED (%d):\n" % len(bad))
        for line in bad[:40]:
            sys.stderr.write("  %s\n" % line)
        return 1
    print("generate_skycoord_reference --verify-oracle: astropy %s agrees (%s)"
          % (astropy.__version__, ", ".join("%d %s" % (v, k) for k, v in counts.items())))
    return 0


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
