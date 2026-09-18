#!/usr/bin/env python3
"""Generate the reference vectors that pin `parquet_sphere`'s contract.

`parquet_sphere` draws random points in sky polygons and HEALPix pixels by rejection, and computes
RA/Dec geometry. A wrong containment rule, area, offset or candidate order is a plausible WRONG
ANSWER rather than an abort, so the vectors below are derived here -- from a 60-digit model of the
contract built on `tools/generate_random_golden_vectors.py`'s own sphere model -- rather than read
back out of a Fortran run, which could only ever confirm that the implementation agrees with
itself.

Emitted (COMMITTED to the repository, like every other generator's output):

  test/test_sphere_vectors.f90    module `test_sphere_vectors`: the family labels and the contract
                                  identifier; polygons with their areas and acceptances;
                                  containment truth tables (chart and great-circle, the antipodes
                                  of inside points included); offsets, position angles and
                                  Fibonacci points; and polygon, pixel and mask draws with the
                                  candidate count each took.

Usage:  tools/generate_sphere_reference.py [--check] [--self-test] [--verify-oracle]

  --check          regenerate into memory and compare with the committed file; exit 1 on any
                   difference.
  --self-test      re-derive the published anchors -- the labels and identifier against
                   src/parquet_sphere.f90, the closed-form areas (a rectangle, a polar cap, the
                   octant), the offset's pole convention, the two containment models' agreement,
                   and the decimal pixel model against tools/generate_healpix_reference.py's --
                   and exit 1 if any of them fails.
  --verify-oracle  cross-check the emitted rows against astropy (offsets, position angles, the
                   Fibonacci grid), matplotlib (chart containment) and healpy (great-circle
                   containment by pixel centre, areas by pixel count, and every pixel draw's
                   pixel). Needs the workspace `astro` environment; the two modes above need a bare
                   `python3`, so CI's lint image can run them.

NEVER HAND-EDIT A VECTOR. A contract change is an edit to the model below plus a regeneration.

--------------------------------------------------------------------------------------------
WHAT IS EXACT AND WHAT IS NOT

Every value the model emits is computed in `decimal` at 60 significant digits plus guard digits
and correctly rounded to double, so `--check` gives the same answer on every host. The library
computes the same quantities in double precision with libm, so the suite asserts them to a
tolerance rather than bit for bit -- except the uniforms, the candidate counts and the
containment answers, which are exact.

A value is pinned only where rounding cannot change it. A containment probe keeps a margin from
every edge; a draw row is emitted only when every candidate of its walk, rejected or accepted,
keeps a margin from the polygon's boundary or the pixel's -- a candidate that close could be
decided the other way by an implementation an ulp away -- and only when the frame axis of each
disc draw is unambiguous. A float predicate (libm) is used only to decide such a margin, never to
produce a value, which is what keeps the output host-independent.

Regenerates `test/test_sphere_vectors.f90`, the golden rows behind `test/test_sphere.f90`: a
60-digit model of `parquet_sphere` built on `tools/generate_random_golden_vectors.py`'s sphere
model, with `--self-test` for its anchors and `--verify-oracle` for astropy, matplotlib and
healpy. Maintainer-only.
"""

import argparse
import decimal
import fractions
import math
import pathlib
import random
import re
import struct
import sys

TOOLS = pathlib.Path(__file__).resolve().parent
sys.path.insert(0, str(TOOLS))

import generate_random_golden_vectors as rgv  # noqa: E402
import generate_healpix_reference as hpr  # noqa: E402

REPO_ROOT = TOOLS.parent
OUT_PATH = REPO_ROOT / "test" / "test_sphere_vectors.f90"
SRC_PATH = REPO_ROOT / "src" / "parquet_sphere.f90"

D = decimal.Decimal
DPI = rgv.DPI
dctx = rgv.dctx
dsin_cos = rgv.dsin_cos
datan2 = rgv.datan2

# ---------------------------------------------------------------------------------------------
# The contract constants. `self_test` reads every one back from src/parquet_sphere.f90.
# ---------------------------------------------------------------------------------------------

LABELS = {
    "polygon": 6296613622937029849,
    "pixel": 4487668616495134178,
    "mask_choice": 2365686713864110313,
    "mask_point": 3124069785697503286,
}
ALGORITHM = "region:box+cap/gnomonic-evenodd/1e-3/v1"
CANDIDATE_CAP = 100000
HEMISPHERE_DEG = D("89.9")

# The measurement `%init` replaces a self-intersecting polygon's signed-sum area with: the fraction
# of an `R2` low-discrepancy lattice falling inside the even-odd interior. The lattice steps in
# integers modulo 2**53, so this model reproduces the library's points exactly rather than
# approximately, and the whole measurement is float64 arithmetic mirroring the Fortran statement for
# statement. `AREA_TOL` is the relative tolerance the golden rows carry for a measured area: the
# lattice's own discretisation error at 2**18 points is about 1e-4 (measured against 2**22), so a
# tighter pin would be pinning noise rather than the contract.
AREA_SAMPLES = 262144
LAT_M = 9007199254740992
LAT_A1 = 6799333552837831
LAT_A2 = 5132665044399055
AREA_TOL = 1.0e-4
EXACT_AREA_TOL = 1.0e-12

EDGE_RADEC = 0
EDGE_GREAT_CIRCLE = 1
HP_RING = 0
HP_NEST = 1

# ---------------------------------------------------------------------------------------------
# Decimal geometry
# ---------------------------------------------------------------------------------------------


def dd(x):
    """A double (or int, or Decimal) as an exact Decimal."""
    return x if isinstance(x, D) else D(x)


def deg2rad(x):
    with dctx():
        return dd(x) * DPI / 180


def rad2deg(x):
    with dctx():
        return dd(x) * 180 / DPI


def dsqrt(x):
    with dctx():
        return dd(x).sqrt()


def dot(a, b):
    with dctx():
        return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def cross(a, b):
    with dctx():
        return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]


def norm(a):
    with dctx():
        return dot(a, a).sqrt()


def dangle(a, b):
    """The angle between two vectors, radians: `atan2(|a x b|, a.b)`."""
    with dctx():
        return datan2(norm(cross(a, b)), dot(a, b))


def dec_sin_cos(dec):
    """`(sin, cos)` of a declination in degrees, exactly `(+/-1, 0)` at `+/-90`: the pole rule."""
    if dec == 90.0:
        return D(1), D(0)
    if dec == -90.0:
        return D(-1), D(0)
    return dsin_cos(deg2rad(dec))


def radec_unit(ra, dec):
    """`pf_radec2vec` in the standard frame."""
    with dctx():
        sd, cd = dec_sin_cos(dec)
        if cd == 0:
            return [D(0), D(0), sd]
        sa, ca = dsin_cos(deg2rad(ra))
        return [cd * ca, cd * sa, sd]


def unit_radec(v):
    """`(ra, dec)` in degrees of a nonzero vector, standard frame; a pole's right ascension is 0."""
    with dctx():
        if v[0] == 0 and v[1] == 0:
            ra = D(0)
        else:
            ra = rad2deg(datan2(v[1], v[0]))
            if ra < 0:
                ra += 360
            if ra >= 360:
                ra = D(0)
        dec = rad2deg(datan2(v[2], (v[0] * v[0] + v[1] * v[1]).sqrt()))
        return ra, dec


def dasin(s):
    with dctx():
        s = dd(s)
        return datan2(s, ((1 - s) * (1 + s)).sqrt())


def dwrap360(x):
    with dctx():
        x = dd(x) % 360
        if x < 0:
            x += 360
        return x


def danint(x):
    """Fortran's `anint`: nearest integer, halves away from zero."""
    with dctx():
        return dd(x).to_integral_value(rounding=decimal.ROUND_HALF_UP)


# ---- Offsets, position angles and the Fibonacci grid ----

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
        dl = dd(ra2) - dd(ra1)
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


PHI = None


def golden_step_deg():
    global PHI
    with dctx():
        if PHI is None:
            PHI = (1 + D(5).sqrt()) / 2
        return 360 / PHI


def fibonacci_point(n, k):
    """Point `k` (from 0) of the `n`-point grid: `(ra, dec)` in degrees."""
    with dctx():
        t = D(k) + D("0.5")
        ra = dwrap360(t * golden_step_deg())
        z = (D(n) - 2 * t) / D(n)
        s = 2 * (t * (D(n) - t)).sqrt() / D(n)
        return ra, rad2deg(datan2(z, s))


# ---------------------------------------------------------------------------------------------
# Polygons
#
# A great-circle fixture has a generic vertex sum on purpose: a centre with two components that
# tie exactly (the octant's (1, 1, 1), a polygon symmetric about a pole) chooses its disc frame's
# axis by the rounding of each implementation, so no draw from it can be pinned.
# ---------------------------------------------------------------------------------------------

CHART_POLYGONS = [
    ("rectangle", [10.0, 30.0, 30.0, 10.0], [-5.0, -5.0, 5.0, 5.0]),
    ("L-shape", [0.0, 20.0, 20.0, 10.0, 10.0, 0.0], [0.0, 0.0, 10.0, 10.0, 20.0, 20.0]),
    ("band across ra = 0", [350.0, 370.0, 370.0, 350.0], [-10.0, -10.0, 10.0, 10.0]),
    ("polar cap", [0.0, 360.0, 360.0, 0.0], [80.0, 80.0, 90.0, 90.0]),
    ("slanted quadrilateral", [100.0, 140.0, 150.0, 95.0], [-40.0, -30.0, 10.0, -5.0]),
    ("whole-sky band", [0.0, 360.0, 360.0, 0.0], [-30.0, -30.0, 30.0, 30.0]),
    ("small box beside the pole", [40.0, 41.0, 41.0, 40.0], [89.9, 89.9, 89.91, 89.91]),
    ("bowtie", [200.0, 220.0, 200.0, 220.0], [0.0, 20.0, 20.0, 0.0]),
]

GC_POLYGONS = [
    ("convex quadrilateral", [10.0, 60.0, 50.0, 0.0], [0.0, 5.0, 40.0, 30.0]),
    ("L-shape", [0.0, 30.0, 30.0, 15.0, 15.0, 0.0], [0.0, 0.0, 15.0, 15.0, 30.0, 30.0]),
    ("around the north pole", [10.0, 100.0, 200.0, 280.0], [70.0, 72.0, 68.0, 75.0]),
    ("tall triangle", [20.0, 100.0, 50.0], [5.0, 10.0, 80.0]),
    ("across ra = 0", [340.0, 20.0, 25.0, 345.0], [-20.0, -25.0, 15.0, 20.0]),
    ("big triangle", [0.0, 125.0, 240.0], [10.0, 12.0, 8.0]),
    ("ten arcminutes", [150.0, 150.1666666666666667, 150.1666666666666667, 150.0],
     [-30.0, -30.0, -29.8333333333333333, -29.8333333333333333]),
]


def fside(ax, ay, bx, by, cx, cy):
    """The sign of `(b - a) x (c - a)`, as `sky_side` computes it."""
    d = (bx - ax) * (cy - ay) - (by - ay) * (cx - ax)
    return 1 if d > 0.0 else (-1 if d < 0.0 else 0)


def self_intersects(px, py):
    """Whether two non-adjacent edges of the closed polygon cross, as `sky_self_intersects` decides.

    The plane is the containment test's: `(ra, dec)` for a chart polygon, the gnomonic `(x, y)` for
    a great-circle one, in which every great circle is a straight line."""
    n = len(px)
    if n < 4:
        return False
    for k in range(n - 1):
        kn = k + 1
        for j in range(k + 2, n):
            jn = 0 if j == n - 1 else j + 1
            if jn == k:
                continue
            s1 = fside(px[k], py[k], px[kn], py[kn], px[j], py[j])
            s2 = fside(px[k], py[k], px[kn], py[kn], px[jn], py[jn])
            if s1 * s2 >= 0:
                continue
            s3 = fside(px[j], py[j], px[jn], py[jn], px[k], py[k])
            s4 = fside(px[j], py[j], px[jn], py[jn], px[kn], py[kn])
            if s3 * s4 < 0:
                return True
    return False


def lattice_uniforms():
    """The `R2` lattice `sky_polygon_measure` walks: `AREA_SAMPLES` pairs in `[0, 1)`."""
    x1 = x2 = 0
    sc = 1.0 / LAT_M
    for _ in range(AREA_SAMPLES):
        x1 = (x1 + LAT_A1) % LAT_M
        x2 = (x2 + LAT_A2) % LAT_M
        yield x1 * sc, x2 * sc


def fdec_sin_cos(dec):
    """`sky_dec_sin_cos` in float64: the pole exact, everything else the libm's sine and cosine."""
    if dec == 90.0:
        return 1.0, 0.0
    if dec == -90.0:
        return -1.0, 0.0
    d = dec * (math.pi / 180.0)
    return math.sin(d), math.cos(d)


def feven_odd(px, py, x, y):
    """`sky_even_odd` in float64."""
    inside = False
    n = len(px)
    j = n - 1
    for k in range(n):
        if (py[k] > y) != (py[j] > y):
            if x < px[k] + (y - py[k]) * (px[j] - px[k]) / (py[j] - py[k]):
                inside = not inside
        j = k
    return inside


def measure_chart(ra, dec):
    """`sky_polygon_measure`'s `PF_EDGE_RADEC` arm, statement for statement."""
    ra_lo, ra_hi = min(ra), max(ra)
    dec_lo, dec_hi = min(dec), max(dec)
    sin_lo = fdec_sin_cos(dec_lo)[0]
    lo = dec_lo * (math.pi / 180.0)
    hi = dec_hi * (math.pi / 180.0)
    sin_span = 2.0 * math.cos(0.5 * (hi + lo)) * math.sin(0.5 * (hi - lo))
    nin = 0
    for u1, u2 in lattice_uniforms():
        ra_c = ra_lo + u1 * (ra_hi - ra_lo)
        s = min(max(sin_lo + u2 * sin_span, -1.0), 1.0)
        dec_c = math.asin(s) * (180.0 / math.pi)
        if feven_odd(ra, dec, ra_c, dec_c):
            nin += 1
    return nin / AREA_SAMPLES


def measure_cap(px, py, cap_radius):
    """`sky_polygon_measure`'s `PF_EDGE_GREAT_CIRCLE` arm, statement for statement.

    The candidate never becomes a unit vector: at offset `h` and azimuth `phi` its gnomonic
    coordinates in the cap's own frame are `(sin*cos(phi), sin*sin(phi))` over `1 - h`."""
    hemi_cos = 1.7453283658983088e-3              # sky_hemisphere_cos, cos(89.9 degrees)
    dh = 2.0 * math.sin(0.5 * cap_radius) ** 2
    nin = 0
    for u1, u2 in lattice_uniforms():
        h = u1 * dh
        zc = 1.0 - h
        sn = math.sqrt(h * (2.0 - h))
        phi = 2.0 * math.pi * u2
        if zc > 0.5 * hemi_cos:
            if feven_odd(px, py, sn * math.cos(phi) / zc, sn * math.sin(phi) / zc):
                nin += 1
    return nin / AREA_SAMPLES


class ChartPolygon:
    """A `PF_EDGE_RADEC` polygon: containment exact in rationals, area and box in decimal."""

    def __init__(self, name, ra, dec):
        self.name, self.ra, self.dec = name, ra, dec
        self.rule = EDGE_RADEC
        self.fra = [fractions.Fraction(x) for x in ra]
        self.fdec = [fractions.Fraction(x) for x in dec]
        self.ra_lo, self.ra_hi = min(ra), max(ra)
        self.dec_lo, self.dec_hi = min(dec), max(dec)
        with dctx():
            self.sin_lo = dec_sin_cos(self.dec_lo)[0]
            self.sin_hi = dec_sin_cos(self.dec_hi)[0]
            self.box = deg2rad(D(self.ra_hi) - D(self.ra_lo)) * (self.sin_hi - self.sin_lo)
            self.area = self.exact_area()
            self.acceptance = min(self.area / self.box, D(1))
        # A self-intersecting polygon's exact area is a signed sum, which is not the region the
        # even-odd rule samples; `%init` measures that instead, and so does this.
        self.simple = not self_intersects(self.ra, self.dec)
        if not self.simple:
            with dctx():
                f = D(repr(measure_chart(self.ra, self.dec)))
                self.area = f * self.box
                self.acceptance = min(f, D(1))

    def exact_area(self):
        """Green's theorem, `sum (ra2 - ra1)*(cos(d1) - cos(d2))/(d2 - d1)`, level edges
        `(ra2 - ra1)*sin(d1)`; at 85 digits nothing here cancels."""
        with dctx():
            total = D(0)
            n = len(self.ra)
            for k in range(n):
                j = (k + 1) % n
                dra = deg2rad(D(self.ra[j]) - D(self.ra[k]))
                d1, d2 = deg2rad(self.dec[k]), deg2rad(self.dec[j])
                if self.dec[k] == self.dec[j]:
                    total += dra * dec_sin_cos(self.dec[k])[0]
                else:
                    c1 = dec_sin_cos(self.dec[k])[1]
                    c2 = dec_sin_cos(self.dec[j])[1]
                    total += dra * (c1 - c2) / (d2 - d1)
            return abs(total)

    def inside_chart(self, x, y):
        """Even-odd in exact rationals, the test point already in the polygon's RA range."""
        inside = False
        n = len(self.fra)
        j = n - 1
        for k in range(n):
            if (self.fdec[k] > y) != (self.fdec[j] > y):
                if x < self.fra[k] + (y - self.fdec[k]) * (self.fra[j] - self.fra[k]) / (self.fdec[j] - self.fdec[k]):
                    inside = not inside
            j = k
        return inside

    def wrap(self, ra):
        """`ra` into `[ra_lo, ra_lo + 360)`, exactly."""
        lo = fractions.Fraction(self.ra_lo)
        return lo + (fractions.Fraction(ra) - lo) % 360

    def contains(self, ra, dec):
        if not (-90.0 <= float(dec) <= 90.0):
            return False
        return self.inside_chart(self.wrap(ra), fractions.Fraction(dec))

    def margin(self, ra, dec):
        """The chart distance, degrees, from `(ra, dec)` (RA wrapped) to the nearest edge."""
        x = float(self.wrap(ra))
        y = float(dec)
        best = math.inf
        n = len(self.ra)
        for k in range(n):
            j = (k + 1) % n
            best = min(best, segment_distance(x, y, self.ra[k], self.dec[k], self.ra[j], self.dec[j]))
        return best


def segment_distance(x, y, x1, y1, x2, y2):
    dx, dy = x2 - x1, y2 - y1
    L2 = dx * dx + dy * dy
    t = 0.0 if L2 == 0 else max(0.0, min(1.0, ((x - x1) * dx + (y - y1) * dy) / L2))
    return math.hypot(x - (x1 + t * dx), y - (y1 + t * dy))


class GreatCirclePolygon:
    """A `PF_EDGE_GREAT_CIRCLE` polygon: centre, cap and area in decimal; containment by winding."""

    def __init__(self, name, ra, dec):
        self.name, self.ra, self.dec = name, ra, dec
        self.rule = EDGE_GREAT_CIRCLE
        self.ra_lo, self.ra_hi = min(ra), max(ra)
        self.dec_lo, self.dec_hi = min(dec), max(dec)
        with dctx():
            self.u = [radec_unit(r, d) for r, d in zip(ra, dec)]
            s = [sum(v[j] for v in self.u) for j in range(3)]
            n = norm(s)
            self.c = [x / n for x in s]
            _, hemi_cos = dsin_cos(deg2rad(HEMISPHERE_DEG))
            for k, v in enumerate(self.u):
                if not dot(v, self.c) > hemi_cos:
                    raise SystemExit("%s: vertex %d breaks the hemisphere rule" % (name, k + 1))
            self.radius = max(dangle(self.c, v) for v in self.u)
            total = D(0)
            m = len(self.u)
            for k in range(m):
                a, b = self.u[k], self.u[(k + 1) % m]
                num = dot(self.c, cross(a, b))
                den = 1 + dot(self.c, a) + dot(a, b) + dot(b, self.c)
                total += 2 * datan2(num, den)
            self.area = abs(total)
            sh, _ = dsin_cos(self.radius / 2)
            self.box = 4 * DPI * sh * sh
            self.acceptance = min(self.area / self.box, D(1))
        self.fu = [[float(x) for x in v] for v in self.u]
        self.fc = [float(x) for x in self.c]
        e1, e2 = frame_of(self.fc)
        self.fpx = [fdot(v, e1) / fdot(v, self.fc) for v in self.fu]
        self.fpy = [fdot(v, e2) / fdot(v, self.fc) for v in self.fu]
        self.simple = not self_intersects(self.fpx, self.fpy)
        if not self.simple:
            with dctx():
                f = D(repr(measure_cap(self.fpx, self.fpy, float(self.radius))))
                self.area = f * self.box
                self.acceptance = min(f, D(1))
        self.normals = []
        for k in range(len(self.fu)):
            nk = fcross(self.fu[k], self.fu[(k + 1) % len(self.fu)])
            ln = math.sqrt(fdot(nk, nk))
            self.normals.append([x / ln for x in nk])

    def winding_inside(self, p):
        """Inside by the signed-angle winding sum, restricted to the centre's open hemisphere."""
        if fdot(p, self.fc) <= 0.0:
            return False
        total = 0.0
        m = len(self.fu)
        for k in range(m):
            a, b = self.fu[k], self.fu[(k + 1) % m]
            total += math.atan2(fdot(p, fcross(a, b)), fdot(a, b) - fdot(p, a) * fdot(p, b))
        return abs(total) > math.pi

    def gnomonic_inside(self, p):
        """Inside by the library's own rule: the far hemisphere out, then gnomonic even-odd."""
        e1, e2 = frame_of([float(x) for x in self.c])
        z = fdot(p, self.fc)
        if z <= 0.5 * math.sin(math.radians(0.1)):
            return False
        px = [fdot(v, e1) / fdot(v, self.fc) for v in self.fu]
        py = [fdot(v, e2) / fdot(v, self.fc) for v in self.fu]
        x, y = fdot(p, e1) / z, fdot(p, e2) / z
        inside = False
        j = len(px) - 1
        for k in range(len(px)):
            if (py[k] > y) != (py[j] > y):
                if x < px[k] + (y - py[k]) * (px[j] - px[k]) / (py[j] - py[k]):
                    inside = not inside
            j = k
        return inside

    def margin(self, p):
        """A lower bound on the angular distance from `p` to the boundary: the nearest edge's great
        circle (or its end points), and the centre's hemisphere boundary."""
        best = abs(fdot(p, self.fc))
        m = len(self.fu)
        for k in range(m):
            a, b = self.fu[k], self.fu[(k + 1) % m]
            n = self.normals[k]
            # The foot of the perpendicular lies within the arc when it is on the inner side of
            # both end points; otherwise the nearer end point is the closest point.
            foot = [p[j] - fdot(p, n) * n[j] for j in range(3)]
            if fdot(fcross(a, foot), n) >= 0.0 and fdot(fcross(foot, b), n) >= 0.0:
                best = min(best, abs(fdot(p, n)))
            else:
                best = min(best, fangle(p, a), fangle(p, b))
        return best

    def contains(self, ra, dec):
        if not (-90.0 <= float(dec) <= 90.0):
            return False
        return self.winding_inside([float(x) for x in radec_unit(ra, dec)])


def fdot(a, b):
    return a[0] * b[0] + a[1] * b[1] + a[2] * b[2]


def fcross(a, b):
    return [a[1] * b[2] - a[2] * b[1], a[2] * b[0] - a[0] * b[2], a[0] * b[1] - a[1] * b[0]]


def fangle(a, b):
    c = fcross(a, b)
    return math.atan2(math.sqrt(fdot(c, c)), fdot(a, b))


def frame_of(c):
    """The disc's frame rule on a float vector: `e1 = c x axis` of its smallest component."""
    k = 0
    if abs(c[1]) < abs(c[k]):
        k = 1
    if abs(c[2]) < abs(c[k]):
        k = 2
    w = [[0.0, c[2], -c[1]], [-c[2], 0.0, c[0]], [c[1], -c[0], 0.0]][k]
    ln = math.sqrt(fdot(w, w))
    e1 = [x / ln for x in w]
    return e1, fcross(c, e1)


def polygons():
    return ([ChartPolygon(*p) for p in CHART_POLYGONS] + [GreatCirclePolygon(*p) for p in GC_POLYGONS])


# ---------------------------------------------------------------------------------------------
# HEALPix, in decimal: a pixel's centre, the bounding cap, and the pixel of a direction
# ---------------------------------------------------------------------------------------------

def pix2vec(nside, scheme, p):
    """The centre of pixel `p` as an exact unit vector."""
    pr = hpr.nest2ring(nside, p) if scheme == HP_NEST else p
    i, j, nr, shifted = hpr.ring_decompose(nside, pr)
    with dctx():
        n = D(nside)
        if i < nside:
            z = 1 - D(i * i) / (3 * n * n)
        elif i > 3 * nside:
            k = 4 * nside - i
            z = -(1 - D(k * k) / (3 * n * n))
        else:
            z = D(2 * nside - i) * 2 / (3 * n)
        phi = (D(j) + D(shifted) / 2) * 2 * DPI / D(nr)
        st = ((1 - z) * (1 + z)).sqrt()
        sp, cp = dsin_cos(phi)
        return [st * cp, st * sp, z]


def max_pixrad(nside):
    """`pf_max_pixrad`: the angle from the first equatorial ring's centre to its north corner."""
    with dctx():
        n = D(nside)
        zc = D(2) / 3
        sc = (1 - zc * zc).sqrt()
        zv = 1 - (n - 1) * (n - 1) / (3 * n * n)
        sv = ((1 - zv) * (1 + zv)).sqrt()
        sp, cp = dsin_cos(DPI / (4 * n))
        return dangle([sc, D(0), zc], [sv * cp, sv * sp, zv])


def vec2pix(nside, scheme, v):
    """The pixel containing a unit vector, exactly: the pixelisation's own equations in decimal,
    with `1 - |z|` formed from the transverse components so a polar ring is resolved."""
    with dctx():
        x, y, z = v
        n = nside
        za = abs(z)
        if x == 0 and y == 0:
            phi = D(0)
        else:
            phi = datan2(y, x)
            if phi < 0:
                phi += 2 * DPI
        tt = (phi / (DPI / 2)) % 4
        if za <= D(2) / 3:
            temp1 = n * (D("0.5") + tt)
            temp2 = n * (z * D("0.75"))
            jp = int((temp1 - temp2).to_integral_value(rounding=decimal.ROUND_FLOOR))
            jm = int((temp1 + temp2).to_integral_value(rounding=decimal.ROUND_FLOOR))
            if scheme == HP_RING:
                ir = n + 1 + jp - jm
                kshift = 1 - (ir & 1)
                ip = (jp + jm - n + kshift + 1) // 2
                ip %= 4 * n
                return hpr.ncap_of(n) + (ir - 1) * 4 * n + ip
            ifp, ifm = jp // n, jm // n
            if ifp == ifm:
                face = (ifp & 3) + 4
            elif ifp < ifm:
                face = ifp & 3
            else:
                face = (ifm & 3) + 8
            return face * n * n + hpr.interleave(jm % n, n - 1 - (jp % n))
        ntt = int(tt)
        if ntt >= 4:
            ntt = 3
        tp = tt - ntt
        one_minus_za = (x * x + y * y) / (1 + za)
        tmp = n * (3 * one_minus_za).sqrt()
        jp = int((tp * tmp).to_integral_value(rounding=decimal.ROUND_FLOOR))
        jm = int(((1 - tp) * tmp).to_integral_value(rounding=decimal.ROUND_FLOOR))
        if scheme == HP_RING:
            ir = jp + jm + 1
            ip = int((tt * ir).to_integral_value(rounding=decimal.ROUND_FLOOR)) % (4 * ir)
            if z > 0:
                return 2 * ir * (ir - 1) + ip
            return hpr.npix_of(n) - 2 * ir * (ir + 1) + ip
        jp = min(jp, n - 1)
        jm = min(jm, n - 1)
        if z > 0:
            return ntt * n * n + hpr.interleave(n - 1 - jm, n - 1 - jp)
        return (ntt + 8) * n * n + hpr.interleave(jp, jm)


def pixel_stable(nside, scheme, v, delta):
    """Whether every direction within about `delta` of `v` along the local axes is in `v`'s pixel."""
    base = vec2pix(nside, scheme, v)
    with dctx():
        e1, e2 = frame_of([float(x) for x in v])
        for e in (e1, e2):
            for s in (1, -1):
                w = [v[j] + s * D(delta) * D(e[j]) for j in range(3)]
                ln = norm(w)
                if vec2pix(nside, scheme, [x / ln for x in w]) != base:
                    return False
    return True


# ---------------------------------------------------------------------------------------------
# The samplers
# ---------------------------------------------------------------------------------------------

class Unstable(Exception):
    """A walk met a candidate too near a boundary, or a disc frame whose axis nearly ties."""


def axis_ok(c):
    a = sorted(abs(float(x)) for x in c)
    return a[0] == a[1] == 0.0 or a[1] - a[0] > 1e-9


def polygon_walk(poly, seed, stream, draw):
    """`pf_sky_polygon%random_at`: `(ra, dec, ncand)`, or `Unstable`."""
    d = rgv.clamp_draw(draw)
    key = rgv.random_key(rgv.random_key(seed, LABELS["polygon"]), d)
    for k in range(1, CANDIDATE_CAP + 1):
        if poly.rule == EDGE_RADEC:
            u1 = rgv.at(key, stream, 2 * k - 1)
            u2 = rgv.at(key, stream, 2 * k)
            with dctx():
                ra_c = D(poly.ra_lo) + D(u1) * (D(poly.ra_hi) - D(poly.ra_lo))
                s = poly.sin_lo + D(u2) * (poly.sin_hi - poly.sin_lo)
                dec_c = rad2deg(dasin(s))
            if poly.margin(fractions.Fraction(ra_c), float(dec_c)) < 1e-9:
                raise Unstable
            if poly.inside_chart(fractions.Fraction(ra_c), fractions.Fraction(dec_c)):
                return dwrap360(ra_c), dec_c, k
        else:
            if not axis_ok(poly.c):
                raise Unstable
            v = rgv.sphere_disc(key, stream, k, poly.c, poly.radius, 0.0)[0]
            fv = [float(x) for x in v]
            if poly.margin(fv) < 1e-9:
                raise Unstable
            if poly.winding_inside(fv):
                ra, dec = unit_radec(v)
                return ra, dec, k
    raise SystemExit("%s: a walk reached the candidate cap" % poly.name)


JRLL = [2, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4]
JPLL = [1, 3, 5, 7, 0, 2, 4, 6, 1, 3, 5, 7]


def compact_bits(v):
    """The exact inverse of the bit spread: gathers the even bits of `v`."""
    v &= 0x5555555555555555
    v = (v | (v >> 1)) & 0x3333333333333333
    v = (v | (v >> 2)) & 0x0F0F0F0F0F0F0F0F
    v = (v | (v >> 4)) & 0x00FF00FF00FF00FF
    v = (v | (v >> 8)) & 0x0000FFFF0000FFFF
    v = (v | (v >> 16)) & 0x00000000FFFFFFFF
    return v


def pix_point(nside, scheme, ipix, dx, dy):
    """`%pix2vec_offset`: the direction at fractional position `(dx, dy)` inside a pixel.

    The continuous inverse of the HEALPix projection, in decimal. `dx` and `dy` are library
    uniforms, so they are exact dyadic rationals and the whole expression is exact to the working
    precision -- this is a reference, not a re-derivation of the float64 path. `(0.5, 0.5)` must
    reproduce `pix2vec`, which `self_test` checks."""
    ipn = hpr.ring2nest(nside, ipix) if scheme == HP_RING else ipix
    face, low = divmod(ipn, nside * nside)
    ix = compact_bits(low)
    iy = compact_bits(low >> 1)
    with dctx():
        n = D(nside)
        x = (D(ix) + D(dx)) / n
        y = (D(iy) + D(dy)) / n
        jr = D(JRLL[face]) - x - y
        if jr < 1:
            nr = jr
            z = 1 - nr * nr / 3
            st = nr * ((1 + z) / 3).sqrt()
        elif jr > 3:
            nr = 4 - jr
            z = nr * nr / 3 - 1
            st = nr * ((1 - z) / 3).sqrt()
        else:
            nr = D(1)
            z = (2 - jr) * 2 / 3
            st = ((1 - z) * (1 + z)).sqrt()
        tmp = D(JPLL[face]) * nr + x - y
        if tmp < 0:
            tmp += 8
        if tmp >= 8:
            tmp -= 8
        phi = DPI * tmp / (4 * nr)
        sp, cp = dsin_cos(phi)
        return [st * cp, st * sp, z]


def pixel_walk(nside, scheme, label, seed, stream, ipix, d):
    """`pf_random_pixel_at` under `label`: `(v, ncand)`, or `Unstable`.

    **Nothing is rejected.** The pixel is a square in the equal-area projection, so the two uniforms
    of one block, read as a position across and along that square, ARE a point uniform over the
    pixel; `ncand` is 1 for every draw. A row whose point lies within rounding of a pixel boundary
    is still refused, so the suite's `%vec2pix` assertions on the mask rows stay decidable."""
    key = rgv.random_key(rgv.random_key(seed, label), d)
    c = rgv.block(key, stream, 0)
    u1 = float(((c[1] << 32) | c[0]) >> 11) * 2.0 ** -53
    u2 = float(((c[3] << 32) | c[2]) >> 11) * 2.0 ** -53
    v = pix_point(nside, scheme, ipix, u1, u2)
    delta = min(1e-12, 1e-4 * float(max_pixrad(nside)))
    if not pixel_stable(nside, scheme, v, delta):
        raise Unstable
    return v, 1


def pixel_draw(nside, scheme, seed, stream, ipix, draw):
    return pixel_walk(nside, scheme, LABELS["pixel"], seed, stream, ipix, rgv.clamp_draw(draw))


def mask_draw(nside, scheme, seed, stream, pixels, draw):
    """`pf_random_mask_at`: `(v, j)` with `j` the 1-based chosen entry."""
    d = rgv.clamp_draw(draw)
    j, _ = rgv.int_at(rgv.random_key(seed, LABELS["mask_choice"]), stream, 1, len(pixels), d)
    v, _ = pixel_walk(nside, scheme, LABELS["mask_point"], seed, stream, pixels[j - 1], d)
    return v, j


# ---------------------------------------------------------------------------------------------
# The rows
# ---------------------------------------------------------------------------------------------

def containment_rows(polys):
    """`(polygon index, ra, dec, inside)`: random probes kept away from every edge, and for a
    great-circle polygon the antipode of every inside probe, which must read outside."""
    rng = random.Random(20260917)
    rows = []
    for pi, poly in enumerate(polys):
        kept = 0
        inside_count = 0
        tries = 0
        while kept < 24 and tries < 100000:
            tries += 1
            if poly.rule == EDGE_RADEC:
                # Probes in and around the polygon's own box, written anywhere in RA.
                wide = poly.ra_hi - poly.ra_lo
                tall = poly.dec_hi - poly.dec_lo
                ra = poly.ra_lo - 0.25 * wide + rng.random() * 1.5 * wide
                ra += 360.0 * rng.choice([-1, 0, 0, 1])
                dec = max(-90.0, min(90.0, poly.dec_lo - 0.25 * tall + rng.random() * 1.5 * tall))
                ra = round(ra, 6)
                dec = round(dec, 6)
                if poly.margin(ra, dec) < 1e-6:
                    continue
                inside = poly.contains(ra, dec)
            else:
                z = 2.0 * rng.random() - 1.0
                if rng.random() < 0.8:
                    # Most probes near the polygon: within twice its cap radius of the centre.
                    ang = rng.random() * min(math.pi, 2.0 * float(poly.radius))
                    az = 2.0 * math.pi * rng.random()
                    e1, e2 = frame_of(poly.fc)
                    p = [math.cos(ang) * poly.fc[j] + math.sin(ang) * (math.cos(az) * e1[j] + math.sin(az) * e2[j])
                         for j in range(3)]
                else:
                    s = math.sqrt(max(0.0, 1.0 - z * z))
                    az = 2.0 * math.pi * rng.random()
                    p = [s * math.cos(az), s * math.sin(az), z]
                ra = round(math.degrees(math.atan2(p[1], p[0])) % 360.0, 6)
                dec = round(math.degrees(math.atan2(p[2], math.hypot(p[0], p[1]))), 6)
                fp = [float(x) for x in radec_unit(ra, dec)]
                if poly.margin(fp) < 1e-6:
                    continue
                inside = poly.winding_inside(fp)
            rows.append((pi, ra, dec, inside))
            kept += 1
            if inside:
                inside_count += 1
                if poly.rule == EDGE_GREAT_CIRCLE:
                    ara = (ra + 180.0) % 360.0
                    if poly.winding_inside([float(x) for x in radec_unit(ara, -dec)]):
                        raise SystemExit("%s: the antipode of an inside probe reads inside" % poly.name)
                    rows.append((pi, ara, -dec, False))
        if inside_count < 4:
            raise SystemExit("%s: only %d inside probes" % (poly.name, inside_count))
    return rows


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


FIB_CASES = [(1, [0]), (2, [0, 1]), (7, list(range(7))), (1000, [0, 1, 2, 499, 500, 998, 999])]


def fibonacci_rows():
    rows = []
    for n, ks in FIB_CASES:
        for k in ks:
            ra, dec = fibonacci_point(n, k)
            # The unwrapped longitude is `(k + 1/2)*222.49` degrees, and its rounding survives the wrap.
            tol = 1e-12 + 8 * 2.2e-16 * (k + 0.5) * 222.5
            rows.append((n, k + 1, ra, dec, tol))
    return rows


DRAW_COORDS = [(1, 0, 1), (-7, 5, 3), (rgv.INT64_MAX, 0, 0), (12345, -3, 2), (1, 5, 7), (2026, 1000000, 1)]


def polygon_draw_rows(polys):
    rows = []
    for pi, poly in enumerate(polys):
        kept = 0
        for n, (seed, stream, draw) in enumerate(DRAW_COORDS + [(s + 11, t + 1, d + 4) for s, t, d in DRAW_COORDS]):
            if kept == 4:
                break
            try:
                ra, dec, ncand = polygon_walk(poly, seed, stream, draw)
            except Unstable:
                continue
            rows.append((pi, seed, stream, draw, ncand, ra, dec))
            kept += 1
        if kept < 4:
            raise SystemExit("%s: too few stable draw rows" % poly.name)
    return rows


# The grids and the pixels whose draws are pinned: for each starting index, the first pixel at or after
# it whose centre does not tie two components (see `axis_ok`) and lies at |z| <= 0.9. nside = 1 has no
# such pixel -- every one of its twelve centres lies on a symmetry plane -- and is covered by the
# suite's statistics.
#
# **No pixel near a pole is pinned.** The library forms a centre's transverse length as
# `sqrt((1 - z)*(1 + z))` from a rounded `z`, so near a pole an ulp of `z` becomes `eps/sin(theta)` of
# the centre -- 40 ulp at nside 64's first ring -- and which ulp is the compiler's choice. The suite
# covers those pixels by statistics and by testing that the cap covers them.
PIXEL_GRIDS = [
    (8, HP_NEST, [300, 400, 700]),
    (64, HP_RING, [6000, 24000, 42000]),
    (1024, HP_RING, [718800, 6000000, 11800000]),
    (1 << 20, HP_NEST, [5 * (1 << 40) + 77, 12 * (1 << 40) // 2 + 1001]),
    (1 << 24, HP_RING, [12 * (1 << 48) // 2 + 3, 12 * (1 << 48) // 3 + 17]),
]


def generic_pixel(nside, scheme, start):
    """The first pixel at or after `start` whose centre chooses its disc frame without a tie and lies
    away from both poles; a start that finds none within 64 pixels is a fixture to change."""
    for p in range(start, start + 64):
        c = pix2vec(nside, scheme, p)
        if axis_ok(c) and abs(float(c[2])) <= 0.9:
            return p
    raise SystemExit("no generic, non-polar pixel within 64 of %d at nside %d" % (start, nside))


def pixel_draw_rows():
    rows = []
    for nside, scheme, starts in PIXEL_GRIDS:
        for start in starts:
            ipix = generic_pixel(nside, scheme, start)
            kept = 0
            for seed, stream, draw in DRAW_COORDS:
                if kept == 2:
                    break
                try:
                    v, ncand = pixel_draw(nside, scheme, seed, stream, ipix, draw)
                except Unstable:
                    continue
                rows.append((nside, scheme, ipix, seed, stream, draw, ncand, v))
                kept += 1
            if kept < 2:
                raise SystemExit("pixel %d at nside %d: too few stable draw rows" % (ipix, nside))
    return rows


# The masks: a pixel list with a duplicate, and a short one; every entry has a generic centre.
MASK_SPECS = [
    (16, HP_NEST, [1100, 1300, 1300, 1000, 2047, 1500, 700]),
    (4, HP_RING, [40, 100, 140]),
]


def masks():
    out = []
    for nside, scheme, starts in MASK_SPECS:
        out.append((nside, scheme, [generic_pixel(nside, scheme, p) for p in starts]))
    return out


def mask_draw_rows():
    rows = []
    for mi, (nside, scheme, pixels) in enumerate(masks()):
        kept = 0
        for seed, stream, draw in DRAW_COORDS + [(s + 11, t + 1, d + 4) for s, t, d in DRAW_COORDS]:
            if kept == 6:
                break
            try:
                v, j = mask_draw(nside, scheme, seed, stream, pixels, draw)
            except Unstable:
                continue
            rows.append((mi, seed, stream, draw, j, v))
            kept += 1
        if kept < 6:
            raise SystemExit("mask %d: too few stable draw rows" % mi)
    return rows


# ---------------------------------------------------------------------------------------------
# Emission
# ---------------------------------------------------------------------------------------------

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_sphere_reference.py
! The contract model lives in that script; edit it there, not here. Never hand-edit a vector.
!"""

int_literal = rgv.int_literal
array = rgv.array
bits64 = rgv.bits64


def logical(b):
    return ".true." if b else ".false."


def gen_module():
    polys = polygons()
    cont = containment_rows(polys)
    offs = offset_rows()
    pas = pa_rows(offs)
    fibs = fibonacci_rows()
    pdraws = polygon_draw_rows(polys)
    pxdraws = pixel_draw_rows()
    mdraws = mask_draw_rows()

    L = [BANNER]
    L.append("!> Golden rows for `parquet_sphere`, derived from a 60-digit model of its contract.")
    L.append("!!")
    L.append("!! Reals are stored as `transfer` bit patterns of the correctly rounded double, so nothing is")
    L.append("!! lost to a decimal literal. Only the uniforms behind a draw, the candidate counts, the chosen")
    L.append("!! mask entries and the containment answers are exact contract; every other value is pinned to")
    L.append("!! a tolerance the suite states, because the library computes it with libm.")
    L.append("module test_sphere_vectors")
    L.append("    use iso_fortran_env, only: int32, int64")
    L.append("    implicit none")
    L.append("    public")
    L.append("")
    L.append("    ! ---- The contract constants, as src/parquet_sphere.f90 declares them ----")
    for name in ("polygon", "pixel", "mask_choice", "mask_point"):
        L.append("    integer(int64), parameter :: ssph_label_%s = %s !! The `%s` family's label."
                 % (name, int_literal(LABELS[name], "int64"), name))
    L.append('    character(len=*), parameter :: ssph_algorithm = "%s" !! `pf_sky_region_algorithm`.' % ALGORITHM)
    L.append("")

    # ---- Polygons ----
    offsets, flat_ra, flat_dec = [], [], []
    for poly in polys:
        offsets.append(len(flat_ra))
        flat_ra += poly.ra
        flat_dec += poly.dec
    L.append("    ! ---- Polygons: vertices flattened, one offset per polygon ----")
    for k, poly in enumerate(polys, start=1):
        L.append("    ! %2d  %-17s %s" % (k, "PF_EDGE_RADEC" if poly.rule == EDGE_RADEC else "PF_EDGE_GREAT_CIRCLE",
                                         poly.name))
    L.append("    integer, parameter :: n_spoly = %d" % len(polys))
    L.append("    integer, parameter :: n_spoly_vertex = %d" % len(flat_ra))
    L += array("integer", "spoly_rule", "n_spoly", [str(p.rule) for p in polys])
    L += array("integer", "spoly_first", "n_spoly", [str(o + 1) for o in offsets])
    L += array("integer", "spoly_count", "n_spoly", [str(len(p.ra)) for p in polys])
    L += array("integer(int64)", "spoly_ra_bits", "n_spoly_vertex", [bits64(x) for x in flat_ra])
    L += array("integer(int64)", "spoly_dec_bits", "n_spoly_vertex", [bits64(x) for x in flat_dec])
    L += array("integer(int64)", "spoly_area_bits", "n_spoly", [bits64(p.area) for p in polys])
    L += array("integer(int64)", "spoly_acceptance_bits", "n_spoly", [bits64(p.acceptance) for p in polys])
    L += array("logical", "spoly_simple", "n_spoly", [logical(p.simple) for p in polys])
    L += array("integer(int64)", "spoly_area_tol_bits", "n_spoly",
               [bits64(D(repr(EXACT_AREA_TOL if p.simple else AREA_TOL))) for p in polys])
    L.append("")

    L.append("    ! ---- Containment: probes kept at least 1e-6 (degrees in the chart, radians on the")
    L.append("    ! ---- sphere) from every edge; a great-circle row after an inside one is its antipode ----")
    L.append("    integer, parameter :: n_scont = %d" % len(cont))
    L += array("integer", "scont_poly", "n_scont", [str(r[0] + 1) for r in cont])
    L += array("integer(int64)", "scont_ra_bits", "n_scont", [bits64(r[1]) for r in cont])
    L += array("integer(int64)", "scont_dec_bits", "n_scont", [bits64(r[2]) for r in cont])
    L += array("logical", "scont_inside", "n_scont", [logical(r[3]) for r in cont])
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

    L.append("    ! ---- pf_fibonacci_grid_radec: point k of an n-point grid, with each row's RA tolerance ----")
    L.append("    integer, parameter :: n_sfib = %d" % len(fibs))
    L += array("integer", "sfib_n", "n_sfib", [str(r[0]) for r in fibs])
    L += array("integer", "sfib_k", "n_sfib", [str(r[1]) for r in fibs])
    L += array("integer(int64)", "sfib_ra_bits", "n_sfib", [bits64(r[2]) for r in fibs])
    L += array("integer(int64)", "sfib_dec_bits", "n_sfib", [bits64(r[3]) for r in fibs])
    L += array("integer(int64)", "sfib_tol_bits", "n_sfib", [bits64(r[4]) for r in fibs])
    L.append("")

    L.append("    ! ---- pf_sky_polygon%random_at: (polygon, seed, stream, draw) -> (ra, dec) and the")
    L.append("    ! ---- candidates it took ----")
    L.append("    integer, parameter :: n_spd = %d" % len(pdraws))
    L += array("integer", "spd_poly", "n_spd", [str(r[0] + 1) for r in pdraws])
    L += array("integer(int64)", "spd_seed", "n_spd", [int_literal(r[1], "int64") for r in pdraws])
    L += array("integer(int64)", "spd_stream", "n_spd", [int_literal(r[2], "int64") for r in pdraws])
    L += array("integer(int64)", "spd_draw", "n_spd", [int_literal(r[3], "int64") for r in pdraws])
    L += array("integer(int64)", "spd_ncand", "n_spd", [int_literal(r[4], "int64") for r in pdraws])
    L += array("integer(int64)", "spd_ra_bits", "n_spd", [bits64(r[5]) for r in pdraws])
    L += array("integer(int64)", "spd_dec_bits", "n_spd", [bits64(r[6]) for r in pdraws])
    L.append("")

    L.append("    ! ---- pf_random_pixel_at: (nside, scheme, ipix, seed, stream, draw) -> v and the")
    L.append("    ! ---- candidates it took; v flattened three per row ----")
    L.append("    integer, parameter :: n_spx = %d" % len(pxdraws))
    L += array("integer(int64)", "spx_nside", "n_spx", [int_literal(r[0], "int64") for r in pxdraws])
    L += array("integer", "spx_scheme", "n_spx", [str(r[1]) for r in pxdraws])
    L += array("integer(int64)", "spx_ipix", "n_spx", [int_literal(r[2], "int64") for r in pxdraws])
    L += array("integer(int64)", "spx_seed", "n_spx", [int_literal(r[3], "int64") for r in pxdraws])
    L += array("integer(int64)", "spx_stream", "n_spx", [int_literal(r[4], "int64") for r in pxdraws])
    L += array("integer(int64)", "spx_draw", "n_spx", [int_literal(r[5], "int64") for r in pxdraws])
    L += array("integer(int64)", "spx_ncand", "n_spx", [int_literal(r[6], "int64") for r in pxdraws])
    L += array("integer(int64)", "spx_v_bits", "3 * n_spx", [bits64(x) for r in pxdraws for x in r[7]])
    L.append("")

    mflat, mfirst = [], []
    mask_list = masks()
    for nside, scheme, pixels in mask_list:
        mfirst.append(len(mflat) + 1)
        mflat += pixels
    L.append("    ! ---- pf_random_mask_at: (mask, seed, stream, draw) -> the chosen entry and v ----")
    L.append("    integer, parameter :: n_smask = %d" % len(mask_list))
    L.append("    integer, parameter :: n_smask_pixel = %d" % len(mflat))
    L += array("integer(int64)", "smask_nside", "n_smask", [int_literal(m[0], "int64") for m in mask_list])
    L += array("integer", "smask_scheme", "n_smask", [str(m[1]) for m in mask_list])
    L += array("integer", "smask_first", "n_smask", [str(x) for x in mfirst])
    L += array("integer", "smask_count", "n_smask", [str(len(m[2])) for m in mask_list])
    L += array("integer(int32)", "smask_pixels", "n_smask_pixel", [int_literal(x, "int32") for x in mflat])
    L.append("    integer, parameter :: n_smd = %d" % len(mdraws))
    L += array("integer", "smd_mask", "n_smd", [str(r[0] + 1) for r in mdraws])
    L += array("integer(int64)", "smd_seed", "n_smd", [int_literal(r[1], "int64") for r in mdraws])
    L += array("integer(int64)", "smd_stream", "n_smd", [int_literal(r[2], "int64") for r in mdraws])
    L += array("integer(int64)", "smd_draw", "n_smd", [int_literal(r[3], "int64") for r in mdraws])
    L += array("integer(int64)", "smd_choice", "n_smd", [int_literal(r[4], "int64") for r in mdraws])
    L += array("integer(int64)", "smd_v_bits", "3 * n_smd", [bits64(x) for r in mdraws for x in r[5]])
    L.append("")
    L.append("    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.")
    L.append("end module test_sphere_vectors ! GCOVR_EXCL_LINE")
    return "\n".join(L) + "\n"


# ---------------------------------------------------------------------------------------------
# --self-test
# ---------------------------------------------------------------------------------------------

def self_test():
    bad = []

    def check(ok, what):
        if not ok:
            bad.append(what)

    def close(got, want, tol, what):
        if not abs(float(got) - float(want)) <= tol:
            bad.append("%s: got %r want %r" % (what, float(got), float(want)))

    # The constants, read back from the library's own source.
    src = SRC_PATH.read_text()
    for name, value in LABELS.items():
        m = re.search(r"sky_%s_label\s*=\s*(\d+)_int64" % name, src)
        check(m is not None and int(m.group(1)) == value, "label %s does not match src/parquet_sphere.f90" % name)
    m = re.search(r'pf_sky_region_algorithm\s*=\s*"([^"]*)"', src)
    check(m is not None and m.group(1) == ALGORITHM, "pf_sky_region_algorithm does not match the source")
    m = re.search(r"sky_candidate_cap\s*=\s*(\d+)_int64", src)
    check(m is not None and int(m.group(1)) == CANDIDATE_CAP, "the candidate cap does not match the source")
    stage1 = set(rgv.SPHERE_LABELS.values())
    check(len(set(LABELS.values())) == 4 and not (set(LABELS.values()) & stage1) and 0 not in LABELS.values(),
          "the region labels must differ from each other, from parquet_random's and from 0")

    # Closed-form areas.
    rect = ChartPolygon("rectangle", [10.0, 30.0, 30.0, 10.0], [-5.0, -5.0, 5.0, 5.0])
    close(rect.area, 20 * math.pi / 180 * 2 * math.sin(math.radians(5)), 1e-15, "rectangle area")
    cap = ChartPolygon("polar cap", [0.0, 360.0, 360.0, 0.0], [80.0, 80.0, 90.0, 90.0])
    close(cap.area, 2 * math.pi * (1 - math.sin(math.radians(80))), 1e-15, "polar cap area")
    octant = GreatCirclePolygon("octant", [0.0, 90.0, 0.0], [0.0, 0.0, 90.0])
    close(octant.area, math.pi / 2, 1e-15, "octant area")
    lune_free = GreatCirclePolygon("half octant", [0.0, 45.0, 0.0], [0.0, 0.0, 90.0])
    close(lune_free.area, math.pi / 4, 1e-15, "a quarter-lune triangle's area")
    # A slanted chart edge's integral against brute quadrature.
    with dctx():
        poly = ChartPolygon("slanted", [100.0, 140.0, 150.0, 95.0], [-40.0, -30.0, 10.0, -5.0])
        steps = 20000
        total = 0.0
        for k in range(steps):
            # The area as the integral over dec of the chart width times cos(dec), by the midpoint rule.
            y = -40.0 + (k + 0.5) * 50.0 / steps
            xs = []
            for a in range(4):
                b = (a + 1) % 4
                y1, y2 = poly.dec[a], poly.dec[b]
                if (y1 > y) != (y2 > y):
                    xs.append(poly.ra[a] + (y - y1) * (poly.ra[b] - poly.ra[a]) / (y2 - y1))
            xs.sort()
            width = sum(xs[i + 1] - xs[i] for i in range(0, len(xs), 2))
            total += math.radians(width) * math.cos(math.radians(y)) * math.radians(50.0 / steps)
        close(poly.area, total, 1e-8, "slanted quadrilateral area by quadrature")

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

    # The Fibonacci grid: the single point, and the half step in the longitude.
    ra, dec = fibonacci_point(1, 0)
    close(ra, 0.5 * 360 / ((1 + math.sqrt(5)) / 2), 1e-12, "the one-point grid's ra")
    close(dec, 0.0, 1e-15, "the one-point grid's dec")

    # The two great-circle containment models agree, and the antipode of an inside point is out.
    rng = random.Random(7)
    for spec in GC_POLYGONS:
        poly = GreatCirclePolygon(*spec)
        for _ in range(4000):
            z = 2 * rng.random() - 1
            az = 2 * math.pi * rng.random()
            s = math.sqrt(1 - z * z)
            p = [s * math.cos(az), s * math.sin(az), z]
            if poly.margin(p) < 1e-9:
                continue
            w = poly.winding_inside(p)
            check(w == poly.gnomonic_inside(p), "%s: the two containment models disagree" % poly.name)
            if w:
                check(not poly.gnomonic_inside([-x for x in p]), "%s: an antipode reads inside" % poly.name)

    # The decimal HEALPix model against the float one, to the float model's own accuracy: its
    # `1 - z` and `sqrt((1 - z)*(1 + z))` cancel near a pole, by about 1e-13 of max_pixrad at 1024.
    for nside in (1, 2, 8, 64, 1024):
        close(max_pixrad(nside), hpr.max_pixrad(nside), 1e-12 * hpr.max_pixrad(nside), "max_pixrad %d" % nside)
        for p in sorted({0, 1, 12 * nside * nside // 2, 12 * nside * nside - 1, 4 * nside * nside // 3}):
            want = hpr.pix2vec_ring(nside, p)
            got = pix2vec(nside, HP_RING, p)
            check(all(abs(float(got[j]) - want[j]) <= 1e-13 for j in range(3)), "pix2vec ring %d/%d" % (nside, p))
            check(vec2pix(nside, HP_RING, got) == p, "vec2pix of the centre of ring pixel %d/%d" % (nside, p))
            q = hpr.ring2nest(nside, p)
            check(vec2pix(nside, HP_NEST, pix2vec(nside, HP_NEST, q)) == q, "nest centre %d/%d" % (nside, q))
            # `%pix2vec_offset` at the half-integer IS the pixel centre: the one anchor that pins
            # the continuous inverse projection against the integer path it generalises.
            mid = pix_point(nside, HP_RING, p, 0.5, 0.5)
            check(all(abs(mid[j] - got[j]) <= D("1e-30") for j in range(3)),
                  "pix2vec_offset(.5,.5) is not the centre, ring %d/%d" % (nside, p))
            check(all(abs(pix_point(nside, HP_NEST, q, 0.5, 0.5)[j] - pix2vec(nside, HP_NEST, q)[j]) <= D("1e-30")
                      for j in range(3)), "pix2vec_offset(.5,.5) is not the centre, nest %d/%d" % (nside, q))
            # Every corner of the square stays in the pixel, which is what makes the offset form a
            # sampler rather than an approximation.
            for ax in (0.02, 0.5, 0.98):
                for ay in (0.02, 0.5, 0.98):
                    check(vec2pix(nside, HP_RING, pix_point(nside, HP_RING, p, ax, ay)) == p,
                          "pix2vec_offset(%g,%g) left ring pixel %d/%d" % (ax, ay, nside, p))
    rng = random.Random(11)
    for _ in range(3000):
        z = 2 * rng.random() - 1
        az = 2 * math.pi * rng.random()
        s = math.sqrt(1 - z * z)
        v = [s * math.cos(az), s * math.sin(az), z]
        theta, phi = hpr.vec2ang(v)
        dv = [D(x) for x in v]
        for nside in (1, 4, 64):
            check(vec2pix(nside, HP_RING, dv) == hpr.ang2pix_ring(nside, theta, phi), "vec2pix ring vs float model")
            check(vec2pix(nside, HP_NEST, dv) == hpr.ang2pix_nest(nside, theta, phi), "vec2pix nest vs float model")
    return bad


# ---------------------------------------------------------------------------------------------
# --verify-oracle
# ---------------------------------------------------------------------------------------------

def verify_oracle():
    try:
        import numpy as np
        import healpy as hp
        import astropy
        import astropy.units as u
        from astropy.coordinates import SkyCoord, golden_spiral_grid
        import matplotlib
        from matplotlib.path import Path
    except ImportError as exc:
        sys.stderr.write("generate_sphere_reference --verify-oracle needs numpy, healpy, astropy and "
                         "matplotlib: %s\n" % exc)
        return 2
    bad = []
    counts = {"offsets": 0, "position angles": 0, "grid points": 0, "chart probes": 0,
              "great-circle polygons": 0, "pixel draws": 0}

    def check(ok, what):
        if not ok:
            bad.append(what)

    polys = polygons()

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
    for ra1, dec1, ra2, dec2, pa, tol in pa_rows(offset_rows()):
        if pa == 0 and dec1 == dec2 and abs(dec1) == 90.0:
            continue                               # the pole's coincidence rule is the library's own
        got = SkyCoord(ra1 * u.deg, dec1 * u.deg).position_angle(SkyCoord(ra2 * u.deg, dec2 * u.deg)).deg
        diff = (got - float(pa) + 180.0) % 360.0 - 180.0
        counts["position angles"] += 1
        check(abs(diff) < max(1e-9, 1e3 * tol), "position angle (%g, %g, %g, %g): astropy %r, model %r"
              % (ra1, dec1, ra2, dec2, got, float(pa)))
    for n, ks in FIB_CASES:
        g = golden_spiral_grid(n)
        for k in ks:
            ra, dec = fibonacci_point(n, k)
            counts["grid points"] += 1
            check(abs(g.lon.deg[k] - float(ra)) < 1e-9 and abs(g.lat.deg[k] - float(dec)) < 1e-9,
                  "fibonacci n=%d k=%d" % (n, k))

    # Chart containment against matplotlib's even-odd path test.
    for pi, ra, dec, inside in containment_rows(polys):
        poly = polys[pi]
        if poly.rule != EDGE_RADEC:
            continue
        path = Path(np.column_stack([poly.ra, poly.dec]), closed=False)
        x = float(poly.wrap(ra))
        # matplotlib's winding rule differs from even-odd only on a self-intersecting polygon.
        if poly.name != "bowtie":
            counts["chart probes"] += 1
            check(bool(path.contains_point((x, dec))) == inside, "%s: matplotlib disagrees at (%g, %g)" % (poly.name, ra, dec))

    # Great-circle containment by pixel centre against healpy.query_polygon, for the convex fixtures.
    nside = 256
    npix = 12 * nside * nside
    cx, cy, cz = hp.pix2vec(nside, np.arange(npix))
    for poly in polys:
        if poly.rule != EDGE_GREAT_CIRCLE or poly.name in ("L-shape",):
            continue
        vecs = np.array(poly.fu)
        try:
            want = set(hp.query_polygon(nside, vecs, inclusive=False).tolist())
        except Exception:
            continue                                # healpy refuses a non-convex polygon
        got = set()
        for p in range(npix):
            q = [cx[p], cy[p], cz[p]]
            if poly.winding_inside(q):
                got.add(p)
        diff = want ^ got
        counts["great-circle polygons"] += 1
        near = [p for p in diff if poly.margin([cx[p], cy[p], cz[p]]) < 1e-9]
        check(len(diff) == len(near), "%s: %d pixel centres disagree with healpy" % (poly.name, len(diff) - len(near)))
        area_hp = len(want) * 4 * math.pi / npix
        check(abs(area_hp - float(poly.area)) < 0.02 * float(poly.area) + 20 * 4 * math.pi / npix,
              "%s: area %r against healpy's pixel count %r" % (poly.name, float(poly.area), area_hp))

    # Every pixel draw names its pixel under healpy too.
    for nside_, scheme, ipix, seed, stream, draw, ncand, v in pixel_draw_rows():
        got = hp.vec2pix(nside_, float(v[0]), float(v[1]), float(v[2]), nest=(scheme == HP_NEST))
        counts["pixel draws"] += 1
        check(int(got) == ipix, "pixel draw nside=%d ipix=%d lands in %d under healpy" % (nside_, ipix, int(got)))
        check(abs(float(max_pixrad(nside_)) - hp.max_pixrad(nside_)) < 1e-9 * hp.max_pixrad(nside_),
              "max_pixrad %d against healpy" % nside_)

    if bad:
        sys.stderr.write("generate_sphere_reference --verify-oracle FAILED (%d):\n" % len(bad))
        for line in bad[:40]:
            sys.stderr.write("  %s\n" % line)
        return 1
    print("generate_sphere_reference --verify-oracle: astropy %s, healpy %s, matplotlib %s agree (%s)"
          % (astropy.__version__, hp.__version__, matplotlib.__version__,
             ", ".join("%d %s" % (v, k) for k, v in counts.items())))
    return 0


def main():
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="compare the committed file with a fresh generation; exit 1 on drift")
    ap.add_argument("--self-test", action="store_true",
                    help="re-derive the published anchors; exit 1 if any of them fails")
    ap.add_argument("--verify-oracle", action="store_true",
                    help="cross-check the rows against astropy, matplotlib and healpy")
    args = ap.parse_args()

    if args.self_test:
        bad = self_test()
        if bad:
            print("generate_sphere_reference.py --self-test FAILED (%d):" % len(bad), file=sys.stderr)
            for line in bad[:40]:
                print("  " + line, file=sys.stderr)
            return 1
        print("generate_sphere_reference.py --self-test: every anchor reproduced")
        return 0
    if args.verify_oracle:
        return verify_oracle()

    text = gen_module()
    over = [n for n, line in enumerate(text.split("\n"), start=1) if len(line) > 132]
    if over:
        print("generate_sphere_reference.py: emitted line(s) exceed 132 columns: %s" % over[:10], file=sys.stderr)
        return 1
    if args.check:
        if not OUT_PATH.exists() or OUT_PATH.read_text() != text:
            print("generate_sphere_reference.py --check: %s differs from a fresh generation."
                  % OUT_PATH.relative_to(REPO_ROOT), file=sys.stderr)
            print("Re-run tools/generate_sphere_reference.py to regenerate.", file=sys.stderr)
            return 1
        print("generate_sphere_reference.py --check: %s is current." % OUT_PATH.relative_to(REPO_ROOT))
        return 0
    OUT_PATH.write_text(text)
    print("generate_sphere_reference.py: wrote %s (%d lines)." % (OUT_PATH.relative_to(REPO_ROOT), text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main())
