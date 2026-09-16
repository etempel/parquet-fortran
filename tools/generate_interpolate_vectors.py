#!/usr/bin/env python3
"""Golden expectations for `parquet_interpolate`, from an exact-rational model that shares no arithmetic.

The committed expectations come from the interpolants' definitions transcribed over
`fractions.Fraction`: every spline system is solved and every polynomial evaluated, differentiated
and integrated EXACTLY, over the fixtures' exact rational values, and each expectation is emitted as
the round-to-nearest double of that exact number. The library's double-precision arithmetic agrees
with it to a few ulp, because the spline systems are strictly diagonally dominant and so well
conditioned; the suite asserts 1e-13 relative. scipy appears only in `--self-test`, as the proof
that the model transcribes the same definitions `scipy.interpolate` implements -- so no emitted
digit depends on a library version, and `--check` needs nothing but the interpreter.

**The model reaches every number by a different road from the library's**, so that a slip in one of
the library's closed forms cannot be copied into the oracle:

* the second derivatives of a cubic spline come from Gauss-Jordan elimination of the FULL system,
  its end rows as they are defined -- the not-a-knot rows included, which reach one knot beyond the
  tridiagonal band and which the library eliminates by hand before a Thomas sweep;
* the PCHIP slopes use the weighted harmonic mean as scipy writes it, where the library uses
  Brodlie's overflow-free form of the same number;
* every segment is converted to a polynomial in `s = x - x(k)` by exact polynomial arithmetic, and
  its value, derivatives and integral are read off that polynomial, where the library evaluates
  closed forms in `t = s/h`;
* an integral is summed over the pieces between the limits and the knots, each piece on the
  polynomial its midpoint selects, where the library brackets both limits and sums whole segments.

**The fixtures are dyadic rationals**: every knot, ordinate, slope, query and limit is an integer
over a power of two, so each is an exact double and Python and Fortran hold the same table bit for
bit. They are emitted as literals rather than mirrored as recipes on the Fortran side: the largest
has 33 points, which is shorter as a literal than as the code that would rebuild it, and a literal
cannot drift from the generator that produced it.

What each fixture pins:

* `U9`, nine knots a quarter apart: the evenly spaced table, whose bracket the library finds by
  arithmetic rather than by bisection;
* `G16`, sixteen knots whose spacing doubles every second knot: a spread of 128 between the
  narrowest and the widest segment, where a boundary row or a spacing taken from the wrong side
  moves every value;
* `R33`, thirty-three knots at irregular gaps: the general case, ordinates of both signs, and a
  PCHIP end slope the shape rule caps at three times the end secant;
* `M12`, twelve knots of non-decreasing data with a flat run of three: the monotone table PCHIP is
  for, whose first end slope the shape rule sets to zero;
* `K4`, four knots: the smallest table `bc="not_a_knot"` accepts, where that spline is the one
  cubic through all four points;
* `P6`, `x**2` at `x = 1..6`: the example the guide page quotes, a natural spline that does NOT
  reproduce the parabola, and its extrapolation to `x = 99`.

Each of the first five is evaluated under every variant -- `linear`, `cubic` under each of the three
end conditions, and `pchip` -- at 25 interior queries (some of which land on knots) and at 4 outside
the table under `outside="extrapolate"`, for the value and both derivatives; and integrated over 6
pairs of limits inside the table and 3 reaching beyond it, the latter under both
`outside="extrapolate"` and `outside="clamp"`.

Usage:
    python3 tools/generate_interpolate_vectors.py              # rewrite test/test_interpolate_golden.f90
    python3 tools/generate_interpolate_vectors.py --self-test  # validate the model against scipy
    python3 tools/generate_interpolate_vectors.py --check      # assert the committed file is current

`--check` runs in CI's lint stage and in `tools/run_lint_check.sh`. `--self-test` needs numpy and
scipy and is a step to run when the MODEL changes, not on every push.

Maintainer-only: it is never run at build time.
"""

import sys
from fractions import Fraction
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
OUT_PATH = REPO_ROOT / "test" / "test_interpolate_golden.f90"

#: Interior queries per fixture, outside queries (two below the table, two above), integral limit
#: pairs inside the table, and integral limit pairs reaching beyond it.
NQ, NO, NI, NJ = 25, 4, 6, 3

#: The variants every fixture is evaluated under, in the order of the expectation arrays' columns:
#: (Fortran index name, method token, end-condition token).
VARIANTS = [
    ("GI_LINEAR", "linear", None),
    ("GI_NATURAL", "cubic", "natural"),
    ("GI_NOT_A_KNOT", "cubic", "not_a_knot"),
    ("GI_CLAMPED", "cubic", "clamped"),
    ("GI_PCHIP", "pchip", None),
]


# ======================================================================================
# The fixtures: dyadic rationals, so every value is an exact double in both languages
# ======================================================================================

def fixture_u9():
    """Nine knots a quarter apart on [0, 2]; ordinates an integer recipe over 16."""
    x = [Fraction(i, 4) for i in range(9)]
    y = [Fraction((i * i * 37 + 11) % 23 - 11, 16) for i in range(9)]
    return x, y, (Fraction(3, 8), Fraction(-5, 4))


def fixture_g16():
    """Sixteen knots whose spacing doubles every second knot, from 1/16 to 8; ordinates over 64."""
    x = [Fraction(0)]
    for j in range(15):
        x.append(x[-1] + Fraction(2 ** (j // 2), 16))
    y = [Fraction((i * i * 53 + 7 * i + 3) % 41 - 20, 64) for i in range(16)]
    return x, y, (Fraction(-1, 2), Fraction(1, 16))


def fixture_r33():
    """Thirty-three knots from -2 at gaps of 1/16 to 11/16; ordinates over 256, both signs."""
    x = [Fraction(-2)]
    for j in range(32):
        x.append(x[-1] + Fraction(1 + (j * j * 13 + 7 * j + 5) % 11, 16))
    y = [Fraction((i * i * i * 7 + 29 * i + 13) % 97 - 48, 256) for i in range(33)]
    return x, y, (Fraction(1, 4), Fraction(3, 4))


def fixture_m12():
    """Twelve knots at gaps of 1/8 to 3/4; non-decreasing ordinates with a flat run of three and a
    first secant far shallower than the second."""
    x = [Fraction(0)]
    for j in range(11):
        x.append(x[-1] + Fraction(1 + (j * 5 + 3) % 6, 8))
    y = [Fraction(v, 32) for v in (0, 1, 24, 24, 24, 28, 32, 34, 40, 48, 52, 64)]
    return x, y, (Fraction(0), Fraction(1, 2))


def fixture_k4():
    """Four knots at gaps of 1/2, 3/4 and 5/4; ordinates of both signs."""
    x = [Fraction(-1, 2), Fraction(0), Fraction(3, 4), Fraction(2)]
    y = [Fraction(5, 8), Fraction(-1, 4), Fraction(1, 2), Fraction(-3, 16)]
    return x, y, (Fraction(-3, 2), Fraction(1))


#: The general fixtures, by the name their Fortran parameters carry.
FIXTURES = [("U9", fixture_u9), ("G16", fixture_g16), ("R33", fixture_r33), ("M12", fixture_m12),
            ("K4", fixture_k4)]


def interior_queries(x):
    """25 dyadic points spread over the table, at `x0 + span*c/64` for rising odd-and-even `c`."""
    span = x[-1] - x[0]
    return [x[0] + span * Fraction(1 + (j * 62) // 24, 64) for j in range(NQ)]


def outside_queries(x):
    """Two points below the table and two above, at an eighth and a sixty-fourth of its span."""
    span = x[-1] - x[0]
    return [x[0] - span / 8, x[0] - span / 64, x[-1] + span / 64, x[-1] + span / 8]


def inside_limits(x):
    """Six pairs of integration limits inside the table: the whole table; the middle half of one
    segment; knot to knot; mid-segment to mid-segment across several; the same reversed; and a knot
    to mid-segment."""
    span, n = x[-1] - x[0], len(x)
    k = segment(x, x[0] + span * Fraction(9, 64))
    h = x[k + 1] - x[k]
    return [(x[0], x[-1]),
            (x[k] + h / 4, x[k] + 3 * h / 4),
            (x[2], x[n - 3]),
            (x[0] + span * Fraction(3, 64), x[0] + span * Fraction(45, 64)),
            (x[0] + span * Fraction(50, 64), x[0] + span * Fraction(7, 64)),
            (x[1], x[0] + span * Fraction(33, 64))]


def outside_limits(x):
    """Three pairs of integration limits reaching beyond the table: from below into it, from inside
    it to above, and from just below to just above."""
    span = x[-1] - x[0]
    o = outside_queries(x)
    return [(o[0], x[0] + span * Fraction(21, 64)),
            (x[0] + span * Fraction(40, 64), o[3]),
            (o[1], o[2])]


#: The guide page's example: `x**2` tabulated at `x = 1..6`, the three probes it prints, and the
#: extrapolation it warns about.
PARABOLA_X = [Fraction(i) for i in range(1, 7)]
PARABOLA_Y = [xi * xi for xi in PARABOLA_X]
PARABOLA_PROBES = [Fraction(3, 2), Fraction(5, 2), Fraction(11, 2)]
PARABOLA_FAR = Fraction(99)


# ======================================================================================
# The model: the definitions, exactly
# ======================================================================================

def solve(matrix, rhs):
    """Solve a square system exactly by Gauss-Jordan elimination with row exchanges."""
    n = len(rhs)
    rows = [list(matrix[i]) + [rhs[i]] for i in range(n)]
    for col in range(n):
        pivot = next(r for r in range(col, n) if rows[r][col] != 0)
        rows[col], rows[pivot] = rows[pivot], rows[col]
        for r in range(n):
            if r != col and rows[r][col] != 0:
                factor = rows[r][col] / rows[col][col]
                rows[r] = [a - factor * b for a, b in zip(rows[r], rows[col])]
    return [rows[i][n] / rows[i][i] for i in range(n)]


def second_derivatives(x, y, bc, slopes):
    """The second derivatives at the knots of the cubic spline through `(x, y)` under `bc`, from the
    full system: the interior continuity rows, and the end rows as each condition defines them."""
    n = len(x)
    h = [x[i + 1] - x[i] for i in range(n - 1)]
    secant = [(y[i + 1] - y[i]) / h[i] for i in range(n - 1)]
    a = [[Fraction(0)] * n for _ in range(n)]
    r = [Fraction(0)] * n
    for i in range(1, n - 1):
        a[i][i - 1], a[i][i], a[i][i + 1] = h[i - 1], 2 * (h[i - 1] + h[i]), h[i]
        r[i] = 6 * (secant[i] - secant[i - 1])
    if bc == "natural":
        # The curvature vanishes at both ends.
        a[0][0] = a[n - 1][n - 1] = Fraction(1)
    elif bc == "clamped":
        # The first derivative at each end is the given slope.
        a[0][0], a[0][1], r[0] = 2 * h[0], h[0], 6 * (secant[0] - slopes[0])
        a[n - 1][n - 2], a[n - 1][n - 1], r[n - 1] = h[n - 2], 2 * h[n - 2], 6 * (slopes[1] - secant[n - 2])
    else:
        # Not-a-knot: the third derivative, (m(i+1) - m(i))/h(i) on segment i, is continuous across
        # the second knot and across the second-to-last.
        a[0][0], a[0][1], a[0][2] = h[1], -(h[0] + h[1]), h[0]
        a[n - 1][n - 3], a[n - 1][n - 2], a[n - 1][n - 1] = h[n - 2], -(h[n - 3] + h[n - 2]), h[n - 3]
    return solve(a, r)


def sign(v):
    """-1, 0 or +1: numpy's `sign`, which is what scipy's shape rules compare."""
    return (v > 0) - (v < 0)


def pchip_slopes(x, y, branches=None):
    """The PCHIP slopes at the knots, as scipy's `PchipInterpolator._find_derivatives` computes them.

    `branches`, when given, collects the name of every rule that decided a slope, so that
    `--self-test` can require the fixtures to reach each one."""
    n = len(x)
    h = [x[i + 1] - x[i] for i in range(n - 1)]
    secant = [(y[i + 1] - y[i]) / h[i] for i in range(n - 1)]
    note = branches.add if branches is not None else (lambda name: None)
    if n == 2:
        return [secant[0], secant[0]]
    d = [Fraction(0)] * n
    for k in range(1, n - 1):
        left, right = secant[k - 1], secant[k]
        if left == 0 or right == 0:
            note("interior flat")
        elif sign(left) != sign(right):
            note("interior opposite")
        else:
            w1, w2 = 2 * h[k] + h[k - 1], h[k] + 2 * h[k - 1]
            d[k] = (w1 + w2) / (w1 / left + w2 / right)
            note("interior mean")

    def end(h0, h1, m0, m1):
        slope = ((2 * h0 + h1) * m0 - h0 * m1) / (h0 + h1)
        if sign(slope) != sign(m0):
            note("end zeroed")
            return Fraction(0)
        if sign(m0) != sign(m1) and abs(slope) > 3 * abs(m0):
            note("end capped")
            return 3 * m0
        note("end formula")
        return slope

    d[0] = end(h[0], h[1], secant[0], secant[1])
    d[n - 1] = end(h[n - 2], h[n - 3], secant[n - 2], secant[n - 3])
    return d


def segment(x, q):
    """The segment `k` (0-based) the library evaluates `q` on: `x[k] <= q < x[k+1]` for a point
    inside the table, the last segment at `x[-1]`, and the end segment for a point outside it --
    which is how `"extrapolate"` continues the end segment's polynomial."""
    k = 0
    while k < len(x) - 2 and q >= x[k + 1]:
        k += 1
    return k


# Polynomials in `s`, as coefficient lists from the constant term up.

def poly_add(p, q):
    return [(p[i] if i < len(p) else 0) + (q[i] if i < len(q) else 0) for i in range(max(len(p), len(q)))]


def poly_mul(p, q):
    out = [Fraction(0)] * (len(p) + len(q) - 1)
    for i, a in enumerate(p):
        for j, b in enumerate(q):
            out[i + j] += a * b
    return out


def poly_scale(p, c):
    return [c * a for a in p]


def poly_at(p, s):
    return sum(c * s ** i for i, c in enumerate(p))


def poly_derivative(p):
    return [i * p[i] for i in range(1, len(p))] or [Fraction(0)]


def poly_antiderivative(p):
    return [Fraction(0)] + [p[i] / (i + 1) for i in range(len(p))]


class Interpolant:
    """One variant built over one table: the per-knot coefficients and each segment's polynomial."""

    def __init__(self, x, y, method, bc=None, slopes=None):
        self.x, self.y, self.method = x, y, method
        if method == "cubic":
            self.coef = second_derivatives(x, y, bc, slopes)
        elif method == "pchip":
            self.coef = pchip_slopes(x, y)
        else:
            self.coef = None

    def polynomial(self, k):
        """Segment `k` as a polynomial in `s = x - x[k]`, from the definition of its method."""
        x, y = self.x, self.y
        h = x[k + 1] - x[k]
        b = [Fraction(0), 1 / h]          # (x - x[k])/h
        a = [Fraction(1), -1 / h]         # (x[k+1] - x)/h
        if self.method == "linear":
            return poly_add(poly_scale(a, y[k]), poly_scale(b, y[k + 1]))
        if self.method == "cubic":
            m0, m1 = self.coef[k], self.coef[k + 1]
            a3 = poly_add(poly_mul(poly_mul(a, a), a), poly_scale(a, -1))
            b3 = poly_add(poly_mul(poly_mul(b, b), b), poly_scale(b, -1))
            curvature = poly_add(poly_scale(a3, m0), poly_scale(b3, m1))
            return poly_add(poly_add(poly_scale(a, y[k]), poly_scale(b, y[k + 1])),
                            poly_scale(curvature, h * h / 6))
        d0, d1 = self.coef[k], self.coef[k + 1]
        t2 = poly_mul(b, b)
        t3 = poly_mul(t2, b)
        h00 = poly_add(poly_add(poly_scale(t3, 2), poly_scale(t2, -3)), [Fraction(1)])
        h10 = poly_add(poly_add(t3, poly_scale(t2, -2)), b)
        h01 = poly_add(poly_scale(t3, -2), poly_scale(t2, 3))
        h11 = poly_add(t3, poly_scale(t2, -1))
        return poly_add(poly_add(poly_scale(h00, y[k]), poly_scale(h10, h * d0)),
                        poly_add(poly_scale(h01, y[k + 1]), poly_scale(h11, h * d1)))

    def at(self, q, order=0):
        """The value (`order` 0) or a derivative at `q`, the end segment continued outside."""
        k = segment(self.x, q)
        p = self.polynomial(k)
        for _ in range(order):
            p = poly_derivative(p)
        return poly_at(p, q - self.x[k])

    def integral(self, lo, hi, outside):
        """The integral from `lo` to `hi` under `outside` ("extrapolate" or "clamp"), summed over the
        pieces between the limits and the knots."""
        if lo == hi:
            return Fraction(0)
        if hi < lo:
            return -self.integral(hi, lo, outside)
        x, y = self.x, self.y
        cuts = sorted({lo, hi} | {v for v in x if lo < v < hi})
        total = Fraction(0)
        for p, q in zip(cuts[:-1], cuts[1:]):
            mid = (p + q) / 2
            if outside == "clamp" and mid < x[0]:
                total += (q - p) * y[0]
            elif outside == "clamp" and mid > x[-1]:
                total += (q - p) * y[-1]
            else:
                k = segment(x, mid)
                antiderivative = poly_antiderivative(self.polynomial(k))
                total += poly_at(antiderivative, q - x[k]) - poly_at(antiderivative, p - x[k])
        return total


def build(x, y, slopes, variant):
    """The interpolant of one variant over one fixture."""
    _, method, bc = variant
    return Interpolant(x, y, method, bc, slopes)


def cases(x, y, slopes):
    """Every expectation one fixture carries, keyed by the suffix its Fortran parameter takes; each
    is a list with one column per variant."""
    inside, outside = interior_queries(x), outside_queries(x)
    pairs_in, pairs_out = inside_limits(x), outside_limits(x)
    table = {key: [] for key in ("VALUE", "VALUE_OUT", "D1", "D2", "D1_OUT", "D2_OUT", "INTEGRAL",
                                 "INTEGRAL_OUT", "INTEGRAL_CLAMP")}
    for variant in VARIANTS:
        f = build(x, y, slopes, variant)
        table["VALUE"].append([f.at(q) for q in inside])
        table["VALUE_OUT"].append([f.at(q) for q in outside])
        table["D1"].append([f.at(q, 1) for q in inside])
        table["D2"].append([f.at(q, 2) for q in inside])
        table["D1_OUT"].append([f.at(q, 1) for q in outside])
        table["D2_OUT"].append([f.at(q, 2) for q in outside])
        table["INTEGRAL"].append([f.integral(a, b, "extrapolate") for a, b in pairs_in])
        table["INTEGRAL_OUT"].append([f.integral(a, b, "extrapolate") for a, b in pairs_out])
        table["INTEGRAL_CLAMP"].append([f.integral(a, b, "clamp") for a, b in pairs_out])
    return table


# ======================================================================================
# --self-test: the model against scipy.interpolate, and against its own definitions
# ======================================================================================

def self_test():
    """Validate the exact model against scipy, and the model against properties it must have."""
    try:
        import numpy as np
        from scipy.interpolate import CubicSpline, PchipInterpolator, make_interp_spline
    except ImportError as exc:                                   # pragma: no cover - maintainer tool
        print("generate_interpolate_vectors.py --self-test: needs numpy and scipy (%s)" % exc,
              file=sys.stderr)
        return 2
    import scipy

    failures = []

    def agree(what, model, oracle, scale):
        for i, (a, b) in enumerate(zip(model, oracle)):
            a, b = float(a), float(b)
            if abs(a - b) > 1.0e-12 * max(abs(a), abs(b), scale):
                failures.append("%s[%d]: model %.17g, scipy %.17g" % (what, i, a, b))

    def scipy_object(variant, xf, yf, slopes):
        _, method, bc = variant
        if method == "linear":
            return make_interp_spline(xf, yf, k=1)
        if method == "pchip":
            return PchipInterpolator(xf, yf, extrapolate=True)
        if bc == "clamped":
            return CubicSpline(xf, yf, bc_type=((1, float(slopes[0])), (1, float(slopes[1]))),
                               extrapolate=True)
        return CubicSpline(xf, yf, bc_type=bc.replace("_", "-"), extrapolate=True)

    def scipy_clamped_integral(obj, xf, yf, a, b):
        """The integral under "clamp" from scipy's integral inside the table and the flat ends."""
        if b < a:
            return -scipy_clamped_integral(obj, xf, yf, b, a)
        below = (min(b, xf[0]) - a) * yf[0] if a < xf[0] else 0.0
        above = (b - max(a, xf[-1])) * yf[-1] if b > xf[-1] else 0.0
        lo, hi = max(a, xf[0]), min(b, xf[-1])
        inner = float(obj.integrate(lo, hi)) if lo < hi else 0.0
        return below + inner + above

    branches = set()
    for name, maker in FIXTURES:
        x, y, slopes = maker()
        pchip_slopes(x, y, branches)
        xf, yf = np.array([float(v) for v in x]), np.array([float(v) for v in y])
        scale = float(np.max(np.abs(yf)))
        span_scale = float((x[-1] - x[0])) * scale
        inside, outside = interior_queries(x), outside_queries(x)
        qin, qout = np.array([float(v) for v in inside]), np.array([float(v) for v in outside])
        pairs_in, pairs_out = inside_limits(x), outside_limits(x)
        table = cases(x, y, slopes)
        for column, variant in enumerate(VARIANTS):
            label = "%s %s" % (name, variant[0])
            obj = scipy_object(variant, xf, yf, slopes)
            agree(label + " value", table["VALUE"][column], obj(qin), scale)
            agree(label + " value outside", table["VALUE_OUT"][column], obj(qout), scale)
            for key, points, nu in (("D1", qin, 1), ("D2", qin, 2), ("D1_OUT", qout, 1),
                                    ("D2_OUT", qout, 2)):
                model = table[key][column]
                agree(label + " " + key, model, obj(points, nu), float(max(abs(v) for v in model)))
            agree(label + " integral", table["INTEGRAL"][column],
                  [obj.integrate(float(a), float(b), extrapolate=True) for a, b in pairs_in], span_scale)
            agree(label + " integral outside", table["INTEGRAL_OUT"][column],
                  [obj.integrate(float(a), float(b), extrapolate=True) for a, b in pairs_out], span_scale)
            agree(label + " integral clamped", table["INTEGRAL_CLAMP"][column],
                  [scipy_clamped_integral(obj, xf, yf, float(a), float(b)) for a, b in pairs_out],
                  span_scale)

            # The model's own properties, independently of scipy.
            f = build(x, y, slopes, variant)
            if any(f.at(x[k]) != y[k] for k in range(len(x))):
                failures.append("%s: the model does not reproduce every knot exactly" % label)
            for a, b in pairs_in + pairs_out:
                # Simpson's rule is exact for a cubic, so on every piece between the knots it is a
                # second, independent integral of the extrapolated interpolant.
                lo, hi = min(a, b), max(a, b)
                cuts = sorted({lo, hi} | {v for v in x if lo < v < hi})
                simpson = Fraction(0)
                for p, q in zip(cuts[:-1], cuts[1:]):
                    mid, k = (p + q) / 2, segment(x, (p + q) / 2)
                    poly = f.polynomial(k)
                    simpson += (q - p) / 6 * (poly_at(poly, p - x[k]) + 4 * poly_at(poly, mid - x[k])
                                              + poly_at(poly, q - x[k]))
                if (simpson if a <= b else -simpson) != f.integral(a, b, "extrapolate"):
                    failures.append("%s: Simpson's rule disagrees with the integral over %s" % (label, (a, b)))
            if variant[2] == "natural" and (f.coef[0] != 0 or f.coef[-1] != 0):
                failures.append("%s: the natural end rows do not hold" % label)
            if variant[2] == "clamped" and (f.at(x[0], 1) != slopes[0] or f.at(x[-1], 1) != slopes[1]):
                failures.append("%s: the clamped end slopes do not hold" % label)
            if variant[2] == "not_a_knot":
                third = [f.polynomial(k)[3] if len(f.polynomial(k)) > 3 else 0 for k in range(len(x) - 1)]
                if third[0] != third[1] or third[-1] != third[-2]:
                    failures.append("%s: the not-a-knot rows do not hold" % label)

    # A straight line is reproduced by every variant (the clamped one given its slope), a cubic by
    # not-a-knot and by clamped given the cubic's own end slopes, and PCHIP keeps monotone data
    # monotone on every segment.
    x, _, _ = fixture_r33()
    line = [3 * v - Fraction(5, 8) for v in x]
    cubic = [v ** 3 - 2 * v for v in x]
    for variant in VARIANTS:
        f = Interpolant(x, line, variant[1], variant[2], (Fraction(3), Fraction(3)))
        if any(f.at(q) != 3 * q - Fraction(5, 8) for q in interior_queries(x) + outside_queries(x)):
            failures.append("%s does not reproduce a straight line" % variant[0])
    for bc, slopes in (("not_a_knot", None), ("clamped", (3 * x[0] ** 2 - 2, 3 * x[-1] ** 2 - 2))):
        f = Interpolant(x, cubic, "cubic", bc, slopes)
        if any(f.at(q) != q ** 3 - 2 * q for q in interior_queries(x) + outside_queries(x)):
            failures.append("bc %s does not reproduce a cubic" % bc)
    x, y, _ = fixture_m12()
    f = Interpolant(x, y, "pchip")
    for k in range(len(x) - 1):
        # The derivative on a segment is a quadratic in s; monotone means it never dips below zero,
        # so it is enough to test the ends and the vertex when it falls inside.
        c = poly_derivative(f.polynomial(k))
        h = x[k + 1] - x[k]
        points = [Fraction(0), h]
        if len(c) > 2 and c[2] != 0 and 0 < -c[1] / (2 * c[2]) < h:
            points.append(-c[1] / (2 * c[2]))
        if any(poly_at(c, s) < 0 for s in points):
            failures.append("M12: pchip is not monotone on segment %d" % k)

    # The fixtures must reach every rule that decides a PCHIP slope, or a mutation of that rule
    # passes every golden row.
    wanted = {"interior mean", "interior opposite", "interior flat", "end formula", "end zeroed",
              "end capped"}
    if branches != wanted:
        failures.append("the fixtures reach the PCHIP rules %s, not all of %s" % (sorted(branches), sorted(wanted)))

    # The page's example, against scipy.
    obj = CubicSpline([float(v) for v in PARABOLA_X], [float(v) for v in PARABOLA_Y], bc_type="natural")
    f = Interpolant(PARABOLA_X, PARABOLA_Y, "cubic", "natural")
    agree("P6 natural", [f.at(q) for q in PARABOLA_PROBES + [PARABOLA_FAR]],
          obj([float(v) for v in PARABOLA_PROBES + [PARABOLA_FAR]]), 36.0)

    # Every value the Fortran side reads must be the exact double the model used.
    for name, maker in FIXTURES:
        x, y, slopes = maker()
        values = list(x) + list(y) + list(slopes) + interior_queries(x) + outside_queries(x)
        values += [v for pair in inside_limits(x) + outside_limits(x) for v in pair]
        for v in values:
            if Fraction(float(v)) != v:
                failures.append("%s: %s is not an exact double" % (name, v))

    if failures:
        for line in failures:
            print("self-test FAIL: " + line, file=sys.stderr)
        return 1
    print("generate_interpolate_vectors.py --self-test: the exact model agrees with scipy %s "
          "(numpy %s) to 1e-12 on %d fixtures under %d variants -- values, both derivatives and "
          "integrals, inside the table and beyond it -- and on the page's example; it holds every "
          "property it is checked for, and the fixtures reach all %d PCHIP rules"
          % (scipy.__version__, np.__version__, len(FIXTURES), len(VARIANTS), len(wanted)))
    return 0


# ======================================================================================
# Emission
# ======================================================================================

def fortran_real(v):
    """The round-to-nearest double of an exact value, as a round-trip-exact Fortran literal."""
    text = "%.17g" % float(v)
    if not any(c in text for c in ".eE"):
        text += ".0"
    return text + "_real64"


def wrap_array(head, items, tail="]", opener=" [", width=128):
    """`head [a, b, c]` wrapped with `&` continuations inside the 132-column limit; `opener` opens the
    list after `head` and `tail` closes it."""
    lines, cur = [], head + opener
    pad = " " * (len(head) - len(head.lstrip()) + 4)
    for k, item in enumerate(items):
        piece = item + (", " if k + 1 < len(items) else tail)
        if len(cur) + len(piece) > width:
            lines.append(cur.rstrip() + " &")
            cur = pad + piece
        else:
            cur += piece
    lines.append(cur)
    return lines


HEADER = '''!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_interpolate_vectors.py
!
!> Golden expectations for `parquet_interpolate`, derived EXACTLY over rationals.
!!
!! **The oracle shares no arithmetic with the library.** The generator transcribes each
!! interpolant's definition over `fractions.Fraction` -- the spline systems solved in full, every
!! segment converted to a polynomial and evaluated, differentiated and integrated exactly -- and
!! emits the round-to-nearest double of each exact result. scipy appears only in its `--self-test`,
!! which confirms the model computes what `scipy.interpolate` computes.
!!
!! Every knot, ordinate, slope, query and limit here is a dyadic rational, so each literal is an
!! exact double. For a fixture `F`: `F_X` and `F_Y` are the table and `F_SLOPES` the end slopes of
!! its `bc="clamped"` variant; `F_Q` are the interior queries and `F_O` the outside ones;
!! `(F_A(i), F_B(i))` are the limits of the integrals inside the table and `(F_OA(i), F_OB(i))` of
!! those reaching beyond it. Every expectation array has one column per variant, numbered by the
!! `GI_*` variant indices. `F_VALUE`, `F_D1` and `F_D2` hold the value and the first and second
!! derivatives at each of `F_Q`, and their `_OUT` twins the same at each of `F_O` under
!! `outside="extrapolate"`; `F_INTEGRAL` holds the integral over each inside pair, and
!! `F_INTEGRAL_OUT` and `F_INTEGRAL_CLAMP` the integral over each outside pair under
!! `outside="extrapolate"` and `outside="clamp"`. `P6_*` is the guide page's `x**2` example.
module test_interpolate_golden
    use iso_fortran_env, only : real64
    implicit none
    public

    !> Interior queries each fixture is evaluated at.
    integer, parameter :: GI_NQ = %d
    !> Outside queries each fixture is evaluated at: two below the table, two above.
    integer, parameter :: GI_NO = %d
    !> Pairs of integration limits inside each table.
    integer, parameter :: GI_NI = %d
    !> Pairs of integration limits reaching beyond each table.
    integer, parameter :: GI_NJ = %d
    !> Variants, the columns of every expectation array.
    integer, parameter :: GI_NV = %d
''' % (NQ, NO, NI, NJ, len(VARIANTS))

#: What each variant column holds.
VARIANT_DOCS = {
    "GI_LINEAR": 'method="linear"',
    "GI_NATURAL": 'method="cubic", bc="natural"',
    "GI_NOT_A_KNOT": 'method="cubic", bc="not_a_knot"',
    "GI_CLAMPED": 'method="cubic", bc="clamped", slopes=F_SLOPES',
    "GI_PCHIP": 'method="pchip"',
}

#: What each fixture's doc-comment says it pins.
FIXTURE_DOCS = {
    "U9": "nine knots a quarter apart: the evenly spaced table, bracketed by arithmetic",
    "G16": "sixteen knots whose spacing doubles every second knot, from 1/16 to 8",
    "R33": "thirty-three knots at irregular gaps from 1/16 to 11/16, ordinates of both signs",
    "M12": "twelve knots of non-decreasing data with a flat run, PCHIP's monotone case",
    "K4": "four knots, the smallest table bc=\"not_a_knot\" accepts",
}

#: What each expectation array holds, and its row count.
CASE_DOCS = {
    "VALUE": ("the value at each interior query", "GI_NQ"),
    "VALUE_OUT": ("the value at each outside query, under outside=\"extrapolate\"", "GI_NO"),
    "D1": ("the first derivative at each interior query", "GI_NQ"),
    "D2": ("the second derivative at each interior query", "GI_NQ"),
    "D1_OUT": ("the first derivative at each outside query, under outside=\"extrapolate\"", "GI_NO"),
    "D2_OUT": ("the second derivative at each outside query, under outside=\"extrapolate\"", "GI_NO"),
    "INTEGRAL": ("the integral over each inside pair of limits", "GI_NI"),
    "INTEGRAL_OUT": ("the integral over each outside pair of limits, under outside=\"extrapolate\"", "GI_NJ"),
    "INTEGRAL_CLAMP": ("the integral over each outside pair of limits, under outside=\"clamp\"", "GI_NJ"),
}


def emit():
    """Return the whole generated file as text."""
    out = [HEADER]
    for index, (name, _, _) in enumerate(VARIANTS, start=1):
        out.append("    !> Column of `%s`." % VARIANT_DOCS[name])
        out.append("    integer, parameter :: %s = %d" % (name, index))
    out.append("")
    for name, maker in FIXTURES:
        x, y, slopes = maker()
        out.append("    ! ---- %s: %s ----" % (name, FIXTURE_DOCS[name]))
        out.append("")
        out.append("    !> Knots in fixture %s." % name)
        out.append("    integer, parameter :: %s_N = %d" % (name, len(x)))
        out.append("    !> Fixture %s's abscissae, strictly increasing." % name)
        out += wrap_array("    real(real64), parameter :: %s_X(%s_N) =" % (name, name), [fortran_real(v) for v in x])
        out.append("    !> Fixture %s's ordinates." % name)
        out += wrap_array("    real(real64), parameter :: %s_Y(%s_N) =" % (name, name), [fortran_real(v) for v in y])
        out.append("    !> Fixture %s's end slopes for bc=\"clamped\", at its first knot and its last." % name)
        out += wrap_array("    real(real64), parameter :: %s_SLOPES(2) =" % name, [fortran_real(v) for v in slopes])
        out.append("    !> Fixture %s's interior queries." % name)
        out += wrap_array("    real(real64), parameter :: %s_Q(GI_NQ) =" % name,
                          [fortran_real(v) for v in interior_queries(x)])
        out.append("    !> Fixture %s's outside queries." % name)
        out += wrap_array("    real(real64), parameter :: %s_O(GI_NO) =" % name,
                          [fortran_real(v) for v in outside_queries(x)])
        for suffix, pairs in (("", inside_limits(x)), ("O", outside_limits(x))):
            count = "GI_NI" if suffix == "" else "GI_NJ"
            where = "inside the table" if suffix == "" else "reaching beyond the table"
            out.append("    !> Fixture %s's lower integration limits %s." % (name, where))
            out += wrap_array("    real(real64), parameter :: %s_%sA(%s) =" % (name, suffix, count),
                              [fortran_real(a) for a, _ in pairs])
            out.append("    !> Fixture %s's upper integration limits %s." % (name, where))
            out += wrap_array("    real(real64), parameter :: %s_%sB(%s) =" % (name, suffix, count),
                              [fortran_real(b) for _, b in pairs])
        for suffix, columns in cases(x, y, slopes).items():
            doc, rows = CASE_DOCS[suffix]
            out.append("    !> Fixture %s: %s, one column per variant." % (name, doc))
            out += wrap_array("    real(real64), parameter :: %s_%s(%s, GI_NV) = reshape(" % (name, suffix, rows),
                              [fortran_real(v) for column in columns for v in column],
                              tail="], [%s, GI_NV])" % rows, opener="[")
        out.append("")

    parabola = Interpolant(PARABOLA_X, PARABOLA_Y, "cubic", "natural")
    out.append("    ! ---- P6: the guide page's example, x**2 at x = 1..6 ----")
    out.append("")
    out.append("    !> The page's probes.")
    out += wrap_array("    real(real64), parameter :: P6_PROBES(3) =", [fortran_real(v) for v in PARABOLA_PROBES])
    out.append("    !> The natural cubic spline at each probe: NOT the parabola, whose second "
               "derivative is not zero at the ends.")
    out += wrap_array("    real(real64), parameter :: P6_NATURAL(3) =",
                      [fortran_real(parabola.at(q)) for q in PARABOLA_PROBES])
    out.append("    !> Where the page extrapolates to.")
    out.append("    real(real64), parameter :: P6_FAR = %s" % fortran_real(PARABOLA_FAR))
    out.append("    !> The natural cubic spline's end segment continued to `P6_FAR`.")
    out.append("    real(real64), parameter :: P6_NATURAL_FAR = %s" % fortran_real(parabola.at(PARABOLA_FAR)))
    out.append("")
    out.append("end module test_interpolate_golden ! GCOVR_EXCL_LINE")
    text = "\n".join(out) + "\n"
    for lineno, line in enumerate(text.split("\n"), start=1):
        if len(line) > 132:
            raise SystemExit("generate_interpolate_vectors.py: emitted line %d is %d columns"
                             % (lineno, len(line)))
    return text


def main(argv):
    if "--self-test" in argv:
        return self_test()
    text = emit()
    if "--check" in argv:
        try:
            have = OUT_PATH.read_text()
        except OSError as exc:
            print("--check: cannot read %s (%s)" % (OUT_PATH, exc), file=sys.stderr)
            return 1
        if have != text:
            print("generate_interpolate_vectors.py --check: %s is out of date. Re-run "
                  "tools/generate_interpolate_vectors.py." % OUT_PATH.relative_to(REPO_ROOT),
                  file=sys.stderr)
            return 1
        print("generate_interpolate_vectors.py --check: %s carries all %d fixtures, current."
              % (OUT_PATH.relative_to(REPO_ROOT), len(FIXTURES) + 1))
        return 0
    OUT_PATH.write_text(text)
    print("generate_interpolate_vectors.py: wrote %s (%d fixtures, exact rational model)"
          % (OUT_PATH.relative_to(REPO_ROOT), len(FIXTURES) + 1))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
