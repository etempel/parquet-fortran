#!/usr/bin/env python3
"""Golden expectations for `parquet_interpolate`, from an exact-rational model that shares no arithmetic.

The committed expectations come from the interpolants' definitions transcribed over
`fractions.Fraction`: every tridiagonal system is solved and every segment polynomial evaluated
EXACTLY, over the fixtures' exact rational values, and each expectation is emitted as the
round-to-nearest double of that exact number. The library's double-precision solve agrees with it
to a few ulp, because the spline systems are strictly diagonally dominant and so well conditioned;
the suite asserts 1e-13 relative. scipy appears only in `--self-test`, as the proof that the model
transcribes the same definitions `scipy.interpolate` implements -- so no emitted digit depends on a
library version, and `--check` needs nothing but the interpreter.

**The fixtures are dyadic rationals**: every knot, ordinate and query is an integer over a power of
two, so each is an exact double and Python and Fortran hold the same table bit for bit. They are
emitted as literals rather than mirrored as recipes on the Fortran side: the largest has 33 points,
which is shorter as a literal than as the code that would rebuild it, and a literal cannot drift
from the generator that produced it.

What each fixture pins:

* `U9`, nine knots a quarter apart: the evenly spaced table, whose bracket the library finds by
  arithmetic rather than by bisection;
* `G16`, sixteen knots whose spacing doubles every second knot: a spread of 128 between the
  narrowest and the widest segment, where a boundary row or a spacing taken from the wrong side
  moves every value;
* `R33`, thirty-three knots at irregular gaps: the general case, and a sign-changing ordinate;
* `P6`, `x**2` at `x = 1..6`: the example the guide page quotes, a natural spline that does NOT
  reproduce the parabola, and its extrapolation to `x = 99`.

Each of the first three is evaluated at 25 interior queries (some of which land on knots) and at 4
outside it, the latter under `outside="extrapolate"`: the end segment's polynomial continued.

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

#: Interior queries per fixture, and outside queries (two below the table, two above).
NQ, NO = 25, 4


# ======================================================================================
# The fixtures: dyadic rationals, so every value is an exact double in both languages
# ======================================================================================

def fixture_u9():
    """Nine knots a quarter apart on [0, 2]; ordinates an integer recipe over 16."""
    x = [Fraction(i, 4) for i in range(9)]
    y = [Fraction((i * i * 37 + 11) % 23 - 11, 16) for i in range(9)]
    return x, y


def fixture_g16():
    """Sixteen knots whose spacing doubles every second knot, from 1/16 to 8; ordinates over 64."""
    x = [Fraction(0)]
    for j in range(15):
        x.append(x[-1] + Fraction(2 ** (j // 2), 16))
    y = [Fraction((i * i * 53 + 7 * i + 3) % 41 - 20, 64) for i in range(16)]
    return x, y


def fixture_r33():
    """Thirty-three knots from -2 at gaps of 1/16 to 11/16; ordinates over 256, both signs."""
    x = [Fraction(-2)]
    for j in range(32):
        x.append(x[-1] + Fraction(1 + (j * j * 13 + 7 * j + 5) % 11, 16))
    y = [Fraction((i * i * i * 7 + 29 * i + 13) % 97 - 48, 256) for i in range(33)]
    return x, y


#: The three general fixtures, by the name their Fortran parameters carry.
FIXTURES = [("U9", fixture_u9), ("G16", fixture_g16), ("R33", fixture_r33)]


def interior_queries(x):
    """25 dyadic points spread over the table, at `x0 + span*c/64` for rising odd-and-even `c`."""
    span = x[-1] - x[0]
    return [x[0] + span * Fraction(1 + (j * 62) // 24, 64) for j in range(NQ)]


def outside_queries(x):
    """Two points below the table and two above, at an eighth and a sixty-fourth of its span."""
    span = x[-1] - x[0]
    return [x[0] - span / 8, x[0] - span / 64, x[-1] + span / 64, x[-1] + span / 8]


#: The guide page's example: `x**2` tabulated at `x = 1..6`, the three probes it prints, and the
#: extrapolation it warns about.
PARABOLA_X = [Fraction(i) for i in range(1, 7)]
PARABOLA_Y = [xi * xi for xi in PARABOLA_X]
PARABOLA_PROBES = [Fraction(3, 2), Fraction(5, 2), Fraction(11, 2)]
PARABOLA_FAR = Fraction(99)


# ======================================================================================
# The model: the definitions, exactly
# ======================================================================================

def thomas(sub, diag, sup, rhs):
    """Solve a tridiagonal system exactly. `sub[i]` multiplies u[i-1] in row i, `sup[i]` u[i+1]."""
    n = len(diag)
    gam = [Fraction(0)] * n
    u = [Fraction(0)] * n
    bet = diag[0]
    u[0] = rhs[0] / bet
    for j in range(1, n):
        gam[j] = sup[j - 1] / bet
        bet = diag[j] - sub[j] * gam[j]
        u[j] = (rhs[j] - sub[j] * u[j - 1]) / bet
    for j in range(n - 2, -1, -1):
        u[j] -= gam[j + 1] * u[j + 1]
    return u


def natural_second_derivatives(x, y):
    """The second derivatives at the knots of the natural cubic spline: zero at both ends."""
    n = len(x)
    sub, diag, sup, rhs = ([Fraction(0)] * n for _ in range(4))
    diag[0] = diag[n - 1] = Fraction(1)
    for i in range(1, n - 1):
        h0, h1 = x[i] - x[i - 1], x[i + 1] - x[i]
        sub[i], diag[i], sup[i] = h0, 2 * (h0 + h1), h1
        rhs[i] = 6 * ((y[i + 1] - y[i]) / h1 - (y[i] - y[i - 1]) / h0)
    return thomas(sub, diag, sup, rhs)


def segment(x, q):
    """The segment `k` (0-based) the library evaluates `q` on: `x[k] <= q < x[k+1]` for a point
    inside the table, the last segment at `x[-1]`, and the end segment for a point outside it --
    which is how `"extrapolate"` continues the end segment's polynomial."""
    k = 0
    while k < len(x) - 2 and q >= x[k + 1]:
        k += 1
    return k


def linear_value(x, y, q):
    """Piecewise-linear value, the end segment's line continued outside the table."""
    k = segment(x, q)
    t = (q - x[k]) / (x[k + 1] - x[k])
    return y[k] + t * (y[k + 1] - y[k])


def cubic_value(x, y, m, q):
    """Cubic-spline value in second-derivative form, the end segment's cubic continued outside."""
    k = segment(x, q)
    h = x[k + 1] - x[k]
    a = (x[k + 1] - q) / h
    b = (q - x[k]) / h
    return a * y[k] + b * y[k + 1] + ((a ** 3 - a) * m[k] + (b ** 3 - b) * m[k + 1]) * h * h / 6


def cases(x, y):
    """Every expectation one fixture carries, keyed by the suffix its Fortran parameter takes."""
    m = natural_second_derivatives(x, y)
    inside, outside = interior_queries(x), outside_queries(x)
    return {
        "LINEAR": [linear_value(x, y, q) for q in inside],
        "LINEAR_OUT": [linear_value(x, y, q) for q in outside],
        "NATURAL": [cubic_value(x, y, m, q) for q in inside],
        "NATURAL_OUT": [cubic_value(x, y, m, q) for q in outside],
    }


# ======================================================================================
# --self-test: the model against scipy.interpolate
# ======================================================================================

def self_test():
    """Validate the exact model against scipy, and the model against properties it must have."""
    try:
        import numpy as np
        from scipy.interpolate import CubicSpline, interp1d
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

    tables = [(name, maker()) for name, maker in FIXTURES] + [("P6", (PARABOLA_X, PARABOLA_Y))]
    for name, (x, y) in tables:
        xf, yf = np.array([float(v) for v in x]), np.array([float(v) for v in y])
        scale = float(np.max(np.abs(yf)))
        inside = interior_queries(x) if name != "P6" else PARABOLA_PROBES
        outside = outside_queries(x) if name != "P6" else [PARABOLA_FAR]
        qin = np.array([float(v) for v in inside])
        qout = np.array([float(v) for v in outside])
        m = natural_second_derivatives(x, y)

        agree(name + " linear", [linear_value(x, y, q) for q in inside], np.interp(qin, xf, yf), scale)
        line = interp1d(xf, yf, kind="linear", fill_value="extrapolate", assume_sorted=True)
        agree(name + " linear outside", [linear_value(x, y, q) for q in outside], line(qout), scale)

        spline = CubicSpline(xf, yf, bc_type="natural", extrapolate=True)
        agree(name + " natural", [cubic_value(x, y, m, q) for q in inside], spline(qin), scale)
        agree(name + " natural outside", [cubic_value(x, y, m, q) for q in outside], spline(qout),
              scale)
        agree(name + " natural m", m, spline(xf, 2), scale)

        # The model's own properties, independently of scipy: every knot is reproduced exactly,
        # and the natural end rows hold.
        for k in range(len(x)):
            if linear_value(x, y, x[k]) != y[k] or cubic_value(x, y, m, x[k]) != y[k]:
                failures.append("%s: the model does not reproduce knot %d exactly" % (name, k))
        if m[0] != 0 or m[-1] != 0:
            failures.append("%s: the natural end rows do not hold" % name)

    # A natural spline reproduces a straight line exactly, and its second derivatives are zero.
    x, _ = fixture_r33()
    y = [3 * v - Fraction(5, 8) for v in x]
    m = natural_second_derivatives(x, y)
    if any(v != 0 for v in m):
        failures.append("the natural spline of a straight line has a nonzero second derivative")
    for q in interior_queries(x) + outside_queries(x):
        if cubic_value(x, y, m, q) != 3 * q - Fraction(5, 8):
            failures.append("the natural spline does not reproduce a straight line at %s" % q)

    # The fixture queries the Fortran side reads must be the exact doubles the model used.
    for name, maker in FIXTURES:
        x, y = maker()
        for v in list(x) + list(y) + interior_queries(x) + outside_queries(x):
            if Fraction(float(v)) != v:
                failures.append("%s: %s is not an exact double" % (name, v))

    if failures:
        for line in failures:
            print("self-test FAIL: " + line, file=sys.stderr)
        return 1
    print("generate_interpolate_vectors.py --self-test: the exact model agrees with scipy %s "
          "(numpy %s) on %d fixtures to 1e-12, reproduces every knot and a straight line exactly"
          % (scipy.__version__, np.__version__, len(tables)))
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


def wrap_array(head, items, width=128):
    """`head [a, b, c]` wrapped with `&` continuations inside the 132-column limit."""
    lines, cur = [], head + " ["
    pad = " " * (len(head) - len(head.lstrip()) + 4)
    for k, item in enumerate(items):
        piece = item + (", " if k + 1 < len(items) else "]")
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
!! interpolant's definition over `fractions.Fraction` -- the tridiagonal solve and the segment
!! polynomials carried out exactly over the fixture's rational values -- and emits the
!! round-to-nearest double of each exact result. scipy appears only in its `--self-test`, which
!! confirms the model computes what `scipy.interpolate` computes.
!!
!! Every knot, ordinate and query here is a dyadic rational, so each literal is an exact double.
!! For a fixture `F`: `F_X` and `F_Y` are the table, `F_Q` the interior queries and `F_O` the
!! outside ones; `F_<METHOD>` holds the expectation at each of `F_Q`, and `F_<METHOD>_OUT` at each
!! of `F_O` under `outside="extrapolate"`. `P6_*` is the guide page's `x**2` example.
module test_interpolate_golden
    use iso_fortran_env, only : real64
    implicit none
    public

    !> Interior queries each fixture is evaluated at.
    integer, parameter :: GI_NQ = %d
    !> Outside queries each fixture is evaluated at: two below the table, two above.
    integer, parameter :: GI_NO = %d
''' % (NQ, NO)

#: What each fixture's doc-comment says it pins.
FIXTURE_DOCS = {
    "U9": "nine knots a quarter apart: the evenly spaced table, bracketed by arithmetic",
    "G16": "sixteen knots whose spacing doubles every second knot, from 1/16 to 8",
    "R33": "thirty-three knots at irregular gaps from 1/16 to 11/16, ordinates of both signs",
}

#: What each expectation array holds.
CASE_DOCS = {
    "LINEAR": "method=\"linear\" at each interior query",
    "LINEAR_OUT": "method=\"linear\", outside=\"extrapolate\", at each outside query",
    "NATURAL": "method=\"cubic\", bc=\"natural\" at each interior query",
    "NATURAL_OUT": "method=\"cubic\", bc=\"natural\", outside=\"extrapolate\", at each outside query",
}


def emit():
    """Return the whole generated file as text."""
    out = [HEADER]
    for name, maker in FIXTURES:
        x, y = maker()
        out.append("    ! ---- %s: %s ----" % (name, FIXTURE_DOCS[name]))
        out.append("")
        out.append("    !> Knots in fixture %s." % name)
        out.append("    integer, parameter :: %s_N = %d" % (name, len(x)))
        out.append("    !> Fixture %s's abscissae, strictly increasing." % name)
        out += wrap_array("    real(real64), parameter :: %s_X(%s_N) =" % (name, name),
                          [fortran_real(v) for v in x])
        out.append("    !> Fixture %s's ordinates." % name)
        out += wrap_array("    real(real64), parameter :: %s_Y(%s_N) =" % (name, name),
                          [fortran_real(v) for v in y])
        out.append("    !> Fixture %s's interior queries." % name)
        out += wrap_array("    real(real64), parameter :: %s_Q(GI_NQ) =" % name,
                          [fortran_real(v) for v in interior_queries(x)])
        out.append("    !> Fixture %s's outside queries." % name)
        out += wrap_array("    real(real64), parameter :: %s_O(GI_NO) =" % name,
                          [fortran_real(v) for v in outside_queries(x)])
        for suffix, values in cases(x, y).items():
            size = "GI_NO" if suffix.endswith("_OUT") else "GI_NQ"
            out.append("    !> %s" % CASE_DOCS[suffix])
            out += wrap_array("    real(real64), parameter :: %s_%s(%s) =" % (name, suffix, size),
                              [fortran_real(v) for v in values])
        out.append("")

    m = natural_second_derivatives(PARABOLA_X, PARABOLA_Y)
    out.append("    ! ---- P6: the guide page's example, x**2 at x = 1..6 ----")
    out.append("")
    out.append("    !> The page's probes.")
    out += wrap_array("    real(real64), parameter :: P6_PROBES(3) =",
                      [fortran_real(v) for v in PARABOLA_PROBES])
    out.append("    !> The natural cubic spline at each probe: NOT the parabola, whose second "
               "derivative is not zero at the ends.")
    out += wrap_array("    real(real64), parameter :: P6_NATURAL(3) =",
                      [fortran_real(cubic_value(PARABOLA_X, PARABOLA_Y, m, q)) for q in PARABOLA_PROBES])
    out.append("    !> Where the page extrapolates to.")
    out.append("    real(real64), parameter :: P6_FAR = %s" % fortran_real(PARABOLA_FAR))
    out.append("    !> The natural cubic spline's end segment continued to `P6_FAR`.")
    out.append("    real(real64), parameter :: P6_NATURAL_FAR = %s"
               % fortran_real(cubic_value(PARABOLA_X, PARABOLA_Y, m, PARABOLA_FAR)))
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
