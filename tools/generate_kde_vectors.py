#!/usr/bin/env python3
"""Golden expectations for `parquet_kde`, from a 50-digit oracle that shares no arithmetic.

The committed expectations are computed by `mpmath` at 50 decimal digits over the fixture's EXACT
rational values, summing every kernel of every point -- no window, no running weight sum, no
binary search -- so they share no arithmetic with the library. scipy, KDEpy and statsmodels
appear only in `--self-test`, confirming that the model transcribes the same definitions those
libraries implement where they share one.

**One definition of the population, and of the scale the rules read.** The fixture recipe, the
family's exclusion order, the standard deviation and the weighted quantile are imported from
`tools/generate_stats_vectors.py`, which is the oracle `parquet_stats` is pinned against, so a
bandwidth rule here reads exactly the `pf_stddev` and `pf_iqr` that module's golden file
certifies. The Fortran side builds the fixture from the same recipe (`kde_fixture` in
test/test_kde.f90) and asserts `KG_PROBE`, which reports a recipe that has drifted.

What each case pins is named in its own comment in the generated file: every kernel's density
and distribution function, Silverman's and Scott's rules, the ISJ rule, `adjust`, both weight
conventions, both boundary corrections at each bound and at both, the case where the kernel is
wider than the whole support (which only the mass normalisation keeps at unit mass), the
single-point degenerate cases, the adaptive kernel over a two-component recipe of its own, and
the least-squares cross-validation CRITERION.

**The bandwidth `rule="lscv"` chooses is deliberately not pinned here**, although its criterion
is; the reasoning is at "The cross-validation criterion's oracle" below, beside the cases.

**The ISJ cases solve the rule's fixed point at 50 digits on the library's own grid.** The Improved
Sheather-Jones rule bins the sample onto cell centres, takes the discrete cosine transform and finds
the smallest root of Botev, Grotowski & Kroese's fixed point at or above one cell. The oracle does
each step in exact arithmetic -- the linear split by rationals, the transform as a 50-digit cosine
sum, the root by `findroot` inside the bracket the library's doubling search would find -- over
`ISJ_CELLS = 1024` cells, which `parquet_debug_set_kde_isj_cells(1024)` makes the library use: at the
library's default of `2**14` the transform would take minutes here. Every sign the search decides
on is asserted to hold with a margin, and a float scan confirms the bracket holds one root, so a
case cannot sit on a knife edge the two arithmetics could decide differently.

**The adaptive cases take the pilot exactly.** The library reads each point's bandwidth from a
pilot GRID, `h * (p(x_j)/g)**(-alpha)`, with `p` the grid interpolated and `log g` the mean of
`log p` over the grid's density; the oracle takes the continuous limit of the same definitions --
`p` the exact fixed estimate, summed over every point, and `log g` its integral over the pilot's
range, `[max(lower, min - 4h), min(upper, max + 4h)]`, by quadrature between the kernels' knots.
The library agrees to the pilot's discretisation, about `1e-3` at the rule's cells and to far
closer once `parquet_debug_set_kde_pilot_cells` makes them fine; the tests assert both.

Usage:
    python3 tools/generate_kde_vectors.py              # rewrite test/test_kde_golden.f90
    python3 tools/generate_kde_vectors.py --check      # assert the committed file is current
    python3 tools/generate_kde_vectors.py --self-test  # validate the model (see below)

Plain generation and `--check` need only `mpmath`; `--check` runs in CI's lint stage beside
`generate_stats_vectors.py --check`. `--self-test` needs scipy and fails without it; it also
cross-checks against KDEpy and statsmodels when they import, and reports each one that does not
as SKIPPED by name. It is a maintainer's run in the `astro` environment, which carries scipy and
KDEpy, so a KDEpy skip reported there is an environment fault. Its ISJ cross-check is built from
KDEpy's fixed-point function and linear binning and scipy's transform, and runs the model at the
library's default grid; KDEpy's own `improved_sheather_jones` is not a reference (see
`self_test`).

Maintainer-only: it is never run at build time and is stripped from the fpm-published package.
"""

import math
import sys
import textwrap
from pathlib import Path

try:
    from mpmath import mp, mpf, sqrt as mpsqrt, erf, erfc, exp, pi, quad, cos as mpcos, findroot
except ImportError:                                            # pragma: no cover - maintainer tool
    print("generate_kde_vectors.py: needs mpmath (pip install mpmath)", file=sys.stderr)
    raise SystemExit(2)

sys.path.insert(0, str(Path(__file__).resolve().parent))
import generate_stats_vectors as gsv                           # noqa: E402  the one population model
from gen_parallel import map_cases                             # noqa: E402  the shared case map

mp.dps = 50

REPO_ROOT = Path(__file__).resolve().parent.parent
OUT_PATH = REPO_ROOT / "test" / "test_kde_golden.f90"

#: Where every case is probed. Chosen to reach the interior, both tails, both sides of the bounds
#: the bounded cases use (-470 and 460) and a point beyond each; every value is an exact double.
PROBES = [-512.0, -466.0, -300.0, -61.25, 0.0, 17.5, 250.0, 458.5, 470.0]

#: The Gaussian's cut, in standard deviations.
CUT = mpf(5)

#: The pilot's reach beyond the extreme points, in global bandwidths (`KDE_PILOT_REACH`).
PILOT_REACH = mpf(4)

#: How much wider the adaptive kernel's global bandwidth is than the same rule's for the fixed
#: estimator, at `alpha = 0.5` (`KDE_ADAPT_INFLATE`). The factor is `ADAPT_INFLATE ** (2 * alpha)`,
#: exactly one at `alpha = 0`. Keep in step with the Fortran parameter: a default that moves in one
#: and not the other makes every adaptive case here disagree, which is what these vectors are for.
ADAPT_INFLATE = mpf("1.5")

#: The default cap on the adaptive rule's SPREAD (`KDE_SPREAD_MAX` in `src/parquet_kde.f90`): no
#: point's bandwidth exceeds this many times the narrowest the rule can give.
#:
#: **This oracle cannot mirror it, and must not have to.** The library forms the cap from the
#: LARGEST CELL DENSITY of its pilot grid, while every pilot here is the exact continuous estimate:
#: the two maxima differ by the grid's own discretisation, so a fixture whose spread reached the cap
#: would be capped at two slightly different numbers and the vectors would disagree for a reason
#: that is not a defect. Every adaptive case here is therefore required to stay BELOW the cap, and
#: `check_spread_below_cap` fails loudly if one ever stops doing so, rather than letting the
#: omission go quiet. The cap itself is pinned by `test_spread_max_caps` in `test/test_kde.f90`,
#: which re-derives it from the pilot the library hands back.
SPREAD_MAX = mpf("100")


def check_spread_below_cap(case, hs):
    """Refuses a case whose adaptive spread reaches the library's default cap.

    `hs` are the per-point bandwidths this oracle formed, uncapped by anything but an explicit
    `bandwidth_max`. See `SPREAD_MAX` for why a case at or above the cap cannot be emitted here.
    """
    if not hs:
        return
    lo, hi = min(hs), max(hs)
    if lo > 0 and hi / lo >= SPREAD_MAX:
        raise SystemExit(
            "the case %r has an adaptive spread of %s, at or above the default spread cap of %s: "
            "the library would cap it from its pilot's cell maximum, which this oracle cannot "
            "reproduce. Narrow the fixture, or give it an explicit bandwidth_max."
            % (case, mp.nstr(hi / lo, 6), mp.nstr(SPREAD_MAX, 6)))

#: The digits the pilot's entropy integral is taken to. `log g` is read to about `1e-12` by the
#: tightest test (a bandwidth moves by `alpha` times its error), and a quadrature at 50 digits
#: would cost minutes; `--self-test` confirms this one against 30.
G_DPS = 18


def two_component(n):
    """The adaptive cases' recipe: two clusters, one five times as wide as the other.

    Two points in three fall in a narrow cluster about -300 (`v/16`, within 32 of it) and the
    third in a wide one about +300 (`5v/16`, within 160), `v` a spread integer in `[-512, 511]`
    from the stats oracle's recipe. Every value is an integer over 16, exact in both languages;
    `kde_two_component` in test/test_kde.f90 mirrors it and `KG_PROBE2` reports a drift.
    """
    out = []
    for i in range(1, n + 1):
        a = (i * i * gsv.RECIPE_A + gsv.RECIPE_B) % gsv.RECIPE_M
        v = a % 1024 - 512
        if i % 3 == 0:
            out.append(300.0 + 5.0 * v / 16.0)
        else:
            out.append(-300.0 + v / 16.0)
    return out


def two_component_rounded(n, step):
    """The two-component recipe rounded to multiples of `step`: `step*floor(v/step + 1/2)`.

    Exact in both languages for a power-of-two `step`: every value of the recipe is an integer over
    16. The ISJ cases use it to put a comb of repeated values on the rule's grid.
    """
    return [step * math.floor(v / step + 0.5) for v in two_component(n)]


# ======================================================================================
# The Improved Sheather-Jones rule
# ======================================================================================

#: The cells the golden ISJ cases bin into, as `parquet_debug_set_kde_isj_cells(1024)` makes the
#: library do. The library's own default is `2**14` (`KDE_ISJ_CELLS`).
ISJ_CELLS = 1024

#: The share of the range the grid reaches beyond each extreme point (`KDE_ISJ_WIDEN`).
ISJ_WIDEN = mpf(1) / 10

#: The fixed point's number of stages, `l` (`KDE_ISJ_STAGES`).
ISJ_STAGES = 7

#: The last `t` the library's bracket search probes (`KDE_ISJ_T_MAX`).
ISJ_T_MAX = mpf(1)

#: A term whose exponent `k**2 pi**2 t` passes this is below `1e-170` of the one before the sum's
#: largest, far beyond 50 digits of any norm here; the library stops at 708 for its own reason.
ISJ_EXP_CUT = 400

#: How clearly a sign the search decides on must hold, relative to `t`: the library's arithmetic
#: agrees with this model to about `1e-14`, so any margin far above that settles the case.
ISJ_MARGIN = mpf("1e-3")


def isj_mass(xs, ws, lo, hi, cells):
    """The sample binned onto `cells` centres and normalised to one, with the grid's span.

    The grid is the range widened by `ISJ_WIDEN` of it on each side, clipped to `[lo, hi]`, cut into
    `cells` cells; each point splits its weight between the two centres about it in proportion to
    its distance from each, and a point in the half cell beyond the first or last centre gives its
    whole weight to that centre. `None` for a sample with no spread.
    """
    pairs = sorted(zip(xs, ws), key=lambda t: t[0])
    if len(pairs) < 2 or pairs[-1][0] - pairs[0][0] <= 0:
        return None
    span = pairs[-1][0] - pairs[0][0]
    a = pairs[0][0] - ISJ_WIDEN * span
    b = pairs[-1][0] + ISJ_WIDEN * span
    if lo is not None and lo > a:
        a = lo
    if hi is not None and hi < b:
        b = hi
    dx = (b - a) / cells
    first = a + dx / 2
    last = a + (cells - mpf(1) / 2) * dx
    mass = {}

    def deposit(j, v):
        mass[j] = mass.get(j, mpf(0)) + v

    for x, w in pairs:
        if x <= first:
            deposit(0, w)
        elif x >= last:
            deposit(cells - 1, w)
        else:
            u = (x - first) / dx
            k = int(mp.floor(u))
            deposit(k, w * (1 - (u - k)))
            deposit(k + 1, w * (u - k))
    total = sum(mass.values())
    return {j: v / total for j, v in mass.items()}, b - a


def isj_spectrum(mass, cells):
    """`(y_k/2)**2` for `k = 1 .. cells - 1`, `y` the type-II transform with its factor of two.

    `y_k = 2 sum_j p_j cos(pi k (2j + 1)/(2 cells))`, summed over the centres that hold mass, with
    the cosine read from a table of the `4 cells` angles the index can reach.
    """
    n4 = 4 * cells
    table = [mpcos(pi * q / (2 * cells)) for q in range(n4)]
    items = sorted(mass.items())
    out = []
    for k in range(1, cells):
        acc = mpf(0)
        for j, v in items:
            acc += v * table[(k * (2 * j + 1)) % n4]
        out.append(acc * acc)          # (y_k/2)**2, y_k = 2*acc
    return out


class IsjFixedPoint:
    """`F(t) = t - xi*gamma^[l](t)` over one sample's spectrum, at the working precision."""

    def __init__(self, b2, n_eff):
        self.n_eff = n_eff
        self.kk = [mpf(k) ** 2 for k in range(1, len(b2) + 1)]
        self.p = {s: [kk ** s * b for kk, b in zip(self.kk, b2)] for s in range(2, ISJ_STAGES + 1)}
        self.pi2 = pi ** 2

    def norm(self, s, t):
        """`2 pi**(2s) sum_k k**(2s) (y_k/2)**2 exp(-k**2 pi**2 t)`."""
        ct = self.pi2 * t
        acc = mpf(0)
        for kk, p in zip(self.kk, self.p[s]):
            if kk * ct > ISJ_EXP_CUT:
                break
            acc += p * exp(-kk * ct)
        return 2 * pi ** (2 * s) * acc

    def __call__(self, t):
        f = self.norm(ISJ_STAGES, t)
        for s in range(ISJ_STAGES - 1, 1, -1):
            if f <= 0:
                return mpf("-inf")
            odd = 1
            for q in range(1, 2 * s, 2):
                odd *= q
            k0 = mpf(odd) / mpsqrt(2 * pi)
            c = (1 + mpf(2) ** (-(s + mpf(1) / 2))) / 3
            f = self.norm(s, (2 * c * k0 / (self.n_eff * f)) ** (mpf(2) / (3 + 2 * s)))
        if f <= 0:
            return mpf("-inf")
        return t - (2 * self.n_eff * mpsqrt(pi) * f) ** (mpf(-2) / 5)


def isj_float_sign_changes(b2, n_eff, lo_t, hi_t, points=96):
    """The sign changes of the fixed point in doubles over `[lo_t, hi_t]`, log-spaced: a guard that
    the bracket the search found holds exactly one root, which the model and the library then both
    converge on whatever the order of their steps."""
    kk = [float(k) ** 2 for k in range(1, len(b2) + 1)]
    bf = [float(v) for v in b2]
    pi2 = math.pi ** 2

    def norm(s, t):
        acc = 0.0
        for k2, b in zip(kk, bf):
            e = k2 * pi2 * t
            if e > 700:
                break
            acc += k2 ** s * b * math.exp(-e)
        return 2 * math.pi ** (2 * s) * acc

    def fixed_point(t):
        f = norm(ISJ_STAGES, t)
        for s in range(ISJ_STAGES - 1, 1, -1):
            if not f > 1e-300:
                return -math.inf
            odd = 1
            for q in range(1, 2 * s, 2):
                odd *= q
            c = (1 + 2.0 ** (-(s + 0.5))) / 3
            f = norm(s, (2 * c * odd / math.sqrt(2 * math.pi) / (float(n_eff) * f)) ** (2 / (3 + 2 * s)))
        if not f > 1e-300:
            return -math.inf
        return t - (2 * float(n_eff) * math.sqrt(math.pi) * f) ** (-0.4)

    ratio = (float(hi_t) / float(lo_t)) ** (1.0 / (points - 1))
    signs = [fixed_point(float(lo_t) * ratio ** i) > 0 for i in range(points)]
    return sum(1 for i in range(1, points) if signs[i] != signs[i - 1])


def isj_bandwidth(xs, ws, n_eff, lo, hi, cells=ISJ_CELLS):
    """`(h, reason)`: the rule's bandwidth, or `None` and why the rule found none.

    The search is the library's: `F` must be negative at one cell, `t = 1/cells**2`; the bracket's
    upper end starts at twice that and moves to `tlo + 2 (b - tlo)` until `F` is positive there or
    it reaches `ISJ_T_MAX`, where the search stops; the root is the one in the last step's bracket.
    """
    got = isj_mass(xs, ws, lo, hi, cells)
    if got is None:
        return None, "the sample has no spread"
    mass, span = got
    b2 = isj_spectrum(mass, cells)
    F = IsjFixedPoint(b2, n_eff)
    tlo = mpf(1) / mpf(cells) ** 2
    f_lo = F(tlo)
    if not f_lo < -ISJ_MARGIN * tlo:
        if f_lo > ISJ_MARGIN * tlo:
            return None, "the fixed point is not negative at one cell"
        raise SystemExit("ISJ: F at one cell is %s, too close to zero to decide" % mp.nstr(f_lo, 5))
    prev, b = tlo, 2 * tlo
    while True:
        fb = F(b)
        if fb > ISJ_MARGIN * b:
            break
        if not fb < -ISJ_MARGIN * b:
            raise SystemExit("ISJ: F at a probe %s is %s, too close to zero" % (mp.nstr(b, 5), mp.nstr(fb, 5)))
        if b >= ISJ_T_MAX:
            return None, "the fixed point has no sign change up to t = 1"
        prev, b = b, min(tlo + 2 * (b - tlo), ISJ_T_MAX)
    changes = isj_float_sign_changes(b2, n_eff, tlo, b)
    if changes != 1:
        raise SystemExit("ISJ: the bracket [%s, %s] holds %d sign changes, not one"
                         % (mp.nstr(tlo, 5), mp.nstr(b, 5), changes))
    t = findroot(F, (prev, b), solver="anderson", tol=mpf(10) ** -40)
    return mpsqrt(t) * span, "found"


def isj_n_eff(ws, weighted, weight_type):
    """`n_eff` as the rules use it: the population's size, `sum(w)` or Kish's."""
    if not weighted:
        return mpf(len(ws))
    if weight_type == "frequency":
        return sum(ws)
    return sum(ws) ** 2 / sum(w * w for w in ws)


# ======================================================================================
# The kernels, in standard-deviation units
# ======================================================================================

#: The constants below at each working precision they have been asked for; the adaptive cases'
#: quadrature evaluates the kernels millions of times, at a precision of its own.
_CONST = {}


def _const(name):
    key = (name, mp.prec)
    if key not in _CONST:
        _CONST[key] = {"sqrt2": lambda: mpsqrt(2), "sqrt3": lambda: mpsqrt(3),
                       "sqrt5": lambda: mpsqrt(5), "sqrt2pi": lambda: mpsqrt(2 * pi),
                       "gmass": lambda: erf(CUT / mpsqrt(2)),
                       # The three values the cut Gaussian's moments read AT THE CUT, which every
                       # one of the millions of calls below would otherwise re-evaluate: each is
                       # spelled as the expression it replaces, so the arithmetic is unchanged.
                       "phicut": lambda: phi(CUT), "bigphicut": lambda: big_phi(-CUT),
                       "gvar": lambda: 1 - 2 * CUT * phi(CUT) / gauss_mass()}[name]()
    return _CONST[key]


def scale(kernel):
    """The kernel's scale per unit standard deviation."""
    if kernel == "gaussian":
        return mpf(1)
    return _const("sqrt5") if kernel == "epanechnikov" else _const("sqrt3")


def phi(z):
    return exp(-z * z / 2) / _const("sqrt2pi")


def big_phi(z):
    return erfc(-z / _const("sqrt2")) / 2


def gauss_mass():
    return _const("gmass")


def kernel_pdf(kernel, z):
    """The kernel's density at `z` standard deviations, integrating to one over `z`."""
    c = scale(kernel)
    if kernel == "gaussian":
        return phi(z) / gauss_mass() if abs(z) <= CUT else mpf(0)
    u = abs(z) / c
    if kernel == "epanechnikov":
        return mpf(3) / 4 * (1 - u * u) / c if u < 1 else mpf(0)
    if kernel == "bspline":
        if u < 1:
            return (mpf(2) / 3 - u * u + u ** 3 / 2) / c
        return (2 - u) ** 3 / 6 / c if u < 2 else mpf(0)
    if kernel == "box":
        return mpf(1) / 2 / c if u <= 1 else mpf(0)
    raise SystemExit("unknown kernel %r" % kernel)


def kernel_m1(kernel, z):
    """The integral of `u K(u)` from minus the support radius to `z` standard deviations.

    `u K(u)` is odd, so this is even in `z`, and zero over the whole support. The closed forms are
    the library's (`kde_kernel_m1`, src/parquet_kde_core.f90); `--self-test` asserts each against
    quadrature at 50 digits.
    """
    c = scale(kernel)
    r = CUT if kernel == "gaussian" else radius(kernel)
    t = -abs(z)
    if t <= -r:
        return mpf(0)
    if kernel == "gaussian":
        return (_const("phicut") - phi(t)) / gauss_mass()
    v = t / c
    if kernel == "epanechnikov":
        return -(3 * c / 16) * (1 - v * v) ** 2
    if kernel == "box":
        return c * (v * v - 1) / 4
    if kernel == "bspline":
        if v <= -1:
            w = 2 + v
            return c * w ** 4 * (2 * w - 5) / 60
        return c * (v * v / 3 - v ** 4 / 4 - v ** 5 / 10 - mpf(7) / 30)
    raise SystemExit("unknown kernel %r" % kernel)


def kernel_m2(kernel, z):
    """The integral of `u**2 K(u)` from minus the support radius to `z` standard deviations.

    `u**2 K(u)` is even, so the moment above zero is the kernel's variance less the moment below
    `-z`; the variance is one but for the cut Gaussian, whose cut removes tail mass.
    """
    c = scale(kernel)
    r = CUT if kernel == "gaussian" else radius(kernel)
    var = _const("gvar") if kernel == "gaussian" else mpf(1)
    t = -abs(z)
    lower = mpf(0)
    if t > -r:
        if kernel == "gaussian":
            lower = ((big_phi(t) - _const("bigphicut")) - t * phi(t)
                     - CUT * _const("phicut")) / gauss_mass()
        else:
            v = t / c
            if kernel == "epanechnikov":
                lower = (1 + v) ** 2 * (2 - 4 * v + 6 * v * v - 3 * v ** 3) / 4
            elif kernel == "box":
                lower = (1 + v) * (1 - v + v * v) / 2
            elif kernel == "bspline":
                if v <= -1:
                    w = 2 + v
                    lower = w ** 4 * (5 * w * w - 24 * w + 30) / 60
                else:
                    lower = mpf(1) / 2 + 2 * v ** 3 / 3 - 3 * v ** 5 / 5 - v ** 6 / 4
            else:
                raise SystemExit("unknown kernel %r" % kernel)
    return var - lower if z > 0 else lower


def radius(kernel):
    """The kernel's support radius in standard deviations."""
    if kernel == "gaussian":
        return CUT
    return scale(kernel) * (2 if kernel == "bspline" else 1)


def kernel_cdf(kernel, z):
    """The kernel's mass at or below `z` standard deviations."""
    if kernel == "gaussian":
        if z <= -CUT:
            return mpf(0)
        if z >= CUT:
            return mpf(1)
        return (big_phi(z) - _const("bigphicut")) / gauss_mass()
    u = z / scale(kernel)
    if kernel == "epanechnikov":
        if u <= -1:
            return mpf(0)
        if u >= 1:
            return mpf(1)
        return mpf(1) / 2 + mpf(3) / 4 * u - u ** 3 / 4
    if kernel == "bspline":
        if u <= -2:
            return mpf(0)
        if u >= 2:
            return mpf(1)
        if u <= -1:
            return (2 + u) ** 4 / 24
        if u <= 0:
            return mpf(1) / 2 + 2 * u / 3 - u ** 3 / 3 - u ** 4 / 8
        if u <= 1:
            return mpf(1) / 2 + 2 * u / 3 - u ** 3 / 3 + u ** 4 / 8
        return 1 - (2 - u) ** 4 / 24
    if kernel == "box":
        if u <= -1:
            return mpf(0)
        if u >= 1:
            return mpf(1)
        return (1 + u) / 2
    raise SystemExit("unknown kernel %r" % kernel)


# ======================================================================================
# The estimate
# ======================================================================================

def population(values, weights, lower, upper):
    """The family's exclusions, then the support: `(xs, ws, n_outside)`."""
    keep, _, _ = gsv.select(values, weights=weights)
    xs, ws, n_out = [], [], 0
    for x, w in keep:
        if (lower is not None and x < lower) or (upper is not None and x > upper):
            n_out += 1
            continue
        xs.append(x)
        ws.append(w)
    return xs, ws, n_out


def rule_bandwidth(rule, xs, ws, weighted, weight_type):
    """Silverman's or Scott's rule over the population, as `pf_kde%fit` forms it."""
    m = gsv.model(xs, weights=ws if weighted else None, weight_type=weight_type)
    s = m["stddev"]
    if s is None or s <= 0:
        return None
    pairs = sorted(zip(xs, ws), key=lambda t: t[0])
    sx = [x for x, _ in pairs]
    sw = [w for _, w in pairs]
    method = "inverted_cdf" if (weighted and weight_type == "frequency") else "linear"
    iqr = (gsv.quantile_at(sx, sw, mpf("0.75"), method, weighted)
           - gsv.quantile_at(sx, sw, mpf("0.25"), method, weighted))
    a = min(s, iqr / mpf("1.349")) if iqr > 0 else s
    if not weighted:
        n_eff = mpf(len(xs))
    elif weight_type == "frequency":
        n_eff = sum(ws)
    else:
        n_eff = sum(ws) ** 2 / sum(w * w for w in ws)
    # Silverman's 0.9, and Scott's exact (4/3)**(1/5) -- the fifth root, not a rounded decimal, so
    # that the oracle carries the rule at fifty digits and the library's own folded constant is what
    # a golden case compares against.
    c = mpf("0.9") if rule == "silverman" else (mpf(4) / 3) ** (mpf(1) / 5)
    return c * a * n_eff ** (mpf(-1) / 5)


def images(x, boundary, lower, upper):
    """The centres a point's kernel is summed over: itself, and its mirrors under reflection."""
    out = [x]
    if boundary == "reflect":
        if lower is not None:
            out.append(2 * lower - x)
        if upper is not None:
            out.append(2 * upper - x)
    return out


#: Where each kernel's density has a kink, a jump or its cut, in standard deviations: the
#: breakpoints the pilot's entropy integral is split at.
KNOTS = {"gaussian": [-CUT, CUT], "epanechnikov": [-mpsqrt(5), mpsqrt(5)],
         "bspline": [-2 * mpsqrt(3), -mpsqrt(3), mpf(0), mpsqrt(3), 2 * mpsqrt(3)],
         "box": [-mpsqrt(3), mpsqrt(3)]}


class Estimate:
    """A kernel estimate over points `xs` with weights `ws`, point `j` of bandwidth `hs[j]`."""

    def __init__(self, xs, ws, hs, kernel, boundary, lo, hi):
        self.xs, self.ws, self.hs = xs, ws, hs
        self.kernel, self.boundary, self.lo, self.hi = kernel, boundary, lo, hi
        self.W = sum(ws)
        self.masses = [self.mass(x, h) for x, h in zip(xs, hs)]
        # Each point's images with their reach, in doubles and widened by a margin, so that `pdf`
        # skips the kernels that are exactly zero at `t` without evaluating them. A skip test
        # only: every kernel that is summed is summed at full precision.
        radius = float(CUT) if kernel == "gaussian" else float(KNOTS[kernel][-1])
        self.reach = [[(float(c), 1.000001 * radius * float(h) + 1e-300)
                       for c in images(x, boundary, lo, hi)] for x, h in zip(xs, hs)]

    def s_of(self, x, h, t):
        return sum(kernel_cdf(self.kernel, (t - c) / h)
                   for c in images(x, self.boundary, self.lo, self.hi))

    def s_lower(self, x, h):
        return self.s_of(x, h, self.lo) if self.lo is not None else mpf(0)

    def mass(self, x, h):
        top = (self.s_of(x, h, self.hi) if self.hi is not None
               else mpf(len(images(x, self.boundary, self.lo, self.hi))))
        return top - self.s_lower(x, h)

    def pdf(self, t):
        if (self.lo is not None and t < self.lo) or (self.hi is not None and t > self.hi):
            return mpf(0)
        acc = mpf(0)
        tf = float(t)
        for x, w, h, mj, reach in zip(self.xs, self.ws, self.hs, self.masses, self.reach):
            if all(abs(tf - cf) > rf for cf, rf in reach):
                continue
            acc += w * sum(kernel_pdf(self.kernel, (t - c) / h)
                           for c in images(x, self.boundary, self.lo, self.hi)) / (h * mj)
        return acc / self.W

    def cdf(self, t):
        if self.lo is not None and t <= self.lo:
            return mpf(0)
        if self.hi is not None and t >= self.hi:
            return mpf(1)
        acc = mpf(0)
        for x, w, h, mj in zip(self.xs, self.ws, self.hs, self.masses):
            acc += w * (self.s_of(x, h, t) - self.s_lower(x, h)) / mj
        return acc / self.W


def quad_smooth(f, interval):
    """`quad` over an interval the integrand is ANALYTIC on, by Gauss-Legendre.

    `PolyEstimate.prepare` cuts the estimate at every kernel knot, correction edge and sign
    change, so no interval it integrates carries a kink or an endpoint singularity -- the case
    Gauss-Legendre converges geometrically on, at a fraction of the default tanh-sinh's
    evaluations. The entropy integrals of `pilot_log_g` and `poly_log_g` keep the default:
    `p log p` has an infinite derivative wherever the density vanishes, which is the endpoint
    behaviour tanh-sinh exists for.
    """
    return quad(f, interval, method="gauss-legendre")


def quad_entropy(f, cuts, dens):
    """The integral of `p log p` over `cuts`, interval by interval, each by the rule that suits it.

    `p` is analytic and strictly positive across a whole interval whenever it is positive at both
    of its ends -- a sum of non-negative kernels vanishes only over a stretch the knots bound, so
    it has no isolated interior zero -- and there `quad_smooth` applies. Where `p` vanishes at an
    end, `p log p` has an infinite derivative there and the default tanh-sinh is kept.
    """
    at = [dens(t) > 0 for t in cuts]
    return sum((quad_smooth(f, [cuts[i], cuts[i + 1]]) if at[i] and at[i + 1]
                else quad(f, [cuts[i], cuts[i + 1]])) for i in range(len(cuts) - 1))


class PolyEstimate:
    """A local-polynomial boundary estimate over points `xs`, weights `ws`, bandwidths `hs`.

    At a query `x` each point's kernel is replaced by `(a_2 - a_1 u) K(u)/D`, `u = (x - x_j)/h_j`
    and `a_l` the kernel's moments over the part of its support inside the bounds, `D = a_0 a_2 -
    a_1**2`. The moments are the closed forms `kernel_cdf`, `kernel_m1` and `kernel_m2`, formed as
    plain differences at the working precision -- which is why the narrow cases are a check of the
    library's centred form rather than a second copy of it.

    `deg` picks the member: 1 is `"linear"`, the form above; 0 is `"renormalise"`, the kernel over
    `a_0` alone, which is the same expression with `a_1 = 0`, `a_2 = 1` and `D = a_0`.

    `raw` is that sum, `clipped` it with its negative part set to zero, and `pdf` the clipped sum
    over its own integral `Z`. Nothing here is decomposed point by point the way the library's
    distribution function is: `Z` and every probe's CDF are quadratures of the SUM, between the
    kernels' knots, the correction edges, the sign changes and the probes.
    """

    def __init__(self, xs, ws, hs, kernel, lo, hi, deg=1):
        self.xs, self.ws, self.hs = xs, ws, hs
        self.kernel, self.lo, self.hi, self.deg = kernel, lo, hi, deg
        self.W = sum(ws)
        self.rad = radius(kernel)
        self._nodes = None
        self._cum = None
        # `(a_1, a_2, D)` over the kernel's WHOLE support, per working precision: what `factor`
        # answers wherever neither bound clips the kernel, which is most of the support and every
        # point of an unbounded side. Keyed on the precision because the pilot's entropy integral
        # runs at `G_DPS` (`poly_log_g`).
        self._whole = {}
        # Each point's centre and reach in doubles, widened by a margin, so that `raw` skips the
        # points whose kernel is exactly zero at `x` without dividing at all. A skip test only:
        # every point that survives it is tested again, and summed, at full precision.
        radf = 1.000001 * float(self.rad)
        self._skip = [(float(x), radf * float(h) + 1e-300) for x, h in zip(xs, hs)]

    def moments(self, lo_z, hi_z):
        """`(a_1, a_2, D)` from the kernel's moments between `lo_z` and `hi_z`."""
        a0 = kernel_cdf(self.kernel, hi_z) - kernel_cdf(self.kernel, lo_z)
        if self.deg == 0:
            return (mpf(0), mpf(1), a0)
        a1 = kernel_m1(self.kernel, hi_z) - kernel_m1(self.kernel, lo_z)
        a2 = kernel_m2(self.kernel, hi_z) - kernel_m2(self.kernel, lo_z)
        return (a1, a2, a0 * a2 - a1 * a1)

    def factor(self, x, h):
        """`(a_1, a_2, D)` at `x` for bandwidth `h`."""
        lo_z, hi_z = -self.rad, self.rad
        if self.lo is not None:
            hi_z = min(hi_z, (x - self.lo) / h)
        if self.hi is not None:
            lo_z = max(lo_z, -(self.hi - x) / h)
        if lo_z == -self.rad and hi_z == self.rad:
            if mp.prec not in self._whole:
                self._whole[mp.prec] = self.moments(lo_z, hi_z)
            return self._whole[mp.prec]
        return self.moments(lo_z, hi_z)

    def raw(self, x):
        """The linear estimate before the clip and before `Z`; zero outside the support.

        The moments are taken per bandwidth that a point in reach of `x` actually carries, and
        cached for the rest of this sum: with the adaptive kernel every point has a bandwidth of
        its own, and forming all of them at every node of every quadrature is what dominates.
        """
        if (self.lo is not None and x < self.lo) or (self.hi is not None and x > self.hi):
            return mpf(0)
        fac = {}
        acc = mpf(0)
        tf = float(x)
        for (xf, rf), xj, w, h in zip(self._skip, self.xs, self.ws, self.hs):
            if abs(tf - xf) > rf:
                continue
            u = (x - xj) / h
            if abs(u) > self.rad:
                continue
            got = fac.get(h)
            if got is None:
                got = fac[h] = self.factor(x, h)
            a1, a2, d = got
            acc += w * (a2 - a1 * u) * kernel_pdf(self.kernel, u) / (d * h)
        return acc / self.W

    def clipped(self, x):
        v = self.raw(x)
        return v if v > 0 else mpf(0)

    def extent(self):
        """The support, clipped to where the kernels reach."""
        a = min(x - self.rad * h for x, h in zip(self.xs, self.hs))
        b = max(x + self.rad * h for x, h in zip(self.xs, self.hs))
        if self.lo is not None:
            a = max(a, self.lo)
        if self.hi is not None:
            b = min(b, self.hi)
        return a, b

    def edges(self, a, b):
        """Every point strictly inside `(a, b)` where the estimate is not smooth: each kernel's own
        knots, and the knots of each point's moments, which sit a knot's distance from a bound."""
        out = set()
        for xj, h in zip(self.xs, self.hs):
            for k in KNOTS[self.kernel]:
                for t in (xj + k * h, ):
                    if a < t < b:
                        out.add(t)
                if self.lo is not None and k > 0:
                    t = self.lo + k * h
                    if a < t < b:
                        out.add(t)
                if self.hi is not None and k > 0:
                    t = self.hi - k * h
                    if a < t < b:
                        out.add(t)
        return out

    def crossings(self, a, b, per_h):
        """Where the raw estimate changes sign, from a scan finer than the library's own.

        The scan runs between the edges, at `per_h` points to the narrowest bandwidth, so that no
        stretch the library can find is missed here; each sign change is then bisected. A jump
        (the box kernel's edges) is a node of the scan, so a sign change across one is found too.
        """
        if self.deg == 0:
            return []          # a kernel over a positive mass: never negative, nothing to cross
        cuts = sorted({a, b} | self.edges(a, b))
        step = min(self.hs) / per_h
        out = []
        for u, v in zip(cuts, cuts[1:]):
            n = int((v - u) / step) + 2
            n = min(n, 4096)
            prev_t = u + (v - u) / (2 * n)
            prev = self.raw(prev_t)
            for i in range(1, n + 1):
                t = u + (v - u) * (2 * i + 1) / (2 * n) if i < n else v - (v - u) / (2 * n)
                cur = self.raw(t)
                if (prev < 0) != (cur < 0):
                    out.append(mp.findroot(self.raw, (prev_t, t), solver="bisect",
                                           tol=mpf(10) ** -40, maxsteps=400))
                prev_t, prev = t, cur
        return out

    def prepare(self, probes, per_h=128):
        """The nodes the mass is integrated between, and the clipped mass up to each."""
        a, b = self.extent()
        nodes = {a, b} | self.edges(a, b)
        nodes |= {t for t in self.crossings(a, b, per_h) if a < t < b}
        nodes |= {mpf(t) for t in probes if a < mpf(t) < b}
        self._nodes = sorted(nodes)
        self._cum = [mpf(0)]
        for u, v in zip(self._nodes, self._nodes[1:]):
            self._cum.append(self._cum[-1] + quad_smooth(self.clipped, [u, v]))
        return self._cum[-1]

    def mass_to(self, t):
        """The clipped estimate's mass from the support's start up to `t`."""
        a, b = self._nodes[0], self._nodes[-1]
        if t <= a:
            return mpf(0)
        if t >= b:
            return self._cum[-1]
        i = max(i for i, v in enumerate(self._nodes) if v <= t)
        if t == self._nodes[i]:
            return self._cum[i]                    # every probe is a node: an empty integral
        return self._cum[i] + quad_smooth(self.clipped, [self._nodes[i], t])


def pilot_log_g(pilot, a, b, dps):
    """`log g`: the mean of `log p` over the pilot's own density on its range `[a, b]`.

    The numerator is integrated between the kernels' knots, where the density is smooth, at
    `dps` digits; the denominator, the mass on the range, is the distribution function's
    difference, exactly.
    """
    cuts = {a, b}
    for x, h in zip(pilot.xs, pilot.hs):
        for c in images(x, pilot.boundary, pilot.lo, pilot.hi):
            for k in KNOTS[pilot.kernel]:
                t = c + k * h
                if a < t < b:
                    cuts.add(t)
    cuts = sorted(cuts)
    den = pilot.cdf(b) - pilot.cdf(a)
    saved = mp.dps
    mp.dps = dps
    try:
        def plogp(t):
            v = pilot.pdf(t)
            return v * mp.log(v) if v > 0 else mpf(0)
        num = quad_entropy(plogp, cuts, pilot.pdf)
    finally:
        mp.dps = saved
    return num / den


def poly_log_g(lin, z, a, b, dps):
    """`log g` for a corrected pilot: the mean of `log p` over the pilot's own density on `[a, b]`,
    `p` the CLIPPED estimate over its whole mass -- which is what the library's pilot grid holds
    there, cell by cell, under this correction.
    """
    cuts = sorted({a, b} | {t for t in lin._nodes if a < t < b})
    saved = mp.dps
    mp.dps = dps
    try:
        def plogp(t):
            v = lin.clipped(t) / z
            return v * mp.log(v) if v > 0 else mpf(0)
        num = quad_entropy(plogp, cuts, lin.clipped)
    finally:
        mp.dps = saved
    den = (lin.mass_to(b) - lin.mass_to(a)) / z
    return num / den


def estimate(case):
    """`(defined, h, [pdf at PROBES], [cdf at PROBES])` for one case, summing every kernel."""
    n = case.get("n", 32)
    if case.get("fixture") == "two":
        values = two_component(n)
    elif case.get("fixture") == "two_rounded8":
        values = two_component_rounded(n, 8.0)
    else:
        values = gsv.fixture(n)
    weights = gsv.weights_mod5(n) if case.get("weights") == "mod5" else None
    weight_type = case.get("weight_type", "reliability")
    kernel = case.get("kernel", "bspline")
    lower = case.get("lower")
    upper = case.get("upper")
    lo = mpf(lower) if lower is not None else None
    hi = mpf(upper) if upper is not None else None
    boundary = case.get("boundary", "reflect" if (lower is not None or upper is not None)
                        else "none")
    xs, ws, _ = population(values, weights, lo, hi)
    if not xs:
        return False, None, None, None
    if "bandwidth" in case:
        h = mpf(case["bandwidth"])
    elif case["rule"] == "isj":
        h, _ = isj_bandwidth(xs, ws, isj_n_eff(ws, weights is not None, weight_type), lo, hi)
        if h is None:
            return False, None, None, None
    else:
        h = rule_bandwidth(case["rule"], xs, ws, weights is not None, weight_type)
        if h is None:
            return False, None, None, None
    if case.get("adaptive") and "bandwidth" not in case:
        # A rule answers the question the FIXED estimator asks; the adaptive kernel's own best
        # global bandwidth is larger. Applied only where the scale came from a rule, so that an
        # explicit `bandwidth=` means exactly what it says, and before `adjust`, which composes.
        h *= ADAPT_INFLATE ** (2 * mpf(case.get("alpha", 0.5)))
    h *= mpf(case.get("adjust", 1.0))
    hs = [h] * len(xs)
    corrected = boundary in ("renormalise", "linear")
    deg = 1 if boundary == "linear" else 0
    if case.get("adaptive") and corrected:
        # The pilot is the CLIPPED corrected estimate at the global bandwidth, over its own mass,
        # which is what the library's pilot grid holds under either member of the family.
        pilot = PolyEstimate(xs, ws, hs, kernel, lo, hi, deg)
        zp = pilot.prepare(PROBES)
        a = min(xs) - PILOT_REACH * h
        b = max(xs) + PILOT_REACH * h
        if lo is not None:
            a = max(a, lo)
        if hi is not None:
            b = min(b, hi)
        if boundary == "linear":
            # R1 and R2, which `"linear"` alone carries: the pilot's range starts at the bound and,
            # with one bound, reaches at least a kernel's reach from it (7.3).
            if lo is not None:
                a = lo
            if hi is not None:
                b = hi
            if (lo is None) != (hi is None):
                rad = radius(kernel) * h
                if b - a < rad:
                    if lo is not None:
                        b = a + rad
                    else:
                        a = b - rad
        log_g = poly_log_g(pilot, zp, a, b, G_DPS)
        alpha = mpf(case.get("alpha", 0.5))
        cap = mpf(case["bandwidth_max"]) if "bandwidth_max" in case else None
        hs = []
        for x in xs:
            hj = h * mp.exp(-alpha * (mp.log(pilot.clipped(x) / zp) - log_g))
            hs.append(min(hj, cap) if cap is not None else hj)
        check_spread_below_cap(case, hs)
    elif case.get("adaptive"):
        # The pilot is the fixed estimate at the global bandwidth, read exactly at every point;
        # `log g` is its entropy over the range the library's pilot grid spans.
        pilot = Estimate(xs, ws, hs, kernel, boundary, lo, hi)
        a = min(xs) - PILOT_REACH * h
        b = max(xs) + PILOT_REACH * h
        if lo is not None:
            a = max(a, lo)
        if hi is not None:
            b = min(b, hi)
        log_g = pilot_log_g(pilot, a, b, G_DPS)
        alpha = mpf(case.get("alpha", 0.5))
        cap = mpf(case["bandwidth_max"]) if "bandwidth_max" in case else None
        hs = []
        for x in xs:
            hj = h * mp.exp(-alpha * (mp.log(pilot.pdf(x)) - log_g))
            hs.append(min(hj, cap) if cap is not None else hj)
        check_spread_below_cap(case, hs)
    if corrected:
        # The clipped estimate over its own integral, both by quadrature of the SUM between its
        # knots, its correction edges and its sign changes -- never point by point, which is how
        # the library does it, so that the two share no arithmetic.
        lin = PolyEstimate(xs, ws, hs, kernel, lo, hi, deg)
        if "negative_at" in case:
            # The case exists to show the clip acting AT a probe: assert the raw estimate is
            # negative there before emitting it, so that a case which stopped showing it fails here
            # rather than passing as an ordinary one.
            v = lin.raw(mpf(case["negative_at"]))
            if not v < 0:
                raise SystemExit("the raw linear estimate at %s is %s, not negative: this case no "
                                 "longer shows the clip" % (case["negative_at"], mp.nstr(v, 6)))
        z = lin.prepare(PROBES)
        pdf = [lin.clipped(mpf(t)) / z for t in PROBES]
        cdf = [lin.mass_to(mpf(t)) / z for t in PROBES]
        return True, h, pdf, cdf
    est = Estimate(xs, ws, hs, kernel, boundary, lo, hi)
    pdf = [est.pdf(mpf(t)) for t in PROBES]
    cdf = [est.cdf(mpf(t)) for t in PROBES]
    return True, h, pdf, cdf


#: One population and one configuration per case. The keys are `pf_kde%fit`'s own argument names,
#: plus `n` (the recipe's length, default 32), `fixture` (`"two"` for the two-component recipe,
#: `"two_rounded8"` for it rounded to multiples of 8), `weights = "mod5"` (`w(i) = mod(i, 5)`, so
#: every fifth weight is zero and removes its element) and, for a `"linear"` case that exists to
#: show the clip, `negative_at`, a probe the raw estimate must be negative at. Every case names its
#: rule or its bandwidth,
#: so that no expectation depends on the default. `test/test_kde.f90`'s `fit_golden_case` makes the
#: same call for each name; the `ISJ` cases run at `ISJ_CELLS`, in the serial suite.
CASES = [
    ("SILVERMAN", "Silverman's rule, the Gaussian kernel, unbounded", {"kernel": "gaussian", "rule": "silverman"}),
    ("SCOTT", "Scott's rule", {"kernel": "gaussian", "rule": "scott"}),
    ("ADJUST", "Silverman's rule times adjust = 1.5", {"kernel": "gaussian", "rule": "silverman", "adjust": 1.5}),
    ("N1000", "the recipe at n = 1000, Silverman's rule", {"kernel": "gaussian", "n": 1000, "rule": "silverman"}),
    ("GAUSS", "the Gaussian kernel at an explicit bandwidth of 60", {"kernel": "gaussian", "bandwidth": 60.0}),
    ("EPAN", "the Epanechnikov kernel at an explicit bandwidth of 60",
     {"bandwidth": 60.0, "kernel": "epanechnikov"}),
    ("BSPL", "the cubic B-spline at an explicit bandwidth of 60",
     {"bandwidth": 60.0, "kernel": "bspline"}),
    ("BOX", "the box kernel at an explicit bandwidth of 60", {"bandwidth": 60.0, "kernel": "box"}),
    ("WREL", "weights mod 5, reliability: Kish's n_eff in the rule",
     {"kernel": "gaussian", "rule": "silverman", "weights": "mod5"}),
    ("WFREQ", "weights mod 5, frequency: sum(w) in the rule, the inverted-CDF quartiles",
     {"kernel": "gaussian", "rule": "silverman", "weights": "mod5", "weight_type": "frequency"}),
    ("REN_LO", "renormalised at a lower bound, Gaussian, h = 60",
     {"kernel": "gaussian", "bandwidth": 60.0, "lower": -470.0, "boundary": "renormalise"}),
    ("REN_BOTH", "renormalised at both bounds, Epanechnikov, h = 60",
     {"bandwidth": 60.0, "kernel": "epanechnikov", "lower": -470.0, "upper": 460.0,
      "boundary": "renormalise"}),
    ("REF_LO", "reflected at a lower bound, B-spline, h = 60",
     {"bandwidth": 60.0, "kernel": "bspline", "lower": -470.0, "boundary": "reflect"}),
    ("REF_HI", "reflected at an upper bound, box, h = 60",
     {"bandwidth": 60.0, "kernel": "box", "upper": 460.0, "boundary": "reflect"}),
    ("REF_WIDE", "reflected at both bounds with the kernel wider than the range: the mass "
     "normalisation carries the doubly reflected terms",
     {"kernel": "gaussian", "bandwidth": 400.0, "lower": -470.0, "upper": 460.0, "boundary": "reflect"}),
    ("REN_WIDE", "renormalised at both bounds with the kernel wider than the range, box",
     {"bandwidth": 600.0, "kernel": "box", "lower": -470.0, "upper": 460.0,
      "boundary": "renormalise"}),
    ("RULE_BOUNDED", "Silverman's rule over the population inside the support: two points are "
     "outside [-400, 400] and leave the rule's sample too",
     {"kernel": "gaussian", "rule": "silverman", "lower": -400.0, "upper": 400.0}),
    ("ONE_H", "one point with an explicit bandwidth: a single bump", {"kernel": "gaussian", "n": 1, "bandwidth": 10.0}),
    ("ONE_RULE", "one point under a rule: no scale, so the estimate is undefined",
     {"kernel": "gaussian", "n": 1, "rule": "silverman"}),
    ("ADAPT", "the adaptive kernel over the two-component recipe: Silverman's rule, the Gaussian, "
     "alpha = 0.5", {"kernel": "gaussian", "fixture": "two", "n": 60, "adaptive": True, "rule": "silverman"}),
    ("ADAPT_CAP", "the adaptive kernel at alpha = 1 with every bandwidth capped at 120, the "
     "B-spline at an explicit bandwidth of 80, renormalised at a lower bound",
     {"fixture": "two", "n": 60, "adaptive": True, "alpha": 1.0, "bandwidth_max": 120.0,
      "bandwidth": 80.0, "kernel": "bspline", "lower": -340.0, "boundary": "renormalise"}),
    ("ADAPT_REF", "the adaptive kernel reflected at both bounds, Epanechnikov, h = 60",
     {"fixture": "two", "n": 60, "adaptive": True, "bandwidth": 60.0, "kernel": "epanechnikov",
      "lower": -340.0, "upper": 470.0, "boundary": "reflect"}),
    ("ADAPT_W", "the adaptive kernel under weights mod 5, reliability: the pilot is weighted too",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "adaptive": True, "weights": "mod5", "rule": "silverman"}),
    ("ISJ", "the ISJ rule over the two-component recipe, 1024 cells: the Gaussian, unbounded",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "rule": "isj"}),
    ("ISJ_WREL", "the ISJ rule under weights mod 5, reliability: Kish's n_eff, the binned mass weighted",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "rule": "isj", "weights": "mod5"}),
    ("ISJ_WFREQ", "the ISJ rule under weights mod 5, frequency: the replicated sample of 120 points "
     "at 48 values, whose repeated values the rule resolves -- its root lies at about 1.2 cells",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "rule": "isj", "weights": "mod5", "weight_type": "frequency"}),
    ("ISJ_BOUNDED", "the ISJ rule on a grid clipped to [-331.625, 470]: the lowest point sits on the "
     "lower bound, in the half cell below the first centre, and lands whole on it",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "rule": "isj", "lower": -331.625, "upper": 470.0}),
    ("ISJ_ROUNDED", "the ISJ rule over the recipe rounded to multiples of 8: the fixed point is not "
     "negative at one cell, so the rule finds no bandwidth and the estimate is undefined",
     {"kernel": "gaussian", "fixture": "two_rounded8", "n": 60, "rule": "isj"}),
    ("ISJ_NO_ROOT", "the ISJ rule over the recipe at n = 32: the fixed point is negative up to t = 1, "
     "so the rule finds no bandwidth and the estimate is undefined", {"kernel": "gaussian", "rule": "isj"}),
    ("LIN_LO", "the linear boundary kernel at a lower bound, Gaussian, h = 60",
     {"kernel": "gaussian", "bandwidth": 60.0, "lower": -470.0, "boundary": "linear"}),
    ("LIN_HI_BOX", "the linear kernel at an upper bound, box, h = 60: a kernel whose value jumps at "
     "its own edges and whose moments kink at the correction edge",
     {"bandwidth": 60.0, "kernel": "box", "upper": 460.0, "boundary": "linear"}),
    ("LIN_BOTH", "the linear kernel at both bounds, Epanechnikov, h = 60",
     {"bandwidth": 60.0, "kernel": "epanechnikov", "lower": -470.0, "upper": 460.0, "boundary": "linear"}),
    ("LIN_BSPL", "the linear kernel at a lower bound, cubic B-spline, h = 60: its kernel's knots and "
     "its moments' knots both fall inside the zone",
     {"bandwidth": 60.0, "kernel": "bspline", "lower": -470.0, "boundary": "linear"}),
    ("LIN_WIDE", "the linear kernel at h = 400 on [-470, 460]: the two zones meet, so every moment is "
     "two-sided and no part of the support is the plain sum",
     {"kernel": "gaussian", "bandwidth": 400.0, "lower": -470.0, "upper": 460.0, "boundary": "linear"}),
    ("LIN_W", "the linear kernel under weights mod 5, reliability, at a lower bound",
     {"kernel": "gaussian", "bandwidth": 60.0, "weights": "mod5", "lower": -470.0, "boundary": "linear"}),
    ("LIN_ZERO", "the linear kernel where the clip acts at a probe: the two-component recipe under a "
     "lower bound at -470, more than two bandwidths below its nearest point, so the raw estimate is "
     "negative at the probe -466 and the clipped one is exactly zero there",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "bandwidth": 60.0, "lower": -470.0, "boundary": "linear",
      "negative_at": -466.0}),
    ("LIN_NARROW3", "both bounds a thousandth of a bandwidth apart (h = 9.3e5 on [-470, 460]), "
     "Gaussian: where the library forms the moments centred on the interval's midpoint and this "
     "oracle takes the plain differences at fifty digits",
     {"kernel": "gaussian", "bandwidth": 930000.0, "lower": -470.0, "upper": 460.0, "boundary": "linear"}),
    ("ADAPT_LIN", "the adaptive kernel under the linear correction: the two-component recipe at a "
     "lower bound, alpha = 0.5, each point corrected at ITS OWN bandwidth and the pilot the clipped "
     "linear estimate",
     {"kernel": "gaussian", "fixture": "two", "n": 60, "adaptive": True, "bandwidth": 60.0, "lower": -470.0,
      "boundary": "linear"}),
    ("LIN_NARROW6", "both bounds a millionth of a bandwidth apart (h = 9.3e8), Epanechnikov: the "
     "estimate is the uniform density on the support to twelve digits",
     {"bandwidth": 930000000.0, "kernel": "epanechnikov", "lower": -470.0, "upper": 460.0,
      "boundary": "linear"}),
]


# ======================================================================================
# Emission
# ======================================================================================

HEADER = '''!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! GENERATED FILE -- DO NOT EDIT BY HAND.
! Regenerate with:  tools/generate_kde_vectors.py
!
!> Golden expectations for `parquet_kde`, derived at 50 decimal digits by `mpmath`.
!!
!! **The oracle deliberately shares no arithmetic with the library.** It sums every kernel of every
!! point over the fixture's exact rationals -- no window, no running weight sum -- and reads the
!! rules' scale from the same model `parquet_stats`' own golden file is generated from.
!!
!! Each case is one configuration of `pf_kde%fit` over the recipe's population. `KG_<name>_DEF`
!! says whether the estimate is defined; `KG_<name>_H` is the bandwidth, and `KG_<name>_PDF` and
!! `KG_<name>_CDF` the density and the distribution function at each of `KG_X`. An undefined case
!! carries zeros, and the test asserts NaN there instead.
!!
!! The population is a recipe rather than a literal -- see `kde_fixture` in test/test_kde.f90,
!! and `KG_PROBE`, which is what reports a recipe that has drifted.
module test_kde_golden
    use iso_fortran_env, only : real64
    implicit none
    public

    !> How many probe points each case is evaluated at.
    integer, parameter :: NKX = @NKX@
'''



# ======================================================================================
# The binned method's oracle
# ======================================================================================

BINNED_CASES = [
    ("BIN_GAUSS", "binned, the Gaussian kernel at h = 60, unbounded, 32 cells over [-600, 600]",
     {"kernel": "gaussian", "bandwidth": 60.0, "cells": 32, "xmin": -600.0, "xmax": 600.0}),
    ("BIN_EPAN", "binned, the Epanechnikov kernel at h = 60, unbounded: a compact kernel, whose "
     "sampled filter differs from the kernel's own transform by far more than rounding",
     {"bandwidth": 60.0, "kernel": "epanechnikov", "cells": 32, "xmin": -600.0, "xmax": 600.0}),
    ("BIN_BOX", "binned, the box kernel at h = 60, unbounded",
     {"bandwidth": 60.0, "kernel": "box", "cells": 32, "xmin": -600.0, "xmax": 600.0}),
    ("BIN_BSPL", "binned, the cubic B-spline at h = 60, unbounded",
     {"bandwidth": 60.0, "kernel": "bspline", "cells": 32, "xmin": -600.0, "xmax": 600.0}),
    ("BIN_REF", "binned and reflected at both bounds with the range EQUAL to the support and a "
     "power-of-two cell count: the unpadded path, where the cosine basis is the correction",
     {"kernel": "gaussian", "bandwidth": 60.0, "cells": 32, "xmin": -470.0, "xmax": 460.0, "lower": -470.0,
      "upper": 460.0, "boundary": "reflect"}),
    ("BIN_REF_IN", "binned and reflected with the range INSIDE the support: the padded path, "
     "where the images are gathered in explicitly",
     {"bandwidth": 60.0, "kernel": "epanechnikov", "cells": 30, "xmin": -400.0, "xmax": 400.0,
      "lower": -470.0, "upper": 460.0, "boundary": "reflect"}),
    ("BIN_REN", "binned and renormalised at a lower bound: the per-cell division by the mass a "
     "kernel centred there keeps inside the support",
     {"kernel": "gaussian", "bandwidth": 60.0, "cells": 32, "xmin": -470.0, "xmax": 460.0, "lower": -470.0,
      "boundary": "renormalise"}),
    ("BIN_LIN", "binned under the local linear correction at both bounds: the second convolution, "
     "against the odd kernel, read back through the inverse sine transform",
     {"kernel": "gaussian", "bandwidth": 60.0, "cells": 32, "xmin": -470.0, "xmax": 460.0, "lower": -470.0,
      "upper": 460.0, "boundary": "linear"}),
]


def next_pow2(n):
    """The smallest power of two at or above `n`."""
    p = 1
    while p < n:
        p *= 2
    return p


def binned_cells(case):
    """The density a binned grid holds in each cell, at full precision.

    The library reaches this through one cosine transform per bandwidth class, multiplying by the
    sampled filter's own multiplier; this sums the convolution directly instead, so the two share
    the definition and nothing else. The extension the transform implies -- half-sample even about
    each end of the array, period `2L` -- is written out here, because the unpadded reflecting case
    is exactly the case where it reaches the cells.
    """
    n = case.get("n", 32)
    values = two_component(n) if case.get("fixture") == "two" else gsv.fixture(n)
    weights = gsv.weights_mod5(n) if case.get("weights") == "mod5" else None
    kernel = case.get("kernel", "bspline")
    lower, upper = case.get("lower"), case.get("upper")
    lo = mpf(lower) if lower is not None else None
    hi = mpf(upper) if upper is not None else None
    boundary = case.get("boundary", "reflect" if (lower is not None or upper is not None)
                        else "none")
    xs, ws, _ = population(values, weights, lo, hi)
    if not xs:
        return None
    h = mpf(case["bandwidth"])
    nc = case["cells"]
    x0, x1 = mpf(case["xmin"]), mpf(case["xmax"])
    dx = (x1 - x0) / nc
    rad = radius(kernel)
    span = rad * h
    if boundary == "reflect":
        span *= 2
    aligned = (boundary == "reflect" and lo is not None and hi is not None
               and lo == x0 and hi == x1 and nc >= 2 and (nc & (nc - 1)) == 0)
    if aligned:
        pad, ntr = 0, nc
    else:
        pad = int(mp.ceil(span / dx)) + 1
        ntr = next_pow2(nc + 2 * pad)
    cb = [x0 + (mpf(k - pad) - mpf(1) / 2) * dx for k in range(1, ntr + 1)]

    # ---- linear binning, and what lies beyond the array altogether ----
    b = [mpf(0)] * ntr
    below, above = mpf(0), mpf(0)
    for x, w in zip(xs, ws):
        if x < cb[0]:
            if pad == 0:
                b[0] += w
            else:
                below += w
            continue
        if x > cb[-1]:
            if pad == 0:
                b[-1] += w
            else:
                above += w
            continue
        u = (x - cb[0]) / dx
        k = int(mp.floor(u))
        if k >= ntr - 1:
            k = ntr - 2
        f = u - k
        b[k] += w * (1 - f)
        b[k + 1] += w * f

    # ---- the sampled filter, normalised by its own mass ----
    taps = min(int(mp.floor(rad * h / dx)) + 2, ntr)
    hd = [dx * kernel_pdf(kernel, mpf(d) * dx / h) / h for d in range(0, taps + 1)]
    d0 = hd[0] + 2 * sum(hd[1:])
    hn = [t / d0 for t in hd]
    gn = [dx * (mpf(d) * dx / h) * kernel_pdf(kernel, mpf(d) * dx / h) / h / d0
          for d in range(0, taps + 1)]

    def bext(j):
        """`b` under the transform's own extension: half-sample even at both ends, period `2L`."""
        m = j % (2 * ntr)
        if m >= ntr:
            m = 2 * ntr - 1 - m
        return b[m]

    val = []
    val1 = []
    for i in range(ntr):
        s0 = mpf(0)
        s1 = mpf(0)
        for d in range(-taps, taps + 1):
            bj = bext(i - d)
            if bj == 0:
                continue
            s0 += bj * hn[abs(d)]
            s1 += bj * (gn[d] if d >= 0 else -gn[-d])
        val.append(s0)
        val1.append(s1)

    def outside(t):
        return (lo is not None and t < lo) or (hi is not None and t > hi)

    # ---- the per-cell correction ----
    if boundary in ("renormalise", "linear"):
        poly = PolyEstimate(xs, ws, [h] * len(xs), kernel, lo, hi,
                            1 if boundary == "linear" else 0)
        for k in range(ntr):
            if outside(cb[k]):
                val[k] = mpf(0)
                continue
            a1, a2, det = poly.factor(cb[k], h)
            val[k] = (a2 * val[k] - a1 * val1[k]) / det

    # ---- the reflecting images, gathered where the basis did not supply them ----
    if boundary == "reflect" and pad > 0:
        def gather(p):
            u = (p - cb[0]) / dx
            if u < 0 or u > ntr - 1:
                return mpf(0)
            k = int(mp.floor(u))
            if k >= ntr - 1:
                return val[ntr - 1]
            f = u - k
            return val[k] * (1 - f) + val[k + 1] * f
        fold = []
        for k in range(ntr):
            if outside(cb[k]):
                fold.append(mpf(0))
                continue
            v = val[k]
            if lo is not None:
                v += gather(2 * lo - cb[k])
            if hi is not None:
                v += gather(2 * hi - cb[k])
            fold.append(v)
        val = fold

    # ---- the cells, the two counters, and the mass the density is over ----
    acc = [val[pad + i] / dx for i in range(nc)]
    for k in range(pad):
        if not outside(cb[k]):
            below += val[k]
    for k in range(pad + nc, ntr):
        if not outside(cb[k]):
            above += val[k]
    corrected = boundary in ("renormalise", "linear")
    cells = [max(a, mpf(0)) if corrected else a for a in acc]
    if corrected:
        mass = dx * sum(cells) + below + above
    else:
        mass = sum(ws)
    if mass <= 0:
        return None
    return [c / mass for c in cells]


# ======================================================================================
# The cross-validation criterion's oracle
# ======================================================================================
#
# **The criterion is pinned here; the bandwidth `rule="lscv"` chooses is not, and that is a
# decision rather than a gap.** LSCV's answer is the minimiser of a search, so an oracle for it
# would have to reproduce the golden section, the bracket, the subsample's draw and
# `KDE_LSCV_TOL` -- it would pin the implementation rather than the mathematics, and every change
# to any of those four would move a vector that is not about them. The criterion itself is a
# closed form over the points, which is exactly what this file can certify honestly, so the
# criterion gets a vector and the minimiser is left to `test_lscv_rule` (which holds it to the
# closed-form normal-reference optimum within a factor of two, LSCV's own variability being wide)
# and `test_lscv_transform_agrees` (which holds the transform route and the pair sum to each
# other). A change that moves the criterion's VALUE fails here; a change that moves only where
# the search stops does not, by design.

#: The bandwidths every criterion case is read at. Deliberately not round against the fixtures'
#: own spacing: the criterion's normal density is EXACTLY zero beyond `LSCV_CUT` standard
#: deviations, so a pair landing on that cut would have its contribution decided by the last bit
#: of one division. `lscv_criterion` asserts the margin each case actually keeps.
LSCV_HS = [37.0, 53.0, 71.0, 97.0, 131.0, 179.0]

#: Where the criterion's normal density is taken as zero, in standard deviations: `KDE_NORM_CUT`
#: in `src/parquet_kde.f90`.
LSCV_CUT = mpf(5)

#: How near that cut any pair may come, in standard deviations. Ten orders of magnitude above the
#: `real64` rounding of the library's own `z/s`, so no case can be decided one way by the library
#: and the other way here.
LSCV_MARGIN = mpf("1e-6")


def lscv_normal(z, s):
    """The criterion's normal density, cut exactly where `kde_norm_at` cuts it."""
    if s <= 0:
        return mpf(0)
    t = z / s
    if abs(t) > LSCV_CUT:
        return mpf(0)
    return mp.exp(-t * t / 2) / (s * mp.sqrt(2 * mp.pi))


def lscv_criterion(case):
    """`[the criterion at each of LSCV_HS]` for one case, summed over every pair.

    `integral of fhat**2 - (2/n) sum_i fhat_{-i}(x_i)` for the FIXED estimator, both terms exact
    for the normal: the integral of a product of two normal densities is a normal density of the
    combined width at their separation, and the leave-one-out sum omits `i == j` -- the INDEX and
    not the value, which is what the repeated-value case is here to pin.

    The points are taken as given, with no exclusion pass: `parquet_debug_kde_lscv_at` takes them
    the same way, and a zero weight contributes nothing to either term or to `sum(w)`, so dropping
    one and keeping one are the same number.
    """
    n = case.get("n", 32)
    if case.get("fixture") == "two_rounded8":
        xs = two_component_rounded(n, 8.0)
    elif case.get("fixture") == "two":
        xs = two_component(n)
    else:
        xs = gsv.fixture(n)
    xs = [mpf(v) for v in xs]
    if case.get("weights") == "mod5":
        ws = [mpf(w) for w in gsv.weights_mod5(n)]
    else:
        ws = [mpf(1)] * len(xs)
    m = len(xs)
    sw = sum(ws)
    out = []
    for hv in LSCV_HS:
        h = mpf(hv)
        wide = mp.sqrt(h * h + h * h)
        # No pair may sit on the cut, for either of the two widths the criterion reads.
        for s in (wide, h):
            for i in range(m):
                for j in range(m):
                    d = abs(abs((xs[i] - xs[j]) / s) - LSCV_CUT)
                    if d < LSCV_MARGIN:
                        raise SystemExit(
                            "lscv_criterion: a pair sits %s from the %s-sigma cut at h = %s; "
                            "this case would be decided by rounding" % (mp.nstr(d, 4), LSCV_CUT, hv))
        s_int = mpf(0)
        s_loo = mpf(0)
        for i in range(m):
            for j in range(m):
                wij = ws[i] * ws[j]
                if wij == 0:
                    continue
                dz = xs[i] - xs[j]
                s_int += wij * lscv_normal(dz, wide)
                if i != j:
                    s_loo += wij * lscv_normal(dz, h)
        out.append(s_int / (sw * sw) - 2 * s_loo / (sw * (sw - mpf(1))))
    return out


#: One population per criterion case; the bandwidths are `LSCV_HS` for all of them.
#: `test/test_kde.f90`'s `test_lscv_criterion_golden` builds the same population and reads the
#: criterion back through `parquet_debug_kde_lscv_at`.
LSCV_CASES = [
    ("LSCV", "the criterion over the recipe at n = 32, unweighted: the normalisers, the combined "
     "width of the integral term and the `sum(w) - 1` of the leave-one-out term",
     {}),
    ("LSCV_W", "the criterion under weights mod 5: both terms weighted by the pair's product, the "
     "denominators by the weight total rather than the count",
     {"weights": "mod5"}),
    ("LSCV_DUP", "the criterion over the two-component recipe rounded to multiples of 8, whose "
     "values repeat: the leave-one-out term omits the point's own INDEX, so a coincident pair "
     "still contributes to it, which a term omitting equal VALUES would drop",
     {"fixture": "two_rounded8", "n": 60}),
]


def emit():
    """Return the whole generated file as text."""
    out = [HEADER.replace("@NKX@", str(len(PROBES)))]
    probe_n = 8
    out.append("    !> The first %d values `kde_fixture` must produce: the stats oracle's recipe."
               % probe_n)
    out += gsv.wrap_array("    real(real64), parameter :: KG_PROBE(%d) =" % probe_n,
                          [gsv.fortran_real(v) for v in gsv.fixture(probe_n)])
    out.append("")
    out.append("    !> The first %d values `kde_two_component` must produce: the adaptive cases' recipe."
               % probe_n)
    out += gsv.wrap_array("    real(real64), parameter :: KG_PROBE2(%d) =" % probe_n,
                          [gsv.fortran_real(v) for v in two_component(probe_n)])
    out.append("")
    out.append("    !> Where every case is probed.")
    out += gsv.wrap_array("    real(real64), parameter :: KG_X(NKX) =",
                          [gsv.fortran_real(v) for v in PROBES])
    for (name, doc, _), (defined, h, pdf, cdf) in zip(CASES, map_cases(
            estimate, [case for _, _, case in CASES])):
        out.append("")
        lines = textwrap.wrap(doc, width=124)
        out.append("    !> %s" % lines[0])
        out += ["    !! %s" % line for line in lines[1:]]
        out.append("    logical, parameter :: KG_%s_DEF = .%s." % (name, "true" if defined else "false"))
        out.append("    real(real64), parameter :: KG_%s_H = %s" % (name, gsv.fortran_real(h)))
        for tag, vals in (("PDF", pdf), ("CDF", cdf)):
            items = [gsv.fortran_real(v) for v in (vals or [None] * len(PROBES))]
            out += gsv.wrap_array("    real(real64), parameter :: KG_%s_%s(NKX) =" % (name, tag),
                                  items)
    for (name, doc, case), cells in zip(BINNED_CASES, map_cases(
            binned_cells, [case for _, _, case in BINNED_CASES])):
        out.append("")
        lines = textwrap.wrap(doc, width=124)
        out.append("    !> %s" % lines[0])
        out += ["    !! %s" % line for line in lines[1:]]
        out.append("    integer, parameter :: KG_%s_NC = %d" % (name, case["cells"]))
        out.append("    real(real64), parameter :: KG_%s_XMIN = %s"
                   % (name, gsv.fortran_real(mpf(case["xmin"]))))
        out.append("    real(real64), parameter :: KG_%s_XMAX = %s"
                   % (name, gsv.fortran_real(mpf(case["xmax"]))))
        out += gsv.wrap_array("    real(real64), parameter :: KG_%s_F(KG_%s_NC) =" % (name, name),
                              [gsv.fortran_real(v) for v in cells])
    out.append("")
    out.append("    !> How many bandwidths each cross-validation case is read at.")
    out.append("    integer, parameter :: NKH = %d" % len(LSCV_HS))
    out.append("")
    out.append("    !> The bandwidths every cross-validation case is read at.")
    out += gsv.wrap_array("    real(real64), parameter :: KG_LSCV_H(NKH) =",
                          [gsv.fortran_real(mpf(v)) for v in LSCV_HS])
    for (name, doc, _), crit in zip(LSCV_CASES, map_cases(
            lscv_criterion, [case for _, _, case in LSCV_CASES])):
        out.append("")
        lines = textwrap.wrap(doc, width=124)
        out.append("    !> %s" % lines[0])
        out += ["    !! %s" % line for line in lines[1:]]
        out += gsv.wrap_array("    real(real64), parameter :: KG_%s_CRIT(NKH) =" % name,
                              [gsv.fortran_real(v) for v in crit])
    out.append("")
    out.append("end module test_kde_golden ! GCOVR_EXCL_LINE")
    return "\n".join(out) + "\n"


# ======================================================================================
# --self-test
# ======================================================================================

KERNELS = ("gaussian", "epanechnikov", "bspline", "box")


def self_test():
    failures = []

    # The model's own definitions: unit mass, unit variance, and a distribution function that is
    # the integral of the density, for every kernel.
    mp.dps = 30
    for kernel in KERNELS:
        r = CUT if kernel == "gaussian" else {"epanechnikov": 1, "bspline": 2, "box": 1}[kernel] \
            * scale(kernel)
        # Split at every knot: the B-spline's pieces join at +-1 unit scales, and a quadrature
        # across a kink converges to nine digits, not thirty.
        c = scale(kernel)
        knots = [-r, -c, 0, c, r] if kernel == "bspline" else [-r, 0, r]
        mass = quad(lambda z: kernel_pdf(kernel, z), knots)
        var = quad(lambda z: z * z * kernel_pdf(kernel, z), knots)
        if abs(mass - 1) > mpf("1e-20"):
            failures.append("%s: mass %s" % (kernel, mass))
        # The cut Gaussian's variance is `1 - 10 phi(5)/erf(5/sqrt 2)`, about `1 - 1.5e-5`: the
        # cut removes tail mass, and the renormalisation cannot put that variance back.
        want_var = 1 - 2 * CUT * phi(CUT) / gauss_mass() if kernel == "gaussian" else mpf(1)
        if abs(var - want_var) > mpf("1e-20"):
            failures.append("%s: variance %s" % (kernel, var))
        for z in (-r * mpf("0.7"), mpf("-0.3"), mpf("0.45"), r * mpf("0.9")):
            want = quad(lambda t: kernel_pdf(kernel, t), [k for k in knots if k < z] + [z])
            if abs(kernel_cdf(kernel, z) - want) > mpf("1e-20"):
                failures.append("%s: cdf(%s) is not the integral of the density" % (kernel, z))
        # The truncated first and second moments of the linear boundary kernel, which the library
        # carries as closed forms of its own (`kde_kernel_m1`, `kde_kernel_m2`): each against the
        # same quadrature, split at every knot, at a dozen offsets across the support and beyond it.
        for z in (-r * mpf("1.3"), -r, -r * mpf("0.99"), -r * mpf("0.7"), -r * mpf("0.35"),
                  mpf("-0.3"), mpf(0), mpf("0.45"), r * mpf("0.35"), r * mpf("0.7"),
                  r * mpf("0.99"), r, r * mpf("1.3")):
            zc = max(-r, min(r, z))
            pts = [k for k in knots if k < zc] + [zc]
            for l, got in ((1, kernel_m1(kernel, z)), (2, kernel_m2(kernel, z))):
                want = quad(lambda t, l=l: t ** l * kernel_pdf(kernel, t), pts) if zc > -r else mpf(0)
                if abs(got - want) > mpf("1e-20"):
                    failures.append("%s: m%d(%s) is %s, quadrature %s"
                                    % (kernel, l, mp.nstr(z, 5), mp.nstr(got, 12), mp.nstr(want, 12)))
    mp.dps = 50
    print("self-test: kernel mass, variance, CDF and the two truncated moments checked for %d kernels"
          % len(KERNELS))

    # The pilot's entropy integral at the generator's working precision against 30 digits.
    xs2 = [mpf(v) for v in two_component(60)]
    h2 = mpf(90)
    pilot = Estimate(xs2, [mpf(1)] * 60, [h2] * 60, "gaussian", "none", None, None)
    a, b = min(xs2) - PILOT_REACH * h2, max(xs2) + PILOT_REACH * h2
    gap = abs(pilot_log_g(pilot, a, b, G_DPS) - pilot_log_g(pilot, a, b, 30))
    if gap > mpf("1e-15"):
        failures.append("log g at %d digits differs from 30 by %s" % (G_DPS, mp.nstr(gap, 3)))
    print("self-test: log g at %d digits agrees with 30 to %s" % (G_DPS, mp.nstr(gap, 3)))

    try:
        import numpy as np
        from scipy.stats import gaussian_kde
    except ImportError as exc:
        print("self-test: scipy is required (%s)" % exc, file=sys.stderr)
        return 1

    x = np.array(gsv.fixture(32))
    probes = np.array(PROBES)

    def model_pdf(kernel, h):
        _, _, pdf, _ = estimate({"bandwidth": h, "kernel": kernel})
        return np.array([float(v) for v in pdf])

    # scipy's Gaussian is not cut, so the two agree to the cut's share of the mass and no closer.
    h = 60.0
    kde = gaussian_kde(x, bw_method=h / np.std(x, ddof=1))
    got = kde(probes)
    want = model_pdf("gaussian", h)
    rel = np.max(np.abs(got - want) / np.maximum(want, 1e-300))
    if rel > 1e-5:
        failures.append("scipy gaussian_kde differs by %.3g relative" % rel)
    print("self-test: scipy gaussian_kde agrees to %.2g relative (the five-sd cut)" % rel)

    try:
        from KDEpy import NaiveKDE
    except ImportError:
        print("self-test: SKIPPED the KDEpy cross-check -- KDEpy does not import here")
    else:
        for kernel, name in (("epanechnikov", "epa"), ("box", "box")):
            got = NaiveKDE(kernel=name, bw=h).fit(x).evaluate(probes)
            want = model_pdf(kernel, h)
            err = np.max(np.abs(got - want))
            if err > 1e-15:
                failures.append("KDEpy %s differs by %.3g" % (name, err))
            print("self-test: KDEpy %s agrees to %.2g absolute" % (name, err))
        # The adaptive estimate is the sample-point one: each point's own kernel, of its own
        # standard deviation, which is what a per-point bandwidth array means there too. Given the
        # oracle's bandwidths, the two sums must agree to rounding.
        xs2 = [mpf(v) for v in two_component(60)]
        h2 = mpf(60)
        pilot = Estimate(xs2, [mpf(1)] * 60, [h2] * 60, "epanechnikov", "none", None, None)
        log_g = pilot_log_g(pilot, min(xs2) - PILOT_REACH * h2, max(xs2) + PILOT_REACH * h2, G_DPS)
        hs = [h2 * mp.exp(-mpf("0.5") * (mp.log(pilot.pdf(v)) - log_g)) for v in xs2]
        est = Estimate(xs2, [mpf(1)] * 60, hs, "epanechnikov", "none", None, None)
        want = np.array([float(est.pdf(mpf(t))) for t in PROBES])
        got = NaiveKDE(kernel="epa", bw=np.array([float(v) for v in hs])).fit(
            np.array(two_component(60))).evaluate(probes)
        err = np.max(np.abs(got - want))
        if err > 1e-15:
            failures.append("KDEpy's per-point bandwidths differ from the adaptive sum by %.3g" % err)
        print("self-test: KDEpy with per-point bandwidths agrees with the adaptive sum to %.2g" % err)

    # The linear correction's two defining behaviours, which no Python library here implements and
    # which a transcription error in the kernel cannot reproduce: on the rising density `2x` the
    # clipped estimate is essentially zero at the bound it vanishes at, and on a uniform -- where a
    # local linear fit is exact -- it is one at the bound to a thousandth.
    rising = [mpf(i) for i in range(1, 201)]
    rising = [mp.sqrt((v - mpf(1) / 2) / 200) for v in rising]
    h1 = rule_bandwidth("silverman", rising, [mpf(1)] * 200, False, "reliability")
    lin1 = PolyEstimate(rising, [mpf(1)] * 200, [h1] * 200, "gaussian", mpf(0), None)
    z1 = lin1.prepare([])
    f_at_bound = lin1.clipped(mpf(0)) / z1
    if not f_at_bound < mpf("1e-3"):
        failures.append("the linear estimate at a bound the density vanishes at is %s, not ~0"
                        % mp.nstr(f_at_bound, 5))
    print("self-test: on the density 2x the clipped linear estimate at the bound is %s" % mp.nstr(f_at_bound, 3))
    flat = [(mpf(i) - mpf(1) / 2) / 200 for i in range(1, 201)]
    h2 = rule_bandwidth("silverman", flat, [mpf(1)] * 200, False, "reliability")
    lin2 = PolyEstimate(flat, [mpf(1)] * 200, [h2] * 200, "gaussian", mpf(0), mpf(1))
    z2 = lin2.prepare([])
    f_flat = lin2.clipped(mpf(0)) / z2
    if abs(f_flat - 1) > mpf("1e-3"):
        failures.append("the linear estimate at a uniform's bound is %s, not 1" % mp.nstr(f_flat, 8))
    print("self-test: on a uniform the linear estimate at the bound is %s, against one" % mp.nstr(f_flat, 8))

    failures += isj_self_test()

    try:
        from statsmodels.nonparametric.kde import KDEUnivariate
    except ImportError:
        print("self-test: SKIPPED the statsmodels cross-check -- statsmodels does not import here")
    else:
        dens = KDEUnivariate(x)
        dens.fit(kernel="gau", bw=h, fft=False)
        got = dens.evaluate(probes)
        want = model_pdf("gaussian", h)
        rel = np.max(np.abs(got - want) / np.maximum(want, 1e-300))
        if rel > 1e-5:
            failures.append("statsmodels gau differs by %.3g relative" % rel)
        print("self-test: statsmodels gau agrees to %.2g relative (the five-sd cut)" % rel)

    for f in failures:
        print("self-test FAILED: %s" % f, file=sys.stderr)
    return 1 if failures else 0


def isj_float_pipeline(xs, ws, n_eff, lo, hi, cells):
    """The ISJ rule in doubles, assembled from other software's pieces on the library's grid.

    KDEpy's linear binning and its fixed-point function (`_fixed_point`, which carries the
    reference implementation's own worked values as doctests) and scipy's transform; the grid, the
    half-cell rule and the search are the library's, since those are the definitions under test.
    `_fixed_point` takes the squared coefficients `y_k**2` where the model squares `y_k/2`, and
    halves the norm's constant to match, so the two functions are the same function.
    """
    import numpy as np
    from scipy.fft import dct
    from scipy.optimize import brentq
    from KDEpy.binning import linear_binning
    from KDEpy.bw_selection import _fixed_point

    order = np.argsort(np.array([float(v) for v in xs]))
    x = np.array([float(xs[i]) for i in order])
    w = np.array([float(ws[i]) for i in order])
    span = x[-1] - x[0]
    a, b = x[0] - span / 10, x[-1] + span / 10
    if lo is not None:
        a = max(a, float(lo))
    if hi is not None:
        b = min(b, float(hi))
    dx = (b - a) / cells
    centres = a + (np.arange(cells) + 0.5) * dx
    mass = linear_binning(np.clip(x, centres[0], centres[-1]).reshape(-1, 1), centres, w)
    mass = mass / mass.sum()
    y = dct(mass)
    i_sq = np.arange(1, cells, dtype=float) ** 2
    a2 = y[1:] ** 2

    def fixed_point(t):
        # A norm that underflows to zero divides by it inside `_fixed_point`; its answer is then
        # `-inf`, the right sign, and the warning is noise.
        with np.errstate(divide="ignore", over="ignore"):
            return float(_fixed_point(t, float(n_eff), i_sq, a2))

    tlo = 1.0 / cells ** 2
    if not fixed_point(tlo) < 0:
        return None
    prev, hi_t = tlo, 2 * tlo
    while not fixed_point(hi_t) > 0:
        if hi_t >= 1:
            return None
        prev, hi_t = hi_t, min(tlo + 2 * (hi_t - tlo), 1.0)
    t = brentq(fixed_point, prev, hi_t, xtol=1e-300, rtol=4 * np.finfo(float).eps)
    return math.sqrt(t) * (b - a)


def isj_self_test():
    """The ISJ model against a pipeline built from KDEpy's and scipy's pieces; the failures found.

    KDEpy's own `improved_sheather_jones` is reported and not asserted: it widens its grid by at
    least six units of the data beyond each end and scales the answer by the data's range rather
    than the grid's, so its bandwidth depends on the data's units (the same sample in other units
    gives another bandwidth, after converting back) and falls well short of the asymptotically
    optimal one for a normal sample. Its fixed-point function and binning are sound, and they are
    what the pipeline uses.
    """
    failures = []
    try:
        import numpy as np
        from KDEpy.bw_selection import improved_sheather_jones
    except ImportError:
        print("self-test: SKIPPED the ISJ cross-check -- KDEpy does not import here")
        return failures

    xs = [mpf(v) for v in two_component(60)]
    ws = [mpf(1)] * 60
    # 1. The transcription: the model and the pipeline on the golden grid, every golden ISJ case.
    worst = 0.0
    for name, _, case in CASES:
        if case.get("rule") != "isj":
            continue
        n = case.get("n", 32)
        if case.get("fixture") == "two":
            values = two_component(n)
        elif case.get("fixture") == "two_rounded8":
            values = two_component_rounded(n, 8.0)
        else:
            values = gsv.fixture(n)
        weights = gsv.weights_mod5(n) if case.get("weights") == "mod5" else None
        lo = mpf(case["lower"]) if "lower" in case else None
        hi = mpf(case["upper"]) if "upper" in case else None
        cx, cw, _ = population(values, weights, lo, hi)
        n_eff = isj_n_eff(cw, weights is not None, case.get("weight_type", "reliability"))
        model, _ = isj_bandwidth(cx, cw, n_eff, lo, hi)
        pipe = isj_float_pipeline(cx, cw, n_eff, lo, hi, ISJ_CELLS)
        if (model is None) != (pipe is None):
            failures.append("ISJ %s: the model says %s and the pipeline %s"
                            % (name, model is not None, pipe is not None))
            continue
        if model is not None:
            worst = max(worst, abs(pipe - float(model)) / float(model))
    if worst > 1e-9:
        failures.append("ISJ: the pipeline differs from the model by %.3g relative" % worst)
    print("self-test: the ISJ model and the KDEpy/scipy pipeline agree on every golden case, to %.2g"
          % worst)

    # 2. The library's default grid: the rule at 2**14 cells within a per cent of the golden grid's.
    h14 = isj_float_pipeline(xs, ws, mpf(60), None, None, 2 ** 14)
    h10, _ = isj_bandwidth(xs, ws, mpf(60), None, None)
    gap = abs(h14 - float(h10)) / float(h10)
    if gap > 0.01:
        failures.append("ISJ: 2**14 cells differ from 2**10 by %.3g" % gap)
    print("self-test: the ISJ rule at 2**14 cells is within %.2g of 2**10 on the two-component recipe"
          % gap)

    # 3. What the rule is for: a large normal sample's bandwidth is the asymptotically optimal
    #    `(4/(3n))**(1/5)` standard deviations, to the few per cent a sample's own spread allows.
    rng = np.random.default_rng(20260919)
    normal = rng.standard_normal(100000)
    got = isj_float_pipeline(list(normal), [1.0] * len(normal), 100000, None, None, 2 ** 14)
    want = (4.0 / (3.0 * len(normal))) ** 0.2 * float(np.std(normal, ddof=1))
    if abs(got / want - 1) > 0.05:
        failures.append("ISJ: a normal sample's bandwidth is %.4g, not about %.4g" % (got, want))
    print("self-test: the ISJ rule on 1e5 normal points gives %.4g against the optimal %.4g" % (got, want))
    theirs = improved_sheather_jones(normal.reshape(-1, 1))
    scaled = improved_sheather_jones((1000 * normal).reshape(-1, 1)) / 1000
    print("self-test: INFO KDEpy's improved_sheather_jones gives %.4g, and %.4g for the same sample "
          "in units a thousand times smaller -- not a reference (see isj_self_test)" % (theirs, scaled))
    return failures


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
            print("generate_kde_vectors.py --check: %s is out of date. Re-run "
                  "tools/generate_kde_vectors.py." % OUT_PATH.relative_to(REPO_ROOT),
                  file=sys.stderr)
            return 1
        print("generate_kde_vectors.py --check: %s carries all %d cases, current."
              % (OUT_PATH.relative_to(REPO_ROOT), len(CASES)))
        return 0
    OUT_PATH.write_text(text)
    print("generate_kde_vectors.py: wrote %s (%d cases, mpmath at %d digits)"
          % (OUT_PATH.relative_to(REPO_ROOT), len(CASES), mp.dps))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
