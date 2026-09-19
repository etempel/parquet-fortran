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
and distribution function, both rules, `adjust`, both weight conventions, both boundary
corrections at each bound and at both, the case where the kernel is wider than the whole support
(which only the mass normalisation keeps at unit mass), the single-point degenerate cases, and the
adaptive kernel over a two-component recipe of its own.

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
KDEpy, so a KDEpy skip reported there is an environment fault.

Maintainer-only: it is never run at build time and is stripped from the fpm-published package.
"""

import sys
import textwrap
from pathlib import Path

try:
    from mpmath import mp, mpf, sqrt as mpsqrt, erf, erfc, exp, pi, quad
except ImportError:                                            # pragma: no cover - maintainer tool
    print("generate_kde_vectors.py: needs mpmath (pip install mpmath)", file=sys.stderr)
    raise SystemExit(2)

sys.path.insert(0, str(Path(__file__).resolve().parent))
import generate_stats_vectors as gsv                           # noqa: E402  the one population model

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
                       "gmass": lambda: erf(CUT / mpsqrt(2))}[name]()
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


def kernel_cdf(kernel, z):
    """The kernel's mass at or below `z` standard deviations."""
    if kernel == "gaussian":
        if z <= -CUT:
            return mpf(0)
        if z >= CUT:
            return mpf(1)
        return (big_phi(z) - big_phi(-CUT)) / gauss_mass()
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
    c = mpf("0.9") if rule == "silverman" else mpf("1.06")
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
        num = sum(quad(plogp, [cuts[i], cuts[i + 1]]) for i in range(len(cuts) - 1))
    finally:
        mp.dps = saved
    return num / den


def estimate(case):
    """`(defined, h, [pdf at PROBES], [cdf at PROBES])` for one case, summing every kernel."""
    n = case.get("n", 32)
    values = two_component(n) if case.get("fixture") == "two" else gsv.fixture(n)
    weights = gsv.weights_mod5(n) if case.get("weights") == "mod5" else None
    weight_type = case.get("weight_type", "reliability")
    kernel = case.get("kernel", "gaussian")
    lower = case.get("lower")
    upper = case.get("upper")
    lo = mpf(lower) if lower is not None else None
    hi = mpf(upper) if upper is not None else None
    boundary = case.get("boundary", "renormalise" if (lower is not None or upper is not None)
                        else "none")
    xs, ws, _ = population(values, weights, lo, hi)
    if not xs:
        return False, None, None, None
    if "bandwidth" in case:
        h = mpf(case["bandwidth"])
    else:
        h = rule_bandwidth(case.get("rule", "silverman"), xs, ws, weights is not None,
                           weight_type)
        if h is None:
            return False, None, None, None
    h *= mpf(case.get("adjust", 1.0))
    hs = [h] * len(xs)
    if case.get("adaptive"):
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
    est = Estimate(xs, ws, hs, kernel, boundary, lo, hi)
    pdf = [est.pdf(mpf(t)) for t in PROBES]
    cdf = [est.cdf(mpf(t)) for t in PROBES]
    return True, h, pdf, cdf


#: One population and one configuration per case. The keys are `pf_kde%fit`'s own argument names,
#: plus `n` (the recipe's length, default 32) and `weights = "mod5"` (`w(i) = mod(i, 5)`, so every
#: fifth weight is zero and removes its element). `test/test_kde.f90`'s `fit_golden_case` makes
#: the same call for each name.
CASES = [
    ("DEFAULT", "the defaults: Silverman's rule, the Gaussian kernel, unbounded", {}),
    ("SCOTT", "Scott's rule", {"rule": "scott"}),
    ("ADJUST", "Silverman's rule times adjust = 1.5", {"adjust": 1.5}),
    ("N1000", "the recipe at n = 1000, Silverman's rule", {"n": 1000}),
    ("GAUSS", "the Gaussian kernel at an explicit bandwidth of 60", {"bandwidth": 60.0}),
    ("EPAN", "the Epanechnikov kernel at an explicit bandwidth of 60",
     {"bandwidth": 60.0, "kernel": "epanechnikov"}),
    ("BSPL", "the cubic B-spline at an explicit bandwidth of 60",
     {"bandwidth": 60.0, "kernel": "bspline"}),
    ("BOX", "the box kernel at an explicit bandwidth of 60", {"bandwidth": 60.0, "kernel": "box"}),
    ("WREL", "weights mod 5, reliability: Kish's n_eff in the rule",
     {"weights": "mod5"}),
    ("WFREQ", "weights mod 5, frequency: sum(w) in the rule, the inverted-CDF quartiles",
     {"weights": "mod5", "weight_type": "frequency"}),
    ("REN_LO", "renormalised at a lower bound, Gaussian, h = 60",
     {"bandwidth": 60.0, "lower": -470.0}),
    ("REN_BOTH", "renormalised at both bounds, Epanechnikov, h = 60",
     {"bandwidth": 60.0, "kernel": "epanechnikov", "lower": -470.0, "upper": 460.0}),
    ("REF_LO", "reflected at a lower bound, B-spline, h = 60",
     {"bandwidth": 60.0, "kernel": "bspline", "lower": -470.0, "boundary": "reflect"}),
    ("REF_HI", "reflected at an upper bound, box, h = 60",
     {"bandwidth": 60.0, "kernel": "box", "upper": 460.0, "boundary": "reflect"}),
    ("REF_WIDE", "reflected at both bounds with the kernel wider than the range: the mass "
     "normalisation carries the doubly reflected terms",
     {"bandwidth": 400.0, "lower": -470.0, "upper": 460.0, "boundary": "reflect"}),
    ("REN_WIDE", "renormalised at both bounds with the kernel wider than the range, box",
     {"bandwidth": 600.0, "kernel": "box", "lower": -470.0, "upper": 460.0}),
    ("RULE_BOUNDED", "Silverman's rule over the population inside the support: two points are "
     "outside [-400, 400] and leave the rule's sample too",
     {"lower": -400.0, "upper": 400.0}),
    ("ONE_H", "one point with an explicit bandwidth: a single bump", {"n": 1, "bandwidth": 10.0}),
    ("ONE_RULE", "one point under a rule: no scale, so the estimate is undefined", {"n": 1}),
    ("ADAPT", "the adaptive kernel over the two-component recipe: Silverman's rule, the Gaussian, "
     "alpha = 0.5", {"fixture": "two", "n": 60, "adaptive": True}),
    ("ADAPT_CAP", "the adaptive kernel at alpha = 1 with every bandwidth capped at 120, the "
     "B-spline at an explicit bandwidth of 80, renormalised at a lower bound",
     {"fixture": "two", "n": 60, "adaptive": True, "alpha": 1.0, "bandwidth_max": 120.0,
      "bandwidth": 80.0, "kernel": "bspline", "lower": -340.0}),
    ("ADAPT_REF", "the adaptive kernel reflected at both bounds, Epanechnikov, h = 60",
     {"fixture": "two", "n": 60, "adaptive": True, "bandwidth": 60.0, "kernel": "epanechnikov",
      "lower": -340.0, "upper": 470.0, "boundary": "reflect"}),
    ("ADAPT_W", "the adaptive kernel under weights mod 5, reliability: the pilot is weighted too",
     {"fixture": "two", "n": 60, "adaptive": True, "weights": "mod5"}),
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
    for name, doc, case in CASES:
        defined, h, pdf, cdf = estimate(case)
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
    mp.dps = 50
    print("self-test: kernel mass, variance and CDF checked for %d kernels" % len(KERNELS))

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
