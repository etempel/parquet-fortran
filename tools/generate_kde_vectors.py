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
(which only the mass normalisation keeps at unit mass), and the single-point degenerate cases.

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


# ======================================================================================
# The kernels, in standard-deviation units
# ======================================================================================

def scale(kernel):
    """The kernel's scale per unit standard deviation."""
    return {"gaussian": mpf(1), "epanechnikov": mpsqrt(5), "bspline": mpsqrt(3),
            "box": mpsqrt(3)}[kernel]


def phi(z):
    return exp(-z * z / 2) / mpsqrt(2 * pi)


def big_phi(z):
    return erfc(-z / mpsqrt(2)) / 2


def gauss_mass():
    return erf(CUT / mpsqrt(2))


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


def estimate(case):
    """`(defined, h, [pdf at PROBES], [cdf at PROBES])` for one case, summing every kernel."""
    n = case.get("n", 32)
    values = gsv.fixture(n)
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
    W = sum(ws)

    def s_of(x, t):
        return sum(kernel_cdf(kernel, (t - c) / h) for c in images(x, boundary, lo, hi))

    def s_lower(x):
        return s_of(x, lo) if lo is not None else mpf(0)

    def mass(x):
        top = s_of(x, hi) if hi is not None else mpf(len(images(x, boundary, lo, hi)))
        return top - s_lower(x)

    masses = [mass(x) for x in xs]
    pdf, cdf = [], []
    for t in PROBES:
        t = mpf(t)
        if (lo is not None and t < lo) or (hi is not None and t > hi):
            pdf.append(mpf(0))
        else:
            acc = mpf(0)
            for x, w, mj in zip(xs, ws, masses):
                acc += w * sum(kernel_pdf(kernel, (t - c) / h)
                               for c in images(x, boundary, lo, hi)) / mj
            pdf.append(acc / (W * h))
        if lo is not None and t <= lo:
            cdf.append(mpf(0))
        elif hi is not None and t >= hi:
            cdf.append(mpf(1))
        else:
            acc = mpf(0)
            for x, w, mj in zip(xs, ws, masses):
                acc += w * (s_of(x, t) - s_lower(x)) / mj
            cdf.append(acc / W)
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
    out.append("    !> Where every case is probed.")
    out += gsv.wrap_array("    real(real64), parameter :: KG_X(NKX) =",
                          [gsv.fortran_real(v) for v in PROBES])
    for name, doc, case in CASES:
        defined, h, pdf, cdf = estimate(case)
        out.append("")
        out.append("    !> %s" % doc)
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
