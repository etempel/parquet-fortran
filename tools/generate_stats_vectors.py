#!/usr/bin/env python3
"""Golden expectations for `parquet_stats`, from a 50-digit oracle that shares no arithmetic.

`CLAUDE.md` records the lesson from `hpx_max_pixrad`: **an external reference computed the same
cancelling way cannot certify a cancellation-free implementation.** A statistics module's whole
accuracy claim is that it does not form `Sigma x**2 - (Sigma x)**2 / n`, and numpy computes several
of these quantities in ways that lose the same digits the Fortran is being asked not to lose. So
the committed expectations come from `mpmath` at 50 decimal digits over the fixture's EXACT
rational values, and numpy/pandas/scipy appear only in `--self-test`, as a cross-check that the
model transcribes the same definitions those libraries do.

**The fixtures are recipes, not literals.** Each is an integer sequence divided by a power of two,
so every value is exactly representable and Python and Fortran agree bit for bit without a
thousand-element literal table -- which would also run straight into the 255-continuation-line
limit `check_statement_continuation_lines` enforces. The Fortran side builds the fixture from the
same recipe and asserts a handful of emitted probe values, so a recipe that has drifted is reported
as a fixture mismatch rather than as a mysterious tolerance failure.

What each case pins is named in its own comment in the generated file. The set is chosen so that
every contract frozen by S4a has at least one case that fails when it is broken:

* shift invariance -- `u1000` against `shift`, the same population offset by 1e9;
* the `ddof` split, and `ddof >= n_valid` giving NaN rather than a division;
* the two `weight_type` conventions, which differ only when the weights are unequal;
* the zero-weight rule -- `wtvar` carries zero weights, so `n_valid` differs from `size(values)`;
* the exclusion ORDER -- `withnan` carries a NaN value whose weight is also NaN;
* the empty, all-null, single-element and all-identical degenerate cases.

Usage:
    python3 tools/generate_stats_vectors.py              # rewrite test/test_stats_golden.f90
    python3 tools/generate_stats_vectors.py --self-test  # validate the model against numpy/scipy
    python3 tools/generate_stats_vectors.py --check      # assert the committed file is current

`--self-test` needs numpy, scipy and pandas; plain generation and `--check` need only `mpmath`.
`--check` runs in CI's lint stage, which installs Ubuntu's `python3-mpmath` for exactly that.

**The emitted digits do not depend on the `mpmath` version, and that was measured rather than
assumed** -- 1.3.0 (what Ubuntu 24.04 packages) and 1.4.1 reproduce the committed file byte for
byte. They cannot easily differ: every operation here is `+`, `*`, `/` or `sqrt` at `mp.dps = 50`,
all correctly rounded, and the emitted literal is the round-to-nearest `float` of that. It is worth
knowing because the alternative -- a `--check` that fails on a machine whose oracle is a minor
version behind -- would look exactly like a real drift in the committed expectations.

Maintainer-only: it is never run at build time and is stripped from the fpm-published package.
"""

import sys
from pathlib import Path

try:
    from mpmath import mp, mpf, sqrt as mpsqrt
except ImportError:                                            # pragma: no cover - maintainer tool
    print("generate_stats_vectors.py: needs mpmath (pip install mpmath)", file=sys.stderr)
    raise SystemExit(2)

mp.dps = 50

REPO_ROOT = Path(__file__).resolve().parent.parent
OUT_PATH = REPO_ROOT / "test" / "test_stats_golden.f90"

#: The recipe every non-degenerate fixture uses. `a(i)` is exact in int64 and the division is by a
#: power of two, so `x(i)` is an exact double in both languages. Mirrored by `golden_fixture` in
#: test/test_stats.f90 -- change one and the emitted probe values report the other.
RECIPE_A, RECIPE_B, RECIPE_M = 7919, 12345, 1000003
RECIPE_OFFSET, RECIPE_SCALE = 500001, 1024.0


def fixture(n):
    """The `x(1:n)` a Fortran `golden_fixture(n, x)` call must produce, as exact doubles."""
    out = []
    for i in range(1, n + 1):
        a = (i * i * RECIPE_A + RECIPE_B) % RECIPE_M
        out.append((a - RECIPE_OFFSET) / RECIPE_SCALE)
    return out


NAN = float("nan")


def weights_mod5(n):
    """`w(i) = mod(i, 5)`, so every fifth weight is ZERO and removes its element."""
    return [float(i % 5) for i in range(1, n + 1)]


# ======================================================================================
# The model: the contract transcribed, at 50 digits, over exact rationals
# ======================================================================================

def select(values, is_valid=None, weights=None, skipnan=True):
    """Apply the family's exclusion order and return the surviving `(x, w)` pairs plus counts.

    Nullness, then NaN, then weight -- the order matters and is the one thing here with an
    observable consequence, since an element already out of the population never has its weight
    examined. `n_nan` counts only elements not already null; a zero weight is counted by neither.
    """
    keep, n_null, n_nan = [], 0, 0
    for i, v in enumerate(values):
        if is_valid is not None and not is_valid[i]:
            n_null += 1
            continue
        if skipnan and v != v:
            n_nan += 1
            continue
        w = 1.0 if weights is None else weights[i]
        if w != w or w < 0.0 or w == float("inf"):
            raise ValueError("weight %d is not finite and non-negative" % (i + 1))
        if w == 0.0:
            continue
        keep.append((mpf(v), mpf(w)))
    return keep, n_null, n_nan


def model(values, is_valid=None, weights=None, skipnan=True, weight_type="reliability",
          ddof=1, bias=False, excess=True):
    """Every tier-A quantity for one case, as `mpf`, plus the counts as plain ints.

    `None` marks a quantity that is undefined for this population -- the Fortran returns a quiet
    NaN there and sets `ok = .false.`, and the emitter turns `None` into that NaN.
    """
    keep, n_null, n_nan = select(values, is_valid, weights, skipnan)
    out = {"n_valid": len(keep), "n_null": n_null, "n_nan": n_nan}
    if not keep:
        # An empty population makes every statistic here undefined EXCEPT the sum, which is the
        # additive identity and is exactly what numpy and pandas return. `n_valid = 0` sits beside
        # it, so nothing is hidden by answering 0 rather than NaN.
        out.update(dict.fromkeys(
            ("mean", "variance", "stddev", "sem", "skewness", "kurtosis", "vmin", "vmax")))
        out["vsum"] = mpf(0)
        return out

    xs = [x for x, _ in keep]
    ws = [w for _, w in keep]
    if any(x != x for x in xs):                     # a NaN survived skipnan=.false.: all NaN out
        out.update(dict.fromkeys(
            ("vsum", "mean", "variance", "stddev", "sem", "skewness", "kurtosis")))
        out["vmin"], out["vmax"] = None, None
        return out

    W = sum(ws)
    W2 = sum(w * w for w in ws)
    S = sum(w * x for w, x in zip(ws, xs))
    mu = S / W
    m2 = sum(w * (x - mu) ** 2 for w, x in zip(ws, xs))
    m3 = sum(w * (x - mu) ** 3 for w, x in zip(ws, xs))
    m4 = sum(w * (x - mu) ** 4 for w, x in zip(ws, xs))

    # The effective sample size: what `ddof` is charged against, and what the bias corrections and
    # the standard error count in. Both reduce to `n` when every weight is 1.
    n_eff = W if weight_type == "frequency" else W * W / W2
    denom = (W - ddof) if weight_type == "frequency" else (W - ddof * W2 / W)

    out["vsum"] = S
    out["vmin"] = min(xs)
    out["vmax"] = max(xs)
    out["mean"] = mu
    out["variance"] = m2 / denom if denom > 0 else None
    out["stddev"] = mpsqrt(out["variance"]) if out["variance"] is not None else None
    out["sem"] = out["stddev"] / mpsqrt(n_eff) if out["stddev"] is not None else None

    # The population central moments the shape statistics are built from. A zero second moment
    # means every value is identical, and neither shape statistic is defined there.
    p2, p3, p4 = m2 / W, m3 / W, m4 / W
    if p2 <= 0:
        out["skewness"], out["kurtosis"] = None, None
        return out
    g1 = p3 / p2 ** mpf("1.5")
    g2 = p4 / (p2 * p2) - 3

    if bias:                                        # scipy's bias=True: the uncorrected moments
        out["skewness"] = g1
        out["kurtosis"] = g2 if excess else g2 + 3
    else:                                           # pandas' default: G1 and G2
        n = n_eff
        out["skewness"] = g1 * mpsqrt(n * (n - 1)) / (n - 2) if n > 2 else None
        if n > 3:
            big = ((n + 1) * g2 + 6) * (n - 1) / ((n - 2) * (n - 3))
            out["kurtosis"] = big if excess else big + 3
        else:
            out["kurtosis"] = None
    return out


# ======================================================================================
# --self-test: the model against the libraries a user will compare against
# ======================================================================================

def self_test():
    """Check the model transcribes the same definitions numpy, pandas and scipy implement.

    This is the half that makes the emitted numbers trustworthy. The model is written from the
    contract rather than from any library, so without this it could be self-consistently wrong;
    with it, a disagreement is either a transcription error here or a genuine divergence worth
    recording in the design document. The tolerance is loose on purpose -- these libraries compute
    the cancelling way, so the model is expected to differ from them in the last few digits, and a
    tight bound here would be asserting that the ORACLE matches the thing it is meant to correct.
    """
    try:
        import numpy as np
        import pandas as pd
        from scipy import stats as sps
    except ImportError as exc:                                 # pragma: no cover - maintainer tool
        print("--self-test: needs numpy, pandas and scipy (%s)" % exc, file=sys.stderr)
        return 2

    tol, checks = 1e-11, 0

    def close(got, want, what):
        nonlocal checks
        checks += 1
        rel = abs(float(got) - float(want)) / max(1.0, abs(float(want)))
        if rel > tol:
            print("--self-test: %s: model %.17g vs library %.17g (rel %.3g)"
                  % (what, float(got), float(want), rel), file=sys.stderr)
            return False
        return True

    ok = True
    for n in (8, 32, 1000):
        x = fixture(n)
        a = np.array(x, dtype=np.float64)
        s = pd.Series(a)
        m = model(x)
        ok &= close(m["vsum"], a.sum(), "n=%d sum" % n)
        ok &= close(m["mean"], a.mean(), "n=%d mean" % n)
        ok &= close(m["variance"], a.var(ddof=1), "n=%d var ddof=1" % n)
        ok &= close(m["stddev"], a.std(ddof=1), "n=%d std ddof=1" % n)
        ok &= close(m["sem"], sps.sem(a), "n=%d sem" % n)
        ok &= close(m["skewness"], s.skew(), "n=%d skew G1 (pandas)" % n)
        ok &= close(m["kurtosis"], s.kurt(), "n=%d kurt G2 (pandas)" % n)
        mb = model(x, bias=True)
        ok &= close(mb["skewness"], sps.skew(a), "n=%d skew g1 (scipy)" % n)
        ok &= close(mb["kurtosis"], sps.kurtosis(a), "n=%d kurt g2 (scipy)" % n)
        ok &= close(model(x, ddof=0)["variance"], a.var(ddof=0), "n=%d var ddof=0" % n)

    # Frequency weights ARE expansion: a weight of 3 must equal the value repeated three times.
    # This is the one place the two weight_type conventions can be checked against something
    # external at all, and it is what makes the reliability formula's denominator meaningful.
    x = fixture(16)
    w = [float(1 + (i % 3)) for i in range(16)]
    expanded = np.array([v for v, k in zip(x, w) for _ in range(int(k))], dtype=np.float64)
    mf = model(x, weights=w, weight_type="frequency", ddof=1)
    ok &= close(mf["mean"], expanded.mean(), "frequency mean == expansion")
    ok &= close(mf["variance"], expanded.var(ddof=1), "frequency var == expansion")
    ok &= close(mf["skewness"], pd.Series(expanded).skew(), "frequency skew == expansion")
    ok &= close(mf["kurtosis"], pd.Series(expanded).kurt(), "frequency kurt == expansion")

    # Equal weights must reduce to the unweighted answer under BOTH conventions -- the property
    # every weighted golden vector in the suite is checked against.
    m1, mr, mfq = model(x), model(x, weights=[3.0] * 16), model(
        x, weights=[3.0] * 16, weight_type="frequency")
    ok &= close(mr["variance"], m1["variance"], "equal weights == unweighted (reliability)")
    if abs(float(mfq["variance"]) - float(m1["variance"])) < 1e-12:
        print("--self-test: frequency weights of 3 must NOT equal the unweighted variance",
              file=sys.stderr)
        ok = False
    checks += 1

    # Shift invariance of the ORACLE itself: exact here, since mpf carries 50 digits.
    xs = [v + 1.0e9 for v in fixture(1000)]
    ok &= close(model(xs)["variance"], model(fixture(1000))["variance"], "shift invariance")

    # ---- The quantile model against numpy, at five probabilities and not only at the median ----
    #
    # Every UNWEIGHTED method is checked against `np.quantile`, whose token spellings this module
    # borrowed deliberately so the comparison is name for name. The weighted linear rule has no
    # counterpart anywhere -- numpy accepts `weights=` only with `inverted_cdf` -- so what is
    # checked instead is the property it was designed for: at EQUAL weights it must reproduce the
    # unweighted answer exactly. Checking only the median would have passed the earlier, wrong
    # rule, which agreed there and nowhere else.
    npmethods = {"linear": "linear", "lower": "lower", "higher": "higher",
                 "nearest": "nearest", "midpoint": "midpoint", "inverted_cdf": "inverted_cdf"}
    base = fixture(32)
    for meth, npname in npmethods.items():
        got = quantile_model(base, method=meth)
        for p, g in zip(QPROBS, got):
            ok &= close(g, np.quantile(np.array(base, dtype=float), p, method=npname),
                        "quantile %s at p=%g" % (meth, p))

    # The equal-weight reduction: the invariant the whole weighted rule exists to preserve.
    #
    # It is an ALGEBRAIC identity -- with every weight equal, `S_{i-1}/(W - w_i)` is exactly
    # `(i-1)/(m-1)` -- and it is asserted to a tight tolerance rather than with `==`, because the
    # two are reached by different expressions and floating point does not promise that identical
    # values arrive by identical roundings. What the user is promised is that adding uniform
    # weights introduces no DISCONTINUITY, and a few ulp is not one. `close` already carries a
    # loose tolerance for the cancelling libraries; this comparison is between two of our own
    # exact-arithmetic paths, so it gets a much tighter one.
    eq = quantile_model(base, weights=[3.0] * 32, method="linear")
    un = quantile_model(base, method="linear")
    for p, a, b in zip(QPROBS, eq, un):
        rel = abs(a - b) / max(mpf(1), abs(b))
        if rel > mpf("1e-40"):
            print("--self-test: equal weights must reproduce the unweighted quantile at p=%g "
                  "(%s vs %s, rel %s)" % (p, a, b, rel), file=sys.stderr)
            ok = False
        checks += 1

    # The weighted inverted_cdf rule IS frequency expansion, and numpy implements that one, so it
    # can be cross-checked directly against the expanded population.
    wv = weights_mod5(32)
    if all(float(w).is_integer() and w >= 0 for w in wv):
        expanded = []
        for x, w in zip(base, wv):
            expanded.extend([float(x)] * int(w))
        got = quantile_model(base, weights=wv, method="inverted_cdf")
        for p, g in zip(QPROBS, got):
            ok &= close(g, np.quantile(np.array(expanded, dtype=float), p, method="inverted_cdf"),
                        "weighted inverted_cdf at p=%g" % p)

    # ---- pf_mad against scipy, on the half scipy implements ----
    #
    # `median_abs_deviation` has no weighted form, so only the unweighted cases can be compared;
    # the weighted rows are pinned the way the weighted quantiles are, by the equal-weight
    # reduction asserted on the Fortran side.
    for name, _, case in MADCASES:
        if case.get("weights") is not None:
            continue
        kwargs = dict(case)
        vals = kwargs.pop("values")
        keep = [v for i, v in enumerate(vals)
                if kwargs.get("is_valid") is None or kwargs["is_valid"][i]]
        arr = np.array([float(v) for v in keep], dtype=np.float64)
        got = mad_model(vals, **kwargs)
        if kwargs.get("center") is None:
            ok &= close(got[1], sps.median_abs_deviation(arr), "mad %s raw" % name)
            ok &= close(got[0], sps.median_abs_deviation(arr, scale="normal"),
                        "mad %s normal" % name)
        else:
            # scipy takes `center` as a CALLABLE, so an explicit centre is expressed as a
            # constant function -- which is also a check that this model reads `center` the way
            # scipy does, rather than as an offset or a shift.
            c = float(kwargs["center"])
            ok &= close(got[1], sps.median_abs_deviation(arr, center=lambda a, axis=None, c=c: c),
                        "mad %s raw (explicit centre)" % name)
    # The factor itself, against scipy's own definition of it rather than against the literal.
    ok &= close(MAD_NORMAL_DENOM, sps.norm.ppf(0.75), "the normal scale denominator Phi^-1(3/4)")

    # ---- pf_mode's tie rule against scipy.stats.mode ----
    #
    # No golden vectors: the answer is an exact integer, so the Fortran tests assert it directly.
    # What is worth cross-checking is the TIE rule -- that the smallest tied value wins, which is
    # the one thing about a mode that is silently wrong rather than loudly wrong.
    for pop in ([3, 1, 3, 2, 2, 7, 2, 1, 1],          # 1 and 2 tie at three each
                [5, 5, 4, 4, 3, 3],                    # a three-way tie
                [9], [2, 2, 2], [-1, -1, 0, 0]):       # single, unanimous, tie across zero
        want = int(sps.mode(np.array(pop), keepdims=False).mode)
        got = min(v for v in set(pop) if pop.count(v) == max(pop.count(u) for u in set(pop)))
        checks += 1
        if got != want:
            print("--self-test: mode tie rule on %r: model %d vs scipy %d" % (pop, got, want),
                  file=sys.stderr)
            ok = False

    # ---- the two-sample family against numpy and scipy ----
    xa, ya = fixture(32), fixture_b(32)
    ax = np.array([float(v) for v in xa]); ay = np.array([float(v) for v in ya])
    ok &= close(cov_model(xa, ya), np.cov(ax, ay, ddof=1)[0, 1], "cov ddof=1")
    ok &= close(cov_model(xa, ya, ddof=0), np.cov(ax, ay, ddof=0)[0, 1], "cov ddof=0")
    ok &= close(corr_model(xa, ya), sps.pearsonr(ax, ay).statistic, "pearson")
    ok &= close(corr_model(xa, ya, spearman=True), sps.spearmanr(ax, ay).statistic, "spearman")
    # The pairwise-complete rule, against pandas -- which is where the rule comes from.
    mask = [(i % 3) != 0 for i in range(1, 33)]
    sx = pd.Series([float(v) if m else np.nan for v, m in zip(xa, mask)])
    sy = pd.Series([float(v) if m else np.nan for v, m in zip(ya, mask)])
    ok &= close(cov_model(xa, ya, is_valid=mask), sx.cov(sy), "cov, pairwise-complete")
    ok &= close(corr_model(xa, ya, is_valid=mask), sx.corr(sy), "pearson, pairwise-complete")
    # Spearman with TIES, which is the only thing midranks are for. Two ties in each sample.
    tx = [1.0, 2.0, 2.0, 3.0, 4.0, 4.0, 5.0]
    ty = [9.0, 8.0, 8.0, 5.0, 3.0, 1.0, 1.0]
    ok &= close(corr_model(tx, ty, spearman=True),
                sps.spearmanr(np.array(tx), np.array(ty)).statistic, "spearman with ties")
    ok &= close(cov_model(xa, xa), model(xa)["variance"], "cov(x, x) is the variance")

    # ---- pf_zscore against scipy ----
    zs = sps.zscore(ax, ddof=1)
    for k, got in zip(ZPROBES, zscore_model(xa, ZPROBES)):
        ok &= close(got, zs[k - 1], "zscore at %d" % k)

    # ---- pf_gmean / pf_hmean against scipy ----
    xp = fixture_pos(32)
    apos = np.array([float(v) for v in xp])
    ok &= close(power_model(xp, False), sps.gmean(apos), "gmean")
    ok &= close(power_model(xp, True), sps.hmean(apos), "hmean")
    wv = weights_mod5(32)
    # scipy drops nothing for a zero weight, so the comparison is made over the SURVIVORS -- which
    # is what this module's zero-weight rule means, and is the point of comparing at all.
    live = [(float(v), float(w)) for v, w in zip(xp, wv) if w > 0]
    ok &= close(power_model(xp, False, weights=wv),
                sps.gmean(np.array([v for v, _ in live]),
                          weights=np.array([w for _, w in live])), "gmean weighted")
    ok &= close(power_model(xp, True, weights=wv),
                sps.hmean(np.array([v for v, _ in live]),
                          weights=np.array([w for _, w in live])), "hmean weighted")

    # ---- the cumulative family against pandas, both `skipna` settings ----
    #
    # pandas is the reference the design names, and it is the ONE place the family's null rule can
    # be checked against something other than this file: "the running value carries past a null"
    # is a sentence, and `Series.cumsum()` is that sentence executed.
    xk = fixture(32)
    valid7 = [(i % 7) != 0 for i in range(1, 33)]
    ser = pd.Series([float(v) for v in xk])
    holed = pd.Series([float(v) if valid7[i] else np.nan for i, v in enumerate(xk)])
    for op, meth in (("sum", "cumsum"), ("prod", "cumprod"), ("max", "cummax"), ("min", "cummin")):
        want = getattr(ser, meth)().to_numpy()
        got = cum_model(xk, op)
        for k in KPROBES:
            ok &= close(got[k - 1], want[k - 1], "cum%s at %d" % (op, k))
        # The nulled arm: pandas' default `skipna=True` IS this family's rule, so a disagreement
        # here would mean the running value restarts or is poisoned rather than carrying past.
        want = getattr(holed, meth)().to_numpy()
        got = cum_model(xk, op, is_valid=valid7)
        for k in KPROBES:
            if got[k - 1] is None:
                checks += 1
                if want[k - 1] == want[k - 1]:
                    print("--self-test: cum%s at %d: model excluded, pandas kept %.17g"
                          % (op, k, want[k - 1]), file=sys.stderr)
                    ok = False
            else:
                ok &= close(got[k - 1], want[k - 1], "cum%s nulled at %d" % (op, k))
        # `skipna=False`: a NaN is a value, enters the fold and poisons every later element. The
        # assertion is that EVERY position from the first NaN on is a NaN, which is the property
        # a `max()`-based running maximum silently fails -- and did, before a probe caught it.
        raw = [float(v) if valid7[i] else float("nan") for i, v in enumerate(xk)]
        want = getattr(pd.Series(raw), meth)(skipna=False).to_numpy()
        got = cum_model(raw, op, skipnan=False)
        for k in range(1, 33):
            checks += 1
            if (want[k - 1] != want[k - 1]) != (got[k - 1] != got[k - 1]):
                print("--self-test: cum%s skipna=False at %d: model %s, pandas %s"
                      % (op, k, got[k - 1], want[k - 1]), file=sys.stderr)
                ok = False

    # ---- the binning family against numpy and pandas ----
    ed = [mpf(repr(e)) for e in HEDGES]
    arr = np.array([float(v) for v in xk])
    npe = np.array(HEDGES)
    for label, kwargs, npkw in (("plain", {}, {}),
                                ("weighted", {"weights": weights_mod5(32)},
                                 {"weights": np.array([float(w) for w in weights_mod5(32)])})):
        _, counts, _, _, nout = bin_model(xk, ed, **kwargs)
        want, _ = np.histogram(arr, bins=npe, **npkw)
        for k in range(len(counts)):
            ok &= close(counts[k], want[k], "histogram %s bin %d" % (label, k + 1))
        # numpy drops out-of-range values silently, so `n_outside` has no direct counterpart --
        # it is checked against a restatement of the definition in numpy's own array algebra:
        # a USABLE element (non-zero weight) that is not inside the closed edge range. The weight
        # has to appear here for the same reason it appears in the model: a zero-weight element
        # has left the population and cannot be one that failed to reach a bin.
        usable = np.ones(len(xk), dtype=bool)
        if npkw:
            usable = npkw["weights"] > 0
        want_out = int((usable & ~((arr >= HEDGES[0]) & (arr <= HEDGES[-1]))).sum())
        ok &= close(nout, want_out, "histogram %s n_outside" % label)

    # `np.digitize(x, bins) - 1` is `pf_bucketize`'s left-closed code, except at the very top
    # edge, where digitize answers `nbins + 1` and this family folds that value into the last
    # bin. That difference IS `np.histogram`'s last-bin rule, so the two numpy functions
    # disagree with each other and the histogram is the one being matched.
    hbx = [mpf(repr(v)) for v in HBX]
    hbe = [mpf(repr(e)) for e in HBE]
    codes, counts, _, _, nout = bin_model(hbx, hbe, right=False)
    dig = np.digitize(np.array(HBX), np.array(HBE), right=False)
    for i, v in enumerate(HBX):
        checks += 1
        # `digitize` answers `nbins + 1` for BOTH the top edge and anything above it, so the
        # two have to be told apart by value rather than by its code -- which is exactly the
        # last-bin rule `np.histogram` applies and `np.digitize` does not.
        if v < HBE[0] or v > HBE[-1]:
            want = 0
        elif v == HBE[-1]:
            want = len(HBE) - 1
        else:
            want = int(dig[i])
        if codes[i] != want:
            print("--self-test: bucketize left at %g: model %d, digitize %d"
                  % (v, codes[i], want), file=sys.stderr)
            ok = False
    want, _ = np.histogram(np.array(HBX), bins=np.array(HBE))
    for k in range(len(counts)):
        ok &= close(counts[k], want[k], "boundary histogram left bin %d" % (k + 1))

    # The right-closed convention is `pd.cut(right=True, include_lowest=True)`, whose 0-based
    # codes are ours minus one and whose -1 is our 0.
    codes, counts, _, _, _ = bin_model(hbx, hbe, right=True)
    cut = pd.cut(pd.Series(HBX), bins=HBE, right=True, include_lowest=True)
    for i, v in enumerate(HBX):
        checks += 1
        want = int(cut.cat.codes.iloc[i]) + 1      # -1 -> 0, 0 -> 1, ...
        if codes[i] != want:
            print("--self-test: bucketize right at %g: model %d, pd.cut %d"
                  % (v, codes[i], want), file=sys.stderr)
            ok = False
    for k in range(len(counts)):
        ok &= close(counts[k], int((cut.cat.codes == k).sum()),
                    "boundary histogram right bin %d" % (k + 1))

    # ---- `density=` against numpy, including the degenerate answers ----
    for label, kwargs, npkw in (("plain", {}, {}),
                                ("weighted", {"weights": weights_mod5(32)},
                                 {"weights": np.array([float(w) for w in weights_mod5(32)])})):
        _, dens, _, _, _ = bin_model(xk, ed, density=True, **kwargs)
        want, _ = np.histogram(arr, bins=npe, density=True, **npkw)
        for k in range(len(dens)):
            ok &= close(dens[k], want[k], "density %s bin %d" % (label, k + 1))
    # `sum(density * width)` is 1 -- the property the normalisation exists for, and the one that
    # a base taken over the wrong population (everything passed, rather than everything binned)
    # would break while every individual bin still looked plausible.
    _, dens, _, _, _ = bin_model(xk, ed, density=True)
    total = sum((dens[j] * (ed[j + 1] - ed[j]) for j in range(len(dens))), mpf(0))
    ok &= close(total, 1, "density integrates to 1")
    # Uneven widths, where a formula that forgot the per-bin width still integrates to something
    # and is wrong bin by bin.
    uneven = [mpf(-400), mpf(-100), mpf(0), mpf(400)]
    _, dens, _, _, _ = bin_model(xk, uneven, density=True)
    want, _ = np.histogram(arr, bins=np.array([-400.0, -100.0, 0.0, 400.0]), density=True)
    for k in range(len(dens)):
        ok &= close(dens[k], want[k], "density uneven bin %d" % (k + 1))
    # Nothing binned: numpy divides by zero and answers NaN, which is this module's own rule for
    # an undefined result, so the two agree without a special case on either side.
    _, dens, _, _, _ = bin_model([mpf(-5000), mpf(5000)], ed, density=True)
    want, _ = np.histogram(np.array([-5000.0, 5000.0]), bins=npe, density=True)
    for k in range(len(dens)):
        checks += 1
        if (dens[k] is not None) or (want[k] == want[k]):
            print("--self-test: density all-outside bin %d: model %s, numpy %s"
                  % (k + 1, dens[k], want[k]), file=sys.stderr)
            ok = False

    # ---- `pf_bin_edges` against `np.histogram_bin_edges`, degenerate answers included ----
    valid_ext = [(i not in (1, 11)) for i in range(1, 33)]
    for label, vals, kwargs, npvals in (
            ("plain", xk, {}, arr),
            ("nulled", xk, {"is_valid": valid_ext},
             np.array([float(v) for i, v in enumerate(xk) if valid_ext[i]])),
            ("constant", [mpf(7)] * 5, {}, np.full(5, 7.0)),
            ("empty", [], {}, np.array([])),
            ("single", [mpf(3)], {}, np.array([3.0]))):
        be, fine = bin_edges_model(vals, NBE, **kwargs)
        want = np.histogram_bin_edges(npvals, bins=NBE)
        for k in range(NBE + 1):
            ok &= close(be[k], want[k], "bin_edges %s edge %d" % (label, k))
        checks += 1
        if fine != (label in ("plain", "nulled")):
            print("--self-test: bin_edges %s: ok flag is %s" % (label, fine), file=sys.stderr)
            ok = False
    # The edges have to be usable as edges, which is the contract the degenerate answers exist to
    # keep: `pf_histogram` aborts on a pair that is not strictly increasing.
    for label, vals in (("plain", xk), ("constant", [mpf(7)] * 5), ("empty", []),
                        ("single", [mpf(3)])):
        be, _ = bin_edges_model(vals, NBE)
        checks += 1
        if any(be[k] <= be[k - 1] for k in range(1, NBE + 1)):
            print("--self-test: bin_edges %s is not strictly increasing" % label, file=sys.stderr)
            ok = False

    # ---- the sigma clip against astropy, if it is installed ----
    #
    # Optional: this is a maintainer-only self-test and astropy is a heavy fourth dependency, so a
    # machine without it gets a printed SKIP rather than a failure. CI runs `--check`, not this.
    try:
        from astropy.stats import sigma_clipped_stats as astro_clip
    except ImportError:                                        # pragma: no cover - maintainer tool
        print("--self-test: astropy not installed; the sigma-clip cross-check was SKIPPED. That "
              "is the ONE cross-check this file cannot do without a fourth library, so a run "
              "without it does not certify the clip.")
    else:
        xc = fixture_clip(400)
        arr = np.array([float(v) for v in xc])
        for cen, std in (("median", "std"), ("mean", "std"), ("median", "mad_std")):
            want = astro_clip(arr, sigma=3.0, maxiters=5, cenfunc=cen, stdfunc=std)
            got = clip_model(xc, cenfunc=cen, stdfunc=std)
            for k, label in enumerate(("mean", "median", "stddev")):
                ok &= close(got[k], want[k], "sigma clip %s/%s %s" % (cen, std, label))
        # `maxiters=None` -- iterate to convergence -- which is what `maxiters <= 0` spells here.
        want = astro_clip(arr, sigma=3.0, maxiters=None)
        got = clip_model(xc, maxiters=0)
        for k, label in enumerate(("mean", "median", "stddev")):
            ok &= close(got[k], want[k], "sigma clip to convergence %s" % label)
        # An asymmetric clip, which is where a `sigma_lower`/`sigma_upper` mix-up would show.
        want = astro_clip(arr, sigma_lower=1.5, sigma_upper=4.0, maxiters=5)
        got = clip_model(xc, sigma_lower=1.5, sigma_upper=4.0)
        for k, label in enumerate(("mean", "median", "stddev")):
            ok &= close(got[k], want[k], "sigma clip asymmetric %s" % label)

    if not ok:
        return 1
    print("generate_stats_vectors.py --self-test: %d model/library comparisons agree "
          "(numpy %s, pandas %s, scipy %s)" % (checks, np.__version__, pd.__version__,
                                               sps.__name__ and __import__("scipy").__version__))
    return 0


# ======================================================================================
# The cases, and the Fortran they become
# ======================================================================================

#: The tier-A quantities, in the order every emitted array uses. `test_stats.f90` reads them back
#: in this order, so adding one here means adding it there -- which is a compile error, not a
#: silent skip, because the array extents must agree.
QUANTITIES = ("vsum", "mean", "variance", "stddev", "sem", "skewness", "kurtosis", "vmin", "vmax")


def nan_at(n, every):
    """The fixture with every `every`-th element replaced by a NaN."""
    return [NAN if i % every == 0 else v for i, v in enumerate(fixture(n), start=1)]


def nan_weights(n, every):
    """Weight 1 everywhere except a NaN on the elements `nan_at` made NaN.

    That pairing is the point: a NaN weight beside a NaN value must be excluded by the NaN rule
    BEFORE the weight is examined, so this case aborts the moment the exclusion order is reversed.
    """
    return [NAN if i % every == 0 else 1.0 for i in range(1, n + 1)]


#: `(name, doc, kwargs-for-model)`. `values` is built by the recipe unless the case supplies its
#: own; everything else is an ordinary `model` keyword.
CASES = [
    ("U32", "the recipe at n=32, unweighted, ddof=1 -- the default configuration",
     dict(values=fixture(32))),
    ("U32_D0", "ddof=0: the population variance, which is what numpy's default returns",
     dict(values=fixture(32), ddof=0)),
    ("U32_D99", "ddof >= n_valid: NaN, not a division by zero and not an abort",
     dict(values=fixture(32), ddof=99)),
    ("U32_BIAS", "bias=.true.: scipy's uncorrected g1 and g2 rather than pandas' G1 and G2",
     dict(values=fixture(32), bias=True)),
    ("U32_RAWK", "excess=.false.: the raw fourth-moment ratio, 3 for a normal population",
     dict(values=fixture(32), excess=False)),
    ("U1000", "the recipe at n=1000 -- the partner of SHIFT",
     dict(values=fixture(1000))),
    ("SHIFT", "the same population offset by 1e9: variance and above must match U1000",
     dict(values=[v + 1.0e9 for v in fixture(1000)])),
    ("W3", "every weight 3, reliability: must equal U32 exactly -- the equal-weight property",
     dict(values=fixture(32), weights=[3.0] * 32)),
    ("W3F", "every weight 3, frequency: must NOT equal U32, because ddof is charged differently",
     dict(values=fixture(32), weights=[3.0] * 32, weight_type="frequency")),
    ("WVAR", "unequal weights including zeros, reliability -- n_valid < size(values)",
     dict(values=fixture(32), weights=weights_mod5(32))),
    ("WVARF", "the same weights, frequency: the two conventions separate here and only here",
     dict(values=fixture(32), weights=weights_mod5(32), weight_type="frequency")),
    ("NULLS", "one element in three null: n_null reports them and nothing else changes",
     dict(values=fixture(32), is_valid=[(i % 3) != 0 for i in range(1, 33)])),
    ("NANS", "every tenth value NaN, its weight NaN too -- pins the exclusion ORDER",
     dict(values=nan_at(32, 10), weights=nan_weights(32, 10))),
    ("NANS_KEEP", "the same values with skipnan=.false.: one NaN makes every answer NaN",
     dict(values=nan_at(32, 10), skipnan=False)),
    ("CONST", "17 identical values: variance exactly 0, and neither shape statistic defined",
     dict(values=[2.5] * 17)),
    ("SINGLE", "one element: mean is that value, variance at ddof=1 is NaN",
     dict(values=[3.75])),
    ("SINGLE_D0", "one element at ddof=0: variance is exactly 0",
     dict(values=[3.75], ddof=0)),
    ("EMPTY", "a zero-length array: every answer NaN except the sum, n_valid 0, and NO abort",
     dict(values=[])),
    ("ALLNULL", "every element null: the empty case reached the way a per-group loop reaches it",
     dict(values=fixture(8), is_valid=[False] * 8)),
    ("ALLZEROW", "every weight zero: the empty case again, and numpy raises where this does not",
     dict(values=fixture(8), weights=[0.0] * 8)),
]


#: The quantile cases. Separate from CASES because they answer a different question and carry a
#: different shape: one array of quantiles per (population, method) pair rather than one row of
#: tier-A quantities.
#:
#: **The unweighted rows are cross-checked against numpy** by `--self-test`, at five probabilities
#: rather than at the median alone -- which is exactly where an earlier draft of the weighted rule
#: hid its error. The weighted rows cannot be: no reference library interpolates a weighted
#: quantile, so they come from the derivation at 50 digits, and what : pins THEM is the
# equal-weight reduction, which `Q_W3_LINEAR` asserts by being required to equal : `Q_U32_LINEAR`
# exactly.
QPROBS = [0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0]

QCASES = [
    ("U32_LINEAR", "the default rule at n=32: Hyndman-Fan type 7, numpy's and pandas' default",
     dict(values=fixture(32), method="linear")),
    ("U32_LOWER", "method=lower: the order statistic at or below the position",
     dict(values=fixture(32), method="lower")),
    ("U32_HIGHER", "method=higher: the order statistic at or above it",
     dict(values=fixture(32), method="higher")),
    ("U32_NEAREST", "method=nearest: whichever of the two is closer",
     dict(values=fixture(32), method="nearest")),
    ("U32_MIDPOINT", "method=midpoint: their mean",
     dict(values=fixture(32), method="midpoint")),
    ("U32_ICDF", "method=inverted_cdf: a step function on the plain cumulative scale",
     dict(values=fixture(32), method="inverted_cdf")),
    ("U33_LINEAR", "odd length, so the median is an element rather than an interpolation",
     dict(values=fixture(33), method="linear")),
    ("W3_LINEAR", "every weight 3: must equal U32_LINEAR EXACTLY -- the equal-weight reduction",
     dict(values=fixture(32), weights=[3.0] * 32, method="linear")),
    ("WVAR_LINEAR", "unequal weights: the derived rule, with no library to cross-check against",
     dict(values=fixture(32), weights=weights_mod5(32), method="linear")),
    ("WVAR_ICDF", "the same weights on the cumulative scale, which numpy DOES implement",
     dict(values=fixture(32), weights=weights_mod5(32), method="inverted_cdf")),
    ("NULLS_LINEAR", "one element in three null: the quantiles are of what survives",
     dict(values=fixture(32), is_valid=[(i % 3) != 0 for i in range(1, 33)], method="linear")),
]


#: The SECOND sample, for the two-sample family. A different multiplier through the same recipe,
#: so it is exactly representable in both languages for the same reason the first is, and is not a
#: shift or a permutation of it -- either of those would give a correlation this family should not
#: be pinned by. Mirrored by `golden_fixture_b` in test/test_stats.f90.
RECIPE_B2 = 3571


def fixture_b(n):
    """The `y(1:n)` a Fortran `golden_fixture_b(n, y)` call must produce, as exact doubles."""
    out = []
    for i in range(1, n + 1):
        a = (i * i * RECIPE_B2 + RECIPE_B) % RECIPE_M
        out.append(mpf(a - RECIPE_OFFSET) / mpf(int(RECIPE_SCALE)))
    return out


def fixture_pos(n):
    """A strictly POSITIVE population, which `pf_gmean` and `pf_hmean` need to be defined at all.

    The same recipe without its offset, so every value is at least `1/1024` and the whole domain
    question is about what the procedures do, not about whether the fixture reaches it. Mirrored
    by `golden_fixture_pos` in test/test_stats.f90.
    """
    out = []
    for i in range(1, n + 1):
        a = (i * i * RECIPE_A + RECIPE_B) % RECIPE_M
        out.append(mpf(a + 1) / mpf(int(RECIPE_SCALE)))
    return out


def pair_select(x, y, is_valid=None, weights=None):
    """The surviving (x, y, w) triples under the PAIRWISE-COMPLETE rule."""
    out = []
    for i in range(len(x)):
        if is_valid is not None and not is_valid[i]:
            continue
        if x[i] != x[i] or y[i] != y[i]:
            continue
        w = mpf(1) if weights is None else mpf(repr(weights[i]))
        if weights is not None and w <= 0:
            continue
        out.append((mpf(x[i]), mpf(y[i]), w))
    return out


def pair_moments(trips):
    """`(mx, my, sxx, sxy, syy, W, Wsq)` at 50 digits, from the definitions."""
    w_sum = sum((w for _, _, w in trips), mpf(0))
    w_sq = sum((w * w for _, _, w in trips), mpf(0))
    mx = sum((w * a for a, _, w in trips), mpf(0)) / w_sum
    my = sum((w * b for _, b, w in trips), mpf(0)) / w_sum
    sxx = sum((w * (a - mx) ** 2 for a, _, w in trips), mpf(0))
    sxy = sum((w * (a - mx) * (b - my) for a, b, w in trips), mpf(0))
    syy = sum((w * (b - my) ** 2 for _, b, w in trips), mpf(0))
    return mx, my, sxx, sxy, syy, w_sum, w_sq


def midrank_model(vals):
    """Each value's midrank: every run of equal values gets the mean of the positions it spans."""
    order = sorted(range(len(vals)), key=lambda k: vals[k])
    out = [mpf(0)] * len(vals)
    i = 0
    while i < len(order):
        j = i
        while j + 1 < len(order) and vals[order[j + 1]] == vals[order[i]]:
            j += 1
        mid = mpf(i + j + 2) / 2               # 1-based positions i+1 .. j+1
        for k in range(i, j + 1):
            out[order[k]] = mid
        i = j + 1
    return out


def cov_model(x, y, is_valid=None, weights=None, ddof=1):
    """The pairwise-complete covariance, with the reliability `ddof` denominator."""
    trips = pair_select(x, y, is_valid, weights)
    if not trips:
        return None
    _, _, _, sxy, _, w_sum, w_sq = pair_moments(trips)
    denom = w_sum - mpf(ddof) * w_sq / w_sum
    if denom <= 0:
        return None
    return sxy / denom


def corr_model(x, y, is_valid=None, weights=None, spearman=False):
    """Pearson, or Pearson over midranks."""
    trips = pair_select(x, y, is_valid, weights)
    if not trips:
        return None
    if spearman:
        rx = midrank_model([a for a, _, _ in trips])
        ry = midrank_model([b for _, b, _ in trips])
        trips = [(rx[k], ry[k], trips[k][2]) for k in range(len(trips))]
    _, _, sxx, sxy, syy, _, _ = pair_moments(trips)
    if sxx <= 0 or syy <= 0:
        return None
    return sxy / (mpsqrt(sxx) * mpsqrt(syy))


def zscore_model(values, probes, is_valid=None, ddof=1):
    """The standardised value at each 1-based index in `probes`."""
    keep, _, _ = select(values, is_valid, None, True)
    xs = [x for x, _ in keep]
    if len(xs) < 2:
        return [None] * len(probes)
    mu = sum(xs, mpf(0)) / mpf(len(xs))
    var = sum(((x - mu) ** 2 for x in xs), mpf(0)) / mpf(len(xs) - ddof)
    if var <= 0:
        return [None] * len(probes)
    sd = mpsqrt(var)
    out = []
    for k in probes:
        if is_valid is not None and not is_valid[k - 1]:
            out.append(None)
        else:
            out.append((mpf(values[k - 1]) - mu) / sd)
    return out


def power_model(values, harmonic, weights=None):
    """The geometric or harmonic mean, from the definition."""
    from mpmath import log as mplog, exp as mpexp
    keep, _, _ = select(values, None, weights, True)
    if not keep:
        return None
    if any(x < 0 for x, _ in keep):
        return None
    if any(x == 0 for x, _ in keep):
        return mpf(0)
    w_sum = sum((w for _, w in keep), mpf(0))
    if harmonic:
        return w_sum / sum((w / x for x, w in keep), mpf(0))
    return mpexp(sum((w * mplog(x) for x, w in keep), mpf(0)) / w_sum)


def fixture_clip(n):
    """The population the sigma-clip rows are taken over: the recipe, plus four wild points.

    Deliberately built from the same recipe rather than from a random generator, so the Fortran
    side reproduces it exactly from `golden_fixture` and four assignments -- a clip fixture that
    could not be mirrored bit for bit would make every disagreement ambiguous.
    """
    out = list(fixture(n))
    for k, v in ((7, 4000), (123, -3500), (200, 5000), (365, -6000)):
        if k <= n:                 # so the 8-element probe row is a prefix of the real fixture
            out[k - 1] = mpf(v)
    return out


def clip_model(values, sigma=3.0, sigma_lower=None, sigma_upper=None, maxiters=5,
               cenfunc="median", stdfunc="std"):
    """astropy's iterative clip, written from the algorithm rather than from astropy.

    Returns `[mean, median, stddev]` of the survivors. Unweighted, because the procedure is.
    """
    slo = mpf(repr(sigma if sigma_lower is None else sigma_lower))
    shi = mpf(repr(sigma if sigma_upper is None else sigma_upper))
    xs = sorted(mpf(v) for v in values if v == v)
    lo, hi = 0, len(xs) - 1
    cap = 100 if maxiters <= 0 else maxiters
    for _ in range(cap):
        n = hi - lo + 1
        if n < 2:
            break
        sl = xs[lo:hi + 1]
        mu = sum(sl, mpf(0)) / mpf(n)
        # ddof = 0, which is what astropy's default `stdfunc='std'` (numpy's `nanstd`) uses --
        # for the ROUND SCALE and for the reported stddev alike.
        var = sum(((v - mu) ** 2 for v in sl), mpf(0)) / mpf(n)
        sd = mpsqrt(var)
        centre = mu if cenfunc == "mean" else _median(sl)
        if stdfunc == "mad_std":
            c = _median(sl)
            sd = _median(sorted(abs(v - c) for v in sl)) / MAD_NORMAL_DENOM
        if not sd > 0:
            break
        a, b = centre - slo * sd, centre + shi * sd
        plo, phi = lo, hi
        while lo <= hi and xs[lo] < a:
            lo += 1
        while hi >= lo and xs[hi] > b:
            hi -= 1
        if hi - lo + 1 < 2:
            lo, hi = plo, phi
            break
        if lo == plo and hi == phi:
            break
    sl = xs[lo:hi + 1]
    n = len(sl)
    if n == 0:
        return [None, None, None]
    mu = sum(sl, mpf(0)) / mpf(n)
    sd = None
    if n >= 1:
        sd = mpsqrt(sum(((v - mu) ** 2 for v in sl), mpf(0)) / mpf(n))
    return [mu, _median(sl), sd]


def _median(sorted_vals):
    """The interpolating median of an already-sorted list."""
    n = len(sorted_vals)
    k = (n - 1) // 2
    if n % 2 == 1:
        return sorted_vals[k]
    return (sorted_vals[k] + sorted_vals[k + 1]) / 2


#: The sigma-clip cases. Each emits `[mean, median, stddev, n_clipped]`.
CCASES = [
    ("DEF", "astropy's defaults: sigma=3, maxiters=5, median centre, std scale", dict()),
    ("MEANC", "cenfunc=mean, which moves with the outliers it is trying to reject",
     dict(cenfunc="mean")),
    ("MADSTD", "stdfunc=mad_std, which barely moves at all", dict(stdfunc="mad_std")),
    ("CONV", "maxiters<=0: iterate until a round removes nothing", dict(maxiters=0)),
    ("ASYM", "an asymmetric clip, where a sigma_lower/sigma_upper mix-up would show",
     dict(sigma_lower=1.5, sigma_upper=4.0)),
]

#: The two-sample and transform cases. Each row emits what its own comment names.
RCASES = [
    ("U32", "the two recipes at n=32, unweighted, ddof=1", dict()),
    ("WVAR", "unequal weights, every fifth of them zero", dict(weights=weights_mod5(32))),
    ("NULLS", "one PAIR in three excluded, which is what pairwise-complete means",
     dict(is_valid=[(i % 3) != 0 for i in range(1, 33)])),
]

#: Which elements `pf_zscore`'s golden row probes. The first, the last and two in between, so a
#: transform that got the mean right and the scale wrong still fails.
ZPROBES = [1, 7, 20, 32]

#: `pf_gmean`/`pf_hmean`'s cases; each emits `[gmean, hmean]`.
PCASES = [
    ("U32", "a strictly positive population at n=32", dict()),
    ("WVAR", "the same, weighted", dict(weights=weights_mod5(32))),
]

#: `pf_mad`'s cases. Each emits the pair `[normal, raw]`, so the normal scale factor is pinned by
#: the RATIO of two committed numbers rather than by a literal a test could copy from the source.
MADCASES = [
    ("U32", "the default: centre is the population's own median, scale is \"normal\"",
     dict(values=fixture(32))),
    ("U33", "odd length, so the centre is an element rather than an interpolation",
     dict(values=fixture(33))),
    ("CTR", "an explicit centre, which skips one selection and changes the deviations",
     dict(values=fixture(32), center=0.5)),
    ("WVAR", "unequal weights: both medians are weighted, values and deviations alike",
     dict(values=fixture(32), weights=weights_mod5(32))),
    ("NULLS", "one element in three null: the deviations are of what survives",
     dict(values=fixture(32), is_valid=[(i % 3) != 0 for i in range(1, 33)])),
    ("OUT", "four wild points in thirty-six: this is the case pf_stddev gets wrong and MAD does not",
     dict(values=fixture(32) + [1.0e6, -1.0e6, 2.0e6, -2.0e6])),
]

#: The "normal" scale, spelled as scipy spells it: a DIVISION by `Phi^-1(3/4)`. The rounded
#: 1.4826 of every textbook is 1.5e-06 away from it, which is four million times the double
#: precision noise floor -- large enough that using it made the scipy cross-check below fail.
MAD_NORMAL_DENOM = mpf("0.6744897501960817")


def mad_model(values, is_valid=None, weights=None, skipnan=True, center=None):
    """`[scale="normal", scale="raw"]` for one population, at 50 digits.

    Written from the definition rather than from scipy, per this file's premise -- and then
    cross-checked against `scipy.stats.median_abs_deviation` in `--self-test` for the unweighted
    cases, which is the only half scipy implements.
    """
    keep, _, _ = select(values, is_valid, weights, skipnan)
    if not keep:
        return [None, None]
    pairs = sorted(keep, key=lambda t: t[0])
    xs = [x for x, _ in pairs]
    ws = [w for _, w in pairs]
    weighted = weights is not None
    if center is None:
        c = quantile_at(xs, ws, mpf("0.5"), "linear", weighted)
    else:
        c = mpf(repr(center))
    # The deviations are a DIFFERENT population: |x - c| is not monotone in x, so they carry their
    # own order and each keeps the weight of the element it came from.
    dev = sorted(zip([abs(x - c) for x in xs], ws), key=lambda t: t[0])
    dxs = [d for d, _ in dev]
    dws = [w for _, w in dev]
    raw = quantile_at(dxs, dws, mpf("0.5"), "linear", weighted)
    return [raw / MAD_NORMAL_DENOM, raw]


def quantile_model(values, is_valid=None, weights=None, skipnan=True, method="linear"):
    """Every probability in QPROBS for one population, at 50 digits."""
    keep, _, _ = select(values, is_valid, weights, skipnan)
    if not keep:
        return [None] * len(QPROBS)
    pairs = sorted(keep, key=lambda t: t[0])
    xs = [x for x, _ in pairs]
    ws = [w for _, w in pairs]
    return [quantile_at(xs, ws, mpf(repr(p)), method, weights is not None) for p in QPROBS]


def positions(ws, weighted):
    """The 0-1 position of each sorted value: `(i-1)/(m-1)`, or `S_{i-1}/(W - w_i)` weighted.

    The weighted form collapses to the unweighted one exactly when every weight is equal, which is
    the property the rule was chosen for and the one `W3_LINEAR` asserts.
    """
    m = len(ws)
    if m == 1:
        return [mpf(0)]
    if not weighted:
        return [mpf(i) / mpf(m - 1) for i in range(m)]
    total = sum(ws, mpf(0))
    pos, run = [], mpf(0)
    for i in range(m):
        denom = total - ws[i]
        pos.append(run / denom if denom > 0 else mpf(0))
        run += ws[i]
    pos[-1] = mpf(1)
    return pos


def quantile_at(xs, ws, p, method, weighted):
    """One quantile of an already-sorted population, by the same rules the Fortran applies."""
    m = len(xs)
    if m == 1:
        return xs[0]
    if method == "inverted_cdf":
        total = sum(ws, mpf(0)) if weighted else mpf(m)
        want, run = p * total, mpf(0)
        for i in range(m):
            run += ws[i] if weighted else mpf(1)
            if run >= want:
                return xs[i]
        return xs[-1]
    if not weighted:
        # The RANK scale, `g = p*(m-1)`, which is numpy's own formulation and what the Fortran
        # computes. Deriving `t` from two entries of a position array instead is algebraically the
        # same and numerically is not: it lands a few ulp either side of an exact tie, which is
        # exactly what `nearest` reads.
        g = p * mpf(m - 1)
        lo = min(m - 1, int(g))
        t = g - mpf(lo)
        hi = min(lo + 1, m - 1)
    else:
        pos = positions(ws, weighted)
        lo = 0
        for i in range(m):
            if pos[i] <= p:
                lo = i
            else:
                break
        hi = min(lo + 1, m - 1)
        t = (p - pos[lo]) / (pos[hi] - pos[lo]) if pos[hi] > pos[lo] else mpf(0)
    if method == "linear":
        return xs[lo] + t * (xs[hi] - xs[lo])
    if method == "lower":
        return xs[lo]
    if method == "higher":
        return xs[hi] if t > 0 else xs[lo]
    if method == "nearest":
        # numpy breaks an exact tie to the EVEN lower index; see `apply_method` in
        # src/parquet_stats_order.f90 for why the tie is reachable and why it matters.
        if t > mpf("0.5"):
            return xs[hi]
        if t < mpf("0.5"):
            return xs[lo]
        return xs[lo] if lo % 2 == 0 else xs[hi]
    if method == "midpoint":
        return (xs[lo] + xs[hi]) / 2 if t > 0 else xs[lo]
    raise SystemExit("generate_stats_vectors.py: unknown method %r" % method)


# ======================================================================================
# P9: the cumulative family and the binning family
# ======================================================================================

def cum_model(values, op, is_valid=None, skipnan=True):
    """The running value at every position, with `None` where the element is excluded.

    The family's rule, written out once: an excluded element contributes NOTHING and the running
    value carries past it unchanged. `skipnan=False` makes a NaN an ordinary value instead, so it
    enters the fold and poisons everything after it.
    """
    out, acc, started = [], None, False
    for i, raw in enumerate(values):
        v = raw
        if is_valid is not None and not is_valid[i]:
            out.append(None)
            continue
        if skipnan and v != v:
            out.append(None)
            continue
        v = mpf(v) if v == v else v
        if not started:
            acc, started = v, True
        elif acc != acc or v != v:
            acc = NAN                      # a NaN kept as a value poisons every later element
        elif op == "sum":
            acc = acc + v
        elif op == "prod":
            acc = acc * v
        elif op == "max":
            acc = v if v > acc else acc
        else:
            acc = v if v < acc else acc
        out.append(acc)
    return out


def bin_of(v, edges, right):
    """The 1-based bin `v` joins, or 0. The convention, transcribed from the doc-comment."""
    nb = len(edges) - 1
    if not (v >= edges[0]) or not (v <= edges[nb]):
        return 0
    if right:
        # bin k is (edges(k), edges(k+1)], and the FIRST bin closes at the bottom.
        k = 1
        while k < nb and not (v <= edges[k]):
            k += 1
        return k
    # bin k is [edges(k), edges(k+1)), and the LAST bin closes at the top.
    k = nb
    while k > 1 and not (v >= edges[k - 1]):
        k -= 1
    return k


def bin_model(values, edges, right=False, density=False, is_valid=None, weights=None,
              skipnan=True):
    """`(codes, counts, n_null, n_nan, n_outside)` -- the whole binning contract in one place."""
    nb = len(edges) - 1
    codes, counts = [], [mpf(0)] * nb
    n_null = n_nan = n_out = 0
    for i, v in enumerate(values):
        if is_valid is not None and not is_valid[i]:
            codes.append(0)
            n_null += 1
            continue
        if skipnan and v != v:
            codes.append(0)
            n_nan += 1
            continue
        w = mpf(1)
        if weights is not None:
            # The weight is examined BEFORE the bin search, which is the module's own exclusion
            # order -- null, then NaN, then weight -- and it has an observable consequence: a
            # zero-weight element has left the POPULATION, so it is not an element that failed to
            # reach a bin and is counted in neither `n_outside` nor any bin.
            if weights[i] <= 0:
                codes.append(0)
                continue
            w = mpf(weights[i])
        k = bin_of(v, edges, right)
        codes.append(k)
        if k == 0:
            n_out += 1
            continue
        counts[k - 1] += w
    if density:
        # `np.histogram(density=True)`: each bin by its OWN width and by the total that was
        # BINNED -- values outside the edges were never counted, so they are not in the base.
        base = sum(counts, mpf(0))
        if base > 0:
            counts = [c / ((edges[j + 1] - edges[j]) * base) for j, c in enumerate(counts)]
        else:
            counts = [None] * nb
    return codes, counts, n_null, n_nan, n_out


def bin_edges_model(values, nbins, is_valid=None, weights=None, skipnan=True):
    """(edges, ok) -- np.histogram_bin_edges(bins=nbins), degenerate answers included."""
    keep, _, _ = select(values, is_valid, weights, skipnan)
    xs = [x for x, _ in keep]
    fine = True
    if not xs:
        lo, hi, fine = mpf(0), mpf(1), False           # numpy's empty fallback
    else:
        lo, hi = min(xs), max(xs)
        if not (hi > lo):
            lo, hi, fine = lo - mpf("0.5"), hi + mpf("0.5"), False   # numpy's constant fallback
    span = hi - lo
    edges = [lo] + [lo + mpf(j) * span / mpf(nbins) for j in range(1, nbins)] + [hi]
    return edges, fine


#: The 1-based positions the K_* cumulative rows are read at. `1` pins the very first fold, `2`
#: the first combination, and `32` the total -- which is where a wrong fold shows up largest.
KPROBES = [1, 2, 8, 17, 32]

#: The edges the H_* histogram rows use: round numbers well inside the fixture's own range, so
#: values fall outside at both ends and `n_outside` is exercised rather than merely reported.
HEDGES = [-400.0, -200.0, 0.0, 200.0, 400.0]

#: The exact-boundary case, where every interior edge lands ON a value. This is the ONLY fixture
#: that can tell the two conventions apart at an edge, and its values are small integers so that
#: the expected answer can also be read off by hand.
HBX = [-1.0, 0.0, 2.0, 4.0, 8.0, 9.0]
HBE = [0.0, 2.0, 4.0, 6.0, 8.0]

#: How many bins the `pf_bin_edges` golden rows ask for. Five rather than a power of two so that
#: the interior boundaries are not exactly representable, which is where a linspace written the
#: obvious way drifts from one that pins its endpoints.
NBE = 5


def fortran_real(v):
    """`v` as a round-trip-exact Fortran real64 literal."""
    if v is None or v != v:
        return "0.0_real64"
    text = "%.17g" % float(v)
    if not any(c in text for c in ".eE"):
        text += ".0"
    return text + "_real64"


def wrap_array(head, items, width=128):
    """`head = [a, b, c]` wrapped with `&` continuations inside the 132-column limit."""
    lines, cur = [], head + " ["
    pad = " " * (len(head) - len(head.lstrip()) + 4)
    for k, item in enumerate(items):
        piece = item + (", " if k + 1 < len(items) else "]")
        if len(cur) + len(piece) > width:
            lines.append(cur + " &")
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
! Regenerate with:  tools/generate_stats_vectors.py
!
!> Golden expectations for `parquet_stats`, derived at 50 decimal digits by `mpmath`.
!!
!! **The oracle deliberately shares no arithmetic with the library, and none with numpy either.**
!! Every value here is computed over the fixture's exact rationals, because the accuracy claim
!! `parquet_stats` makes is precisely that it does not lose the digits a cancelling formula loses
!! -- and a reference computed the cancelling way cannot certify that. numpy, pandas and scipy
!! appear only in the generator's `--self-test`, where they confirm the model transcribes the same
!! definitions they implement.
!!
!! Each case is one population and one configuration. `G_<name>` holds the nine tier-A quantities
!! in the order `QNAME` names; `G_<name>_DEF` says which of them are DEFINED, so an undefined
!! statistic is asserted to be a quiet NaN rather than compared against a number. `G_<name>_N`
!! carries `[n_valid, n_null, n_nan]`.
!!
!! The populations themselves are recipes rather than literals -- see `golden_fixture` in
!! test/test_stats.f90, and `G_PROBE`, which is what reports a recipe that has drifted.
module test_stats_golden
    use iso_fortran_env, only : int64, real64
    implicit none
    public

    !> How many tier-A quantities each case carries.
    integer, parameter :: NQ = 9
    !> What each slot of a `G_*` array holds, in order.
    character(len=8), parameter :: QNAME(NQ) = [character(len=8) :: &
        "sum", "mean", "var", "stddev", "sem", "skew", "kurt", "min", "max"]

    !> How many probabilities each `Q_*` row carries.
    integer, parameter :: NQP = 7

    !> How many elements each `Z_*` row probes.
    integer, parameter :: NZP = 4

    !> How many positions each `K_*` cumulative row is read at.
    integer, parameter :: NKP = 5

    !> How many bins the `H_*` histogram rows carry.
    integer, parameter :: NHB = 4

    !> How many values the exact-boundary binning case carries. Its edge array holds one fewer,
    !> which is what makes `NHBX - 2` the number of bins.
    integer, parameter :: NHBX = 6

    !> How many bins the `B_*` edge rows describe.
    integer, parameter :: NBE = 5
'''


def emit():
    """Return the whole generated file as text."""
    out = [HEADER]
    probe_n = 8
    out.append("    !> The first %d values `golden_fixture` must produce. A recipe that has "
               "drifted from the" % probe_n)
    out.append("    !! generator's is reported here, as a fixture mismatch, rather than as an "
               "unexplained")
    out.append("    !! tolerance failure in every case at once.")
    out += wrap_array("    real(real64), parameter :: G_PROBE(%d) =" % probe_n,
                      [fortran_real(v) for v in fixture(probe_n)])
    out.append("")

    for name, doc, case in CASES:
        kwargs = dict(case)                      # never mutate CASES: emit() runs more than once
        m = model(kwargs.pop("values"), **kwargs)
        vals = [m[q] for q in QUANTITIES]
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: G_%s(NQ) =" % name,
                          [fortran_real(v) for v in vals])
        out += wrap_array("    logical, parameter :: G_%s_DEF(NQ) =" % name,
                          [".true." if v is not None and v == v else ".false." for v in vals])
        out.append("    integer(int64), parameter :: G_%s_N(3) = [%d_int64, %d_int64, %d_int64]"
                   % (name, m["n_valid"], m["n_null"], m["n_nan"]))
        out.append("")

    out.append("    !> The probabilities every Q_* row below is evaluated at.")
    out += wrap_array("    real(real64), parameter :: G_QPROBS(NQP) =",
                      [fortran_real(mpf(repr(p))) for p in QPROBS])
    out.append("")
    for name, doc, case in QCASES:
        kwargs = dict(case)
        vals = quantile_model(kwargs.pop("values"), **kwargs)
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: Q_%s(NQP) =" % name,
                          [fortran_real(v) for v in vals])
        out.append("")

    out.append("    !> Each M_* row is `[scale=\"normal\", scale=\"raw\"]`, so their ratio pins the "
               "normal scale factor.")
    for name, doc, case in MADCASES:
        kwargs = dict(case)
        vals = mad_model(kwargs.pop("values"), **kwargs)
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: M_%s(2) =" % name,
                          [fortran_real(v) for v in vals])
        out.append("")

    out.append("    !> The second sample the two-sample rows are taken against.")
    out += wrap_array("    real(real64), parameter :: G_PROBE_B(%d) =" % probe_n,
                      [fortran_real(v) for v in fixture_b(probe_n)])
    out.append("    !> The first values of the strictly positive population.")
    out += wrap_array("    real(real64), parameter :: G_PROBE_POS(%d) =" % probe_n,
                      [fortran_real(v) for v in fixture_pos(probe_n)])
    out.append("")
    out.append("    !> Each R_* row is `[cov, pearson, spearman]` over the two n=32 recipes.")
    xa, ya = fixture(32), fixture_b(32)
    for name, doc, case in RCASES:
        kwargs = dict(case)
        vals = [cov_model(xa, ya, **kwargs),
                corr_model(xa, ya, **kwargs),
                corr_model(xa, ya, spearman=True, **kwargs)]
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: R_%s(3) =" % name,
                          [fortran_real(v) for v in vals])
        out.append("")
    out.append("    !> The 1-based positions Z_U32 probes, and the standardised values there.")
    out.append("    integer, parameter :: G_ZPROBES(NZP) = [%s]"
               % ", ".join(str(k) for k in ZPROBES))
    out += wrap_array("    real(real64), parameter :: Z_U32(NZP) =",
                      [fortran_real(v) for v in zscore_model(xa, ZPROBES)])
    out.append("")
    out.append("    !> Each P_* row is `[gmean, hmean]` over the strictly positive population.")
    xp = fixture_pos(32)
    for name, doc, case in PCASES:
        kwargs = dict(case)
        vals = [power_model(xp, False, **kwargs), power_model(xp, True, **kwargs)]
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: P_%s(2) =" % name,
                          [fortran_real(v) for v in vals])
        out.append("")

    out.append("    !> The first values of the sigma-clip fixture: the recipe with four wild points.")
    out += wrap_array("    real(real64), parameter :: G_PROBE_CLIP(%d) =" % probe_n,
                      [fortran_real(v) for v in fixture_clip(probe_n)])
    out.append("    !> Each C_* row is `[mean, median, stddev]` of the survivors.")
    xc = fixture_clip(400)
    for name, doc, case in CCASES:
        vals = clip_model(xc, **dict(case))
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: C_%s(3) =" % name,
                          [fortran_real(v) for v in vals])
        out.append("")

    out.append("    !> The 1-based positions the K_* cumulative rows are read at.")
    out.append("    integer, parameter :: G_KPROBES(NKP) = [%s]"
               % ", ".join(str(k) for k in KPROBES))
    xk = fixture(32)
    valid7 = [(i % 7) != 0 for i in range(1, 33)]
    for op, tag in (("sum", "SUM"), ("prod", "PROD"), ("max", "MAX"), ("min", "MIN")):
        full = cum_model(xk, op)
        out.append("    !> The running %s of the n=32 recipe, at G_KPROBES." % op)
        out += wrap_array("    real(real64), parameter :: K_%s(NKP) =" % tag,
                          [fortran_real(full[k - 1]) for k in KPROBES])
    out.append("    !> The same running sum with every 7th element null: the running value must")
    out.append("    !> carry PAST a null unchanged rather than restart or be poisoned by it.")
    nulled = cum_model(xk, "sum", is_valid=valid7)
    out += wrap_array("    real(real64), parameter :: K_SUM_NULL(NKP) =",
                      [fortran_real(nulled[k - 1]) for k in KPROBES])
    out.append("")

    out.append("    !> The bin edges the H_* rows are taken against.")
    out += wrap_array("    real(real64), parameter :: G_HEDGES(NHB + 1) =",
                      [fortran_real(mpf(repr(e))) for e in HEDGES])
    ed = [mpf(repr(e)) for e in HEDGES]
    wk = weights_mod5(32)
    for tag, doc, kwargs in (
            ("LEFT", "numpy's convention: bin k is [edges(k), edges(k+1)), last closed at the top.",
             {}),
            ("RIGHT", "pandas' convention: bin k is (edges(k), edges(k+1)], first closed at the "
             "bottom.", {"right": True}),
            ("WT", "the weighted counts, `w(i) = mod(i, 5)`, so every 5th element weighs nothing.",
             {"weights": wk})):
        _, counts, _, _, nout = bin_model(xk, ed, **kwargs)
        out.append("    !> %s" % doc)
        out += wrap_array("    real(real64), parameter :: H_%s(NHB) =" % tag,
                          [fortran_real(c) for c in counts])
        out.append("    integer, parameter :: H_%s_OUT = %d" % (tag, nout))
    out.append("    !> The same bins as a DENSITY: each count by its own width and by the total")
    out.append("    !> that was binned, so `sum(H_DENS * widths)` is 1.")
    _, dens, _, _, _ = bin_model(xk, ed, density=True)
    out += wrap_array("    real(real64), parameter :: H_DENS(NHB) =",
                      [fortran_real(c) for c in dens])
    _, densw, _, _, _ = bin_model(xk, ed, density=True, weights=wk)
    out.append("    !> And weighted, where the base is the sum of the WEIGHTS that were binned.")
    out += wrap_array("    real(real64), parameter :: H_DENS_WT(NHB) =",
                      [fortran_real(c) for c in densw])
    out.append("")
    out.append("    !> `pf_bin_edges` over the n=32 recipe: NBE bins spanning its own range.")
    be, _ = bin_edges_model(xk, NBE)
    out += wrap_array("    real(real64), parameter :: B_EDGES(NBE + 1) =",
                      [fortran_real(v) for v in be])
    # **The mask has to remove the EXTREMES, and the obvious every-7th one does not.** The recipe
    # takes its minimum at 1 and its maximum at 11, so nulling every 7th element leaves the range
    # untouched and the row is byte-identical to the one above -- a golden row that asserts
    # nothing. The range is the ONLY thing an exclusion can move here, so the mask has to aim at
    # it directly.
    valid_ext = [(i not in (1, 11)) for i in range(1, 33)]
    out.append("    !> The same with the population's own minimum and maximum excluded, which is")
    out.append("    !> the only kind of exclusion a RANGE can see.")
    be, _ = bin_edges_model(xk, NBE, is_valid=valid_ext)
    out += wrap_array("    real(real64), parameter :: B_EDGES_NULL(NBE + 1) =",
                      [fortran_real(v) for v in be])
    out.append("    !> numpy's two degenerate answers: a CONSTANT population widens by half a")
    out.append("    !> unit either way, and an EMPTY one falls back to [0, 1].")
    be, _ = bin_edges_model([mpf(7)] * 5, NBE)
    out += wrap_array("    real(real64), parameter :: B_CONST(NBE + 1) =",
                      [fortran_real(v) for v in be])
    be, _ = bin_edges_model([], NBE)
    out += wrap_array("    real(real64), parameter :: B_EMPTY(NBE + 1) =",
                      [fortran_real(v) for v in be])
    out.append("")
    out.append("    !> The exact-boundary case: every interior edge lands ON one of these values,")
    out.append("    !> which is the only way the two conventions can be told apart at an edge.")
    out += wrap_array("    real(real64), parameter :: G_HBX(NHBX) =",
                      [fortran_real(mpf(repr(v))) for v in HBX])
    out += wrap_array("    real(real64), parameter :: G_HBE(NHBX - 1) =",
                      [fortran_real(mpf(repr(e))) for e in HBE])
    hbe = [mpf(repr(e)) for e in HBE]
    hbx = [mpf(repr(v)) for v in HBX]
    for tag, right in (("LEFT", False), ("RIGHT", True)):
        codes, counts, _, _, nout = bin_model(hbx, hbe, right=right)
        out.append("    !> Its codes and bin counts under the %s-closed convention."
                   % ("upper" if right else "lower"))
        out.append("    integer, parameter :: HB_%s(NHBX) = [%s]"
                   % (tag, ", ".join(str(c) for c in codes)))
        out.append("    integer, parameter :: HB_%s_N(NHBX - 2) = [%s]"
                   % (tag, ", ".join(str(int(c)) for c in counts)))
        out.append("    integer, parameter :: HB_%s_OUT = %d" % (tag, nout))
    out.append("")

    out.append("end module test_stats_golden ! GCOVR_EXCL_LINE")
    text = "\n".join(out) + "\n"
    for lineno, line in enumerate(text.split("\n"), start=1):
        if len(line) > 132:
            raise SystemExit("generate_stats_vectors.py: emitted line %d is %d columns"
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
            print("generate_stats_vectors.py --check: %s is out of date. Re-run "
                  "tools/generate_stats_vectors.py." % OUT_PATH.relative_to(REPO_ROOT),
                  file=sys.stderr)
            return 1
        print("generate_stats_vectors.py --check: %s carries all %d cases, current."
              % (OUT_PATH.relative_to(REPO_ROOT), len(CASES)))
        return 0
    OUT_PATH.write_text(text)
    print("generate_stats_vectors.py: wrote %s (%d cases, mpmath at %d digits)"
          % (OUT_PATH.relative_to(REPO_ROOT), len(CASES), mp.dps))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
