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
