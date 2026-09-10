#!/usr/bin/env python3
"""Golden expectations for `pf_probit` and the normal family, from a 50-digit `mpmath` oracle.

`parquet_utils`' normal family claims about 2 ulp over the whole range of `real64`, including into
the subnormal tail. A claim of that shape can only be checked against a reference computed to more
digits than the answer has, so the committed expectations here come from `mpmath`, and scipy
appears only in `--self-test` as a cross-check that the model transcribes the same definitions
scipy implements.

**The oracle never forms `2p - 1`, and that is the whole reason it is written this way.** The
obvious route to `Phi^-1(p)` is `sqrt(2)*erfinv(2p - 1)`, and for `p = 1e-300` the subtraction
gives exactly `-1` at any working precision below 300 digits -- so the oracle answers `-Infinity`
for every deep-tail point and does it silently. This solves `erfc(t) = 2q` for `t` instead, where
`q = min(p, 1-p)`: both sides are tiny together and nothing cancels. The root is bracketed by
bisection over `[0, 45]` before Newton refines it, so the oracle needs no starting estimate and
cannot be walked off a cliff by one -- which matters, because a root-finder seeded from a float
approximation is exactly as good as that approximation in the range where it is worst.

Emitted (COMMITTED to the repository, like every other generator's output -- nothing is generated
at build time, so the fpm build stays dependency-free):

  test/test_probit_golden.f90    module `test_probit_golden`, read by the `utils` suite.

Usage:  tools/generate_probit_reference.py [--check] [--self-test]

  --check      regenerate into memory and compare with the committed file; exit 1 on difference.
  --self-test  cross-check the oracle against scipy, re-derive the `erfinv` coefficients that
               `src/parquet_utils.f90` carries, confirm the frozen `MAD_NORMAL_SCALE` in
               `src/parquet_stats_order.f90` is the double nearest `1/Phi^-1(3/4)` exactly, and
               confirm every emitted literal round-trips to the same double it was made from.

`--self-test` needs scipy; plain generation and `--check` need only `mpmath`. `--check` runs in
CI's lint stage, which installs Ubuntu's `python3-mpmath` for exactly that.

**Every number here is computed in `mpmath` at 50 digits, never in float.** The lesson is
`tools/generate_parquet_ziggurat.py`'s: a generator that reaches for `math.erfc` produces a
different table on a different libm, and `--check` then fails in CI against a file generated on a
developer machine -- with the table it produced there being a different table, not a rounding of
the same one.

Maintainer-only: it is never run at build time and is stripped from the fpm-published package.
"""

import math
import re
import sys
from pathlib import Path

try:
    from mpmath import mp, mpf, erfc as merfc, exp as mexp, sqrt as msqrt, pi as mpi
except ImportError:                                            # pragma: no cover - maintainer tool
    print("generate_probit_reference.py: needs mpmath (pip install mpmath)", file=sys.stderr)
    raise SystemExit(2)

mp.dps = 50

REPO_ROOT = Path(__file__).resolve().parent.parent
OUT_PATH = REPO_ROOT / "test" / "test_probit_golden.f90"

#: The bracket the bisection starts from. `erfc(45)` is below the smallest subnormal double, so
#: every `q` a `real64` can hold has its root strictly inside; `erfc(0)` is 1, so `q = 1/2` sits at
#: the low end. Widening it costs nothing but is not needed, and narrowing it would silently clamp
#: the deep tail rather than fail.
T_HI = 45


def oracle_t(q):
    """The `t >= 0` with `erfc(t) = 2q`, to 50 digits. `Phi^-1(q) = -sqrt(2)*t` for `q <= 1/2`."""
    target = 2 * mpf(q)
    lo, hi = mpf(0), mpf(T_HI)
    # Bisection first, and unconditionally: it needs no starting estimate, so the oracle's
    # robustness does not rest on a float approximation being good in the range where it is worst.
    for _ in range(70):
        mid = (lo + hi) / 2
        if merfc(mid) > target:
            lo = mid
        else:
            hi = mid
    t = (lo + hi) / 2
    # Newton on erfc: t <- t + (erfc(t) - 2q)*sqrt(pi)/2*exp(t*t). Quadratic from ~1e-21.
    for _ in range(4):
        t = t + (merfc(t) - target) * msqrt(mpi) / 2 * mexp(t * t)
    return t


def probit(p):
    """`Phi^-1(p)` at 50 digits, for a float `p` strictly inside `(0, 1)`."""
    if p == 0.5:
        return mpf(0)
    q = mpf(p) if p < 0.5 else 1 - mpf(p)
    t = oracle_t(q)
    return -msqrt(2) * t if p < 0.5 else msqrt(2) * t


def norm_cdf(z):
    """`Phi(z)` at 50 digits."""
    return merfc(-mpf(z) / msqrt(2)) / 2


def norm_sf(z):
    """`1 - Phi(z)` at 50 digits, as the upper tail rather than as that subtraction."""
    return merfc(mpf(z) / msqrt(2)) / 2


def norm_pdf(z):
    """`phi(z)` at 50 digits."""
    return mexp(-mpf(z) * mpf(z) / 2) / msqrt(2 * mpi)


# --------------------------------------------------------------------------------------
# The grids
# --------------------------------------------------------------------------------------

#: Where `pf_probit` changes branch, as a value of `q = min(p, 1-p)`. Mirrors
#: `PROBIT_QSPLIT_R64` in `src/parquet_utils.f90`; `--self-test` checks the two agree, because a
#: grid that no longer straddles the seam would stop testing the one place a branch defect hides.
QSPLIT = 0.1


def probit_grid():
    """The probabilities `pf_probit` is pinned at, sorted and free of duplicates.

    Chosen so that every branch, both sides of the seam, the centre and the subnormal tail each
    carry several points: a grid that samples only the comfortable middle passes against a
    kernel whose tail is wrong by a factor of two.
    """
    ps = []
    # The seam itself and the two doubles either side of it, taken with `nextafter` rather than
    # written as decimals: `"%.17g" % 0.1` is `0.10000000000000001`, which is the SAME double, so
    # a hand-written pair of neighbours silently lands on one side twice.
    for base in (QSPLIT, 1.0 - QSPLIT):
        below = math.nextafter(base, 0.0)
        ps += [math.nextafter(below, 0.0), below, base,
               math.nextafter(base, 1.0), math.nextafter(math.nextafter(base, 1.0), 1.0)]
    # The central branch, including points close enough to 1/2 that a formulation with a
    # cancellation there would show it.
    ps += [0.5 - 10.0 ** -k for k in range(1, 17)]
    ps += [0.5 + 10.0 ** -k for k in range(1, 16)]
    ps += [0.11, 0.125, 0.15, 0.2, 0.25, 1.0 / 3.0, 0.375, 0.4, 0.45, 0.49]
    ps += [0.51, 0.55, 0.6, 0.625, 2.0 / 3.0, 0.75, 0.8, 0.875, 0.89]
    # The tail branch, one point per decade plus a second off-decade point, down through the
    # smallest normal double and into the subnormals.
    for e in list(range(2, 25)) + [30, 40, 60, 80, 120, 160, 200, 250, 290, 300, 305, 307, 308]:
        ps.append(10.0 ** -e)
        ps.append(3.7 * 10.0 ** -e)
    ps += [2.2250738585072014e-308,     # the smallest normal double
           1e-310, 1e-315, 1e-320,      # subnormals: the density thins out but the answer is real
           5e-324]                      # the smallest subnormal there is
    # The upper mirrors, as far as a double can distinguish 1 - q from 1.
    ps += [1.0 - 10.0 ** -k for k in range(2, 17)]
    return sorted({p for p in ps if 0.0 < p < 1.0})


def z_grid():
    """The quantiles `pf_norm_cdf`, `pf_norm_sf` and `pf_norm_pdf` are pinned at.

    The positive end runs far enough out that `1 - Phi(z)` has no significant digits left, which
    is the range `pf_norm_sf` exists for and the one an implementation written as that subtraction
    fails in.
    """
    zs = [0.0, 0.25, 0.5, 0.75, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0,
          10.0, 12.0, 15.0, 20.0, 25.0, 30.0, 37.0, 38.0]
    return sorted({z for z in zs} | {-z for z in zs})


def fortran_real(v):
    """`v` as a round-trip-exact Fortran `real64` literal."""
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
! Regenerate with:  tools/generate_probit_reference.py
!
!> Golden expectations for `parquet_utils`' normal family, derived at 50 decimal digits by
!! `mpmath`.
!!
!! **The oracle shares no arithmetic with the library, and none with scipy either.** `pf_probit`
!! claims about 2 ulp, and a reference computed in the same double precision as the thing under
!! test cannot certify a claim of that size. Every value here comes from `mpmath` solving
!! `erfc(t) = 2q` by bracketed bisection and Newton -- never through `2p - 1`, which cancels to
!! exactly -1 for any `p` below about 1e-17 and would silently make the whole deep tail
!! `-Infinity`. scipy appears only in the generator's `--self-test`.
!!
!! `P_GRID`/`PROBIT_GRID` pin `pf_probit`, and `Z_GRID` with `CDF_GRID`/`SF_GRID`/`PDF_GRID` pin
!! the three forward functions. The grids straddle the branch seam at `q = 0.1` deliberately: a
!! grid sampling only the comfortable middle passes against a kernel whose tail is wrong.
!!
!! `PROBIT_Q3` and `MAD_NORMAL_SCALE_REF` are the two spellings of one number that
!! `feature_risks.md` Risk-252 is about -- `src/parquet_stats_order.f90` freezes the second as a
!! literal, and nothing but a test compares them.
module test_probit_golden
    use iso_fortran_env, only : real64
    implicit none
    public
'''


def emit():
    """Return the whole generated file as text."""
    ps = probit_grid()
    zs = z_grid()
    out = [HEADER]
    out.append("    !> How many probabilities `P_GRID` carries.")
    out.append("    integer, parameter :: NP = %d" % len(ps))
    out.append("    !> How many quantiles `Z_GRID` carries.")
    out.append("    integer, parameter :: NZ = %d" % len(zs))
    out.append("")
    out.append("    !> Where `pf_probit` changes branch, as a value of `min(p, 1-p)`. Mirrors")
    out.append("    !! `PROBIT_QSPLIT_R64` in `src/parquet_utils.f90`; the grids straddle it.")
    out.append("    real(real64), parameter :: QSPLIT = %s" % fortran_real(QSPLIT))
    out.append("")
    out.append("    !> The probabilities `PROBIT_GRID` is taken at: both branches, both sides of")
    out.append("    !! the seam, the centre, and the tail down to the smallest subnormal double.")
    out += wrap_array("    real(real64), parameter :: P_GRID(NP) =", [fortran_real(p) for p in ps])
    out.append("")
    out.append("    !> `Phi**(-1)(P_GRID)` at 50 digits, rounded to the nearest double.")
    out += wrap_array("    real(real64), parameter :: PROBIT_GRID(NP) =",
                      [fortran_real(probit(p)) for p in ps])
    out.append("")
    out.append("    !> The quantiles the three forward functions are taken at.")
    out += wrap_array("    real(real64), parameter :: Z_GRID(NZ) =", [fortran_real(z) for z in zs])
    out.append("")
    out.append("    !> `Phi(Z_GRID)` at 50 digits.")
    out += wrap_array("    real(real64), parameter :: CDF_GRID(NZ) =",
                      [fortran_real(norm_cdf(z)) for z in zs])
    out.append("")
    out.append("    !> `1 - Phi(Z_GRID)` at 50 digits. Above about `z = 8` this has no digits in")
    out.append("    !! common with `1 - CDF_GRID`, which is what `pf_norm_sf` exists to give.")
    out += wrap_array("    real(real64), parameter :: SF_GRID(NZ) =",
                      [fortran_real(norm_sf(z)) for z in zs])
    out.append("")
    out.append("    !> `phi(Z_GRID)` at 50 digits. Zero where the density underflows a double.")
    out += wrap_array("    real(real64), parameter :: PDF_GRID(NZ) =",
                      [fortran_real(norm_pdf(z)) for z in zs])
    out.append("")
    out.append("    !> `Phi**(-1)(3/4)`, and its reciprocal -- the scale `pf_mad(scale=\"normal\")`")
    out.append("    !! multiplies by, frozen as a literal in `src/parquet_stats_order.f90`.")
    out.append("    real(real64), parameter :: PROBIT_Q3 = %s" % fortran_real(probit(0.75)))
    out.append("    real(real64), parameter :: MAD_NORMAL_SCALE_REF = %s"
               % fortran_real(1 / probit(0.75)))
    out.append("")
    out.append("end module test_probit_golden")
    return "\n".join(out) + "\n"


# --------------------------------------------------------------------------------------
# --self-test: the oracle against scipy, and the two constants the sources carry
# --------------------------------------------------------------------------------------

def _read_source_reals(path, name, count):
    """Pull `count` real64 literals out of the `name` parameter declaration in `path`."""
    text = path.read_text()
    m = re.search(r"parameter\s*::\s*%s\s*(?:\([^)]*\))?\s*=(.*?)(?:\n\s*(?:!|real|integer|end))"
                  % re.escape(name), text, re.S)
    if not m:
        return None
    body = re.sub(r"&\s*\n", " ", m.group(1))
    body = re.sub(r"!.*", "", body)
    vals = re.findall(r"[-+]?\d*\.?\d+(?:[eEdD][-+]?\d+)?_real64", body)
    out = [float(v.replace("_real64", "").replace("d", "e").replace("D", "e")) for v in vals]
    return out if len(out) == count else None


def erfinv_coeffs(n):
    """The first `n` Maclaurin coefficients of `erfinv`, by SERIES REVERSION of `erf`.

    Re-derived rather than tabulated, so that `--self-test` is a check on provenance as well as on
    value: what `src/parquet_utils.f90` carries has to be the reversion of `erf`'s own series and
    not a coefficient set from somewhere with its own licence.
    """
    size = 2 * n + 2
    a = [mpf(0)] * (size + 1)
    for k in range(n + 2):
        j = 2 * k + 1
        if j <= size:
            a[j] = mpf(-1) ** k / (mp.factorial(k) * (2 * k + 1)) * 2 / msqrt(mpi)
    b = [mpf(0)] * (size + 1)
    b[1] = 1 / a[1]
    for m in range(2, size + 1):
        xs = [mpf(0)] * (size + 1)
        xs[:m] = b[:m]
        cur = [mpf(0)] * (size + 1)
        cur[0] = mpf(1)
        rest = mpf(0)
        for power in range(1, m + 1):
            new = [mpf(0)] * (size + 1)
            for i in range(size + 1):
                if cur[i] == 0:
                    continue
                for j in range(1, size + 1 - i):
                    if xs[j] == 0:
                        continue
                    new[i + j] += cur[i] * xs[j]
            cur = new
            if a[power] != 0:
                rest += a[power] * cur[m]
        b[m] = -rest / a[1]
    return [b[2 * k + 1] for k in range(n)]


def self_test():
    """Cross-check the oracle, the committed coefficients and the frozen MAD scale."""
    fails = 0
    checks = 0

    # 1. Every emitted literal must parse back to the double it was made from. A grid point that
    #    does not round-trip is a test comparing the library against a different input.
    for p in probit_grid() + z_grid():
        checks += 1
        if float(fortran_real(p).replace("_real64", "")) != float(p):
            print("--self-test: %r does not round-trip through its emitted literal" % p,
                  file=sys.stderr)
            fails += 1

    # 2. The oracle against scipy, which computes these a completely different way.
    try:
        from scipy.special import ndtri, erfc as sperfc
        from scipy.stats import norm
    except ImportError as exc:                                 # pragma: no cover - maintainer tool
        print("--self-test: needs scipy (%s)" % exc, file=sys.stderr)
        return 1
    for p in probit_grid():
        want = ndtri(p)
        if want != want or abs(want) == float("inf"):
            continue                                           # scipy's own range, not ours
        got = float(probit(p))
        checks += 1
        if got != want and abs(got - want) > 4 * abs(want) * 2.3e-16:
            print("--self-test: probit(%r): oracle %.17g vs scipy %.17g" % (p, got, want),
                  file=sys.stderr)
            fails += 1
    # The forward functions are compared LOOSELY on purpose. This step asks whether the oracle
    # transcribes the same definitions scipy implements, not which of the two is more accurate --
    # and in the far tail the answer to the second question is the oracle, by construction. scipy
    # is about 1e-12 relative at z = -37 and returns a flat 0 from z = -38 out, where the true
    # value is a perfectly ordinary subnormal double. Tightening this to a few ulp would be
    # asserting that a double-precision library matches a 50-digit one, which it cannot.
    tiny_normal = 2.2250738585072014e-308
    for z in z_grid():
        for label, ours, theirs in (("cdf", norm_cdf(z), norm.cdf(z)),
                                    ("sf", norm_sf(z), norm.sf(z)),
                                    ("pdf", norm_pdf(z), norm.pdf(z))):
            got, want = float(ours), float(theirs)
            checks += 1
            if want == 0.0:
                if got > tiny_normal:
                    print("--self-test: %s(%r): oracle %.17g vs scipy 0, and the oracle's value "
                          "is a NORMAL double -- scipy did not merely underflow"
                          % (label, z, got), file=sys.stderr)
                    fails += 1
            elif abs(got - want) > 1e-9 * abs(want):
                print("--self-test: %s(%r): oracle %.17g vs scipy %.17g (rel %.3g)"
                      % (label, z, got, want, abs(got - want) / abs(want)), file=sys.stderr)
                fails += 1

    # 3. The erfinv coefficients the kernel starts from, re-derived by reversion. They decide only
    #    how fast pf_probit converges, never what it answers -- but a coefficient set that came
    #    from somewhere else is a provenance question this library cannot leave open.
    src = REPO_ROOT / "src" / "parquet_utils.f90"
    committed = _read_source_reals(src, "ERFINV_C_R64", 6)
    checks += 1
    if committed is None:
        print("--self-test: could not read ERFINV_C_R64 out of %s -- this check has gone blind"
              % src.name, file=sys.stderr)
        fails += 1
    else:
        for k, (have, want) in enumerate(zip(committed, erfinv_coeffs(6))):
            checks += 1
            if have != float(want):
                print("--self-test: ERFINV_C_R64(%d) is %.17g; the reversion of erf gives %.17g"
                      % (k + 1, have, float(want)), file=sys.stderr)
                fails += 1

    # 4. The branch seam the grids straddle must be the one the kernel uses.
    qs = _read_source_reals(src, "PROBIT_QSPLIT_R64", 1)
    checks += 1
    if qs is None or qs[0] != QSPLIT:
        print("--self-test: PROBIT_QSPLIT_R64 is %s; this generator's grid straddles %r"
              % ("unreadable" if qs is None else "%.17g" % qs[0], QSPLIT), file=sys.stderr)
        fails += 1

    # 5. feature_risks.md Risk-252, from the generator's side: the scale pf_mad multiplies by is
    #    frozen as a literal, and the double nearest 1/Phi^-1(3/4) is what it has to be.
    order = REPO_ROOT / "src" / "parquet_stats_order.f90"
    checks += 1
    if not order.exists():
        print("--self-test: %s is missing" % order.name, file=sys.stderr)
        fails += 1
    else:
        m = re.search(r"MAD_NORMAL_SCALE\s*=\s*([0-9.]+)_real64", order.read_text())
        if not m:
            print("--self-test: could not read MAD_NORMAL_SCALE out of %s -- this check has gone "
                  "blind" % order.name, file=sys.stderr)
            fails += 1
        else:
            # EXACT equality, with no tolerance at all. Up to v2.3.0 the frozen literal was
            # `1.482602218505602` = 0x3ff7b8bd1a975674, one ulp ABOVE the nearest double to
            # 1/Phi^-1(3/4) (...673): sixteen decimal digits do not name that double uniquely, and
            # `1.0/0.6744897501960817` rounds twice -- once to the quartile, once to its
            # reciprocal. It now reads `1.4826022185056018` and is the correctly rounded value, so
            # a tolerance here would only re-admit the spelling that was wrong.
            have, want = float(m.group(1)), float(1 / probit(0.75))
            checks += 1
            if have != want:
                print("--self-test: MAD_NORMAL_SCALE is %.17g; the double nearest 1/Phi^-1(3/4) "
                      "is %.17g -- they differ by %.1f ulp and must not differ at all"
                      % (have, want, abs(have - want) / math.ulp(want)), file=sys.stderr)
                fails += 1

    if fails:
        print("generate_probit_reference.py --self-test: %d of %d checks FAILED"
              % (fails, checks), file=sys.stderr)
        return 1
    print("generate_probit_reference.py --self-test: %d checks agree (oracle vs scipy, the "
          "erfinv coefficients re-derived by reversion, the branch seam, and the frozen "
          "MAD_NORMAL_SCALE)" % checks)
    return 0


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
            print("generate_probit_reference.py --check: %s is out of date. Re-run "
                  "tools/generate_probit_reference.py." % OUT_PATH.relative_to(REPO_ROOT),
                  file=sys.stderr)
            return 1
        print("generate_probit_reference.py --check: %s carries %d probabilities and %d "
              "quantiles, current."
              % (OUT_PATH.relative_to(REPO_ROOT), len(probit_grid()), len(z_grid())))
        return 0
    OUT_PATH.write_text(text)
    print("generate_probit_reference.py: wrote %s (%d probabilities, %d quantiles, mpmath at %d "
          "digits)" % (OUT_PATH.relative_to(REPO_ROOT), len(probit_grid()), len(z_grid()), mp.dps))
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
