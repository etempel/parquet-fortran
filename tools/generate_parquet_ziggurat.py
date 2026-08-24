#!/usr/bin/env python3
"""Generate `src/parquet_ziggurat.f90` -- the layer tables behind `pf_random_normal_at`.

The Ziggurat method covers the half-normal density `f(x) = exp(-x*x/2)` with `N` regions of
EQUAL area: one base strip that carries the infinite tail, and `N-1` rectangles stacked above it.
Which regions those are is decided by two numbers, `R` (where the tail begins) and `V` (the shared
area), and those two are decided by nothing at all -- they are whatever makes the construction
close exactly at the peak.  So the table is not a set of magic constants: it is the unique
solution of one equation, re-derived here rather than pasted from a paper.

Emitted (COMMITTED to the repository, like every other generator's output -- nothing is generated
at build time, so the fpm build stays dependency-free):

  src/parquet_ziggurat.f90    module `parquet_ziggurat`: zig_w, zig_k, zig_f, zig_r, zig_layers.

Usage:  tools/generate_parquet_ziggurat.py [--check] [--self-test]

  --check      regenerate into memory and compare with the committed file; exit 1 on difference.
  --self-test  re-derive the construction's defining properties -- equal areas, the closure at
               the peak, the published (R, V) pair, monotonicity, the acceptance ratio, and that
               the answer does not depend on the working precision -- and exit 1 if any fails.

EVERY NUMBER HERE IS COMPUTED IN `decimal`, NOT IN FLOAT, AND THAT IS THE WHOLE DIFFERENCE BETWEEN
A GENERATOR ANYONE CAN VERIFY AND ONE THAT ONLY WORKS ON THE MACHINE THAT RAN IT.  The first
version of this script used `math.exp`, `math.log`, `math.sqrt` and `math.erfc` -- all libm, none
of them correctly rounded, and none of them identical between glibc versions.  `--check` therefore
FAILED IN CI against a file generated on a developer machine, which is the mildest symptom
available: the table it produced there was a different table, not a rounding of the same one.

The amplification is what makes that fatal rather than cosmetic.  The construction walks 255 layers
from `R` downward, each step feeding the next, so a **one-ulp** change in `R` moves **254 of the
255 widths** -- measured, not estimated.  `R` itself comes out of a bisection whose residual is a
libm expression, so a last-bit disagreement anywhere in libm re-rolls essentially the whole table.
The float version's accumulated drift was visible in the result: its narrowest layer came out
0.2152418959849138 against the correct 0.2152418959848817, wrong in the 13th digit.

`decimal` is exact arithmetic at a stated precision with correctly-rounded `exp`, `ln` and `sqrt`,
so it gives the same digits on every platform and every Python build.  The tail integral has no
`decimal` equivalent of `erfc`, so it is summed from its own Taylor series below.  Rounding to
`real64` happens once, at emission.

Three rules follow for anyone editing this file:

  * **Do not reintroduce `math.` anywhere in the derivation.**  It will appear to work, and the
    committed table will silently become whatever the last machine to regenerate it produced.
  * **Do not reduce a list of floats either -- not `sum()`, not `min`/`max` over a running
    total.**  This is the same hazard in different clothes and it bit later, in the one place the
    rule above did not name: `acceptance_rate` computed `sum(k) / len(k)` over 256 doubles, which
    is deterministic for a given interpreter and NOT across interpreters -- CPython 3.12 switched
    builtin `sum()` over floats to Neumaier compensated summation, so 3.13 emits a last bit that
    3.11 does not.  `--check` then failed on a machine that had changed nothing but its Python.
    Sum in `decimal` and round once, as everything else here does.
  * **`--self-test` re-derives the whole thing at two different working precisions and requires
    identical doubles.**  That is the check that would have caught the original defect, and it is
    the reason the search precision below can be chosen for speed without anyone having to argue
    about whether it is enough.  Note it would NOT have caught the second one: it compares the
    TABLES across two precisions, and both arms would have called the same `sum()`.  A value that
    is derived but not tabulated has only `--check` standing behind it.

WHY THIS IS A GENERATOR AND NOT A PASTED TABLE.  A ziggurat table is 771 floating-point constants
whose only correctness argument is the construction that produced them.  A pasted table can be
transcribed with one digit wrong and still look entirely plausible: every value is near its
neighbours, the algorithm still terminates, and the resulting distribution is off by an amount no
casual test would see.  Deriving them here means `--self-test` can check the PROPERTY (all regions
have area V, and the top one closes at f = 1) rather than the digits.

THE TABLES, AND WHAT EACH INDEX MEANS.  Layers are numbered 0 .. N-1 with layer 0 the BASE STRIP
at the bottom and layer 1 the topmost, narrowest rectangle containing the peak.  That ordering
looks upside down and is what the algorithm wants: the layer index comes straight out of 8 random
bits, and the base strip -- the only one needing special handling -- lands on index 0.

  zig_w(i)  the layer's half-width.  A draw is `u * zig_w(i)` for a uniform `u` in [0, 1).
            zig_w(0) is the base strip, which is WIDER than R because it must also carry the
            tail's area; zig_w(1) < zig_w(2) < ... < zig_w(N-1) = R.
  zig_k(i)  the fraction of that width covered by the inner rectangle, i.e. the part that is
            wholly under the curve and can be accepted with no further work.  This is where the
            method's speed comes from: ~99 % of draws stop at `u < zig_k(i)`.
            zig_k(1) = 0, because layer 1 reaches the peak and has no inner rectangle at all.
  zig_f(i)  `f(zig_w(i))`, the height of the curve at the layer's outer edge, used by the wedge
            test.  zig_f(0) = 1 by convention -- layer 0 never runs the wedge test, and the value
            1 is what makes layer 1's test span the full height up to the peak.

Regenerates `src/parquet_ziggurat.f90` -- the 771 layer
constants behind `pf_random_normal_at` and `%normal`. It exists because those constants are the
unique solution of one equation rather than a table anyone chose: 256 regions of equal area under
`exp(-x*x/2)`, closing exactly at the peak, which fixes `R` and `V` and everything derived from
them. A pasted table can be transcribed with one digit wrong and still look entirely plausible --
every value near its neighbours, the algorithm still terminating, the distribution off by an
amount no casual test would see -- so `--self-test` checks the property (equal areas to 1e-12,
closure at the peak, the published `R = 3.6541528853610088` and `V = 0.00492867323399`, monotonic
widths, and an immediate-acceptance rate in the 0.98-0.995 a correct 256-layer table gives) rather
than the digits. Two traps worth knowing if you re-derive it. The closure residual decreases in
`R`, because `V = R*f(R) + tail(R)` falls as `R` grows, so a larger tail means smaller steps and a
stack that runs out of layers below the peak. And every number is computed in `decimal`, never in
`float` -- the first version of this script used `math.exp`/`log`/`sqrt`/`erfc` and emitted a
different table on CI than on the machine that committed it, because a one-ulp change in `R` moves
254 of the 255 widths and libm is not identical between glibc versions (`feature_risks.md`
Risk-130). `--self-test` re-derives the whole table at a higher working precision and requires
identical doubles, which is the check that has power against that; the property checks all passed
on the broken version. `--check` verifies the committed output as usual. Maintainer-only (stripped
from the fpm-published package).
"""

import argparse
import pathlib
import sys
from decimal import Decimal, getcontext, localcontext

#: Layers.  256 is the usual choice for a 64-bit draw: the index costs 8 bits, which leaves 53 for
#: the uniform and one for the sign inside a single 64-bit pattern, so a normal draw reads exactly
#: one word pair in the common case.  Raising it would improve the acceptance rate by very little
#: and cost another bit out of the same pattern.
N_LAYERS = 256

#: Working precision for the bisection, and for the single final walk.  The search is the
#: expensive half -- one full 255-layer walk per iteration -- so it runs lower.  Neither number is
#: load-bearing: `--self-test` re-derives everything at a different pair and requires the same
#: doubles, which is what makes them a speed choice rather than an accuracy claim.
SEARCH_PREC = 30
FINAL_PREC = 45

#: Bisection steps from the bracket below.  `9.5 / 2**80` is about 8e-24, and the walk amplifies an
#: error in R by roughly three orders of magnitude, so this leaves R pinned far tighter than the
#: 2.8e-17 ulp of the narrowest width it has to produce.
SEARCH_ITERS = 80

#: pi to 75 digits.  A mathematical constant, independently checkable, and needed because the tail
#: integral is `sqrt(pi/2)` minus a series.
PI = Decimal("3.141592653589793238462643383279502884197169399375105820974944592307816406286")

WIDTH = 128           # emitted lines stay inside the project's 132-column limit

BANNER = """!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
! GENERATED FILE -- DO NOT EDIT BY HAND.
!
! Emitted by tools/generate_parquet_ziggurat.py; run that script to change anything here, and run
! it with --check to prove the committed copy is current. Editing this file directly appears to
! work, passes every test, and is silently reverted the next time anyone regenerates."""


def f(x):
    """The half-normal shape `exp(-x*x/2)`, unnormalised.  `f(0) = 1`."""
    return (-(x * x) / 2).exp()


def f_inv(y):
    """The `x >= 0` with `f(x) = y`.  Inverse of `f` on the half-line; `y` must be in (0, 1)."""
    return (-2 * y.ln()).sqrt()


def half_normal_0_to_r(r):
    """`integral_0^r exp(-x*x/2) dx`, from its Taylor series.

    `decimal` has no `erfc`, so this is summed directly: the integrand's series integrates term by
    term to `sum_n (-1)**n r**(2n+1) / (2**n n! (2n+1))`.  It alternates, so it loses digits to
    cancellation for large `r` -- about 21 of them at `r = 10`, the top of the bisection bracket --
    which is why the working precision has plenty of headroom and why `--self-test` checks the
    answer at two of them.
    """
    r2 = r * r
    term = r                                   # n = 0
    total = r
    n = 0
    guard = Decimal(10) ** (-(getcontext().prec + 5))
    while True:
        n += 1
        term = -term * r2 / (2 * n)
        add = term / (2 * n + 1)
        total += add
        if abs(add) < guard * (abs(total) + 1):
            return total


def tail_area(r):
    """Area under `f` beyond `r`, i.e. `integral_r^inf exp(-x*x/2) dx`."""
    return (PI / 2).sqrt() - half_normal_0_to_r(r)


def build(r, n=N_LAYERS):
    """Walk the ziggurat up from `r`, returning the layer edges and the closure residual.

    `v` is fixed by `r`: the base strip is the rectangle `[0, r] x [0, f(r)]` plus the tail, and
    every other region must have that same area.  Walking up, region `i` is the rectangle of
    height `f(x[i]) - f(x[i-1])` and width `x[i]`, so `x[i-1]` follows from `x[i]` by
    `f(x[i-1]) = f(x[i]) + v / x[i]`.

    Returns `(xs, v, residual)` where `xs[i]` is layer `i`'s half-width and `residual` is
    `f(xs[1]) + v/xs[1] - 1`: zero exactly when the ziggurat closes at the peak, which is the
    one equation `r` has to satisfy.
    """
    v = r * f(r) + tail_area(r)
    xs = [Decimal(0)] * n
    xs[n - 1] = r
    y = f(r)
    for i in range(n - 1, 1, -1):
        y = y + v / xs[i]
        if y >= 1:
            # `r` is too small: the stack reaches the peak before it has used every layer.
            return xs, v, y - 1 + (i - 1)
        xs[i - 1] = f_inv(y)
    xs[0] = v / f(r)                      # the base strip's width, tail area included
    return xs, v, f(xs[1]) + v / xs[1] - 1


def solve_r(n=N_LAYERS, prec=SEARCH_PREC, iters=SEARCH_ITERS):
    """Bisect for the `r` that makes the ziggurat close exactly at the peak.

    The residual DECREASES in `r`, which is what makes bisection sufficient -- and which is the
    opposite of the intuition that a larger tail means a larger step.  `v = r*f(r) + tail(r)` and
    `f` falls faster than `r` rises, so a larger `r` gives a SMALLER shared area, hence smaller
    steps, hence a stack that runs out of layers below the peak.
    """
    with localcontext() as ctx:
        ctx.prec = prec
        lo, hi = Decimal("0.5"), Decimal(10)   # residual(0.5) > 0, residual(10) < 0
        for _ in range(iters):
            mid = (lo + hi) / 2
            _, _, residual = build(mid, n)
            if residual > 0:
                lo = mid
            else:
                hi = mid
        return +((lo + hi) / 2)


def tables(n=N_LAYERS, search_prec=SEARCH_PREC, final_prec=FINAL_PREC):
    """`(r, v, w, k, fy)` for the solved ziggurat, as `Decimal` -- the five things published."""
    r = solve_r(n, prec=search_prec)
    with localcontext() as ctx:
        ctx.prec = final_prec
        w, v, _ = build(+Decimal(r), n)
        k = [Decimal(0)] * n
        # Layer i accepts immediately when `u < k(i)`, i.e. when the draw falls inside layer i-1's
        # width, which is wholly under the curve. Layer 1 has no layer below it that is narrower --
        # it contains the peak -- so it never accepts immediately.
        k[0] = r / w[0]                   # the base strip's rectangle part; beyond it is the tail
        k[1] = Decimal(0)
        for i in range(2, n):
            k[i] = w[i - 1] / w[i]
        fy = [f(x) for x in w]
        fy[0] = Decimal(1)                # never used by layer 0; makes layer 1's wedge reach the peak
        return r, v, w, k, fy


def as_floats(n=N_LAYERS, search_prec=SEARCH_PREC, final_prec=FINAL_PREC):
    """`tables()` rounded to `real64` -- the only place the derivation meets binary floating point."""
    r, v, w, k, fy = tables(n, search_prec, final_prec)
    return float(r), float(v), [float(x) for x in w], [float(x) for x in k], [float(x) for x in fy]


def acceptance_rate(k):
    """Fraction of CANDIDATES accepted without a wedge test or a tail walk.

    Each layer is chosen with probability 1/n and accepts immediately with probability `k(i)`, so
    this is just their mean.  Reported by `--self-test` because a table that is subtly wrong often
    still terminates -- it just does so more slowly, and nothing else would notice.

    **Per CANDIDATE, not per returned value**, and the two differ by more than they look.  A
    candidate that fails the wedge test is discarded and redrawn, so it never appears in a census
    of "which branch produced this value": conditioning on acceptance raises the observed
    immediate-acceptance share from this 0.985 to about 0.992.  Measured, not derived -- a path
    census over 200000 draws gives 0.9918 / 0.0080 / 0.00024 for rectangle / wedge / tail.

    Summed in `decimal` rather than as `sum(k) / len(k)`, for the same reason the derivation above
    avoids `math.`: **a float reduction is not reproducible across Python versions.**  CPython 3.12
    changed builtin `sum()` over floats to Neumaier compensated summation, so the naive left-to-
    right loop it used before gives a DIFFERENT last bit -- 0.9850809499905379 against
    0.9850809499905381 for this table, measured on 3.13 against the committed value.  That is one
    ulp in a constant nothing reads, and it still broke `--check` on a machine whose Python simply
    differed from the one that last regenerated the file.  Summing the exact decimal values and
    rounding once gives the correctly-rounded mean (it agrees with `math.fsum`) on every build.
    """
    with localcontext() as ctx:
        ctx.prec = FINAL_PREC
        total = Decimal(0)
        for x in k:
            total += Decimal(x)          # exact for a float; a no-op for a Decimal
        return float(total / Decimal(len(k)))


def self_test():
    """Re-derive the construction's defining properties.  Returns a list of failure descriptions."""
    bad = []
    r, v, w, k, fy = tables()
    rf, vf, wf, kf, fyf = float(r), float(v), [float(x) for x in w], [float(x) for x in k], \
        [float(x) for x in fy]

    with localcontext() as ctx:
        ctx.prec = FINAL_PREC

        # 1. Every region has the same area. This is the definition of a ziggurat, and it is the
        #    property a mistyped digit breaks while leaving everything else looking reasonable.
        worst = Decimal(0)
        for i in range(2, N_LAYERS):
            area = w[i] * (f(w[i - 1]) - f(w[i]))
            worst = max(worst, abs(area - v) / v)
        base = r * f(r) + tail_area(r)
        worst = max(worst, abs(base - v) / v)
        top = w[1] * (1 - f(w[1]))
        worst = max(worst, abs(top - v) / v)
        # The bound is set by SEARCH_ITERS, not by the arithmetic: the bisection pins `R` to
        # about 8e-24 and the 255-step walk amplifies an error there by some three orders of
        # magnitude, so ~1e-21 is the floor. 1e-18 leaves margin and is still six orders better
        # than the libm version of this script could manage.
        if worst > Decimal("1e-18"):
            bad.append("regions are not equal in area: worst relative deviation %.3e "
                       "(want <= 1e-18)" % float(worst))

        # 2. The stack closes at the peak: layer 1's top edge is f = 1 exactly.
        residual = f(w[1]) + v / w[1] - 1
        if abs(residual) > Decimal("1e-18"):
            bad.append("the ziggurat does not close at the peak: residual %.3e (want <= 1e-18)"
                       % float(residual))

    # 3. THE CHECK THE ORIGINAL VERSION OF THIS SCRIPT DID NOT HAVE, and the one that would have
    #    caught its defect: the answer must not depend on the arithmetic used to reach it. A
    #    derivation in `decimal` is platform-independent; one in libm is not, and the first version
    #    of this generator emitted a different table on CI than on the machine that committed it.
    r2, v2, w2, k2, fy2 = as_floats(search_prec=SEARCH_PREC + 12, final_prec=FINAL_PREC + 12)
    if (r2, v2, w2, k2, fy2) != (rf, vf, wf, kf, fyf):
        nw = sum(1 for a, b in zip(w2, wf) if a != b)
        bad.append("re-deriving at a higher working precision gives a DIFFERENT table (%d of %d "
                   "widths move), so the emitted values depend on the arithmetic rather than on "
                   "the construction -- the working precision is too low, or something in the "
                   "derivation is not exact" % (nw, N_LAYERS))

    # 4. The published (R, V) for a 256-layer normal ziggurat, as an INDEPENDENT anchor -- these
    #    are the numbers every implementation of this table agrees on, and reproducing them is
    #    what says the bisection above solved the same equation everyone else solved.
    if abs(rf - 3.6541528853610088) > 1e-12:
        bad.append("R = %.16f does not reproduce the published 3.6541528853610088" % rf)
    if abs(vf - 0.00492867323399) > 1e-12:
        bad.append("V = %.14f does not reproduce the published 0.00492867323399" % vf)

    # 5. Monotonicity, which the algorithm's immediate-acceptance test depends on: a layer must be
    #    at least as wide as the one above it, or `u < zig_k(i)` would accept points outside the
    #    curve. Layer 0 is excluded -- the base strip is wider than R on purpose.
    for i in range(2, N_LAYERS):
        if not (wf[i - 1] < wf[i]):
            bad.append("layer widths are not increasing at i = %d" % i)
            break
    if not (wf[0] > rf):
        bad.append("the base strip (%.6f) is not wider than R (%.6f), which it must be because "
                   "it carries the tail's area as well" % (wf[0], rf))
    if kf[1] != 0.0:
        bad.append("zig_k(1) is not 0: layer 1 contains the peak and has no inner rectangle")

    # 6. Every k is a fraction, and every f a probability. A ratio above 1 would accept a point
    #    beyond the layer's own width.
    for i in range(N_LAYERS):
        if not (0.0 <= kf[i] <= 1.0):
            bad.append("zig_k(%d) = %r is outside [0, 1]" % (i, kf[i]))
            break
        if not (0.0 < fyf[i] <= 1.0):
            bad.append("zig_f(%d) = %r is outside (0, 1]" % (i, fyf[i]))
            break

    # 7. The speed claim. A correct 256-layer table accepts about 99 % of draws with no further
    #    work; a table that is wrong in a way the checks above miss usually shows up here.
    rate = acceptance_rate(kf)
    if not (0.98 <= rate <= 0.995):
        bad.append("the immediate-acceptance rate is %.4f, outside the 0.98-0.995 a correct "
                   "256-layer table gives" % rate)

    return bad


def real_literal(value):
    """Shortest round-tripping decimal, with an explicit kind so no literal is default-real."""
    text = repr(float(value))
    if "e" in text or "E" in text:
        mantissa, _, exponent = text.partition("e")
        if "." not in mantissa:
            mantissa += ".0"
        return "%s_real64 * 10.0_real64**(%d)" % (mantissa, int(exponent))
    if "." not in text:
        text += ".0"
    return "%s_real64" % text


def array(name, size_expr, items, indent=4):
    """Emit `real(real64), parameter :: name(size_expr) = [ ... ]` with '&' continuations."""
    pad = " " * indent
    body = " " * (indent + 4)
    out = ["%sreal(real64), parameter :: %s(%s) = [ &" % (pad, name, size_expr)]
    line = body
    for n, item in enumerate(items):
        piece = item + (", " if n < len(items) - 1 else "]")
        if line.rstrip() != body.rstrip() and len(line) + len(piece) + 2 > WIDTH:
            out.append(line.rstrip() + " &")
            line = body
        line += piece
    out.append(line.rstrip())
    return out


def gen_module():
    r, v, w, k, fy = as_floats()
    rate = acceptance_rate(k)

    L = [BANNER, ""]
    L.append("!> Layer tables for the Ziggurat normal draw. **Data only -- no procedures.**")
    L.append("!>")
    L.append("!> A leaf module: `iso_fortran_env` and nothing else, which is what lets")
    L.append("!> `tools/check_random_kernels.sh` keep compiling `parquet_random` standalone.")
    L.append("!>")
    L.append("!> Every constant here is the unique solution of one equation rather than a choice,")
    L.append("!> and `tools/generate_parquet_ziggurat.py --self-test` re-derives it: %d regions of"
             % N_LAYERS)
    L.append("!> equal area under `exp(-x*x/2)`, closing exactly at the peak. Do not hand-edit --")
    L.append("!> a single wrong digit leaves a table that still terminates and draws from a")
    L.append("!> slightly wrong distribution, which no casual test would see.")
    L.append("!>")
    L.append("!> **These values are frozen contract**, named by `pf_normal_algorithm`: they decide")
    L.append("!> every value `pf_random_normal_at` and `%normal` return, so changing the layer")
    L.append("!> count or the solver would change published draws.")
    L.append("module parquet_ziggurat")
    L.append("")
    L.append("    use iso_fortran_env, only: int32, real64")
    L.append("")
    L.append("    implicit none")
    L.append("    public")
    L.append("")
    L.append("    !> Layers, including the base strip at index 0. The index costs 8 bits of a")
    L.append("    !! 64-bit draw, which is why it is a power of two and why it is this one.")
    L.append("    integer(int32), parameter :: zig_layers = %d" % N_LAYERS)
    L.append("")
    L.append("    !> Where the tail begins: the outer edge of layer %d, and the point beyond which"
             % (N_LAYERS - 1))
    L.append("    !! a draw is generated by the tail algorithm rather than by a rectangle.")
    L.append("    real(real64), parameter :: zig_r = %s" % real_literal(r))
    L.append("")
    L.append("    !> The shared area of every region, base strip and tail included. Not used by")
    L.append("    !! the draw itself -- published so a reader can check the construction.")
    L.append("    real(real64), parameter :: zig_v = %s" % real_literal(v))
    L.append("")
    L.append("    !> Fraction of draws accepted with no wedge test and no tail walk: %.5f."
             % rate)
    L.append("    !! Published for the same reason as `zig_v`; nothing reads it.")
    L.append("    real(real64), parameter :: zig_accept_rate = %s" % real_literal(rate))
    L.append("")
    L.append("    !> Layer half-widths. A draw is `u * zig_w(i)`. Index 0 is the base strip and is")
    L.append("    !! WIDER than `zig_r`, because its area has to cover the tail as well.")
    L += array("zig_w", "0:zig_layers-1", [real_literal(x) for x in w])
    L.append("")
    L.append("    !> Fraction of `zig_w(i)` that lies wholly under the curve, so a uniform below")
    L.append("    !! it is accepted immediately. `zig_k(1)` is 0: layer 1 contains the peak.")
    L += array("zig_k", "0:zig_layers-1", [real_literal(x) for x in k])
    L.append("")
    L.append("    !> `exp(-zig_w(i)**2/2)`, the curve's height at the layer's outer edge, for the")
    L.append("    !! wedge test. `zig_f(0)` is 1 rather than `f(zig_w(0))`: layer 0 never runs")
    L.append("    !! that test, and 1 is what makes layer 1's wedge span the full height.")
    L += array("zig_f", "0:zig_layers-1", [real_literal(x) for x in fy])
    L.append("")
    L.append("end module parquet_ziggurat   ! GCOVR_EXCL_LINE")
    L.append("")
    return "\n".join(L)


def main(argv):
    ap = argparse.ArgumentParser(description=__doc__.split("\n")[0])
    ap.add_argument("--check", action="store_true",
                    help="compare the committed file with a fresh generation; exit 1 on drift")
    ap.add_argument("--self-test", action="store_true",
                    help="re-derive the construction's defining properties; exit 1 on failure")
    args = ap.parse_args(argv[1:])
    here = pathlib.Path(__file__).resolve().parent.parent
    out = here / "src" / "parquet_ziggurat.f90"

    if args.self_test:
        bad = self_test()
        if bad:
            print("generate_parquet_ziggurat.py: %d property/properties NOT reproduced:" % len(bad))
            for line in bad:
                print("  " + line)
            print("The construction is wrong. Do NOT regenerate from it.")
            return 1
        r, v, w, k, fy = as_floats()
        print("generate_parquet_ziggurat.py: self-test OK (%d equal-area regions, closure at the "
              "peak, published R = %.16f and V = %.14f, %.4f immediate acceptance, and an "
              "identical table at a higher working precision)."
              % (N_LAYERS, r, v, acceptance_rate(k)))
        return 0

    text = gen_module()
    if args.check:
        if not out.is_file():
            print("generate_parquet_ziggurat.py: %s is missing" % out)
            return 1
        if out.read_text() != text:
            print("generate_parquet_ziggurat.py: %s is NOT what this generator emits -- either it "
                  "was hand-edited or the generator changed without a regeneration." % out)
            return 1
        print("generate_parquet_ziggurat.py: %s is current." % out)
        return 0

    out.write_text(text)
    print("generate_parquet_ziggurat.py: wrote %s (%d lines)." % (out, text.count("\n")))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
