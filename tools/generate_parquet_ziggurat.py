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
               the peak, the published (R, V) pair, monotonicity, and the acceptance ratio --
               and exit 1 if any of them fails.

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
"""

import argparse
import math
import pathlib
import sys
from decimal import Decimal, getcontext

getcontext().prec = 60

#: Layers.  256 is the usual choice for a 64-bit draw: the index costs 8 bits, which leaves 53 for
#: the uniform and one for the sign inside a single 64-bit pattern, so a normal draw reads exactly
#: one word pair in the common case.  Raising it would improve the acceptance rate by very little
#: and cost another bit out of the same pattern.
N_LAYERS = 256

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
    return math.exp(-0.5 * x * x)


def f_inv(y):
    """The `x >= 0` with `f(x) = y`.  Inverse of `f` on the half-line."""
    return math.sqrt(-2.0 * math.log(y))


def tail_area(r):
    """Area under `f` beyond `r`, i.e. `integral_r^inf exp(-x*x/2) dx`.

    `erfc` gives it in closed form, so this is not a quadrature and carries no tolerance of its
    own: `integral_r^inf exp(-x*x/2) dx = sqrt(pi/2) * erfc(r/sqrt(2))`.
    """
    return math.sqrt(math.pi / 2.0) * math.erfc(r / math.sqrt(2.0))


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
    xs = [0.0] * n
    xs[n - 1] = r
    y = f(r)
    for i in range(n - 1, 1, -1):
        y = y + v / xs[i]
        if y >= 1.0:
            # `r` is too small: the stack reaches the peak before it has used every layer.
            return xs, v, y - 1.0 + (i - 1)
        xs[i - 1] = f_inv(y)
    xs[0] = v / f(r)                      # the base strip's width, tail area included
    return xs, v, f(xs[1]) + v / xs[1] - 1.0


def solve_r(n=N_LAYERS):
    """Bisect for the `r` that makes the ziggurat close exactly at the peak.

    The residual DECREASES in `r`, which is what makes bisection sufficient -- and which is the
    opposite of the intuition that a larger tail means a larger step.  `v = r*f(r) + tail(r)` and
    `f` falls faster than `r` rises, so a larger `r` gives a SMALLER shared area, hence smaller
    steps, hence a stack that runs out of layers below the peak.
    """
    lo, hi = 0.5, 10.0                    # residual(0.5) > 0 (overshoots), residual(10) < 0
    for _ in range(200):
        mid = 0.5 * (lo + hi)
        _, _, residual = build(mid, n)
        if residual > 0.0:
            lo = mid
        else:
            hi = mid
    return 0.5 * (lo + hi)


def tables(n=N_LAYERS):
    """`(r, v, w, k, fy)` for the solved ziggurat -- the five things the module publishes."""
    r = solve_r(n)
    w, v, _ = build(r, n)
    k = [0.0] * n
    # Layer i accepts immediately when `u < k(i)`, i.e. when the draw falls inside layer i-1's
    # width, which is wholly under the curve. Layer 1 has no layer below it that is narrower --
    # it contains the peak -- so it never accepts immediately.
    k[0] = r / w[0]                       # the base strip's rectangle part; beyond it is the tail
    k[1] = 0.0
    for i in range(2, n):
        k[i] = w[i - 1] / w[i]
    fy = [f(x) for x in w]
    fy[0] = 1.0                           # never used by layer 0; makes layer 1's wedge reach the peak
    return r, v, w, k, fy


def acceptance_rate(w, k, fy, n=N_LAYERS):
    """Fraction of CANDIDATES accepted without a wedge test or a tail walk.

    Each layer is chosen with probability 1/n and accepts immediately with probability `k(i)`, so
    this is just their mean.  Reported by `--self-test` because a table that is subtly wrong often
    still terminates -- it just does so more slowly, and nothing else would notice.

    **Per CANDIDATE, not per returned value**, and the two differ by more than they look.  A
    candidate that fails the wedge test is discarded and redrawn, so it never appears in a census
    of "which branch produced this value": conditioning on acceptance raises the observed
    immediate-acceptance share from this 0.985 to about 0.992.  Measured, not derived -- a path
    census over 200000 draws gives 0.9918 / 0.0080 / 0.00024 for rectangle / wedge / tail.
    """
    return sum(k) / float(n)


def self_test():
    """Re-derive the construction's defining properties.  Returns a list of failure descriptions."""
    bad = []
    r, v, w, k, fy = tables()

    # 1. Every region has the same area. This is the definition of a ziggurat, and it is the
    #    property a mistyped digit breaks while leaving everything else looking reasonable.
    worst = 0.0
    for i in range(2, N_LAYERS):
        area = w[i] * (f(w[i - 1]) - f(w[i]))
        worst = max(worst, abs(area - v) / v)
    base = r * f(r) + tail_area(r)
    worst = max(worst, abs(base - v) / v)
    top = w[1] * (1.0 - f(w[1]))
    worst = max(worst, abs(top - v) / v)
    if worst > 1e-12:
        bad.append("regions are not equal in area: worst relative deviation %.3e (want <= 1e-12)"
                   % worst)

    # 2. The stack closes at the peak: layer 1's top edge is f = 1 exactly.
    residual = f(w[1]) + v / w[1] - 1.0
    if abs(residual) > 1e-12:
        bad.append("the ziggurat does not close at the peak: residual %.3e (want <= 1e-12)"
                   % residual)

    # 3. The published (R, V) for a 256-layer normal ziggurat, as an INDEPENDENT anchor -- these
    #    are the numbers every implementation of this table agrees on, and reproducing them is
    #    what says the bisection above solved the same equation everyone else solved.
    if abs(r - 3.6541528853610088) > 1e-12:
        bad.append("R = %.16f does not reproduce the published 3.6541528853610088" % r)
    if abs(v - 0.00492867323399) > 1e-12:
        bad.append("V = %.14f does not reproduce the published 0.00492867323399" % v)

    # 4. Monotonicity, which the algorithm's immediate-acceptance test depends on: a layer must be
    #    at least as wide as the one above it, or `u < zig_k(i)` would accept points outside the
    #    curve. Layer 0 is excluded -- the base strip is wider than R on purpose.
    for i in range(2, N_LAYERS):
        if not (w[i - 1] < w[i]):
            bad.append("layer widths are not increasing at i = %d" % i)
            break
    if not (w[0] > r):
        bad.append("the base strip (%.6f) is not wider than R (%.6f), which it must be because "
                   "it carries the tail's area as well" % (w[0], r))
    if k[1] != 0.0:
        bad.append("zig_k(1) is not 0: layer 1 contains the peak and has no inner rectangle")

    # 5. Every k is a fraction, and every f a probability. A ratio above 1 would accept a point
    #    beyond the layer's own width.
    for i in range(N_LAYERS):
        if not (0.0 <= k[i] <= 1.0):
            bad.append("zig_k(%d) = %r is outside [0, 1]" % (i, k[i]))
            break
        if not (0.0 < fy[i] <= 1.0):
            bad.append("zig_f(%d) = %r is outside (0, 1]" % (i, fy[i]))
            break

    # 6. The speed claim. A correct 256-layer table accepts about 99 % of draws with no further
    #    work; a table that is wrong in a way the checks above miss usually shows up here.
    rate = acceptance_rate(w, k, fy)
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
    r, v, w, k, fy = tables()
    rate = acceptance_rate(w, k, fy)

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
        r, v, w, k, fy = tables()
        print("generate_parquet_ziggurat.py: self-test OK (%d equal-area regions, closure at the "
              "peak, published R = %.16f and V = %.14f, %.4f immediate acceptance)."
              % (N_LAYERS, r, v, acceptance_rate(w, k, fy)))
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
