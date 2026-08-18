#!/usr/bin/env python3
"""Golden vectors for `pf_random_perm_at`, from an independent arbitrary-precision model.

The whole point is that this file shares no code with `src/parquet_random.f90`: it is a
transcription of the *contract* -- the two constructions, the threshold between them, the width
rule, the round count, the round function, the key derivation and the parity correction -- written
from the specification rather than from the Fortran. A vector produced by running the library would
only prove the library agrees with itself.

It does share code with `generate_random_golden_vectors.py`, deliberately: the exact path spends a
`pf_random_int_at` draw on its rank, and a second transcription of Philox and of the rejection rule
is a second thing that can drift. That sibling is a model of the same contract, not the library.

Python integers are arbitrary precision, so nothing here wraps, and every intermediate is masked
exactly where the Fortran masks it. That makes this the reference for the claim that the Fortran
kernel cannot overflow: if the two agree, no int64 intermediate exceeded its bound.

**These vectors are what guards the frozen contract.** Bijectivity cannot see the round count -- an
odd count transposes the factors and still yields a permutation -- so a change from sixteen rounds
to fifteen passes every structural test and breaks only these. The same is true of the multipliers,
the width rule, the key schedule, the parity bit and the exact threshold.

**The model is parameterised, and that is its own validation.** `--self-test` sets it back to the
published `/v1` configuration -- four rounds, no parity correction, no exact path -- and checks it
reproduces that release's 27 published values. A model that cannot reproduce the kernel it replaced
is not evidence about the kernel that replaced it.

Usage:
    python3 tools/generate_random_perm_vectors.py                # emit Fortran declarations
    python3 tools/generate_random_perm_vectors.py --self-test    # validate the model, emit nothing
    python3 tools/generate_random_perm_vectors.py --check        # assert test/ still carries them
"""

import math
import sys

sys.path.insert(0, __file__.rsplit("/", 1)[0])
import generate_random_golden_vectors as draws   # noqa: E402  (path set above)

M31 = (1 << 31) - 1
M32 = (1 << 32) - 1
MASK64 = (1 << 64) - 1
C1 = 2654435761          # 0x9E3779B1, the odd 32-bit golden-ratio constant
C2 = 2246822519          # 0x85EBCA77, xxHash PRIME32_2

ROUNDS = 16              # perm_rounds
EXACT_MAX = 20           # perm_exact_max: 20! fits an int64 and 21! does not
PARITY_KEY = ROUNDS + 1  # perm_parity_key: fixed, so it does not move with a forced round count
FAMILY_LABEL = 6813122891117395759   # perm_family_label

ALGORITHM = "feistel-mix2-16p/zaxzb/exact20/v2"


def mix2(rk, x):
    """Two masked multiplies. Every operand is below 2**31 before a multiply by a value below
    2**32, so each product is below 2**63 -- the bound the Fortran relies on."""
    z = x ^ rk
    z = (z & M31) * C1
    assert z < (1 << 63), "product exceeded the int64 bound the Fortran assumes"
    z ^= z >> 29
    z = (z & M31) * C2
    assert z < (1 << 63), "product exceeded the int64 bound the Fortran assumes"
    z ^= z >> 31
    return z & M32


def round_keys(seed, rounds):
    s = seed & MASK64
    return [mix2((j * C1), s & M32) ^ mix2(j, (s >> 32) & M32) for j in range(1, rounds + 1)]


def parity_flip(seed):
    """The parity correction's bit: one key from the same schedule, at a FIXED index.

    Without it a Feistel over `Z_q x Z_q` with `q` odd cannot reach an odd permutation at any round
    count, so exactly half of `S_m` is unreachable at m = 25, 49, 81, ...
    """
    s = seed & MASK64
    pk = mix2(PARITY_KEY * C1, s & M32) ^ mix2(PARITY_KEY, (s >> 32) & M32)
    return (pk & 1) == 1


def factors(m):
    """`a = ceil(sqrt(m))`, `b = ceil(m/a)`, so `a*b` sits just above `m`."""
    a = 1
    while a * a < m:
        a += 1
    return a, (m + a - 1) // a


def feistel(rk, rounds, a, b, x):
    p, q = a, b
    l = x // q
    r = x - l * q
    for j in range(rounds):
        w = mix2(rk[j], r) & M31
        prod = w * p
        assert prod < (1 << 63), "multiply-shift exceeded the int64 bound"
        t = l + (prod >> 31)
        if t >= p:
            t -= p
        l, r = r, t
        p, q = q, p
    return l * q + r


def exact_perm(seed, m):
    """The exact construction: an exactly uniform rank in `[0, m!)`, then Lehmer unranking.

    `m` is the STREAM index rather than part of the key, so a permutation of 5 and one of 6 under
    the same seed are independent instead of both being monotone in one 64-bit word.
    """
    rank = draws.int_at(draws.random_key(seed, FAMILY_LABEL), m, 0, math.factorial(m) - 1)[0]
    pool = list(range(1, m + 1))
    out = []
    rem = rank
    for i in range(1, m + 1):
        f = math.factorial(m - i)
        d = rem // f
        rem -= d * f
        out.append(pool.pop(d))
    return out


def perm_at(seed, m, k, rounds=None, parity=True, exact_max=None):
    # Resolved from the module globals rather than captured as default argument values. A default
    # is evaluated once, at definition time, so `ROUNDS = 14` set anywhere afterwards would leave
    # this function running sixteen -- and the vectors would then be "checked" against exactly the
    # configuration they were generated from, whatever anyone believed they had changed. That is
    # not hypothetical: it silently defeated this file's own drift check the first time it was run.
    if rounds is None:
        rounds = ROUNDS
    if exact_max is None:
        exact_max = EXACT_MAX
    n = max(m, 1)
    x = min(max(k - 1, 0), n - 1)
    if n == 1:
        return 1
    if n <= exact_max:
        return exact_perm(seed, n)[x]
    a, b = factors(n)
    rk = round_keys(seed, rounds)
    while True:
        x = feistel(rk, rounds, a, b, x)
        if x < n:
            break
    if parity and parity_flip(seed) and x <= 1:
        x = 1 - x
    return x + 1


# ---------------------------------------------------------------------------------------------
# The model's own validation.
#
# These 27 values are what `pf_random_perm_algorithm == "feistel-mix2-4/zaxzb/v1"` published, taken
# from that release's `test_perm_golden`. Reproducing them with the model set back to four rounds,
# no parity and no exact path is the only check available that the transcription is of the KERNEL
# and not of one particular configuration of it -- and it is the check that would catch an error
# introduced while changing the round count, which is exactly when new vectors get regenerated.
LEGACY = [
    (20260816, 100, [1, 2, 3, 50, 99, 100], [50, 57, 15, 70, 60, 63]),
    (20260816, 5, [1, 2, 3, 4, 5], [1, 5, 4, 2, 3]),
    (20260816, 7, [1, 4, 7], [7, 1, 6]),
    (1, 1000, [1, 2, 500, 999, 1000], [879, 530, 65, 176, 547]),
    (-7, 1000, [1, 2, 500, 1000], [614, 185, 562, 298]),
    (20260816, 4000000000, [1, 2, 3999999999, 4000000000],
     [2145434036, 2404499848, 2116569574, 6342502]),
]


def self_test():
    bad = 0
    for seed, m, ks, expect in LEGACY:
        for k, want in zip(ks, expect):
            got = perm_at(seed, m, k, rounds=4, parity=False, exact_max=0)
            if got != want:
                bad += 1
                print("legacy mismatch: perm_at(%d, %d, %d) = %d, /v1 published %d"
                      % (seed, m, k, got, want), file=sys.stderr)
    if bad:
        print("SELF-TEST FAILED: %d of %d legacy values disagree -- the model does not describe "
              "the kernel it replaced, so its new vectors are not evidence." % (bad, 27),
              file=sys.stderr)
        return 1

    # Exactness is a theorem, so what is checked here is the transcription of it: unranking must be
    # a bijection onto S_m. Every rank in [0, m!) mapping to a distinct permutation is that claim.
    seen = set()
    for rank in range(math.factorial(6)):
        pool, out, rem = list(range(1, 7)), [], rank
        for i in range(1, 7):
            f = math.factorial(6 - i)
            d, rem = rem // f, rem % f
            out.append(pool.pop(d))
        seen.add(tuple(out))
    if len(seen) != math.factorial(6):
        print("SELF-TEST FAILED: Lehmer unranking is not a bijection at m=6 (%d of %d)"
              % (len(seen), math.factorial(6)), file=sys.stderr)
        return 1

    # And the parity bit has to be balanced, or the correction trades one skew for another.
    ones = sum(1 for s in range(200000) if parity_flip(s * 2654435761))
    z = (ones - 100000) / (200000 * 0.25) ** 0.5
    if abs(z) > 4.0:
        print("SELF-TEST FAILED: the parity bit is biased, z = %.2f" % z, file=sys.stderr)
        return 1

    print("generate_random_perm_vectors.py --self-test: 27/27 legacy values reproduced at "
          "4 rounds; unranking is a bijection at m=6; parity bit z = %+.2f." % z)
    return 0


# ---------------------------------------------------------------------------------------------
# Cases chosen for what they exercise, not for tidiness.
#
#   5, 7, 20     the EXACT path, including its largest population (20! is the last factorial that
#                fits an int64), checked whole so the unranking cannot be half right
#   21           the smallest population that reaches the Feistel at all -- the threshold itself
#   25           an ODD SQUARE, checked whole: without the parity correction exactly half of S_25
#                is unreachable, and this is the vector that would notice
#   100          a size where a*b == m exactly, so the cycle walk never runs
#   1000         a non-square mid size, with a negative seed among them (the key schedule shifts
#                the seed right, and Fortran's ISHFT is logical, so a negative seed is the case a
#                signed shift would get wrong)
#   4000000000   a population above int32
CASES = [
    (20260816, 5, list(range(1, 6))),
    (20260816, 7, list(range(1, 8))),
    (20260816, 20, list(range(1, 21))),
    (20260816, 21, list(range(1, 22))),
    (20260816, 25, list(range(1, 26))),
    (20260816, 100, [1, 2, 3, 50, 99, 100]),
    (1, 1000, [1, 2, 500, 999, 1000]),
    (-7, 1000, [1, 2, 500, 1000]),
    (20260816, 4000000000, [1, 2, 3999999999, 4000000000]),
]


def wrap(prefix, items, width=124, indent=14):
    """Emits `prefix` followed by `items`, continued with `&` inside the 132-column limit."""
    lines, cur = [], prefix
    pad = " " * indent
    for n, item in enumerate(items):
        piece = item + ("," if n < len(items) - 1 else "]")
        if len(cur) + len(piece) + 2 > width:
            lines.append(cur + " &")
            cur = pad
        cur += piece + (" " if n < len(items) - 1 else "")
    lines.append(cur.rstrip())
    return lines


TEST_PATH = __file__.rsplit("/", 2)[0] + "/test/test_random.f90"


def check(blocks):
    """Assert `test_perm_golden` still carries exactly what this model derives.

    The vectors are pasted inline into the test rather than written to a generated file, so
    without this they can only drift silently -- and a golden vector that has drifted from its
    oracle is worse than none, because it still passes while freezing whatever the library did on
    the day someone regenerated it. Compared literally, so a reflow is reported too; the fix in
    that case is to re-paste, which is a second's work and keeps one canonical form.
    """
    try:
        text = open(TEST_PATH).read()
    except OSError as exc:
        print("--check: cannot read %s (%s)" % (TEST_PATH, exc), file=sys.stderr)
        return 1
    missing = [name for name, block in blocks if block not in text]
    if missing:
        print("generate_random_perm_vectors.py --check: test/test_random.f90's %s "
              "differ(s) from a fresh derivation." % ", ".join(missing), file=sys.stderr)
        print("Re-run tools/generate_random_perm_vectors.py and re-paste into test_perm_golden.",
              file=sys.stderr)
        return 1
    print("generate_random_perm_vectors.py --check: test_perm_golden's %d vectors are current."
          % len(CASES and [k for _, _, ks in CASES for k in ks]))
    return 0


def main():
    if "--self-test" in sys.argv[1:]:
        return self_test()
    if self_test() != 0:
        return 1

    seeds, ms, kk, vv = [], [], [], []
    for seed, m, ks in CASES:
        for k in ks:
            seeds.append(seed)
            ms.append(m)
            kk.append(k)
            vv.append(perm_at(seed, m, k))

    blocks = [("NV", "integer, parameter :: NV = %d" % len(vv))]
    for name, arr in (("sd", seeds), ("mm", ms), ("kk", kk), ("ex", vv)):
        body = ["%d_int64" % v for v in arr]
        blocks.append((name, "\n".join(wrap("        %s = [" % name, body))))
    if "--check" in sys.argv[1:]:
        return check(blocks)

    print()
    print("    ! ---- paste into test_perm_golden ----")
    for _, block in blocks:
        print(block if block.startswith("  ") else "        " + block)
    print()
    print("    ! ---- provenance ----")
    print("    ! algorithm = %s" % ALGORITHM)
    for seed, m, ks in CASES:
        vals = [perm_at(seed, m, k) for k in ks]
        path = "exact" if m <= EXACT_MAX else "feistel"
        print("    ! seed=%d m=%d (%s) k=%s" % (seed, m, path, ks))
        print("    !   val = %s" % vals)
    return 0


if __name__ == "__main__":
    sys.exit(main())
