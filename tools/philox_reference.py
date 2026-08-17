#!/usr/bin/env python3
"""An independent Philox4x32-10, written from the Random123 specification.

This is the ORACLE half of `tools/check_philox_compliance.sh`. It reads blocks dumped by
`app/probe_random_philox.f90` -- each one a `(key, stream, index)` coordinate and the four output
words this library's shipped kernel produced for it -- and recomputes every one from the
specification, in Python's arbitrary-precision integers where nothing can overflow.

**Why this exists when the suite already checks known-answer vectors.** `test_kat_vectors` pins the
kernel at the three published vectors, and the golden vectors pin a 6 x 5 x 4 grid of the public
surface. Both are exact and neither is broad: three points and a grid of coordinates a draw index
can actually reach. This sweeps arbitrary 64-bit coordinates instead -- including counter values no
draw can produce -- so a defect that happens to agree at those points has nowhere to hide.

**It refuses to act as an oracle until it has proved it is Philox.** The three published vectors are
checked on every run before a single dumped row is read; a mismatch exits 2 and reports nothing
about the library. An oracle that agrees with a wrong kernel because both are wrong the same way is
worse than no oracle at all.

`--mutate` deliberately breaks it, so the shell wrapper can prove the comparison has power. A sweep
reporting zero mismatches is exactly what a vacuous comparison reports too, and this is what
separates the two:

  * `--mutate=rounds`      nine rounds instead of ten -- must be caught by the self-validation
  * `--mutate=multiplier`  one multiplier bit flipped -- must be caught by the self-validation
  * `--mutate=packing`     correct Philox, but the coordinate packing swapped. This one PASSES the
                           self-validation (the cipher is untouched) and must be caught by the
                           sweep, which is the half that would otherwise go untested.

Exit codes: 0 clean, 1 mismatches or nothing swept, 2 the oracle is not Philox.

Usage:
    app/probe_random_philox --n=20000 | tools/philox_reference.py
    tools/philox_reference.py --self-test
"""
import argparse
import sys

M0, M1 = 0xD2511F53, 0xCD9E8D57          # the Philox4x32 multipliers
W0, W1 = 0x9E3779B9, 0xBB67AE85          # the golden-ratio and sqrt(5) Weyl bumps
MASK32 = 0xFFFFFFFF
MASK64 = 0xFFFFFFFFFFFFFFFF

# The three published Random123 philox4x32-10 known-answer vectors: counter, key, expected output.
# KAT 3's counter and key are the digits of pi, which is how the Random123 distribution states them.
KATS = [
    ((0x00000000, 0x00000000, 0x00000000, 0x00000000), (0x00000000, 0x00000000),
     (0x6627e8d5, 0xe169c58d, 0xbc57ac4c, 0x9b00dbd8)),
    ((0xffffffff, 0xffffffff, 0xffffffff, 0xffffffff), (0xffffffff, 0xffffffff),
     (0x408f276d, 0x41c83b0e, 0xa20bc7c6, 0x6d5451fd)),
    ((0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344), (0xa4093822, 0x299f31d0),
     (0xd16cfe09, 0x94fdcceb, 0x5001e420, 0x24126ea1)),
]


def philox4x32_10(c0, c1, c2, c3, k0, k1, mutate="none"):
    """One 10-round Philox4x32 block over 32-bit words.

    Each round multiplies two counter words by the fixed constants, splits both products into
    halves, and cross-combines. Both operands are below 2**32, so each product is below 2**64 and
    Python's unbounded integers carry it exactly -- there is no wrapping site here to reason about,
    which is the point of writing the oracle in Python rather than in Fortran.
    """
    rounds = 9 if mutate == "rounds" else 10
    m0 = (M0 ^ 1) if mutate == "multiplier" else M0
    for _ in range(rounds):
        p0 = m0 * c0
        p1 = M1 * c2
        hi0, lo0 = p0 >> 32, p0 & MASK32
        hi1, lo1 = p1 >> 32, p1 & MASK32
        c0, c1, c2, c3 = (hi1 ^ c1 ^ k0) & MASK32, lo1, (hi0 ^ c3 ^ k1) & MASK32, lo0
        k0 = (k0 + W0) & MASK32
        k1 = (k1 + W1) & MASK32
    return c0, c1, c2, c3


def validate_oracle(mutate):
    """Checks this implementation against the three published vectors. Exits 2 if it is not Philox."""
    for ctr, key, want in KATS:
        got = philox4x32_10(*ctr, *key, mutate=mutate)
        if got != want:
            print("ORACLE INVALID -- this is not philox4x32-10, so it says nothing about the library",
                  file=sys.stderr)
            print(f"  counter {[hex(x) for x in ctr]} key {[hex(x) for x in key]}", file=sys.stderr)
            print(f"  got     {[hex(x) for x in got]}", file=sys.stderr)
            print(f"  want    {[hex(x) for x in want]}", file=sys.stderr)
            sys.exit(2)
    return len(KATS)


def expected_block(key, stream, index, mutate):
    """The four words this library's coordinates must produce.

    The library's packing, which `parquet_debug_random_block` documents and `test_kat_vectors`
    repacks the published vectors into: counter words 0 and 1 are the block index with its LOW half
    first, counter words 2 and 3 are the stream, and the two key words are the 64-bit key.
    """
    if mutate == "packing":
        index, stream = stream, index        # correct cipher, wrong coordinates
    return philox4x32_10(index & MASK32, (index >> 32) & MASK32,
                         stream & MASK32, (stream >> 32) & MASK32,
                         key & MASK32, (key >> 32) & MASK32)


def main():
    ap = argparse.ArgumentParser(description="Independent Philox4x32-10 oracle.")
    ap.add_argument("--self-test", action="store_true",
                    help="validate against the published vectors and exit, reading no input")
    ap.add_argument("--mutate", default="none",
                    choices=["none", "rounds", "multiplier", "packing"],
                    help="deliberately break the oracle, to prove the comparison has power")
    ap.add_argument("--quiet", action="store_true", help="suppress the per-mismatch report")
    args = ap.parse_args()

    n_kat = validate_oracle(args.mutate)
    if args.self_test:
        print(f"philox_reference.py: reproduces all {n_kat} published Random123 "
              f"philox4x32-10 vectors")
        return 0
    print(f"oracle validated against {n_kat} published Random123 vectors", file=sys.stderr)

    rows = bad = 0
    for line in sys.stdin:
        f = line.split()
        if len(f) != 7:
            continue                                    # a comment or a blank line
        key, stream, index = (int(v) & MASK64 for v in f[0:3])
        got = tuple(int(v) & MASK64 for v in f[3:7])
        want = expected_block(key, stream, index, args.mutate)
        rows += 1
        if got != want:
            bad += 1
            if not args.quiet and bad <= 5:
                print(f"MISMATCH  key={key} stream={stream} index={index}", file=sys.stderr)
                print(f"  library {[hex(x) for x in got]}", file=sys.stderr)
                print(f"  oracle  {[hex(x) for x in want]}", file=sys.stderr)

    if rows == 0:
        print("philox_reference.py: NOTHING WAS SWEPT -- no blocks arrived on stdin, so this run "
              "proves nothing", file=sys.stderr)
        return 1
    print(f"swept {rows} blocks, {bad} mismatches")
    return 1 if bad else 0


if __name__ == "__main__":
    sys.exit(main())
