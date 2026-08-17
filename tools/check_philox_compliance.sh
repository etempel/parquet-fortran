#!/usr/bin/env bash
#
# Sweeps the SHIPPED Philox kernel against an independent implementation of philox4x32-10.
#
# Why this exists alongside the suite's own known-answer test:
#
#   * `test_kat_vectors` pins the kernel at the THREE published Random123 vectors, and the golden
#     vectors pin a 6 x 5 x 4 grid of the public surface. Both are exact and both are narrow -- a
#     defect that happens to agree at those points is invisible to them.
#   * This sweeps thousands of arbitrary 64-bit `(key, stream, index)` coordinates, opening with a
#     cross product of awkward values (zero, all-ones, single bits, the 32-bit boundary) where a
#     shift or half-swap defect actually lives, and only then going pseudo-random.
#   * It goes through `parquet_debug_random_block`, so it reaches counter values no draw index can
#     produce -- which is where two of the three published vectors sit -- and it exercises whichever
#     arm of the route (e) 128-bit fork this build took.
#
# **The vacuity guards matter more than the sweep.** "0 mismatches" is also exactly what a comparison
# that never happened reports, and what an oracle agreeing with a wrong kernel reports. So this
# script proves, on every run, that the comparison can fail: it re-runs the sweep against a
# deliberately mis-packed oracle and requires mismatches, feeds the oracle nothing and requires it to
# refuse, and requires a non-Philox oracle to be rejected before it reads any input. If any of those
# guards does not fire, the run is reported as worthless rather than as a pass.
#
# Everything configurable is in the CONFIGURATION block, and can be overridden from the environment.
#
# Exit codes: 0 compliant, 1 pre-flight failure, 2 terminated early, 3 a guard did not fire,
#             4 the kernel disagrees with the specification.

set -euo pipefail

# ============================================================================
# CONFIGURATION -- edit here, or override from the environment
# ============================================================================

# How many blocks to sweep. The first 4096 are the structured cross product; the rest are
# pseudo-random. 20000 takes about a second end to end.
N="${N:-20000}"

# fpm profile for the dumper. Any profile is correct -- the kernel's OUTPUT must not depend on
# optimisation, and a build where it does is exactly what this is meant to catch.
PROFILE="${PROFILE:-release}"

# The Python interpreter used for the oracle.
PYTHON="${PYTHON:-python3}"

# ============================================================================
# End of configuration
# ============================================================================

cd "$(dirname "$0")/.."

ORACLE="tools/philox_reference.py"

# A tool that stops early must not exit 0. Deliberate exits set the flag first, so their own codes
# survive; anything else lands here as a 2.
finished=0
trap '[ "$finished" = "1" ] || { echo ""; echo "check_philox_compliance.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

echo "=== Philox4x32-10 compliance sweep ==="

if ! command -v "$PYTHON" >/dev/null 2>&1; then
    echo "check_philox_compliance.sh: $PYTHON not found; set PYTHON= to your interpreter" >&2
    finished=1
    exit 1
fi
if [ ! -f "$ORACLE" ]; then
    echo "check_philox_compliance.sh: missing $ORACLE" >&2
    finished=1
    exit 1
fi

echo "building the dumper (--profile $PROFILE) ..."
fpm build --profile "$PROFILE" >/dev/null

DUMPER="$(find build -type f -perm -u+x -name probe_random_philox | head -n 1)"
if [ -z "$DUMPER" ]; then
    echo "check_philox_compliance.sh: could not find the built probe_random_philox binary" >&2
    finished=1
    exit 1
fi
echo "dumper    : $DUMPER"
echo "blocks    : $N"

# --- Guard 1: the oracle must prove it is Philox before it judges anything ----------------------

echo ""
echo "--- guard 1: the oracle reproduces the published vectors ---"
"$PYTHON" "$ORACLE" --self-test

# --- Guard 2: a non-Philox oracle must be REJECTED, not used -----------------------------------

echo "--- guard 2: a mutated oracle is rejected before it reads input ---"
for m in rounds multiplier; do
    set +e
    "$PYTHON" "$ORACLE" --self-test --mutate="$m" >/dev/null 2>&1
    rc=$?
    set -e
    if [ "$rc" != "2" ]; then
        echo "check_philox_compliance.sh: an oracle mutated by '$m' was NOT rejected (exit $rc)." >&2
        echo "The self-validation is not working, so nothing below it can be trusted." >&2
        finished=1
        exit 3
    fi
    echo "  --mutate=$m  rejected, as required"
done

# --- Guard 3: an empty sweep must be refused, not reported as clean -----------------------------

echo "--- guard 3: an empty input is refused rather than passed ---"
set +e
: | "$PYTHON" "$ORACLE" >/dev/null 2>&1
rc=$?
set -e
if [ "$rc" = "0" ]; then
    echo "check_philox_compliance.sh: an EMPTY sweep reported success." >&2
    echo "A run where the dumper produced nothing would read as compliant." >&2
    finished=1
    exit 3
fi
echo "  empty input refused, as required"

# --- Guard 4: the sweep must be able to DETECT a wrong kernel ------------------------------------

# `--mutate=packing` leaves the cipher correct -- so it passes guard 1 -- and swaps the coordinate
# packing, which is precisely the class of defect the sweep exists to catch. If this does not
# produce mismatches then the comparison has no power and a clean result below means nothing.
echo "--- guard 4: the sweep detects a deliberately wrong kernel ---"
ctl_out="$("$DUMPER" --n=500 --quiet 2>/dev/null | "$PYTHON" "$ORACLE" --mutate=packing --quiet 2>/dev/null || true)"
ctl_bad="$(printf '%s' "$ctl_out" | sed -n 's/.*swept [0-9]* blocks, \([0-9]*\) mismatches.*/\1/p')"
if [ -z "$ctl_bad" ] || [ "$ctl_bad" = "0" ]; then
    echo "check_philox_compliance.sh: the mis-packed control produced NO mismatches." >&2
    echo "  oracle said: ${ctl_out:-<nothing>}" >&2
    echo "The comparison has no power, so a clean sweep would mean nothing." >&2
    finished=1
    exit 3
fi
echo "  mis-packed control: $ctl_out -- detected, as required"

# --- The sweep itself ----------------------------------------------------------------------------

echo ""
echo "--- sweeping $N blocks of the shipped kernel ---"
set +e
"$DUMPER" --n="$N" 2>/dev/null | "$PYTHON" "$ORACLE"
st=("${PIPESTATUS[@]}")
set -e

finished=1

if [ "${st[0]}" != "0" ]; then
    echo ""
    echo "check_philox_compliance.sh: the dumper failed (exit ${st[0]})" >&2
    exit 1
fi
if [ "${st[1]}" != "0" ]; then
    echo ""
    echo "check_philox_compliance.sh: THE SHIPPED KERNEL DISAGREES WITH THE SPECIFICATION." >&2
    echo "Every guard above passed, so the oracle is philox4x32-10 and the comparison has power." >&2
    echo "Check the reported coordinates: an edge value (0, all-ones, 2**32) points at the" >&2
    echo "coordinate packing or a shift; a pseudo-random one points at the round function." >&2
    exit 4
fi

echo ""
echo "The shipped kernel is philox4x32-10, over $N blocks including every edge coordinate."
