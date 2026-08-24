#!/usr/bin/env bash
#
# S7-9: does replacing the float-to-integer integrality test pay on THIS machine?
#
#   bench/check_s7_9.sh
#
# Run it on the machine, capture the whole output, send it back. Nothing is built
# outside test_run/ and no source is modified.
#
# ---------------------------------------------------------------------------
#  THE QUESTION
# ---------------------------------------------------------------------------
#
# Every float-to-integer write in this library checks `src(i) /= anint(src(i))`
# to reject a non-integral value. `anint` on real64 means round-half-away-from-
# zero. aarch64 has FRINTA -- one instruction with exactly those semantics.
# x86-64 has no SSE/AVX instruction for that rounding mode, so the compiler must
# emit a CALL to libm's round(), once per element.
#
# This script answers two things that have to be separated:
#
#   PART 1, the mechanism. Is there really a libm call in the shipped object on
#   this machine? `nm -u` either shows an undefined `round` or it does not.
#   This is a fact, not a measurement, and it needs no quiet machine.
#
#   PART 2, the size. A candidate replacement (a magnitude test plus an integer
#   round trip, no anint) is timed against the shipped form in the SAME fused
#   loop. Both live inside bench/benchmark_stage7.f90, so neither pays a
#   cross-module call the other does not.
#
# ---------------------------------------------------------------------------
#  WHY BOTH PARTS ARE NEEDED, AND WHAT THE MACHINE THIS WAS WRITTEN ON FOUND
# ---------------------------------------------------------------------------
#
# Machine A (Apple M1 Pro, aarch64, gfortran 15.2) measured the candidate at
# ratio 0.35 for int32 and 0.83 for int64 -- i.e. it LOSES there, by up to 2.9x,
# exactly as FRINTA predicts. The change was implemented, measured, and REVERTED
# on the strength of that. So this is not a confirmation run for a decided
# change; it is the other half of the evidence, and a result either way is
# useful:
#
#   * ratio well above 1 here -> the two architectures want different code, and
#     the change has to be made per-architecture rather than unconditionally.
#   * ratio at or below 1 here -> S7-9 is dropped outright, and the libm call is
#     simply not worth removing.
#
# Do NOT report the mechanism (part 1) as if it were the gain. An undefined
# `round` proves the call exists; only part 2 says whether removing it pays.
#
set -uo pipefail
cd "$(dirname "$0")/.."
mkdir -p test_run

fc="$(fpm build --show-model 2>/dev/null | grep -oE 'fc="[^"]*"' | head -1 | sed 's/fc="//;s/"//')"
fc="${fc:-${FPM_FC:-gfortran}}"
tag="$(basename "$fc")-$("$fc" -dumpversion 2>/dev/null | cut -d. -f1)"
tag="$(printf '%s' "$tag" | tr -c 'A-Za-z0-9._-' '_')"
bdir="test_run/s7-9-$tag"

echo "=================================================================="
echo " S7-9 -- integrality test: is anint's libm call worth removing here?"
echo "=================================================================="
echo " host     : $(uname -srm)"
echo " commit   : $(git rev-parse --short HEAD 2>/dev/null || echo '?')$(git diff --quiet 2>/dev/null || echo ' + UNCOMMITTED CHANGES')"
echo " fortran  : $("$fc" --version 2>&1 | head -1)"
echo " build dir: $bdir"
echo "=================================================================="
echo

echo "---- building (release) ----"
if ! FPM_BUILD_DIR="$bdir" fpm build --profile release > "test_run/s7-9-build-$tag.log" 2>&1; then
    echo "BUILD FAILED -- last 20 lines:" >&2
    tail -20 "test_run/s7-9-build-$tag.log" >&2
    exit 1
fi
echo "ok"
echo

echo "---- PART 1: is there a libm round() call in the shipped write path? ----"
obj="$(find "$bdir" -name 'src_parquet_write_numeric.f90.o' | head -1)"
if [ -z "$obj" ]; then
    echo "  could not find src_parquet_write_numeric.f90.o under $bdir -- cannot answer part 1"
else
    echo "  object: $obj"
    # Linux nm prints "U round"; macOS prints "U _round". Match either, and show
    # the raw lines so a reader can check rather than trust the count.
    hits="$(nm -u "$obj" 2>/dev/null | grep -E '(^|[[:space:]_])round$' || true)"
    if [ -n "$hits" ]; then
        echo "  UNDEFINED ROUND SYMBOL PRESENT -- anint compiles to a libm call here:"
        printf '%s\n' "$hits" | sed 's/^/    /'
    else
        echo "  no undefined round symbol -- anint is inlined to an instruction on this target"
        echo "  (expected on aarch64; if you see this on x86-64 it is itself the finding)"
    fi
fi
echo

echo "---- PART 2: candidate vs shipped, same loop, best of N ----"
FPM_BUILD_DIR="$bdir" fpm run --profile release benchmark_stage7 -- --only=s7-9 2>&1 \
    | sed -n '/^S7-9/,$p'
echo
echo "=================================================================="
echo " done. Send the WHOLE output back, including part 1."
echo " Clean up with: rm -rf $bdir test_run/s7-9-*.log"
echo "=================================================================="
