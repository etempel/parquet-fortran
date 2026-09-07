#!/usr/bin/env bash
# Drives bench/benchmark_filter_set.f90 -- what the set-valued filter clause (`id in @set`) costs
# against the row-group decode it precedes, and what the map underneath it costs on its own.
#
# This script only builds and runs; bench/benchmark_filter_set.f90 decides what each mode times
# and writes its own two fixture files (a sorted and a shuffled id column, four float64 payload
# columns each), which it removes at the end unless KEEP=1.
#
# THE FIGURE EVERY OTHER NUMBER IS READ AGAINST is `decode p1, no filter`: the plain decode of one
# payload column of the same file. The clause is cheap when `open with the filter` -- the leaf's
# pre-evaluation, one row group of the key column at a time -- is a fraction of it, and the
# sorted-file arms then show what pruning buys on top: `decode p1 through the filter` reads only
# the row groups the screen kept, and `row groups pruned=` says how many that was.
#
# Usage:
#   bench/benchmark_filter_set.sh                      # both modes at the defaults
#   bench/benchmark_filter_set.sh --mode=leaf          # one mode
#   NROWS=100000000 NSET=10000000 bench/benchmark_filter_set.sh --mode=leaf
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   NROWS=10000000  Rows in each fixture file. Five int64/float64 columns, so about 40 bytes per
#                    row per file on disk uncompressed, and two files: budget 80 * NROWS bytes.
#   NSET=1000000    Members of the bound set. The dense set is 1..NSET, the sparse one every
#                    NROWS/NSET-th id.
#   CHUNK=500000    Rows per row group of the fixtures. Pruning is per row group, so this is what
#                    decides how much of the sorted file a dense set lets the screen skip.
#   ROUNDS=3        Rounds per figure; the best is kept, per this repo's benchmarking rules.
#   THREADS=0       Team size for the threaded arms of --mode=map. 0 means "ask the library",
#                    which is what a real caller gets.
#   FILE=test_run/benchmark_filter_set   Fixture path prefix; `_sorted.parquet` and
#                    `_shuffled.parquet` are appended.
#   KEEP=0          Set to 1 to leave the fixtures in place.
#   MODE=all        leaf | map | all.
#
# THIS WRAPPER ASSERTS THAT BOTH HALVES OF THE BUILD ARE OPTIMISED, the C++ half included: every
# arm of --mode=leaf opens a file and so passes through src/parquet_wrapper.cpp, and fpm gives the
# C++ half no profile flags at all under a compiler family it does not recognise as a C family
# (nagfor, flang) -- a run that passed the Fortran check alone would print a plausible table with
# every C++-side figure wrong. See bench/benchmark_sort_readtime.sh, where this was measured.
#
# Maintainer-only: nothing outside this repository runs it.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_filter_set.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

cd "$(dirname "$0")/.."

NROWS="${NROWS:-10000000}"
NSET="${NSET:-1000000}"
CHUNK="${CHUNK:-500000}"
ROUNDS="${ROUNDS:-3}"
THREADS="${THREADS:-0}"
FILE="${FILE:-test_run/benchmark_filter_set}"
KEEP="${KEEP:-0}"
MODE="${MODE:-all}"

for arg in "$@"; do
    case "$arg" in
        --mode=*) MODE="${arg#--mode=}" ;;
        --nrows=*) NROWS="${arg#--nrows=}" ;;
        --nset=*) NSET="${arg#--nset=}" ;;
        --chunk=*) CHUNK="${arg#--chunk=}" ;;
        --rounds=*) ROUNDS="${arg#--rounds=}" ;;
        --threads=*) THREADS="${arg#--threads=}" ;;
        --file=*) FILE="${arg#--file=}" ;;
        --keep=*) KEEP="${arg#--keep=}" ;;
        -h|--help) sed -n '2,44p' "$0"; finished=1; exit 0 ;;
        *) echo "benchmark_filter_set.sh: unknown argument '$arg'" >&2; exit 2 ;;
    esac
done

# The build tree carries the COMPILER, not just the configuration, for the reason
# bench/benchmark_index.sh gives: a tree named for the configuration alone lets a second
# toolchain's binary land in the first one's directory.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
if [ -z "${FPM_BUILD_DIR:-}" ]; then
    export FPM_BUILD_DIR="test_run/filterset-${FC_TAG}"
else
    echo "benchmark_filter_set.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

# ifx and its siblings optimise at -O2 by DEFAULT, so an absent -O is not evidence of an -O0
# build for them. Short and evidence-based: a wrong entry turns this check into the silent -O0 run
# it exists to prevent.
compiler_defaults_to_optimised() {
    case "$FC_TAG" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

MODEL="$(fpm build --profile release --show-model 2>/dev/null || true)"
FLAGS_LINE="$(printf '%s\n' "$MODEL" | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
CXX_FLAGS_LINE="$(printf '%s\n' "$MODEL" | grep -o 'cxx_compile_flags="[^"]*"' | head -n 1 || true)"
if [ -z "$FLAGS_LINE" ] || [ -z "$CXX_FLAGS_LINE" ]; then
    echo "benchmark_filter_set.sh: could not read the compile flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
elif ! compiler_defaults_to_optimised; then
    if [ "${FLAGS_LINE#*" -O"}" = "$FLAGS_LINE" ]; then
        echo "benchmark_filter_set.sh: '--profile release' produced NO optimisation flag for the Fortran half:" >&2
        echo "  $FLAGS_LINE" >&2
        echo "  Append one yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):" >&2
        echo "      FPM_FFLAGS=\"\${FPM_FFLAGS:-} -O3\" bench/benchmark_filter_set.sh ..." >&2
        echo "  and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check." >&2
        [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
    fi
    if [ "${CXX_FLAGS_LINE#*" -O"}" = "$CXX_FLAGS_LINE" ]; then
        echo "benchmark_filter_set.sh: '--profile release' produced NO optimisation flag for the C++ half:" >&2
        echo "  $CXX_FLAGS_LINE" >&2
        echo "  This is normal for nagfor and flang -- fpm gives their C/C++ half no profile flags." >&2
        echo "  Append one yourself (append, never assign -- FPM_CXXFLAGS carries Arrow's paths):" >&2
        echo "      FPM_CXXFLAGS=\"\${FPM_CXXFLAGS:-} -O3\" bench/benchmark_filter_set.sh ..." >&2
        echo "  and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check." >&2
        [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
    fi
fi

echo "# benchmark_filter_set.sh: FPM_BUILD_DIR=$FPM_BUILD_DIR"
echo "# $FLAGS_LINE"
echo "# $CXX_FLAGS_LINE"

mkdir -p "$(dirname "$FILE")"
fpm run benchmark_filter_set --profile release -- \
    --mode="$MODE" --nrows="$NROWS" --nset="$NSET" --chunk="$CHUNK" --rounds="$ROUNDS" \
    --threads="$THREADS" --file="$FILE" --keep="$KEEP"

finished=1
