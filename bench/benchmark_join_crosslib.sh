#!/usr/bin/env bash
# Drives the cross-library join comparison: parquet_table%join against pandas, astropy and STILTS.
#
# This script builds bench/benchmark_join_crosslib.f90 --profile release, then hands its path to
# bench/benchmark_join_crosslib.py, which owns the fixtures, the other three arms, the row-for-row
# comparison and the report. Nothing here decides what is measured.
#
# WHAT THE RUN ANSWERS, in the order the report prints it:
#
#   correctness -- every join this library offers, run against whichever of the other three can
#                  express the same thing, over the SAME parquet files, compared as multisets of
#                  rows. This is the half that would find a defect.
#   semantics   -- the places the four are ENTITLED to disagree, pinned so that neither reads as a
#                  defect later. Null keys are the whole list: parquet-fortran, STILTS and astropy
#                  treat a null as matching nothing, pandas matches NA to NA.
#   claims      -- the properties only parquet-fortran promises, so no other library can
#                  cross-check them: output row order under each order= value, whether the table
#                  detaches, what matched= counts, the zero patterns pairs=/other_pairs= must show
#                  under each how, and that require= and max_rows= refuse what they say they do
#                  (each with a negative control). The order claim IS cross-checked, against
#                  pandas, whose merge documents the same rule.
#   perf        -- read, join and write timed SEPARATELY on three shapes. Separately because a
#                  single end-to-end figure over a parquet pipeline mostly measures Arrow against
#                  pyarrow against parquet-mr rather than the join. STILTS is a subprocess and can
#                  only be timed whole, so it gets a control run -- the same files read and one
#                  written, with no matching -- and the difference is reported as an UPPER BOUND
#                  on its join (the control's write is lighter, so some write cost stays in the
#                  difference). Its whole-process figure is what a STILTS user actually pays.
#   floor       -- this harness's own run-to-run spread, measured with this harness on this
#                  campaign's own fixture. A floor borrowed from another tool is a fact about that
#                  tool (CLAUDE.md's benchmarking section says so at length).
#
# Usage:
#   bench/benchmark_join_crosslib.sh                     # everything, at the default sizes
#   bench/benchmark_join_crosslib.sh --stages=correctness # the comparison only, no timing
#   NLEFT=20000000 bench/benchmark_join_crosslib.sh --stages=perf
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   NLEFT=4000000    Left rows in the `lookup` shape -- a large catalogue against a small table.
#   NRIGHT=10000     Right rows there. Its key is unique by construction, so require="m:1" holds.
#   NSYM=1000000     Rows on EACH side of the `symmetric` and `one_to_one` shapes. The keys are
#                     drawn from a range equal to the row count, so a symmetric join emits roughly
#                     NSYM rows rather than NSYM^2 -- a larger NSYM over the same range goes
#                     quadratic, which is what require= and max_rows= exist to catch.
#   NCOLS=4          Payload columns per side. Names are disjoint across the two sides, so nothing
#                     is suffixed except in the one correctness case that tests suffixing.
#   ROUNDS=3         Rounds per figure; the best is kept, per this repo's benchmarking rules.
#   THREADS=0        Forwarded to %join. 0 means "ask the library", which is what a caller gets.
#   ASTROPY_MAX=5000000  astropy's join is pure-python and roughly 20x the others; above this row
#                     count it is skipped rather than allowed to dominate the run.
#   STAGES=...       correctness,semantics,claims,perf,floor
#   WORK=test_run/joinxlib   scratch directory; fixtures and every arm's output land under it.
#
# REQUIREMENTS beyond this repository: python3 with pandas, astropy, pyarrow and numpy, and a
# `stilts` on PATH. Any arm that is missing is reported as n/a rather than silently skipped.
#
# WHAT THIS MEASURES AND WHAT IT DOES NOT. Every arm reads real parquet files, so unlike
# bench/benchmark_join.sh (which builds its tables in memory and asks where the time goes INSIDE
# the join) this one does reach src/parquet_wrapper.cpp -- and so do pyarrow and parquet-mr on
# their own sides. That is the point: the read and write columns are a comparison of three parquet
# stacks, and only the join column compares join engines. Do not add them up and quote one number.
#
# Maintainer-only: nothing outside this repository runs it, and CI does not.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_join_crosslib.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

cd "$(dirname "$0")/.."

NLEFT="${NLEFT:-4000000}"
NRIGHT="${NRIGHT:-10000}"
NSYM="${NSYM:-1000000}"
NCOLS="${NCOLS:-4}"
ROUNDS="${ROUNDS:-3}"
THREADS="${THREADS:-0}"
ASTROPY_MAX="${ASTROPY_MAX:-5000000}"
STAGES="${STAGES:-correctness,semantics,claims,perf,floor}"
WORK="${WORK:-test_run/joinxlib}"
JSON="${JSON:-}"

for arg in "$@"; do
    case "$arg" in
        --nleft=*) NLEFT="${arg#--nleft=}" ;;
        --nright=*) NRIGHT="${arg#--nright=}" ;;
        --nsym=*) NSYM="${arg#--nsym=}" ;;
        --ncols=*) NCOLS="${arg#--ncols=}" ;;
        --rounds=*) ROUNDS="${arg#--rounds=}" ;;
        --threads=*) THREADS="${arg#--threads=}" ;;
        --astropy-max=*) ASTROPY_MAX="${arg#--astropy-max=}" ;;
        --stages=*) STAGES="${arg#--stages=}" ;;
        --work=*) WORK="${arg#--work=}" ;;
        --json=*) JSON="${arg#--json=}" ;;
        -h|--help) sed -n '2,58p' "$0"; finished=1; exit 0 ;;
        *) echo "benchmark_join_crosslib.sh: unknown argument '$arg'" >&2; exit 2 ;;
    esac
done

# The build tree carries the COMPILER, not just the configuration: fpm keeps objects apart per
# compiler but `find ... | head -n 1` lookups elsewhere in this repo do not, so a tree named for
# the configuration alone lets a second toolchain's binary land in the first one's directory.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
if [ -z "${FPM_BUILD_DIR:-}" ]; then
    export FPM_BUILD_DIR="test_run/joinxlib-${FC_TAG}"
else
    echo "benchmark_join_crosslib.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

# ifx and its siblings optimise at -O2 by DEFAULT, so an absent -O is not evidence of an -O0
# build for them. The list is deliberately short and evidence-based: a wrong entry turns this
# check into the silent -O0 run it exists to prevent.
compiler_defaults_to_optimised() {
    case "$FC_TAG" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

# --profile release is not optional and asking for it is not the same as getting it: fpm applies
# NO optimisation flags without a profile, and for some compilers has no release profile at all
# (flang, as of fpm 0.13.0 alpha), which makes the whole run an -O0 run that looks valid.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [ -z "$FLAGS_LINE" ]; then
    echo "benchmark_join_crosslib.sh: could not read fortran_compile_flags from --show-model." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
elif [ "${FLAGS_LINE#*" -O"}" = "$FLAGS_LINE" ] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_join_crosslib.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

This would be an -O0 run reported as a release one, and the whole point of this comparison is
that the Fortran arm is optimised. If THIS compiler optimises by default (ifx does, at -O2), add
it to compiler_defaults_to_optimised() in this script rather than reaching for SKIP_OPT_CHECK --
an override and an appended -O3 are two different configurations.

Fix by appending the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_join_crosslib.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
fi

# The C++ half matters here in a way it does not for an in-memory benchmark: every arm reads and
# writes real parquet files, so src/parquet_wrapper.cpp and all of Arrow are on the timed path.
# fpm derives its C++ compiler from the FORTRAN compiler's family and emits no profile flags at
# all for a family it does not recognise, so this half can silently be an -O0 build while the
# Fortran half is fine -- and then every read and write figure in the report is wrong.
CXX_LINE="$(fpm build --profile release --show-model 2>/dev/null \
            | grep -o 'cxx_compile_flags="[^"]*"' | head -n 1 || true)"
if [ -n "$CXX_LINE" ] && [ "${CXX_LINE#*" -O"}" = "$CXX_LINE" ]; then
    cat >&2 <<EOF
benchmark_join_crosslib.sh: '--profile release' produced NO optimisation flag for the C++ half.

  $CXX_LINE

Every read and write figure in this report passes through src/parquet_wrapper.cpp, so an -O0 C++
half makes them meaningless while the join figures stay valid. Fix by appending:

    FPM_CXXFLAGS="\${FPM_CXXFLAGS:-} -O3" bench/benchmark_join_crosslib.sh ...

(append, never assign -- FPM_CXXFLAGS carries Arrow's include path). SKIP_OPT_CHECK=1 overrides.
EOF
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
fi

echo "# benchmark_join_crosslib.sh: FPM_BUILD_DIR=$FPM_BUILD_DIR"
echo "# $FLAGS_LINE"
echo "# $CXX_LINE"

fpm build --profile release benchmark_join_crosslib >&2

BIN="$(find "$FPM_BUILD_DIR" -type f -name benchmark_join_crosslib | head -n 1)"
if [ -z "$BIN" ]; then
    echo "benchmark_join_crosslib.sh: built, but no benchmark_join_crosslib binary found under" >&2
    echo "  $FPM_BUILD_DIR -- refusing to run." >&2
    exit 1
fi
echo "# binary: $BIN"

mkdir -p "$WORK"
JSON_ARG=""
[ -n "$JSON" ] && JSON_ARG="--json=$JSON"

python3 bench/benchmark_join_crosslib.py \
    --binary="$BIN" \
    --work="$WORK/work" \
    --fixtures="$WORK/fixtures" \
    --nleft="$NLEFT" --nright="$NRIGHT" --nsym="$NSYM" --ncols="$NCOLS" \
    --rounds="$ROUNDS" --threads="$THREADS" --astropy-max="$ASTROPY_MAX" \
    --stages="$STAGES" $JSON_ARG

finished=1
