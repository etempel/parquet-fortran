#!/usr/bin/env bash
# Drives bench/benchmark_join.f90 -- what parquet_table%join costs, and whether the lookup-table
# shape is paying for a sort it does not need.
#
# This script only builds and runs; bench/benchmark_join.f90 decides what each mode times.
#
# THE FIGURE THE WHOLE RUN EXISTS FOR is the last row of --mode=shape: a `pf_index_map` built
# over the small right key and probed once per left row, beside the `pf_argsort` over
# nleft+nright that the shipped engine does instead. That ratio is what decides whether S6 P7's
# hash path is worth a second engine -- and a second engine has to reproduce this stage's null,
# NaN and cardinality contracts exactly, so the bar is a large factor rather than a few percent.
#
# The two `engine:` rows above it decompose the join through the library's own public API -- the
# same %deep_copy/%append pair join_build_keys makes, and the same group-offset pf_argsort
# table_join_pairs makes. Their sum must come in UNDER the full join they decompose; if it does
# not, the harness is wrong and every figure in the run is suspect.
#
# Usage:
#   bench/benchmark_join.sh                       # every mode
#   bench/benchmark_join.sh --mode=shape          # the lookup-table join alone
#   NLEFT=20000000 NRIGHT=5000 bench/benchmark_join.sh --mode=shape
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   NLEFT=4000000   Left rows in --mode=shape. The WAVES shape is 100M against a few thousand;
#                    4M is what fits on an ordinary machine and still separates the arms.
#   NRIGHT=4000     Right rows in --mode=shape. Its key is unique by construction, so
#                    require="m:1" holds and the pf_index_map arm can build at all.
#   NSYM=1000000    Rows on EACH side for --mode=size, --mode=how and --mode=payload. Note the
#                    keys are drawn from a range equal to the row count, so a symmetric join
#                    emits roughly NSYM rows rather than NSYM^2 -- but a larger NSYM with the
#                    same range would go quadratic, which is what require= and max_rows= exist
#                    to catch and what this default deliberately avoids.
#   ROUNDS=3        Rounds per figure; the best is kept, per this repo's benchmarking rules.
#                    Each round joins a fresh %clone, taken outside the timer, so a round costs
#                    a clone as well -- keep ROUNDS small on the larger sizes.
#   THREADS=0       Forwarded to %join and to the bare pf_argsort arm. 0 means "ask the library",
#                    which is what a real caller gets.
#   NCOLS=8         Largest payload width --mode=payload sweeps to (it visits 1, 2, 4, 8).
#   MODE=all        shape | size | how | payload | all.
#   LTO=0           Set to 1 when you have appended LTO flags, so the build tree is named apart
#                    from its non-LTO twin.
#
# WHAT THIS MEASURES AND WHAT IT DOES NOT. Every table is built in memory, so no figure opens a
# file, none reaches src/parquet_wrapper.cpp, and this wrapper deliberately does NOT assert
# anything about FPM_CXXFLAGS -- a gate that cannot fail meaningfully is worse than none (see
# bench/benchmark_index.sh, which says the same, and bench/benchmark_colindex.sh, which must).
# It follows that these numbers say nothing about a join whose payload columns are still on
# disk: `columns=` naming an unread column reads it, and that read is Arrow's cost, not the
# join's.
#
# Maintainer-only: nothing outside this repository runs it.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_join.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

cd "$(dirname "$0")/.."

NLEFT="${NLEFT:-4000000}"
NRIGHT="${NRIGHT:-4000}"
NSYM="${NSYM:-1000000}"
ROUNDS="${ROUNDS:-3}"
THREADS="${THREADS:-0}"
NCOLS="${NCOLS:-8}"
MODE="${MODE:-all}"
LTO="${LTO:-0}"

for arg in "$@"; do
    case "$arg" in
        --mode=*) MODE="${arg#--mode=}" ;;
        --nleft=*) NLEFT="${arg#--nleft=}" ;;
        --nright=*) NRIGHT="${arg#--nright=}" ;;
        --nsym=*) NSYM="${arg#--nsym=}" ;;
        --rounds=*) ROUNDS="${arg#--rounds=}" ;;
        --threads=*) THREADS="${arg#--threads=}" ;;
        --ncols=*) NCOLS="${arg#--ncols=}" ;;
        -h|--help) sed -n '2,53p' "$0"; finished=1; exit 0 ;;
        *) echo "benchmark_join.sh: unknown argument '$arg'" >&2; exit 2 ;;
    esac
done

# The build tree carries the COMPILER, not just the configuration: fpm keeps objects apart per
# compiler but `find ... | head -n 1` lookups elsewhere in this repo do not, so a tree named for
# the configuration alone lets a second toolchain's binary land in the first one's directory.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
LTO_TAG=""
[ "$LTO" = "1" ] && LTO_TAG="-lto"
if [ -z "${FPM_BUILD_DIR:-}" ]; then
    export FPM_BUILD_DIR="test_run/join-${FC_TAG}${LTO_TAG}"
else
    echo "benchmark_join.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
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
    echo "benchmark_join.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
elif [ "${FLAGS_LINE#*" -O"}" = "$FLAGS_LINE" ] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_join.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

This would be an -O0 run reported as a release one. If THIS compiler optimises by default (ifx
does, at -O2), add it to compiler_defaults_to_optimised() in this script rather than reaching for
SKIP_OPT_CHECK -- an override and an appended -O3 are two different configurations, and mixing
them across machines is a comparability trap.

Fix by appending the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_join.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
fi

echo "# benchmark_join.sh: FPM_BUILD_DIR=$FPM_BUILD_DIR"
echo "# $FLAGS_LINE"

fpm run benchmark_join --profile release -- \
    --mode="$MODE" --nleft="$NLEFT" --nright="$NRIGHT" --nsym="$NSYM" \
    --rounds="$ROUNDS" --threads="$THREADS" --ncols="$NCOLS"

finished=1
