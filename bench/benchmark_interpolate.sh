#!/usr/bin/env bash
# What `parquet_interpolate` costs: building an interpolant against the size of its table, a
# query against the table's size, its spacing and the order the queries come in, a grid's build
# and query, and the rebuild the one-shot `pf_interp` repeats on every call.
#
# Four modes, all driving `bench/benchmark_interpolate.f90`:
#
#   build     `pf_interp_1d%init` per method over tables of 64 to 1 048 576 points, doubling, in
#             nanoseconds per point, on an evenly spaced table and on one with random gaps. The
#             spline's system is tridiagonal and PCHIP's slopes are local, so each column should
#             stay flat as the table grows: the flatness is the O(n) claim, and its height the
#             constant. Where a column rises at the largest tables, look at the allocator before
#             the solver: glibc hands a large freed block back to the kernel, so every rebuild
#             faults its pages in again, which a caller rebuilding such a table pays too. Running
#             with the environment variable
#               GLIBC_TUNABLES=glibc.malloc.mmap_threshold=33554432:glibc.malloc.trim_threshold=4294967296
#             keeps the blocks, and takes most of the rise away.
#
#   eval      `%eval` in nanoseconds per query, one call over an array of a million queries, at 16,
#             1024 and 65 536 points, per method, with the queries in a random order and then
#             sorted. Three tables per method: an evenly spaced one, bracketed by arithmetic; the
#             SAME object forced off the arithmetic path by `parquet_debug_interp_force_search`,
#             which changes the bracket search and nothing else, so the difference between the two
#             rows is what the arithmetic saves; and one with random gaps, which searches as the
#             forced object does. That search bisects queries in a random order, and starts sorted
#             ones from the previous query's segment, so the sorted column of those two rows is what
#             searching from a neighbour saves. The mode confirms through that hook that the even
#             table was on the arithmetic path and the uneven one was not, and refuses to report
#             otherwise: two searches of one kind compared would look like a finding. `mismatches`
#             counts the answers the forced object gave in other bits than the arithmetic did. The
#             library promises none, and the program EXITS NONZERO on any, since a faster path
#             answering differently is not the same computation made cheaper.
#
#   grid      `pf_interp_2d%init` in nanoseconds per node and `%eval` in nanoseconds per query,
#             bilinear and bicubic, on 128 by 128, 512 by 512 and 1024 by 1024 grids, with the
#             same three kinds of axes as `eval` and the same gate. A bicubic object holds four
#             arrays the size of its grid, 32 bytes a node, so the largest is about 32 MB, and its
#             queries are bound by memory: on a loaded machine they are the noisiest figures here.
#
#   oneshot   `pf_interp` against an object, answering 1, 100 and 10 000 queries on a 1024-point
#             table with the cubic spline, in microseconds for the whole job: one `pf_interp` call
#             per query, one call for all of them, and one `%init` followed by one `%eval`. The
#             first column is the rebuild the guide page warns about, and `each/object` says how
#             soon an object pays. `mismatches` counts the `pf_interp` answers that differ in any
#             bit from the object's, under the same exit rule.
#
# Every timed row prints `relerr`, the largest distance of the answers it timed from the closed
# form the fixture samples: `sin(x)` over [0, 2 pi] for a table and `sin(x) sin(y)` over [0, 2 pi]
# by [0, pi] for a grid, both of amplitude one. It is not decoration. A bracket one segment out is
# faster and wrong, and without the column it would rank first.
#
# No mode threads. `%eval` only reads an object, so a threaded bulk evaluation is the caller's own
# loop, and the promise that one object may be shared is asserted by
# `test/test_interpolate_omp.f90` rather than timed here.
#
# The guide page quotes no figure from here, only three statements that `eval` and `oneshot` must
# keep true; re-read them against a new run. An evaluation is a search for the segment plus a few
# multiplications, the search being arithmetic on an evenly spaced table, where it saves more the
# longer the table is and may save nothing on a table of a few dozen points (sorted queries there
# search faster than they divide). An array of queries in order finds each segment in a step or two
# on a table of any length. And the one-shot form repeats the build on every call, so a table
# queried more than a handful of times wants an object.
#
# Usage:
#   bench/benchmark_interpolate.sh              # every mode
#   MODE=eval bench/benchmark_interpolate.sh    # one of them
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=all          Which mode to run: build, eval, grid, oneshot, or all, which runs the four.
#   ROUNDS=5          Timed laps per figure; the FASTEST is kept, never the mean, because the
#                     slow laps are the machine's other work rather than this code's. Before
#                     them, each job is repeated in doubling counts until one lap lasts 20 ms,
#                     which is also what warms every page the job writes.
#   QUERIES=1000000   Queries per lap in `eval` and `grid`.
#
# Name the machine and the toolchain when reporting a figure from this script (it runs
# `tools/machine_report.sh` first for that), and quote the load average it prints before each
# mode: this repository's reference machine runs other work beside a benchmark.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_interpolate.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-all}"
ROUNDS="${ROUNDS:-5}"
QUERIES="${QUERIES:-1000000}"

for arg in "$@"; do
    case "$arg" in
        -h|--help) sed -n '2,78p' "$0"; finished=1; exit 0 ;;
        *) echo "benchmark_interpolate.sh: unknown argument '$arg';" \
                "configure it through MODE, ROUNDS and QUERIES" >&2
           exit 2 ;;
    esac
done

case "$MODE" in
    build|eval|grid|oneshot|all) ;;
    *) echo "benchmark_interpolate.sh: MODE must be build, eval, grid, oneshot or all" \
            "(got '$MODE')" >&2
       exit 2 ;;
esac

FC_TAG="$(basename "${FPM_FC:-gfortran}")"

# ifx optimises at -O2 when given no -O at all, and fpm's release profile for it gives none, so
# for ifx an absent -O is an optimised build. The list is deliberately short and evidence-based: a
# wrong entry turns this check into the silent -O0 run it exists to prevent.
compiler_defaults_to_optimised() {
    case "$FC_TAG" in
        ifx|ifx-*) return 0 ;;
        *) return 1 ;;
    esac
}

# A timing from a build without optimisation is not a slower measurement of the same thing, it is
# a measurement of a different program: the refusal below is what keeps a `--profile release`
# figure from being quoted off an -O0 build. The LAST -O on the line is the one the compiler obeys.
model_flags="$(fpm build --profile release --show-model 2>/dev/null \
    | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$model_flags" ]]; then
    echo "benchmark_interpolate.sh: could not read the compile flags from" \
         "'fpm build --show-model'" >&2
    exit 2
fi
flags="$(printf '%s' "$model_flags" | sed -e 's/^fortran_compile_flags="//' -e 's/"$//')"
opt_level=""
set -f
for word in $flags; do
    case "$word" in
        -O|-O[0-9]|-Os|-Ofast|-Og) opt_level="$word" ;;
    esac
done
set +f
case "$opt_level" in
    -O0|-Og) optimised=0 ;;
    "") optimised=0; if compiler_defaults_to_optimised; then optimised=1; fi ;;
    *) optimised=1 ;;
esac
if [[ "$optimised" != "1" ]]; then
    cat >&2 <<EOF
benchmark_interpolate.sh: '--profile release' leaves this compiler ($FC_TAG) unoptimised, so every
figure would describe an -O0 build. The flags were:

  $model_flags

Append an optimisation flag yourself (append, never assign: FPM_FFLAGS carries include paths), and
say in the report that you did:

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_interpolate.sh
EOF
    exit 2
fi

echo "=== parquet_interpolate benchmark ==="
if [[ -n "$opt_level" ]]; then
    echo "compiler: $FC_TAG at $opt_level"
else
    echo "compiler: $FC_TAG at its default optimisation, since no -O is given"
fi
echo "flags   : $model_flags"
echo "commit  : $(git rev-parse --short HEAD 2>/dev/null || echo 'not a git checkout')," \
     "$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ') path(s) modified"
echo "rounds  : $ROUNDS timed laps per figure, the fastest kept"
echo "queries : $QUERIES per lap in eval and grid"
echo
echo "--- tools/machine_report.sh, in this shell ---"
bash tools/machine_report.sh 2>&1 || true
echo

# Built once up front, so that no mode's output carries the build's.
fpm build --profile release

for mode in build eval grid oneshot; do
    if [[ "$MODE" == "$mode" || "$MODE" == "all" ]]; then
        echo
        echo "load    : $(uptime | sed 's/.*load average[s]*: //')"
        fpm run benchmark_interpolate --profile release -- \
            --mode="$mode" --rounds="$ROUNDS" --queries="$QUERIES"
    fi
done
echo
echo "load    : $(uptime | sed 's/.*load average[s]*: //') (at the end)"

finished=1
