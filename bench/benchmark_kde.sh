#!/usr/bin/env bash
# What `parquet_kde` costs: `pf_kde`'s exact queries against the bandwidth, `pf_kde_grid`'s deposit
# against the cells one kernel reaches and its merge against the grid's size, and how fast the grid
# converges to the exact estimate.
#
# Three modes, all driving `bench/benchmark_kde.f90`:
#
#   evaluate  `pf_kde%pdf` and `%cdf` in microseconds per query point, one call over an array of
#             QUERIES points spread across the sample, per kernel, at bandwidths of 1/50, 1/200 and
#             1/1000 of the sample's range. `window` is the share of the sample within one kernel's
#             reach of a query, on average: a query sums exactly those points, so its cost should
#             follow that column rather than the sample's size. The sample is dense at its two ends
#             and thin in the middle, so the column is an average over very different windows.
#
#   grid      `pf_kde_grid%add` in nanoseconds per point, on ONE thread, per kernel, at 2, 4, 8
#             and 16 cells per bandwidth; `cells` is the number of cells one kernel reaches, which
#             the deposit should cost in proportion to. `mass-1` is the grid's total weight over the
#             points' less one, which the deposit promises is zero to rounding at every cell width:
#             the mode EXITS NONZERO when a grid misses by more than 1e-10, since a deposit that
#             loses weight is not the same computation made cheaper. Rounding over a million
#             deposits of one size reaches 1e-13 (the box kernel's); a deposit normalised by the
#             midpoint rule instead of by its own sum misses by 1e-8 and more. The mode closes with
#             `%merge` of sixteen grids into one, per cell merged. `threads=` on `%add` is not timed
#             here.
#
#   accuracy  No timing. The grid's `%pdf` against `pf_kde%pdf` at QUERIES irregular points, and its
#             `%density` against the exact estimate at its own centres, over a ladder of cell widths
#             from 25 cells to 1600, per kernel. `order` is log2 of the RMS error's fall between two
#             rungs: 2 is the second order the guide page states for the Gaussian and B-spline
#             kernels; the Epanechnikov estimate has kinks and the box estimate steps, and both
#             converge more slowly. `centres` is the largest gap at a centre over the peak density.
#
# The guide page quotes no figure from here, only statements `evaluate` and `accuracy` must keep
# true: a query costs in proportion to the points within reach of it, and the grid converges as the
# square of its cell width for the two smooth kernels. Re-read them against a new run.
#
# Usage:
#   bench/benchmark_kde.sh                     # every mode
#   MODE=grid bench/benchmark_kde.sh           # one of them
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=all          evaluate, grid, accuracy, or all.
#   ROUNDS=5          Timed laps per figure; the FASTEST is kept, never the mean, because the slow
#                     laps are the machine's other work rather than this code's.
#   POINTS=100000     The sample's size in `evaluate` and `accuracy`.
#   GRID_POINTS=1000000
#                     The sample's size in `grid`.
#   QUERIES=10000     Query points per call in `evaluate`, and the error's points in `accuracy`.
#
# Name the machine and the toolchain when reporting a figure from this script
# (`tools/machine_report.sh` prints both), and quote the load average: this repository's reference
# machine runs other work beside a benchmark.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-all}"
ROUNDS="${ROUNDS:-5}"
POINTS="${POINTS:-100000}"
GRID_POINTS="${GRID_POINTS:-1000000}"
QUERIES="${QUERIES:-10000}"

case "$MODE" in
    evaluate|grid|accuracy|all) ;;
    *) echo "benchmark_kde.sh: MODE must be evaluate, grid, accuracy or all (got '$MODE')" >&2
       exit 2 ;;
esac

# A timing from a build without optimisation is not a slower measurement of the same thing, it is
# a measurement of a different program: the refusal below keeps a `--profile release` figure from
# being quoted off a debug build.
model_flags="$(fpm build --profile release --show-model 2>/dev/null \
    | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$model_flags" ]]; then
    echo "benchmark_kde.sh: could not read the compile flags from 'fpm build --show-model'" >&2
    exit 2
fi
if ! grep -qE '\-O[123s]|\-Ofast' <<<"$model_flags"; then
    echo "benchmark_kde.sh: the release profile carries no optimisation flag; refusing to" >&2
    echo "  report a timing from it. Flags were: $model_flags" >&2
    exit 2
fi

echo "=== parquet_kde benchmark ==="
echo "flags   : $model_flags"
echo "rounds  : $ROUNDS (fastest kept)"
echo "load    : $(uptime | sed 's/.*load average: //')"
echo

if [[ "$MODE" == "evaluate" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=evaluate --rounds="$ROUNDS" --points="$POINTS" --queries="$QUERIES"
    echo
fi

if [[ "$MODE" == "grid" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=grid --rounds="$ROUNDS" --points="$GRID_POINTS"
    echo
fi

if [[ "$MODE" == "accuracy" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=accuracy --points="$POINTS" --queries="$QUERIES"
    echo
fi
