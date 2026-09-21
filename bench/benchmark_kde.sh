#!/usr/bin/env bash
# What `parquet_kde` costs: `pf_kde`'s exact queries against the bandwidth, `pf_kde_grid`'s deposit
# against the cells one kernel reaches and its merge against the grid's size, how fast the grid
# converges to the exact estimate, what the adaptive kernel adds, what each bandwidth rule adds to a
# fit, what a draw costs, and how the threaded forms scale.
#
# Seven modes, all driving `bench/benchmark_kde.f90`:
#
#   evaluate  `pf_kde%pdf` and `%cdf` in microseconds per query point on ONE thread, one call
#             over an array of QUERIES points spread across the sample, per kernel, at bandwidths of
#             1/50, 1/200 and 1/1000 of the sample's range. `window` is the share of the sample
#             within one kernel's reach of a query, on average: a query sums exactly those points,
#             so its cost should follow that column rather than the sample's size. The sample is
#             dense at its two ends and thin in the middle, so the column is an average over very
#             different windows.
#
#   binned    `method="binned"` against the exact deposit at matched cell counts, on ONE thread:
#             the binning and the transform timed apart, so the transform's share is visible, and
#             the largest gap from the exact grid's cells beside each figure -- the two approximate
#             differently, so a saving at matched CELLS is not a saving at matched accuracy. The
#             mode EXITS NONZERO when the binned deposit loses weight.
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
#   adaptive  `pf_kde%fit` fixed and adaptive, in milliseconds on ONE thread, per kernel, at a
#             bandwidth of 1/200 of the sample's range, with the adaptive fit's three phases as the
#             library times them (`parquet_debug_kde_fit_nanos`): `sort` orders the survivors,
#             `pilot` deposits the pilot grid and reads its table, `lookup` gives each point its
#             bandwidth and its mass inside the support. `cells` is the pilot's, `h_j/h` the range
#             of the bandwidths over the global one, and `pdf` the microseconds per query point
#             under each fit: an adaptive query sums the points within reach of the WIDEST kernel,
#             so its cost follows `max`.
#
#   rules     `pf_kde%fit` in milliseconds on ONE thread under `rule="isj"`, `"silverman"` and
#             `"scott"`, and with the bandwidth given as a number (range/200), on samples of 1e3
#             points and every tenfold size up to POINTS. The fit given a number is the sort and
#             the bookkeeping every fit shares, so a rule's column less it is what that rule adds:
#             the ISJ rule's is one pass binning the sample and a fixed amount for the transform of
#             its 16384 cells and its fixed point, the rules of thumb's their passes over the
#             population, the interquartile range's selection among them. `isj/silverman` is the two
#             rules' bandwidths over each other.
#
#   sample    `%sample` in nanoseconds per draw on ONE thread, per kernel, at a bandwidth of
#             1/200 of the sample's range, QUERIES draws per call: from the exact form unweighted
#             (`exact`), weighted (`weighted`, a uniform located in the running weight), and with
#             `lower=` at the sample's minimum (`bounded`, where a draw beyond the bound is drawn
#             again from the same kernel), and from a grid of four cells to a bandwidth (`grid`,
#             one uniform and one quadratic per draw).
#
#   boundary  What each boundary correction costs, on `Exp(1)`'s quantiles with `lower = 0` -- a
#             density large at its bound, so the zone holds most of the mass -- at three sample
#             sizes: `%fit` in milliseconds, `%pdf`, `%cdf` inside the zone and between the zones
#             and `%quantile` inside it, `%sample` per draw, and the grid's deposit per point,
#             under `"renormalise"`, `"reflect"` and `"linear"`. Under `"linear"` a query inside a
#             zone integrates each nearby point's corrected kernel, so `cdf zone` follows the
#             window where `cdf mid` does not, and `fit` pays one such integral per point that
#             reaches a zone; `fb us` is a draw with every rejection attempt refused
#             (`parquet_debug_set_kde_sample_tries(0)`), which is the fallback inversion's own
#             cost. The zone queries are taken at a few points only, one of them costing a tenth
#             of a second at 1e5 points. It closes with the adaptive kernel with and without
#             `bandwidth_max`, whose ratio of the widest bandwidth to the global one is what
#             decides how wide the corrected zone is.
#
#   threads   `%add`, `%pdf` and `%sample` across a ladder of thread counts, 1, 2, 4, ... up to the
#             threads OpenMP offers (at most 64): milliseconds per call, the speed-up over one
#             thread, and the team each call actually opened (`parquet_debug_kde_threads_used`),
#             which is smaller than asked for where the work would not pay for a larger one.
#             `%pdf` at QUERIES points and `%sample` of 100*QUERIES draws must give the serial
#             bits at every count, and `%add`'s grid must agree with the serial one to 1e-12 of
#             its largest cell (the team partitions the points, so its sums group differently):
#             the mode EXITS NONZERO when a rung misses its gate. Run it with
#             `OMP_PLACES=sockets` and say so when quoting it; a thread bound to one core per
#             place clamps every team to the processors the mask allows.
#
# The guide page quotes no figure from here, only statements `evaluate`, `accuracy`, `adaptive`,
# `rules` and `threads` must keep true: a query costs in proportion to the points within reach of it
# (of the widest kernel, under the adaptive kernel), the grid converges as the square of its cell
# width for the two smooth kernels, the adaptive fit costs one grid pass and one look-up per point
# beyond the fixed one, the ISJ rule costs one binning pass and a fixed amount besides -- less than
# the rules of thumb on a large sample, many times more on a small one -- and a threaded query or
# sample answers the serial bits. Re-read them against a new run.
#
# Usage:
#   bench/benchmark_kde.sh                     # every mode
#   MODE=grid bench/benchmark_kde.sh           # one of them
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=all          evaluate, grid, binned, accuracy, adaptive, rules, sample, boundary,
#                     threads, or all.
#   ROUNDS=5          Timed laps per figure; the FASTEST is kept, never the mean, because the slow
#                     laps are the machine's other work rather than this code's.
#   POINTS=100000     The sample's size in `evaluate`, `accuracy`, `adaptive`, `sample` and
#                     `threads`, and the largest in `rules`.
#   GRID_POINTS=1000000
#                     The sample's size in `grid` and `binned`.
#   QUERIES=10000     Query points per call in `evaluate`, `adaptive` and `threads`, draws per
#                     call in `sample` (and a hundred times as many in `threads`), and the error's
#                     points in `accuracy`.
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
    evaluate|grid|binned|accuracy|adaptive|rules|sample|boundary|threads|all) ;;
    *) echo "benchmark_kde.sh: MODE must be evaluate, grid, binned, accuracy, adaptive, rules," \
            "sample, boundary, threads or all (got '$MODE')" >&2
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

if [[ "$MODE" == "binned" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=binned --rounds="$ROUNDS" --points="$GRID_POINTS"
    echo
fi

if [[ "$MODE" == "accuracy" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=accuracy --points="$POINTS" --queries="$QUERIES"
    echo
fi

if [[ "$MODE" == "adaptive" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=adaptive --rounds="$ROUNDS" --points="$POINTS" --queries="$QUERIES"
    echo
fi

if [[ "$MODE" == "rules" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=rules --rounds="$ROUNDS" --points="$POINTS"
    echo
fi

if [[ "$MODE" == "sample" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=sample --rounds="$ROUNDS" --points="$POINTS" --queries="$QUERIES"
    echo
fi

if [[ "$MODE" == "boundary" || "$MODE" == "all" ]]; then
    fpm run benchmark_kde --profile release -- \
        --mode=boundary --rounds="$ROUNDS" --points="$POINTS" --queries="$QUERIES"
    echo
fi

if [[ "$MODE" == "threads" || "$MODE" == "all" ]]; then
    echo "OMP_PLACES=${OMP_PLACES:-unset} OMP_PROC_BIND=${OMP_PROC_BIND:-unset}"
    fpm run benchmark_kde --profile release -- \
        --mode=threads --rounds="$ROUNDS" --points="$POINTS" --queries="$QUERIES"
    echo
fi
