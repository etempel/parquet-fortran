#!/usr/bin/env bash
# What `parquet_optimize` and `parquet_prima` cost: evaluations per engine per function, what
# `threads=` buys and
# whether the answer survives it, how differential evolution's knobs trade against each other, and
# how many starts the multistart driver needs to find how many basins.
#
# Four modes, all driving `bench/benchmark_optimize.f90`:
#
#   evals     Evaluations each engine spends reaching the known minimum of four reference
#             functions -- Rosenbrock in 2 and 10 variables, the sphere in 10, Rastrigin in 5 --
#             with the seeded engines averaged over several seeds and their spread printed beside
#             the mean. The spread is not decoration: one seed is one sample of a stochastic
#             search, and a mean quoted without it says nothing about what a caller will see.
#             The simplex and the three PRIMA rows are one run from one start each and report no
#             spread, which is the honest answer for an engine that takes no seed. BOBYQA appears
#             twice: alone, and under the multistart driver as a `pf_bobyqa_solver`, which is how a
#             local model-based engine is used on a function with more than one basin. LINCOA and
#             COBYLA take the same box, LINCOA as linear constraints and COBYLA with no nonlinear
#             constraint at all, so their rows measure the engines rather than the problem: the
#             quadratic-model pair against the linear-model one.
#
#             It is the table the guide page's "which engine to reach for" section rests on, in
#             orders of magnitude only. A local engine's row on Rastrigin is expected to be
#             CHEAP and WRONG -- it converges to the nearest basin and reports success -- which is
#             the comparison the page is making.
#
#   threads   `pf_minimize_de` over an objective of configurable cost (`COST` work units per
#             evaluation) across a thread ladder 1, 2, 4, ... 32, with the wall time, the speedup,
#             and the number of population values differing from the serial arm's.
#
#             **That last column is the point, and the mode EXITS NONZERO when it is not zero.**
#             The engine claims its answer is a function of `(seed, arguments, objective)` and not
#             of the thread count; a speedup table with no such gate would happily report a
#             speedup obtained by answering a different question on each arm. The team column is
#             what the run ACHIEVED, not what it asked for, so a machine that granted fewer
#             threads is visible rather than silently flattening the ladder.
#
#             `test/test_optimize_omp.f90` asserts the same equality as a test; this mode is where
#             the ladder and the timing live, which a test may not assert. Quote
#             `OMP_PLACES=sockets` and the load average when reporting a figure from here.
#
#   params    `np`, `f_weight` and `cr` swept against each other on Rastrigin in five variables:
#             evaluations to `f < 1e-8`, averaged over several seeds, with a `hits` column
#             counting the seeds that reached it at all. A mean over seeds that mostly FAILED is
#             meaningless, which is what `hits` exists to say; read the two columns together.
#
#   starts    The multistart driver's `nstart` against the number of distinct minima it finds and
#             what it costs, on a function with six basins and on one with 121. The second is the
#             case the guide page says to use `pf_minimize_de` for instead, and this is the table
#             that shows why: the basin count keeps climbing with `nstart` and the best value does
#             not.
#
# Every number here is machine- and toolchain-dependent, and the counts are whole-search-path
# quantities: a compiler that contracts `a + F*(b - c)` into a fused multiply-add flips one
# selection and moves the total. The guide page carries orders of magnitude only.
#
# Usage:
#   bench/benchmark_optimize.sh                    # every mode
#   MODE=threads bench/benchmark_optimize.sh       # one of them
#   MODE=threads COST=2000 OMP_PLACES=sockets bench/benchmark_optimize.sh
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=both      Which mode to run: evals, threads, params, starts, or both, which runs all four.
#   ROUNDS=3       Timed rounds per thread-ladder arm; the BEST is kept, never the mean, because
#                  the slow rounds are the machine's other work rather than this code's.
#   SEEDS=5        Seeds averaged over in the `evals` and `params` modes.
#   NSTART_MAX=80  Largest start count the `starts` mode tries; it doubles from 5.
#   COST=200       Work units per objective evaluation in `threads` mode. A free objective makes
#                  the ladder measure OpenMP's own overhead; raise this until the serial arm takes
#                  a second or so, which is the regime a real objective is in.
#
# Name the machine and the toolchain when reporting a figure from this script
# (`tools/machine_report.sh` prints both), and quote the load average: this repository's reference
# machine runs other work beside a benchmark.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-both}"
ROUNDS="${ROUNDS:-3}"
SEEDS="${SEEDS:-5}"
NSTART_MAX="${NSTART_MAX:-80}"
COST="${COST:-200}"

case "$MODE" in
    evals|threads|params|starts|both) ;;
    *) echo "benchmark_optimize.sh: MODE must be evals, threads, params, starts or both" \
            "(got '$MODE')" >&2
       exit 2 ;;
esac

# A timing from a build without optimisation is not a slower measurement of the same thing, it is
# a measurement of a different program: the refusal below is what keeps a `--profile release`
# figure from being quoted off a debug build.
model_flags="$(fpm build --profile release --show-model 2>/dev/null \
    | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$model_flags" ]]; then
    echo "benchmark_optimize.sh: could not read the compile flags from 'fpm build --show-model'" >&2
    exit 2
fi
if ! grep -qE '\-O[123s]|\-Ofast' <<<"$model_flags"; then
    echo "benchmark_optimize.sh: the release profile carries no optimisation flag; refusing to" >&2
    echo "  report a timing from it. Flags were: $model_flags" >&2
    exit 2
fi

echo "=== parquet_optimize benchmark ==="
echo "flags   : $model_flags"
echo "rounds  : $ROUNDS (best kept)"
echo "seeds   : $SEEDS"
echo "load    : $(uptime | sed 's/.*load average: //')"
echo

if [[ "$MODE" == "evals" || "$MODE" == "both" ]]; then
    fpm run benchmark_optimize --profile release -- --mode=evals --seeds="$SEEDS"
    echo
fi

if [[ "$MODE" == "threads" || "$MODE" == "both" ]]; then
    fpm run benchmark_optimize --profile release -- \
        --mode=threads --rounds="$ROUNDS" --cost="$COST"
    echo
fi

if [[ "$MODE" == "params" || "$MODE" == "both" ]]; then
    fpm run benchmark_optimize --profile release -- --mode=params --seeds="$SEEDS"
    echo
fi

if [[ "$MODE" == "starts" || "$MODE" == "both" ]]; then
    fpm run benchmark_optimize --profile release -- \
        --mode=starts --nstart-max="$NSTART_MAX"
    echo
fi
