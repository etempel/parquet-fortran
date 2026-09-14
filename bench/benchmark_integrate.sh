#!/usr/bin/env bash
# What `pf_integrate` costs: accuracy against evaluations per integrand class, and the per-call
# floor the work arrays and the record buffer impose.
#
# Three modes, all driving `bench/benchmark_integrate.f90`:
#
#   cost      Four integrand classes -- smooth and oscillatory, an endpoint singularity, an
#             interior peak, and a smooth integrand that is expensive per evaluation -- over a
#             tolerance ladder, with the Wynn-epsilon extrapolation off and on. Reports the
#             evaluation count, the RELATIVE ERROR against each integrand's closed form, and the
#             microseconds per call. The error column is not decoration: an integrator that
#             stopped subdividing too early would otherwise look like the fastest one here.
#
#   overhead  A trivial integrand at a loose tolerance, which converges in one rule application,
#             so its time per call IS the floor: validation, the work arrays and (in the second
#             arm) the record buffer. A third arm integrates a sharp gaussian to a tight
#             tolerance, where the work arrays actually grow, which is what the geometric growth
#             policy exists for.
#
#   walk      What the outward walk of an infinite range costs: five tail shapes and the two
#             features QUADPACK's change of variable loses -- a narrow spike just above the lower
#             bound and a unit-width bump at 40 -- with the evaluation count, the panel count, the
#             relative error against each closed form, and the ratio against `dqagi`'s count at
#             the same tolerances. COUNTS ONLY, no timing: the count is the walk's cost and is
#             deterministic to within one rule application across compilers, while the oscillatory
#             shape would dominate a wall clock without saying anything.
#
#             The width of one panel is a parameter of the module (`TAIL_STEP`), not an argument,
#             so sweeping it means recompiling: copy `src/parquet_integrate*.f90` outside the
#             repository, edit the constant, and build them with a driver -- the module is
#             Arrow-free and needs nothing else. The sweep over 1, 2 and 3 natural-log units was
#             run that way and is recorded in the design document; a width above 1 loses the
#             spike, which is what settles it.
#
# The figures the guide page quotes are derived from `cost`, in general terms only -- the page
# carries no machine, no compiler and no exact count, because a bisection decision rests on an
# error estimate whose last bits differ between compilers.
#
# Usage:
#   bench/benchmark_integrate.sh              # every mode
#   MODE=walk bench/benchmark_integrate.sh    # one of them
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=both      Which mode to run: cost, overhead, walk, or both, which runs all three.
#   ROUNDS=5       Timed rounds per measurement; the BEST is kept, never the mean, because the
#                  slow rounds are the machine's other work rather than this code's.
#   REPEATS=200    Integrations inside one timed round. A single `pf_integrate` of a cheap
#                  integrand is well under a microsecond, which `cpu_time` cannot resolve.
#
# Name the machine and the toolchain when reporting a figure from this script
# (`tools/machine_report.sh` prints both), and quote the load average: this repository's reference
# machine runs other work beside a benchmark.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-both}"
ROUNDS="${ROUNDS:-5}"
REPEATS="${REPEATS:-200}"

case "$MODE" in
    cost|overhead|walk|both) ;;
    *) echo "benchmark_integrate.sh: MODE must be cost, overhead, walk or both (got '$MODE')" >&2
       exit 2 ;;
esac

# A timing from a build without optimisation is not a slower measurement of the same thing, it is
# a measurement of a different program: the refusal below is what keeps a `--profile release`
# figure from being quoted off a debug build.
model_flags="$(fpm build --profile release --show-model 2>/dev/null \
    | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$model_flags" ]]; then
    echo "benchmark_integrate.sh: could not read the compile flags from 'fpm build --show-model'" >&2
    exit 2
fi
if ! grep -qE '\-O[123s]|\-Ofast' <<<"$model_flags"; then
    echo "benchmark_integrate.sh: the release profile carries no optimisation flag; refusing to" >&2
    echo "  report a timing from it. Flags were: $model_flags" >&2
    exit 2
fi

echo "=== pf_integrate benchmark ==="
echo "flags   : $model_flags"
echo "rounds  : $ROUNDS (best kept)"
echo "repeats : $REPEATS integrations per timed round"
echo "load    : $(uptime | sed 's/.*load average: //')"
echo

if [[ "$MODE" == "cost" || "$MODE" == "both" ]]; then
    fpm run benchmark_integrate --profile release -- \
        --mode=cost --rounds="$ROUNDS" --repeats="$REPEATS"
    echo
fi

if [[ "$MODE" == "overhead" || "$MODE" == "both" ]]; then
    fpm run benchmark_integrate --profile release -- \
        --mode=overhead --rounds="$ROUNDS" --repeats="$REPEATS"
    echo
fi

if [[ "$MODE" == "walk" || "$MODE" == "both" ]]; then
    fpm run benchmark_integrate --profile release -- --mode=walk
    echo
fi
