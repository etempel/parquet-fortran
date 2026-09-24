#!/usr/bin/env bash
# What `parquet_cosmology` costs: building a cosmology against its `zmax`, a query inside the
# table against one beyond it, each binding's own cost, and the two inverses.
#
# Five modes, all driving `bench/benchmark_cosmology.f90`:
#
#   build     `%init` at seven values of `zmax` from 1 to 1e10, in milliseconds, beside the
#             table's node count and the integrand evaluations the build actually made (read from
#             `parquet_debug_cosmology_neval`). The grid spacing in `ln(1+z)` is fixed, so the
#             node count rises only as the LOGARITHM of `zmax`: the whole point of the coordinate
#             is that reaching recombination costs a few hundred more nodes than reaching z = 1,
#             not a thousand times more. The nanoseconds-per-evaluation column is the integrand's
#             own cost, which is where a model with massive neutrinos pays its `pow`.
#
#   eval      `%comoving_distance` over a column of a million redshifts in random order, in
#             nanoseconds per query, for five ranges: inside the table, one panel beyond it,
#             sixteen panels beyond it (z = 1e10), a blueshift the table reaches and one below it.
#             The tabulated rows are an interpolant read; the rest add 20 evaluations of E per
#             panel. This is the figure behind the guide's "tens of nanoseconds inside it, a few
#             microseconds outside", and the last two are what `zmin=` trades between.
#
#   bindings  each binding over the same column, inside the table, in nanoseconds per query. They
#             should differ by a few flops and one transcendental at most; a row far above the
#             others is a binding doing more work than the design says it does. `%growth_factor`
#             and `%growth_rate` read a table of their own -- filled by an ODE rather than by a
#             quadrature -- so they belong with the others rather than above them.
#
#   sound     the four sound-horizon bindings, in MICROseconds. They are the only ones in the
#             module that tabulate nothing: `%sound_horizon` lays panels of a twenty-point rule
#             over the whole sound-crossing history at every call, and `%z_drag` does that inside
#             a solve. The column is capped at two thousand for that reason, and the rows sweep
#             the redshift because the panel COUNT follows the interval's own length -- the top of
#             the table is the cheapest place to ask and a deep blueshift the dearest. The last
#             row of the four repeats the first one element at a time, which is how a program
#             actually calls it.
#
#   inverse   all five inverses over a column, with the worst relative round trip printed beside
#             each. On the table an inverse is one interpolant read plus one Newton step; beyond
#             it, a bracketed solve over the panel rule, which is the expensive case. The three
#             that used to have no table of their own -- `%z_at_age`, and the two luminosity
#             inverses, which bracket from the distance table instead -- are the rows to watch.
#
# USAGE
#   bench/benchmark_cosmology.sh [build|eval|bindings|inverse|sound|all] [queries]
#
# NAME THE MACHINE AND THE TOOLCHAIN on any figure taken from this; `tools/machine_report.sh`
# prints both, and is run below. No guide page carries a machine name -- see
# `.claude/rules/documentation.md`.
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_cosmology.sh TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

MODE="${1:-all}"
QUERIES="${2:-1000000}"

case "$MODE" in
    build|eval|bindings|inverse|sound|all) ;;
    *) echo "benchmark_cosmology.sh: unknown mode '$MODE'" >&2; exit 2 ;;
esac

cd "$(dirname "$0")/.."

echo "=== benchmark_cosmology ==="
echo "date    : $(date -u '+%Y-%m-%d %H:%M:%SZ')"
echo "commit  : $(git rev-parse --short HEAD 2>/dev/null || echo 'not a git checkout')," \
     "$(git status --porcelain 2>/dev/null | wc -l | tr -d ' ') path(s) modified"
echo "queries : $QUERIES per timed column"
echo
echo "--- tools/machine_report.sh, in this shell ---"
bash tools/machine_report.sh 2>&1 || true
echo

# Built once up front, so that no mode's output carries the build's.
fpm build --profile release

for mode in build eval bindings inverse sound; do
    if [[ "$MODE" == "$mode" || "$MODE" == "all" ]]; then
        echo
        echo "load    : $(uptime | sed 's/.*load average[s]*: //')"
        fpm run benchmark_cosmology --profile release -- "$mode" "$QUERIES"
    fi
done
echo
echo "load    : $(uptime | sed 's/.*load average[s]*: //') (at the end)"

finished=1
