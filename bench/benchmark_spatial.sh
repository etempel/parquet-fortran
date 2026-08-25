#!/usr/bin/env bash
# Drives bench/benchmark_spatial.f90 -- the pf_spatial_index cell-size tuner, build and queries.
#
# Usage:
#   bench/benchmark_spatial.sh                       # the acceptance measurement, uniform 1M
#   MODE=ab bench/benchmark_spatial.sh               # re-fit the probe's A/B constant here
#   MODE=build bench/benchmark_spatial.sh            # what the probe costs
#   MODE=threads bench/benchmark_spatial.sh          # how a bulk sweep scales
#   DIST=clustered NP=2000000 bench/benchmark_spatial.sh
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=tune        tune | ab | build | query | threads
#   DIST=uniform     uniform | clustered | wedge | sphere | flat
#   NP=1000000       points in the cloud. 1e6 real64 x 3 is 24 MB.
#   NQ=20000         single queries per timed round.
#   ROUNDS=3         rounds per arm; the best is kept, per this repo's rules.
#   SIDE=100         box side, so 1e6 in 100^3 matches the engine gate's fixture.
#   RLO=1 RHI=5      the radius range, also matching the gate.
#   THREADS=0        team size for the bulk arms; 0 means whatever OpenMP offers.
#
# WHAT EACH MODE ANSWERS, and why three of them have to be re-run per machine:
#
#   tune    The ACCEPTANCE measurement. Sweeps the cell size, times a fixed query workload at each,
#           and reports how far the cell the tuner picked lands from the swept optimum.
#           feature_pandas_S3_cubesort.md predicts a ~1.2% mean penalty and ~7.8% worst case, where
#           the cost model ALONE reaches +42.9%. Run this on every fixture: the clustered one is
#           where the model fails, so it is the one that matters.
#   ab      Re-fits the probe's A/B constant. The probe ranks a candidate cell by
#           A*cells_visited + B*points_tested; A/B = 2 was fitted on arm64/NEON and is a ratio of a
#           cache-miss-ish cost to an arithmetic-ish one -- exactly what differs on AVX2 and
#           AVX-512. The table prints each swept cell's measured time beside its two counts, then
#           scores every candidate ratio. The constant is meant to be forgiving: anything from 0.5
#           to 16 kept the mean penalty under 3.2% where it was fitted, so a machine landing
#           anywhere in that band CONFIRMS the shipped value rather than contradicting it.
#   build   What the probe costs: a tuned build against one at the same cell with cell= given,
#           which skips the probe entirely. The difference is the probe.
#   query   Single-query and bulk-sweep throughput at the tuned cell.
#   threads How a bulk sweep scales with the team size.
#
# Report the NOISE FLOOR with any figure. Re-running one binary reproduces to a fraction of a
# per cent here; anything compared across two BUILDS needs a floor measured across rebuilds with an
# untouched control arm, because code layout alone moves an untouched arm by over 10%.
#
# The C++ half is deliberately NOT gated below: parquet_spatial is Arrow-free and every timed loop
# here is Fortran, so src/parquet_wrapper.cpp's optimisation level cannot reach any figure.
#
# Bash 3.2 compatible (two of the three machines are macOS), and it refuses rather than degrades
# when it cannot confirm the build is optimised -- both per CLAUDE.md.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-tune}"
DIST="${DIST:-uniform}"
NP="${NP:-1000000}"
NQ="${NQ:-20000}"
ROUNDS="${ROUNDS:-3}"
SIDE="${SIDE:-100}"
RLO="${RLO:-1}"
RHI="${RHI:-5}"
THREADS="${THREADS:-0}"

for arg in "$@"; do
    case "$arg" in
        -h|--help)
            sed -n '2,45p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_spatial.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

# One build tree per compiler. Naming a tree for the configuration alone lets a second FPM_FC drop
# its binary into the same directory, after which fpm's own lookup picks between them arbitrarily.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="$(printf '%s' "$FC_TAG" | tr -c 'A-Za-z0-9._-' '_')"
if [ -z "${FPM_BUILD_DIR:-}" ]; then
    export FPM_BUILD_DIR="test_run/spatial-bench-${FC_TAG}"
else
    echo "benchmark_spatial.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them. Keep this
# list short and evidence-based: a wrong entry turns the check into the silent -O0 run it prevents.
compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

# --profile release is not optional (fpm applies NO optimisation without a profile), and asking for
# it is not the same as getting it (fpm 0.13.0 alpha has no release profile for flang, so the run
# would be -O0 while looking perfectly valid). Verify rather than assume.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [ -z "$FLAGS_LINE" ]; then
    echo "benchmark_spatial.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    if [ "${SKIP_OPT_CHECK:-0}" = "0" ]; then exit 1; fi
elif [ "${FLAGS_LINE#*" -O"}" = "$FLAGS_LINE" ] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_spatial.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

That would be an -O0 run reported as a release one. If THIS compiler optimises by default (ifx does,
at -O2), add it to compiler_defaults_to_optimised() in this script rather than reaching for
SKIP_OPT_CHECK -- an override and an appended -O3 are two different configurations.

Otherwise append the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_spatial.sh

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    if [ "${SKIP_OPT_CHECK:-0}" = "0" ]; then exit 1; fi
fi

echo "=============================================================================="
echo "benchmark_spatial.sh"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  flags       : $FLAGS_LINE"
echo "  uname -m    : $(uname -m)"
echo "  mode=$MODE dist=$DIST np=$NP nq=$NQ rounds=$ROUNDS side=$SIDE r=$RLO..$RHI threads=$THREADS"
echo "=============================================================================="
echo

fpm build --profile release >/dev/null

fpm run benchmark_spatial --profile release -- \
    --mode="$MODE" --dist="$DIST" --np="$NP" --nq="$NQ" --rounds="$ROUNDS" \
    --side="$SIDE" --rlo="$RLO" --rhi="$RHI" --threads="$THREADS"

echo
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/spatial-bench-* to clean up)."
