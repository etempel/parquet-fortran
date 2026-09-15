#!/usr/bin/env bash
# Drives bench/benchmark_spatial.f90 -- the pf_spatial_index cell-size tuner, build and queries.
#
# Usage:
#   bench/benchmark_spatial.sh                       # the acceptance measurement, uniform 1M
#   MODE=ab bench/benchmark_spatial.sh               # re-fit the probe's A/B constant here
#   MODE=build bench/benchmark_spatial.sh            # what the probe costs
#   MODE=threads bench/benchmark_spatial.sh          # how a bulk sweep scales
#   MODE=combine bench/benchmark_spatial.sh          # what each combine= rule costs
#   MODE=backend bench/benchmark_spatial.sh          # 3D grid vs HEALPix, on the sky
#   MODE=backend DIST=clustered bench/benchmark_spatial.sh
#   MODE=los DIST=wedge SIDE=1500 RLO=0.3 RHI=0.5 RATIO=10 bench/benchmark_spatial.sh   # the LOS cylinder
#   MODE=los DIST=wedge SIDE=1500 RLO=0.3 RHI=0.5 WALK=ball bench/benchmark_spatial.sh   # its covering-ball walk
#   MODE=los DIST=wedge SIDE=1500 RLO=0.3 RHI=0.5 WALK=cylinder bench/benchmark_spatial.sh   # the cylinder for every point
#   MODE=los DIST=wedge SIDE=1500 RLO=0.3 RHI=0.5 CELLS_PER_POINT=30 bench/benchmark_spatial.sh
#   MODE=los DIST=fluxlim TS=0.25 SIDE=1500 RLO=0.3 RHI=0.5 RATIO=30 bench/benchmark_spatial.sh   # a steep selection
#   DIST=clustered NP=2000000 bench/benchmark_spatial.sh
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=tune        tune | ab | build | query | threads | combine | backend | los
#   DIST=uniform     uniform | clustered | wedge | fluxlim | sphere | flat
#                    (MODE=backend takes uniform or clustered only, on the SPHERE; MODE=los any
#                    3D fixture, the wedge and fluxlim being the survey shapes it is about)
#   TS=0.5           DIST=fluxlim only: the wedge thinned by exp(-(D / (TS*SIDE))**2), redrawn to keep
#                    NP points; smaller is steeper (0.5 and 0.25 are the two profiles measured so far).
#   SKYR=1           MODE=backend only: the build radius in DEGREES.
#   RATIO=10         MODE=los only: the cylinders' aspect ratio in distance, L*b_par / b_perp.
#   WALK=auto        MODE=los only: auto (the shipped choice per point: the cylinder unless the covering
#                    ball is no wider than a cell or shorter than the cylinder) | cylinder (forced for
#                    every point that can walk one) | ball (the covering-ball walk, forced).
#   SPREAD=local     MODE=los only: local (each emitter's own parallel window bounds its distance
#                    range, the shipped bound) | global (the catalogue-wide max(L*W, g)).
#   CELLS_PER_POINT=0  MODE=los only: > 0 relaxes the cells-per-point ceiling (shipped 0.3) through
#                    its test-only override, so the cell can follow the cross-section.
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
#           scores every candidate ratio. BOTH arms use the probe's own workload -- cloud points,
#           radii cycling the declared list -- because a ratio fitted between two different
#           workloads is a ratio for neither. On machine A the usable band is 2 to 4 and 2 is the
#           unique best by mean and by worst case, so a machine whose best row is 2 or 4 CONFIRMS
#           the shipped value; only a best row outside that is a reason to change anything.
#   build   What the probe costs: a tuned build against one at the same cell with cell= given,
#           which skips the probe entirely. The difference is the probe.
#   query   Single-query and bulk-sweep throughput at the tuned cell.
#   combine WHAT EACH PAIR RULE COSTS. Sweeps %pairs_within four times over one per-point radius
#           list drawn from RLO..RHI, once per combine= rule, and prints each arm's time, its pair
#           count and its ratio to the max arm (the default, and the released behaviour). min
#           walks smaller balls and should not be slower; mean adds two gathers per candidate; sum
#           walks twice each radius, so in three dimensions it visits about eight times the volume.
#           The pair counts must increase strictly from min to sum or the fixture is not
#           separating the rules -- the mode says so itself when they do not.
#   threads How a bulk sweep scales with the team size.
#   backend THE BACKEND COMPARISON. Builds one (ra, dec) catalogue with the 3D grid and with the
#           HEALPix pixelisation, checks the two answer identically, then times %within_sky across
#           a radius sweep from SKYR/3 to 3*SKYR. The last column is healpix/grid3d, so below 1
#           means HEALPix is faster. Run it on BOTH fixtures: `uniform` is the baseline and
#           `clustered` is where the gap should be widest, because a 3D cell holding a cluster is
#           tested in full while a disc's pixels can exclude most of it.
#             The crossover is machine-dependent and radius-dependent, which is exactly why this
#           is a mode rather than a number written down anywhere: HEALPix pays a fixed per-query
#           cost for the disc walk that the grid's integer cell arithmetic does not, and wins it
#           back on candidates only once the radius is large enough.
#   los     THE LINE-OF-SIGHT CYLINDER against the work-around it replaces, and its cylinder walk
#           against the covering-ball walk. Derives a redshift-like los= from the fixture's
#           distances, sets per-point lengths so that L*b_par/b_perp is RATIO, and times
#           %pairs_within_los -- choosing its walk per point, or forced onto the cylinder or the
#           covering ball under WALK=cylinder / WALK=ball; bounding
#           each emitter's distance range by its own parallel window, or by the catalogue-wide slope
#           under SPREAD=global -- against %pairs_within at the covering radius plus a serial re-test
#           of every candidate. Prints CANDIDATES TESTED PER PAIR KEPT for both, read for the library
#           arm from its own counter, beside the geometric volume ratio, each arm's cell and the peak
#           list the work-around held; and for the library arm's best round its five PHASES (setup,
#           rank, windows, sweep, copy) and how many emitters read a distance window. Run at
#           RATIO=10 and RATIO=30 on DIST=wedge with SURVEY dimensions, SIDE=1500 RLO=0.3 RHI=0.5
#           (a wedge 1500 deep with sub-Mpc transverse lengths), once per WALK, once per SPREAD,
#           and with CELLS_PER_POINT relaxed (30) to see how much of the remaining excess is the
#           ceiling's; DIST=fluxlim at TS=0.5 and 0.25 for a survey whose density falls with
#           distance; SIDE=1800 reaches z = 1, where the
#           global spread overshoots most. At the other modes' defaults, a 100-unit box with radii
#           1..5, each covering ball is a tenth of the box at RATIO=10 and the work-around's pair
#           list would not fit in memory; the mode counts first and refuses rather than allocate it.
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
SKYR="${SKYR:-1}"
RATIO="${RATIO:-10}"
WALK="${WALK:-auto}"
SPREAD="${SPREAD:-local}"
CELLS_PER_POINT="${CELLS_PER_POINT:-0}"
TS="${TS:-0.5}"

for arg in "$@"; do
    case "$arg" in
        -h|--help)
            awk 'NR >= 2 && !/^#/ { exit } NR >= 2 { print }' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
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
echo "  mode=$MODE dist=$DIST np=$NP nq=$NQ rounds=$ROUNDS side=$SIDE r=$RLO..$RHI threads=$THREADS ts=$TS"
echo "  skyr=$SKYR (MODE=backend only)  ratio=$RATIO walk=$WALK spread=$SPREAD cells_per_point=$CELLS_PER_POINT (MODE=los only)"
echo "=============================================================================="
echo

fpm build --profile release >/dev/null

fpm run benchmark_spatial --profile release -- \
    --mode="$MODE" --dist="$DIST" --np="$NP" --nq="$NQ" --rounds="$ROUNDS" \
    --side="$SIDE" --rlo="$RLO" --rhi="$RHI" --threads="$THREADS" --skyr="$SKYR" --ratio="$RATIO" \
    --walk="$WALK" --spread="$SPREAD" --cells="$CELLS_PER_POINT" --ts="$TS"

echo
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/spatial-bench-* to clean up)."
