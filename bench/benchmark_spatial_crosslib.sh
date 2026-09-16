#!/usr/bin/env bash
# Drives the cross-library spatial comparison: pf_spatial_index against scipy's cKDTree and
# scikit-learn's KDTree/BallTree.
#
# This script builds bench/benchmark_spatial_crosslib.f90 --profile release, then hands its path to
# bench/benchmark_spatial_crosslib.py, which owns the fixtures, the Python arms, the row-for-row
# comparison and the report. Nothing here decides what is measured.
#
# bench/benchmark_spatial.sh is the different question of where the time goes INSIDE this library
# -- the tuner, the cell size, the walk, the backends. This one treats pf_spatial_index as a black
# box and asks how it compares with what a caller would otherwise reach for.
#
# WHAT THE RUN ANSWERS, in the order the report prints it:
#
#   correct -- every query family both libraries can express, over the SAME bytes, compared as sets
#              of rows. The three AXIS shapes have no counterpart in any other library, so they are
#              cross-checked through scipy's own tree instead: the ball that contains the shape
#              supplies the candidates and numpy applies the shape's predicate. That is the
#              experiment test_axis_is_the_covering_ball_filtered runs inside the suite, with the
#              candidate set coming from a different library's walk.
#   perf    -- build, single ball queries, the bulk self-join, the pair list and k-nearest, timed
#              separately on a uniform, a mildly clustered and a tightly clumped catalogue. The
#              ranking inverts between them, which is why all three are run: a uniform grid answers
#              an empty region without descending anything and pays for a cell that lands on a
#              clump. The k-nearest rows print the ball expansions per query beside the time,
#              which is what separates "the seed radius was wrong" from "the final sort was large".
#   sky     -- the sky metric against sklearn's haversine BallTree, on a uniform sphere and on a
#              fixture piled at both poles and across 0h.
#   scale   -- whether a query costs what it RETURNS: the box grows as n**(1/3) so the density is
#              fixed and each query returns the same count at every size.
#   floor   -- this harness's own run-to-run spread, measured with this harness on this campaign's
#              own fixture. A floor borrowed from another tool is a fact about that tool.
#
# Usage:
#   bench/benchmark_spatial_crosslib.sh                        # everything, at the default sizes
#   bench/benchmark_spatial_crosslib.sh --stages=correct       # the comparison only, no timing
#   bench/benchmark_spatial_crosslib.sh --n=4000000 --stages=perf
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   N=1000000        points in the timed catalogue.
#   NQ=20000         single queries per timed round.
#   N_CORRECT=200000 points in the correctness catalogue (every arm is compared row for row).
#   N_SKY=500000     points in the sky catalogue.
#   ROUNDS=3         rounds per figure; the best is kept, per this repo's rules.
#   THREADS=64       team size for the threaded arms on both sides.
#   STAGES=correct,perf,sky,scale,floor
#   WORK=test_run/spatialxlib
#
# Needs scipy and scikit-learn (the sky stage reports n/a without the latter). Nothing here runs
# under `fpm test` or in CI. Bash 3.2 compatible, and it refuses rather than degrades when it
# cannot confirm the build is optimised -- both per CLAUDE.md.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_spatial_crosslib.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

cd "$(dirname "$0")/.."

N="${N:-1000000}"
NQ="${NQ:-20000}"
N_CORRECT="${N_CORRECT:-200000}"
NQ_CORRECT="${NQ_CORRECT:-20000}"
N_SKY="${N_SKY:-500000}"
NQ_SKY="${NQ_SKY:-5000}"
ROUNDS="${ROUNDS:-3}"
THREADS="${THREADS:-64}"
STAGES="${STAGES:-correct,perf,sky,scale,floor}"
WORK="${WORK:-test_run/spatialxlib}"

for arg in "$@"; do
    case "$arg" in
        --n=*) N="${arg#--n=}" ;;
        --nq=*) NQ="${arg#--nq=}" ;;
        --n-correct=*) N_CORRECT="${arg#--n-correct=}" ;;
        --nq-correct=*) NQ_CORRECT="${arg#--nq-correct=}" ;;
        --n-sky=*) N_SKY="${arg#--n-sky=}" ;;
        --nq-sky=*) NQ_SKY="${arg#--nq-sky=}" ;;
        --rounds=*) ROUNDS="${arg#--rounds=}" ;;
        --threads=*) THREADS="${arg#--threads=}" ;;
        --stages=*) STAGES="${arg#--stages=}" ;;
        --work=*) WORK="${arg#--work=}" ;;
        -h|--help) awk 'NR >= 2 && !/^#/ { exit } NR >= 2 { print }' "$0" | sed 's/^# \{0,1\}//'; finished=1; exit 0 ;;
        *) echo "benchmark_spatial_crosslib.sh: unknown argument '$arg' (try --help)" >&2; exit 2 ;;
    esac
done

# One build tree per compiler. Naming a tree for the configuration alone lets a second FPM_FC drop
# its binary into the same directory, after which fpm's own lookup picks between them arbitrarily.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="$(printf '%s' "$FC_TAG" | tr -c 'A-Za-z0-9._-' '_')"
if [ -z "${FPM_BUILD_DIR:-}" ]; then
    export FPM_BUILD_DIR="test_run/spatialxlib-${FC_TAG}"
else
    echo "benchmark_spatial_crosslib.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them. Keep
# this list short and evidence-based: a wrong entry turns the check into the silent -O0 run it
# prevents.
compiler_defaults_to_optimised() {
    case "$FC_TAG" in
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
    echo "benchmark_spatial_crosslib.sh: could not read fortran_compile_flags from --show-model." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    if [ "${SKIP_OPT_CHECK:-0}" = "0" ]; then exit 1; fi
elif [ "${FLAGS_LINE#*" -O"}" = "$FLAGS_LINE" ] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_spatial_crosslib.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

That would be an -O0 run reported as a release one. If THIS compiler optimises by default (ifx does,
at -O2), add it to compiler_defaults_to_optimised() in this script rather than reaching for
SKIP_OPT_CHECK -- an override and an appended -O3 are two different configurations.

Otherwise append the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_spatial_crosslib.sh

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    if [ "${SKIP_OPT_CHECK:-0}" = "0" ]; then exit 1; fi
fi

echo "# benchmark_spatial_crosslib.sh: FPM_BUILD_DIR=$FPM_BUILD_DIR"
echo "# $FLAGS_LINE"
echo "# uname -m: $(uname -m)"

fpm build --profile release benchmark_spatial_crosslib >&2

BIN="$(find "$FPM_BUILD_DIR" -type f -name benchmark_spatial_crosslib | head -n 1)"
if [ -z "$BIN" ]; then
    echo "benchmark_spatial_crosslib.sh: built, but no benchmark_spatial_crosslib binary found under" >&2
    echo "  $FPM_BUILD_DIR -- refusing to run." >&2
    exit 1
fi
echo "# binary: $BIN"

mkdir -p "$WORK/work"
python3 bench/benchmark_spatial_crosslib.py \
    --binary="$BIN" \
    --work="$WORK/work" \
    --stages="$STAGES" \
    --n="$N" --nq="$NQ" \
    --n-correct="$N_CORRECT" --nq-correct="$NQ_CORRECT" \
    --n-sky="$N_SKY" --nq-sky="$NQ_SKY" \
    --rounds="$ROUNDS" --threads="$THREADS"

finished=1
