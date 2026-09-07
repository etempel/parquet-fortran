#!/usr/bin/env bash
# Drives bench/benchmark_index.f90 -- what a pf_index_map lookup, a build and a guarded mutation
# actually cost.
#
# This script only builds and runs; bench/benchmark_index.f90 decides what each mode times.
#
# THE TWO FIGURES EVERY OTHER NUMBER IS READ AGAINST are printed by --mode=lookup, before any
# map is built: a raw array read (the floor the direct backend should sit on) and a `findloc`
# scan (the naive alternative the module replaces). If the direct backend does not land within a
# small factor of the array read, stop -- something is wrong with the harness or the build, and
# every other figure in the run is suspect. A harness that replicates library call shapes is
# untested code until one of its rows reproduces a figure measured independently.
#
# Usage:
#   bench/benchmark_index.sh                      # every mode
#   bench/benchmark_index.sh --mode=lookup        # one mode
#   NKEYS=10000000 bench/benchmark_index.sh --mode=build
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   NKEYS=1000000   Keys in the map. The direct backend allocates 8 bytes per slot over the key
#                    RANGE, and --mode=lookup's dense pattern makes the range equal NKEYS, so
#                    memory is roughly 8 * NKEYS per map plus the probe arrays.
#   NACCESS=4000000 Lookups per timed arm. Independent of NKEYS: the probe walk wraps around.
#   ROUNDS=5        Rounds per figure; the best is kept, per this repo's benchmarking rules --
#                    the minimum is the run least disturbed by everything else on the machine.
#   THREADS=0       Team size for the threaded build, the threaded get_many arm and the
#                    contended arms. 0 means "ask the library", which is what a real caller
#                    gets; any other value is passed through so a figure is attributable to a
#                    known team size. The team a threaded get_many arm used is in its label.
#   NCOMP=4         Largest component count --mode=tuple sweeps to (it visits 1, 2 and 4).
#   REPEAT=1        --mode=multimap only: how many times each key repeats on average. The build
#                    side is NKEYS rows over NKEYS/REPEAT distinct keys, so REPEAT=1 is the
#                    distinct-key join shape and REPEAT=1000 the lookup-table one.
#   MODE=all        lookup | build | tuple | mutate | pool | multimap | all.
#   LTO=0           Set to 1 when you have appended LTO flags, so the build tree is named apart
#                    from its non-LTO twin.
#
# WHAT THIS MEASURES AND WHAT IT DOES NOT. Every figure here is Fortran-side and Arrow-free: no
# mode opens a file and none reaches src/parquet_wrapper.cpp, so this wrapper deliberately does
# NOT assert anything about FPM_CXXFLAGS. A wrapper whose timed work reaches C++ must (see
# bench/benchmark_colindex.sh); adding a gate here that cannot fail meaningfully would be worse
# than none.
#
# --mode=multimap ENDS WITH A pf_match_all ARM over the same arrays, which is the sort engine the
# join runs today and the contract %probe_many reproduces; the two answers are compared row for
# row before either is timed, so a figure there is for an answer known to be the same one.
#
# THE THREADED get_many ARM is read against the `threads=1` arm printed just above it: the pair is
# the speed-up a bulk probe gains from the team, which is what the filter's per-row-group probe
# and the join's probe actually pay. It is bandwidth-bound past a few dozen threads, so a larger
# THREADS is not automatically a larger speed-up.
#
# The contended arms in --mode=mutate and --mode=pool are what the named critical costs. They are
# the only figures here that are expected to get WORSE with more threads: every mutation of every
# map in the process takes one lock, which is the documented trade. Read them as the price of the
# shared-mutation contract, not as a scaling result.
#
# Maintainer-only: nothing outside this repository runs it.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

finished=0
trap '[ "$finished" = "1" ] || { echo "benchmark_index.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

cd "$(dirname "$0")/.."

NKEYS="${NKEYS:-1000000}"
NACCESS="${NACCESS:-4000000}"
ROUNDS="${ROUNDS:-5}"
THREADS="${THREADS:-0}"
NCOMP="${NCOMP:-4}"
REPEAT="${REPEAT:-1}"
MODE="${MODE:-all}"
LTO="${LTO:-0}"

for arg in "$@"; do
    case "$arg" in
        --mode=*) MODE="${arg#--mode=}" ;;
        --nkeys=*) NKEYS="${arg#--nkeys=}" ;;
        --naccess=*) NACCESS="${arg#--naccess=}" ;;
        --rounds=*) ROUNDS="${arg#--rounds=}" ;;
        --threads=*) THREADS="${arg#--threads=}" ;;
        --ncomp=*) NCOMP="${arg#--ncomp=}" ;;
        --repeat=*) REPEAT="${arg#--repeat=}" ;;
        -h|--help) sed -n '2,58p' "$0"; finished=1; exit 0 ;;
        *) echo "benchmark_index.sh: unknown argument '$arg'" >&2; exit 2 ;;
    esac
done

# The build tree carries the COMPILER, not just the configuration: fpm keeps objects apart per
# compiler but `find ... | head -n 1` lookups elsewhere in this repo do not, so a tree named for
# the configuration alone lets a second toolchain's binary land in the first one's directory.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
LTO_TAG=""
[ "$LTO" = "1" ] && LTO_TAG="-lto"
if [ -z "${FPM_BUILD_DIR:-}" ]; then
    export FPM_BUILD_DIR="test_run/index-${FC_TAG}${LTO_TAG}"
else
    echo "benchmark_index.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
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
    echo "benchmark_index.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
elif [ "${FLAGS_LINE#*" -O"}" = "$FLAGS_LINE" ] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_index.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

This would be an -O0 run reported as a release one. If THIS compiler optimises by default (ifx
does, at -O2), add it to compiler_defaults_to_optimised() in this script rather than reaching for
SKIP_OPT_CHECK -- an override and an appended -O3 are two different configurations, and mixing
them across machines is a comparability trap.

Fix by appending the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_index.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [ "${SKIP_OPT_CHECK:-0}" = "0" ] && exit 1
fi

echo "# benchmark_index.sh: FPM_BUILD_DIR=$FPM_BUILD_DIR"
echo "# $FLAGS_LINE"

fpm run benchmark_index --profile release -- \
    --mode="$MODE" --nkeys="$NKEYS" --naccess="$NACCESS" \
    --rounds="$ROUNDS" --threads="$THREADS" --ncomp="$NCOMP" --repeat="$REPEAT"

finished=1
