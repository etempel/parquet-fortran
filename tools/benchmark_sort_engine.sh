#!/usr/bin/env bash
# Drives app/benchmark_sort_engine.f90 -- the Stage 0 baseline for the pure-Fortran sort engine.
#
# See feature_sort.md for the design this measures and feature_benchmark_sort.md for the run sheet
# (which machine runs which mode, what the acceptance bar is, and where the report goes). This
# script only builds and runs; app/benchmark_sort_engine.f90 decides what each mode times.
#
# Usage:
#   tools/benchmark_sort_engine.sh                        # every mode, default sweep
#   tools/benchmark_sort_engine.sh --mode=argsort         # one mode
#   tools/benchmark_sort_engine.sh --mode=threads
#   PERM=64 tools/benchmark_sort_engine.sh --mode=argsort # the int64 permutation path
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   SIZES=...       Comma-separated row counts. Default 1000..20000000. The largest entry decides
#                    peak memory: roughly n*8 for the values plus n*4 (PERM=32) or n*8 (PERM=64)
#                    for the permutation, plus whatever the engine allocates internally -- which
#                    for the current C++ engine is up to three more n-element buffers, so budget
#                    ~40 bytes/row at PERM=32 and ~48 at PERM=64.
#   FAMILIES=...    Key families. Default all eight.
#   DISTS=...       Input distributions for --mode=dist. Default all eight.
#   ROUNDS=5        Max rounds per figure; the program lowers it for the largest sizes and REPORTS
#                    the count it used, because best-of-1 and best-of-5 are different numbers.
#   PERM=32         32 or 64: which permutation kind to ask the library for. Not cosmetic -- the
#                    int32 specifics allocate an int64 permutation and narrow it into a second
#                    array, so the two paths differ by one full O(n) allocation and copy.
#   THREADS=        High thread count. Empty = omp_get_max_threads().
#   STRWIDTH=16     Declared width of a string key's elements.
#   SEED=20260814   PRNG seed. The generator is xorshift64, so the same seed gives the same data on
#                    every compiler -- which is what makes a gfortran figure and an ifx figure
#                    comparable at all. Do not change it between the two arms of a comparison.
#   TAG=            Extra build-tree suffix, when you need two trees for one compiler.
#
# Output goes to stdout; redirect it per the run sheet. Build trees go under test_run/, which is
# already git-ignored -- a benchmark tool in this repository must never need a new .gitignore entry.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

SIZES="${SIZES:-1000,10000,100000,1000000,5000000,20000000}"
FAMILIES="${FAMILIES:-i32,i64,i64lo,f32,f64,str,multi2,multi3}"
DISTS="${DISTS:-rand,sorted,reverse,organ,equal,null001,null10,nan}"
ROUNDS="${ROUNDS:-5}"
PERM="${PERM:-32}"
THREADS="${THREADS:-}"
STRWIDTH="${STRWIDTH:-16}"
SEED="${SEED:-20260814}"
TAG="${TAG:-}"

MODES=(argsort dist ops)
want_mode=""
for arg in "$@"; do
    case "$arg" in
        --mode=*) want_mode="${arg#--mode=}" ;;
        -h|--help)
            sed -n '2,34p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_sort_engine.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done
if [[ -n "$want_mode" ]]; then
    case "$want_mode" in
        argsort|dist|ops|threads|all) MODES=("$want_mode") ;;
        *)
            echo "benchmark_sort_engine.sh: unknown mode '$want_mode'" >&2
            echo "  one of: argsort dist ops threads all" >&2
            exit 2
            ;;
    esac
fi

if [[ "$PERM" != "32" && "$PERM" != "64" ]]; then
    echo "benchmark_sort_engine.sh: PERM must be 32 or 64 (got '$PERM')" >&2
    exit 2
fi

# --- one build tree per (compiler, perm kind, tag) ----------------------------------------------
#
# The compiler goes in the name, not just the configuration. tools/benchmark_stage7.sh names its
# trees for the configuration alone, and running it under a second FPM_FC drops that compiler's
# binary into the same tree -- after which `find ... | head -n 1` picks between them arbitrarily.
# Machine B runs two toolchains in this campaign, so that trap is live here.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/sortbench-${FC_TAG}-p${PERM}${TAG:+-$TAG}"
else
    echo "benchmark_sort_engine.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

echo "=============================================================================="
echo "benchmark_sort_engine.sh"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  modes       : ${MODES[*]}"
echo "  sizes       : $SIZES"
echo "  families    : $FAMILIES"
echo "  perm kind   : int$PERM      rounds: $ROUNDS      seed: $SEED"
echo "  threads     : ${THREADS:-omp_get_max_threads()}"
echo "  date        : $(date -u '+%Y-%m-%dT%H:%M:%SZ')  host: $(hostname)"
echo "=============================================================================="
echo

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them.
#
# This check exists because a plain `fpm run` with no profile applies NO optimisation at all and
# every figure from it is meaningless (measured 5.7x on pf_argsort alone in an earlier campaign --
# enough to invert a comparison, and it did). It has to know the difference between "no flag,
# therefore -O0" (gfortran, flang) and "no flag, therefore this compiler's default" (ifx, icx): an
# earlier version of this check without that distinction blocked machine B's whole ifx arm.
#
# Keep the list short and evidence-based. A wrong entry turns the check into the silent -O0 run it
# exists to prevent.
compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_sort_engine.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers." >&2
    echo "  Set SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_sort_engine.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

fpm has no release profile for some compilers (flang, as of fpm 0.13.0 alpha), so this would be an
-O0 run reported as a release one.

If THIS compiler optimises by default (ifx does, at -O2), add it to compiler_defaults_to_optimised()
in this script rather than reaching for SKIP_OPT_CHECK -- an override and an appended -O3 are two
different configurations, and mixing them across machines is a comparability trap.

Otherwise append the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" tools/benchmark_sort_engine.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

# OpenMP is what the threaded arms are measured through, and the program falls back to
# system_clock without it -- which still produces plausible-looking serial numbers while every
# threaded figure silently equals its serial twin. Warn loudly rather than refusing: the serial
# arms are still valid, and on a toolchain with no omp_lib module (MacPorts flang) a serial-only
# run is the only run available.
if [[ "$FLAGS_LINE" != *fopenmp* && "$FLAGS_LINE" != *qopenmp* && "$FLAGS_LINE" != *"openmp"* ]]; then
    echo "benchmark_sort_engine.sh: WARNING -- no OpenMP flag in the compile line:" >&2
    echo "    $FLAGS_LINE" >&2
    echo "  Threaded arms will report the serial figure. Say so in the report, and prefer" >&2
    echo "  --mode=argsort/dist/ops (serial columns) over --mode=threads on this toolchain." >&2
    echo >&2
fi

fpm build --profile release >/dev/null

THREADS_ARG=()
[[ -n "$THREADS" ]] && THREADS_ARG=(--threads="$THREADS")

for m in "${MODES[@]}"; do
    fpm run benchmark_sort_engine --profile release -- \
        --mode="$m" --sizes="$SIZES" --families="$FAMILIES" --dists="$DISTS" \
        --rounds="$ROUNDS" --perm="$PERM" --strwidth="$STRWIDTH" --seed="$SEED" \
        "${THREADS_ARG[@]}"
    echo
done

echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/sortbench-* to clean up)."
