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
#   tools/benchmark_sort_engine.sh --mode=argsort --sizes=1000000
#   PERM=64 tools/benchmark_sort_engine.sh --mode=argsort # the int64 permutation path
#
# Every setting below is available BOTH as an environment variable and as a --flag of the same
# name, and the flag wins. The run sheet writes them as flags (`--sizes=1000000`), so a sheet
# command has to work as written -- an earlier version of this script accepted only the
# environment form and rejected every such line with "unknown argument", which turns a documented
# step into a deviation the runner has to invent a workaround for.
#
# Config (env-overridable; the matching --flag overrides the environment):
#   SIZES=...       Comma-separated row counts. Default 1000..5000000. The largest entry decides
#                    peak memory: roughly n*8 for the values plus n*4 (PERM=32) or n*8 (PERM=64)
#                    for the permutation, plus whatever the engine allocates internally -- which
#                    for the current C++ engine is up to three more n-element buffers, so budget
#                    ~40 bytes/row at PERM=32 and ~48 at PERM=64.
#                    It also decides RUNTIME, and by more than anything else here: measured on
#                    machine B, n=2e7 was 92% of the whole sweep's timed seconds against 7% for
#                    n=5e6 and 1% for everything at or below 1e6, because the serial arms are
#                    superlinear in n and every mode but `argsort` runs at the LARGEST size only.
#                    Adding one entry above 5e6 therefore costs far more than it looks. Large-n
#                    behaviour has its own dedicated run in the sheet (SIZES=50000000 over a
#                    reduced family list), which is where it belongs -- do not restore a big
#                    entry here to get it.
#   FAMILIES=...    Key families for `argsort`, the main baseline table. Default all eight.
#   DIST_FAMILIES=  Key families for `dist`. Default f64,i64lo,str -- one comparator-path family,
#                    one counting-path family and the string path. `dist` asks whether the SHAPE
#                    of the input changes the cost, and it multiplies eight distributions by every
#                    family at the largest size, so running all eight families here costs more
#                    than the entire size ladder while answering the same question three times.
#                    Set it to "$FAMILIES" for the exhaustive form.
#   THREAD_FAMILIES= Key families for `threads`. Default f64,i64lo,str, for the same reason: the
#                    thread ladder is one figure per family per rung. i64lo is deliberately in the
#                    list -- the counting path barely threads at all, and seeing that flat line
#                    next to f64's is the point of the mode.
#   DISTS=...       Input distributions for --mode=dist. Default all eight.
#   ROUNDS=3        Max rounds per figure; the program lowers it for the largest sizes and REPORTS
#                    the count it used, because best-of-1 and best-of-3 are different numbers.
#   PERM=32         32 or 64: which permutation kind to ask the library for. Not cosmetic -- the
#                    int32 specifics allocate an int64 permutation and narrow it into a second
#                    array, so the two paths differ by one full O(n) allocation and copy.
#   THREADS=64      High thread count: the threaded column of every mode, and the top rung of the
#                    `threads` ladder. Empty = omp_get_max_threads().
#                    64 rather than the machine maximum because on machine B (384 logical cores,
#                    two sockets) 64 threads has been measured FASTER than 384 -- past roughly one
#                    socket the merge phase is crossing NUMA nodes and SMT siblings are sharing a
#                    core's load/store units, so the extra threads cost more than they add. This
#                    also caps OMP_NUM_THREADS (below), so nothing in the run quietly opens a
#                    384-thread region behind the engine's back.
#                    Set THREADS= (empty) to restore "use every core the machine has", which is
#                    what a run investigating the scaling limit itself wants.
#   STRWIDTH=16     Declared width of a string key's elements.
#   SEED=20260814   PRNG seed. The generator is xorshift64, so the same seed gives the same data on
#                    every compiler -- which is what makes a gfortran figure and an ifx figure
#                    comparable at all. Do not change it between the two arms of a comparison.
#   ENGINE=cpp      Which sort engine to measure: cpp (the shipped one) or fortran
#                    (feature_sort.md's Stage 2 engine, reached through a debug hook). The two
#                    answer identically -- test/test_sorting.f90 asserts that -- so this changes
#                    timing and nothing else, and an A/B is two runs of this script differing only
#                    here. Do not change anything else between them.
#   TAG=            Extra build-tree suffix, when you need two trees for one compiler.
#
# Output goes to stdout; redirect it per the run sheet. Build trees go under test_run/, which is
# already git-ignored -- a benchmark tool in this repository must never need a new .gitignore entry.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

SIZES="${SIZES:-1000,10000,100000,1000000,5000000}"
FAMILIES="${FAMILIES:-i32,i64,i64lo,f32,f64,str,multi2,multi3}"
DIST_FAMILIES="${DIST_FAMILIES:-f64,i64lo,str}"
THREAD_FAMILIES="${THREAD_FAMILIES:-f64,i64lo,str}"
DISTS="${DISTS:-rand,sorted,reverse,organ,equal,null001,null10,nan}"
ROUNDS="${ROUNDS:-3}"
PERM="${PERM:-32}"
THREADS="${THREADS-64}"
STRWIDTH="${STRWIDTH:-16}"
SEED="${SEED:-20260814}"
ENGINE="${ENGINE:-cpp}"
TAG="${TAG:-}"

MODES=(argsort dist ops)
want_mode=""
for arg in "$@"; do
    case "$arg" in
        --mode=*)            want_mode="${arg#*=}" ;;
        --sizes=*)           SIZES="${arg#*=}" ;;
        --families=*)        FAMILIES="${arg#*=}" ;;
        --dist-families=*)   DIST_FAMILIES="${arg#*=}" ;;
        --thread-families=*) THREAD_FAMILIES="${arg#*=}" ;;
        --dists=*)           DISTS="${arg#*=}" ;;
        --rounds=*)          ROUNDS="${arg#*=}" ;;
        --perm=*)            PERM="${arg#*=}" ;;
        --threads=*)         THREADS="${arg#*=}" ;;
        --strwidth=*)        STRWIDTH="${arg#*=}" ;;
        --seed=*)            SEED="${arg#*=}" ;;
        --engine=*)          ENGINE="${arg#*=}" ;;
        --tag=*)             TAG="${arg#*=}" ;;
        -h|--help)
            # Print the leading comment block, however long it grows. A hard-coded line range
            # here (it used to be `sed -n '2,34p'`) silently truncates or overruns the help the
            # first time anyone edits the header, and nothing reports it.
            awk 'NR>1 { if ($0 !~ /^#/) exit; sub(/^# ?/, ""); print }' "${BASH_SOURCE[0]}"
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
        # `all` is expanded here rather than passed through to the program's own `all` mode: one
        # process can only be given one family list, and the whole point of DIST_FAMILIES is that
        # `dist` gets a different one from `argsort`. Expanding keeps the scope cut in every path.
        all) MODES=(argsort dist ops) ;;
        argsort|dist|ops|threads) MODES=("$want_mode") ;;
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
if [[ -n "$THREADS" && ! "$THREADS" =~ ^[1-9][0-9]*$ ]]; then
    echo "benchmark_sort_engine.sh: THREADS must be a positive integer or empty (got '$THREADS')" >&2
    exit 2
fi

# Cap the OpenMP runtime as well as the engine's own thread argument. Asking the engine for 64
# threads while the runtime still reports 384 available leaves every region this harness does not
# pass a count to -- and every one the library opens internally -- running at the machine maximum,
# which is exactly the configuration the cap exists to avoid measuring. An OMP_NUM_THREADS the
# caller exported deliberately wins; say so in the banner either way.
if [[ -n "$THREADS" ]]; then
    export OMP_NUM_THREADS="${OMP_NUM_THREADS:-$THREADS}"
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

# The family list is per mode, so the banner has to name the one each mode will actually use --
# otherwise a `dist` table covering three families reads as a run that lost five of them.
families_for() {
    case "$1" in
        dist)    echo "$DIST_FAMILIES" ;;
        threads) echo "$THREAD_FAMILIES" ;;
        *)       echo "$FAMILIES" ;;
    esac
}

echo "=============================================================================="
echo "benchmark_sort_engine.sh"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  modes       : ${MODES[*]}"
echo "  sizes       : $SIZES"
echo "  families    : $FAMILIES        (argsort)"
echo "                $DIST_FAMILIES        (dist)"
echo "                $THREAD_FAMILIES        (threads)"
echo "  perm kind   : int$PERM      rounds: $ROUNDS      seed: $SEED"
echo "  sort engine : $ENGINE"
echo "  threads     : ${THREADS:-omp_get_max_threads()}      OMP_NUM_THREADS=${OMP_NUM_THREADS:-(unset)}"
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
        --mode="$m" --sizes="$SIZES" --families="$(families_for "$m")" --dists="$DISTS" \
        --rounds="$ROUNDS" --perm="$PERM" --strwidth="$STRWIDTH" --seed="$SEED" \
        --engine="$ENGINE" \
        "${THREADS_ARG[@]}"
    echo
done

echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/sortbench-* to clean up)."
