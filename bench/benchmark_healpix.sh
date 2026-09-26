#!/usr/bin/env bash
# Drives bench/benchmark_healpix.f90 -- the native HEALPix benchmark (TA-5).
#
# This is the in-repository half of the tier-A work and the whole measurement instrument for
# tier B. It links NO libhealpix, so it travels with the repository and any machine can reproduce
# the native side; the GPL-linking comparison lives out of tree in test_run/healpix_bench/ (decision
# D16).
#
# Usage:
#   bench/benchmark_healpix.sh --mode=disc
#   MODE=bulk THREADS=1,2,4,8,16 bench/benchmark_healpix.sh
#   ELEMENTS=1000000 ROUNDS=2 bench/benchmark_healpix.sh --mode=cross
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=all         conv | bulk | cross | disc | ovh | grid | dist | all
#   ELEMENTS=10000000  largest bulk array size. Memory is roughly 105 bytes per element
#                      (two input angle arrays, two output angle arrays, two 3-vectors, four index
#                      arrays), so 1e7 costs about 1.05 GB.
#   QUERIES=50000    disc queries per timed row.
#   ROUNDS=3         timed repetitions per figure; the fastest is kept, per this repo's rules.
#   NSIDE=1024       resolution for the conv/bulk/cross arms. At most 8192: half the bulk table is
#                      int32 specifics, whose pixel indices cannot address more.
#   THREADS=1,2,4,8,16  team sizes to sweep.
#
# WHICH NUMBER TO READ, per mode:
#
#   conv   Nanoseconds per call for each scalar entry point, per nside. The int32 and int64 rows of
#          one entry point differ only by a widening conversion, so a large gap between them is a
#          finding, not noise.
#   bulk   Section 7.8's G2 and T1. G2 is the "thr=1" column against the "scalar" column -- the
#          bulk wrapper must be within 5% of a hand-written loop over the elemental form, or it
#          costs more than it saves. T1 is the "thr=8" column against "thr=1" at 1000000 elements:
#          the target is at least 4x. The "auto" column omits threads= entirely, which is what
#          library callers get.
#   cross  Section 7.8's T2: the smallest array size at which threads=N beat threads=1, per form and
#          per team size. This is the measurement that replaces the PROVISIONAL value of
#          hpx_parallel_min_elements in src/parquet_healpix_bulk.f90. Read the LARGEST team size's
#          column: it is the worst case, and the constant has to hold G3 there too.
#   disc   Microseconds per disc, in the two call shapes the libhealpix baseline was measured in.
#          Compare against the recorded production columns.
#
# The host should be IDLE and the report should say so: section 1.2's own figures were taken at
# load 65/384 and carry that caveat, and re-taking a comparison against them under load repeats it.
# Check `uptime` before and after.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-all}"
ELEMENTS="${ELEMENTS:-10000000}"
QUERIES="${QUERIES:-50000}"
ROUNDS="${ROUNDS:-3}"
NSIDE="${NSIDE:-1024}"
THREADS="${THREADS:-1,2,4,8,16}"

for arg in "$@"; do
    case "$arg" in
        --mode=*)     MODE="${arg#--mode=}" ;;
        --elements=*) ELEMENTS="${arg#--elements=}" ;;
        --queries=*)  QUERIES="${arg#--queries=}" ;;
        --rounds=*)   ROUNDS="${arg#--rounds=}" ;;
        --nside=*)    NSIDE="${arg#--nside=}" ;;
        --threads=*)  THREADS="${arg#--threads=}" ;;
        -h|--help)
            sed -n '2,45p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_healpix.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

case "$MODE" in
    conv|bulk|cross|disc|ovh|grid|dist|all) ;;
    *) echo "benchmark_healpix.sh: MODE must be conv, bulk, cross, disc, ovh, grid, dist or all (got '$MODE')" >&2; exit 2 ;;
esac

# One build tree per compiler. Naming a tree for the benchmark alone lets a second toolchain's
# binary land in the first one's directory, and this campaign runs two of them on the same host.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/healpix-bench-${FC_TAG}"
else
    echo "benchmark_healpix.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

# ifx at -O2 buffers these hand-written loops of elemental calls through a temporary the size of
# the whole array, on the STACK -- so --mode=conv and --mode=bulk segfault at a million elements
# and run at a hundred thousand. `ulimit -s unlimited` does not help; -heap-arrays does, and the
# program's checksums are byte-identical either way, so this changes where a temporary lives and
# nothing else. Appended, never assigned: FPM_FFLAGS carries Arrow's include paths.
#
# It is worth knowing WHY the flag is here rather than treating it as boilerplate: the temporary
# exists because ifx collapses `do k; call elemental(a(k), b(k)); end do` into one array-wide
# elemental call. That makes the "scalar" reference column in --mode=bulk a VECTORISED array
# operation under ifx rather than a loop of calls, which is the harder reference for G2 to clear,
# not the easier one -- so a G2 pass under ifx still means what it says. gfortran does not do this.
case "$(basename "${FPM_FC:-gfortran}")" in
    ifx|ifx-*|ifort|ifort-*)
        export FPM_FFLAGS="${FPM_FFLAGS:-} -heap-arrays"
        HEAP_NOTE="  (-heap-arrays appended: see this script's header)"
        ;;
    *) HEAP_NOTE="" ;;
esac

echo "=============================================================================="
echo "benchmark_healpix.sh"
echo "  mode        : $MODE"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}$HEAP_NOTE"
echo "  elements=$ELEMENTS queries=$QUERIES rounds=$ROUNDS nside=$NSIDE threads=$THREADS"
echo "  host        : $(uname -n)   $(uptime | sed 's/^ *//')"
echo "=============================================================================="
echo

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them. Keep
# this list evidence-based: a wrong entry turns the check into the silent -O0 run it prevents.
compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

# --profile release is not optional, and asking for it is not the same as getting it: fpm 0.13.0
# alpha emits no optimisation flag at all for some compilers, so the run would be an -O0 run that
# looks exactly like a valid one. Refuse rather than degrade.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_healpix.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_healpix.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

Append it yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_healpix.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi
echo "  flags       : $FLAGS_LINE"
echo

fpm build --profile release >/dev/null

fpm run benchmark_healpix --profile release -- \
    --mode="$MODE" --elements="$ELEMENTS" --queries="$QUERIES" \
    --rounds="$ROUNDS" --nside="$NSIDE" --threads="$THREADS"

echo
echo "Host after the run: $(uptime | sed 's/^ *//')"
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/healpix-bench-* to clean up)."
