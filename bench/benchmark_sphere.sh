#!/usr/bin/env bash
# Drives bench/benchmark_sphere.f90 -- what parquet_sphere's samplers and geometry cost.
#
# Usage:
#   bench/benchmark_sphere.sh
#   MODE=pixel DRAWS=1000000 bench/benchmark_sphere.sh
#   bench/benchmark_sphere.sh --mode=polygon --rounds=5
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   MODE=all       polygon | pixel | geometry | all
#   DRAWS=200000   draws (or elements) per timed round. The 100-vertex polygon row uses a tenth of
#                  it and the strip near the floor a hundredth, since each of its draws takes
#                  hundreds of candidates.
#   ROUNDS=3       timed rounds per row; the fastest is kept, never the mean.
#
# WHICH NUMBER TO READ, per mode:
#
#   polygon   ns/draw per polygon, beside the acceptance %acceptance reports and the mean candidate
#             count the debug entry point measures over the first 20000 draws. The count must sit
#             near 1/acceptance: a candidate count of 1 on the strip means the walk stopped
#             rejecting, and the time is then meaningless. Cost per draw is roughly the candidate
#             count times a block plus a containment test over the vertices.
#   pixel     ns/draw at three resolutions, with about 3.4 candidates per draw at each; a count
#             near 1 means the walk stopped rejecting. The mask row adds the choice of a listed
#             pixel to the same walk.
#   geometry  ns per element of each conversion; the offset and the position angle run as elemental
#             calls over whole arrays.
#
# Every row prints a checksum over what it drew. Two runs of one build print the same checksums; a
# changed checksum means the row did different work, whatever its time says.
#
# Name the machine and the toolchain when reporting a figure from this script
# (tools/machine_report.sh prints both), and quote the load average.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MODE="${MODE:-all}"
DRAWS="${DRAWS:-200000}"
ROUNDS="${ROUNDS:-3}"

for arg in "$@"; do
    case "$arg" in
        --mode=*)   MODE="${arg#--mode=}" ;;
        --draws=*)  DRAWS="${arg#--draws=}" ;;
        --rounds=*) ROUNDS="${arg#--rounds=}" ;;
        -h|--help)
            sed -n '2,31p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_sphere.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

case "$MODE" in
    polygon|pixel|geometry|all) ;;
    *) echo "benchmark_sphere.sh: MODE must be polygon, pixel, geometry or all (got '$MODE')" >&2; exit 2 ;;
esac

# One build tree per compiler, so a second toolchain's binary cannot land in the first one's tree.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/sphere-bench-${FC_TAG}"
else
    echo "benchmark_sphere.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

echo "=============================================================================="
echo "benchmark_sphere.sh"
echo "  mode        : $MODE"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  draws=$DRAWS rounds=$ROUNDS"
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
# alpha emits no optimisation flag at all for some compilers. Refuse rather than degrade.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_sphere.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_sphere.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

Append it yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_sphere.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi
echo "  flags       : $FLAGS_LINE"
echo

fpm build --profile release >/dev/null

fpm run benchmark_sphere --profile release -- --mode="$MODE" --draws="$DRAWS" --rounds="$ROUNDS"

echo
echo "Host after the run: $(uptime | sed 's/^ *//')"
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/sphere-bench-* to clean up)."
