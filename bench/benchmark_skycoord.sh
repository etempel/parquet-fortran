#!/usr/bin/env bash
# Drives bench/benchmark_skycoord.f90 -- whether a prepared pf_sky_rotation converts a column faster
# than pf_sky_convert, which decides whether pf_sky_convert should become a wrapper over the object.
#
# Usage:
#   bench/benchmark_skycoord.sh
#   PROFILE=flagless bench/benchmark_skycoord.sh
#   bench/benchmark_skycoord.sh --elements=4000000 --rounds=7
#
# Config (env-overridable, matching this repo's other bench/*.sh scripts):
#   PROFILE=release    release | flagless. The decision is taken on BOTH: `release` builds with
#                      `--profile release`, `flagless` with no profile at all, which is ifx's own
#                      `-O2 -fp-model=fast` and gfortran's -O0 -- so the flagless arm refuses to run
#                      under a compiler that does not optimise by default unless SKIP_OPT_CHECK=1,
#                      and a figure taken that way says so.
#   ELEMENTS=2000000   positions in the column.
#   ROUNDS=5           timed rounds per arm, interleaved; the fastest is kept, never the mean.
#
# WHICH NUMBER TO READ: `convert / object`, for both pairs. pf_sky_convert stays elemental unless the
# object is at least twice as fast, flagless and at --profile release, on every machine measured.
# `convert / named` is the price of the dispatch itself against the floor. The checksums of one pair's
# arms agree to rounding, and the largest difference between them is printed: an arm that stopped
# doing its work shows there, whatever its time says.
#
# Name the machine and the toolchain when reporting a figure from this script
# (tools/machine_report.sh prints both), and quote the load average: it is printed before and after.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

PROFILE="${PROFILE:-release}"
ELEMENTS="${ELEMENTS:-2000000}"
ROUNDS="${ROUNDS:-5}"

for arg in "$@"; do
    case "$arg" in
        --profile=*)  PROFILE="${arg#--profile=}" ;;
        --elements=*) ELEMENTS="${arg#--elements=}" ;;
        --rounds=*)   ROUNDS="${arg#--rounds=}" ;;
        -h|--help)
            sed -n '2,26p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_skycoord.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

case "$PROFILE" in
    release) PROFILE_ARGS=(--profile release) ;;
    flagless) PROFILE_ARGS=() ;;
    *) echo "benchmark_skycoord.sh: PROFILE must be release or flagless (got '$PROFILE')" >&2; exit 2 ;;
esac

# One build tree per compiler and per profile, so a second toolchain's binary cannot land in the
# first one's tree and the two profiles never share objects.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/skycoord-bench-${FC_TAG}-${PROFILE}"
else
    echo "benchmark_skycoord.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

echo "=============================================================================="
echo "benchmark_skycoord.sh"
echo "  profile     : $PROFILE"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  elements=$ELEMENTS rounds=$ROUNDS"
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

# Asking for an optimised build is not the same as getting one: fpm 0.13.0 alpha emits no
# optimisation flag at all for some compilers, and none for any without a profile. Refuse rather
# than degrade.
FLAGS_LINE="$(fpm build ${PROFILE_ARGS[@]+"${PROFILE_ARGS[@]}"} --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_skycoord.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_skycoord.sh: the '$PROFILE' build has NO optimisation flag for this compiler.

  $FLAGS_LINE

For PROFILE=release, append one yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_skycoord.sh ...

and record in the report that you did. PROFILE=flagless under such a compiler measures an
unoptimised build; SKIP_OPT_CHECK=1 runs it anyway, and the report says so.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi
echo "  flags       : $FLAGS_LINE"
echo

fpm build ${PROFILE_ARGS[@]+"${PROFILE_ARGS[@]}"} >/dev/null

fpm run benchmark_skycoord ${PROFILE_ARGS[@]+"${PROFILE_ARGS[@]}"} -- --elements="$ELEMENTS" --rounds="$ROUNDS"

echo
echo "Host after the run: $(uptime | sed 's/^ *//')"
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/skycoord-bench-* to clean up)."
