#!/usr/bin/env bash
# Drives app/benchmark_random.f90 -- pf_random_* against the intrinsic random_number.
#
# The two generators do not do the same job (see the program's own header), so this measures a
# price, not a defect: counter addressing, reproducibility across compilers, and thread safety are
# what the difference buys.
#
# Usage:
#   tools/benchmark_random.sh
#   ROUNDS=9 tools/benchmark_random.sh
#   SIZES=10000,1000000 tools/benchmark_random.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   SIZES=10000,1000000,100000000   Bulk fill lengths. The largest dominates the run time and the
#                                    memory: 1e8 real64 is 800 MB.
#   ROUNDS=5                        Rounds per arm; the best is kept, per this repo's rules.
#   SCALAR=10000000                 Iterations in the scalar arm.
#
# Bash 3.2 compatible (two of the three machines are macOS), and it refuses rather than degrades
# when it cannot confirm the build is optimised -- both per CLAUDE.md.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

SIZES="${SIZES:-10000,1000000,100000000}"
ROUNDS="${ROUNDS:-5}"
SCALAR="${SCALAR:-10000000}"

for arg in "$@"; do
    case "$arg" in
        -h|--help)
            sed -n '2,20p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_random.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done

# One build tree per compiler. Naming a tree for the configuration alone lets a second FPM_FC drop
# its binary into the same directory, after which fpm's own lookup picks between them arbitrarily --
# a trap both remote machines hit independently with an older wrapper.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/random-bench-${FC_TAG}"
else
    echo "benchmark_random.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
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
# would be -O0 while looking valid). Verify rather than assume.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_random.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_random.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

That would be an -O0 run reported as a release one. If THIS compiler optimises by default (ifx does,
at -O2), add it to compiler_defaults_to_optimised() in this script rather than reaching for
SKIP_OPT_CHECK -- an override and an appended -O3 are two different configurations.

Otherwise append the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" tools/benchmark_random.sh

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

echo "=============================================================================="
echo "benchmark_random.sh"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  flags       : $FLAGS_LINE"
echo "  uname -m    : $(uname -m)"
echo "  sizes=$SIZES rounds=$ROUNDS scalar=$SCALAR"
echo "=============================================================================="
echo

fpm build --profile release >/dev/null

fpm run benchmark_random --profile release -- \
    --sizes="$SIZES" --rounds="$ROUNDS" --scalar="$SCALAR"

echo
echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/random-bench-* to clean up)."
