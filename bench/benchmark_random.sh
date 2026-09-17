#!/usr/bin/env bash
# Drives bench/benchmark_random.f90 -- pf_random_* against the intrinsic random_number.
#
# The two generators do not do the same job (see the program's own header), so this measures a
# price, not a defect: counter addressing, reproducibility across compilers, and thread safety are
# what the difference buys.
#
# Usage:
#   bench/benchmark_random.sh
#   ROUNDS=9 bench/benchmark_random.sh
#   SIZES=10000,1000000 bench/benchmark_random.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   SIZES=10000,1000000,100000000   Bulk fill lengths. The largest dominates the run time and the
#                                    memory: 1e8 real64 is 800 MB.
#   ROUNDS=5                        Rounds per arm; the best is kept, per this repo's rules.
#   SCALAR=10000000                 Iterations in the scalar arm.
#
# Bash 3.2 compatible (two of the three machines are macOS), and it refuses rather than degrades
# when it cannot confirm the build is optimised -- both per CLAUDE.md.
# ---------------------------------------------------------------------------------------------
# Times `pf_random_*` against the intrinsic `random_number`, per value,
# driving `bench/benchmark_random.f90`. Three arms: the `real64` and `real32` bulk fills (`call
# random_number(v)` against `call pf_random_fill_draws(seed, s, v)`, the like-for-like array shape)
# and a scalar loop (`call random_number(x)` against `x = pf_random_at(seed, i)`). Then three rows
# for a point on a sphere -- the scalar `pf_random_direction_at` loop, `%direction` on a stream and
# `pf_random_fill_direction` -- each against the two `pf_random_*` uniforms per direction it
# replaces, so the price of the transform is measured rather than stated; their length is the
# smaller of SCALAR and the largest of SIZES.
#
# Its result inverts between compilers, which is the reason to run it per machine rather than quote
# a figure. On machine B the library costs 2.2x the intrinsic under gfortran 15.2 and 0.48x -- i.e.
# it is over twice as fast -- under ifx 2026.1, because ifx's own `random_number` is about 11 ns per
# value for both kinds while gfortran's is 3.9 (`real64`) and 1.4 (`real32`). Neither figure says
# anything about this library on its own.
#
# Read the ratio as a price rather than a defect: the two generators do not do the same job. The
# intrinsic advances hidden per-process state, so no value can be named, nothing is reproducible
# across compilers, and concurrent use needs care; `pf_random_*` is counter-based, so every value
# has a coordinate, any value can be produced without producing its predecessors, and the answer is
# frozen by `pf_random_algorithm`. The scalar arm is not even like-for-like -- the library arm
# addresses a fresh stream per iteration, which the intrinsic cannot express at all.
#
# The harness ties itself down to an independently measured anchor, per this project's rule that a
# benchmark replicating library call shapes is untested code until one of its rows reproduces a
# figure measured elsewhere: its intrinsic rows match a standalone program that never links this
# library, to within 0.1% on both compilers. Build trees go to `test_run/random-bench-<compiler>/`.
# Maintainer-only (stripped from the fpm-published package).
#
#     bench/benchmark_random.sh
#     ROUNDS=9 bench/benchmark_random.sh
#     SIZES=10000,1000000 bench/benchmark_random.sh
# ---------------------------------------------------------------------------------------------
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

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" bench/benchmark_random.sh

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
