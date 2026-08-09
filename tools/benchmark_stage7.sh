#!/usr/bin/env bash
#
# Runs the stage 7 gate measurements (app/benchmark_stage7.f90) and prints, ahead of
# them, the toolchain provenance every number has to be read against.
#
# Usage:
#   tools/benchmark_stage7.sh [--both|--lto] [--only=s7-2,s7-4] [--test]
#
#   (no flags)   build and measure normally
#   --lto        build and measure with link-time optimisation instead
#   --both       measure BOTH ways and print them one after the other (this is
#                what a run on another machine should use -- it answers S7-8 and
#                every other gate in a single invocation, which is the point)
#   --only=LIST  restrict to some items, e.g. --only=s7-2 (default: all)
#   --test       additionally run the full test suite under the same build
#                flags. Slow, and required for the LTO measurement: a faster
#                build that fails the suite is not a result, and this project
#                has four documented ifx codegen bugs that appear only at -O1+.
#
# Environment:
#   FPM_FC, FPM_CXX and the FPM_*FLAGS variables are read from the environment
#   and APPENDED to, never replaced -- on every machine this project is built on
#   they already carry Arrow's include and link paths, and overwriting them
#   produces "fatal error: 'arrow/api.h' file not found", which reads like a
#   missing dependency rather than a flag mistake.
#
# The LTO flag is chosen from the Fortran compiler: -ipo for ifx, -flto
# otherwise. They are the same idea with different spellings; passing the wrong
# one is silently accepted by some drivers and ignored.
#
set -euo pipefail
cd "$(dirname "$0")/.."

MODE="plain"
ONLY=""
RUN_TESTS=0
for arg in "$@"; do
    case "$arg" in
        --both) MODE="both" ;;
        --lto)  MODE="lto" ;;
        --test) RUN_TESTS=1 ;;
        --only=*) ONLY="$arg" ;;
        *) echo "unknown argument: $arg" >&2; exit 2 ;;
    esac
done

# ---------------------------------------------------------------- provenance --
fc="${FPM_FC:-gfortran}"
cxx="${FPM_CXX:-c++}"
echo "=================================================================="
echo " parquet-fortran stage 7 -- toolchain provenance"
echo "=================================================================="
echo " host        : $(uname -srm)"
echo " commit      : $(git rev-parse --short HEAD 2>/dev/null || echo '(not a git checkout)')$(git diff --quiet 2>/dev/null || echo ' + UNCOMMITTED CHANGES')"
echo " FPM_FC      : ${FPM_FC:-(unset, fpm default)}"
echo " fortran     : $("$fc" --version 2>&1 | head -1 || echo '?')"
echo " FPM_CXX     : ${FPM_CXX:-(unset, fpm default)}"
echo " c++         : $("$cxx" --version 2>&1 | head -1 || echo '?')"
echo " arrow       : $(pkg-config --modversion arrow 2>/dev/null || echo '?')"
echo " parquet     : $(pkg-config --modversion parquet 2>/dev/null || echo '?')"
echo " fpm         : $(fpm --version 2>&1 | sed -n 's/^Version: *//p' | head -1)"
echo " cores       : $(nproc 2>/dev/null || sysctl -n hw.logicalcpu 2>/dev/null || echo '?')"
echo " mode        : $MODE"
echo "=================================================================="
echo

# Pick the right link-time-optimisation flag for this Fortran compiler.
if "$fc" --version 2>&1 | head -1 | grep -qi "ifx\|intel"; then
    LTO_FLAG="-ipo"
else
    LTO_FLAG="-flto"
fi

run_one() {  # $1 = "plain" | "lto"
    local label="$1"
    local ff="${FPM_FFLAGS:-}" cf="${FPM_CXXFLAGS:-}" lf="${FPM_LDFLAGS:-}"
    if [ "$label" = "lto" ]; then
        ff="$ff $LTO_FLAG"; cf="$cf $LTO_FLAG"; lf="$lf $LTO_FLAG"
        echo "###################### BUILD: $label ($LTO_FLAG) ######################"
    else
        echo "###################### BUILD: $label (no LTO) ######################"
    fi
    # Exported for this invocation only; the caller's environment is untouched.
    FPM_FFLAGS="$ff" FPM_CXXFLAGS="$cf" FPM_LDFLAGS="$lf" \
        fpm run --profile release benchmark_stage7 -- ${ONLY:+"$ONLY"}
    if [ "$RUN_TESTS" -eq 1 ]; then
        echo
        echo "-------- full test suite under this build ($label) --------"
        if FPM_FFLAGS="$ff" FPM_CXXFLAGS="$cf" FPM_LDFLAGS="$lf" fpm test > /tmp/s7_test_$label.log 2>&1; then
            echo "fpm test            : PASS ($(grep -c PASSED /tmp/s7_test_$label.log) assertions)"
        else
            echo "fpm test            : *** FAIL *** -- last 40 lines:"
            tail -40 "/tmp/s7_test_$label.log"
        fi
        if FPM_FFLAGS="$ff" FPM_CXXFLAGS="$cf" FPM_LDFLAGS="$lf" ./tools/run_error_scenarios.sh > /tmp/s7_scen_$label.log 2>&1; then
            echo "error scenarios     : PASS"
        else
            echo "error scenarios     : *** FAIL *** -- last 30 lines:"
            tail -30 "/tmp/s7_scen_$label.log"
        fi
    fi
    echo
}

case "$MODE" in
    plain) run_one plain ;;
    lto)   run_one lto ;;
    both)  run_one plain; run_one lto ;;
esac
