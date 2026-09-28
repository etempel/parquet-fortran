#!/usr/bin/env bash
# UndefinedBehaviorSanitizer over src/parquet_random.f90, on BOTH arms of its route (e) fork.
#
# Why this exists as its own script rather than a flag on check_random_kernels.sh: that one asks
# whether the two kernels AGREE, this one asks whether either of them is undefined. They differ in
# what a pass means. A kernel can produce every correct value while resting on signed overflow the
# optimiser is entitled to reason from -- this project has been caught with exactly that, where a
# wrapping multiply measured correct and the compiler still used its undefinedness to delete a
# branch two functions away.
#
# **A -ftrapv build cannot answer this and must not be substituted.** Whether a given overflow is
# instrumented there depends on optimisation level and inlining, and a site whose result is dead is
# optimised away before instrumentation while remaining fully available to the optimiser as a range
# assumption. UBSan instruments the operation itself.
#
# WHAT A CLEAN RUN LOOKS LIKE. The shipped (int128) arm must report NOTHING. The forced-wrapping arm
# must report exactly the six DOCUMENTED deliberate sites and no others:
#     random_block  -- 2 sites, the `#else` multiplies       (p0 = PHILOX_M0 * c0, p1 = ... * c2)
#     mulhilo64     -- 4 sites, the partial products         (p00, p01, p10, p11)
# Both are declared in src/parquet_random.f90's own header, with the measurement that ruled out the
# overflow-free spelling. A site OUTSIDE those two procedures is a real finding: six such sites in
# the bulk fills (`i0 + k - 1` forming huge+1 at the documented boundary) were found and fixed by
# this check's first run.
#
# Usage:
#   tools/check_random_ubsan.sh            # both arms
#   FC=<compiler> tools/check_random_ubsan.sh
#
# REQUIRES A 64-BIT libubsan. On machine B that means gcc-toolset-15, not 14:
#   source /opt/fortran/activate_gcc.sh </dev/null >/dev/null 2>&1
#   source /opt/rh/gcc-toolset-15/enable
# The activation script ends by spawning an interactive subshell, which exits immediately when
# sourced non-interactively -- so the second line is required, and without it `gfortran` is the
# system 11.5.0, which is below this project's floor and miscompiles it.
# ---------------------------------------------------------------------------------------------
# Runs UndefinedBehaviorSanitizer over the same standalone compile, on
# both arms of the same fork. It answers a different question from its sibling: that one asks
# whether the two kernels agree, this one asks whether either is undefined. The distinction is the
# whole point -- a kernel can produce every correct value while resting on signed overflow the
# optimiser is entitled to reason from, which is exactly what was seen here, where a wrapping
# multiply measured correct and the compiler still used its undefinedness to delete a branch two
# functions away. A `-ftrapv` build cannot substitute:
# whether a site is instrumented there depends on optimisation level and inlining, and a site whose
# result is dead is optimised away before instrumentation while staying available to the optimiser
# as a range assumption. A clean run means the shipped (`int128`) arm reports nothing and the
# forced-wrapping arm reports exactly the six documented deliberate sites -- `random_block`'s two
# `#else` multiplies and `mulhilo64`'s four partial products. Anything outside those two procedures
# is a real finding; its first run produced six, in the bulk fills. It needs a 64-bit `libubsan`,
# which on machine B means gcc-toolset-15 rather than 14, and it is a machine-B-only instrument --
# MacPorts gcc15 ships no `libubsan` and flang rejects `-fsanitize` for Fortran.
# Maintainer-only (stripped from the fpm-published package).
#
#     tools/check_random_ubsan.sh                   # both arms
#     FC=ifx tools/check_random_ubsan.sh            # ifx ships the wrapping arm
# ---------------------------------------------------------------------------------------------

set -u

finished=0
cleanup() {
    [ -n "${WORK:-}" ] && rm -rf "$WORK"
    if [ "$finished" != "1" ]; then
        echo "check_random_ubsan: TERMINATED EARLY -- this run proves nothing" >&2
        exit 2
    fi
}
trap cleanup EXIT

cd "$(dirname "$0")/.."
REPO="$PWD"
FC="${FC:-${FPM_FC:-gfortran}}"

if ! command -v "$FC" >/dev/null 2>&1; then
    echo "check_random_ubsan: compiler '$FC' not found on PATH" >&2
    exit 1
fi

echo "compiler: $($FC --version | head -1)"

# The floor check check_random_kernels.sh carries, for the same reason: on a machine whose ifx
# environment is active a bare `gfortran` is the system 11.5.0, and a confident set of results from
# a compiler that miscompiles the library is worse than no results.
case "$FC" in
    *gfortran*)
        major="$($FC -dumpversion 2>/dev/null | cut -d. -f1)"
        if [ -n "$major" ] && [ "$major" -lt 13 ] 2>/dev/null; then
            echo "check_random_ubsan: $FC is version $major; this project requires >= 13." >&2
            echo "  A run with this compiler proves nothing. Activate the intended toolchain." >&2
            exit 1
        fi
        ;;
esac

# parquet_random's three imports first, since this is a plain ordered compile: parquet_constants,
# parquet_expkey and parquet_ziggurat, each a leaf importing only iso_fortran_env, which is what keeps
# this standalone compile possible at all.
SRC="src/parquet_constants.f90 src/parquet_expkey.f90 src/parquet_ziggurat.f90 src/parquet_random.f90 test/test_random_reference.f90 test/test_random_vectors.f90 tools/check_random_kernels.f90"
ABS_SRC=""
for f in $SRC; do ABS_SRC="$ABS_SRC $REPO/$f"; done

# Keep in step with the `#if defined(...)` line at the top of src/parquet_random.f90.
case "$FC" in
    *flang*) FORCE="-U__flang__ -U__FLANG"; CPP="-cpp" ;;
    *ifx*|*ifort*) FORCE=""; CPP="-fpp" ;;   # already takes the wrapping arm; nothing to defeat
    *) FORCE="-U__GFORTRAN__"; CPP="-cpp" ;;
esac

WORK="$(mktemp -d)"
rc=0

run_arm() {   # run_arm <label> <extra-flags>
    local label="$1" extra="$2" out log
    log="$WORK/$label.log"
    rm -f "$WORK"/*.mod "$WORK/drv"
    if ! ( cd "$WORK" && $FC $CPP $extra -O2 -fsanitize=undefined -I. -o drv $ABS_SRC ) \
            > "$WORK/build.log" 2>&1; then
        echo "  $label: BUILD-FAIL"
        sed 's/^/        /' "$WORK/build.log" | head -8
        rc=1
        return
    fi
    ( cd "$WORK" && UBSAN_OPTIONS=halt_on_error=0:print_stacktrace=0 ./drv ) > "$log" 2>&1
    out="$(grep -a "KERNEL=" "$log" | head -1)"
    echo "  $label: ${out:-<driver printed no kernel line>}"
    # Report each distinct source location once; UBSan already deduplicates, but a build that
    # somehow ran the same site twice would otherwise read as two findings.
    if grep -aq "runtime error" "$log"; then
        grep -a "runtime error" "$log" | sed 's|.*/src/|src/|' | cut -d: -f1-3 | sort -u \
            | sed 's/^/        UB  /'
    else
        echo "        (no undefined behaviour reported)"
    fi
}

echo "--- as shipped (whichever kernel this compiler selects) ---"
run_arm "shipped" ""
echo "--- forced to the other kernel ---"
run_arm "forced " "$FORCE"

echo
echo "Expected: 'shipped' clean on a compiler with a 128-bit integer kind; 'forced' reporting"
echo "exactly the documented deliberate sites in random_block and mulhilo64. Anything else is a"
echo "finding -- see this script's header."

finished=1
exit $rc
