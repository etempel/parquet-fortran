#!/usr/bin/env bash
#
# Assert the ARGSORT TIER builds and runs with a bare compiler -- no fpm, no dependency
# resolver, no Arrow anywhere on the system.
#
# WHY THIS EXISTS. `parquet_argsort` is the tier `parquet_sampling` and `parquet_sorting` both
# sit on, and its value is that its Fortran graph reaches nothing but `parquet_settings_base`.
# That property has exactly one failure mode: someone adds a `use` line for a convenience, the
# graph acquires `parquet_bindings`, and every consumer of the tier starts compiling the C++
# wrapper and linking the Arrow stack. `fpm build` and `fpm test` stay green throughout, because
# the library obviously has Arrow. This script is one of only two instruments that can see it,
# and the only one that would still work if the other were deleted.
#
# It is the direct descendant of tools/check_random_kernels.sh, and the precedent is not
# hypothetical: when the weighted draw first acquired `use parquet_sorting`, every configuration
# in that check died on a missing `.mod` while the full build stayed green, and nothing else
# noticed. See CLAUDE.md, "`parquet_random` is a LEAF".
#
# IF THIS FAILS, ADDING THE MISSING MODULE TO `SRC` IS THE WRONG FIX. It makes the check pass
# while destroying the property it measures. The right fix is to move whatever needed the import
# UP a tier -- into `parquet_sorting`, which may reach anything -- exactly as
# check_random_kernels.sh says for `parquet_random`. The static counterpart,
# check_parquet_argsort_stays_arrow_free in tools/check_source_conventions.py, enforces the same
# rule by reading the source; this one enforces it by refusing to build.
#
# COMPILE ORDER IS LOAD-BEARING AND THERE IS NO RESOLVER TO FIX IT. A module spec must be
# compiled before its own submodules, so the order below is settings_base, the argsort spec, then
# the engine and kernel submodules in either order. Getting it wrong is a hard failure, which is
# the point of building this way at all.
#
# NOT AN OPENMP BUILD. No `-fopenmp`/`-openmp` is passed, so every `#ifdef _OPENMP` arm compiles
# out and `pf_sort_threads` resolves to 1. That is deliberate: it keeps the check free of a
# runtime that a bare compiler may not have configured, and it exercises the serial fallbacks,
# which nothing else here does. It also means this script proves nothing about the THREADED
# paths -- test/test_sorting.f90 does that, under fpm.
#
# Usage:  tools/check_argsort_standalone.sh
#   FC=<compiler>   Fortran compiler to use (default: $FPM_FC, else gfortran).
#
# bash 3.2 only (macOS ships 3.2): no associative arrays, no mapfile, no ${var,,}.
set -u

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

# Follow FPM_FC when FC is unset, as check_exp_key.sh does: without it, a run from an activated
# ifx or nagfor environment silently selects whatever bare `gfortran` is on PATH and reports a
# result for a compiler nothing here ships.
FC="${FC:-${FPM_FC:-gfortran}}"

if ! command -v "$FC" >/dev/null 2>&1; then
    echo "check_argsort_standalone.sh: compiler '$FC' not found on PATH" >&2
    exit 2
fi

case "$FC" in
    *gfortran*|*gcc*)
        ver="$("$FC" -dumpfullversion 2>/dev/null || "$FC" -dumpversion 2>/dev/null)"
        major="${ver%%.*}"
        if [ -n "$major" ] && [ "$major" -lt 13 ] 2>/dev/null; then
            echo "check_argsort_standalone.sh: $FC is $ver; this project requires gfortran >= 13." >&2
            echo "  A run with this compiler proves nothing." >&2
            exit 2
        fi
        ;;
esac

# THE WHOLE TIER, in compile order. Every entry here is a claim: that this file is part of the
# argsort tier and that the tier needs nothing else. Do not extend it to make a failure go away.
SRC="src/parquet_settings_base.f90 \
src/parquet_argsort.f90 \
src/parquet_argsort_engine.f90 \
src/parquet_argsort_kernel.f90 \
tools/check_argsort_standalone.f90"

# ABSOLUTE paths, because each build runs with the WORK directory as its cwd -- gfortran searches
# the CURRENT directory for `.mod` files, so a stray parquet_argsort.mod left in the repository
# root by a hand-run compile without `-J` would shadow this script's own output and fail every
# configuration with a message that reads as a defect in the library. A global `*.mod` gitignore
# hides such a file from `git status`. See CLAUDE.md, "A hand-run `gfortran` without `-J`".
ABS_SRC=""
for f in $SRC; do ABS_SRC="$ABS_SRC $ROOT_DIR/$f"; done

WORK="$(mktemp -d)"
# `finished` guards against the script stopping early and still exiting 0. A check whose failure
# mode is a green report is worse than no check, because it also removes the doubt that would
# have prompted a look.
finished=0
trap '[ "$finished" = "1" ] || { echo "check_argsort_standalone.sh: TERMINATED EARLY -- this run proves nothing" >&2; rm -rf "$WORK"; exit 2; }' EXIT

case "$($FC --version 2>&1 | head -1)" in
    *ifx*|*ifort*) PP="-fpp"; MD="-module $WORK"; CONFIGS=("-O0" "-O2" "-O3") ;;
    *NAG*)         PP="-fpp"; MD="-mdir $WORK"; CONFIGS=("-O0" "-O2" "-O3") ;;
    *flang*)       PP="-cpp"; MD="-J$WORK"; CONFIGS=("-O0" "-O2" "-O3") ;;
    *)             PP="-cpp"; MD="-J$WORK"; CONFIGS=("-O0" "-O2" "-O3") ;;
esac

echo "check_argsort_standalone.sh: $FC -- $($FC --version 2>&1 | head -1)"

built=0
failed=0
for cfg in "${CONFIGS[@]}"; do
    exe="$WORK/probe"
    rm -f "$exe" "$WORK"/*.mod "$WORK"/*.smod 2>/dev/null
    # shellcheck disable=SC2086
    if ! ( cd "$WORK" && $FC $PP $cfg $MD -o "$exe" $ABS_SRC ) > "$WORK/build.log" 2>&1; then
        echo "  [FAIL] $cfg -- did not build"
        sed -n '1,25p' "$WORK/build.log"
        failed=$((failed + 1))
        continue
    fi
    if ! out="$("$exe" 2>&1)"; then
        echo "  [FAIL] $cfg -- built but did not run"
        echo "$out"
        failed=$((failed + 1))
        continue
    fi
    # Every case prints "<name> bad=<n>"; anything but 0 is a wrong answer, not a build problem.
    if echo "$out" | grep -qv 'bad=0$'; then
        echo "  [FAIL] $cfg -- ran, but a case reported a wrong answer:"
        echo "$out" | sed 's/^/         /'
        failed=$((failed + 1))
        continue
    fi
    echo "  [ok]   $cfg -- $(echo "$out" | grep -c 'bad=0$') case(s) correct"
    built=$((built + 1))
done

# The vacuity guard, and it matters more than the assertions above: a run in which NOTHING built
# must never look like a pass. This is the shape that failed silently in check_random_kernels.sh
# when a bash-4 construct died under macOS's bash 3.2 -- one configuration ran, eleven did not,
# and the script exited 0.
if [ "$built" -eq 0 ]; then
    echo "check_argsort_standalone.sh: NO configuration built -- this run proves nothing" >&2
    finished=1
    rm -rf "$WORK"
    exit 1
fi

rm -rf "$WORK"
finished=1
if [ "$failed" -ne 0 ]; then
    echo "check_argsort_standalone.sh: $failed of $((built + failed)) configuration(s) FAILED" >&2
    exit 1
fi
echo "check_argsort_standalone.sh: all $built configuration(s) built and ran standalone."
exit 0
