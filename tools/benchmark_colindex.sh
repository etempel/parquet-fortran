#!/usr/bin/env bash
# Drives app/benchmark_colindex.f90 -- the F1 index/handle accessor screening campaign.
#
# See feature_colindex.md for the design this measures and feature_benchmark_colindex.md for the
# run sheet (which machine runs which mode, what the thresholds are, and where the report goes).
# This script only builds and runs; app/benchmark_colindex.f90 decides what each mode times.
#
# The two figures every other number is read against are printed by --mode=baseline: a `%col`
# pointer read (expected ~0.93 ns) and `%get_element` (expected ~24 ns at ncols=4). If those do
# not reproduce, stop -- a harness that replicates library call shapes is untested code until one
# of its rows reproduces a figure measured independently.
#
# Usage:
#   tools/benchmark_colindex.sh                     # every mode, shipped guards
#   tools/benchmark_colindex.sh --mode=decompose    # one mode
#   GUARDS=inline tools/benchmark_colindex.sh --mode=getat
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   NROWS=100000    Rows per column. Memory is roughly NROWS * NCOLS * 8 bytes, so the 128-column
#                    sweep in --mode=baseline costs 128 * NROWS * 8 -- keep NROWS modest.
#   NCOLS=40        Columns in the table for the modes that take one.
#   ROUNDS=5        Rounds per figure; the best is kept, per this repo's benchmarking rules.
#   ACCESS=2000000  Accesses per arm. Independent of NROWS: the loop wraps around.
#   WIDTH=4         Elements per row for --mode=vector and --mode=handle's vector section.
#   GUARDS=shipped  Which get_at guard variant to build: shipped | inline | none.
#                    `inline` and `none` REQUIRE the source to have been regenerated with
#                    tools/generate_parquet_columns.py --bench-guards first -- see below.
#   LADDER=          Stage-0b rung: one PF_BENCH_NO_* macro naming a `table_resolve` phase to
#                    compile out. REQUIRES tools/bench_resolve_ladder.py --apply first; run
#                    `tools/bench_resolve_ladder.py --list` for the rungs. Empty = the full path.
#   LTO=0            Set to 1 when you have appended LTO flags, so the build tree is named apart
#                    from its non-LTO twin.
#
# GUARDS is the one setting that can silently measure the wrong thing, so it does not degrade:
# asking for `inline` or `none` against a tree whose generated source carries no cpp scaffolding
# would build an identical binary to `shipped` and report a null result that looks like a
# finding. This script checks for the scaffolding and FAILS if it is absent (CLAUDE.md: "A
# wrapper must FAIL rather than degrade when it cannot engage the configuration it was asked
# for"). The program prints which variant it was actually compiled with, as a second, independent
# check on the same thing.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

NROWS="${NROWS:-100000}"
NCOLS="${NCOLS:-40}"
ROUNDS="${ROUNDS:-5}"
ACCESS="${ACCESS:-2000000}"
WIDTH="${WIDTH:-4}"
GUARDS="${GUARDS:-shipped}"

MODES=(baseline decompose getat vector loop rowfinal handle)
want_mode=""
for arg in "$@"; do
    case "$arg" in
        --mode=*) want_mode="${arg#--mode=}" ;;
        -h|--help)
            sed -n '2,36p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "benchmark_colindex.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done
if [[ -n "$want_mode" ]]; then
    found=0
    for m in "${MODES[@]}"; do [[ "$m" == "$want_mode" ]] && found=1; done
    if [[ $found -eq 0 ]]; then
        echo "benchmark_colindex.sh: unknown mode '$want_mode' (one of: ${MODES[*]})" >&2
        exit 2
    fi
    MODES=("$want_mode")
fi

# --- select the guard variant -----------------------------------------------------------------
GUARD_FLAG=""
case "$GUARDS" in
    shipped) GUARD_FLAG="" ;;
    inline)  GUARD_FLAG="-DPF_BENCH_INLINE_GUARDS" ;;
    none)    GUARD_FLAG="-DPF_BENCH_NO_GUARDS" ;;
    *)
        echo "benchmark_colindex.sh: GUARDS must be shipped, inline or none (got '$GUARDS')" >&2
        exit 2
        ;;
esac
if [[ -n "$GUARD_FLAG" ]] && ! grep -q "PF_BENCH_INLINE_GUARDS" src/parquet_columns_access.f90; then
    cat >&2 <<'EOF'
benchmark_colindex.sh: GUARDS=inline/none was asked for, but src/parquet_columns_access.f90
carries no benchmark scaffolding, so the flag would have no effect and this run would silently
measure the shipped build under another name.

Regenerate first, ON A MEASUREMENT BRANCH ONLY:

    python3 tools/generate_parquet_columns.py --bench-guards

and restore main's committed output afterwards with:

    git checkout src/parquet_columns_access.f90
EOF
    exit 1
fi

# --- select the stage-0b ladder rung ------------------------------------------------------------
#
# Same refusal rule as GUARDS, and for the same reason: a macro against unscaffolded source is a
# no-op, so the run would build the full path and file it under a rung name. The program prints the
# rung it was actually compiled with, as an independent second check.
LADDER="${LADDER:-}"
LADDER_FLAG=""
LADDER_TAG=""
if [[ -n "$LADDER" ]]; then
    if [[ "$LADDER" != PF_BENCH_NO_* ]]; then
        echo "benchmark_colindex.sh: LADDER must name a PF_BENCH_NO_* macro (got '$LADDER')." >&2
        echo "  tools/bench_resolve_ladder.py --list" >&2
        exit 2
    fi
    if ! grep -q "PF_BENCH_NO_APPEND_CHECK" src/parquet_tables_query.f90; then
        cat >&2 <<'EOF'
benchmark_colindex.sh: LADDER was asked for, but src/parquet_tables_query.f90 carries no ladder
scaffolding, so the macro would have no effect and this run would measure the full path under a
rung's name.

Apply it first, ON A MEASUREMENT BRANCH ONLY:

    tools/bench_resolve_ladder.py --apply

and restore afterwards with:

    git checkout src/parquet_tables_query.f90
EOF
        exit 1
    fi
    if ! grep -q -- "$LADDER" src/parquet_tables_query.f90; then
        echo "benchmark_colindex.sh: '$LADDER' is not a rung this scaffolding defines." >&2
        echo "  tools/bench_resolve_ladder.py --list" >&2
        exit 2
    fi
    LADDER_FLAG="-D$LADDER"
    LADDER_TAG="-${LADDER#PF_BENCH_NO_}"
fi

# --- one build tree per (variant, compiler) -----------------------------------------------------
#
# The compiler goes in the name, not just the variant. tools/benchmark_stage7.sh names its trees
# for the configuration alone, so running it under a second FPM_FC drops that compiler's binary
# into the same tree and `find ... | head -n 1` then picks between them arbitrarily. Machine B
# runs two toolchains in this campaign, so that trap is live here.
FC_TAG="$(basename "${FPM_FC:-gfortran}")"
FC_TAG="${FC_TAG//[^A-Za-z0-9._-]/_}"
# The LTO axis is part of the name too. Naming a tree for (guards, compiler) alone let an LTO
# build land in its non-LTO twin's directory -- machines A and B both hit it and worked around it
# by hand. Set LTO=1 when you have appended LTO flags, or set FPM_BUILD_DIR yourself; a pre-set
# value is respected rather than overwritten, which is the general escape hatch.
LTO_TAG=""
[[ "${LTO:-0}" != "0" ]] && LTO_TAG="-lto"
if [[ -z "${FPM_BUILD_DIR:-}" ]]; then
    export FPM_BUILD_DIR="test_run/colindex-${GUARDS}${LADDER_TAG}-${FC_TAG}${LTO_TAG}"
else
    echo "benchmark_colindex.sh: using the FPM_BUILD_DIR you set: $FPM_BUILD_DIR" >&2
fi

# Appended, never assigned: on every machine this project is developed on, FPM_FFLAGS already
# carries Arrow-adjacent include paths, and replacing it produces a missing-header error that
# reads like a missing dependency rather than a flag mistake.
if [[ -n "$GUARD_FLAG$LADDER_FLAG" ]]; then
    export FPM_FFLAGS="${FPM_FFLAGS:-} $GUARD_FLAG $LADDER_FLAG"
fi

echo "=============================================================================="
echo "benchmark_colindex.sh"
echo "  guards      : $GUARDS${GUARD_FLAG:+  ($GUARD_FLAG)}"
echo "  ladder rung : ${LADDER:-(none -- full path)}"
echo "  build tree  : $FPM_BUILD_DIR"
echo "  fortran     : ${FPM_FC:-gfortran (fpm default)}"
echo "  nrows=$NROWS ncols=$NCOLS rounds=$ROUNDS access=$ACCESS width=$WIDTH"
echo "  modes       : ${MODES[*]}"
echo "=============================================================================="
echo

# Some compilers optimise by DEFAULT, so "no -O in the flags" does not imply -O0 for them.
#
# The check below was a false positive on ifx and blocked machine B's whole arm: fpm gives ifx no
# `-O` either, but ifx's own default is -O2, so that build was already optimised and refusing it was
# wrong. The check is still the right design -- it failed rather than degrading -- but it has to
# know the difference between "no flag, therefore -O0" (gfortran, flang) and "no flag, therefore
# this compiler's default" (ifx, icx).
#
# Keep this list short and evidence-based: a compiler belongs here only once someone has confirmed
# its default, because a wrong entry turns the check into the silent -O0 run it exists to prevent.
compiler_defaults_to_optimised() {
    case "$(basename "${FPM_FC:-gfortran}")" in
        ifx|ifx-*|ifort|ifort-*|icx|icx-*) return 0 ;;
        *) return 1 ;;
    esac
}

# --profile release is not optional: fpm applies NO optimisation flags at all without a profile,
# and every figure from an -O0 build is meaningless (measured 5.7x on one item in an earlier
# campaign, enough to invert a comparison, and it did).
#
# And asking for it is NOT the same as getting it. fpm 0.13.0 alpha has no release profile for
# flang: `--profile release` emits `-cpp` and the -I paths and nothing else, so the whole run is
# an -O0 run that looks exactly like a valid one. Machine A found this the hard way -- a plain
# Fortran array read at 5.43 ns instead of 0.95, and %get_element 3.7x too slow, from a flag the
# wrapper believed it had set. So verify, the same way GUARDS is verified above: this script
# refuses to measure a build it cannot confirm is optimised.
FLAGS_LINE="$(fpm build --profile release --show-model 2>/dev/null \
              | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1 || true)"
if [[ -z "$FLAGS_LINE" ]]; then
    echo "benchmark_colindex.sh: could not read fortran_compile_flags from 'fpm build --show-model'." >&2
    echo "  Cannot confirm the build is optimised; refusing to produce numbers. Set" >&2
    echo "  SKIP_OPT_CHECK=1 to override, and SAY SO IN THE REPORT." >&2
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
elif [[ "$FLAGS_LINE" != *" -O"* ]] && ! compiler_defaults_to_optimised; then
    cat >&2 <<EOF
benchmark_colindex.sh: '--profile release' produced NO optimisation flag for this compiler.

  $FLAGS_LINE

fpm has no release profile for some compilers (flang, as of fpm 0.13.0 alpha), so this would be an
-O0 run reported as a release one -- measured 3.7x wrong on the headline figure when it happened.

If THIS compiler optimises by default (ifx does, at -O2), add it to compiler_defaults_to_optimised()
in this script rather than reaching for SKIP_OPT_CHECK -- an override and an appended -O3 are two
different configurations, and mixing them across machines is a comparability trap.

Fix by appending the flag yourself (append, never assign -- FPM_FFLAGS carries Arrow's paths):

    FPM_FFLAGS="\${FPM_FFLAGS:-} -O3" tools/benchmark_colindex.sh ...

and record in the report that you did. SKIP_OPT_CHECK=1 overrides this check.
EOF
    [[ "${SKIP_OPT_CHECK:-0}" == "0" ]] && exit 1
fi

fpm build --profile release >/dev/null

for m in "${MODES[@]}"; do
    fpm run benchmark_colindex --profile release -- \
        --mode="$m" --nrows="$NROWS" --ncols="$NCOLS" --rounds="$ROUNDS" \
        --access="$ACCESS" --width="$WIDTH"
    echo
done

echo "Build tree left at $FPM_BUILD_DIR (rm -rf test_run/colindex-* to clean up)."
