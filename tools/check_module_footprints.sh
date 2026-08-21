#!/usr/bin/env bash
#
# check_module_footprints.sh -- assert how much of this library each advertised entry module
# actually drags into a consumer's build.
#
# WHAT THIS MEASURES, AND WHY NOTHING ELSE CAN. fpm prunes at MODULE granularity: a consumer
# compiles a source file only if some module it imports needs it, and a submodule is never pruned
# separately from the module it belongs to. So `use parquet_argsort` in a downstream project
# compiles a specific, knowable set of this library's files -- and one added `use` line anywhere in
# that set can silently double it. `fpm test` cannot see that happen (the library obviously builds
# either way), `tools/check_source_conventions.py`'s Arrow-free checks cannot either (a tier can
# double in size without ever touching parquet_bindings), and no compiler warns. This script is the
# only instrument that answers "how big did the graph get".
#
# It is the size counterpart of check_parquet_argsort_stays_arrow_free and friends, which answer a
# different question -- "does the graph reach the Arrow stack" -- and the two catch different
# regressions. Keep both.
#
# HOW IT WORKS. For each entry module it writes a throwaway consumer project into a temporary
# directory, points it at this repository through a relative symlink (fpm 0.13.0 alpha rejects an
# absolute `path =` dependency), builds it COLD, and lists the library files fpm actually compiled.
# The result is compared against tools/module_footprints.txt, which is committed.
#
# WHEN IT FAILS, THE FIX IS ALMOST NEVER TO UPDATE THE EXPECTATION. A module that grew did so
# because something it imports gained a dependency; move whatever needed that import UP a tier
# instead, exactly as the Arrow-free checks' headers say. Update the expectation only when the
# growth is the deliberate, reviewed point of the change -- and say so in the commit message,
# because a silently-updated expectation is a check that has been switched off.
#
# USAGE
#   tools/check_module_footprints.sh                 check every entry module
#   tools/check_module_footprints.sh parquet_argsort check one
#   tools/check_module_footprints.sh --print         print the current footprints in the
#                                                    expectation file's own format, for review
#
# This is slow by construction -- each module is a cold build of its whole subtree -- so it is a
# maintainer command, not part of tools/run_lint_check.sh or CI's lint stage.
#
# bash 3.2 only (macOS ships 3.2 and this is run by hand there): no associative arrays, no
# mapfile, no ${var,,}. See CLAUDE.md, "A tools/*.sh check must run under bash 3.2".

set -u

finished=0
work=""
cleanup() {
    if [ -n "$work" ] && [ -d "$work" ]; then rm -rf "$work"; fi
    if [ "$finished" != "1" ]; then
        echo "check_module_footprints.sh: TERMINATED EARLY -- this run proves nothing" >&2
        exit 2
    fi
}
trap cleanup EXIT

repo="$(cd "$(dirname "$0")/.." && pwd)"
expect_file="$repo/tools/module_footprints.txt"

# The advertised entry modules, in tier order. Adding one here without adding its section to
# tools/module_footprints.txt is a hard failure, which is the intent.
ENTRY_MODULES="parquet_temporal parquet_strings parquet_random parquet_argsort parquet_sampling \
parquet_columns parquet_sorting parquet_io parquet"

mode="check"
only=""
if [ $# -gt 0 ]; then
    case "$1" in
        --print) mode="print" ;;
        -h|--help) sed -n '2,40p' "$0"; finished=1; exit 0 ;;
        -*) echo "check_module_footprints.sh: unknown option '$1'" >&2; exit 2 ;;
        *) only="$1" ;;
    esac
fi

if [ ! -f "$expect_file" ] && [ "$mode" = "check" ]; then
    echo "check_module_footprints.sh: missing expectation file $expect_file" >&2
    exit 2
fi

work="$(mktemp -d "${TMPDIR:-/tmp}/pf-footprint.XXXXXX")"
ln -s "$repo" "$work/pf"

# Prints the sorted list of library source files a cold build of `use <module>` compiles.
# The probe's own m.f90 is excluded; src/parquet_wrapper.cpp is NOT -- it is compiled for every
# module because `link` is a package-level key in fpm.toml, and seeing it in every section is the
# reminder that no module makes the PACKAGE Arrow-free.
measure_module() {
    _mod="$1"
    _probe="$work/probe_$_mod"
    rm -rf "$_probe"
    mkdir -p "$_probe/src"
    printf 'name = "probe"\nversion = "0.1.0"\n[dependencies]\nparquet-fortran = { path = "../pf" }\n' \
        > "$_probe/fpm.toml"
    printf 'module m_probe\n    use %s\n    implicit none\nend module m_probe\n' "$_mod" \
        > "$_probe/src/m.f90"
    ( cd "$_probe" && fpm build ) > "$_probe/build.log" 2>&1
    _rc=$?
    if [ "$_rc" -ne 0 ]; then
        echo "check_module_footprints.sh: probe build FAILED for 'use $_mod'" >&2
        tail -n 25 "$_probe/build.log" >&2
        exit 1
    fi
    # LC_ALL=C, or the ORDER of this list depends on the caller's locale and every section
    # reshuffles: under en_US.UTF-8 collation punctuation is weighted differently, so
    # parquet_settings_base.f90 sorts before parquet_settings.f90 while under C it sorts after.
    # tools/module_footprints.txt was generated under C, and a regeneration from a UTF-8 shell
    # produces a diff touching all nine sections in which the one real change is invisible.
    grep -oE '[a-z_0-9]+\.(f90|cpp)' "$_probe/build.log" | grep -v '^m\.f90$' | LC_ALL=C sort -u
}

# Reads one module's expected file list out of the committed expectation file.
expected_for() {
    awk -v want="[$1]" '
        $0 == want { on = 1; next }
        /^\[/      { on = 0 }
        on && NF   { print }
    ' "$expect_file"
}

status=0
checked=0

for mod in $ENTRY_MODULES; do
    if [ -n "$only" ] && [ "$only" != "$mod" ]; then continue; fi
    got="$work/got_$mod"
    measure_module "$mod" > "$got"
    n=$(grep -c . "$got")

    if [ "$mode" = "print" ]; then
        echo "[$mod]"
        cat "$got"
        echo ""
        checked=$((checked + 1))
        continue
    fi

    want="$work/want_$mod"
    expected_for "$mod" > "$want"
    if [ ! -s "$want" ]; then
        echo "[FAIL] $mod: no [$mod] section in tools/module_footprints.txt" >&2
        status=1
        checked=$((checked + 1))
        continue
    fi

    if diff -u "$want" "$got" > "$work/diff_$mod" 2>&1; then
        echo "[ok]   $mod: $n files"
    else
        echo "[FAIL] $mod: footprint changed ($(grep -c . "$want") expected, $n compiled)" >&2
        sed -n '3,200p' "$work/diff_$mod" >&2
        status=1
    fi
    checked=$((checked + 1))
done

if [ "$checked" -eq 0 ]; then
    echo "check_module_footprints.sh: '$only' is not an advertised entry module" >&2
    exit 2
fi

if [ "$mode" = "check" ]; then
    if [ "$status" -eq 0 ]; then
        echo "All $checked module footprint(s) match tools/module_footprints.txt."
    else
        echo "check_module_footprints.sh: at least one footprint changed -- read this script's" >&2
        echo "header before updating tools/module_footprints.txt." >&2
    fi
fi

finished=1
exit $status
