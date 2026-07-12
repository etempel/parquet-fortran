#!/usr/bin/env bash
# Builds parquet-fortran with gcov instrumentation, runs the test suite, and
# reports per-file + total line coverage for src/*.f90.
#
# Usage:
#   tools/coverage.sh              # full run_tester suite + all error_scenarios
#   tools/coverage.sh reading       # only the "reading" run_tester testsuite
#                                   # (passed through to `fpm test run_tester --`;
#                                   # error_scenarios/run_error_scenarios.sh are
#                                   # skipped in this mode, since they're
#                                   # independent of run_tester's suite selection)
#
# Requires a `gcov` build whose version matches the gfortran used to compile
# (a mismatched gcov fails with "Invalid .gcno file!"), so this script derives
# the matching gcov from gfortran's own resolved binary name (e.g.
# gfortran-mp-15 -> gcov-mp-15) rather than assuming the first `gcov` on PATH.
# Requires python3 (already a tool dependency in this repo -- see
# tools/check_doc_anchors.py, tools/count_lines.py) to parse gcov's JSON
# intermediate format into per-file/total percentages.
#
# Lines marked with GCOVR_EXCL_LINE, or bracketed by GCOVR_EXCL_START /
# GCOVR_EXCL_STOP, are dropped from the counts the same way the real `gcovr`
# (run by .gitlab-ci.yml) excludes them, so this script's percentages agree
# with CI's rather than under-reporting genuinely-uncoverable lines.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FC="${FPM_FC:-gfortran}"

resolve_gcov() {
    local fc_path resolved dir base candidate
    fc_path="$(command -v "$FC" 2>/dev/null)" || { echo "gcov"; return; }
    resolved="$(readlink -f "$fc_path" 2>/dev/null || echo "$fc_path")"
    dir="$(dirname "$resolved")"
    base="$(basename "$resolved")"
    candidate="${base/gfortran/gcov}"
    if [ -x "$dir/$candidate" ]; then
        echo "$dir/$candidate"
    elif command -v "$candidate" >/dev/null 2>&1; then
        command -v "$candidate"
    else
        echo "gcov"
    fi
}

GCOV="$(resolve_gcov)"
echo "Using compiler: $(command -v "$FC")" >&2
echo "Using gcov:      $GCOV" >&2

export FPM_BUILD_DIR="${FPM_BUILD_DIR:-build/gcov}"
export FPM_FFLAGS="${FPM_FFLAGS:-} -O0 -g --coverage"

rm -rf "$FPM_BUILD_DIR"

echo "Building + running run_tester with coverage instrumentation..." >&2
fpm test run_tester -- "$@"

if [ "$#" -eq 0 ]; then
    echo "Running tools/run_error_scenarios.sh for additional error-path coverage..." >&2
    # Reuses FPM_BUILD_DIR/FPM_FFLAGS from this script's environment, since
    # both this script and test_errors.f90's own subprocess checks just
    # invoke `fpm test error_scenarios -- <scenario>`, which inherits them --
    # so every scenario run here accumulates into the same .gcda files.
    "$ROOT_DIR/tools/run_error_scenarios.sh" >&2 || true
fi

echo "Collecting coverage data..." >&2

# Find every src/*.f90 object's .gcda under the coverage build dir (test/*
# and vendored dependencies -- e.g. test-drive -- produce their own .gcda
# too, but we only report on src/). Avoids bash4-only builtins (mapfile,
# associative arrays) since macOS ships bash 3.2 by default.
gcda_count=$(find "$FPM_BUILD_DIR" -name '*.gcda' -path '*/src_*' | wc -l | tr -d ' ')
if [ "$gcda_count" -eq 0 ]; then
    echo "No src/*.gcda files found under $FPM_BUILD_DIR -- did the build/run succeed?" >&2
    exit 1
fi

# -j emits one gzipped JSON file per .gcda next to it, with exact per-line
# hit counts (no need for the original source file to be present, and no
# rounding-error accumulation like parsing gcov's percentage summaries would
# have). Run once per object directory so relative paths inside the JSON
# resolve the same way they did in our manual check (from $ROOT_DIR).
find "$FPM_BUILD_DIR" -name '*.gcda' -path '*/src_*' -exec dirname {} \; | sort -u | \
while IFS= read -r dir; do
    ( cd "$dir" && "$GCOV" -j -o . src_*.gcda >/dev/null )
done

python3 - "$FPM_BUILD_DIR" <<'PY'
import gzip, json, sys, glob, os

build_dir = sys.argv[1]

per_file = {}  # src path -> {line_number: max(count seen)}
for path in glob.glob(os.path.join(build_dir, "**", "*.gcov.json.gz"), recursive=True):
    with gzip.open(path) as f:
        data = json.load(f)
    for entry in data["files"]:
        src = entry["file"]
        if not src.startswith("src/"):
            continue
        lines = per_file.setdefault(src, {})
        for line in entry["lines"]:
            ln = line["line_number"]
            lines[ln] = max(lines.get(ln, 0), line["count"])

if not per_file:
    print("No src/ coverage data found in JSON output.", file=sys.stderr)
    sys.exit(1)


def gcovr_excluded_lines(path):
    """Line numbers gcovr itself would drop from both numerator and
    denominator: a single line carrying GCOVR_EXCL_LINE, or every line from a
    GCOVR_EXCL_START comment through its matching GCOVR_EXCL_STOP (inclusive
    of both marker lines) -- see .gitlab-ci.yml's `gcovr` invocation, the
    tool these markers are actually written for. Mirroring this here keeps
    this script's percentages in agreement with CI's."""
    excluded = set()
    in_block = False
    try:
        with open(path) as f:
            for i, line in enumerate(f, start=1):
                if "GCOVR_EXCL_START" in line:
                    in_block = True
                    excluded.add(i)
                elif "GCOVR_EXCL_STOP" in line:
                    in_block = False
                    excluded.add(i)
                elif in_block or "GCOVR_EXCL_LINE" in line:
                    excluded.add(i)
    except FileNotFoundError:
        pass
    return excluded


for src in per_file:
    for ln in gcovr_excluded_lines(src):
        per_file[src].pop(ln, None)

total_exec = 0
total_lines = 0
rows = []
for src in sorted(per_file):
    lines = per_file[src]
    n = len(lines)
    exec_n = sum(1 for c in lines.values() if c > 0)
    total_exec += exec_n
    total_lines += n
    pct = 100.0 * exec_n / n if n else 0.0
    rows.append((src, exec_n, n, pct))

name_w = max(len(r[0]) for r in rows)
print()
print(f"{'file':<{name_w}}  {'covered/total':>14}  {'pct':>7}")
print("-" * (name_w + 26))
for src, exec_n, n, pct in rows:
    print(f"{src:<{name_w}}  {exec_n:>6}/{n:<6}  {pct:6.2f}%")
print("-" * (name_w + 26))
total_pct = 100.0 * total_exec / total_lines if total_lines else 0.0
print(f"{'TOTAL':<{name_w}}  {total_exec:>6}/{total_lines:<6}  {total_pct:6.2f}%")


def condense_ranges(line_numbers):
    """[143, 150, 151, 152, 181] -> ['143', '150-152', '181']"""
    ranges = []
    for ln in sorted(line_numbers):
        if ranges and ln == ranges[-1][1] + 1:
            ranges[-1] = (ranges[-1][0], ln)
        else:
            ranges.append((ln, ln))
    return [str(a) if a == b else f"{a}-{b}" for a, b in ranges]


print()
print("Uncovered lines by file:")
print("-" * (name_w + 26))
for src in sorted(per_file):
    uncovered = [ln for ln, c in per_file[src].items() if c == 0]
    if not uncovered:
        print(f"{src}: (fully covered)")
        continue
    print(f"{src}:")
    print("  " + ", ".join(condense_ranges(uncovered)))
PY
