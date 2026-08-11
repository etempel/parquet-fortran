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

# Safeguard against a stale fpm build cache (see CLAUDE.md, "Stale fpm build cache").
# A leftover default build/ tree with several build/gfortran_* dirs (accumulated from
# runs with different FPM_FFLAGS) can make fpm -- and test_errors.f90's
# error_scenarios_bin binary lookup, which does `find build -name error_scenarios |
# head -1` -- pick up an out-of-date binary, surfacing as spurious test failures.
# Wipe fpm's default build tree up front so this run (and the next plain `fpm test`)
# starts from a clean slate. This script's own instrumented build lives in build/gcov
# and is cleaned separately below. Run before FPM_BUILD_DIR is exported so `fpm clean`
# targets the default `build/`; </dev/null + `|| true` keep it non-interactive and
# non-fatal if there is nothing to clean.
echo "Cleaning fpm default build tree (fpm clean --all)..." >&2
fpm clean --skip </dev/null >/dev/null 2>&1 || true

export FPM_BUILD_DIR="${FPM_BUILD_DIR:-build/gcov}"
# -fprofile-update=atomic is NOT optional here, and its absence does not look like a bug.
# test-drive runs the tests of a suite concurrently inside ONE process (its own !$omp parallel
# do), so without atomic updates two threads incrementing the same gcov counter lose one of the
# increments -- and a line hit only once or twice in the whole run is then reported as
# UNCOVERED. Measured directly on this project: adding the flag (with the serialisation below)
# took the total from 24322/24324 to 24324/24324, and the two lines it recovered were an
# ordinary, well-tested branch that had been chased as a real gap. The cost is a slower
# instrumented build; the alternative is a report that invents gaps.
export FPM_FFLAGS="${FPM_FFLAGS:-} -O0 -g --coverage -fprofile-update=atomic"

# Unconditional on every invocation -- this build dir is separate from fpm's default `build/`
# (so `fpm clean --all` never touches it) and is NOT reused/merged across runs: a manual
# `fpm test ...` pointed at this same FPM_BUILD_DIR outside this script (e.g. for quick
# iteration while debugging) leaves its own coverage data behind, which would otherwise get
# silently merged into the next report here and inflate/skew its numbers. Always start clean.
echo "Cleaning previous coverage build directory: $FPM_BUILD_DIR" >&2
rm -rf "$FPM_BUILD_DIR"

# ...and remove it again on the way out, however this script exits. The instrumented tree
# contains its own test binaries, including error_scenarios -- and both
# tools/run_error_scenarios.sh and test/test_errors.f90 locate that binary with
# `find build -type f -name error_scenarios | head -1`, which cannot tell an instrumented
# copy from the ordinary one. Leaving this tree behind therefore makes the NEXT plain
# `fpm test`/`tools/run_error_scenarios.sh` run a binary it did not build: in the benign
# direction every scenario reports "scenario name not recognized", and in the dangerous one
# a stale binary reports "All error scenarios behaved as expected" (see CLAUDE.md, "Stale
# fpm build cache"). The coverage report is computed from this tree, so cleanup can only
# happen at exit, not before the report.
#
# Set COVERAGE_KEEP_BUILD=1 to keep it -- useful when a run fails and the .gcda/.gcno files
# themselves need inspecting.
cleanup_coverage_build() {
    if [ -n "${COVERAGE_KEEP_BUILD:-}" ]; then
        echo "COVERAGE_KEEP_BUILD is set: leaving $FPM_BUILD_DIR in place." >&2
        echo "Delete it before the next plain fpm test/run_error_scenarios.sh run." >&2
        return
    fi
    rm -rf "$FPM_BUILD_DIR"
}
trap cleanup_coverage_build EXIT

echo "Building + running run_tester with coverage instrumentation..." >&2
fpm test run_tester -- "$@"

if [ "$#" -eq 0 ]; then
    echo "Running tools/run_error_scenarios.sh for additional error-path coverage..." >&2
    # Reuses FPM_BUILD_DIR/FPM_FFLAGS from this script's environment, since
    # both this script and test_errors.f90's own subprocess checks just
    # invoke `fpm test error_scenarios -- <scenario>`, which inherits them --
    # so every scenario run here accumulates into the same .gcda files.
    #
    # ...which is exactly why the scenarios are run SERIALLY here and nowhere else. The runner
    # normally dispatches them across `nproc` workers, and each worker merges its counters into
    # those same shared .gcda files as it exits -- hundreds of processes writing one file. Under
    # coverage that reliably corrupts one: three consecutive runs on this machine died with
    # `src_parquet_tables_rowmutate.f90.gcda: not a gcov data file`, always the same file, and a
    # serial run has not reproduced it. Milder instances are worse, because they do not stop the
    # run: a whole untouched source file coming back at 47%, or a handful of lines flipping
    # between covered and uncovered, both of which read as regressions. Override with
    # RUN_ERROR_SCENARIOS_JOBS if you want the speed back and can live with that.
    RUN_ERROR_SCENARIOS_JOBS="${RUN_ERROR_SCENARIOS_JOBS:-1}" \
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


ARTIFACT_PHRASE = "gcov attribution artifact"


def gcovr_artifact_lines(path):
    """Excluded line numbers that are *expected* to show a positive gcov hit
    count despite being genuinely dead/unreachable code -- a known gfortran/gcov
    quirk (see CLAUDE.md's "Fortran gcov attribution artifacts" note), not
    evidence of a stale exclusion: a guard-clause `if (...) then` condition is
    evaluated -- and thus counted as "hit" -- on every call regardless of
    whether the guarded body is ever reached, and a few bare `return`
    statements suffer a similar mis-attribution. Sites known to hit this are
    tagged with the literal phrase "gcov attribution artifact", either inline
    on their own GCOVR_EXCL_START/GCOVR_EXCL_LINE marker line or on a
    comment-only line (or contiguous run of them) directly above it (used when
    the marker line has no room left under the 132-column limit). Any line so
    tagged is dropped from the "candidates for a stale exclusion" report
    below."""
    tagged = set()
    in_block = False
    block_lines = []
    block_tagged = False
    try:
        with open(path) as f:
            raw_lines = f.readlines()
    except FileNotFoundError:
        return tagged

    def preceded_by_phrase(i):
        # i is 1-based. Scans upward through a contiguous run of comment-only
        # lines directly above line i (the marker line itself is checked by
        # the caller), since a long explanatory tag sometimes spans more than
        # one comment line. Also walks back over `&`-continuation lines first
        # -- gcov attributes a multi-line `if (...) then` condition's hit
        # count to whichever physical line carries the `then` (and the
        # GCOVR_EXCL_START marker), not the first line of the statement, so
        # the explanatory comment two lines above still needs to be found.
        j = i - 1
        while j >= 1:
            prev = raw_lines[j - 1]
            if prev.rstrip().endswith("&"):
                j -= 1
                continue
            if prev.strip().startswith("!"):
                if ARTIFACT_PHRASE in prev:
                    return True
                j -= 1
                continue
            break
        return False

    for i, line in enumerate(raw_lines, start=1):
        if "GCOVR_EXCL_START" in line:
            in_block = True
            block_lines = [i]
            block_tagged = ARTIFACT_PHRASE in line or preceded_by_phrase(i)
        elif "GCOVR_EXCL_STOP" in line:
            block_lines.append(i)
            if ARTIFACT_PHRASE in line:
                block_tagged = True
            if block_tagged:
                tagged.update(block_lines)
            in_block = False
            block_lines = []
        elif in_block:
            block_lines.append(i)
            if ARTIFACT_PHRASE in line:
                block_tagged = True
        elif "GCOVR_EXCL_LINE" in line and (ARTIFACT_PHRASE in line or preceded_by_phrase(i)):
            tagged.add(i)
    return tagged


excluded_with_hits = {}  # src path -> sorted list of excluded line numbers that had count > 0
for src in per_file:
    excluded = gcovr_excluded_lines(src)
    artifacts = gcovr_artifact_lines(src)
    hits = sorted(ln for ln in excluded if per_file[src].get(ln, 0) > 0 and ln not in artifacts)
    if hits:
        excluded_with_hits[src] = hits
    for ln in excluded:
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

print()
print("Excluded lines with positive hits (candidates for a stale/no-longer-dead exclusion):")
print("-" * (name_w + 26))
if not excluded_with_hits:
    print("(none)")
else:
    for src in sorted(excluded_with_hits):
        print(f"{src}:")
        print("  " + ", ".join(condense_ranges(excluded_with_hits[src])))
PY
