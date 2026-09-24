#!/usr/bin/env bash
# Builds parquet-fortran with gcov instrumentation, runs the test suite, and
# reports per-file + total line coverage for src/*.f90.
#
# Usage:
#   tools/coverage.sh              # every test runner + all error_scenarios
#   tools/coverage.sh reading       # only the "reading" testsuite
#                                   # (error_scenarios/run_error_scenarios.sh are
#                                   # skipped in this mode, since they're
#                                   # independent of a runner's suite selection)
#
# Every error scenario IS measured by the no-argument form, but `run_tester_errors` is what runs
# them (its prime_error_scenarios spawns the whole list in parallel); the separate
# run_error_scenarios.sh pass afterwards adds the concurrency scenarios priming skips, plus any
# scenario priming did not reach. The block that decides this, near the end of the runner section,
# carries the measurement.
#
# THE SUITES ARE SPREAD ACROSS FIVE RUNNERS, not one, so neither half of the usage
# above can assume `run_tester`. `test/run_tester{,_pf,_cpp,_errors,_noundef}.f90`
# partition them (tools/check_source_conventions.py's check_test_runner_partition
# enforces that), so a named suite belongs to exactly one runner and a full run has
# to drive all of them. This script therefore derives the suite -> runner map from
# those files' own `new_testsuite("name", ...)` registrations rather than carrying a
# list: a list of that shape goes stale in the direction that stops measuring, and
# did -- `tools/coverage.sh stats` used to exit 1 with a testsuite listing that did
# not contain "stats", and a bare `tools/coverage.sh` silently excluded every suite
# outside run_tester (which is most of them, `parquet_stats` included).
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
#
# Reports FOUR things, and the percentage is the weakest of them:
#   1. per-file and total line coverage;
#   2. the uncovered line ranges, so a gap can be read without a browser;
#   3. every procedure NO line of which ran -- see the note above that report
#      for why a percentage cannot answer this and why it matters most on a
#      generated layer;
#   4. GCOVR_EXCL'd lines that nevertheless show a hit, i.e. candidates for a
#      stale exclusion (artifact-tagged sites excepted).
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

# Safeguard against a stale fpm build cache (see `.claude/rules/build.md`, "Stale build cache").
# A leftover default build/ tree with several build/gfortran_* dirs (accumulated from
# runs with different FPM_FFLAGS) can make fpm -- and test_errors.f90's
# error_scenarios_bin binary lookup, which does `find build -name error_scenarios |
# head -1` -- pick up an out-of-date binary, surfacing as spurious test failures.
# Wipe fpm's default build tree up front so this run (and the next plain `fpm test`)
# starts from a clean slate. This script's own instrumented build lives in build/gcov
# and is cleaned separately below. Run before FPM_BUILD_DIR is exported so `fpm clean`
# targets the default `build/`; </dev/null + `|| true` keep it non-interactive and
# non-fatal if there is nothing to clean.
# The suite -> runner map, derived from the runners' own registrations. `sed` rather than
# `grep -o`, for BSD grep, which has no -o on some of the machines this repo is built on.
runner_for_suite() {
    local want="$1" f name
    for f in "$ROOT_DIR"/test/run_tester*.f90; do
        for name in $(sed -n 's/.*new_testsuite("\([a-z_0-9]*\)".*/\1/p' "$f"); do
            if [ "$name" = "$want" ]; then
                basename "$f" .f90
                return 0
            fi
        done
    done
    return 1
}

if [ "$#" -gt 0 ]; then
    if ! COVERAGE_RUNNER="$(runner_for_suite "$1")"; then
        echo "tools/coverage.sh: no test runner registers a suite named '$1'." >&2
        echo "Known suites:" >&2
        sed -n 's/.*new_testsuite("\([a-z_0-9]*\)".*/  \1/p' "$ROOT_DIR"/test/run_tester*.f90 \
            | sort >&2
        exit 1
    fi
fi

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

if [ "$#" -eq 0 ]; then
    # Drop any capture directory an earlier run left behind. run_tester_errors' priming wipes and
    # rewrites it too, but only if it gets that far -- and the error-scenario block after this loop
    # reads "this scenario has a capture" as "this scenario has run under THIS instrumented build".
    # A stale directory would make that false for every scenario it still holds, silently, and a
    # silently smaller measurement is the one failure this script must not have. Observed on this
    # machine: a .primed left over from another run held a capture for a scenario name that no
    # longer exists anywhere in the tree.
    rm -rf test_run/.primed

    echo "Building + running every test runner with coverage instrumentation..." >&2
    for runner in "$ROOT_DIR"/test/run_tester*.f90; do
        runner="$(basename "$runner" .f90)"
        echo "  ... $runner" >&2
        fpm test "$runner"
    done
else
    echo "Building + running $COVERAGE_RUNNER -- $1 with coverage instrumentation..." >&2
    fpm test "$COVERAGE_RUNNER" -- "$@"
fi

if [ "$#" -eq 0 ]; then
    # `run_tester_errors` above has ALREADY run every `scenarios=(...)` entry in
    # tools/run_error_scenarios.sh, once, in parallel: that is what its prime_error_scenarios
    # does (test/test_errors.f90), and it spawns the same instrumented binary out of this same
    # FPM_BUILD_DIR, so all of those processes have already merged their counters into the .gcda
    # files this report is computed from. Re-running that array here therefore measures nothing --
    # verified rather than assumed: replaying all of it after the runner added 0 covered src/
    # lines, at 0.226 s per scenario serially, i.e. about eight minutes of silence. Its verdict
    # was not being used either (the `|| true` below), so it was not acting as a check.
    #
    # What priming does NOT run is `concurrency_scenarios=(...)`: it reads the `scenarios=(`
    # array alone, and those live in a second one. They are worth about ninety further src/ lines
    # (parquet_sampling, the optimiser multistart, the table parallel guards) and cost seconds, so
    # they are what this pass now runs.
    #
    # RUN_ERROR_SCENARIOS_SKIP_PRIMED is what keeps that safe, and it is deliberately a filter per
    # SCENARIO rather than a decision about the pass as a whole. Priming degrades to on-demand
    # spawning whenever anything about it fails, and then only the scenarios some test NAMES are
    # reached -- about 120 fewer than the list holds. Asking "does this one have a capture?" runs
    # exactly those and nothing else, with no count to compare and nothing to go stale. The
    # `rm -rf test_run/.primed` before the runner loop is the other half: without it a capture left
    # by an EARLIER run would answer for this one, and the scenario would be skipped having never
    # run under this build.
    #
    # NOTE on concurrency and .gcda corruption. Running the scenarios serially used to be this
    # script's guard against concurrent .gcda merges corrupting a file (`... .gcda: not a gcov
    # data file`, three consecutive runs; milder instances silently flip lines between covered and
    # uncovered). That guard never covered the run: priming reaches the same .gcda files from
    # `nproc` processes at once, and does so before this point. The lever that actually bounds it
    # is PARQUET_TEST_PRIME_JOBS=1, which would have to be exported before the runner loop above --
    # reach for that, not for RUN_ERROR_SCENARIOS_JOBS, if a corrupt .gcda reappears.
    echo "Running tools/run_error_scenarios.sh for the error paths priming did not reach..." >&2
    RUN_ERROR_SCENARIOS_SKIP_PRIMED=1 \
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
import gzip, json, re, sys, glob, os

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

# ---- Hottest lines, against gcovr's "suspicious hit" ceiling -----------------
#
# gcovr 7.x and later treat a hit count at or above 2**32 as evidence of the
# counter race of GCC PR68080 and refuse to parse the file, so CI's `gcovr`
# step (.gitlab-ci.yml) dies with exit 64 while THIS script, which reads the
# same JSON itself and has no such ceiling, reports a clean 100%. That is a
# miserable way to find out: the counts are genuine -- the build sets
# -fprofile-update=atomic, so they are not racing -- and the real message is
# that some test drives a hot loop far harder than the branches it covers
# need. Recorded once: two pf_kde sampler tests fitted 500 points and drew
# 20000 from a kernel wider than the support, which puts every draw through a
# root find over the whole fit with an eight-point quadrature per term, and
# took kde_kernel_pdf past 1.3e10 calls. Report the ceiling here so it is seen
# on the machine where it can be fixed.
SUSPICIOUS_HITS = 2 ** 32
hottest = sorted(
    ((c, src, ln) for src, lines in per_file.items() for ln, c in lines.items()),
    reverse=True,
)[:3]


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
    quirk (see `.claude/rules/coverage.md`'s "Fortran gcov attribution artifacts" note), not
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

if hottest:
    peak = hottest[0][0]
    print()
    print("Hottest lines (gcovr rejects a file holding a hit count at or above 2**32):")
    print("-" * (name_w + 26))
    for c, src, ln in hottest:
        print(f"  {c:>14,}  {src}:{ln}")
    if peak >= SUSPICIOUS_HITS:
        print(f"  WARNING: {peak:,} is at or above gcovr's ceiling of {SUSPICIOUS_HITS:,}.")
        print("  CI's gcovr step will refuse to parse this and exit 64. The counts are real,")
        print("  not racing (-fprofile-update=atomic): find the test driving that line and cut")
        print("  its size to what the branches it covers actually need.")
    elif peak >= SUSPICIOUS_HITS // 4:
        print(f"  NOTE: {100.0 * peak / SUSPICIOUS_HITS:.0f}% of gcovr's {SUSPICIOUS_HITS:,} ceiling.")
    else:
        print(f"  ({100.0 * peak / SUSPICIOUS_HITS:.1f}% of that ceiling.)")


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

# ---- Entirely-unreached procedures -------------------------------------------
#
# A percentage answers "how much of this file ran"; it cannot answer "which
# procedures never ran at all", and the two come apart badly on a GENERATED
# layer. src/parquet_stats_kernel.f90 read 30.23% at S4's release gate, which
# is easy to wave through as "generated code, of course it is low" -- while the
# number that actually mattered, 24 entirely-unreached public specifics
# spanning three sub-stages, was invisible inside it. This report answers with
# a list of NAMES instead of a number there is nothing to argue about.
#
# A procedure counts as unreached when every gcov-counted line attributed to it
# has a zero hit count. Lines are attributed to the INNERMOST enclosing
# procedure, so a covered contained procedure does not vouch for its host.
# Excluded lines are already gone from `per_file` by this point, so a procedure
# made entirely of GCOVR_EXCL'd lines has nothing counted and is skipped rather
# than reported.

OPEN_RE = re.compile(
    r"^\s*(?:(?:module|pure|impure|elemental|recursive|non_recursive)\s+)*"
    r"(?P<kind>subroutine|function|procedure)\s+(?P<name>[a-z_][a-z_0-9]*)",
    re.IGNORECASE)
END_RE = re.compile(r"^\s*end\s*(?:subroutine|function|procedure)\b", re.IGNORECASE)
IFACE_RE = re.compile(r"^\s*(?:abstract\s+)?interface\b", re.IGNORECASE)
ENDIFACE_RE = re.compile(r"^\s*end\s*interface\b", re.IGNORECASE)


def strip_fortran_comment(line):
    out = []
    quote = None
    for ch in line:
        if quote:
            out.append(ch)
            if ch == quote:
                quote = None
        elif ch in "'\"":
            quote = ch
            out.append(ch)
        elif ch == "!":
            break
        else:
            out.append(ch)
    return "".join(out)


def procedure_spans(path):
    """[(name, first_line, last_line)] for every procedure BODY, innermost
    first. Everything between `interface` and `end interface` is a declaration
    rather than a body -- neither an opener nor an ender -- which is what keeps
    a spec file full of interface bodies (src/parquet_stats.f90 and friends)
    from reporting hundreds of never-executed "procedures"."""
    spans = []
    stack = []
    iface = 0
    try:
        with open(path) as f:
            raw = f.readlines()
    except FileNotFoundError:
        return spans
    for i, line in enumerate(raw, start=1):
        code = strip_fortran_comment(line)
        if ENDIFACE_RE.match(code):
            iface = max(0, iface - 1)
            continue
        if IFACE_RE.match(code):
            iface += 1
            continue
        if iface:
            continue
        if END_RE.match(code):
            if stack:
                name, start = stack.pop()
                spans.append((name, start, i))
            continue
        m = OPEN_RE.match(code)
        if m:
            stack.append((m.group("name"), i))
    return spans


unreached = {}   # src path -> [(name, first_line)]
proc_total = 0
proc_unreached = 0
for src in sorted(per_file):
    counted = per_file[src]
    claimed = set()          # lines already attributed to an inner procedure
    for name, start, end in procedure_spans(src):   # innermost first
        mine = [ln for ln in range(start, end + 1) if ln in counted and ln not in claimed]
        claimed.update(range(start, end + 1))
        if not mine:
            continue
        proc_total += 1
        if all(counted[ln] == 0 for ln in mine):
            proc_unreached += 1
            unreached.setdefault(src, []).append((name, start))

print()
print("Entirely-unreached procedures (no line in the body ran):")
print("-" * (name_w + 26))
if not unreached:
    print("(none)")
else:
    for src in sorted(unreached):
        print(f"{src}:")
        for name, start in sorted(unreached[src], key=lambda t: t[1]):
            print(f"  {src}:{start}  {name}")
reached = proc_total - proc_unreached
pct_p = 100.0 * reached / proc_total if proc_total else 0.0
print("-" * (name_w + 26))
print(f"TOTAL  {reached}/{proc_total} procedures reached  ({pct_p:6.2f}%)")

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
