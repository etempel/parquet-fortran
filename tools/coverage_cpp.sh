#!/usr/bin/env bash
# Builds parquet-fortran with gcov instrumentation and reports line coverage
# only for src/parquet_wrapper.cpp.
#
# Usage:
#   tools/coverage_cpp.sh              # full run_tester suite + all error_scenarios
#   tools/coverage_cpp.sh reading      # only the "reading" run_tester testsuite
#
# Notes:
# - This script needs a C++ toolchain/config that can build parquet_wrapper.cpp,
#   including C++20 and Arrow/Parquet include+link flags.
# - For macOS in this repo, a working baseline is typically:
#     source ~/.zprofile
#     export FPM_CXX=clang++
#     fpm clean --all
#     ./tools/coverage_cpp.sh
# - Separate from tools/coverage.sh (which covers src/*.f90) rather than a
#   combined report or a shared flag: on a dev machine with a mismatched
#   Fortran/C++ toolchain (e.g. gfortran + a default clang++ FPM_CXX), the
#   two gcov data formats can't be instrumented/collected in the same pass --
#   see CONTRIBUTING.md's "Continuous integration (GitLab CI)" section for
#   why CI can do this in one gcovr pass and a local dev machine generally
#   can't.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

detect_arrow_prefix() {
    # Prefer explicit/local installs first, then common package-manager roots.
    local p
    for p in \
        "${ARROW_PREFIX:-}" \
        "/opt/local" \
        "/opt/homebrew/opt/apache-arrow" \
        "/usr/local/opt/apache-arrow" \
        "/opt/homebrew" \
        "/usr/local"
    do
        [ -n "$p" ] || continue
        if [ -f "$p/include/arrow/api.h" ] && [ -d "$p/lib" ]; then
            echo "$p"
            return
        fi
    done
    echo ""
}

ARROW_PREFIX="${ARROW_PREFIX:-$(detect_arrow_prefix)}"
if [ -z "$ARROW_PREFIX" ]; then
    echo "Error: could not locate Arrow install prefix." >&2
    echo "Set ARROW_PREFIX to a path containing include/arrow/api.h and lib/." >&2
    exit 1
fi

export FPM_FC="${FPM_FC:-gfortran}"
export FPM_CXX="${FPM_CXX:-clang++}"

if [ -z "${FPM_FFLAGS:-}" ]; then
    export FPM_FFLAGS="-I$ARROW_PREFIX/include"
fi

if [ -z "${FPM_CXXFLAGS:-}" ]; then
    if [[ "$(basename "$(command -v "$FPM_CXX" 2>/dev/null || echo "$FPM_CXX")")" == clang++* ]]; then
        export FPM_CXXFLAGS="-std=c++20 -stdlib=libc++ -I$ARROW_PREFIX/include -O0 -g --coverage"
    else
        export FPM_CXXFLAGS="-std=c++20 -I$ARROW_PREFIX/include -O0 -g --coverage"
    fi
fi

if [ -z "${FPM_LDFLAGS:-}" ]; then
    if [[ "$(basename "$(command -v "$FPM_CXX" 2>/dev/null || echo "$FPM_CXX")")" == clang++* ]]; then
        export FPM_LDFLAGS="-L$ARROW_PREFIX/lib -lc++"
    else
        export FPM_LDFLAGS="-L$ARROW_PREFIX/lib -lstdc++"
    fi
fi

export LIBRARY_PATH="$ARROW_PREFIX/lib:${LIBRARY_PATH:-}"

FC="$FPM_FC"
CXX="$FPM_CXX"
CXX_PATH="$(command -v "$CXX" 2>/dev/null || echo "$CXX")"

resolve_gcov_from_tool() {
    local tool path resolved dir base candidate
    tool="$1"
    path="$(command -v "$tool" 2>/dev/null)" || { echo ""; return; }
    resolved="$(readlink -f "$path" 2>/dev/null || echo "$path")"
    dir="$(dirname "$resolved")"
    base="$(basename "$resolved")"
    candidate=""

    # GNU-style tool names: gfortran-mp-15 -> gcov-mp-15, g++-mp-15 -> gcov-mp-15.
    if [[ "$base" == gfortran* ]]; then
        candidate="${base/gfortran/gcov}"
    elif [[ "$base" == g++* ]]; then
        candidate="${base/g++/gcov}"
    fi

    # Apple clang does not ship a sibling GNU gcov binary.
    if [ -z "$candidate" ] && [[ "$base" == clang++* ]]; then
        echo ""
        return
    fi

    if [ -z "$candidate" ]; then
        if command -v gcov >/dev/null 2>&1; then
            command -v gcov
            return
        fi
        echo ""
        return
    fi

    if [ -x "$dir/$candidate" ]; then
        echo "$dir/$candidate"
    elif command -v "$candidate" >/dev/null 2>&1; then
        command -v "$candidate"
    else
        echo ""
    fi
}

GCOV_MODE="json"
if [[ "$(basename "$CXX_PATH")" == clang++* ]]; then
    if command -v xcrun >/dev/null 2>&1 && xcrun --find llvm-cov >/dev/null 2>&1; then
        GCOV_MODE="llvm-i"
        GCOV="xcrun llvm-cov gcov"
    else
        echo "Error: clang++ build detected, but llvm-cov is not available via xcrun." >&2
        exit 1
    fi
else
    GCOV="$(resolve_gcov_from_tool "$CXX")"
    if [ -z "$GCOV" ]; then
        GCOV="$(resolve_gcov_from_tool "$FC")"
    fi
    if [ -z "$GCOV" ]; then
        GCOV="gcov"
    fi

    # coverage_cpp.sh relies on gcov JSON output (-j), which is GNU gcov-specific.
    if ! "$GCOV" --help 2>&1 | grep -q -- '-j'; then
        if [ -n "${FC:-}" ]; then
            fc_path="$(command -v "$FC" 2>/dev/null || true)"
            if [ -n "$fc_path" ]; then
                fc_base="$(basename "$(readlink -f "$fc_path" 2>/dev/null || echo "$fc_path")")"
                candidate="${fc_base/gfortran/gcov}"
                if command -v "$candidate" >/dev/null 2>&1; then
                    GCOV="$(command -v "$candidate")"
                fi
            fi
        fi
    fi

    if ! "$GCOV" --help 2>&1 | grep -q -- '-j'; then
        echo "Error: no gcov frontend with JSON (-j) support found." >&2
        exit 1
    fi
fi

echo "Using Fortran compiler: $(command -v "$FC" || echo "$FC")" >&2
echo "Using C++ compiler:     $(command -v "$CXX" || echo "$CXX")" >&2
echo "Using gcov:             $GCOV" >&2
echo "Coverage parse mode:    $GCOV_MODE" >&2

# Keep the same stale-build safeguard pattern as tools/coverage.sh.
echo "Cleaning fpm default build tree (fpm clean --all)..." >&2
fpm clean --skip </dev/null >/dev/null 2>&1 || true

export FPM_BUILD_DIR="${FPM_BUILD_DIR:-build/gcov-cpp}"
export FPM_CXXFLAGS="${FPM_CXXFLAGS:-} -O0 -g --coverage"

# Clang's --coverage needs its profile runtime available at final link.
if [[ "$(basename "$CXX_PATH")" == clang++* ]]; then
    profile_rt="$($CXX_PATH --print-file-name=libclang_rt.profile_osx.a 2>/dev/null || true)"
    if [ -n "$profile_rt" ] && [ -f "$profile_rt" ]; then
        export FPM_LDFLAGS="${FPM_LDFLAGS:-} $profile_rt"
    else
        echo "Warning: clang coverage runtime not found; link may fail." >&2
    fi
fi

echo "Cleaning previous C++ coverage build directory: $FPM_BUILD_DIR" >&2
rm -rf "$FPM_BUILD_DIR"

echo "Building + running run_tester with coverage instrumentation..." >&2
fpm test run_tester -- "$@"

if [ "$#" -eq 0 ]; then
    echo "Running tools/run_error_scenarios.sh for additional error-path coverage..." >&2
    "$ROOT_DIR/tools/run_error_scenarios.sh" >&2 || true
fi

echo "Collecting C++ coverage data for src/parquet_wrapper.cpp..." >&2

gcda_count=$(find "$FPM_BUILD_DIR" -name '*.gcda' -path '*/src_parquet_wrapper.cpp.gcda' | wc -l | tr -d ' ')
if [ "$gcda_count" -eq 0 ]; then
    echo "No parquet_wrapper.cpp .gcda files found under $FPM_BUILD_DIR -- did the build/run succeed?" >&2
    exit 1
fi

find "$FPM_BUILD_DIR" -name '*.gcda' -path '*/src_parquet_wrapper.cpp.gcda' -exec dirname {} \; | sort -u | \
while IFS= read -r dir; do
    if [ "$GCOV_MODE" = "json" ]; then
        ( cd "$dir" && "$GCOV" -j -o . src_parquet_wrapper.cpp.gcda >/dev/null )
    else
        ( cd "$dir" && xcrun llvm-cov gcov -i -o . src_parquet_wrapper.cpp.gcda >/dev/null )
    fi
done

python3 - "$FPM_BUILD_DIR" "$GCOV_MODE" <<'PY'
import gzip
import glob
import json
import os
import re
import sys

build_dir = sys.argv[1]
mode = sys.argv[2]
target = "src/parquet_wrapper.cpp"

# Mirrors gcovr's own exclusion rules (GCOVR_EXCL_LINE / GCOVR_EXCL_START.../GCOVR_EXCL_STOP
# comment markers, plus .gitlab-ci.yml's --exclude-lines-by-pattern regexes) so this local tool's
# percentage matches what CI's gcovr invocation reports, rather than counting lines that std::abort()
# (or an uncaught throw crossing the extern "C" boundary) makes gcov unable to ever observe as
# covered, no matter how well-tested.
# Deliberately anchored to "only whitespace before report_fatal_error(" (not ".*report_fatal_error(")
# -- a handful of call sites are the tail end of an `if (cond) report_fatal_error(...);` guard on
# one line (the temporal nrows-mismatch checks), where the `if` itself genuinely runs (and is hit)
# on every normal read; a bare ".*" pattern would silently discard that real coverage signal along
# with the embedded call gcov can't separately attribute. Keep EXCLUDE_LINE_PATTERNS in sync with
# .gitlab-ci.yml's gcovr call if that ever changes.
EXCLUDE_LINE_PATTERNS = [re.compile(r"\s*report_fatal_error\(")]


def compute_excluded_lines(source_path):
    with open(source_path) as f:
        src_lines = f.readlines()
    excluded = set()
    start_line = None
    for lineno, text in enumerate(src_lines, start=1):
        if "GCOVR_EXCL_START" in text:
            start_line = lineno
        if "GCOVR_EXCL_LINE" in text:
            excluded.add(lineno)
        if any(p.match(text) for p in EXCLUDE_LINE_PATTERNS):
            excluded.add(lineno)
        if "GCOVR_EXCL_STOP" in text and start_line is not None:
            excluded.update(range(start_line, lineno + 1))
            start_line = None
    return excluded


lines = {}
if mode == "json":
    for path in glob.glob(os.path.join(build_dir, "**", "*.gcov.json.gz"), recursive=True):
        with gzip.open(path) as f:
            data = json.load(f)
        for entry in data["files"]:
            if entry["file"] != target:
                continue
            for line in entry["lines"]:
                ln = line["line_number"]
                lines[ln] = max(lines.get(ln, 0), line["count"])
else:
    for path in glob.glob(os.path.join(build_dir, "**", "*.gcov"), recursive=True):
        current = None
        with open(path) as f:
            for raw in f:
                line = raw.strip()
                if line.startswith("file:"):
                    current = line[5:]
                    continue
                if current != target:
                    continue
                if line.startswith("lcount:"):
                    payload = line[7:]
                    num, count = payload.split(",", 1)
                    ln = int(num)
                    c = int(count)
                    lines[ln] = max(lines.get(ln, 0), c)

if not lines:
    print("No coverage data found for src/parquet_wrapper.cpp", file=sys.stderr)
    sys.exit(1)

excluded_lines = compute_excluded_lines(target)
excluded_with_hits = sorted(ln for ln in excluded_lines if lines.get(ln, 0) > 0)
if excluded_with_hits:
    print(f"Warning: {len(excluded_with_hits)} GCOVR_EXCL'd line(s) actually had hits "
        f"(excluded from the totals below anyway, matching gcovr's own behavior): "
        f"{', '.join(str(ln) for ln in excluded_with_hits)}", file=sys.stderr)
for ln in excluded_lines:
    lines.pop(ln, None)

covered = sum(1 for c in lines.values() if c > 0)
total = len(lines)
pct = 100.0 * covered / total if total else 0.0

print()
print("file                      covered/total      pct")
print("-------------------------------------------------")
print(f"{target:<25}  {covered:>6}/{total:<6}  {pct:6.2f}%")
print("-------------------------------------------------")

uncovered = sorted(ln for ln, c in lines.items() if c == 0)
if not uncovered:
    print("Uncovered lines: (fully covered)")
else:
    ranges = []
    for ln in uncovered:
        if ranges and ln == ranges[-1][1] + 1:
            ranges[-1] = (ranges[-1][0], ln)
        else:
            ranges.append((ln, ln))
    text = ", ".join(str(a) if a == b else f"{a}-{b}" for a, b in ranges)
    print(f"Uncovered lines: {text}")
PY
