#!/usr/bin/env bash
# Benchmarks parquet-fortran's write and read performance scaling across Arrow thread-pool
# sizes (parquet_set_max_threads), on one synthetic multi-type Parquet file. Each thread-count
# data point is measured by its own `fpm run benchmark_threads` subprocess -- see
# app/benchmark_threads.f90 for what a single run actually times (open/write.close or
# open(prefetch)/read/close only, excluding schema build and synthetic-data generation). This
# script only decides how many steps to sweep, what file size to target, drives the sweep, and
# tabulates the results.
#
# The read sweep reuses a single static file -- the one written by the write sweep's
# highest-thread-count run -- for every thread count, so it measures how Arrow's internal
# thread pool speeds up decoding *one* file, not concurrent multi-file throughput.
#
# Usage:
#   tools/benchmark_threads.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   MAX_STEPS=10             Max number of thread-count steps to sweep (log-spaced, 1..cores).
#   TARGET_FILE_SIZE_GB=1.0  Approximate uncompressed (in-memory) size of the test file.
#   TEST_FILE                Path for the synthetic test file. Default: a fresh mktemp -d
#                             directory, deleted automatically when the script exits. Set this
#                             to keep the file around afterward (e.g. to inspect it, or reuse it
#                             across separate write-only/read-only runs) -- it is NOT deleted
#                             when explicitly set, and its parent directory is created if needed.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

MAX_STEPS="${MAX_STEPS:-10}"
TARGET_FILE_SIZE_GB="${TARGET_FILE_SIZE_GB:-1.0}"
TEST_FILE="${TEST_FILE:-}"

if command -v nproc >/dev/null 2>&1; then
    total_cores="$(nproc)"
elif command -v sysctl >/dev/null 2>&1; then
    total_cores="$(sysctl -n hw.ncpu)"
else
    total_cores=1
fi

echo "Detected cores: $total_cores (up to $MAX_STEPS steps, target size ${TARGET_FILE_SIZE_GB}GB)" >&2

# Log-spaced thread-count list from 1..total_cores, capped at MAX_STEPS distinct steps
# (duplicates collapsed after rounding -- see CONTRIBUTING.md's benchmark_threads entry for
# a worked example). Avoids bash4-only associative arrays/negative indices throughout this
# script since macOS ships bash 3.2 by default (see tools/coverage.sh's own note on this).
core_list=($(python3 - "$total_cores" "$MAX_STEPS" <<'PY'
import math
import sys

total_cores = int(sys.argv[1])
max_steps = int(sys.argv[2])

if total_cores <= 1:
    print(1)
    sys.exit()

steps = min(max_steps, total_cores)
if steps <= 1:
    print(total_cores)
    sys.exit()

values = []
for i in range(steps):
    exponent = i / (steps - 1)
    v = round(math.exp(math.log(total_cores) * exponent))
    values.append(max(1, v))
values[-1] = total_cores

seen = []
for v in values:
    if not seen or seen[-1] != v:
        seen.append(v)

print(" ".join(str(v) for v in seen))
PY
))

echo "Thread-count steps: ${core_list[*]}" >&2

echo "Building..." >&2
fpm build >&2

if [ -n "$TEST_FILE" ]; then
    # User-specified path: created under whatever directory they chose, and left in place
    # afterward (no cleanup trap) so it can be inspected or reused.
    tmpfile="$TEST_FILE"
    mkdir -p "$(dirname "$tmpfile")"
else
    tmpdir="$(mktemp -d)"
    trap 'rm -rf "$tmpdir"' EXIT
    tmpfile="$tmpdir/benchmark_threads.parquet"
fi

parse_elapsed() {
    # $1: a "RESULT mode=... threads=... nrows=... elapsed_s=..." line (possibly preceded by
    # other fpm run build-status noise on earlier lines).
    printf '%s\n' "$1" | grep -oE 'elapsed_s=[0-9.]+' | cut -d= -f2
}

write_cores=()
write_times=()
echo "Running write sweep..." >&2
for n in "${core_list[@]}"; do
    line="$(fpm run benchmark_threads -- --mode=write --threads="$n" --size="$TARGET_FILE_SIZE_GB" --file="$tmpfile")"
    elapsed="$(parse_elapsed "$line")"
    write_cores+=("$n")
    write_times+=("$elapsed")
    echo "  threads=$n elapsed_s=$elapsed" >&2
done

read_cores=()
read_times=()
last_index=$((${#core_list[@]} - 1))
echo "Running read sweep (single static file, written above at threads=${core_list[$last_index]})..." >&2
for n in "${core_list[@]}"; do
    line="$(fpm run benchmark_threads -- --mode=read --threads="$n" --file="$tmpfile")"
    elapsed="$(parse_elapsed "$line")"
    read_cores+=("$n")
    read_times+=("$elapsed")
    echo "  threads=$n elapsed_s=$elapsed" >&2
done

echo
echo "Write (threads vs. time)"
printf '%8s  %12s  %12s\n' "cores" "total_s" "s_per_core"
printf -- '------------------------------------\n'
for i in "${!write_cores[@]}"; do
    c="${write_cores[$i]}"
    t="${write_times[$i]}"
    per_core="$(awk "BEGIN { printf \"%.6f\", $t / $c }")"
    printf '%8s  %12s  %12s\n' "$c" "$t" "$per_core"
done

echo
echo "Read (threads vs. time)"
printf '%8s  %12s  %12s\n' "cores" "total_s" "s_per_core"
printf -- '------------------------------------\n'
for i in "${!read_cores[@]}"; do
    c="${read_cores[$i]}"
    t="${read_times[$i]}"
    per_core="$(awk "BEGIN { printf \"%.6f\", $t / $c }")"
    printf '%8s  %12s  %12s\n' "$c" "$t" "$per_core"
done
