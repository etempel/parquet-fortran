#!/usr/bin/env bash
# Asserts that every parquet_table materialization path releases its Arrow-side buffers.
#
# feature_risks.md Risk-1: a path that forgets to release the Arrow column after copying it into
# the table fails NOTHING -- the values are right, every test passes, and the table quietly holds
# two copies of every column it reads. Only Arrow's own pool counter notices, and RSS cannot answer
# the question at all (Arrow's pool keeps freed pages rather than returning them to the OS, so a
# correct release and a complete failure to release look nearly identical in `ps` -- see CLAUDE.md's
# "Measuring whether Arrow memory was actually freed").
#
# This is a CHECK, not a benchmark: it exits nonzero if any path retained more than its tolerance,
# so it can be run as a regression gate after touching the materialization paths. It is NOT part of
# `fpm test` -- it needs its own process per measurement, which a test-drive suite cannot give it.
#
# Each mode runs in its OWN process, deliberately. A baseline and the path under test measured in
# one process report the high-water mark of the pair, which makes whichever ran second look like it
# retained memory it had already released.
#
# The `control` run is not optional and runs FIRST: it asserts the counter MOVES when a column is
# deliberately retained. Without it, a measurement that silently reported zero -- a different Arrow
# build, a pool that is not the default one -- would print PASS for every path and mean nothing.
#
# Usage:
#   tools/check_arrow_release.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   TARGET_FILE_SIZE_GB=0.05  Approximate uncompressed size of the test file. Small on purpose:
#                              this is a check, and the verdict is a ratio rather than a timing,
#                              so a bigger file buys nothing but runtime.
#   NCOLS=6                   float64 columns in the test file.
#   TOLERANCE=0.02            Allowed retention after a path completes, as a fraction of one copy
#                              of the data it read. Non-zero only because Arrow keeps a little
#                              schema/metadata state per open reader -- not to leave room for a
#                              retained column, which would be orders of magnitude larger.
#   TEST_FILE                 Path for the synthetic test file. Default: a fresh mktemp -d
#                              directory, deleted automatically on exit. Set this to keep the file.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

TARGET_FILE_SIZE_GB="${TARGET_FILE_SIZE_GB:-0.05}"
NCOLS="${NCOLS:-6}"
TOLERANCE="${TOLERANCE:-0.02}"
TEST_FILE="${TEST_FILE:-}"

cleanup_dir=""
if [[ -z "$TEST_FILE" ]]; then
    cleanup_dir="$(mktemp -d)"
    TEST_FILE="$cleanup_dir/check_arrow_release.parquet"
    trap 'rm -rf "$cleanup_dir" check_arrow_release_out.parquet' EXIT
else
    mkdir -p "$(dirname "$TEST_FILE")"
    trap 'rm -f check_arrow_release_out.parquet' EXIT
fi

echo "=== Arrow release check (feature_risks.md Risk-1) ==="
echo "file        : $TEST_FILE"
echo "target size : ${TARGET_FILE_SIZE_GB} GB uncompressed"
echo "columns     : ${NCOLS} x float64"
echo "tolerance   : ${TOLERANCE} of one copy of the data each path read"
echo

fpm run check_arrow_release --profile release -- \
    --mode=write_fixture --file="$TEST_FILE" --size="$TARGET_FILE_SIZE_GB" --ncols="$NCOLS"
echo

# control FIRST: every PASS below is meaningless if the counter does not move at all.
for mode in control materialize_all prefetch get slice write_release; do
    fpm run check_arrow_release --profile release -- \
        --mode="$mode" --file="$TEST_FILE" --tolerance="$TOLERANCE"
    echo
done

# ...and materialize_all/prefetch AGAIN on one thread, which is not redundant.
#
# The internally-parallel %prefetch gives each thread its OWN reader and closes it at the end of
# the region, and closing a reader frees whatever it had cached whether or not the release call
# ran. So the parallel path PASSES even with every parquet_release_column call removed -- verified
# by doing exactly that. `omp_get_max_threads() <= 1` is the first clause of parallel_prefetch_ok,
# so OMP_NUM_THREADS=1 forces the serial batch-release path, where the release is the only thing
# that can free the column. Without these two runs, materialize_marked's own release policy is
# untested here.
for mode in materialize_all prefetch; do
    echo "(serial batch-release path: OMP_NUM_THREADS=1)"
    OMP_NUM_THREADS=1 fpm run check_arrow_release --profile release -- \
        --mode="$mode" --file="$TEST_FILE" --tolerance="$TOLERANCE"
    echo
done

echo "All materialization paths released their Arrow buffers."
