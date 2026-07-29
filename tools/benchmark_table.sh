#!/usr/bin/env bash
# Benchmarks the parquet_table layer against reading/writing columns directly, on one synthetic
# float64 file. See app/benchmark_table.f90 for what each mode actually times; this script only
# decides the file size, generates the fixture once, and drives the four measurement runs.
#
# The number worth watching is the RSS line in the `read` section: parquet_table keeps its own
# Fortran copy of every column and frees the Arrow-side buffers as it materializes them, so a
# fully materialized table should sit near ONE copy of the column data. Roughly two copies means
# the release stopped happening (feature_table.md D3).
#
# Usage:
#   tools/benchmark_table.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   TARGET_FILE_SIZE_GB=0.25  Approximate uncompressed (in-memory) size of the test file.
#   NCOLS=8                   Number of float64 columns in the test file.
#   TEST_FILE                 Path for the synthetic test file. Default: a fresh mktemp -d
#                              directory, deleted automatically when the script exits. Set this
#                              to keep the file around afterward -- it is NOT deleted when
#                              explicitly set, and its parent directory is created if needed.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

TARGET_FILE_SIZE_GB="${TARGET_FILE_SIZE_GB:-0.25}"
NCOLS="${NCOLS:-8}"
TEST_FILE="${TEST_FILE:-}"

cleanup_dir=""
if [[ -z "$TEST_FILE" ]]; then
    cleanup_dir="$(mktemp -d)"
    TEST_FILE="$cleanup_dir/benchmark_table.parquet"
    trap 'rm -rf "$cleanup_dir" benchmark_table_out1.parquet benchmark_table_out2.parquet' EXIT
else
    mkdir -p "$(dirname "$TEST_FILE")"
    trap 'rm -f benchmark_table_out1.parquet benchmark_table_out2.parquet' EXIT
fi

echo "=== parquet_table benchmark ==="
echo "file        : $TEST_FILE"
echo "target size : ${TARGET_FILE_SIZE_GB} GB uncompressed"
echo "columns     : ${NCOLS} x float64"
echo

fpm run benchmark_table --profile release -- \
    --mode=write_fixture --file="$TEST_FILE" --size="$TARGET_FILE_SIZE_GB" --ncols="$NCOLS"
echo

# Each read mode runs in its OWN process: malloc does not reliably return freed pages to the
# OS, so measuring both in one process would report the pair's high-water mark and understate
# the table's release.
fpm run benchmark_table --profile release -- --mode=read_raw --file="$TEST_FILE"
echo

fpm run benchmark_table --profile release -- --mode=read_table --file="$TEST_FILE"
echo

fpm run benchmark_table --profile release -- --mode=access --file="$TEST_FILE"
echo

fpm run benchmark_table --profile release -- --mode=write --file="$TEST_FILE"
