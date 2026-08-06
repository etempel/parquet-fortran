#!/usr/bin/env bash
# Benchmarks the parquet_table layer against reading/writing columns directly, on one synthetic
# float64 file. See app/benchmark_table.f90 for what each mode actually times; this script only
# decides the file size, generates the fixture once, and drives the six measurement runs.
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
#   NCOLS=8                   Number of float64 columns in the test file. Minimum 2 -- the access
#                              run compares `z = x + y` across access paths and needs two columns.
#   TOUCH=2                   Columns the lazy-read mode actually reads, out of NCOLS.
#   SLICES=4                  Equal row slices to divide the file into for the slice mode.
#   NULLFRAC=0.1              Fraction of rows the write_nulls run marks null (0 < f < 1).
#   SORT_SIZE_GB=1            Size of the IN-MEMORY table the sort run builds. That run needs no
#                              file fixture, so it is sized independently of TARGET_FILE_SIZE_GB --
#                              the cost it measures grows with rows AND with columns, and the
#                              default file size is too small to separate the two.
#   TEST_FILE                 Path for the synthetic test file. Default: a fresh mktemp -d
#                              directory, deleted automatically when the script exits. Set this
#                              to keep the file around afterward -- it is NOT deleted when
#                              explicitly set, and its parent directory is created if needed.
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

TARGET_FILE_SIZE_GB="${TARGET_FILE_SIZE_GB:-0.25}"
NCOLS="${NCOLS:-8}"
TOUCH="${TOUCH:-2}"
SLICES="${SLICES:-4}"
NULLFRAC="${NULLFRAC:-0.1}"
SORT_SIZE_GB="${SORT_SIZE_GB:-1}"
TEST_FILE="${TEST_FILE:-}"

cleanup_dir=""
if [[ -z "$TEST_FILE" ]]; then
    cleanup_dir="$(mktemp -d)"
    TEST_FILE="$cleanup_dir/benchmark_table.parquet"
    trap 'rm -rf "$cleanup_dir" benchmark_table_out1.parquet benchmark_table_out2.parquet benchmark_table_out3.parquet benchmark_table_nulls.parquet' EXIT
else
    mkdir -p "$(dirname "$TEST_FILE")"
    trap 'rm -f benchmark_table_out1.parquet benchmark_table_out2.parquet benchmark_table_out3.parquet benchmark_table_nulls.parquet' EXIT
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

# Opening is lazy, so these two are the point of the whole layer: what an open costs when it
# reads nothing, and what a program that wants only part of the file actually pays.
fpm run benchmark_table --profile release -- --mode=read_lazy --file="$TEST_FILE" --touch="$TOUCH"
echo

fpm run benchmark_table --profile release -- --mode=read_slice --file="$TEST_FILE" --slices="$SLICES"
echo

fpm run benchmark_table --profile release -- --mode=access --file="$TEST_FILE"
echo

fpm run benchmark_table --profile release -- --mode=write --file="$TEST_FILE"
echo

# The write run above measures the null-free path, where the writer is handed no validity mask at
# all. This one measures what a mask actually costs, which is the case the shortcut cannot help.
fpm run benchmark_table --profile release -- --mode=write_nulls --file="$TEST_FILE" --nullfrac="$NULLFRAC"
echo

# The one run that uses no file at all: it builds its table in memory, because what it measures is
# the cost of REORDERING a resident table (%sort_by's permutation build against its per-column
# reindex), and reading a fixture first would only add a decode to both sides.
fpm run benchmark_table --profile release -- --mode=sort --size="$SORT_SIZE_GB" --ncols="$NCOLS"
