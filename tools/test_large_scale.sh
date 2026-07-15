#!/usr/bin/env bash
# Manual, user-runnable large-scale write/read/verify check for every supported data type -- see
# app/test_large_scale.f90 for what a single run actually does (one column at a time, memory
# bounded to one column's data at a time). Not part of `fpm test`/CI: this is purely a manual
# tool.
#
# Runs 12 cases (6 scalar + 6 vector, one per supported data type) via RUN_VECTOR_CASES = .true.
# in app/test_large_scale.f90. A large NROWS*NELEM for a vector case is not a problem: Arrow/
# Parquet's real list-element-count ceiling (2^31-1, a plain int32_t counter in Parquet's own
# repetition/definition-level generation; see check_chunk_size_fits_limit_for_col_size in
# parquet_wrapper.cpp) is scoped to one row group, not the whole file, and
# parquet_close_writer's row-group auto-sizing already keeps every row group under it regardless
# of how large the total gets -- see the README's Limitations section.
#
# Usage:
#   tools/test_large_scale.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   NROWS=1000       Row count for every case. Default is a small, cheap sanity value -- a
#                     genuine >huge(1_int32) (2,147,483,647) run needs a large-memory machine
#                     and is expected to be set explicitly.
#   NELEM=2          Vector-column width (col_size).
#   MAX_SIZE_GB=8    Skip any case whose estimated uncompressed size would exceed this many GB,
#                     rather than let an accidental NROWS/NELEM combination exhaust memory/disk.
#
# To reproduce "row count itself over huge(1_int32)" (needs a large-memory machine):
#   NROWS=3000000000 MAX_SIZE_GB=64 tools/test_large_scale.sh
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

NROWS="${NROWS:-1000}"
NELEM="${NELEM:-2}"
MAX_SIZE_GB="${MAX_SIZE_GB:-8}"

echo "Running tools/test_large_scale.sh with NROWS=$NROWS NELEM=$NELEM MAX_SIZE_GB=$MAX_SIZE_GB" >&2

fpm run test_large_scale -- --nrows="$NROWS" --nelem="$NELEM" --max-size-gb="$MAX_SIZE_GB"
