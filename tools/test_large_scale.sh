#!/usr/bin/env bash
# Manual, user-runnable large-scale write/read/verify check for every supported data type -- see
# app/test_large_scale.f90 for what a single run actually does (one column at a time, memory
# bounded to one column's data at a time). Not part of `fpm test`/CI: this is purely a manual
# tool.
#
# Vector-column (col_size=NELEM) cases are currently hard-disabled via RUN_VECTOR_CASES in
# app/test_large_scale.f90 (6 scalar-only cases run instead of the full 12) -- Arrow/Parquet's
# own list-element-count ceiling (nrows * col_size capped at 2^31-1; see
# check_list_element_count_fits_arrow_limit in parquet_wrapper.cpp) means a NELEM > 1 run aborts
# on the vector cases well before an NROWS large enough to be an interesting scalar check. Flip
# RUN_VECTOR_CASES back on there once Arrow supports more than 2^31-1 elements in a list column.
#
# Usage:
#   tools/test_large_scale.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   NROWS=1000       Row count for every case. Default is a small, cheap sanity value -- a
#                     genuine >huge(1_int32) (2,147,483,647) run needs a large-memory machine
#                     and is expected to be set explicitly.
#   NELEM=2          Vector-column width (col_size); unused while RUN_VECTOR_CASES is .false.
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
