#!/usr/bin/env bash
# Manual, user-runnable large-scale write/read/verify check for every supported data type -- see
# bench/large_scale.f90 for what a single run actually does (one column at a time, memory
# bounded to one column's data at a time). Not part of `fpm test`/CI: this is purely a manual
# tool.
#
# Runs 13 cases (7 scalar + 6 vector, one per supported data type plus the compact
# parquet_string_column scalar-only case) via RUN_VECTOR_CASES = .true. in bench/large_scale.f90.
# A large NROWS*NELEM for a vector case is not a problem: Arrow/
# Parquet's real list-element-count ceiling (2^31-1, a plain int32_t counter in Parquet's own
# repetition/definition-level generation; see check_chunk_size_fits_limit_for_col_size in
# parquet_wrapper.cpp) is scoped to one row group, not the whole file, and
# parquet_close_writer's row-group auto-sizing already keeps every row group under it regardless
# of how large the total gets -- see the README's Limitations section.
#
# Usage:
#   bench/large_scale.sh
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
#   NROWS=3000000000 MAX_SIZE_GB=64 bench/large_scale.sh
# ---------------------------------------------------------------------------------------------
# Is a manual, user-runnable check (never run by `fpm test`/CI) that
# this library genuinely reads/writes columns correctly beyond `huge(1_int32)` (2,147,483,647) rows
# -- the scale no automated test in this repository ever attempts, since doing so needs a machine
# with substantial memory and disk. It drives `bench/large_scale.f90` (a maintainer/user-only fpm
# executable, not part of the public library) through 13 cases (7 scalar types plus 6 vector-column
# cases, one per type), one at a time, each checking `parquet_get_nrows` and a full read-back
# against the true data. Set the `RUN_VECTOR_CASES` compile-time parameter at the top of
# `bench/large_scale.f90` to `.false.` and rebuild to skip the 6 vector cases and run only the 7
# scalar ones. `NROWS`, `NELEM`, and `MAX_SIZE_GB` are its env-overridable config -- `NROWS` sets
# the row count for every case (default a small, cheap `1000`), `NELEM` sets the vector cases'
# `col_size` (default `2`), and `MAX_SIZE_GB` (default `8`) skips any case whose estimated
# uncompressed size would exceed it instead of letting an oversized value exhaust memory/disk.
# Progress is printed per case (`Running test X of 13: ...` / `Finished test X of 13: ... -- PASSED
# (12.345s)` / `Skipped test X of 13: ...`, or `of 7` when `RUN_VECTOR_CASES = .false.`):
#
#     bench/large_scale.sh
#     # Row count itself beyond huge(1_int32) (needs a large-memory machine); the vector cases here
#     # (NROWS * NELEM = 6 billion) still round-trip fine via row-group auto-sizing:
#     NROWS=3000000000 MAX_SIZE_GB=120 bench/large_scale.sh
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

NROWS="${NROWS:-1000}"
NELEM="${NELEM:-2}"
MAX_SIZE_GB="${MAX_SIZE_GB:-8}"

echo "Running bench/large_scale.sh with NROWS=$NROWS NELEM=$NELEM MAX_SIZE_GB=$MAX_SIZE_GB" >&2

fpm run large_scale -- --nrows="$NROWS" --nelem="$NELEM" --max-size-gb="$MAX_SIZE_GB"
