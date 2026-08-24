#!/usr/bin/env bash
# Times every parquet_string_column bulk operation on one synthetic column, so that an
# optimisation to that type is measured rather than argued. See bench/benchmark_strings.f90 for what
# each row means, and feature_string_parallel.md for the plan these numbers gate.
#
# Runs twice by default: once on a null-free column and once on a null-containing one, since
# several operations have a separate validity pass whose cost only appears in the second.
#
# Usage:
#   bench/benchmark_strings.sh
#
# Config (env-overridable, matching this repo's other tools/ *.sh scripts):
#   NROWS=4000000    Elements in the synthetic column.
#   LEN=24           Mean element length in bytes; lengths vary deterministically around it.
#   ROUNDS=3         Timed rounds per operation. The best is kept, since the minimum is the run
#                     least disturbed by everything else on the machine.
#   NULL_EVERY=7     Stride of the null elements in the second run.
#   NULLS=both       Which runs to do: "no", "yes", or "both".
# ---------------------------------------------------------------------------------------------
# Times every `parquet_string_column` bulk operation -- `reindex`,
# `gather`, `delete_by_mask`, `trim_all`, `clone`, `slice`, `append_column`, `to_character`,
# `view_all`, `build_from` and a per-element `length` loop -- on one synthetic in-memory column, so
# an optimisation to that type is measured rather than argued. It touches no file. Two runs by
# default, null-free and null-containing, since several operations have a separate validity pass
# whose cost appears only in the second.
#
# Each operation is best-of-`ROUNDS`, every round starting from a fresh `%clone()` of the same
# source column so no round inherits another's page state or allocation. The throughput column is
# payload-equivalent -- the source column's byte count over the elapsed time -- and compares rows
# against each other only; it is not a claim about bytes moved, since `gather` selects half the rows
# and `to_character` writes a wider padded result than it reads.
#
#     bench/benchmark_strings.sh
#     NROWS=20000000 LEN=48 ROUNDS=5 bench/benchmark_strings.sh
#     NULLS=no bench/benchmark_strings.sh                          # skip the null-containing run
# Config: `NROWS` (default 4000000), `LEN` (24, the mean element length -- lengths vary
# deterministically around it), `ROUNDS` (3), `NULL_EVERY` (7), `NULLS` (`both`, or `no`/`yes`).
#
# The app also takes `--threads=N`, which caps `parquet_set_string_threads` for the run -- a sweep
# over `1 2 4 8` is how the internally-threaded operations' scaling is measured, and it is the only
# way to see the serial path and the threaded path of the same operation side by side.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

NROWS="${NROWS:-4000000}"
LEN="${LEN:-24}"
ROUNDS="${ROUNDS:-3}"
NULL_EVERY="${NULL_EVERY:-7}"
NULLS="${NULLS:-both}"

case "$NULLS" in
    no|yes|both) ;;
    *) echo "benchmark_strings.sh: NULLS must be 'no', 'yes' or 'both' (got '$NULLS')" >&2; exit 1 ;;
esac

# --profile release is not optional: fpm applies optimisation flags only when --profile is given,
# and this benchmark compares loops that optimise very differently.
run() {
    fpm run benchmark_strings --profile release -- "$@"
}

if [ "$NULLS" = "no" ] || [ "$NULLS" = "both" ]; then
    echo "=== no nulls ==="
    run --nrows="$NROWS" --len="$LEN" --rounds="$ROUNDS"
fi

if [ "$NULLS" = "yes" ] || [ "$NULLS" = "both" ]; then
    echo ""
    echo "=== with nulls (every ${NULL_EVERY}th element) ==="
    run --nrows="$NROWS" --len="$LEN" --rounds="$ROUNDS" --null-every="$NULL_EVERY"
fi
