#!/usr/bin/env bash
# Times every parquet_string_column bulk operation on one synthetic column, so that an
# optimisation to that type is measured rather than argued. See app/benchmark_strings.f90 for what
# each row means, and feature_string_parallel.md for the plan these numbers gate.
#
# Runs twice by default: once on a null-free column and once on a null-containing one, since
# several operations have a separate validity pass whose cost only appears in the second.
#
# Usage:
#   tools/benchmark_strings.sh
#
# Config (env-overridable, matching this repo's other tools/ *.sh scripts):
#   NROWS=4000000    Elements in the synthetic column.
#   LEN=24           Mean element length in bytes; lengths vary deterministically around it.
#   ROUNDS=3         Timed rounds per operation. The best is kept, since the minimum is the run
#                     least disturbed by everything else on the machine.
#   NULL_EVERY=7     Stride of the null elements in the second run.
#   NULLS=both       Which runs to do: "no", "yes", or "both".
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
