#!/usr/bin/env bash
# Manual large-scale check that `pf_random_fill_draws` is correct past 2**31 elements.
#
# Both fill routines take their length from `size(v)`, which without an explicit `kind=` returns a
# DEFAULT-kind integer and wraps above 2**31 elements. The wrap fails silently: a length that wraps
# negative makes the routine return having written nothing, and one that wraps to a small positive
# value fills a short prefix and leaves the rest of the caller's `intent(out)` array undefined.
# Both were measured on the shipped module before the fix; neither raises anything, and no test
# that `fpm test` could run is large enough to reach the boundary.
#
# `check_fill_size_kind` in tools/check_source_conventions.py is the cheap, always-on guard against
# the declaration regressing. This script is the end-to-end proof, and it needs real memory:
# 2**31 real32 values is about 8.6 GB, and the same count of real64 about 17.2 GB.
#
# NOT part of `fpm test` or CI, deliberately: anything needing more memory, disk or time than
# `fpm test` should ever attempt lives under app/ plus a thin tools/ wrapper, never under test/.
#
# Usage:
#   bench/random_large_fill.sh
#
# Config (env-overridable, matching this repo's other tools/*.sh scripts):
#   ELEMENTS=2147483648   Element count. The default is 2**31 -- the smallest size that wraps a
#                         default-kind `size()`, so the cheapest run that proves anything. Lower
#                         values still exercise the code but CANNOT detect the bug this exists for.
#   RKIND=32              Real kind to fill, 32 or 64. real32 halves the memory for the same
#                         element count, so it is the default; 64 is the heavier confirmation.
# ---------------------------------------------------------------------------------------------
# Is a manual, user-runnable check (never run by `fpm test`/CI)
# that `pf_random_fill_draws` is still correct past 2³¹ array elements. Both fill routines take
# their length from `size(v)`, and a `size` asked without an explicit `kind=` returns a default-kind
# integer, which wraps there. The wrap fails silently in two different ways, both measured on this
# module before they were fixed: 2³¹ elements exactly gave a length of −2147483648, which tripped
# the zero-size guard so the routine returned having written nothing and left the caller's
# `intent(out)` array undefined; and 2³^2 + 8 elements gave a length of 8, so eight values were
# written and the remaining 4.29 billion were left undefined. Neither raises anything, and no test
# `fpm test` could run is large enough to reach the boundary -- the smallest array that does is 2³¹
# `real32` values, about 8.6 GB. `check_fill_size_kind` in `tools/check_source_conventions.py` is
# the cheap always-on guard against the declarations regressing; this is the end-to-end proof, and
# it verifies correctness (each probed element against `pf_random32_at`/`pf_random_at` at the same
# position) rather than merely that something was written:
#
#     bench/random_large_fill.sh                    # 231 real32 elements, about 8.6 GB
#     RKIND=64 bench/random_large_fill.sh           # the same count as real64, about 17.2 GB
#     ELEMENTS=4294967304 bench/random_large_fill.sh  # 232 + 8: the partial-fill variant
# `ELEMENTS` below 2³¹ still exercises the fill but cannot detect the bug this tool exists for, and
# the script says so on stderr rather than passing quietly.
# ---------------------------------------------------------------------------------------------

set -euo pipefail

cd "$(dirname "$0")/.."

ELEMENTS="${ELEMENTS:-2147483648}"
RKIND="${RKIND:-32}"

bytes_per=$([ "$RKIND" = "32" ] && echo 4 || echo 8)
need_gb=$(( ELEMENTS * bytes_per / 1000000000 ))
echo "test_random_large_fill.sh: ${ELEMENTS} elements, real${RKIND} (about ${need_gb} GB)"

if [ "$ELEMENTS" -lt 2147483648 ]; then
    echo "  NOTE: below 2**31 elements. This run exercises the fill but cannot detect a" >&2
    echo "        default-kind size() regression, which is the whole point of this tool." >&2
fi

# --profile release deliberately: fpm applies no -O at all without it, and a multi-GB fill at -O0
# turns a short check into a long one.
exec fpm run --profile release random_large_fill -- \
    --elements="$ELEMENTS" --kind="$RKIND"
