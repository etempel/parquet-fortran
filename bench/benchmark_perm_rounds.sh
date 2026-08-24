#!/usr/bin/env bash
#
# Prices the proposed 24-round + parity permutation against Fisher-Yates and the shipped 4-round
# kernel. Thin wrapper over `bench/probe_random_perm_rounds.f90`; see `feature_random_feistel_A.md`
# for what the numbers are for.
#
# The probe validates its own replica against `pf_random_perm_at` and `pf_random_permutation`
# before it times anything, and exits nonzero if that fails -- a timing of an algorithm that is not
# the library's is worse than no timing.
#
# Everything configurable is below and can be overridden from the environment.
#
# Exit codes: 0 all modes ran, 1 a mode failed (including a failed replica gate), 2 terminated early.
# ---------------------------------------------------------------------------------------------
# Prices the proposed 24-round + parity permutation of
# `feature_random_feistel_A.md` against Fisher-Yates and against the shipped 4-round kernel, over
# `bench/probe_random_perm_rounds.f90`. Three modes, and they answer different questions: `whole`
# builds a complete permutation, where Fisher-Yates is the faster algorithm; `subset` draws `n` of
# `m`, where the Feistel wins by whatever factor `m/n` happens to be, because Fisher-Yates has to
# shuffle the whole population and the Feistel reads `n` elements; and `scalar` measures single-
# element random access, which Fisher-Yates cannot do at all. Sweep `m` rather than taking one size
# -- Fisher-Yates degrades with the population (its random access misses cache) while the Feistel's
# cost is flat, so the two cross over, and a single size mischaracterises the comparison. The probe
# gates its own replica of the kernel against `pf_random_perm_at` and `pf_random_permutation` before
# timing anything, and the wrapper refuses to run if the requested profile produced no `-O` flag
# rather than reporting an `-O0` run as a release one. Maintainer-only.
#
#     bench/benchmark_perm_rounds.sh                          # every mode, default sweep
#     MODES=whole WHOLE_SIZES="1000 1000000" bench/benchmark_perm_rounds.sh
#     ROUNDS=16 bench/benchmark_perm_rounds.sh                # price a different proposal
# ---------------------------------------------------------------------------------------------

set -uo pipefail

# ============================================================================
# CONFIGURATION -- edit here, or override from the environment
# ============================================================================

# Which modes to run. `whole` = full permutation, `subset` = n of m, `scalar` = random access.
MODES="${MODES:-whole subset scalar}"

# Population sizes swept by `whole`. Note Fisher-Yates degrades with m (cache) while the Feistel
# does not, so a single size does not characterise the comparison.
WHOLE_SIZES="${WHOLE_SIZES:-1000 100000 1000000 10000000}"

# `subset` population and draw sizes. The gap here is structural and grows linearly with m.
SUBSET_M="${SUBSET_M:-10000000}"
SUBSET_N="${SUBSET_N:-1000}"

# Population size used by `scalar`.
SCALAR_M="${SCALAR_M:-1000}"

# Round count of the proposal under test.
ROUNDS="${ROUNDS:-24}"

# Timed repetitions; the best is reported, per CLAUDE.md's benchmarking notes.
REPS="${REPS:-3}"

# fpm profile. Release is required for any figure to mean anything.
PROFILE="${PROFILE:-release}"

# ============================================================================
# End of configuration
# ============================================================================

cd "$(dirname "$0")/.."

# A tool that stops early must not exit 0. Deliberate exits set the flag first.
finished=0
trap '[ "$finished" = "1" ] || { echo ""; echo "benchmark_perm_rounds.sh: TERMINATED EARLY -- this run proves nothing" >&2; exit 2; }' EXIT

echo "=== permutation round-count benchmark ==="
echo "building (--profile $PROFILE) ..."
if ! fpm build --profile "$PROFILE" >/dev/null 2>&1; then
    echo "benchmark_perm_rounds.sh: build failed" >&2
    finished=1
    exit 1
fi

BIN="$(find build -type f -perm -u+x -name probe_random_perm_rounds | head -n 1)"
if [ -z "$BIN" ]; then
    echo "benchmark_perm_rounds.sh: could not find the built probe_random_perm_rounds binary" >&2
    finished=1
    exit 1
fi

# A release profile that carries no -O is an -O0 run wearing a release label, and it silently
# invalidates every figure below. Refuse rather than degrade.
FLAGS="$(fpm build --profile "$PROFILE" --show-model 2>/dev/null | grep -o 'fortran_compile_flags="[^"]*"' | head -n 1)"
case "$FLAGS" in
    *-O*) : ;;
    *)
        case "${FPM_FC:-gfortran}" in
            ifx|icx|*/ifx|*/icx)
                echo "note: no -O in the flags, but ifx optimises by default -- continuing" ;;
            *)
                echo "benchmark_perm_rounds.sh: --profile $PROFILE produced no -O flag for" \
                     "${FPM_FC:-gfortran}; this would be an -O0 run reported as a release one." >&2
                echo "  Append one instead of assigning: FPM_FFLAGS=\"\${FPM_FFLAGS:-} -O3\"" >&2
                finished=1
                exit 1 ;;
        esac ;;
esac

echo "binary   : $BIN"
echo "rounds   : $ROUNDS"
echo ""

rc=0
for mode in $MODES; do
    case "$mode" in
        whole)
            for m in $WHOLE_SIZES; do
                "$BIN" --mode=whole --m="$m" --rounds="$ROUNDS" --reps="$REPS" || rc=1
                echo ""
            done ;;
        subset)
            "$BIN" --mode=subset --m="$SUBSET_M" --n="$SUBSET_N" --rounds="$ROUNDS" --reps="$REPS" || rc=1
            echo "" ;;
        scalar)
            "$BIN" --mode=scalar --m="$SCALAR_M" --rounds="$ROUNDS" || rc=1
            echo "" ;;
        *)
            echo "benchmark_perm_rounds.sh: unknown mode '$mode'" >&2
            rc=1 ;;
    esac
done

finished=1
if [ "$rc" != "0" ]; then
    echo "benchmark_perm_rounds.sh: at least one mode failed" >&2
    exit 1
fi
echo "all modes completed."
