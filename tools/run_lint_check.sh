#!/usr/bin/env bash
# Runs the same checks as .gitlab-ci.yml's `lint` stage, locally.
#
# Usage:
#   tools/run_lint_check.sh              # run every check, report a summary
#   tools/run_lint_check.sh --fail-fast  # stop at the first failing check
#
# Exits 0 only if every check passed, so this is usable as a pre-commit/pre-push
# gate. Needs nothing but python3, mpmath and bash -- no fpm, no gfortran, no
# Arrow -- which is why the CI job it mirrors overrides the pipeline's own
# heavyweight before_script.
#
# By default every check runs even after one fails, and the failures are listed
# together at the end. That deliberately differs from CI, whose `script:` stops
# at the first nonzero command: locally it is more useful to see every problem
# in one pass than to rediscover them one commit at a time. The verdict is the
# same either way (nonzero if anything failed); pass --fail-fast for CI's exact
# stop-on-first behaviour.
#
# WHEN A CHECK IS ADDED TO OR REMOVED FROM .gitlab-ci.yml's `lint` JOB, UPDATE
# THE `CHECKS` LIST BELOW TO MATCH. Nothing enforces that the two stay in step,
# so a check added only to CI would pass here and then fail the pipeline, and
# one added only here would give false confidence. The list is kept in the same
# order the CI job runs them.
# ---------------------------------------------------------------------------------------------
# Runs the same checks as `.gitlab-ci.yml`'s `lint` stage, locally: the three static checks
# (`check_bindc_boundary.py`, `check_doc_anchors.py`, `check_source_conventions.py`), then each
# generator's `--check` -- which re-derives its committed output and fails if the file on disk has
# drifted from it -- and, for the generators that also verify their own model, its `--self-test`.
# The `CHECKS` array below is the list; it is not enumerated here as well, because a second copy of
# it would go stale the first time one was added.
#
# THREE ENTRIES NEED A PYTHON LIBRARY RATHER THAN JUST THE INTERPRETER: `generate_stats_vectors.py
# --check` and `generate_kde_vectors.py --check` re-derive `parquet_stats`' and `parquet_kde`'s golden
# expectations at 50 digits, and `generate_cosmology_reference.py` re-derives `parquet_cosmology`'s
# at 30, so all three need `mpmath` (`pip install mpmath`; CI's lint job installs Ubuntu's
# `python3-mpmath` for the same reason). Without it those checks fail and the rest still run. The
# first two `--self-test`s, which validate each model against the Python libraries, are heavier
# still and are deliberately in neither list -- steps to run when a MODEL changes, not on every
# push. `generate_cosmology_reference.py --self-test` IS in both lists: it needs nothing beyond
# mpmath, and it is what holds the module's own constants and its Gauss-Legendre rule to the
# generator's values. Its `--verify-oracle`, which needs astropy, is the one kept out.
#
# Apart from that one dependency it needs nothing but `python3` and
# `bash` -- no fpm, no gfortran, no Arrow -- and is meant to stay inside about a minute, so that it
# is worth running before every push. THE CHECKS RUN IN PARALLEL, `RUN_LINT_CHECK_JOBS` at a time
# (default: the core count), because most of the run is a handful of checks doing real arithmetic
# rather than pattern matching, and the rest are idle while they finish: the two mpmath oracles
# re-derive `parquet_kde`'s and `parquet_cosmology`'s golden expectations, and the ziggurat and
# golden-vector generators re-derive their contracts from scratch (the ziggurat's twice, at two
# working precisions -- see Risk-130 for why that second derivation is the check with power).
# The checks write nothing and share no state, which is what makes that safe; each one's output is
# captured whole and reported in the `CHECKS` order below, never interleaved. `RUN_LINT_CHECK_JOBS=1`
# forces the plain serial run.
#
# WHEN A CHECK GETS SLOW ENOUGH TO NOTICE, MEASURE BEFORE TOUCHING ITS ARITHMETIC. An oracle's
# precision and scan density are what it certifies and are not tuning parameters; the two heavy
# generators were brought down by mapping their independent cases across processes
# (`tools/gen_parallel.py`, which also records the `gmpy2` route), and `check_source_conventions.py`
# by deriving each file's comment-stripped form once instead of once per check. All three left
# every answer identical, which is the bar.
#
#     tools/run_lint_check.sh              # run every check, then list any that failed
#     tools/run_lint_check.sh --fail-fast  # stop at the first failure, like CI does (serial)
# By default it runs every check even after one fails and lists the failures together at the end,
# which differs deliberately from CI (whose `script:` stops at the first nonzero command): locally
# it is more useful to see every problem in one pass. The verdict is the same either way -- it exits
# nonzero if anything failed. Its `CHECKS` list is kept in sync with the CI job by hand, so a check
# added to one must be added to the other; nothing enforces it. Maintainer-only: nothing outside
# this repository runs it.
# ---------------------------------------------------------------------------------------------
set -euo pipefail

ROOT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT_DIR"

FAIL_FAST=0
# `__worker__` is this script re-entering itself for one check (see the worker block below, which
# has to sit under `CHECKS`); it is not a user-facing argument, so it skips the parsing here
# rather than being rejected by it.
if [ "${1:-}" != "__worker__" ]; then
for arg in "$@"; do
    case "$arg" in
        --fail-fast) FAIL_FAST=1 ;;
        -h|--help)
            sed -n '2,25p' "${BASH_SOURCE[0]}" | sed 's/^# \{0,1\}//'
            exit 0
            ;;
        *)
            echo "run_lint_check.sh: unknown argument '$arg' (try --help)" >&2
            exit 2
            ;;
    esac
done
fi

# One entry per check, in .gitlab-ci.yml's own order. Each is a full command
# line, run from the repository root.
CHECKS=(
    "python3 tools/check_bindc_boundary.py"
    "python3 tools/check_doc_anchors.py"
    "python3 tools/check_source_conventions.py"
    "python3 tools/generate_parquet_columns.py --check"
    "python3 tools/generate_parquet_tables.py --check"
    "python3 tools/generate_parquet_sorting.py --self-test"
    "python3 tools/generate_parquet_sorting.py --check"
    "python3 tools/generate_parquet_stats.py --self-test"
    "python3 tools/generate_parquet_stats.py --check"
    "python3 tools/generate_stats_vectors.py --check"
    "python3 tools/generate_kde_vectors.py --check"
    "python3 tools/generate_interpolate_vectors.py --check"
    "bash tools/generate_parquet_maml_base.sh --check"
    "python3 tools/generate_user_table_code.py --self-test"
    "python3 tools/generate_user_table_code.py --check"
    "python3 tools/generate_parquet_ziggurat.py --self-test"
    "python3 tools/generate_parquet_ziggurat.py --check"
    "python3 tools/generate_random_golden_vectors.py --self-test"
    "python3 tools/generate_random_golden_vectors.py --check"
    "python3 tools/generate_random_perm_vectors.py --self-test"
    "python3 tools/generate_random_perm_vectors.py --check"
    "python3 tools/generate_sphere_reference.py --self-test"
    "python3 tools/generate_sphere_reference.py --check"
    "python3 tools/generate_cosmology_reference.py --self-test"
    "python3 tools/generate_cosmology_reference.py --check"
    "python3 tools/generate_skycoord_reference.py --self-test"
    "python3 tools/generate_skycoord_reference.py --check"
    "python3 tools/generate_path_reference.py --self-test"
    "python3 tools/generate_path_reference.py --check"
    "python3 tools/generate_healpix_reference.py --self-test"
)

if ! command -v python3 >/dev/null 2>&1; then
    echo "run_lint_check.sh: python3 not found on PATH (every check needs it)" >&2
    exit 2
fi

# ---- worker mode ------------------------------------------------------------------------------
# Re-entry for one check, dispatched by the parallel run below: `$0 __worker__ <dir> <index>`.
# The whole script is re-read, so `CHECKS` above is the one definition either way. Output is
# captured to a file rather than written to the terminal, because several workers run at once and
# a check's report is only readable whole.
if [ "${1:-}" = "__worker__" ]; then
    results_dir="$2"
    index="$3"
    set +e
    eval "${CHECKS[$index]}" > "$results_dir/$index.out" 2>&1
    echo "$?" > "$results_dir/$index.rc"
    exit 0
fi

# How many checks to run at once. The checks share no state and write nothing (each generator's
# `--check` re-derives its output in memory and compares), so they are safe to run together, and
# on a developer machine that is the difference between a minute and a quarter of one. Two of them
# are themselves process-parallel (`tools/gen_parallel.py`), so the pool is deliberately not
# widened beyond the core count. `--fail-fast` runs serially: stopping at the first failure is
# only meaningful in a defined order.
jobs="${RUN_LINT_CHECK_JOBS:-}"
if [ -z "$jobs" ]; then
    jobs="$( (command -v nproc >/dev/null 2>&1 && nproc) \
            || sysctl -n hw.ncpu 2>/dev/null || echo 4)"
fi
case "$jobs" in
    ''|*[!0-9]*) jobs=1 ;;
esac
[ "$jobs" -lt 1 ] && jobs=1
[ "$FAIL_FAST" -eq 1 ] && jobs=1

failed=()
if [ "$jobs" -gt 1 ]; then
    results_dir="$(mktemp -d "${TMPDIR:-/tmp}/run_lint_check.XXXXXX")"
    trap 'rm -rf "$results_dir"' EXIT
    total="${#CHECKS[@]}"
    echo "Running $total lint checks, $jobs at a time..."
    indices=()
    i=0
    while [ "$i" -lt "$total" ]; do
        indices+=("$i")
        i=$((i + 1))
    done
    printf '%s\n' "${indices[@]}" | xargs -P "$jobs" -n 1 "$0" __worker__ "$results_dir" &
    dispatch_pid=$!
    reported=0
    while kill -0 "$dispatch_pid" 2>/dev/null; do
        sleep 2
        done_n="$(find "$results_dir" -name '*.rc' | wc -l | tr -d ' ')"
        if [ "$done_n" -gt "$reported" ] && [ "$done_n" -lt "$total" ]; then
            printf "  ... %d/%d checks done\n" "$done_n" "$total"
            reported="$done_n"
        fi
    done
    wait "$dispatch_pid"
    echo
    # Reported in the CHECKS order, not the order they happened to finish, so that two runs of the
    # same tree read identically.
    i=0
    while [ "$i" -lt "$total" ]; do
        echo "=== ${CHECKS[$i]}"
        if [ -f "$results_dir/$i.out" ]; then
            cat "$results_dir/$i.out"
        fi
        rc="$(cat "$results_dir/$i.rc" 2>/dev/null)"
        if [ "$rc" = "0" ]; then
            echo "--- PASS"
        else
            # An absent .rc means the worker never recorded one: reported as a failure, because a
            # check whose verdict was not written is a check that did not pass.
            echo "--- FAIL (exit ${rc:-unknown -- the worker recorded no exit status})"
            failed+=("${CHECKS[$i]}")
        fi
        echo
        i=$((i + 1))
    done
else
    for check in "${CHECKS[@]}"; do
        echo "=== $check"
        # `set -e` is deliberately suspended for the check itself: a nonzero exit is
        # a result to record, not a reason to abandon the remaining checks.
        if eval "$check"; then
            echo "--- PASS"
        else
            echo "--- FAIL (exit $?)"
            failed+=("$check")
            if [ "$FAIL_FAST" -eq 1 ]; then
                break
            fi
        fi
        echo
    done
fi

if [ ${#failed[@]} -eq 0 ]; then
    echo "All ${#CHECKS[@]} lint checks passed."
    exit 0
fi

echo "${#failed[@]} of ${#CHECKS[@]} lint check(s) FAILED:"
for check in "${failed[@]}"; do
    echo "  $check"
done
exit 1
