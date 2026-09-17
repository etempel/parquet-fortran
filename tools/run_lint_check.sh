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
# ONE ENTRY NEEDS A PYTHON LIBRARY RATHER THAN JUST THE INTERPRETER: `generate_stats_vectors.py
# --check` re-derives `parquet_stats`' golden expectations at 50 digits, so it needs `mpmath`
# (`pip install mpmath`; CI's lint job installs Ubuntu's `python3-mpmath` for the same reason).
# Without it that one check fails and the rest still run. Its `--self-test`, which validates the
# model against numpy/pandas/scipy, is heavier still and is deliberately in neither list -- it is a
# step to run when the MODEL changes, not on every push.
#
# Apart from that one dependency it needs nothing but `python3` and
# `bash` -- no fpm, no gfortran, no Arrow -- and takes about ten seconds, so it is worth running
# before every push. Most of that is three checks doing real arithmetic rather than pattern
# matching: `check_source_conventions.py` walks every source file, and the ziggurat and golden-
# vector generators re-derive their contracts from scratch (the ziggurat's twice, at two working
# precisions -- see Risk-130 for why that second derivation is the check with power):
#
#     tools/run_lint_check.sh              # run every check, then list any that failed
#     tools/run_lint_check.sh --fail-fast  # stop at the first failure, like CI does
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
    "python3 tools/generate_interpolate_vectors.py --check"
    "bash tools/generate_parquet_maml.sh base --check"
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
)

if ! command -v python3 >/dev/null 2>&1; then
    echo "run_lint_check.sh: python3 not found on PATH (every check needs it)" >&2
    exit 2
fi

failed=()
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

if [ ${#failed[@]} -eq 0 ]; then
    echo "All ${#CHECKS[@]} lint checks passed."
    exit 0
fi

echo "${#failed[@]} of ${#CHECKS[@]} lint check(s) FAILED:"
for check in "${failed[@]}"; do
    echo "  $check"
done
exit 1
