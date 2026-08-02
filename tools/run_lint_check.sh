#!/usr/bin/env bash
# Runs the same checks as .gitlab-ci.yml's `lint` stage, locally.
#
# Usage:
#   tools/run_lint_check.sh              # run every check, report a summary
#   tools/run_lint_check.sh --fail-fast  # stop at the first failing check
#
# Exits 0 only if every check passed, so this is usable as a pre-commit/pre-push
# gate. Needs nothing but python3 and bash -- no fpm, no gfortran, no Arrow --
# and takes well under a second, which is why the CI job it mirrors overrides
# the pipeline's own heavyweight before_script.
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
    "python3 tools/generate_parquet_columns.py --check"
    "python3 tools/generate_parquet_tables.py --check"
    "bash tools/generate_parquet_maml.sh base --check"
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
