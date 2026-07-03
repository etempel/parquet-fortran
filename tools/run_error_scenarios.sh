#!/usr/bin/env bash
# Runs every scenario supported by the test/error_scenarios.f90 helper program
# and checks that each one aborts (or exits cleanly) as expected. This is the
# same check test_errors.f90 does from inside the test-drive suite, exposed
# here as a standalone script for manual/CI use without going through fpm's
# test-drive runner.
set -uo pipefail

cd "$(dirname "$0")/.."

# scenario:expect_abort ("0" or "1")
scenarios=(
    "ok:0"
    "write_undeclared_column:1"
    "write_type_mismatch:1"
    "write_column_twice:1"
    "validate_bad_data_type:1"
    "validate_duplicate_name:1"
    "validate_missing_table:1"
    "validate_no_fields:1"
    "validate_user_maml_unknown_column:1"
    "get_column_index_not_found:1"
)

echo "Building error_scenarios..."
if ! fpm build 2>&1 | tail -5; then
    echo "fpm build failed" >&2
    exit 1
fi

failures=0

for entry in "${scenarios[@]}"; do
    scenario="${entry%%:*}"
    expect_abort="${entry##*:}"

    fpm test error_scenarios -- "$scenario" > /dev/null 2>&1
    exitstat=$?

    if [ "$exitstat" -ne 0 ]; then
        aborted=1
    else
        aborted=0
    fi

    if [ "$aborted" -eq "$expect_abort" ]; then
        printf "[PASS] %-40s (exit=%d, expected abort=%s)\n" "$scenario" "$exitstat" "$expect_abort"
    else
        printf "[FAIL] %-40s (exit=%d, expected abort=%s)\n" "$scenario" "$exitstat" "$expect_abort"
        failures=$((failures + 1))
    fi
done

echo
if [ "$failures" -eq 0 ]; then
    echo "All error scenarios behaved as expected."
    exit 0
else
    echo "$failures scenario(s) did not behave as expected."
    exit 1
fi
