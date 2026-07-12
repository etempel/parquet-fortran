#!/usr/bin/env bash
# Runs every scenario supported by the test/error_scenarios.f90 helper program
# and checks that each one aborts (or exits cleanly) as expected. This is the
# same check test_errors.f90 does from inside the test-drive suite, exposed
# here as a standalone script for manual/CI use without going through fpm's
# test-drive runner.
#
# Keep this list in sync with the `select case` in test/error_scenarios.f90 --
# it is intentionally a complete, independent mirror of every scenario name
# there (not a curated subset), since a script that silently only covers some
# scenarios would be misleading for the "quick manual check"/"CI step" use
# this is documented for in README.md.
set -uo pipefail

cd "$(dirname "$0")/.."

# scenario:expect_abort ("0" or "1")
scenarios=(
    "ok:0"
    "print_stat_smoke:0"
    "write_undeclared_column:1"
    "write_type_mismatch:1"
    "write_column_twice:1"
    "write_column_twice_no_schema:1"
    "write_maml_without_metadata:1"
    "write_protected_column_with_null:1"
    "write_values_not_divisible_by_col_size:1"
    "write_int64_to_int32_overflow:1"
    "write_float_to_int32_non_integral:1"
    "write_float_to_int32_out_of_range:1"
    "write_float_to_int64_non_integral:1"
    "write_float_to_int64_out_of_range:1"
    "write_string_matrix_exceeds_array_size:1"
    "write_string_exceeds_array_size:1"
    "write_unknown_compression:1"
    "validate_bad_data_type:1"
    "validate_excluded_date_type:1"
    "validate_excluded_timestamp_type:1"
    "validate_excluded_decimal_type:1"
    "validate_duplicate_name:1"
    "validate_missing_table:1"
    "validate_no_fields:1"
    "validate_user_maml_unknown_column:1"
    "validate_unknown_top_level_section:1"
    "validate_unknown_field_subkey:1"
    "validate_unknown_qc_subkey:1"
    "validate_protected_cols_unknown_name:1"
    "validate_col_map_unknown_internal:1"
    "validate_col_map_duplicate_internal:1"
    "validate_col_map_output_collision:1"
    "validate_col_map_output_not_declared:1"
    "validate_col_map_internal_also_in_fields:1"
    "validate_col_map_output_matches_other_field:1"
    "validate_qc_min_not_numeric:1"
    "validate_qc_min_non_integral_for_int32:1"
    "validate_qc_min_out_of_int32_range:1"
    "validate_qc_min_wrong_operator:1"
    "validate_qc_max_wrong_operator:1"
    "qc_maml_min_wrong_operator:1"
    "add_col_qc_min_reversed_operator:1"
    "add_col_qc_operator_without_value:1"
    "add_col_qc_bad_miss_value:1"
    "add_col_qc_too_many_fields:1"
    "add_col_qc_empty_column_name:1"
    "add_col_qc_duplicate_column:1"
    "get_col_qc_reversed_operator:1"
    "get_column_index_not_found:1"
    "get_field_name_index_too_low:1"
    "get_field_name_index_too_high:1"
    "read_column_with_nulls:1"
    "read_unsupported_physical_type:1"
    "prefetch_unknown_column:1"
    "filter_unknown_column:1"
    "filter_vector_column:1"
    "filter_malformed_rule:1"
    "filter_bad_numeric_value:1"
    "filter_unquoted_string_value:1"
    "filter_bad_boolean_value:1"
    "filter_bool_ordering_not_supported:1"
    "qc_range_violation_warns:0"
    "qc_null_violation_warns:0"
    "qc_range_violation_hard_aborts:1"
    "qc_null_violation_hard_aborts:1"
    "qc_miss_null_no_warning:0"
    "qc_existing_null_abort_unchanged:1"
    "qc_column_not_in_file:0"
    "qc_disabled_explicit_no_warning:0"
    "qc_maml_bad_miss_value:1"
    "qc_maml_duplicate_field:1"
    "qc_maml_missing_name:1"
    "qc_maml_unknown_subkey:1"
    "write_row_count_mismatch:1"
    "read_row_count_mismatch:1"
    "read_before_open:1"
    "write_before_open:1"
    "get_nrows_before_open:1"
    "close_reader_before_open:1"
    "close_writer_before_open:1"
    "close_writer_missing_write:1"
    "read_unknown_column:1"
    "open_reader_missing_file:1"
    "open_reader_nrows_zero_rows:1"
    "open_writer_bad_path:1"
    "set_max_threads_below_one:1"
    "qc_warning_numeric:0"
    "qc_warning_string:0"
    "qc_silently_ignored_for_boolean:0"
    "get_metadata_missing_key_no_default:1"
    "get_metadata_conversion_failure_no_default:1"
    "schema_add_field_before_init:1"
    "schema_init_twice:1"
    "schema_init_empty_table:1"
    "schema_add_field_empty_name:1"
    "schema_add_field_duplicate_name:1"
    "schema_add_field_invalid_data_type:1"
    "schema_add_field_qc_min_reversed_operator:1"
    "schema_add_field_qc_max_reversed_operator:1"
    "schema_add_field_qc_operator_without_value:1"
    "schema_add_field_bad_qc_miss_value:1"
)

# concurrent_calls_into_shared_reader/writer are deliberately not in the list
# above: they only reliably trigger under genuine multi-threaded execution (a
# real OpenMP flag supplied via FPM_FFLAGS -- see README's "Thread safety"
# section), which this script does not assume. When OpenMP is not active the
# scenarios detect that (omp_get_max_threads() <= 1) and skip cleanly. They are
# run separately below, best-effort, and do not count towards failures whether
# they abort (guard fired) or skip (no real concurrency available).
concurrency_scenarios=(
    "concurrent_calls_into_shared_reader"
    "concurrent_calls_into_shared_writer"
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

    # exit code 97 means error_scenarios.f90's `case default` was hit --
    # i.e. this scenario name isn't recognized there (typo, or renamed on
    # one side but not the other) -- distinct from a genuine abort so this
    # doesn't silently "pass" by coincidence.
    if [ "$exitstat" -eq 97 ]; then
        printf "[FAIL] %-50s scenario name not recognized by error_scenarios.f90 (typo?)\n" "$scenario"
        failures=$((failures + 1))
        continue
    fi

    if [ "$exitstat" -ne 0 ]; then
        aborted=1
    else
        aborted=0
    fi

    if [ "$aborted" -eq "$expect_abort" ]; then
        printf "[PASS] %-50s (exit=%d, expected abort=%s)\n" "$scenario" "$exitstat" "$expect_abort"
    else
        printf "[FAIL] %-50s (exit=%d, expected abort=%s)\n" "$scenario" "$exitstat" "$expect_abort"
        failures=$((failures + 1))
    fi
done

echo
echo "Concurrency scenarios (best-effort, need FPM_FFLAGS with a real OpenMP flag to reliably trigger):"
for scenario in "${concurrency_scenarios[@]}"; do
    if fpm test error_scenarios -- "$scenario" > /dev/null 2>&1; then
        printf "[INFO] %-50s did not abort (skipped or single-threaded: needs genuine OpenMP concurrency)\n" "$scenario"
    else
        printf "[PASS] %-50s aborted as expected\n" "$scenario"
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
