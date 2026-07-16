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

# Worker mode: this script re-invokes itself (via xargs -P below) as
#   run_error_scenarios.sh __worker__ <bin> <results_dir> <idx> <scenario> <expect_abort>
# to run exactly one scenario and record its PASS/FAIL line + status to
# "$results_dir/$idx.{status,line}". This lets the scenario list further down
# be dispatched across several worker processes in parallel via xargs -P
# instead of one `fpm test error_scenarios -- ...` invocation at a time --
# each scenario is an independent process writing to its own uniquely-named
# file under test_run/, so there is no cross-scenario interference. Kept as a
# re-invocation of this same file (rather than a separate helper script or an
# exported shell function) so it works unchanged under the old bash (3.2, no
# `export -f`/`wait -n`) macOS ships as /bin/bash.
if [ "${1:-}" = "__worker__" ]; then
    bin="$2"
    results_dir="$3"
    idx="$4"
    scenario="$5"
    expect_abort="$6"

    "$bin" "$scenario" > /dev/null 2>&1
    exitstat=$?

    # exit code 97 means error_scenarios.f90's `case default` was hit --
    # i.e. this scenario name isn't recognized there (typo, or renamed on
    # one side but not the other) -- distinct from a genuine abort so this
    # doesn't silently "pass" by coincidence.
    if [ "$exitstat" -eq 97 ]; then
        status=FAIL
        line=$(printf "[FAIL] %-50s scenario name not recognized by error_scenarios.f90 (typo?)" "$scenario")
    else
        if [ "$exitstat" -ne 0 ]; then aborted=1; else aborted=0; fi
        if [ "$aborted" -eq "$expect_abort" ]; then
            status=PASS
            line=$(printf "[PASS] %-50s (exit=%d, expected abort=%s)" "$scenario" "$exitstat" "$expect_abort")
        else
            status=FAIL
            line=$(printf "[FAIL] %-50s (exit=%d, expected abort=%s)" "$scenario" "$exitstat" "$expect_abort")
        fi
    fi
    echo "$status" > "$results_dir/$idx.status"
    echo "$line" > "$results_dir/$idx.line"
    exit 0
fi

# scenario:expect_abort ("0" or "1")
scenarios=(
    "ok:0"
    "print_stat_smoke:0"
    "write_undeclared_column:1"
    "write_undeclared_column_int64:1"
    "write_undeclared_column_float32:1"
    "write_undeclared_column_float64:1"
    "write_undeclared_column_logical:1"
    "write_undeclared_column_string:1"
    "write_undeclared_column_int32_matrix:1"
    "write_undeclared_column_int64_matrix:1"
    "write_undeclared_column_float32_matrix:1"
    "write_undeclared_column_float64_matrix:1"
    "write_undeclared_column_logical_matrix:1"
    "write_undeclared_column_string_matrix:1"
    "write_not_divisible_int64:1"
    "write_not_divisible_float32:1"
    "write_not_divisible_float64:1"
    "write_not_divisible_logical:1"
    "write_not_divisible_string:1"
    "write_array_mismatch_int32_matrix:1"
    "write_array_mismatch_int64_matrix:1"
    "write_array_mismatch_float32_matrix:1"
    "write_array_mismatch_float64_matrix:1"
    "write_array_mismatch_logical_matrix:1"
    "write_array_mismatch_string_matrix:1"
    "write_chunk_undeclared_column_int32:1"
    "write_chunk_undeclared_column_int64:1"
    "write_chunk_undeclared_column_float32:1"
    "write_chunk_undeclared_column_float64:1"
    "write_chunk_undeclared_column_logical:1"
    "write_chunk_undeclared_column_string:1"
    "write_chunk_undeclared_column_int32_matrix:1"
    "write_chunk_undeclared_column_int64_matrix:1"
    "write_chunk_undeclared_column_float32_matrix:1"
    "write_chunk_undeclared_column_float64_matrix:1"
    "write_chunk_undeclared_column_logical_matrix:1"
    "write_chunk_undeclared_column_string_matrix:1"
    "write_chunk_not_divisible_int32:1"
    "write_chunk_not_divisible_int64:1"
    "write_chunk_not_divisible_float32:1"
    "write_chunk_not_divisible_float64:1"
    "write_chunk_not_divisible_logical:1"
    "write_chunk_not_divisible_string:1"
    "write_chunk_array_mismatch_int32_matrix:1"
    "write_chunk_array_mismatch_int64_matrix:1"
    "write_chunk_array_mismatch_float32_matrix:1"
    "write_chunk_array_mismatch_float64_matrix:1"
    "write_chunk_array_mismatch_logical_matrix:1"
    "write_chunk_array_mismatch_string_matrix:1"
    "write_chunk_string_matrix_exceeds_array_size:1"
    "write_chunk_string_exceeds_array_size:1"
    "write_chunk_no_row_group_open:1"
    "write_chunk_row_count_mismatch:1"
    "write_chunk_type_mismatch:1"
    "read_chunk_with_filter:1"
    "read_chunk_qc_hard_aborts:1"
    "read_chunk_qc_soft_warns:0"
    "read_chunk_check_complete_hard_aborts:1"
    "read_chunk_row_group_out_of_range:1"
    "get_chunk_size_row_group_out_of_range:1"
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
    "write_overwrite_false_existing_file:1"
    "validate_bad_data_type:1"
    "validate_excluded_date_type:1"
    "validate_excluded_timestamp_type:1"
    "validate_excluded_decimal_type:1"
    "validate_empty_field_name:1"
    "validate_duplicate_name:1"
    "validate_missing_table:1"
    "validate_no_fields:1"
    "validate_trailing_depends_no_fields:1"
    "validate_trailing_keywords_no_fields:1"
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
    "set_column_available_deactivated:1"
    "set_column_unavailable_deactivated:1"
    "validate_qc_min_not_numeric:1"
    "validate_qc_max_not_numeric:1"
    "validate_qc_min_non_integral_for_int32:1"
    "validate_qc_min_out_of_int32_range:1"
    "validate_qc_min_wrong_operator:1"
    "validate_qc_max_wrong_operator:1"
    "qc_maml_min_wrong_operator:1"
    "qc_maml_max_wrong_operator:1"
    "add_col_qc_min_reversed_operator:1"
    "add_col_qc_max_reversed_operator:1"
    "add_col_qc_operator_without_value:1"
    "add_col_qc_max_operator_without_value:1"
    "add_col_qc_bad_miss_value:1"
    "add_col_qc_too_many_fields:1"
    "add_col_qc_empty_column_name:1"
    "add_col_qc_duplicate_column:1"
    "add_col_qc_duplicate_column_single_quoted:1"
    "add_col_qc_duplicate_column_double_quoted:1"
    "set_col_qc_reversed_operator:1"
    "get_column_index_not_found:1"
    "get_field_name_index_too_low:1"
    "get_field_name_index_too_high:1"
    "read_column_with_nulls:1"
    "read_unsupported_physical_type:1"
    "col_size_overflow:1"
    "col_size_and_row_mode_avoid_whole_column_read:0"
    "whole_column_read_forced_error_control:1"
    "list_element_count_explicit_chunk_size_overflow:1"
    "row_group_explicit_nrows_overflow:1"
    "row_group_dangling_at_close:1"
    "row_group_whole_column_undercovered:1"
    "row_group_whole_column_overrun:1"
    "row_group_new_column_after_first:1"
    "column_count_overflow:1"
    "prefetch_unknown_column:1"
    "filter_unknown_column:1"
    "filter_vector_column:1"
    "filter_malformed_rule:1"
    "filter_rule_too_long:1"
    "filter_bad_numeric_value:1"
    "filter_unquoted_string_value:1"
    "filter_bad_boolean_value:1"
    "filter_bool_ordering_not_supported:1"
    "qc_range_violation_warns:0"
    "qc_maml_stray_no_colon_line:0"
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
    "close_writer_missing_write_unnamed_schema:1"
    "read_unknown_column:1"
    "open_reader_missing_file:1"
    "open_reader_nrows_zero_rows:1"
    "open_writer_bad_path:1"
    "set_max_threads_below_one:1"
    "qc_warning_numeric:0"
    "qc_warning_fractional_bound:0"
    "qc_warning_string:0"
    "qc_silently_ignored_for_boolean:0"
    "get_metadata_missing_key_no_default:1"
    "get_metadata_conversion_failure_no_default:1"
    "get_metadata_missing_int64_no_default:1"
    "get_metadata_missing_float32_no_default:1"
    "get_metadata_missing_float64_no_default:1"
    "get_metadata_missing_logical_no_default:1"
    "get_metadata_missing_string_no_default:1"
    "get_metadata_conversion_int64_no_default:1"
    "get_metadata_conversion_float32_no_default:1"
    "get_metadata_conversion_float64_no_default:1"
    "get_metadata_conversion_logical_no_default:1"
    "get_metadata_missing_int32_array_no_default:1"
    "get_metadata_conversion_int32_array_no_default:1"
    "get_metadata_missing_int64_array_no_default:1"
    "get_metadata_conversion_int64_array_no_default:1"
    "get_metadata_missing_float32_array_no_default:1"
    "get_metadata_conversion_float32_array_no_default:1"
    "get_metadata_missing_float64_array_no_default:1"
    "get_metadata_conversion_float64_array_no_default:1"
    "get_metadata_missing_logical_array_no_default:1"
    "get_metadata_conversion_logical_array_no_default:1"
    "get_metadata_missing_string_array_no_default:1"
    "schema_add_field_before_init:1"
    "schema_init_twice:1"
    "schema_init_empty_table:1"
    "schema_add_field_empty_name:1"
    "schema_add_field_duplicate_name:1"
    "schema_add_field_invalid_data_type:1"
    "schema_add_field_qc_min_reversed_operator:1"
    "schema_add_field_qc_max_reversed_operator:1"
    "schema_add_field_qc_operator_without_value:1"
    "schema_add_field_qc_max_operator_without_value:1"
    "schema_add_field_bad_qc_miss_value:1"
    "string_column_index_out_of_range:1"
    "string_column_get_null:1"
    "string_column_to_character_null:1"
    "string_handle_unassociated:1"
    "string_handle_stale_index:1"
    "string_column_append_buffers_offset_not_zero:1"
)

# concurrent_calls_into_shared_reader/writer are deliberately not in the list
# above: they only reliably trigger under genuine multi-threaded execution (a
# real OpenMP flag supplied via FPM_FFLAGS -- see README's "Thread safety"
# section), which this script does not assume. When OpenMP is not active the
# scenarios detect that (omp_get_max_threads() <= 1) and skip cleanly. They are
# run separately below, sequentially (not part of the parallel batch -- they
# specifically probe OpenMP thread contention, and running them alongside a
# pool of other concurrent processes would only add timing noise), and do not
# count towards failures whether they abort (guard fired) or skip (no real
# concurrency available).
concurrency_scenarios=(
    "concurrent_calls_into_shared_reader"
    "concurrent_calls_into_shared_writer"
)

echo "Building error_scenarios..."
if ! fpm build --tests 2>&1 | tail -5; then
    echo "fpm build failed" >&2
    exit 1
fi

bin="$(find "${FPM_BUILD_DIR:-build}" -type f -name error_scenarios | head -n 1)"
if [ -z "$bin" ]; then
    echo "could not locate the built error_scenarios binary under build/" >&2
    exit 1
fi

mkdir -p test_run
results_dir="$(mktemp -d test_run/run_error_scenarios_results.XXXXXX)"
trap 'rm -rf "$results_dir"' EXIT

# Number of scenarios to run concurrently -- override with
# RUN_ERROR_SCENARIOS_JOBS=N if the default (one per logical CPU) is too
# aggressive for a given machine.
jobs_n="${RUN_ERROR_SCENARIOS_JOBS:-$(getconf _NPROCESSORS_ONLN 2>/dev/null || echo 4)}"

idx=0
tokens=()
for entry in "${scenarios[@]}"; do
    scenario="${entry%%:*}"
    expect_abort="${entry##*:}"
    idx=$((idx + 1))
    tokens+=("$idx" "$scenario" "$expect_abort")
done

printf '%s\n' "${tokens[@]}" | xargs -P "$jobs_n" -n 3 "$0" __worker__ "$bin" "$results_dir"

failures=0
i=1
while [ "$i" -le "$idx" ]; do
    echo "$(cat "$results_dir/$i.line")"
    if [ "$(cat "$results_dir/$i.status")" = "FAIL" ]; then
        failures=$((failures + 1))
    fi
    i=$((i + 1))
done

echo
echo "Concurrency scenarios (best-effort, need FPM_FFLAGS with a real OpenMP flag to reliably trigger):"
for scenario in "${concurrency_scenarios[@]}"; do
    if "$bin" "$scenario" > /dev/null 2>&1; then
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
