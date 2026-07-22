!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests that the library correctly aborts (via Fortran `error stop`) on
!> invalid input. Because `error stop` terminates the whole process, each
!> scenario is run in a subprocess (the "error_scenarios" helper test
!> program) and only the process exit status is observed here.
!> NB: always pass the failure message directly to `check(error, cond, message)`.
!> Do NOT follow a failed `check` with a separate `test_failed` call on the same
!> `error` variable: `check` already allocates `error` internally when `cond` is
!> false, and passing that already-allocated `error` into another `intent(out)`
!> argument (as `test_failed` also expects) triggers Fortran's automatic
!> finalization of the old value before reassignment, which for test-drive's
!> `error_type` calls its FINAL `escalate_error` and aborts the whole process.
module test_errors
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !$ use omp_lib, only : omp_get_max_threads
    !
    implicit none
    private
    public :: collect_tests_parquet_errors
    ! Exposed for test_writing.f90 to reuse (avoids a second copy of the same
    ! subprocess-driving helper -- see check_scenario_exit_status below).
    public :: check_scenario_exit_status
    public :: check_scenario_exit_status_and_stderr
    public :: check_scenario_exit_status_and_no_output
    ! Exposed for test_writing.f90's own qc-warning scenario helpers -- see
    ! run_error_scenario below.
    public :: run_error_scenario
    !
    ! Cached path to the built error_scenarios helper binary -- see
    ! error_scenarios_bin below.
    character(len=:), allocatable, save :: g_error_scenarios_bin
    logical, save :: g_error_scenarios_bin_ready = .false.
    !
contains
    !
    subroutine collect_tests_parquet_errors(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("control scenario exits cleanly", test_ok_scenario_exits_cleanly), &
            new_unittest("write to undeclared column aborts", test_write_undeclared_column_aborts), &
            new_unittest("writing an undeclared int64 column aborts", &
                test_write_undeclared_column_int64_aborts), &
            new_unittest("writing an undeclared float32 column aborts", &
                test_write_undeclared_column_float32_aborts), &
            new_unittest("writing an undeclared float64 column aborts", &
                test_write_undeclared_column_float64_aborts), &
            new_unittest("writing an undeclared logical column aborts", &
                test_write_undeclared_column_logical_aborts), &
            new_unittest("writing an undeclared string column aborts", &
                test_write_undeclared_column_string_aborts), &
            new_unittest("writing an undeclared column via a compact string write aborts", &
                test_write_undeclared_column_string_compact_aborts), &
            new_unittest("writing an undeclared int32 matrix column aborts", &
                test_write_undeclared_column_int32_matrix_aborts), &
            new_unittest("writing an undeclared int64 matrix column aborts", &
                test_write_undeclared_column_int64_matrix_aborts), &
            new_unittest("writing an undeclared float32 matrix column aborts", &
                test_write_undeclared_column_float32_matrix_aborts), &
            new_unittest("writing an undeclared float64 matrix column aborts", &
                test_write_undeclared_column_float64_matrix_aborts), &
            new_unittest("writing an undeclared logical matrix column aborts", &
                test_write_undeclared_column_logical_matrix_aborts), &
            new_unittest("writing an undeclared string matrix column aborts", &
                test_write_undeclared_column_string_matrix_aborts), &
            new_unittest("writing a int64 values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_int64_aborts), &
            new_unittest("writing a float32 values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_float32_aborts), &
            new_unittest("writing a float64 values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_float64_aborts), &
            new_unittest("writing a logical values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_logical_aborts), &
            new_unittest("writing a string values(:) array not divisible by col_size aborts", &
                test_write_not_divisible_string_aborts), &
            new_unittest("writing a int32 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_int32_matrix_aborts), &
            new_unittest("writing a int64 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_int64_matrix_aborts), &
            new_unittest("writing a float32 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_float32_matrix_aborts), &
            new_unittest("writing a float64 matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_float64_matrix_aborts), &
            new_unittest("writing a logical matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_logical_matrix_aborts), &
            new_unittest("writing a string matrix with a col_size mismatch aborts", &
                test_write_array_mismatch_string_matrix_aborts), &
            new_unittest("streaming: writing an undeclared int32 chunk column aborts", &
                test_write_chunk_undeclared_column_int32_aborts), &
            new_unittest("streaming: writing an undeclared int64 chunk column aborts", &
                test_write_chunk_undeclared_column_int64_aborts), &
            new_unittest("streaming: writing an undeclared float32 chunk column aborts", &
                test_write_chunk_undeclared_column_float32_aborts), &
            new_unittest("streaming: writing an undeclared float64 chunk column aborts", &
                test_write_chunk_undeclared_column_float64_aborts), &
            new_unittest("streaming: writing an undeclared logical chunk column aborts", &
                test_write_chunk_undeclared_column_logical_aborts), &
            new_unittest("streaming: writing an undeclared string chunk column aborts", &
                test_write_chunk_undeclared_column_string_aborts), &
            new_unittest("streaming: writing an undeclared column via a compact string chunk write aborts", &
                test_write_chunk_undeclared_column_string_compact_aborts), &
            new_unittest("streaming: writing an undeclared int32 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_int32_matrix_aborts), &
            new_unittest("streaming: writing an undeclared int64 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_int64_matrix_aborts), &
            new_unittest("streaming: writing an undeclared float32 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_float32_matrix_aborts), &
            new_unittest("streaming: writing an undeclared float64 matrix chunk column aborts", &
                test_write_chunk_undeclared_column_float64_matrix_aborts), &
            new_unittest("streaming: writing an undeclared logical matrix chunk column aborts", &
                test_write_chunk_undeclared_column_logical_matrix_aborts), &
            new_unittest("streaming: writing an undeclared string matrix chunk column aborts", &
                test_write_chunk_undeclared_column_string_matrix_aborts), &
            new_unittest("streaming: writing a int32 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_int32_aborts), &
            new_unittest("streaming: writing a int64 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_int64_aborts), &
            new_unittest("streaming: writing a float32 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_float32_aborts), &
            new_unittest("streaming: writing a float64 chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_float64_aborts), &
            new_unittest("streaming: writing a logical chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_logical_aborts), &
            new_unittest("streaming: writing a string chunk values(:) array not divisible by col_size aborts", &
                test_write_chunk_not_divisible_string_aborts), &
            new_unittest("streaming: writing a int32 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_int32_matrix_aborts), &
            new_unittest("streaming: writing a int64 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_int64_matrix_aborts), &
            new_unittest("streaming: writing a float32 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_float32_matrix_aborts), &
            new_unittest("streaming: writing a float64 chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_float64_matrix_aborts), &
            new_unittest("streaming: writing a logical chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_logical_matrix_aborts), &
            new_unittest("streaming: writing a string chunk matrix with a col_size mismatch aborts", &
                test_write_chunk_array_mismatch_string_matrix_aborts), &
            new_unittest("streaming: writing an over-length string into a fixed-size string matrix chunk " // &
                "column aborts", test_write_chunk_string_matrix_exceeds_array_size_aborts), &
            new_unittest("streaming: writing an over-length string into a fixed-size string vector chunk " // &
                "column (flat form) aborts", test_write_chunk_string_exceeds_array_size_aborts), &
            new_unittest("streaming: writing a chunk column before parquet_new_row_group aborts", &
                test_write_chunk_no_row_group_open_aborts), &
            new_unittest("streaming: writing a chunk whose row count doesn't match the open row group aborts", &
                test_write_chunk_row_count_mismatch_aborts), &
            new_unittest("streaming: writing a chunk with type mismatch aborts", &
                test_write_chunk_type_mismatch_aborts), &
            new_unittest("chunked read: parquet_read_column_chunk on a filtered reader aborts", &
                test_read_chunk_with_filter_aborts), &
            new_unittest("chunked read: hard qc violation in one row group aborts, naming that row group", &
                test_read_chunk_qc_hard_aborts), &
            new_unittest("chunked read: soft qc violation warns once per column across multiple row groups", &
                test_read_chunk_qc_soft_warns), &
            new_unittest("chunked read: check_complete=.true. aborts on a column missing a row group", &
                test_read_chunk_check_complete_hard_aborts), &
            new_unittest("chunked read: an out-of-range row_group aborts", &
                test_read_chunk_row_group_out_of_range_aborts), &
            new_unittest("parquet_get_chunk_size with an out-of-range row_group aborts", &
                test_get_chunk_size_row_group_out_of_range_aborts), &
            new_unittest("write with type mismatch aborts", test_write_type_mismatch_aborts), &
            new_unittest("writing the same column twice aborts", test_write_column_twice_aborts), &
            new_unittest("writing the same column twice on a schema-less writer aborts", &
                test_write_column_twice_no_schema_aborts), &
            new_unittest("validating an invalid data_type aborts", test_validate_bad_data_type_aborts), &
            new_unittest("validating data_type: decimal aborts", test_validate_excluded_decimal_type_aborts), &
            new_unittest("validating a fields: entry with no name: sub-key aborts", &
                test_validate_empty_field_name_aborts), &
            new_unittest("validating a duplicate field name aborts", test_validate_duplicate_name_aborts), &
            new_unittest("validating a MAML without table: aborts", test_validate_missing_table_aborts), &
            new_unittest("validating a MAML without fields aborts", test_validate_no_fields_aborts), &
            new_unittest("a MAML with a trailing, unclosed depends: list and no fields: aborts", &
                test_validate_trailing_depends_no_fields_aborts), &
            new_unittest("a MAML with a trailing, unclosed keywords: list and no fields: aborts", &
                test_validate_trailing_keywords_no_fields_aborts), &
            new_unittest("validating a MAML with an unknown top-level section aborts", &
                test_validate_unknown_top_level_section_aborts), &
            new_unittest("validating a MAML with an unknown field sub-key aborts", &
                test_validate_unknown_field_subkey_aborts), &
            new_unittest("validating a MAML with an unknown qc: sub-key aborts", &
                test_validate_unknown_qc_subkey_aborts), &
            new_unittest("user MAML with unknown column aborts", test_validate_user_maml_unknown_column_aborts), &
            new_unittest("col_map: referencing an unknown internal column aborts", &
                test_validate_col_map_unknown_internal_aborts), &
            new_unittest("col_map: duplicate internal column key aborts", &
                test_validate_col_map_duplicate_internal_aborts), &
            new_unittest("col_map: two internal columns colliding on the same output name aborts", &
                test_validate_col_map_output_collision_aborts), &
            new_unittest("col_map: renamed output not declared in fields: aborts", &
                test_validate_col_map_output_not_declared_aborts), &
            new_unittest("col_map: remapped internal column also declared un-renamed in fields: aborts", &
                test_validate_col_map_internal_also_in_fields_aborts), &
            new_unittest("col_map: output name coincides with an existing base column aborts", &
                test_validate_col_map_output_matches_other_field_aborts), &
            new_unittest("set_column_available on a deactivated column aborts", &
                test_set_column_available_deactivated_aborts), &
            new_unittest("set_column_unavailable on a deactivated column aborts", &
                test_set_column_unavailable_deactivated_aborts), &
            new_unittest("get_column_index on unknown column aborts", test_get_column_index_not_found_aborts), &
            new_unittest("get_field_name with an index below 1 aborts", test_get_field_name_index_too_low_aborts), &
            new_unittest("get_field_name with an index past get_num_fields aborts", &
                test_get_field_name_index_too_high_aborts), &
            new_unittest("write_maml without metadata aborts", test_write_maml_without_metadata_aborts), &
            new_unittest("reading a column with genuine Null values aborts", &
                test_read_column_with_nulls_aborts), &
            new_unittest("reading a column of an unsupported physical type aborts", &
                test_read_unsupported_physical_type_aborts), &
            new_unittest("array-mode whole-column read of an int32 vector column as logical aborts", &
                test_read_array_full_bool_type_mismatch_aborts), &
            new_unittest("array-mode whole-column read of an int32 vector column as string aborts", &
                test_read_array_full_string_type_mismatch_aborts), &
            new_unittest("array-mode row_mode read of an int32 vector column as logical aborts", &
                test_read_array_row_mode_bool_type_mismatch_aborts), &
            new_unittest("array-mode row_mode read of an int32 vector column as string aborts", &
                test_read_array_row_mode_string_type_mismatch_aborts), &
            new_unittest("array-mode element_mode (filtered) with an out-of-range col_index (logical) aborts", &
                test_read_array_em_filt_bool_oob_aborts), &
            new_unittest("array-mode element_mode (filtered) read of an int32 vector column as logical aborts", &
                test_read_array_em_filt_bool_tm_aborts), &
            new_unittest("array-mode element_mode (filtered) with an out-of-range col_index (string) aborts", &
                test_read_array_em_filt_string_oob_aborts), &
            new_unittest("array-mode element_mode (filtered) read of an int32 vector column as string aborts", &
                test_read_array_em_filt_string_tm_aborts), &
            new_unittest("array-mode element_mode (unfiltered) with an out-of-range col_index (logical) aborts", &
                test_read_array_em_bool_oob_aborts), &
            new_unittest("array-mode element_mode (unfiltered) read of an int32 vector column as logical aborts", &
                test_read_array_em_bool_tm_aborts), &
            new_unittest("array-mode element_mode (unfiltered) with an out-of-range col_index (string) aborts", &
                test_read_array_em_string_oob_aborts), &
            new_unittest("array-mode element_mode (unfiltered) read of an int32 vector column as string aborts", &
                test_read_array_em_string_tm_aborts), &
            new_unittest("array-mode chunk read of an int32 vector column as logical aborts", &
                test_read_array_column_chunk_bool_type_mismatch_aborts), &
            new_unittest("array-mode chunk read of an int32 vector column as string aborts", &
                test_read_array_column_chunk_string_type_mismatch_aborts), &
            new_unittest("array-mode element_mode (filtered) with an out-of-range col_index (int32) aborts", &
                test_read_array_em_filt_int32_oob_aborts), &
            new_unittest("array-mode element_mode (unfiltered) with an out-of-range col_index (int32) aborts", &
                test_read_array_em_int32_oob_aborts), &
            new_unittest("reading an out-of-int32-range uint32 value aborts", &
                test_extended_uint32_overflow_int32_aborts), &
            new_unittest("reading an out-of-int32-range uint64 value aborts", &
                test_extended_uint64_overflow_int32_aborts), &
            new_unittest("reading an out-of-int64-range uint64 value aborts", &
                test_extended_uint64_overflow_int64_aborts), &
            new_unittest("reading a non-integral double value into int32 aborts", &
                test_extended_real_nonintegral_int32_aborts), &
            new_unittest("reading an out-of-int32-range double value aborts", &
                test_extended_real_overflow_int32_aborts), &
            new_unittest("reading a non-integral double value into int64 aborts", &
                test_extended_real_nonintegral_int64_aborts), &
            new_unittest("reading an out-of-int64-range double value aborts", &
                test_extended_real_overflow_int64_aborts), &
            new_unittest("reading a non-integral decimal value into int32 aborts", &
                test_extended_decimal_nonintegral_int32_aborts), &
            new_unittest("reading an out-of-int32-range decimal value aborts", &
                test_extended_decimal_overflow_int32_aborts), &
            new_unittest("reading a non-integral decimal value into int64 aborts", &
                test_extended_decimal_nonintegral_int64_aborts), &
            new_unittest("reading an out-of-int64-range decimal value aborts", &
                test_extended_decimal_overflow_int64_aborts), &
            new_unittest("writing a vector column with col_size exceeding Arrow's FixedSizeListType limit aborts", &
                test_write_col_size_overflow_aborts), &
            new_unittest("writing a vector column with an explicit chunk_size*col_size exceeding Arrow/Parquet's " // &
                "list-element-count limit aborts", test_write_list_element_count_chunk_size_overflow_aborts), &
            new_unittest("parquet_new_row_group with an explicit nrows*col_size exceeding Arrow/Parquet's " // &
                "list-element-count limit aborts", test_row_group_explicit_nrows_overflow_aborts), &
            new_unittest("closing a writer with a dangling open row group aborts", &
                test_row_group_dangling_at_close_aborts), &
            new_unittest("closing a writer with an under-covered whole column aborts", &
                test_row_group_whole_column_undercovered_aborts), &
            new_unittest("a row group reading past a whole column's own row count aborts", &
                test_row_group_whole_column_overrun_aborts), &
            new_unittest("introducing a new column after the first row group aborts", &
                test_row_group_new_column_after_first_aborts), &
            new_unittest("a whole-column write after the streaming row-group API already started aborts", &
                test_row_group_whole_column_after_streaming_started_aborts), &
            new_unittest("starting a new row group while one is already open aborts", &
                test_row_group_started_while_open_aborts), &
            new_unittest("writing a table with column count exceeding Arrow's Schema field-count limit aborts", &
                test_write_column_count_overflow_aborts), &
            new_unittest("prefetching an unknown column aborts", &
                test_prefetch_unknown_column_aborts), &
            new_unittest("filter: unknown column aborts", &
                test_filter_unknown_column_aborts), &
            new_unittest("filter: vector column aborts", &
                test_filter_vector_column_aborts), &
            new_unittest("filter: malformed rule aborts", &
                test_filter_malformed_rule_aborts), &
            new_unittest("filter: rule longer than 512 characters aborts", &
                test_filter_rule_too_long_aborts), &
            new_unittest("filter: non-numeric value against a numeric column aborts", &
                test_filter_bad_numeric_value_aborts), &
            new_unittest("filter: int32 value out of range aborts", &
                test_filter_int32_value_out_of_range_aborts), &
            new_unittest("filter: non-numeric value against a float column aborts", &
                test_filter_bad_numeric_value_float_aborts), &
            new_unittest("filter: unquoted value against a string column aborts", &
                test_filter_unquoted_string_value_aborts), &
            new_unittest("filter: invalid boolean value aborts", &
                test_filter_bad_boolean_value_aborts), &
            new_unittest("filter: a double-quoted boolean value aborts", &
                test_filter_boolean_value_must_be_unquoted_aborts), &
            new_unittest("filter: ordering comparison against a boolean column aborts", &
                test_filter_bool_ordering_not_supported_aborts), &
            new_unittest("filter: filtering an unsupported (temporal) column type aborts", &
                test_filter_unsupported_column_type_aborts), &
            new_unittest("sample_fraction: negative value aborts", &
                test_sample_negative_fraction_aborts), &
            new_unittest("sample_fraction: NaN value aborts", &
                test_sample_nan_fraction_aborts), &
            new_unittest("chunked read on a reader opened with sample_fraction (no filter=) aborts", &
                test_read_chunk_with_sample_aborts), &
            new_unittest("print_stat reports the sample: fraction=.../seed=... line", &
                test_print_stat_sampled_rows), &
            new_unittest("a forced sample-mask-build failure aborts via a clean error stop", &
                test_sample_mask_build_error_aborts), &
            new_unittest("string length query on a non-string column aborts", &
                test_string_length_on_non_string_column_aborts), &
            new_unittest("qc-maml: unrecognized miss: value aborts", &
                test_qc_maml_bad_miss_value_aborts), &
            new_unittest("qc-maml: duplicate field name aborts", &
                test_qc_maml_duplicate_field_aborts), &
            new_unittest("qc-maml: field missing name aborts", &
                test_qc_maml_missing_name_aborts), &
            new_unittest("qc-maml: unknown qc: sub-key aborts", &
                test_qc_maml_unknown_subkey_aborts), &
            new_unittest("qc: existing Null-abort behavior is unchanged", &
                test_qc_existing_null_abort_unchanged_aborts), &
            new_unittest("qc: hard mode (default) aborts on a range violation", &
                test_qc_range_violation_hard_aborts), &
            new_unittest("qc: hard mode (default) aborts on a range violation (string column)", &
                test_qc_range_violation_string_hard_aborts), &
            new_unittest("qc: hard mode (default) aborts on a range violation (float column)", &
                test_qc_range_violation_float_hard_aborts), &
            new_unittest("qc: hard mode (default) aborts on an unexpected Null", &
                test_qc_null_violation_hard_aborts), &
            new_unittest("writing columns with mismatched row counts aborts", &
                test_write_row_count_mismatch_aborts), &
            new_unittest("reading a column into a wrong-size array aborts", &
                test_read_row_count_mismatch_aborts), &
            new_unittest("reading from an unopened reader aborts", &
                test_read_before_open_aborts), &
            new_unittest("writing to an unopened writer aborts", &
                test_write_before_open_aborts), &
            new_unittest("calling parquet_get_nrows on an unopened reader aborts", &
                test_get_nrows_before_open_aborts), &
            new_unittest("closing a never-opened reader aborts", &
                test_close_reader_before_open_aborts), &
            new_unittest("closing a never-opened writer aborts", &
                test_close_writer_before_open_aborts), &
            new_unittest("closing a writer with an unwritten enabled column aborts", &
                test_close_writer_missing_write_aborts), &
            new_unittest("closing a hand-built-schema writer with an unwritten enabled column " // &
                "aborts with an unnamed-schema message", test_close_writer_missing_write_unnamed_schema_aborts), &
            new_unittest("reading an unknown column via parquet_read_column aborts", &
                test_read_unknown_column_aborts), &
            new_unittest("reading a nested struct-field path with a mid-path typo aborts", &
                test_read_nested_struct_field_not_found_aborts), &
            new_unittest("reading a nested struct-field path through a non-struct segment aborts", &
                test_read_nested_struct_path_not_a_struct_aborts), &
            new_unittest("reading a nested struct-field path that resolves to an intermediate struct aborts", &
                test_read_nested_struct_intermediate_not_leaf_aborts), &
            new_unittest("reading two nested struct-field leaves under one struct shares one physical read", &
                test_nested_struct_shares_cached_read), &
            new_unittest("opening a nonexistent file for reading aborts", &
                test_open_reader_missing_file_aborts), &
            new_unittest("parquet_open_reader(nrows=) with a filter matching zero rows aborts", &
                test_open_reader_nrows_zero_rows_aborts), &
            new_unittest("opening a writer at a bad path aborts", &
                test_open_writer_bad_path_aborts), &
            new_unittest("writing an over-length string into a fixed-size string matrix column aborts", &
                test_write_string_matrix_exceeds_array_size_aborts), &
            new_unittest("writing an over-length string into a fixed-size string vector column (flat form) aborts", &
                test_write_string_exceeds_array_size_aborts), &
            new_unittest("protected_cols: referencing an unknown field aborts", &
                test_validate_protected_cols_unknown_name_aborts), &
            new_unittest("writing a Null into a protected column aborts", &
                test_write_protected_column_with_null_aborts), &
            new_unittest("qc: min value that does not parse as a number aborts", &
                test_validate_qc_min_not_numeric_aborts), &
            new_unittest("qc: max value that does not parse as a number aborts", &
                test_validate_qc_max_not_numeric_aborts), &
            new_unittest("qc: min value with a fractional part on an int32 field aborts", &
                test_validate_qc_min_non_integral_for_int32_aborts), &
            new_unittest("qc: min value out of int32 range aborts", &
                test_validate_qc_min_out_of_int32_range_aborts), &
            new_unittest("qc: min value with a reversed (</<=) operator aborts", &
                test_validate_qc_min_wrong_operator_aborts), &
            new_unittest("qc: max value with a reversed (>/>=) operator aborts", &
                test_validate_qc_max_wrong_operator_aborts), &
            new_unittest("qc-maml: min value with a reversed (</<=) operator aborts", &
                test_qc_maml_min_wrong_operator_aborts), &
            new_unittest("qc-maml: max value with a reversed (>/>=) operator aborts", &
                test_qc_maml_max_wrong_operator_aborts), &
            new_unittest("schema%add_field: qc_max operator with no value aborts", &
                test_schema_add_field_qc_max_operator_without_value_aborts), &
            new_unittest("add_col_qc: reversed min operator aborts", &
                test_add_col_qc_min_reversed_operator_aborts), &
            new_unittest("add_col_qc: reversed max operator aborts", &
                test_add_col_qc_max_reversed_operator_aborts), &
            new_unittest("add_col_qc: operator with no value aborts", &
                test_add_col_qc_operator_without_value_aborts), &
            new_unittest("add_col_qc: max operator with no value aborts", &
                test_add_col_qc_max_operator_without_value_aborts), &
            new_unittest("add_col_qc: invalid miss value aborts", &
                test_add_col_qc_bad_miss_value_aborts), &
            new_unittest("add_col_qc: more than four fields aborts", &
                test_add_col_qc_too_many_fields_aborts), &
            new_unittest("add_col_qc: empty column name aborts", &
                test_add_col_qc_empty_column_name_aborts), &
            new_unittest("add_col_qc: duplicate column name aborts", &
                test_add_col_qc_duplicate_column_aborts), &
            new_unittest("add_col_qc: duplicate column name (single-quoted existing entry) aborts", &
                test_add_col_qc_duplicate_column_single_quoted_aborts), &
            new_unittest("add_col_qc: duplicate column name (double-quoted existing entry) aborts", &
                test_add_col_qc_duplicate_column_double_quoted_aborts), &
            new_unittest("set_col_qc: reversed min operator aborts (shared validation)", &
                test_set_col_qc_reversed_operator_aborts), &
            new_unittest("writing values(:) not divisible by col_size aborts", &
                test_write_values_not_divisible_by_col_size_aborts), &
            new_unittest("writing an out-of-int32-range int64 value to an int32 schema column aborts", &
                test_write_int64_to_int32_overflow_aborts), &
            new_unittest("writing a non-integral float64 value to an int32 schema column aborts", &
                test_write_float_to_int32_non_integral_aborts), &
            new_unittest("writing an out-of-int32-range float64 value to an int32 schema column aborts", &
                test_write_float_to_int32_out_of_range_aborts), &
            new_unittest("writing a non-integral float64 value to an int64 schema column aborts", &
                test_write_float_to_int64_non_integral_aborts), &
            new_unittest("writing an out-of-int64-range float64 value to an int64 schema column aborts", &
                test_write_float_to_int64_out_of_range_aborts), &
            new_unittest("parquet_set_max_threads(0) aborts", &
                test_set_max_threads_below_one_aborts), &
            new_unittest("concurrent calls into a shared parquet_reader abort", &
                test_concurrent_calls_into_shared_reader_aborts), &
            new_unittest("concurrent calls into a shared parquet_writer abort", &
                test_concurrent_calls_into_shared_writer_aborts), &
            new_unittest("parquet_string_column indexing out of range aborts", &
                test_string_column_index_out_of_range_aborts), &
            new_unittest("parquet_string_column view_all with a mismatched-size array aborts", &
                test_string_column_view_all_size_mismatch_aborts), &
            new_unittest("parquet_string_column get on a null aborts", &
                test_string_column_get_null_aborts), &
            new_unittest("parquet_string_column to_character on a null aborts", &
                test_string_column_to_character_null_aborts), &
            new_unittest("parquet_string unassociated handle aborts", &
                test_string_handle_unassociated_aborts), &
            new_unittest("parquet_string stale handle index aborts", &
                test_string_handle_stale_index_aborts), &
            new_unittest("parquet_string set_null on an unassociated handle aborts", &
                test_string_set_null_unassociated_aborts), &
            new_unittest("parquet_string set_null on a stale handle aborts", &
                test_string_set_null_stale_index_aborts), &
            new_unittest("parquet_string_column slice with first > last aborts", &
                test_string_slice_invalid_range_aborts), &
            new_unittest("parquet_string_column view_slice with an out-of-range last aborts", &
                test_string_view_slice_invalid_range_aborts), &
            new_unittest("parquet_string_column view_slice with a mismatched-size array aborts", &
                test_string_view_slice_size_mismatch_aborts), &
            new_unittest("parquet_string_column build_from with a self-aliasing handle aborts", &
                test_string_build_from_self_alias_aborts), &
            new_unittest("parquet_string_column build_from with an unassociated handle aborts", &
                test_string_build_from_unassociated_aborts), &
            new_unittest("parquet_string_column build_from with a stale handle aborts", &
                test_string_build_from_stale_index_aborts), &
            new_unittest("parquet_string_column append_buffers with un-rebased offsets aborts", &
                test_string_column_append_buffers_offset_not_zero_aborts), &
            new_unittest("parquet_string_column append_buffers with un-rebased int32 offsets aborts", &
                test_string_column_append_buffers_offset_not_zero_int32_aborts), &
            new_unittest("compact string write into a vector (col_size>1) schema column aborts", &
                test_compact_string_write_requires_scalar_column_aborts), &
            new_unittest("compact string chunk write into a vector (col_size>1) schema column aborts", &
                test_compact_string_write_chunk_requires_scalar_column_aborts), &
            new_unittest("reading a STRING_VIEW column into a compact parquet_string_column aborts", &
                test_string_view_compact_read_unsupported_aborts), &
            new_unittest("parquet_date set with an invalid month aborts", &
                test_temporal_date_set_invalid_month_aborts), &
            new_unittest("parquet_date set with an invalid day aborts", &
                test_temporal_date_set_invalid_day_aborts), &
            new_unittest("parquet_date set beyond the representable range aborts", &
                test_temporal_date_set_out_of_range_aborts), &
            new_unittest("parquet_date set_mjd beyond the representable range aborts", &
                test_temporal_date_set_mjd_out_of_range_aborts), &
            new_unittest("parquet_date parse failure without success= aborts", &
                test_temporal_date_parse_invalid_aborts), &
            new_unittest("parquet_date get on a null element aborts", &
                test_temporal_date_null_get_aborts), &
            new_unittest("parquet_date comparison with a null element aborts", &
                test_temporal_date_null_comparison_aborts), &
            new_unittest("parquet_time set with an invalid hour aborts", &
                test_temporal_time_set_invalid_hour_aborts), &
            new_unittest("parquet_time set with an invalid nanosecond aborts", &
                test_temporal_time_set_invalid_nanosecond_aborts), &
            new_unittest("parquet_time set_raw outside a day aborts", &
                test_temporal_time_set_raw_out_of_range_aborts), &
            new_unittest("parquet_time parse failure without success= aborts", &
                test_temporal_time_parse_invalid_aborts), &
            new_unittest("parquet_time accessor on a null element aborts", &
                test_temporal_time_null_get_aborts), &
            new_unittest("parquet_timestamp set with an invalid day aborts", &
                test_temporal_ts_set_invalid_day_aborts), &
            new_unittest("parquet_timestamp parse failure without success= aborts", &
                test_temporal_ts_parse_invalid_aborts), &
            new_unittest("parquet_timestamp to_unix on a null element aborts", &
                test_temporal_ts_null_to_unix_aborts), &
            new_unittest("parquet_timestamp comparison with a null element aborts", &
                test_temporal_ts_null_comparison_aborts), &
            new_unittest("parquet_timestamp to_unix precision loss aborts by default", &
                test_temporal_ts_to_unix_precision_loss_aborts), &
            new_unittest("parquet_timestamp to_unix int64 overflow aborts", &
                test_temporal_ts_to_unix_overflow_aborts), &
            new_unittest("parquet_timestamp set_mjd beyond the representable range aborts", &
                test_temporal_ts_set_mjd_out_of_range_aborts), &
            new_unittest("parquet_timestamp set_raw with an invalid nanosecond part aborts", &
                test_temporal_ts_set_raw_invalid_nanoseconds_aborts), &
            new_unittest("an invalid time-unit selector aborts", &
                test_temporal_invalid_time_unit_aborts), &
            new_unittest("parquet_timestamp get with a year beyond int32 aborts", &
                test_temporal_ts_get_year_overflow_aborts), &
            new_unittest("writing a time value finer than its declared unit aborts", &
                test_temporal_write_time_precision_loss_aborts), &
            new_unittest("writing a Null into a protected timestamp column aborts", &
                test_temporal_protected_col_null_aborts), &
            new_unittest("reading a date column via the plain int32 reader aborts", &
                test_temporal_read_date_via_int32_aborts), &
            new_unittest("reading an int32 column via the date reader aborts", &
                test_temporal_read_int32_via_date_aborts), &
            new_unittest("parquet_date operator(==) with a null operand aborts", test_temporal_date_eq_null_aborts), &
            new_unittest("parquet_time get on a null element aborts", test_temporal_time_get_null_aborts), &
            new_unittest("parquet_time minute on a null element aborts", test_temporal_time_minute_null_aborts), &
            new_unittest("parquet_time second on a null element aborts", test_temporal_time_second_null_aborts), &
            new_unittest("parquet_time nanosecond on a null element aborts", &
                test_temporal_time_nanosecond_null_aborts), &
            new_unittest("parquet_time operator(==) with a null operand aborts", test_temporal_time_eq_null_aborts), &
            new_unittest("parquet_time operator(<) with a null operand aborts", test_temporal_time_lt_null_aborts), &
            new_unittest("parquet_date year on a null element aborts", test_temporal_date_year_null_aborts), &
            new_unittest("parquet_date month on a null element aborts", test_temporal_date_month_null_aborts), &
            new_unittest("parquet_date day on a null element aborts", test_temporal_date_day_null_aborts), &
            new_unittest("parquet_date to_mjd on a null element aborts", test_temporal_date_to_mjd_null_aborts), &
            new_unittest("parquet_date to_string on a null element aborts", test_temporal_date_to_string_null_aborts), &
            new_unittest("parquet_time to_string on a null element aborts", test_temporal_time_to_string_null_aborts), &
            new_unittest("parquet_timestamp set with an invalid month aborts", &
                test_temporal_ts_set_invalid_month_aborts), &
            new_unittest("parquet_timestamp set with an invalid nanosecond aborts", &
                test_temporal_ts_set_invalid_nanosecond_aborts), &
            new_unittest("parquet_timestamp get on a null element aborts", test_temporal_ts_get_null_aborts), &
            new_unittest("parquet_timestamp get_date beyond parquet_date's range aborts", &
                test_temporal_ts_get_date_range_exceeded_aborts), &
            new_unittest("parquet_timestamp to_unix negative-branch overflow aborts", &
                test_temporal_ts_to_unix_overflow_negative_aborts), &
            new_unittest("parquet_timestamp to_mjd on a null element aborts", test_temporal_ts_to_mjd_null_aborts), &
            new_unittest("parquet_timestamp to_string on a null element aborts", &
                test_temporal_ts_to_string_null_aborts), &
            new_unittest("parquet_timestamp operator(<) with a null operand aborts", test_temporal_ts_lt_null_aborts), &
            new_unittest("writing an undeclared temporal column aborts", &
                test_temporal_write_column_not_defined_aborts), &
            new_unittest("writing a temporal vector column with the wrong col_size aborts", &
                test_temporal_write_array_size_mismatch_aborts), &
            new_unittest("writing a temporal column of the wrong data_type aborts", &
                test_temporal_write_type_mismatch_aborts), &
            new_unittest("chunk-writing an undeclared temporal column aborts", &
                test_temporal_chunk_column_not_defined_aborts), &
            new_unittest("chunk-writing a temporal vector column with the wrong col_size aborts", &
                test_temporal_chunk_array_size_mismatch_aborts), &
            new_unittest("chunk-writing a temporal column of the wrong data_type aborts", &
                test_temporal_chunk_type_mismatch_aborts), &
            new_unittest("parquet_write_row_mask after the writer has started aborts", &
                test_mask_row_mask_after_write_started_aborts), &
            new_unittest("a whole-column write whose row count doesn't match the mask aborts", &
                test_mask_row_mask_shape_mismatch_aborts), &
            new_unittest("parquet_write_row_mask with a zero-length mask aborts", &
                test_mask_row_mask_zero_length_aborts), &
            new_unittest("calling parquet_write_row_mask twice for the same writer aborts", &
                test_mask_row_mask_called_twice_aborts), &
            new_unittest("parquet_write_chunk_row_mask after parquet_write_row_mask aborts", &
                test_mask_chunk_row_mask_after_row_mask_aborts), &
            new_unittest("parquet_write_row_mask after the writer has started row groups aborts", &
                test_mask_row_mask_after_chunk_row_mask_aborts), &
            new_unittest("parquet_write_chunk_row_mask after a whole-column write aborts", &
                test_mask_chunk_row_mask_after_whole_column_aborts), &
            new_unittest("parquet_write_chunk_row_mask not used for every row group aborts", &
                test_mask_chunk_row_mask_not_used_every_group_aborts), &
            new_unittest("parquet_write_chunk_row_mask introduced after the row group's first chunk write aborts", &
                test_mask_chunk_row_mask_introduced_late_aborts), &
            new_unittest("calling parquet_write_chunk_row_mask twice for the same row group aborts", &
                test_mask_chunk_row_mask_called_twice_aborts), &
            new_unittest("parquet_write_chunk_row_mask with the wrong mask size aborts", &
                test_mask_chunk_row_mask_size_mismatch_aborts), &
            new_unittest("a row group claiming more mask positions than parquet_write_row_mask provided aborts", &
                test_mask_row_mask_window_exhausted_aborts), &
            new_unittest("closing a writer with an unconsumed parquet_write_row_mask tail aborts", &
                test_mask_row_mask_not_fully_consumed_aborts), &
            new_unittest("parquet_write_chunk_row_mask with no row group open aborts", &
                test_mask_chunk_row_mask_no_row_group_open_aborts), &
            new_unittest("parquet_write_chunk_row_mask after an earlier row group declined it aborts", &
                test_mask_chunk_row_mask_scheme_declined_aborts), &
            new_unittest("finishing a row group with no column ever written for it aborts", &
                test_mask_row_group_no_writes_at_all_aborts), &
            new_unittest("parquet_get_version with an invalid mode aborts", &
                test_get_version_invalid_mode_aborts), &
            new_unittest("parquet_column_exists with an unrecognized types= token aborts", &
                test_column_exists_bad_type_token_aborts), &
            new_unittest("parquet_column_exists with a blank types= filter aborts", &
                test_column_exists_empty_type_filter_aborts), &
            new_unittest("parquet_get_column_type on a column outside the 9 canonical types aborts", &
                test_get_column_type_unsupported_aborts) &
            ]
    end subroutine collect_tests_parquet_errors

    subroutine test_string_column_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_index_out_of_range", expect_abort=.true., &
            failure_message="indexing a parquet_string_column out of range was expected to abort", &
            required_stderr="parquet_strings: index out of range in get")
    end subroutine test_string_column_index_out_of_range_aborts

    subroutine test_string_column_view_all_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_view_all_size_mismatch", expect_abort=.true., &
            failure_message="view_all with a mismatched-size data_string array was expected to abort", &
            required_stderr="parquet_strings: view_all: size(data_string) does not match self%size()")
    end subroutine test_string_column_view_all_size_mismatch_aborts

    subroutine test_string_column_get_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_get_null", expect_abort=.true., &
            failure_message="get on a null element with no null option was expected to abort", &
            required_stderr="parquet_strings: null element accessed in get")
    end subroutine test_string_column_get_null_aborts

    subroutine test_string_column_to_character_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_to_character_null", expect_abort=.true., &
            failure_message="to_character on a null-containing column with no null_value was expected to abort", &
            required_stderr="parquet_strings: null element accessed in to_character")
    end subroutine test_string_column_to_character_null_aborts

    subroutine test_string_handle_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_handle_unassociated", expect_abort=.true., &
            failure_message="using an unassociated parquet_string handle was expected to abort", &
            required_stderr="parquet_strings: unassociated string handle in length")
    end subroutine test_string_handle_unassociated_aborts

    subroutine test_string_handle_stale_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_handle_stale_index", expect_abort=.true., &
            failure_message="using a parquet_string handle whose index no longer exists was expected to abort", &
            required_stderr="parquet_strings: string handle index out of range (column changed?) in length")
    end subroutine test_string_handle_stale_index_aborts

    subroutine test_string_set_null_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_set_null_unassociated", expect_abort=.true., &
            failure_message="set_null on an unassociated parquet_string handle was expected to abort", &
            required_stderr="parquet_strings: unassociated string handle in set_null")
    end subroutine test_string_set_null_unassociated_aborts

    subroutine test_string_set_null_stale_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_set_null_stale_index", expect_abort=.true., &
            failure_message="set_null on a stale parquet_string handle was expected to abort", &
            required_stderr="parquet_strings: string handle index out of range (column changed?) in set_null")
    end subroutine test_string_set_null_stale_index_aborts

    subroutine test_string_slice_invalid_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_slice_invalid_range", expect_abort=.true., &
            failure_message="slice with first > last was expected to abort", &
            required_stderr="parquet_strings: invalid row range in slice")
    end subroutine test_string_slice_invalid_range_aborts

    subroutine test_string_view_slice_invalid_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_view_slice_invalid_range", expect_abort=.true., &
            failure_message="view_slice with last > self%size() was expected to abort", &
            required_stderr="parquet_strings: invalid row range in view_slice")
    end subroutine test_string_view_slice_invalid_range_aborts

    subroutine test_string_view_slice_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_view_slice_size_mismatch", expect_abort=.true., &
            failure_message="view_slice with a mismatched-size data_string array was expected to abort", &
            required_stderr="parquet_strings: view_slice: size(data_string) does not match last-first+1")
    end subroutine test_string_view_slice_size_mismatch_aborts

    subroutine test_string_build_from_self_alias_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_self_alias", expect_abort=.true., &
            failure_message="build_from with a handle aliasing the destination column was expected to abort", &
            required_stderr="parquet_strings: build_from: handle aliases the destination column self")
    end subroutine test_string_build_from_self_alias_aborts

    subroutine test_string_build_from_unassociated_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_unassociated", expect_abort=.true., &
            failure_message="build_from with an unassociated handle in the input array was expected to abort", &
            required_stderr="parquet_strings: build_from: unassociated handle in input array")
    end subroutine test_string_build_from_unassociated_aborts

    subroutine test_string_build_from_stale_index_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_build_from_stale_index", expect_abort=.true., &
            failure_message="build_from with a stale handle in the input array was expected to abort", &
            required_stderr="parquet_strings: build_from: stale or out-of-range handle in input array")
    end subroutine test_string_build_from_stale_index_aborts

    subroutine test_string_column_append_buffers_offset_not_zero_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_append_buffers_offset_not_zero", &
            expect_abort=.true., &
            failure_message="append_buffers with a source offsets(1) /= 0 was expected to abort", &
            required_stderr="parquet_strings: append_buffers: source offsets(1) must be 0")
    end subroutine test_string_column_append_buffers_offset_not_zero_aborts

    subroutine test_string_column_append_buffers_offset_not_zero_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_column_append_buffers_offset_not_zero_int32", &
            expect_abort=.true., &
            failure_message="append_buffers with a source int32 offsets(1) /= 0 was expected to abort", &
            required_stderr="parquet_strings: append_buffers: source offsets(1) must be 0")
    end subroutine test_string_column_append_buffers_offset_not_zero_int32_aborts

    subroutine test_compact_string_write_requires_scalar_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "compact_string_write_requires_scalar_column", &
            expect_abort=.true., &
            failure_message="a compact string write into a vector (col_size>1) column was expected to abort", &
            required_stderr="a parquet_string_column write requires a scalar (col_size=1) column")
    end subroutine test_compact_string_write_requires_scalar_column_aborts

    subroutine test_compact_string_write_chunk_requires_scalar_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "compact_string_write_chunk_requires_scalar_column", &
            expect_abort=.true., &
            failure_message="a compact string chunk write into a vector (col_size>1) column was expected to abort", &
            required_stderr="a parquet_string_column write requires a scalar (col_size=1) column")
    end subroutine test_compact_string_write_chunk_requires_scalar_column_aborts

    subroutine test_string_view_compact_read_unsupported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "string_view_compact_read_unsupported", &
            expect_abort=.true., &
            failure_message="reading a STRING_VIEW column into a compact parquet_string_column was expected to abort", &
            required_stderr="STRING_VIEW columns are not supported by this compact buffer read")
    end subroutine test_string_view_compact_read_unsupported_aborts

    subroutine test_ok_scenario_exits_cleanly(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "ok", expect_abort=.false., &
            failure_message="control scenario 'ok' was expected to exit cleanly")
    end subroutine test_ok_scenario_exits_cleanly

    !> Also checks the error names both the output file and the schema's
    !> maml -- see writer_context_suffix in src/parquet_write.f90.
    subroutine test_write_undeclared_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_undeclared_column", expect_abort=.true., &
            failure_message="writing an undeclared column was expected to error stop", &
            required_stderr="parquet_write_column: column not defined in parquet_open_writer: not_a_real_column " // &
                "(file: test_run/error_scenario_undeclared.parquet, maml: maml_example.maml)")
    end subroutine test_write_undeclared_column_aborts

    subroutine test_write_undeclared_column_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_int64", expect_abort=.true., &
            failure_message="writing an undeclared int64 column was expected to error stop")
    end subroutine test_write_undeclared_column_int64_aborts

    subroutine test_write_undeclared_column_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float32", expect_abort=.true., &
            failure_message="writing an undeclared float32 column was expected to error stop")
    end subroutine test_write_undeclared_column_float32_aborts

    subroutine test_write_undeclared_column_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float64", expect_abort=.true., &
            failure_message="writing an undeclared float64 column was expected to error stop")
    end subroutine test_write_undeclared_column_float64_aborts

    subroutine test_write_undeclared_column_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_logical", expect_abort=.true., &
            failure_message="writing an undeclared logical column was expected to error stop")
    end subroutine test_write_undeclared_column_logical_aborts

    subroutine test_write_undeclared_column_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_string", expect_abort=.true., &
            failure_message="writing an undeclared string column was expected to error stop")
    end subroutine test_write_undeclared_column_string_aborts

    subroutine test_write_undeclared_column_string_compact_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_string_compact", expect_abort=.true., &
            failure_message="writing an undeclared column via a compact string write was expected to error stop")
    end subroutine test_write_undeclared_column_string_compact_aborts

    subroutine test_write_undeclared_column_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_int32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int32 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_int32_matrix_aborts

    subroutine test_write_undeclared_column_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_int64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int64 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_int64_matrix_aborts

    subroutine test_write_undeclared_column_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float32 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_float32_matrix_aborts

    subroutine test_write_undeclared_column_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_float64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float64 matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_float64_matrix_aborts

    subroutine test_write_undeclared_column_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_logical_matrix", expect_abort=.true., &
            failure_message="writing an undeclared logical matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_logical_matrix_aborts

    subroutine test_write_undeclared_column_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column_string_matrix", expect_abort=.true., &
            failure_message="writing an undeclared string matrix column was expected to error stop")
    end subroutine test_write_undeclared_column_string_matrix_aborts

    subroutine test_write_not_divisible_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_int64", expect_abort=.true., &
            failure_message="writing a int64 values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_int64_aborts

    subroutine test_write_not_divisible_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_float32", expect_abort=.true., &
            failure_message="writing a float32 values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_float32_aborts

    subroutine test_write_not_divisible_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_float64", expect_abort=.true., &
            failure_message="writing a float64 values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_float64_aborts

    subroutine test_write_not_divisible_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_logical", expect_abort=.true., &
            failure_message="writing a logical values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_logical_aborts

    subroutine test_write_not_divisible_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_not_divisible_string", expect_abort=.true., &
            failure_message="writing a string values(:) array not divisible by col_size was expected to error stop")
    end subroutine test_write_not_divisible_string_aborts

    subroutine test_write_array_mismatch_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_int32_matrix", expect_abort=.true., &
            failure_message="writing a int32 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_int32_matrix_aborts

    subroutine test_write_array_mismatch_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_int64_matrix", expect_abort=.true., &
            failure_message="writing a int64 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_int64_matrix_aborts

    subroutine test_write_array_mismatch_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_float32_matrix", expect_abort=.true., &
            failure_message="writing a float32 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_float32_matrix_aborts

    subroutine test_write_array_mismatch_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_float64_matrix", expect_abort=.true., &
            failure_message="writing a float64 matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_float64_matrix_aborts

    subroutine test_write_array_mismatch_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_logical_matrix", expect_abort=.true., &
            failure_message="writing a logical matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_logical_matrix_aborts

    subroutine test_write_array_mismatch_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_array_mismatch_string_matrix", expect_abort=.true., &
            failure_message="writing a string matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_array_mismatch_string_matrix_aborts

    !> streaming (parquet_write_column_chunk) counterparts of the undeclared-column/
    !! not-divisible/array-mismatch tests above -- one deep check_scenario_exit_status_and_stderr
    !! (int32 scalar) plus exit-status-only checks for the rest, matching the batch API's own
    !! mix above.
    subroutine test_write_chunk_undeclared_column_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_undeclared_column_int32", expect_abort=.true., &
            failure_message="writing an undeclared int32 chunk column was expected to error stop", &
            required_stderr="parquet_write_column_chunk: column not defined in parquet_open_writer: " // &
                "not_a_real_column (file: test_run/error_scenario_chunk_undeclared_int32.parquet, " // &
                "maml: internal:multitype_table)")
    end subroutine test_write_chunk_undeclared_column_int32_aborts

    subroutine test_write_chunk_undeclared_column_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_int64", expect_abort=.true., &
            failure_message="writing an undeclared int64 chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_int64_aborts

    subroutine test_write_chunk_undeclared_column_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float32", expect_abort=.true., &
            failure_message="writing an undeclared float32 chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float32_aborts

    subroutine test_write_chunk_undeclared_column_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float64", expect_abort=.true., &
            failure_message="writing an undeclared float64 chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float64_aborts

    subroutine test_write_chunk_undeclared_column_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_logical", expect_abort=.true., &
            failure_message="writing an undeclared logical chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_logical_aborts

    subroutine test_write_chunk_undeclared_column_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_string", expect_abort=.true., &
            failure_message="writing an undeclared string chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_string_aborts

    subroutine test_write_chunk_undeclared_column_string_compact_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_string_compact", expect_abort=.true., &
            failure_message="writing an undeclared column via a compact string chunk write was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_string_compact_aborts

    subroutine test_write_chunk_undeclared_column_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_int32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int32 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_int32_matrix_aborts

    subroutine test_write_chunk_undeclared_column_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_int64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared int64 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_int64_matrix_aborts

    subroutine test_write_chunk_undeclared_column_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float32_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float32 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float32_matrix_aborts

    subroutine test_write_chunk_undeclared_column_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_float64_matrix", expect_abort=.true., &
            failure_message="writing an undeclared float64 matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_float64_matrix_aborts

    subroutine test_write_chunk_undeclared_column_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_logical_matrix", expect_abort=.true., &
            failure_message="writing an undeclared logical matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_logical_matrix_aborts

    subroutine test_write_chunk_undeclared_column_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_undeclared_column_string_matrix", expect_abort=.true., &
            failure_message="writing an undeclared string matrix chunk column was expected to error stop")
    end subroutine test_write_chunk_undeclared_column_string_matrix_aborts

    subroutine test_write_chunk_not_divisible_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_int32", expect_abort=.true., &
            failure_message="writing a int32 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_int32_aborts

    subroutine test_write_chunk_not_divisible_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_int64", expect_abort=.true., &
            failure_message="writing a int64 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_int64_aborts

    subroutine test_write_chunk_not_divisible_float32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_float32", expect_abort=.true., &
            failure_message="writing a float32 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_float32_aborts

    subroutine test_write_chunk_not_divisible_float64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_float64", expect_abort=.true., &
            failure_message="writing a float64 chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_float64_aborts

    subroutine test_write_chunk_not_divisible_logical_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_logical", expect_abort=.true., &
            failure_message="writing a logical chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_logical_aborts

    subroutine test_write_chunk_not_divisible_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_not_divisible_string", expect_abort=.true., &
            failure_message="writing a string chunk values(:) array not divisible by col_size was expected " // &
                "to error stop")
    end subroutine test_write_chunk_not_divisible_string_aborts

    subroutine test_write_chunk_array_mismatch_int32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_int32_matrix", expect_abort=.true., &
            failure_message="writing a int32 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_int32_matrix_aborts

    subroutine test_write_chunk_array_mismatch_int64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_int64_matrix", expect_abort=.true., &
            failure_message="writing a int64 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_int64_matrix_aborts

    subroutine test_write_chunk_array_mismatch_float32_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_float32_matrix", expect_abort=.true., &
            failure_message="writing a float32 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_float32_matrix_aborts

    subroutine test_write_chunk_array_mismatch_float64_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_float64_matrix", expect_abort=.true., &
            failure_message="writing a float64 chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_float64_matrix_aborts

    subroutine test_write_chunk_array_mismatch_logical_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_logical_matrix", expect_abort=.true., &
            failure_message="writing a logical chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_logical_matrix_aborts

    subroutine test_write_chunk_array_mismatch_string_matrix_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_array_mismatch_string_matrix", expect_abort=.true., &
            failure_message="writing a string chunk matrix with a col_size mismatch was expected to error stop")
    end subroutine test_write_chunk_array_mismatch_string_matrix_aborts

    subroutine test_write_chunk_string_matrix_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_string_matrix_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string matrix chunk column " // &
                "was expected to error stop")
    end subroutine test_write_chunk_string_matrix_exceeds_array_size_aborts

    !> Same check as test_write_chunk_string_matrix_exceeds_array_size_aborts, but for
    !! parquet_write_string_column_chunk's 1D/flat form (values(:), dispatched for a rank-1
    !! actual argument).
    subroutine test_write_chunk_string_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_chunk_string_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string vector chunk column " // &
                "(flat form) was expected to error stop")
    end subroutine test_write_chunk_string_exceeds_array_size_aborts

    subroutine test_write_chunk_no_row_group_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_no_row_group_open", expect_abort=.true., &
            failure_message="writing a chunk column before parquet_new_row_group was expected to error stop", &
            required_stderr="parquet_write_column_chunk: no row group is open (call parquet_new_row_group " // &
                "first) for column i32 (file: test_run/error_scenario_chunk_no_row_group.parquet, " // &
                "maml: internal:multitype_table)")
    end subroutine test_write_chunk_no_row_group_open_aborts

    subroutine test_write_chunk_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_row_count_mismatch", expect_abort=.true., &
            failure_message="writing a chunk with a row count mismatch was expected to error stop", &
            required_stderr="parquet_write_column_chunk: row count mismatch for column i32: the open row " // &
                "group has 3 rows but this chunk has 2 " // &
                "(file: test_run/error_scenario_chunk_row_count_mismatch.parquet, maml: internal:multitype_table)")
    end subroutine test_write_chunk_row_count_mismatch_aborts

    subroutine test_write_chunk_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_chunk_type_mismatch", expect_abort=.true., &
            failure_message="writing a chunk with a type mismatch was expected to error stop", &
            required_stderr="parquet_write_column_chunk: type mismatch for column i32 (expected boolean, " // &
                "got int32) -- parquet_write_column_chunk requires an exact type match, unlike " // &
                "parquet_write_column (file: test_run/error_scenario_chunk_type_mismatch.parquet, " // &
                "maml: internal:multitype_table)")
    end subroutine test_write_chunk_type_mismatch_aborts

    subroutine test_read_chunk_with_filter_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_with_filter", expect_abort=.true., &
            failure_message="a chunked read on a filtered reader was expected to error stop", &
            required_stderr="chunked reads are not supported on a reader opened with an active filter=")
    end subroutine test_read_chunk_with_filter_aborts

    subroutine test_read_chunk_qc_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_qc_hard_aborts", expect_abort=.true., &
            failure_message="a hard qc violation in a chunk read was expected to abort", &
            required_stderr="qc violation for column 'ra [row group 1]'")
    end subroutine test_read_chunk_qc_hard_aborts

    subroutine test_read_chunk_qc_soft_warns(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_qc_soft_warns", expect_abort=.false., &
            failure_message="a soft qc violation in a chunk read was expected to warn, not abort", &
            required_stderr="WARNING: qc violation for column 'ra [row group 1]'")
    end subroutine test_read_chunk_qc_soft_warns

    subroutine test_read_chunk_check_complete_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_check_complete_hard_aborts", expect_abort=.true., &
            failure_message="closing an incomplete chunked read with check_complete=.true. was expected to abort", &
            required_stderr="column 'v' was read via parquet_read_column_chunk but not every row group was read")
    end subroutine test_read_chunk_check_complete_hard_aborts

    subroutine test_read_chunk_row_group_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_row_group_out_of_range", expect_abort=.true., &
            failure_message="reading an out-of-range row_group was expected to error stop", &
            required_stderr="row_group 3 out of range (file has 2 row group(s))")
    end subroutine test_read_chunk_row_group_out_of_range_aborts

    !> parquet_get_chunk_size has its own row_group bounds check, independent of
    !> parquet_read_column_chunk's (see check_row_group_valid vs
    !> parquet_get_chunk_size_reader_impl in parquet_read.f90) -- must abort even when
    !> parquet_read_column_chunk itself is never called.
    subroutine test_get_chunk_size_row_group_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_chunk_size_row_group_out_of_range", &
            expect_abort=.true., &
            failure_message="parquet_get_chunk_size with an out-of-range row_group was expected to error stop", &
            required_stderr="parquet_get_chunk_size: row_group 3 out of range (file has 2 row group(s))")
    end subroutine test_get_chunk_size_row_group_out_of_range_aborts

    subroutine test_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_type_mismatch", expect_abort=.true., &
            failure_message="writing a type-mismatched column was expected to error stop")
    end subroutine test_write_type_mismatch_aborts

    subroutine test_write_column_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_column_twice", expect_abort=.true., &
            failure_message="writing the same column twice was expected to error stop")
    end subroutine test_write_column_twice_aborts

    !> Same rule as test_write_column_twice_aborts, but for a schema-less
    !> writer (no cinfo), which previously had no tracking at all for this.
    subroutine test_write_column_twice_no_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_column_twice_no_schema", expect_abort=.true., &
            failure_message="writing the same column twice on a schema-less writer was expected to error stop", &
            required_stderr="parquet_write_column: column written more than once: id")
    end subroutine test_write_column_twice_no_schema_aborts

    subroutine test_validate_bad_data_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_bad_data_type", expect_abort=.true., &
            failure_message="validating an invalid data_type was expected to error stop")
    end subroutine test_validate_bad_data_type_aborts

    !> Locks in the "decimal" type exclusion from the README's Limitations section (date/
    !> timestamp were excluded here too once, but are now supported -- see
    !> src/parquet_temporal.f90 -- so those two tests were removed) so that accidentally adding
    !> it to the allowed type list would be caught here, not just a generic invalid-token check.
    subroutine test_validate_excluded_decimal_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_excluded_decimal_type", expect_abort=.true., &
            failure_message="'decimal' is documented as unsupported and was expected to error stop")
    end subroutine test_validate_excluded_decimal_type_aborts

    subroutine test_validate_empty_field_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_empty_field_name", expect_abort=.true., &
            failure_message="validating a fields: entry with no name: sub-key was expected to error stop")
    end subroutine test_validate_empty_field_name_aborts

    subroutine test_validate_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_duplicate_name", expect_abort=.true., &
            failure_message="validating a duplicate field name was expected to error stop")
    end subroutine test_validate_duplicate_name_aborts

    subroutine test_validate_missing_table_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_missing_table", expect_abort=.true., &
            failure_message="validating a MAML file without 'table:' was expected to error stop")
    end subroutine test_validate_missing_table_aborts

    subroutine test_validate_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_no_fields", expect_abort=.true., &
            failure_message="validating a MAML file without fields was expected to error stop")
    end subroutine test_validate_no_fields_aborts

    subroutine test_validate_trailing_depends_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_trailing_depends_no_fields", expect_abort=.true., &
            failure_message="a MAML with a trailing, unclosed depends: list and no fields: " // &
                "was expected to error stop")
    end subroutine test_validate_trailing_depends_no_fields_aborts

    subroutine test_validate_trailing_keywords_no_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_trailing_keywords_no_fields", expect_abort=.true., &
            failure_message="a MAML with a trailing, unclosed keywords: list and no fields: " // &
                "was expected to error stop")
    end subroutine test_validate_trailing_keywords_no_fields_aborts

    subroutine test_validate_unknown_top_level_section_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_unknown_top_level_section", expect_abort=.true., &
            failure_message="validating a MAML file with an unknown top-level section was expected to error stop")
    end subroutine test_validate_unknown_top_level_section_aborts

    subroutine test_validate_unknown_field_subkey_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_unknown_field_subkey", expect_abort=.true., &
            failure_message="validating a MAML file with an unknown field sub-key was expected to error stop")
    end subroutine test_validate_unknown_field_subkey_aborts

    subroutine test_validate_unknown_qc_subkey_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_unknown_qc_subkey", expect_abort=.true., &
            failure_message="validating a MAML file with an unknown qc: sub-key was expected to error stop")
    end subroutine test_validate_unknown_qc_subkey_aborts

    subroutine test_validate_user_maml_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_user_maml_unknown_column", expect_abort=.true., &
            failure_message="validating a user MAML with an unknown column was expected to error stop")
    end subroutine test_validate_user_maml_unknown_column_aborts

    subroutine test_validate_col_map_unknown_internal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_unknown_internal", expect_abort=.true., &
            failure_message="col_map: referencing an internal column not in the base MAML was expected to error stop")
    end subroutine test_validate_col_map_unknown_internal_aborts

    !> Asserts the specific "duplicate internal column" message (not just
    !> that something aborted), since this scenario's fixture is carefully
    !> built to avoid tripping the (unrelated) generic "duplicate field
    !> name" check first -- see its doc comment in error_scenarios.f90.
    subroutine test_validate_col_map_duplicate_internal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_col_map_duplicate_internal", expect_abort=.true., &
            failure_message="col_map: a duplicate internal column key was expected to error stop", &
            required_stderr="col_map: duplicate internal column 'a'")
    end subroutine test_validate_col_map_duplicate_internal_aborts

    subroutine test_validate_col_map_output_collision_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_output_collision", expect_abort=.true., &
            failure_message="col_map: two internal columns mapped to the same output name was expected to error stop")
    end subroutine test_validate_col_map_output_collision_aborts

    subroutine test_validate_col_map_output_not_declared_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_output_not_declared", expect_abort=.true., &
            failure_message="col_map: a renamed output not declared in fields: was expected to error stop")
    end subroutine test_validate_col_map_output_not_declared_aborts

    subroutine test_validate_col_map_internal_also_in_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_internal_also_in_fields", expect_abort=.true., &
            failure_message="col_map: a remapped internal column also declared un-renamed in fields: " // &
                "was expected to error stop")
    end subroutine test_validate_col_map_internal_also_in_fields_aborts

    subroutine test_validate_col_map_output_matches_other_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_output_matches_other_field", expect_abort=.true., &
            failure_message="col_map: output name coinciding with an existing base column was expected to error stop")
    end subroutine test_validate_col_map_output_matches_other_field_aborts

    subroutine test_set_column_available_deactivated_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_column_available_deactivated", expect_abort=.true., &
            failure_message="set_column_available on a deactivated column was expected to error stop")
    end subroutine test_set_column_available_deactivated_aborts

    subroutine test_set_column_unavailable_deactivated_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_column_unavailable_deactivated", expect_abort=.true., &
            failure_message="set_column_unavailable on a deactivated column was expected to error stop")
    end subroutine test_set_column_unavailable_deactivated_aborts

    subroutine test_get_column_index_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_column_index_not_found", expect_abort=.true., &
            failure_message="get_column_index on an unknown column was expected to error stop")
    end subroutine test_get_column_index_not_found_aborts

    subroutine test_get_field_name_index_too_low_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_field_name_index_too_low", expect_abort=.true., &
            failure_message="get_field_name(0) was expected to error stop")
    end subroutine test_get_field_name_index_too_low_aborts

    subroutine test_get_field_name_index_too_high_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_field_name_index_too_high", expect_abort=.true., &
            failure_message="get_field_name(get_num_fields()+1) was expected to error stop")
    end subroutine test_get_field_name_index_too_high_aborts

    subroutine test_write_maml_without_metadata_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_maml_without_metadata", expect_abort=.true., &
            failure_message="write_maml=.true. without metadata was expected to error stop")
    end subroutine test_write_maml_without_metadata_aborts

    !> A genuine Parquet Null with no null_value/is_valid given now aborts via
    !> report_fatal_error (print + std::abort()), same as the type-mismatch
    !> case below, instead of an uncaught C++ exception reaching
    !> std::terminate() -- still a C++-level abort, not a Fortran error stop
    !> (see README's Null values section), just a clean, diagnosable one.
    subroutine test_read_column_with_nulls_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_column_with_nulls", expect_abort=.true., &
            failure_message="reading a column with a genuine Parquet Null was expected to abort", &
            required_stderr="column contains Null value(s), which is not supported: id_with_null")
    end subroutine test_read_column_with_nulls_aborts

    !> README's Limitations section documents that reading a column whose
    !> physical Parquet type doesn't match what was requested aborts the
    !> process via a C++-level abort (not a clean Fortran error stop).
    !> parquet_wrapper.cpp's read functions report that failure via
    !> report_fatal_error (print + std::abort()) instead of letting an
    !> uncaught C++ exception reach std::terminate() -- this just checks the
    !> process still aborts (nonzero exit), which is all this out-of-process
    !> harness can observe either way.
    subroutine test_read_unsupported_physical_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_unsupported_physical_type", expect_abort=.true., &
            failure_message="reading a column of an unsupported physical Parquet type was expected to abort")
    end subroutine test_read_unsupported_physical_type_aborts

    !> The 16 tests below are the Fortran-side counterpart of 16 scenarios added to
    !> error_scenarios.f90 covering array-mode bool8/string type-mismatch + col_index bounds
    !> across whole-column/row-mode/element-mode/chunk-mode reads, plus two extra element-mode
    !> col_index-bounds tests (numeric) -- see error_scenarios.f90's own comment above those
    !> 16 scenarios.

    subroutine test_read_array_full_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_full_bool_type_mismatch", expect_abort=.true., &
            failure_message="reading an int32 vector column via the whole-column logical array specific " // &
            "was expected to abort")
    end subroutine test_read_array_full_bool_type_mismatch_aborts

    subroutine test_read_array_full_string_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_full_string_type_mismatch", expect_abort=.true., &
            failure_message="reading an int32 vector column via the whole-column string array specific " // &
            "was expected to abort")
    end subroutine test_read_array_full_string_type_mismatch_aborts

    subroutine test_read_array_row_mode_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_row_mode_bool_type_mismatch", expect_abort=.true., &
            failure_message="reading one row of an int32 vector column via the logical row_mode specific " // &
            "was expected to abort")
    end subroutine test_read_array_row_mode_bool_type_mismatch_aborts

    subroutine test_read_array_row_mode_string_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_row_mode_string_type_mismatch", expect_abort=.true., &
            failure_message="reading one row of an int32 vector column via the string row_mode specific " // &
            "was expected to abort")
    end subroutine test_read_array_row_mode_string_type_mismatch_aborts

    subroutine test_read_array_em_filt_bool_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_bool_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on a filtered " // &
            "logical reader was expected to abort")
    end subroutine test_read_array_em_filt_bool_oob_aborts

    subroutine test_read_array_em_filt_bool_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_bool_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as logical on a " // &
            "filtered reader was expected to abort")
    end subroutine test_read_array_em_filt_bool_tm_aborts

    subroutine test_read_array_em_filt_string_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_string_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on a filtered " // &
            "string reader was expected to abort")
    end subroutine test_read_array_em_filt_string_oob_aborts

    subroutine test_read_array_em_filt_string_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_string_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as string on a " // &
            "filtered reader was expected to abort")
    end subroutine test_read_array_em_filt_string_tm_aborts

    subroutine test_read_array_em_bool_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_bool_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on an unfiltered " // &
            "logical reader was expected to abort")
    end subroutine test_read_array_em_bool_oob_aborts

    subroutine test_read_array_em_bool_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_bool_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as logical on an " // &
            "unfiltered reader was expected to abort")
    end subroutine test_read_array_em_bool_tm_aborts

    subroutine test_read_array_em_string_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_string_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on an unfiltered " // &
            "string reader was expected to abort")
    end subroutine test_read_array_em_string_oob_aborts

    subroutine test_read_array_em_string_tm_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_string_type_mismatch", &
            expect_abort=.true., failure_message="element_mode read of an int32 vector column as string on an " // &
            "unfiltered reader was expected to abort")
    end subroutine test_read_array_em_string_tm_aborts

    subroutine test_read_array_column_chunk_bool_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_column_chunk_bool_type_mismatch", expect_abort=.true., &
            failure_message="reading a chunked int32 vector column as logical was expected to abort")
    end subroutine test_read_array_column_chunk_bool_type_mismatch_aborts

    subroutine test_read_array_column_chunk_string_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_column_chunk_string_type_mismatch", expect_abort=.true., &
            failure_message="reading a chunked int32 vector column as string was expected to abort")
    end subroutine test_read_array_column_chunk_string_type_mismatch_aborts

    subroutine test_read_array_em_filt_int32_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_filtered_int32_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on a filtered " // &
            "int32 reader was expected to abort")
    end subroutine test_read_array_em_filt_int32_oob_aborts

    subroutine test_read_array_em_int32_oob_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_array_element_mode_int32_col_index_out_of_range", &
            expect_abort=.true., failure_message="element_mode with an out-of-range col_index on an unfiltered " // &
            "int32 reader was expected to abort")
    end subroutine test_read_array_em_int32_oob_aborts

    !> The 11 tests below are the Fortran-side counterpart of
    !> error_scenarios.f90's own "extended_..." scenarios documented there --
    !> each checks the exact stderr message for one report_fatal_error call
    !> site added to convert_values_to_int32/int64 (parquet_wrapper.cpp) for
    !> the extended read-time source types (see doc/pages/supported-data-types.md
    !> and CONTRIBUTING.md's "Additional scalar types" note).

    subroutine test_extended_uint32_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_uint32_overflow_int32", expect_abort=.true., &
            failure_message="reading a uint32 value exceeding int32 range was expected to error stop", &
            required_stderr="uint32->int32 overflow for column: v_uint32_ovf")
    end subroutine test_extended_uint32_overflow_int32_aborts

    subroutine test_extended_uint64_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_uint64_overflow_int32", expect_abort=.true., &
            failure_message="reading a uint64 value exceeding int32 range was expected to error stop", &
            required_stderr="uint64->int32 overflow for column: v_uint64_ovf32")
    end subroutine test_extended_uint64_overflow_int32_aborts

    subroutine test_extended_uint64_overflow_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_uint64_overflow_int64", expect_abort=.true., &
            failure_message="reading a uint64 value exceeding int64 range was expected to error stop", &
            required_stderr="uint64->int64 overflow for column: v_uint64_ovf64")
    end subroutine test_extended_uint64_overflow_int64_aborts

    subroutine test_extended_real_nonintegral_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_nonintegral_int32", expect_abort=.true., &
            failure_message="reading a non-integral double value into int32 was expected to error stop", &
            required_stderr="double value has a fractional part, cannot convert to int32 for column: v_double_fractional")
    end subroutine test_extended_real_nonintegral_int32_aborts

    subroutine test_extended_real_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_overflow_int32", expect_abort=.true., &
            failure_message="reading an out-of-int32-range double value was expected to error stop", &
            required_stderr="double->int32 overflow for column: v_double_ovf32")
    end subroutine test_extended_real_overflow_int32_aborts

    subroutine test_extended_real_nonintegral_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_nonintegral_int64", expect_abort=.true., &
            failure_message="reading a non-integral double value into int64 was expected to error stop", &
            required_stderr="double value has a fractional part, cannot convert to int64 for column: v_double_fractional")
    end subroutine test_extended_real_nonintegral_int64_aborts

    subroutine test_extended_real_overflow_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_real_overflow_int64", expect_abort=.true., &
            failure_message="reading an out-of-int64-range double value was expected to error stop", &
            required_stderr="double->int64 overflow for column: v_double_ovf64")
    end subroutine test_extended_real_overflow_int64_aborts

    subroutine test_extended_decimal_nonintegral_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_nonintegral_int32", expect_abort=.true., &
            failure_message="reading a non-integral decimal value into int32 was expected to error stop", &
            required_stderr="value has a fractional part, cannot convert to int32 for column: v_decimal_scaled")
    end subroutine test_extended_decimal_nonintegral_int32_aborts

    subroutine test_extended_decimal_overflow_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_overflow_int32", expect_abort=.true., &
            failure_message="reading an out-of-int32-range decimal value was expected to error stop", &
            required_stderr="->int32 overflow for column: v_decimal_ovf32")
    end subroutine test_extended_decimal_overflow_int32_aborts

    subroutine test_extended_decimal_nonintegral_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_nonintegral_int64", expect_abort=.true., &
            failure_message="reading a non-integral decimal value into int64 was expected to error stop", &
            required_stderr="value has a fractional part, cannot convert to int64 for column: v_decimal_scaled")
    end subroutine test_extended_decimal_nonintegral_int64_aborts

    subroutine test_extended_decimal_overflow_int64_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "extended_decimal_overflow_int64", expect_abort=.true., &
            failure_message="reading an out-of-int64-range decimal value was expected to error stop", &
            required_stderr="->int64 overflow for column: v_decimal_ovf64")
    end subroutine test_extended_decimal_overflow_int64_aborts

    !> check_col_size_fits_arrow_limit in parquet_wrapper.cpp aborts via a C++-level
    !> report_fatal_error (not a Fortran error stop) the moment a vector column's col_size
    !> would exceed Arrow's FixedSizeListType int32_t limit -- see the README's Limitations
    !> section and scenario_col_size_overflow's own comment for why this is tested with a
    !> shrunk test-only threshold rather than a genuinely oversized column.
    subroutine test_write_col_size_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "col_size_overflow", expect_abort=.true., &
            failure_message="writing a vector column with col_size exceeding the (shrunk) Arrow limit " // &
                "was expected to abort", &
            required_stderr="parquet_append_column: column 'v': col_size (6) exceeds 2147483647, " // &
                "the maximum vector-column width Arrow's FixedSizeListType supports")
    end subroutine test_write_col_size_overflow_aborts

    !> check_explicit_chunk_size_fits_arrow_limit in parquet_wrapper.cpp aborts via a C++-level
    !> report_fatal_error (not a Fortran error stop) the moment an *explicit* chunk_size
    !> (parquet_open_writer(..., chunk_size=)/parquet_set_writer_options) combined with a vector
    !> column's col_size would exceed Arrow/Parquet's int32_t per-row-group list-element-count
    !> limit -- distinct from col_size alone (test_write_col_size_overflow_aborts, above). An
    !> *auto*-sized chunk_size no longer aborts for this (see
    !> scenario_list_element_count_auto_multi_row_group in test_writing.f90's
    !> test_list_element_count_auto_multi_row_group_roundtrip): only a caller-chosen chunk_size
    !> that conflicts with col_size is validated rather than silently overridden. See the
    !> README's Limitations section and
    !> scenario_list_element_count_explicit_chunk_size_overflow's own comment for why this is
    !> tested with a shrunk test-only threshold rather than a genuinely oversized
    !> (chunk_size * col_size > 2^31-1) vector column.
    subroutine test_write_list_element_count_chunk_size_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "list_element_count_explicit_chunk_size_overflow", &
            expect_abort=.true., &
            failure_message="writing a vector column with an explicit chunk_size*col_size exceeding the " // &
                "(shrunk) Arrow limit was expected to abort", &
            required_stderr="close_parquet_writer: column 'v': chunk_size (3) * col_size (2) exceeds 2147483647, " // &
                "the maximum per-row-group element count Arrow/Parquet's list-column level generation supports")
    end subroutine test_write_list_element_count_chunk_size_overflow_aborts

    !> Counterpart to test_write_list_element_count_chunk_size_overflow_aborts, above, for the
    !> streaming row-group API: an explicit parquet_new_row_group(writer, nrows) whose nrows
    !> conflicts with a schema-declared vector column's col_size aborts via a C++-level
    !> report_fatal_error, the same way an explicit chunk_size does for the batch path. See
    !> scenario_row_group_explicit_nrows_overflow's own comment for why this is tested with a
    !> shrunk test-only threshold.
    subroutine test_row_group_explicit_nrows_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_explicit_nrows_overflow", expect_abort=.true., &
            failure_message="starting a row group with an explicit nrows*col_size exceeding the (shrunk) Arrow " // &
                "limit was expected to abort", &
            required_stderr="parquet_new_row_group: column 'v': nrows (3) * col_size (2) exceeds 2147483647, " // &
                "the maximum per-row-group element count Arrow/Parquet's list-column level generation supports")
    end subroutine test_row_group_explicit_nrows_overflow_aborts

    !> close_streaming_writer in parquet_wrapper.cpp aborts (a C++-level uncaught
    !> std::runtime_error, not a clean Fortran error stop) if a row group was started via
    !> parquet_new_row_group but parquet_finish_row_group was never called before
    !> parquet_close_writer -- see scenario_row_group_dangling_at_close's own comment.
    subroutine test_row_group_dangling_at_close_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_dangling_at_close", expect_abort=.true., &
            failure_message="closing a writer with a dangling open row group was expected to abort", &
            required_stderr="A row group was started via parquet_new_row_group but never finished via " // &
                "parquet_finish_row_group before close")
    end subroutine test_row_group_dangling_at_close_aborts

    !> close_streaming_writer aborts if a whole (parquet_write_column) column's row count
    !> exceeds what the streaming row-group API actually covered by the time the writer closes
    !> -- see scenario_row_group_whole_column_undercovered's own comment.
    subroutine test_row_group_whole_column_undercovered_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_whole_column_undercovered", &
            expect_abort=.true., &
            failure_message="closing a writer with an under-covered whole column was expected to abort", &
            required_stderr="column 'whole' has 5 rows (written via parquet_write_column), but only 3 were " // &
                "covered by row groups written via parquet_new_row_group/parquet_write_column_chunk/" // &
                "parquet_finish_row_group")
    end subroutine test_row_group_whole_column_undercovered_aborts

    !> parquet_finish_row_group in parquet_wrapper.cpp aborts (via report_fatal_error)
    !> immediately -- rather than waiting until close -- if a row group's own rows would read
    !> past the end of an already-whole (parquet_write_column) column. See
    !> scenario_row_group_whole_column_overrun's own comment.
    subroutine test_row_group_whole_column_overrun_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_whole_column_overrun", expect_abort=.true., &
            failure_message="a row group reading past a whole column's own row count was expected to abort", &
            required_stderr="parquet_finish_row_group: column 'whole' has 3 rows (written via " // &
                "parquet_write_column), but row groups have already covered 0 of them and this row group " // &
                "would add 5 more, exceeding the column's own row count")
    end subroutine test_row_group_whole_column_overrun_aborts

    !> check_column_chunk_write_preconditions in parquet_wrapper.cpp aborts (via
    !> report_fatal_error) if a column is introduced (its first parquet_write_column_chunk call)
    !> after the first row group has already been written -- a Parquet file's schema is fixed
    !> from that point on. See scenario_row_group_new_column_after_first's own comment.
    subroutine test_row_group_new_column_after_first_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_new_column_after_first", expect_abort=.true., &
            failure_message="introducing a new column after the first row group was expected to abort", &
            required_stderr="parquet_write_column_chunk: column 'b': introduced after the first row group " // &
                "was already written -- every column must appear in the first row group, since a Parquet " // &
                "file's schema is fixed once the first row group is written")
    end subroutine test_row_group_new_column_after_first_aborts

    !> The converse of the test above: append_column (the whole-column parquet_write_column
    !> path) throws once the streaming row-group API has already started, even for a column
    !> the streaming API never touched. Unlike
    !> test_row_group_new_column_after_first_aborts above, this is a plain `throw`, not
    !> report_fatal_error.
    subroutine test_row_group_whole_column_after_streaming_started_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_whole_column_after_streaming_started", &
            expect_abort=.true., &
            failure_message="a whole-column write after the streaming row-group API already started " // &
                "was expected to abort", &
            required_stderr="Column written via parquet_write_column after the streaming row-group API " // &
                "already started")
    end subroutine test_row_group_whole_column_after_streaming_started_aborts

    !> parquet_new_row_group must not be called again while a row group is already open --
    !> see the writer%in_row_group guard added to parquet_new_row_group_impl.
    subroutine test_row_group_started_while_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "row_group_started_while_open", expect_abort=.true., &
            failure_message="starting a new row group while one was already open was expected to abort", &
            required_stderr="parquet_new_row_group: a row group is already open")
    end subroutine test_row_group_started_while_open_aborts

    !> check_column_count_fits_arrow_limit in parquet_wrapper.cpp aborts via a C++-level
    !> report_fatal_error (not a Fortran error stop) the moment a table's column count would
    !> exceed Arrow's Schema int32_t field-count limit -- see the README's Limitations section
    !> and scenario_column_count_overflow's own comment for why this is tested with a shrunk
    !> test-only threshold rather than a genuinely huge number of columns.
    subroutine test_write_column_count_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "column_count_overflow", expect_abort=.true., &
            failure_message="writing a table with column count exceeding the (shrunk) Arrow limit " // &
                "was expected to abort", &
            required_stderr="parquet_append_column: column 'c4': this table would have 4 columns, " // &
                "exceeding 2147483647, the maximum column count Arrow's Schema supports")
    end subroutine test_write_column_count_overflow_aborts

    !> parquet_prefetch_columns now validates names against the file's schema
    !> up front and error stops with a dedicated message, instead of letting
    !> the C++ "Column not found" exception escape uncaught.
    subroutine test_prefetch_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "prefetch_unknown_column", expect_abort=.true., &
            failure_message="prefetching an unknown column was expected to abort", &
            required_stderr="parquet_prefetch_columns: column not found in parquet file: not_a_real_column")
    end subroutine test_prefetch_unknown_column_aborts

    !> parquet_open_reader(..., filter=) validates every filter column name
    !> against the schema before applying it -- an unknown column aborts
    !> cleanly rather than reaching Arrow's own uncaught exception.
    subroutine test_filter_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unknown_column", expect_abort=.true., &
            failure_message="opening a reader with a filter naming an unknown column was expected to abort", &
            required_stderr="unknown column in filter: not_a_real_column")
    end subroutine test_filter_unknown_column_aborts

    !> Filtering only supports scalar columns; naming a vector column in a
    !> filter rule aborts cleanly instead of silently doing something
    !> undefined per-row.
    subroutine test_filter_vector_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_vector_column", expect_abort=.true., &
            failure_message="opening a reader with a filter naming a vector column was expected to abort", &
            required_stderr="filter column 'vec' is a vector column")
    end subroutine test_filter_vector_column_aborts

    !> A filter rule missing its operator ("<column> <op> [value]" shape)
    !> aborts with a clean, syntax-specific message from the Fortran-side
    !> tokenizer, before ever reaching the C++ side.
    subroutine test_filter_malformed_rule_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_malformed_rule", expect_abort=.true., &
            failure_message="opening a reader with a malformed filter rule was expected to abort", &
            required_stderr="invalid filter rule")
    end subroutine test_filter_malformed_rule_aborts

    subroutine test_filter_rule_too_long_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_rule_too_long", expect_abort=.true., &
            failure_message="adding a filter rule longer than 512 characters was expected to abort", &
            required_stderr="parquet_filter%add: rule exceeds the maximum supported length")
    end subroutine test_filter_rule_too_long_aborts

    !> A non-numeric value against a numeric filter column (rule shape is
    !> fine, the value itself isn't) aborts with a message naming the value
    !> and the column, distinct from the shape-only rejection above.
    subroutine test_filter_bad_numeric_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_numeric_value", expect_abort=.true., &
            failure_message="a non-numeric filter value against a numeric column was expected to abort", &
            required_stderr="filter rule: value 'abc' is not a valid integer for column 'id_with_null'")
    end subroutine test_filter_bad_numeric_value_aborts

    subroutine test_filter_int32_value_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_int32_value_out_of_range", expect_abort=.true., &
            failure_message="an out-of-int32-range filter value was expected to abort", &
            required_stderr="is out of int32 range for column 'id_with_null'")
    end subroutine test_filter_int32_value_out_of_range_aborts

    subroutine test_filter_bad_numeric_value_float_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_numeric_value_float", expect_abort=.true., &
            failure_message="a non-numeric filter value against a float column was expected to abort", &
            required_stderr="value 'abc' is not a valid number for column 'v'")
    end subroutine test_filter_bad_numeric_value_float_aborts

    !> A string column's filter value must be double-quoted -- a bare,
    !> unquoted word aborts rather than being silently treated as a string.
    subroutine test_filter_unquoted_string_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unquoted_string_value", expect_abort=.true., &
            failure_message="an unquoted value against a string filter column was expected to abort", &
            required_stderr="must be double-quoted")
    end subroutine test_filter_unquoted_string_value_aborts

    !> A boolean column's filter value must be the literal true/false.
    subroutine test_filter_bad_boolean_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_boolean_value", expect_abort=.true., &
            failure_message="an invalid boolean filter value was expected to abort", &
            required_stderr="is not true/false for boolean column")
    end subroutine test_filter_bad_boolean_value_aborts

    !> A double-quoted boolean filter value ('flag == "true"') is rejected too -- distinct from
    !> test_filter_bad_boolean_value_aborts above, which uses an unquoted-but-invalid word.
    subroutine test_filter_boolean_value_must_be_unquoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_boolean_value_must_be_unquoted", &
            expect_abort=.true., &
            failure_message="a double-quoted boolean filter value was expected to abort", &
            required_stderr="must be true or false (unquoted)")
    end subroutine test_filter_boolean_value_must_be_unquoted_aborts

    !> Ordering comparisons (>, >=, <, <=) aren't supported against a boolean
    !> filter column -- only ==//= are.
    subroutine test_filter_bool_ordering_not_supported_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bool_ordering_not_supported", expect_abort=.true., &
            failure_message="an ordering comparison against a boolean filter column was expected to abort", &
            required_stderr="ordering comparisons")
    end subroutine test_filter_bool_ordering_not_supported_aborts

    !> eval_filter_clause's `default:` branch -- filtering a column type (temporal) it doesn't
    !> support at all.
    subroutine test_filter_unsupported_column_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_unsupported_column_type", expect_abort=.true., &
            failure_message="filtering a temporal column was expected to abort", &
            required_stderr="has a type that filtering does not support")
    end subroutine test_filter_unsupported_column_type_aborts

    !> parquet_open_reader's sample_fraction < 0.0 aborts immediately.
    subroutine test_sample_negative_fraction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sample_negative_fraction", expect_abort=.true., &
            failure_message="a negative sample_fraction was expected to abort", &
            required_stderr="sample_fraction must not be negative")
    end subroutine test_sample_negative_fraction_aborts

    !> parquet_open_reader's sample_fraction NaN aborts immediately -- checked before any relational
    !> comparison, since NaN compares false against every threshold and would otherwise silently
    !> fall through as a no-op (see parquet_open_reader_base's own comment, parquet_read.f90).
    subroutine test_sample_nan_fraction_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sample_nan_fraction", expect_abort=.true., &
            failure_message="a NaN sample_fraction was expected to abort", &
            required_stderr="sample_fraction must not be NaN")
    end subroutine test_sample_nan_fraction_aborts

    !> Chunked reads are disallowed on a reader opened with sample_fraction < 1.0 alone (no
    !> filter=) -- sampling shares filter_mask/parquet_reader_has_filter with filter=, so
    !> check_reader_no_filter's guard fires the same way it does for an active filter=.
    subroutine test_read_chunk_with_sample_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_chunk_with_sample", expect_abort=.true., &
            failure_message="a chunked read on a sampled reader was expected to error stop", &
            required_stderr="chunked reads are not supported on a reader opened with an active filter=/sample_fraction=")
    end subroutine test_read_chunk_with_sample_aborts

    !> parquet_reader_print_stat's "sample: fraction=... seed=..." line.
    subroutine test_print_stat_sampled_rows(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "print_stat_sampled_rows", expect_abort=.false., &
            failure_message="print_stat was expected to run cleanly on a sampled reader", &
            required_stderr="sample: fraction=0.4 seed=42")
    end subroutine test_print_stat_sampled_rows

    !> parquet_reader_set_sample's failure return (parquet_wrapper.cpp) surfaces as a clean
    !> Fortran error stop from parquet_apply_sample (parquet_read.f90), forced here via a
    !> debug-only hook since a genuine BooleanBuilder allocation failure isn't fixture-triggerable.
    subroutine test_sample_mask_build_error_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "sample_mask_build_error", expect_abort=.true., &
            failure_message="a forced sample-mask-build failure was expected to abort", &
            required_stderr="parquet_open_reader: forced debug error: sample mask build failed")
    end subroutine test_sample_mask_build_error_aborts

    !> parquet_reader_get_string_length's `default:` fallback for a column that isn't
    !> string-like/LIST-typed at all -- reached via a plain `throw`, not report_fatal_error, but
    !> still uncaught: terminates via std::terminate(), not a clean Fortran `error stop`.
    subroutine test_string_length_on_non_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "string_length_on_non_string_column", &
            expect_abort=.true., &
            failure_message="querying string length of a non-string column was expected to abort", &
            required_stderr="Column is not string-like")
    end subroutine test_string_length_on_non_string_column_aborts

    !> qc: miss: must be Null/NA (case-insensitive) or empty -- anything
    !> else is rejected as invalid qc-maml syntax before the parquet file is
    !> even touched.
    subroutine test_qc_maml_bad_miss_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_bad_miss_value", expect_abort=.true., &
            failure_message="an unrecognized qc: miss: value was expected to abort", &
            required_stderr="is not recognized")
    end subroutine test_qc_maml_bad_miss_value_aborts

    !> Two qc-maml fields: entries sharing the same name are ambiguous and
    !> rejected, the same as a schema-authoring maml already rejects a
    !> duplicate field name.
    subroutine test_qc_maml_duplicate_field_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_duplicate_field", expect_abort=.true., &
            failure_message="a duplicate qc-maml field name was expected to abort", &
            required_stderr="duplicate field name")
    end subroutine test_qc_maml_duplicate_field_aborts

    !> name is the one required attribute for a qc-maml field.
    subroutine test_qc_maml_missing_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_missing_name", expect_abort=.true., &
            failure_message="a qc-maml field missing 'name' was expected to abort", &
            required_stderr="missing required 'name'")
    end subroutine test_qc_maml_missing_name_aborts

    !> parquet_parse_qc_maml reuses the same section/sub-key name schema
    !> every other maml validation path checks -- an unknown qc: sub-key
    !> (here a typo, "minimum" instead of "min") is still caught.
    subroutine test_qc_maml_unknown_subkey_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_maml_unknown_subkey", expect_abort=.true., &
            failure_message="an unknown qc: sub-key was expected to abort", &
            required_stderr="unknown sub-key")
    end subroutine test_qc_maml_unknown_subkey_aborts

    !> qc being active must never change the existing strict-by-default Null
    !> behavior: reading a column with a genuine Null and no null_value=/
    !> is_valid= still aborts with the very same message as without any
    !> qc-maml at all.
    subroutine test_qc_existing_null_abort_unchanged_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_existing_null_abort_unchanged", expect_abort=.true., &
            failure_message="a genuine Null with no null_value=/is_valid= was expected to abort even with qc active", &
            required_stderr="column contains Null value(s), which is not supported: id")
    end subroutine test_qc_existing_null_abort_unchanged_aborts

    !> The DEFAULT read-time qc mode (qc_soft=.false., hard): an out-of-range
    !> value aborts the process (via report_fatal_error) rather than merely
    !> warning -- the diagnostic reaches stderr and names the offending column.
    subroutine test_qc_range_violation_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_hard_aborts", expect_abort=.true., &
            failure_message="an out-of-range value under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'ra'")
    end subroutine test_qc_range_violation_hard_aborts

    !> Same as test_qc_range_violation_hard_aborts, but proves run_qc_range_check's
    !> STRING/LARGE_STRING/STRING_VIEW branch under hard qc mode.
    subroutine test_qc_range_violation_string_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_string_hard_aborts", &
            expect_abort=.true., &
            failure_message="an out-of-range string value under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'sv'")
    end subroutine test_qc_range_violation_string_hard_aborts

    !> Same as test_qc_range_violation_hard_aborts, but proves run_qc_range_check's
    !> FLOAT/DOUBLE/DECIMAL* bounds-description formatting branch under hard qc mode.
    subroutine test_qc_range_violation_float_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_range_violation_float_hard_aborts", &
            expect_abort=.true., &
            failure_message="an out-of-range float value under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'fv'")
    end subroutine test_qc_range_violation_float_hard_aborts

    !> The DEFAULT read-time qc mode (qc_soft=.false., hard): an unexpected
    !> Null aborts even when is_valid= was passed (so the read itself would
    !> otherwise have succeeded).
    subroutine test_qc_null_violation_hard_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "qc_null_violation_hard_aborts", expect_abort=.true., &
            failure_message="an unexpected Null under the default hard qc mode was expected to abort", &
            required_stderr="qc hard check: qc violation for column 'id'")
    end subroutine test_qc_null_violation_hard_aborts

    !> parquet_write_column now catches a row-count mismatch itself and error
    !> stops with a dedicated message, instead of letting Arrow's own
    !> "table.Validate()" exception escape uncaught inside WriteTable.
    subroutine test_write_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_row_count_mismatch", expect_abort=.true., &
            failure_message="writing columns with mismatched row counts was expected to abort", &
            required_stderr="parquet_write_column: row count mismatch for column b: " // &
                "expected 5 rows (from an earlier column) but got 3")
    end subroutine test_write_row_count_mismatch_aborts

    !> parquet_read_column now catches a values-array/file row-count mismatch
    !> itself and error stops with a dedicated message, instead of letting the
    !> C++ side's own "nrows mismatch" check abort the process.
    subroutine test_read_row_count_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_row_count_mismatch", expect_abort=.true., &
            failure_message="reading a column into a wrong-size array was expected to abort", &
            required_stderr="parquet_read_column: row count mismatch for column a: " // &
                "file has 5 rows but the values array implies 3")
    end subroutine test_read_row_count_mismatch_aborts

    !> parquet_read_column now checks c_associated(reader%handle) itself and
    !> error stops, instead of dereferencing a null handle (a message-less
    !> SIGSEGV) inside the C++ ConcurrencyGuard.
    subroutine test_read_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_before_open", expect_abort=.true., &
            failure_message="reading from an unopened reader was expected to abort", &
            required_stderr="parquet_read_column: reader has not been opened (call parquet_open_reader first)")
    end subroutine test_read_before_open_aborts

    !> Same as test_read_before_open_aborts, but for the write side.
    subroutine test_write_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_before_open", expect_abort=.true., &
            failure_message="writing to an unopened writer was expected to abort", &
            required_stderr="parquet_write_column: writer has not been opened (call parquet_open_writer first)")
    end subroutine test_write_before_open_aborts

    !> Representative of the same guard now applied to every other
    !> reader-taking procedure (parquet_prefetch_columns, parquet_get_col_size,
    !> parquet_get_column_total_elements, parquet_get_string_length,
    !> parquet_read_array_row_mode, parquet_read_array_element_mode), not just
    !> parquet_read_column/parquet_get_nrows.
    subroutine test_get_nrows_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "get_nrows_before_open", expect_abort=.true., &
            failure_message="calling parquet_get_nrows on an unopened reader was expected to abort", &
            required_stderr="parquet_get_nrows: reader has not been opened (call parquet_open_reader first)")
    end subroutine test_get_nrows_before_open_aborts

    !> parquet_close_reader now error stops on a reader that was never
    !> opened, instead of silently no-oping.
    subroutine test_close_reader_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_reader_before_open", expect_abort=.true., &
            failure_message="closing a never-opened reader was expected to abort", &
            required_stderr="parquet_close_reader: reader has not been opened, or was already closed")
    end subroutine test_close_reader_before_open_aborts

    !> Same as test_close_reader_before_open_aborts, but for the writer side.
    subroutine test_close_writer_before_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_before_open", expect_abort=.true., &
            failure_message="closing a never-opened writer was expected to abort", &
            required_stderr="parquet_close_writer: writer has not been opened, or was already closed")
    end subroutine test_close_writer_before_open_aborts

    !> parquet_close_writer's missing-write abort names the schema as
    !> "internal:<table>" for a schema built via parquet_schema(...) (never
    !> loaded from a .maml file) -- see schema_init in src/parquet_metadata.f90
    !> and parquet_close_writer in src/parquet_write.f90.
    subroutine test_close_writer_missing_write_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_missing_write", expect_abort=.true., &
            failure_message="closing a writer with an unwritten enabled column was expected to abort", &
            required_stderr="parquet_close_writer: schema: internal:demo")
    end subroutine test_close_writer_missing_write_aborts

    !> Same as test_close_writer_missing_write_aborts, but for a schema built
    !> fully by hand (schema%maml%lines set directly, never going through
    !> %init/parquet_schema(...)) -- the missing-write abort must fall back
    !> to "(unnamed, built in-memory)" since such a schema's %maml%name is
    !> never set.
    subroutine test_close_writer_missing_write_unnamed_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "close_writer_missing_write_unnamed_schema", &
            expect_abort=.true., &
            failure_message="closing a hand-built-schema writer with an unwritten enabled column " // &
                "was expected to abort", &
            required_stderr="parquet_close_writer: schema: (unnamed, built in-memory)")
    end subroutine test_close_writer_missing_write_unnamed_schema_aborts

    !> parquet_read_column now validates the column name against the file's
    !> schema and error stops, instead of letting the C++ side's uncaught
    !> "Column not found" exception abort the process. Also checks the error
    !> names the file being read -- see reader_filename_suffix in
    !> src/parquet_read.f90.
    subroutine test_read_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_unknown_column", expect_abort=.true., &
            failure_message="reading an unknown column via parquet_read_column was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: not_a_real_column " // &
                "(file: test/fixtures/has_null.parquet)")
    end subroutine test_read_unknown_column_aborts

    !> A dotted struct-field path with a mid-path typo ("main.inr.age") is rejected the same way
    !> as any other unknown column name -- check_column_exists catches it before any read is
    !> attempted (struct_path_exists's schema-level walk in parquet_wrapper.cpp), same clean
    !> error_stop message class as test_read_unknown_column_aborts, not a crash.
    subroutine test_read_nested_struct_field_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_nested_struct_field_not_found", expect_abort=.true., &
            failure_message="reading a nested struct-field path with a mid-path typo was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: main.inr.age " // &
                "(file: test/fixtures/nested_struct.parquet)")
    end subroutine test_read_nested_struct_field_not_found_aborts

    !> A dotted path where a middle segment resolves to a scalar leaf rather than continuing to
    !> nest ("main.id.extra" -- "id" is int32, not a struct) is rejected as "not found", not a
    !> crash from treating a non-struct array as a StructArray.
    subroutine test_read_nested_struct_path_not_a_struct_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_nested_struct_path_not_a_struct", expect_abort=.true., &
            failure_message="reading a nested struct-field path through a non-struct segment was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: main.id.extra " // &
                "(file: test/fixtures/nested_struct.parquet)")
    end subroutine test_read_nested_struct_path_not_a_struct_aborts

    !> A dotted path that resolves exactly to an intermediate STRUCT ("main.inner", not a leaf)
    !> is rejected -- this library has no struct/record output type, so a path must always name a
    !> scalar/vector leaf column (struct_path_exists's terminal-type gate in parquet_wrapper.cpp).
    subroutine test_read_nested_struct_intermediate_not_leaf_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_nested_struct_intermediate_not_leaf", expect_abort=.true., &
            failure_message="reading a nested struct-field path resolving to an intermediate struct was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: main.inner " // &
                "(file: test/fixtures/nested_struct.parquet)")
    end subroutine test_read_nested_struct_intermediate_not_leaf_aborts

    !> Confirms (via a process-global disk-read counter, parquet_debug_get_physical_column_read_count
    !> -- see its own comment in parquet_wrapper.cpp) that reading two different leaf paths under
    !> the same physical top-level struct column only triggers one real disk read of that struct --
    !> i.e. struct-path resolution shares get_single_chunk_array's existing column_cache rather
    !> than re-reading per leaf path. This scenario itself error stops if the invariant is
    !> violated, so it is expected to exit cleanly here (expect_abort=.false.).
    subroutine test_nested_struct_shares_cached_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "nested_struct_shares_cached_read", expect_abort=.false., &
            failure_message="reading two leaves under the same struct should share one physical disk read")
    end subroutine test_nested_struct_shares_cached_read

    !> create_parquet_reader (parquet_wrapper.cpp) now checks Arrow's file-open
    !> status directly instead of calling ValueOrDie() unchecked, so a missing
    !> file aborts cleanly with a diagnostic instead of an unconditional abort
    !> with no context.
    subroutine test_open_reader_missing_file_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "open_reader_missing_file", expect_abort=.true., &
            failure_message="opening a nonexistent file for reading was expected to abort")
    end subroutine test_open_reader_missing_file_aborts

    !> parquet_open_reader(nrows=) implies check_positive=.true., so a filter
    !> matching zero rows must abort at open time, exactly like
    !> parquet_get_nrows(..., check_positive=.true.) would.
    subroutine test_open_reader_nrows_zero_rows_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "open_reader_nrows_zero_rows", expect_abort=.true., &
            failure_message="parquet_open_reader(nrows=) with a filter matching zero rows was expected to abort", &
            required_stderr="parquet_get_nrows: file test_run/open_reader_nrows_zero_rows.parquet " // &
                "has zero rows after filtering (3 total)")
    end subroutine test_open_reader_nrows_zero_rows_aborts

    subroutine test_open_writer_bad_path_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "open_writer_bad_path", expect_abort=.true., &
            failure_message="opening a writer at a path under a nonexistent directory was expected to abort")
    end subroutine test_open_writer_bad_path_aborts

    !> parquet_write_string_matrix_column previously had no array_size check
    !> (unlike the scalar parquet_write_string_column), silently truncating
    !> an over-length string in a fixed-size string matrix column.
    subroutine test_write_string_matrix_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_string_matrix_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string matrix column was expected to error stop")
    end subroutine test_write_string_matrix_exceeds_array_size_aborts

    !> Same check as test_write_string_matrix_exceeds_array_size_aborts, but
    !> for parquet_write_string_column's 1D/flat form (a flattened
    !> string-vector column written as values(:) rather than values(:,:)).
    subroutine test_write_string_exceeds_array_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_string_exceeds_array_size", expect_abort=.true., &
            failure_message="writing an over-length string into a fixed-size string vector column " // &
                "(flat form) was expected to error stop")
    end subroutine test_write_string_exceeds_array_size_aborts

    subroutine test_validate_protected_cols_unknown_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_protected_cols_unknown_name", expect_abort=.true., &
            failure_message="protected_cols: referencing a column not declared in fields: " // &
                "was expected to error stop")
    end subroutine test_validate_protected_cols_unknown_name_aborts

    subroutine test_write_protected_column_with_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_protected_column_with_null", expect_abort=.true., &
            failure_message="writing a Null (via is_valid) into a protected column was expected to error stop")
    end subroutine test_write_protected_column_with_null_aborts

    subroutine test_validate_qc_min_not_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_not_numeric", expect_abort=.true., &
            failure_message="qc: min: value that does not parse as a number was expected to error stop")
    end subroutine test_validate_qc_min_not_numeric_aborts

    subroutine test_validate_qc_max_not_numeric_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_max_not_numeric", expect_abort=.true., &
            failure_message="qc: max: value that does not parse as a number was expected to error stop")
    end subroutine test_validate_qc_max_not_numeric_aborts

    subroutine test_validate_qc_min_non_integral_for_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_non_integral_for_int32", expect_abort=.true., &
            failure_message="qc: min: value with a fractional part on an int32 field was expected to error stop")
    end subroutine test_validate_qc_min_non_integral_for_int32_aborts

    subroutine test_validate_qc_min_out_of_int32_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_out_of_int32_range", expect_abort=.true., &
            failure_message="qc: min: value out of int32 range was expected to error stop")
    end subroutine test_validate_qc_min_out_of_int32_range_aborts

    subroutine test_validate_qc_min_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_min_wrong_operator", expect_abort=.true., &
            failure_message="qc: min: with a reversed (</<=) operator was expected to error stop")
    end subroutine test_validate_qc_min_wrong_operator_aborts

    subroutine test_validate_qc_max_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_qc_max_wrong_operator", expect_abort=.true., &
            failure_message="qc: max: with a reversed (>/>=) operator was expected to error stop")
    end subroutine test_validate_qc_max_wrong_operator_aborts

    subroutine test_qc_maml_min_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "qc_maml_min_wrong_operator", expect_abort=.true., &
            failure_message="qc-maml qc: min: with a reversed (</<=) operator was expected to error stop")
    end subroutine test_qc_maml_min_wrong_operator_aborts

    subroutine test_qc_maml_max_wrong_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "qc_maml_max_wrong_operator", expect_abort=.true., &
            failure_message="qc-maml qc: max: with a reversed (>/>=) operator was expected to error stop")
    end subroutine test_qc_maml_max_wrong_operator_aborts

    subroutine test_schema_add_field_qc_max_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_max_operator_without_value", expect_abort=.true., &
            failure_message="schema%add_field with a qc_max operator but no value was expected to error stop")
    end subroutine test_schema_add_field_qc_max_operator_without_value_aborts

    subroutine test_add_col_qc_min_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_min_reversed_operator", expect_abort=.true., &
            failure_message="add_col_qc with a reversed min operator was expected to error stop")
    end subroutine test_add_col_qc_min_reversed_operator_aborts

    subroutine test_add_col_qc_max_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_max_reversed_operator", expect_abort=.true., &
            failure_message="add_col_qc with a reversed max operator was expected to error stop")
    end subroutine test_add_col_qc_max_reversed_operator_aborts

    subroutine test_add_col_qc_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_operator_without_value", expect_abort=.true., &
            failure_message="add_col_qc with an operator but no value was expected to error stop")
    end subroutine test_add_col_qc_operator_without_value_aborts

    subroutine test_add_col_qc_max_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_max_operator_without_value", expect_abort=.true., &
            failure_message="add_col_qc with a max operator but no value was expected to error stop")
    end subroutine test_add_col_qc_max_operator_without_value_aborts

    subroutine test_add_col_qc_bad_miss_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_bad_miss_value", expect_abort=.true., &
            failure_message="add_col_qc with an invalid miss value was expected to error stop")
    end subroutine test_add_col_qc_bad_miss_value_aborts

    subroutine test_add_col_qc_too_many_fields_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_too_many_fields", expect_abort=.true., &
            failure_message="add_col_qc with more than four fields was expected to error stop")
    end subroutine test_add_col_qc_too_many_fields_aborts

    subroutine test_add_col_qc_empty_column_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_empty_column_name", expect_abort=.true., &
            failure_message="add_col_qc with an empty column name was expected to error stop")
    end subroutine test_add_col_qc_empty_column_name_aborts

    subroutine test_add_col_qc_duplicate_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_duplicate_column", expect_abort=.true., &
            failure_message="add_col_qc with a duplicate column name was expected to error stop")
    end subroutine test_add_col_qc_duplicate_column_aborts

    !> A pre-existing "- name: 'dup'" entry (single-quoted) must still be
    !> recognized as declaring "dup" -- the duplicate-name check strips a
    !> single pair of surrounding quotes before comparing.
    subroutine test_add_col_qc_duplicate_column_single_quoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_duplicate_column_single_quoted", expect_abort=.true., &
            failure_message="add_col_qc with a duplicate column name behind a single-quoted " // &
                "existing entry was expected to error stop")
    end subroutine test_add_col_qc_duplicate_column_single_quoted_aborts

    !> Same as above, for a double-quoted existing entry ('- name: "dup"').
    subroutine test_add_col_qc_duplicate_column_double_quoted_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_col_qc_duplicate_column_double_quoted", expect_abort=.true., &
            failure_message="add_col_qc with a duplicate column name behind a double-quoted " // &
                "existing entry was expected to error stop")
    end subroutine test_add_col_qc_duplicate_column_double_quoted_aborts

    subroutine test_set_col_qc_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_col_qc_reversed_operator", expect_abort=.true., &
            failure_message="set_col_qc with a reversed min operator was expected to error stop")
    end subroutine test_set_col_qc_reversed_operator_aborts

    !> Also checks the error now names the column, output file and maml --
    !> previously it named none of those (see writer_context_suffix in
    !> src/parquet_write.f90).
    subroutine test_write_values_not_divisible_by_col_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_values_not_divisible_by_col_size", expect_abort=.true., &
            failure_message="writing a values(:) array whose length isn't divisible by col_size was expected to error stop", &
            required_stderr="parquet_write_int32_column: values size is not divisible by col_size for column v " // &
                "(file: test_run/error_scenario_col_size_mismatch.parquet, maml: col_size_mismatch.maml)")
    end subroutine test_write_values_not_divisible_by_col_size_aborts

    subroutine test_write_int64_to_int32_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_int64_to_int32_overflow", expect_abort=.true., &
            failure_message="writing an out-of-int32-range int64 value to an int32 schema column " // &
                "was expected to error stop", &
            required_stderr="parquet_write_column: int64 value out of int32 range for column v")
    end subroutine test_write_int64_to_int32_overflow_aborts

    subroutine test_write_float_to_int32_non_integral_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int32_non_integral", expect_abort=.true., &
            failure_message="writing a non-integral float64 value to an int32 schema column was expected to error stop", &
            required_stderr="parquet_write_column: non-integral float value written to int column v")
    end subroutine test_write_float_to_int32_non_integral_aborts

    subroutine test_write_float_to_int32_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int32_out_of_range", expect_abort=.true., &
            failure_message="writing an out-of-int32-range float64 value to an int32 schema column " // &
                "was expected to error stop", &
            required_stderr="parquet_write_column: float value out of int32 range for column v")
    end subroutine test_write_float_to_int32_out_of_range_aborts

    subroutine test_write_float_to_int64_non_integral_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int64_non_integral", expect_abort=.true., &
            failure_message="writing a non-integral float64 value to an int64 schema column was expected to error stop", &
            required_stderr="parquet_write_column: non-integral float value written to int column v")
    end subroutine test_write_float_to_int64_non_integral_aborts

    subroutine test_write_float_to_int64_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "write_float_to_int64_out_of_range", expect_abort=.true., &
            failure_message="writing an out-of-int64-range float64 value to an int64 schema column " // &
                "was expected to error stop", &
            required_stderr="parquet_write_column: float value out of int64 range for column v")
    end subroutine test_write_float_to_int64_out_of_range_aborts

    subroutine test_set_max_threads_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_max_threads_below_one", expect_abort=.true., &
            failure_message="parquet_set_max_threads(0) was expected to error stop")
    end subroutine test_set_max_threads_below_one_aborts

    !> These two concurrency tests are self-adapting: the race they check can
    !> only occur when this build genuinely runs multi-threaded (OpenMP flag
    !> supplied via FPM_FFLAGS, and more than one thread available). When it
    !> doesn't -- plain `fpm test` with no OpenMP flag, or OMP_NUM_THREADS=1 --
    !> the shared-reader/writer race cannot happen, the guard cannot fire, and
    !> running the scenario would just report a misleading failure. So we skip
    !> (pass trivially) when omp_get_max_threads() <= 1, and only assert the
    !> abort when real concurrency is available. The subprocess spawned by the
    !> scenario inherits the same build/environment, so its thread count
    !> matches what we observe here.
    subroutine test_concurrent_calls_into_shared_reader_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: nthreads

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) return   ! no real concurrency -> skip (pass)

        ! README's Thread safety section explicitly promises a diagnostic on
        ! stderr for this case, not just a bare abort -- check that promise
        ! from the same run as the exit-status check, rather than re-running
        ! the (race-dependent) scenario a second time.
        call check_scenario_exit_status_and_stderr(error, "concurrent_calls_into_shared_reader", &
            expect_abort=.true., &
            failure_message="concurrent parquet_read_column calls into one shared reader were expected to abort", &
            required_stderr="concurrent access to a single parquet_reader detected")
    end subroutine test_concurrent_calls_into_shared_reader_aborts

    subroutine test_concurrent_calls_into_shared_writer_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        integer :: nthreads

        nthreads = 1
        !$ nthreads = omp_get_max_threads()
        if (nthreads <= 1) return   ! no real concurrency -> skip (pass)

        call check_scenario_exit_status_and_stderr(error, "concurrent_calls_into_shared_writer", &
            expect_abort=.true., &
            failure_message="concurrent parquet_write_column calls into one shared writer were expected to abort", &
            required_stderr="concurrent access to a single parquet_writer detected")
    end subroutine test_concurrent_calls_into_shared_writer_aborts

    !> Resolves the path to the built error_scenarios helper binary, building
    !> it (via `fpm build --tests`, which builds every test executable
    !> including run_tester itself, so this is a fast up-to-date check on the
    !> common path) on first use and caching the result for the rest of this
    !> process. This suite and test_writing.f90's qc-warning scenario tests
    !> together spawn well over a hundred scenario subprocesses; going
    !> through a full `fpm test error_scenarios -- ...` invocation for each
    !> one (as this used to do) re-parses the manifest and re-checks
    !> dependencies every single time -- about 2s of pure fpm overhead per
    !> call, dwarfing the ~0.05s the scenario itself takes to run. Resolving
    !> the binary path once and invoking it directly cuts that to a single
    !> ~1.5s build check for the whole suite. `find` is scoped to
    !> $FPM_BUILD_DIR (defaulting to "build", fpm's own default) rather than
    !> a bare "build" -- tools/coverage.sh builds under build/gcov via its
    !> own FPM_BUILD_DIR, and searching the whole build/ tree regardless of
    !> that would risk picking up whichever of the two happens to sort
    !> first, not the one actually requested for this run.
    function error_scenarios_bin() result(bin)
        character(len=:), allocatable :: bin
        character(len=*), parameter :: path_file = "test_run/.error_scenarios_bin_path"
        integer :: unit, ios
        character(len=1024) :: line

        if (.not. g_error_scenarios_bin_ready) then
            call execute_command_line("mkdir -p test_run", wait=.true.)
            call execute_command_line("fpm build --tests > /dev/null 2>&1", wait=.true.)
            call execute_command_line( &
                "find ""${FPM_BUILD_DIR:-build}"" -type f -name error_scenarios 2>/dev/null | head -n 1 > "// &
                path_file, &
                wait=.true.)
            line = ""
            open(newunit=unit, file=path_file, status="old", action="read", iostat=ios)
            if (ios == 0) then
                read(unit, '(a)', iostat=ios) line
                close(unit)
            end if
            g_error_scenarios_bin = trim(adjustl(line))
            g_error_scenarios_bin_ready = .true.
        end if
        bin = g_error_scenarios_bin
    end function error_scenarios_bin

    !> Runs the error_scenarios helper binary as `<binary> <scenario>
    !> <redirect>`, resolving/building it via error_scenarios_bin on first
    !> use. Centralizes what used to be six near-identical
    !> `execute_command_line("fpm test error_scenarios -- ...")` call sites
    !> (three here, three in test_writing.f90's qc-warning scenario tests).
    subroutine run_error_scenario(scenario, redirect, exitstat, cmdstat)
        character(len=*), intent(in) :: scenario, redirect
        integer, intent(out) :: exitstat, cmdstat
        character(len=:), allocatable :: bin

        bin = error_scenarios_bin()
        if (len_trim(bin) == 0) then
            cmdstat = 1
            exitstat = -1
            return
        end if
        call execute_command_line(trim(bin)//" "//trim(scenario)//" "//redirect, &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)
    end subroutine run_error_scenario

    subroutine check_scenario_exit_status(error, scenario, expect_abort, failure_message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message
        logical, intent(in) :: expect_abort
        integer :: exitstat, cmdstat
        logical :: aborted

        call run_error_scenario(scenario, "> /dev/null 2>&1", exitstat, cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return

        ! exitstat == 97 means error_scenarios.f90's `case default` was hit --
        ! i.e. `scenario` doesn't match any case there (a typo, or a case
        ! renamed on one side but not the other). Distinguishing this from a
        ! genuine abort (which this scenario name should never produce) is
        ! the whole point of that distinctive exit code -- otherwise this
        ! check could pass "by accident" while silently testing nothing.
        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
    end subroutine check_scenario_exit_status

    !> Like check_scenario_exit_status, but also asserts stderr (captured
    !> from the same single run) contains `required_stderr`. Used only where
    !> the diagnostic text itself is an explicit documented guarantee (e.g.
    !> the concurrency guard's stderr message in the README's Thread safety
    !> section) -- not applied broadly to every scenario, since most
    !> diagnostic wording isn't a documented contract and shouldn't be
    !> locked down by regression tests.
    subroutine check_scenario_exit_status_and_stderr(error, scenario, expect_abort, failure_message, required_stderr)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message, required_stderr
        logical, intent(in) :: expect_abort
        character(len=:), allocatable :: out_file
        integer :: exitstat, cmdstat, unit, ios
        character(len=512) :: line
        logical :: aborted, found

        out_file = "test_run/" // trim(scenario) // "_stderr.txt"

        call run_error_scenario(scenario, "> " // out_file // " 2>&1", exitstat, cmdstat)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return

        ! See check_scenario_exit_status's identical check for why: exit
        ! code 97 means the scenario name itself was not recognized.
        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
        if (allocated(error)) return

        found = .false.
        open(newunit=unit, file=out_file, status="old", action="read", iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, required_stderr) > 0) found = .true.
            end do
            close(unit)
        end if

        call check(error, found, &
            "expected stderr to contain '" // trim(required_stderr) // "' for scenario '" // trim(scenario) // "'")
    end subroutine check_scenario_exit_status_and_stderr

    !> Like check_scenario_exit_status_and_stderr, but asserts `forbidden_text`
    !> is ABSENT from the captured (combined stdout+stderr) output instead of
    !> present -- used for read-time qc scenarios that must NOT print a
    !> WARNING (e.g. qc: miss: Null explicitly allowing Nulls, or qc=.false.
    !> suppressing a would-be violation).
    subroutine check_scenario_exit_status_and_no_output(error, scenario, expect_abort, failure_message, forbidden_text)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message, forbidden_text
        logical, intent(in) :: expect_abort
        character(len=:), allocatable :: out_file
        integer :: exitstat, cmdstat, unit, ios
        character(len=512) :: line
        logical :: aborted, found

        out_file = "test_run/" // trim(scenario) // "_stdout.txt"

        call run_error_scenario(scenario, "> " // out_file // " 2>&1", exitstat, cmdstat)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return

        call check(error, exitstat /= 97, &
            "scenario name not recognized by error_scenarios.f90 (typo?): "//trim(scenario))
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
        if (allocated(error)) return

        found = .false.
        open(newunit=unit, file=out_file, status="old", action="read", iostat=ios)
        if (ios == 0) then
            do
                read(unit, '(a)', iostat=ios) line
                if (ios /= 0) exit
                if (index(line, forbidden_text) > 0) found = .true.
            end do
            close(unit)
        end if

        call check(error, .not. found, &
            "expected output to NOT contain '" // trim(forbidden_text) // "' for scenario '" // trim(scenario) // "'")
    end subroutine check_scenario_exit_status_and_no_output
    !
    ! ------------------------------------------------------------------------------
    ! parquet_temporal (parquet_date/parquet_time/parquet_timestamp) abort paths
    ! ------------------------------------------------------------------------------
    !
    subroutine test_temporal_date_set_invalid_month_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_invalid_month", expect_abort=.true., &
            failure_message="parquet_date%set with month 13 was expected to abort", &
            required_stderr="parquet_temporal: invalid month in parquet_date%set")
    end subroutine test_temporal_date_set_invalid_month_aborts

    subroutine test_temporal_date_set_invalid_day_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_invalid_day", expect_abort=.true., &
            failure_message="parquet_date%set with 1900-02-29 was expected to abort", &
            required_stderr="parquet_temporal: invalid day of month in parquet_date%set")
    end subroutine test_temporal_date_set_invalid_day_aborts

    subroutine test_temporal_date_set_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_out_of_range", expect_abort=.true., &
            failure_message="parquet_date%set beyond the int32 day range was expected to abort", &
            required_stderr="parquet_temporal: date out of range in parquet_date%set")
    end subroutine test_temporal_date_set_out_of_range_aborts

    subroutine test_temporal_date_set_mjd_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_set_mjd_out_of_range", expect_abort=.true., &
            failure_message="parquet_date%set_mjd beyond the int32 day range was expected to abort", &
            required_stderr="parquet_temporal: MJD out of range in parquet_date%set_mjd")
    end subroutine test_temporal_date_set_mjd_out_of_range_aborts

    subroutine test_temporal_date_parse_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_parse_invalid", expect_abort=.true., &
            failure_message="parquet_date%parse failure without success= was expected to abort", &
            required_stderr="parquet_temporal: not a valid ISO-8601 date (expected YYYY-MM-DD) in parquet_date%parse")
    end subroutine test_temporal_date_parse_invalid_aborts

    subroutine test_temporal_date_null_get_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_null_get", expect_abort=.true., &
            failure_message="parquet_date%get on a null element was expected to abort", &
            required_stderr="parquet_temporal: null parquet_date element accessed in get")
    end subroutine test_temporal_date_null_get_aborts

    subroutine test_temporal_date_null_comparison_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_null_comparison", expect_abort=.true., &
            failure_message="comparing against a null parquet_date was expected to abort", &
            required_stderr="parquet_temporal: comparison with a null parquet_date element")
    end subroutine test_temporal_date_null_comparison_aborts

    subroutine test_temporal_time_set_invalid_hour_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_set_invalid_hour", expect_abort=.true., &
            failure_message="parquet_time%set with hour 24 was expected to abort", &
            required_stderr="parquet_temporal: invalid hour in parquet_time%set")
    end subroutine test_temporal_time_set_invalid_hour_aborts

    subroutine test_temporal_time_set_invalid_nanosecond_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_set_invalid_nanosecond", expect_abort=.true., &
            failure_message="parquet_time%set with nanosecond 1e9 was expected to abort", &
            required_stderr="parquet_temporal: invalid nanosecond in parquet_time%set")
    end subroutine test_temporal_time_set_invalid_nanosecond_aborts

    subroutine test_temporal_time_set_raw_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_set_raw_out_of_range", expect_abort=.true., &
            failure_message="parquet_time%set_raw outside a day was expected to abort", &
            required_stderr="parquet_temporal: nanoseconds-of-day out of range in parquet_time%set_raw")
    end subroutine test_temporal_time_set_raw_out_of_range_aborts

    subroutine test_temporal_time_parse_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_parse_invalid", expect_abort=.true., &
            failure_message="parquet_time%parse failure without success= was expected to abort", &
            required_stderr="parquet_temporal: not a valid ISO-8601 time")
    end subroutine test_temporal_time_parse_invalid_aborts

    subroutine test_temporal_time_null_get_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_null_get", expect_abort=.true., &
            failure_message="parquet_time%hour on a null element was expected to abort", &
            required_stderr="parquet_temporal: null parquet_time element accessed in hour")
    end subroutine test_temporal_time_null_get_aborts

    subroutine test_temporal_ts_set_invalid_day_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_invalid_day", expect_abort=.true., &
            failure_message="parquet_timestamp%set with Feb 30 was expected to abort", &
            required_stderr="parquet_temporal: invalid day of month in parquet_timestamp%set")
    end subroutine test_temporal_ts_set_invalid_day_aborts

    subroutine test_temporal_ts_parse_invalid_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_parse_invalid", expect_abort=.true., &
            failure_message="parquet_timestamp%parse failure without success= was expected to abort", &
            required_stderr="parquet_temporal: not a valid ISO-8601 date-time")
    end subroutine test_temporal_ts_parse_invalid_aborts

    subroutine test_temporal_ts_null_to_unix_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_null_to_unix", expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix on a null element was expected to abort", &
            required_stderr="parquet_temporal: null parquet_timestamp element accessed in to_unix")
    end subroutine test_temporal_ts_null_to_unix_aborts

    subroutine test_temporal_ts_null_comparison_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_null_comparison", expect_abort=.true., &
            failure_message="comparing against a null parquet_timestamp was expected to abort", &
            required_stderr="parquet_temporal: comparison with a null parquet_timestamp element")
    end subroutine test_temporal_ts_null_comparison_aborts

    subroutine test_temporal_ts_to_unix_precision_loss_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_unix_precision_loss", expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix with sub-unit precision was expected to abort", &
            required_stderr="parquet_temporal: precision loss in parquet_timestamp%to_unix")
    end subroutine test_temporal_ts_to_unix_precision_loss_aborts

    subroutine test_temporal_ts_to_unix_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_unix_overflow", expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix overflowing int64 was expected to abort", &
            required_stderr="parquet_temporal: overflow in parquet_timestamp%to_unix")
    end subroutine test_temporal_ts_to_unix_overflow_aborts

    subroutine test_temporal_ts_set_mjd_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_mjd_out_of_range", expect_abort=.true., &
            failure_message="parquet_timestamp%set_mjd beyond the int64 seconds range was expected to abort", &
            required_stderr="parquet_temporal: MJD out of range in parquet_timestamp%set_mjd")
    end subroutine test_temporal_ts_set_mjd_out_of_range_aborts

    subroutine test_temporal_ts_set_raw_invalid_nanoseconds_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_raw_invalid_nanoseconds", &
            expect_abort=.true., &
            failure_message="parquet_timestamp%set_raw with a negative nanosecond part was expected to abort", &
            required_stderr="parquet_temporal: nanosecond part out of range in parquet_timestamp%set_raw")
    end subroutine test_temporal_ts_set_raw_invalid_nanoseconds_aborts

    subroutine test_temporal_invalid_time_unit_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_invalid_time_unit", expect_abort=.true., &
            failure_message="an invalid time-unit selector was expected to abort", &
            required_stderr="parquet_temporal: invalid time unit")
    end subroutine test_temporal_invalid_time_unit_aborts

    subroutine test_temporal_ts_get_year_overflow_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_get_year_overflow", expect_abort=.true., &
            failure_message="parquet_timestamp%get with a year beyond int32 was expected to abort", &
            required_stderr="parquet_temporal: year out of integer(int32) range in parquet_timestamp%get")
    end subroutine test_temporal_ts_get_year_overflow_aborts

    !> C++-side precision guard (build_time_array), distinct from parquet_timestamp%to_unix's own
    !> Fortran-side guard (see test_temporal_ts_to_unix_precision_loss_aborts, above).
    subroutine test_temporal_write_time_precision_loss_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_time_precision_loss", expect_abort=.true., &
            failure_message="writing a sub-millisecond time value into a time[ms] column was expected to abort", &
            required_stderr="time value has finer precision than the column's declared unit")
    end subroutine test_temporal_write_time_precision_loss_aborts

    subroutine test_temporal_protected_col_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_protected_col_null", expect_abort=.true., &
            failure_message="writing a Null into a protected timestamp column was expected to abort", &
            required_stderr="column 'ev' is protected")
    end subroutine test_temporal_protected_col_null_aborts

    subroutine test_temporal_read_date_via_int32_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_read_date_via_int32", expect_abort=.true., &
            failure_message="reading a date column via parquet_read_int32_column was expected to abort", &
            required_stderr="expected int32/int64, got date32")
    end subroutine test_temporal_read_date_via_int32_aborts

    subroutine test_temporal_read_int32_via_date_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_read_int32_via_date", expect_abort=.true., &
            failure_message="reading an int32 column via parquet_read_date_column was expected to abort", &
            required_stderr="expected date, got int32")
    end subroutine test_temporal_read_int32_via_date_aborts

    subroutine test_temporal_date_eq_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_eq_null", expect_abort=.true., &
            failure_message="parquet_date operator(==) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_date element")
    end subroutine test_temporal_date_eq_null_aborts

    subroutine test_temporal_time_get_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_get_null", expect_abort=.true., &
            failure_message="parquet_time%get on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in get")
    end subroutine test_temporal_time_get_null_aborts

    subroutine test_temporal_time_minute_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_minute_null", expect_abort=.true., &
            failure_message="parquet_time%minute on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in minute")
    end subroutine test_temporal_time_minute_null_aborts

    subroutine test_temporal_time_second_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_second_null", expect_abort=.true., &
            failure_message="parquet_time%second on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in second")
    end subroutine test_temporal_time_second_null_aborts

    subroutine test_temporal_time_nanosecond_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_nanosecond_null", expect_abort=.true., &
            failure_message="parquet_time%nanosecond on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in nanosecond")
    end subroutine test_temporal_time_nanosecond_null_aborts

    subroutine test_temporal_time_eq_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_eq_null", expect_abort=.true., &
            failure_message="parquet_time operator(==) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_time element")
    end subroutine test_temporal_time_eq_null_aborts

    subroutine test_temporal_time_lt_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_lt_null", expect_abort=.true., &
            failure_message="parquet_time operator(<) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_time element")
    end subroutine test_temporal_time_lt_null_aborts

    subroutine test_temporal_date_year_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_year_null", expect_abort=.true., &
            failure_message="parquet_date%year on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in year")
    end subroutine test_temporal_date_year_null_aborts

    subroutine test_temporal_date_month_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_month_null", expect_abort=.true., &
            failure_message="parquet_date%month on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in month")
    end subroutine test_temporal_date_month_null_aborts

    subroutine test_temporal_date_day_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_day_null", expect_abort=.true., &
            failure_message="parquet_date%day on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in day")
    end subroutine test_temporal_date_day_null_aborts

    subroutine test_temporal_date_to_mjd_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_to_mjd_null", expect_abort=.true., &
            failure_message="parquet_date%to_mjd on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in to_mjd")
    end subroutine test_temporal_date_to_mjd_null_aborts

    subroutine test_temporal_date_to_string_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_date_to_string_null", expect_abort=.true., &
            failure_message="parquet_date%to_string on a null element was expected to abort", &
            required_stderr="null parquet_date element accessed in to_string")
    end subroutine test_temporal_date_to_string_null_aborts

    subroutine test_temporal_time_to_string_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_time_to_string_null", expect_abort=.true., &
            failure_message="parquet_time%to_string on a null element was expected to abort", &
            required_stderr="null parquet_time element accessed in to_string")
    end subroutine test_temporal_time_to_string_null_aborts

    subroutine test_temporal_ts_set_invalid_month_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_invalid_month", expect_abort=.true., &
            failure_message="parquet_timestamp%set with month 13 was expected to abort", &
            required_stderr="invalid month in parquet_timestamp%set")
    end subroutine test_temporal_ts_set_invalid_month_aborts

    subroutine test_temporal_ts_set_invalid_nanosecond_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_set_invalid_nanosecond", expect_abort=.true., &
            failure_message="parquet_timestamp%set with nanosecond 1e9 was expected to abort", &
            required_stderr="invalid nanosecond in parquet_timestamp%set")
    end subroutine test_temporal_ts_set_invalid_nanosecond_aborts

    subroutine test_temporal_ts_get_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_get_null", expect_abort=.true., &
            failure_message="parquet_timestamp%get on a null element was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in get")
    end subroutine test_temporal_ts_get_null_aborts

    subroutine test_temporal_ts_get_date_range_exceeded_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_get_date_range_exceeded", &
            expect_abort=.true., &
            failure_message="parquet_timestamp%get_date beyond parquet_date's range was expected to abort", &
            required_stderr="date part out of parquet_date range in parquet_timestamp%get_date")
    end subroutine test_temporal_ts_get_date_range_exceeded_aborts

    subroutine test_temporal_ts_to_unix_overflow_negative_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_unix_overflow_negative", &
            expect_abort=.true., &
            failure_message="parquet_timestamp%to_unix negative-branch overflow was expected to abort", &
            required_stderr="overflow in parquet_timestamp%to_unix")
    end subroutine test_temporal_ts_to_unix_overflow_negative_aborts

    subroutine test_temporal_ts_to_mjd_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_mjd_null", expect_abort=.true., &
            failure_message="parquet_timestamp%to_mjd on a null element was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in to_mjd")
    end subroutine test_temporal_ts_to_mjd_null_aborts

    subroutine test_temporal_ts_to_string_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_to_string_null", expect_abort=.true., &
            failure_message="parquet_timestamp%to_string on a null element was expected to abort", &
            required_stderr="null parquet_timestamp element accessed in to_string")
    end subroutine test_temporal_ts_to_string_null_aborts

    subroutine test_temporal_ts_lt_null_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_ts_lt_null", expect_abort=.true., &
            failure_message="parquet_timestamp operator(<) with a null operand was expected to abort", &
            required_stderr="comparison with a null parquet_timestamp element")
    end subroutine test_temporal_ts_lt_null_aborts

    subroutine test_temporal_write_column_not_defined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_column_not_defined", expect_abort=.true., &
            failure_message="writing an undeclared temporal column was expected to abort", &
            required_stderr="column not defined in parquet_open_writer: not_a_real_column")
    end subroutine test_temporal_write_column_not_defined_aborts

    subroutine test_temporal_write_array_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_array_size_mismatch", &
            expect_abort=.true., &
            failure_message="writing a temporal vector column with the wrong col_size was expected to abort", &
            required_stderr="array size mismatch for column ev")
    end subroutine test_temporal_write_array_size_mismatch_aborts

    subroutine test_temporal_write_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_write_type_mismatch", expect_abort=.true., &
            failure_message="writing a timestamp array into a date-declared column was expected to abort", &
            required_stderr="type mismatch for column day (expected timestamp, got date)")
    end subroutine test_temporal_write_type_mismatch_aborts

    subroutine test_temporal_chunk_column_not_defined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_chunk_column_not_defined", expect_abort=.true., &
            failure_message="chunk-writing an undeclared temporal column was expected to abort", &
            required_stderr="parquet_write_column_chunk: column not defined")
    end subroutine test_temporal_chunk_column_not_defined_aborts

    subroutine test_temporal_chunk_array_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_chunk_array_size_mismatch", &
            expect_abort=.true., &
            failure_message="chunk-writing a temporal vector column with the wrong col_size was expected to abort", &
            required_stderr="parquet_write_column_chunk: array size mismatch for column ev")
    end subroutine test_temporal_chunk_array_size_mismatch_aborts

    subroutine test_temporal_chunk_type_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "temporal_chunk_type_mismatch", expect_abort=.true., &
            failure_message="chunk-writing a timestamp array into a date-declared column was expected to abort", &
            required_stderr="parquet_write_column_chunk: type mismatch for column day")
    end subroutine test_temporal_chunk_type_mismatch_aborts

    subroutine test_get_version_invalid_mode_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "get_version_invalid_mode", expect_abort=.true., &
            failure_message="parquet_get_version with an unrecognized mode was expected to abort", &
            required_stderr="parquet_get_version: invalid mode 'bogus'")
    end subroutine test_get_version_invalid_mode_aborts

    subroutine test_column_exists_bad_type_token_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_exists_bad_type_token", expect_abort=.true., &
            failure_message="parquet_column_exists with an unrecognized types= token was expected to abort", &
            required_stderr="parquet_column_exists: unrecognized data type token 'itn32'")
    end subroutine test_column_exists_bad_type_token_aborts

    subroutine test_column_exists_empty_type_filter_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "column_exists_empty_type_filter", expect_abort=.true., &
            failure_message="parquet_column_exists with a blank types= filter was expected to abort", &
            required_stderr="parquet_column_exists: types= must not be empty")
    end subroutine test_column_exists_empty_type_filter_aborts

    subroutine test_get_column_type_unsupported_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "get_column_type_unsupported", expect_abort=.true., &
            failure_message="parquet_get_column_type on a column outside the 9 canonical types was expected to abort", &
            required_stderr="parquet_get_column_type: column 'v_uint32' has an unsupported data type for this query")
    end subroutine test_get_column_type_unsupported_aborts

    subroutine test_mask_row_mask_after_write_started_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_after_write_started", expect_abort=.true., &
            failure_message="parquet_write_row_mask after the writer had started was expected to abort", &
            required_stderr="parquet_write_row_mask: must be called before the writer's first")
    end subroutine test_mask_row_mask_after_write_started_aborts

    subroutine test_mask_row_mask_shape_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_shape_mismatch", expect_abort=.true., &
            failure_message="a whole-column write with the wrong row count for the mask was expected to abort", &
            required_stderr="does not match the mask set via parquet_write_row_mask")
    end subroutine test_mask_row_mask_shape_mismatch_aborts

    subroutine test_mask_row_mask_zero_length_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_zero_length", expect_abort=.true., &
            failure_message="parquet_write_row_mask with a zero-length mask was expected to abort", &
            required_stderr="parquet_write_row_mask: mask must not be zero-length")
    end subroutine test_mask_row_mask_zero_length_aborts

    subroutine test_mask_row_mask_called_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_called_twice", expect_abort=.true., &
            failure_message="a second parquet_write_row_mask call for the same writer was expected to abort", &
            required_stderr="parquet_write_row_mask: already called for this writer")
    end subroutine test_mask_row_mask_called_twice_aborts

    subroutine test_mask_chunk_row_mask_after_row_mask_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_after_row_mask", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after parquet_write_row_mask was expected to abort", &
            required_stderr="cannot be used together with parquet_write_row_mask")
    end subroutine test_mask_chunk_row_mask_after_row_mask_aborts

    subroutine test_mask_row_mask_after_chunk_row_mask_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_after_chunk_row_mask", expect_abort=.true., &
            failure_message="parquet_write_row_mask after the writer had started row groups was expected to abort", &
            required_stderr="parquet_write_row_mask: must be called before the writer's first")
    end subroutine test_mask_row_mask_after_chunk_row_mask_aborts

    subroutine test_mask_chunk_row_mask_after_whole_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_after_whole_column", &
            expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after a whole-column write was expected to abort", &
            required_stderr="unavailable once a column has been written whole")
    end subroutine test_mask_chunk_row_mask_after_whole_column_aborts

    subroutine test_mask_chunk_row_mask_not_used_every_group_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_not_used_every_group", &
            expect_abort=.true., &
            failure_message="a row group without parquet_write_chunk_row_mask after an earlier one used it " // &
                "was expected to abort", &
            required_stderr="parquet_write_chunk_row_mask must be called for every row group")
    end subroutine test_mask_chunk_row_mask_not_used_every_group_aborts

    subroutine test_mask_chunk_row_mask_introduced_late_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_introduced_late", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after this row group's first chunk write " // &
                "was expected to abort", &
            required_stderr="must be called before this row group's first")
    end subroutine test_mask_chunk_row_mask_introduced_late_aborts

    subroutine test_mask_chunk_row_mask_called_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_called_twice", expect_abort=.true., &
            failure_message="a second parquet_write_chunk_row_mask call for the same row group was expected to abort", &
            required_stderr="parquet_write_chunk_row_mask: already called for this row group")
    end subroutine test_mask_chunk_row_mask_called_twice_aborts

    subroutine test_mask_chunk_row_mask_size_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_size_mismatch", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask with the wrong mask size was expected to abort", &
            required_stderr="mask size (3) does not match the open row group's own nrows (2)")
    end subroutine test_mask_chunk_row_mask_size_mismatch_aborts

    subroutine test_mask_row_mask_window_exhausted_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_window_exhausted", expect_abort=.true., &
            failure_message="a row group claiming more mask positions than parquet_write_row_mask provided " // &
                "was expected to abort", &
            required_stderr="would claim more positions of the mask")
    end subroutine test_mask_row_mask_window_exhausted_aborts

    subroutine test_mask_row_mask_not_fully_consumed_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_mask_not_fully_consumed", expect_abort=.true., &
            failure_message="closing a writer with an unconsumed parquet_write_row_mask tail was expected to abort", &
            required_stderr="was not fully consumed by this writer's row groups")
    end subroutine test_mask_row_mask_not_fully_consumed_aborts

    subroutine test_mask_chunk_row_mask_no_row_group_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_no_row_group_open", &
            expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask with no row group open was expected to abort", &
            required_stderr="parquet_write_chunk_row_mask: no row group is open")
    end subroutine test_mask_chunk_row_mask_no_row_group_open_aborts

    subroutine test_mask_chunk_row_mask_scheme_declined_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_chunk_row_mask_scheme_declined", expect_abort=.true., &
            failure_message="parquet_write_chunk_row_mask after an earlier row group declined it " // &
                "was expected to abort", &
            required_stderr="was not used for this writer's first row group")
    end subroutine test_mask_chunk_row_mask_scheme_declined_aborts

    subroutine test_mask_row_group_no_writes_at_all_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        call check_scenario_exit_status_and_stderr(error, "mask_row_group_no_writes_at_all", expect_abort=.true., &
            failure_message="finishing a row group with no column ever written for it was expected to abort", &
            required_stderr="no columns have been written")
    end subroutine test_mask_row_group_no_writes_at_all_aborts
    !
    ! Note: the temporal_foreign_int96_roundtrip/temporal_foreign_tz_roundtrip scenarios (clean
    ! exits, not aborts) are checked from test_reading.f90's test_datetime_foreign_fixtures, not
    ! here -- same convention as test_writing.f90's test_large_string_column_roundtrip/
    ! test_string_view_column_roundtrip, which also drive an out-of-process debug-hook fixture
    ! via check_scenario_exit_status but live in the suite whose read/write feature they exercise.
end module test_errors
