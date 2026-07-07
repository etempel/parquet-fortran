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
    !
    implicit none
    private
    public :: collect_tests_parquet_errors
    ! Exposed for test_writing.f90 to reuse (avoids a second copy of the same
    ! subprocess-driving helper -- see check_scenario_exit_status below).
    public :: check_scenario_exit_status
    public :: check_scenario_exit_status_and_stderr
    public :: check_scenario_exit_status_and_no_output
    !
contains
    !
    subroutine collect_tests_parquet_errors(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)

        testsuite = [ &
            new_unittest("control scenario exits cleanly", test_ok_scenario_exits_cleanly), &
            new_unittest("write to undeclared column aborts", test_write_undeclared_column_aborts), &
            new_unittest("write with type mismatch aborts", test_write_type_mismatch_aborts), &
            new_unittest("writing the same column twice aborts", test_write_column_twice_aborts), &
            new_unittest("writing the same column twice on a schema-less writer aborts", &
                test_write_column_twice_no_schema_aborts), &
            new_unittest("validating an invalid data_type aborts", test_validate_bad_data_type_aborts), &
            new_unittest("validating data_type: date aborts", test_validate_excluded_date_type_aborts), &
            new_unittest("validating data_type: timestamp aborts", test_validate_excluded_timestamp_type_aborts), &
            new_unittest("validating data_type: decimal aborts", test_validate_excluded_decimal_type_aborts), &
            new_unittest("validating a duplicate field name aborts", test_validate_duplicate_name_aborts), &
            new_unittest("validating a MAML without table: aborts", test_validate_missing_table_aborts), &
            new_unittest("validating a MAML without fields aborts", test_validate_no_fields_aborts), &
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
            new_unittest("get_column_index on unknown column aborts", test_get_column_index_not_found_aborts), &
            new_unittest("write_maml without metadata aborts", test_write_maml_without_metadata_aborts), &
            new_unittest("reading a column with genuine Null values aborts", &
                test_read_column_with_nulls_aborts), &
            new_unittest("reading a column of an unsupported physical type aborts", &
                test_read_unsupported_physical_type_aborts), &
            new_unittest("prefetching an unknown column aborts", &
                test_prefetch_unknown_column_aborts), &
            new_unittest("filter: unknown column aborts", &
                test_filter_unknown_column_aborts), &
            new_unittest("filter: vector column aborts", &
                test_filter_vector_column_aborts), &
            new_unittest("filter: malformed rule aborts", &
                test_filter_malformed_rule_aborts), &
            new_unittest("filter: non-numeric value against a numeric column aborts", &
                test_filter_bad_numeric_value_aborts), &
            new_unittest("filter: unquoted value against a string column aborts", &
                test_filter_unquoted_string_value_aborts), &
            new_unittest("filter: invalid boolean value aborts", &
                test_filter_bad_boolean_value_aborts), &
            new_unittest("filter: ordering comparison against a boolean column aborts", &
                test_filter_bool_ordering_not_supported_aborts), &
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
            new_unittest("reading an unknown column via parquet_read_column aborts", &
                test_read_unknown_column_aborts), &
            new_unittest("opening a nonexistent file for reading aborts", &
                test_open_reader_missing_file_aborts), &
            new_unittest("opening a writer at a bad path aborts", &
                test_open_writer_bad_path_aborts), &
            new_unittest("writing an over-length string into a fixed-size string matrix column aborts", &
                test_write_string_matrix_exceeds_array_size_aborts), &
            new_unittest("protected_cols: referencing an unknown field aborts", &
                test_validate_protected_cols_unknown_name_aborts), &
            new_unittest("writing a Null into a protected column aborts", &
                test_write_protected_column_with_null_aborts), &
            new_unittest("qc: min value that does not parse as a number aborts", &
                test_validate_qc_min_not_numeric_aborts), &
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
            new_unittest("writing values(:) not divisible by col_size aborts", &
                test_write_values_not_divisible_by_col_size_aborts), &
            new_unittest("parquet_set_max_threads(0) aborts", &
                test_set_max_threads_below_one_aborts), &
            new_unittest("concurrent calls into a shared parquet_reader abort", &
                test_concurrent_calls_into_shared_reader_aborts), &
            new_unittest("concurrent calls into a shared parquet_writer abort", &
                test_concurrent_calls_into_shared_writer_aborts) &
            ]
    end subroutine collect_tests_parquet_errors

    subroutine test_ok_scenario_exits_cleanly(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "ok", expect_abort=.false., &
            failure_message="control scenario 'ok' was expected to exit cleanly")
    end subroutine test_ok_scenario_exits_cleanly

    subroutine test_write_undeclared_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_undeclared_column", expect_abort=.true., &
            failure_message="writing an undeclared column was expected to error stop")
    end subroutine test_write_undeclared_column_aborts

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

    !> Locks in the specific type exclusions from the README's Limitations
    !> section (no date/timestamp/decimal support) so that accidentally
    !> adding one of these to the allowed type list would be caught here,
    !> not just a generic invalid-token check.
    subroutine test_validate_excluded_date_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_excluded_date_type", expect_abort=.true., &
            failure_message="'date' is documented as unsupported and was expected to error stop")
    end subroutine test_validate_excluded_date_type_aborts

    subroutine test_validate_excluded_timestamp_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_excluded_timestamp_type", expect_abort=.true., &
            failure_message="'timestamp' is documented as unsupported and was expected to error stop")
    end subroutine test_validate_excluded_timestamp_type_aborts

    subroutine test_validate_excluded_decimal_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_excluded_decimal_type", expect_abort=.true., &
            failure_message="'decimal' is documented as unsupported and was expected to error stop")
    end subroutine test_validate_excluded_decimal_type_aborts

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

    subroutine test_validate_col_map_duplicate_internal_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "validate_col_map_duplicate_internal", expect_abort=.true., &
            failure_message="col_map: a duplicate internal column key was expected to error stop")
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

    subroutine test_get_column_index_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_column_index_not_found", expect_abort=.true., &
            failure_message="get_column_index on an unknown column was expected to error stop")
    end subroutine test_get_column_index_not_found_aborts

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

    !> A non-numeric value against a numeric filter column (rule shape is
    !> fine, the value itself isn't) aborts with a message naming the value
    !> and the column, distinct from the shape-only rejection above.
    subroutine test_filter_bad_numeric_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bad_numeric_value", expect_abort=.true., &
            failure_message="a non-numeric filter value against a numeric column was expected to abort", &
            required_stderr="is not a valid integer for column 'id_with_null'")
    end subroutine test_filter_bad_numeric_value_aborts

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

    !> Ordering comparisons (>, >=, <, <=) aren't supported against a boolean
    !> filter column -- only ==//= are.
    subroutine test_filter_bool_ordering_not_supported_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "filter_bool_ordering_not_supported", expect_abort=.true., &
            failure_message="an ordering comparison against a boolean filter column was expected to abort", &
            required_stderr="ordering comparisons")
    end subroutine test_filter_bool_ordering_not_supported_aborts

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

    !> parquet_read_column now validates the column name against the file's
    !> schema and error stops, instead of letting the C++ side's uncaught
    !> "Column not found" exception abort the process.
    subroutine test_read_unknown_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "read_unknown_column", expect_abort=.true., &
            failure_message="reading an unknown column via parquet_read_column was expected to abort", &
            required_stderr="parquet_read_column: column not found in parquet file: not_a_real_column")
    end subroutine test_read_unknown_column_aborts

    !> create_parquet_reader (parquet_wrapper.cpp) now checks Arrow's file-open
    !> status directly instead of calling ValueOrDie() unchecked, so a missing
    !> file aborts cleanly with a diagnostic instead of an unconditional abort
    !> with no context.
    subroutine test_open_reader_missing_file_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "open_reader_missing_file", expect_abort=.true., &
            failure_message="opening a nonexistent file for reading was expected to abort")
    end subroutine test_open_reader_missing_file_aborts

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

    subroutine test_write_values_not_divisible_by_col_size_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "write_values_not_divisible_by_col_size", expect_abort=.true., &
            failure_message="writing a values(:) array whose length isn't divisible by col_size was expected to error stop")
    end subroutine test_write_values_not_divisible_by_col_size_aborts

    subroutine test_set_max_threads_below_one_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "set_max_threads_below_one", expect_abort=.true., &
            failure_message="parquet_set_max_threads(0) was expected to error stop")
    end subroutine test_set_max_threads_below_one_aborts

    subroutine test_concurrent_calls_into_shared_reader_aborts(error)
        type(error_type), allocatable, intent(out) :: error

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

        call check_scenario_exit_status_and_stderr(error, "concurrent_calls_into_shared_writer", &
            expect_abort=.true., &
            failure_message="concurrent parquet_write_column calls into one shared writer were expected to abort", &
            required_stderr="concurrent access to a single parquet_writer detected")
    end subroutine test_concurrent_calls_into_shared_writer_aborts

    subroutine check_scenario_exit_status(error, scenario, expect_abort, failure_message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message
        logical, intent(in) :: expect_abort
        integer :: exitstat, cmdstat
        logical :: aborted

        ! --features thread_safe (see fpm.toml) is forced here regardless
        ! of how the outer `fpm test` was invoked: execute_command_line
        ! spawns a brand new fpm process that only inherits environment
        ! variables, not the parent's own command-line flags, so without
        ! this the concurrent_calls_into_shared_reader/writer scenarios
        ! below would silently run single-threaded and never actually
        ! exercise the race they're meant to check. The actual OpenMP
        ! flag itself must come from the environment (e.g. FPM_FFLAGS),
        ! since it's compiler-dependent.
        call execute_command_line( &
            "fpm test error_scenarios --features thread_safe -- "//trim(scenario)//" > /dev/null 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
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

        call execute_command_line( &
            "fpm test error_scenarios --features thread_safe -- " // trim(scenario) // &
            " > " // out_file // " 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
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

        call execute_command_line( &
            "fpm test error_scenarios --features thread_safe -- " // trim(scenario) // &
            " > " // out_file // " 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
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
end module test_errors
