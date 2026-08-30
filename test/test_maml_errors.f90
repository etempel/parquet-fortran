!> Abort-path tests for `maml`, split out of `test_maml.f90` so that the
!> reader/writer runner forks no subprocesses at all.
!!
!! **Why these live apart from the tests they are about.** Every test here drives
!! `test/error_scenarios.f90` through `run_error_scenario`/`check_scenario_*`, i.e. it spawns a
!! subprocess and asserts on its exit status and stderr. That is the one thing `run_tester_cpp`
!! must not do -- see `feature_tests.md` section 5 -- so they gather in `run_tester_errors`
!! beside the 913 tests already there.
!!
!! **The cost is locality and it is real**: a test asserting that a bad maml call aborts is a
!! test *about maml*, and it no longer sits beside the maml tests. Run
!! `run_tester_errors -- maml_errors` to see them as a group.
module test_maml_errors
    use parquet
    use parquet_maml_base, only : parquet_maml_file, get_parquet_maml
    use iso_fortran_env, only : int32, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use test_errors, only : check_scenario_exit_status, check_scenario_exit_status_and_stderr
    !
    implicit none
    private
    public :: collect_tests_parquet_maml_errors

contains

    subroutine collect_tests_parquet_maml_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end: a single array constructor can exceed
        ! the standard's 255-continuation-line limit, which only nagfor enforces, and the
        ! self-referential `testsuite = [testsuite, ...]` form makes nagfor double-free each
        ! entry's name string at run time. See `test_writing.f90` for the full note.
        type(unittest_type), allocatable :: p1(:)
        !
        p1 = [ &
            new_unittest("schema%add_field before schema%init aborts", test_schema_add_field_before_init_aborts), &
            new_unittest("schema%init called twice aborts", test_schema_init_twice_aborts), &
            new_unittest("schema%init after a MAML parse (no force) aborts", &
                test_schema_init_after_maml_parse_aborts), &
            new_unittest("loading a .maml file into an already-initialized schema aborts", &
                test_parse_maml_file_after_init_aborts), &
            new_unittest("schema%init with an empty table aborts", test_schema_init_empty_table_aborts), &
            new_unittest("schema%add_field with an empty name aborts", test_schema_add_field_empty_name_aborts), &
            new_unittest("schema%add_field with a duplicate name aborts", &
                test_schema_add_field_duplicate_name_aborts), &
            new_unittest("schema%add_field with an invalid data_type aborts", &
                test_schema_add_field_invalid_data_type_aborts), &
            new_unittest("schema%add_field: reversed qc_min operator aborts", &
                test_schema_add_field_qc_min_reversed_operator_aborts), &
            new_unittest("schema%add_field: reversed qc_max operator aborts", &
                test_schema_add_field_qc_max_reversed_operator_aborts), &
            new_unittest("schema%add_field: qc operator with no value aborts", &
                test_schema_add_field_qc_operator_without_value_aborts), &
            new_unittest("schema%add_field: invalid qc_miss value aborts", &
                test_schema_add_field_bad_qc_miss_value_aborts), &
            new_unittest("schema%get_field(name=) on a non-existent field aborts", &
                test_get_field_by_name_not_found_aborts), &
            new_unittest("schema%get_field(index=) out of range aborts", &
                test_get_field_by_index_out_of_range_aborts), &
            new_unittest("schema%add_field_from on a non-existent source field aborts", &
                test_add_field_from_source_not_found_aborts), &
            new_unittest("schema%add_field: a unit suffix on date aborts", &
                test_schema_add_field_date_with_unit_aborts), &
            new_unittest("parquet_validate_maml rejects qc: on a temporal field", &
                test_validate_qc_on_temporal_column_aborts), &
            new_unittest("schema%add_field: an explicit seconds unit aborts (Parquet has no " // &
                "seconds-resolution TIME/TIMESTAMP encoding)", test_schema_add_field_seconds_unit_aborts), &
            new_unittest("schema%add_field: a ,utc suffix on time (not timestamp) aborts", &
                test_schema_add_field_time_utc_aborts), &
            new_unittest("schema%add_field: an unclosed unit bracket aborts", &
                test_schema_add_field_unclosed_bracket_aborts), &
            new_unittest("a malformed col_size: value aborts", test_col_size_malformed_value_aborts), &
            new_unittest("a malformed array_size: value aborts", test_array_size_malformed_value_aborts), &
            new_unittest("array_size: auto on a non-string column aborts", &
                test_array_size_auto_on_non_string_aborts), &
            new_unittest("set_col_size with a non-positive value aborts", test_set_col_size_non_positive_aborts), &
            new_unittest("set_col_size on an already-resolved column aborts without force=.true.", &
                test_set_col_size_already_resolved_no_force_aborts), &
            new_unittest("set_array_size on a non-string column aborts", &
                test_set_array_size_non_string_column_aborts), &
            new_unittest("set_array_size with a non-positive value aborts", &
                test_set_array_size_non_positive_aborts), &
            new_unittest("set_array_size on an already-resolved column aborts without force=.true.", &
                test_set_array_size_already_resolved_no_force_aborts) &
            ]
        !
        testsuite = p1
    end subroutine collect_tests_parquet_maml_errors

    subroutine test_schema_add_field_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_before_init", expect_abort=.true., &
            failure_message="schema%add_field before schema%init was expected to error stop")
    end subroutine test_schema_add_field_before_init_aborts

    subroutine test_schema_init_twice_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_init_twice", expect_abort=.true., &
            failure_message="calling schema%init twice was expected to error stop")
    end subroutine test_schema_init_twice_aborts

    subroutine test_schema_init_after_maml_parse_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_init_after_maml_parse", expect_abort=.true., &
            failure_message="schema%init on a MAML-parsed schema (without force) was expected to error stop")
    end subroutine test_schema_init_after_maml_parse_aborts

    subroutine test_parse_maml_file_after_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "parse_maml_file_after_init", expect_abort=.true., &
            failure_message="loading a .maml file into an already-initialized schema was expected to error stop")
    end subroutine test_parse_maml_file_after_init_aborts

    subroutine test_schema_init_empty_table_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_init_empty_table", expect_abort=.true., &
            failure_message="schema%init with an empty table was expected to error stop")
    end subroutine test_schema_init_empty_table_aborts

    subroutine test_schema_add_field_empty_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_empty_name", expect_abort=.true., &
            failure_message="schema%add_field with an empty name was expected to error stop")
    end subroutine test_schema_add_field_empty_name_aborts

    !> Also checks the error names which schema/maml it came from: schema%init
    !> (table="t" in this scenario) always gives an in-memory schema the name
    !> "internal:t" -- see maml_name_suffix in src/parquet_metadata.f90.
    subroutine test_schema_add_field_duplicate_name_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_duplicate_name", expect_abort=.true., &
            failure_message="schema%add_field with a duplicate field name was expected to error stop", &
            required_stderr="parquet_schema%add_field: duplicate field name 'ra' (maml: internal:t)")
    end subroutine test_schema_add_field_duplicate_name_aborts

    subroutine test_schema_add_field_invalid_data_type_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_invalid_data_type", expect_abort=.true., &
            failure_message="schema%add_field with an invalid data_type was expected to error stop")
    end subroutine test_schema_add_field_invalid_data_type_aborts

    subroutine test_schema_add_field_qc_min_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_min_reversed_operator", expect_abort=.true., &
            failure_message="schema%add_field with a reversed qc_min operator was expected to error stop")
    end subroutine test_schema_add_field_qc_min_reversed_operator_aborts

    subroutine test_schema_add_field_qc_max_reversed_operator_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_max_reversed_operator", expect_abort=.true., &
            failure_message="schema%add_field with a reversed qc_max operator was expected to error stop")
    end subroutine test_schema_add_field_qc_max_reversed_operator_aborts

    subroutine test_schema_add_field_qc_operator_without_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_qc_operator_without_value", expect_abort=.true., &
            failure_message="schema%add_field with a qc operator but no value was expected to error stop")
    end subroutine test_schema_add_field_qc_operator_without_value_aborts

    subroutine test_schema_add_field_bad_qc_miss_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "schema_add_field_bad_qc_miss_value", expect_abort=.true., &
            failure_message="schema%add_field with an invalid qc_miss value was expected to error stop")
    end subroutine test_schema_add_field_bad_qc_miss_value_aborts

    subroutine test_get_field_by_name_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_field_by_name_not_found", expect_abort=.true., &
            failure_message="schema%get_field(name=) on a non-existent field was expected to error stop")
    end subroutine test_get_field_by_name_not_found_aborts

    subroutine test_get_field_by_index_out_of_range_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_field_by_index_out_of_range", expect_abort=.true., &
            failure_message="schema%get_field(index=) out of range was expected to error stop")
    end subroutine test_get_field_by_index_out_of_range_aborts

    subroutine test_add_field_from_source_not_found_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "add_field_from_source_not_found", expect_abort=.true., &
            failure_message="schema%add_field_from with a non-existent source field was expected to error stop")
    end subroutine test_add_field_from_source_not_found_aborts

    !> "date" takes no unit -- a bracketed suffix on it is rejected the same way any other
    !> malformed data_type token is.
    subroutine test_schema_add_field_date_with_unit_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_date_with_unit", expect_abort=.true., &
            failure_message="schema%add_field with 'date[us]' was expected to error stop", &
            required_stderr="invalid data_type 'date[us]'")
    end subroutine test_schema_add_field_date_with_unit_aborts

    !> qc: min:/max: on a date/time/timestamp field is deliberately unsupported (see
    !> feature_temporal.md); parquet_validate_maml rejects it with a clear message rather than
    !> silently ignoring the declared bound.
    subroutine test_validate_qc_on_temporal_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "validate_qc_on_temporal_column", expect_abort=.true., &
            failure_message="qc: on a timestamp field was expected to error stop", &
            required_stderr="declares qc: min:/max:, which is not supported for a timestamp column (qc: miss: is)")
    end subroutine test_validate_qc_on_temporal_column_aborts

    !> Regression test: an explicit seconds unit ("timestamp[s]") must be rejected at add_field
    !> time, not silently accepted and then silently downgraded to milliseconds by Arrow's
    !> Parquet writer on write (Parquet's physical format has no seconds-resolution TIME/
    !> TIMESTAMP encoding at all) -- see apply_temporal_unit_token's own comment in
    !> parquet_metadata.f90 for how this was caught.
    subroutine test_schema_add_field_seconds_unit_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_seconds_unit_rejected", &
            expect_abort=.true., &
            failure_message="schema%add_field with 'timestamp[s]' was expected to error stop", &
            required_stderr="invalid data_type 'timestamp[s]'")
    end subroutine test_schema_add_field_seconds_unit_aborts

    !> "time" has no timezone concept (no date part to be UTC-adjusted relative to) -- only
    !> "timestamp" accepts a ",utc" suffix.
    subroutine test_schema_add_field_time_utc_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_time_utc_rejected", &
            expect_abort=.true., &
            failure_message="schema%add_field with 'time[ms,utc]' was expected to error stop", &
            required_stderr="invalid data_type 'time[ms,utc]'")
    end subroutine test_schema_add_field_time_utc_aborts

    subroutine test_schema_add_field_unclosed_bracket_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "schema_add_field_unclosed_bracket_rejected", &
            expect_abort=.true., &
            failure_message="schema%add_field with an unclosed unit bracket ('time[ms') was expected to error stop", &
            required_stderr="invalid data_type 'time[ms'")
    end subroutine test_schema_add_field_unclosed_bracket_aborts

    subroutine test_col_size_malformed_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "col_size_malformed_value", expect_abort=.true., &
            failure_message="a malformed col_size value should abort", &
            required_stderr="has an invalid col_size")
    end subroutine test_col_size_malformed_value_aborts

    subroutine test_array_size_malformed_value_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "array_size_malformed_value", expect_abort=.true., &
            failure_message="a malformed array_size value should abort", &
            required_stderr="has an invalid array_size")
    end subroutine test_array_size_malformed_value_aborts

    subroutine test_array_size_auto_on_non_string_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "array_size_auto_on_non_string", expect_abort=.true., &
            failure_message="array_size: auto on a non-string column should abort", &
            required_stderr="only applies to string columns")
    end subroutine test_array_size_auto_on_non_string_aborts

    subroutine test_set_col_size_non_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_col_size_non_positive", expect_abort=.true., &
            failure_message="set_col_size with a non-positive value should abort", &
            required_stderr="must be a positive integer")
    end subroutine test_set_col_size_non_positive_aborts

    subroutine test_set_col_size_already_resolved_no_force_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_col_size_already_resolved_no_force", &
            expect_abort=.true., &
            failure_message="set_col_size on an already-resolved column should abort without force=.true.", &
            required_stderr="pass force=.true. to override")
    end subroutine test_set_col_size_already_resolved_no_force_aborts

    subroutine test_set_array_size_non_string_column_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_array_size_non_string_column", expect_abort=.true., &
            failure_message="set_array_size on a non-string column should abort", &
            required_stderr="not a string column")
    end subroutine test_set_array_size_non_string_column_aborts

    subroutine test_set_array_size_non_positive_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_array_size_non_positive", expect_abort=.true., &
            failure_message="set_array_size with a non-positive value should abort", &
            required_stderr="must be a positive integer")
    end subroutine test_set_array_size_non_positive_aborts

    subroutine test_set_array_size_already_resolved_no_force_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status_and_stderr(error, "set_array_size_already_resolved_no_force", &
            expect_abort=.true., &
            failure_message="set_array_size on an already-resolved column should abort without force=.true.", &
            required_stderr="pass force=.true. to override")
    end subroutine test_set_array_size_already_resolved_no_force_aborts

end module test_maml_errors
