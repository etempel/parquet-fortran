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

    subroutine test_read_column_with_nulls_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "read_column_with_nulls", expect_abort=.true., &
            failure_message="reading a column with a genuine Parquet Null was expected to error stop")
    end subroutine test_read_column_with_nulls_aborts

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
    !
end module test_errors
