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
            new_unittest("get_column_index on unknown column aborts", test_get_column_index_not_found_aborts), &
            new_unittest("write_maml without metadata aborts", test_write_maml_without_metadata_aborts) &
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

    subroutine check_scenario_exit_status(error, scenario, expect_abort, failure_message)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), intent(in) :: scenario, failure_message
        logical, intent(in) :: expect_abort
        integer :: exitstat, cmdstat
        logical :: aborted

        call execute_command_line( &
            "fpm test error_scenarios -- "//trim(scenario)//" > /dev/null 2>&1", &
            wait=.true., exitstat=exitstat, cmdstat=cmdstat)

        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper program via fpm")
        if (allocated(error)) return

        aborted = (exitstat /= 0)
        call check(error, aborted .eqv. expect_abort, failure_message)
    end subroutine check_scenario_exit_status
    !
end module test_errors
