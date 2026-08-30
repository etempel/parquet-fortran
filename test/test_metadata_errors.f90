!> Abort-path tests for `metadata`, split out of `test_metadata.f90` so that the
!> reader/writer runner forks no subprocesses at all.
!!
!! **Why these live apart from the tests they are about.** Every test here drives
!! `test/error_scenarios.f90` through `run_error_scenario`/`check_scenario_*`, i.e. it spawns a
!! subprocess and asserts on its exit status and stderr. That is the one thing `run_tester_cpp`
!! must not do -- see `feature_tests.md` section 5 -- so they gather in `run_tester_errors`
!! beside the 913 tests already there.
!!
!! **The cost is locality and it is real**: a test asserting that a bad metadata call aborts is a
!! test *about metadata*, and it no longer sits beside the metadata tests. Run
!! `run_tester_errors -- metadata_errors` to see them as a group.
module test_metadata_errors
    use parquet
    use parquet_maml_base
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    use test_errors, only : check_scenario_exit_status, run_error_scenario, scenario_capture_contains
    !
    implicit none
    private
    public :: collect_tests_parquet_metadata_errors

contains

    subroutine collect_tests_parquet_metadata_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end: a single array constructor can exceed
        ! the standard's 255-continuation-line limit, which only nagfor enforces, and the
        ! self-referential `testsuite = [testsuite, ...]` form makes nagfor double-free each
        ! entry's name string at run time. See `test_writing.f90` for the full note.
        type(unittest_type), allocatable :: p1(:)
        !
        p1 = [ &
            new_unittest("missing key with no default aborts", test_missing_key_no_default_aborts), &
            new_unittest("unparsable value with no default aborts", test_conversion_failure_no_default_aborts), &
            new_unittest("missing int64 key with no default aborts", test_missing_int64_no_default_aborts), &
            new_unittest("missing float32 key with no default aborts", test_missing_float32_no_default_aborts), &
            new_unittest("missing float64 key with no default aborts", test_missing_float64_no_default_aborts), &
            new_unittest("missing logical key with no default aborts", test_missing_logical_no_default_aborts), &
            new_unittest("missing string key with no default aborts", test_missing_string_no_default_aborts), &
            new_unittest("unparsable int64 value with no default aborts", test_conversion_int64_no_default_aborts), &
            new_unittest("unparsable float32 value with no default aborts", &
                test_conversion_float32_no_default_aborts), &
            new_unittest("unparsable float64 value with no default aborts", &
                test_conversion_float64_no_default_aborts), &
            new_unittest("unparsable logical value with no default aborts", &
                test_conversion_logical_no_default_aborts), &
            new_unittest("missing int32 array key with no default aborts", test_missing_int32_array_no_default_aborts), &
            new_unittest("unparsable int32 array value with no default aborts", &
                test_conversion_int32_array_no_default_aborts), &
            new_unittest("missing int64 array key with no default aborts", test_missing_int64_array_no_default_aborts), &
            new_unittest("unparsable int64 array value with no default aborts", &
                test_conversion_int64_array_no_default_aborts), &
            new_unittest("missing float32 array key with no default aborts", &
                test_missing_float32_array_no_default_aborts), &
            new_unittest("unparsable float32 array value with no default aborts", &
                test_conversion_float32_array_no_default_aborts), &
            new_unittest("missing float64 array key with no default aborts", &
                test_missing_float64_array_no_default_aborts), &
            new_unittest("unparsable float64 array value with no default aborts", &
                test_conversion_float64_array_no_default_aborts), &
            new_unittest("missing logical array key with no default aborts", &
                test_missing_logical_array_no_default_aborts), &
            new_unittest("unparsable logical array value with no default aborts", &
                test_conversion_logical_array_no_default_aborts), &
            new_unittest("missing string array key with no default aborts", &
                test_missing_string_array_no_default_aborts), &
            new_unittest("print_schema_info: neither unit nor filename given aborts", &
                test_print_schema_info_no_unit_no_filename_aborts), &
            new_unittest("print_schema_info: unit not already open aborts", &
                test_print_schema_info_unit_not_open_aborts), &
            new_unittest("print_schema_info: unit open for reading only aborts", &
                test_print_schema_info_unit_read_only_aborts), &
            new_unittest("print_schema_info: unit/filename mismatch aborts", &
                test_print_schema_info_unit_filename_mismatch_aborts), &
            new_unittest("print_schema_info: uninitialized schema aborts by default", &
                test_print_schema_info_uninitialized_schema_aborts), &
            new_unittest("print_schema_info: filename that cannot be opened for writing aborts", &
                test_print_schema_info_open_failure_aborts), &
            new_unittest("add_metadata before the schema has been initialized aborts", &
                test_add_metadata_before_init_aborts), &
            new_unittest("add_field applies the per-field rules parquet_validate_maml would", &
                test_add_field_validates_field_rules) &
            ]
        !
        testsuite = p1
    end subroutine collect_tests_parquet_metadata_errors

    subroutine test_missing_key_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_key_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing metadata key with no default was expected to abort")
    end subroutine test_missing_key_no_default_aborts

    subroutine test_conversion_failure_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_failure_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable metadata value with no default was expected to abort")
    end subroutine test_conversion_failure_no_default_aborts

    subroutine test_missing_int64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_int64_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing int64 metadata key with no default was expected to abort")
    end subroutine test_missing_int64_no_default_aborts

    subroutine test_missing_float32_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float32_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float32 metadata key with no default was expected to abort")
    end subroutine test_missing_float32_no_default_aborts

    subroutine test_missing_float64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float64_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float64 metadata key with no default was expected to abort")
    end subroutine test_missing_float64_no_default_aborts

    subroutine test_missing_logical_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_logical_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing logical metadata key with no default was expected to abort")
    end subroutine test_missing_logical_no_default_aborts

    subroutine test_missing_string_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_string_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing string metadata key with no default was expected to abort")
    end subroutine test_missing_string_no_default_aborts

    subroutine test_conversion_int64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_int64_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable int64 metadata value with no default was expected to abort")
    end subroutine test_conversion_int64_no_default_aborts

    subroutine test_conversion_float32_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float32_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float32 metadata value with no default was expected to abort")
    end subroutine test_conversion_float32_no_default_aborts

    subroutine test_conversion_float64_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float64_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float64 metadata value with no default was expected to abort")
    end subroutine test_conversion_float64_no_default_aborts

    subroutine test_conversion_logical_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_logical_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable logical metadata value with no default was expected to abort")
    end subroutine test_conversion_logical_no_default_aborts

    subroutine test_missing_int32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_int32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing int32 array metadata key with no default was expected to abort")
    end subroutine test_missing_int32_array_no_default_aborts

    subroutine test_conversion_int32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_int32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable int32 array metadata value with no default was expected to abort")
    end subroutine test_conversion_int32_array_no_default_aborts

    subroutine test_missing_int64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_int64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing int64 array metadata key with no default was expected to abort")
    end subroutine test_missing_int64_array_no_default_aborts

    subroutine test_conversion_int64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_int64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable int64 array metadata value with no default was expected to abort")
    end subroutine test_conversion_int64_array_no_default_aborts

    subroutine test_missing_float32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float32 array metadata key with no default was expected to abort")
    end subroutine test_missing_float32_array_no_default_aborts

    subroutine test_conversion_float32_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float32_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float32 array metadata value with no default was expected to abort")
    end subroutine test_conversion_float32_array_no_default_aborts

    subroutine test_missing_float64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_float64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing float64 array metadata key with no default was expected to abort")
    end subroutine test_missing_float64_array_no_default_aborts

    subroutine test_conversion_float64_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_float64_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable float64 array metadata value with no default was expected to abort")
    end subroutine test_conversion_float64_array_no_default_aborts

    subroutine test_missing_logical_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_logical_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing logical array metadata key with no default was expected to abort")
    end subroutine test_missing_logical_array_no_default_aborts

    subroutine test_conversion_logical_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_conversion_logical_array_no_default", &
            expect_abort=.true., &
            failure_message="reading an unparsable logical array metadata value with no default was expected to abort")
    end subroutine test_conversion_logical_array_no_default_aborts

    subroutine test_missing_string_array_no_default_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "get_metadata_missing_string_array_no_default", &
            expect_abort=.true., &
            failure_message="reading a missing string array metadata key with no default was expected to abort")
    end subroutine test_missing_string_array_no_default_aborts

    subroutine test_print_schema_info_no_unit_no_filename_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_no_unit_no_filename", expect_abort=.true., &
            failure_message="print_schema_info with neither unit nor filename was expected to abort")
    end subroutine test_print_schema_info_no_unit_no_filename_aborts

    subroutine test_print_schema_info_unit_not_open_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_unit_not_open", expect_abort=.true., &
            failure_message="print_schema_info with an unopened unit was expected to abort")
    end subroutine test_print_schema_info_unit_not_open_aborts

    subroutine test_print_schema_info_unit_read_only_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_unit_read_only", expect_abort=.true., &
            failure_message="print_schema_info with a read-only unit was expected to abort")
    end subroutine test_print_schema_info_unit_read_only_aborts

    subroutine test_print_schema_info_unit_filename_mismatch_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_unit_filename_mismatch", expect_abort=.true., &
            failure_message="print_schema_info with a unit/filename mismatch was expected to abort")
    end subroutine test_print_schema_info_unit_filename_mismatch_aborts

    subroutine test_print_schema_info_uninitialized_schema_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_uninitialized_schema", expect_abort=.true., &
            failure_message="print_schema_info on a never-initialized schema was expected to abort by default")
    end subroutine test_print_schema_info_uninitialized_schema_aborts

    subroutine test_print_schema_info_open_failure_aborts(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "print_schema_info_open_failure", expect_abort=.true., &
            failure_message="print_schema_info with a filename that cannot be opened was expected to abort")
    end subroutine test_print_schema_info_open_failure_aborts

    !> %add_metadata needs %init (or a parse) to have established the metadata table first.
    !! The scenario runs the LEGAL "after %init, before any parse" call before the illegal one,
    !! so a guard that fired unconditionally would fail this test rather than pass it.
    subroutine test_add_metadata_before_init_aborts(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("schema_add_metadata_before_init", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "add_metadata before schema%init was expected to abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, "schema has no metadata table yet", found)
        call check(error, found, "the abort should say the schema has no metadata table yet")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "metadata entries after add_metadata with no parse: 2", found)
        call check(error, found, &
            "the negative control (add_metadata after %init, before any parse) should have been accepted")
    end subroutine test_add_metadata_before_init_aborts

    !> An in-code schema may now never be validated as a whole document, so %add_field runs the
    !! per-field half of parquet_validate_maml itself -- literally the same procedure, so the two
    !! routes cannot disagree about what a valid field is. The scenario's negative control is a
    !! field that passes: it proves the WHOLE-document half stays skipped, since a one-field
    !! sub-document carries no table: line and the full validator would reject every one.
    subroutine test_add_field_validates_field_rules(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=:), allocatable :: out_file, err_file
        integer :: exitstat, cmdstat
        logical :: found

        call run_error_scenario("schema_add_field_validates_field_rules", exitstat, cmdstat, out_file, err_file)
        call check(error, cmdstat == 0, "failed to invoke the error_scenarios helper binary")
        if (allocated(error)) return
        call check(error, exitstat /= 0, "a qc_min that is not integral for an int32 column should abort")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "parquet_schema%add_field: field 'x' has an invalid qc: min value '3.7'", found)
        call check(error, found, "the abort should come from %add_field and name the offending bound")
        if (allocated(error)) return
        call scenario_capture_contains(out_file, err_file, &
            "fields accepted with no whole-document validation: 1", found)
        call check(error, found, &
            "a valid field should still be accepted, with no whole-document validation applied to it")
    end subroutine test_add_field_validates_field_rules

end module test_metadata_errors
