!> Abort-path tests for `reading`, split out of `test_reading.f90` so that the
!> reader/writer runner forks no subprocesses at all.
!!
!! **Why these live apart from the tests they are about.** Every test here drives
!! `test/error_scenarios.f90` through `run_error_scenario`/`check_scenario_*`, i.e. it spawns a
!! subprocess and asserts on its exit status and stderr. That is the one thing `run_tester_cpp`
!! must not do, so they gather in `run_tester_errors` beside the 913 tests already there.
!!
!! **The cost is locality and it is real**: a test asserting that a bad reading call aborts is a
!! test *about reading*, and it no longer sits beside the reading tests. Run
!! `run_tester_errors -- reading_errors` to see them as a group.
module test_reading_errors
    use parquet
    use parquet_maml_base
    use parquet_strings, only : parquet_string_column
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    use test_random_vectors, only : samp_label, n_samp, n_samp_frac, samp_seed, samp_row, &
        samp_u_bits, samp_frac_bits, samp_keep
    use test_errors, only : check_scenario_exit_status
    !
    implicit none
    private
    public :: collect_tests_parquet_reading_errors

contains

    subroutine collect_tests_parquet_reading_errors(testsuite)
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        ! Built in parts and concatenated ONCE at the end: a single array constructor can exceed
        ! the standard's 255-continuation-line limit, which only nagfor enforces, and the
        ! self-referential `testsuite = [testsuite, ...]` form makes nagfor double-free each
        ! entry's name string at run time. See `test_writing.f90` for the full note.
        type(unittest_type), allocatable :: p1(:)
        !
        p1 = [ &
            new_unittest("parquet_get_col_size/parquet_get_column_total_elements/parquet_read_array_row_mode/" // &
                "parquet_read_array_element_mode avoid a whole-column read", &
                test_col_size_and_row_mode_avoid_whole_column_read), &
            new_unittest("parquet_get_col_size/parquet_get_column_total_elements avoid a whole-column " // &
                "read on a plain LIST column", test_plain_list_size_queries_avoid_whole_column_read), &
            new_unittest("date/time/timestamp: foreign INT96 and non-UTC-timezone fixtures round-trip", &
                test_datetime_foreign_fixtures), &
            new_unittest("plain LIST/LARGE_LIST columns from a foreign-written file (col_size, string length, print_stat)", &
                test_list_type_foreign_fixture), &
            new_unittest("a string column over Arrow's int32 offset ceiling reads back widened, through every " // &
                "whole-column entry point", test_string_read_over_offset_limit) &
            ]
        !
        testsuite = p1
    end subroutine collect_tests_parquet_reading_errors

    !> Regression coverage for the "List index overflow" crash parquet_get_col_size/
    !> parquet_get_column_total_elements/parquet_read_array_row_mode/parquet_read_array_element_
    !> mode used to hit once a vector column's total element count (nrows * col_size) exceeded
    !> 2^31-1 -- all four used to read the *whole* column just to answer a size query, fetch one
    !> row, or fetch one element position across all rows. Exercising this for real would need a
    !> genuine multi-billion-element column, far too slow/large for this suite -- so the actual
    !> proof runs out-of-process as
    !> scenario_col_size_and_row_mode_avoid_whole_column_read in error_scenarios.f90 (same pattern
    !> as test_list_element_count_auto_multi_row_group_roundtrip in test_writing.f90), against a
    !> tiny fixture with a test-only hook that forces a whole-column read to abort; see that
    !> scenario's own comment (and its negative control,
    !> scenario_whole_column_read_forced_error_control) for why this proves the fix rather than
    !> just "nothing happened to call the old path anyway". Note parquet_read_array_element_mode's
    !> fix is streaming row group by row group, not skipping all but one -- it inherently needs
    !> every row group's data (see stream_element_mode_row_groups's own comment in
    !> parquet_wrapper.cpp), unlike parquet_read_array_row_mode which only ever needs one.
    subroutine test_col_size_and_row_mode_avoid_whole_column_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "col_size_and_row_mode_avoid_whole_column_read", expect_abort=.false., &
            failure_message="parquet_get_col_size/parquet_get_column_total_elements/" // &
                "parquet_read_array_row_mode/parquet_read_array_element_mode did not all avoid a whole-column read")
    end subroutine test_col_size_and_row_mode_avoid_whole_column_read

    !
    !> The same guarantee for a PLAIN LIST/LARGE_LIST column, which the scenario above cannot
    !> reach: it writes its fixture with this library's own writer, so every column there is a
    !> FIXED_SIZE_LIST whose width is a schema constant and no data is read at all. A plain
    !> variable-length list is the only shape whose width lives in the data, so it is the only one
    !> where these two queries can read anything -- and therefore the only one where reading too
    !> much is possible. parquet_get_column_total_elements did exactly that until it was moved onto
    !> list_width_verified, the screen-then-prove helper parquet_get_col_size already used;
    !> reverting that change makes this test fail. Negative control:
    !> scenario_whole_column_read_forced_error_control, which proves the forcing hook fires.
    subroutine test_plain_list_size_queries_avoid_whole_column_read(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "plain_list_size_queries_avoid_whole_column_read", expect_abort=.false., &
            failure_message="parquet_get_col_size/parquet_get_column_total_elements read a whole " // &
                "plain LIST column instead of measuring it one row group at a time")
    end subroutine test_plain_list_size_queries_avoid_whole_column_read

    !
    !> The actual assertions run out-of-process (scenario_temporal_foreign_int96_roundtrip/
    !> scenario_temporal_foreign_tz_roundtrip in error_scenarios.f90, each error-stopping on any
    !> mismatch) since building the fixtures needs a test-only debug hook -- this just checks
    !> both scenarios exit cleanly. See those scenarios' own comments for what each verifies:
    !> a legacy INT96 timestamp column and a real non-UTC IANA timezone, neither ever produced
    !> by this library's own writer.
    subroutine test_datetime_foreign_fixtures(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "temporal_foreign_int96_roundtrip", expect_abort=.false., &
            failure_message="a legacy INT96 timestamp fixture did not round-trip correctly")
        if (allocated(error)) return
        call check_scenario_exit_status(error, "temporal_foreign_tz_roundtrip", expect_abort=.false., &
            failure_message="a non-UTC timezone fixture did not round-trip correctly")
    end subroutine test_datetime_foreign_fixtures

    !
    !> get_col_size/flatten_for_stats/parquet_reader_get_string_length's
    !> plain LIST/LARGE_LIST branches -- this library's own writer only ever emits FIXED_SIZE_LIST,
    !> so these are only reachable through a foreign-written file, built by
    !> parquet_debug_write_list_fixture (a test-only C++ hook, see its own comment). The actual
    !> assertions run out-of-process (scenario_list_type_foreign_fixture in error_scenarios.f90,
    !> error-stopping on any mismatch, same convention as test_datetime_foreign_fixtures above) --
    !> this just checks the scenario exits cleanly.
    subroutine test_list_type_foreign_fixture(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "list_type_foreign_fixture", expect_abort=.false., &
            failure_message="LIST/LARGE_LIST foreign-fixture columns did not report the expected sizes/lengths")
    end subroutine test_list_type_foreign_fixture

    !> A plain `string` column whose byte payload exceeds Arrow's int32 offset ceiling (~2 GiB)
    !> arrives from Arrow's decoder as several chunks that no single `utf8` array can hold, and
    !> reading it aborted the process with `offset overflow while concatenating arrays`.
    !> combine_column_chunks (parquet_wrapper.cpp) widens such a column to `large_utf8` before
    !> combining it. A genuine case needs more than 2 GiB of strings, so the proof runs out of
    !> process as scenario_string_read_over_offset_limit in error_scenarios.f90, which reaches the
    !> same path on a tiny fixture through two process-global test hooks (a shrunk ceiling and a
    !> forced chunk split -- see the scenario's own comment for why that is safe only in
    !> isolation), drives every whole-column entry point through it, and carries its own negative
    !> controls.
    subroutine test_string_read_over_offset_limit(error)
        type(error_type), allocatable, intent(out) :: error

        call check_scenario_exit_status(error, "string_read_over_offset_limit", expect_abort=.false., &
            failure_message="a string column over the int32 offset ceiling (shrunk by the debug hook) did not " // &
            "read back widened and intact through every whole-column entry point")
    end subroutine test_string_read_over_offset_limit

end module test_reading_errors
