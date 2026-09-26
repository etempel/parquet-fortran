!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
! https://github.com/fortran-lang/test-drive
!
! To exclude code lines from gcovr coverage report, use the following markers:
! GCOVR_EXCL_START
! GCOVR_EXCL_STOP
! GCOVR_EXCL_LINE
! GCOVR_EXCL_FUNCTION
!
!> Test driver (b): the Arrow-free parquet-domain modules — `parquet_column`, `parquet_string_column`,
!> the three container element types, and the temporal element types.
!!
!! **The name is deliberately unchanged** even though the contents are completely different:
!! `CONTRIBUTING.md`, `tools/coverage.sh`, `tools/coverage_cpp.sh` and `fpm.toml`'s own comment
!! all name `run_tester`, and a rename would silently change what those commands measure rather
!! than fail.
!!
!! Like (a) this executes no `bind(C)` call and is undef-safe.
program run_tester
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_columns, only : collect_tests_parquet_columns
    use test_columns_parallel, only : collect_tests_columns_parallel
    use test_list, only : collect_tests_parquet_list
    use test_map, only : collect_tests_parquet_map
    use test_struct, only : collect_tests_parquet_struct
    use test_parquet_string, only : collect_tests_parquet_string
    use test_string_parallel, only : collect_tests_string_parallel
    use test_temporal, only : collect_tests_parquet_temporal
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=:), allocatable :: suite_name, test_name
    !
    call run_tester_args(suite_name, test_name)
    !
    testsuites = [ &
        ! Undef-safe only since `ensure_capacity` gained its `#ifdef UNDEFINED_CHECK` fill: the
        ! copy there reads a null row's value bytes, which `%init` leaves unspecified on purpose,
        ! and that build now DEFINES them (with poison, so an unsound read stays visible). The
        ! contract is unchanged -- see the fill site in tools/generate_parquet_columns.py.
        new_testsuite("columns", collect_tests_parquet_columns), &
        new_testsuite("parquet_string", collect_tests_parquet_string), &
        new_testsuite("list", collect_tests_parquet_list), &
        new_testsuite("map", collect_tests_parquet_map), &
        new_testsuite("struct", collect_tests_parquet_struct), &
        new_testsuite("temporal", collect_tests_parquet_temporal), &
        new_testsuite("string_parallel", collect_tests_string_parallel), &
        new_testsuite("columns_parallel", collect_tests_columns_parallel) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester
