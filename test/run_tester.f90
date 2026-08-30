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
!> Test driver (b): the Arrow-free parquet-domain modules — `parquet_string_column`, the three
!> container element types, and the temporal element types.
!!
!! **The name is deliberately unchanged** even though the contents are completely different:
!! `CONTRIBUTING.md`, `tools/coverage.sh`, `tools/coverage_cpp.sh` and `fpm.toml`'s own comment
!! all name `run_tester`, and a rename would silently change what those commands measure rather
!! than fail (`feature_tests.md` section 6.1).
!!
!! Like (a) this executes no `bind(C)` call and is undef-safe.
program run_tester
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_list, only : collect_tests_parquet_list
    use test_map, only : collect_tests_parquet_map
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
        new_testsuite("parquet_string", collect_tests_parquet_string), &
        new_testsuite("list", collect_tests_parquet_list), &
        new_testsuite("map", collect_tests_parquet_map), &
        new_testsuite("temporal", collect_tests_parquet_temporal), &
        new_testsuite("string_parallel", collect_tests_string_parallel) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester
