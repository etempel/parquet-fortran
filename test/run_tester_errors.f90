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
!> Test driver: everything that drives an `error_scenarios` subprocess.
!!
!! **The only program here that forks**, and the only one that calls `prime_error_scenarios`. That
!! is what makes every other runner a pure in-process test binary — `fpm test run_tester_cpp` no
!! longer pays for ~114 scenario subprocesses, and the priming machinery (one fork, `xargs -P`,
!! `test_run/.primed/`) is confined to one program rather than being a property of any runner
!! anyone adds later.
!!
!! The `errors` suite is split four ways -- `errors`, `table_errors`, `analysis_errors`,
!! `numeric_errors` -- one per `error_scenarios_*` group module, so that a scenario and the test
!! driving it stay in files with matching names. The split is for compile time; see
!! `test_table_errors.f90`'s header.
!!
!! Priming is all-or-nothing over the whole scenario set, which is why it is unconditional here and
!! absent everywhere else: a narrower trade — priming ~1100 scenarios to serve one suite's few
!! dozen — is a bad one, and was measured to be so before this split.
program run_tester_errors
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_errors, only : collect_tests_parquet_errors
    use test_table_errors, only : collect_tests_parquet_table_errors
    use test_analysis_errors, only : collect_tests_parquet_analysis_errors
    use test_numeric_errors, only : collect_tests_parquet_numeric_errors
    use test_maml_errors, only : collect_tests_parquet_maml_errors
    use test_metadata_errors, only : collect_tests_parquet_metadata_errors
    use test_reading_errors, only : collect_tests_parquet_reading_errors
    use test_writing_errors, only : collect_tests_parquet_writing_errors
    use parquet_bindings, only : parquet_warmup_memory_pool
    use test_errors, only : prime_error_scenarios
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=:), allocatable :: suite_name, test_name
    !
    call run_tester_args(suite_name, test_name)
    call parquet_warmup_memory_pool()
    ! Pre-run every error scenario once, in parallel, before any suite starts -- see
    ! prime_error_scenarios in test_errors.f90 for what this buys and why it is safe here and
    ! nowhere else (exactly one fork, with no OpenMP team active). Unconditional: this is the
    ! only runner that consumes the scenario set, so the old "is this suite worth priming for?"
    ! gate reduces to "this program always is". A named single test still skips it, since paying
    ! for the whole set to run one test is never right.
    if (.not. allocated(test_name)) call prime_error_scenarios()
    !
    testsuites = [ &
        new_testsuite("errors", collect_tests_parquet_errors), &
        new_testsuite("table_errors", collect_tests_parquet_table_errors), &
        new_testsuite("analysis_errors", collect_tests_parquet_analysis_errors), &
        new_testsuite("numeric_errors", collect_tests_parquet_numeric_errors), &
        new_testsuite("writing_errors", collect_tests_parquet_writing_errors), &
        new_testsuite("reading_errors", collect_tests_parquet_reading_errors), &
        new_testsuite("maml_errors", collect_tests_parquet_maml_errors), &
        new_testsuite("metadata_errors", collect_tests_parquet_metadata_errors) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester_errors
