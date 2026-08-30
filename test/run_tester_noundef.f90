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
!> Test driver: Arrow-free, yet NOT undef-safe.
!!
!! **Membership here cannot be derived from source — it is discovered by running the check**, which
!! is the one way this runner differs from every other (`feature_tests.md` section 6.3). Each entry
!! carries its reason at the registration below, and entries arrive and leave **by hand**: a tool
!! that promoted a suite on one green run would be deciding, on one machine and one compiler,
!! something that needs a person to look at why it changed.
!!
!! The cost of a wrong entry is asymmetric and silent: a suite parked here that would have passed
!! loses its undef coverage with nothing to report it. `tools/check_nag_undefined.sh` therefore
!! re-tries these suites and REPORTS any that now pass, without moving them.
program run_tester_noundef
    use testdrive, only : new_testsuite, testsuite_type
    use test_runner_support, only : run_tester_args, run_tester_main
    use test_sorting, only : collect_tests_parquet_sorting
    implicit none
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=:), allocatable :: suite_name, test_name
    !
    call run_tester_args(suite_name, test_name)
    !
    testsuites = [ &
        ! NOT a C++ dependency -- `test_sorting.f90` is Arrow-free and the partition check
        ! proves it. It is here because `-C=undefined` SEGFAULTS inside its own
        ! instrumentation, at `parquet_argsort_kernel.f90:303` (`extract_chr`, writing
        ! `buf(1)%data(pos + j)`), reached from `pf_sort` on a `character` array. The line is
        ! correct -- `buf(1)%data` is allocated five lines above -- and the suite passes under
        ! every other profile including `nagdeb`; the faulting frame shows NAG's own
        ! per-argument definedness maps arriving null or garbage on a separate module
        ! procedure. Not stack exhaustion: it reproduces at OMP_STACKSIZE=64M, ulimit -s
        ! 65520 and OMP_NUM_THREADS=1. feature_tests.md section 3 recorded the same stop at
        ! test 17 and called for a per-test bisect; this is that bisect's result.
        new_testsuite("sorting", collect_tests_parquet_sorting) &
        ]
    !
    call run_tester_main(testsuites, suite_name, test_name)
    !
end program run_tester_noundef
