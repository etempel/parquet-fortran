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
!> Driver for unit testing
program tester
    use, intrinsic :: iso_fortran_env, only : error_unit
    use testdrive, only : run_testsuite, new_testsuite, testsuite_type, select_suite, run_selected,get_argument,init_color_output
    use test_writing, only : collect_tests_parquet_writing
    use test_reading, only : collect_tests_parquet_reading
    use test_maml, only : collect_tests_parquet_maml
    use test_errors, only : collect_tests_parquet_errors
    use test_examples, only : collect_tests_parquet_examples
    use test_openmp, only : collect_tests_parquet_openmp_write, collect_tests_parquet_openmp
    !
    implicit none
    integer :: stat, is, cmdstat
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    !
    stat = 0
    !
    ! Tests write scratch parquet/maml files under test_run/; ensure it
    ! exists up front so a fresh checkout or `rm -rf test_run` doesn't
    ! break tests that don't create the directory themselves.
    call execute_command_line("mkdir -p test_run", wait=.true., cmdstat=cmdstat)
    !
    ! Add all testsuites here as a comma separated list
    testsuites = [ &
        new_testsuite("writing", collect_tests_parquet_writing), &
        new_testsuite("reading", collect_tests_parquet_reading), &
        new_testsuite("maml", collect_tests_parquet_maml), &
        new_testsuite("errors", collect_tests_parquet_errors), &
        new_testsuite("examples", collect_tests_parquet_examples), &
        ! openmp_write must run (and fully complete) as its own suite before
        ! openmp: testdrive runs every test *within* one suite concurrently
        ! with each other by default, but different suites still run
        ! sequentially relative to each other (see the note on
        ! collect_tests_parquet_openmp_write in test_openmp.f90) -- several
        ! tests in the openmp suite depend on files this one writes.
        new_testsuite("openmp_write", collect_tests_parquet_openmp_write), &
        new_testsuite("openmp", collect_tests_parquet_openmp) &
        ]
    !
    ! command line argument for a specific testsuite and test
    call get_argument(1, suite_name)
    call get_argument(2, test_name)
    !
    call init_color_output(.true.)
    !
    if (allocated(suite_name)) then
        is = select_suite(testsuites, suite_name)
        if (is > 0 .and. is <= size(testsuites)) then
            if (allocated(test_name)) then
                write(error_unit, fmt) "Suite:", testsuites(is)%name
                call run_selected(testsuites(is)%collect, test_name, error_unit, stat)
                if (stat < 0) then
                    error stop 1
                end if
            else
                write(error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat)
            end if
        else
            write(error_unit, fmt) "Available testsuites"
            do is = 1, size(testsuites)
                write(error_unit, fmt) "-", testsuites(is)%name
            end do
            error stop 1
        end if
    else
        do is = 1, size(testsuites)
            write(error_unit, fmt) "Testing:", testsuites(is)%name
            call run_testsuite(testsuites(is)%collect, error_unit, stat)
        end do
    end if
    !
    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 1
    end if
    !
end program tester
