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
    use test_openmp, only : collect_tests_parquet_openmp
    !
    implicit none
    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    !
    stat = 0
    !
    ! Add all testsuites here as a comma separated list
    testsuites = [ &
        new_testsuite("writing", collect_tests_parquet_writing), &
        new_testsuite("reading", collect_tests_parquet_reading), &
        new_testsuite("maml", collect_tests_parquet_maml), &
        new_testsuite("errors", collect_tests_parquet_errors), &
        new_testsuite("examples", collect_tests_parquet_examples), &
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
                    call cleanup_testsuites()
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
        call cleanup_testsuites()
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 1
    end if

    call cleanup_testsuites()
    !
contains
    !
    subroutine cleanup_testsuites()
        integer :: status
        !
        !call execute_command_line("rm -f test_parquet.parquet",wait=.true.,cmdstat=status)
        !
    end subroutine cleanup_testsuites
    !
end program tester
