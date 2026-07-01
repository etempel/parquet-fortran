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
    use parameters, only : activate_unit_testing_run
    use test_dummy, only : collect_tests_parquet_fortran
    !
    implicit none
    integer :: stat, is
    character(len=:), allocatable :: suite_name, test_name
    type(testsuite_type), allocatable :: testsuites(:)
    character(len=*), parameter :: fmt = '("#", *(1x, a))'
    !
    stat = 0
    !
    call activate_unit_testing_run()
    !
    ! Add all testsuites here as a comma separated list
    testsuites = [ &
        new_testsuite("dummy", collect_tests_parquet_fortran) &
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
                call cleanup_testsuites()
                if (stat < 0) then
                    error stop 1
                end if
            else
                write(error_unit, fmt) "Testing:", testsuites(is)%name
                call run_testsuite(testsuites(is)%collect, error_unit, stat)
                call cleanup_testsuites()
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
            call cleanup_testsuites()
        end do
    end if
    !
    if (stat > 0) then
        write(error_unit, '(i0, 1x, a)') stat, "test(s) failed!"
        error stop 1
    end if
    !
contains
    !
    subroutine cleanup_testsuites()
        integer :: status
        !
        call execute_command_line("rm -f run_status.txt",wait=.true.,cmdstat=status)
        call execute_command_line("rm -rf test_run",wait=.true.,cmdstat=status)
        !
    end subroutine cleanup_testsuites
    !
end program tester
