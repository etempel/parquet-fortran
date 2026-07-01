!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
module test_dummy
    use testdrive, only : new_unittest, unittest_type, error_type, check, test_failed
    !
    implicit none
    private
    public :: collect_tests_parquet_fortran
    !
contains
    !
    !> Collect all exported unit tests
    subroutine collect_tests_parquet_fortran(testsuite)
        implicit none
        integer :: status
        !> Collection of tests
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        !
        testsuite = [ &
            new_unittest("dummy", test_x) &
            ]
        !
    end subroutine collect_tests_parquet_fortran
    !
    subroutine test_x(error)
        type(error_type), allocatable, intent(out) :: error
        !
        return
        !
    end subroutine test_x
    !
end module test_dummy