!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> The one `parquet_temporal` test that reaches the C++ layer, split out of `test_temporal.f90`.
!!
!! **It lives in its own file for a mechanical reason, not a thematic one.** A test file may feed
!! an undef-safe runner only if it declares no `bind(C)` interface at all, and that rule is
!! file-level because a per-test rule is not statically decidable. This test calls two
!! `parquet_debug_*` hooks in `src/parquet_wrapper.cpp`, so keeping it beside its 22 siblings
!! would disqualify the whole of `test_temporal.f90` from `run_tester`.
!!
!! What it asserts is unchanged: this module's pure-Fortran civil<->days calendar math against
!! Arrow's vendored copy of the same (Hinnant) algorithm. Registered as the suite `temporal_cpp`
!! in `run_tester_cpp`; the Arrow-free remainder stays as `temporal`.
module test_temporal_cpp
    use parquet_temporal
    use iso_fortran_env, only : int32, int64
    use iso_c_binding, only : c_int32_t, c_int64_t
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_parquet_temporal_cpp
    !
contains

    !> Registers this suite's single test.
    subroutine collect_tests_parquet_temporal_cpp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("Arrow cross-validation of civil<->days math", test_arrow_cross_validation) &
            ]
    end subroutine collect_tests_parquet_temporal_cpp

    !> Cross-validates this module's pure-Fortran civil<->days math against Arrow's vendored
    !> copy of the same (Hinnant date.h) algorithm, via two test-only parquet_debug_* hooks in
    !> src/parquet_wrapper.cpp. The sweep stays within years +-32767 -- the vendored library's
    !> `year` is a 16-bit type (this module itself goes far beyond; the algorithm is
    !> range-independent exact integer math, so agreement here validates it everywhere).
    subroutine test_arrow_cross_validation(error)
        type(error_type), allocatable, intent(out) :: error
        interface
            subroutine arrow_civil_from_days(days_in, year, month, day) &
                    bind(C, name="parquet_debug_civil_from_days")
                import :: c_int32_t, c_int64_t
                integer(c_int64_t), value :: days_in        !! days since 1970-01-01.
                integer(c_int32_t), intent(out) :: year     !! civil year (Arrow's answer).
                integer(c_int32_t), intent(out) :: month    !! civil month (Arrow's answer).
                integer(c_int32_t), intent(out) :: day      !! civil day (Arrow's answer).
            end subroutine arrow_civil_from_days
            subroutine arrow_days_from_civil(year, month, day, days_out) &
                    bind(C, name="parquet_debug_days_from_civil")
                import :: c_int32_t, c_int64_t
                integer(c_int32_t), value :: year           !! civil year.
                integer(c_int32_t), value :: month          !! civil month.
                integer(c_int32_t), value :: day            !! civil day.
                integer(c_int64_t), intent(out) :: days_out !! days since 1970-01-01 (Arrow's answer).
            end subroutine arrow_days_from_civil
        end interface
        type(parquet_date) :: d
        integer(int64) :: days
        integer(c_int64_t) :: adays
        integer(c_int32_t) :: ay, amo, add
        integer(int32) :: y, mo, dd, k
        character(len=96) :: msg
        !> Targeted civil dates: epoch, MJD epoch, leap days, century (non-)leap boundaries,
        !> the 4-digit year edges, and negative (proleptic) years including a BC leap day.
        integer(int32), parameter :: CIVIL(3, 14) = reshape([ &
            1970, 1, 1,    1858, 11, 17,  2000, 2, 29,   2024, 2, 29, &
            1900, 2, 28,   1900, 3, 1,    2100, 2, 28,   2100, 3, 1, &
            9999, 12, 31,  1, 1, 1,       -1, 12, 31,    -4, 2, 29, &
            32000, 6, 15,  -32000, 6, 15], [3, 14])
        ! broad two-way sweep across years ~ +-30000 (step is coprime to 7/400-year cycles)
        do days = -11000000_int64, 11000000_int64, 6007_int64
            call d%set_raw(int(days, int32))
            call d%get(y, mo, dd)
            call arrow_civil_from_days(int(days, c_int64_t), ay, amo, add)
            if (y /= ay .or. mo /= amo .or. dd /= add) then
                write(msg, '(a,i0,a,i0,"-",i0,"-",i0,a,i0,"-",i0,"-",i0)') &
                    "civil_from_days mismatch at day ", days, ": fortran ", y, mo, dd, " arrow ", ay, amo, add
                call check(error, .false., trim(msg))
                return
            end if
            call arrow_days_from_civil(ay, amo, add, adays)
            if (adays /= days) then
                write(msg, '(a,i0,a,i0)') "days_from_civil mismatch: expected ", days, " arrow ", adays
                call check(error, .false., trim(msg))
                return
            end if
        end do
        ! dense sweep around the epoch (every day of ~5.5 years on each side)
        do days = -2000_int64, 2000_int64
            call d%set_raw(int(days, int32))
            call d%get(y, mo, dd)
            call arrow_civil_from_days(int(days, c_int64_t), ay, amo, add)
            if (y /= ay .or. mo /= amo .or. dd /= add) then
                write(msg, '(a,i0)') "civil_from_days mismatch in the dense epoch sweep at day ", days
                call check(error, .false., trim(msg))
                return
            end if
        end do
        ! targeted civil -> days comparisons
        do k = 1, size(CIVIL, 2)
            call d%set(CIVIL(1, k), CIVIL(2, k), CIVIL(3, k))
            call arrow_days_from_civil(int(CIVIL(1, k), c_int32_t), int(CIVIL(2, k), c_int32_t), &
                int(CIVIL(3, k), c_int32_t), adays)
            if (int(d%raw(), int64) /= adays) then
                write(msg, '(a,i0,"-",i0,"-",i0,a,i0,a,i0)') "days_from_civil mismatch for ", &
                    CIVIL(1, k), CIVIL(2, k), CIVIL(3, k), ": fortran ", d%raw(), " arrow ", adays
                call check(error, .false., trim(msg))
                return
            end if
        end do
        call check(error, .true., "cross-validation completed")
    end subroutine test_arrow_cross_validation

end module test_temporal_cpp
