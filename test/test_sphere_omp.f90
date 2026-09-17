!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> `parquet_sphere`'s samplers under OpenMP: a fill chunked over a team is the serial fill.
!>
!> A rejection sampler is where a schedule could most plausibly leak into the values -- the number of
!> candidates a point takes varies -- and the per-draw key is what keeps it out. So the polygon and the
!> mask fills are split into chunks, run over a dynamic schedule at three team sizes, and compared with
!> the serial fill bit for bit.
!>
!> **This suite is excluded from test-drive's per-test parallelism** (`suite_is_safe_to_parallelize`
!> in `test/test_runner_support.f90`), for the reason `test/test_random_omp.f90` gives: a region opened
!> inside test-drive's own team would be nested and get a team of one, and the comparison would then
!> pass without two threads ever running. One polygon object is built before each region and read,
!> never declared, inside it -- ifx cannot privatise a type with allocatable components in a `block`.
module test_sphere_omp

    ! Narrow imports, never `use parquet`: `check_test_runner_partition` keeps this suite in the
    ! runner that reaches no `bind(C)` call.
    use parquet_sphere
    use iso_fortran_env, only: int64, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
#ifdef _OPENMP
    use omp_lib, only: omp_get_num_threads
#endif

    implicit none
    private
    public :: collect_tests_sphere_omp

    !> The seed every arm uses.
    integer(int64), parameter :: seed = 20260917_int64

contains

    !> Registers every test in the `sphere_omp` suite.
    subroutine collect_tests_sphere_omp(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the suite's tests

        testsuite = [ &
            new_unittest("polygon and mask fills are identical under every schedule and thread count", &
                         test_region_fill_schedule) &
            ]
    end subroutine collect_tests_sphere_omp

    !> A chart polygon, a great-circle polygon and a mask, each filled serially and then in chunks over a
    !! dynamic schedule at teams of 1, 2 and 8: every value bit-identical to the serial fill.
    subroutine test_region_fill_schedule(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NCOL = 6000
        integer, parameter :: CHUNK = 37
        integer, parameter :: teams(3) = [1, 2, 8]
        ! A variable, not a named constant: nagfor 7.2 generates C naming an undeclared identifier for a
        ! procedure's array PARAMETER referenced inside a parallel region.
        integer(int64) :: pixels(6)
        type(pf_sky_polygon) :: chart, gc
        type(pf_healpix_grid) :: grid
        real(real64), allocatable :: ra(:, :), dec(:, :), sra(:, :), sdec(:, :), mask(:, :), smask(:, :)
        integer :: ti, c, lo, hi, team
        character(len=100) :: msg

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the region below is a serial loop, so every team size " // &
            "would be the serial fill compared with itself")
        return
#endif
        call chart%init([0.0_real64, 20.0_real64, 20.0_real64, 10.0_real64, 10.0_real64, 0.0_real64], &
                        [0.0_real64, 0.0_real64, 10.0_real64, 10.0_real64, 20.0_real64, 20.0_real64])
        call gc%init([10.0_real64, 60.0_real64, 50.0_real64, 0.0_real64], [0.0_real64, 5.0_real64, 40.0_real64, 30.0_real64], &
                     PF_EDGE_GREAT_CIRCLE)
        call grid%init(16_int64, PF_HP_NEST)
        pixels = [100_int64, 101_int64, 2000_int64, 2000_int64, 3000_int64, 777_int64]
        allocate(ra(2, NCOL), dec(2, NCOL), sra(2, NCOL), sdec(2, NCOL), mask(3, NCOL), smask(3, NCOL))
        call chart%random_fill(seed, 3_int64, sra(1, :), sdec(1, :))
        call gc%random_fill(seed, 3_int64, sra(2, :), sdec(2, :))
        call pf_random_fill_mask(grid, seed, 3_int64, pixels, smask)
        do ti = 1, size(teams)
            ra = 0.0_real64
            dec = 0.0_real64
            mask = 0.0_real64
            team = 1
            !$omp parallel do schedule(dynamic, 1) num_threads(teams(ti)) default(shared) private(c, lo, hi)
            do c = 1, (NCOL + CHUNK - 1) / CHUNK
#ifdef _OPENMP
                if (c == 1) team = omp_get_num_threads()
#endif
                lo = (c - 1) * CHUNK + 1
                hi = min(NCOL, c * CHUNK)
                call chart%random_fill(seed, 3_int64, ra(1, lo:hi), dec(1, lo:hi), int(lo, int64))
                call gc%random_fill(seed, 3_int64, ra(2, lo:hi), dec(2, lo:hi), int(lo, int64))
                call pf_random_fill_mask(grid, seed, 3_int64, pixels, mask(:, lo:hi), int(lo, int64))
            end do
            !$omp end parallel do
            if (teams(ti) > 1) then
                call check(error, team > 1, "vacuity guard: the fills' region ran with a team of one, so nothing was varied")
                if (allocated(error)) return
            end if
            if (any(ra /= sra) .or. any(dec /= sdec) .or. any(mask /= smask)) then
                write (msg, '(a,i0,a)') "a polygon or mask fill chunked over ", teams(ti), " threads differs from the serial fill"
                call check(error, .false., trim(msg))
                return
            end if
        end do
    end subroutine test_region_fill_schedule

end module test_sphere_omp
