!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Whether a prepared `pf_sky_rotation` converts a column faster than `pf_sky_convert`: the
!> measurement that decides whether `pf_sky_convert` should become a wrapper over the object.
!!
!! One `real64` column of positions spread evenly over the sphere, converted ICRS to Galactic by
!! three arms, each an elemental call over the whole column and each the best of `--rounds` timed
!! calls, the arms interleaved round by round:
!!
!! * `convert` -- `pf_sky_convert(lon, lat, from, to, ...)`: the two selectors read per element, then
!!   the named procedure the pair has;
!! * `object`  -- `rot%init` once, then `rot%apply`: no selector read per element;
!! * `named`   -- `pf_icrs2gal`, the floor: no selector and no object.
!!
!! Then the same `convert` and `object` for Galactic to ecliptic, a pair with no named procedure,
!! where `pf_sky_convert` rotates by the pair's own matrix after its dispatch.
!!
!! The rule the numbers feed (`convert / object` is the ratio to read): `pf_sky_convert` stays
!! `elemental` unless the object is at least twice as fast, flagless and at `--profile release`, on
!! every machine measured. Every arm prints a checksum over what it wrote, and the arms of one pair
!! must agree to rounding -- the largest difference is printed -- so an arm that stopped doing its
!! work shows. Usage and configuration are in `bench/benchmark_skycoord.sh`, which checks the
!! optimisation flags before any number is believed.
program benchmark_skycoord

    use iso_fortran_env, only : int64, real64, output_unit
    use parquet_skycoord
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime
#endif

    implicit none

    integer(int64) :: n
    integer :: rounds
    real(real64), allocatable :: lon(:), lat(:), lo(:, :), la(:, :)
    real(real64) :: best(5), t0, diff_named, diff_unnamed
    type(pf_sky_rotation) :: rot, rot2
    integer :: r, k

    call read_arguments(n, rounds)
    write (output_unit, '(a,i0,a,i0)') "elements=", n, "  rounds=", rounds
    allocate(lon(n), lat(n), lo(n, 5), la(n, 5))
    call fill_positions(lon, lat)

    ! Warm every output page, and the code, through every arm once before anything is timed.
    call rot%init(PF_COORD_ICRS, PF_COORD_GALACTIC)
    call rot2%init(PF_COORD_GALACTIC, PF_COORD_ECLIPTIC)
    call run_arm(1)
    call run_arm(2)
    call run_arm(3)
    call run_arm(4)
    call run_arm(5)

    best = huge(1.0_real64)
    do r = 1, rounds
        do k = 1, 5
            t0 = now()
            call run_arm(k)
            best(k) = min(best(k), now() - t0)
        end do
    end do

    diff_named = max(maxval(abs(la(:, 1) - la(:, 2))), maxval(abs(la(:, 1) - la(:, 3))))
    diff_unnamed = maxval(abs(la(:, 4) - la(:, 5)))
    write (output_unit, '(a)') "ICRS to Galactic (a pair with a named procedure):"
    call report("  convert   pf_sky_convert", best(1), 1)
    call report("  object    rot%apply", best(2), 2)
    call report("  named     pf_icrs2gal", best(3), 3)
    write (output_unit, '(a,f8.3,a,f8.3)') "  convert / object ", best(1) / best(2), "   convert / named ", best(1) / best(3)
    write (output_unit, '(a,es10.3,a)') "  largest latitude difference between the arms ", diff_named, " degrees"
    write (output_unit, '(a)') "Galactic to ecliptic (a pair with no named procedure):"
    call report("  convert   pf_sky_convert", best(4), 4)
    call report("  object    rot%apply", best(5), 5)
    write (output_unit, '(a,f8.3)') "  convert / object ", best(4) / best(5)
    write (output_unit, '(a,es10.3,a)') "  largest latitude difference between the arms ", diff_unnamed, " degrees"
    if (min(best(1) / best(2), best(4) / best(5)) >= 2.0_real64) then
        write (output_unit, '(a)') "verdict: the object is at least twice as fast on both pairs -- the 2x bar is cleared here"
    else
        write (output_unit, '(a)') "verdict: the object is not twice as fast -- the 2x bar is not cleared here"
    end if

contains

    !> Reads `--elements=` and `--rounds=`, with the wrapper's defaults.
    subroutine read_arguments(n, rounds)
        integer(int64), intent(out) :: n !! elements in the column
        integer, intent(out) :: rounds !! timed rounds per arm; the fastest is kept
        character(len=64) :: arg
        integer :: k

        n = 2000000_int64
        rounds = 5
        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            if (arg(1:11) == "--elements=") read (arg(12:), *) n
            if (arg(1:9) == "--rounds=") read (arg(10:), *) rounds
        end do
    end subroutine read_arguments

    !> Positions spread evenly over the sphere: a Fibonacci lattice, equal area per point, so no
    !! region of the sky -- a pole, where the kernel's work is not the same -- is over-represented.
    subroutine fill_positions(lon, lat)
        real(real64), intent(out) :: lon(:) !! longitudes, degrees
        real(real64), intent(out) :: lat(:) !! latitudes, degrees
        real(real64), parameter :: GOLDEN_ANGLE = 137.50776405003785_real64
        real(real64), parameter :: RAD2DEG = 57.295779513082321_real64
        integer(int64) :: k, m

        m = size(lon, kind=int64)
        do k = 1_int64, m
            lon(k) = modulo(GOLDEN_ANGLE * real(k, real64), 360.0_real64)
            lat(k) = asin(-1.0_real64 + (2.0_real64 * real(k, real64) - 1.0_real64) / real(m, real64)) * RAD2DEG
        end do
    end subroutine fill_positions

    !> One arm over the whole column, writing column `k` of the outputs.
    subroutine run_arm(k)
        integer, intent(in) :: k !! which arm
        select case (k)
        case (1)
            call pf_sky_convert(lon, lat, PF_COORD_ICRS, PF_COORD_GALACTIC, lo(:, 1), la(:, 1))
        case (2)
            call rot%apply(lon, lat, lo(:, 2), la(:, 2))
        case (3)
            call pf_icrs2gal(lon, lat, lo(:, 3), la(:, 3))
        case (4)
            call pf_sky_convert(lon, lat, PF_COORD_GALACTIC, PF_COORD_ECLIPTIC, lo(:, 4), la(:, 4))
        case (5)
            call rot2%apply(lon, lat, lo(:, 5), la(:, 5))
        end select
    end subroutine run_arm

    !> One arm's line: nanoseconds per element and its checksum.
    subroutine report(label, seconds, k)
        character(len=*), intent(in) :: label !! the arm
        real(real64), intent(in) :: seconds !! its best time over the column
        integer, intent(in) :: k !! its output column
        write (output_unit, '(a,t34,f10.3,a,es22.14)') label, 1.0e9_real64 * seconds / real(n, real64), &
            " ns/element  checksum ", sum(lo(:, k)) + sum(la(:, k))
    end subroutine report

    !> Seconds on an arbitrary origin: the wall clock under OpenMP, `system_clock` without it.
    function now() result(t)
        real(real64) :: t !! seconds, on an arbitrary origin
#ifdef _OPENMP
        t = omp_get_wtime()
#else
        integer(int64) :: c, rate

        call system_clock(c, rate)
        t = real(c, real64) / real(rate, real64)
#endif
    end function now

end program benchmark_skycoord
