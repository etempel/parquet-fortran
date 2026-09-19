!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> What `parquet_sphere` costs: draws per second in sky polygons and HEALPix pixels, with the
!> candidates each took, and the RA/Dec geometry per element.
!!
!! Three modes, and `all`:
!!
!! * `polygon` -- a box (acceptance 1), the chart L-shape, a 100-vertex great-circle polygon and a
!!   thin diagonal strip just above the 1e-3 floor: nanoseconds per `%random_at`, the acceptance
!!   `%acceptance` reports, and the mean candidate count `parquet_debug_sphere_polygon_draw` measures,
!!   which must be close to its reciprocal.
!! * `pixel` -- `pf_random_pixel_at` at nside 64, 1024 and 2**20, nanoseconds per draw and candidates
!!   per draw, and `pf_random_mask_at` over a 1000-pixel list.
!! * `geometry` -- nanoseconds per element of `pf_radec2vec`, `pf_vec2radec`, `pf_offset_radec`,
!!   `pf_position_angle_deg` and `pf_fibonacci_grid_radec`; the offset and the position angle
!!   are `parquet_skycoord`'s.
!!
!! Every row prints a checksum over what it drew, so a row that stopped doing its work shows as a
!! changed checksum rather than as a faster time. Usage and configuration are in
!! `bench/benchmark_sphere.sh`, which checks the optimisation flags before any number is believed.
program benchmark_sphere

    use iso_fortran_env, only : int64, real64, output_unit
    use parquet_sphere
    use parquet_skycoord, only : pf_offset_radec, pf_position_angle_deg
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime
#endif

    implicit none

    character(len=32) :: mode
    integer :: rounds
    integer(int64) :: draws

    call read_arguments(mode, draws, rounds)
    write (output_unit, '(a,a,a,i0,a,i0)') "mode=", trim(mode), "  draws=", draws, "  rounds=", rounds
    if (mode == "polygon" .or. mode == "all") call polygon_rows(draws, rounds)
    if (mode == "pixel" .or. mode == "all") call pixel_rows(draws, rounds)
    if (mode == "geometry" .or. mode == "all") call geometry_rows(draws, rounds)

contains

    !> Reads `--mode=`, `--draws=` and `--rounds=`, with the wrapper's defaults.
    subroutine read_arguments(mode, draws, rounds)
        character(len=32), intent(out) :: mode !! polygon, pixel, geometry or all
        integer(int64), intent(out) :: draws !! draws (or elements) per timed round
        integer, intent(out) :: rounds !! timed rounds; the fastest is kept
        character(len=64) :: arg
        integer :: k

        mode = "all"
        draws = 200000_int64
        rounds = 3
        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            if (arg(1:7) == "--mode=") mode = arg(8:)
            if (arg(1:8) == "--draws=") read (arg(9:), *) draws
            if (arg(1:9) == "--rounds=") read (arg(10:), *) rounds
        end do
    end subroutine read_arguments

    !> One polygon row: the best of `rounds` timed loops of `%random_at`, then the candidate count.
    subroutine polygon_row(label, poly, draws, rounds)
        character(len=*), intent(in) :: label !! what the polygon is
        type(pf_sky_polygon), intent(in) :: poly !! the polygon
        integer(int64), intent(in) :: draws !! draws per round
        integer, intent(in) :: rounds !! rounds
        real(real64) :: ra, dec, sum_ra, t0, best
        integer(int64) :: k, ncand, total
        integer :: r

        best = huge(1.0_real64)
        do r = 1, rounds
            sum_ra = 0.0_real64
            t0 = now()
            do k = 1_int64, draws
                call poly%random_at(20260917_int64, k, ra, dec)
                sum_ra = sum_ra + ra
            end do
            best = min(best, now() - t0)
        end do
        total = 0_int64
        do k = 1_int64, min(draws, 20000_int64)
            call parquet_debug_sphere_polygon_draw(poly, 20260917_int64, k, 1_int64, ra, dec, ncand)
            total = total + ncand
        end do
        write (output_unit, '(a,t34,f10.1,a,f11.5,a,f10.3,a,f12.4,a,es13.6)') label, 1.0e9_real64 * best / real(draws, real64), &
            " ns/draw  acceptance ", poly%acceptance(), "  candidates ", &
            real(total, real64) / real(min(draws, 20000_int64), real64), "  1/acceptance ", 1.0_real64 / poly%acceptance(), &
            "  checksum ", sum_ra
    end subroutine polygon_row

    !> The polygon rows.
    subroutine polygon_rows(draws, rounds)
        integer(int64), intent(in) :: draws !! draws per round
        integer, intent(in) :: rounds !! rounds
        type(pf_sky_polygon) :: box, lshape, many, strip
        real(real64) :: ra(100), dec(100), a
        integer :: k

        call box%init([10.0_real64, 30.0_real64, 30.0_real64, 10.0_real64], [-5.0_real64, -5.0_real64, 5.0_real64, 5.0_real64])
        call polygon_row("box, chart edges", box, draws, rounds)
        call lshape%init([0.0_real64, 20.0_real64, 20.0_real64, 10.0_real64, 10.0_real64, 0.0_real64], &
                         [0.0_real64, 0.0_real64, 10.0_real64, 10.0_real64, 20.0_real64, 20.0_real64])
        call polygon_row("L-shape, chart edges", lshape, draws, rounds)
        ! A 100-vertex polygon about (150, -30), its radius swinging between 12 and 20 degrees.
        do k = 1, 100
            a = 2.0_real64 * 3.141592653589793_real64 * real(k - 1, real64) / 100.0_real64
            call pf_offset_radec(150.0_real64, -30.0_real64, 360.0_real64 * real(k - 1, real64) / 100.0_real64, &
                                 16.0_real64 + 4.0_real64 * sin(5.0_real64 * a), ra(k), dec(k))
        end do
        call many%init(ra, dec, PF_EDGE_GREAT_CIRCLE)
        call polygon_row("100 vertices, great circles", many, draws / 10_int64, rounds)
        ! A diagonal strip whose acceptance is just above the floor.
        call strip%init([0.0_real64, 40.0_real64, 40.0_real64, 0.0_real64], [0.0_real64, 40.0_real64, 40.2_real64, 0.2_real64])
        call polygon_row("thin strip near the floor", strip, draws / 100_int64, rounds)
    end subroutine polygon_rows

    !> The pixel and mask rows.
    subroutine pixel_rows(draws, rounds)
        integer(int64), intent(in) :: draws !! draws per round
        integer, intent(in) :: rounds !! rounds
        integer(int64), parameter :: NSIDES(3) = [64_int64, 1024_int64, 1048576_int64]
        type(pf_healpix_grid) :: grid
        integer(int64) :: k, ncand, total, list(1000), ipix
        real(real64) :: v(3), sum_z, t0, best
        integer :: g, r

        do g = 1, size(NSIDES)
            call grid%init(NSIDES(g), PF_HP_NEST)
            ipix = 6_int64 * NSIDES(g) * NSIDES(g) + 2_int64 * NSIDES(g)
            best = huge(1.0_real64)
            do r = 1, rounds
                sum_z = 0.0_real64
                t0 = now()
                do k = 1_int64, draws
                    v = pf_random_pixel_at(grid, 20260917_int64, k, ipix)
                    sum_z = sum_z + v(3)
                end do
                best = min(best, now() - t0)
            end do
            total = 0_int64
            do k = 1_int64, min(draws, 20000_int64)
                call parquet_debug_sphere_pixel_draw(grid, 20260917_int64, k, ipix, 1_int64, v, ncand)
                total = total + ncand
            end do
            write (output_unit, '(a,i0,t34,f10.1,a,f10.3,a,es13.6)') "pixel draw, nside ", NSIDES(g), &
                1.0e9_real64 * best / real(draws, real64), " ns/draw  candidates ", &
                real(total, real64) / real(min(draws, 20000_int64), real64), "  checksum ", sum_z
        end do

        call grid%init(1024_int64, PF_HP_RING)
        do k = 1_int64, 1000_int64
            list(k) = 37_int64 * k
        end do
        best = huge(1.0_real64)
        do r = 1, rounds
            sum_z = 0.0_real64
            t0 = now()
            do k = 1_int64, draws
                v = pf_random_mask_at(grid, 20260917_int64, k, list)
                sum_z = sum_z + v(3)
            end do
            best = min(best, now() - t0)
        end do
        write (output_unit, '(a,t34,f10.1,a,es13.6)') "mask draw, 1000 pixels", &
            1.0e9_real64 * best / real(draws, real64), " ns/draw  checksum ", sum_z
    end subroutine pixel_rows

    !> The geometry rows, per element.
    subroutine geometry_rows(draws, rounds)
        integer(int64), intent(in) :: draws !! elements per round
        integer, intent(in) :: rounds !! rounds
        real(real64), allocatable :: ra(:), dec(:), ra2(:), dec2(:)
        real(real64) :: v(3), t0, best(5), check(5)
        integer(int64) :: k
        integer :: r

        allocate(ra(draws), dec(draws), ra2(draws), dec2(draws))
        call pf_fibonacci_grid_radec(draws, ra, dec)
        best = huge(1.0_real64)
        do r = 1, rounds
            check = 0.0_real64
            t0 = now()
            do k = 1_int64, draws
                call pf_radec2vec(ra(k), dec(k), v)
                check(1) = check(1) + v(3)
            end do
            best(1) = min(best(1), now() - t0)
            t0 = now()
            do k = 1_int64, draws
                v = [cos(dec(k)), sin(ra(k)), cos(ra(k))]
                call pf_vec2radec(v, ra2(k), dec2(k))
                check(2) = check(2) + dec2(k)
            end do
            best(2) = min(best(2), now() - t0)
            t0 = now()
            call pf_offset_radec(ra, dec, 33.0_real64, 1.5_real64, ra2, dec2)
            best(3) = min(best(3), now() - t0)
            check(3) = sum(dec2)
            t0 = now()
            check(4) = sum(pf_position_angle_deg(ra, dec, ra2, dec2))
            best(4) = min(best(4), now() - t0)
            t0 = now()
            call pf_fibonacci_grid_radec(draws, ra2, dec2)
            best(5) = min(best(5), now() - t0)
            check(5) = sum(ra2)
        end do
        write (output_unit, '(a,t34,f10.2,a,es13.6)') "pf_radec2vec", 1.0e9_real64 * best(1) / real(draws, real64), &
            " ns/element  checksum ", check(1)
        write (output_unit, '(a,t34,f10.2,a,es13.6)') "pf_vec2radec", 1.0e9_real64 * best(2) / real(draws, real64), &
            " ns/element  checksum ", check(2)
        write (output_unit, '(a,t34,f10.2,a,es13.6)') "pf_offset_radec (elemental)", &
            1.0e9_real64 * best(3) / real(draws, real64), " ns/element  checksum ", check(3)
        write (output_unit, '(a,t34,f10.2,a,es13.6)') "pf_position_angle_deg (elemental)", &
            1.0e9_real64 * best(4) / real(draws, real64), " ns/element  checksum ", check(4)
        write (output_unit, '(a,t34,f10.2,a,es13.6)') "pf_fibonacci_grid_radec", &
            1.0e9_real64 * best(5) / real(draws, real64), " ns/element  checksum ", check(5)
    end subroutine geometry_rows

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

end program benchmark_sphere
