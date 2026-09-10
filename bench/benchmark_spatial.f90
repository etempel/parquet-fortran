!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Benchmarks `pf_spatial_index`: the cell-size tuner, the build, the queries and the threading.
!>
!> **Driven by `bench/benchmark_spatial.sh`, which is the supported entry point** -- it asserts
!> `--profile release` really reached the compile line, which is the one mistake that silently
!> turns every figure below into an `-O0` measurement.
!>
!> Five modes, each answering a question the design document leaves open. The whole point of
!> running this on a second machine is that three of them are ARCHITECTURE-dependent:
!>
!> * `--mode=tune` -- **the acceptance measurement.** Sweeps the cell size, times a fixed query
!>   workload at each, and reports how far the cell the tuner picked lands from the swept optimum.
!>   `feature_pandas_S3_cubesort.md` predicts a mean penalty of ~1.2% and a worst case of ~7.8%
!>   against the model alone reaching +42.9%; this either reproduces that or reopens the question.
!> * `--mode=ab` -- **re-fits the probe's `A/B` constant on this machine.** The probe ranks a
!>   candidate cell by `A*cells_visited + B*points_tested`; `A/B = 2` was fitted on arm64/NEON and
!>   is a ratio of a cache-miss-ish cost to an arithmetic-ish one, which is exactly what differs on
!>   AVX2 and AVX-512. This prints, for each swept cell, the measured time beside the two counts,
!>   then scores every candidate ratio by how often it picks the timed winner.
!> * `--mode=build` -- **what the probe costs.** Times a tuned build against a build at the same
!>   cell with `cell=` given (which skips the probe entirely), so the difference is the probe.
!> * `--mode=query` -- single-query and bulk-sweep throughput at the tuned cell.
!> * `--mode=threads` -- how a bulk sweep scales.
!> * `--mode=backend` -- **the 3D grid against the HEALPix pixelisation, on the sky.** Builds the
!>   same `(ra, dec)` catalogue with each backend and times `%within_sky` across a radius sweep
!>   from `R/3` to `3R` about the build radius. It is the only mode that uses `--skyr` and the
!>   only one whose fixture is angular rather than Cartesian, so `--side` does not reach it.
!>
!> **Every timed figure is a best-of-N**, because a single round swings more than the effects being
!> measured. Report the noise floor with any result: rebuilding the same source moves untouched
!> arms by more than re-running one binary does.
!>
!> Fixtures match the engine gate's (`feature_pandas_S3.md`) so figures are comparable: `uniform`,
!> `clustered`, `wedge` (a sparse survey cone), `sphere` (a zero-thickness shell -- the worst case
!> for a dense cell array) and `flat` (2D).
program benchmark_spatial
    use parquet_spatial
    use parquet_random, only : pf_random_at
    use iso_fortran_env, only : int64, real64, output_unit
#ifdef _OPENMP
    use omp_lib, only : omp_get_wtime, omp_get_max_threads
#endif
    implicit none

    integer, parameter :: max_cells = 24 !! most cell sizes one sweep may carry.

    character(len=32) :: mode, dist
    integer(int64) :: np, nq
    integer :: rounds, threads
    real(real64) :: side, rlo, rhi, skyr
    real(real64), allocatable :: x(:), y(:), z(:)

    mode = "tune"
    dist = "uniform"
    np = 1000000_int64
    nq = 20000_int64
    rounds = 3
    threads = 0
    side = 100.0_real64
    rlo = 1.0_real64
    rhi = 5.0_real64
    skyr = 1.0_real64
    call read_arguments()

    write (output_unit, '(a)') "# benchmark_spatial"
    write (output_unit, '(a,a)') "# mode      : ", trim(mode)
    write (output_unit, '(a,a)') "# fixture   : ", trim(dist)
    write (output_unit, '(a,i0)') "# points    : ", np
    write (output_unit, '(a,i0)') "# queries   : ", nq
    write (output_unit, '(a,i0)') "# rounds    : ", rounds
    write (output_unit, '(a,f0.3,a,f0.3)') "# radii     : ", rlo, " .. ", rhi
    write (output_unit, '(a,i0)') "# omp max   : ", omp_available()
    call make_cloud()

    select case (trim(mode))
    case ("tune")
        call mode_tune()
    case ("ab")
        call mode_ab()
    case ("build")
        call mode_build()
    case ("query")
        call mode_query()
    case ("combine")
        call mode_combine()
    case ("threads")
        call mode_threads()
    case ("backend")
        call mode_backend()
    case default
        write (output_unit, '(a)') "unknown --mode: " // trim(mode)
        error stop 2
    end select

contains

    !> Wall-clock seconds. `omp_get_wtime` when there is OpenMP, `system_clock` otherwise -- an
    !> unguarded `use omp_lib` is a COMPILE failure rather than a graceful fallback to serial.
    real(real64) function wtime() result(t)
        integer(int64) :: c, rate

#ifdef _OPENMP
        t = omp_get_wtime()
#else
        call system_clock(count=c, count_rate=rate)
        t = real(c, kind=real64) / real(rate, kind=real64)
#endif
    end function wtime

    !> How many threads OpenMP offers, or 1 without it.
    integer function omp_available() result(n)

        n = 1
#ifdef _OPENMP
        n = omp_get_max_threads()
#endif
    end function omp_available

    !> Parses `--key=value` arguments.
    subroutine read_arguments()
        character(len=256) :: arg
        integer :: i, eq

        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            eq = index(arg, "=")
            if (eq < 2) cycle
            select case (arg(1:eq - 1))
            case ("--mode")
                mode = arg(eq + 1:)
            case ("--dist")
                dist = arg(eq + 1:)
            case ("--np")
                read (arg(eq + 1:), *) np
            case ("--nq")
                read (arg(eq + 1:), *) nq
            case ("--rounds")
                read (arg(eq + 1:), *) rounds
            case ("--threads")
                read (arg(eq + 1:), *) threads
            case ("--skyr")
                read (arg(eq + 1:), *) skyr
            case ("--side")
                read (arg(eq + 1:), *) side
            case ("--rlo")
                read (arg(eq + 1:), *) rlo
            case ("--rhi")
                read (arg(eq + 1:), *) rhi
            case default
                write (output_unit, '(a)') "unknown argument: " // trim(arg)
                error stop 2
            end select
        end do
    end subroutine read_arguments

    !> Builds the requested fixture. Named, never selected by an `if` chain that can leave a branch
    !> unreachable -- the engine gate lost a whole fixture to exactly that and reported the results
    !> of a different cloud under its name.
    subroutine make_cloud()
        integer(int64) :: i, nc, c
        real(real64) :: u1, u2, u3, ra, dec, t, rad, cw

        allocate (x(np), y(np), z(np))
        select case (trim(dist))
        case ("uniform")
            do i = 1_int64, np
                x(i) = side * pf_random_at(11_int64, i, 1_int64)
                y(i) = side * pf_random_at(11_int64, i, 2_int64)
                z(i) = side * pf_random_at(11_int64, i, 3_int64)
            end do
        case ("clustered")
            ! 200 gaussian-ish clumps of width side/200, on a uniform background of 10%.
            nc = 200_int64
            cw = side / 200.0_real64
            do i = 1_int64, np
                u1 = pf_random_at(11_int64, i, 1_int64)
                if (u1 < 0.1_real64) then
                    x(i) = side * pf_random_at(11_int64, i, 2_int64)
                    y(i) = side * pf_random_at(11_int64, i, 3_int64)
                    z(i) = side * pf_random_at(11_int64, i, 4_int64)
                else
                    c = 1_int64 + int(real(nc, kind=real64) * pf_random_at(11_int64, i, 5_int64), kind=int64)
                    x(i) = side * pf_random_at(77_int64, c, 1_int64) + cw * gauss(i, 1_int64)
                    y(i) = side * pf_random_at(77_int64, c, 2_int64) + cw * gauss(i, 3_int64)
                    z(i) = side * pf_random_at(77_int64, c, 3_int64) + cw * gauss(i, 5_int64)
                end if
            end do
        case ("wedge")
            ! A survey cone: a few per cent of its own bounding box, which is what makes the dense
            ! cell array's worst realistic case.
            do i = 1_int64, np
                t = pf_random_at(11_int64, i, 1_int64) ** (1.0_real64 / 3.0_real64)
                u2 = pf_random_at(11_int64, i, 2_int64)
                u3 = pf_random_at(11_int64, i, 3_int64)
                ra = 0.35_real64 * (u2 - 0.5_real64)
                dec = 0.35_real64 * (u3 - 0.5_real64)
                rad = side * t
                x(i) = rad * cos(dec) * cos(ra)
                y(i) = rad * cos(dec) * sin(ra)
                z(i) = rad * sin(dec)
            end do
        case ("sphere")
            ! A zero-thickness shell inside its bounding cube: the geometry that maximises empty
            ! cells, and so the case the cell-count clamp exists for.
            do i = 1_int64, np
                u2 = 2.0_real64 * pf_random_at(11_int64, i, 1_int64) - 1.0_real64
                ra = 6.283185307179586_real64 * pf_random_at(11_int64, i, 2_int64)
                t = sqrt(max(0.0_real64, 1.0_real64 - u2 * u2))
                x(i) = side * t * cos(ra)
                y(i) = side * t * sin(ra)
                z(i) = side * u2
            end do
        case ("flat")
            do i = 1_int64, np
                x(i) = side * pf_random_at(11_int64, i, 1_int64)
                y(i) = side * pf_random_at(11_int64, i, 2_int64)
                z(i) = 0.0_real64
            end do
        case default
            write (output_unit, '(a)') "unknown --dist: " // trim(dist)
            error stop 2
        end select
    end subroutine make_cloud

    !> One standard normal from two uniforms, Box-Muller. Only the fixtures use it.
    real(real64) function gauss(i, d) result(g)
        integer(int64), intent(in) :: i !! stream index.
        integer(int64), intent(in) :: d !! draw index; `d` and `d+1` are consumed.
        real(real64) :: u1, u2

        u1 = max(1.0e-12_real64, pf_random_at(31_int64, i, d))
        u2 = pf_random_at(31_int64, i, d + 1_int64)
        g = sqrt(-2.0_real64 * log(u1)) * cos(6.283185307179586_real64 * u2)
    end function gauss

    !> The cell sizes a sweep visits, geometric about the tuner's own answer.
    subroutine sweep_cells(h_tuned, cells, n)
        real(real64), intent(in) :: h_tuned !! the tuner's chosen cell.
        real(real64), intent(out) :: cells(max_cells) !! the sweep points.
        integer, intent(out) :: n !! how many were filled.
        integer :: k

        n = 13
        do k = 1, n
            cells(k) = h_tuned * 2.0_real64 ** (real(k - 7, kind=real64) * 0.5_real64)
        end do
    end subroutine sweep_cells

    !> Times `nq` single ball queries at radii cycling through `[rlo, rhi]`. Best of `rounds`.
    real(real64) function time_queries(sx, keep) result(best)
        type(pf_spatial_index), intent(in) :: sx !! the index to query.
        integer(int64), intent(out) :: keep !! total neighbours found, so nothing is optimised away.
        integer(int64), allocatable :: out(:)
        integer(int64) :: q, m, total
        integer :: it
        real(real64) :: t0, t1, p(3), r

        allocate (out(4096))
        best = huge(1.0_real64)
        keep = 0_int64
        do it = 1, rounds
            total = 0_int64
            t0 = wtime()
            do q = 1_int64, nq
                p(1) = side * pf_random_at(5_int64, q, 1_int64)
                p(2) = side * pf_random_at(5_int64, q, 2_int64)
                p(3) = side * pf_random_at(5_int64, q, 3_int64)
                r = rlo + (rhi - rlo) * pf_random_at(5_int64, q, 4_int64)
                m = sx%count_within(p, r)
                total = total + m
            end do
            t1 = wtime()
            best = min(best, t1 - t0)
            keep = total
        end do
    end function time_queries


    !> The 3D grid against the HEALPix pixelisation, on the same sky catalogue.
    !>
    !> **The end comparison the whole backend feature exists to be judged by.** Both indexes are
    !> built from one `(ra, dec)` fixture at one radius, then queried across a radius sweep about
    !> that radius -- because the interesting behaviour is not at the tuned radius but away from
    !> it, where the two backends' costs diverge in opposite directions.
    !>
    !> **It asserts the two agree before reporting any timing.** A backend that returns a shorter
    !> neighbour list is faster for a reason no benchmark should reward, and that failure mode --
    !> silently losing edge points -- is exactly the one the walk's `inclusive` mode exists to
    !> prevent. A mismatch stops the run.
    !>
    !> `--dist=clustered` is the fixture to read most carefully. The 3D grid buckets a sphere in a
    !> cube, so its cells are coarse and a cluster falls in few of them and is tested in full; a
    !> disc's pixels can exclude most of that same cluster. If the two backends differ anywhere by
    !> more than they differ on `uniform`, this is where.
    subroutine mode_backend()
        real(real64), allocatable :: ra(:), dec(:), qra(:), qdec(:)
        type(pf_spatial_index) :: s3, sh
        integer(int64), allocatable :: o3(:), oh(:)
        integer(int64) :: i, q, m3, mh, keep3, keeph, nc, c
        integer :: it, k
        real(real64) :: t0, t1, b3, bh, u1, cw, rq
        real(real64) :: q3, qh
        real(real64), parameter :: rad2deg = 57.29577951308232_real64
        real(real64), parameter :: mult(5) = &
            [1.0_real64 / 3.0_real64, 0.5_real64, 1.0_real64, 2.0_real64, 3.0_real64]

        allocate (ra(np), dec(np))
        select case (trim(dist))
        case ("uniform", "sphere")
            ! Uniform on the sphere: `sin(dec)` uniform, not `dec`, or the poles are oversampled.
            do i = 1_int64, np
                ra(i) = 360.0_real64 * pf_random_at(11_int64, i, 1_int64)
                dec(i) = rad2deg * asin(2.0_real64 * pf_random_at(11_int64, i, 2_int64) - 1.0_real64)
            end do
        case ("clustered")
            ! 200 clumps a few times the build radius across, on a 10% uniform background -- the
            ! same shape as the Cartesian clustered fixture, projected onto the sphere.
            nc = 200_int64
            cw = 3.0_real64 * skyr
            do i = 1_int64, np
                u1 = pf_random_at(11_int64, i, 1_int64)
                if (u1 < 0.1_real64) then
                    ra(i) = 360.0_real64 * pf_random_at(11_int64, i, 2_int64)
                    dec(i) = rad2deg * asin(2.0_real64 * pf_random_at(11_int64, i, 3_int64) - 1.0_real64)
                else
                    c = 1_int64 + int(real(nc, kind=real64) * pf_random_at(11_int64, i, 5_int64), kind=int64)
                    ra(i) = modulo(360.0_real64 * pf_random_at(77_int64, c, 1_int64) + cw * gauss(i, 1_int64), &
                                   360.0_real64)
                    dec(i) = max(-90.0_real64, min(90.0_real64, &
                        rad2deg * asin(2.0_real64 * pf_random_at(77_int64, c, 2_int64) - 1.0_real64) &
                        + cw * gauss(i, 3_int64)))
                end if
            end do
        case default
            write (output_unit, '(a)') "--mode=backend takes --dist=uniform or --dist=clustered"
            error stop 2
        end select

        ! Query points drawn from the catalogue itself, which is what a self-join or a cross-match
        ! actually does and is already density-weighted on a clustered fixture.
        allocate (qra(nq), qdec(nq))
        do q = 1_int64, nq
            i = 1_int64 + modulo(q * 2654435761_int64, np)
            qra(q) = ra(i)
            qdec(q) = dec(i)
        end do

        b3 = huge(1.0_real64)
        bh = huge(1.0_real64)
        do it = 1, rounds
            call s3%clear()
            t0 = wtime()
            call s3%build_sky(ra, dec, radius_deg=skyr)
            t1 = wtime()
            b3 = min(b3, t1 - t0)
            call sh%clear()
            t0 = wtime()
            call sh%build_sky(ra, dec, radius_deg=skyr, backend=PF_SKY_HEALPIX)
            t1 = wtime()
            bh = min(bh, t1 - t0)
        end do

        write (output_unit, '(a,f0.4,a)') "# build radius : ", skyr, " deg"
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "backend      buckets    nside     build s"
        write (output_unit, '(a,i11,i9,f12.4)') "grid3d ", s3%cells(), 0_int64, b3
        write (output_unit, '(a,i11,i9,f12.4)') "healpix", sh%cells(), sh%nside(), bh
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  radius/R   radius deg      hits    grid3d us    healpix us   healpix/grid3d"

        allocate (o3(np), oh(np))
        do k = 1, size(mult)
            rq = skyr * mult(k)
            ! Correctness before timing: a faster backend that answers differently is not faster.
            m3 = s3%within_sky(qra(1), qdec(1), rq, o3, sorted=.true.)
            mh = sh%within_sky(qra(1), qdec(1), rq, oh, sorted=.true.)
            if (mh /= m3) then
                write (output_unit, '(a)') "BACKENDS DISAGREE on the neighbour count -- refusing to time"
                error stop 3
            end if
            if (m3 > 0_int64) then
                if (any(oh(1:m3) /= o3(1:m3))) then
                    write (output_unit, '(a)') "BACKENDS DISAGREE on the rows -- refusing to time"
                    error stop 3
                end if
            end if
            q3 = time_sky(s3, rq, qra, qdec, keep3)
            qh = time_sky(sh, rq, qra, qdec, keeph)
            if (keeph /= keep3) then
                write (output_unit, '(a)') "BACKENDS DISAGREE over the sweep -- refusing to report"
                error stop 3
            end if
            write (output_unit, '(f10.4,f13.5,i10,f13.4,f14.4,f17.3)') mult(k), rq, &
                keep3 / nq, 1.0e6_real64 * q3 / real(nq, kind=real64), &
                1.0e6_real64 * qh / real(nq, kind=real64), qh / q3
        end do
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "# healpix/grid3d below 1 means HEALPix is faster."
    end subroutine mode_backend

    !> Times `nq` sky queries at one radius, best of `rounds`.
    real(real64) function time_sky(sx, rq, qra, qdec, keep) result(best)
        type(pf_spatial_index), intent(in) :: sx !! the index to query.
        real(real64), intent(in) :: rq !! the angular radius, degrees.
        real(real64), intent(in) :: qra(:) !! query right ascensions.
        real(real64), intent(in) :: qdec(:) !! query declinations.
        integer(int64), intent(out) :: keep !! total neighbours found, so nothing is elided.
        integer(int64), allocatable :: out(:)
        integer(int64) :: q, total
        integer :: it
        real(real64) :: t0, t1

        ! **A zero-length buffer, deliberately.** `%within_sky` returns the TRUE count whatever
        ! the buffer holds, so this times the WALK -- the thing the backends differ in -- without
        ! also timing a store per hit, which they share and which would dilute the comparison at
        ! large radii where the hit count runs into the thousands.
        allocate (out(0))
        best = huge(1.0_real64)
        keep = 0_int64
        do it = 1, rounds
            total = 0_int64
            t0 = wtime()
            do q = 1_int64, nq
                total = total + sx%within_sky(qra(q), qdec(q), rq, out)
            end do
            t1 = wtime()
            best = min(best, t1 - t0)
            keep = total
        end do
    end function time_sky

    !> The acceptance measurement: how far the tuner's cell lands from the swept optimum.
    subroutine mode_tune()
        type(pf_spatial_index) :: sx
        real(real64) :: cells(max_cells), times(max_cells), used(max_cells)
        real(real64) :: h_tuned, t_tuned, best_t, penalty
        character(len=:), allocatable :: saved_verbosity
        integer(int64) :: keep, ncand
        integer :: n, k, best_k

        call sx%build(x, y, z, radius=[rlo, rhi])
        h_tuned = sx%cell_size()
        ncand = parquet_debug_spatial_probe_count()
        ! Discarded: it is a WARM-UP, not a measurement. Timing the tuned arm first and the sweep
        ! afterwards would charge it for a machine that has not settled -- an ordered bias that
        ! best-of-N does not remove, and which read as a 9.5% penalty where the real one is 1.8%.
        ! The sweep is centred on the tuner's own cell, so `times(7)` IS the tuned arm, measured
        ! under conditions identical to every other row.
        t_tuned = time_queries(sx, keep)
        call sweep_cells(h_tuned, cells, n)
        ! The coarsening warning is correct and, over a sweep that deliberately asks for cells
        ! below the clamp, says the same thing a dozen times. The `cell used` column below reports
        ! it per row instead.
        call parquet_get_verbosity(saved_verbosity)
        call parquet_set_verbosity("errors_only")
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  cell asked    cell used     cells       us/query    vs tuned"
        best_t = huge(1.0_real64)
        best_k = 1
        do k = 1, n
            call sx%build(x, y, z, radius=[rlo, rhi], cell=cells(k))
            times(k) = time_queries(sx, keep)
            used(k) = sx%cell_size()
            if (times(k) < best_t) then
                best_t = times(k)
                best_k = k
            end if
        end do
        call parquet_set_verbosity(saved_verbosity)
        do k = 1, n
            call sx%build(x, y, z, radius=[rlo, rhi], cell=cells(k))
            write (output_unit, '(2x,es10.3,2x,es10.3,2x,i12,2x,f10.4,2x,f8.3,a)') cells(k), used(k), &
                sx%cells(), 1.0e6_real64 * times(k) / real(nq, kind=real64), times(k) / times(7), "x"
        end do
        t_tuned = times(7)
        penalty = 100.0_real64 * (t_tuned / best_t - 1.0_real64)
        write (output_unit, '(a)') ""
        write (output_unit, '(a,es10.3,a,i0,a)') "  tuner chose      : ", h_tuned, &
            "   (", ncand, " candidates probed)"
        ! Reported as the cell the grid actually USED. Below the cell-count clamp several swept
        ! points collapse onto one grid, so naming the cell that was ASKED for makes an identical
        ! grid look like a different and better one -- which is what a first reading of this table
        ! did, turning a 0.04% agreement into an apparent factor of eight.
        write (output_unit, '(a,es10.3,a,es10.3,a)') "  sweep optimum    : ", used(best_k), &
            "   (asked ", cells(best_k), ")"
        write (output_unit, '(a,f10.4)') "  tuned  us/query  : ", 1.0e6_real64 * t_tuned / real(nq, kind=real64)
        write (output_unit, '(a,f10.4)') "  best   us/query  : ", 1.0e6_real64 * best_t / real(nq, kind=real64)
        write (output_unit, '(a,f8.2,a)') "  PENALTY          : ", penalty, " %  (design predicts ~1.2% mean, ~7.8% worst)"
        write (output_unit, '(a)') "  (the sweep steps by sqrt(2); the probe's bracket steps by 2, so a residual of one"
        write (output_unit, '(a)') "   half-step is bracket GRANULARITY rather than a mis-ranking -- check the probe's own"
        write (output_unit, '(a)') "   candidates, at the tuned cell times 0.5, 1 and 2, against this table before concluding.)"
        write (output_unit, '(a,i0)') "  checksum         : ", keep
    end subroutine mode_tune

    !> Times `nq` queries drawn EXACTLY as the probe draws them: query points sampled from the
    !> cloud by the same golden-ratio stride, radii cycling the declared list.
    !>
    !> **`--mode=ab` must time what the probe counts, or it fits the constant against a workload the
    !> probe never faces.** The first version of this mode timed random points in the box at radii
    !> uniform in `[rlo, rhi]` while counting work at `r_eff` alone -- three different workloads, and
    !> a ratio fitted between two of them says little about the third. `--mode=tune` deliberately
    !> keeps the random-point workload, because there the question IS whether the tuner's pick serves
    !> a realistic query load; here the question is whether the proxy ranks what it claims to rank.
    real(real64) function time_probe_workload(sx, radii, keep) result(best)
        type(pf_spatial_index), intent(in) :: sx !! the index to query.
        real(real64), intent(in) :: radii(:) !! the radius list, cycled exactly as the probe cycles it.
        integer(int64), intent(out) :: keep !! total neighbours found, so nothing is optimised away.
        integer(int64) :: q, m, total, i, pos, stride
        integer :: it, nr
        real(real64) :: t0, t1, p(3)

        best = huge(1.0_real64)
        keep = 0_int64
        nr = size(radii)
        stride = max(1_int64, int(0.6180339887498949_real64 * real(np, kind=real64), kind=int64))
        do it = 1, rounds
            total = 0_int64
            pos = 0_int64
            t0 = wtime()
            do q = 1_int64, nq
                i = pos + 1_int64
                p(1) = x(i)
                p(2) = y(i)
                p(3) = z(i)
                m = sx%count_within(p, radii(1 + int(mod(q - 1_int64, int(nr, kind=int64)))))
                total = total + m
                pos = modulo(pos + stride, np)
            end do
            t1 = wtime()
            best = min(best, t1 - t0)
            keep = total
        end do
    end function time_probe_workload

    !> Re-fits the probe's `A/B` constant against timed reality on this machine.
    subroutine mode_ab()
        type(pf_spatial_index) :: sx
        real(real64) :: cells(max_cells), times(max_cells)
        real(real64) :: cw(max_cells), pw(max_cells)
        real(real64) :: h_tuned, ratio, best_score, work, penalty, worst
        integer(int64) :: keep, cc, pp
        character(len=:), allocatable :: saved_verbosity
        integer :: n, k, ir, best_k, pick, hits
        real(real64), parameter :: ratios(9) = [0.25_real64, 0.5_real64, 1.0_real64, 2.0_real64, &
            4.0_real64, 8.0_real64, 16.0_real64, 32.0_real64, 64.0_real64]

        call sx%build(x, y, z, radius=[rlo, rhi])
        h_tuned = sx%cell_size()
        call sweep_cells(h_tuned, cells, n)
        call parquet_get_verbosity(saved_verbosity)
        call parquet_set_verbosity("errors_only")
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  (both arms use the probe's own workload: cloud points, radii cycling the list)"
        write (output_unit, '(a)') "  cell          us/query       cells_visited     points_tested"
        best_k = 1
        do k = 1, n
            call sx%build(x, y, z, radius=[rlo, rhi], cell=cells(k))
            times(k) = time_probe_workload(sx, [rlo, rhi], keep)
            call parquet_debug_spatial_work(sx, cells(k), [rlo, rhi], cc, pp)
            cw(k) = real(cc, kind=real64)
            pw(k) = real(pp, kind=real64)
            write (output_unit, '(2x,es10.3,2x,f12.4,2x,i16,2x,i16)') cells(k), &
                1.0e6_real64 * times(k) / real(nq, kind=real64), cc, pp
            if (times(k) < times(best_k)) best_k = k
        end do
        call parquet_set_verbosity(saved_verbosity)
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  A/B     picks       penalty vs the timed optimum"
        do ir = 1, size(ratios)
            ratio = ratios(ir)
            best_score = huge(1.0_real64)
            pick = 1
            do k = 1, n
                work = ratio * cw(k) + pw(k)
                if (work < best_score) then
                    best_score = work
                    pick = k
                end if
            end do
            penalty = 100.0_real64 * (times(pick) / times(best_k) - 1.0_real64)
            hits = 0
            if (pick == best_k) hits = 1
            write (output_unit, '(2x,f6.2,2x,i8,2x,f14.2,a)') ratio, hits, penalty, " %"
        end do
        worst = 100.0_real64 * (times(n) / times(best_k) - 1.0_real64)
        write (output_unit, '(a)') ""
        write (output_unit, '(a,es10.3)') "  timed optimum cell : ", cells(best_k)
        write (output_unit, '(a,f8.2,a)') "  spread over sweep  : ", worst, " % (worst swept cell vs best)"
        write (output_unit, '(a,i0)') "  checksum           : ", keep
    end subroutine mode_ab

    !> What the probe costs: a tuned build against one at the same cell with `cell=` given.
    subroutine mode_build()
        type(pf_spatial_index) :: sx
        real(real64) :: t_auto, t_fixed, t0, h_tuned
        integer(int64) :: ncand
        integer :: it

        call sx%build(x, y, z, radius=[rlo, rhi])
        h_tuned = sx%cell_size()
        ncand = parquet_debug_spatial_probe_count()
        t_auto = huge(1.0_real64)
        do it = 1, rounds
            call sx%clear()
            t0 = wtime()
            call sx%build(x, y, z, radius=[rlo, rhi])
            t_auto = min(t_auto, wtime() - t0)
        end do
        t_fixed = huge(1.0_real64)
        do it = 1, rounds
            call sx%clear()
            t0 = wtime()
            call sx%build(x, y, z, radius=[rlo, rhi], cell=h_tuned)
            t_fixed = min(t_fixed, wtime() - t0)
        end do
        write (output_unit, '(a)') ""
        write (output_unit, '(a,f10.4,a,i0,a)') "  tuned build   : ", t_auto, " s   (", ncand, " candidates probed)"
        write (output_unit, '(a,f10.4,a)') "  cell= build   : ", t_fixed, " s   (no probe)"
        write (output_unit, '(a,f10.4,a)') "  probe costs   : ", t_auto - t_fixed, " s"
        write (output_unit, '(a,f8.3,a)') "  as a factor   : ", t_auto / t_fixed, "x the unprobed build"
        write (output_unit, '(a,es10.3)') "  chosen cell   : ", h_tuned
        write (output_unit, '(a,i0)') "  cells         : ", sx%cells()
    end subroutine mode_build

    !> Single-query and bulk-sweep throughput at the tuned cell.
    subroutine mode_query()
        type(pf_spatial_index) :: sx
        integer(int64), allocatable :: offs(:), nb(:), counts(:)
        integer(int64) :: keep, total
        real(real64) :: t_single, t0, t_count, t_csr, t_pairs
        integer :: it
        integer(int64), allocatable :: pi(:), pj(:)

        call sx%build(x, y, z, radius=[rlo, rhi])
        t_single = time_queries(sx, keep)
        t_count = huge(1.0_real64)
        do it = 1, rounds
            t0 = wtime()
            call sx%count_all_within(rlo, counts, threads=team())
            t_count = min(t_count, wtime() - t0)
        end do
        total = sum(counts)
        t_csr = huge(1.0_real64)
        do it = 1, rounds
            t0 = wtime()
            call sx%all_within(rlo, offs, nb, threads=team())
            t_csr = min(t_csr, wtime() - t0)
        end do
        t_pairs = huge(1.0_real64)
        do it = 1, rounds
            t0 = wtime()
            call sx%pairs_within(rlo, pi, pj, threads=team())
            t_pairs = min(t_pairs, wtime() - t0)
        end do
        write (output_unit, '(a)') ""
        write (output_unit, '(a,es10.3)') "  cell               : ", sx%cell_size()
        write (output_unit, '(a,f10.4)') "  single us/query    : ", 1.0e6_real64 * t_single / real(nq, kind=real64)
        write (output_unit, '(a,f10.4,a,i0,a)') "  count_all_within   : ", t_count, " s   (", total, " neighbours)"
        write (output_unit, '(a,f10.4,a,i0,a)') "  all_within (CSR)   : ", t_csr, " s   (", size(nb, kind=int64), " entries)"
        write (output_unit, '(a,f10.4,a,i0,a)') "  pairs_within       : ", t_pairs, " s   (", size(pi, kind=int64), " pairs)"
        write (output_unit, '(a,i0)') "  threads used       : ", parquet_debug_spatial_threads_used()
        write (output_unit, '(a,i0)') "  checksum           : ", keep
    end subroutine mode_query

    !> What each `combine=` rule costs on one per-point radius list.
    !>
    !> **All four arms run over the same fixture and the same radii**, because the question is not
    !> how fast any one rule is but what the rules that carry a per-candidate bound cost against
    !> the one that does not. `max` is the baseline: it is the released behaviour and the default.
    !> `min` should sit at or below it, walking smaller balls; `mean` pays two gathers and two
    !> multiplies per candidate and nothing else; `sum` walks twice each radius, which is eight
    !> times the volume in three dimensions, so it is expected to be several times the baseline
    !> and is reported as a ratio rather than compared with the other three.
    !>
    !> The pair COUNTS are printed beside the times and are the check that the arms did different
    !> work: they must increase strictly from `min` through `mean` and `max` to `sum`. Equal counts
    !> mean the radius spread is too narrow for this fixture to separate the rules, and the timings
    !> then measure one rule four times.
    subroutine mode_combine()
        type(pf_spatial_index) :: sx
        real(real64), allocatable :: rv(:)
        integer(int64), allocatable :: pi(:), pj(:)
        real(real64) :: t(4), t0
        integer(int64) :: npairs(4), i
        integer :: rules(4), ri, it
        character(len=4) :: names(4)

        rules = [PF_LINK_MIN, PF_LINK_MEAN, PF_LINK_MAX, PF_LINK_SUM]
        names = ["min ", "mean", "max ", "sum "]
        ! Radii spread over the declared range, so that every rule sees genuinely different values
        ! at the two ends of a pair. A single radius would make three of the four coincide.
        allocate (rv(np))
        do i = 1_int64, np
            rv(i) = rlo + (rhi - rlo) * pf_random_at(20260910_int64, i, 7_int64)
        end do
        call sx%build(x, y, z, radius=rv)
        do ri = 1, 4
            t(ri) = huge(1.0_real64)
            do it = 1, rounds
                t0 = wtime()
                call sx%pairs_within(rv, pi, pj, threads=team(), combine=rules(ri))
                t(ri) = min(t(ri), wtime() - t0)
            end do
            npairs(ri) = size(pi, kind=int64)
        end do
        write (output_unit, '(a)') ""
        write (output_unit, '(a,es10.3)') "  cell               : ", sx%cell_size()
        write (output_unit, '(a)') "  rule        seconds        pairs   vs max"
        do ri = 1, 4
            write (output_unit, '(a,a4,f12.4,i13,f9.3)') "  ", names(ri), t(ri), npairs(ri), t(ri) / t(3)
        end do
        write (output_unit, '(a,i0)') "  threads used       : ", parquet_debug_spatial_threads_used()
        if (.not. (npairs(1) < npairs(2) .and. npairs(2) < npairs(3) .and. npairs(3) < npairs(4))) then
            write (output_unit, '(a)') "  WARNING: the four pair counts do not strictly increase, so this " // &
                "fixture does not separate the rules and the timings above compare one rule with itself."
        end if
    end subroutine mode_combine

    !> How a bulk sweep scales with the team size.
    subroutine mode_threads()
        type(pf_spatial_index) :: sx
        integer(int64), allocatable :: counts(:)
        real(real64) :: t0, best, base
        integer :: nt, it, cap

        call sx%build(x, y, z, radius=[rlo, rhi])
        cap = omp_available()
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "  threads   used    count_all_within (s)   speed-up"
        base = 0.0_real64
        nt = 1
        do
            if (nt > cap) exit
            best = huge(1.0_real64)
            do it = 1, rounds
                t0 = wtime()
                call sx%count_all_within(rlo, counts, threads=nt)
                best = min(best, wtime() - t0)
            end do
            if (nt == 1) base = best
            write (output_unit, '(2x,i7,2x,i6,2x,f20.4,2x,f9.2,a)') nt, &
                parquet_debug_spatial_threads_used(), best, base / best, "x"
            nt = nt * 2
        end do
        write (output_unit, '(a,i0)') "  checksum : ", sum(counts)
    end subroutine mode_threads

    !> The team size to ask a bulk call for: `--threads=` when given, otherwise automatic.
    integer function team() result(n)

        n = threads
        if (n < 1) n = omp_available()
    end function team

end program benchmark_spatial
