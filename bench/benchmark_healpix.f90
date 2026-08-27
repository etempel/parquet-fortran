!> The in-repo HEALPix benchmark: conversion throughput, bulk thread scaling and `pf_query_disc`.
!!
!! **This is TA-5**, the in-repository half of `feature_healpix_tier_a.md` section 8.8 and the
!! whole measurement instrument for `feature_healpix_tier_b.md` section 7.8 (gates G2/G3, targets
!! T1/T2). It links **no HEALPix at all**, so it travels with the repository and any machine can
!! reproduce the native side; the GPL-linking comparison against `libhealpix` stays out of tree in
!! `test_run/healpix_bench/` (decision D16).
!!
!! **What each mode answers.**
!!
!! - `--mode=conv` -- per-call cost of every scalar entry point, in both integer kinds, swept over
!!   `nside`. The kind columns are there because the int32 specifics are thin wrappers that widen
!!   to the int64 core, so their cost is the core's plus a conversion, and "plus a conversion" is
!!   the kind of claim that should be measured rather than asserted.
!! - `--mode=bulk` -- section 7.8's table: every one of the 18 bulk specifics, at four array sizes,
!!   at each thread count, against a hand-written `do` loop over the same elemental form. **G2** is
!!   the `threads=1` column against that loop; **T1** is the 8-thread speedup.
!! - `--mode=cross` -- **T2**: the array size at which threading first pays, per form and per
!!   thread count. This is what `hpx_parallel_min_elements` is supposed to encode, and running it
!!   at more than one thread count is the point: the crossover is not one number unless it happens
!!   not to move with the team size.
!! - `--mode=disc` -- `pf_query_disc` in microseconds per disc across `nside` and radius, for the
!!   two call shapes `feature_healpix_tier_a.md` section 1.2 measured `libhealpix` in: the pixel
!!   disc (`nside` 1024, RING, exact) and candidate generation (`nside` 256, NEST, inclusive). The
!!   three output forms -- caller buffer, count-only and self-sizing -- are timed side by side.
!!
!! **The fixtures are deterministic and compiler-independent, by construction rather than by
!! seeding.** Directions come from a Fibonacci spiral (`z` stepped linearly, longitude advanced by
!! the golden-ratio conjugate), which is exactly area-uniform and identical under every compiler --
!! so a gfortran column and an ifx column are the same query on the same points, not two samples
!! from one distribution. `random_seed` would not do that: the sequence a given seed produces is
!! processor-dependent, and the cost of a disc query depends on how many pixels the disc holds.
!!
!! **Two traps this file is written around**, both from CLAUDE.md's benchmarking rules and both of
!! which produce a confident wrong number rather than a failure. Every output array is written once
!! before the timed loop, so no arm absorbs its first-touch page faults on behalf of the others --
!! that alone reversed a comparison elsewhere in this repository. And every timed loop feeds a
!! checksum that is printed, so nothing measured here can be elided as dead.
!!
!! Driven by `bench/benchmark_healpix.sh`, which is where the environment (`--profile release`, and
!! the assertion that it actually produced optimisation) is handled. Maintainer-only.
program benchmark_healpix
    use omp_lib, only: omp_get_wtime, omp_get_max_threads
    use, intrinsic :: iso_fortran_env, only: real64, int32, int64, output_unit, error_unit
    use parquet_healpix
    implicit none

    !> pi.
    real(real64), parameter :: pi = 3.141592653589793238462643_real64
    !> Radians per degree.
    real(real64), parameter :: d2r = pi / 180.0_real64
    !> The golden-ratio conjugate, the Fibonacci spiral's longitude stride in turns.
    real(real64), parameter :: gold = 0.6180339887498948482_real64
    !> How many bulk specifics the form table below enumerates.
    integer, parameter :: nforms = 18

    character(len=:), allocatable :: mode !! which arm to run.
    integer(int64) :: nel !! largest element count for the bulk ladder.
    integer(int64) :: nq !! disc queries per timed row.
    integer :: rounds !! timed repetitions; the fastest is kept.
    integer :: nside_b !! the resolution the bulk and conversion arms run at.
    integer :: tlist(16) !! thread counts to sweep.
    integer :: ntl !! how many of `tlist` are in use.
    integer(int64) :: chk !! integer checksum, printed so no timed loop is dead code.
    real(real64) :: chkr !! real checksum, same purpose.

    ! ---- Fixtures, allocated once at the largest size every arm asks for ----
    real(real64), allocatable :: th(:) !! input colatitudes.
    real(real64), allocatable :: ph(:) !! input longitudes.
    real(real64), allocatable :: vv(:,:) !! input directions, shaped (3, n).
    real(real64), allocatable :: oth(:) !! output colatitudes.
    real(real64), allocatable :: oph(:) !! output longitudes.
    real(real64), allocatable :: ov(:,:) !! output directions, shaped (3, n).
    integer(int32), allocatable :: pin32(:) !! input pixel indices, int32.
    integer(int64), allocatable :: pin64(:) !! input pixel indices, int64.
    integer(int32), allocatable :: o32(:) !! output pixel indices, int32.
    integer(int64), allocatable :: o64(:) !! output pixel indices, int64.

    chk = 0_int64
    chkr = 0.0_real64
    call parse_arguments()

    write(output_unit, '(a)') repeat('=', 96)
    write(output_unit, '(a)') 'benchmark_healpix -- native pf_healpix, no libhealpix linkage'
    write(output_unit, '(a,a)') '  mode          : ', mode
    write(output_unit, '(a,i0)') '  nside         : ', nside_b
    write(output_unit, '(a,i0)') '  rounds        : ', rounds
    write(output_unit, '(a,i0)') '  omp_get_max_threads() : ', omp_get_max_threads()
    write(output_unit, '(a)') repeat('=', 96)
    write(output_unit, '(a)') ''
    flush(output_unit)

    select case (mode)
    case ("conv")
        call mode_conv()
    case ("bulk")
        call mode_bulk()
    case ("cross")
        call mode_cross()
    case ("disc")
        call mode_disc()
    case ("ovh")
        call mode_overhead()
    case ("grid")
        call mode_grid()
    case ("all")
        call mode_conv()
        call mode_bulk()
        call mode_overhead()
        call mode_cross()
        call mode_disc()
        call mode_grid()
    case default
        write(error_unit, '(a)') "benchmark_healpix: unknown --mode '" // mode // "'"
        error stop 1
    end select

    write(output_unit, '(a)') ''
    write(output_unit, '(a,i0,a,es12.5)') 'checksums (ignore the values; they exist so that no ' // &
        'timed loop is dead code): ', chk, '  ', chkr

contains

    ! ============================================================================================
    ! Command line
    ! ============================================================================================

    !> Reads `--key=value` arguments into the host's configuration variables.
    subroutine parse_arguments()
        character(len=256) :: arg
        character(len=:), allocatable :: key, val
        integer :: i, eq, ios

        mode = ""
        nel = 10000000_int64
        nq = 50000_int64
        rounds = 3
        nside_b = 1024
        tlist(1:5) = [1, 2, 4, 8, 16]
        ntl = 5

        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            eq = index(arg, "=")
            if (eq <= 0) then
                if (trim(arg) == "--help" .or. trim(arg) == "-h") then
                    call usage()
                    stop 0
                end if
                cycle
            end if
            key = trim(arg(1:eq - 1))
            val = trim(arg(eq + 1:))
            ios = 0
            select case (key)
            case ("--mode")
                mode = val
            case ("--elements")
                read (val, *, iostat=ios) nel
            case ("--queries")
                read (val, *, iostat=ios) nq
            case ("--rounds")
                read (val, *, iostat=ios) rounds
            case ("--nside")
                read (val, *, iostat=ios) nside_b
            case ("--threads")
                call parse_threads(val)
            case default
                write(error_unit, '(a)') "benchmark_healpix: unknown option '" // key // "'"
                error stop 1
            end select
            if (ios /= 0) then
                write(error_unit, '(a)') "benchmark_healpix: could not read '" // key // "=" // val // "'"
                error stop 1
            end if
        end do

        if (len(mode) == 0) then
            write(error_unit, '(a)') "benchmark_healpix: --mode is required"
            call usage()
            error stop 1
        end if
        if (nel < 1_int64 .or. nq < 1_int64 .or. rounds < 1 .or. nside_b < 1) then
            write(error_unit, '(a)') "benchmark_healpix: every numeric option must be >= 1"
            error stop 1
        end if
        ! The int32 specifics are half the table, so a resolution their pixel indices cannot
        ! address would silently measure nothing rather than fail.
        if (nside_b > 8192) then
            write(error_unit, '(a)') "benchmark_healpix: --nside must be at most 8192; the int32 " // &
                "specifics are half of what is measured here"
            error stop 1
        end if
    end subroutine parse_arguments

    !> Reads a comma-separated thread-count list into `tlist`/`ntl`.
    subroutine parse_threads(val)
        character(len=*), intent(in) :: val !! e.g. `1,2,4,8,16`.
        integer :: i, p, ios

        ntl = 0
        i = 1
        do while (i <= len_trim(val))
            p = index(val(i:), ",")
            if (p == 0) then
                p = len_trim(val) + 1
            else
                p = i + p - 1
            end if
            ntl = ntl + 1
            if (ntl > size(tlist)) then
                write(error_unit, '(a)') "benchmark_healpix: --threads takes at most 16 values"
                error stop 1
            end if
            read (val(i:p - 1), *, iostat=ios) tlist(ntl)
            if (ios /= 0 .or. tlist(ntl) < 1) then
                write(error_unit, '(a)') "benchmark_healpix: --threads must be positive integers"
                error stop 1
            end if
            i = p + 1
        end do
        if (ntl == 0) then
            write(error_unit, '(a)') "benchmark_healpix: --threads must name at least one count"
            error stop 1
        end if
    end subroutine parse_threads

    !> Prints the usage text.
    subroutine usage()
        write(output_unit, '(a)') "Usage: benchmark_healpix --mode=<mode> [options]"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --mode=grid    pf_healpix_grid bindings against the free procedures"
        write(output_unit, '(a)') "  --mode=conv    scalar entry-point cost, both kinds, swept over nside"
        write(output_unit, '(a)') "  --mode=bulk    the 18 bulk specifics vs a scalar loop (G2, T1)"
        write(output_unit, '(a)') "  --mode=cross   where threading first pays, per form and team size (T2, G3)"
        write(output_unit, '(a)') "  --mode=disc    pf_query_disc, microseconds per disc (section 8.8)"
        write(output_unit, '(a)') "  --mode=ovh     what opening a team of each size costs, per call"
        write(output_unit, '(a)') "  --mode=all     every mode above, in that order"
        write(output_unit, '(a)') ""
        write(output_unit, '(a)') "  --elements=N   largest bulk array size          (default 10000000)"
        write(output_unit, '(a)') "  --queries=N    disc queries per timed row       (default 50000)"
        write(output_unit, '(a)') "  --rounds=N     timed repetitions, fastest kept  (default 3)"
        write(output_unit, '(a)') "  --nside=N      resolution for conv/bulk/cross   (default 1024, max 8192)"
        write(output_unit, '(a)') "  --threads=L    comma-separated team sizes       (default 1,2,4,8,16)"
    end subroutine usage

    ! ============================================================================================
    ! Fixtures
    ! ============================================================================================

    !> Fills `n` directions from a Fibonacci spiral over the whole sphere.
    !>
    !> Area-uniform and identical on every compiler, which is what makes a gfortran column and an
    !> ifx column comparable row by row rather than only in distribution.
    subroutine spiral(n, t, p)
        integer(int64), intent(in) :: n !! how many directions to produce.
        real(real64), intent(out) :: t(:) !! colatitudes, radians.
        real(real64), intent(out) :: p(:) !! longitudes, radians, in [0, 2*pi).
        integer(int64) :: k
        real(real64) :: z, u

        do k = 1_int64, n
            z = 1.0_real64 - 2.0_real64 * (real(k, real64) - 0.5_real64) / real(n, real64)
            t(k) = acos(max(-1.0_real64, min(1.0_real64, z)))
            u = modulo(real(k, real64) * gold, 1.0_real64)
            p(k) = 2.0_real64 * pi * u
        end do
    end subroutine spiral

    !> Fills `n` directions from the same spiral, restricted to a declination band.
    !>
    !> The band is `test_run/healpix_bench`'s footprint -- dec in [-70, +20] degrees -- so that a
    !> disc row here is the same query shape as the out-of-tree comparison against `libhealpix`.
    subroutine spiral_band(n, t, p)
        integer(int64), intent(in) :: n !! how many directions to produce.
        real(real64), intent(out) :: t(:) !! colatitudes, radians.
        real(real64), intent(out) :: p(:) !! longitudes, radians, in [0, 2*pi).
        integer(int64) :: k
        real(real64) :: z, u, s1, s2

        s1 = sin(-70.0_real64 * d2r)
        s2 = sin(20.0_real64 * d2r)
        do k = 1_int64, n
            u = (real(k, real64) - 0.5_real64) / real(n, real64)
            z = s1 + u * (s2 - s1)
            t(k) = acos(max(-1.0_real64, min(1.0_real64, z)))
            p(k) = 2.0_real64 * pi * modulo(real(k, real64) * gold, 1.0_real64)
        end do
    end subroutine spiral_band

    !> Allocates every fixture array at `n` elements and fills the inputs.
    !>
    !> **Both the inputs and the outputs are written before any timing happens.** A freshly
    !> allocated array pays first-touch page faults on its first pass and never again, so whichever
    !> arm ran first would otherwise absorb them for all the others.
    subroutine make_fixtures(n)
        integer(int64), intent(in) :: n !! elements to allocate.
        integer(int64) :: k, npix

        allocate (th(n), ph(n), oth(n), oph(n), vv(3, n), ov(3, n))
        allocate (pin32(n), pin64(n), o32(n), o64(n))
        call spiral(n, th, ph)
        call pf_ang2vec_bulk(th, ph, vv)
        npix = pf_nside2npix(int(nside_b, int64))
        do k = 1_int64, n
            ! A golden-ratio stride over the pixel index range: deterministic, uniform, and not
            ! sequential, so a pixel-to-position arm is not measured on a perfectly linear walk.
            pin64(k) = int(modulo(real(k, real64) * gold, 1.0_real64) * real(npix, real64), int64)
            if (pin64(k) >= npix) pin64(k) = npix - 1_int64
            pin32(k) = int(pin64(k), int32)
        end do
        oth = 0.0_real64
        oph = 0.0_real64
        ov = 0.0_real64
        o32 = 0_int32
        o64 = 0_int64
    end subroutine make_fixtures

    ! ============================================================================================
    ! Mode: conv -- the scalar entry points
    ! ============================================================================================

    !> Times every scalar entry point, in both kinds, over a sweep of `nside`.
    subroutine mode_conv()
        integer, parameter :: nsw = 4
        integer :: sweep(nsw)
        integer(int64) :: n

        sweep = [64, 256, 1024, 8192]
        n = min(nel, 2000000_int64)
        call make_fixtures(n)

        write(output_unit, '(a)') '---- mode=conv: scalar entry points, nanoseconds per call ----'
        write(output_unit, '(a)') ''
        write(output_unit, '(a,i0,a)') 'Each figure is the fastest of ', rounds, &
            ' passes over the whole array; the loop is written out by hand, so this is the'
        write(output_unit, '(a)') 'cost of one call including its argument passing, not a vectorised inner loop.'
        write(output_unit, '(a)') ''
        write(output_unit, '(a12,4(a12))') 'entry point', 'ns @ 64', 'ns @ 256', 'ns @ 1024', 'ns @ 8192'
        write(output_unit, '(a)') repeat('-', 62)
        flush(output_unit)

        call conv_row(n, sweep, 'ang2pix_r32')
        call conv_row(n, sweep, 'ang2pix_r64')
        call conv_row(n, sweep, 'ang2pix_n32')
        call conv_row(n, sweep, 'ang2pix_n64')
        call conv_row(n, sweep, 'pix2ang_r32')
        call conv_row(n, sweep, 'pix2ang_r64')
        call conv_row(n, sweep, 'pix2ang_n64')
        call conv_row(n, sweep, 'vec2pix_r64')
        call conv_row(n, sweep, 'vec2pix_n64')
        call conv_row(n, sweep, 'pix2vec_r64')
        call conv_row(n, sweep, 'pix2vec_n64')
        call conv_row(n, sweep, 'ring2nest64')
        call conv_row(n, sweep, 'nest2ring64')
        call conv_row(n, sweep, 'ang2vec    ')
        call conv_row(n, sweep, 'vec2ang    ')
        call conv_row(n, sweep, 'angdist    ')
        write(output_unit, '(a)') ''
        call release_fixtures()
    end subroutine mode_conv

    !> **The delegation bar**: every `pf_healpix_grid` binding against the free procedure it calls.
    !>
    !> The whole design rests on the object costing nothing measurable, so this is the measurement
    !> that keeps it honest. The passed-object dummy of a type-bound procedure must be `class(...)`
    !> (F2018 C760), so the question is whether the compiler devirtualises a binding whose body is
    !> one call -- and it is a question about the compiler, not about the source, which is why it is
    !> measured rather than asserted.
    !>
    !> **Bar: within 5 percent on every row.** A larger figure is a finding about that compiler, to
    !> be written into the guide page, not a reason to widen the bar.
    !>
    !> **What it found, 2026-08-27, machine B, two million elements at nside 1024, best of five.**
    !> gfortran 15.2.1 clears the bar everywhere, worst row 1.048. ifx 2026.1 does not: `ang2pix`
    !> is 1.11-1.12 and `pix2ang` is 0.665-0.666 -- the bound form a third FASTER than the free one
    !> -- both reproducing to three digits over four runs. The sub-unity figure is the useful one:
    !> a delegation that had not been inlined could not beat what it delegates to, so this is the
    !> optimiser choosing differently at two call sites, not dispatch. Recorded in the guide page
    !> and in the type's doc-comment rather than smoothed away.
    subroutine mode_grid()
        integer(int64) :: nsg, nq, k, nlist, sink
        integer :: rep, r
        real(real64) :: t0, tf, tb, best_f, best_b, rad
        integer(int64), allocatable :: disc(:)
        type(pf_healpix_grid) :: gring, gnest

        nsg = 1024_int64
        nq = min(nel, 2000000_int64)
        call make_fixtures(nq)
        call gring%init(nsg, PF_HP_RING)
        call gnest%init(nsg, PF_HP_NEST)
        do k = 1_int64, nq
            pin64(k) = int(modulo(real(k, real64) * gold, 1.0_real64) &
                           * real(pf_nside2npix(nsg), real64), int64)
        end do

        write(output_unit, '(a)') '---- mode=grid: pf_healpix_grid against the free procedures ----'
        write(output_unit, '(a)') ''
        write(output_unit, '(a,i0,a,i0)') 'nside ', nsg, ', elements per pass ', nq
        write(output_unit, '(a,i0,a)') 'Each figure is the fastest of ', rounds, ' passes, in nanoseconds per call.'
        write(output_unit, '(a)') 'Bar: the bound column within 5 percent of the free column on every row.'
        write(output_unit, '(a)') ''
        write(output_unit, '(a16,3(a12))') 'operation', 'free ns', 'bound ns', 'ratio'
        write(output_unit, '(a)') repeat('-', 52)
        flush(output_unit)

        sink = 0_int64
        best_f = huge(0.0_real64); best_b = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_ang2pix_ring(nsg, th(k), ph(k), o64(k))
            end do
            tf = omp_get_wtime() - t0
            best_f = min(best_f, tf)
            sink = sink + o64(1)
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call gring%ang2pix(th(k), ph(k), o64(k))
            end do
            tb = omp_get_wtime() - t0
            best_b = min(best_b, tb)
            sink = sink + o64(1)
        end do
        call grid_row('ang2pix RING', best_f, best_b, nq)

        best_f = huge(0.0_real64); best_b = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_vec2pix_nest(nsg, vv(:, k), o64(k))
            end do
            best_f = min(best_f, omp_get_wtime() - t0)
            sink = sink + o64(1)
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call gnest%vec2pix(vv(:, k), o64(k))
            end do
            best_b = min(best_b, omp_get_wtime() - t0)
            sink = sink + o64(1)
        end do
        call grid_row('vec2pix NEST', best_f, best_b, nq)

        best_f = huge(0.0_real64); best_b = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_pix2ang_ring(nsg, pin64(k), oth(k), oph(k))
            end do
            best_f = min(best_f, omp_get_wtime() - t0)
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call gring%pix2ang(pin64(k), oth(k), oph(k))
            end do
            best_b = min(best_b, omp_get_wtime() - t0)
        end do
        call grid_row('pix2ang RING', best_f, best_b, nq)

        ! The RA/Dec layer has no free counterpart -- the free column is the theta/phi call it
        ! reduces to, so the ratio prices the degree scaling and the reflection, and nothing else.
        best_f = huge(0.0_real64); best_b = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call gring%ang2pix(th(k), ph(k), o64(k))
            end do
            best_f = min(best_f, omp_get_wtime() - t0)
            sink = sink + o64(1)
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call gring%radec2pix(oph(k), oth(k), o64(k))
            end do
            best_b = min(best_b, omp_get_wtime() - t0)
            sink = sink + o64(1)
        end do
        call grid_row('radec2pix/ang', best_f, best_b, nq)

        ! One disc query is thousands of pixels, so this row is per DISC rather than per call.
        allocate (disc(4000000))
        rad = 1.0_real64 * 3.141592653589793_real64 / 180.0_real64
        best_f = huge(0.0_real64); best_b = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            do r = 1, 2000
                call pf_query_disc(nsg, vv(:, r), rad, disc, nlist, scheme=PF_HP_RING)
                sink = sink + nlist
            end do
            best_f = min(best_f, omp_get_wtime() - t0)
            t0 = omp_get_wtime()
            do r = 1, 2000
                call gring%query_disc(vv(:, r), rad, disc, nlist)
                sink = sink + nlist
            end do
            best_b = min(best_b, omp_get_wtime() - t0)
        end do
        write(output_unit, '(a16,3(f12.3))') 'query_disc (us)', &
            best_f * 1.0e6_real64 / 2000.0_real64, best_b * 1.0e6_real64 / 2000.0_real64, &
            best_b / best_f
        deallocate (disc)

        if (sink == -1_int64) write(output_unit, '(i0)') sink
        write(output_unit, '(a)') ''
        call release_fixtures()
    end subroutine mode_grid

    !> Prints one `mode=grid` row as nanoseconds per call plus the bound/free ratio.
    subroutine grid_row(what, best_f, best_b, nq)
        character(len=*), intent(in) :: what !! the operation.
        real(real64), intent(in) :: best_f !! fastest free pass, seconds.
        real(real64), intent(in) :: best_b !! fastest bound pass, seconds.
        integer(int64), intent(in) :: nq !! elements per pass.

        write(output_unit, '(a16,3(f12.3))') what, best_f * 1.0e9_real64 / real(nq, real64), &
            best_b * 1.0e9_real64 / real(nq, real64), best_b / best_f
        flush(output_unit)
    end subroutine grid_row

    !> Times one entry point across the `nside` sweep and prints its row.
    subroutine conv_row(n, sweep, what)
        integer(int64), intent(in) :: n !! elements per pass.
        integer, intent(in) :: sweep(:) !! the `nside` values to sweep.
        character(len=*), intent(in) :: what !! which entry point, as named in `conv_one`.
        real(real64) :: ns(size(sweep))
        integer :: s

        do s = 1, size(sweep)
            call conv_one(what, sweep(s), n, ns(s))
        end do
        write(output_unit, '(a12,4(f12.2))') what, ns
        flush(output_unit)
    end subroutine conv_row

    !> Times one entry point at one `nside`, returning nanoseconds per call.
    subroutine conv_one(what, ns_in, n, out_ns)
        character(len=*), intent(in) :: what !! which entry point.
        integer, intent(in) :: ns_in !! the resolution.
        integer(int64), intent(in) :: n !! elements per pass.
        real(real64), intent(out) :: out_ns !! nanoseconds per call.
        integer(int64) :: k, ns64, npix
        integer(int32) :: ns32
        real(real64) :: t0, t, best, v(3), d
        integer :: rep

        ns64 = int(ns_in, int64)
        ns32 = int(ns_in, int32)
        npix = pf_nside2npix(ns64)
        ! The pixel-index inputs are rebuilt for this nside: an index drawn for another resolution
        ! would be out of range, and these entry points are total, so nothing would report it.
        do k = 1_int64, n
            pin64(k) = int(modulo(real(k, real64) * gold, 1.0_real64) * real(npix, real64), int64)
            if (pin64(k) >= npix) pin64(k) = npix - 1_int64
            pin32(k) = int(pin64(k), int32)
        end do

        best = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            select case (what)
            case ('ang2pix_r32')
                do k = 1_int64, n
                    call pf_ang2pix_ring(ns32, th(k), ph(k), o32(k))
                end do
            case ('ang2pix_r64')
                do k = 1_int64, n
                    call pf_ang2pix_ring(ns64, th(k), ph(k), o64(k))
                end do
            case ('ang2pix_n32')
                do k = 1_int64, n
                    call pf_ang2pix_nest(ns32, th(k), ph(k), o32(k))
                end do
            case ('ang2pix_n64')
                do k = 1_int64, n
                    call pf_ang2pix_nest(ns64, th(k), ph(k), o64(k))
                end do
            case ('pix2ang_r32')
                do k = 1_int64, n
                    call pf_pix2ang_ring(ns32, pin32(k), oth(k), oph(k))
                end do
            case ('pix2ang_r64')
                do k = 1_int64, n
                    call pf_pix2ang_ring(ns64, pin64(k), oth(k), oph(k))
                end do
            case ('pix2ang_n64')
                do k = 1_int64, n
                    call pf_pix2ang_nest(ns64, pin64(k), oth(k), oph(k))
                end do
            case ('vec2pix_r64')
                do k = 1_int64, n
                    call pf_vec2pix_ring(ns64, vv(:, k), o64(k))
                end do
            case ('vec2pix_n64')
                do k = 1_int64, n
                    call pf_vec2pix_nest(ns64, vv(:, k), o64(k))
                end do
            case ('pix2vec_r64')
                do k = 1_int64, n
                    call pf_pix2vec_ring(ns64, pin64(k), ov(:, k))
                end do
            case ('pix2vec_n64')
                do k = 1_int64, n
                    call pf_pix2vec_nest(ns64, pin64(k), ov(:, k))
                end do
            case ('ring2nest64')
                do k = 1_int64, n
                    call pf_ring2nest(ns64, pin64(k), o64(k))
                end do
            case ('nest2ring64')
                do k = 1_int64, n
                    call pf_nest2ring(ns64, pin64(k), o64(k))
                end do
            case ('ang2vec    ')
                do k = 1_int64, n
                    call pf_ang2vec(th(k), ph(k), ov(:, k))
                end do
            case ('vec2ang    ')
                do k = 1_int64, n
                    call pf_vec2ang(vv(:, k), oth(k), oph(k))
                end do
            case ('angdist    ')
                d = 0.0_real64
                v = vv(:, 1)
                do k = 1_int64, n
                    call pf_angdist(v, vv(:, k), oth(k))
                end do
            case default
                write(error_unit, '(a)') "benchmark_healpix: conv_one has no case '" // what // "'"
                error stop 1
            end select
            t = omp_get_wtime() - t0
            best = min(best, t)
        end do

        chk = ieor(ishftc(chk, 1), ieor(o64(n / 2_int64), int(o32(n / 2_int64), int64)))
        chkr = chkr + oth(n / 2_int64) + ov(1, n / 2_int64)
        out_ns = 1.0e9_real64 * best / real(n, real64)
    end subroutine conv_one

    ! ============================================================================================
    ! Mode: bulk -- G2 and T1
    ! ============================================================================================

    !> Runs the bulk table: every specific, four sizes, every thread count, against a scalar loop.
    subroutine mode_bulk()
        integer(int64) :: sizes(4), n
        integer :: id, s, j, nsz
        real(real64) :: tscalar, tb(size(tlist)), tauto

        ! The ladder is the fixed rungs that fit inside --elements, plus --elements itself when it
        ! is not already one of them. Built rather than written out so that a small --elements (a
        ! smoke run) does not produce a duplicated row and a skipped one.
        nsz = 0
        do s = 1, 3
            n = 10000_int64 * 10_int64 ** (s - 1)
            if (n < nel) then
                nsz = nsz + 1
                sizes(nsz) = n
            end if
        end do
        nsz = nsz + 1
        sizes(nsz) = nel
        call make_fixtures(nel)

        write(output_unit, '(a)') '---- mode=bulk: the 18 bulk specifics against a hand-written scalar loop ----'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'scalar = a `do` loop calling the elemental/scalar form, written out here.'
        write(output_unit, '(a)') 'G2 = bulk at threads=1 within 5% of scalar.  T1 = >= 4x at 8 threads on 1e6.'
        write(output_unit, '(a)') 'Figures are nanoseconds per element; "auto" omits threads= entirely.'
        write(output_unit, '(a)') ''
        flush(output_unit)

        do id = 1, nforms
            write(output_unit, '(a)') ''
            write(output_unit, '(a,a)') 'form: ', form_name(id)
            write(output_unit, '(a10,a11,a11,20a11)', advance='no') 'elements', 'scalar', 'auto'
            do j = 1, ntl
                write(output_unit, '(a8,i3)', advance='no') 'thr=', tlist(j)
            end do
            write(output_unit, '(a)') ''
            write(output_unit, '(a)') repeat('-', 32 + 11 * ntl)
            do s = 1, nsz
                n = sizes(s)
                call time_scalar(id, n, tscalar)
                call time_bulk(id, n, tauto)
                do j = 1, ntl
                    call time_bulk(id, n, tb(j), threads=tlist(j))
                end do
                write(output_unit, '(i10,f11.3,f11.3)', advance='no') n, &
                    1.0e9_real64 * tscalar / real(n, real64), 1.0e9_real64 * tauto / real(n, real64)
                do j = 1, ntl
                    write(output_unit, '(f11.3)', advance='no') 1.0e9_real64 * tb(j) / real(n, real64)
                end do
                write(output_unit, '(a)') ''
                flush(output_unit)
            end do
        end do
        write(output_unit, '(a)') ''
        call release_fixtures()
    end subroutine mode_bulk

    ! ============================================================================================
    ! Mode: cross -- T2 and G3
    ! ============================================================================================

    !> Finds, per form and per thread count, the smallest array size at which threading pays.
    !>
    !> **The reason this is a sweep rather than a single number** is that the crossover is a
    !> property of the pair (per-element cost, team size), and `hpx_parallel_min_elements` is one
    !> constant. If the crossover moves with the team size, a single constant cannot hold G3 on
    !> every machine, and this table is the evidence for whatever replaces it.
    subroutine mode_cross()
        integer, parameter :: nlad = 13
        integer(int64) :: ladder(nlad), n
        integer :: id, s, j, first(size(tlist))
        real(real64) :: t1, tn

        ladder = [100_int64, 200_int64, 500_int64, 1000_int64, 2000_int64, 5000_int64, &
                  10000_int64, 20000_int64, 50000_int64, 100000_int64, 200000_int64, &
                  500000_int64, 1000000_int64]
        call make_fixtures(max(1000000_int64, ladder(nlad)))

        write(output_unit, '(a)') '---- mode=cross: smallest array size at which threading beats serial (T2) ----'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'Per form and per team size: the first ladder rung where threads=N is strictly'
        write(output_unit, '(a)') 'faster than threads=1. A dash means threading never won on this ladder.'
        write(output_unit, '(a)') ''
        write(output_unit, '(a20)', advance='no') 'form'
        do j = 1, ntl
            write(output_unit, '(a8,i3)', advance='no') 'thr=', tlist(j)
        end do
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') repeat('-', 20 + 11 * ntl)
        flush(output_unit)

        do id = 1, nforms
            first = 0
            do s = 1, nlad
                n = ladder(s)
                call time_bulk(id, n, t1, threads=1)
                do j = 1, ntl
                    if (first(j) /= 0) cycle
                    if (tlist(j) == 1) then
                        first(j) = -1
                        cycle
                    end if
                    call time_bulk(id, n, tn, threads=tlist(j))
                    if (tn < t1) first(j) = s
                end do
            end do
            write(output_unit, '(a20)', advance='no') form_name(id)
            do j = 1, ntl
                if (first(j) > 0) then
                    write(output_unit, '(i11)', advance='no') ladder(first(j))
                else if (first(j) == -1) then
                    write(output_unit, '(a11)', advance='no') 'n/a'
                else
                    write(output_unit, '(a11)', advance='no') '-'
                end if
            end do
            write(output_unit, '(a)') ''
            flush(output_unit)
        end do
        write(output_unit, '(a)') ''
        call release_fixtures()
    end subroutine mode_cross

    ! ============================================================================================
    ! The bulk form table
    ! ============================================================================================

    !> The name of bulk specific `id`, for the tables above.
    function form_name(id) result(nm)
        integer, intent(in) :: id !! 1 .. `nforms`.
        character(len=20) :: nm !! a fixed-width label.

        select case (id)
        case (1); nm = 'ang2pix_ring i32'
        case (2); nm = 'ang2pix_ring i64'
        case (3); nm = 'ang2pix_nest i32'
        case (4); nm = 'ang2pix_nest i64'
        case (5); nm = 'pix2ang_ring i32'
        case (6); nm = 'pix2ang_ring i64'
        case (7); nm = 'pix2ang_nest i32'
        case (8); nm = 'pix2ang_nest i64'
        case (9); nm = 'vec2pix_ring i32'
        case (10); nm = 'vec2pix_ring i64'
        case (11); nm = 'vec2pix_nest i32'
        case (12); nm = 'vec2pix_nest i64'
        case (13); nm = 'pix2vec_ring i32'
        case (14); nm = 'pix2vec_ring i64'
        case (15); nm = 'pix2vec_nest i32'
        case (16); nm = 'pix2vec_nest i64'
        case (17); nm = 'ang2vec'
        case (18); nm = 'vec2ang'
        case default; nm = '??'
        end select
    end function form_name

    !> Times bulk specific `id` over `n` elements, keeping the fastest of `rounds` passes.
    !>
    !> `threads` is passed straight through: an absent optional actual stays absent at the library
    !> call, which is what makes the "auto" column the real automatic path rather than an
    !> approximation of it.
    subroutine time_bulk(id, n, t, threads)
        integer, intent(in) :: id !! 1 .. `nforms`.
        integer(int64), intent(in) :: n !! elements.
        real(real64), intent(out) :: t !! seconds for the fastest pass.
        integer, intent(in), optional :: threads !! team size; absent means the automatic path.
        integer(int64) :: ns64
        integer(int32) :: ns32
        real(real64) :: t0, el
        integer :: rep

        ns64 = int(nside_b, int64)
        ns32 = int(nside_b, int32)
        t = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            select case (id)
            case (1); call pf_ang2pix_ring_bulk(ns32, th(1:n), ph(1:n), o32(1:n), threads=threads)
            case (2); call pf_ang2pix_ring_bulk(ns64, th(1:n), ph(1:n), o64(1:n), threads=threads)
            case (3); call pf_ang2pix_nest_bulk(ns32, th(1:n), ph(1:n), o32(1:n), threads=threads)
            case (4); call pf_ang2pix_nest_bulk(ns64, th(1:n), ph(1:n), o64(1:n), threads=threads)
            case (5); call pf_pix2ang_ring_bulk(ns32, pin32(1:n), oth(1:n), oph(1:n), threads=threads)
            case (6); call pf_pix2ang_ring_bulk(ns64, pin64(1:n), oth(1:n), oph(1:n), threads=threads)
            case (7); call pf_pix2ang_nest_bulk(ns32, pin32(1:n), oth(1:n), oph(1:n), threads=threads)
            case (8); call pf_pix2ang_nest_bulk(ns64, pin64(1:n), oth(1:n), oph(1:n), threads=threads)
            case (9); call pf_vec2pix_ring_bulk(ns32, vv(:, 1:n), o32(1:n), threads=threads)
            case (10); call pf_vec2pix_ring_bulk(ns64, vv(:, 1:n), o64(1:n), threads=threads)
            case (11); call pf_vec2pix_nest_bulk(ns32, vv(:, 1:n), o32(1:n), threads=threads)
            case (12); call pf_vec2pix_nest_bulk(ns64, vv(:, 1:n), o64(1:n), threads=threads)
            case (13); call pf_pix2vec_ring_bulk(ns32, pin32(1:n), ov(:, 1:n), threads=threads)
            case (14); call pf_pix2vec_ring_bulk(ns64, pin64(1:n), ov(:, 1:n), threads=threads)
            case (15); call pf_pix2vec_nest_bulk(ns32, pin32(1:n), ov(:, 1:n), threads=threads)
            case (16); call pf_pix2vec_nest_bulk(ns64, pin64(1:n), ov(:, 1:n), threads=threads)
            case (17); call pf_ang2vec_bulk(th(1:n), ph(1:n), ov(:, 1:n), threads=threads)
            case (18); call pf_vec2ang_bulk(vv(:, 1:n), oth(1:n), oph(1:n), threads=threads)
            end select
            el = omp_get_wtime() - t0
            t = min(t, el)
        end do
        call absorb(n)
    end subroutine time_bulk

    !> Times the hand-written scalar loop that bulk specific `id` is supposed to match (G2).
    subroutine time_scalar(id, n, t)
        integer, intent(in) :: id !! 1 .. `nforms`.
        integer(int64) :: k
        integer(int64), intent(in) :: n !! elements.
        real(real64), intent(out) :: t !! seconds for the fastest pass.
        integer(int64) :: ns64
        integer(int32) :: ns32
        real(real64) :: t0, el
        integer :: rep

        ns64 = int(nside_b, int64)
        ns32 = int(nside_b, int32)
        t = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            select case (id)
            case (1); do k = 1_int64, n; call pf_ang2pix_ring(ns32, th(k), ph(k), o32(k)); end do
            case (2); do k = 1_int64, n; call pf_ang2pix_ring(ns64, th(k), ph(k), o64(k)); end do
            case (3); do k = 1_int64, n; call pf_ang2pix_nest(ns32, th(k), ph(k), o32(k)); end do
            case (4); do k = 1_int64, n; call pf_ang2pix_nest(ns64, th(k), ph(k), o64(k)); end do
            case (5); do k = 1_int64, n; call pf_pix2ang_ring(ns32, pin32(k), oth(k), oph(k)); end do
            case (6); do k = 1_int64, n; call pf_pix2ang_ring(ns64, pin64(k), oth(k), oph(k)); end do
            case (7); do k = 1_int64, n; call pf_pix2ang_nest(ns32, pin32(k), oth(k), oph(k)); end do
            case (8); do k = 1_int64, n; call pf_pix2ang_nest(ns64, pin64(k), oth(k), oph(k)); end do
            case (9); do k = 1_int64, n; call pf_vec2pix_ring(ns32, vv(:, k), o32(k)); end do
            case (10); do k = 1_int64, n; call pf_vec2pix_ring(ns64, vv(:, k), o64(k)); end do
            case (11); do k = 1_int64, n; call pf_vec2pix_nest(ns32, vv(:, k), o32(k)); end do
            case (12); do k = 1_int64, n; call pf_vec2pix_nest(ns64, vv(:, k), o64(k)); end do
            case (13); do k = 1_int64, n; call pf_pix2vec_ring(ns32, pin32(k), ov(:, k)); end do
            case (14); do k = 1_int64, n; call pf_pix2vec_ring(ns64, pin64(k), ov(:, k)); end do
            case (15); do k = 1_int64, n; call pf_pix2vec_nest(ns32, pin32(k), ov(:, k)); end do
            case (16); do k = 1_int64, n; call pf_pix2vec_nest(ns64, pin64(k), ov(:, k)); end do
            case (17); do k = 1_int64, n; call pf_ang2vec(th(k), ph(k), ov(:, k)); end do
            case (18); do k = 1_int64, n; call pf_vec2ang(vv(:, k), oth(k), oph(k)); end do
            end select
            el = omp_get_wtime() - t0
            t = min(t, el)
        end do
        call absorb(n)
    end subroutine time_scalar

    !> Folds one element of each output array into the checksums.
    subroutine absorb(n)
        integer(int64), intent(in) :: n !! the size the arm just ran at.
        integer(int64) :: m

        m = max(1_int64, n / 2_int64)
        chk = ieor(ishftc(chk, 1), ieor(o64(m), int(o32(m), int64)))
        chkr = chkr + oth(m) + oph(m) + ov(1, m)
    end subroutine absorb

    !> Frees the fixtures, so that two modes in one run do not hold two sets at once.
    subroutine release_fixtures()
        deallocate (th, ph, oth, oph, vv, ov, pin32, pin64, o32, o64)
    end subroutine release_fixtures

    ! ============================================================================================
    ! Mode: ovh -- what a parallel region costs before any work is done
    ! ============================================================================================

    !> Times a bulk call on a tiny array at each team size, which is very nearly pure overhead.
    !>
    !> **This is the number `hpx_parallel_min_elements` is really a proxy for.** The crossover
    !> element count is not a property of the library: it is this fixed cost divided by the
    !> per-element saving, so measuring it directly says how the threshold has to be shaped rather
    !> than only where one particular machine's crossover happens to land.
    !>
    !> `--elements` is ignored here; the array is deliberately small enough that the work inside
    !> the region is negligible against the region itself.
    subroutine mode_overhead()
        integer(int64), parameter :: tiny = 64_int64
        integer, parameter :: reps = 200
        integer :: j, r
        real(real64) :: t0, el, best, tfirst, tthird, ttenth, tsum, each(reps)

        call make_fixtures(max(tiny, 1000_int64))

        write(output_unit, '(a)') '---- mode=ovh: the cost of opening a team, before any work ----'
        write(output_unit, '(a)') ''
        write(output_unit, '(a,i0,a)') 'pf_ang2pix_ring_bulk over ', tiny, ' elements, so the region holds ' // &
            'almost no work: what is timed is very nearly the region itself.'
        write(output_unit, '(a,i0,a)') 'The same call is then made ', reps, ' times in a row and the ' // &
            'individual timings reported,'
        write(output_unit, '(a)') 'because a large team does NOT reach its steady-state cost on the first ' // &
            'call or the third.'
        write(output_unit, '(a)') 'A caller that makes one bulk call pays the "1st" column; one that ' // &
            'loops pays "best".'
        write(output_unit, '(a)') ''
        write(output_unit, '(a10,5a14)') 'threads', 'us 1st', 'us 3rd', 'us 10th', 'us mean', 'us best'
        write(output_unit, '(a)') repeat('-', 80)

        do j = 1, ntl
            best = huge(0.0_real64)
            tsum = 0.0_real64
            do r = 1, reps
                t0 = omp_get_wtime()
                call pf_ang2pix_ring_bulk(int(nside_b, int64), th(1:tiny), ph(1:tiny), &
                                          o64(1:tiny), threads=tlist(j))
                el = omp_get_wtime() - t0
                each(r) = el
                best = min(best, el)
                tsum = tsum + el
            end do
            call absorb(tiny)
            tfirst = each(1)
            tthird = each(min(3, reps))
            ttenth = each(min(10, reps))
            write(output_unit, '(i10,5f14.4)') tlist(j), 1.0e6_real64 * tfirst, &
                1.0e6_real64 * tthird, 1.0e6_real64 * ttenth, &
                1.0e6_real64 * tsum / real(reps, real64), 1.0e6_real64 * best
            flush(output_unit)
        end do
        write(output_unit, '(a)') ''
        call release_fixtures()
    end subroutine mode_overhead

    ! ============================================================================================
    ! Mode: disc -- pf_query_disc
    ! ============================================================================================

    !> Times `pf_query_disc` in the two call shapes the out-of-tree `libhealpix` comparison uses.
    subroutine mode_disc()
        real(real64) :: rad_pix(3), rad_tar(3)
        integer :: i

        rad_pix = [0.5_real64, 1.0_real64, 2.0_real64]
        rad_tar = [0.08_real64, 0.80_real64, 1.30_real64]

        write(output_unit, '(a)') '---- mode=disc: pf_query_disc, microseconds per disc ----'
        write(output_unit, '(a)') ''
        write(output_unit, '(a,i0,a)') 'Query centres: ', nq, ' points of the Fibonacci spiral inside the ' // &
            'dec [-70, +20] band,'
        write(output_unit, '(a)') 'which is the footprint test_run/healpix_bench uses, so these rows are ' // &
            'directly'
        write(output_unit, '(a)') 'comparable with the libhealpix columns recorded in feature_healpix_tier_a.md.'
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'PIXEL DISC -- nside 1024, RING, inclusive=.false. (the query_disc(inclusive=0) shape)'
        call disc_header()
        do i = 1, 3
            call disc_row(1024_int64, rad_pix(i), PF_HP_RING, .false.)
        end do
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'CANDIDATE GENERATION -- nside 256, NEST, inclusive=.true. (the A4 shape)'
        call disc_header()
        do i = 1, 3
            call disc_row(256_int64, rad_tar(i), PF_HP_NEST, .true.)
        end do
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'SCHEME COST -- the same geometry in both schemes, so the difference between the'
        write(output_unit, '(a)') 'two rows is what a NEST result costs over a RING one: the walk is identical and'
        write(output_unit, '(a)') 'only the emission differs, by one Morton encode per pixel. Subtract the "count"'
        write(output_unit, '(a)') 'column from "buffer" in each row to get the emission alone.'
        call disc_header()
        call disc_row(1024_int64, 1.0_real64, PF_HP_RING, .false.)
        call disc_row(1024_int64, 1.0_real64, PF_HP_NEST, .false.)
        write(output_unit, '(a)') ''
        write(output_unit, '(a)') 'The three columns are the three output forms of one walk: a caller-owned'
        write(output_unit, '(a)') 'buffer, the count-only mode, and the self-sizing form (which walks twice and'
        write(output_unit, '(a)') 'allocates once). "pix" is the mean number of pixels a disc returned.'
        write(output_unit, '(a)') ''
    end subroutine mode_disc

    !> Prints the column header for the disc table.
    subroutine disc_header()
        write(output_unit, '(a10,a10,4a12)') 'radius', 'pix/disc', 'us buffer', 'us count', 'us alloc', 'us i32'
        write(output_unit, '(a)') repeat('-', 68)
        flush(output_unit)
    end subroutine disc_header

    !> Times one (nside, radius, scheme, inclusive) row in all four forms.
    subroutine disc_row(ns, rad_deg, scheme, inclusive)
        integer(int64), intent(in) :: ns !! resolution.
        real(real64), intent(in) :: rad_deg !! disc radius, degrees.
        integer, intent(in) :: scheme !! `PF_HP_RING` or `PF_HP_NEST`.
        logical, intent(in) :: inclusive !! the overlap superset, or the exact walk.
        real(real64), allocatable :: qt(:), qp(:)
        integer(int64), allocatable :: buf(:), alloced(:)
        integer(int32), allocatable :: buf32(:)
        integer(int64) :: k, nl, tot, cap
        integer(int32) :: nl32
        real(real64) :: v(3), r, t0, tbuf, tcnt, tall, t32, el
        integer :: rep

        allocate (qt(nq), qp(nq))
        call spiral_band(nq, qt, qp)
        r = rad_deg * d2r
        cap = disc_capacity(ns, rad_deg)
        allocate (buf(cap), buf32(cap))
        buf = 0_int64
        buf32 = 0_int32

        ! One untimed pass: it warms the buffer's pages and produces the pixel count reported
        ! below, so no timed arm pays for either.
        tot = 0_int64
        do k = 1_int64, nq
            call pf_ang2vec(qt(k), qp(k), v)
            call pf_query_disc(ns, v, r, buf, nl, scheme=scheme, inclusive=inclusive)
            tot = tot + nl
        end do

        tbuf = huge(0.0_real64)
        tcnt = huge(0.0_real64)
        tall = huge(0.0_real64)
        t32 = huge(0.0_real64)
        do rep = 1, rounds
            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_ang2vec(qt(k), qp(k), v)
                call pf_query_disc(ns, v, r, buf, nl, scheme=scheme, inclusive=inclusive)
                chk = ieor(ishftc(chk, 1), nl)
            end do
            el = omp_get_wtime() - t0
            tbuf = min(tbuf, el)

            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_ang2vec(qt(k), qp(k), v)
                call pf_query_disc_count(ns, v, r, nl, scheme=scheme, inclusive=inclusive)
                chk = ieor(ishftc(chk, 1), nl)
            end do
            el = omp_get_wtime() - t0
            tcnt = min(tcnt, el)

            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_ang2vec(qt(k), qp(k), v)
                call pf_query_disc_alloc(ns, v, r, alloced, nl, scheme=scheme, inclusive=inclusive)
                chk = ieor(ishftc(chk, 1), nl)
            end do
            el = omp_get_wtime() - t0
            tall = min(tall, el)

            t0 = omp_get_wtime()
            do k = 1_int64, nq
                call pf_ang2vec(qt(k), qp(k), v)
                call pf_query_disc(int(ns, int32), v, r, buf32, nl32, scheme=scheme, inclusive=inclusive)
                chk = ieor(ishftc(chk, 1), int(nl32, int64))
            end do
            el = omp_get_wtime() - t0
            t32 = min(t32, el)
        end do

        chkr = chkr + real(tot, real64)
        write(output_unit, '(f9.2,a,f10.1,4f12.4)') rad_deg, ' deg', &
            real(tot, real64) / real(nq, real64), &
            1.0e6_real64 * tbuf / real(nq, real64), 1.0e6_real64 * tcnt / real(nq, real64), &
            1.0e6_real64 * tall / real(nq, real64), 1.0e6_real64 * t32 / real(nq, real64)
        flush(output_unit)
        deallocate (qt, qp, buf, buf32)
        if (allocated(alloced)) deallocate (alloced)
    end subroutine disc_row

    !> A generous buffer for a disc of `rad_deg` degrees at resolution `ns`.
    !>
    !> The same recipe the out-of-tree harness uses for `libhealpix`'s `listpix`: twice the cap's
    !> area in pixels, plus a floor. Generous on purpose -- a buffer that is exactly right would
    !> make the timing depend on whether the estimate was, which is not what is being measured.
    function disc_capacity(ns, rad_deg) result(cap)
        integer(int64), intent(in) :: ns !! resolution.
        real(real64), intent(in) :: rad_deg !! disc radius, degrees.
        integer(int64) :: cap !! elements to allocate.
        real(real64) :: cap_area, pix_area, r

        pix_area = 4.0_real64 * pi / real(pf_nside2npix(ns), real64)
        r = min(pi, abs(rad_deg) * d2r)
        cap_area = 2.0_real64 * pi * (1.0_real64 - cos(r))
        cap = 1000_int64 + 2_int64 * ceiling(cap_area / pix_area, int64)
    end function disc_capacity

end program benchmark_healpix
