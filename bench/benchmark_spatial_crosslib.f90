!> The parquet-fortran arm of the cross-library spatial comparison: reads coordinates and queries
!> from files, answers one family of queries, and writes the rows it found plus its own timings.
!!
!! Driven by `bench/benchmark_spatial_crosslib.sh`, which builds it `--profile release` and hands
!! its path to `bench/benchmark_spatial_crosslib.py`; that script owns the fixtures, the scipy and
!! scikit-learn arms, the row-for-row comparison and the report. `bench/benchmark_spatial.sh` is
!! the different question of where the time goes INSIDE this library -- the tuner, the cell size,
!! the walk; this program treats `pf_spatial_index` as a black box and exists to be compared
!! against other software.
!!
!! **Both arms see the same bytes.** Nothing is generated here: the coordinates and the query list
!! arrive as raw `real64` streams the Python side wrote, so a difference in the answer is a
!! difference between the two libraries and never between two fixtures.
!!
!! **Rules from CLAUDE.md's benchmarking section that this program follows.** Best of `--rounds`
!! per figure, the run least disturbed by everything else on the machine; every output buffer is
!! allocated before any timed loop; the rows are written out in a pass of their own, AFTER the
!! timed rounds, so no figure includes the file write; and the total row count is printed, so an
!! arm cannot be optimised away.
!!
!! Command line, all `--key=value`:
!!
!!   * `--pts=<file>`      the coordinates: `3*n` `real64`, point-major (`x1 y1 z1 x2 ...`).
!!   * `--queries=<file>`  the queries: `8*nq` `real64`, `p1(3) p2(3) r1 r2` each. A ball uses
!!                         `p1` and `r1`; a sky query uses `p1(1)` as RA, `p1(2)` as Dec and `r1`
!!                         as the angular radius in degrees.
!!   * `--n=` `--nq=`      how many of each.
!!   * `--mode=`           `ball`, `countball`, `seg`, `cyl`, `cone`, `sky`, `knn`, `selfjoin`,
!!                         `csr` or `pairs`.
!!   * `--out=<prefix>`    writes `<prefix>.bin` (the answer) and `<prefix>.time` (the figures).
!!   * `--rounds=`         timed repetitions; the best is kept.
!!   * `--threads=`        team size for the bulk modes; 0 leaves it to the library.
!!   * `--k=`              neighbours for `--mode=knn`.
program benchmark_spatial_crosslib
    use, intrinsic :: iso_fortran_env, only: int64, real64, output_unit
    use parquet_spatial, only: pf_spatial_index, parquet_set_verbosity, &
        parquet_debug_spatial_shell_rounds, parquet_debug_reset_spatial_counters
    implicit none

    character(len=256) :: ptsfile, qfile, outfile, mode, arg
    integer(int64) :: n, nq, k, q, m, kk, total, rounds, ir, threads, kval
    real(real64), allocatable, target :: x(:), y(:), z(:)
    real(real64), allocatable :: buf(:), qb(:), dist(:)
    integer(int64), allocatable :: got(:), counts(:), pi(:), pj(:), offs(:), nb(:)
    type(pf_spatial_index) :: sx
    integer(int64) :: c0, c1, crate
    real(real64) :: tbuild, tquery, tbest_b, tbest_q, r
    integer :: u, i

    call parquet_set_verbosity("silent")
    ptsfile = ""
    qfile = ""
    outfile = "benchmark_spatial_crosslib_out"
    mode = "ball"
    n = 0_int64
    nq = 0_int64
    rounds = 3_int64
    threads = 0_int64
    kval = 10_int64
    do i = 1, command_argument_count()
        call get_command_argument(i, arg)
        if (index(arg, "--pts=") == 1) ptsfile = arg(7:)
        if (index(arg, "--queries=") == 1) qfile = arg(11:)
        if (index(arg, "--out=") == 1) outfile = arg(7:)
        if (index(arg, "--mode=") == 1) mode = arg(8:)
        if (index(arg, "--n=") == 1) read (arg(5:), *) n
        if (index(arg, "--nq=") == 1) read (arg(6:), *) nq
        if (index(arg, "--rounds=") == 1) read (arg(10:), *) rounds
        if (index(arg, "--threads=") == 1) read (arg(11:), *) threads
        if (index(arg, "--k=") == 1) read (arg(5:), *) kval
    end do
    if (n < 1_int64) error stop "benchmark_spatial_crosslib: --n= must be given and positive"
    if (nq < 1_int64) error stop "benchmark_spatial_crosslib: --nq= must be given and positive"
    call system_clock(count_rate=crate)

    allocate (buf(3_int64 * n), x(n), y(n), z(n))
    open (newunit=u, file=trim(ptsfile), access="stream", form="unformatted", status="old", action="read")
    read (u) buf
    close (u)
    do k = 1_int64, n
        x(k) = buf(3_int64 * (k - 1_int64) + 1_int64)
        y(k) = buf(3_int64 * (k - 1_int64) + 2_int64)
        z(k) = buf(3_int64 * (k - 1_int64) + 3_int64)
    end do
    deallocate (buf)
    allocate (qb(8_int64 * nq))
    open (newunit=u, file=trim(qfile), access="stream", form="unformatted", status="old", action="read")
    read (u) qb
    close (u)

    ! Every buffer is allocated and touched before the first timed round, so no arm pays another's
    ! page faults.
    allocate (got(n), dist(n), counts(n))
    got = 0_int64
    dist = 0.0_real64
    counts = 0_int64

    r = qb(7)
    tbest_b = huge(1.0_real64)
    do ir = 1_int64, rounds
        call sx%clear()
        call system_clock(c0)
        if (trim(mode) == "sky") then
            call sx%build_sky(x, y, radius_deg=r)
        else
            call sx%build(x, y, z, radius=r)
        end if
        call system_clock(c1)
        tbuild = real(c1 - c0, real64) / real(crate, real64)
        if (tbuild < tbest_b) tbest_b = tbuild
    end do

    open (newunit=u, file=trim(outfile) // ".bin", access="stream", form="unformatted", &
          status="replace", action="write")
    total = 0_int64
    tbest_q = huge(1.0_real64)

    select case (trim(mode))
    case ("countball")
        do ir = 1_int64, rounds
            call system_clock(c0)
            do q = 1_int64, nq
                m = sx%count_within(qb(8_int64 * (q - 1_int64) + 1_int64:8_int64 * (q - 1_int64) + 3_int64), &
                                    qb(8_int64 * (q - 1_int64) + 7_int64))
                total = total + m
            end do
            call system_clock(c1)
            tquery = real(c1 - c0, real64) / real(crate, real64)
            if (tquery < tbest_q) tbest_q = tquery
        end do
        total = total / rounds
        write (u) total
    case ("ball", "seg", "cyl", "cone", "sky")
        do ir = 1_int64, rounds
            call system_clock(c0)
            do q = 1_int64, nq
                m = one_query(q)
            end do
            call system_clock(c1)
            tquery = real(c1 - c0, real64) / real(crate, real64)
            if (tquery < tbest_q) tbest_q = tquery
        end do
        do q = 1_int64, nq
            m = one_query(q)
            write (u) m
            do kk = 1_int64, min(m, n)
                write (u) got(kk)
            end do
            total = total + m
        end do
    case ("knn")
        do ir = 1_int64, rounds
            call parquet_debug_reset_spatial_counters()
            call system_clock(c0)
            do q = 1_int64, nq
                m = sx%nearest(qb(8_int64 * (q - 1_int64) + 1_int64:8_int64 * (q - 1_int64) + 3_int64), &
                               kval, got, dist=dist)
            end do
            call system_clock(c1)
            tquery = real(c1 - c0, real64) / real(crate, real64)
            if (tquery < tbest_q) tbest_q = tquery
        end do
        do q = 1_int64, nq
            m = sx%nearest(qb(8_int64 * (q - 1_int64) + 1_int64:8_int64 * (q - 1_int64) + 3_int64), &
                           kval, got, dist=dist)
            write (u) m
            do kk = 1_int64, min(m, n)
                write (u) got(kk)
            end do
            total = total + m
        end do
    case ("selfjoin")
        do ir = 1_int64, rounds
            call system_clock(c0)
            if (threads > 0_int64) then
                call sx%count_all_within(r, counts, threads=int(threads))
            else
                call sx%count_all_within(r, counts)
            end if
            call system_clock(c1)
            tquery = real(c1 - c0, real64) / real(crate, real64)
            if (tquery < tbest_q) tbest_q = tquery
        end do
        do k = 1_int64, n
            write (u) counts(k)
            total = total + counts(k)
        end do
    case ("csr")
        do ir = 1_int64, rounds
            if (allocated(offs)) deallocate (offs, nb)
            call system_clock(c0)
            if (threads > 0_int64) then
                call sx%all_within(r, offs, nb, threads=int(threads))
            else
                call sx%all_within(r, offs, nb)
            end if
            call system_clock(c1)
            tquery = real(c1 - c0, real64) / real(crate, real64)
            if (tquery < tbest_q) tbest_q = tquery
        end do
        total = size(nb, kind=int64)
        do k = 1_int64, n
            write (u) offs(k + 1_int64) - offs(k)
        end do
    case ("pairs")
        do ir = 1_int64, rounds
            if (allocated(pi)) deallocate (pi, pj)
            call system_clock(c0)
            if (threads > 0_int64) then
                call sx%pairs_within(r, pi, pj, threads=int(threads))
            else
                call sx%pairs_within(r, pi, pj)
            end if
            call system_clock(c1)
            tquery = real(c1 - c0, real64) / real(crate, real64)
            if (tquery < tbest_q) tbest_q = tquery
        end do
        total = size(pi, kind=int64)
        ! A degree per point rather than the pair list itself: the comparison is exact either way
        ! and this is length n instead of length 2*total.
        counts = 0_int64
        do k = 1_int64, total
            counts(pi(k)) = counts(pi(k)) + 1_int64
            counts(pj(k)) = counts(pj(k)) + 1_int64
        end do
        do k = 1_int64, n
            write (u) counts(k)
        end do
    case default
        error stop "benchmark_spatial_crosslib: unknown --mode="
    end select
    close (u)

    open (newunit=u, file=trim(outfile) // ".time", status="replace", action="write")
    write (u, '(a,es16.9)') "build_s ", tbest_b
    write (u, '(a,es16.9)') "query_s ", tbest_q
    write (u, '(a,i0)') "total_rows ", total
    write (u, '(a,i0)') "n ", n
    write (u, '(a,i0)') "nq ", nq
    write (u, '(a,es16.9)') "cell ", sx%cell_size()
    write (u, '(a,i0)') "cells ", sx%cells()
    write (u, '(a,i0)') "shell_rounds ", parquet_debug_spatial_shell_rounds()
    close (u)
    write (output_unit, '(a,a,a,es12.5,a,es12.5,a,i0)') "# ", trim(mode), ": build ", tbest_b, &
        " s, query ", tbest_q, " s, rows ", total

contains

    !> One query of whichever family `mode` names, into `got`; returns the TRUE count.
    function one_query(qi) result(mm)
        integer(int64), intent(in) :: qi !! which query, 1-based.
        integer(int64) :: mm !! how many rows are in the region, whatever the buffer holds.
        integer(int64) :: b
        real(real64) :: p1(3), p2(3), r1, r2

        b = 8_int64 * (qi - 1_int64)
        p1 = [qb(b + 1_int64), qb(b + 2_int64), qb(b + 3_int64)]
        p2 = [qb(b + 4_int64), qb(b + 5_int64), qb(b + 6_int64)]
        r1 = qb(b + 7_int64)
        r2 = qb(b + 8_int64)
        select case (trim(mode))
        case ("ball")
            mm = sx%within(p1, r1, got)
        case ("seg")
            mm = sx%within_segment(p1, p2, r1, got)
        case ("cyl")
            mm = sx%within_cylinder(p1, p2, r1, got)
        case ("cone")
            mm = sx%within_cone(p1, p2, r1, r2, got)
        case default
            mm = sx%within_sky(p1(1), p1(2), r1, got)
        end select
    end function one_query
end program benchmark_spatial_crosslib
