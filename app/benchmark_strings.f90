!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> **What each `parquet_string_column` bulk operation costs**, on one synthetic column, so that an
!! optimisation to this type is measured rather than argued. Driven by `tools/benchmark_strings.sh`;
!! see `feature_string_parallel.md` for the plan these numbers gate.
!!
!! Every operation is timed **best of `--rounds`**, each round starting from a fresh `%clone()` of
!! the same source column so no round inherits another's page state or another's allocation. The
!! throughput column is payload-equivalent (the source column's byte count divided by the elapsed
!! time) and is for comparing rows against each other, not a claim about bytes actually moved --
!! `gather` selects half the rows, `to_character` writes a wider padded result than it reads, and
!! so on.
!!
!! **Run it through `--profile release`.** A bare `fpm run` builds at `-O0` (fpm applies profile
!! flags only when `--profile` is given), and this measures ratios between loops that optimise very
!! differently, so an unoptimised run does not merely scale everything down -- it reorders the table.
program benchmark_strings
    use, intrinsic :: iso_fortran_env, only : int64, real64, output_unit, error_unit
    use parquet
    implicit none

    integer(int64) :: nrows
    integer :: avg_len, rounds
    logical :: with_nulls
    integer(int64) :: null_every

    call parse_arguments(nrows, avg_len, rounds, with_nulls, null_every)
    call run_all(nrows, avg_len, rounds, with_nulls, null_every)

contains

    !> Reads the `--key=value` command line, applying each default when the flag is absent.
    subroutine parse_arguments(nrows, avg_len, rounds, with_nulls, null_every)
        integer(int64), intent(out) :: nrows      !! elements in the synthetic column.
        integer, intent(out) :: avg_len           !! mean element length in bytes.
        integer, intent(out) :: rounds            !! timed rounds; the best of them is kept.
        logical, intent(out) :: with_nulls        !! .true. => make every null_every-th element null.
        integer(int64), intent(out) :: null_every !! null stride, when with_nulls.
        character(len=64) :: arg, val
        integer :: k, eq

        nrows = 4000000_int64
        avg_len = 24
        rounds = 3
        with_nulls = .false.
        null_every = 7_int64

        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            eq = index(arg, "=")
            if (eq == 0) then
                if (trim(arg) == "--nulls") then
                    with_nulls = .true.
                    cycle
                end if
                write (error_unit, '(a)') "benchmark_strings: unrecognised argument '"//trim(arg)//"'"
                error stop 1
            end if
            val = arg(eq+1:)
            select case (arg(1:eq-1))
            case ("--nrows")
                read (val, *) nrows
            case ("--len")
                read (val, *) avg_len
            case ("--rounds")
                read (val, *) rounds
            case ("--null-every")
                read (val, *) null_every
                with_nulls = .true.
            case default
                write (error_unit, '(a)') "benchmark_strings: unknown option '"//trim(arg(1:eq-1))//"'"
                write (error_unit, '(a)') "Usage: benchmark_strings [--nrows=N] [--len=N] " // &
                    "[--rounds=N] [--nulls] [--null-every=N]"
                error stop 1
            end select
        end do

        if (nrows < 1_int64) error stop "benchmark_strings: --nrows must be >= 1"
        if (avg_len < 2) error stop "benchmark_strings: --len must be >= 2"
        if (rounds < 1) error stop "benchmark_strings: --rounds must be >= 1"
        if (null_every < 2_int64) error stop "benchmark_strings: --null-every must be >= 2"
    end subroutine parse_arguments

    !> Fills `c` with `n` elements whose lengths vary deterministically around `avg_len`.
    !!
    !! Built with `%append_string` rather than `%set`: `%set` rewrites the offsets of every later
    !! element, so filling an empty column that way is O(n^2) and does not finish at these sizes.
    subroutine build(c, n, avg_len, with_nulls, null_every)
        type(parquet_string_column), intent(inout) :: c !! receives the synthetic column.
        integer(int64), intent(in) :: n                 !! element count.
        integer, intent(in) :: avg_len                  !! mean element length.
        logical, intent(in) :: with_nulls               !! .true. => insert nulls.
        integer(int64), intent(in) :: null_every        !! null stride.
        integer(int64) :: k, l, lo, span
        character(len=512) :: buf

        lo = int(avg_len, int64)/2_int64
        span = max(int(avg_len, int64) - lo, 1_int64)
        call c%clear()
        call c%reserve(n, n*int(avg_len, int64))
        do k = 1_int64, n
            if (with_nulls) then
                if (mod(k, null_every) == 0_int64) then
                    call c%append_null()
                    cycle
                end if
            end if
            ! A cheap deterministic spread, so the column is not uniform-width and the run is
            ! reproducible without carrying a seed.
            l = lo + mod(k*2654435761_int64/65536_int64, span)
            l = max(1_int64, min(l, int(len(buf), int64)))
            buf = repeat("x", int(l))
            call c%append_string(buf(1:int(l)))
        end do
    end subroutine build

    !> Times every operation and writes the report.
    subroutine run_all(nrows, avg_len, rounds, with_nulls, null_every)
        integer(int64), intent(in) :: nrows      !! elements in the synthetic column.
        integer, intent(in) :: avg_len           !! mean element length.
        integer, intent(in) :: rounds            !! timed rounds.
        logical, intent(in) :: with_nulls        !! .true. => column contains nulls.
        integer(int64), intent(in) :: null_every !! null stride.
        type(parquet_string_column) :: src, work, dest
        type(parquet_string), allocatable :: handles(:)
        character(len=:), allocatable :: chars(:)
        integer(int64), allocatable :: perm(:), idx(:)
        logical, allocatable :: keep(:)
        integer(int64) :: k, payload, sink
        integer :: r
        real(real64) :: t0, best

        call build(src, nrows, avg_len, with_nulls, null_every)
        payload = src%character_size()
        write (output_unit, '(a,i0,a,i0,a,l1,a,i0)') "rows=", src%size(), "  avg_len=", avg_len, &
            "  nulls=", with_nulls, "  null_count=", src%null_count()
        write (output_unit, '(a,f9.1,a,i0,a)') "payload=", real(payload, real64)/1.0e6_real64, &
            " MB   best of ", rounds, " rounds"
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') "operation                     seconds     MB/s (payload-equiv)"
        write (output_unit, '(a)') "---------------------------------------------------------------"

        allocate(perm(nrows), idx(nrows/2_int64), keep(nrows))
        do k = 1_int64, nrows
            perm(k) = nrows - k + 1_int64      ! reversal: a true permutation, worst-case locality
        end do
        do k = 1_int64, nrows/2_int64
            idx(k) = 2_int64*k
        end do
        keep = .true.
        do k = 1_int64, nrows, 3_int64
            keep(k) = .false.
        end do

        best = huge(1.0_real64)
        do r = 1, rounds
            work = src%clone()
            t0 = now()
            call work%reindex_trusted(perm)
            best = min(best, now() - t0)
        end do
        call report("reindex_trusted", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            work = src%clone()
            t0 = now()
            call work%reindex(perm)
            best = min(best, now() - t0)
        end do
        call report("reindex (validated)", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            work = src%clone()
            t0 = now()
            call work%gather(idx)
            best = min(best, now() - t0)
        end do
        call report("gather (n/2)", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            work = src%clone()
            t0 = now()
            call work%delete_by_mask(keep)
            best = min(best, now() - t0)
        end do
        call report("delete_by_mask", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            work = src%clone()
            t0 = now()
            call work%trim_all()
            best = min(best, now() - t0)
        end do
        call report("trim_all", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            work = src%clone()
            best = min(best, now() - t0)
        end do
        call report("clone", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call src%slice(1_int64, nrows, dest)
            best = min(best, now() - t0)
        end do
        call report("slice (whole)", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            call work%clear()
            call work%reserve(2_int64*nrows, 2_int64*payload)
            t0 = now()
            call work%append_column(src)
            best = min(best, now() - t0)
        end do
        call report("append_column", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            if (allocated(chars)) deallocate(chars)
            t0 = now()
            if (with_nulls) then
                call src%to_character(chars, null_value="")
            else
                call src%to_character(chars)
            end if
            best = min(best, now() - t0)
        end do
        call report("to_character", best, payload)
        if (allocated(chars)) deallocate(chars)

        allocate(handles(nrows))
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call src%view_all(handles)
            best = min(best, now() - t0)
        end do
        call report("view_all", best, payload)

        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call work%build_from(handles)
            best = min(best, now() - t0)
        end do
        call report("build_from", best, payload)

        sink = 0_int64
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            sink = 0_int64
            do k = 1_int64, nrows
                sink = sink + src%length(k)
            end do
            best = min(best, now() - t0)
        end do
        call report("length loop", best, payload)

        write (output_unit, '(a)') ""
        write (output_unit, '(a,i0,a)') "(checksum ", sink, ")"
    end subroutine run_all

    !> Writes one result row.
    subroutine report(label, secs, payload)
        character(len=*), intent(in) :: label !! operation name.
        real(real64), intent(in) :: secs      !! best elapsed time.
        integer(int64), intent(in) :: payload !! source payload bytes, for the throughput column.
        character(len=30) :: pad

        pad = label
        write (output_unit, '(a,f10.4,a,f12.1)') pad, secs, "   ", &
            real(payload, real64)/1.0e6_real64/max(secs, 1.0e-9_real64)
    end subroutine report

    !> Wall-clock seconds from the system clock.
    real(real64) function now()
        integer(int64) :: c, r

        call system_clock(c, r)
        now = real(c, real64)/real(r, real64)
    end function now

end program benchmark_strings
