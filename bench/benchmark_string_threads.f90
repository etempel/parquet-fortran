!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> **Does threading `parquet_string_column`'s rebuild pay on THIS machine?**
!!
!! A single-purpose measurement, written to settle one question with one command and one block of
!! output — see `feature_string_test.md` for the decision it feeds and for how to report the result.
!!
!! It sweeps `parquet_set_string_threads` over a list of thread counts, timing
!! `%reindex_trusted` (the only internally-threaded operation) at each, and reports the speedup
!! against the serial baseline. It also times `%to_character` and `%build_from`, which are **not**
!! threaded and never will be — they are here because their cost is dominated by per-element heap
!! allocation, which behaves very differently between compilers, and knowing whether that
!! difference is larger on this machine is the second thing the decision needs.
!!
!! **Correctness is checked, not assumed**: every threaded result is compared element-by-element
!! against the serial one, so a run that reports a speedup has also proved the answers agree.
!!
!! **Must be built `--profile release`.** A bare `fpm run` builds at `-O0` — fpm applies profile
!! flags only when `--profile` is given — and at `-O0` the ratios this reports are meaningless.
program benchmark_string_threads
    use, intrinsic :: iso_fortran_env, only : int64, real64, output_unit, error_unit
#ifdef _OPENMP
    use omp_lib, only : omp_get_max_threads
#endif
    use parquet
    implicit none

    integer, parameter :: MAX_SWEEP = 16
    integer(int64) :: nrows
    integer :: rounds, avg_len, nsweep, sweep(MAX_SWEEP)
    logical :: with_nulls

    call parse_arguments(nrows, avg_len, rounds, with_nulls, sweep, nsweep)
    call run(nrows, avg_len, rounds, with_nulls, sweep, nsweep)

contains

    !> Reads the `--key=value` command line, applying each default when a flag is absent.
    subroutine parse_arguments(nrows, avg_len, rounds, with_nulls, sweep, nsweep)
        integer(int64), intent(out) :: nrows   !! elements in the synthetic column.
        integer, intent(out) :: avg_len        !! mean element length in bytes.
        integer, intent(out) :: rounds         !! timed rounds; the best is kept.
        logical, intent(out) :: with_nulls     !! .true. => every 7th element is null.
        integer, intent(out) :: sweep(:)       !! thread counts to sweep.
        integer, intent(out) :: nsweep         !! how many entries of `sweep` are used.
        character(len=256) :: arg, val
        integer :: k, eq, avail

        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        nrows = 4000000_int64
        avg_len = 24
        rounds = 5
        with_nulls = .false.
        ! 1 is the serial baseline every ratio below is measured against, so it must come first.
        ! 2 and 3 are included deliberately even though the library declines to thread there: seeing
        ! them resolve to 1 is how a reader confirms the break-even clause is in force.
        call default_sweep(avail, sweep, nsweep)

        do k = 1, command_argument_count()
            call get_command_argument(k, arg)
            eq = index(arg, "=")
            if (eq == 0) then
                if (trim(arg) == "--nulls") then
                    with_nulls = .true.
                    cycle
                end if
                write (error_unit, '(a)') "benchmark_string_threads: unrecognised argument '"//trim(arg)//"'"
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
            case ("--threads")
                call parse_sweep(val, sweep, nsweep)
            case default
                write (error_unit, '(a)') "benchmark_string_threads: unknown option '"//trim(arg(1:eq-1))//"'"
                write (error_unit, '(a)') "Usage: benchmark_string_threads [--nrows=N] [--len=N] " // &
                    "[--rounds=N] [--nulls] [--threads=1,2,4,8,...]"
                error stop 1
            end select
        end do

        if (nrows < 1000_int64) error stop "benchmark_string_threads: --nrows must be >= 1000"
        if (avg_len < 4) error stop "benchmark_string_threads: --len must be >= 4"
        if (rounds < 1) error stop "benchmark_string_threads: --rounds must be >= 1"
        if (nsweep < 1) error stop "benchmark_string_threads: --threads must name at least one count"
        if (sweep(1) /= 1) error stop "benchmark_string_threads: the first --threads entry must be 1 (the baseline)"
    end subroutine parse_arguments

    !> Powers of two up to the machine's thread count, plus 2 and 3 so the break-even clause is
    !> visible, always starting at 1.
    subroutine default_sweep(avail, sweep, nsweep)
        integer, intent(in) :: avail     !! threads OpenMP offers here.
        integer, intent(out) :: sweep(:) !! receives the counts.
        integer, intent(out) :: nsweep   !! how many were written.
        integer :: cand(MAX_SWEEP), ncand, n, i, j
        logical :: dup

        ncand = 0
        call add_candidate(cand, ncand, 1)
        call add_candidate(cand, ncand, 2)
        call add_candidate(cand, ncand, 3)
        n = 4
        do while (n < avail)
            call add_candidate(cand, ncand, n)
            n = n*2
        end do
        call add_candidate(cand, ncand, avail)

        nsweep = 0
        do i = 1, ncand
            dup = .false.
            do j = 1, nsweep
                if (sweep(j) == cand(i)) dup = .true.
            end do
            if (dup) cycle
            if (nsweep >= size(sweep)) exit
            nsweep = nsweep + 1
            sweep(nsweep) = cand(i)
        end do
    end subroutine default_sweep

    !> Appends `v` to `cand` if there is room. A separate procedure because Fortran permits only one
    !> level of internal procedures, so `default_sweep` cannot contain its own helper.
    subroutine add_candidate(cand, ncand, v)
        integer, intent(inout) :: cand(:) !! the candidate list.
        integer, intent(inout) :: ncand   !! how many entries are used.
        integer, intent(in) :: v          !! the value to append.

        if (v < 1) return
        if (ncand >= size(cand)) return
        ncand = ncand + 1
        cand(ncand) = v
    end subroutine add_candidate

    !> Parses a comma-separated thread list such as `1,2,4,8,16`.
    subroutine parse_sweep(text, sweep, nsweep)
        character(len=*), intent(in) :: text !! the comma-separated list.
        integer, intent(out) :: sweep(:)     !! receives the counts.
        integer, intent(out) :: nsweep       !! how many were written.
        integer :: lo, hi, v

        nsweep = 0
        lo = 1
        do while (lo <= len_trim(text))
            hi = index(text(lo:), ",")
            if (hi == 0) then
                hi = len_trim(text) + 1
            else
                hi = lo + hi - 1
            end if
            if (hi > lo) then
                read (text(lo:hi-1), *) v
                if (v < 1) error stop "benchmark_string_threads: thread counts must be >= 1"
                if (nsweep < size(sweep)) then
                    nsweep = nsweep + 1
                    sweep(nsweep) = v
                end if
            end if
            lo = hi + 1
        end do
    end subroutine parse_sweep

    !> Fills `c` with `n` elements of deterministically varying length.
    !!
    !! Built with `%append_string`: `%set` rewrites every later element's offset, so filling an empty
    !! column that way is O(n^2) and does not finish at these sizes.
    subroutine build(c, n, avg_len, with_nulls)
        type(parquet_string_column), intent(inout) :: c !! receives the column.
        integer(int64), intent(in) :: n                 !! element count.
        integer, intent(in) :: avg_len                  !! mean element length.
        logical, intent(in) :: with_nulls               !! .true. => every 7th element is null.
        integer(int64) :: k, l, lo, span
        character(len=512) :: buf

        lo = int(avg_len, int64)/2_int64
        span = max(int(avg_len, int64) - lo, 1_int64)
        call c%clear()
        call c%reserve(n, n*int(avg_len, int64))
        do k = 1_int64, n
            if (with_nulls) then
                if (mod(k, 7_int64) == 0_int64) then
                    call c%append_null()
                    cycle
                end if
            end if
            l = lo + mod(k*2654435761_int64/65536_int64, span)
            l = max(1_int64, min(l, int(len(buf), int64)))
            buf = repeat("x", int(l))
            call c%append_string(buf(1:int(l)))
        end do
    end subroutine build

    !> Whether two columns agree in every observable: rows, payload, nulls and every value.
    logical function same_column(a, b) result(res)
        type(parquet_string_column), intent(in) :: a !! first column.
        type(parquet_string_column), intent(in) :: b !! second column.
        character(len=:), allocatable :: sa, sb
        integer(int64) :: k

        res = .false.
        if (a%size() /= b%size()) return
        if (a%character_size() /= b%character_size()) return
        if (a%null_count() /= b%null_count()) return
        do k = 1_int64, a%size()
            if (a%is_null(k) .neqv. b%is_null(k)) return
            if (a%is_null(k)) cycle
            call a%get(k, sa)
            call b%get(k, sb)
            ! Nested, not `.and.`-ed: Fortran does not short-circuit, so a combined length-and-content
            ! test would compare two differently-sized strings.
            if (len(sa) /= len(sb)) return
            if (sa /= sb) return
        end do
        res = .true.
    end function same_column

    !> Runs the sweep and writes the report.
    subroutine run(nrows, avg_len, rounds, with_nulls, sweep, nsweep)
        integer(int64), intent(in) :: nrows !! elements in the column.
        integer, intent(in) :: avg_len      !! mean element length.
        integer, intent(in) :: rounds       !! timed rounds.
        logical, intent(in) :: with_nulls   !! .true. => column contains nulls.
        integer, intent(in) :: sweep(:)     !! thread counts.
        integer, intent(in) :: nsweep       !! entries used in `sweep`.
        ! `src` must be a target -- %view_all hands back pointers into it; see the same note in
        ! test/test_string_parallel.f90 for why F2018 15.5.2.4 makes this mandatory, not stylistic.
        type(parquet_string_column), target :: src, work, reference
        character(len=:), allocatable :: chars(:)
        type(parquet_string), allocatable :: handles(:)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: k, payload
        integer :: i, r, resolved, avail, best_i
        real(real64) :: t0, best, serial_t, best_t
        logical :: ok

        avail = 1
#ifdef _OPENMP
        avail = omp_get_max_threads()
#endif
        call build(src, nrows, avg_len, with_nulls)
        payload = src%character_size()
        allocate(perm(nrows))
        do k = 1_int64, nrows
            perm(k) = nrows - k + 1_int64   ! reversal: a true permutation, worst case for locality
        end do

        write (output_unit, '(a)') "=================================================================="
        write (output_unit, '(a)') " parquet-fortran :: string column threading measurement"
        write (output_unit, '(a)') "=================================================================="
        write (output_unit, '(a,i0)')   " omp_get_max_threads()  : ", avail
        write (output_unit, '(a,i0)')   " rows                   : ", src%size()
        write (output_unit, '(a,f10.1,a)') " payload                : ", &
            real(payload, real64)/1.0e6_real64, " MB"
        write (output_unit, '(a,i0)')   " nulls                  : ", src%null_count()
        write (output_unit, '(a,i0)')   " timed rounds (best of) : ", rounds
        write (output_unit, '(a)') ""
        write (output_unit, '(a)') " PART 1 -- reindex_trusted, swept over thread counts"
        write (output_unit, '(a)') " ----------------------------------------------------------------"
        write (output_unit, '(a)') "  cap    used      seconds     speedup   identical"
        write (output_unit, '(a)') " ----------------------------------------------------------------"

        serial_t = -1.0_real64
        best_t = huge(1.0_real64)
        best_i = 1
        do i = 1, nsweep
            call parquet_set_string_threads(sweep(i))
            best = huge(1.0_real64)
            do r = 1, rounds
                work = src%clone()
                t0 = now()
                call work%reindex_trusted(perm)
                best = min(best, now() - t0)
            end do
            ! What the OPERATION will use, not merely what the cap allows -- the two differ below
            ! the break-even, and that difference is exactly what this column exists to show.
            resolved = parquet_debug_string_bulk_threads(src)
            if (i == 1) then
                serial_t = best
                reference = work%clone()
                ok = .true.
            else
                ok = same_column(reference, work)
            end if
            if (best < best_t) then
                best_t = best
                best_i = i
            end if
            write (output_unit, '(i5,i10,f14.5,f11.2,a,a)') sweep(i), resolved, best, &
                serial_t/max(best, 1.0e-12_real64), "     ", yesno(ok)
            if (.not. ok) then
                write (error_unit, '(a,i0,a)') "FAIL: the result at ", sweep(i), &
                    " threads differs from the serial result -- this is a CORRECTNESS failure, " // &
                    "not a performance one. Report it and stop."
                error stop 1
            end if
        end do
        call parquet_set_string_threads(0)

        write (output_unit, '(a)') " ----------------------------------------------------------------"
        write (output_unit, '(a,f6.2,a,i0,a)') " BEST SPEEDUP           : ", &
            serial_t/max(best_t, 1.0e-12_real64), "x  (at a cap of ", sweep(best_i), " threads)"
        write (output_unit, '(a,i0)')   " automatic resolves to  : ", parquet_debug_string_bulk_threads(src)
        write (output_unit, '(a)') ""

        ! ---- Part 2: the two SERIAL optimisations, for compiler context ----
        write (output_unit, '(a)') " PART 2 -- serial operations (never threaded), for context"
        write (output_unit, '(a)') " ----------------------------------------------------------------"
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
        if (allocated(chars)) deallocate(chars)
        write (output_unit, '(a,f12.5,a)') "  to_character          : ", best, " s"

        allocate(handles(nrows))
        call src%view_all(handles)
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call work%build_from(handles)
            best = min(best, now() - t0)
        end do
        write (output_unit, '(a,f12.5,a)') "  build_from            : ", best, " s"
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            work = src%clone()
            best = min(best, now() - t0)
        end do
        write (output_unit, '(a,f12.5,a)') "  clone (copy + alloc)  : ", best, " s"
        !
        ! **A real bandwidth floor, over WARM buffers.** The `clone` row above is a library
        ! operation and allocates its destination, so on a large machine it is dominated by
        ! first-touch page faults rather than by copying -- measured varying 5.6x between runs at
        ! the same payload size, and in one case coming out slower than `to_character`, which no
        ! genuine copy floor can be. Both buffers here are allocated and fully written BEFORE the
        ! timer starts, so this measures memory bandwidth and nothing else. Compare the three rows
        ! above against this one, not against `clone`.
        block
            character(len=1), allocatable :: warm_src(:), warm_dst(:)
            integer(int64) :: nb
            nb = max(payload, 1_int64)
            allocate(warm_src(nb), warm_dst(nb))
            warm_src = "x"
            warm_dst = "y"            ! first-touch both, outside the timer
            best = huge(1.0_real64)
            do r = 1, rounds
                t0 = now()
                warm_dst(1:nb) = warm_src(1:nb)
                best = min(best, now() - t0)
            end do
            write (output_unit, '(a,f12.5,a,f9.1,a)') "  memcpy floor (warm)   : ", best, " s  = ", &
                real(nb, real64)/1.0e6_real64/max(best, 1.0e-12_real64), " MB/s"
            ! Keep the copy observable so no compiler can elide the loop above.
            if (warm_dst(1) /= "x") write (output_unit, '(a)') "  (warm copy check failed)"
        end block
        write (output_unit, '(a)') "=================================================================="
    end subroutine run

    !> "yes"/"no ", padded so the column lines up.
    function yesno(v) result(res)
        logical, intent(in) :: v      !! the flag.
        character(len=3) :: res       !! "yes" or "no ".

        if (v) then
            res = "yes"
        else
            res = "no "
        end if
    end function yesno

    !> Wall-clock seconds from the system clock.
    real(real64) function now()
        integer(int64) :: c, r

        call system_clock(c, r)
        now = real(c, real64)/real(r, real64)
    end function now

end program benchmark_string_threads
