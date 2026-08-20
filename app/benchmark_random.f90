!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Times this library's generator against the intrinsic `random_number`, per value.
!!
!! **The two do not do the same job, so a ratio here is not a verdict.** `random_number` advances a
!! hidden per-process state: it is sequential by construction, gives no way to name a value, and is
!! neither reproducible across compilers nor safe to call from several threads without care.
!! `pf_random_*` is counter-based: every value has a coordinate, any value can be produced without
!! producing the ones before it, and the answer is frozen by `pf_random_algorithm`. The cost measured
!! here buys those properties. Read the ratio as their price, not as a defect.
!!
!! Three arms, because the comparison inverts between them:
!!
!!   - **bulk r64/r32** -- `call random_number(v)` against `call pf_random_fill_draws(seed, s, v)`,
!!     the like-for-like array fill and the shape almost every caller should use.
!!   - **scalar** -- `call random_number(x)` in a loop against `x = pf_random_at(seed, i)`. The
!!     library arm is doing strictly more here: it addresses a fresh stream per iteration, which the
!!     intrinsic cannot express at all.
!!
!! Rules this program follows, all of them from CLAUDE.md's benchmarking notes: every buffer is
!! fully written before the timer starts (first-touch page faults otherwise land entirely on
!! whichever arm runs first), each arm runs both orders, the reported figure is the best of several
!! rounds, the keep-it-live checksum is accumulated outside every timed region, and the index
!! arithmetic carries no `mod` (a runtime divisor is an `idivq`, ~6 ns/iteration, which would be
!! larger than the scalar arm's whole per-value cost).
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Run it through its wrapper, which asserts the build is optimised:
!!
!! ```bash
!! tools/benchmark_random.sh
!! ```
program benchmark_random

    use iso_fortran_env, only: int64, real32, real64, output_unit
    use parquet, only: pf_random_at, pf_random_fill_draws, pf_random_algorithm, &
                       parquet_debug_random_uses_int128

    implicit none

    integer(int64), parameter :: SEED = 20260817_int64      !! Any fixed seed; the cost is seed-independent.
    integer(int64), parameter :: STREAM = 1_int64           !! Draw-axis fill walks draws of one stream.

    integer(int64) :: sizes(16)
    integer :: rounds, k, nsizes
    integer(int64) :: n_scalar

    sizes(1:3) = [10000_int64, 1000000_int64, 100000000_int64]
    nsizes = 3
    rounds = 5
    n_scalar = 10000000_int64

    call parse_args(sizes, nsizes, rounds, n_scalar)

    write (output_unit, "(a)") "benchmark_random -- pf_random vs intrinsic random_number"
    write (output_unit, "(a,a)") "  algorithm    : ", pf_random_algorithm
    write (output_unit, "(a,l1)") "  int128 arm   : ", parquet_debug_random_uses_int128()
    write (output_unit, "(a,i0)") "  rounds       : ", rounds
    write (output_unit, "(a)") ""
    write (output_unit, "(a)") "Per value, nanoseconds. 'ratio' is library / intrinsic;"
    write (output_unit, "(a)") "a ratio above 1 means the library costs that many times the intrinsic."
    write (output_unit, "(a)") ""
    write (output_unit, "(a)") "  arm        n            intrinsic      library        ratio"
    write (output_unit, "(a)") "  ---------- ------------ -------------- -------------- ------"

    do k = 1, nsizes
        call bulk_r64(sizes(k), rounds)
    end do
    do k = 1, nsizes
        call bulk_r32(sizes(k), rounds)
    end do
    call scalar_r64(n_scalar, rounds)

contains

    !> Reads `--sizes=a,b,c`, `--rounds=n`, `--scalar=n` from the command line.
    !!
    !! `--sizes=` REPLACES the sweep rather than overwriting its leading entries, so asking for one
    !! size runs one size. The first version overwrote in place and left the rest at their defaults,
    !! which silently ran a 10**8 case nobody asked for -- a benchmark that measures more than it was
    !! told to is the same class of fault as one that measures less.
    subroutine parse_args(sizes, nsizes, rounds, n_scalar)
        integer(int64), intent(inout) :: sizes(:)           !! Bulk fill lengths to sweep.
        integer, intent(inout) :: nsizes                    !! How many leading entries of `sizes` are live.
        integer, intent(inout) :: rounds                    !! Rounds per arm; the best is reported.
        integer(int64), intent(inout) :: n_scalar           !! Length of the scalar arm's loop.

        character(len=256) :: arg
        integer :: i, nargs, ios, p, q, j
        integer(int64) :: val

        nargs = command_argument_count()
        do i = 1, nargs
            call get_command_argument(i, arg)
            if (arg(1:min(9, len_trim(arg))) == "--rounds=") then
                read (arg(10:len_trim(arg)), *, iostat=ios) rounds
            else if (arg(1:min(9, len_trim(arg))) == "--scalar=") then
                read (arg(10:len_trim(arg)), *, iostat=ios) n_scalar
            else if (arg(1:min(8, len_trim(arg))) == "--sizes=") then
                p = 9
                j = 0
                do while (p <= len_trim(arg) .and. j < size(sizes))
                    q = index(arg(p:len_trim(arg)), ",")
                    if (q == 0) then
                        q = len_trim(arg) + 1
                    else
                        q = p + q - 1
                    end if
                    read (arg(p:q - 1), *, iostat=ios) val
                    if (ios == 0) then
                        j = j + 1
                        sizes(j) = val
                    end if
                    p = q + 1
                end do
                if (j > 0) nsizes = j
            end if
        end do
    end subroutine parse_args

    !> Wall-clock seconds since an arbitrary origin, from `system_clock`'s int64 counter.
    function now() result(t)
        real(real64) :: t                                   !! Seconds; only differences are meaningful.
        integer(int64) :: c, r
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
    end function now

    !> Emits one result row, converting both totals to nanoseconds per value.
    subroutine report(arm, n, t_intr, t_lib)
        character(len=*), intent(in) :: arm                 !! Short arm label.
        integer(int64), intent(in) :: n                     !! Values produced per round.
        real(real64), intent(in) :: t_intr                  !! Best intrinsic round, seconds.
        real(real64), intent(in) :: t_lib                   !! Best library round, seconds.

        real(real64) :: ns_i, ns_l

        ns_i = t_intr * 1.0e9_real64 / real(n, real64)
        ns_l = t_lib * 1.0e9_real64 / real(n, real64)
        write (output_unit, "(a,a10,1x,i12,1x,f14.3,1x,f14.3,1x,f6.2)") &
            "  ", arm, n, ns_i, ns_l, ns_l / max(ns_i, tiny(1.0_real64))
    end subroutine report

    !> `call random_number(v)` against `call pf_random_fill_draws(seed, stream, v)`, `real64`.
    subroutine bulk_r64(n, rounds)
        integer(int64), intent(in) :: n                     !! Array length.
        integer, intent(in) :: rounds                       !! Rounds; the best is reported.

        real(real64), allocatable :: v(:)
        real(real64) :: t0, t_intr, t_lib, sink
        integer :: r

        allocate (v(n))
        ! Warm both the pages and the caches through BOTH arms before either is timed.
        call random_number(v)
        call pf_random_fill_draws(SEED, STREAM, v)
        sink = sum(v)

        t_intr = huge(1.0_real64)
        t_lib = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call random_number(v)
            t_intr = min(t_intr, now() - t0)
            sink = sink + v(1) + v(n)

            t0 = now()
            call pf_random_fill_draws(SEED, STREAM, v)
            t_lib = min(t_lib, now() - t0)
            sink = sink + v(1) + v(n)
        end do

        call report("bulk r64", n, t_intr, t_lib)
        if (sink == -1.0_real64) write (output_unit, "(a)") ""   ! keeps `sink` live; never taken
        deallocate (v)
    end subroutine bulk_r64

    !> The same comparison at `real32`, where one Philox word serves one value instead of two.
    subroutine bulk_r32(n, rounds)
        integer(int64), intent(in) :: n                     !! Array length.
        integer, intent(in) :: rounds                       !! Rounds; the best is reported.

        real(real32), allocatable :: v(:)
        real(real64) :: t0, t_intr, t_lib
        real(real32) :: sink
        integer :: r

        allocate (v(n))
        call random_number(v)
        call pf_random_fill_draws(SEED, STREAM, v)
        sink = sum(v)

        t_intr = huge(1.0_real64)
        t_lib = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call random_number(v)
            t_intr = min(t_intr, now() - t0)
            sink = sink + v(1) + v(n)

            t0 = now()
            call pf_random_fill_draws(SEED, STREAM, v)
            t_lib = min(t_lib, now() - t0)
            sink = sink + v(1) + v(n)
        end do

        call report("bulk r32", n, t_intr, t_lib)
        if (sink == -1.0_real32) write (output_unit, "(a)") ""
        deallocate (v)
    end subroutine bulk_r32

    !> Scalar `call random_number(x)` against `x = pf_random_at(seed, i)`, one value per iteration.
    !!
    !! The library arm addresses a DIFFERENT stream each iteration, which is the coordinate-addressed
    !! shape a caller reaches for when values must be reproducible per index. The intrinsic has no
    !! equivalent, so this arm compares a strictly larger capability against a smaller one.
    subroutine scalar_r64(n, rounds)
        integer(int64), intent(in) :: n                     !! Iterations per round.
        integer, intent(in) :: rounds                       !! Rounds; the best is reported.

        real(real64) :: t0, t_intr, t_lib, x, acc
        integer(int64) :: i
        integer :: r

        acc = 0.0_real64
        do i = 1_int64, min(n, 1000_int64)
            call random_number(x)
            acc = acc + x + pf_random_at(SEED, i)
        end do

        t_intr = huge(1.0_real64)
        t_lib = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do i = 1_int64, n
                call random_number(x)
                acc = acc + x
            end do
            t_intr = min(t_intr, now() - t0)

            t0 = now()
            do i = 1_int64, n
                acc = acc + pf_random_at(SEED, i)
            end do
            t_lib = min(t_lib, now() - t0)
        end do

        call report("scalar r64", n, t_intr, t_lib)
        if (acc == -1.0_real64) write (output_unit, "(a)") ""
    end subroutine scalar_r64

end program benchmark_random
