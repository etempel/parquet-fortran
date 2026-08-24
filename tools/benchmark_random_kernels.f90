!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Driver for `bench/benchmark_random_kernels.sh`: times `parquet_random` in whichever route (e)
!> kernel it was compiled with, and emits a checksum so the two builds can be proved equivalent.
!>
!> **Not under `app/` OR `bench/`**, for the same reason `tools/check_random_kernels.f90` is not --
!> and note `bench/` is a source-dir in `fpm.toml`, so a file placed there IS built by fpm. Forcing the
!> other kernel needs `-U__GFORTRAN__`, which also flips `src/parquet.f90`'s stringify branch, so
!> the package will not build that way at all. Only a standalone compile of `parquet_random` and
!> its one dependency works, and fpm cannot express that. Nothing builds this but its own wrapper.
!>
!> **The checksum is not decoration -- it is the gate.** The two kernels compute the same algorithm
!> by different arithmetic, so they must agree BIT FOR BIT; a speed figure from an arm that
!> disagrees is a figure for something that is not this library. gfortran is known to miscompile the
!> wrapping arithmetic under LTO (`feature_risks.md` Risk-101), which is exactly the failure this
!> gate catches, and exactly why the wrapper refuses to report timings unless the checksums match.
!>
!> Output is one `KERNEL=... CHECK=... NS_R64=... NS_R32=... NS_SCALAR=...` line, machine-readable
!> so the wrapper can compare two runs without parsing prose.
program benchmark_random_kernels

    use iso_fortran_env, only: int32, int64, real32, real64, output_unit
    use parquet_random

    implicit none

    integer(int64), parameter :: SEED = 20260817_int64
    integer(int64), parameter :: STREAM = 1_int64

    character(len=8) :: kernel
    integer(int64) :: n, n_scalar, chk
    integer :: rounds
    real(real64) :: ns_r64, ns_r32, ns_scalar

    n = 10000000_int64
    n_scalar = 10000000_int64
    rounds = 5
    call read_args(n, n_scalar, rounds)

    if (parquet_debug_random_uses_int128()) then
        kernel = "int128"
    else
        kernel = "wrapping"
    end if

    call time_bulk_r64(n, rounds, ns_r64)
    call time_bulk_r32(n, rounds, ns_r32)
    call time_scalar(n_scalar, rounds, ns_scalar)
    call checksum(chk)

    ! `f0.4`, not `f10.4`: a padded field puts spaces between the `=` and the number, and the
    ! wrapper splits this line on spaces. That cost a run -- every field parsed as empty and the
    ! ratio arithmetic divided by zero, which at least failed loudly rather than reporting 0.00x.
    write (output_unit, "(a,a,a,i0,a,f0.4,a,f0.4,a,f0.4)") &
        "KERNEL=", trim(kernel), " CHECK=", chk, &
        " NS_R64=", ns_r64, " NS_R32=", ns_r32, " NS_SCALAR=", ns_scalar

contains

    !> Reads `--n=`, `--scalar=` and `--rounds=` from the command line.
    subroutine read_args(n, n_scalar, rounds)
        integer(int64), intent(inout) :: n                  !! Bulk fill length.
        integer(int64), intent(inout) :: n_scalar           !! Scalar loop iterations.
        integer, intent(inout) :: rounds                    !! Rounds per arm; the best is kept.

        character(len=128) :: arg
        integer :: i, ios

        do i = 1, command_argument_count()
            call get_command_argument(i, arg)
            if (arg(1:min(4, len_trim(arg))) == "--n=") then
                read (arg(5:len_trim(arg)), *, iostat=ios) n
            else if (arg(1:min(9, len_trim(arg))) == "--scalar=") then
                read (arg(10:len_trim(arg)), *, iostat=ios) n_scalar
            else if (arg(1:min(9, len_trim(arg))) == "--rounds=") then
                read (arg(10:len_trim(arg)), *, iostat=ios) rounds
            end if
        end do
    end subroutine read_args

    !> Wall-clock seconds from `system_clock`'s int64 counter; only differences are meaningful.
    function now() result(t)
        real(real64) :: t                                   !! Seconds since an arbitrary origin.
        integer(int64) :: c, r
        call system_clock(count=c, count_rate=r)
        t = real(c, real64) / real(r, real64)
    end function now

    !> Best-of-`rounds` `pf_random_fill_draws` over `real64`, in nanoseconds per value.
    subroutine time_bulk_r64(n, rounds, ns)
        integer(int64), intent(in) :: n                     !! Array length.
        integer, intent(in) :: rounds                       !! Rounds; the best is kept.
        real(real64), intent(out) :: ns                     !! Nanoseconds per value.

        real(real64), allocatable :: v(:)
        real(real64) :: t0, best, sink
        integer :: r

        allocate (v(n))
        call pf_random_fill_draws(SEED, STREAM, v)          ! first touch, before any timing
        sink = v(1)
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_random_fill_draws(SEED, STREAM, v)
            best = min(best, now() - t0)
            sink = sink + v(n)
        end do
        ns = best * 1.0e9_real64 / real(n, real64)
        if (sink == -1.0_real64) write (output_unit, "(a)") ""
        deallocate (v)
    end subroutine time_bulk_r64

    !> Best-of-`rounds` `pf_random_fill_draws` over `real32`, in nanoseconds per value.
    subroutine time_bulk_r32(n, rounds, ns)
        integer(int64), intent(in) :: n                     !! Array length.
        integer, intent(in) :: rounds                       !! Rounds; the best is kept.
        real(real64), intent(out) :: ns                     !! Nanoseconds per value.

        real(real32), allocatable :: v(:)
        real(real64) :: t0, best
        real(real32) :: sink
        integer :: r

        allocate (v(n))
        call pf_random_fill_draws(SEED, STREAM, v)
        sink = v(1)
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            call pf_random_fill_draws(SEED, STREAM, v)
            best = min(best, now() - t0)
            sink = sink + v(n)
        end do
        ns = best * 1.0e9_real64 / real(n, real64)
        if (sink == -1.0_real32) write (output_unit, "(a)") ""
        deallocate (v)
    end subroutine time_bulk_r32

    !> Best-of-`rounds` scalar `pf_random_at` along the stream axis, in nanoseconds per value.
    subroutine time_scalar(n, rounds, ns)
        integer(int64), intent(in) :: n                     !! Iterations per round.
        integer, intent(in) :: rounds                       !! Rounds; the best is kept.
        real(real64), intent(out) :: ns                     !! Nanoseconds per value.

        real(real64) :: t0, best, acc
        integer(int64) :: i
        integer :: r

        acc = 0.0_real64
        do i = 1_int64, min(n, 1000_int64)
            acc = acc + pf_random_at(SEED, i)
        end do
        best = huge(1.0_real64)
        do r = 1, rounds
            t0 = now()
            do i = 1_int64, n
                acc = acc + pf_random_at(SEED, i)
            end do
            best = min(best, now() - t0)
        end do
        ns = best * 1.0e9_real64 / real(n, real64)
        if (acc == -1.0_real64) write (output_unit, "(a)") ""
    end subroutine time_scalar

    !> A bit-exact fingerprint of the generator's output, for cross-kernel comparison.
    !!
    !! Built from `pf_random_bits_at` rather than from the reals: comparing `real64` values would
    !! let a low-bit difference hide behind formatting, and the raw bits are what the two kernels
    !! are actually obliged to agree on. Sweeps both axes and a spread of coordinates, including the
    !! wide integer draw whose stride-2 grid is where the two kernels' arithmetic differs most.
    subroutine checksum(chk)
        integer(int64), intent(out) :: chk                  !! Fingerprint; equal across kernels.

        integer(int64) :: i, d, b, lo, hi
        integer(int64), parameter :: MULT = 6364136223846793005_int64

        chk = 0_int64
        do i = 1_int64, 400_int64
            do d = 1_int64, 4_int64
                b = pf_random_bits_at(SEED, i, d)
                chk = chk * MULT + b
                chk = ieor(chk, ishft(chk, -29))
            end do
        end do
        ! The integer rule, both narrow (stride 1) and wide (stride 2), plus a rejection-heavy width.
        lo = 0_int64
        do i = 1_int64, 400_int64
            hi = 1000_int64
            chk = chk * MULT + pf_random_int_at(SEED, i, lo, hi)
            hi = 4611686018427387904_int64
            chk = chk * MULT + pf_random_int_at(SEED, i, lo, hi)
            hi = 3_int64 * (huge(1_int64) / 5_int64)
            chk = chk * MULT + pf_random_int_at(SEED, i, lo, hi)
            chk = ieor(chk, ishft(chk, -29))
        end do
    end subroutine checksum

end program benchmark_random_kernels
