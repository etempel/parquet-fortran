!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Dumps blocks from the SHIPPED Philox kernel, for `tools/philox_reference.py` to verify.
!!
!! The Fortran half of `tools/check_philox_compliance.sh`. Each line is one block:
!!
!! ```
!! key stream index w0 w1 w2 w3
!! ```
!!
!! as signed decimal `integer(int64)` values, which the oracle reads back as unsigned bit patterns.
!! The words come from `parquet_debug_random_block`, so this exercises **the arithmetic that ships**
!! -- including the route (e) fork, whichever arm this build took -- and not a re-derivation of it.
!!
!! **Why arbitrary coordinates rather than draws.** `test_kat_vectors` already pins the kernel at the
!! three published vectors and the golden vectors pin a grid of the public surface. Both are exact
!! and narrow. Two of the three published vectors are not even reachable through a draw, because
!! they need counter words above the roughly `2**62` a block index can hold. Going through
!! `parquet_debug_random_block` lifts that restriction and lets the whole 64-bit coordinate space be
!! swept.
!!
!! **The structured coordinates matter more than the pseudo-random ones, and they come first.** A
!! shift, mask or half-swap defect lives at `0`, at all-ones, at a single set bit, and at the 32-bit
!! boundary where the packing splits a coordinate in half -- not at a uniformly random 64-bit value,
!! which such a defect corrupts into another value that looks equally random. So the dump opens with
!! the full cross product of a table of awkward values and only then draws pseudo-random ones.
!!
!! The pseudo-random coordinates are taken from the library's own generator, which is sound here
!! because they are only *test points*: the comparison is against an implementation that shares
!! nothing with it, so where the points came from cannot make a mismatch invisible.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Driven by `tools/check_philox_compliance.sh`; run directly with:
!!
!! ```bash
!! fpm run --profile release probe_random_philox -- --n=20000 | tools/philox_reference.py
!! ```
program probe_random_philox

    use iso_fortran_env, only: int64, output_unit, error_unit
    use parquet, only: parquet_debug_random_block, parquet_debug_random_uses_int128, &
                       pf_random_bits_at

    implicit none

    !> Awkward 64-bit values: zero, all-ones, the 32-bit boundary either side, single set bits at
    !! every byte and at both halves' edges, and the two alternating patterns. A packing defect that
    !! swaps or mis-shifts a half shows up here and nowhere else.
    integer(int64), parameter :: EDGE(16) = [ &
        0_int64, &
        -1_int64, &                                   ! all ones
        1_int64, &
        huge(1_int64), &                              ! 2**63 - 1
        -huge(1_int64) - 1_int64, &                   ! 2**63, the sign bit alone
        4294967295_int64, &                           ! 2**32 - 1: low half full, high half empty
        4294967296_int64, &                           ! 2**32: high half's lowest bit alone
        -4294967296_int64, &                          ! high half full, low half empty
        6148914691236517205_int64, &                  ! 0x5555...
        -6148914691236517206_int64, &                 ! 0xAAAA...
        255_int64, &
        -72057594037927936_int64, &                   ! 0xFF00000000000000
        2147483648_int64, &                           ! 2**31, the low half's sign bit
        2147483647_int64, &
        281474976710656_int64, &                      ! 2**48
        65535_int64]

    integer(int64) :: n, i, key, stream, blk, emitted
    integer :: a, b, c
    character(len=64) :: arg
    logical :: quiet

    n = read_arg_int('--n=', 20000_int64)
    quiet = has_flag('--quiet')

    if (.not. quiet) then
        write (error_unit, '(a,l1)') 'probe_random_philox: int128 kernel arm = ', &
            parquet_debug_random_uses_int128()
    end if

    emitted = 0_int64

    ! Phase 1: the full cross product of the edge table -- 16**3 = 4096 blocks. These are the
    ! coordinates a packing or shifting defect actually lives at.
    do a = 1, size(EDGE)
        do b = 1, size(EDGE)
            do c = 1, size(EDGE)
                if (emitted >= n) exit
                call emit(EDGE(a), EDGE(b), EDGE(c))
                emitted = emitted + 1_int64
            end do
        end do
    end do

    ! Phase 2: pseudo-random coordinates for the rest, to reach parts of the space no hand-written
    ! table would think of. Three unrelated seeds so the three coordinates cannot move together.
    i = 1_int64
    do while (emitted < n)
        key = pf_random_bits_at(11_int64, i, 1_int64)
        stream = pf_random_bits_at(22_int64, i, 1_int64)
        blk = pf_random_bits_at(33_int64, i, 1_int64)
        call emit(key, stream, blk)
        emitted = emitted + 1_int64
        i = i + 1_int64
    end do

    if (.not. quiet) then
        write (error_unit, '(a,i0,a)') 'probe_random_philox: emitted ', emitted, ' blocks'
    end if

contains

    !> Enciphers one coordinate through the shipped kernel and writes its row.
    subroutine emit(kk, ss, ii)
        integer(int64), intent(in) :: kk            !! the 64-bit key
        integer(int64), intent(in) :: ss            !! the stream, counter words 2 and 3
        integer(int64), intent(in) :: ii            !! the block index, counter words 0 and 1
        integer(int64) :: o0, o1, o2, o3
        call parquet_debug_random_block(kk, ss, ii, o0, o1, o2, o3)
        write (output_unit, '(7(i0,1x))') kk, ss, ii, o0, o1, o2, o3
    end subroutine emit

    !> Reads `--key=<int>` from the command line; returns `dflt` when absent.
    function read_arg_int(key_, dflt) result(v)
        character(len=*), intent(in) :: key_        !! the flag, including its trailing `=`
        integer(int64), intent(in) :: dflt          !! value when the flag is absent
        integer(int64) :: v                         !! the parsed value
        integer :: j, ios
        v = dflt
        do j = 1, command_argument_count()
            call get_command_argument(j, arg)
            if (index(arg, key_) == 1) then
                read (arg(len(key_) + 1:), *, iostat=ios) v
                if (ios /= 0) v = dflt
            end if
        end do
    end function read_arg_int

    !> Whether a bare flag is present on the command line.
    function has_flag(key_) result(p)
        character(len=*), intent(in) :: key_        !! the flag, exactly as typed
        logical :: p                                !! `.true.` when present
        integer :: j
        p = .false.
        do j = 1, command_argument_count()
            call get_command_argument(j, arg)
            if (trim(arg) == key_) p = .true.
        end do
    end function has_flag

end program probe_random_philox
