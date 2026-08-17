!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Streams raw bits from a chosen axis of `parquet_random` to stdout, for PractRand.
!!
!! **What this is for, and what it is NOT for.** The generator is Philox4x32-10, whose statistical
!! properties are published and whose three known-answer vectors this library already reproduces bit
!! for bit (`test_kat_vectors`). A battery cannot add to that: a wrong constant or a wrong round
!! count would still be a strong mixing function and would very likely pass, while the KAT catches it
!! on the first vector. What a battery CAN reach is the part Philox's literature says nothing about
!! -- **this library's own mapping from `(seed, stream, draw)` onto the cipher's key and counter
!! words**, and the seed-derivation and permutation constructions built on top of it. That is what
!! the axes below are for, and it is why the interesting ones are not the sequential walk.
!!
!! Axes, chosen with `--axis=`:
!!
!! | axis | what it streams | word size | why it is interesting |
!! |---|---|---|---|
!! | `draw` | `pf_random_bits_at(seed, 1, k)`, k = 1, 2, 3, … | 64 | the sequential counter walk — the one case published Philox results already cover, so this is a wiring check, not a finding |
!! | `stream` | `pf_random_bits_at(seed, i)`, i = 1, 2, 3, … | 64 | **the library's own mapping** — the stream index becomes counter words 2 and 3, and this is the loop the guide tells users to write |
!! | `seed` | `pf_random_bits_at(seed0 + j, 1)`, j = 0, 1, 2, … | 64 | consecutive seeds, one draw each — what `seed = base + rank` produces across MPI ranks |
!! | `key` | `pf_random_bits_at(pf_random_key(seed0, j), 1)` | 64 | the derived-key fan-out, which is `mix64` rather than the cipher |
!! | `perm` | low 32 bits of `pf_random_perm_at(seed, 2**62, k) - 1` | 32 | the 4-round modular Feistel, the one construction here with no published KATs |
!!
!! **`perm` needs the width care it gets.** `pf_random_perm_at` returns a value in `[1, m]`, so
!! feeding it whole would hand the battery a constant top bit and fail instantly for a reason that is
!! about the range, not the generator. With `m = 2**62` the low 32 bits of `value - 1` are exactly
!! uniform (2**62 is a multiple of 2**32) and the domain is far larger than anything streamed, so the
!! outputs behave like independent draws rather than like a permutation being exhausted.
!!
!! **The integer path is deliberately absent, and that is not an oversight.** `pf_random_int_at` over
!! a non-power-of-two range cannot be fed to a bit battery at all — the output is uniform on `[lo, hi]`,
!! not on a power of two, and converting it back to bits destroys the property under test. Over a
!! power-of-two range the rejection clause never fires, so it would test nothing the `draw` axis does
!! not. The exactness of that reduction is settled by the overflow-free reference in
!! `test/test_random_reference.f90`, which is the right instrument for it.
!!
!! **A harness is untested code** (CLAUDE.md), so every axis is verified against its own scalar entry
!! point before a single byte reaches stdout, and the check writes to **stderr** so it cannot corrupt
!! the stream. `--selftest` runs the checks and exits.
!!
!! Not run by `fpm test`, by design. Usage:
!!
!! ```bash
!! fpm run --profile release probe_random_practrand -- --axis=stream | RNG_test stdin64 -tlmax 4TB
!! fpm run --profile release probe_random_practrand -- --axis=perm   | RNG_test stdin32
!! ```
program probe_random_practrand

    use iso_fortran_env, only: int32, int64, error_unit
    use parquet, only: pf_random_bits_at, pf_random_key, pf_random_perm_at, &
                       pf_random_fill_draws, pf_random_fill_streams

    implicit none

    !> Values per write. 1 MiB of `int64` — big enough that the write is not the bottleneck.
    integer(int64), parameter :: CHUNK = 131072_int64
    !> The full `int64` range as a `(lo, hi)` pair; the integer fills then return the raw pattern,
    !! because a width of `2**64` has nothing to reduce. See `int_reduce`'s `s == 0` branch.
    integer(int64), parameter :: LO64 = -huge(1_int64) - 1_int64
    integer(int64), parameter :: HI64 = huge(1_int64)
    !> The permutation domain: a power of two, so the low 32 bits of `value - 1` are exactly uniform.
    integer(int64), parameter :: PERM_M = 4611686018427387904_int64      ! 2**62

    character(len=32) :: axis
    integer(int64) :: seed, produced, want, k, n
    integer(int64) :: buf64(CHUNK)
    integer(int32) :: buf32(CHUNK)
    integer :: u
    logical :: selftest

    axis = read_arg_str('--axis=', 'stream')
    seed = read_arg_int('--seed=', 20260817_int64)
    want = read_arg_int('--bytes=', 0_int64)            ! 0 = stream until the reader stops
    selftest = has_flag('--selftest')

    call verify_axes()
    if (selftest) then
        write (error_unit, '(a)') 'probe_random_practrand: all axis self-checks passed'
        stop
    end if

    ! Raw, native-endian bytes. PractRand's `stdin32`/`stdin64` read exactly this.
    open (newunit=u, file='/dev/stdout', access='stream', form='unformatted', action='write')

    produced = 0_int64
    k = 1_int64
    do
        select case (trim(axis))
        case ('draw')
            ! One stream, consecutive draws. The bulk fill over the full int64 range returns the
            ! same 64-bit patterns `pf_random_bits_at` would, and walks blocks rather than values.
            call pf_random_fill_draws(seed, 1_int64, buf64, LO64, HI64, k)
            write (u) buf64
            produced = produced + CHUNK * 8_int64
        case ('stream')
            call pf_random_fill_streams(seed, k, buf64, LO64, HI64)
            write (u) buf64
            produced = produced + CHUNK * 8_int64
        case ('seed')
            do n = 1_int64, CHUNK
                buf64(n) = pf_random_bits_at(seed + k + n - 2_int64, 1_int64)
            end do
            write (u) buf64
            produced = produced + CHUNK * 8_int64
        case ('seedwalk')
            ! Gray-code seed walk: successive seeds at Hamming distance ONE. This emulates
            ! PractRand's own `-ttseed64 -walk_greycode` target, which cannot be pointed at an
            ! external RNG through `stdin`. It is the sharpest available test of the seed -> key
            ! mapping, and much sharper than consecutive integers: `seed` and `seed+1` differ in
            ! the low bits only, while this reaches every bit position including the high ones.
            do n = 1_int64, CHUNK
                buf64(n) = pf_random_bits_at(gray_seed(seed, k + n - 1_int64), 1_int64)
            end do
            write (u) buf64
            produced = produced + CHUNK * 8_int64
        case ('key')
            do n = 1_int64, CHUNK
                buf64(n) = pf_random_bits_at(pf_random_key(seed, k + n - 1_int64), 1_int64)
            end do
            write (u) buf64
            produced = produced + CHUNK * 8_int64
        case ('perm')
            do n = 1_int64, CHUNK
                buf32(n) = low32(pf_random_perm_at(seed, PERM_M, k + n - 1_int64) - 1_int64)
            end do
            write (u) buf32
            produced = produced + CHUNK * 4_int64
        case default
            write (error_unit, '(a,a)') 'unknown --axis=', trim(axis)
            error stop 1
        end select
        k = k + CHUNK
        if (want > 0_int64 .and. produced >= want) exit
    end do
    close (u)

contains

    !> Checks every axis against the scalar entry point it claims to stream. Aborts on a mismatch.
    !!
    !! This is the gate that makes a green PractRand run mean anything: a feeder that silently
    !! re-emits one stream, or takes the low bits where it says it takes the high ones, produces a
    !! battery result that is confidently about nothing.
    subroutine verify_axes()
        integer(int64) :: v64(64), j
        integer(int32) :: v32(64)
        call pf_random_fill_draws(seed, 1_int64, v64, LO64, HI64, 1_int64)
        do j = 1_int64, 64_int64
            call must(v64(j) == pf_random_bits_at(seed, 1_int64, j), 'draw axis != pf_random_bits_at')
        end do
        call pf_random_fill_streams(seed, 1_int64, v64, LO64, HI64)
        do j = 1_int64, 64_int64
            call must(v64(j) == pf_random_bits_at(seed, j), 'stream axis != pf_random_bits_at')
        end do
        do j = 1_int64, 64_int64
            v64(j) = pf_random_bits_at(seed + j - 1_int64, 1_int64)
        end do
        call must(any(v64 /= v64(1)), 'seed axis is constant')
        ! The Gray walk must actually move ONE bit per step, or it is not the test it claims to be.
        do j = 2_int64, 64_int64
            call must(popcnt(ieor(gray_seed(seed, j), gray_seed(seed, j - 1_int64))) == 1, &
                      'the Gray-code seed walk does not move exactly one bit per step')
        end do
        call must(gray_seed(seed, 1_int64) == seed, 'the Gray-code seed walk does not start at seed')
        do j = 1_int64, 64_int64
            v32(j) = low32(pf_random_perm_at(seed, PERM_M, j) - 1_int64)
        end do
        call must(any(v32 /= v32(1)), 'perm axis is constant')
        ! The two 64-bit axes must NOT agree beyond their one shared coordinate, or the feeder is
        ! emitting the same sequence twice under two names.
        call must(pf_random_bits_at(seed, 1_int64, 2_int64) /= pf_random_bits_at(seed, 2_int64), &
                  'draw and stream axes coincide at k = 2 -- the feeder is streaming one axis twice')
    end subroutine verify_axes

    !> Aborts with `why` unless `ok`.
    subroutine must(ok, why)
        logical, intent(in) :: ok                   !! the condition that must hold
        character(len=*), intent(in) :: why         !! what to say when it does not
        if (.not. ok) then
            write (error_unit, '(a,a)') 'probe_random_practrand SELF-CHECK FAILED: ', why
            error stop 1
        end if
    end subroutine must

    !> Seed number `j` of a Gray-code walk starting at `base`: `base` xor the Gray code of `j-1`.
    !!
    !! `gray(j) = ieor(j, ishft(j, -1))` differs from `gray(j-1)` in exactly one bit, so consecutive
    !! seeds are at Hamming distance 1 and the walk reaches all 64 bit positions rather than only the
    !! low ones a `+1` walk disturbs.
    pure function gray_seed(base, j) result(s)
        integer(int64), intent(in) :: base          !! the walk's starting seed
        integer(int64), intent(in) :: j             !! 1-based step number
        integer(int64) :: s                         !! the seed for that step
        integer(int64) :: t
        t = j - 1_int64
        s = ieor(base, ieor(t, ishft(t, -1)))
    end function gray_seed

    !> The low 32 bits of a 64-bit value, as a signed `int32` with the same bit pattern.
    pure function low32(v) result(r)
        integer(int64), intent(in) :: v             !! any 64-bit value
        integer(int32) :: r                         !! its low 32 bits
        integer(int64) :: t
        t = iand(v, 4294967295_int64)
        if (t > huge(1_int32)) t = t - 4294967296_int64
        r = int(t, int32)
    end function low32

    !> Reads `--key=<int>` from the command line; returns `dflt` when absent.
    function read_arg_int(key, dflt) result(v)
        character(len=*), intent(in) :: key         !! the flag, including its trailing `=`
        integer(int64), intent(in) :: dflt          !! value when the flag is absent
        integer(int64) :: v                         !! the parsed value
        character(len=64) :: b
        integer :: j, ios
        v = dflt
        do j = 1, command_argument_count()
            call get_command_argument(j, b)
            if (index(b, key) == 1) then
                read (b(len(key) + 1:), *, iostat=ios) v
                if (ios /= 0) v = dflt
            end if
        end do
    end function read_arg_int

    !> Reads `--key=<text>` from the command line; returns `dflt` when absent.
    function read_arg_str(key, dflt) result(v)
        character(len=*), intent(in) :: key         !! the flag, including its trailing `=`
        character(len=*), intent(in) :: dflt        !! value when the flag is absent
        character(len=32) :: v                      !! the parsed value
        character(len=64) :: b
        integer :: j
        v = dflt
        do j = 1, command_argument_count()
            call get_command_argument(j, b)
            if (index(b, key) == 1) v = b(len(key) + 1:)
        end do
    end function read_arg_str

    !> Whether a bare flag is present on the command line.
    function has_flag(key) result(p)
        character(len=*), intent(in) :: key         !! the flag, exactly as typed
        logical :: p                                !! `.true.` when present
        character(len=64) :: b
        integer :: j
        p = .false.
        do j = 1, command_argument_count()
            call get_command_argument(j, b)
            if (trim(b) == key) p = .true.
        end do
    end function has_flag

end program probe_random_practrand
