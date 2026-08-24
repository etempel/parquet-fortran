!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Phase-2 Q8 probe: should the tier-1 stream keep the enciphered block, or re-encipher per value?
!!
!! A Philox4x32 block is four 32-bit words. One `real64` draw consumes two of them and one `real32`
!! draw one, so a stream that discards the block after each value enciphers **twice** the blocks a
!! `real64` walk needs and **four times** what a `real32` walk needs. Q8 asks whether keeping the
!! block is worth the contract complication.
!!
!! The two arms differ by exactly one `if`. `stream_plain%uniform` enciphers on every call;
!! `stream_cached%uniform` enciphers only when the value it wants falls outside the block it is
!! already holding. Everything else -- the type shape, the type-bound call, the position
!! arithmetic, the word-to-value mapping -- is identical, so the measured difference is the cache
!! and nothing else.
!!
!! **The cache is keyed on the block index, not consumed as a queue.** That is deliberate and is
!! half of what the probe is for: `%position` still means a word index, `%rewind` still just sets
!! it, and the cached block is derivable state that either hits or misses. A saved-and-restored
!! stream therefore round-trips through `%position` alone, with no buffer to serialise -- so this
!! form changes no contract, which is what section 13's "still open" note doubted.
!!
!! Both types are plain scalars with no allocatable components and no `FINAL`, matching the
!! structural constraint `feature_random_phase2.md` §3.3 puts on `pf_random_stream` (Risk-45).
!!
!! Every arm's output is asserted equal to `pf_random_at`/`pf_random32_at` at the same coordinates
!! before any timing is reported, so a cache that returns a stale block fails loudly rather than
!! quietly measuring faster.
!!
!! Not run by `fpm test`, by design (CLAUDE.md, "Manual (never-`fpm test`) large-scale/benchmark
!! tools"). Build and run with `--profile release`.
!!
!! The two candidate stream types live in a module beside the program because a type-bound
!! procedure must be a module procedure -- a program's own contained procedures cannot be binding
!! targets. `bench/probe_random_int_rule.f90` is the precedent for the shape.
module pf_probe_stream

    use iso_fortran_env, only: int32, int64, real32, real64
    use parquet, only: parquet_debug_random_block

    implicit none

    !> One stream, re-enciphering on every value: the simple tier-1 form.
    type :: stream_plain
        integer(int64) :: seed = 0_int64            !! the stream family's seed
        integer(int64) :: stream = 0_int64          !! which stream
        integer(int64) :: pos = 0_int64             !! 0-based word position
    contains
        procedure :: uniform => plain_uniform
        procedure :: uniform32 => plain_uniform32
    end type stream_plain

    !> The same stream, holding the block it last enciphered.
    type :: stream_cached
        integer(int64) :: seed = 0_int64            !! the stream family's seed
        integer(int64) :: stream = 0_int64          !! which stream
        integer(int64) :: pos = 0_int64             !! 0-based word position
        integer(int64) :: blk = -1_int64            !! block index held, -1 when none
        integer(int64) :: c0 = 0_int64              !! held word 0
        integer(int64) :: c1 = 0_int64              !! held word 1
        integer(int64) :: c2 = 0_int64              !! held word 2
        integer(int64) :: c3 = 0_int64              !! held word 3
    contains
        procedure :: uniform => cached_uniform
        procedure :: uniform32 => cached_uniform32
    end type stream_cached

contains

    ! ---- the two candidate streams ----

    !> Next `real64`, re-enciphering the block every time.
    subroutine plain_uniform(self, x)
        class(stream_plain), intent(inout) :: self
        real(real64), intent(out) :: x
        integer(int64) :: w0, w1, w2, w3, b
        call parquet_debug_random_block(self%seed, self%stream, self%pos / 4_int64, w0, w1, w2, w3)
        if (modulo(self%pos, 4_int64) == 0_int64) then
            b = ior(ishft(w1, 32), w0)
        else
            b = ior(ishft(w3, 32), w2)
        end if
        x = to_r64(b)
        self%pos = self%pos + 2_int64
    end subroutine plain_uniform

    !> Next `real32`, re-enciphering the block every time.
    subroutine plain_uniform32(self, x)
        class(stream_plain), intent(inout) :: self
        real(real32), intent(out) :: x
        integer(int64) :: w0, w1, w2, w3, w
        call parquet_debug_random_block(self%seed, self%stream, self%pos / 4_int64, w0, w1, w2, w3)
        select case (int(modulo(self%pos, 4_int64), int32))
        case (0)
            w = w0
        case (1)
            w = w1
        case (2)
            w = w2
        case default
            w = w3
        end select
        x = to_r32(w)
        self%pos = self%pos + 1_int64
    end subroutine plain_uniform32

    !> Next `real64`, enciphering only on a block miss.
    subroutine cached_uniform(self, x)
        class(stream_cached), intent(inout) :: self
        real(real64), intent(out) :: x
        integer(int64) :: b, want
        want = self%pos / 4_int64
        if (want /= self%blk) then
            call parquet_debug_random_block(self%seed, self%stream, want, &
                                            self%c0, self%c1, self%c2, self%c3)
            self%blk = want
        end if
        if (modulo(self%pos, 4_int64) == 0_int64) then
            b = ior(ishft(self%c1, 32), self%c0)
        else
            b = ior(ishft(self%c3, 32), self%c2)
        end if
        x = to_r64(b)
        self%pos = self%pos + 2_int64
    end subroutine cached_uniform

    !> Next `real32`, enciphering only on a block miss.
    subroutine cached_uniform32(self, x)
        class(stream_cached), intent(inout) :: self
        real(real32), intent(out) :: x
        integer(int64) :: w, want
        want = self%pos / 4_int64
        if (want /= self%blk) then
            call parquet_debug_random_block(self%seed, self%stream, want, &
                                            self%c0, self%c1, self%c2, self%c3)
            self%blk = want
        end if
        select case (int(modulo(self%pos, 4_int64), int32))
        case (0)
            w = self%c0
        case (1)
            w = self%c1
        case (2)
            w = self%c2
        case default
            w = self%c3
        end select
        x = to_r32(w)
        self%pos = self%pos + 1_int64
    end subroutine cached_uniform32

    !> The module's own `real64` mapping: the top 53 bits, scaled.
    pure function to_r64(bits) result(r)
        integer(int64), intent(in) :: bits
        real(real64) :: r
        r = real(ishft(bits, -11), real64) * 2.0_real64**(-53)
    end function to_r64

    !> The module's own `real32` mapping: the top 24 bits of one word, scaled.
    pure function to_r32(w) result(r)
        integer(int64), intent(in) :: w
        real(real32) :: r
        r = real(ishft(w, -8), real32) * 2.0_real32**(-24)
    end function to_r32

end module pf_probe_stream

!> Drives the two candidate streams against tier 0 and against the shipped bulk fill.
program probe_random_stream

    use iso_fortran_env, only: int32, int64, real32, real64, output_unit
    use pf_probe_stream, only: stream_plain, stream_cached
    use parquet, only: pf_random_at, pf_random32_at, pf_random_int_at, pf_random_fill_draws, &
                       pf_random_fill_streams, parquet_debug_random_uses_int128

    implicit none

    integer(int64), parameter :: SEED = 20260816_int64
    integer(int64), parameter :: STREAM = 7_int64

    integer(int64) :: sizes(3)
    integer :: si, g_rounds

    sizes = [10000_int64, 1000000_int64, read_arg_int('--big=', 50000000_int64)]
    g_rounds = int(read_arg_int('--rounds=', 5_int64), int32)

    write(output_unit, '(a)') 'probe_random_stream: tier-1 block cache against re-enciphering'
    write(output_unit, '(a,l1)') 'PF_INT128 kernel compiled: ', parquet_debug_random_uses_int128()
    write(output_unit, '(a,i0)') 'times are ns per value, best of rounds = ', g_rounds
    write(output_unit, '(a)') ''
    flush(output_unit)

    do si = 1, size(sizes)
        call one_size(sizes(si))
    end do

contains

    !> Reads `--key=<int>` from the command line; returns `dflt` when absent.
    function read_arg_int(key, dflt) result(v)
        character(len=*), intent(in) :: key
        integer(int64), intent(in) :: dflt
        integer(int64) :: v
        character(len=64) :: buf
        integer :: k, ios
        v = dflt
        do k = 1, command_argument_count()
            call get_command_argument(k, buf)
            if (index(buf, key) == 1) then
                read(buf(len(key) + 1:), *, iostat=ios) v
                if (ios /= 0) v = dflt
                return
            end if
        end do
    end function read_arg_int

    ! ---- the measurement ----

    subroutine one_size(n)
        integer(int64), intent(in) :: n
        real(real64), allocatable :: v(:), ref(:)
        real(real32), allocatable :: v32(:), ref32(:)
        integer(int64), allocatable :: vi(:)
        real(real64) :: t_at, t_stream_axis, t_plain, t_cached, t_fill, t_fill_streams
        real(real64) :: q_at, q_plain, q_cached, q_fill, q_fill_streams
        real(real64) :: t_int
        integer(int64) :: k
        type(stream_plain) :: rp
        type(stream_cached) :: rc

        allocate(v(n), ref(n), v32(n), ref32(n), vi(n))

        ! Warm every buffer: first touch is page faults, and whichever arm ran first would pay them.
        v = 0.0_real64; ref = 0.0_real64; v32 = 0.0_real32; ref32 = 0.0_real32; vi = 0_int64

        ! ---- reference values, and the correctness gate ----
        do k = 1_int64, n
            ref(k) = pf_random_at(SEED, STREAM, k)
            ref32(k) = pf_random32_at(SEED, STREAM, k)
        end do

        rp = stream_plain(SEED, STREAM, 0_int64)
        do k = 1_int64, n
            call rp%uniform(v(k))
        end do
        call same_r64(v, ref, n, 'stream_plain%uniform')

        rc = stream_cached(SEED, STREAM, 0_int64, -1_int64, 0_int64, 0_int64, 0_int64, 0_int64)
        do k = 1_int64, n
            call rc%uniform(v(k))
        end do
        call same_r64(v, ref, n, 'stream_cached%uniform')

        call pf_random_fill_draws(SEED, STREAM, v)
        call same_r64(v, ref, n, 'pf_random_fill_draws')

        rp = stream_plain(SEED, STREAM, 0_int64)
        do k = 1_int64, n
            call rp%uniform32(v32(k))
        end do
        call same_r32(v32, ref32, n, 'stream_plain%uniform32')

        rc = stream_cached(SEED, STREAM, 0_int64, -1_int64, 0_int64, 0_int64, 0_int64, 0_int64)
        do k = 1_int64, n
            call rc%uniform32(v32(k))
        end do
        call same_r32(v32, ref32, n, 'stream_cached%uniform32')

        ! A cache that is never invalidated would also pass the sweep above, because the sweep only
        ! ever moves forward. Rewind and re-read a value from an EARLIER block: a stale-cache bug
        ! survives the forward walk and dies here.
        rc%pos = 0_int64
        call rc%uniform(v(1))
        if (v(1) /= ref(1)) then
            write(output_unit, '(a)') 'FATAL: cached stream did not honour a rewind'
            error stop 1
        end if

        ! ---- timing ----
        t_at = timed_at(v, n)
        t_stream_axis = timed_stream_axis(v, n)
        t_plain = timed_plain(v, n)
        t_cached = timed_cached(v, n)
        t_fill = timed_fill(v, n)
        t_fill_streams = timed_fill_streams(v, n)
        q_at = timed_at32(v32, n)
        q_plain = timed_plain32(v32, n)
        q_cached = timed_cached32(v32, n)
        q_fill = timed_fill32(v32, n)
        q_fill_streams = timed_fill_streams32(v32, n)
        t_int = timed_int(vi, n)

        write(output_unit, '(a,i0,a)') '================ n = ', n, ' values'
        write(output_unit, '(a)') 'real64                                ns/value   vs plain'
        call row('  pf_random_at (draw axis)      ', t_at, t_plain)
        call row('  pf_random_at (stream axis)    ', t_stream_axis, t_plain)
        call row('  pf_random_fill_streams        ', t_fill_streams, t_plain)
        call row('  stream_plain%uniform          ', t_plain, t_plain)
        call row('  stream_cached%uniform         ', t_cached, t_plain)
        call row('  pf_random_fill_draws          ', t_fill, t_plain)
        write(output_unit, '(a)') 'real32                                ns/value   vs plain'
        call row('  pf_random32_at                ', q_at, q_plain)
        call row('  pf_random_fill_streams (r32)  ', q_fill_streams, q_plain)
        call row('  stream_plain%uniform32        ', q_plain, q_plain)
        call row('  stream_cached%uniform32       ', q_cached, q_plain)
        call row('  pf_random_fill_draws          ', q_fill, q_plain)
        write(output_unit, '(a)') 'integer                               ns/value'
        write(output_unit, '(a,f11.3)') '  pf_random_int_at              ', t_int
        write(output_unit, '(a)') ''
        flush(output_unit)

        deallocate(v, ref, v32, ref32, vi)
    end subroutine one_size

    subroutine row(label, t, base)
        character(len=*), intent(in) :: label
        real(real64), intent(in) :: t, base
        write(output_unit, '(a,f11.3,f11.2)') label, t, base / t
    end subroutine row

    ! Each timed_* arm is its own subroutine so that no arm can be hoisted out of a shared loop.

    real(real64) function timed_at(v, n)
        real(real64), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        timed_at = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            do k = 1_int64, n
                v(k) = pf_random_at(SEED, STREAM, k)
            end do
            call tick(t1)
            timed_at = min(timed_at, ns(t0, t1, n))
        end do
        call keep(v)
    end function timed_at

    real(real64) function timed_stream_axis(v, n)
        real(real64), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        timed_stream_axis = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            do k = 1_int64, n
                v(k) = pf_random_at(SEED, k)
            end do
            call tick(t1)
            timed_stream_axis = min(timed_stream_axis, ns(t0, t1, n))
        end do
        call keep(v)
    end function timed_stream_axis

    real(real64) function timed_plain(v, n)
        real(real64), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        type(stream_plain) :: s
        timed_plain = huge(1.0_real64)
        do r = 1, g_rounds
            s = stream_plain(SEED, STREAM, 0_int64)
            call tick(t0)
            do k = 1_int64, n
                call s%uniform(v(k))
            end do
            call tick(t1)
            timed_plain = min(timed_plain, ns(t0, t1, n))
        end do
        call keep(v)
    end function timed_plain

    real(real64) function timed_cached(v, n)
        real(real64), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        type(stream_cached) :: s
        timed_cached = huge(1.0_real64)
        do r = 1, g_rounds
            s = stream_cached(SEED, STREAM, 0_int64, -1_int64, 0_int64, 0_int64, 0_int64, 0_int64)
            call tick(t0)
            do k = 1_int64, n
                call s%uniform(v(k))
            end do
            call tick(t1)
            timed_cached = min(timed_cached, ns(t0, t1, n))
        end do
        call keep(v)
    end function timed_cached

    real(real64) function timed_fill(v, n)
        real(real64), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1
        integer :: r
        timed_fill = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            call pf_random_fill_draws(SEED, STREAM, v)
            call tick(t1)
            timed_fill = min(timed_fill, ns(t0, t1, n))
        end do
        call keep(v)
    end function timed_fill

    !> The shipped stream-AXIS bulk fill, against the scalar loop over streams two rows above.
    !!
    !! **This pair is `feature_random_phase2.md` §11 item 5's live evidence.** That item asks
    !! whether a lane-blocked kernel is worth building; the answer depends on how much room is left
    !! between the scalar loop and the shipped one-stream-per-body form, and Phase 1's figures for
    !! that gap predate every change the module has had since. The two lane-blocked prototypes it
    !! measured no longer exist, so their rows cannot be re-run -- but the baseline they were
    !! measured against can, and a decision resting on a stale baseline is not a decision.
    real(real64) function timed_fill_streams(v, n)
        real(real64), intent(inout) :: v(:)         !! destination, already warmed
        integer(int64), intent(in) :: n             !! element count
        integer(int64) :: t0, t1
        integer :: r
        timed_fill_streams = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            call pf_random_fill_streams(SEED, 1_int64, v)
            call tick(t1)
            timed_fill_streams = min(timed_fill_streams, ns(t0, t1, n))
        end do
        call keep(v)
    end function timed_fill_streams

    !> `timed_fill_streams` for `real32`, the least block-efficient entry point in the module.
    real(real64) function timed_fill_streams32(v, n)
        real(real32), intent(inout) :: v(:)         !! destination, already warmed
        integer(int64), intent(in) :: n             !! element count
        integer(int64) :: t0, t1
        integer :: r
        timed_fill_streams32 = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            call pf_random_fill_streams(SEED, 1_int64, v)
            call tick(t1)
            timed_fill_streams32 = min(timed_fill_streams32, ns(t0, t1, n))
        end do
        call keep32(v)
    end function timed_fill_streams32

    real(real64) function timed_at32(v, n)
        real(real32), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        timed_at32 = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            do k = 1_int64, n
                v(k) = pf_random32_at(SEED, STREAM, k)
            end do
            call tick(t1)
            timed_at32 = min(timed_at32, ns(t0, t1, n))
        end do
        call keep32(v)
    end function timed_at32

    real(real64) function timed_plain32(v, n)
        real(real32), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        type(stream_plain) :: s
        timed_plain32 = huge(1.0_real64)
        do r = 1, g_rounds
            s = stream_plain(SEED, STREAM, 0_int64)
            call tick(t0)
            do k = 1_int64, n
                call s%uniform32(v(k))
            end do
            call tick(t1)
            timed_plain32 = min(timed_plain32, ns(t0, t1, n))
        end do
        call keep32(v)
    end function timed_plain32

    real(real64) function timed_cached32(v, n)
        real(real32), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        type(stream_cached) :: s
        timed_cached32 = huge(1.0_real64)
        do r = 1, g_rounds
            s = stream_cached(SEED, STREAM, 0_int64, -1_int64, 0_int64, 0_int64, 0_int64, 0_int64)
            call tick(t0)
            do k = 1_int64, n
                call s%uniform32(v(k))
            end do
            call tick(t1)
            timed_cached32 = min(timed_cached32, ns(t0, t1, n))
        end do
        call keep32(v)
    end function timed_cached32

    real(real64) function timed_fill32(v, n)
        real(real32), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1
        integer :: r
        timed_fill32 = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            call pf_random_fill_draws(SEED, STREAM, v)
            call tick(t1)
            timed_fill32 = min(timed_fill32, ns(t0, t1, n))
        end do
        call keep32(v)
    end function timed_fill32

    real(real64) function timed_int(v, n)
        integer(int64), intent(inout) :: v(:)
        integer(int64), intent(in) :: n
        integer(int64) :: t0, t1, k
        integer :: r
        timed_int = huge(1.0_real64)
        do r = 1, g_rounds
            call tick(t0)
            do k = 1_int64, n
                v(k) = pf_random_int_at(SEED, STREAM, 1_int64, n, k)
            end do
            call tick(t1)
            timed_int = min(timed_int, ns(t0, t1, n))
        end do
        if (v(1) == -12345_int64) write(output_unit, '(a)') ''
    end function timed_int

    subroutine tick(t)
        integer(int64), intent(out) :: t
        call system_clock(t)
    end subroutine tick

    real(real64) function ns(t0, t1, n)
        integer(int64), intent(in) :: t0, t1, n
        integer(int64) :: rate
        call system_clock(count_rate=rate)
        ns = real(t1 - t0, real64) / real(rate, real64) / real(n, real64) * 1.0e9_real64
    end function ns

    !> Keeps a filled array live without perturbing the timed region.
    subroutine keep(v)
        real(real64), intent(in) :: v(:)
        if (v(1) == -1.0_real64) write(output_unit, '(a)') ''
    end subroutine keep

    subroutine keep32(v)
        real(real32), intent(in) :: v(:)
        if (v(1) == -1.0_real32) write(output_unit, '(a)') ''
    end subroutine keep32

    subroutine same_r64(a, b, n, what)
        real(real64), intent(in) :: a(:), b(:)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: what
        integer(int64) :: k
        do k = 1_int64, n
            if (a(k) /= b(k)) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', what, ' disagrees with pf_random_at at ', k
                error stop 1
            end if
        end do
    end subroutine same_r64

    subroutine same_r32(a, b, n, what)
        real(real32), intent(in) :: a(:), b(:)
        integer(int64), intent(in) :: n
        character(len=*), intent(in) :: what
        integer(int64) :: k
        do k = 1_int64, n
            if (a(k) /= b(k)) then
                write(output_unit, '(a,a,a,i0)') 'FATAL: ', what, ' disagrees with pf_random32_at at ', k
                error stop 1
            end if
        end do
    end subroutine same_r32

end program probe_random_stream
