!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> A strictly overflow-free reference implementation of `parquet_random`'s contract.
!>
!> **This module deliberately shares nothing with the library.** It calls no procedure from
!> `parquet_random`, imports nothing but `iso_fortran_env`, and re-derives every value from the
!> written contract. That independence is the whole point: a known-answer test cannot detect a
!> miscompiled build -- the published Philox vectors once passed 18 of 18 in a build whose
!> generator returned wrong values, because a self-check is small and simple by construction and
!> so never produces the specialised instantiation that goes wrong. Only comparing the library
!> against an independent implementation, over the shapes the library itself uses, has ever caught
!> that class of fault.
!>
!> **Every ARITHMETIC value here runs on 16-bit limbs**, so no intermediate product ever exceeds
!> 2**35. That is not merely a safety measure, it is what makes the reference independent *on the
!> axis the faults live on*: the library's remaining signed-overflow sites, and the signed/unsigned
!> confusion that makes the naive rejection threshold wrong above 2**63, simply have no counterpart
!> in limb arithmetic, where every quantity is a small non-negative integer.
!>
!> **The INDEX arithmetic is a second category, and it is overflow-free for a different reason:
!> nothing here forms a word index by multiplying.** That distinction is not decoration. `ref_bits`
!> once computed `2 * (draw - 1)`, which overflows above draw 2**62, and the consequence was worse
!> than an ordinary bug -- the reference reported the *library* as wrong at high draws while the
!> library was right, which is the one failure mode an independent oracle must never have. Keep any
!> new index derivation in the divide-and-modulo form `ref_bits` now uses.
!>
!> One trap if you try to verify that claim by breaking it on purpose: writing the multiply and the
!> divide in ONE expression, `(2*(d-1))/4`, changes nothing. The compiler folds it straight back to
!> `(d-1)/2` -- it is entitled to, precisely because the overflow is undefined -- so it is a no-op at
!> every optimisation level and no test fails. The form that actually breaks is the original one,
!> where the product is passed as an ARGUMENT to a helper and so has to be materialised. Mutate that
!> shape, not the inline one, or you will conclude the tests are weak when they are not; `test_edges`
!> and `test_cross_form` both fail on the real form.
!>
!> `ref_umod_2p64` is deliberately derived by a DIFFERENT route from the library's -- bitwise long
!> division rather than halve-reduce-double. A reference that reproduces the implementation's own
!> reasoning is worth very little: this project has already been caught three times by a check that
!> compared something with itself or shared the very assumption under test.
module test_random_reference

    use iso_fortran_env, only: int32, int64, real32, real64

    implicit none
    private

    public :: ref_philox_block, ref_bits, ref_at, ref_at32, ref_int_at, ref_key, ref_mix64
    public :: ref_umod_2p64, ref_mulhilo64, ref_width, ref_offset

    ! ---- Per-generic word spaces ----
    !
    ! Written out here from the specification rather than imported from `parquet_random`, which
    ! is the whole point of this file: a reference that borrowed the library's own constants
    ! could only ever confirm that the library agrees with itself. The tag sits in bits 62-63 of the
    ! block index, which no draw index can reach.
    !
    ! `REF_DOM_REAL64` is zero, so every value in that space is what it was before the split.

    !> Word space of `pf_random_at` and `pf_random_bits_at`.
    integer(int64), parameter :: REF_DOM_REAL64 = 0_int64
    !> Word space of `pf_random32_at`.
    integer(int64), parameter :: REF_DOM_REAL32 = ibset(0_int64, 62)
    !> Word space of `pf_random_int_at` at a range of `NARROW32_CAP` values or fewer.
    integer(int64), parameter :: REF_DOM_INT_NARROW = ibset(0_int64, 63)
    !> Word space of `pf_random_int_at` at a wider range.
    integer(int64), parameter :: REF_DOM_INT_WIDE = ibset(ibset(0_int64, 62), 63)

    !> Limbs per 128-bit value. Sixteen bits each, so a product of two limbs is below 2**32.
    integer, parameter :: NL = 8
    !> Low 16 bits set.
    integer(int64), parameter :: M16 = 65535_int64
    !> Low 32 bits set.
    integer(int64), parameter :: M32 = 4294967295_int64

    !> Philox4x32-10 constants, restated here rather than imported: an imported constant would
    !! make the reference agree with the library about the one thing a wrong constant would break.
    integer(int64), parameter :: RM0 = int(z'D2511F53', int64)
    !> Second Philox multiplier.
    integer(int64), parameter :: RM1 = int(z'CD9E8D57', int64)
    !> First Philox key bump.
    integer(int64), parameter :: RW0 = int(z'9E3779B9', int64)
    !> Second Philox key bump.
    integer(int64), parameter :: RW1 = int(z'BB67AE85', int64)
    !> First SplitMix64 multiplier.
    integer(int64), parameter :: RMIX_A = ior(ishft(int(z'BF58476D', int64), 32), int(z'1CE4E5B9', int64))
    !> Second SplitMix64 multiplier.
    integer(int64), parameter :: RMIX_B = ior(ishft(int(z'94D049BB', int64), 32), int(z'133111EB', int64))
    !> The retry key's offset in label space.
    integer(int64), parameter :: RRETRY_TAG = ishft(int(z'5A170000', int64), 32)
    !> The widest range served by the 32-bit candidate grid. Frozen contract, never a setting; see
    !! `NARROW32_CAP` in `src/parquet_random.f90` and in the Python oracle.
    integer(int64), parameter :: RNARROW32_CAP = 16777216_int64

contains

    ! ---- limb primitives -------------------------------------------------------------------

    !> Splits a 64-bit pattern into the low four limbs; the high four are zero.
    pure subroutine lset(x, u)
        integer(int64), intent(in) :: x             !! any 64-bit pattern
        integer(int64), intent(out) :: u(NL)        !! limbs, least significant first
        u = 0_int64
        u(1) = iand(x, M16)
        u(2) = iand(ishft(x, -16), M16)
        u(3) = iand(ishft(x, -32), M16)
        u(4) = iand(ishft(x, -48), M16)
    end subroutine lset

    !> Reassembles the low four limbs into a 64-bit pattern.
    pure function lget(u) result(x)
        integer(int64), intent(in) :: u(NL)         !! limbs, least significant first
        integer(int64) :: x                         !! the 64-bit pattern of limbs 1..4
        x = ior(ior(u(1), ishft(u(2), 16)), ior(ishft(u(3), 32), ishft(u(4), 48)))
    end function lget

    !> `a + b` modulo 2**128.
    pure subroutine ladd(a, b, r)
        integer(int64), intent(in) :: a(NL)         !! left operand
        integer(int64), intent(in) :: b(NL)         !! right operand
        integer(int64), intent(out) :: r(NL)        !! the sum
        integer(int64) :: carry, t
        integer :: i
        carry = 0_int64
        do i = 1, NL
            t = a(i) + b(i) + carry
            r(i) = iand(t, M16)
            carry = ishft(t, -16)
        end do
    end subroutine ladd

    !> `a - b` modulo 2**128.
    pure subroutine lsub(a, b, r)
        integer(int64), intent(in) :: a(NL)         !! left operand
        integer(int64), intent(in) :: b(NL)         !! right operand
        integer(int64), intent(out) :: r(NL)        !! the difference
        integer(int64) :: borrow, t
        integer :: i
        borrow = 0_int64
        do i = 1, NL
            t = a(i) - b(i) - borrow
            if (t < 0_int64) then
                r(i) = t + 65536_int64
                borrow = 1_int64
            else
                r(i) = t
                borrow = 0_int64
            end if
        end do
    end subroutine lsub

    !> Three-way comparison of two limb values: -1, 0 or 1.
    pure function lcmp(a, b) result(c)
        integer(int64), intent(in) :: a(NL)         !! left operand
        integer(int64), intent(in) :: b(NL)         !! right operand
        integer :: c                                !! -1 if `a < b`, 0 if equal, 1 if `a > b`
        integer :: i
        c = 0
        do i = NL, 1, -1
            if (a(i) < b(i)) then
                c = -1
                return
            else if (a(i) > b(i)) then
                c = 1
                return
            end if
        end do
    end function lcmp

    !> `a * 2` modulo 2**128.
    pure subroutine lshl1(a, r)
        integer(int64), intent(in) :: a(NL)         !! the value to double
        integer(int64), intent(out) :: r(NL)        !! twice it
        call ladd(a, a, r)
    end subroutine lshl1

    !> The full product of two 64-bit patterns, as eight limbs (128 bits).
    !!
    !! Schoolbook over sixteen 16x16 partial products. Each is below 2**32 and each accumulator
    !! value below 2**33, so nothing here can overflow on any compiler.
    pure subroutine lmul(a, b, r)
        integer(int64), intent(in) :: a(NL)         !! left operand; only limbs 1..4 are read
        integer(int64), intent(in) :: b(NL)         !! right operand; only limbs 1..4 are read
        integer(int64), intent(out) :: r(NL)        !! the 128-bit product
        integer(int64) :: carry, t
        integer :: i, j, k
        r = 0_int64
        do i = 1, 4
            carry = 0_int64
            do j = 1, 4
                t = r(i + j - 1) + a(i) * b(j) + carry
                r(i + j - 1) = iand(t, M16)
                carry = ishft(t, -16)
            end do
            k = i + 4
            do while (carry /= 0_int64 .and. k <= NL)
                t = r(k) + carry
                r(k) = iand(t, M16)
                carry = ishft(t, -16)
                k = k + 1
            end do
        end do
    end subroutine lmul

    ! ---- the cipher ------------------------------------------------------------------------

    !> The high and low halves of a 32x32 product, on 16-bit limbs.
    pure subroutine ref_mul32(a, b, hi, lo)
        integer(int64), intent(in) :: a             !! left operand, below 2**32
        integer(int64), intent(in) :: b             !! right operand, below 2**32
        integer(int64), intent(out) :: hi           !! bits 32..63 of the product
        integer(int64), intent(out) :: lo           !! bits 0..31 of the product
        integer(int64) :: a0, a1, b0, b1, t, c0, c1, c2, c3
        a0 = iand(a, M16)
        a1 = iand(ishft(a, -16), M16)
        b0 = iand(b, M16)
        b1 = iand(ishft(b, -16), M16)
        t = a0 * b0
        c0 = iand(t, M16)
        t = ishft(t, -16) + a0 * b1 + a1 * b0
        c1 = iand(t, M16)
        t = ishft(t, -16) + a1 * b1
        c2 = iand(t, M16)
        c3 = iand(ishft(t, -16), M16)
        lo = ior(c0, ishft(c1, 16))
        hi = ior(c2, ishft(c3, 16))
    end subroutine ref_mul32

    !> Philox4x32-10, taking and returning the four counter words directly.
    !!
    !! Exposed with raw counter words so the published Random123 known-answer vectors can be
    !! asserted against it: two of the three use counter values no draw index could ever produce,
    !! so they are unreachable through the library's own surface and this is the only place they
    !! can be checked at all.
    pure subroutine ref_philox_block(c0_in, c1_in, c2_in, c3_in, k0_in, k1_in, o0, o1, o2, o3)
        integer(int64), intent(in) :: c0_in         !! counter word 0
        integer(int64), intent(in) :: c1_in         !! counter word 1
        integer(int64), intent(in) :: c2_in         !! counter word 2
        integer(int64), intent(in) :: c3_in         !! counter word 3
        integer(int64), intent(in) :: k0_in         !! key word 0
        integer(int64), intent(in) :: k1_in         !! key word 1
        integer(int64), intent(out) :: o0           !! output word 0
        integer(int64), intent(out) :: o1           !! output word 1
        integer(int64), intent(out) :: o2           !! output word 2
        integer(int64), intent(out) :: o3           !! output word 3
        integer(int64) :: c0, c1, c2, c3, k0, k1, hi0, hi1, lo0, lo1
        integer :: r
        c0 = c0_in
        c1 = c1_in
        c2 = c2_in
        c3 = c3_in
        k0 = k0_in
        k1 = k1_in
        do r = 1, 10
            call ref_mul32(RM0, c0, hi0, lo0)
            call ref_mul32(RM1, c2, hi1, lo1)
            c0 = ieor(ieor(hi1, c1), k0)
            c1 = lo1
            c2 = ieor(ieor(hi0, c3), k1)
            c3 = lo0
            if (r < 10) then                        ! the bump belongs strictly between rounds
                k0 = iand(k0 + RW0, M32)
                k1 = iand(k1 + RW1, M32)
            end if
        end do
        o0 = c0
        o1 = c1
        o2 = c2
        o3 = c3
    end subroutine ref_philox_block

    !> Block `index` of `stream` under `key`, with the key and counter split done here.
    pure subroutine ref_block(key, stream, index, o0, o1, o2, o3)
        integer(int64), intent(in) :: key           !! the 64-bit key
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: index         !! the 0-based block index
        integer(int64), intent(out) :: o0           !! output word 0
        integer(int64), intent(out) :: o1           !! output word 1
        integer(int64), intent(out) :: o2           !! output word 2
        integer(int64), intent(out) :: o3           !! output word 3
        call ref_philox_block(iand(index, M32), iand(ishft(index, -32), M32), &
                              iand(stream, M32), iand(ishft(stream, -32), M32), &
                              iand(key, M32), iand(ishft(key, -32), M32), o0, o1, o2, o3)
    end subroutine ref_block

    !> Word `index` (0-based) of a stream.
    pure function ref_word(seed, stream, index) result(w)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: index         !! the 0-based word index
        integer(int64) :: w                         !! that Philox output word
        integer(int64) :: o(0:3)
        call ref_block(seed, stream, ior(REF_DOM_REAL32, index / 4_int64), o(0), o(1), o(2), o(3))
        w = o(int(modulo(index, 4_int64), int32))
    end function ref_word

    !> The 64-bit pattern of `real64`/raw-bits value `draw` of a stream.
    pure function ref_bits(seed, stream, draw) result(b)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: draw          !! the 1-based value index; clamped here too
        integer(int64) :: b                         !! the 64-bit pattern
        integer(int64) :: d, o(0:3)
        integer(int32) :: slot
        d = max(draw, 1_int64)
        ! Value d occupies words 2(d-1) and 2(d-1)+1 -- but that index is deliberately NOT formed.
        ! It exceeds huge(int64) for d above 2**62, which made this the one place in a module whose
        ! whole claim is to be overflow-free that could actually overflow. It did, silently, and
        ! the symptom pointed the wrong way: the reference reported the LIBRARY as wrong at high
        ! draws, when the library was right and this line was not.
        !
        ! The block and the slot within it each follow from d without multiplying anything: the
        ! block is the word index over 4, which is (d-1)/2, and the slot is the word index modulo
        ! 4, which is 0 when d is odd and 2 when d is even.
        call ref_block(seed, stream, ior(REF_DOM_REAL64, (d - 1_int64) / 2_int64), &
                       o(0), o(1), o(2), o(3))
        slot = 2_int32 * int(modulo(d - 1_int64, 2_int64), int32)
        b = ior(ishft(o(slot + 1), 32), o(slot))
    end function ref_bits

    !> `pf_random_at`'s value.
    pure function ref_at(seed, stream, draw) result(r)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: draw          !! the 1-based value index
        real(real64) :: r                           !! a uniform draw in `[0, 1)`
        r = real(ishft(ref_bits(seed, stream, draw), -11), real64) * 2.0_real64**(-53)
    end function ref_at

    !> `pf_random32_at`'s value.
    pure function ref_at32(seed, stream, draw) result(r)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: draw          !! the 1-based value index
        real(real32) :: r                           !! a uniform draw in `[0, 1)`
        r = real(ishft(ref_word(seed, stream, max(draw, 1_int64) - 1_int64), -8), real32) * 2.0_real32**(-24)
    end function ref_at32

    ! ---- the mixer -------------------------------------------------------------------------

    !> The low 64 bits of a product, via the general limb multiply.
    pure function ref_mul64_lo(a, b) result(r)
        integer(int64), intent(in) :: a             !! one factor
        integer(int64), intent(in) :: b             !! the other factor
        integer(int64) :: r                         !! bits 0..63 of the product
        integer(int64) :: ua(NL), ub(NL), ur(NL)
        call lset(a, ua)
        call lset(b, ub)
        call lmul(ua, ub, ur)
        r = lget(ur)
    end function ref_mul64_lo

    !> The SplitMix64 finaliser.
    pure function ref_mix64(x) result(z)
        integer(int64), intent(in) :: x             !! any 64-bit pattern
        integer(int64) :: z                         !! the mixed pattern
        z = x
        z = ref_mul64_lo(ieor(z, ishft(z, -30)), RMIX_A)
        z = ref_mul64_lo(ieor(z, ishft(z, -27)), RMIX_B)
        z = ieor(z, ishft(z, -31))
    end function ref_mix64

    !> `pf_random_key`'s derivation.
    pure function ref_key(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int64), intent(in) :: label         !! which derived family
        integer(int64) :: r                         !! an independent seed
        r = ref_mix64(ieor(ref_mix64(seed), label))
    end function ref_key

    ! ---- the integer rule ------------------------------------------------------------------

    !> The full 128-bit product of two unsigned 64-bit patterns.
    !!
    !! The library's own `mulhilo64` is its third and last signed-overflow site, carried on both
    !! sides of the route (e) fork. This is the overflow-free twin that would notice a compiler
    !! beginning to exploit it.
    pure subroutine ref_mulhilo64(a, b, low, high)
        integer(int64), intent(in) :: a             !! one factor, read as unsigned
        integer(int64), intent(in) :: b             !! the other factor, read as unsigned
        integer(int64), intent(out) :: low          !! bits 0..63
        integer(int64), intent(out) :: high         !! bits 64..127
        integer(int64) :: ua(NL), ub(NL), ur(NL), hi(NL)
        call lset(a, ua)
        call lset(b, ub)
        call lmul(ua, ub, ur)
        low = lget(ur)
        hi = 0_int64
        hi(1:4) = ur(5:8)
        high = lget(hi)
    end subroutine ref_mulhilo64

    !> `2**64 mod s` for `s` read as unsigned, by bitwise long division.
    !!
    !! A different route from the library's halve-reduce-double, on purpose. Long division has no
    !! signed/unsigned distinction at all -- every limb is a small non-negative integer -- so it
    !! cannot share the confusion that makes the naive spelling wrong for two thirds of the widths
    !! at or above 2**63.
    pure function ref_umod_2p64(s) result(t)
        integer(int64), intent(in) :: s             !! the width, unsigned and non-zero
        integer(int64) :: t                         !! `2**64 mod s`
        integer(int64) :: us(NL), x(NL), two64(NL), rem(NL), tmp(NL)
        integer :: bit, limb, off
        call lset(s, us)
        two64 = 0_int64
        two64(5) = 1_int64                          ! 2**64 exactly, in limbs
        call lsub(two64, us, x)                     ! 2**64 - s, which fits in 64 bits for s >= 1
        rem = 0_int64
        do bit = 63, 0, -1
            call lshl1(rem, tmp)
            rem = tmp
            limb = bit / 16 + 1
            off = modulo(bit, 16)
            if (iand(ishft(x(limb), -off), 1_int64) == 1_int64) rem(1) = rem(1) + 1_int64
            if (lcmp(rem, us) >= 0) then
                call lsub(rem, us, tmp)
                rem = tmp
            end if
        end do
        t = lget(rem)
    end function ref_umod_2p64

    !> `hi - lo + 1` as an unsigned 64-bit pattern; 0 means the full `int64` range.
    !!
    !! Computed with limb subtraction, which borrows explicitly. A dropped borrow in the low limb
    !! is exactly the mutation that once survived an entire sweep because every case had `lo = 0`.
    pure function ref_width(lo, hi) result(s)
        integer(int64), intent(in) :: lo            !! the low end
        integer(int64), intent(in) :: hi            !! the high end
        integer(int64) :: s                         !! the width, read as unsigned
        integer(int64) :: ulo(NL), uhi(NL), d(NL), one(NL), r(NL)
        call lset(lo, ulo)
        call lset(hi, uhi)
        call lsub(uhi, ulo, d)
        call lset(1_int64, one)
        call ladd(d, one, r)
        r(5:NL) = 0_int64                           ! modulo 2**64: the width 2**64 reads back as 0
        s = lget(r)
    end function ref_width

    !> `base + offset` modulo 2**64, with `offset` read as unsigned.
    pure function ref_offset(base, offset) result(r)
        integer(int64), intent(in) :: base          !! the range's low end
        integer(int64), intent(in) :: offset        !! the unsigned offset into the range
        integer(int64) :: r                         !! the value inside the range
        integer(int64) :: ub(NL), uo(NL), sum(NL)
        call lset(base, ub)
        call lset(offset, uo)
        call ladd(ub, uo, sum)
        r = lget(sum)
    end function ref_offset

    !> `pf_random_int_at`'s value, and how many times the rejection loop re-keyed.
    !!
    !! **The candidate is the pair `ref_bits` would return at this coordinate** -- stride 2, block
    !! `(d-1)/2`, pair `mod(d-1,2)` -- because under `pf_random_algorithm` `/v2` the integer generic
    !! sits on the same draw grid as `pf_random_at`. It is spelled out here from the block words
    !! rather than delegated to `ref_bits`, so that this model still says what it means when read
    !! alone, and so that the parity selection is visible in the one place a reader would check it.
    !!
    !! Under `/v1` it read block `d - 1` and always the first pair. The change is one line here, and
    !! the retry loop inherits it: a rejection re-keys and re-reads the SAME coordinate, so it moves
    !! to the same pair of a differently-keyed block. `test/test_random_vectors.f90` carries two rows
    !! whose whole purpose is to pin that -- a retry at draw 2 and one at draw 3.
    pure subroutine ref_int_at(seed, stream, lo, hi, draw, value, retries)
        integer(int64), intent(in) :: seed          !! the seed
        integer(int64), intent(in) :: stream        !! the stream index
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! the 1-based value index
        integer(int64), intent(out) :: value        !! a uniform integer in the closed range
        integer(int64), intent(out) :: retries      !! how many rejections preceded it
        integer(int64) :: a, b, s, d, blk, x, low, high, threshold, key
        integer(int64) :: o0, o1, o2, o3
        integer :: slot
        integer(int64) :: ulow(NL), us(NL), ut(NL)
        logical :: second
        a = min(lo, hi)
        b = max(lo, hi)
        s = ref_width(a, b)
        ! No multiplication, for the reason `ref_bits` records at length: `2 * (draw - 1)` overflows
        ! above draw 2**62 and made this model accuse the library of being wrong at high draws.
        d = max(draw, 1_int64)
        retries = 0_int64
        ! **A narrow range takes the 32-bit grid**: one word rather than a pair, at word index
        ! `d - 1`, so one block serves four draws. It is the same INDEXING `pf_random32_at` uses
        ! but in a different word SPACE, so the two never share a word. Spelled out here rather
        ! than shared with the wide arm below, so that each rule reads as one piece.
        if (s >= 1_int64 .and. s <= RNARROW32_CAP) then
            blk = ior(REF_DOM_INT_NARROW, (d - 1_int64) / 4_int64)
            slot = int(modulo(d - 1_int64, 4_int64), int32)
            threshold = modulo(4294967296_int64, s)     ! `2**32 mod s`; both operands positive
            do
                if (retries == 0_int64) then
                    key = seed
                else
                    key = ref_mix64(ieor(ref_mix64(seed), ieor(retries, RRETRY_TAG)))
                end if
                call ref_block(key, stream, blk, o0, o1, o2, o3)
                select case (slot)
                case (0)
                    x = o0
                case (1)
                    x = o1
                case (2)
                    x = o2
                case default
                    x = o3
                end select
                ! `x < 2**32` and `s <= 2**24`, so `x * s < 2**56`: an ordinary signed multiply,
                ! with no 128-bit product and nothing to reduce on limbs.
                low = x * s
                if (iand(low, 4294967295_int64) >= threshold) exit
                retries = retries + 1_int64
            end do
            value = a + ishft(low, -32)
            return
        end if
        blk = ior(REF_DOM_INT_WIDE, (d - 1_int64) / 2_int64)
        second = (modulo(d - 1_int64, 2_int64) == 1_int64)
        if (s == 0_int64) then
            call ref_block(seed, stream, blk, o0, o1, o2, o3)
            if (second) then
                value = ior(ishft(o3, 32), o2)
            else
                value = ior(ishft(o1, 32), o0)
            end if
            return
        end if
        threshold = ref_umod_2p64(s)
        call lset(s, us)
        call lset(threshold, ut)
        do
            if (retries == 0_int64) then
                key = seed
            else
                key = ref_mix64(ieor(ref_mix64(seed), ieor(retries, RRETRY_TAG)))
            end if
            call ref_block(key, stream, blk, o0, o1, o2, o3)
            if (second) then
                x = ior(ishft(o3, 32), o2)
            else
                x = ior(ishft(o1, 32), o0)
            end if
            call ref_mulhilo64(x, s, low, high)
            call lset(low, ulow)
            if (lcmp(ulow, ut) >= 0) exit           ! accept: unsigned compare, on limbs
            retries = retries + 1_int64
        end do
        value = ref_offset(a, high)
    end subroutine ref_int_at

end module test_random_reference
