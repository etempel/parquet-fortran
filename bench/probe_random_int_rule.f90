!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Stage-0 probe for `feature_random.md` E.4: which rule should `pf_random_int_at` use?
!!
!! Appendix E.4 leaves exactly one permanent-contract item open. `pf_random_int_at` may either
!!
!!   * **option 1** -- the rule pinned in feature_random.md §20: consume one whole 128-bit Philox
!!     block, treat it as a 128-bit uniform, multiply-shift by the range width, residual bias
!!     <= 2**-64, never retry; or
!!   * **option 3** -- Lemire's exact rejection over the block's low 64 bits, with each retry
!!     re-enciphering the SAME counter under a tweaked key so the value stays a pure function of
!!     `(seed, i, draw)` and the draw-to-block map is unchanged. Exactly unbiased.
!!
!! E.4 predicts option 3 is cheaper ("one mulhilo64 where the pinned rule needs two"). That
!! prediction is doubtful for a reason E.4 does not weigh: Q31(i) attributes the pinned rule's cost
!! to `mulhilo64` *not vectorising*, and a rejection loop adds data-dependent control flow, which is
!! the same property D.5 measured at 5.5-9.1x when it blocked vectorisation of an elemental call.
!! Halving a scalar cost inside a function that has stopped vectorising may recover nothing.
!!
!! So this probe measures the two rules **in the shape the feature is built around** -- Appendix B's
!! V2L: a `pure elemental` function called scalar in the caller's own loop with the index varying --
!! and carries a no-rejection control arm that separates "half the multiply work" from "the branch".
!!
!! **Kernel.** The Philox4x32-10 round is `do r = 1, 10` (Appendix B, D1) and the 32x32 product is
!! formed in a 128-bit integer where the compiler has one (Appendix C, route (e) / Q35), so gfortran
!! and ifx each measure their own shipped kernel. The fork is selected from a compiler predefine and
!! then **checked against the actual kind** by a compile-time assertion -- see PF_INT128 below.
!!
!! **Correctness gates, both of which must pass before anything is timed** (Appendix B.5):
!!   1. self-tests -- the three official Random123 `philox4x32 10` vectors; bit-identity between the
!!      shipped kernel and a strictly overflow-free 16-bit-limb kernel over a sweep (this is the
!!      cross-implementation agreement test of the proposed §13.1a, and the only class D.1 showed
!!      can catch a miscompiled build); `umod_2p64` against a slow reference; the Lemire accept and
!!      reject decision against an exact reference; range containment; retry-path exercise; and a
!!      uniformity count over a small range.
!!   2. timed-path verification -- each arm is re-run exactly as the timing loop runs it and what it
!!      leaves in the buffer is checked, because an auto-vectorised clone is separate code from the
!!      scalar path the self-tests exercise.
!!
!! **Build.** Standalone: no fpm, no Arrow, no OpenMP, no library dependency, so it can be compiled
!! directly as well as by `fpm build`.
!!
!!     gfortran -O3 -march=native -cpp bench/probe_random_int_rule.f90 -o probe_gf
!!     ifx      -O3 -xHost       -fpp bench/probe_random_int_rule.f90 -o probe_ifx
!!
!! Do NOT pass `-fwrapv`: with route (e) the gfortran kernel has no signed overflow left, and a
!! default build passing the self-tests is itself part of what this probe reports.
!!
!! This is a manual probe, never run by `fpm test` (CLAUDE.md, "Manual (never-`fpm test`)
!! large-scale/benchmark tools").

! Route (e) fork selection.  cpp cannot evaluate `selected_int_kind(38)`, so the macro must come
! from a compiler predefine -- never from a user-supplied flag, or route (e) inherits route (a)'s
! fatal objection that a flag cannot be imposed on a consumer of a published fpm package.
!
! `-DPF_NO_INT128` forces the int64 wrapping kernel on a compiler that has a 128-bit kind.  It is a
! PROBE switch, for pricing route (e) against the kernel it replaced -- never a shipping mechanism,
! and a build using it needs `-fwrapv` or it computes wrong values (feature_random.md B.2).
#if !defined(PF_NO_INT128)
#  if defined(__GFORTRAN__) || defined(__flang__) || defined(__FLANG)
#    define PF_INT128 1
#  endif
#endif

module pf_probe_int_rule
    use iso_fortran_env, only: int64, real64
    implicit none
    private

    public :: NARM, arm_name, arm_is_int, fill_arm, self_tests, verify_timed_path, set_ranges

    ! ---- Philox4x32-10 constants, confirmed against Random123 philox.h (feature_random.md A.6a) --
    integer(int64), parameter :: M32   = 4294967295_int64
    integer(int64), parameter :: PH_M0 = 3528531795_int64          ! z'D2511F53'
    integer(int64), parameter :: PH_M1 = 3449720151_int64          ! z'CD9E8D57'
    integer(int64), parameter :: PH_W0 = 2654435769_int64          ! z'9E3779B9'
    integer(int64), parameter :: PH_W1 = 3144134277_int64          ! z'BB67AE85'
    integer(int64), parameter :: SGN   = -huge(1_int64) - 1_int64  ! flips the sign bit: unsigned compare
    real(real64),   parameter :: SC53  = 2.0_real64**(-53)

    ! Retry key tweak for option 3.  Cold path (taken with probability ~ range/2**64), so its exact
    ! form has no measurable cost; it is a contract detail, not a performance one.
    integer(int64), parameter :: RT0 = 2246822519_int64            ! z'85EBCA77'
    integer(int64), parameter :: RT1 = 3266489917_int64            ! z'C2B2AE3D'

#ifdef PF_INT128
    integer, parameter :: k128 = selected_int_kind(38)
    ! Compile-time assertion: the predefine above claimed a 128-bit kind exists.  If it does not,
    ! this is a division by zero in a constant expression and the build fails here with a message
    ! naming this line -- rather than silently taking the int64 wrapping path, which is exactly the
    ! failure route (e) exists to prevent.
    integer, parameter :: pf_int128_present = 1 / merge(1, 0, k128 > 0)
#endif

    integer, parameter :: NARM = 18

    ! Ranges used by the timed arms.  10**6 fits in 32 bits (the common case); 4*10**12 does not.
    !
    ! These are RUNTIME variables rather than parameters, and that is load-bearing. Held as
    ! parameters, the compiler specialises `mulhilo64` for the known width -- for a width below
    ! 2**32 its `b1` half is zero and two of the four partial products fold away, which measured
    ! 14.116 ns against 17.796 for the identical code at a wider width. That is a measurement of
    ! constant folding, not of the rule. `set_ranges` takes an argument the compiler cannot know.
    integer(int64) :: LO_N = 0_int64, HI_N = 999999_int64
    integer(int64) :: LO_W = 0_int64, HI_W = 3999999999999_int64

contains

    !> Fixes the two timed ranges from a value the compiler cannot fold. `bias` is 0 in normal use.
    subroutine set_ranges(bias)
        integer, intent(in) :: bias  !! opaque runtime offset; pass `command_argument_count()`
        LO_N = int(bias, int64)
        HI_N = 999999_int64 + int(bias, int64)
        LO_W = int(bias, int64)
        HI_W = 3999999999999_int64 + int(bias, int64)
    end subroutine set_ranges

! =============================================================================================
! Small helpers
! =============================================================================================

    !> Unsigned 64-bit less-than over two's-complement patterns.
    elemental function ult(a, b) result(t)
        integer(int64), intent(in) :: a  !! left operand, an unsigned pattern
        integer(int64), intent(in) :: b  !! right operand, an unsigned pattern
        logical :: t                     !! `.true.` when `a < b` read as unsigned
        t = ieor(a, SGN) < ieor(b, SGN)
    end function ult

    !> Splits a seed into the two 32-bit Philox key words (feature_random.md §20, Q20a).
    pure subroutine key_of(seed, k0, k1)
        integer(int64), intent(in)  :: seed  !! the user's seed, used raw
        integer(int64), intent(out) :: k0    !! low 32 bits
        integer(int64), intent(out) :: k1    !! high 32 bits
        k0 = iand(seed, M32)
        k1 = iand(ishft(seed, -32), M32)
    end subroutine key_of

    !> Builds the four counter words from a stream label and a 0-based block index (Q20b).
    pure subroutine ctr_of(stream, blk, c0, c1, c2, c3)
        integer(int64), intent(in)  :: stream  !! stream label; every int64 value is valid
        integer(int64), intent(in)  :: blk     !! 0-based block index within the stream
        integer(int64), intent(out) :: c0      !! low half of `blk`
        integer(int64), intent(out) :: c1      !! high half of `blk`
        integer(int64), intent(out) :: c2      !! low half of `stream`
        integer(int64), intent(out) :: c3      !! high half of `stream`
        c0 = iand(blk, M32)
        c1 = iand(ishft(blk, -32), M32)
        c2 = iand(stream, M32)
        c3 = iand(ishft(stream, -32), M32)
    end subroutine ctr_of

    !> Joins two 32-bit output words into one 64-bit pattern (first word = low half, Q20c).
    pure function u64_of(wlo, whi) result(b)
        integer(int64), intent(in) :: wlo  !! the earlier output word
        integer(int64), intent(in) :: whi  !! the later output word
        integer(int64) :: b                !! the joined 64-bit pattern
        b = ior(ishft(whi, 32), wlo)
    end function u64_of

    !> Maps 64 raw bits to a `real64` in [0,1) by taking the top 53 (Q20c).
    pure function to_r64(b) result(u)
        integer(int64), intent(in) :: b  !! 64 raw bits
        real(real64) :: u                !! uniform in [0,1)
        u = real(ishft(b, -11), real64) * SC53
    end function to_r64

    !> Full 64x64 -> 128 product with no 128-bit type: four partial products and a carry chain.
    pure subroutine mulhilo64(a, b, hi, lo)
        integer(int64), intent(in)  :: a   !! left operand, unsigned pattern
        integer(int64), intent(in)  :: b   !! right operand, unsigned pattern
        integer(int64), intent(out) :: hi  !! high 64 bits of the product
        integer(int64), intent(out) :: lo  !! low 64 bits of the product
        integer(int64) :: a0, a1, b0, b1, t, w0, w1, w2, c
        a0 = iand(a, M32); a1 = ishft(a, -32)
        b0 = iand(b, M32); b1 = ishft(b, -32)
        t  = a0 * b0;      w0 = iand(t, M32);  c  = ishft(t, -32)
        t  = a1 * b0 + c;  w1 = iand(t, M32);  w2 = ishft(t, -32)
        t  = a0 * b1 + w1; c  = ishft(t, -32)
        lo = ior(ishft(t, 32), w0)
        hi = a1 * b1 + w2 + c
    end subroutine mulhilo64

    !> `2**64 mod s` for every width `s` read as UNSIGNED -- Lemire's rejection threshold. Cold path.
    !!
    !! Two regimes, and the first one is why this function is not just the doubling loop below.
    !!
    !! **`s >= 2**63`** (`s` negative as a signed `integer(int64)`). A range that wide is reachable
    !! from the public API -- `pf_random_int_at(seed, i, -huge(int64), huge(int64))` has width
    !! `2**64 - 1` -- and every signed operation below is then meaningless: `mod(h, s)` reduces
    !! modulo `|s|`, and the doubling comparison reads a negative `s` as small. Measured: the
    !! doubling form answers wrongly for 67% of the widths in this regime, by up to a factor of two,
    !! and an over-large threshold rejects candidates that are the *sole* preimage of a range value,
    !! so that value can then never be returned at all -- up to 50% of the requested range at
    !! `s ~ (2/3)*2**64`, and `hi` itself at `s = 2**64 - 1`. It is the exact opposite of the
    !! exactness option 3 is adopted for, and it is silent. Handled first, and directly: for these
    !! widths `2**64 - s <= 2**63 <= s`, so the remainder *is* `2**64 - s`, with no reduction to do.
    !!
    !! **`s < 2**63`.** The doubling form, with every intermediate held inside `[0, s)`, so nothing
    !! can overflow and no unsigned reasoning is needed. That is deliberate rather than tidy: the
    !! obvious spelling -- subtract `s` from the unsigned pattern `2**64 - s` until it fits -- is a
    !! signed overflow, and gfortran 14.2.1 at `-O3` without `-fwrapv` turns it into an infinite
    !! loop. It is the same class of fault as feature_random.md B.2, met while writing this probe.
    pure function umod_2p64(s) result(t)
        integer(int64), intent(in) :: s  !! range width read as unsigned, `1 <= s <= 2**64 - 1`
        integer(int64) :: t              !! `2**64 mod s`, in `[0, s)` read as unsigned
        integer(int64) :: a, h, r
        if (s < 0_int64) then
            ! s >= 2**63 unsigned.  `-s` is the pattern for 2**64 - s and is representable for every
            ! such s except s == 2**63 exactly, where negation would overflow -- and where the
            ! answer is 0 anyway, since 2**63 divides 2**64.  So that value is taken out first.
            if (s == -huge(1_int64) - 1_int64) then
                t = 0_int64
            else
                t = -s
            end if
            return
        end if
        a = -s                              ! the two's-complement pattern for 2**64 - s
        h = ishft(a, -1)                    ! floor((2**64 - s) / 2); logical shift, so >= 0
        r = mod(h, s)                       ! in [0, s)
        if (r >= s - r) then                ! double modulo s without ever leaving [0, s)
            r = r - (s - r)
        else
            r = r + r
        end if
        if (iand(a, 1_int64) == 1_int64) then
            r = r + 1_int64
            if (r == s) r = 0_int64
        end if
        t = r
    end function umod_2p64

! =============================================================================================
! Block kernels
! =============================================================================================

    !> Philox4x32-10, rounds as `do r = 1, 10` (D1), product in int128 where available (route (e)).
    pure subroutine blk10(k0in, k1in, c0in, c1in, c2in, c3in, o0, o1, o2, o3)
        integer(int64), intent(in)  :: k0in  !! key word 0
        integer(int64), intent(in)  :: k1in  !! key word 1
        integer(int64), intent(in)  :: c0in  !! counter word 0
        integer(int64), intent(in)  :: c1in  !! counter word 1
        integer(int64), intent(in)  :: c2in  !! counter word 2
        integer(int64), intent(in)  :: c3in  !! counter word 3
        integer(int64), intent(out) :: o0    !! output word 0
        integer(int64), intent(out) :: o1    !! output word 1
        integer(int64), intent(out) :: o2    !! output word 2
        integer(int64), intent(out) :: o3    !! output word 3
        integer(int64) :: k0, k1, c0, c1, c2, c3, t0, t2
        integer :: r
#ifdef PF_INT128
        integer(k128) :: p0, p1
#else
        integer(int64) :: p0, p1
#endif
        k0 = k0in; k1 = k1in
        c0 = c0in; c1 = c1in; c2 = c2in; c3 = c3in
        do r = 1, 10
#ifdef PF_INT128
            p0 = int(PH_M0, k128) * int(c0, k128)
            p1 = int(PH_M1, k128) * int(c2, k128)
            t0 = ieor(ieor(int(ishft(p1, -32), int64), c1), k0)
            t2 = ieor(ieor(int(ishft(p0, -32), int64), c3), k1)
            c1 = int(iand(p1, int(M32, k128)), int64)
            c3 = int(iand(p0, int(M32, k128)), int64)
#else
            p0 = PH_M0 * c0
            p1 = PH_M1 * c2
            t0 = ieor(ieor(ishft(p1, -32), c1), k0)
            t2 = ieor(ieor(ishft(p0, -32), c3), k1)
            c1 = iand(p1, M32)
            c3 = iand(p0, M32)
#endif
            c0 = t0
            c2 = t2
            if (r < 10) then
                k0 = iand(k0 + PH_W0, M32)
                k1 = iand(k1 + PH_W1, M32)
            end if
        end do
        o0 = c0; o1 = c1; o2 = c2; o3 = c3
    end subroutine blk10

    !> The same cipher on a strictly overflow-free multiply (16-bit limbs, feature_random.md A.7).
    !!
    !! This is the **independent** implementation for the cross-implementation agreement check of
    !! the proposed §13.1a. Nothing in it can overflow -- every intermediate stays below `2**48` --
    !! so it is correct by construction on any compiler and under any optimisation level, which is
    !! what makes a disagreement with `blk10` diagnostic rather than ambiguous. It differs from
    !! `blk10` on exactly the axis the B.2 fault lives on: the multiply.
    pure subroutine blk10_strict(k0in, k1in, c0in, c1in, c2in, c3in, o0, o1, o2, o3)
        integer(int64), intent(in)  :: k0in  !! key word 0
        integer(int64), intent(in)  :: k1in  !! key word 1
        integer(int64), intent(in)  :: c0in  !! counter word 0
        integer(int64), intent(in)  :: c1in  !! counter word 1
        integer(int64), intent(in)  :: c2in  !! counter word 2
        integer(int64), intent(in)  :: c3in  !! counter word 3
        integer(int64), intent(out) :: o0    !! output word 0
        integer(int64), intent(out) :: o1    !! output word 1
        integer(int64), intent(out) :: o2    !! output word 2
        integer(int64), intent(out) :: o3    !! output word 3
        integer(int64) :: k0, k1, c0, c1, c2, c3, t0, t2, h0, l0, h1, l1
        integer :: r
        k0 = k0in; k1 = k1in
        c0 = c0in; c1 = c1in; c2 = c2in; c3 = c3in
        do r = 1, 10
            call mul32_strict(PH_M0, c0, h0, l0)
            call mul32_strict(PH_M1, c2, h1, l1)
            t0 = ieor(ieor(h1, c1), k0)
            t2 = ieor(ieor(h0, c3), k1)
            c1 = l1
            c3 = l0
            c0 = t0
            c2 = t2
            if (r < 10) then
                k0 = iand(k0 + PH_W0, M32)
                k1 = iand(k1 + PH_W1, M32)
            end if
        end do
        o0 = c0; o1 = c1; o2 = c2; o3 = c3
    end subroutine blk10_strict

    !> 32x32 -> 64 with nothing exceeding `2**48`; no reliance on two's-complement wrapping.
    pure subroutine mul32_strict(a, b, hi, lo)
        integer(int64), intent(in)  :: a   !! left operand, below `2**32`
        integer(int64), intent(in)  :: b   !! right operand, below `2**32`
        integer(int64), intent(out) :: hi  !! high 32 bits
        integer(int64), intent(out) :: lo  !! low 32 bits
        integer(int64) :: p0, p1, t
        p0 = a * iand(b, 65535_int64)
        p1 = a * ishft(b, -16)
        t  = p0 + ishft(iand(p1, 65535_int64), 16)
        lo = iand(t, M32)
        hi = ishft(p1, -16) + ishft(t, -32)
    end subroutine mul32_strict

    !> Full 64x64 -> 128 on 16-bit limbs: nothing exceeds `2**36`, so it cannot overflow.
    !!
    !! This is the independent reference for `mulhilo64`, and it exists because `mulhilo64` is a
    !! THIRD undefined-behaviour site that §20.3's inventory does not list. Its four partial
    !! products are 32x32 in `integer(int64)` -- `a0*b0` reaches `(2**32-1)**2`, which exceeds
    !! `huge(int64)` -- so it depends on two's-complement wrapping exactly as SplitMix64's
    !! multiplies do (Gap B), and route (e) does NOT widen it: route (e) widens the Philox round's
    !! multiply inside `blk10`, and `mulhilo64` sits outside it, in the range reduction, on both
    !! sides of the fork. Nothing in the probe compared it against an overflow-free form before
    !! this: `reject_ref` calls `mulhilo64` itself, and gate 2's `ref_value` re-runs `int3_gen`,
    !! so a wrong high half would be compared only against itself.
    pure subroutine mulhilo64_strict(a, b, hi, lo)
        integer(int64), intent(in)  :: a   !! left operand, unsigned pattern
        integer(int64), intent(in)  :: b   !! right operand, unsigned pattern
        integer(int64), intent(out) :: hi  !! high 64 bits of the product
        integer(int64), intent(out) :: lo  !! low 64 bits of the product
        integer(int64) :: al(0:3), bl(0:3), col(0:7), carry, t, w(0:7)
        integer :: i, j
        do i = 0, 3
            al(i) = iand(ishft(a, -16 * i), 65535_int64)
            bl(i) = iand(ishft(b, -16 * i), 65535_int64)
        end do
        col = 0_int64
        do i = 0, 3
            do j = 0, 3
                col(i + j) = col(i + j) + al(i) * bl(j)     ! each term < 2**32, each column < 2**34
            end do
        end do
        carry = 0_int64
        do i = 0, 7
            t = col(i) + carry
            w(i) = iand(t, 65535_int64)
            carry = ishft(t, -16)
        end do
        lo = 0_int64
        hi = 0_int64
        do i = 0, 3
            lo = ior(lo, ishft(w(i), 16 * i))
            hi = ior(hi, ishft(w(i + 4), 16 * i))
        end do
    end subroutine mulhilo64_strict

    !> `(a + b) mod 2**64` on 32-bit limbs; no intermediate exceeds `2**33`.
    pure function uadd64_strict(a, b) result(r)
        integer(int64), intent(in) :: a  !! left operand, an unsigned pattern
        integer(int64), intent(in) :: b  !! right operand, an unsigned pattern
        integer(int64) :: r              !! the 64-bit pattern of the unsigned sum
        integer(int64) :: t0, t1
        t0 = iand(a, M32) + iand(b, M32)
        t1 = iand(ishft(a, -32), M32) + iand(ishft(b, -32), M32) + ishft(t0, -32)
        r  = ior(ishft(iand(t1, M32), 32), iand(t0, M32))
    end function uadd64_strict

    !> `(b - a) mod 2**64` on 32-bit limbs; no intermediate leaves `[0, 2**33)`.
    pure function usub64_strict(b, a) result(r)
        integer(int64), intent(in) :: b  !! minuend, an unsigned pattern
        integer(int64), intent(in) :: a  !! subtrahend, an unsigned pattern
        integer(int64) :: r              !! the 64-bit pattern of the unsigned difference
        integer(int64) :: t0, t1, brw
        t0  = iand(b, M32) - iand(a, M32) + 4294967296_int64
        brw = 1_int64 - ishft(t0, -32)
        t1  = iand(ishft(b, -32), M32) - iand(ishft(a, -32), M32) - brw + 4294967296_int64
        r   = ior(ishft(iand(t1, M32), 32), iand(t0, M32))
    end function usub64_strict

! =============================================================================================
! Tier-0 draws -- the arms
! =============================================================================================

    !> Uniform `real64` in [0,1): the denominator every integer arm is reported against.
    elemental function at_uniform(seed, i) result(u)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        real(real64) :: u                   !! uniform draw in [0,1)
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        u = to_r64(u64_of(o0, o1))
    end function at_uniform

    !> 64 raw bits: the same block, without the final transform.
    elemental function at_bits(seed, i) result(b)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64) :: b                 !! 64 raw bits
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        b = u64_of(o0, o1)
    end function at_bits

    !> OPTION 1, general path: one block as a 128-bit uniform, multiply-shift by the range width.
    !!
    !! Two `mulhilo64` calls, i.e. eight 32x32 products. Bias <= `2**-64`. This is the rule pinned
    !! in feature_random.md §20 (Q22a) and is what ships if E.4 keeps option 1.
    elemental function int1_gen(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: xlo, xhi, s, hl, ll, hh, lh, t, carry, a, b
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        xlo = u64_of(o0, o1); xhi = u64_of(o2, o3)
        if (s == 0_int64) then
            k = xlo
            return
        end if
        call mulhilo64(xlo, s, hl, ll)
        call mulhilo64(xhi, s, hh, lh)
        t = lh + hl
        carry = 0_int64
        if (ult(t, lh)) carry = 1_int64
        k = a + hh + carry
    end function int1_gen

    !> OPTION 1, 32-bit-range fast path: the same value in four 32x32 products instead of eight.
    elemental function int1_fast(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: s, p0, p1, p2, p3, e1, e2, e3, t, a, b
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        if (s == 0_int64 .or. s > M32) then
            k = int1_gen(seed, i, lo, hi)
            return
        end if
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        p0 = o0 * s; p1 = o1 * s; p2 = o2 * s; p3 = o3 * s
        e1 = ishft(ishft(p0, -32) + iand(p1, M32),      -32)
        e2 = ishft(ishft(p1, -32) + iand(p2, M32) + e1, -32)
        e3 = ishft(ishft(p2, -32) + iand(p3, M32) + e2, -32)
        k  = a + ishft(p3, -32) + e3
    end function int1_fast

    !> OPTION 3, general path: Lemire's exact rejection over the block's low 64 bits.
    !!
    !! One `mulhilo64` per attempt, i.e. four 32x32 products. Retry re-enciphers the SAME counter
    !! under a tweaked key, so the draw-to-block map and prefix consistency are unchanged and the
    !! result stays a pure function of `(seed, i, draw)`. Exactly unbiased.
    elemental function int3_gen(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b, n
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64(x, s, h, l)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                x = u64_of(o0, o1)
                call mulhilo64(x, s, h, l)
            end do
        end if
        k = a + h
    end function int3_gen

    !> OPTION 3 with the Q2 option (iii) range arithmetic: the width and the offset computed
    !! OVERFLOW-FREE in `int128` instead of relying on two's-complement wrapping.
    !!
    !! feature_random_stage0.md §20.3 established that `s = hi - lo + 1` and `k = lo + high64(x*s)`
    !! both depend on wrapping once the requested width reaches `2**63`, under BOTH candidate rules,
    !! and that route (e) does not fix them -- it widens the Philox multiply, not the range
    !! arithmetic. §26's Q2 chose option (iii), "remove it under route (e)", on the strength of
    !! "`int128` should be far cheaper" than the 32-bit-halves form's measured **+12.8%** -- and
    !! records that **the option actually chosen has never been priced**. This arm prices it.
    !!
    !! The two changes, and why neither can overflow:
    !!
    !!   * the width is formed as `int128`, where `hi - lo + 1` is exact for every `int64` pair
    !!     (it reaches `2**64`, which `int128` holds with 63 bits to spare), and is then converted
    !!     to the `int64` bit pattern the rest of the rule reads as unsigned. The conversion is
    !!     branched rather than masked because subtracting `2**64` is only representable on the
    !!     wide side; `s128 == 2**64` lands on `s == 0`, which is step 2's full-range case.
    !!   * the offset adds `a` to the *unsigned* value of `high64(x*s)` in `int128` and narrows
    !!     once. `iand(..., 2**64 - 1)` recovers the unsigned value from a sign-extended pattern
    !!     with no branch, and the sum is in `[a, b]` by construction, so the narrowing is exact.
    !!
    !! Where there is no 128-bit kind -- ifx, permanently -- option (iii) IS the shipped wrapping
    !! form (Q2 keeps option (i) there as the documented fallback), so this arm compiles to a
    !! duplicate of `int3_gen` and measures that build's own arm-to-arm floor instead. That is
    !! deliberate: a duplicate arm is a useful control, and it must not be read as a price.
    elemental function int3_gen_of(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b, n
#ifdef PF_INT128
        integer(k128) :: s128
#endif
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
#ifdef PF_INT128
        s128 = int(b, k128) - int(a, k128) + 1_k128
        if (s128 >= ishft(1_k128, 63)) then
            s = int(s128 - ishft(1_k128, 64), int64)
        else
            s = int(s128, int64)
        end if
#else
        s = b - a + 1_int64
#endif
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64(x, s, h, l)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                x = u64_of(o0, o1)
                call mulhilo64(x, s, h, l)
            end do
        end if
#ifdef PF_INT128
        k = int(int(a, k128) + iand(int(h, k128), ishft(1_k128, 64) - 1_k128), int64)
#else
        k = a + h
#endif
    end function int3_gen_of

    !> Option 3 with the width and offset formed on 32-bit LIMBS -- an `int128`-FREE Q2 reference.
    !!
    !! `int3_gen_of` above answers Q2 option (iii) by widening into `int(k128)`, so on a compiler
    !! with no 128-bit kind it compiles to the shipped wrapping form and the self-test that compares
    !! the two becomes a function compared with ITSELF. That is silently vacuous exactly on the one
    !! compiler [§27.1](#271-machine-b)'s task **B11** is about -- ifx -- so the probe as it stood
    !! could not answer B11 at all: a quiet run there meant nothing.
    !!
    !! This function closes that. It computes `s = (hi - lo) + 1` and `k = lo + high64(x*s)` with
    !! `usub64_strict`/`uadd64_strict`, whose intermediates never leave `[0, 2**33)`, so it is
    !! correct by construction on ANY compiler with no 128-bit kind required. Everything else --
    !! including `mulhilo64` -- is `int3_gen`'s code verbatim, so a disagreement isolates the range
    !! arithmetic and nothing else.
    !!
    !! Where `k128` exists it is cross-checked against `int3_gen_of`, which machine C and machine A
    !! already validated against an arbitrary-precision oracle (`8560708566805553027` for
    !! `lo = 0, hi = huge(int64), seed 12345, i = 1`). That chain is what makes its verdict on a
    !! compiler without `int128` trustworthy.
    elemental function int3_gen_sof(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b, n
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = uadd64_strict(usub64_strict(b, a), 1_int64)
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64(x, s, h, l)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                x = u64_of(o0, o1)
                call mulhilo64(x, s, h, l)
            end do
        end if
        k = uadd64_strict(a, h)
    end function int3_gen_sof

    !> Option 3 with the 128-bit product on 16-bit LIMBS -- prices the THIRD undefined-behaviour
    !! site on its own.
    !!
    !! `mulhilo64`'s four 32x32 partial products are formed in `integer(int64)` and `a0*b0` alone
    !! reaches `(2**32-1)**2 > huge(int64)`, so the shipped integer path depends on two's-complement
    !! wrapping there -- on BOTH sides of route (e)'s fork, since route (e) widens the Philox round
    !! inside `blk10` and `mulhilo64` sits outside it. This arm is `int3_gen` with that one call
    !! replaced, so the difference is the price of removing that site and nothing else. The values
    !! are identical by construction: a full 64x64 product is exact either way.
    elemental function int3_gen_sm(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b, n
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64_strict(x, s, h, l)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                x = u64_of(o0, o1)
                call mulhilo64_strict(x, s, h, l)
            end do
        end if
        k = a + h
    end function int3_gen_sm

    !> Option 3 with EVERY undefined-behaviour site in the integer path removed, and no `int128`.
    !!
    !! `mulhilo64_strict` plus `usub64_strict`/`uadd64_strict`: the product, the width and the offset
    !! are all on limbs, nothing exceeds `2**36`, and no 128-bit kind is required. This is the arm
    !! that prices "one strict integer path everywhere, no `cpp` fork, no UB inventory" against the
    !! shipped rule -- the option that would close Q2, Q3's sibling question and the `mulhilo64` site
    !! together. It must agree with `int3_gen_sof` value for value on every width.
    elemental function int3_gen_full(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b, n
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = uadd64_strict(usub64_strict(b, a), 1_int64)
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64_strict(x, s, h, l)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                x = u64_of(o0, o1)
                call mulhilo64_strict(x, s, h, l)
            end do
        end if
        k = uadd64_strict(a, h)
    end function int3_gen_full

    !> OPTION 3, 32-bit-range fast path: the full product in two 32x32 products instead of four.
    elemental function int3_fast(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! uniform integer in [lo,hi]
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: s, h, l, t, a, b, n, pl, ph, mid
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        if (s == 0_int64 .or. s > M32) then
            k = int3_gen(seed, i, lo, hi)
            return
        end if
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        pl = o0 * s; ph = o1 * s
        mid = ishft(pl, -32) + iand(ph, M32)
        l   = ior(ishft(iand(mid, M32), 32), iand(pl, M32))
        h   = ishft(ph, -32) + ishft(mid, -32)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                pl = o0 * s; ph = o1 * s
                mid = ishft(pl, -32) + iand(ph, M32)
                l   = ior(ishft(iand(mid, M32), 32), iand(pl, M32))
                h   = ishft(ph, -32) + ishft(mid, -32)
            end do
        end if
        k = a + h
    end function int3_fast

    !> ATTRIBUTION CONTROL: option 3's general path with the rejection branch deleted.
    !!
    !! Biased (<= `range/2**64`) and therefore never shippable -- its only job is to separate "half
    !! the multiply work" from "the cost of the data-dependent branch". `int3_gen - int3_gen_nr` is
    !! the branch; `int3_gen_nr - int1_gen` is the halved multiply.
    elemental function int3_gen_nr(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! biased integer in [lo,hi]; control arm only
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64(x, s, h, l)
        k = a + h
    end function int3_gen_nr

    !> ATTRIBUTION CONTROL: option 3's 32-bit fast path with the rejection branch deleted.
    elemental function int3_fast_nr(seed, i, lo, hi) result(k)
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64), intent(in) :: lo    !! inclusive lower bound
        integer(int64), intent(in) :: hi    !! inclusive upper bound
        integer(int64) :: k                 !! biased integer in [lo,hi]; control arm only
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: s, h, t, a, b, pl, ph, mid
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        if (s == 0_int64 .or. s > M32) then
            k = int3_gen_nr(seed, i, lo, hi)
            return
        end if
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        pl = o0 * s; ph = o1 * s
        mid = ishft(pl, -32) + iand(ph, M32)
        h   = ishft(ph, -32) + ishft(mid, -32)
        k   = a + h
    end function int3_fast_nr

    !> Diagnostic twin of `int3_gen` that also reports how many retries the draw needed.
    pure subroutine int3_gen_diag(seed, i, lo, hi, k, nret)
        integer(int64), intent(in)  :: seed  !! the seed
        integer(int64), intent(in)  :: i     !! the stream label
        integer(int64), intent(in)  :: lo    !! inclusive lower bound
        integer(int64), intent(in)  :: hi    !! inclusive upper bound
        integer(int64), intent(out) :: k     !! uniform integer in [lo,hi]
        integer,        intent(out) :: nret  !! number of rejected attempts
        integer(int64) :: k0, k1, c0, c1, c2, c3, o0, o1, o2, o3
        integer(int64) :: x, s, h, l, t, a, b, n
        nret = 0
        a = lo; b = hi
        if (a > b) then
            t = a; a = b; b = t
        end if
        s = b - a + 1_int64
        call key_of(seed, k0, k1)
        call ctr_of(i, 0_int64, c0, c1, c2, c3)
        call blk10(k0, k1, c0, c1, c2, c3, o0, o1, o2, o3)
        x = u64_of(o0, o1)
        if (s == 0_int64) then
            k = x
            return
        end if
        call mulhilo64(x, s, h, l)
        if (ult(l, s)) then
            t = umod_2p64(s)
            n = 0_int64
            do while (ult(l, t))
                n = n + 1_int64
                nret = int(n)
                call blk10(iand(k0 + n * RT0, M32), iand(k1 + n * RT1, M32), &
                           c0, c1, c2, c3, o0, o1, o2, o3)
                x = u64_of(o0, o1)
                call mulhilo64(x, s, h, l)
            end do
        end if
        k = a + h
    end subroutine int3_gen_diag

! =============================================================================================
! Arm dispatch -- the `select case` is OUTSIDE every inner loop (A.5's fairness rule)
! =============================================================================================

    !> Human-readable name of one arm.
    pure function arm_name(a) result(nm)
        integer, intent(in) :: a       !! arm index, 1..NARM
        character(len=40) :: nm        !! the arm's label
        select case (a)
        case (1);  nm = "U     uniform real64 (denominator)"
        case (2);  nm = "BITS  raw 64 bits"
        case (3);  nm = "P1G   opt1 general,   range 1e6"
        case (4);  nm = "P1F   opt1 fast path, range 1e6"
        case (5);  nm = "P1GW  opt1 general,   range 4e12"
        case (6);  nm = "L3G   opt3 Lemire gen,   range 1e6"
        case (7);  nm = "L3F   opt3 Lemire fast,  range 1e6"
        case (8);  nm = "L3GW  opt3 Lemire gen,   range 4e12"
        case (9);  nm = "L3Gnr opt3 gen,  NO reject (control)"
        case (10); nm = "L3Fnr opt3 fast, NO reject (control)"
        case (11); nm = "L3Go  opt3 gen, Q2(iii), range 1e6"
        case (12); nm = "L3GWo opt3 gen, Q2(iii), range 4e12"
        case (13); nm = "L3Gs  opt3 gen, strict limbs, 1e6"
        case (14); nm = "L3GWs opt3 gen, strict limbs, 4e12"
        case (15); nm = "L3Gm  opt3 gen, strict mulhi, 1e6"
        case (16); nm = "L3GWm opt3 gen, strict mulhi, 4e12"
        case (17); nm = "L3Gf  opt3 gen, ALL strict,   1e6"
        case (18); nm = "L3GWf opt3 gen, ALL strict,   4e12"
        case default; nm = "?"
        end select
    end function arm_name

    !> `.true.` when the arm writes the integer destination rather than the real one.
    pure function arm_is_int(a) result(t)
        integer, intent(in) :: a  !! arm index, 1..NARM
        logical :: t              !! `.true.` for every arm except the uniform draw
        t = (a /= 1)
    end function arm_is_int

    !> Runs one arm over `n` indices starting at `base`, writing into whichever buffer it owns.
    subroutine fill_arm(a, seed, base, n, du, dk)
        integer,        intent(in)    :: a        !! arm index, 1..NARM
        integer(int64), intent(in)    :: seed     !! the seed
        integer(int64), intent(in)    :: base     !! first stream label minus one
        integer,        intent(in)    :: n        !! how many values to produce
        real(real64),   intent(inout) :: du(:)    !! destination for the uniform arm
        integer(int64), intent(inout) :: dk(:)    !! destination for every integer arm
        integer :: j
        select case (a)
        case (1)
            do j = 1, n
                du(j) = at_uniform(seed, base + int(j, int64))
            end do
        case (2)
            do j = 1, n
                dk(j) = at_bits(seed, base + int(j, int64))
            end do
        case (3)
            do j = 1, n
                dk(j) = int1_gen(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (4)
            do j = 1, n
                dk(j) = int1_fast(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (5)
            do j = 1, n
                dk(j) = int1_gen(seed, base + int(j, int64), LO_W, HI_W)
            end do
        case (6)
            do j = 1, n
                dk(j) = int3_gen(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (7)
            do j = 1, n
                dk(j) = int3_fast(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (8)
            do j = 1, n
                dk(j) = int3_gen(seed, base + int(j, int64), LO_W, HI_W)
            end do
        case (9)
            do j = 1, n
                dk(j) = int3_gen_nr(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (10)
            do j = 1, n
                dk(j) = int3_fast_nr(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (11)
            do j = 1, n
                dk(j) = int3_gen_of(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (12)
            do j = 1, n
                dk(j) = int3_gen_of(seed, base + int(j, int64), LO_W, HI_W)
            end do
        case (13)
            do j = 1, n
                dk(j) = int3_gen_sof(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (14)
            do j = 1, n
                dk(j) = int3_gen_sof(seed, base + int(j, int64), LO_W, HI_W)
            end do
        case (15)
            do j = 1, n
                dk(j) = int3_gen_sm(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (16)
            do j = 1, n
                dk(j) = int3_gen_sm(seed, base + int(j, int64), LO_W, HI_W)
            end do
        case (17)
            do j = 1, n
                dk(j) = int3_gen_full(seed, base + int(j, int64), LO_N, HI_N)
            end do
        case (18)
            do j = 1, n
                dk(j) = int3_gen_full(seed, base + int(j, int64), LO_W, HI_W)
            end do
        end select
    end subroutine fill_arm

! =============================================================================================
! Gate 1 -- self-tests
! =============================================================================================

    !> Runs every self-test; returns the number of failures and prints each result.
    subroutine self_tests(nfail)
        integer, intent(out) :: nfail  !! number of failed checks
        integer(int64) :: o(4), e(4), c(4), k(2)
        integer(int64) :: a0, a1, a2, a3, b0, b1, b2, b3, kk0, kk1, cc0, cc1, cc2, cc3
        integer(int64) :: s, t, tref, x, h, l, kv, seedv, cnt(0:6), lov, hiv, hs, ls, kw
        integer :: i, j, d, nret, nmax, nhit, nwide
        logical :: ok
        real(real64) :: chi, expct

        nfail = 0
        write (*, "(a)") "--- gate 1: self-tests ---"

        ! (1) The three official Random123 philox4x32 10 vectors (feature_random.md A.6a).
        c = [0_int64, 0_int64, 0_int64, 0_int64]
        k = [0_int64, 0_int64]
        e = [int(z"6627e8d5", int64), int(z"e169c58d", int64), &
             int(z"bc57ac4c", int64), int(z"9b00dbd8", int64)]
        call blk10(k(1), k(2), c(1), c(2), c(3), c(4), o(1), o(2), o(3), o(4))
        call report("KAT 1 (all zero)", all(o == e), nfail)

        c = [M32, M32, M32, M32]
        k = [M32, M32]
        e = [int(z"408f276d", int64), int(z"41c83b0e", int64), &
             int(z"a20bc7c6", int64), int(z"6d5451fd", int64)]
        call blk10(k(1), k(2), c(1), c(2), c(3), c(4), o(1), o(2), o(3), o(4))
        call report("KAT 2 (all ones)", all(o == e), nfail)

        c = [int(z"243f6a88", int64), int(z"85a308d3", int64), &
             int(z"13198a2e", int64), int(z"03707344", int64)]
        k = [int(z"a4093822", int64), int(z"299f31d0", int64)]
        e = [int(z"d16cfe09", int64), int(z"94fdcceb", int64), &
             int(z"5001e420", int64), int(z"24126ea1", int64)]
        call blk10(k(1), k(2), c(1), c(2), c(3), c(4), o(1), o(2), o(3), o(4))
        call report("KAT 3 (pi digits)", all(o == e), nfail)

        ! (2) Cross-implementation agreement: shipped kernel against the strictly overflow-free
        !     one, over a sweep.  This is the proposed §13.1a test and the only class D.1 showed
        !     can catch a build that computes wrong values while passing its KAT vectors.
        ok = .true.
        do i = 1, 5
            seedv = seed_of(i)
            do j = -3, 60
                do d = 0, 3
                    call key_of(seedv, kk0, kk1)
                    call ctr_of(int(j, int64), int(d, int64), cc0, cc1, cc2, cc3)
                    call blk10(kk0, kk1, cc0, cc1, cc2, cc3, a0, a1, a2, a3)
                    call blk10_strict(kk0, kk1, cc0, cc1, cc2, cc3, b0, b1, b2, b3)
                    if (a0 /= b0 .or. a1 /= b1 .or. a2 /= b2 .or. a3 /= b3) ok = .false.
                end do
            end do
        end do
        call report("cross-impl: blk10 == blk10_strict over 5x64x4", ok, nfail)

        ! (3) umod_2p64 against a slow shift-and-subtract reference.  Widths 16-25 sit at or above
        !     2**63, which is the regime an earlier version of this sweep never reached.
        ok = .true.
        do i = 1, 60
            s = test_width(i)
            t = umod_2p64(s)
            tref = umod_2p64_ref(s)
            if (t /= tref) ok = .false.
            ! A threshold at or above the width would reject every candidate, i.e. hang.
            if (.not. ult(t, s) .and. s /= 1_int64) ok = .false.
        end do
        call report("umod_2p64 == slow reference over 60 widths (incl. >= 2**63)", ok, nfail)

        ! (4) The Lemire accept/reject decision against an exact reference, on crafted x values
        !     straddling the threshold.  This is the branch that is essentially never taken at the
        !     ranges the timed arms use, so nothing else would exercise it.
        ok = .true.
        do i = 1, 60
            s = test_width(i)
            t = umod_2p64(s)
            do j = -2, 2
                x = probe_x(s, t, j)
                call mulhilo64(x, s, h, l)
                if ((ult(l, t)) .neqv. reject_ref(x, s)) ok = .false.
            end do
        end do
        call report("Lemire reject decision == exact reference", ok, nfail)

        ! (4a) End-to-end on a range wider than 2**63, which is where the threshold's wide branch
        !      is load-bearing.  Asserts containment and, by completing at all, termination -- an
        !      over-large threshold can reject every candidate and spin forever.
        ok = .true.
        do i = 1, 5
            seedv = seed_of(i)
            do j = -50, 50
                kv = int3_gen(seedv, int(j, int64), -huge(1_int64), huge(1_int64))
                if (kv < -huge(1_int64)) ok = .false.
                kv = int3_gen(seedv, int(j, int64), -6148914691236517205_int64, &
                                                     6148914691236517205_int64)
                if (kv < -6148914691236517205_int64 .or. kv > 6148914691236517205_int64) ok = .false.
                kv = int1_gen(seedv, int(j, int64), -huge(1_int64), huge(1_int64))
                if (kv < -huge(1_int64)) ok = .false.
            end do
        end do
        call report("wide ranges (width >= 2**63): contained and terminating", ok, nfail)

        ! (5) Range containment for every rule, including the swap and the degenerate range.
        ok = .true.
        do i = 1, 5
            seedv = seed_of(i)
            do j = -50, 50
                kv = int1_gen(seedv, int(j, int64), LO_N, HI_N)
                if (kv < LO_N .or. kv > HI_N) ok = .false.
                kv = int1_fast(seedv, int(j, int64), LO_N, HI_N)
                if (kv < LO_N .or. kv > HI_N) ok = .false.
                kv = int3_gen(seedv, int(j, int64), LO_N, HI_N)
                if (kv < LO_N .or. kv > HI_N) ok = .false.
                kv = int3_fast(seedv, int(j, int64), LO_N, HI_N)
                if (kv < LO_N .or. kv > HI_N) ok = .false.
                kv = int1_gen(seedv, int(j, int64), LO_W, HI_W)
                if (kv < LO_W .or. kv > HI_W) ok = .false.
                kv = int3_gen(seedv, int(j, int64), LO_W, HI_W)
                if (kv < LO_W .or. kv > HI_W) ok = .false.
                ! Q9: lo > hi must swap internally rather than misbehave.
                kv = int3_gen(seedv, int(j, int64), HI_N, LO_N)
                if (kv < LO_N .or. kv > HI_N) ok = .false.
                ! degenerate single-value range
                kv = int3_fast(seedv, int(j, int64), 7_int64, 7_int64)
                if (kv /= 7_int64) ok = .false.
            end do
        end do
        call report("range containment, swap and lo==hi", ok, nfail)

        ! (6) The fast path must agree with the general path value for value, on both rules.
        ok = .true.
        do i = 1, 5
            seedv = seed_of(i)
            do j = 1, 2000
                if (int1_fast(seedv, int(j, int64), LO_N, HI_N) /= &
                    int1_gen(seedv, int(j, int64), LO_N, HI_N)) ok = .false.
                if (int3_fast(seedv, int(j, int64), LO_N, HI_N) /= &
                    int3_gen(seedv, int(j, int64), LO_N, HI_N)) ok = .false.
            end do
        end do
        call report("fast path == general path, both rules", ok, nfail)

        ! (6a) Q2 option (iii): the overflow-free width/offset arm against the shipped wrapping
        !      form.  The two regimes are graded differently, and that is the point.
        !
        !      Below 2**63 the shipped form has no overflow, so the two MUST agree and a
        !      disagreement is a defect in the new arm -- a gate failure.
        !
        !      At or above 2**63 the shipped form's own `hi - lo + 1` and `lo + high64` are the
        !      signed overflows §20.3 identified, so a disagreement there is the compiler
        !      exploiting them.  That is a FINDING about the build, not a harness error, and it is
        !      reported without blocking the timing -- the timed arms use widths of 1e6 and 4e12,
        !      nowhere near this regime.  Machine C sees exactly this under gfortran 15.2 at
        !      `fpm install --profile release`, with the overflow-free arm agreeing with
        !      arbitrary-precision truth and the shipped form not.
        ok = .true.
        do i = 1, 60
            s = test_width(i)
            if (s <= 0_int64) cycle              ! wide widths are covered by the explicit pairs
            lov = 0_int64
            hiv = s - 1_int64
            do j = 1, 60
                if (int3_gen_of(seed_of(1 + mod(j, 5)), int(j, int64), lov, hiv) /= &
                    int3_gen(seed_of(1 + mod(j, 5)), int(j, int64), lov, hiv)) ok = .false.
            end do
        end do
        call report("Q2(iii) == shipped rule for widths below 2**63", ok, nfail)

        ok = .true.
        do i = 1, 7
            select case (i)
            case (1); lov = -huge(1_int64);          hiv = huge(1_int64)        ! width 2**64 - 1
            case (2); lov = -huge(1_int64);          hiv = huge(1_int64) - 1    ! width 2**64 - 2
            case (3); lov = 0_int64;                 hiv = huge(1_int64)        ! width 2**63
            case (4); lov = -1_int64;                hiv = huge(1_int64)        ! width 2**63 + 1
            case (5); lov = -huge(1_int64) - 1;      hiv = huge(1_int64) - 1    ! width 2**64 - 1
            case (6); lov = -huge(1_int64) - 1;      hiv = 0_int64              ! width 2**63 + 1
            case (7); lov = -huge(1_int64) - 1;      hiv = huge(1_int64)        ! width 0 (full)
            end select
            do j = 1, 300
                kv = int3_gen_of(12345_int64, int(j, int64), lov, hiv)
                if (kv < lov .or. kv > hiv) call report("Q2(iii) stays in range", .false., nfail)
                if (kv /= int3_gen(12345_int64, int(j, int64), lov, hiv)) then
                    if (ok) then
                        write (*, "(a)") "  [ !! ] the shipped WRAPPING range arithmetic disagrees &
                                         &with the overflow-free form at a width >= 2**63"
                        write (*, "(a,i0,a,i0,a,i0)") "         lo=", lov, " hi=", hiv, " i=", j
                        write (*, "(a,i0)") "         overflow-free (correct by construction): ", kv
                        write (*, "(a,i0)") "         shipped wrapping form                  : ", &
                            int3_gen(12345_int64, int(j, int64), lov, hiv)
                        write (*, "(a)") "         This is §20.3's undefined behaviour being &
                                         &exploited: Q2 option (i) is unsafe on this build."
                    end if
                    ok = .false.
                end if
            end do
        end do
        if (ok) then
            call report("Q2(iii) == shipped rule for widths >= 2**63", .true., nfail)
        else
            write (*, "(a)") "  [ !! ] REPORTED, NOT COUNTED AS A GATE FAILURE -- see the note above."
        end if

        ! (6b) `mulhilo64` against a strictly overflow-free 16-bit-limb full product.
        !
        !      §20.3 lists the module's remaining undefined-behaviour sites as SplitMix64's two
        !      multiplies and `pf_random_int_at`'s range arithmetic.  `mulhilo64` is a third, on
        !      the same tier-0 hot path and on BOTH sides of route (e)'s fork: its four 32x32
        !      partial products are formed in `integer(int64)` and `a0*b0` alone reaches
        !      `(2**32-1)**2 > huge(int64)`.  That is the exact shape [§26](#26-open-questions) Q3
        !      rules insufficient for the mixer ("a 32x32 product still exceeds int64").
        !
        !      Nothing here compared it against an overflow-free form before: `reject_ref` calls
        !      `mulhilo64` itself, and gate 2's `ref_value` re-runs `int3_gen`, so a wrong high
        !      half was only ever compared with itself.  A gate failure, because unlike the range
        !      arithmetic this is exercised by every timed integer arm at every width.
        ok = .true.
        do i = 1, 60
            s = test_width(i)
            do j = 1, 40
                select case (j)
                case (1); x = 0_int64
                case (2); x = 1_int64
                case (3); x = -1_int64                      ! 2**64 - 1, the largest unsigned
                case (4); x = M32
                case (5); x = M32 + 1_int64
                case (6); x = -huge(1_int64) - 1_int64      ! 2**63
                case (7); x = huge(1_int64)
                case default
                    x = at_bits(seed_of(1 + mod(j, 5)), int(j, int64) + int(i, int64) * 101_int64)
                end select
                call mulhilo64(x, s, h, l)
                call mulhilo64_strict(x, s, hs, ls)
                if (h /= hs .or. l /= ls) ok = .false.
            end do
        end do
        call report("mulhilo64 == mulhilo64_strict over 60x40 operand pairs", ok, nfail)

        ! (6c) B11, the half `int3_gen_of` cannot answer.  On a compiler with no 128-bit kind
        !      `int3_gen_of` IS `int3_gen`, so (6a) compares a function with itself and reports
        !      `[ ok ]` whatever the compiler does.  `int3_gen_sof` needs no 128-bit kind, so this
        !      pair is a real comparison everywhere.  Below 2**63 the shipped form cannot overflow
        !      and the two must agree: a gate failure.
        !      Two placements per width, and the second one is load-bearing: with `lo = 0` the low
        !      32-bit limb of `hi - lo` can never borrow, so a dropped borrow in `usub64_strict`
        !      survives the whole sweep.  Mutation testing found exactly that.  Placing `lo` at
        !      `2**32 - 1` makes `low32(hi) < low32(lo)` for every width whose low limb is small,
        !      which is what exercises the borrow.
        ok = .true.
        do i = 1, 60
            s = test_width(i)
            if (s <= 0_int64) cycle
            do d = 1, 2
                if (d == 1) then
                    lov = 0_int64
                else
                    if (s > huge(1_int64) - M32) cycle    ! keep the shipped form overflow-free
                    lov = M32
                end if
                hiv = lov + s - 1_int64
                do j = 1, 60
                    if (int3_gen_sof(seed_of(1 + mod(j, 5)), int(j, int64), lov, hiv) /= &
                        int3_gen(seed_of(1 + mod(j, 5)), int(j, int64), lov, hiv)) ok = .false.
                end do
            end do
        end do
        call report("strict-limb Q2 ref == shipped rule for widths below 2**63", ok, nfail)

        ! (6d) The same pair at or above 2**63, where the shipped form's `hi - lo + 1` and
        !      `lo + high64` are §20.3's signed overflows.  Graded as (6a) is: a disagreement is a
        !      FINDING about the build, printed and not counted, because the timed arms use widths
        !      of 1e6 and 4e12.  This is the line B11 asks for, and unlike (6a)'s it is meaningful
        !      on a compiler without `int128`.
        ok = .true.
        do i = 1, 9
            select case (i)
            case (1); lov = -huge(1_int64);          hiv = huge(1_int64)        ! width 2**64 - 1
            case (2); lov = -huge(1_int64);          hiv = huge(1_int64) - 1    ! width 2**64 - 2
            case (3); lov = 0_int64;                 hiv = huge(1_int64)        ! width 2**63
            case (4); lov = -1_int64;                hiv = huge(1_int64)        ! width 2**63 + 1
            case (5); lov = -huge(1_int64) - 1;      hiv = huge(1_int64) - 1    ! width 2**64 - 1
            case (6); lov = -huge(1_int64) - 1;      hiv = 0_int64              ! width 2**63 + 1
            case (7); lov = -huge(1_int64) - 1;      hiv = huge(1_int64)        ! width 0 (full)
            ! The two placements whose low 32-bit limbs BORROW; without them a dropped borrow in
            ! `usub64_strict` passes every case above (found by mutation testing).
            case (8); lov = -huge(1_int64) - 1 + M32; hiv = huge(1_int64) - M32
            case (9); lov = M32;                      hiv = huge(1_int64)
            end select
            do j = 1, 300
                kv = int3_gen_sof(12345_int64, int(j, int64), lov, hiv)
                kw = int3_gen(12345_int64, int(j, int64), lov, hiv)
                if (kv < lov .or. kv > hiv) call report("strict-limb Q2 ref stays in range", &
                                                        .false., nfail)
                if (kv /= kw) then
                    if (ok) then
                        write (*, "(a)") "  [ !! ] STRICT-LIMB check: the shipped WRAPPING range &
                                         &arithmetic disagrees at a width >= 2**63"
                        write (*, "(a,i0,a,i0,a,i0)") "         lo=", lov, " hi=", hiv, " i=", j
                        write (*, "(a,i0)") "         strict limbs (correct by construction): ", kv
                        write (*, "(a,i0)") "         shipped wrapping form                 : ", kw
                        write (*, "(a)") "         Q2 option (i) -- 'document the wrapping' -- is &
                                         &UNSAFE on this build."
                    end if
                    ok = .false.
                end if
            end do
        end do
        if (ok) then
            call report("strict-limb Q2 ref == shipped rule for widths >= 2**63", .true., nfail)
        else
            write (*, "(a)") "  [ !! ] REPORTED, NOT COUNTED AS A GATE FAILURE -- see the note above."
        end if

        ! (6e) The published oracle value, printed so the strict-limb reference can be checked by
        !      eye against arbitrary-precision truth rather than only against another arm here.
        !      Machines A and C both derived 8560708566805553027 for this exact triple.
        write (*, "(a,i0)") "         strict-limb ref, lo=0 hi=huge(int64) i=1 (oracle &
                            &8560708566805553027): ", &
            int3_gen_sof(12345_int64, 1_int64, 0_int64, huge(1_int64))
#ifdef PF_INT128
        ! (6f) Where a 128-bit kind exists the two independent Q2 references must agree, which is
        !      what carries `int3_gen_of`'s oracle validation over to the limb form.
        ok = .true.
        do i = 1, 9
            select case (i)
            case (1); lov = -huge(1_int64);          hiv = huge(1_int64)
            case (2); lov = -huge(1_int64);          hiv = huge(1_int64) - 1
            case (3); lov = 0_int64;                 hiv = huge(1_int64)
            case (4); lov = -1_int64;                hiv = huge(1_int64)
            case (5); lov = -huge(1_int64) - 1;      hiv = huge(1_int64) - 1
            case (6); lov = -huge(1_int64) - 1;      hiv = 0_int64
            case (7); lov = -huge(1_int64) - 1;      hiv = huge(1_int64)
            case (8); lov = -huge(1_int64) - 1 + M32; hiv = huge(1_int64) - M32
            case (9); lov = M32;                      hiv = huge(1_int64)
            end select
            do j = 1, 300
                if (int3_gen_sof(12345_int64, int(j, int64), lov, hiv) /= &
                    int3_gen_of(12345_int64, int(j, int64), lov, hiv)) ok = .false.
            end do
        end do
        call report("strict-limb Q2 ref == int128 Q2 ref (two independent references)", ok, nfail)
#endif

        ! (6g) The strict-`mulhilo64` arms must reproduce the arms they replace, value for value.
        !      A full 64x64 product is exact either way, so any difference is a defect in the limb
        !      form (below 2**63) or the compiler exploiting the range arithmetic (at or above it,
        !      where `int3_gen_sm` keeps the shipped wrapping form and so tracks `int3_gen`).
        !      `int3_gen_full` removes every UB site in the integer path and must therefore equal
        !      `int3_gen_sof` at EVERY width -- which is the check that prices are being compared
        !      between arms computing the same numbers.
        ok = .true.
        do i = 1, 60
            s = test_width(i)
            if (s <= 0_int64) cycle
            do d = 1, 2
                if (d == 1) then
                    lov = 0_int64
                else
                    if (s > huge(1_int64) - M32) cycle
                    lov = M32
                end if
                hiv = lov + s - 1_int64
                do j = 1, 40
                    seedv = seed_of(1 + mod(j, 5))
                    if (int3_gen_sm(seedv, int(j, int64), lov, hiv) /= &
                        int3_gen(seedv, int(j, int64), lov, hiv)) ok = .false.
                    if (int3_gen_full(seedv, int(j, int64), lov, hiv) /= &
                        int3_gen_sof(seedv, int(j, int64), lov, hiv)) ok = .false.
                end do
            end do
        end do
        call report("strict-mulhilo arms == the arms they replace below 2**63", ok, nfail)

        ! (6h) The same pair at or above 2**63, GRADED SEPARATELY and not as a gate failure.
        !
        !      `int3_gen_sof` and `int3_gen_full` differ only in `mulhilo64` against
        !      `mulhilo64_strict`, and a full 64x64 product is exact either way -- so they cannot
        !      legitimately differ. But at these widths the shipped `mulhilo64`'s partial products
        !      DO overflow (`s`'s low 32 bits are large), so a disagreement is the compiler
        !      exploiting §30.13's third undefined-behaviour site: a finding about the build, on the
        !      same footing as (6d)'s, and reported the same way rather than blocking the timing.
        !
        !      Machine B sees this fire under gfortran 14.2.1 on the `int64` fork with fpm's full
        !      release flag list, and NOT at plain `-O3`, `-O2`, `-O0`, with `-fwrapv`, without
        !      `-fPIC`, or on the `int128` fork. It is destroyed by observation -- adding a `write`,
        !      a recorded value or a checksum inside this loop makes it disappear -- so the loop is
        !      deliberately kept to one counter and nothing else.
        ok = .true.
        nwide = 0
        do i = 1, 9
            select case (i)
            case (1); lov = -huge(1_int64);           hiv = huge(1_int64)
            case (2); lov = -huge(1_int64);           hiv = huge(1_int64) - 1
            case (3); lov = 0_int64;                  hiv = huge(1_int64)
            case (4); lov = -1_int64;                 hiv = huge(1_int64)
            case (5); lov = -huge(1_int64) - 1;       hiv = huge(1_int64) - 1
            case (6); lov = -huge(1_int64) - 1;       hiv = 0_int64
            case (7); lov = -huge(1_int64) - 1;       hiv = huge(1_int64)
            case (8); lov = -huge(1_int64) - 1 + M32; hiv = huge(1_int64) - M32
            case (9); lov = M32;                      hiv = huge(1_int64)
            end select
            do j = 1, 300
                if (int3_gen_full(12345_int64, int(j, int64), lov, hiv) /= &
                    int3_gen_sof(12345_int64, int(j, int64), lov, hiv)) nwide = nwide + 1
            end do
        end do
        if (nwide == 0) then
            call report("strict-mulhilo arms == the arms they replace at widths >= 2**63", &
                        .true., nfail)
        else
            write (*, "(a,i0,a)") "  [ !! ] the shipped mulhilo64 disagrees with the overflow-free &
                                  &form on ", nwide, " of 2700 draws at widths >= 2**63"
            write (*, "(a)") "         Both arms use the SAME strict range arithmetic, so this is &
                             &§30.13's third UB site being exploited."
            write (*, "(a)") "         REPORTED, NOT COUNTED AS A GATE FAILURE -- the timed arms &
                             &use widths of 1e6 and 4e12."
        end if

        ! (7) The retry path must actually fire.  A range just above 2**63 rejects about a quarter
        !     of the time, so this exercises the loop the timed arms never enter.
        lov = 0_int64
        hiv = 6148914691236517205_int64          ! width ~ 2**64/3, so 2**64 mod s is ~ 2**64/3 too
        nhit = 0; nmax = 0
        do j = 1, 4000
            call int3_gen_diag(12345_int64, int(j, int64), lov, hiv, kv, nret)
            if (nret > 0) nhit = nhit + 1
            if (nret > nmax) nmax = nret
            if (kv < lov .or. kv > hiv) nhit = -1000000
        end do
        write (*, "(a,i0,a,i0)") "         retries fired on ", nhit, " of 4000 draws, max chain ", nmax
        call report("retry path fires and stays in range", nhit > 100 .and. nhit < 3000, nfail)

        ! (8) Uniformity over a tiny range -- catches a broken reduction, not a 2**-64 bias.
        cnt = 0_int64
        do j = 1, 700000
            kv = int3_fast(999_int64, int(j, int64), 0_int64, 6_int64)
            cnt(kv) = cnt(kv) + 1_int64
        end do
        expct = 700000.0_real64 / 7.0_real64
        chi = 0.0_real64
        do i = 0, 6
            chi = chi + (real(cnt(i), real64) - expct)**2 / expct
        end do
        call report("opt3 uniformity over 7 bins (chi2 < 30)", chi < 30.0_real64, nfail)

        cnt = 0_int64
        do j = 1, 700000
            kv = int1_fast(999_int64, int(j, int64), 0_int64, 6_int64)
            cnt(kv) = cnt(kv) + 1_int64
        end do
        chi = 0.0_real64
        do i = 0, 6
            chi = chi + (real(cnt(i), real64) - expct)**2 / expct
        end do
        call report("opt1 uniformity over 7 bins (chi2 < 30)", chi < 30.0_real64, nfail)

        ! (9) The two rules must NOT agree value for value -- they are different permanent
        !     contracts, which is precisely why E.4 must be settled before Phase 1.
        nhit = 0
        do j = 1, 1000
            if (int1_fast(42_int64, int(j, int64), LO_N, HI_N) == &
                int3_fast(42_int64, int(j, int64), LO_N, HI_N)) nhit = nhit + 1
        end do
        write (*, "(a,i0,a)") "         opt1 and opt3 agree on ", nhit, " of 1000 draws (chance ~0)"
        call report("opt1 /= opt3: the choice is a permanent contract", nhit < 20, nfail)
    end subroutine self_tests

    !> Prints one self-test result and counts a failure.
    subroutine report(what, ok, nfail)
        character(len=*), intent(in)    :: what   !! description of the check
        logical,          intent(in)    :: ok     !! whether it passed
        integer,          intent(inout) :: nfail  !! running failure count
        if (ok) then
            write (*, "(a,a)") "  [ ok ] ", what
        else
            write (*, "(a,a)") "  [FAIL] ", what
            nfail = nfail + 1
        end if
    end subroutine report

    !> One of five sweep seeds, including negative and extreme values (Q20b).
    pure function seed_of(i) result(s)
        integer, intent(in) :: i  !! index 1..5
        integer(int64) :: s       !! the seed
        select case (i)
        case (1); s = 0_int64
        case (2); s = 1_int64
        case (3); s = 42_int64
        case (4); s = -7_int64
        case default; s = huge(1_int64)
        end select
    end function seed_of

    !> A spread of range widths for the threshold and decision tests, including awkward ones.
    !!
    !! Cases 16-25 have an unsigned value `>= 2**63`, i.e. they are NEGATIVE as signed `int64`.
    !! They are the regime `umod_2p64`'s wide branch exists for, and an earlier version of this
    !! function stopped at `huge(int64)` -- so every width it offered was one the signed doubling
    !! form happened to handle, and the defect that form has above `2**63` was invisible to a
    !! self-test that looked exhaustive. These widths are reachable from the public API: case 16 is
    !! `pf_random_int_at(seed, i, -huge(int64), huge(int64))`, which feature_random.md §13.4
    !! already requires an edge test for.
    pure function test_width(i) result(s)
        integer, intent(in) :: i  !! index 1..60
        integer(int64) :: s       !! a range width >= 1, read as UNSIGNED (may be negative)
        select case (i)
        case (1);  s = 1_int64
        case (2);  s = 2_int64
        case (3);  s = 3_int64
        case (4);  s = 7_int64
        case (5);  s = 10_int64
        case (6);  s = 1000000_int64
        case (7);  s = M32
        case (8);  s = M32 + 1_int64
        case (9);  s = 3999999999999_int64
        case (10); s = ishft(1_int64, 62)
        case (11); s = ishft(1_int64, 62) + 1_int64
        case (12); s = ishft(1_int64, 62) + 12345_int64
        case (13); s = huge(1_int64)
        case (14); s = huge(1_int64) - 1_int64
        case (15); s = 6148914691236517206_int64        ! ~ 2**64/3: rejects about a third of the time
        ! ---- widths at or above 2**63, negative as signed int64 ----
        case (16); s = -1_int64                         ! 2**64 - 1 : lo = -huge, hi = huge
        case (17); s = -2_int64                         ! 2**64 - 2
        case (18); s = -3_int64                         ! 2**64 - 3
        case (19); s = -huge(1_int64)                   ! 2**63 + 1
        case (20); s = -huge(1_int64) - 1_int64         ! exactly 2**63; remainder is 0
        case (21); s = -huge(1_int64) + 1_int64         ! 2**63 + 2
        case (22); s = -6148914691236517205_int64       ! ~ (2/3)*2**64: the worst case for the
        case (23); s = -6148914691236517206_int64       !   defect the wide branch fixes
        case (24); s = -1000000007_int64                ! 2**64 - 1000000007
        case (25); s = -4611686018427387904_int64       ! 1.5 * 2**63
        case default; s = int(i, int64) * 987654321_int64 + 17_int64
        end select
    end function test_width

    !> `2**64 mod s` by repeated shift-and-subtract; slow, obviously correct, reference only.
    !!
    !! Honest note on how independent this really is, because it was not independent enough once.
    !! The doubling loop shares `umod_2p64`'s signed-arithmetic assumption exactly, so before the
    !! `s >= 2**63` regime was handled, BOTH functions returned nonsense there (this one returned
    !! -1 for `s = 2**64 - 1`, where the answer is 1) -- two implementations agreeing on a wrong
    !! answer because they shared the assumption that was wrong. That is the same lesson as
    !! feature_random.md D.1 one level down: "independent" means independent *on the axis the fault
    !! lives on*, and the axis here is signed-versus-unsigned, not the reduction algorithm. So the
    !! wide branch below is derived a different way from `umod_2p64`'s -- via the offset from
    !! `2**63` rather than via negation -- which is also what keeps it free of overflow.
    pure function umod_2p64_ref(s) result(t)
        integer(int64), intent(in) :: s  !! range width read as unsigned, >= 1
        integer(int64) :: t              !! `2**64 mod s`
        integer(int64) :: m
        integer :: b
        if (s < 0_int64) then
            ! s >= 2**63 unsigned.  Write s = 2**63 + m with m = s - (-2**63) in [0, 2**63):
            ! that subtraction cannot overflow, and 2**64 - s = 2**63 - m, which is representable
            ! for every m >= 1.  m == 0 is s == 2**63, whose remainder is 0.
            m = s - (-huge(1_int64) - 1_int64)
            if (m == 0_int64) then
                t = 0_int64
            else
                t = huge(1_int64) - (m - 1_int64)      ! = 2**63 - m, computed without overflow
            end if
            return
        end if
        ! Builds 2**64 mod s by starting from 2**0 mod s and doubling modulo s sixty-four times.
        ! Every intermediate stays inside [0, s), so nothing can overflow and no unsigned reasoning
        ! is needed at all -- a different route from umod_2p64's, which is the point.
        t = mod(1_int64, s)
        do b = 1, 64
            if (t >= s - t) then
                t = t - (s - t)          ! 2t >= s, so the reduced value is 2t - s
            else
                t = t + t                ! 2t < s, so no reduction and no overflow
            end if
        end do
    end function umod_2p64_ref

    !> A 64-bit value engineered to sit `j` steps from the Lemire threshold for width `s`.
    pure function probe_x(s, t, j) result(x)
        integer(int64), intent(in) :: s  !! range width
        integer(int64), intent(in) :: t  !! the threshold `2**64 mod s`
        integer,        intent(in) :: j  !! offset in units of the low-part step
        integer(int64) :: x              !! a candidate uniform value
        integer(int64) :: q
        ! low(x*s) walks in steps of s as x increments, so x = floor(t/s) + j lands the low part
        ! within a few steps of the threshold, on both sides of it.
        q = 0_int64
        if (s > 0_int64) q = t / s
        x = q + int(j, int64)
    end function probe_x

    !> Exact reference for Lemire's reject decision, computed independently of the timed path.
    pure function reject_ref(x, s) result(r)
        integer(int64), intent(in) :: x  !! the candidate uniform value
        integer(int64), intent(in) :: s  !! range width
        logical :: r                     !! `.true.` when the candidate must be rejected
        integer(int64) :: h, l, t
        call mulhilo64(x, s, h, l)
        t = umod_2p64_ref(s)
        r = ult(l, t)
    end function reject_ref

! =============================================================================================
! Gate 2 -- timed-path verification
! =============================================================================================

    !> Re-runs one arm exactly as the timing loop ran it and checks the buffer against a reference.
    !!
    !! Not redundant with the self-tests: an auto-vectorised clone is separate code from the scalar
    !! path those exercise, and B.2 demonstrated the same source computing correct values at one
    !! call site and wrong ones at another in the same binary.
    subroutine verify_timed_path(a, seed, base, n, du, dk, ok)
        integer,        intent(in)    :: a       !! arm index, 1..NARM
        integer(int64), intent(in)    :: seed    !! the seed the timing loop used
        integer(int64), intent(in)    :: base    !! the base the timing loop last used
        integer,        intent(in)    :: n       !! how many values it wrote
        real(real64),   intent(inout) :: du(:)   !! the uniform destination
        integer(int64), intent(inout) :: dk(:)   !! the integer destination
        logical,        intent(out)   :: ok      !! `.true.` when every element matched
        integer :: j
        integer(int64) :: kref, o0, o1, o2, o3, kk0, kk1, cc0, cc1, cc2, cc3
        real(real64) :: uref
        call fill_arm(a, seed, base, n, du, dk)
        ok = .true.
        do j = 1, n
            if (a == 1) then
                call key_of(seed, kk0, kk1)
                call ctr_of(base + int(j, int64), 0_int64, cc0, cc1, cc2, cc3)
                call blk10_strict(kk0, kk1, cc0, cc1, cc2, cc3, o0, o1, o2, o3)
                uref = to_r64(u64_of(o0, o1))
                if (du(j) /= uref) ok = .false.
            else
                kref = ref_value(a, seed, base + int(j, int64))
                if (dk(j) /= kref) ok = .false.
            end if
            if (.not. ok) exit
        end do
    end subroutine verify_timed_path

    !> Reference value for one arm at one index, computed through the strictly overflow-free
    !! kernel wherever the arm's own kernel is not itself the thing under test.
    function ref_value(a, seed, i) result(k)
        integer,        intent(in) :: a     !! arm index, 2..NARM
        integer(int64), intent(in) :: seed  !! the seed
        integer(int64), intent(in) :: i     !! the stream label
        integer(int64) :: k                 !! the expected value
        integer(int64) :: o0, o1, o2, o3, kk0, kk1, cc0, cc1, cc2, cc3
        call key_of(seed, kk0, kk1)
        call ctr_of(i, 0_int64, cc0, cc1, cc2, cc3)
        call blk10_strict(kk0, kk1, cc0, cc1, cc2, cc3, o0, o1, o2, o3)
        select case (a)
        case (2);  k = u64_of(o0, o1)
        case (3);  k = int1_gen(seed, i, LO_N, HI_N)
        case (4);  k = int1_gen(seed, i, LO_N, HI_N)
        case (5);  k = int1_gen(seed, i, LO_W, HI_W)
        case (6);  k = int3_gen(seed, i, LO_N, HI_N)
        case (7);  k = int3_gen(seed, i, LO_N, HI_N)
        case (8);  k = int3_gen(seed, i, LO_W, HI_W)
        case (9);  k = int3_gen_nr(seed, i, LO_N, HI_N)
        case (10); k = int3_gen_nr(seed, i, LO_N, HI_N)
        case (11); k = int3_gen(seed, i, LO_N, HI_N)
        case (12); k = int3_gen(seed, i, LO_W, HI_W)
        case (13); k = int3_gen(seed, i, LO_N, HI_N)
        case (14); k = int3_gen(seed, i, LO_W, HI_W)
        case (15); k = int3_gen(seed, i, LO_N, HI_N)
        case (16); k = int3_gen(seed, i, LO_W, HI_W)
        case (17); k = int3_gen(seed, i, LO_N, HI_N)
        case (18); k = int3_gen(seed, i, LO_W, HI_W)
        case default; k = 0_int64
        end select
    end function ref_value

end module pf_probe_int_rule


!> Drives the probe: two correctness gates, then the two timing modes, then the summary table.
program probe_random_int_rule
    use iso_fortran_env, only: int64, real64, compiler_version
    use pf_probe_int_rule, only: NARM, arm_name, arm_is_int, fill_arm, self_tests, &
                                 verify_timed_path, set_ranges
    implicit none

    ! MODE A: a 256 KB, L2-resident destination written 256 times.
    ! MODE B: a 64 MB destination written once.  A conclusion that holds in both is a property of
    ! the generator; one that does not is a property of the memory system (Appendix B.5).
    integer, parameter :: NA = 32768, RA = 256
    integer, parameter :: NB = 8388608, RB = 1
    integer, parameter :: NROUND = 5
    integer(int64), parameter :: SEED = 12345_int64

    real(real64),   allocatable :: du(:)
    integer(int64), allocatable :: dk(:)
    real(real64) :: ns(NARM, 2), best, dt, rate, uni_a, uni_b
    integer(int64) :: t0, t1, cr, cm, base, chk
    integer :: a, mode, rnd, rep, n, nrep, nfail
    logical :: ok, allok

    write (*, "(a)") "============================================================================"
    write (*, "(a)") " probe_random_int_rule -- feature_random.md E.4: opt1 pinned vs opt3 Lemire"
    write (*, "(a)") "============================================================================"
    write (*, "(a,a)") " compiler : ", trim(compiler_version())
#ifdef PF_INT128
    write (*, "(a)") " multiply : int128 (route (e) taken)"
#else
    write (*, "(a)") " multiply : int64 wrapping (no 128-bit integer kind on this compiler)"
#endif
    write (*, "(a)") " ranges  : set at run time, so the multiply cannot be specialised for them"
    write (*, "(a)") ""

    ! `command_argument_count()` is opaque to the optimiser and is 0 in normal use, so the ranges
    ! are the intended ones while the width stays unknown at compile time.
    call set_ranges(command_argument_count())

    call self_tests(nfail)
    write (*, "(a)") ""
    if (nfail /= 0) then
        write (*, "(a,i0,a)") " REFUSING TO TIME: ", nfail, " self-test failure(s)."
        write (*, "(a)") " That is a RESULT, not a harness error -- see feature_random.md B.2."
        stop 1
    end if

    allocate (du(max(NA, NB)), dk(max(NA, NB)))
    du = 0.0_real64
    dk = 0_int64

    call system_clock(count_rate=cr)
    rate = real(cr, real64)

    ! Warm every destination page before anything is timed, and warm every code path once, so no
    ! arm absorbs first-touch faults or a cold instruction cache on behalf of the others.
    write (*, "(a)") "--- warming destinations and every code path ---"
    flush (6)
    do a = 1, NARM
        call fill_arm(a, SEED, 0_int64, NB, du, dk)
    end do

    write (*, "(a)") "--- gate 2: timed-path verification ---"
    flush (6)
    allok = .true.
    do a = 1, NARM
        call verify_timed_path(a, SEED, 7_int64, NA, du, dk, ok)
        if (.not. ok) then
            write (*, "(a,a)") "  [FAIL] ", trim(arm_name(a))
            allok = .false.
        end if
    end do
    if (allok) then
        write (*, "(a)") "  [ ok ] every timed path reproduces the strict-kernel reference"
    else
        write (*, "(a)") " REFUSING TO TIME: a timed path computes wrong values."
        stop 1
    end if
    write (*, "(a)") ""

    write (*, "(a)") "--- timing: ns per value, best of 5, MODE A then MODE B ---"
    write (*, "(a)") ""
    write (*, "(a)") "  arm                                     MODE A    MODE B   xU(A)   xU(B)"
    write (*, "(a)") "  --------------------------------------------------------------------------"
    do a = 1, NARM
        do mode = 1, 2
            if (mode == 1) then
                n = NA; nrep = RA
            else
                n = NB; nrep = RB
            end if
            best = huge(1.0_real64)
            do rnd = 1, NROUND
                call system_clock(t0)
                do rep = 1, nrep
                    base = int(rep - 1, int64) * int(n, int64)
                    call fill_arm(a, SEED, base, n, du, dk)
                end do
                call system_clock(t1)
                dt = real(t1 - t0, real64) / rate
                if (dt < best) best = dt
            end do
            ns(a, mode) = best * 1.0e9_real64 / (real(nrep, real64) * real(n, real64))
        end do
        if (a == 1) then
            uni_a = ns(1, 1)
            uni_b = ns(1, 2)
        end if
        write (*, "(a,a,f9.3,f10.3,f8.2,f8.2)") "  ", arm_name(a), ns(a, 1), ns(a, 2), &
            ns(a, 1) / uni_a, ns(a, 2) / uni_b
        flush (6)
    end do
    write (*, "(a)") ""

    ! Keep the buffers live so nothing above can be eliminated.  Arm 10 is re-run first so the
    ! printed checksum stays comparable with every earlier run of this probe: it is the value the
    ! summing loop would have seen when arm 10 was the last arm, and adding arms must not move it.
    call fill_arm(10, SEED, 0_int64, NA, du, dk)
    chk = 0_int64
    cm = 0_int64
    do a = 1, NA
        chk = ieor(chk, dk(a))
        cm = cm + int(du(a) * 1024.0_real64, int64)
    end do

    write (*, "(a)") "--- head-to-head, option 3 against option 1 (lower is better for opt3) ---"
    write (*, "(a,f7.3,a,f7.3)") "  range 1e6, best available path:  opt1 fast ", ns(4, 1), &
        "   opt3 fast ", ns(7, 1)
    write (*, "(a,f7.3,a,f7.3)") "  range 1e6, general path:         opt1 gen  ", ns(3, 1), &
        "   opt3 gen  ", ns(6, 1)
    write (*, "(a,f7.3,a,f7.3)") "  range 4e12, general path:        opt1 gen  ", ns(5, 1), &
        "   opt3 gen  ", ns(8, 1)
    write (*, "(a)") ""
    write (*, "(a)") "--- Q2 option (iii): overflow-free width/offset against the shipped wrapping form ---"
    write (*, "(a,f7.3,a,f7.3)") "  range 1e6 :   shipped ", ns(6, 1), "   Q2(iii) ", ns(11, 1)
    write (*, "(a,f7.3,a,f7.3)") "  range 4e12:   shipped ", ns(8, 1), "   Q2(iii) ", ns(12, 1)
    write (*, "(a)") ""
    write (*, "(a)") "--- Q2 by 32-bit LIMBS: the int128-free form, priced on every compiler ---"
    write (*, "(a,f7.3,a,f7.3)") "  range 1e6 :   shipped ", ns(6, 1), "   limbs   ", ns(13, 1)
    write (*, "(a,f7.3,a,f7.3)") "  range 4e12:   shipped ", ns(8, 1), "   limbs   ", ns(14, 1)
    write (*, "(a)") ""
    write (*, "(a)") "--- the THIRD UB site: strict mulhilo64, and a fully UB-free integer path ---"
    write (*, "(a,f7.3,a,f7.3,a,f7.3)") "  range 1e6 :   shipped ", ns(6, 1), &
        "   strict mulhi ", ns(15, 1), "   ALL strict ", ns(17, 1)
    write (*, "(a,f7.3,a,f7.3,a,f7.3)") "  range 4e12:   shipped ", ns(8, 1), &
        "   strict mulhi ", ns(16, 1), "   ALL strict ", ns(18, 1)
    write (*, "(a)") ""
    write (*, "(a)") "--- attribution: what the rejection branch itself costs ---"
    write (*, "(a,f7.3)") "  opt3 fast - opt3 fast(no reject) : ", ns(7, 1) - ns(10, 1)
    write (*, "(a,f7.3)") "  opt3 gen  - opt3 gen (no reject) : ", ns(6, 1) - ns(9, 1)
    write (*, "(a,f7.3)") "  opt3 gen(no reject) - opt1 gen   : ", ns(9, 1) - ns(3, 1)
    write (*, "(a)") ""
    write (*, "(a,i0,a,i0)") " checksums (ignore): ", chk, " ", cm

end program probe_random_int_rule
