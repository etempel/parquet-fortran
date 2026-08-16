!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
! ROUTE (e), and why this file opens with a preprocessor fork.
!
! Philox's round is built on two 32x32 -> 64 multiplies. Both operands are below 2**32, so the
! true product is below 2**64 -- which does not fit in a signed 64-bit integer, so writing the
! multiply directly is signed overflow. That is not a theoretical complaint: gfortran 14 and 15
! have both been observed MISCOMPILING the unprotected form at -O3, silently, with no warning
! under -Wall -Wextra, producing plausible-looking wrong values.
!
! Where the compiler has a 128-bit integer kind the product is formed in it instead. The
! operands still being below 2**32, the product is then provably in range and the undefined
! behaviour is removed outright rather than merely avoided; the optimiser narrows it straight
! back to a native multiply, so this is a statement that the product cannot overflow rather than
! a wider operation at run time. Where there is no such kind (ifx), the wrapping int64 product
! ships, and the agreement tests are what stand behind it.
!
! "The compiler wraps this expression" is NOT the same claim as "this expression is safe", and
! conflating the two has already cost this module one silent wrong answer. ifx wraps the round
! multiply faithfully -- and was still caught using the undefinedness of a DIFFERENT overflowing
! expression to delete a branch that read the result's sign, hundreds of lines away (see
! `width_of`). Every remaining wrapping site therefore rests on the cross-implementation agreement
! sweep in `test_random`, not on a wrapping measurement.
!
! The fork is a compiler ALLOWLIST plus a build-breaking capability assertion. cpp cannot
! evaluate selected_int_kind(38), which is why a predefine is needed at all; fpm passes -cpp
! (or -fpp) automatically, so nothing is asked of a consumer. Deliberately absent: any
! consumer-supplied macro or escape hatch, which would reintroduce exactly the silent
! wrong-kernel hazard this exists to close. The remaining silent direction -- a capable compiler
! not named below quietly taking the wrapping path -- is closed by a test, not by cpp:
! `parquet_debug_random_uses_int128()` must agree with `selected_int_kind(38) > 0`.

#if defined(__GFORTRAN__) || defined(__flang__) || defined(__FLANG)
#  define PF_INT128 1
#endif

!> Counter-based random numbers: reproducible under any OpenMP schedule, at any thread count.
!!
!! An ordinary generator carries state, so which value a loop iteration receives depends on how
!! many draws came before it -- which, in a parallel loop, depends on the schedule, the thread
!! count and the machine's timing. This module removes the state instead of guarding it. Every
!! value is a pure function of three coordinates: a `seed`, a stream index `i`, and a 1-based
!! `draw` within that stream. Iteration 5000 gets the same number whether it ran first, last,
!! alone, or on a machine with 384 cores.
!!
!! ```fortran
!! !$omp parallel do schedule(dynamic)
!! do i = 1, n
!!     x(i) = pf_random_at(seed, i)          ! same value for this i, always
!! end do
!! ```
!!
!! The generator is **Philox4x32-10**, a counter-based cipher of established quality. Every value
!! this module can produce is frozen by the contract `pf_random_algorithm` names: a change to any
!! of it is a major version, and the identifier is how a program can tell.
!!
!! Not cryptographic. Do not use it for keys, tokens, or anything an adversary should not be able
!! to predict -- the seed is recoverable from a handful of outputs.
module parquet_random

    use iso_fortran_env, only: int32, int64, real32, real64

    implicit none
    private

    public :: pf_random_algorithm
    public :: pf_random_at
    public :: pf_random32_at
    public :: pf_random_bits_at
    public :: pf_random_int_at
    public :: pf_random_fill_at
    public :: pf_random_seed
    public :: pf_random_key
    public :: parquet_debug_random_uses_int128
    public :: parquet_debug_random_block

    !> Identifies the algorithm together with every mapping this module freezes -- the cipher, the
    !! key and counter layout, the word order, the integer rule and its retry key. Its value changes
    !! if and only if one of those changes, so a program that records it can tell whether a stored
    !! result is still reproducible. Identical on both sides of the route (e) fork, which changes
    !! how a product is formed and never what it equals.
    character(len=*), parameter :: pf_random_algorithm = "philox4x32-10/v1"

    ! ---- Philox4x32-10 constants (verified against Random123) ----

    !> Low 32 bits set: masks a 64-bit register down to one Philox word.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> First round multiplier.
    integer(int64), parameter :: PHILOX_M0 = int(z'D2511F53', int64)
    !> Second round multiplier.
    integer(int64), parameter :: PHILOX_M1 = int(z'CD9E8D57', int64)
    !> Key bump for word 0 (the golden ratio).
    integer(int64), parameter :: PHILOX_W0 = int(z'9E3779B9', int64)
    !> Key bump for word 1 (sqrt(3) - 1).
    integer(int64), parameter :: PHILOX_W1 = int(z'BB67AE85', int64)
    !> Round count. Ten is the standard, well past the point where bias is measurable.
    integer, parameter :: PHILOX_ROUNDS = 10

    ! ---- SplitMix64 finaliser constants ----
    !
    ! Assembled from two 32-bit halves rather than written as one 64-bit BOZ literal: the halves
    ! are unambiguous constant expressions on every compiler, where a full-width BOZ whose top bit
    ! is set relies on a conversion rule that is easy to get subtly wrong.

    !> First multiplier of the SplitMix64 finaliser.
    integer(int64), parameter :: MIX_A = ior(ishft(int(z'BF58476D', int64), 32), int(z'1CE4E5B9', int64))
    !> Second multiplier of the SplitMix64 finaliser.
    integer(int64), parameter :: MIX_B = ior(ishft(int(z'94D049BB', int64), 32), int(z'133111EB', int64))

    !> Offset in label space that keeps a retried integer draw's key away from
    !! `pf_random_key(seed, n)`. Without it a retried draw would read the same block as the
    !! user's own derived-key family for label `n` -- 100 % of the time, against a documented
    !! idiom. Load-bearing, not decoration.
    integer(int64), parameter :: RETRY_TAG = ishft(int(z'5A170000', int64), 32)

    !> Bit 63 alone: XORing it flips the ordering between signed and unsigned, which is how this
    !! module compares 64-bit patterns as unsigned without leaving the bit domain.
    integer(int64), parameter :: SIGN_BIT = ibset(0_int64, 63)
    !> Low 16 bits set: one limb of the strict multiply.
    integer(int64), parameter :: M16 = 65535_int64

#ifdef PF_INT128
    !> The 128-bit integer kind route (e) forms Philox's multiplies in.
    integer, parameter :: k128 = selected_int_kind(38)
    !> Build-breaking capability assertion: if the allowlist above selected this fork on a target
    !! whose compiler has no 128-bit kind, `k128` is -1 and this is a division by zero in a
    !! constant expression -- a compile error naming this line, rather than a kind error somewhere
    !! confusing, or worse, silence.
    integer, parameter :: pf_int128_assert = 1 / merge(1, 0, k128 > 0)
    !> One Philox word's mask, widened.
    integer(k128), parameter :: M32_128 = int(M32, k128)
    !> 2**63, as the boundary at which a 64-bit pattern held in the wide kind is negative.
    integer(k128), parameter :: TWO63_128 = int(huge(1_int64), k128) + 1_k128
    !> 2**64.
    integer(k128), parameter :: TWO64_128 = 2_k128 * TWO63_128
    !> Low 64 bits set, widened.
    integer(k128), parameter :: MASK64_128 = TWO64_128 - 1_k128
#endif

    !> Process-wide call counter behind `pf_random_seed`, and the module's only mutable state.
    !! Every other procedure here is a pure function of its arguments.
    integer(int64), save :: seed_call_counter = 0_int64

    !> One uniform `real64` in `[0, 1)`: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! `i` is `integer(int32)` or `integer(int64)`; the two give identical values, because an
    !! `int32` stream sign-extends before it reaches the counter. `seed` and `draw` are always
    !! `integer(int64)`, so an explicit draw literal is written `3_int64`.
    interface pf_random_at
        module procedure pf_random_at_i32
        module procedure pf_random_at_i64
    end interface pf_random_at

    !> One uniform `real32` in `[0, 1)`: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! This enumerates its OWN sequence, one word per value, and is deliberately not a narrowing
    !! of `pf_random_at` -- the two share a stream but not a value. `i` is `integer(int32)` or
    !! `integer(int64)`; `seed` and `draw` are `integer(int64)`.
    interface pf_random32_at
        module procedure pf_random32_at_i32
        module procedure pf_random32_at_i64
    end interface pf_random32_at

    !> 64 raw bits: value `draw` (default 1) of stream `i` under `seed`, as `integer(int64)`.
    !!
    !! Reads the same two words as `pf_random_at`, so `pf_random_at` is exactly this pattern's top
    !! 53 bits scaled into `[0, 1)`. `i` is `integer(int32)` or `integer(int64)`; `seed` and `draw`
    !! are `integer(int64)`. Every one of the 2**64 patterns is possible.
    interface pf_random_bits_at
        module procedure pf_random_bits_at_i32
        module procedure pf_random_bits_at_i64
    end interface pf_random_bits_at

    !> A uniform integer in `[lo, hi]`, exactly unbiased: value `draw` (default 1) of stream `i`.
    !!
    !! `i`, `lo` and `hi` share one kind -- `integer(int32)` or `integer(int64)` -- and the result
    !! follows it; `seed` and `draw` are always `integer(int64)`. Swaps internally when `lo > hi`,
    !! so the function is total. Every width is exact, including the widest: no modulo bias, at any
    !! range, on either side of the fork.
    interface pf_random_int_at
        module procedure pf_random_int_at_i32
        module procedure pf_random_int_at_i64
    end interface pf_random_int_at

    !> Fills `v` with consecutive values of one stream, starting at `draw` (default 1).
    !!
    !! `v` is a rank-1 `real(real64)` or `real(real32)` array, `intent(out)`; `i` is
    !! `integer(int32)` or `integer(int64)`; `seed` and `draw` are `integer(int64)`. The values are
    !! exactly what the matching scalar draws would give at those positions, so a prefix is a
    !! prefix: `v(1:3)` filled alone equals the first three of `v(1:6)`. A zero-sized `v` is a
    !! defined no-op.
    !!
    !! **Precondition on the draw axis: `draw + size(v) - 1` must not exceed `huge(int64)`.** The
    !! last element's position has to be representable, because there is no value at a position
    !! that cannot be named -- a fill that runs past the end is asking for draws that do not exist,
    !! and it silently receives wrapped ones. Everything up to and including the boundary is exact:
    !! a fill whose final position is `huge(int64)` itself is correct, and is tested. The scalar
    !! entry points have no such limit, since every representable `draw` is a valid one.
    interface pf_random_fill_at
        module procedure pf_random_fill_at_r64_i32
        module procedure pf_random_fill_at_r64_i64
        module procedure pf_random_fill_at_r32_i32
        module procedure pf_random_fill_at_r32_i64
    end interface pf_random_fill_at

    !> Derives an independent seed from a seed and a label, so one seed can fan out into families.
    !!
    !! `label` is `integer(int32)` or `integer(int64)`; `seed` and the result are `integer(int64)`.
    !! A derived key IS a seed, so derivations compose by nesting.
    interface pf_random_key
        module procedure pf_random_key_i32
        module procedure pf_random_key_i64
    end interface pf_random_key

contains

    ! ================================================================================
    ! Tier 0 -- stateless indexed draws
    ! ================================================================================

    !> `pf_random_at` for an `integer(int32)` stream index.
    pure elemental function pf_random_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real64(bits_of(seed, int(i, int64), draw_or_1(draw)))
    end function pf_random_at_i32

    !> `pf_random_at` for an `integer(int64)` stream index.
    pure elemental function pf_random_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid, negatives included
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real64) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real64(bits_of(seed, i, draw_or_1(draw)))
    end function pf_random_at_i64

    !> `pf_random32_at` for an `integer(int32)` stream index.
    pure elemental function pf_random32_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real32) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real32(word_of(seed, int(i, int64), draw_or_1(draw) - 1_int64))
    end function pf_random32_at_i32

    !> `pf_random32_at` for an `integer(int64)` stream index.
    pure elemental function pf_random32_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        real(real32) :: r                           !! a uniform draw in `[0, 1)`
        r = to_real32(word_of(seed, i, draw_or_1(draw) - 1_int64))
    end function pf_random32_at_i64

    !> `pf_random_bits_at` for an `integer(int32)` stream index.
    pure elemental function pf_random_bits_at_i32(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int64) :: r                         !! 64 raw bits
        r = bits_of(seed, int(i, int64), draw_or_1(draw))
    end function pf_random_bits_at_i32

    !> `pf_random_bits_at` for an `integer(int64)` stream index.
    pure elemental function pf_random_bits_at_i64(seed, i, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int64) :: r                         !! 64 raw bits
        r = bits_of(seed, i, draw_or_1(draw))
    end function pf_random_bits_at_i64

    !> `pf_random_int_at` for `integer(int32)` stream index and bounds.
    !!
    !! The result is inside `[min(lo,hi), max(lo,hi)]` by construction, so narrowing the `int64`
    !! worker's answer back to `int32` is exact and cannot overflow.
    pure elemental function pf_random_int_at_i32(seed, i, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int32) :: r                         !! a uniform integer in `[min(lo,hi), max(lo,hi)]`
        r = int(int_at_impl(seed, int(i, int64), int(lo, int64), int(hi, int64), draw_or_1(draw)), int32)
    end function pf_random_int_at_i32

    !> `pf_random_int_at` for `integer(int64)` stream index and bounds.
    pure elemental function pf_random_int_at_i64(seed, i, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based value index; absent means 1
        integer(int64) :: r                         !! a uniform integer in `[min(lo,hi), max(lo,hi)]`
        r = int_at_impl(seed, i, lo, hi, draw_or_1(draw))
    end function pf_random_int_at_i64

    !> `pf_random_fill_at` filling `real64` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_at_r64_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r64(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_at_r64_i32

    !> `pf_random_fill_at` filling `real64` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_at_r64_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r64(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_at_r64_i64

    !> `pf_random_fill_at` filling `real32` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_at_r32_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r32(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_at_r32_i32

    !> `pf_random_fill_at` filling `real32` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_at_r32_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r32(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_at_r32_i64

    ! ================================================================================
    ! Seeding and key derivation
    ! ================================================================================

    !> A fresh, nondeterministic seed, in `[1, huge(int64)]`.
    !!
    !! Folds the clock at its finest resolution together with a process-wide call counter, so two
    !! calls differ even when the clock has not ticked between them, and so do two calls racing on
    !! different threads -- the counter is incremented inside a named critical region. This is the
    !! one procedure in the module that is not a pure function, and the one that is deliberately
    !! not reproducible: a program that wants a reproducible run stores the value it got and passes
    !! that back next time.
    !!
    !! Two calls can only collide if their mixer outputs differ in nothing but the bit the range
    !! restriction clears -- one specific partner per call out of 2**64, which no run will meet.
    !!
    !! **Not cryptographic**, and not a substitute for one: the clock is guessable and the counter
    !! is small. For anything an adversary must not predict, take the seed from a CSPRNG instead.
    function pf_random_seed() result(s)
        integer(int64) :: s                         !! a nondeterministic seed in `[1, huge(int64)]`
        integer(int64) :: counted, ticks, rate, ceiling_
        !$omp critical (pf_random_seed_counter)
        seed_call_counter = seed_call_counter + 1_int64
        counted = seed_call_counter
        !$omp end critical (pf_random_seed_counter)
        call system_clock(count=ticks, count_rate=rate, count_max=ceiling_)
        s = mix64(ieor(mix64(ieor(ticks, rate)), counted))
        s = ibclr(s, 63)                            ! into [0, huge]; the mixer is otherwise a bijection
        if (s == 0_int64) s = 1_int64
    end function pf_random_seed

    !> `pf_random_key` for an `integer(int32)` label.
    pure elemental function pf_random_key_i32(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int32), intent(in) :: label         !! which derived family; sign-extends
        integer(int64) :: r                         !! an independent seed
        r = key_from(seed, int(label, int64))
    end function pf_random_key_i32

    !> `pf_random_key` for an `integer(int64)` label.
    pure elemental function pf_random_key_i64(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int64), intent(in) :: label         !! which derived family
        integer(int64) :: r                         !! an independent seed
        r = key_from(seed, label)
    end function pf_random_key_i64

    !> Reports which side of the route (e) fork was compiled. **Test-only.**
    !!
    !! Public only because it has to be: this module reaches no `bind(C)` surface, so the C++-side
    !! debug-hook convention the rest of the library uses is unavailable to it, and the fork is a
    !! compile-time source selection no runtime query could otherwise observe. It is excluded from
    !! README's API overview, no library code calls it, and it has no setter -- the fork is not a
    !! knob. Its whole job is to let one assertion close the allowlist's silent direction: this must
    !! agree with `selected_int_kind(38) > 0`, or a capable compiler is quietly shipping the
    !! wrapping kernel.
    pure function parquet_debug_random_uses_int128() result(r)
        logical :: r                                !! `.true.` if the 128-bit multiply was compiled
#ifdef PF_INT128
        r = .true.
#else
        r = .false.
#endif
    end function parquet_debug_random_uses_int128

    !> Runs the Philox block function directly, on raw counter and key words. **Test-only.**
    !!
    !! **This exists so the LIBRARY'S OWN kernel can be known-answer-checked against all three
    !! published Random123 vectors, rather than only the one the public API can reach.** The mapping
    !! puts the 0-based block index in counter words 0 and 1, and the block index comes from an
    !! `integer(int64)` draw — so it cannot exceed roughly `2**62`, while KAT 2 needs `0xffffffff`
    !! there and KAT 3 about `9.6e18`. Both are unreachable through `pf_random_at` and friends by
    !! construction, for any seed, stream or draw. Note the *key* is not restricted in the same way:
    !! it is derived from the seed by a bijection, so every 64-bit key is reachable; it is only the
    !! counter that is bounded.
    !!
    !! Without this, KAT 2 and KAT 3 could only be asserted against `test_random_reference.f90`, and
    !! the library kernel was tied to them transitively — directly known-answer-checked **once** and
    !! indirectly **twice**, through a reference that is itself checked three times. That chain is
    !! sound but it is a chain, and it leaves the one arithmetic that actually ships less directly
    !! evidenced than the reference written to check it. This hook makes all three direct.
    !!
    !! Public only because it has to be, exactly as `parquet_debug_random_uses_int128` is: this
    !! module reaches no `bind(C)` surface, so the C++-side debug-hook convention is unavailable to
    !! it. It is excluded from README's API overview, no library code calls it, and it is a pure
    !! observation with no setter — it cannot change what any draw returns.
    !!
    !! Arguments are the module's own coordinates, not Philox's four counter words: `key` splits
    !! into key words 0 and 1 (low half first), `stream` into counter words 2 and 3, and `index`
    !! into counter words 0 and 1. So KAT 2 — every counter and key word `0xffffffff` — is the call
    !! `parquet_debug_random_block(-1_int64, -1_int64, -1_int64, ...)`.
    pure subroutine parquet_debug_random_block(key, stream, index, w0, w1, w2, w3)
        integer(int64), intent(in) :: key           !! 64-bit key: key words 0 and 1, low half first
        integer(int64), intent(in) :: stream        !! splits into counter words 2 and 3
        integer(int64), intent(in) :: index         !! 0-based block index: counter words 0 and 1
        integer(int64), intent(out) :: w0           !! output word `c0`
        integer(int64), intent(out) :: w1           !! output word `c1`
        integer(int64), intent(out) :: w2           !! output word `c2`
        integer(int64), intent(out) :: w3           !! output word `c3`
        call random_block(key, stream, index, w0, w1, w2, w3)
    end subroutine parquet_debug_random_block

    ! ================================================================================
    ! The cipher
    ! ================================================================================

    !> Enciphers one Philox4x32-10 block: four 32-bit output words, each held in an `int64`.
    !!
    !! The state is four named scalars rather than a four-element array -- the array spelling
    !! measures 3.2x slower. The rounds are a loop rather than hand-unrolled, which ifx pays up to
    !! 2.37x for; that sign flips for the lane-blocked bulk kernels a later phase adds, which is
    !! why they will be separate procedures rather than a flag on this one.
    pure subroutine random_block(key, stream, index, w0, w1, w2, w3)
        integer(int64), intent(in) :: key           !! 64-bit key: a seed, or a retry key
        integer(int64), intent(in) :: stream        !! stream index; splits into counter words 2 and 3
        integer(int64), intent(in) :: index         !! 0-based block index; splits into counter words 0 and 1
        integer(int64), intent(out) :: w0           !! output word `c0`
        integer(int64), intent(out) :: w1           !! output word `c1`
        integer(int64), intent(out) :: w2           !! output word `c2`
        integer(int64), intent(out) :: w3           !! output word `c3`
        integer(int64) :: c0, c1, c2, c3, k0, k1, hi0, hi1, lo0, lo1
        integer :: r
#ifdef PF_INT128
        integer(k128) :: p0, p1
#else
        integer(int64) :: p0, p1
#endif
        k0 = iand(key, M32)
        k1 = iand(ishft(key, -32), M32)
        c0 = iand(index, M32)
        c1 = iand(ishft(index, -32), M32)
        c2 = iand(stream, M32)
        c3 = iand(ishft(stream, -32), M32)
        do r = 1, PHILOX_ROUNDS
#ifdef PF_INT128
            ! Both operands are below 2**32, so each product is below 2**64 and fits the wide kind
            ! with 63 bits to spare. Each half is extracted while still wide and is below 2**32, so
            ! narrowing back is exact -- no value at or above 2**63 is ever formed in an int64.
            p0 = int(PHILOX_M0, k128) * int(c0, k128)
            p1 = int(PHILOX_M1, k128) * int(c2, k128)
            hi0 = int(ishft(p0, -32), int64)
            lo0 = int(iand(p0, M32_128), int64)
            hi1 = int(ishft(p1, -32), int64)
            lo1 = int(iand(p1, M32_128), int64)
#else
            ! UB site 1 of 2: the true product can exceed huge(int64) and wraps. `ishft` is a
            ! LOGICAL shift, so it recovers the correct high half from the wrapped pattern. Shipped
            ! only where there is no 128-bit kind, on a compiler measured to wrap faithfully.
            p0 = PHILOX_M0 * c0
            p1 = PHILOX_M1 * c2
            hi0 = ishft(p0, -32)
            lo0 = iand(p0, M32)
            hi1 = ishft(p1, -32)
            lo1 = iand(p1, M32)
#endif
            c0 = ieor(ieor(hi1, c1), k0)
            c1 = lo1
            c2 = ieor(ieor(hi0, c3), k1)
            c3 = lo0
            ! The bump belongs between rounds, so round 10's is dead. Computing it anyway is
            ! cheaper than branching on the round number, and cannot change a value: nothing reads
            ! the key again. Both bumps are masked, so neither can overflow.
            k0 = iand(k0 + PHILOX_W0, M32)
            k1 = iand(k1 + PHILOX_W1, M32)
        end do
        w0 = c0
        w1 = c1
        w2 = c2
        w3 = c3
    end subroutine random_block

    !> Word `index` (0-based) of a stream: blocks 0, 1, 2, ... each giving `c0, c1, c2, c3`.
    pure function word_of(seed, stream, index) result(w)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: index         !! 0-based word index within the stream
        integer(int64) :: w                         !! that word, in `[0, 2**32)`
        integer(int64) :: c(0:3)
        call random_block(seed, stream, index / 4_int64, c(0), c(1), c(2), c(3))
        w = c(int(modulo(index, 4_int64), int32))
    end function word_of

    !> The 64-bit pattern of `real64`/raw-bits value `draw` (1-based) of a stream.
    !!
    !! Value `d` occupies words `2d-2` and `2d-1`, so a block carries TWO values: draw 1 takes the
    !! pair `(c0, c1)` and draw 2 the pair `(c2, c3)`. The first word is the LOW half. A later
    !! phase's draw-axis bulk fill is faster precisely because it can use both pairs of a block
    !! where a scalar draw uses one.
    pure function bits_of(seed, stream, draw) result(b)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: b                         !! the 64-bit pattern
        integer(int64) :: w0, w1, w2, w3
        call random_block(seed, stream, (draw - 1_int64) / 2_int64, w0, w1, w2, w3)
        if (modulo(draw - 1_int64, 2_int64) == 0_int64) then
            b = ior(ishft(w1, 32), w0)
        else
            b = ior(ishft(w3, 32), w2)
        end if
    end function bits_of

    !> The top 53 bits of a 64-bit pattern, as a `real64` in `[0, 1)`.
    !!
    !! Exact: 53 bits fit a `real64` significand without rounding, and scaling by a power of two
    !! is exact, so this is the rational `(bits >> 11) / 2**53` and nothing else. 1.0 is
    !! unreachable by construction; 0.0 occurs with probability 2**-53.
    pure function to_real64(bits) result(r)
        integer(int64), intent(in) :: bits          !! any 64-bit pattern
        real(real64) :: r                           !! `[0, 1)`
        r = real(ishft(bits, -11), real64) * 2.0_real64**(-53)
    end function to_real64

    !> The top 24 bits of one 32-bit word, as a `real32` in `[0, 1)`.
    !!
    !! Exact for the same reason as `to_real64`, and it cannot return 1.0 -- which a narrowing of a
    !! `real64` draw could, by rounding up. That is why the `real32` sequence is its own.
    pure function to_real32(w) result(r)
        integer(int64), intent(in) :: w             !! one Philox word, in `[0, 2**32)`
        real(real32) :: r                           !! `[0, 1)`
        r = real(ishft(w, -8), real32) * 2.0_real32**(-24)
    end function to_real32

    !> Resolves an optional `draw` to a valid 1-based value index.
    !!
    !! Absent means 1. A non-positive `draw` is a documented precondition violation, absorbed
    !! rather than reported: every tier-0 procedure is `pure elemental` and so has no way to abort,
    !! and a caller that has computed a bad index deserves a defined answer over a silent one. It
    !! must be clamped HERE rather than left to flow into the block arithmetic, where truncating
    !! division would map `draw = 0` onto `draw = 1` by accident instead of by rule -- and would
    !! map `draw = -1` somewhere else again.
    pure function draw_or_1(draw) result(d)
        integer(int64), intent(in), optional :: draw !! the caller's `draw`, present or not
        integer(int64) :: d                         !! a value index of at least 1
        d = 1_int64
        if (present(draw)) d = draw
        if (d < 1_int64) d = 1_int64
    end function draw_or_1

    ! ================================================================================
    ! Bulk fills
    ! ================================================================================

    !> Fills `v` with consecutive `real64` values of one stream, starting at `draw`.
    !!
    !! Walks blocks rather than values, taking both of a block's pairs where it can, so a fill of
    !! `m` values enciphers about `m/2` blocks where `m` scalar calls would encipher `m`. The
    !! values are identical to those scalar calls either way -- that is what makes a prefix a
    !! prefix.
    pure subroutine fill_r64(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: position, w0, w1, w2, w3
        ! Both counters and the length are int64, and `size` is asked for that kind EXPLICITLY.
        ! `size(v)` defaults to a default-kind result, which wraps for an array of 2**31 elements
        ! or more -- and this fails silently rather than loudly: a wrapped negative length returns
        ! through the guard below having written nothing, and a wrapped small positive length
        ! (2**32 + 8 elements yields 8) fills a short prefix and leaves the rest of the caller's
        ! `intent(out)` array undefined. Neither raises anything. A 2**31-element `real64` array is
        ! 17 GB, which is ordinary for the data this library exists to handle, and no unit test can
        ! reach it -- `check_fill_size_kind` in tools/check_source_conventions.py is what keeps this
        ! from regressing, with tools/test_random_large_fill.sh as the end-to-end proof.
        integer(int64) :: k, m
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        k = 0_int64
        position = draw
        do while (k < m)
            call random_block(seed, stream, (position - 1_int64) / 2_int64, w0, w1, w2, w3)
            if (modulo(position - 1_int64, 2_int64) == 0_int64) then
                k = k + 1_int64
                v(k) = to_real64(ior(ishft(w1, 32), w0))
                if (k < m) then
                    k = k + 1_int64
                    v(k) = to_real64(ior(ishft(w3, 32), w2))
                end if
            else
                k = k + 1_int64
                v(k) = to_real64(ior(ishft(w3, 32), w2))
            end if
            ! The guard is what keeps this from being a THIRD unguarded signed-overflow site, and
            ! it is not about invalid input. `k` has already reached `m` on the final pass, so an
            ! unguarded `draw + k` computes the position one PAST the last element -- which is
            ! `huge(int64) + 1` for a perfectly valid fill whose every requested position is
            ! representable. The result is dead (the loop exits immediately), which is exactly why
            ! the answers stayed right and why `-ftrapv` never trapped it; it is also exactly the
            ! situation `width_of` documents, where a compiler used a dead overflow's undefinedness
            ! to reason about live code elsewhere. `fill_r32` needs no such guard: it derives its
            ! position at the TOP of the loop, so it never forms an index past the last element.
            if (k < m) position = draw + k
        end do
    end subroutine fill_r64

    !> Fills `v` with consecutive `real32` values of one stream, starting at `draw`.
    !!
    !! Four values to a block, since a `real32` value is one word. Same contract as `fill_r64`:
    !! identical to the matching scalar calls, so prefixes agree.
    pure subroutine fill_r32(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: position, c(0:3)
        ! int64 counters and an explicit `kind=` on `size`, for the reason spelled out in
        ! `fill_r64`: a default-kind length wraps above 2**31 elements and fails silently, either
        ! writing nothing or writing a short prefix of the caller's `intent(out)` array.
        integer(int64) :: k, m
        integer :: slot                             ! 0..3 within one block; default kind is ample
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        k = 0_int64
        do while (k < m)
            position = draw + k - 1_int64                    ! 0-based word index
            call random_block(seed, stream, position / 4_int64, c(0), c(1), c(2), c(3))
            slot = int(modulo(position, 4_int64), int32)
            do while (slot <= 3 .and. k < m)
                k = k + 1_int64
                v(k) = to_real32(c(slot))
                slot = slot + 1
            end do
        end do
    end subroutine fill_r32

    ! ================================================================================
    ! The integer rule -- exact rejection
    ! ================================================================================

    !> A uniform integer in `[min(lo,hi), max(lo,hi)]`, exactly unbiased.
    !!
    !! Lemire's multiply-shift with the exact rejection test. A candidate is the block's low 64
    !! bits; `x * s` is formed as a full 128-bit product, whose high half is the answer's offset
    !! and whose low half decides acceptance. Only the last `2**64 mod s` candidates of the range
    !! are rejected, which is what makes the result exactly uniform rather than uniform to within
    !! 2**-64.
    !!
    !! A rejection RE-KEYS and re-enciphers the SAME counter, so consumption stays fixed in counter
    !! positions -- block ownership and prefix consistency are untouched and the answer is still a
    !! pure function of `(seed, i, draw)`. The loop is deliberately uncapped: accepting a candidate
    !! at a cap would reintroduce the bias the whole scheme exists to remove. It terminates with
    !! probability 1, the worst chain measured is 7, and at a range of a few million the retry
    !! probability is around 2**-40.
    !!
    !! **At the same coordinate this reads the SAME two words as `pf_random_at` and
    !! `pf_random_bits_at`, so the three are not independent draws.** The block index is `draw - 1`
    !! here and `(draw - 1)/2` there, and those coincide at draw 1 -- which is the default and by
    !! far the commonest call. They diverge from draw 2 onwards, where this takes block 1 while the
    !! real draws take block 0's second pair.
    !!
    !! The returned *value* differs, because Lemire's reduction is a different function of those
    !! bits and a rejection re-keys; but "different value" is not "independent", and at a small
    !! range the integer is a **deterministic function** of the real. Measured on two machines and
    !! two architectures: `pf_random_int_at(seed, i, 1, 6)` equals `1 + floor(6 * pf_random_at(seed,
    !! i))` for **20000 of 20000** streams, against 1-in-6 when the integer is taken at draw 2. So a
    !! caller wanting one real and one integer per iteration must separate them on the draw axis or
    !! with `pf_random_key`, exactly as `doc/pages/utilities/random.md` says.
    !!
    !! An earlier version of this comment opened "Unlike `pf_random_at` and `pf_random_bits_at`,
    !! which read the same two words as each other" -- asserting that this procedure does *not*.
    !! It does. Do not restore that reading; and note the rejection clause cannot rescue it, since
    !! at a realistic range the retry probability is around 2**-40, so the no-rejection case is
    !! effectively the only case.
    pure function int_at_impl(seed, stream, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: a, b, s, x, low, high, threshold, index, attempt
        integer(int64) :: w0, w1, w2, w3

        a = min(lo, hi)                             ! `lo > hi` swaps: the function is total
        b = max(lo, hi)
        s = width_of(a, b)                          ! the width, read as an UNSIGNED 64-bit pattern
        index = draw - 1_int64

        if (s == 0_int64) then
            ! The whole int64 range: every pattern is in range, so there is nothing to reduce and
            ! nothing to reject. At draw 1 this is exactly `pf_random_bits_at`.
            call random_block(seed, stream, index, w0, w1, w2, w3)
            r = ior(ishft(w1, 32), w0)
            return
        end if

        attempt = 0_int64
        call random_block(seed, stream, index, w0, w1, w2, w3)
        x = ior(ishft(w1, 32), w0)
        call mulhilo64(x, s, low, high)
        if (ult(low, s)) then
            ! Lemire's lazy threshold: the division is reached only when the candidate falls in the
            ! last partial block, which at a realistic range is about never. Do not hoist it.
            threshold = umod_2p64(s)
            do while (ult(low, threshold))
                attempt = attempt + 1_int64
                call random_block(retry_key_of(seed, attempt), stream, index, w0, w1, w2, w3)
                x = ior(ishft(w1, 32), w0)
                call mulhilo64(x, s, low, high)
            end do
        end if
        r = offset_by(a, high)
    end function int_at_impl

    !> The retry key for attempt `n` (1-based): a re-key, never a tweak.
    !!
    !! Two simpler spellings were measured and both alias real seed pairs -- an additive tweak
    !! collides one fixed-offset partner on a third of wide-range draws, and `mix64(ieor(seed, n))`
    !! collides EVERY consecutive (even, odd) seed pair, which is exactly what `seed = base + i`
    !! produces. This form measures zero agreements on every pair tested. It is contract, and must
    !! not be simplified.
    pure function retry_key_of(seed, n) result(k)
        integer(int64), intent(in) :: seed          !! the original seed
        integer(int64), intent(in) :: n             !! attempt number, 1 for the first retry
        integer(int64) :: k                         !! the key to re-encipher this counter under
        k = mix64(ieor(mix64(seed), ieor(n, RETRY_TAG)))
    end function retry_key_of

    !> `a - b` as a 64-bit pattern, computed on 32-bit halves so that nothing overflows.
    !!
    !! See `width_of` for why this exists rather than a plain `a - b`.
    !!
    !! **This and `add64` are called only from the `#else` arm of the route (e) fork, so on a
    !! compiler that has a 128-bit kind they are compiled and never entered.** Coverage here is
    !! measured under gfortran, which has one, so both will always report as uncovered -- and both
    !! are live under ifx, where they are the fix for the branch-deletion defect `width_of`
    !! describes. Neither is dead code; do not delete either on the strength of a coverage report.
    !! What asserts them is the suite's cross-implementation agreement sweep, run by a compiler that
    !! takes this arm.
    pure function sub64(a, b) result(r)
        integer(int64), intent(in) :: a             !! left operand, read as a bit pattern
        integer(int64), intent(in) :: b             !! right operand, read as a bit pattern
        integer(int64) :: r                         !! `a - b` modulo 2**64
        integer(int64) :: low, high
        low = iand(a, M32) - iand(b, M32)           ! strictly inside (-2**32, 2**32)
        high = ishft(a, -32) - ishft(b, -32)        ! both shifts are logical, so both are >= 0
        if (low < 0_int64) then
            low = low + 4294967296_int64
            high = high - 1_int64
        end if
        r = ior(ishft(iand(high, M32), 32), low)
    end function sub64

    !> `a + b` as a 64-bit pattern, computed on 32-bit halves so that nothing overflows.
    !!
    !! Reached only from the fork's `#else` arm, exactly as `sub64` is -- see its note on why this
    !! reports as uncovered on every compiler this project measures coverage with.
    pure function add64(a, b) result(r)
        integer(int64), intent(in) :: a             !! left operand, read as a bit pattern
        integer(int64), intent(in) :: b             !! right operand, read as a bit pattern
        integer(int64) :: r                         !! `a + b` modulo 2**64
        integer(int64) :: low, high
        low = iand(a, M32) + iand(b, M32)           ! below 2**33
        high = ishft(a, -32) + ishft(b, -32) + ishft(low, -32)   ! below 2**33 + 1
        r = ior(ishft(iand(high, M32), 32), iand(low, M32))
    end function add64

    !> `hi - lo + 1` as an unsigned 64-bit pattern; 0 means the full `int64` range.
    !!
    !! **This must not be written as the obvious `hi - lo + 1`, and the reason is a measured ifx
    !! finding rather than a precaution.** That subtraction overflows for any width above 2**63,
    !! and the wrapped pattern is exactly what is wanted -- but the overflow is undefined, and a
    !! compiler entitled to assume it cannot happen can also assume the result is positive, since
    !! `hi >= lo` here by construction. ifx 2026.1.1 does precisely that: it computes the width
    !! correctly and then **deletes `umod_2p64`'s `s < 0` branch as provably dead**, so a width at
    !! or above 2**63 takes the narrow-width path and produces a wrong rejection threshold. The
    !! draws stay inside `[lo, hi]` and stay plausible, so nothing but a comparison against an
    !! independent implementation notices.
    !!
    !! The lesson generalises past this one function: verifying that a compiler *wraps* an
    !! overflowing expression is not the same as verifying that it does not use the overflow's
    !! undefinedness to reason about the result somewhere else entirely.
    pure function width_of(lo, hi) result(s)
        integer(int64), intent(in) :: lo            !! the low end, already ordered
        integer(int64), intent(in) :: hi            !! the high end, already ordered
        integer(int64) :: s                         !! the width, read as unsigned
#ifdef PF_INT128
        ! The true width is up to 2**64, which no int64 can hold, so it is formed wide and folded
        ! into the two's-complement pattern that represents it. Nothing overflows.
        integer(k128) :: w
        w = iand(int(hi, k128) - int(lo, k128) + 1_k128, MASK64_128)
        if (w >= TWO63_128) w = w - TWO64_128
        s = int(w, int64)
#else
        s = add64(sub64(hi, lo), 1_int64)
#endif
    end function width_of

    !> `base + offset`, where `offset` is an unsigned 64-bit pattern known to keep the sum in range.
    pure function offset_by(base, offset) result(r)
        integer(int64), intent(in) :: base          !! the range's low end
        integer(int64), intent(in) :: offset        !! `high64(x*s)`, unsigned and below the width
        integer(int64) :: r                         !! a value inside the closed range
#ifdef PF_INT128
        ! The offset is below the width and the width is at most 2**64, so the mathematical sum is
        ! inside [lo, hi] and the narrowing is exact. Recovering the offset's unsigned value is a
        ! mask rather than a branch.
        r = int(int(base, k128) + iand(int(offset, k128), MASK64_128), int64)
#else
        ! Same reasoning as width_of: the sum overflows whenever the offset's pattern is negative,
        ! and the halves form is what keeps that from being undefined.
        r = add64(base, offset)
#endif
    end function offset_by

    !> `2**64 mod s`, for `s` read as an unsigned 64-bit pattern -- including at and above `2**63`.
    !!
    !! The natural spelling of this is correct only below `2**63`; above it, it computes a
    !! too-large threshold for two thirds of widths, which silently makes up to HALF of the
    !! requested range unreachable. That failure is invisible to every obvious test: the returned
    !! values are all still inside `[lo, hi]`, and they are still uniform over the values that do
    !! occur, so containment and chi-square both pass. Only a comparison against an independent
    !! reference over wide widths sees it.
    pure function umod_2p64(s) result(t)
        integer(int64), intent(in) :: s             !! the width, unsigned and non-zero
        integer(int64) :: t                         !! `2**64 mod s`
        integer(int64) :: a, h, r
        if (s < 0_int64) then
            ! s >= 2**63 unsigned. Then 2**64 - s <= 2**63 <= s, so the remainder IS 2**64 - s and
            ! there is nothing to reduce. `-s` is that pattern, and is representable for every such
            ! s except s == 2**63 exactly, where negation would overflow -- and where 2**63 divides
            ! 2**64, so the answer is 0. That value is taken out first.
            if (s == -huge(1_int64) - 1_int64) then
                t = 0_int64
            else
                t = -s
            end if
            return
        end if
        a = -s                                      ! the two's-complement pattern for 2**64 - s
        h = ishft(a, -1)                            ! floor((2**64 - s)/2); logical shift, so >= 0
        r = mod(h, s)
        if (r >= s - r) then                        ! double it modulo s without ever leaving [0, s)
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

    !> The full 128-bit product of two unsigned 64-bit patterns, as a low and a high half.
    !!
    !! UB site 2 of 2, and the only one carried on BOTH sides of the fork: each 32x32 partial
    !! product can exceed `huge(int64)` and wrap. Route (e) does not cover it, and a strictly
    !! overflow-free spelling was measured at 1.31x on gfortran and 2.05x on ifx for the whole
    !! integer path -- far more than any other fix here, because the cost is the product itself
    !! (four 32x32 replaced by sixteen 16x16) rather than a few range operations. It is carried
    !! deliberately and guarded by the suite's cross-implementation agreement sweep, which is the
    !! only thing that would notice a compiler starting to exploit it.
    !!
    !! That guard is now the ONLY protection this site has. The width arithmetic used to be a
    !! third such site, carried on the same reasoning -- that ifx had been verified to wrap -- and
    !! ifx was then caught using the overflow's undefinedness to delete a branch somewhere else
    !! entirely (see `width_of`). Nothing in that finding says this site is safe; it says the
    !! evidence thought to make it safe was never evidence about this question. Treat a future
    !! agreement failure here as the expected outcome rather than as a surprise.
    pure subroutine mulhilo64(a, b, low, high)
        integer(int64), intent(in) :: a             !! one factor, read as unsigned
        integer(int64), intent(in) :: b             !! the other factor, read as unsigned
        integer(int64), intent(out) :: low          !! bits 0..63 of the product
        integer(int64), intent(out) :: high         !! bits 64..127 of the product
        integer(int64) :: a0, a1, b0, b1, p00, p01, p10, p11, mid, mid2
        a0 = iand(a, M32)
        a1 = ishft(a, -32)
        b0 = iand(b, M32)
        b1 = ishft(b, -32)
        p00 = a0 * b0
        p01 = a0 * b1
        p10 = a1 * b0
        p11 = a1 * b1
        mid = p10 + ishft(p00, -32)
        mid2 = iand(mid, M32) + p01
        low = ior(ishft(mid2, 32), iand(p00, M32))
        high = p11 + ishft(mid, -32) + ishft(mid2, -32)
    end subroutine mulhilo64

    !> Unsigned `a < b` for two 64-bit patterns.
    !!
    !! Flipping the sign bit of both maps the unsigned order onto the signed one, so this is two
    !! bit operations and a comparison -- it cannot overflow, and needs no wide kind.
    pure function ult(a, b) result(r)
        integer(int64), intent(in) :: a             !! left operand, read as unsigned
        integer(int64), intent(in) :: b             !! right operand, read as unsigned
        logical :: r                                !! `.true.` if `a < b` as unsigned
        r = ieor(a, SIGN_BIT) < ieor(b, SIGN_BIT)
    end function ult

    ! ================================================================================
    ! The mixer
    ! ================================================================================

    !> The SplitMix64 finaliser: a bijection on 64 bits with good avalanche.
    !!
    !! `mix64(0) == 0`, which is why `pf_random_key(0, 0)` is 0 and why every seed has exactly one
    !! label whose derived key is 0. Surprising, not a defect: a bijection has to send something to
    !! zero, and hiding it would cost the bijection.
    pure function mix64(x) result(z)
        integer(int64), intent(in) :: x             !! any 64-bit pattern
        integer(int64) :: z                         !! the mixed pattern
        z = x
        z = mul64_lo_strict(ieor(z, ishft(z, -30)), MIX_A)
        z = mul64_lo_strict(ieor(z, ishft(z, -27)), MIX_B)
        z = ieor(z, ishft(z, -31))
    end function mix64

    !> The low 64 bits of `a * b`, computed on 16-bit limbs so that nothing ever overflows.
    !!
    !! **This spelling is a blocker-grade requirement, not a precaution.** Written as a plain
    !! `int64` multiply, gfortran folds the whole of `mix64` at `-O2` AND `-O3`, on three
    !! architectures and two major versions, to the single constant `z'7FFFFFFF00000000'` for every
    !! input -- so `pf_random_key` would return one key for every seed and every label, with no
    !! abort, no warning, and downstream output that still looks random.
    !!
    !! Splitting into 32x32 products is NOT sufficient: a 32x32 product still exceeds `int64`.
    !! On 16-bit limbs nothing exceeds 2**35, so this is correct by construction on any compiler at
    !! any optimisation level, and needs no 128-bit kind -- which is what makes it available to the
    !! compiler that has none. It costs about 10 ns, paid once per stream family, never per draw.
    pure function mul64_lo_strict(a, b) result(r)
        integer(int64), intent(in) :: a             !! one factor
        integer(int64), intent(in) :: b             !! the other factor
        integer(int64) :: r                         !! bits 0..63 of the product
        integer(int64) :: a0, a1, a2, a3, b0, b1, b2, b3, acc, d0, d1, d2, d3
        a0 = iand(a, M16)
        a1 = iand(ishft(a, -16), M16)
        a2 = iand(ishft(a, -32), M16)
        a3 = iand(ishft(a, -48), M16)
        b0 = iand(b, M16)
        b1 = iand(ishft(b, -16), M16)
        b2 = iand(ishft(b, -32), M16)
        b3 = iand(ishft(b, -48), M16)
        ! Column by column, carrying as we go. The widest accumulator value is below 2**35, so no
        ! partial product, sum or carry can reach the sign bit.
        acc = a0 * b0
        d0 = iand(acc, M16)
        acc = ishft(acc, -16) + a0 * b1 + a1 * b0
        d1 = iand(acc, M16)
        acc = ishft(acc, -16) + a0 * b2 + a1 * b1 + a2 * b0
        d2 = iand(acc, M16)
        acc = ishft(acc, -16) + a0 * b3 + a1 * b2 + a2 * b1 + a3 * b0
        d3 = iand(acc, M16)
        r = ior(ior(d0, ishft(d1, 16)), ior(ishft(d2, 32), ishft(d3, 48)))
    end function mul64_lo_strict

    !> `pf_random_key`'s derivation, shared by both label kinds.
    pure function key_from(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int64), intent(in) :: label         !! which derived family
        integer(int64) :: r                         !! an independent seed
        r = mix64(ieor(mix64(seed), label))
    end function key_from

end module parquet_random
