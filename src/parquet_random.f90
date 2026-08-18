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
    use, intrinsic :: ieee_arithmetic, only: ieee_is_nan, ieee_is_finite
    ! parquet_settings_base, NOT parquet_settings: that one imports parquet_bindings to mirror the
    ! C++-side knobs. The leaf module exists for exactly this, and parquet_strings reaches it the
    ! same way -- see its own header.
    !
    ! **This module is NO LONGER free of the Arrow link dependency, and that was a deliberate
    ! decision rather than drift.** `pf_weighted_permutation` sorts its keys, and the project's one
    ! sort lives in `parquet_sorting`, which imports `parquet_bindings` -- so a program whose only
    ! dependency is this generator must now link `parquet_wrapper.cpp` and hence Arrow. Two things
    ! make the trade defensible: no C++ actually RUNS for `pf_argsort`, whose engine has been the
    ! Fortran one in `parquet_sorting_engine.f90` since the cutover, so this is a link-time cost
    ! only; and the alternative was a second copy of a sorting algorithm inside this module, which
    ! is a far worse thing to own than an unused link edge. The `parquet_settings_base` split above
    ! is still worth keeping: it is what stops the SETTINGS half dragging the same edge in for
    ! every program that merely draws a random number.
    use parquet_sorting, only: pf_argsort
    use parquet_expkey, only: exp_key, exp_key_contract_ok
    use parquet_settings_base, only: parquet_get_random_threads, &
                                     parquet_get_random_parallel_min_elements, &
                                     parquet_auto_thread_count, parquet_nested_team_unsafe

    implicit none
    private

    public :: pf_random_algorithm
    public :: pf_random_at
    public :: pf_random32_at
    public :: pf_random_bits_at
    public :: pf_random_int_at
    public :: pf_random_fill_draws
    public :: pf_random_fill_streams
    public :: pf_random_seed
    public :: pf_random_key
    public :: parquet_debug_random_uses_int128
    public :: parquet_debug_random_bulk_threads
    public :: parquet_debug_random_block
    public :: parquet_debug_set_perm_rounds
    public :: parquet_debug_set_perm_parity
    public :: parquet_debug_set_perm_force_feistel
    public :: parquet_debug_perm_config
    public :: pf_random_perm_algorithm
    public :: pf_random_perm_at
    public :: pf_random_permutation
    public :: pf_random_subset
    public :: pf_random_resample
    public :: pf_weighted_subset
    public :: pf_weighted_permutation

    !> Identifies the algorithm together with every mapping this module freezes -- the cipher, the
    !! key and counter layout, the word order, the integer rule and its retry key. Its value changes
    !! if and only if one of those changes, so a program that records it can tell whether a stored
    !! result is still reproducible. Identical on both sides of the route (e) fork, which changes
    !! how a product is formed and never what it equals.
    !!
    !! `/v2` differs from `/v1` in exactly one mapping: `pf_random_int_at`'s draw axis, which had
    !! stride 4 (a whole block per value, half of it unused) and now has stride 2, agreeing with
    !! `pf_random_at` and `pf_random_bits_at`. Draw 1 is unchanged; draws from 2 up moved. The
    !! cipher, the key and counter layout, the word order, the rejection rule and the retry key are
    !! all identical between the two.
    character(len=*), parameter :: pf_random_algorithm = "philox4x32-10/v2"

    ! ---- Philox4x32-10 constants (verified against Random123) ----

    !> Low 32 bits set: masks a 64-bit register down to one Philox word.
    integer(int64), parameter :: M32 = 4294967295_int64
    !> The widest range the 32-bit candidate rule is allowed to serve. A block is four 32-bit words,
    !! so a narrow draw costs a quarter of an enciphering instead of a half; the price is that the
    !! rejection rate is `(2**32 mod s)/2**32`, which rises with `s` and reaches 33.3 % just above
    !! `2**32/3`. Capping the WIDTH at `2**24` bounds the worst case over every admitted `s` at
    !! 0.389 %, at `s = 16 711 936`. See `feature_random_resample.md` sections 5 and 19.4 -- and note
    !! the cap is on the width `hi - lo + 1`, never on `size(idx)`, which are the same number for a
    !! resample and different for everything else.
    integer(int64), parameter :: NARROW32_CAP = 16777216_int64
    !> `2**32`, the modulus of the 32-bit rejection threshold.
    integer(int64), parameter :: TWO32 = 4294967296_int64
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

    ! ---- The permutation bijection (see `pf_random_perm_at`) ----

    !> Identifies the PERMUTATION contract, separately from `pf_random_algorithm`.
    !!
    !! Two identifiers rather than one, deliberately. `pf_random_algorithm` names the draw grid's
    !! mappings; the permutation is a different construction with its own kernel, so folding it into
    !! that string would tell every program which recorded it that its stored *draws* had changed
    !! when only the permutation had. This one covers exactly five things and nothing else: the
    !! construction (a Feistel network with cycle-walking), the width rule (`Z_a x Z_b` with
    !! `a = ceil(sqrt(m))`), the round count, the round function, and the round-key derivation.
    !!
    !! **The construction is piecewise and the identifier says so.** `exact20` records that
    !! `m <= perm_exact_max` does not use the Feistel at all -- it is ranked and unranked, and is
    !! therefore exactly uniform rather than approximately so -- while `16p` records the round count
    !! and the parity correction the Feistel carries above that threshold. All four facts fix the
    !! answer, so all four belong in the string; a program that recorded `feistel-mix2-4/zaxzb/v1`
    !! must see a different string, because every value it stored has changed.
    !!
    !! **`/v2`'s values changed once, after this string existed, and the string deliberately did not
    !! move -- read this before concluding the identifier is unreliable.** Folding `m` into the
    !! parity key (`perm_parity_flip`) changed what `/v2` answers for the populations where the
    !! network is parity-locked. The maintainer's decision was to apply that fix under `/v2` rather
    !! than bump to `/v3`, on the grounds that `/v2` was found and corrected inside internal testing
    !! and never reached a released version or an outside user, so no stored value anywhere
    !! disagrees with the current one. **That reasoning expires the moment `/v2` ships**: from the
    !! first release carrying it, any change to what it answers requires a new string, and the
    !! exception recorded here is not a precedent for one taken afterwards.
    character(len=*), parameter :: pf_random_perm_algorithm = "feistel-mix2-16p/zaxzb/exact20/v2"

    !> Feistel rounds. **Sixteen, and it must stay even.**
    !!
    !! Even because the two factors swap places every round, so an odd count leaves the state in
    !! `Z_b x Z_a` and the output encoded against transposed factors -- the value would depend on the
    !! parity of the round count in a way nothing else in this module does.
    !!
    !! **Sixteen because four was measured wrong, and because the sizes that would have forced a
    !! higher count no longer reach this kernel at all.** Four was chosen against a *marginal*
    !! distinguisher -- pairs of inputs sharing a component, asking whether their outputs share one
    !! -- and a marginal statistic cannot see the deficit that matters. An all-cells chi-square over
    !! every one of the `m!` permutations scores **z = 27729 at m = 5** against four rounds: the
    !! construction is a long way from uniform at small `m`, and nothing that looks at one position
    !! at a time can tell.
    !!
    !! The requirement is set by the *narrower* Feistel half, not by `m`: exhaustively, half-width 2
    !! needs 20 rounds, **half-width 3 needs 16**, and half-width 5 and above is clean at 8. Because
    !! `m <= perm_exact_max` is answered exactly (see `perm_exact`), every `m` that reaches this
    !! kernel has `min(a, b) >= 5` -- so 16 gives the network the count a strictly *harder* class
    !! than any it can meet requires, two half-width classes of margin, anchored on an exhaustive
    !! measurement rather than on an extrapolation.
    !!
    !! **This is correctness, not tuning, and it may not become a setting** -- see `perm_parity_bit`
    !! for the other half of the same statement, and `feature_risks.md` for what fails silently if
    !! either is lowered. `parquet_debug_set_perm_rounds` exists so a test can weaken it deliberately
    !! and show that the gates have power; nothing in normal operation may vary it.
    integer, parameter :: perm_rounds = 16

    !> Largest `m` answered EXACTLY, by ranking rather than by enciphering. See `perm_exact`.
    !!
    !! `20! = 2432902008176640000` fits an `integer(int64)` and `21!` does not, so this is arithmetic
    !! rather than policy: it is the largest population whose permutations can be addressed one-to-one
    !! by a 64-bit rank. **It is not a tuning knob and must never become one** -- it fixes what the
    !! library answers, which is why it appears in `pf_random_perm_algorithm`.
    integer, parameter :: perm_exact_max = 20

    !> `0!` through `perm_exact_max!`, for the ranking. Every entry is below `huge(int64)`.
    integer(int64), parameter :: perm_fact(0:perm_exact_max) = [ &
        1_int64, 1_int64, 2_int64, 6_int64, &
        24_int64, 120_int64, 720_int64, 5040_int64, &
        40320_int64, 362880_int64, 3628800_int64, 39916800_int64, &
        479001600_int64, 6227020800_int64, 87178291200_int64, 1307674368000_int64, &
        20922789888000_int64, 355687428096000_int64, 6402373705728000_int64, 121645100408832000_int64, &
        2432902008176640000_int64]

    !> `pf_random_key` label separating the permutation family's rank from every draw family.
    !!
    !! The exact path spends one `pf_random_int_at` draw on its rank. Taken at the caller's own seed
    !! it would be the *same* value that caller gets from `pf_random_int_at(seed, 1, ...)`, so a
    !! program using both would find its permutation locked to its first integer draw -- the same
    !! class of defect as a shared grid, and invisible to every test that looks at one family alone.
    integer(int64), parameter :: perm_family_label = 6813122891117395759_int64

    !> Elements enciphered together in the bulk fill's transposed loop. **Speed only.**
    !!
    !! The round loop runs OUTSIDE this block and the element loop inside it, so each round is a
    !! straight-line pass over `perm_block` independent values instead of one value's 16-round
    !! dependency chain. That is the standard counter-mode shape and it is worth 2.07x on machine B
    !! at plain `-O3` with **no vector instruction at all** -- pure instruction-level parallelism,
    !! so the gain is not an ISA assumption -- and 10.77x once the compiler is allowed AVX-512.
    !!
    !! **It changes no value**, which is the property that lets it be chosen freely: the arithmetic
    !! is identical and only its order is not. 64 is comfortably past the point where the chain is
    !! hidden and still a 1.5 KB stack footprint.
    integer, parameter :: perm_block = 64

    !> Key-schedule index the parity bit is drawn from. **Fixed, and deliberately not `perm_rounds`.**
    !!
    !! Keying it on the effective round count would make `parquet_debug_set_perm_rounds` change two
    !! things at once, so a round-count negative control could no longer say which of them it had
    !! detected. A constant index one past the largest round keeps each hook to one variable.
    integer, parameter :: perm_parity_key = perm_rounds + 1

    !> First `mix2` multiplier: the odd 32-bit golden-ratio constant, as used by xxHash and others.
    integer(int64), parameter :: perm_c1 = 2654435761_int64
    !> Second `mix2` multiplier: xxHash's PRIME32_2. Odd, below `2**32`, and well studied.
    integer(int64), parameter :: perm_c2 = 2246822519_int64
    !> Low 31 bits set. **Load-bearing, not decoration** -- see `perm_mix2`.
    integer(int64), parameter :: perm_m31 = 2147483647_int64
    !> Largest `a` whose square is representable, so `a * a` in `perm_factors` cannot overflow.
    integer(int64), parameter :: perm_a_max = 3037000499_int64

    !> Process-wide call counter behind `pf_random_seed`.
    integer(int64), save :: seed_call_counter = 0_int64

    ! ---- Test-only overrides of the permutation kernel ----
    !
    ! **These are the module's only other mutable state, they are process-global, and no library
    ! code reads them outside the three accessors below.** They exist so a test can weaken the
    ! kernel deliberately and demonstrate that a gate has power: a uniformity test asserting only
    ! that the shipped configuration is clean cannot distinguish a correct kernel from a loose
    ! test, which is exactly the error that let four rounds ship. See `parquet_debug_set_perm_rounds`.
    !
    ! They are read from `pure` procedures, which the standard permits -- purity forbids *defining*
    ! a host- or use-associated variable, not referencing one. The consequence to keep in mind is
    ! that a compiler may legally common up two calls with identical arguments across a change of
    ! these values; every test that sets them therefore asserts the two configurations DISAGREE, so
    ! a hoisted call fails loudly instead of quietly reporting a pass.

    !> Forced Feistel round count; `0` means the compiled-in `perm_rounds`.
    integer, save :: perm_dbg_rounds = 0
    !> `.true.` suppresses the parity correction. See `perm_parity_bit`.
    logical, save :: perm_dbg_no_parity = .false.
    !> `.true.` sends `m <= perm_exact_max` through the Feistel instead of the exact path.
    logical, save :: perm_dbg_force_feistel = .false.

    !> One uniform `real64` in `[0, 1)`: value `draw` (default 1) of stream `i` under `seed`.
    !!
    !! `i` is `integer(int32)` or `integer(int64)`; the two give identical values, because an
    !! `int32` stream sign-extends before it reaches the counter. `seed` and `draw` are always
    !! `integer(int64)`, so an explicit draw literal is written `3_int64`.
    interface pf_random_at
        module procedure pf_random_at_i32
        module procedure pf_random_at_i64
    end interface pf_random_at

    ! ---- ONE STREAM IS ONE SEQUENCE OF WORDS, AND real32 WALKS A FINER GRID ----
    !
    ! This is the module's single most important cross-cutting fact and the one a caller is most
    ! likely to get wrong, so it is stated once here rather than a third of it on each generic.
    !
    ! A `(seed, i)` pair names one deterministic sequence of 32-bit Philox words. The four
    ! coordinate-addressed generics are VIEWS of that one sequence, and two strides exist:
    !
    !   generic                                    words read for draw d (1-based)   stride
    !   pf_random32_at(seed, i, d)                 d-1                               1
    !   pf_random_at / pf_random_bits_at(.., d)    2d-2, 2d-1                        2
    !   pf_random_int_at(seed, i, lo, hi, d)       2d-2, 2d-1                        2
    !
    ! Two rules follow, and together they are the whole story:
    !
    !   1. The three 64-bit generics AGREE on what draw `d` means. At one coordinate they are three
    !      presentations of the same 64 bits -- not independent draws -- and at different draws they
    !      are independent. So "walk the draw axis" IS a safe rule among them.
    !   2. `pf_random32_at` has its own finer grid, one word per value, and it deliberately is not a
    !      narrowing of `pf_random_at`. Its draws `2d-1` and `2d` are the two halves of 64-bit draw
    !      `d`, so mixing it with the others on ONE stream still aliases across draw indices:
    !
    !        pf_random32_at(seed, i, 2d-1)  ==  the LOW word of bits_at(seed, i, d)   500 of 500
    !
    ! The three constructions that cannot alias at all are: a separate STREAM index, a separate
    ! family from `pf_random_key`, or `pf_random_stream`, which tracks its own word cursor.
    !
    ! **`pf_random_int_at` had stride 4 until `pf_random_algorithm` reached `/v2`**, addressing the
    ! whole of block `d-1` and using its first pair -- which made it equal `pf_random_bits_at` at
    ! draw `2d-1`, so rule 1 above was false and an integer at draw 2 collided with a real at draw
    ! 3. Aligning the strides fixed that, halved the cost of a draw-axis integer fill, and left draw
    ! 1 bit-identical. See `feature_risks.md` Risk-113.
    !
    ! Domain-separating the generics -- folding a per-generic constant into the key -- would remove
    ! rule 2 as well, and would break the property `pf_random_stream` exists to provide: that a
    ! stream hands out exactly the values the coordinate-addressed calls give at the same positions
    ! (`test_stream_values`). That correspondence is possible only because every generic reads one
    ! word space. It was considered and declined for that reason.

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
    !!
    !! **An INTEGER `v` takes two further required arguments, `lo` and `hi`**, which share `v`'s kind
    !! -- `call pf_random_fill_draws(seed, i, v, lo, hi [, draw])`. Element `k` is then exactly
    !! `pf_random_int_at(seed, i, lo, hi, draw+k-1)`, the same identity the real forms have with
    !! `pf_random_at`. The specifics are distinguishable on `v`'s type alone, so the generic resolves
    !! without ambiguity, and `lo > hi` is swapped rather than refused, exactly as in the scalar draw.
    !!
    !! **The integer form amortises much as `real64` does**: an integer draw has stride 2, so
    !! consecutive draws pair up two to a block and the fill enciphers once for each pair. Measured
    !! on machine B (gfortran 15.2.1, `-O3 -funroll-loops`, 4M values, best of three alternating
    !! rounds against the committed `/v1` build, with the `real64` fill flat at 8.66-8.69 ns as a
    !! cross-build control): **25.21-25.32 ns per value before, 15.74-15.76 after, 1.61x**. It was
    !! not always so -- see `pf_random_int_at`'s own note on the stride change that made it possible.
    interface pf_random_fill_draws
        module procedure pf_random_fill_draws_r64_i32
        module procedure pf_random_fill_draws_r64_i64
        module procedure pf_random_fill_draws_r32_i32
        module procedure pf_random_fill_draws_r32_i64
        module procedure pf_random_fill_draws_i32_i32
        module procedure pf_random_fill_draws_i32_i64
        module procedure pf_random_fill_draws_i64_i32
        module procedure pf_random_fill_draws_i64_i64
    end interface pf_random_fill_draws

    !> Fills `v` with ONE draw of each of `size(v)` consecutive streams, starting at stream `i0`.
    !!
    !! The other axis. Where `pf_random_fill_draws` fixes the stream and walks the draws, this fixes
    !! the draw and walks the streams -- so it is the bulk form of the loop this module's own
    !! documentation opens with, `x(i) = pf_random_at(seed, i)`. Element `k` is exactly
    !! `pf_random_at(seed, i0 + k - 1 [, draw])`, so the two forms are interchangeable and a prefix
    !! is a prefix.
    !!
    !! `v` is a rank-1 `real(real64)` or `real(real32)` array, `intent(out)`; `i0` is
    !! `integer(int32)` or `integer(int64)`; `seed` and `draw` are `integer(int64)`, and `draw`
    !! (default 1) is which draw of every one of those streams to take. A zero-sized `v` is a
    !! defined no-op.
    !!
    !! **Precondition, on the STREAM axis this time: `i0 + size(v) - 1` must not exceed
    !! `huge(int64)`.** Same reasoning as the draw-axis fill's own precondition -- the last element's
    !! stream has to be nameable. Note `i0` may be negative, and usually the whole range is nowhere
    !! near the boundary.
    !!
    !! **It cannot be as cheap per value as `pf_random_fill_draws`, and that is the contract rather
    !! than the implementation.** Each stream needs its own block, and one `real64` value consumes
    !! two of that block's four words -- the other two belong to draw 2 of the same stream, which
    !! this call is not asking for. A `real32` value consumes one of four. So where the draw-axis
    !! fill amortises one enciphering over two (or four) values, this one enciphers per value and
    !! wins only by removing the per-element call. Measured on machine B against the scalar loop it
    !! replaces: **1.28x on gfortran and 1.48x on ifx** for `real64`, **1.47x and 1.60x** for
    !! `real32`. When several values per stream
    !! are wanted, `pf_random_fill_draws` remains much the cheaper shape.
    !!
    !! **An INTEGER `v` takes two further required arguments, `lo` and `hi`**, which share `v`'s kind
    !! -- `call pf_random_fill_streams(seed, i0, v, lo, hi [, draw])`. Element `k` is exactly
    !! `pf_random_int_at(seed, i0+k-1, lo, hi [, draw])`. This axis was already one enciphering per
    !! value for the real kinds, so unlike the draw-axis integer form there is nothing given up here
    !! at all: it is the same work with the per-element call removed.
    interface pf_random_fill_streams
        module procedure pf_random_fill_streams_r64_i32
        module procedure pf_random_fill_streams_r64_i64
        module procedure pf_random_fill_streams_r32_i32
        module procedure pf_random_fill_streams_r32_i64
        module procedure pf_random_fill_streams_i32_i32
        module procedure pf_random_fill_streams_i32_i64
        module procedure pf_random_fill_streams_i64_i32
        module procedure pf_random_fill_streams_i64_i64
    end interface pf_random_fill_streams

    !> Element `k` of a uniform-looking permutation of `1 .. m`, addressed by its coordinates.
    !!
    !! The permutation counterpart of `pf_random_at`: a pure function of `(seed, m, k)`, computed in
    !! constant time and constant memory, touching no array. Element `k` depends on no other element,
    !! so a loop over `k` may be run in any order, on any number of threads, and gives the same
    !! answer -- which is the property the whole module exists for, extended from draws to
    !! permutations.
    !!
    !! ```fortran
    !! !$omp parallel do
    !! do k = 1, m
    !!     perm(k) = pf_random_perm_at(seed, m, k)      ! same result at any thread count
    !! end do
    !! ```
    !!
    !! **The first `n` values are a uniform random `n`-subset of `1 .. m`**, so a subset needs no
    !! separate machinery and no memory: ask for `k = 1 .. n`. Prefix consistency follows for free --
    !! a size-3 subset is a prefix of a size-6 one from the same `(seed, m)` -- and a subset at
    !! `n == m` **is** the permutation, rather than merely agreeing with it.
    !!
    !! **`m` and `k` share their kind** (`integer(int32)` or `integer(int64)`) and the result follows
    !! them; `seed` is always `integer(int64)`. `k` outside `[1, m]` is clamped rather than reported,
    !! the same convention `draw_or_1` uses and for the same reason: this is `pure elemental` and has
    !! no way to abort.
    !!
    !! **Uniformity comes in two grades, split at `m = 20`, and the split is visible in
    !! `pf_random_perm_algorithm`.**
    !!
    !! For `m <= 20` the result is **exactly uniform over all `m!` permutations** -- one exactly
    !! uniform rank in `[0, m!)` composed with a bijection onto `S_m`, so this is a property of the
    !! construction rather than a measurement. 20 is where it stops because `20!` is the last
    !! factorial an `integer(int64)` holds.
    !!
    !! For `m >= 21` a 64-bit seed cannot address `m!` permutations at all, so exact uniformity is
    !! not available to *any* construction with this signature. What is measured instead is that the
    !! result is indistinguishable from a uniform permutation under an all-cells chi-square, an
    !! order-4 tuple statistic, a parity test over the alternating group, and fixed-point,
    !! cycle-structure, position-uniformity, subset-membership and structural tests.
    !!
    !! **Consecutive `m` are independent, in both regimes.** Asking for a permutation of 5 and one of
    !! 6 under the same seed gives two unrelated answers rather than two views of one draw.
    interface pf_random_perm_at
        module procedure pf_random_perm_at_i32
        module procedure pf_random_perm_at_i64
    end interface pf_random_perm_at

    !> Fills `perm` with the whole permutation of `1 .. size(perm)` under `seed`.
    !!
    !! The bulk form of `pf_random_perm_at`, and an **identity rather than a second algorithm**:
    !! `perm(k)` equals `pf_random_perm_at(seed, size(perm), k)` for every `k`, so the two may be
    !! mixed freely and a prefix of one is a prefix of the other. That is the same relationship
    !! `pf_random_at` and `pf_random_fill_draws` already have, and it is what keeps the two forms
    !! from drifting apart under a later optimisation.
    !!
    !! `perm` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `seed` is
    !! `integer(int64)`. A zero-sized `perm` is a defined no-op.
    !!
    !! **`threads` is optional and changes only how fast the array is filled, never what is in
    !! it** -- and that is provable here rather than merely intended, which is unusual for a
    !! threading argument. `perm(k)` is a pure function of `(seed, size(perm), k)` and reads
    !! nothing another element writes, so the 1-thread and N-thread results are bit-identical and
    !! a program may switch thread counts while debugging without its permutation changing under
    !! it. Absent means automatic: as many threads as OpenMP offers, capped by
    !! `parquet_set_random_threads`, and **1 inside an OpenMP parallel region**, since a nested
    !! region is the caller's business. An explicit `threads=` is honoured there too.
    !!
    !! **A work floor applies to both, and it is not a tuning detail.** Below
    !! `parquet_set_random_parallel_min_elements` (default 1000) elements per thread the call runs
    !! serially, because threading a small permutation is measurably *slower* than not threading
    !! it -- so `threads=8` on a 100-element array is honoured by being declined.
    !!
    !! **It is about 2.4x cheaper per element than calling the elemental form `size(perm)` times**
    !! (machine B, gfortran 14.2.1, `--profile release`: 9.9 ns against 24.1 ns at `m = 10**8`), for
    !! two structural reasons rather than any tuning. Enumerating the domain in order hands the loop
    !! the `(l, r)` split that the scalar form has to recover with an integer division; and, the
    !! larger of the two, the width rule and the key schedule are derived once per call instead of
    !! once per element, which a `pure elemental` function has no way to avoid. The values are
    !! unchanged by either -- only the route to them is.
    interface pf_random_permutation
        module procedure pf_random_permutation_i32
        module procedure pf_random_permutation_i64
    end interface pf_random_permutation

    !> Fills `idx` with the first `size(idx)` elements of the permutation of `1 .. m` under `seed`.
    !!
    !! A uniform random subset of size `size(idx)` drawn without replacement, in uniform random
    !! order -- and it costs `O(size(idx))` rather than `O(m)`, which is the reason this permutation
    !! is coordinate-addressed at all: 1000 rows out of `10**12` reads 1000 elements and never
    !! materialises the population. `pf_random_subset(idx, size(idx), seed)` **is**
    !! `pf_random_permutation(idx, seed)`, and a subset of size `n` is a prefix of one of size `2n`.
    !!
    !! `idx` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `m` is
    !! `integer(int32)` or `integer(int64)`; `seed` is `integer(int64)`. A zero-sized `idx` is a
    !! defined no-op and is not validated -- it asks for nothing, so no precondition applies to it.
    !! `threads` behaves exactly as it does on `pf_random_permutation`, including the work floor;
    !! see there. Note the floor is measured in elements PRODUCED, `size(idx)`, not in `m` -- a
    !! 100-element subset of a population of `10**12` is 100 elements of work.
    !!
    !! **Three preconditions, all of which abort rather than truncate or wrap** when `idx` is
    !! non-empty: `m >= 1`; `size(idx) <= m`, since a subset drawn without replacement cannot be
    !! larger than its population; and -- for an `integer(int32)` `idx` only -- `m <= huge(int32)`,
    !! since an element may be any value in `[1, m]` and one above `huge(int32)` has nowhere to go.
    !! The last of those is why the `integer(int32)`-array/`integer(int64)`-`m` pairing is accepted
    !! at compile time and rejected at run time: refusing it in the interface would make a perfectly
    !! ordinary `integer :: m` fail to compile against an `integer(int64)` array.
    interface pf_random_subset
        module procedure pf_random_subset_i32_i32
        module procedure pf_random_subset_i32_i64
        module procedure pf_random_subset_i64_i32
        module procedure pf_random_subset_i64_i64
    end interface pf_random_subset

    !> Fills `idx` with `size(idx)` values drawn from `1 .. m` **with replacement**.
    !!
    !! The third member of the resampling trio, and the one whose construction is not a construction
    !! at all: drawing with replacement means `size(idx)` independent uniform integers in `[1, m]`,
    !! with no dedup structure, no permutation and no sort. So this is the draw-axis integer bulk
    !! fill under another name, and the identity is exact and is asserted by the suite:
    !!
    !! ```fortran
    !! call pf_random_resample(idx, m, seed, stream)
    !! call pf_random_fill_draws(seed, stream, idx, 1_int64, m)   ! the SAME values
    !! ```
    !!
    !! **What the name buys is the 1.4x-1.6x a caller loses by writing the obvious loop.** Without
    !! it the natural code is `do k = 1, n; idx(k) = pf_random_int_at(seed, i, 1, m, k); end do`,
    !! which re-enciphers a Philox block for every value where the bulk form serves two draws from
    !! each one -- measured at 25.7 against 15.7 ns per value on machine B (gfortran 15.2.1,
    !! `--profile release`, 10M values) and 16.1 against 10.1 on machine A. The values are identical
    !! either way; only the route to them differs.
    !!
    !! `idx` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `m` is
    !! `integer(int32)` or `integer(int64)`; `seed` is `integer(int64)`. `stream` is optional,
    !! `integer(int32)` or `integer(int64)`, and defaults to 1 -- it selects which replicate this is,
    !! so replicate `b` is reproducible from `(seed, b)` alone, independent of how many replicates
    !! were asked for or in what order they ran. A zero-sized `idx` is a defined no-op and is not
    !! validated: it asks for nothing, so no precondition applies to it.
    !!
    !! **Two preconditions, both aborting rather than truncating or wrapping**, and only when `idx`
    !! is non-empty: `m >= 1`; and -- for an `integer(int32)` `idx` only -- `m <= huge(int32)`, since
    !! an element may be any value in `[1, m]` and one above `huge(int32)` has nowhere to go. The
    !! second is why the `integer(int32)`-array/`integer(int64)`-`m` pairing is accepted at compile
    !! time and rejected at run time, exactly as on `pf_random_subset`.
    !!
    !! **There is deliberately NO `size(idx) <= m` precondition**, and its absence is the clearest
    !! statement of how this differs from its sibling. That bound belongs to a subset drawn *without*
    !! replacement; here `size(idx) == m` is the most ordinary bootstrap there is, and `size(idx)`
    !! far beyond `m` is perfectly meaningful. A guard copied across from `pf_random_subset` would
    !! refuse the procedure's main use.
    !!
    !! **`stream` is this procedure's replicate axis, and the siblings do not have one.**
    !! `pf_random_permutation` and `pf_random_subset` are keyed by `(seed, m)` alone, so independent
    !! replicates of those come from `pf_random_key(seed, b)` instead. A resample is built on the
    !! draw axis, which carries a stream coordinate already, so it costs a caller one integer rather
    !! than a key derivation. Both routes are available here: `stream = b` and
    !! `seed = pf_random_key(seed, b)` are equally independent.
    !!
    !! **`threads` behaves exactly as it does on `pf_random_permutation` and `pf_random_subset`** --
    !! same `parquet_set_random_threads` default, same `parquet_set_random_parallel_min_elements`
    !! work floor, and the floor applies to an explicit request too, so a small resample stays serial
    !! however many workers are asked for. The result is **bit-identical at every thread count**, and
    !! by construction rather than by care: element `k` is a pure function of `(seed, stream, k)`, so
    !! a thread filling elements `lo .. hi` is the serial fill started at draw `lo`. That is asserted
    !! rather than argued.
    !!
    !! **`threads` requires an explicit `stream`, and that is a language constraint rather than a
    !! choice.** `threads` and `stream` are both integers in argument position 4, so a generic
    !! offering `(idx, m, seed [, threads])` beside `(idx, m, seed, stream)` is **rejected by the
    !! compiler** -- "Ambiguous interfaces in generic interface 'pf_random_resample'" -- and giving
    !! the two dummies different keyword names does not rescue it, because keyword names do not make
    !! specifics distinguishable. Both spellings were compiled to confirm it. So write
    !! `call pf_random_resample(idx, m, seed, 1, threads=8)` for the default replicate; omitting
    !! `stream` produces a "no specific subroutine matches" error that does not explain itself. This
    !! is the same constraint the `parquet_open_reader` split answers one level along, where an
    !! optional argument differing only by *kind* could not disambiguate either.
    interface pf_random_resample
        module procedure pf_random_resample_i32_i32
        module procedure pf_random_resample_i32_i64
        module procedure pf_random_resample_i64_i32
        module procedure pf_random_resample_i64_i64
        module procedure pf_random_resample_i32_i32_s32
        module procedure pf_random_resample_i32_i64_s32
        module procedure pf_random_resample_i64_i32_s32
        module procedure pf_random_resample_i64_i64_s32
        module procedure pf_random_resample_i32_i32_s64
        module procedure pf_random_resample_i32_i64_s64
        module procedure pf_random_resample_i64_i32_s64
        module procedure pf_random_resample_i64_i64_s64
    end interface pf_random_resample

    !> Derives an independent seed from a seed and a label, so one seed can fan out into families.
    !!
    !! `label` is `integer(int32)` or `integer(int64)`; `seed` and the result are `integer(int64)`.
    !! A derived key IS a seed, so derivations compose by nesting.
    interface pf_random_key
        module procedure pf_random_key_i32
        module procedure pf_random_key_i64
    end interface pf_random_key

    ! ================================================================================
    ! Tier 1 -- the stateful stream
    ! ================================================================================

    !> Highest 0-based word position a stream may hold. A stream addresses `2**63` words, which at
    !! the measured cost of a draw is some thousands of times the age of the universe -- so this
    !! bound exists to keep the position arithmetic provably free of signed overflow, not because a
    !! program will approach it. **That is a correctness requirement, not tidiness**: Risk-94 records
    !! this module being caught with a compiler using one overflowing expression's undefinedness to
    !! delete a branch hundreds of lines away, so a new unguarded overflow site on a hot path would
    !! be a regression against Risk-95's "every remaining wrapping site is `#else`-arm only".
    integer(int64), parameter :: stream_pos_max = huge(1_int64)

    !> A walk along one stream: the same values tier 0 addresses, reached in sequence.
    !!
    !! Tier 0 answers "what is the value at this coordinate?". Some programs cannot ask that,
    !! because how many values they need is data-dependent -- a rejection sampler, a random walk, a
    !! resample of unknown length. This type carries the position so the caller does not have to,
    !! and hands out consecutive values of one stream.
    !!
    !! **It changes no value.** A freshly seeded stream's `k`-th `%uniform` is exactly
    !! `pf_random_at(seed, stream, k)`, its `k`-th `%uniform32` exactly `pf_random32_at(seed,
    !! stream, k)`. The stream is a different way to reach the same grid, never a second generator.
    !!
    !! **Reproducibility is per iteration, and that is the discipline to follow**: seed at the top of
    !! each loop iteration from a run-invariant label, then draw as many values as that iteration
    !! needs.
    !!
    !! ```fortran
    !! type(pf_random_stream) :: rng
    !! !$omp parallel do schedule(dynamic) private(...)
    !! do i = 1, n
    !!     call rng%seed(seed, i)              ! O(1), no warm-up
    !!     do while (...)                      ! however many draws this iteration turns out to need
    !!         call rng%uniform(x)
    !!     end do
    !! end do
    !! ```
    !!
    !! What can never be reproducible is one long-lived stream consumed *across* the iterations of a
    !! dynamically scheduled loop -- the value an iteration receives then depends on how many draws
    !! ran before it, which depends on the schedule. That is a property of every stateful generator,
    !! not a limitation of this one; the answer to it is that `%seed` costs nothing.
    !!
    !! **Position is measured in 32-bit words, 1-based, and the word cost of each producer is
    !! contract** -- `%jump`, `%position` and `%rewind` are denominated in it: `%uniform` 2,
    !! `%uniform32` 1, `%bits` 2, `%int_range` 2. `%int_range` additionally starts on a word PAIR
    !! boundary, advancing one word first if a `%uniform32` has left the cursor odd. This is what
    !! keeps an `%int_range` equal to the `pf_random_int_at` at the same coordinate rather than
    !! re-reading a word a previous draw already used. It cost 4 words plus up to 3 of alignment
    !! until `pf_random_algorithm` reached `/v2`, when the integer generic's stride became 2.
    !!
    !! **The type is plain scalars: no allocatable components, no `FINAL`, deliberately and
    !! permanently.** gfortran does not reliably default-initialise an OpenMP `private()` copy of a
    !! finalizable type, and ifx segfaults on a block-local instance of a type with allocatable
    !! components inside a parallel region (`feature_risks.md` Risk-45). A type that is neither is
    !! safe in both shapes, which is what makes a per-thread instance usable at all. Adding either to
    !! this type would break every parallel use of it, on one compiler or the other.
    !!
    !! It holds the block it last enciphered, keyed by that block's index. One enciphering carries
    !! two `real64` values or four `real32`s, so keeping it is worth **1.70x (gfortran) / 1.78x (ifx)**
    !! on `real64` and **2.88x / 3.35x** on `real32` -- measured, and enough to take the stream from
    !! slower than a tier-0 loop to faster than one. Because the cache is *keyed* rather than
    !! consumed, it reaches nothing in the contract: `%position` still means a word index, and a
    !! stream saved and restored through `%position` alone is exact.
    !!
    !! **`pf_random_fill_draws` is still cheaper again** (1.87x / 1.67x against this type), so bulk
    !! work whose length is known in advance belongs there, not in a loop over a stream.
    type, public :: pf_random_stream
        private
        integer(int64) :: key = 0_int64             !! the stream family's seed
        integer(int64) :: stream = 0_int64          !! which stream of that family
        integer(int64) :: pos = 0_int64             !! 0-based word position; `%position` reports `pos+1`
        integer(int64) :: blk = -1_int64            !! block index held below, or -1 when none is
        integer(int64) :: c0 = 0_int64              !! held word 0
        integer(int64) :: c1 = 0_int64              !! held word 1
        integer(int64) :: c2 = 0_int64              !! held word 2
        integer(int64) :: c3 = 0_int64              !! held word 3
    contains
        procedure, private :: seed_base => stream_seed_base   !! `%seed` with no stream index
        procedure, private :: seed_i32 => stream_seed_i32     !! `%seed` with an `int32` stream index
        procedure, private :: seed_i64 => stream_seed_i64     !! `%seed` with an `int64` stream index
        !> (Re)seeds the stream to position 1. O(1), with no warm-up; `stream` defaults to 0.
        generic :: seed => seed_base, seed_i32, seed_i64
        procedure :: uniform => stream_uniform      !! Next `real64` in `[0, 1)`; costs 2 words.
        procedure :: uniform32 => stream_uniform32  !! Next `real32` in `[0, 1)`; costs 1 word.
        procedure :: bits => stream_bits            !! Next 64 raw bits; costs 2 words.
        procedure, private :: int_range_i32 => stream_int_range_i32  !! `%int_range`, `int32`
        procedure, private :: int_range_i64 => stream_int_range_i64  !! `%int_range`, `int64`
        !> Next integer in `[lo, hi]`, exactly unbiased; costs one word pair, taken pair-aligned.
        generic :: int_range => int_range_i32, int_range_i64
        procedure, private :: fill_arr_r64 => stream_fill_r64        !! `%fill`, `real64`
        procedure, private :: fill_arr_r32 => stream_fill_r32        !! `%fill`, `real32`
        procedure, private :: fill_arr_i32 => stream_fill_i32        !! `%fill`, `int32`
        procedure, private :: fill_arr_i64 => stream_fill_i64        !! `%fill`, `int64`
        !> Fills `v` with the next `size(v)` values; an integer `v` also takes `lo` and `hi`.
        generic :: fill => fill_arr_r64, fill_arr_r32, fill_arr_i32, fill_arr_i64
        procedure, private :: jump_i32 => stream_jump_i32            !! `%jump`, `int32`
        procedure, private :: jump_i64 => stream_jump_i64            !! `%jump`, `int64`
        !> Seeks `n` words, in O(1). Negative seeks backwards; the result must stay in range.
        generic :: jump => jump_i32, jump_i64
        procedure, private :: rewind_base => stream_rewind_base      !! `%rewind` to position 1
        procedure, private :: rewind_i32 => stream_rewind_i32        !! `%rewind`, `int32`
        procedure, private :: rewind_i64 => stream_rewind_i64        !! `%rewind`, `int64`
        !> Sets the position; with no argument, back to 1. Accepts any value `%position` gave.
        generic :: rewind => rewind_base, rewind_i32, rewind_i64
        procedure :: position => stream_position    !! Current 1-based word position.
    end type pf_random_stream

    ! ================================================================================
    ! Tier 2 -- the weighted sequential draw
    ! ================================================================================

    !> Successive sampling without replacement: draw one item with probability proportional to its
    !> weight, remove it, renormalise over the survivors, repeat.
    !>
    !> **What this distribution is, and one thing it is not.** Drawn to exhaustion it is the
    !> weighted shuffle, also called the Plackett-Luce order. The FIRST draw is exactly
    !> proportional to weight; later draws are proportional among the survivors. It is **not**
    !> inclusion-probability-proportional-to-size: an item with twice the weight is not twice as
    !> likely to appear somewhere in the first `k`, and no construction with this interface can
    !> make it so. Most callers who reach for "weighted sampling without replacement" want this
    !> one, but the distinction is worth knowing before relying on it.
    !>
    !> ```fortran
    !> type(pf_weighted_draw) :: d
    !> integer :: item
    !> logical :: ok
    !>
    !> call d%init(weights, seed)
    !> do
    !>     call d%next(item, ok)
    !>     if (.not. ok) exit          ! the population is exhausted
    !>     ! ... judge item; exit on acceptance ...
    !> end do
    !> ```
    !>
    !> **The tree is seed-independent, and that is the whole reason this type exists.** It is built
    !> from the weights alone; the seed only steers the descent. So a second sequence over the same
    !> weights costs `O(k log n)` to restore rather than an `O(n)` rebuild, which is what makes an
    !> outer loop of many short sequences -- the shape this type was designed for -- nearly free.
    !> Use `%reseed` for that, holding the seed fixed and passing the outer iteration as `stream`.
    !>
    !> **Cost**: `%init` is `O(n)`, `%next` is `O(log n)`, `%reset` and `%reseed` are `O(k log n)`
    !> in the draws already taken.
    !>
    !> **Serial by nature, and there is no `threads=`.** Draw `k+1` cannot be produced until draw
    !> `k` has been removed, so there is no parallelism inside one sequence to expose; an argument
    !> that was accepted and ignored would be worse than its absence. Independent sequences ARE
    !> parallel, and that is where the threads go -- see the note below.
    !>
    !> **To run many sequences in parallel, give each thread its own sampler, built inside the
    !> parallel region.** `%next` mutates the tree, so one sampler cannot serve two threads.
    !>
    !> ```fortran
    !> type(pf_weighted_draw), allocatable :: dd(:)
    !> integer :: nt, tid, j
    !>
    !> nt = 1
    !> !$ nt = omp_get_max_threads()
    !> allocate(dd(nt))
    !> !$omp parallel do default(shared) private(tid)
    !> do tid = 1, nt
    !>     call dd(tid)%init(weights, seed)     ! nt builds, wall-clock of ONE
    !> end do
    !>
    !> !$omp parallel do default(shared) private(tid, item, ok) schedule(dynamic)
    !> do j = 1, n_outer
    !>     tid = 1
    !>     !$ tid = omp_get_thread_num() + 1
    !>     call dd(tid)%reseed(seed, stream=int(j, int64))
    !>     do
    !>         call dd(tid)%next(item, ok)
    !>         if (.not. ok) exit
    !>     end do
    !> end do
    !> ```
    !>
    !> **Never put this type in an OpenMP `private()` clause.** A private copy is a FRESH object,
    !> not a copy of yours: measured on gfortran 15.2.1 and ifx 2026.1.1, the tree comes back
    !> allocated to the right shape with UNINITIALISED contents and every scalar reset to its
    !> default. Nothing aborts and every drawn item still looks valid, so the failure is a wrong
    !> answer with no symptom. The shared per-thread array above avoids the privatisation machinery
    !> entirely; `firstprivate()` also copies correctly on both compilers, if you prefer it.
    !>
    !> **Because each sequence is named by its coordinates, the result does not depend on the
    !> schedule or the thread count.** `schedule(dynamic)` above costs nothing in reproducibility.
    !>
    !> **Zero-weight items are kept out of the tree and handed out last**, in uniform random order,
    !> so draining a population is always a genuine permutation of every item. Keeping them out is
    !> not an optimisation: a zero leaf would be indistinguishable from a spent one, and the
    !> descent's liveness test reads exactly that.

    !> Label deriving the zero-weight tail's own permutation seed. Any fixed value would do.
    integer(int64), parameter :: wd_zero_label = 7965600847521931_int64

    !> Journal entries allocated on first use; it doubles from there.
    integer(int64), parameter :: wd_journal_min = 64_int64

    type, public :: pf_weighted_draw
        private
        !> The segment tree: `2*p2` nodes, leaf `j` at `st(p2 + j - 1)`, every internal node the
        !! sum of its two children. Node `1` is unused padding's parent -- the root is `st(1)`.
        real(real64), allocatable :: st(:)
        !> Leaf `j` holds the weight of item `leaf_item(j)`. **Unallocated when no weight is
        !! zero**, which is the common case; leaf `j` is then item `j` and the map is skipped.
        integer(int64), allocatable :: leaf_item(:)
        !> The zero-weight items, in input order. Unallocated when there are none.
        integer(int64), allocatable :: zero_item(:)
        !> Undo journal: the leaf node zeroed by each draw so far, in order.
        !!
        !! **One entry per DRAW, not one per tree write.** Undoing a draw restores its leaf and
        !! then recomputes that leaf's ancestors from their children, exactly as the forward
        !! direction does -- so the `O(log n)` ancestor writes need not be journalled at all. That
        !! is 1 entry where the obvious design has `1 + log2(n)`, which at `n = 10**6` is a 21x
        !! difference in what a fully drained sampler holds, and it is why no capacity policy or
        !! rebuild fallback is needed. It is exact rather than approximate: every node is a pure
        !! function of its two children, so replaying the leaves reproduces the built tree bit for
        !! bit -- `test_reset_restores_tree_exactly` asserts precisely that.
        integer(int64), allocatable :: jr_pos(:)
        !> Undo journal: the weight each of those leaves held.
        real(real64), allocatable :: jr_val(:)
        integer(int64) :: p2 = 0        !! leaves in the tree; the least power of two `>= npos`
        integer(int64) :: npos = 0      !! items with a strictly positive weight
        integer(int64) :: nzero = 0     !! items with weight exactly zero
        integer(int64) :: live = 0      !! positive-weight items not yet drawn
        integer(int64) :: ndrawn = 0    !! draws taken in this sequence, zero-weight tail included
        integer(int64) :: jr_n = 0      !! journal entries in use
        integer(int64) :: wseed = 0     !! the seed steering the descent
        integer(int64) :: wstream = 0   !! which sequence of that seed
        integer(int64) :: zkey = 0      !! derived seed ordering the zero-weight tail
        logical :: ready = .false.      !! `%init` has run; guards every other entry point
    contains
        procedure, private :: init_base => wd_init_base  !! `%init` with no stream
        procedure, private :: init_s32 => wd_init_s32    !! `%init` with an `int32` stream
        procedure, private :: init_s64 => wd_init_s64    !! `%init` with an `int64` stream
        !> Prepares the sampler over `weights`. `O(n)`. `stream` defaults to 0.
        generic :: init => init_base, init_s32, init_s64
        procedure, private :: next_i32 => wd_next_i32    !! `%next` into an `int32` item
        procedure, private :: next_i64 => wd_next_i64    !! `%next` into an `int64` item
        !> Draws the next item. `O(log n)`. `ok` is `.false.` once every item has been drawn.
        generic :: next => next_i32, next_i64
        procedure :: reset => wd_reset                   !! Restarts the SAME sequence. `O(k log n)`.
        procedure, private :: reseed_base => wd_reseed_base !! `%reseed` with no stream
        procedure, private :: reseed_s32 => wd_reseed_s32   !! `%reseed` with an `int32` stream
        procedure, private :: reseed_s64 => wd_reseed_s64   !! `%reseed` with an `int64` stream
        !> Starts a DIFFERENT sequence over the same weights. `O(k log n)`; `stream` defaults to 0.
        generic :: reseed => reseed_base, reseed_s32, reseed_s64
        procedure :: remaining => wd_remaining           !! Items not yet drawn; exact, `O(1)`.
    end type pf_weighted_draw

    !> Fills `idx` with the first `size(idx)` items of a weighted sequential draw over `weights`.
    !!
    !! **Defined as `size(idx)` calls to `pf_weighted_draw%next`, not as a second algorithm**, so a
    !! subset of size `k` is a prefix of one of size `2k`, and stopping a `%next` loop after `k`
    !! draws gives exactly this. That identity is asserted by the suite rather than intended.
    !!
    !! `idx` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`; `weights` is
    !! `real(real64)`, one per item; `seed` is `integer(int64)`; `stream` is optional and is
    !! `integer(int32)` or `integer(int64)`. The population is `size(weights)` and the number drawn
    !! is `size(idx)` -- note this differs from `pf_random_subset`, whose second argument is the
    !! POPULATION, because here the population is carried by the weights themselves.
    !!
    !! **Preconditions, all of which abort** when `idx` is non-empty: `size(idx) <= size(weights)`;
    !! every weight `>= 0`; at least one weight `> 0`; and, for an `integer(int32)` `idx`,
    !! `size(weights) <= huge(1_int32)`. A zero-sized `idx` asks for nothing and is a defined no-op.
    !!
    !! **`O(n + k log n)`**, so it is the cheaper form whenever `k` is small against `n`; past
    !! roughly `k = n/2` a caller wanting most of the population is better served by asking for the
    !! whole weighted permutation.
    !> Fills `perm` with a weighted random permutation of `1 .. size(weights)`.
    !!
    !! **The order family.** `perm(k)` is the item drawn `k`-th by successive sampling: the first
    !! is proportional to weight, each later one proportional among the survivors. Drawn to
    !! exhaustion, that is the weighted shuffle. Same distribution as `pf_weighted_draw` -- see the
    !! note on realizations below.
    !!
    !! `perm` is a rank-1 `integer(int32)` or `integer(int64)` array, `intent(out)`, whose size
    !! must equal `size(weights)`; `weights` is `real(real64)`; `seed` is `integer(int64)`;
    !! `stream` is optional and `integer(int64)` -- note the asymmetry with `pf_weighted_draw`,
    !! whose `%init`/`%reseed` take either kind. It is forced rather than chosen: an optional
    !! `threads` and an `integer(int32)` `stream` are not distinguishable as a positional fourth
    !! argument, so one generic cannot carry both, and `threads=` is worth more here than saving a
    !! cast. Write `stream=int(j, int64)` in a loop. `threads` behaves as it does elsewhere in this
    !! module, including the work floor.
    !!
    !! **It is a DIFFERENT REALIZATION from the sequential family, not a different distribution.**
    !! `pf_weighted_permutation(perm, w, seed)` and a `pf_weighted_draw` drained under the same
    !! seed both draw from successive sampling, and for one seed they give different draws from it
    !! -- in the same way two different seeds would. There is no prefix identity across the two
    !! families, and the suite asserts that they differ rather than leaving it to be discovered.
    !! Within the sequential family the prefix identity does hold; see `pf_weighted_subset`.
    !!
    !! **Construction: the exponential race.** Give item `i` the key `-log(u_i)/w_i` for
    !! independent uniforms, and sort ascending. That is exactly successive sampling, and unlike
    !! the sequential form it is embarrassingly parallel and coordinate-addressed, so the answer is
    !! **bit-identical at every thread count**. Use this when you want many whole shuffles; use
    !! `pf_weighted_draw` when you want a few draws, or many short sequences over the same weights,
    !! where it is thousands of times cheaper.
    !!
    !! **To produce many shuffles, parallelise ACROSS them rather than within one.** The key loop
    !! is memory-bound and scales sub-linearly, whereas independent shuffles are perfectly
    !! parallel. An OpenMP loop over sequences, each calling this with `threads=1`, beats a serial
    !! loop over threaded calls; the module's own auto-threading already resolves to serial inside
    !! an active parallel region, so this needs no special handling.
    !!
    !! **Zero-weight items come last, in uniform random order** -- they can never be drawn while a
    !! positive weight remains, so a full shuffle is still a genuine permutation of every item.
    !!
    !! **Preconditions, all of which abort**: `size(perm) == size(weights)`; every weight finite
    !! and `>= 0`; at least one weight `> 0`; no weight so small that `-log(u)/w` overflows (below
    !! about `2e-307`, which no real weighting reaches and which would otherwise tie several items
    !! at infinity); and, for an `integer(int32)` `perm`, `size(weights) <= huge(1_int32)`.
    interface pf_weighted_permutation
        module procedure wperm_i32_base
        module procedure wperm_i32_s64
        module procedure wperm_i64_base
        module procedure wperm_i64_s64
    end interface pf_weighted_permutation

    interface pf_weighted_subset
        module procedure wsub_i32_base
        module procedure wsub_i32_s32
        module procedure wsub_i32_s64
        module procedure wsub_i64_base
        module procedure wsub_i64_s32
        module procedure wsub_i64_s64
    end interface pf_weighted_subset

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

    !> `pf_random_fill_draws` filling `real64` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_r64_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r64(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r64_i32

    !> `pf_random_fill_draws` filling `real64` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_r64_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r64(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r64_i64

    !> `pf_random_fill_draws` filling `real32` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_r32_i32(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r32(seed, int(i, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r32_i32

    !> `pf_random_fill_draws` filling `real32` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_r32_i64(seed, i, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_r32(seed, i, v, draw_or_1(draw))
    end subroutine pf_random_fill_draws_r32_i64

    !> `pf_random_fill_streams` filling `real64` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_r64_i32(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        real(real64), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r64(seed, int(i0, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r64_i32

    !> `pf_random_fill_streams` filling `real64` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_r64_i64(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        real(real64), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r64(seed, i0, v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r64_i64

    !> `pf_random_fill_streams` filling `real32` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_r32_i32(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        real(real32), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r32(seed, int(i0, int64), v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r32_i32

    !> `pf_random_fill_streams` filling `real32` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_r32_i64(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        real(real32), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_r32(seed, i0, v, draw_or_1(draw))
    end subroutine pf_random_fill_streams_r32_i64

    ! The eight integer specifics. The two-token suffix reads value-kind first and stream-index-kind
    ! second, exactly as `_r64_i32` does -- so `_i32_i64` fills an `integer(int32)` array from an
    ! `integer(int64)` stream index.

    !> `pf_random_fill_draws` filling `integer(int32)` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_i32_i32(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i32(seed, int(i, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i32_i32

    !> `pf_random_fill_draws` filling `integer(int32)` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_i32_i64(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i32(seed, i, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i32_i64

    !> `pf_random_fill_draws` filling `integer(int64)` from an `integer(int32)` stream index.
    pure subroutine pf_random_fill_draws_i64_i32(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i             !! stream index; sign-extends, so any value is valid
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i64(seed, int(i, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i64_i32

    !> `pf_random_fill_draws` filling `integer(int64)` from an `integer(int64)` stream index.
    pure subroutine pf_random_fill_draws_i64_i64(seed, i, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i             !! stream index; every value is valid
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! 1-based starting value index; absent means 1
        call fill_draws_i64(seed, i, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_draws_i64_i64

    !> `pf_random_fill_streams` filling `integer(int32)` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_i32_i32(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        integer(int32), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i32(seed, int(i0, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i32_i32

    !> `pf_random_fill_streams` filling `integer(int32)` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_i32_i64(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        integer(int32), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i32(seed, i0, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i32_i64

    !> `pf_random_fill_streams` filling `integer(int64)` from an `integer(int32)` first stream index.
    pure subroutine pf_random_fill_streams_i64_i32(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: i0            !! first stream index; sign-extends, so any value is valid
        integer(int64), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i64(seed, int(i0, int64), v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i64_i32

    !> `pf_random_fill_streams` filling `integer(int64)` from an `integer(int64)` first stream index.
    pure subroutine pf_random_fill_streams_i64_i64(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index; every value is valid
        integer(int64), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end; `lo > hi` is swapped, not an error
        integer(int64), intent(in), optional :: draw !! which draw of each stream; absent means 1
        call fill_streams_i64(seed, i0, v, lo, hi, draw_or_1(draw))
    end subroutine pf_random_fill_streams_i64_i64

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
    !!
    !! **The four words are named scalars selected by `select case`, not a local array indexed at
    !! run time**, which is worth about 5-7 % of `pf_random32_at` on x86-64 -- 18.32 -> 17.37 ns on
    !! machine C (gfortran 15.2) and 22.33 -> 20.86 / 15.75 -> 14.67 on machine B (gfortran 14.2.1 /
    !! ifx). Machine A (arm64) measures no change, so this is positive-or-neutral rather than
    !! universal. Note it is the array indexing that pays and **not** the `/` and `modulo`:
    !! rewriting those as `ishft`/`iand` -- valid, since `index` is `draw - 1` with `draw` clamped
    !! to at least 1 -- was measured at nothing on both machines that tried it, so that spelling was
    !! deliberately NOT taken. It would trade a form correct for every input for one correct only
    !! for non-negative inputs, and buy zero.
    pure function word_of(seed, stream, index) result(w)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: index         !! 0-based word index within the stream
        integer(int64) :: w                         !! that word, in `[0, 2**32)`
        integer(int64) :: c0, c1, c2, c3
        call random_block(seed, stream, index / 4_int64, c0, c1, c2, c3)
        select case (int(modulo(index, 4_int64), int32))
        case (0)
            w = c0
        case (1)
            w = c1
        case (2)
            w = c2
        case default
            w = c3
        end select
    end function word_of

    !> The 64-bit pattern of `real64`/raw-bits value `draw` (1-based) of a stream.
    !!
    !! Value `d` occupies words `2d-2` and `2d-1`, so a block carries TWO values: draw 1 takes the
    !! pair `(c0, c1)` and draw 2 the pair `(c2, c3)`. The first word is the LOW half. A draw-axis
    !! bulk fill is faster precisely because it can use both pairs of a block where a scalar draw
    !! uses one. **`pf_random_int_at` reaches this too**, and has since `pf_random_algorithm` `/v2`
    !! -- the three 64-bit generics are one grid, so there is one function that decides which words
    !! a draw index names, and it is this one.
    !!
    !! **`ishft`/`iand` rather than `/` and `modulo`, and the spelling is measured, not assumed.**
    !! Machine B, gfortran 15.2.1, `-O3 -funroll-loops`, best of three rounds over 4M values, with
    !! the `real64` bulk fill as a cross-build control (8.66-8.70 ns in every build, a 0.5 % floor):
    !! a scalar `pf_random_int_at` costs **27.9 ns** with `/`+`modulo` and **25.8** with
    !! `ishft`+`iand`, and the integer draw-axis fill **16.3** against **15.8**. So this is worth
    !! about 2 ns on the integer path, which is the whole of what stride-2 addressing costs it.
    !!
    !! Note `word_of`'s own header records the same substitution measuring **nothing** for its
    !! `index/4` and `modulo(index,4)`, on two machines, and being declined there for that reason.
    !! Both notes are right: they are different functions on different paths, and the point is that
    !! the spelling is decided per site by measurement. Do not propagate either verdict to the other.
    !!
    !! The `max(draw, 1)` is what keeps the substitution safe, and it is **free** -- it measured
    !! inside the noise of the arms above. `ISHFT` is a LOGICAL shift, so `ishft(-1_int64, -1)` is
    !! `2**63 - 1` rather than 0: without the clamp, a negative `draw` would name an absurd block
    !! where `/` and `modulo` merely named the wrong pair. Every caller already clamps (`draw_or_1`
    !! at tier 0, `take_pair` at the stream, `draw + (k-1)` in the fills), so this changes no value
    !! any caller can obtain -- it makes the function total on its own rather than by their courtesy.
    pure function bits_of(seed, stream, draw) result(b)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index; `< 1` clamps to 1
        integer(int64) :: b                         !! the 64-bit pattern
        integer(int64) :: w0, w1, w2, w3, e
        e = max(draw, 1_int64) - 1_int64            ! 0-based value index; non-negative by the max
        call random_block(seed, stream, ishft(e, -1), w0, w1, w2, w3)
        if (iand(e, 1_int64) == 0_int64) then
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
    !!
    !! **Shape: an alignment head, a TWO-BLOCK steady state, then a one-block and a one-value
    !! tail.** Two blocks rather than one because consecutive Philox blocks are independent, so
    !! enciphering two in the same body interleaves two 10-round dependency chains and fills the
    !! issue slots one chain leaves idle. No value moves -- the same blocks are computed in the
    !! same order. **Two is the measured optimum and more is worse**: machine C measured 1 / 2 / 3 /
    !! 4 blocks at 7.85 / 6.40 / 8.87 / 7.93 ns per value, register pressure giving back more than
    !! the extra parallelism buys past two, and that held whether the extra block state was named
    !! scalars or an array. Re-derive the shape of that curve before changing the count, rather
    !! than the winner.
    !!
    !! **The head is what removes the per-iteration parity test AND the overflow guard this loop
    !! used to need.** Aligning once means the steady state advances `blk` by increment, and `blk`
    !! tops out at `(huge - 1)/2`, so no index here can overflow -- where the previous form derived
    !! a position from `draw + k` each pass and needed a guard against forming `huge + 1` on the
    !! final, dead iteration. `position + 1` in the head cannot overflow either: it fires only when
    !! `position` is odd, and the largest odd `position` is `huge - 2`.
    pure subroutine fill_r64(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real64), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: position, blk
        integer(int64) :: w0, w1, w2, w3, x0, x1, x2, x3
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
        position = draw - 1_int64                   ! 0-based value index; `draw` >= 1, so >= 0
        ! Head: one value when `draw` lands on a block's SECOND pair, after which we are aligned.
        if (iand(position, 1_int64) /= 0_int64) then
            call random_block(seed, stream, ishft(position, -1), w0, w1, w2, w3)
            k = 1_int64
            v(1) = to_real64(ior(ishft(w3, 32), w2))
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        do while (k + 4_int64 <= m)                 ! steady state: two blocks, four values
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            call random_block(seed, stream, blk + 1_int64, x0, x1, x2, x3)
            v(k + 1_int64) = to_real64(ior(ishft(w1, 32), w0))
            v(k + 2_int64) = to_real64(ior(ishft(w3, 32), w2))
            v(k + 3_int64) = to_real64(ior(ishft(x1, 32), x0))
            v(k + 4_int64) = to_real64(ior(ishft(x3, 32), x2))
            k = k + 4_int64
            blk = blk + 2_int64
        end do
        do while (k + 2_int64 <= m)                 ! tail: whole blocks
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k + 1_int64) = to_real64(ior(ishft(w1, 32), w0))
            v(k + 2_int64) = to_real64(ior(ishft(w3, 32), w2))
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final half-block
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(m) = to_real64(ior(ishft(w1, 32), w0))
        end if
    end subroutine fill_r64

    !> Fills `v` with consecutive `real32` values of one stream, starting at `draw`.
    !!
    !! Four values to a block, since a `real32` value is one word. Same contract as `fill_r64`:
    !! identical to the matching scalar calls, so prefixes agree.
    !!
    !! **Same three-part shape as `fill_r64` -- head, two-block steady state, tail -- and it is
    !! worth more here than there** (machine C: 4.85 -> 3.15 ns per value, 1.54x, against 1.23x for
    !! `real64`). The extra gain is not extra batching: it is that the steady state no longer runs
    !! the `do while (slot <= 3 .and. k < m)` inner loop this subroutine used to carry on **every**
    !! block. That loop wrote through a local array indexed by a runtime `slot` and tested a
    !! compound condition four times per block, where an aligned block is simply four straight-line
    !! writes from named scalars. The head and tail still need slot handling and still have it, in
    !! the unrolled `if` form -- **`slot <= 0` is deliberately unreachable in the head** (which
    !! fires only for `slot /= 0`), and is written that way so the four lines read as one aligned
    !! pattern rather than three special cases.
    !!
    !! The words are named scalars rather than an array for the reason `random_block`'s header
    !! gives: an array whose subscript is not a compile-time constant is liable to be spilled, and
    !! spilling it here would give back exactly what removing the inner loop won.
    pure subroutine fill_r32(seed, stream, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        real(real32), intent(out) :: v(:)           !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: position, blk
        integer(int64) :: c0, c1, c2, c3, d0, d1, d2, d3
        ! int64 counters and an explicit `kind=` on `size`, for the reason spelled out in
        ! `fill_r64`: a default-kind length wraps above 2**31 elements and fails silently, either
        ! writing nothing or writing a short prefix of the caller's `intent(out)` array.
        integer(int64) :: k, m
        integer :: slot                             ! 0..3 within one block; default kind is ample
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        k = 0_int64
        position = draw - 1_int64                   ! 0-based word index; `draw` >= 1, so >= 0
        blk = position / 4_int64
        slot = int(modulo(position, 4_int64), int32)
        ! Head: finish the first block when the start is not block-aligned.
        if (slot /= 0) then
            call random_block(seed, stream, blk, c0, c1, c2, c3)
            if (slot <= 0 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c0)
            end if
            if (slot <= 1 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c1)
            end if
            if (slot <= 2 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c2)
            end if
            if (slot <= 3 .and. k < m) then
                k = k + 1_int64
                v(k) = to_real32(c3)
            end if
            blk = blk + 1_int64
        end if
        do while (k + 8_int64 <= m)                 ! steady state: two blocks, eight values
            call random_block(seed, stream, blk, c0, c1, c2, c3)
            call random_block(seed, stream, blk + 1_int64, d0, d1, d2, d3)
            v(k + 1_int64) = to_real32(c0)
            v(k + 2_int64) = to_real32(c1)
            v(k + 3_int64) = to_real32(c2)
            v(k + 4_int64) = to_real32(c3)
            v(k + 5_int64) = to_real32(d0)
            v(k + 6_int64) = to_real32(d1)
            v(k + 7_int64) = to_real32(d2)
            v(k + 8_int64) = to_real32(d3)
            k = k + 8_int64
            blk = blk + 2_int64
        end do
        do while (k + 4_int64 <= m)                 ! tail: whole blocks
            call random_block(seed, stream, blk, c0, c1, c2, c3)
            v(k + 1_int64) = to_real32(c0)
            v(k + 2_int64) = to_real32(c1)
            v(k + 3_int64) = to_real32(c2)
            v(k + 4_int64) = to_real32(c3)
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final partial block, at most 3 values
            call random_block(seed, stream, blk, c0, c1, c2, c3)
            if (k < m) then
                k = k + 1_int64
                v(k) = to_real32(c0)
            end if
            if (k < m) then
                k = k + 1_int64
                v(k) = to_real32(c1)
            end if
            if (k < m) then
                k = k + 1_int64
                v(k) = to_real32(c2)
            end if
        end if
    end subroutine fill_r32

    !> Fills `v` with one draw of each of `size(v)` consecutive streams, starting at `i0`.
    !!
    !! **Why this is a plain loop over `random_block` and not a lane-blocked kernel.** Both were
    !! measured on machine B, over ten prototypes gated bit-identical to `pf_random_at` first. The
    !! win here is almost entirely from having a bulk entry point at all -- not from batching:
    !!
    !! | form | gfortran | ifx |
    !! |---|---|---|
    !! | scalar loop (what this replaces) | 19.74 ns | 14.95 ns |
    !! | **this: one stream per body, rounds a loop** | **15.76 (1.25x)** | **9.51 (1.57x)** |
    !! | 4 streams per body, rounds a loop | 19.71 (1.00x) | 11.42 (1.31x) |
    !! | 4 streams per body, rounds UNROLLED | 15.91 (1.24x) | 7.39 (2.02x) |
    !!
    !! So lane blocking is worth **nothing on gfortran** -- whose release profile carries
    !! `-funroll-loops`, and which is therefore indifferent to the round form -- and a further 29 %
    !! on ifx, but only when the ten rounds are also written out. That reproduces
    !! `feature_random_reference.md`'s "must use the unrolled kernel; with loop rounds it is a
    !! 1.3x-1.6x loss" **as an ifx-specific effect**, which is worth knowing before anyone quotes it
    !! as a general rule. Writing four lanes x ten rounds out costs roughly 800 lines per worker and
    !! a second hand-maintained copy of the cipher, which is exactly the duplication this module
    !! exists to avoid; `random_block`'s own header already schedules lane-blocked kernels for the
    !! phase that introduces the rest of the bulk tier. Take that work there, with a generator, not
    !! here.
    !!
    !! The `second` test is hoisted out of the loop rather than being recomputed per element, and
    !! `blk` with it: both are functions of `draw` alone.
    pure subroutine fill_streams_r64(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        real(real64), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m, blk, w0, w1, w2, w3
        logical :: second                           ! does `draw` sit in its block's second pair?
        ! `size(v, kind=int64)` for the reason `fill_r64` spells out: a default-kind length wraps
        ! above 2**31 elements and fails silently.
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        blk = (draw - 1_int64) / 2_int64
        second = (modulo(draw - 1_int64, 2_int64) == 1_int64)
        ! **`i0 + (k - 1)`, parenthesised, and the brackets are load-bearing.** Fortran evaluates
        ! `i0 + k - 1` left to right, so at the documented boundary -- a fill whose last stream is
        ! exactly `huge(int64)`, which the suite tests -- the intermediate `i0 + k` forms
        ! `huge + 1` and overflows, wraps, and the `- 1` brings it back to the right answer. The
        ! result is correct on both compilers and the arithmetic is undefined, which is precisely
        ! the shape feature_risks.md Risk-94 records the optimiser exploiting two functions away.
        ! Since `k >= 1`, `k - 1` is non-negative and `i0 + (k - 1)` cannot exceed the sum the
        ! precondition already bounds. Found by UBSan; see feature_risks.md Risk-112.
        do k = 1_int64, m
            call random_block(seed, i0 + (k - 1_int64), blk, w0, w1, w2, w3)
            if (second) then
                v(k) = to_real64(ior(ishft(w3, 32), w2))
            else
                v(k) = to_real64(ior(ishft(w1, 32), w0))
            end if
        end do
    end subroutine fill_streams_r64

    !> Fills `v` with one `real32` draw of each of `size(v)` consecutive streams, starting at `i0`.
    !!
    !! One word of four per stream, so this is the least block-efficient entry point in the module
    !! -- see `pf_random_fill_streams`' own note, where that is stated as contract rather than as a
    !! shortcoming. Measured 21.96 -> 14.96 ns (gfortran) and 14.67 -> 9.19 (ifx) against the scalar
    !! loop it replaces. Same shape as `fill_streams_r64`: `blk` and `slot` are functions of `draw`
    !! alone and are hoisted -- and one stream per body wins here too, beating the 2- and 4-lane
    !! prototypes on both compilers, so the `real64` verdict below carries across unchanged.
    pure subroutine fill_streams_r32(seed, i0, v, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        real(real32), intent(out) :: v(:)           !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m, blk, c0, c1, c2, c3
        integer :: slot                             ! 0..3 within one block; default kind is ample
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        blk = (draw - 1_int64) / 4_int64
        slot = int(modulo(draw - 1_int64, 4_int64), int32)
        do k = 1_int64, m
            call random_block(seed, i0 + (k - 1_int64), blk, c0, c1, c2, c3)
            select case (slot)
            case (0)
                v(k) = to_real32(c0)
            case (1)
                v(k) = to_real32(c1)
            case (2)
                v(k) = to_real32(c2)
            case default
                v(k) = to_real32(c3)
            end select
        end do
    end subroutine fill_streams_r32

    !> Fills `v` with consecutive `integer(int64)` draws of one stream, starting at `draw`.
    !!
    !! **One enciphering serves two draws.** An integer draw has stride 2, exactly like its `real64`
    !! sibling, so draws `d` and `d+1` for odd `d` are the two pairs of one block. The values are
    !! identical to a loop of `int_at_impl` by construction -- the same blocks in the same order --
    !! and the `lo`/`hi` normalisation is done once per call rather than once per element.
    !!
    !! **Shape: an alignment head, a ONE-BLOCK steady state, then a one-value tail** -- the
    !! `fill_r64` shape, minus its two-block interleave. Aligning once is what removes the whole of
    !! the per-element index arithmetic this loop used to carry: a signed division `(d-1)/2`, a
    !! `modulo(d-1, 2)`, and a `blk /= held` test whose real cost was not the compare but the
    !! loop-carried dependency it put on the block state, which stops the compiler overlapping one
    !! iteration's enciphering with the next.
    !!
    !! **Measured in the library, before and after, on machine B (gfortran 15.2.1, `--profile
    !! release`, 10M values, best of 5): 15.72 -> 12.04 ns per value, 1.31x**, with every untouched
    !! arm flat across the two builds (`fill_r64` 8.67/8.68, the cipher floors 12.04/12.06 and
    !! 6.05/6.05) so the cross-build noise floor is about 0.5 % and the gain is 30 times it. Probe
    !! arms predicted 1.47x-1.67x; the library form does not reach that, and the difference is the
    !! usual one between a contained procedure the compiler may specialise freely and a module
    !! procedure it may not. **Quote 1.31x, not the prediction.**
    !!
    !! **A second measurement is load-bearing and must be repeated after any change here: the SCALAR
    !! draw.** `pf_random_int_at` shares `int_reduce` with this loop, and the first version of this
    !! restructure made it 9 % slower without touching it -- see `int_reduce_retry` for the mechanism
    !! and the one-command check. It now sits within 1.2 % (25.6 -> 25.9), which is the cost of the
    !! cold retry call and is the price of the 31 % here.
    !!
    !! **The interleave is deliberately NOT ported, and this is the one place the `real64` sibling
    !! must not be copied wholesale.** Two blocks per body is worth 1.23x there and is a *regression*
    !! here: measured 9.79 against 10.09 ns per value on machine B under gfortran, reproducibly and
    !! far above that machine's 0.5 % floor. The reason is visible in the arithmetic -- the interleave
    !! exists to fill issue slots one 10-round Philox chain leaves idle, and on the `real64` path the
    !! only post-cipher work is a single multiply, so they really are idle; here the Lemire reduction
    !! is already the second instruction stream. Machine A measures the same change +4 %, so its
    !! *sign* differs by architecture, which is on its own a reason not to carry it.
    !!
    !! A rejection is never served from the block in hand: `int_reduce` re-keys and goes back to
    !! `bits_of` under a derived key, exactly as the scalar entry point does. There is still exactly
    !! one copy of the rejection rule, which is what that split is for.
    !!
    !! An earlier revision could do none of this, because the integer generic then had stride 4 and
    !! consumed a whole block per value; that comment said a block-walking form "returns different
    !! values" -- true then, and no longer true now that the strides agree.
    pure subroutine fill_draws_i64(seed, stream, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, a, s, position, blk
        integer(int64) :: w0, w1, w2, w3
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        a = min(lo, hi)
        s = width_of(a, max(lo, hi))
        if (narrow32_ok(s)) then                    ! the 32-bit grid; see NARROW32_CAP
            call fill_draws32_i64(seed, stream, v, a, s, draw)
            return
        end if
        k = 0_int64
        position = draw - 1_int64                   ! 0-based value index; `draw` >= 1, so >= 0
        ! Head: one value when `draw` lands on a block's SECOND pair, after which we are aligned.
        if (iand(position, 1_int64) /= 0_int64) then
            call random_block(seed, stream, ishft(position, -1), w0, w1, w2, w3)
            k = 1_int64
            v(1) = int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, draw)
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        ! Every draw index below is `draw + (something <= m - 1)`, with the inner sum parenthesised,
        ! so none can form `huge + 1` even when the fill ends exactly at the representable boundary
        ! -- the hazard `fill_streams_r64` spells out at length. Risk-112.
        do while (k + 2_int64 <= m)                 ! steady state: one block, two values
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k + 1_int64) = int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, draw + k)
            v(k + 2_int64) = int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, &
                                        draw + (k + 1_int64))
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final half-block
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(m) = int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, draw + (m - 1_int64))
        end if
    end subroutine fill_draws_i64

    !> `fill_draws_i64` narrowed to `integer(int32)`.
    !!
    !! The result is inside `[min(lo,hi), max(lo,hi)]` by construction, so the narrowing is exact --
    !! the same argument `pf_random_int_at_i32` rests on. The head/steady-state/tail shape is the one
    !! `fill_draws_i64` documents, written out again rather than shared, because sharing it would
    !! mean materialising an `integer(int64)` temporary the size of `v`.
    pure subroutine fill_draws_i32(seed, stream, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, a, s, position, blk
        integer(int64) :: w0, w1, w2, w3
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        a = int(min(lo, hi), int64)
        s = width_of(a, int(max(lo, hi), int64))
        if (narrow32_ok(s)) then                    ! the 32-bit grid; see NARROW32_CAP
            call fill_draws32_i32(seed, stream, v, a, s, draw)
            return
        end if
        k = 0_int64
        position = draw - 1_int64                   ! 0-based value index; `draw` >= 1, so >= 0
        if (iand(position, 1_int64) /= 0_int64) then            ! head: see `fill_draws_i64`
            call random_block(seed, stream, ishft(position, -1), w0, w1, w2, w3)
            k = 1_int64
            v(1) = int(int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, draw), int32)
            position = position + 1_int64
        end if
        blk = ishft(position, -1)
        do while (k + 2_int64 <= m)                 ! steady state: one block, two values
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k + 1_int64) = int(int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, &
                                            draw + k), int32)
            v(k + 2_int64) = int(int_reduce(ior(ishft(w3, 32), w2), a, s, seed, stream, &
                                            draw + (k + 1_int64)), int32)
            k = k + 2_int64
            blk = blk + 1_int64
        end do
        if (k < m) then                             ! tail: a final half-block
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(m) = int(int_reduce(ior(ishft(w1, 32), w0), a, s, seed, stream, &
                                  draw + (m - 1_int64)), int32)
        end if
    end subroutine fill_draws_i32

    !> Fills `v` with one `integer(int64)` draw of each of `size(v)` consecutive streams.
    !!
    !! Unlike `fill_draws_i64` this gives up nothing at all against the real-valued form on the same
    !! axis: that one already enciphered a block per value, because each element belongs to a
    !! different stream.
    !!
    !! **Do not "fix" this to hoist the loop-invariant setup out of the loop -- it was implemented,
    !! measured and reverted.** The body below re-derives, per element, the range normalisation
    !! `min(lo,hi)`/`width_of(...)` inside `int_at_impl`, and the block index and pair parity inside
    !! `bits_of`, all of which are constant across the loop; both `real64` siblings hoist exactly
    !! these and say so, so the asymmetry reads as an oversight. It is not, because **GCC already
    !! does it**: these private workers inline into the public specific (there is no
    !! `__parquet_random_MOD_fill_streams_i64` symbol at all), after which loop-invariant code motion
    !! lifts the whole setup. Hoisting by hand took a 373-instruction loop body to 364 -- neither
    !! version has a 128-bit subtract or a division inside the loop -- and measured 17.63 -> 17.54 ns
    !! per value on machine B (**1.005x**), with `fill_streams_i32` unchanged at 17.52 -> 17.53 and an
    !! untouched `fill_streams_r64` control flat at 14.71 across the two builds.
    !!
    !! The probe arms that predicted 1.04x-1.29x were comparing a *hoisted probe* arm against this
    !! *unhoisted library* one across the wrapper boundary, with no unhoisted probe arm to subtract;
    !! one added afterwards to settle it measures 25.12 against the hoisted 25.23, i.e. zero there
    !! too. See `feature_random_resample.md` stage 3 for both routes. A compiler that does NOT inline
    !! these workers would change the verdict, so this is a finding about the build and not about the
    !! source -- re-measure rather than assuming either answer.
    pure subroutine fill_streams_i64(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int64), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m
        integer(int64) :: a, sw, st, nblk, xw, x0, x1, x2, x3
        integer :: nj
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        ! The narrow rule must be reached INLINE here, not through `int_at_impl`. That procedure
        ! keeps its own narrow arm out of line so the scalar entry point stays inlined, and a
        ! per-element call to it costs this loop 60 % (17.0 -> 27.2 ns per value, measured). The
        ! stream axis gains only the cheaper reduction -- one block per value either way -- so this
        ! is a small win here and a large loss if forgotten.
        a = min(lo, hi)
        sw = width_of(a, max(lo, hi))
        if (narrow32_ok(sw)) then
            ! `random_block` DIRECTLY, not via `bits32_of` -- that helper has three-plus call sites
            ! and GCC emits it out of line, which costs this loop a call per element (17.0 -> 27.2
            ! ns per value, measured). `fill_draws32_i64`'s steady state avoids it for the same
            ! reason. Block and word are functions of `draw` alone, so both hoist.
            nblk = ishft(draw - 1_int64, -2)
            nj = int(iand(draw - 1_int64, 3_int64), int32)
            do k = 1_int64, m
                st = i0 + (k - 1_int64)
                call random_block(seed, st, nblk, x0, x1, x2, x3)
                select case (nj)
                case (0)
                    xw = x0
                case (1)
                    xw = x1
                case (2)
                    xw = x2
                case default
                    xw = x3
                end select
                v(k) = int_reduce32(xw, a, sw, seed, st, draw)
            end do
            return
        end if
        do k = 1_int64, m
            v(k) = int_at_impl(seed, i0 + (k - 1_int64), lo, hi, draw)
        end do
    end subroutine fill_streams_i64

    !> `fill_streams_i64` narrowed to `integer(int32)`; exact for the same reason.
    pure subroutine fill_streams_i32(seed, i0, v, lo, hi, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: i0            !! first stream index
        integer(int32), intent(out) :: v(:)         !! filled from streams `i0 .. i0+size(v)-1`
        integer(int32), intent(in) :: lo            !! one end of the closed range
        integer(int32), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: k, m, a, b
        integer(int64) :: lw, sw, st, nblk, xw, x0, x1, x2, x3
        integer :: nj
        m = size(v, kind=int64)
        if (m <= 0_int64) return                    ! a zero-sized fill is a defined no-op
        a = int(lo, int64)
        b = int(hi, int64)
        lw = min(a, b)                              ! see `fill_streams_i64` for why this is inline
        sw = width_of(lw, max(a, b))
        if (narrow32_ok(sw)) then
            nblk = ishft(draw - 1_int64, -2)        ! see `fill_streams_i64` for why not `bits32_of`
            nj = int(iand(draw - 1_int64, 3_int64), int32)
            do k = 1_int64, m
                st = i0 + (k - 1_int64)
                call random_block(seed, st, nblk, x0, x1, x2, x3)
                select case (nj)
                case (0)
                    xw = x0
                case (1)
                    xw = x1
                case (2)
                    xw = x2
                case default
                    xw = x3
                end select
                v(k) = int(int_reduce32(xw, lw, sw, seed, st, draw), int32)
            end do
            return
        end if
        do k = 1_int64, m
            v(k) = int(int_at_impl(seed, i0 + (k - 1_int64), a, b, draw), int32)
        end do
    end subroutine fill_streams_i32

    ! ================================================================================
    ! The integer rule -- exact rejection
    ! ================================================================================

    !> A uniform integer in `[min(lo,hi), max(lo,hi)]`, exactly unbiased.
    !!
    !! Lemire's multiply-shift with the exact rejection test, over the same 64-bit pattern
    !! `pf_random_bits_at` returns at this coordinate; `x * s` is formed as a full 128-bit product,
    !! whose high half is the answer's offset and whose low half decides acceptance. Only the last
    !! `2**64 mod s` candidates of the range are rejected, which is what makes the result exactly
    !! uniform rather than uniform to within 2**-64. The reduction itself lives in `int_reduce`, so
    !! that the bulk fills can reuse it against a candidate they enciphered once for two draws.
    !!
    !! A rejection RE-KEYS and re-enciphers the SAME counter, so consumption stays fixed in counter
    !! positions -- block ownership and prefix consistency are untouched and the answer is still a
    !! pure function of `(seed, i, draw)`. The loop is deliberately uncapped: accepting a candidate
    !! at a cap would reintroduce the bias the whole scheme exists to remove. It terminates with
    !! probability 1, the worst chain measured is 7, and at a range of a few million the retry
    !! probability is around 2**-40.
    !!
    !! **This reads exactly the two words `pf_random_at` and `pf_random_bits_at` read at the SAME
    !! coordinate -- stride 2, the same grid** -- so all three 64-bit generics agree on what draw
    !! `d` means, and separating two of them on the draw axis really does separate them. That is a
    !! deliberate contract choice, not an accident of the implementation; `pf_random32_at` still
    !! walks a finer grid of its own, so the full rule is stated once on the `pf_random_at`
    !! interface above and in `doc/pages/utilities/random.md`.
    !!
    !! An earlier revision gave this generic **stride 4**: draw `d` addressed the whole of block
    !! `d-1` and used only its first pair. That made `pf_random_int_at(seed, i, lo, hi, d)` read the
    !! same 64 bits as `pf_random_bits_at(seed, i, 2d-1)` -- verified 1000 of 1000 -- so an integer
    !! at draw 2 and a real at draw 3 were the same randomness, and "walk the draw axis" was unsafe
    !! for the next pairing anybody would write after the one the guide illustrated. It also left
    !! half of every enciphering unused, which is why a draw-axis integer fill could not amortise.
    !! Both are fixed by this mapping. Draw 1 is unchanged by the switch (block 0, first pair, under
    !! either rule); every draw from 2 up moved, which is why `pf_random_algorithm` is at `/v2`.
    !!
    !! What remains, and is contract: at ONE coordinate the three 64-bit generics are three
    !! *presentations* of the same 64 bits, not independent draws. The returned value still differs,
    !! because Lemire's reduction is a different function of those bits; but "different value" is
    !! not "independent", and at a small range the integer is a **deterministic function** of the
    !! real -- `pf_random_int_at(seed, i, 1, 6)` equals `1 + floor(6 * pf_random_at(seed, i))` for
    !! 20000 of 20000 streams. The rejection clause cannot rescue that, since at a realistic range
    !! the retry probability is around 2**-40, so the no-rejection case is effectively the only
    !! case. Take the two at different draws, or on different streams.
    pure function int_at_impl(seed, stream, lo, hi, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: lo            !! one end of the closed range
        integer(int64), intent(in) :: hi            !! the other end
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: a, s

        a = min(lo, hi)                             ! `lo > hi` swaps: the function is total
        s = width_of(a, max(lo, hi))                ! the width, read as an UNSIGNED 64-bit pattern
        if (narrow32_ok(s)) then
            ! **Out of line deliberately, and this shape was chosen by measurement over two
            ! rivals.** Inlining the narrow arm here puts two complete Philox encipherings in one
            ! body; GCC then gives up and pushes `int_at_impl` itself out of line, costing the WIDE
            ! scalar draw 13 %. Hoisting the enciphering above the fork so there is one
            ! `random_block` site and no call at all -- which sounds strictly better -- makes the
            ! body bigger still and costs gfortran the same 13 % while saving ifx 4 %. This shape
            ! costs gfortran 3.6 % and ifx 11.7 % on the wide path, which is the best worst case of
            ! the three. See `feature_random_resample.md` stage 5 for all six measurements, and
            ! `feature_risks.md` Risk-115 for why nothing here may be "simplified" without
            ! re-measuring both compilers.
            r = int_at_narrow32(seed, stream, a, s, draw)
            return
        end if
        r = int_reduce(bits_of(seed, stream, draw), a, s, seed, stream, draw)
    end function int_at_impl

    !> Lemire's reduction with the exact rejection loop, over a candidate already in hand.
    !!
    !! Split out from `int_at_impl` so that `fill_draws_i64`/`fill_draws_i32` can supply a candidate
    !! they enciphered once for two draws. There is exactly one copy of the rejection rule, which is
    !! the point: a second copy could drift, and a drifted rejection rule is a biased generator that
    !! passes every structural test.
    !!
    !! A rejection re-keys and re-enciphers the same counter, so it is `bits_of` under a derived key
    !! -- consumption stays fixed in counter positions, and the answer is still a pure function of
    !! `(seed, stream, draw)`. Two draws sharing a block also share their retry blocks, at the two
    !! pairs they already occupy, so no new correlation is introduced by the sharing.
    pure function int_reduce(x0, a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x0            !! the first candidate's 64-bit pattern
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, as an unsigned pattern; 0 = full
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: low, high

        if (s == 0_int64) then
            ! The whole int64 range: every pattern is in range, so there is nothing to reduce and
            ! nothing to reject. This is exactly `pf_random_bits_at` at the same coordinate.
            r = x0
            return
        end if

        call mulhilo64(x0, s, low, high)
        if (ult(low, s)) then
            ! Lemire's LAZY GUARD, and it is deliberately conservative: it fires whenever the
            ! candidate *might* be in the last partial block, and `int_reduce_retry` then computes
            ! the real threshold and decides. Being too eager costs a cold call; it cannot bias the
            ! result, because nothing here accepts or rejects anything.
            r = int_reduce_retry(x0, a, s, seed, stream, draw)
            return
        end if
        r = offset_by(a, high)
    end function int_reduce

    !> The rejection rule: the threshold, the retry loop, and the only copy of either.
    !!
    !! **Split out of `int_reduce` for a codegen reason, and the split is load-bearing.** This body
    !! contains `bits_of`, i.e. a whole Philox enciphering, so inlining it is cheap while a caller
    !! has one hot reduction site and ruinous when it has four. When `fill_draws_i64` was
    !! restructured into a two-values-per-block body, GCC's inline budget gave out and it emitted an
    !! out-of-line `int_reduce.isra.0` that the *scalar* entry point then had to call: measured
    !! 25.6 -> 28.0 ns per value for `pf_random_int_at` in a loop, a 9 % regression on public API
    !! caused by a change that touched only the bulk fill. With the cold half behind its own
    !! procedure, `int_reduce` is a multiply, a compare and an add, and inlines at every site again.
    !!
    !! **Check it after any change here** -- this must print nothing:
    !!
    !! ```bash
    !! objdump -d --no-show-raw-insn build/gfortran_*/parquet-fortran/src_parquet_random.f90.o \
    !!   | awk '/<__parquet_random_MOD_pf_random_int_at_i64>:/{p=1} p&&/^$/{exit} p' | grep call
    !! ```
    !!
    !! A rejection re-keys and re-enciphers the same counter, so it is `bits_of` under a derived key
    !! -- consumption stays fixed in counter positions, and the answer is still a pure function of
    !! `(seed, stream, draw)`. The loop is deliberately uncapped: accepting a candidate at a cap
    !! would reintroduce the bias the whole scheme exists to remove. It terminates with probability
    !! 1, and the worst chain measured is 7.
    pure function int_reduce_retry(x0, a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x0            !! the first candidate's 64-bit pattern
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, as an unsigned pattern; non-zero
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: x, low, high, threshold, attempt
        ! Keeping this OUT of line is the whole point of the split; with one call site an optimiser
        ! will otherwise inline it straight back into `int_reduce` and undo it. Both directives are
        ! ordinary comments to a compiler that does not know them, so neither is a portability
        ! hazard, and a compiler that ignores both is merely back to the slower shape.
!GCC$ ATTRIBUTES noinline :: int_reduce_retry
!DIR$ ATTRIBUTES NOINLINE :: int_reduce_retry

        attempt = 0_int64
        x = x0
        call mulhilo64(x, s, low, high)
        ! The division is reached only when the caller's lazy guard fired, which at a realistic
        ! width happens with probability about 2**-40. Do not hoist it into `int_reduce`.
        threshold = umod_2p64(s)
        do while (ult(low, threshold))
            attempt = attempt + 1_int64
            x = bits_of(retry_key_of(seed, attempt), stream, draw)
            call mulhilo64(x, s, low, high)
        end do
        r = offset_by(a, high)
    end function int_reduce_retry

    !> `fill_draws_i64` on the 32-bit grid: FOUR values per enciphering.
    !!
    !! Same head/steady-state/tail shape as the 64-bit form, with the alignment now to a block of
    !! four rather than a pair. The head enciphers its block once per value, which costs at most
    !! three redundant encipherings for the whole call and keeps the steady state branch-free.
    pure subroutine fill_draws32_i64(seed, stream, v, a, s, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, position, blk, w0, w1, w2, w3
        m = size(v, kind=int64)
        k = 0_int64
        position = draw - 1_int64
        do while (k < m .and. iand(position, 3_int64) /= 0_int64)        ! head, up to three values
            v(k + 1_int64) = int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k)
            k = k + 1_int64
            position = position + 1_int64
        end do
        blk = ishft(position, -2)
        do while (k + 4_int64 <= m)                 ! steady state: one block, four values
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k + 1_int64) = int_reduce32(w0, a, s, seed, stream, draw + k)
            v(k + 2_int64) = int_reduce32(w1, a, s, seed, stream, draw + (k + 1_int64))
            v(k + 3_int64) = int_reduce32(w2, a, s, seed, stream, draw + (k + 2_int64))
            v(k + 4_int64) = int_reduce32(w3, a, s, seed, stream, draw + (k + 3_int64))
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        do while (k < m)                            ! tail, up to three values
            v(k + 1_int64) = int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k)
            k = k + 1_int64
        end do
    end subroutine fill_draws32_i64

    !> `fill_draws32_i64` narrowed to `integer(int32)`.
    pure subroutine fill_draws32_i32(seed, stream, v, a, s, draw)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int32), intent(out) :: v(:)         !! filled with values `draw .. draw+size(v)-1`
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: draw          !! 1-based starting value index, already clamped
        integer(int64) :: k, m, position, blk, w0, w1, w2, w3
        m = size(v, kind=int64)
        k = 0_int64
        position = draw - 1_int64
        do while (k < m .and. iand(position, 3_int64) /= 0_int64)
            v(k + 1_int64) = int(int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k), int32)
            k = k + 1_int64
            position = position + 1_int64
        end do
        blk = ishft(position, -2)
        do while (k + 4_int64 <= m)
            call random_block(seed, stream, blk, w0, w1, w2, w3)
            v(k + 1_int64) = int(int_reduce32(w0, a, s, seed, stream, draw + k), int32)
            v(k + 2_int64) = int(int_reduce32(w1, a, s, seed, stream, &
                                              draw + (k + 1_int64)), int32)
            v(k + 3_int64) = int(int_reduce32(w2, a, s, seed, stream, &
                                              draw + (k + 2_int64)), int32)
            v(k + 4_int64) = int(int_reduce32(w3, a, s, seed, stream, &
                                              draw + (k + 3_int64)), int32)
            k = k + 4_int64
            blk = blk + 1_int64
        end do
        do while (k < m)
            v(k + 1_int64) = int(int_reduce32(bits32_of(seed, stream, draw + k), a, s, seed, stream, draw + k), int32)
            k = k + 1_int64
        end do
    end subroutine fill_draws32_i32

    !> The scalar narrow draw, kept out of line on purpose.
    !!
    !! See `int_at_impl`'s call site for why. The cost is one call on a path that then enciphers a
    !! whole Philox block, so it is small in relative terms; the alternative costs every *wide*
    !! scalar draw as well, which is far worse.
    pure function int_at_narrow32(seed, stream, a, s, draw) result(r)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
!GCC$ ATTRIBUTES noinline :: int_at_narrow32
!DIR$ ATTRIBUTES NOINLINE :: int_at_narrow32

        r = int_reduce32(bits32_of(seed, stream, draw), a, s, seed, stream, draw)
    end function int_at_narrow32

    !> Whether the 32-bit candidate rule may serve this width.
    pure function narrow32_ok(s) result(ok)
        integer(int64), intent(in) :: s             !! the width, as an unsigned pattern; 0 = full
        logical :: ok                               !! `.true.` when a 32-bit candidate suffices
        ok = (s >= 1_int64 .and. s <= NARROW32_CAP)
    end function narrow32_ok

    !> The 32-bit word this draw addresses: block `(d-1)/4`, word
    !! `(d-1) mod 4` -- which is exactly the grid `pf_random32_at` walks.
    pure function bits32_of(seed, stream, draw) result(x)
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index; `< 1` clamps to 1
        integer(int64) :: x                         !! the 32-bit candidate, in `[0, 2**32)`
        integer(int64) :: e, w0, w1, w2, w3
        e = max(draw, 1_int64) - 1_int64
        call random_block(seed, stream, ishft(e, -2), w0, w1, w2, w3)
        select case (int(iand(e, 3_int64), int32))
        case (0)
            x = w0
        case (1)
            x = w1
        case (2)
            x = w2
        case default
            x = w3
        end select
    end function bits32_of

    !> Lemire's reduction over a 32-bit candidate, exactly unbiased.
    !!
    !! `x < 2**32` and `s <= 2**24`, so `x * s` is below `2**56` and is an ordinary signed multiply:
    !! no 128-bit product, no unsigned compare, no fold back into an `int64` pattern. That is why
    !! this lever is worth more than the halved cipher work alone. The rejection test is the exact
    !! 32-bit analogue -- accept iff `low32(x*s) >= 2**32 mod s` -- so the result is exactly uniform
    !! rather than uniform to within `2**-32`.
    !!
    !! Split hot/cold exactly as `int_reduce`/`int_reduce_retry` are, and for the same reason: an
    !! A/B that let this one inline differently from the rule it is being compared against would be
    !! measuring the inline budget rather than the grid. See `feature_risks.md` Risk-114.
    !! **The threshold is LAZY, exactly as the 64-bit rule's is, and this is not a detail.** An
    !! eager `modulo(2**32, s)` is an integer division on every call. A bulk fill hoists it once and
    !! never notices; `pf_random_int_at` cannot, and paying it per call measured a **6 % regression
    !! on the scalar draw at every width, including widths the narrow grid never serves**. Deferring
    !! it behind the same conservative guard `int_reduce` uses -- fire whenever the candidate
    !! *might* be in the last partial block -- costs nothing and cannot bias anything, because
    !! nothing here accepts or rejects.
    pure function int_reduce32(x, a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: x             !! the 32-bit candidate
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width, `1 .. NARROW32_CAP`
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: p
        p = x * s
        if (iand(p, M32) < s) then                  ! lazy guard: `int_reduce`'s, at 32 bits
            r = int_reduce32_retry(a, s, seed, stream, draw)
            return
        end if
        r = a + ishft(p, -32)
    end function int_reduce32

    !> The 32-bit threshold and rejection loop, kept out of line.
    pure function int_reduce32_retry(a, s, seed, stream, draw) result(r)
        integer(int64), intent(in) :: a             !! the low end of the normalised range
        integer(int64), intent(in) :: s             !! the width
        integer(int64), intent(in) :: seed          !! the stream family's seed, for a re-key
        integer(int64), intent(in) :: stream        !! stream index
        integer(int64), intent(in) :: draw          !! 1-based value index, already clamped
        integer(int64) :: r                         !! a uniform integer in the closed range
        integer(int64) :: p, thr, attempt
!GCC$ ATTRIBUTES noinline :: int_reduce32_retry
!DIR$ ATTRIBUTES NOINLINE :: int_reduce32_retry

        ! Reached only when the caller's lazy guard fired, so the division is off the hot path.
        thr = modulo(TWO32, s)                      ! `2**32 mod s`; both operands positive
        p = bits32_of(seed, stream, draw) * s
        attempt = 0_int64
        do while (iand(p, M32) < thr)
            attempt = attempt + 1_int64
            ! Re-key and re-encipher the SAME counter, so consumption stays fixed in counter
            ! positions -- the rule `int_reduce_retry` follows, at the 32-bit grid's coordinate.
            p = bits32_of(retry_key_of(seed, attempt), stream, draw) * s
        end do
        r = a + ishft(p, -32)
    end function int_reduce32_retry

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
    !! **Route (e) now covers this, so the wrapping arm is the `#else` arm only.** It used to be
    !! "UB site 2 of 2, and the only one carried on BOTH sides of the fork" -- each 32x32 partial
    !! product can exceed `huge(int64)` and wrap -- and where a 128-bit kind exists that is no
    !! longer so. The remaining wrapping sites are both on the `#else` arm: this one, and
    !! `random_block`'s multiplies. See `feature_risks.md` Risk-95.
    !!
    !! **Why only ONE operand is split, and why the obvious spelling is wrong.** The full unsigned
    !! product reaches nearly 2**128, which does NOT fit a signed 128-bit integer -- so
    !! `iand(int(a,k128), MASK64_128) * iand(int(b,k128), MASK64_128)` **overflows**, and would add
    !! a third wrapping site while appearing to remove one. It measures faster than what ships here
    !! (a single wide multiply against two) and must not be adopted on that basis: Risk-95's first
    !! rule is that a wrapping measurement is not evidence. Splitting `b` alone bounds each product
    !! by 2**96 and every intermediate below 2**97, which is provably in range.
    !!
    !! **Cost.** Two wide multiplies against four narrow ones plus their carries: `pf_random_int_at`
    !! at a narrow range measured 26.14 -> 25.03 ns and at a rejecting width 48.12 -> 41.64 on
    !! machine B (gfortran 14.2.1, release flags), the larger part of the second figure coming from
    !! `mul64_lo_strict` on the retry path rather than from here. So this arm is both faster and
    !! free of undefined behaviour, which is why it is taken despite the strict 16-bit-limb spelling
    !! having been rejected on cost (1.31x gfortran / 2.05x ifx for the whole integer path).
    !!
    !! **The `#else` arm keeps every word of its former warning.** It is guarded by the suite's
    !! cross-implementation agreement sweep and by nothing else -- that is the only thing that would
    !! notice a compiler starting to exploit it. The width arithmetic used to be a third such site,
    !! carried on the same reasoning -- that ifx had been verified to wrap -- and ifx was then
    !! caught using the overflow's undefinedness to delete a branch somewhere else entirely (see
    !! `width_of`). Nothing in that finding says this site is safe; it says the evidence thought to
    !! make it safe was never evidence about this question. Treat a future agreement failure on that
    !! arm as the expected outcome rather than as a surprise.
    pure subroutine mulhilo64(a, b, low, high)
        integer(int64), intent(in) :: a             !! one factor, read as unsigned
        integer(int64), intent(in) :: b             !! the other factor, read as unsigned
        integer(int64), intent(out) :: low          !! bits 0..63 of the product
        integer(int64), intent(out) :: high         !! bits 64..127 of the product
#ifdef PF_INT128
        integer(k128) :: au, bhi, blo, t0, t1, s, wl, wh
        ! ONE operand is split, not both, and that is what keeps this in range. The full unsigned
        ! product reaches nearly 2**128 and so does NOT fit a signed 128-bit integer -- forming it
        ! as a single wide multiply of two unsigned-masked operands overflows, and would be a THIRD
        ! wrapping site rather than the removal of one. Splitting `b` into 32-bit halves bounds each
        ! product by 2**96 and every intermediate below 2**97, so nothing here can overflow at all.
        au = iand(int(a, k128), MASK64_128)
        bhi = ishft(iand(int(b, k128), MASK64_128), -32)
        blo = iand(int(b, k128), M32_128)
        t0 = au * blo                               ! < 2**96
        t1 = au * bhi                               ! < 2**96
        ! a*b = (t1 >> 32)*2**64 + s, where s gathers t1's low limb and the whole of t0.
        s = t0 + ishft(iand(t1, M32_128), 32)       ! < 2**97
        wl = iand(s, MASK64_128)
        wh = ishft(t1, -32) + ishft(s, -64)         ! below 2**64 because the product is
        if (wl >= TWO63_128) wl = wl - TWO64_128
        if (wh >= TWO63_128) wh = wh - TWO64_128
        low = int(wl, int64)
        high = int(wh, int64)
#else
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
#endif
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

    !> The low 64 bits of `a * b`, computed so that nothing ever overflows -- by the route (e) fork.
    !!
    !! **Neither arm may be replaced by a plain `int64` multiply, and this is blocker-grade rather
    !! than a precaution.** Written that way, gfortran folds the whole of `mix64` at `-O2` AND `-O3`,
    !! on three architectures and two major versions, to the single constant `z'7FFFFFFF00000000'`
    !! for every input -- so `pf_random_key` would return one key for every seed and every label,
    !! with no abort, no warning, and downstream output that still looks random.
    !!
    !! **Where a 128-bit kind exists the product is formed in it**, where two `int64` operands give
    !! at most 2**126 and so cannot overflow: there is no undefined behaviour left for an optimiser
    !! to fold from, which is why this arm does not reintroduce the miscompilation above. The low
    !! half is signedness-independent -- two's-complement multiplication gives
    !! `(a + 2**64 m)(b + 2**64 n) = ab (mod 2**64)` -- so the operands are used as they come and
    !! only the result is folded back into signed range, exactly as `width_of` does.
    !!
    !! **The `#else` arm keeps the 16-bit limbs**, and 16 is the load-bearing number: splitting into
    !! 32x32 products is NOT sufficient, since a 32x32 product still exceeds `int64`. On 16-bit limbs
    !! nothing exceeds 2**35, so that arm is correct by construction on any compiler at any
    !! optimisation level and needs no wide kind -- which is what makes it available to the compiler
    !! that has none.
    !!
    !! **Cost, and why it is not "once per stream family".** The limb form is sixteen 16x16 products;
    !! the wide form is one multiply. Measured on machine A (arm64, gfortran 15.2, release flags):
    !! `pf_random_key` 7.68 -> 4.66 ns (1.65x), and a rejecting-width `pf_random_int_at` 54.4 -> 42.4
    !! (1.28x) -- because `retry_key_of` calls `mix64` twice on **every retry**, so this sits on the
    !! integer draw's rejection path and not only on key derivation.
    pure function mul64_lo_strict(a, b) result(r)
        integer(int64), intent(in) :: a             !! one factor
        integer(int64), intent(in) :: b             !! the other factor
        integer(int64) :: r                         !! bits 0..63 of the product
#ifdef PF_INT128
        integer(k128) :: p
        p = iand(int(a, k128) * int(b, k128), MASK64_128)
        if (p >= TWO63_128) p = p - TWO64_128       ! fold to the signed pattern before narrowing
        r = int(p, int64)
#else
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
#endif
    end function mul64_lo_strict

    !> `pf_random_key`'s derivation, shared by both label kinds.
    pure function key_from(seed, label) result(r)
        integer(int64), intent(in) :: seed          !! the seed to derive from
        integer(int64), intent(in) :: label         !! which derived family
        integer(int64) :: r                         !! an independent seed
        r = mix64(ieor(mix64(seed), label))
    end function key_from

    ! ================================================================================
    ! The permutation bijection
    ! ================================================================================

    !> `pf_random_perm_at` for `integer(int32)` population size and index.
    !!
    !! The result is inside `[1, m]` by construction, so narrowing the `int64` worker's answer is
    !! exact -- the same argument `pf_random_int_at_i32` rests on.
    pure elemental function pf_random_perm_at_i32(seed, m, k) result(r)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int32), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int32), intent(in) :: k             !! 1-based position; clamped into `[1, m]`
        integer(int32) :: r                         !! element `k` of that permutation
        r = int(perm_at_impl(seed, int(m, int64), int(k, int64)), int32)
    end function pf_random_perm_at_i32

    !> `pf_random_perm_at` for `integer(int64)` population size and index.
    pure elemental function pf_random_perm_at_i64(seed, m, k) result(r)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: k             !! 1-based position; clamped into `[1, m]`
        integer(int64) :: r                         !! element `k` of that permutation
        r = perm_at_impl(seed, m, k)
    end function pf_random_perm_at_i64

    !> The permutation itself. **Two constructions, split at `perm_exact_max`.**
    !!
    !! For `m <= perm_exact_max` the answer is *ranked*, not enciphered: one exactly-uniform draw in
    !! `[0, m!)` and a Lehmer unranking, which is a bijection onto `S_m`. That composition is exactly
    !! uniform over all `m!` permutations -- by construction, with no mixing question, no round count
    !! and nothing to validate by measurement. See `perm_exact`.
    !!
    !! For `m >= perm_exact_max + 1` it is the Feistel network: split, encipher, cycle-walk, rejoin,
    !! then the parity correction. The walk is what turns a bijection on `[0, a*b)` into one on
    !! `[0, m)`: re-apply the network while the value is out of range. It terminates because the
    !! network is a bijection, so every orbit closes, and the orbit of a value below `m` must return
    !! to it. With `a*b` sitting on top of `m` it is entered essentially never -- measured at 1.0000
    !! applications per element at m = 10**6, 10**7 and 10**8.
    !!
    !! **The boundary costs nothing in consistency, which is the reason it is affordable at all.**
    !! Both sides are reached from here and from `perm_fill_i32`/`perm_fill_i64` through the *same*
    !! two procedures, so `perm(k) == pf_random_perm_at(seed, m, k)` holds because there is one
    !! computation rather than two that have to be kept equal by testing. And the boundary is a
    !! compile-time constant fixed by `20! < huge(int64)`, so it cannot drift.
    pure function perm_at_impl(seed, m, k) result(r)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: k             !! 1-based position, not yet clamped
        integer(int64) :: r                         !! element `k`, in `[1, m]`
        integer(int64) :: a, b, rk(perm_rounds), x, n, small(perm_exact_max)
        integer :: nr
        n = m
        if (n < 1_int64) n = 1_int64                ! a degenerate population is one element
        x = k - 1_int64
        if (x < 0_int64) x = 0_int64                ! clamp rather than report; see the interface doc
        if (x >= n) x = n - 1_int64
        if (n == 1_int64) then
            r = 1_int64
            return
        end if
        if (perm_use_exact(n)) then
            call perm_exact(seed, n, small)
            r = small(x + 1_int64)
            return
        end if
        call perm_factors(n, a, b)
        nr = perm_round_count()
        call perm_round_keys(seed, nr, rk)
        do
            x = perm_feistel(rk, nr, a, b, x)
            if (x < n) exit
        end do
        r = perm_swap01(perm_parity_flip(seed, n), x) + 1_int64
    end function perm_at_impl

    !> Whether population `m` takes the exact path. See `perm_exact` and `perm_exact_max`.
    pure function perm_use_exact(m) result(yes)
        integer(int64), intent(in) :: m             !! population size, at least 2
        logical :: yes                              !! `.true.` when `m` is ranked rather than enciphered
        yes = (m <= int(perm_exact_max, int64)) .and. .not. perm_dbg_force_feistel
    end function perm_use_exact

    !> Rounds the Feistel will actually run: `perm_rounds`, or whatever a test has forced.
    pure function perm_round_count() result(nr)
        integer :: nr                               !! effective round count, always at least 1
        nr = perm_rounds
        if (perm_dbg_rounds > 0) nr = perm_dbg_rounds
    end function perm_round_count

    !> The whole permutation of `1 .. m` for `m <= perm_exact_max`, EXACTLY uniform.
    !!
    !! **This is the only part of the module whose uniformity is a theorem rather than a
    !! measurement, and it covers precisely the sizes where the Feistel was worst.** `pf_random_int_at`
    !! is exactly uniform over any closed range -- that is what its rejection step buys -- and Lehmer
    !! unranking is a bijection from `[0, m!)` onto `S_m`. The composition of an exactly uniform draw
    !! with a bijection is exactly uniform, so there is nothing here for an ensemble test to find.
    !!
    !! **`m` is the STREAM index, not part of the key.** Reducing one candidate into `[0, 5!)` and
    !! into `[0, 6!)` would give two ranks that are both monotone in the same 64-bit word, so a
    !! program asking for a permutation of 5 and one of 6 under the same seed would get two strongly
    !! correlated answers -- structure a uniform construction does not have. Separate streams are
    !! separate Philox counters, so the ranks are independent.
    !!
    !! **`perm_family_label` is not optional either**: without it the rank is literally the value
    !! `pf_random_int_at(seed, m, ...)` hands the same caller, so a program using both families would
    !! find its permutation locked to its own integer draws.
    !!
    !! Cost is one draw plus `m-1` divisions and an `O(m**2)/2` selection scan, all bounded by
    !! `perm_exact_max`; the pool lives on the stack and nothing is allocated.
    pure subroutine perm_exact(seed, m, p)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size, `2 <= m <= perm_exact_max`
        integer(int64), intent(out) :: p(perm_exact_max) !! `p(1:m)` is the permutation of `1 .. m`
        integer(int64) :: rank, rem, d, pool(perm_exact_max)
        integer :: i, j, mm, dd
        mm = int(m)
        rank = int_at_impl(key_from(seed, perm_family_label), m, 0_int64, perm_fact(mm) - 1_int64, 1_int64)
        do i = 1, mm
            pool(i) = int(i, int64)
        end do
        rem = rank
        do i = 1, mm
            ! `rem < (mm-i+1)!` on entry, so `d < mm-i+1` and the pool index is always in range.
            d = rem / perm_fact(mm - i)
            rem = rem - d * perm_fact(mm - i)
            dd = int(d)
            p(i) = pool(dd + 1)
            do j = dd + 1, mm - i
                pool(j) = pool(j + 1)
            end do
        end do
        do i = mm + 1, perm_exact_max
            p(i) = 0_int64                          ! never read; defined so nothing can read it
        end do
    end subroutine perm_exact

    !> Whether this seed's permutation is composed with the transposition `(0 1)`.
    !!
    !! **This is the parity correction, and it is correctness rather than mixing.** A Feistel over
    !! `Z_a x Z_b` cannot reach an odd permutation when `a == b` and `a` is odd: every round is a
    !! product of `a` cyclic shifts of `Z_a` (or of `Z_b`), a cyclic shift of `Z_q` by `t` has parity
    !! `(-1)**(q - gcd(q, t))` which is always even for odd `q`, and the half-swap between rounds is
    !! likewise even. So at `m = 25, 49, 81, 121, ...` -- `q**2` with `q` odd -- **exactly half of
    !! `S_m` is unreachable at any round count**, and the fraction of odd permutations reads 0.0 %.
    !! No amount of mixing closes that; only composing with an odd permutation does.
    !!
    !! One transposition is enough because it flips the parity of whatever the network produced, and
    !! the bit deciding it is unbiased: 0.4997 ones over 2 000 000 seeds, `z = -0.82`. Applying it
    !! after the cycle walk keeps it a permutation of `[0, m)` for every `m >= 2`.
    !!
    !! **`m` IS FOLDED INTO THE KEY, and dropping it re-creates a defect no single-`m` test can
    !! see.** Where the network is parity-locked its own contribution is *constant*, so the answer's
    !! parity is exactly this bit -- and if the bit depended on the seed alone, every locked size
    !! would share one parity under a given seed. Measured before the fold: pairwise agreement
    !! **1.0000** across m = 25, 49, 81, 121, 169, where a uniform pair agrees half the time; after
    !! it, 0.4984-0.5046. Each size on its own looked perfectly uniform in both cases, which is why
    !! `test_perm_parity` cannot see this and `test_perm_parity_cross_m` exists.
    !!
    !! The locked set is wider than the exact squares, which is worth knowing before deciding this
    !! is a corner case: sweeping raw network parity over m = 21..400 finds a band below each odd
    !! square -- 80-81, 119-121, 166-169, 221-225, 284-289, 354-361 -- exactly locked at `q**2` and
    !! 98-99 % determined just below it, where the cycle walk perturbs it without freeing it. That
    !! band grows as `sqrt(m)`.
    !!
    !! **Do not "simplify" this away because every structural test still passes without it** -- they
    !! all do. A whole-coset loss is invisible to fixed points, cycle counts, position marginals and
    !! inversions alike; only a parity test sees it. See `feature_risks.md` Risk-116.
    pure function perm_parity_flip(seed, m) result(f)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size; see the note above
        logical :: f                                !! `.true.` when outputs 0 and 1 are swapped
        integer(int64) :: pk
        f = .false.
        if (perm_dbg_no_parity) return
        ! `m` rides in on the key material of the two evaluations that were already happening, so
        ! this costs two `ieor`s and no extra `perm_mix2`. Both halves of `m` are folded, or two
        ! populations differing only above bit 32 would share a bit.
        pk = perm_mix2(ieor(int(perm_parity_key, int64) * perm_c1, m), iand(seed, M32))
        pk = ieor(pk, perm_mix2(ieor(int(perm_parity_key, int64), ishft(m, -32)), &
                                iand(ishft(seed, -32), M32)))
        f = iand(pk, 1_int64) == 1_int64
    end function perm_parity_flip

    !> Applies the parity correction to one already-walked output in `[0, m)`.
    pure function perm_swap01(flip, y) result(z)
        logical, intent(in) :: flip                 !! from `perm_parity_flip`
        integer(int64), intent(in) :: y             !! an output in `[0, m)`, `m >= 2`
        integer(int64) :: z                         !! `y`, with 0 and 1 exchanged when `flip`
        z = y
        if (flip .and. y <= 1_int64) z = 1_int64 - y
    end function perm_swap01

    !> The width rule: `a = ceil(sqrt(m))`, `b = ceil(m/a)`, so `a*b` sits just above `m`.
    !!
    !! **This is contract, not tuning** -- it decides the cycle-walk orbit and therefore every value.
    !! It was chosen by measurement over two power-of-two alternatives: a balanced rule (`b` even)
    !! walks up to 2.68 applications per element at m = 10**8, an unbalanced one 1.34 and only when
    !! `ceil(log2(m))` happens to be odd, and this one 1.0000 at every size measured. The price is a
    !! division per application instead of a mask, which measured 22 % -- worth paying whenever the
    !! walk ratio exceeds about 1.22, which is most of its range.
    !!
    !! `a` is capped at `perm_a_max` so that `a * a` cannot overflow while the loops correct it.
    pure subroutine perm_factors(m, a, b)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(out) :: a            !! left factor, `ceil(sqrt(m))`
        integer(int64), intent(out) :: b            !! right factor, `ceil(m/a)`
        a = int(sqrt(real(m, real64)), int64)
        if (a < 1_int64) a = 1_int64
        if (a > perm_a_max) a = perm_a_max
        do while (a < perm_a_max .and. a * a < m)
            a = a + 1_int64
        end do
        do while (a > 1_int64 .and. (a - 1_int64) * (a - 1_int64) >= m)
            a = a - 1_int64
        end do
        b = (m + a - 1_int64) / a
    end subroutine perm_factors

    !> One Feistel round-function evaluation: two multiplies, and **no overflow is possible**.
    !!
    !! **Every operand is masked to 31 bits before each multiply, and that is a correctness
    !! requirement rather than tidiness.** The multipliers are below `2**32`, so each product is
    !! bounded by `(2**31 - 1) * (2**32 - 1) < 2**63` and no signed overflow can occur.
    !! `feature_risks.md` Risk-94 records this repository being caught with a wrapping multiply that
    !! *measured* correct while the optimiser used the overflow's undefinedness to delete a branch
    !! two functions away, so a new kernel must be free of it by construction. **Do not remove the
    !! masks**; the alternative, routing through `mul64_lo_strict`, is correct but builds the product
    !! from 16-bit limbs on the wrapping arm and is far too expensive for a per-round path.
    !!
    !! A Feistel network is a bijection for **any** round function, so the mask costs a little
    !! mixing and can cost nothing else. What it costs was measured: nothing on gfortran, and it is
    !! slightly *faster* than the unmasked form on ifx.
    pure function perm_mix2(rk, x) result(w)
        integer(int64), intent(in) :: rk            !! this round's key
        integer(int64), intent(in) :: x             !! the half being mixed, below `2**32`
        integer(int64) :: w                         !! a mixed word, below `2**32`
        integer(int64) :: z
        z = ieor(x, rk)
        z = iand(z, perm_m31) * perm_c1
        z = ieor(z, ishft(z, -29))
        z = iand(z, perm_m31) * perm_c2
        z = ieor(z, ishft(z, -31))
        w = iand(z, M32)
    end function perm_mix2

    !> The round keys, derived from the seed once per call.
    !!
    !! Only the first `nr` are derived; the tail is zeroed rather than left undefined, so that a
    !! forced round count cannot leave a sanitiser reading uninitialised stack. Two `perm_mix2`
    !! evaluations per key, which is why the SCALAR path's cost is dominated by this schedule rather
    !! than by the network -- 16 rounds is 32 evaluations of setup against 16 of work. The bulk forms
    !! derive it once per call and so do not care.
    pure subroutine perm_round_keys(seed, nr, rk)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in) :: nr                   !! rounds actually to be run
        integer(int64), intent(out) :: rk(perm_rounds)  !! one key per round; `rk(nr+1:)` is zero
        integer :: j
        do j = 1, nr
            rk(j) = perm_mix2(int(j, int64) * perm_c1, iand(seed, M32))
            rk(j) = ieor(rk(j), perm_mix2(int(j, int64), iand(ishft(seed, -32), M32)))
        end do
        do j = nr + 1, perm_rounds
            rk(j) = 0_int64
        end do
    end subroutine perm_round_keys

    !> One application of the network on `[0, a*b)`; a bijection for any round function.
    !!
    !! Splits `x` into `(l, r)` -- the one integer division on the scalar path -- and hands off to
    !! `perm_feistel_lr`, which is where the rounds actually are. The bulk fill skips this split
    !! entirely because its loop indices already are `(l, r)`; see that function's own note.
    pure function perm_feistel(rk, nr, a, b, x) result(y)
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: x             !! input in `[0, a*b)`
        integer(int64) :: y                         !! output in `[0, a*b)`
        integer(int64) :: l, r
        l = x / b
        r = x - l * b
        y = perm_feistel_lr(rk, nr, a, b, l, r)
    end function perm_feistel

    !> The rounds themselves, from an ALREADY-SPLIT input -- the shape the bulk fill uses.
    !!
    !! One element, expressed as a block of one, so that `perm_feistel_block` stays the module's
    !! only copy of the round loop. The two `(1)`-shaped locals cost 24 bytes of stack against a
    !! scalar path that already derives a 16-key schedule per call.
    pure function perm_feistel_lr(rk, nr, a, b, l0, r0) result(y)
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: l0            !! left half of the input, in `[0, a)`
        integer(int64), intent(in) :: r0            !! right half of the input, in `[0, b)`
        integer(int64) :: y                         !! output in `[0, a*b)`
        integer(int64) :: l(1), r(1), yy(1)
        l(1) = l0
        r(1) = r0
        call perm_feistel_block(rk, nr, a, b, 1, l, r, yy)
        y = yy(1)
    end function perm_feistel_lr

    !> `nb` independent elements through `nr` rounds. **The module's only copy of the round loop.**
    !!
    !! **The scalar and bulk entry points differ only in how they obtain `(l, r)`**, and both reach
    !! the rounds through here, so their agreement is structural rather than something two
    !! implementations have to be kept equal by testing. `feature_risks.md` Risk-109 is the rule that
    !! it stays that way, and it survived the transposition below intact.
    !!
    !! **The loops are ROUNDS OUTSIDE, ELEMENTS INSIDE, and that order is the whole point.** Run the
    !! other way round -- all `nr` rounds for one element, then the next element -- each element is a
    !! sequential dependency chain `nr` long and the loop runs at the latency of that chain instead
    !! of at the throughput of the machine. This way every round is a straight-line pass over `nb`
    !! independent values. Measured on machine B at `m = 10**6` and 16 rounds: **2.07x** at plain
    !! `-O3`, in a build containing no vector instruction at all -- the gain is pure instruction-level
    !! parallelism and needs no ISA assumption -- and **10.77x** with `-march=native`, which lands the
    !! 16-round kernel below the cost of the old four-round one. Do not reorder these loops.
    !!
    !! Three arithmetic details are worth keeping. The factors swap every round, which is why
    !! `perm_rounds` must be even -- an odd count leaves the output encoded against transposed
    !! factors. `t = l + F` needs at most **one** conditional subtraction because `l < p` and `F < p`,
    !! so no division is needed to reduce it; that subtraction is also what makes the round a
    !! bijection at all, and removing it makes the caller's cycle-walk **non-terminating** rather than
    !! merely wrong. And the multiply-shift takes the mixed word down to 31 bits before multiplying by
    !! `p`, which bounds that product below `2**63` for every `m` a caller can name -- shifting a full
    !! 32-bit word instead would overflow above `m ~ 2**62`.
    pure subroutine perm_feistel_block(rk, nr, a, b, nb, l, r, y)
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer, intent(in) :: nb                   !! elements in this block, at least 1
        integer(int64), intent(inout) :: l(nb)      !! in: left halves in `[0, a)`; out: enciphered
        integer(int64), intent(inout) :: r(nb)      !! in: right halves in `[0, b)`; out: enciphered
        integer(int64), intent(out) :: y(nb)        !! the rejoined outputs, in `[0, a*b)`
        integer(int64) :: t, p, q, sw
        integer :: e, j
        p = a
        q = b
        do j = 1, nr
            do e = 1, nb
                t = l(e) + ishft(iand(perm_mix2(rk(j), r(e)), perm_m31) * p, -31)
                if (t >= p) t = t - p
                l(e) = r(e)
                r(e) = t
            end do
            sw = p
            p = q
            q = sw
        end do
        do e = 1, nb
            y(e) = l(e) * q + r(e)
        end do
    end subroutine perm_feistel_block

    ! ---- The bulk forms ----

    !> `pf_random_permutation` for an `integer(int32)` result array.
    subroutine pf_random_permutation_i32(perm, seed, threads)
        integer(int32), intent(out) :: perm(:)      !! filled with the permutation of `1 .. size(perm)`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call perm_fill_i32(seed, size(perm, kind=int64), size(perm, kind=int64), perm, threads)
    end subroutine pf_random_permutation_i32

    !> `pf_random_permutation` for an `integer(int64)` result array.
    subroutine pf_random_permutation_i64(perm, seed, threads)
        integer(int64), intent(out) :: perm(:)      !! filled with the permutation of `1 .. size(perm)`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call perm_fill_i64(seed, size(perm, kind=int64), size(perm, kind=int64), perm, threads)
    end subroutine pf_random_permutation_i64

    !> `pf_random_subset` for an `integer(int32)` result and an `integer(int32)` population size.
    subroutine pf_random_subset_i32_i32(idx, m, seed, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int32), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), int(m, int64), .true.)
        call perm_fill_i32(seed, int(m, int64), size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i32_i32

    !> `pf_random_subset` for an `integer(int32)` result and an `integer(int64)` population size.
    subroutine pf_random_subset_i32_i64(idx, m, seed, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int64), intent(in) :: m             !! population size; must not exceed `huge(int32)`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), m, .true.)
        call perm_fill_i32(seed, m, size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i32_i64

    !> `pf_random_subset` for an `integer(int64)` result and an `integer(int32)` population size.
    subroutine pf_random_subset_i64_i32(idx, m, seed, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int32), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), int(m, int64), .false.)
        call perm_fill_i64(seed, int(m, int64), size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i64_i32

    !> `pf_random_subset` for an `integer(int64)` result and an `integer(int64)` population size.
    subroutine pf_random_subset_i64_i64(idx, m, seed, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with the first `size(idx)` elements
        integer(int64), intent(in) :: m             !! population size; the permutation is of `1 .. m`
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer, intent(in), optional :: threads    !! thread request; absent means automatic
        call subset_check(size(idx, kind=int64), m, .false.)
        call perm_fill_i64(seed, m, size(idx, kind=int64), idx, threads)
    end subroutine pf_random_subset_i64_i64

    !> The `pf_random_subset` preconditions, in one place so all four specifics state them alike.
    !!
    !! A zero-sized request returns without checking anything: it asks for no element, so neither
    !! `m` nor the representability of an element is a question about it.
    subroutine subset_check(n, m, narrow)
        integer(int64), intent(in) :: n             !! requested subset size, `size(idx)`
        integer(int64), intent(in) :: m             !! population size, widened
        logical, intent(in) :: narrow               !! `.true.` when `idx` is `integer(int32)`
        character(len=32) :: t1, t2
        if (n <= 0_int64) return
        if (m < 1_int64) then
            write (t1, '(i0)') m
            error stop "pf_random_subset: population size m must be at least 1, got " // trim(t1)
        end if
        if (n > m) then
            write (t1, '(i0)') n
            write (t2, '(i0)') m
            error stop "pf_random_subset: subset size " // trim(t1) // &
                       " exceeds population size " // trim(t2)
        end if
        if (narrow .and. m > int(huge(1_int32), int64)) then
            write (t1, '(i0)') m
            error stop "pf_random_subset: population size " // trim(t1) // &
                       " exceeds huge(int32); the elements need an integer(int64) array"
        end if
    end subroutine subset_check

    ! ---- pf_random_resample: draw with replacement -------------------------------------------

    !> The `pf_random_resample` preconditions, in one place so all twelve specifics state them alike.
    !!
    !! A zero-sized request returns without checking anything: it asks for no element, so neither `m`
    !! nor the representability of an element is a question about it.
    !!
    !! **The absent `n > m` test is the one line separating this from `subset_check`**, and it is
    !! absent on purpose: drawing with replacement has no such bound, and `n == m` is the most
    !! ordinary bootstrap there is. See the `pf_random_resample` interface.
    subroutine resample_check(n, m, narrow)
        integer(int64), intent(in) :: n             !! requested sample size, `size(idx)`
        integer(int64), intent(in) :: m             !! population size, widened
        logical, intent(in) :: narrow               !! `.true.` when `idx` is `integer(int32)`
        character(len=32) :: t1
        if (n <= 0_int64) return
        if (m < 1_int64) then
            write (t1, '(i0)') m
            error stop "pf_random_resample: population size m must be at least 1, got " // trim(t1)
        end if
        if (narrow .and. m > int(huge(1_int32), int64)) then
            write (t1, '(i0)') m
            error stop "pf_random_resample: population size " // trim(t1) // &
                       " exceeds huge(int32); the elements need an integer(int64) array"
        end if
    end subroutine resample_check

    !> The resample worker, `integer(int32)` result. `resample_i64` carries the design note.
    !!
    !! The early return after the check keeps `int(m, int32)` from being evaluated for a zero-sized
    !! request, which is not validated at all and so may still carry an `m` above `huge(int32)` --
    !! an out-of-range conversion, and a standard violation whatever a given compiler does with it.
    !!
    !! **It is defensive, not load-bearing, and that was established by mutation rather than
    !! assumed**: deleting it changes no observable behaviour, because `fill_draws_i32` returns on a
    !! zero-sized `v` before it reads `hi` at all. The whole suite passes without it, including under
    !! `--profile debug`. Keep it anyway -- it costs one comparison on a path that then returns --
    !! but do not expect a test to defend it, and do not add one that pins a wrapped value.
    subroutine resample_i32(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size, widened
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index, already defaulted
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: n, chunk, lo, hi
        integer :: nth, t
        n = size(idx, kind=int64)
        call resample_check(n, m, .true.)
        if (n <= 0_int64) return
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call fill_draws_i32(seed, stream, idx, 1_int32, int(m, int32), 1_int64)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call fill_draws_i32(seed, stream, idx(lo:hi), 1_int32, int(m, int32), lo)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine resample_i32

    !> The resample worker, `integer(int64)` result.
    !!
    !! **There is no resample-specific arithmetic here, and that is the design.** `pf_random_resample`
    !! is `pf_random_fill_draws` over `1 .. m` starting at draw 1, so it delegates rather than
    !! reimplementing: one integer rule, one rejection test, one grid. Every value it can return is
    !! already frozen by `pf_random_algorithm`, which is why this procedure needs no golden vectors of
    !! its own -- what the suite asserts instead is the identity with the fill and with the scalar
    !! draw, which is what would break if a future optimisation moved one of the three.
    !!
    !! **Threading splits the DRAW axis, and that is what makes it bit-identical rather than merely
    !! equivalent.** Element `k` is a pure function of `(seed, stream, k)`, so a chunk covering
    !! elements `lo .. hi` is exactly `fill_draws_i64` started at draw `lo` -- no per-thread state, no
    !! reduction, nothing to order. Chunk boundaries land anywhere, including in the middle of a
    !! block, which the fill's alignment head already handles; before the stage-2 restructure gave it
    !! one, an arbitrary starting draw would have needed a special case here. `num_threads(nth)` is
    !! load-bearing for the reason `perm_fill_i32` records at length: without it OpenMP opens the
    !! default team, 192 on machine B, on every call whatever the caller asked for.
    !!
    !! The work floor inside `random_threads` applies to an explicit `threads=` as well as to the
    !! automatic answer, so a small resample stays serial even when threading is requested -- a
    !! property of the work rather than of the caller's intent.
    subroutine resample_i64(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index, already defaulted
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: n, chunk, lo, hi
        integer :: nth, t
        n = size(idx, kind=int64)
        call resample_check(n, m, .false.)
        if (n <= 0_int64) return
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call fill_draws_i64(seed, stream, idx, 1_int64, m, 1_int64)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call fill_draws_i64(seed, stream, idx(lo:hi), 1_int64, m, lo)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine resample_i64

    !> `pf_random_resample`, `integer(int32)` result and `integer(int32)` population, stream 1.
    subroutine pf_random_resample_i32_i32(idx, m, seed)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i32(idx, int(m, int64), seed, 1_int64)
    end subroutine pf_random_resample_i32_i32

    !> `pf_random_resample`, `integer(int32)` result and `integer(int64)` population, stream 1.
    subroutine pf_random_resample_i32_i64(idx, m, seed)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i32(idx, m, seed, 1_int64)
    end subroutine pf_random_resample_i32_i64

    !> `pf_random_resample`, `integer(int64)` result and `integer(int32)` population, stream 1.
    subroutine pf_random_resample_i64_i32(idx, m, seed)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i64(idx, int(m, int64), seed, 1_int64)
    end subroutine pf_random_resample_i64_i32

    !> `pf_random_resample`, `integer(int64)` result and `integer(int64)` population, stream 1.
    subroutine pf_random_resample_i64_i64(idx, m, seed)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        call resample_i64(idx, m, seed, 1_int64)
    end subroutine pf_random_resample_i64_i64

    !> `pf_random_resample` with an `integer(int32)` stream; `integer(int32)` result and population.
    !!
    !! **The stream-carrying specifics are separate procedures rather than one with an optional
    !! dummy**, because an optional argument that differs only by kind cannot be the sole
    !! disambiguator in a generic interface -- a call omitting it would match both. This is the split
    !! CLAUDE.md's "Public numeric arguments" note prescribes and `parquet_open_reader` already uses.
    subroutine pf_random_resample_i32_i32_s32(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, int(m, int64), seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i32_i32_s32

    !> `pf_random_resample` with an `integer(int32)` stream; `int32` result, `int64` population.
    subroutine pf_random_resample_i32_i64_s32(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, m, seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i32_i64_s32

    !> `pf_random_resample` with an `integer(int32)` stream; `int64` result, `int32` population.
    subroutine pf_random_resample_i64_i32_s32(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, int(m, int64), seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i64_i32_s32

    !> `pf_random_resample` with an `integer(int32)` stream; `int64` result and population.
    subroutine pf_random_resample_i64_i64_s32(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int32), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, m, seed, int(stream, int64), threads)
    end subroutine pf_random_resample_i64_i64_s32

    !> `pf_random_resample` with an `integer(int64)` stream; `int32` result and population.
    subroutine pf_random_resample_i32_i32_s64(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, int(m, int64), seed, stream, threads)
    end subroutine pf_random_resample_i32_i32_s64

    !> `pf_random_resample` with an `integer(int64)` stream; `int32` result, `int64` population.
    subroutine pf_random_resample_i32_i64_s64(idx, m, seed, stream, threads)
        integer(int32), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i32(idx, m, seed, stream, threads)
    end subroutine pf_random_resample_i32_i64_s64

    !> `pf_random_resample` with an `integer(int64)` stream; `int64` result, `int32` population.
    subroutine pf_random_resample_i64_i32_s64(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int32), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, int(m, int64), seed, stream, threads)
    end subroutine pf_random_resample_i64_i32_s64

    !> `pf_random_resample` with an `integer(int64)` stream; `int64` result and population.
    subroutine pf_random_resample_i64_i64_s64(idx, m, seed, stream, threads)
        integer(int64), intent(out) :: idx(:)       !! filled with `size(idx)` draws from `1 .. m`
        integer(int64), intent(in) :: m             !! population size
        integer(int64), intent(in) :: seed          !! the stream family's seed
        integer(int64), intent(in) :: stream        !! replicate index
        integer, intent(in), optional :: threads    !! worker count; absent means automatic
        call resample_i64(idx, m, seed, stream, threads)
    end subroutine pf_random_resample_i64_i64_s64

    !> How many threads a bulk permutation of `n` elements should use.
    !!
    !! **Both OpenMP rules come from `parquet_settings_base`** -- `parquet_auto_thread_count` for
    !! the automatic answer and `parquet_nested_team_unsafe` for the Risk-104 deadlock guard on an
    !! explicit request. This module deliberately holds no copy of either; CLAUDE.md's auto-threading
    !! note names a further copy as the mistake, and `pf_sort_threads` asks the identical questions.
    !!
    !! What is specific to this module is the **work floor**. It applies to an explicit `threads=`
    !! as well as to the automatic answer, because it asks whether the array is worth splitting at
    !! all -- a property of the work, not of the caller's intent -- and threading a small
    !! permutation is measurably *worse* than not threading it.
    integer function random_threads(n, threads) result(nth)
        integer(int64), intent(in) :: n             !! elements to produce
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: want, floor_per, by_work
        if (present(threads)) then
            want = max(1_int64, int(threads, int64))
            if (parquet_nested_team_unsafe()) want = 1_int64
        else
            want = int(parquet_auto_thread_count(parquet_get_random_threads()), int64)
        end if
        floor_per = parquet_get_random_parallel_min_elements()
        if (floor_per > 0_int64) then
            by_work = n / floor_per                 ! how many threads this much work can feed
            if (want > by_work) want = by_work
        end if
        if (want > n) want = n                      ! never more threads than elements
        if (want < 1_int64) want = 1_int64
        nth = int(want, int32)
    end function random_threads

    !> Exposes `random_threads` for testing. **Test-only**; no library code calls it.
    !!
    !! Public because the rule it reports lives behind a private procedure in a module with no
    !! `bind(C)` surface, so the C++-side debug-hook convention this library prefers is unavailable
    !! here -- the same position `parquet_debug_string_bulk_threads` is in, and this mirrors it
    !! deliberately rather than inventing a second shape. See CLAUDE.md, "A Fortran-side debug hook
    !! has to be PUBLIC, so prefer a C++ one".
    !!
    !! **It RE-COMPUTES the rule rather than reporting what a call did**, which is the same
    !! limitation the string-column hook has and is worth stating: it can assert that
    !! `parquet_set_random_parallel_min_elements` changes the answer, and it cannot assert that the
    !! fill went on to honour that answer. The bit-identity of the 1-thread and N-thread results is
    !! what the suite checks instead, and `app/probe_random_perm.f90 --mode=floor` is what shows the
    !! threading actually happens. See `feature_risks.md` Risk-111.
    integer function parquet_debug_random_bulk_threads(n, threads) result(nth)
        integer(int64), intent(in) :: n             !! elements a bulk call would produce
        integer, intent(in), optional :: threads    !! the caller's request, if any
        nth = random_threads(n, threads)
    end function parquet_debug_random_bulk_threads

    !> Forces the Feistel round count. **Test-only.** `n <= 0` restores the compiled-in value.
    !!
    !! Public because it has to be: the value lives in Fortran, and this module reaches no `bind(C)`
    !! surface, so the C++-side debug-hook convention the rest of the library uses is unavailable
    !! here. See CLAUDE.md, "A Fortran-side debug hook has to be PUBLIC, so prefer a C++ one".
    !!
    !! **It is load-bearing rather than convenient, and that is a consequence of the exact path.**
    !! Every `m` small enough to enumerate all `m!` permutations of is also small enough to be
    !! answered exactly, so the shipped kernel is uniform by construction at exactly the sizes an
    !! exhaustive test can reach -- and an exhaustive test that can only ever confirm a construction
    !! proves nothing about the round count. Forcing the count (with
    !! `parquet_debug_set_perm_force_feistel`, which sends those sizes back through the network) is
    !! the only way a test can still show that 16 rounds is clean where 4, 8 and 12 are not.
    !!
    !! It is excluded from README.md's API overview, no library code calls it, and it has no
    !! counterpart in the public contract: `pf_random_perm_algorithm` names the compiled-in count,
    !! and this does not change that string.
    subroutine parquet_debug_set_perm_rounds(n)
        integer, intent(in) :: n                    !! rounds to force; `<= 0` restores the default
        if (n <= 0) then
            perm_dbg_rounds = 0
        else
            perm_dbg_rounds = min(n, perm_rounds)
        end if
    end subroutine parquet_debug_set_perm_rounds

    !> Enables or disables the parity correction. **Test-only**; see `parquet_debug_set_perm_rounds`.
    subroutine parquet_debug_set_perm_parity(on)
        logical, intent(in) :: on                   !! `.false.` suppresses the correction
        perm_dbg_no_parity = .not. on
    end subroutine parquet_debug_set_perm_parity

    !> Sends `m <= perm_exact_max` through the Feistel. **Test-only**; see the two hooks above.
    subroutine parquet_debug_set_perm_force_feistel(on)
        logical, intent(in) :: on                   !! `.true.` bypasses the exact path
        perm_dbg_force_feistel = on
    end subroutine parquet_debug_set_perm_force_feistel

    !> Reports the permutation kernel's effective configuration. **Test-only.**
    !!
    !! An observation hook rather than a fourth override, and it exists because the three setters
    !! above have no readback: a test that restores the defaults cannot otherwise assert it really
    !! did, and one suite leaking a forced round count into another would show up as an unrelated
    !! golden-vector failure in whichever test happened to run next.
    subroutine parquet_debug_perm_config(rounds, parity, exact_max)
        integer, intent(out) :: rounds              !! rounds the kernel will actually run
        logical, intent(out) :: parity              !! whether the parity correction is applied
        integer, intent(out) :: exact_max           !! largest `m` answered exactly; 0 when forced off
        rounds = perm_round_count()
        parity = .not. perm_dbg_no_parity
        exact_max = 0
        if (.not. perm_dbg_force_feistel) exact_max = perm_exact_max
    end subroutine parquet_debug_perm_config

    !> The bulk permutation fill, `integer(int32)` result. `perm_fill_i64` carries the design note.
    subroutine perm_fill_i32(seed, m, n, v, threads)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size, at least 1 when `n > 0`
        integer(int64), intent(in) :: n             !! how many elements to produce; `0 <= n <= m`
        integer(int32), intent(out) :: v(:)         !! filled with elements `1 .. n`
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: a, b, rk(perm_rounds), lo, hi, chunk, small(perm_exact_max)
        integer :: nth, t, nr, i
        logical :: flip
        if (n <= 0_int64) return
        if (m <= 1_int64) then
            v(1) = 1_int32                          ! `n <= m` leaves only n == m == 1 here
            return
        end if
        if (perm_use_exact(m)) then
            ! `n <= m <= perm_exact_max`, so this is at most 20 elements and never worth threading.
            call perm_exact(seed, m, small)
            do i = 1, int(n)
                v(i) = int(small(i), int32)
            end do
            return
        end if
        call perm_factors(m, a, b)
        nr = perm_round_count()
        flip = perm_parity_flip(seed, m)
        call perm_round_keys(seed, nr, rk)
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call perm_range_i32(m, a, b, rk, nr, flip, 1_int64, n, v)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        ! **`num_threads(nth)` is load-bearing, not decoration.** Without it OpenMP opens the
        ! DEFAULT team -- 384 threads on machine B -- and then runs a loop with `nth`
        ! iterations, so every call pays to create and destroy a full team whatever the
        ! caller asked for. Measured before the clause was added: `threads=2` on a
        ! 1000-element permutation cost 11.3 ms against 10 us serial, a fixed ~6-11 ms on
        ! every call at every size, which reads as an absurdly expensive work floor rather
        ! than as a missing clause.
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call perm_range_i32(m, a, b, rk, nr, flip, lo, hi, v)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine perm_fill_i32

    !> Fills `v(lo:hi)` with elements `lo .. hi` of the permutation. `perm_range_i64` has the note.
    pure subroutine perm_range_i32(m, a, b, rk, nr, flip, lo, hi, v)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        logical, intent(in) :: flip                 !! the seed's parity correction
        integer(int64), intent(in) :: lo            !! first element index, 1-based
        integer(int64), intent(in) :: hi            !! last element index, 1-based
        integer(int32), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64) :: lb(perm_block), rb(perm_block), yb(perm_block)
        integer(int64) :: l, r, y, k
        integer :: e, nb
        k = lo
        l = (lo - 1_int64) / b
        r = (lo - 1_int64) - l * b
        do while (k <= hi)
            nb = int(min(int(perm_block, int64), hi - k + 1_int64))
            ! (1) The block's inputs. One branch per element, in a prologue rather than in the
            !     rounds -- `l` advances only when `r` wraps at `b`.
            do e = 1, nb
                lb(e) = l
                rb(e) = r
                r = r + 1_int64
                if (r >= b) then
                    r = 0_int64
                    l = l + 1_int64
                end if
            end do
            ! (2) The rounds, transposed: see `perm_feistel_block`.
            call perm_feistel_block(rk, nr, a, b, nb, lb, rb, yb)
            ! (3) The cycle walk, the parity correction and the store. The walk is a FIXUP pass
            !     rather than part of the main loop, which is what lets (2) stay branch-free; it
            !     touches only the `a*b - m` values that land outside `[0, m)`, about `sqrt(m)` of
            !     `m` -- 0.1 % at m = 10**6 -- which is why that split is nearly free here.
            do e = 1, nb
                y = yb(e)
                do while (y >= m)
                    y = perm_feistel(rk, nr, a, b, y)
                end do
                v(k + int(e - 1, int64)) = int(perm_swap01(flip, y) + 1_int64, int32)
            end do
            k = k + int(nb, int64)
        end do
    end subroutine perm_range_i32

    !> The bulk permutation fill, `integer(int64)` result -- and the division-free `(l, r)` shape.
    !!
    !! **Three things move out of the per-element path here, and the division is the smallest.**
    !!
    !! The division first, since it names the shape: `perm_at_impl` must compute `l = x/b` and
    !! `r = x - l*b` for each `k`, whereas walking `l` in the outer loop and `r` in the inner one
    !! enumerates `x = l*b + r` as `0, 1, 2, ...` -- exactly the sequence of `k - 1` the caller
    !! asked for, with the split already in hand.
    !!
    !! **The SETUP is the larger saving, and it is easy to miss because it is not in this loop.**
    !! `pf_random_perm_at` is `pure elemental` and stateless, so every single call re-derives the
    !! width rule (`perm_factors`: a `sqrt` and two correction loops) and the whole key schedule
    !! (`perm_round_keys`: **32** `perm_mix2` evaluations at 16 rounds, against the 16 the network
    !! itself performs). Here both happen once per call, whatever `n` is.
    !!
    !! **And the round loop is TRANSPOSED here, which the scalar form cannot do** -- one element has
    !! nothing to interleave with. See `perm_feistel_block`; it is worth 2.07x on machine B with no
    !! vector instruction involved, so the bulk-versus-scalar gap is wider than the 2.4x measured
    !! before either change (machine B, gfortran 14.2.1, `--profile release`, four rounds: 24.1 ns
    !! per element scalar against 9.9 ns bulk at `m = 10**8`).
    !!
    !! **The cycle-walk keeps the division and that is fine**, because it is entered essentially
    !! never -- 1.0000 applications per element at every size measured -- so it reuses the scalar
    !! `perm_feistel` rather than duplicating a split-aware walk.
    !!
    !! **The loop bounds are the highest-risk edit in this file.** An off-by-one produces an array
    !! that looks entirely plausible and is not a permutation. `feature_risks.md` Risk-109.
    !!
    !! **Threading is answer-invariant here by construction, not by care.** Element `k` is a pure
    !! function of `(seed, m, k)` and reads nothing any other element writes, so splitting `1 .. n`
    !! into contiguous chunks cannot change a value -- which is why `threads=` is admissible on this
    !! call at all, and why the 1-thread and N-thread outputs are asserted bit-identical rather than
    !! merely statistically similar. The factors and the round keys are derived **before** the
    !! region and shared read-only, so each thread pays one division at entry and none per element.
    subroutine perm_fill_i64(seed, m, n, v, threads)
        integer(int64), intent(in) :: seed          !! the permutation family's seed
        integer(int64), intent(in) :: m             !! population size, at least 1 when `n > 0`
        integer(int64), intent(in) :: n             !! how many elements to produce; `0 <= n <= m`
        integer(int64), intent(out) :: v(:)         !! filled with elements `1 .. n`
        integer, intent(in), optional :: threads    !! caller's request; absent means automatic
        integer(int64) :: a, b, rk(perm_rounds), lo, hi, chunk, small(perm_exact_max)
        integer :: nth, t, nr, i
        logical :: flip
        if (n <= 0_int64) return
        if (m <= 1_int64) then
            v(1) = 1_int64                          ! `n <= m` leaves only n == m == 1 here
            return
        end if
        if (perm_use_exact(m)) then
            ! `n <= m <= perm_exact_max`, so this is at most 20 elements and never worth threading.
            call perm_exact(seed, m, small)
            do i = 1, int(n)
                v(i) = small(i)
            end do
            return
        end if
        call perm_factors(m, a, b)
        nr = perm_round_count()
        flip = perm_parity_flip(seed, m)
        call perm_round_keys(seed, nr, rk)
        nth = random_threads(n, threads)
        if (nth <= 1) then
            call perm_range_i64(m, a, b, rk, nr, flip, 1_int64, n, v)
            return
        end if
        chunk = (n + int(nth, int64) - 1_int64) / int(nth, int64)
#ifdef _OPENMP
        ! **`num_threads(nth)` is load-bearing, not decoration.** Without it OpenMP opens the
        ! DEFAULT team -- 384 threads on machine B -- and then runs a loop with `nth`
        ! iterations, so every call pays to create and destroy a full team whatever the
        ! caller asked for. Measured before the clause was added: `threads=2` on a
        ! 1000-element permutation cost 11.3 ms against 10 us serial, a fixed ~6-11 ms on
        ! every call at every size, which reads as an absurdly expensive work floor rather
        ! than as a missing clause.
        !$omp parallel do default(shared) private(t, lo, hi) schedule(static) num_threads(nth)
#endif
        do t = 0, nth - 1
            lo = int(t, int64) * chunk + 1_int64
            hi = min(n, lo + chunk - 1_int64)
            if (lo <= hi) call perm_range_i64(m, a, b, rk, nr, flip, lo, hi, v)
        end do
#ifdef _OPENMP
        !$omp end parallel do
#endif
    end subroutine perm_fill_i64

    !> Fills `v(lo:hi)` with elements `lo .. hi` of the permutation, from an arbitrary start.
    !!
    !! **This is the only place elements are produced**, serial and threaded alike -- a thread's
    !! chunk and the whole array are the same call with different bounds. That is deliberate: two
    !! shapes for one computation is how a threaded path comes to disagree with its serial twin
    !! over some boundary nobody tested.
    !!
    !! **One division per CALL, not per element.** Entering at `lo` needs `(l, r)` for
    !! `x = lo - 1`, which costs a division once; from there the pair advances by the loop
    !! structure exactly as the whole-array walk does.
    !!
    !! **Three passes per block, and the split into three is what buys the speed.** The inputs are
    !! gathered first, so the `r`-wraps-at-`b` branch sits in a prologue; then `perm_feistel_block`
    !! runs every round over the whole block with no branch and no carried dependency; then the
    !! cycle walk, the parity correction and the store happen in a fixup pass. Folding the walk
    !! back into the middle pass would undo the transposition, because it is a `do while` whose
    !! trip count differs per element -- and it can be a fixup precisely because it is entered
    !! essentially never (1.0000 applications per element at every size measured).
    pure subroutine perm_range_i64(m, a, b, rk, nr, flip, lo, hi, v)
        integer(int64), intent(in) :: m             !! population size, at least 2
        integer(int64), intent(in) :: a             !! left factor
        integer(int64), intent(in) :: b             !! right factor
        integer(int64), intent(in) :: rk(perm_rounds)   !! the round keys
        integer, intent(in) :: nr                   !! rounds to run
        logical, intent(in) :: flip                 !! the seed's parity correction
        integer(int64), intent(in) :: lo            !! first element index, 1-based
        integer(int64), intent(in) :: hi            !! last element index, 1-based
        integer(int64), intent(inout) :: v(:)       !! only `v(lo:hi)` is written
        integer(int64) :: lb(perm_block), rb(perm_block), yb(perm_block)
        integer(int64) :: l, r, y, k
        integer :: e, nb
        k = lo
        l = (lo - 1_int64) / b
        r = (lo - 1_int64) - l * b
        do while (k <= hi)
            nb = int(min(int(perm_block, int64), hi - k + 1_int64))
            ! (1) The block's inputs. One branch per element, in a prologue rather than in the
            !     rounds -- `l` advances only when `r` wraps at `b`.
            do e = 1, nb
                lb(e) = l
                rb(e) = r
                r = r + 1_int64
                if (r >= b) then
                    r = 0_int64
                    l = l + 1_int64
                end if
            end do
            ! (2) The rounds, transposed: see `perm_feistel_block`.
            call perm_feistel_block(rk, nr, a, b, nb, lb, rb, yb)
            ! (3) The cycle walk, the parity correction and the store. The walk is a FIXUP pass
            !     rather than part of the main loop, which is what lets (2) stay branch-free; it
            !     touches only the `a*b - m` values that land outside `[0, m)`, about `sqrt(m)` of
            !     `m` -- 0.1 % at m = 10**6 -- which is why that split is nearly free here.
            do e = 1, nb
                y = yb(e)
                do while (y >= m)
                    y = perm_feistel(rk, nr, a, b, y)
                end do
                v(k + int(e - 1, int64)) = perm_swap01(flip, y) + 1_int64
            end do
            k = k + int(nb, int64)
        end do
    end subroutine perm_range_i64

    ! ================================================================================
    ! Tier 1 -- the stateful stream
    ! ================================================================================
    !
    ! These live in this file rather than in a `parquet_random_stream` submodule, and the reason is
    ! a compiler fact rather than a preference. gfortran does not emit an out-of-line copy of a
    ! private module-contained procedure whose in-module calls it has all inlined, so a submodule
    ! calling `random_block`, `to_real64`, `int_at_impl` or any of the fill workers fails at LINK
    ! time with `undefined reference` -- confirmed here, and the shape CLAUDE.md's "A private
    ! procedure contained directly in a module ... fails at LINK time" note describes. The documented
    ! fix is to give each such helper an interface in the module and a body in a submodule, which
    ! for these eight would mean moving the cipher and its route (e) fork out of this file. That is
    ! exactly what `feature_random_phase2.md` §6 says must not happen: `random_block`, `mulhilo64`,
    ! `mul64_lo_strict` and `width_of` are what the whole correctness story rests on and belong
    ! together. So the split was dropped, not the helpers.
    !
    ! Two things below are load-bearing and easy to undo by accident.
    !
    ! The BLOCK CACHE is keyed on the block index (`self%blk`), never drained as a queue. That is
    ! what keeps it out of the contract: `%position` means a word index and nothing else, `%rewind`
    ! just sets it, and a stream restored from a saved `%position` is exact because the cache is
    ! derivable state that either matches or is replaced. A queue-shaped buffer would compute the
    ! same values while putting a buffer state into everything `%position` means.
    !
    ! The POSITION GUARDS (`advance_by`, `seek_by`) exist so that no arithmetic on `pos` can
    ! overflow. This module has already been caught once with a compiler using an overflowing
    ! expression's undefinedness to delete a branch far away (`feature_risks.md` Risk-94), so an
    ! unguarded `pos + 2` on the hot path would be a real regression rather than a theoretical one.

    !> `%seed` with no stream index: stream 0.
    pure subroutine stream_seed_base(self, seed)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        call reseed(self, seed, 0_int64)
    end subroutine stream_seed_base

    !> `%seed` with an `integer(int32)` stream index; sign-extends, so any value is valid.
    pure subroutine stream_seed_i32(self, seed, stream)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        integer(int32), intent(in) :: stream            !! which stream of that family
        call reseed(self, seed, int(stream, int64))
    end subroutine stream_seed_i32

    !> `%seed` with an `integer(int64)` stream index; every value is valid.
    pure subroutine stream_seed_i64(self, seed, stream)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        integer(int64), intent(in) :: stream            !! which stream of that family
        call reseed(self, seed, stream)
    end subroutine stream_seed_i64

    !> The current 1-based word position.
    pure function stream_position(self) result(p)
        class(pf_random_stream), intent(in) :: self     !! the stream to query
        integer(int64) :: p                             !! 1-based word position
        p = self%pos + 1_int64
    end function stream_position

    !> `%rewind` with no argument: back to position 1.
    pure subroutine stream_rewind_base(self)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        call set_pos(self, 1_int64)
    end subroutine stream_rewind_base

    !> `%rewind` to an `integer(int32)` position.
    pure subroutine stream_rewind_i32(self, pos)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        integer(int32), intent(in) :: pos               !! 1-based word position
        call set_pos(self, int(pos, int64))
    end subroutine stream_rewind_i32

    !> `%rewind` to an `integer(int64)` position.
    pure subroutine stream_rewind_i64(self, pos)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        integer(int64), intent(in) :: pos               !! 1-based word position
        call set_pos(self, pos)
    end subroutine stream_rewind_i64

    !> `%jump` by an `integer(int32)` word count.
    pure subroutine stream_jump_i32(self, n)
        class(pf_random_stream), intent(inout) :: self  !! the stream to seek
        integer(int32), intent(in) :: n                 !! words to seek; negative seeks backwards
        call seek_by(self, int(n, int64))
    end subroutine stream_jump_i32

    !> `%jump` by an `integer(int64)` word count.
    pure subroutine stream_jump_i64(self, n)
        class(pf_random_stream), intent(inout) :: self  !! the stream to seek
        integer(int64), intent(in) :: n                 !! words to seek; negative seeks backwards
        call seek_by(self, n)
    end subroutine stream_jump_i64

    !> The next `real64` in `[0, 1)`, advancing two words.
    pure subroutine stream_uniform(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: x                  !! a uniform draw in `[0, 1)`
        integer(int64) :: w0, w1
        call advance_by(self, 2_int64)
        call word_at(self, self%pos - 2_int64, w0)
        call word_at(self, self%pos - 1_int64, w1)
        x = to_real64(ior(ishft(w1, 32), w0))
    end subroutine stream_uniform

    !> The next `real32` in `[0, 1)`, advancing one word.
    pure subroutine stream_uniform32(self, x)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real32), intent(out) :: x                  !! a uniform draw in `[0, 1)`
        integer(int64) :: w
        call advance_by(self, 1_int64)
        call word_at(self, self%pos - 1_int64, w)
        x = to_real32(w)
    end subroutine stream_uniform32

    !> The next 64 raw bits, advancing two words -- the same two `%uniform` would have read.
    pure subroutine stream_bits(self, b)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: b                !! 64 raw bits
        integer(int64) :: w0, w1
        call advance_by(self, 2_int64)
        call word_at(self, self%pos - 2_int64, w0)
        call word_at(self, self%pos - 1_int64, w1)
        b = ior(ishft(w1, 32), w0)
    end subroutine stream_bits

    !> `%int_range` for `integer(int64)` bounds and result.
    pure subroutine stream_int_range_i64(self, lo, hi, r)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(in) :: lo                !! one end of the closed range
        integer(int64), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int64), intent(out) :: r                !! a uniform integer in the closed range
        integer(int64) :: d
        call take_pair(self, d)
        r = int_at_impl(self%key, self%stream, lo, hi, d)
    end subroutine stream_int_range_i64

    !> `%int_range` for `integer(int32)` bounds and result.
    !!
    !! The result is inside the closed range by construction, so the narrowing is exact -- the same
    !! argument `pf_random_int_at_i32` rests on.
    pure subroutine stream_int_range_i32(self, lo, hi, r)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int32), intent(in) :: lo                !! one end of the closed range
        integer(int32), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int32), intent(out) :: r                !! a uniform integer in the closed range
        integer(int64) :: d
        call take_pair(self, d)
        r = int(int_at_impl(self%key, self%stream, int(lo, int64), int(hi, int64), d), int32)
    end subroutine stream_int_range_i32

    !> `%fill` for a `real64` array.
    !!
    !! Routes to `fill_r64` whenever the position is pair-aligned, because the bulk fills walk
    !! blocks rather than values and beat even the cached stream by 1.67-1.87x. The values are
    !! identical either way; only the number of encipherings differs.
    pure subroutine stream_fill_r64(self, v)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real64), intent(out) :: v(:)               !! filled with the next `size(v)` values
        integer(int64) :: m, k, start
        real(real64) :: x
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call advance_by(self, 2_int64 * m)              ! guard first: an abort writes nothing
        start = self%pos - 2_int64 * m
        if (modulo(start, 2_int64) == 0_int64) then
            call fill_r64(self%key, self%stream, v, start / 2_int64 + 1_int64)
        else
            ! Started mid-pair, so every value straddles a pair boundary -- a position no tier-2
            ! entry point addresses. Correct rather than fast, and rare.
            self%pos = start
            do k = 1_int64, m
                call stream_uniform(self, x)
                v(k) = x
            end do
        end if
    end subroutine stream_fill_r64

    !> `%fill` for a `real32` array. Every position is aligned for it: one value is one word.
    pure subroutine stream_fill_r32(self, v)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        real(real32), intent(out) :: v(:)               !! filled with the next `size(v)` values
        integer(int64) :: m
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call advance_by(self, m)
        call fill_r32(self%key, self%stream, v, self%pos - m + 1_int64)
    end subroutine stream_fill_r32

    !> `%fill` for an `integer(int64)` array; aligns to a pair first, exactly as `%int_range` does.
    pure subroutine stream_fill_i64(self, v, lo, hi)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: v(:)             !! filled with the next `size(v)` values
        integer(int64), intent(in) :: lo                !! one end of the closed range
        integer(int64), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int64) :: m, d
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call align_to_pair(self)
        d = self%pos / 2_int64 + 1_int64
        call advance_by(self, 2_int64 * m)
        call fill_draws_i64(self%key, self%stream, v, lo, hi, d)
    end subroutine stream_fill_i64

    !> `%fill` for an `integer(int32)` array.
    pure subroutine stream_fill_i32(self, v, lo, hi)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int32), intent(out) :: v(:)             !! filled with the next `size(v)` values
        integer(int32), intent(in) :: lo                !! one end of the closed range
        integer(int32), intent(in) :: hi                !! the other end; `lo > hi` is swapped
        integer(int64) :: m, d
        m = size(v, kind=int64)
        if (m <= 0_int64) return                        ! a zero-sized fill is a defined no-op
        call align_to_pair(self)
        d = self%pos / 2_int64 + 1_int64
        call advance_by(self, 2_int64 * m)
        call fill_draws_i32(self%key, self%stream, v, lo, hi, d)
    end subroutine stream_fill_i32

    !> Points the stream at `(seed, stream)`, position 1, holding nothing.
    !!
    !! Dropping the cache is required, not tidiness: `blk` indexes the *previous* family's blocks,
    !! and a reseeded stream that kept it would answer its next draw from the old seed's words.
    !! `-1` is unreachable as a real block index, since a position is never negative.
    pure subroutine reseed(self, seed, stream)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reseed
        integer(int64), intent(in) :: seed              !! the stream family's seed
        integer(int64), intent(in) :: stream            !! which stream of that family
        self%key = seed
        self%stream = stream
        self%pos = 0_int64
        self%blk = -1_int64
    end subroutine reseed

    !> Sets the 1-based position, refusing one that names no word.
    pure subroutine set_pos(self, pos)
        class(pf_random_stream), intent(inout) :: self  !! the stream to reposition
        integer(int64), intent(in) :: pos               !! 1-based word position
        if (pos < 1_int64) then
            error stop "pf_random_stream%rewind: position must be at least 1 (positions are 1-based)"
        end if
        self%pos = pos - 1_int64
    end subroutine set_pos

    !> Seeks `n` words, forwards or backwards, refusing to leave the addressable range.
    !!
    !! Both comparisons are written as subtractions from the bound rather than as `pos + n`,
    !! precisely so that the check itself cannot overflow the thing it is checking.
    pure subroutine seek_by(self, n)
        class(pf_random_stream), intent(inout) :: self  !! the stream to seek
        integer(int64), intent(in) :: n                 !! words to seek; negative seeks backwards
        if (n >= 0_int64) then
            if (self%pos > stream_pos_max - n) then
                error stop "pf_random_stream%jump: seek would pass the last addressable word of the stream"
            end if
        else
            if (self%pos < -n) then
                error stop "pf_random_stream%jump: backward seek would pass position 1"
            end if
        end if
        self%pos = self%pos + n
    end subroutine seek_by

    !> Reserves the next `w` words and advances past them; the reads then use `pos-w .. pos-1`.
    !!
    !! Advancing BEFORE reading is what makes the guard total: a producer that aborts here has
    !! written nothing and moved nothing, so an exhausted stream is left exactly where it was.
    pure subroutine advance_by(self, w)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(in) :: w                 !! words this producer consumes
        if (self%pos > stream_pos_max - w) then
            error stop "pf_random_stream: the stream is exhausted -- it addresses at most 2**63 words"
        end if
        self%pos = self%pos + w
    end subroutine advance_by

    !> Advances to the next word PAIR boundary if the stream is not already on one.
    !!
    !! `%int_range` and the integer fills need this because the integer rule addresses a word pair
    !! at a fixed grid -- draw `d` is words `2d-2, 2d-1` -- while `%uniform32` can leave the cursor
    !! on an odd word. Without it, an integer draw taken at word 1 would re-read word 0, a bit
    !! pattern an earlier producer had already handed out. Aligning costs at most ONE word, and is
    !! what keeps a stream's `%int_range` equal to the `pf_random_int_at` at the same coordinate.
    !!
    !! It cost up to three words, and was called `align_to_block`, while the integer generic had
    !! stride 4 and consumed a whole block per value.
    pure subroutine align_to_pair(self)
        class(pf_random_stream), intent(inout) :: self  !! the stream to align
        if (modulo(self%pos, 2_int64) /= 0_int64) call advance_by(self, 1_int64)
    end subroutine align_to_pair

    !> Aligns, then reserves one word pair, returning the 1-based draw index it occupies.
    pure subroutine take_pair(self, draw)
        class(pf_random_stream), intent(inout) :: self  !! the stream to advance
        integer(int64), intent(out) :: draw             !! draw index of the pair reserved
        call align_to_pair(self)
        draw = self%pos / 2_int64 + 1_int64
        call advance_by(self, 2_int64)
    end subroutine take_pair

    !> One word of the stream, from the held block when it is the right one.
    !!
    !! The only place the cache is read or written. `p` is always a position this call's own
    !! `advance_by` has already checked, so it is in range by construction.
    pure subroutine word_at(self, p, w)
        class(pf_random_stream), intent(inout) :: self  !! the stream holding the cache
        integer(int64), intent(in) :: p                 !! 0-based word position
        integer(int64), intent(out) :: w                !! that word, in `[0, 2**32)`
        integer(int64) :: want
        want = p / 4_int64
        if (want /= self%blk) then
            call random_block(self%key, self%stream, want, self%c0, self%c1, self%c2, self%c3)
            self%blk = want
        end if
        select case (int(modulo(p, 4_int64), int32))
        case (0)
            w = self%c0
        case (1)
            w = self%c1
        case (2)
            w = self%c2
        case default
            w = self%c3
        end select
    end subroutine word_at

    ! ================================================================================
    ! Tier 2 -- the weighted sequential draw
    ! ================================================================================
    !
    ! Three properties of the descent are load-bearing, and each fails SILENTLY if it is dropped --
    ! a wrong answer with nothing to report it, which is why all three are stated here rather than
    ! left to be inferred from the code.
    !
    !  1. **An ancestor is RECOMPUTED from its two children, never decremented.** Subtracting the
    !     drawn weight folds another rounding error into a running value at every draw, so the root
    !     drifts away from the true remaining weight and the descent starts landing on spent
    !     leaves. Measured during design at 16% duplicate draws over 18 decades of dynamic range --
    !     and, less comfortably, at 100 duplicates with plain uniform(0,1) weights, so this is not
    !     an exotic-input problem. Recomputing costs exactly the same and is exact to one ulp.
    !
    !  2. **Exhaustion is tested on a live integer COUNT, never on the root's weight.** This one is
    !     REDUNDANT GIVEN (1) and is kept anyway, in the same way `column_has_nulls_from_footer`'s
    !     two guards are individually redundant and jointly load-bearing: because ancestors are
    !     recomputed, a drawn leaf is exactly `0.0` and any node above only-drawn leaves is an
    !     exact sum of exact zeros, so `st(1) > 0` and `live > 0` are the same predicate. Mutating
    !     the test to the root's weight duly survives the whole suite. It stops being redundant the
    !     moment (1) is weakened, which is exactly when nothing else would notice -- and the count
    !     is what makes `%remaining` an exact `O(1)` answer rather than a question about rounding.
    !
    !  3. **The descent may never enter a subtree whose sum is zero.** `st(p)` is the rounded sum
    !     of its children and can exceed their exact total, so a `target` just below `st(p)` can
    !     exceed `st(left) + st(right)`; a root-only clamp does not close that, because the excess
    !     reappears at every level. `wd_draw` tests each child for liveness before descending.
    !
    !     **This is a DEFENSIVE branch and its mutation survives the suite -- deliberately, not for
    !     want of trying.** The obvious route into a dead subtree is already closed by (1): a dead
    !     subtree sums to EXACTLY zero, and `x + 0.0` is exact, so a node with one live child
    !     equals that child exactly and `target < st(p)` implies `target < st(live child)`. What
    !     remains is the case of two live children where the parent's rounding lets `target`
    !     overshoot into the right subtree by up to an ulp, and that overshoot then cascades onto a
    !     dead leaf further down. That needs `u` within about `2**-52` of 1 at a specific node, so
    !     no fixture this repository can build will reach it. The guard costs two comparisons and
    !     turns "vanishingly unlikely" into "cannot happen"; do not delete it on the strength of a
    !     coverage report or a surviving mutant.
    !
    !     A zero-weight leaf would defeat this test -- it is indistinguishable from a spent one --
    !     which is the real reason such items are held outside the tree rather than given a leaf.

    !> Validates `weights` and builds the tree. The shared worker behind all three `%init` forms.
    subroutine wd_build(self, weights)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler, freshly guarded
        real(real64), intent(in) :: weights(:)          !! one weight per item; all finite, all `>= 0`
        integer(int64) :: n, i, j, z, p, p2

        n = size(weights, kind=int64)
        if (n < 1_int64) error stop "pf_weighted_draw%init: weights must have at least one element"
        self%npos = 0_int64
        self%nzero = 0_int64
        do i = 1_int64, n
            if (ieee_is_nan(weights(i))) &
                error stop "pf_weighted_draw%init: a weight is NaN; a NaN compares false against " // &
                           "every bound and would silently be treated as zero"
            if (.not. ieee_is_finite(weights(i))) &
                error stop "pf_weighted_draw%init: a weight is infinite; the total weight would be " // &
                           "infinite and every draw degenerate"
            if (weights(i) < 0.0_real64) &
                error stop "pf_weighted_draw%init: a weight is negative; a weight is a relative " // &
                           "frequency and cannot be below zero"
            if (weights(i) > 0.0_real64) then
                self%npos = self%npos + 1_int64
            else
                self%nzero = self%nzero + 1_int64
            end if
        end do
        if (self%npos == 0_int64) &
            error stop "pf_weighted_draw%init: every weight is zero; there is no distribution to " // &
                       "draw from"

        p2 = 1_int64
        do while (p2 < self%npos)
            p2 = 2_int64 * p2
        end do
        self%p2 = p2
        if (allocated(self%st)) deallocate(self%st)
        allocate(self%st(2_int64 * p2 - 1_int64))
        ! Only the PADDING leaves need clearing: every internal node is written by the sweep below
        ! and every real leaf by the fill. Blanking the whole array first would double the build's
        ! memory traffic, which is what the build costs.
        self%st(p2 + self%npos : 2_int64 * p2 - 1_int64) = 0.0_real64

        ! The item map exists only when a zero weight is present. Without one, leaf j IS item j,
        ! and skipping the map saves an int64 per item -- worth having, since a parallel caller
        ! holds one sampler per thread.
        if (allocated(self%leaf_item)) deallocate(self%leaf_item)
        if (allocated(self%zero_item)) deallocate(self%zero_item)
        if (self%nzero > 0_int64) then
            allocate(self%leaf_item(self%npos))
            allocate(self%zero_item(self%nzero))
        end if
        j = 0_int64
        z = 0_int64
        do i = 1_int64, n
            if (weights(i) > 0.0_real64) then
                j = j + 1_int64
                self%st(p2 + j - 1_int64) = weights(i)
                if (self%nzero > 0_int64) self%leaf_item(j) = i
            else
                z = z + 1_int64
                self%zero_item(z) = i
            end if
        end do
        do p = p2 - 1_int64, 1_int64, -1_int64
            self%st(p) = self%st(2_int64 * p) + self%st(2_int64 * p + 1_int64)
        end do

        self%live = self%npos
        self%ndrawn = 0_int64
        self%jr_n = 0_int64
        self%ready = .true.
    end subroutine wd_build

    !> Installs the coordinates and derives the zero-weight tail's own seed from them.
    pure subroutine wd_set_coords(self, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the seed steering the descent
        integer(int64), intent(in) :: stream            !! which sequence of that seed

        self%wseed = seed
        self%wstream = stream
        ! Two derivations rather than one: the tail's order must vary with BOTH coordinates, and
        ! folding the stream into a label instead could collide with an ordinary stream index.
        self%zkey = pf_random_key(pf_random_key(seed, stream), wd_zero_label)
    end subroutine wd_set_coords

    !> Refuses a call on a sampler `%init` has not prepared.
    subroutine wd_require_ready(self, proc)
        class(pf_weighted_draw), intent(in) :: self     !! the sampler
        character(len=*), intent(in) :: proc            !! the calling binding, for the message

        if (.not. self%ready) &
            error stop "pf_weighted_draw%" // proc // ": this sampler has not been initialised; " // &
                       "call %init(weights, seed) first"
    end subroutine wd_require_ready

    !> Records one drawn leaf so `%reset`/`%reseed` can put it back. Doubles on overflow.
    subroutine wd_push(self, pos, val)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: pos               !! the leaf node just zeroed
        real(real64), intent(in) :: val                 !! the weight it held
        integer(int64), allocatable :: np(:)
        real(real64), allocatable :: nv(:)
        integer(int64) :: cap

        if (.not. allocated(self%jr_pos)) then
            allocate(self%jr_pos(wd_journal_min))
            allocate(self%jr_val(wd_journal_min))
        else
            cap = size(self%jr_pos, kind=int64)
            if (self%jr_n >= cap) then
                allocate(np(2_int64 * cap))
                allocate(nv(2_int64 * cap))
                np(1:cap) = self%jr_pos
                nv(1:cap) = self%jr_val
                call move_alloc(np, self%jr_pos)
                call move_alloc(nv, self%jr_val)
            end if
        end if
        self%jr_n = self%jr_n + 1_int64
        self%jr_pos(self%jr_n) = pos
        self%jr_val(self%jr_n) = val
    end subroutine wd_push

    !> Puts every drawn leaf back and recomputes the paths above them.
    !!
    !! **The result is bit-identical to a fresh `%init`, and that is provable rather than hoped
    !! for.** Every internal node is a pure function of its two children, and the last restore that
    !! writes a given node necessarily writes it after both children have reached their final
    !! values -- any restore touching a child touches that node too, so none can come later. So the
    !! order entries are replayed in does not matter either.
    subroutine wd_restore(self)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64) :: t, p

        do t = self%jr_n, 1_int64, -1_int64
            p = self%jr_pos(t)
            self%st(p) = self%jr_val(t)
            p = p / 2_int64
            do while (p >= 1_int64)
                self%st(p) = self%st(2_int64 * p) + self%st(2_int64 * p + 1_int64)
                p = p / 2_int64
            end do
        end do
        self%jr_n = 0_int64
        self%live = self%npos
        self%ndrawn = 0_int64
    end subroutine wd_restore

    !> One draw: descend the tree by weight, remove the item, journal what was removed.
    subroutine wd_draw(self, item, ok)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(out) :: item             !! the item drawn, or 0 when exhausted
        logical, intent(out) :: ok                      !! `.false.` once every item has been drawn
        integer(int64) :: p, l, r, leaf
        real(real64) :: u, target

        item = 0_int64
        ok = .false.
        if (self%live > 0_int64) then
            self%ndrawn = self%ndrawn + 1_int64
            u = pf_random_at(self%wseed, self%wstream, draw=self%ndrawn)
            target = u * self%st(1_int64)
            ! `u` reaches 1 - 2**-53 and the product is rounded, so `target` can land exactly on
            ! the root. Written as a negated `<` so a NaN -- which cannot arise here, the weights
            ! having been validated, but which would otherwise walk the tree silently -- also lands
            ! on the safe branch.
            if (.not. (target < self%st(1_int64))) target = 0.0_real64
            p = 1_int64
            do while (p < self%p2)
                l = 2_int64 * p
                r = l + 1_int64
                if (self%st(l) > 0.0_real64 .and. &
                    (self%st(r) <= 0.0_real64 .or. target < self%st(l))) then
                    if (target >= self%st(l)) target = 0.0_real64
                    p = l
                else
                    target = target - self%st(l)
                    if (target < 0.0_real64) target = 0.0_real64
                    p = r
                end if
            end do
            leaf = p - self%p2 + 1_int64
            call wd_push(self, p, self%st(p))
            self%st(p) = 0.0_real64
            p = p / 2_int64
            do while (p >= 1_int64)
                self%st(p) = self%st(2_int64 * p) + self%st(2_int64 * p + 1_int64)
                p = p / 2_int64
            end do
            self%live = self%live - 1_int64
            if (allocated(self%leaf_item)) then
                item = self%leaf_item(leaf)
            else
                item = leaf
            end if
            ok = .true.
        else if (self%ndrawn < self%npos + self%nzero) then
            ! The zero-weight tail. These can never be drawn while any positive weight remains, so
            ! they necessarily land here; handing them out through the uniform permutation gets
            ! Q5's "uniform random order" without a second construction and without state.
            self%ndrawn = self%ndrawn + 1_int64
            item = self%zero_item(pf_random_perm_at(self%zkey, self%nzero, self%ndrawn - self%npos))
            ok = .true.
        end if
    end subroutine wd_draw

    ! ---- pf_weighted_draw bindings ----

    !> `%init` with no stream; the stream is 0.
    subroutine wd_init_base(self, weights, seed)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        real(real64), intent(in) :: weights(:)          !! one weight per item
        integer(int64), intent(in) :: seed              !! the seed

        call wd_init_s64(self, weights, seed, 0_int64)
    end subroutine wd_init_base

    !> `%init` with an `integer(int32)` stream.
    subroutine wd_init_s32(self, weights, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        real(real64), intent(in) :: weights(:)          !! one weight per item
        integer(int64), intent(in) :: seed              !! the seed
        integer(int32), intent(in) :: stream            !! which sequence; sign-extends

        call wd_init_s64(self, weights, seed, int(stream, int64))
    end subroutine wd_init_s32

    !> `%init` with an `integer(int64)` stream. The form the other two delegate to.
    !!
    !! **Deliberately `intent(inout)` with an explicit called-twice guard, not `intent(out)`.**
    !! `intent(out)` would reset the object on entry and make a second `%init` silently succeed,
    !! quietly discarding a sequence in progress; `%reseed` is the supported way to reuse a
    !! sampler and is `O(k log n)` where a rebuild is `O(n)`, so a caller reaching for `%init`
    !! twice is nearly always reaching for the wrong one.
    subroutine wd_init_s64(self, weights, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        real(real64), intent(in) :: weights(:)          !! one weight per item
        integer(int64), intent(in) :: seed              !! the seed
        integer(int64), intent(in) :: stream            !! which sequence

        if (self%ready) &
            error stop "pf_weighted_draw%init: already called for this sampler; use %reseed to " // &
                       "start another sequence over the same weights"
        call wd_build(self, weights)
        call wd_set_coords(self, seed, stream)
    end subroutine wd_init_s64

    !> `%next` into an `integer(int32)` item index.
    subroutine wd_next_i32(self, item, ok)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int32), intent(out) :: item             !! the item drawn, or 0 when exhausted
        logical, intent(out), optional :: ok            !! `.false.` once exhausted
        integer(int64) :: item64
        logical :: got

        call wd_require_ready(self, "next")
        if (self%npos + self%nzero > int(huge(1_int32), int64)) &
            error stop "pf_weighted_draw%next: the population exceeds huge(int32) and cannot be " // &
                       "reported into an integer(int32) item; declare it integer(int64)"
        call wd_draw(self, item64, got)
        item = int(item64, int32)
        if (present(ok)) ok = got
    end subroutine wd_next_i32

    !> `%next` into an `integer(int64)` item index.
    subroutine wd_next_i64(self, item, ok)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(out) :: item             !! the item drawn, or 0 when exhausted
        logical, intent(out), optional :: ok            !! `.false.` once exhausted
        logical :: got

        call wd_require_ready(self, "next")
        call wd_draw(self, item, got)
        if (present(ok)) ok = got
    end subroutine wd_next_i64

    !> Restarts the SAME sequence: the next `%next` returns what the first one did.
    subroutine wd_reset(self)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler

        call wd_require_ready(self, "reset")
        call wd_restore(self)
    end subroutine wd_reset

    !> `%reseed` with no stream; the stream is 0.
    subroutine wd_reseed_base(self, seed)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the new seed

        call wd_reseed_s64(self, seed, 0_int64)
    end subroutine wd_reseed_base

    !> `%reseed` with an `integer(int32)` stream.
    subroutine wd_reseed_s32(self, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the new seed
        integer(int32), intent(in) :: stream            !! the new stream; sign-extends

        call wd_reseed_s64(self, seed, int(stream, int64))
    end subroutine wd_reseed_s32

    !> `%reseed` with an `integer(int64)` stream. The form the other two delegate to.
    !!
    !! **Works on a pristine sampler**, one that has never drawn: the journal is empty, the replay
    !! is a no-op and only the coordinates change. That is not an edge case to tolerate but the
    !! normal path for the per-thread-array idiom, where every sampler is freshly built and then
    !! immediately reseeded on its first outer iteration.
    subroutine wd_reseed_s64(self, seed, stream)
        class(pf_weighted_draw), intent(inout) :: self  !! the sampler
        integer(int64), intent(in) :: seed              !! the new seed
        integer(int64), intent(in) :: stream            !! the new stream

        call wd_require_ready(self, "reseed")
        call wd_restore(self)
        call wd_set_coords(self, seed, stream)
    end subroutine wd_reseed_s64

    !> Items not yet drawn, zero-weight ones included. Exact and `O(1)` -- never a weight sum.
    function wd_remaining(self) result(r)
        class(pf_weighted_draw), intent(in) :: self     !! the sampler
        integer(int64) :: r                             !! items still available

        call wd_require_ready(self, "remaining")
        r = self%npos + self%nzero - self%ndrawn
    end function wd_remaining

    ! ---- pf_weighted_subset ----

    !> The shared worker: `size(idx)` calls to `%next`, and nothing else.
    subroutine wsub_impl(idx, weights, seed, stream)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        type(pf_weighted_draw) :: d
        integer(int64) :: k
        logical :: ok

        if (size(idx, kind=int64) == 0_int64) return
        if (size(idx, kind=int64) > size(weights, kind=int64)) &
            error stop "pf_weighted_subset: more items requested than there are weights; a subset " // &
                       "drawn without replacement cannot be larger than its population"
        call d%init(weights, seed, stream)
        do k = 1_int64, size(idx, kind=int64)
            call d%next(idx(k), ok)
            if (.not. ok) &
                error stop "pf_weighted_subset: the sampler was exhausted early; this cannot " // &
                           "happen once the population check has passed, and is a library defect"
        end do
    end subroutine wsub_impl

    !> Refuses an `integer(int32)` result array for a population that cannot fit in one.
    subroutine wsub_check_i32(weights)
        real(real64), intent(in) :: weights(:)  !! the weights, whose size bounds every item index

        if (size(weights, kind=int64) > int(huge(1_int32), int64)) &
            error stop "pf_weighted_subset: the population exceeds huge(int32) and an item index " // &
                       "has nowhere to go; declare idx as integer(int64)"
    end subroutine wsub_check_i32

    !> `pf_weighted_subset` into an `int32` array, no stream.
    subroutine wsub_i32_base(idx, weights, seed)
        integer(int32), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed

        call wsub_i32_s64(idx, weights, seed, 0_int64)
    end subroutine wsub_i32_base

    !> `pf_weighted_subset` into an `int32` array, `int32` stream.
    subroutine wsub_i32_s32(idx, weights, seed, stream)
        integer(int32), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int32), intent(in) :: stream    !! which sequence; sign-extends

        call wsub_i32_s64(idx, weights, seed, int(stream, int64))
    end subroutine wsub_i32_s32

    !> `pf_weighted_subset` into an `int32` array, `int64` stream.
    subroutine wsub_i32_s64(idx, weights, seed, stream)
        integer(int32), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer(int64), allocatable :: tmp(:)

        if (size(idx, kind=int64) == 0_int64) return
        call wsub_check_i32(weights)
        allocate(tmp(size(idx, kind=int64)))
        call wsub_impl(tmp, weights, seed, stream)
        idx = int(tmp, int32)
    end subroutine wsub_i32_s64

    !> `pf_weighted_subset` into an `int64` array, no stream.
    subroutine wsub_i64_base(idx, weights, seed)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed

        call wsub_impl(idx, weights, seed, 0_int64)
    end subroutine wsub_i64_base

    !> `pf_weighted_subset` into an `int64` array, `int32` stream.
    subroutine wsub_i64_s32(idx, weights, seed, stream)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int32), intent(in) :: stream    !! which sequence; sign-extends

        call wsub_impl(idx, weights, seed, int(stream, int64))
    end subroutine wsub_i64_s32

    !> `pf_weighted_subset` into an `int64` array, `int64` stream.
    subroutine wsub_i64_s64(idx, weights, seed, stream)
        integer(int64), intent(out) :: idx(:)   !! the items drawn, in order
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence

        call wsub_impl(idx, weights, seed, stream)
    end subroutine wsub_i64_s64

    ! ---- pf_weighted_permutation: the exponential race ----

    !> The shared worker: build the keys, sort them, then place the zero-weight tail.
    subroutine wperm_impl(perm, weights, seed, stream, threads)
        integer(int64), intent(out) :: perm(:)  !! the weighted permutation of `1 .. size(weights)`
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer, intent(in), optional :: threads !! thread request; absent means auto
        real(real64), allocatable :: u(:), key(:)
        integer(int64), allocatable :: ord(:), pos_item(:), zero_item(:)
        integer(int64) :: n, npos, nzero, i, j, z, zkey
        real(real64) :: uu, k1

        n = size(weights, kind=int64)
        if (size(perm, kind=int64) /= n) &
            error stop "pf_weighted_permutation: perm and weights must have the same size; a " // &
                       "weighted permutation returns every item exactly once"
        if (n == 0_int64) return
        if (.not. exp_key_contract_ok()) &
            error stop "pf_weighted_permutation: this build does not reproduce the frozen -log(u) " // &
                       "transform, so its permutations would disagree with every other build. " // &
                       "Rebuild without -ffast-math/-Ofast, or on ifx with -fp-model=precise " // &
                       "(fpm --profile release and --profile debug both pass it)."

        npos = 0_int64
        nzero = 0_int64
        do i = 1_int64, n
            if (ieee_is_nan(weights(i))) &
                error stop "pf_weighted_permutation: a weight is NaN; a NaN compares false against " // &
                           "every bound and would silently be treated as zero"
            if (.not. ieee_is_finite(weights(i))) &
                error stop "pf_weighted_permutation: a weight is infinite; the key would be zero " // &
                           "and that item drawn first every time"
            if (weights(i) < 0.0_real64) &
                error stop "pf_weighted_permutation: a weight is negative; the key would be " // &
                           "negative and that item drawn FIRST, the opposite of any sane reading"
            if (weights(i) > 0.0_real64) then
                npos = npos + 1_int64
            else
                nzero = nzero + 1_int64
            end if
        end do
        if (npos == 0_int64) &
            error stop "pf_weighted_permutation: every weight is zero; there is no distribution to " // &
                       "draw from"

        ! Zero-weight items are held OUT of the race rather than given a sentinel key. A sentinel
        ! would have to sort after every real key, and there is no such value: a denormal weight
        ! makes `-log(u)/w` overflow to infinity, which would then sort after the sentinel and put
        ! zero-weight items in the middle. Keeping them out removes the question, and it is also
        ! what gets them into UNIFORM RANDOM order -- tied sentinel keys would come back in index
        ! order, since every sort here is stable.
        allocate(u(n), key(npos), pos_item(npos))
        call pf_random_fill_draws(seed, stream, u)
        j = 0_int64
        do i = 1_int64, n
            if (weights(i) > 0.0_real64) then
                j = j + 1_int64
                ! u is in [0,1); the race needs (0,1], and u = 0 would give the key exactly 0 and
                ! draw that item first every time.
                uu = 1.0_real64 - u(i)
                k1 = exp_key(uu) / weights(i)
                if (.not. ieee_is_finite(k1)) &
                    error stop "pf_weighted_permutation: a weight is so small that its key " // &
                               "overflows; weights below about 2e-307 cannot be ordered and " // &
                               "would tie several items at infinity"
                key(j) = k1
                pos_item(j) = i
            end if
        end do
        deallocate(u)

        ! Ties break by item index for free: every sort in `parquet_sorting` is stable, so equal
        ! keys keep their original order and `(key, index)` is a strict total order without a
        ! second sort key. `test_race_tie_rule` asserts that rather than trusting it.
        call pf_argsort(key, ord, threads=threads)
        do i = 1_int64, npos
            perm(i) = pos_item(ord(i))
        end do

        if (nzero > 0_int64) then
            allocate(zero_item(nzero))
            z = 0_int64
            do i = 1_int64, n
                if (.not. (weights(i) > 0.0_real64)) then
                    z = z + 1_int64
                    zero_item(z) = i
                end if
            end do
            ! The same two derivations the sequential family uses, so both give the tail the same
            ! kind of order from the same coordinates.
            zkey = pf_random_key(pf_random_key(seed, stream), wd_zero_label)
            do i = 1_int64, nzero
                perm(npos + i) = zero_item(pf_random_perm_at(zkey, nzero, i))
            end do
        end if
    end subroutine wperm_impl

    !> Refuses an `integer(int32)` result array for a population that cannot fit in one.
    subroutine wperm_check_i32(weights)
        real(real64), intent(in) :: weights(:)  !! the weights, whose size bounds every item index

        if (size(weights, kind=int64) > int(huge(1_int32), int64)) &
            error stop "pf_weighted_permutation: the population exceeds huge(int32) and an item " // &
                       "index has nowhere to go; declare perm as integer(int64)"
    end subroutine wperm_check_i32

    !> `pf_weighted_permutation` into an `int32` array, no stream.
    subroutine wperm_i32_base(perm, weights, seed, threads)
        integer(int32), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer, intent(in), optional :: threads !! thread request; absent means auto

        call wperm_i32_s64(perm, weights, seed, 0_int64, threads)
    end subroutine wperm_i32_base

    !> `pf_weighted_permutation` into an `int32` array, `int64` stream.
    subroutine wperm_i32_s64(perm, weights, seed, stream, threads)
        integer(int32), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer, intent(in), optional :: threads !! thread request; absent means auto
        integer(int64), allocatable :: tmp(:)

        if (size(perm, kind=int64) == 0_int64 .and. size(weights, kind=int64) == 0_int64) return
        call wperm_check_i32(weights)
        allocate(tmp(size(perm, kind=int64)))
        call wperm_impl(tmp, weights, seed, stream, threads)
        perm = int(tmp, int32)
    end subroutine wperm_i32_s64

    !> `pf_weighted_permutation` into an `int64` array, no stream.
    subroutine wperm_i64_base(perm, weights, seed, threads)
        integer(int64), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer, intent(in), optional :: threads !! thread request; absent means auto

        call wperm_impl(perm, weights, seed, 0_int64, threads)
    end subroutine wperm_i64_base

    !> `pf_weighted_permutation` into an `int64` array, `int64` stream.
    subroutine wperm_i64_s64(perm, weights, seed, stream, threads)
        integer(int64), intent(out) :: perm(:)  !! the weighted permutation
        real(real64), intent(in) :: weights(:)  !! one weight per item
        integer(int64), intent(in) :: seed      !! the seed
        integer(int64), intent(in) :: stream    !! which sequence
        integer, intent(in), optional :: threads !! thread request; absent means auto

        call wperm_impl(perm, weights, seed, stream, threads)
    end subroutine wperm_i64_s64

end module parquet_random
