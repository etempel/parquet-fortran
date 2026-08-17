!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!> Tests for `parquet_random`, the counter-based random number module.
!>
!> Four layers, in increasing order of what they can catch:
!>
!>  1. **Known-answer vectors** for the cipher itself. Necessary, and nowhere near sufficient --
!>     the published Philox vectors once passed 18 of 18 in a build whose generator returned wrong
!>     values, because a self-check is small and simple by construction and so never produces the
!>     specialised instantiation that goes wrong.
!>  2. **Golden vectors** for every public procedure, generated from an arbitrary-precision model
!>     of the contract (`tools/generate_random_golden_vectors.py`). These freeze the values the
!>     library promises. The same vectors passing under `fpm test`, `--profile release` and
!>     `--profile debug` IS this project's three-profile bit-identity requirement, and them
!>     passing on another machine is the cross-machine one. **On flang those three are one build
!>     run three times** -- fpm 0.13.0 alpha defines no flang profiles, so all three emit the same
!>     flags, and not even `-O`. The requirement is therefore met by gfortran and ifx; a flang run
!>     needs an explicit optimisation level appended to `FPM_FFLAGS` to vary anything at all, and
!>     `tools/check_random_kernels.sh` is what sweeps optimisation settings for this module without
!>     depending on profiles at all.
!>  3. **Cross-implementation agreement** against `test_random_reference`, which shares no code
!>     with the library and works entirely on 16-bit limbs. This is the only layer demonstrated to
!>     catch a miscompiled build.
!>  4. **Statistical and structural properties** -- containment, uniformity, aliasing. These catch
!>     transcription errors; they are not the load-bearing layer and no BigCrush-style battery
!>     belongs here.
!>
!> Everything is in memory and touches no file, so the suite is safe under test-drive's per-test
!> parallelism. The schedule-independence test -- the module's actual claim -- needs a real OpenMP
!> team of its own and so lives in `test_random_omp`, which is excluded from that parallelism.
module test_random

    use parquet                              ! deliberately the facade: a dropped re-export must
                                             ! break the build rather than a later assertion
    use test_random_vectors
    use test_random_reference
    use iso_fortran_env, only: int32, int64, real32, real64
    use testdrive, only: new_unittest, unittest_type, error_type, check

    implicit none
    private
    public :: collect_tests_parquet_random

contains

    !> Registers every test in the `random` suite.
    subroutine collect_tests_parquet_random(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)   !! the suite's tests

        testsuite = [ &
            new_unittest("Random123 known-answer vectors", test_kat_vectors), &
            new_unittest("golden vectors: pf_random_at, pf_random32_at, pf_random_bits_at", test_golden_scalar), &
            new_unittest("golden vectors: pf_random_int_at, including every contract edge", test_golden_int), &
            new_unittest("golden vectors: pf_random_key", test_golden_key), &
            new_unittest("golden vectors: pf_random_fill_draws", test_golden_fill), &
            new_unittest("cross-form: fill agrees with the scalar draws, prefixes are prefixes", test_cross_form), &
            new_unittest("the stream-axis fill agrees with the scalar draws on every shape", test_fill_streams), &
            new_unittest("the integer bulk fills agree with pf_random_int_at on both axes", test_fill_int), &
            new_unittest("a stream walks exactly the tier-0 grid, on every producer", test_stream_values), &
            new_unittest("stream positioning: jump, rewind, position, and the block-aligned draw", &
                         test_stream_position), &
            new_unittest("stream fills equal the scalar bindings, aligned and unaligned", test_stream_fill), &
            new_unittest("golden vectors: pf_random_perm_at", test_perm_golden), &
            new_unittest("the permutation is a bijection at every small m, exhaustively", test_perm_bijection), &
            new_unittest("permutation contract: prefixes, subsets, kinds, clamping, determinism", &
                         test_perm_contract), &
            new_unittest("pf_random_at is exactly to_real64(pf_random_bits_at)", test_bits_identity), &
            new_unittest("the integer draw shares a block with the real draw at one coordinate", &
                test_int_shares_block), &
            new_unittest("the int32 specifics equal the int64 specifics", test_kind_specifics), &
            new_unittest("elemental calls equal elementwise scalar calls", test_elemental), &
            new_unittest("contract edges: swap, clamp, degenerate and extreme arguments", test_edges), &
            new_unittest("pf_random_key composes by nesting", test_key_composition), &
            new_unittest("pf_random_seed is in range and does not repeat", test_seed), &
            new_unittest("both real kinds stay inside [0, 1)", test_containment), &
            new_unittest("the compiled route (e) fork matches the compiler's capability", test_fork_selection), &
            new_unittest("agreement with the strict reference: scalar draws", test_agreement_scalar), &
            new_unittest("agreement with the strict reference: fills", test_agreement_fill), &
            new_unittest("agreement with the strict reference: integers, graded by width regime", test_agreement_int), &
            new_unittest("no aliasing between seeds that differ by a small offset", test_aliasing), &
            new_unittest("uniformity: chi-square, moments, small-range counts, serial correlation", test_statistics), &
            new_unittest("uniformity on the draw, derived-key, real32 and wide-integer axes", test_statistics_axes) &
            ]
    end subroutine collect_tests_parquet_random

    ! ============================================================================================
    ! Layer 1 -- the cipher's own known-answer vectors
    ! ============================================================================================

    !> The three published Random123 `philox4x32 10` vectors.
    !!
    !! **All three are asserted twice: against the strict reference, and against the library's own
    !! kernel.** Only KAT 1 is reachable through the PUBLIC draw API -- seed 0 and stream 0 give an
    !! all-zero key and counter, so block 0 is that vector -- because the other two need counter
    !! words 0 and 1 to hold values a block index cannot produce: that index comes from an
    !! `integer(int64)` draw and so cannot exceed roughly 2**62, while KAT 2 needs `0xffffffff`
    !! there and KAT 3 about 9.6e18. No seed, stream or draw reaches either.
    !!
    !! The library kernel is therefore reached for KAT 2 and KAT 3 through
    !! `parquet_debug_random_block`, which runs the Philox block function on raw words. **Without
    !! it these two were tied to the library only transitively** -- checked against the reference,
    !! with the library tied to the reference by the `test_agreement_*` sweeps -- so the arithmetic
    !! that actually ships was directly known-answer-checked once and indirectly twice, through a
    !! reference that is itself checked three times. That chain was sound but it was a chain, and it
    !! left the shipped kernel less directly evidenced than the code written to check it. Note the
    !! *key* was never the obstacle: it is derived from the seed by a bijection, so every 64-bit key
    !! is reachable; only the counter is bounded.
    !!
    !! KAT 1 stays checked through the public surface as well, which is what ties the public mapping
    !! -- not just the kernel -- to the published cipher. Both real surfaces reach it: `real64`
    !! takes two words per value, so block 0 is draws 1 and 2, and `real32` takes one, so the same
    !! block is draws 1 to 4.
    !!
    !! **The `real32` half is the only assertion in this suite that pins a `real32` value to
    !! something outside this project.** Its golden vectors and the strict reference's `real32` arm
    !! both descend from one reading of the contract's word-to-value rule, so a misreading would be
    !! consistent across generator, reference and library alike, with nothing to contradict it. This
    !! is what contradicts it, and it costs nothing: the words are already here.
    !!
    !! What this test does NOT prove is more important than what it does: it cannot detect a
    !! miscompiled build. That is `test_agreement_*`'s job.
    subroutine test_kat_vectors(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k
        integer(int64) :: o0, o1, o2, o3, bits1, bits2
        integer(int64) :: kkey, kstream, kindex
        real(real32) :: want32, v32(4)

        do k = 1, n_kat
            call ref_philox_block(kat_ctr(4 * k - 3), kat_ctr(4 * k - 2), kat_ctr(4 * k - 1), kat_ctr(4 * k), &
                                  kat_key(2 * k - 1), kat_key(2 * k), o0, o1, o2, o3)
            call check(error, o0 == kat_out(4 * k - 3), "reference Philox: KAT word 0 mismatch")
            if (allocated(error)) return
            call check(error, o1 == kat_out(4 * k - 2), "reference Philox: KAT word 1 mismatch")
            if (allocated(error)) return
            call check(error, o2 == kat_out(4 * k - 1), "reference Philox: KAT word 2 mismatch")
            if (allocated(error)) return
            call check(error, o3 == kat_out(4 * k), "reference Philox: KAT word 3 mismatch")
            if (allocated(error)) return
        end do

        ! The SAME three vectors through the LIBRARY's own kernel, which is the arithmetic that
        ! ships. The hook takes the module's coordinates rather than Philox's four counter words,
        ! so each vector is repacked: counter words 0 and 1 are the block index (low half first),
        ! words 2 and 3 the stream, and the two key words the 64-bit key. Every combined value is a
        ! bit pattern, not a magnitude -- KAT 2's are all-ones, i.e. -1 as int64 -- which is exactly
        ! why these vectors cannot be reached through a draw: the block index is bounded by
        ! `integer(int64)` at roughly 2**62.
        do k = 1, n_kat
            kindex  = ior(kat_ctr(4 * k - 3), ishft(kat_ctr(4 * k - 2), 32))
            kstream = ior(kat_ctr(4 * k - 1), ishft(kat_ctr(4 * k), 32))
            kkey    = ior(kat_key(2 * k - 1), ishft(kat_key(2 * k), 32))
            call parquet_debug_random_block(kkey, kstream, kindex, o0, o1, o2, o3)
            call check(error, o0 == kat_out(4 * k - 3), "library kernel: KAT word 0 mismatch")
            if (allocated(error)) return
            call check(error, o1 == kat_out(4 * k - 2), "library kernel: KAT word 1 mismatch")
            if (allocated(error)) return
            call check(error, o2 == kat_out(4 * k - 1), "library kernel: KAT word 2 mismatch")
            if (allocated(error)) return
            call check(error, o3 == kat_out(4 * k), "library kernel: KAT word 3 mismatch")
            if (allocated(error)) return
        end do

        ! Negative control for the repacking above: if the three vectors' coordinates all collapsed
        ! to the same value -- the way a mis-shifted `ishft` could make every high half vanish --
        ! the loop would be checking one vector three times and still pass on KAT 1's row alone.
        ! These are the three vectors' block indices, which must differ from each other.
        call check(error, ior(kat_ctr(1), ishft(kat_ctr(2), 32)) /= ior(kat_ctr(5), ishft(kat_ctr(6), 32)) .and. &
                          ior(kat_ctr(5), ishft(kat_ctr(6), 32)) /= ior(kat_ctr(9), ishft(kat_ctr(10), 32)), &
            "negative control: the three KATs must repack to three DIFFERENT block indices, or the " // &
            "loop above is checking one vector three times")
        if (allocated(error)) return

        ! KAT 1 through the library: seed 0 and stream 0 give an all-zero key and counter, so
        ! block 0's four words are exactly that vector -- draw 1 carries words 0 and 1, draw 2
        ! words 2 and 3, with the first word of each pair as the LOW half.
        bits1 = pf_random_bits_at(0_int64, 0_int64, 1_int64)
        bits2 = pf_random_bits_at(0_int64, 0_int64, 2_int64)
        call check(error, iand(bits1, 4294967295_int64) == kat_out(1), &
            "library: KAT 1 word 0 is not the low half of draw 1")
        if (allocated(error)) return
        call check(error, iand(ishft(bits1, -32), 4294967295_int64) == kat_out(2), &
            "library: KAT 1 word 1 is not the high half of draw 1")
        if (allocated(error)) return
        call check(error, iand(bits2, 4294967295_int64) == kat_out(3), &
            "library: KAT 1 word 2 is not the low half of draw 2")
        if (allocated(error)) return
        call check(error, iand(ishft(bits2, -32), 4294967295_int64) == kat_out(4), &
            "library: KAT 1 word 3 is not the high half of draw 2")
        if (allocated(error)) return

        ! The same four words through the real32 surface: one word per draw, each read as its top
        ! 24 bits scaled by 2**-24. Both sides are exact -- a 24-bit integer is exact in real32 and
        ! the scale is a power of two -- so this is an equality, not a tolerance.
        do k = 1, 4
            want32 = real(ishft(kat_out(k), -8), real32) * 2.0_real32**(-24)
            call check(error, pf_random32_at(0_int64, 0_int64, int(k, int64)) == want32, &
                "library: pf_random32_at does not read KAT 1's words one per draw, top 24 bits each")
            if (allocated(error)) return
        end do

        call pf_random_fill_draws(0_int64, 0_int64, v32)
        do k = 1, 4
            want32 = real(ishft(kat_out(k), -8), real32) * 2.0_real32**(-24)
            call check(error, v32(k) == want32, "library: a real32 fill does not reproduce KAT 1's four words")
            if (allocated(error)) return
        end do
    end subroutine test_kat_vectors

    ! ============================================================================================
    ! Layer 2 -- the golden vectors
    ! ============================================================================================

    !> Every row of the scalar golden grid, for all three scalar draws.
    !!
    !! Real values are compared as `transfer` bit patterns, which is exact and unambiguous; the
    !! decimal literals beside them are compared too, which is a genuine assertion that the
    !! compiler's decimal conversion agrees, and is the form a human can read.
    !!
    !! The first assertion is the identifier these vectors are the contract FOR. Nothing else in the
    !! suite reads `pf_random_algorithm`, and a version identifier nothing asserts cannot do the job
    !! it exists for: the string could be advanced while every value below stayed put, announcing a
    !! break that did not happen, or the values could move while the string still promised the old
    !! contract. Pinning it here means the two have to change together or not at all.
    subroutine test_golden_scalar(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k
        real(real64) :: got64
        real(real32) :: got32

        call check(error, pf_random_algorithm == "philox4x32-10/v1", &
            "pf_random_algorithm no longer reads philox4x32-10/v1: the vectors in this suite are the contract for " // &
            "that exact string, so they must be regenerated with it, or the identifier must go back")
        if (allocated(error)) return

        do k = 1, n_scalar
            call check(error, pf_random_bits_at(scalar_seed(k), scalar_stream(k), scalar_draw(k)) == scalar_bits(k), &
                "pf_random_bits_at does not match its golden vector")
            if (allocated(error)) return
            got64 = pf_random_at(scalar_seed(k), scalar_stream(k), scalar_draw(k))
            call check(error, transfer(got64, 0_int64) == scalar_at_bits(k), &
                "pf_random_at does not match its golden bit pattern")
            if (allocated(error)) return
            call check(error, got64 == scalar_at(k), "pf_random_at does not match its golden decimal")
            if (allocated(error)) return
            got32 = pf_random32_at(scalar_seed(k), scalar_stream(k), scalar_draw(k))
            call check(error, transfer(got32, 0_int32) == scalar_at32_bits(k), &
                "pf_random32_at does not match its golden bit pattern")
            if (allocated(error)) return
            call check(error, got32 == scalar_at32(k), "pf_random32_at does not match its golden decimal")
            if (allocated(error)) return
        end do
    end subroutine test_golden_scalar

    !> Every row of the integer golden table, plus the properties that hold across rows.
    !!
    !! The retry counts are data in the table, and this asserts that some of them are non-zero --
    !! a vacuity guard, because a suite whose rows all accept on the first candidate would pass
    !! exactly as happily against a rejection loop that is compiled and never entered.
    subroutine test_golden_int(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, retried
        integer(int64) :: value, refv, refr

        retried = 0
        do k = 1, n_int
            value = pf_random_int_at(int_seed(k), int_stream(k), int_lo(k), int_hi(k), int_draw(k))
            call check(error, value == int_value(k), "pf_random_int_at does not match its golden vector")
            if (allocated(error)) return
            call check(error, value >= min(int_lo(k), int_hi(k)) .and. value <= max(int_lo(k), int_hi(k)), &
                "pf_random_int_at returned a value outside the requested range")
            if (allocated(error)) return
            ! The retry count is not observable from the library, so it is confirmed against the
            ! reference, which reports it -- and the golden table records what it should be.
            call ref_int_at(int_seed(k), int_stream(k), int_lo(k), int_hi(k), int_draw(k), refv, refr)
            call check(error, refr == int(int_retries(k), int64), &
                "the reference disagrees with the golden retry count for this row")
            if (allocated(error)) return
            if (int_retries(k) > 0_int32) retried = retried + 1
        end do

        call check(error, retried >= 5, &
            "vacuity guard: fewer than five golden rows exercise the rejection loop, so the retry path is barely tested")
        if (allocated(error)) return

        ! The width-0 row is the whole int64 range, where there is nothing to reduce: it must be
        ! exactly the raw bits.
        call check(error, pf_random_int_at(12345_int64, 1_int64, -huge(1_int64) - 1_int64, huge(1_int64)) == &
                          pf_random_bits_at(12345_int64, 1_int64), &
            "a full-range pf_random_int_at is not pf_random_bits_at")
    end subroutine test_golden_int

    !> Every row of the derived-key golden table.
    subroutine test_golden_key(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k

        do k = 1, n_key
            call check(error, pf_random_key(key_seed(k), key_label(k)) == key_value(k), &
                "pf_random_key does not match its golden vector")
            if (allocated(error)) return
        end do
        call check(error, pf_random_key(0_int64, 0_int64) == 0_int64, &
            "pf_random_key(0, 0) must be 0: the finaliser is a bijection with mix64(0) = 0")
    end subroutine test_golden_key

    !> Every fill case of the golden table, for both real kinds.
    subroutine test_golden_fill(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: c, k
        real(real64) :: v64(16)
        real(real32) :: v32(16)

        do c = 1, n_fill
            call pf_random_fill_draws(fill_seed(c), fill_stream(c), v64(1:fill_count(c)), fill_start(c))
            call pf_random_fill_draws(fill_seed(c), fill_stream(c), v32(1:fill_count(c)), fill_start(c))
            do k = 1, fill_count(c)
                call check(error, transfer(v64(k), 0_int64) == fill_at_bits(fill_first(c) + k - 1), &
                    "pf_random_fill_draws (real64) does not match its golden bit pattern")
                if (allocated(error)) return
                call check(error, transfer(v32(k), 0_int32) == fill_at32_bits(fill_first(c) + k - 1), &
                    "pf_random_fill_draws (real32) does not match its golden bit pattern")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_golden_fill

    ! ============================================================================================
    ! Layer 2b -- contract properties, asserted as properties rather than as values
    ! ============================================================================================

    !> A fill is exactly the matching scalar draws, so prefixes are prefixes and offsets line up.
    subroutine test_cross_form(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k
        integer(int64) :: base
        real(real64) :: one(1), three(3), six(6), tail(3), top6(6)
        real(real32) :: s_one(1), s_six(6), s_top6(6)
        real(real64) :: empty(0)

        call pf_random_fill_draws(12345_int64, 1_int64, one)
        call check(error, one(1) == pf_random_at(12345_int64, 1_int64), &
            "a one-element fill is not the scalar draw")
        if (allocated(error)) return

        call pf_random_fill_draws(12345_int64, 1_int64, six)
        do k = 1, 6
            call check(error, six(k) == pf_random_at(12345_int64, 1_int64, int(k, int64)), &
                "element k of a fill is not the scalar draw at position k")
            if (allocated(error)) return
        end do

        call pf_random_fill_draws(12345_int64, 1_int64, three)
        call check(error, all(three == six(1:3)), "a short fill is not a prefix of a longer one")
        if (allocated(error)) return

        call pf_random_fill_draws(12345_int64, 1_int64, tail, 4_int64)
        call check(error, all(tail == six(4:6)), "a fill starting at draw 4 is not the tail of the whole prefix")
        if (allocated(error)) return

        call pf_random_fill_draws(12345_int64, 1_int64, s_one)
        call check(error, s_one(1) == pf_random32_at(12345_int64, 1_int64), &
            "a one-element real32 fill is not the scalar real32 draw")
        if (allocated(error)) return

        call pf_random_fill_draws(12345_int64, 1_int64, s_six)
        do k = 1, 6
            call check(error, s_six(k) == pf_random32_at(12345_int64, 1_int64, int(k, int64)), &
                "element k of a real32 fill is not the scalar real32 draw at position k")
            if (allocated(error)) return
        end do

        ! A zero-sized fill is a defined no-op, not an error and not an out-of-bounds write.
        call pf_random_fill_draws(12345_int64, 1_int64, empty)
        call check(error, size(empty) == 0, "a zero-sized fill must be a defined no-op")
        if (allocated(error)) return

        ! The documented ceiling of the draw axis: a fill whose LAST position is exactly
        ! huge(int64). It must be exact, not merely non-crashing -- and it is the regression test
        ! for `fill_r64`'s guard, because the unguarded form computed a position one PAST this
        ! element, overflowing on a call every one of whose requested positions is representable.
        ! Asserted against the reference AND against the scalar draws: the first says the values
        ! are right, the second says the fill still agrees with the rest of the API up here.
        base = huge(1_int64) - 6_int64
        call pf_random_fill_draws(12345_int64, 1_int64, top6, base + 1_int64)
        call pf_random_fill_draws(12345_int64, 1_int64, s_top6, base + 1_int64)
        do k = 1, 6
            call check(error, top6(k) == ref_at(12345_int64, 1_int64, base + int(k, int64)), &
                "a real64 fill ending exactly at huge(int64) disagrees with the strict reference")
            if (allocated(error)) return
            call check(error, top6(k) == pf_random_at(12345_int64, 1_int64, base + int(k, int64)), &
                "a real64 fill ending exactly at huge(int64) disagrees with the scalar draw at that position")
            if (allocated(error)) return
            call check(error, s_top6(k) == ref_at32(12345_int64, 1_int64, base + int(k, int64)), &
                "a real32 fill ending exactly at huge(int64) disagrees with the strict reference")
            if (allocated(error)) return
        end do
    end subroutine test_cross_form

    !> `pf_random_fill_streams` is exactly the matching scalar draws, on every shape that matters.
    !!
    !! This entry point needs **no golden vectors of its own**: its contract is that element `k` is
    !! `pf_random_at(seed, i0 + k - 1 [, draw])`, and those values are already frozen by the scalar
    !! grid in `test_random_vectors`. What has to be asserted is the identity itself -- which is
    !! also the only thing that can break, since the values come from the same `random_block`.
    !!
    !! Four shapes are deliberate rather than decorative. **`draw` is swept over 1..5** because the
    !! `real64` worker branches on the draw's parity within its block and the `real32` worker on its
    !! slot of four, so a single `draw` would exercise one arm of two, or one of four, and leave a
    !! wrong-word bug in the others invisible. **A negative `i0`** because stream indices are signed
    !! and a fill starting below zero must still walk upwards. **Length 0** because a zero-sized
    !! fill is a documented no-op rather than an error. And **the far end of the stream axis**,
    !! where `i0 + size(v) - 1` is exactly `huge(int64)`, because that is this entry point's
    !! documented precondition and the golden vectors do not reach it -- the stream-axis counterpart
    !! of the `huge(int64)` draw-axis case in `test_cross_form`.
    subroutine test_fill_streams(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, d
        integer(int64) :: dr, base
        real(real64) :: v(7), top(4)
        real(real32) :: s(7), stop4(4)
        real(real64) :: empty(0)

        ! Every draw parity (real64) and every slot of four (real32).
        do d = 1, 5
            dr = int(d, int64)
            call pf_random_fill_streams(12345_int64, 1_int64, v, dr)
            call pf_random_fill_streams(12345_int64, 1_int64, s, dr)
            do k = 1, 7
                call check(error, v(k) == pf_random_at(12345_int64, int(k, int64), dr), &
                    "a real64 stream-axis fill element is not the scalar draw for that stream")
                if (allocated(error)) return
                call check(error, s(k) == pf_random32_at(12345_int64, int(k, int64), dr), &
                    "a real32 stream-axis fill element is not the scalar real32 draw for that stream")
                if (allocated(error)) return
            end do
        end do

        ! The default `draw` is 1, and an int32 first-stream index gives the same values as int64.
        call pf_random_fill_streams(12345_int64, 1_int64, v)
        do k = 1, 7
            call check(error, v(k) == pf_random_at(12345_int64, int(k, int64)), &
                "a stream-axis fill with the default draw is not draw 1 of each stream")
            if (allocated(error)) return
        end do
        call pf_random_fill_streams(12345_int64, 1_int32, s)
        do k = 1, 7
            call check(error, s(k) == pf_random32_at(12345_int64, int(k, int64)), &
                "an int32 first-stream index does not agree with the int64 one")
            if (allocated(error)) return
        end do

        ! Negative first stream index: the walk is upwards from it, through zero.
        call pf_random_fill_streams(12345_int64, -3_int64, v, 2_int64)
        do k = 1, 7
            call check(error, v(k) == pf_random_at(12345_int64, -4_int64 + int(k, int64), 2_int64), &
                "a stream-axis fill from a negative first index does not walk upwards through zero")
            if (allocated(error)) return
        end do

        ! A zero-sized fill is a defined no-op.
        call pf_random_fill_streams(12345_int64, 1_int64, empty)
        call check(error, size(empty) == 0, "a zero-sized stream-axis fill must be a defined no-op")
        if (allocated(error)) return

        ! The documented ceiling of the STREAM axis: last stream is exactly huge(int64).
        base = huge(1_int64) - 4_int64
        call pf_random_fill_streams(12345_int64, base + 1_int64, top)
        call pf_random_fill_streams(12345_int64, base + 1_int64, stop4)
        do k = 1, 4
            call check(error, top(k) == pf_random_at(12345_int64, base + int(k, int64)), &
                "a stream-axis fill ending exactly at stream huge(int64) disagrees with the scalar draw")
            if (allocated(error)) return
            call check(error, top(k) == ref_at(12345_int64, base + int(k, int64), 1_int64), &
                "a stream-axis fill ending exactly at stream huge(int64) disagrees with the strict reference")
            if (allocated(error)) return
            call check(error, stop4(k) == ref_at32(12345_int64, base + int(k, int64), 1_int64), &
                "a real32 stream-axis fill at the top of the stream axis disagrees with the strict reference")
            if (allocated(error)) return
        end do
    end subroutine test_fill_streams

    !> The integer bulk fills return exactly what `pf_random_int_at` returns at the same coordinate.
    !!
    !! No new golden vectors: every value here is already frozen by the tier-0 integer grid in
    !! `test_random_vectors`, so what has to be asserted is the identity -- and the identity is also
    !! the only thing that can break, since both forms reach the same `int_at_impl`.
    !!
    !! The shapes are chosen against the ways a bulk form can differ from its scalar twin rather
    !! than for coverage: **both value kinds and both stream-index kinds** (four specifics per
    !! generic, and a wrongly wired one would silently fill from the wrong axis); **a swept `draw`**,
    !! because the draw-axis worker offsets each element and an off-by-one there is invisible at
    !! `draw = 1`; **`lo > hi`**, which is documented as swapped rather than refused and is handled
    !! once per call here against once per element in the elemental scalar; **`lo == hi`** and **the
    !! full int64 width**, which are the two ranges that take their own arms inside `int_at_impl`;
    !! **a negative `i0`**, since stream indices are signed; **a zero-sized fill**, a documented
    !! no-op; and **prefix consistency**, which is what makes a bulk fill interchangeable with a
    !! shorter one.
    subroutine test_fill_int(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, d
        integer(int64) :: dr
        integer(int64) :: v(7), wide(5), short(3)
        integer(int32) :: v32(7)
        integer(int64) :: empty(0)

        ! Draw axis, both value kinds, swept over five draws.
        do d = 1, 5
            dr = int(d, int64)
            call pf_random_fill_draws(999_int64, 4_int64, v, 1_int64, 6_int64, dr)
            call pf_random_fill_draws(999_int64, 4_int32, v32, 1_int32, 6_int32, dr)
            do k = 1, 7
                call check(error, v(k) == pf_random_int_at(999_int64, 4_int64, 1_int64, 6_int64, &
                                                           dr + int(k, int64) - 1_int64), &
                    "an int64 draw-axis fill element is not the scalar integer draw at that position")
                if (allocated(error)) return
                call check(error, v32(k) == pf_random_int_at(999_int64, 4_int32, 1_int32, 6_int32, &
                                                             dr + int(k, int64) - 1_int64), &
                    "an int32 draw-axis fill element is not the scalar int32 integer draw")
                if (allocated(error)) return
            end do
        end do

        ! The default `draw` is 1, and the two stream-index kinds agree.
        call pf_random_fill_draws(999_int64, 4_int64, v, 1_int64, 6_int64)
        do k = 1, 7
            call check(error, v(k) == pf_random_int_at(999_int64, 4_int64, 1_int64, 6_int64, int(k, int64)), &
                "an integer draw-axis fill with the default draw is not draw 1 onwards")
            if (allocated(error)) return
        end do
        call pf_random_fill_draws(999_int64, 4_int32, v, 1_int64, 6_int64)
        do k = 1, 7
            call check(error, v(k) == pf_random_int_at(999_int64, 4_int64, 1_int64, 6_int64, int(k, int64)), &
                "an int32 stream index does not agree with the int64 one on the integer draw axis")
            if (allocated(error)) return
        end do

        ! `lo > hi` is swapped, not refused -- and the swap is done once per call here.
        call pf_random_fill_draws(999_int64, 4_int64, v, 6_int64, 1_int64)
        do k = 1, 7
            call check(error, v(k) == pf_random_int_at(999_int64, 4_int64, 1_int64, 6_int64, int(k, int64)), &
                "a reversed range in an integer fill does not equal the swapped range")
            if (allocated(error)) return
        end do

        ! The two ranges with their own arms inside int_at_impl: degenerate, and the full width.
        call pf_random_fill_draws(999_int64, 4_int64, v, 42_int64, 42_int64)
        do k = 1, 7
            call check(error, v(k) == 42_int64, "a degenerate range in an integer fill must return that value")
            if (allocated(error)) return
        end do
        call pf_random_fill_draws(999_int64, 4_int64, wide, -huge(1_int64) - 1_int64, huge(1_int64))
        do k = 1, 5
            call check(error, wide(k) == pf_random_int_at(999_int64, 4_int64, -huge(1_int64) - 1_int64, &
                                                          huge(1_int64), int(k, int64)), &
                "a full-width integer fill disagrees with the scalar draw")
            if (allocated(error)) return
        end do
        ! The full-width draw IS the raw bits -- but only at draw 1, where the integer rule's block
        ! index `draw-1` and `bits_of`'s `(draw-1)/2` coincide. They diverge from draw 2 onwards, and
        ! asserting the identity across the whole fill would be asserting something untrue of the
        ! shipped scalar draw too.
        call check(error, wide(1) == pf_random_bits_at(999_int64, 4_int64, 1_int64), &
            "a full-width integer draw at draw 1 must be the raw bits at that coordinate")
        if (allocated(error)) return
        call check(error, wide(2) /= pf_random_bits_at(999_int64, 4_int64, 2_int64), &
            "the full-width draw and the raw bits must diverge from draw 2, as int_at_impl documents")
        if (allocated(error)) return

        ! Prefix consistency: a shorter fill is a prefix of a longer one.
        call pf_random_fill_draws(999_int64, 4_int64, short, 1_int64, 6_int64)
        call pf_random_fill_draws(999_int64, 4_int64, v, 1_int64, 6_int64)
        do k = 1, 3
            call check(error, short(k) == v(k), "an integer fill's prefix is not a prefix of a longer fill")
            if (allocated(error)) return
        end do

        ! Stream axis, both value kinds, and a negative first stream index.
        call pf_random_fill_streams(999_int64, 1_int64, v, 1_int64, 6_int64, 3_int64)
        call pf_random_fill_streams(999_int64, 1_int32, v32, 1_int32, 6_int32, 3_int64)
        do k = 1, 7
            call check(error, v(k) == pf_random_int_at(999_int64, int(k, int64), 1_int64, 6_int64, 3_int64), &
                "an int64 stream-axis integer fill element is not the scalar draw for that stream")
            if (allocated(error)) return
            call check(error, int(v32(k), int64) == v(k), &
                "the int32 stream-axis integer fill disagrees with the int64 one")
            if (allocated(error)) return
        end do
        call pf_random_fill_streams(999_int64, -3_int64, v, 1_int64, 6_int64)
        do k = 1, 7
            call check(error, v(k) == pf_random_int_at(999_int64, -4_int64 + int(k, int64), &
                                                       1_int64, 6_int64), &
                "a stream-axis integer fill from a negative first index does not walk upwards")
            if (allocated(error)) return
        end do

        ! Zero-sized fills on both axes are defined no-ops.
        call pf_random_fill_draws(999_int64, 4_int64, empty, 1_int64, 6_int64)
        call pf_random_fill_streams(999_int64, 4_int64, empty, 1_int64, 6_int64)
        call check(error, size(empty) == 0, "a zero-sized integer fill must be a defined no-op")
        if (allocated(error)) return

        ! **Every one of the eight specifics, called explicitly.** The sweeps above resolve to
        ! whichever specific their literals happen to select, and three of the eight were reached by
        ! none of them -- found by mutation testing, not by reading: rewiring
        ! `pf_random_fill_draws_i32_i64` to the stream-axis worker survived the whole suite. Eight
        ! near-identical specifics differing only in two kind tokens are exactly the shape where a
        ! copy-paste defect hides, so each is named here with a distinct value/index kind pair.
        call check_specifics(error)
    end subroutine test_fill_int

    !> Calls each of the eight integer fill specifics by name-resolving literal kinds, and checks it
    !! against the scalar draw at the same coordinate.
    !!
    !! Separate from `test_fill_int`'s body only because it is mechanical: the point is exhaustive
    !! reach over the generic's specifics, not another property.
    subroutine check_specifics(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: d64(4)
        integer(int32) :: d32(4)
        integer(int64), parameter :: SD = 4242_int64
        integer(int64), parameter :: ST = 11_int64

        ! ---- draw axis: v kind x stream-index kind ----
        call pf_random_fill_draws(SD, int(ST, int32), d32, 1_int32, 100_int32, 2_int64)   ! i32_i32
        call expect32(error, d32, SD, ST, 1_int32, 100_int32, 2_int64, .true., "draws i32_i32")
        if (allocated(error)) return
        call pf_random_fill_draws(SD, ST, d32, 1_int32, 100_int32, 2_int64)               ! i32_i64
        call expect32(error, d32, SD, ST, 1_int32, 100_int32, 2_int64, .true., "draws i32_i64")
        if (allocated(error)) return
        call pf_random_fill_draws(SD, int(ST, int32), d64, 1_int64, 100_int64, 2_int64)   ! i64_i32
        call expect64(error, d64, SD, ST, 1_int64, 100_int64, 2_int64, .true., "draws i64_i32")
        if (allocated(error)) return
        call pf_random_fill_draws(SD, ST, d64, 1_int64, 100_int64, 2_int64)               ! i64_i64
        call expect64(error, d64, SD, ST, 1_int64, 100_int64, 2_int64, .true., "draws i64_i64")
        if (allocated(error)) return

        ! ---- stream axis ----
        call pf_random_fill_streams(SD, int(ST, int32), d32, 1_int32, 100_int32, 2_int64) ! i32_i32
        call expect32(error, d32, SD, ST, 1_int32, 100_int32, 2_int64, .false., "streams i32_i32")
        if (allocated(error)) return
        call pf_random_fill_streams(SD, ST, d32, 1_int32, 100_int32, 2_int64)             ! i32_i64
        call expect32(error, d32, SD, ST, 1_int32, 100_int32, 2_int64, .false., "streams i32_i64")
        if (allocated(error)) return
        call pf_random_fill_streams(SD, int(ST, int32), d64, 1_int64, 100_int64, 2_int64) ! i64_i32
        call expect64(error, d64, SD, ST, 1_int64, 100_int64, 2_int64, .false., "streams i64_i32")
        if (allocated(error)) return
        call pf_random_fill_streams(SD, ST, d64, 1_int64, 100_int64, 2_int64)             ! i64_i64
        call expect64(error, d64, SD, ST, 1_int64, 100_int64, 2_int64, .false., "streams i64_i64")

        ! The two axes must DISAGREE on this fixture, or the checks above would pass for a specific
        ! wired to either worker -- which is precisely the defect that survived before they existed.
        if (allocated(error)) return
        call pf_random_fill_draws(SD, ST, d64, 1_int64, 100_int64, 2_int64)
        call pf_random_fill_streams(SD, ST, d32, 1_int32, 100_int32, 2_int64)
        call check(error, .not. all(int(d32, int64) == d64), &
            "the draw and stream axes agree on this fixture, so an axis mix-up would be invisible")
    end subroutine check_specifics

    !> Checks an `int32` integer fill against the scalar draw; `by_draw` selects which axis walks.
    subroutine expect32(error, v, seed, stream, lo, hi, draw, by_draw, what)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int32), intent(in) :: v(:)          !! the filled array
        integer(int64), intent(in) :: seed          !! the seed used
        integer(int64), intent(in) :: stream        !! the stream (or first stream) used
        integer(int32), intent(in) :: lo            !! range low end
        integer(int32), intent(in) :: hi            !! range high end
        integer(int64), intent(in) :: draw          !! the starting (or fixed) draw
        logical, intent(in) :: by_draw              !! `.true.` for the draw axis
        character(len=*), intent(in) :: what        !! the specific's name, for the failure message
        integer :: k
        do k = 1, size(v)
            if (by_draw) then
                call check(error, v(k) == pf_random_int_at(seed, stream, int(lo, int64), int(hi, int64), &
                                                           draw + int(k, int64) - 1_int64), what)
            else
                call check(error, v(k) == pf_random_int_at(seed, stream + int(k, int64) - 1_int64, &
                                                           int(lo, int64), int(hi, int64), draw), what)
            end if
            if (allocated(error)) return
        end do
    end subroutine expect32

    !> `expect32` for an `int64` fill.
    subroutine expect64(error, v, seed, stream, lo, hi, draw, by_draw, what)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), intent(in) :: v(:)          !! the filled array
        integer(int64), intent(in) :: seed          !! the seed used
        integer(int64), intent(in) :: stream        !! the stream (or first stream) used
        integer(int64), intent(in) :: lo            !! range low end
        integer(int64), intent(in) :: hi            !! range high end
        integer(int64), intent(in) :: draw          !! the starting (or fixed) draw
        logical, intent(in) :: by_draw              !! `.true.` for the draw axis
        character(len=*), intent(in) :: what        !! the specific's name, for the failure message
        integer :: k
        do k = 1, size(v)
            if (by_draw) then
                call check(error, v(k) == pf_random_int_at(seed, stream, lo, hi, &
                                                           draw + int(k, int64) - 1_int64), what)
            else
                call check(error, v(k) == pf_random_int_at(seed, stream + int(k, int64) - 1_int64, &
                                                           lo, hi, draw), what)
            end if
            if (allocated(error)) return
        end do
    end subroutine expect64

    !> A freshly seeded stream hands out exactly the tier-0 values, for every producer.
    !!
    !! **No new golden vectors, and that is a property of the design rather than an omission.**
    !! `feature_random_phase2.md` §8 anticipated tier 1 needing its own vectors "because `%uniform`
    !! is a new mapping from position to value". It is not: a block-cached stream reads the same
    !! words at the same positions as the tier-0 grid, which the existing vectors already freeze. So
    !! the thing to assert is the identity -- and the identity is also the only thing that can break,
    !! since a wrong cache, a wrong word order or a wrong advance all show up as a disagreement here.
    !!
    !! The mixed-producer case at the end is the sharp one: `%uniform32` leaves the position odd, so
    !! the following `%uniform` reads a pair that straddles a block boundary. That pair is a position
    !! no tier-0 entry point names, so it is checked against the raw block words instead.
    subroutine test_stream_values(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng, dflt
        integer :: k
        real(real64) :: x, ref(6)
        real(real32) :: y
        integer(int64) :: b, ir
        integer(int32) :: ir32
        integer(int64) :: w0, w1, w2, w3, v0, v1, v2, v3

        ! %uniform walks the draw axis of one stream.
        call rng%seed(777_int64, 3_int64)
        do k = 1, 6
            call rng%uniform(x)
            call check(error, x == pf_random_at(777_int64, 3_int64, int(k, int64)), &
                "a stream's k-th %uniform is not pf_random_at at draw k")
            if (allocated(error)) return
        end do

        ! ... and equals the bulk draw-axis fill over the same range.
        call pf_random_fill_draws(777_int64, 3_int64, ref)
        call rng%seed(777_int64, 3_int64)
        do k = 1, 6
            call rng%uniform(x)
            call check(error, x == ref(k), "a stream disagrees with pf_random_fill_draws over the same range")
            if (allocated(error)) return
        end do

        ! %uniform32 is one word per value, so it walks the real32 grid.
        call rng%seed(777_int64, 3_int64)
        do k = 1, 6
            call rng%uniform32(y)
            call check(error, y == pf_random32_at(777_int64, 3_int64, int(k, int64)), &
                "a stream's k-th %uniform32 is not pf_random32_at at draw k")
            if (allocated(error)) return
        end do

        ! %bits reads the same two words %uniform would have.
        call rng%seed(777_int64, 3_int64)
        do k = 1, 6
            call rng%bits(b)
            call check(error, b == pf_random_bits_at(777_int64, 3_int64, int(k, int64)), &
                "a stream's k-th %bits is not pf_random_bits_at at draw k")
            if (allocated(error)) return
        end do

        ! %int_range takes one block per draw, so it walks the integer grid.
        call rng%seed(777_int64, 3_int64)
        do k = 1, 6
            call rng%int_range(1_int64, 1000_int64, ir)
            call check(error, ir == pf_random_int_at(777_int64, 3_int64, 1_int64, 1000_int64, int(k, int64)), &
                "a stream's k-th %int_range is not pf_random_int_at at draw k")
            if (allocated(error)) return
        end do
        call rng%seed(777_int64, 3_int64)
        call rng%int_range(1_int32, 1000_int32, ir32)
        call check(error, int(ir32, int64) == pf_random_int_at(777_int64, 3_int64, 1_int64, 1000_int64), &
            "the int32 %int_range specific disagrees with the int64 one")
        if (allocated(error)) return

        ! Seeding with no stream index means stream 0, and the two kind specifics agree.
        call rng%seed(777_int64)
        call rng%uniform(x)
        call check(error, x == pf_random_at(777_int64, 0_int64), "%seed with no stream index is not stream 0")
        if (allocated(error)) return
        call rng%seed(777_int64, 3_int32)
        call rng%uniform(x)
        call check(error, x == pf_random_at(777_int64, 3_int64), &
            "%seed with an int32 stream index disagrees with the int64 one")
        if (allocated(error)) return

        ! A default-initialised stream is %seed(0, 0) -- there is no constructor to lean on, since
        ! the type deliberately has no FINAL and no allocatable components.
        call dflt%uniform(x)
        call rng%seed(0_int64, 0_int64)
        call rng%uniform(ref(1))
        call check(error, x == ref(1), "a default-initialised stream is not %seed(0, 0)")
        if (allocated(error)) return

        ! Mixed producers: %uniform32 leaves the position odd, so the next %uniform reads words 1
        ! and 2 of the block -- a pair no tier-0 entry point addresses. Check it against the words.
        call parquet_debug_random_block(777_int64, 3_int64, 0_int64, w0, w1, w2, w3)
        call parquet_debug_random_block(777_int64, 3_int64, 1_int64, v0, v1, v2, v3)
        call rng%seed(777_int64, 3_int64)
        call rng%uniform32(y)
        call check(error, rng%position() == 2_int64, "%uniform32 must cost exactly one word")
        if (allocated(error)) return
        call rng%uniform(x)
        call check(error, x == to_r64_local(ior(ishft(w2, 32), w1)), &
            "a straddling %uniform did not read words 1 and 2, low half first")
        if (allocated(error)) return
        call rng%uniform(x)
        call check(error, x == to_r64_local(ior(ishft(v0, 32), w3)), &
            "a %uniform spanning a block boundary did not read word 3 then word 0 of the next block")
    end subroutine test_stream_values

    !> `%jump`, `%rewind`, `%position`, and the block alignment `%int_range` performs.
    subroutine test_stream_position(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng, ref
        integer :: k
        real(real64) :: a(4), b(4), x
        real(real32) :: y
        integer(int64) :: saved, ir

        ! A fresh stream is at position 1, and each producer costs its documented number of words.
        call rng%seed(5_int64, 2_int64)
        call check(error, rng%position() == 1_int64, "a freshly seeded stream is not at position 1")
        if (allocated(error)) return
        call rng%uniform(x)
        call check(error, rng%position() == 3_int64, "%uniform must cost exactly two words")
        if (allocated(error)) return
        call rng%bits(saved)
        call check(error, rng%position() == 5_int64, "%bits must cost exactly two words")
        if (allocated(error)) return
        call rng%int_range(1_int64, 6_int64, ir)
        call check(error, rng%position() == 9_int64, "%int_range from a block boundary must cost four words")
        if (allocated(error)) return

        ! From an unaligned position, %int_range first advances to the next block boundary.
        call rng%seed(5_int64, 2_int64)
        call rng%uniform32(y)                        ! position 2, which is not a block boundary
        call rng%int_range(1_int64, 6_int64, ir)
        call check(error, rng%position() == 9_int64, &
            "%int_range from an unaligned position must align first, then cost four words")
        if (allocated(error)) return
        call check(error, ir == pf_random_int_at(5_int64, 2_int64, 1_int64, 6_int64, 2_int64), &
            "an aligned %int_range must equal the integer draw of the block it landed on")
        if (allocated(error)) return

        ! %jump(n) equals discarding n words' worth of draws.
        call rng%seed(5_int64, 2_int64)
        call rng%jump(6_int64)                       ! six words == three real64 draws
        call rng%uniform(x)
        call check(error, x == pf_random_at(5_int64, 2_int64, 4_int64), &
            "%jump by six words did not land on draw 4")
        if (allocated(error)) return
        call ref%seed(5_int64, 2_int64)
        do k = 1, 3
            call ref%uniform(a(1))
        end do
        call ref%uniform(a(2))
        call check(error, a(2) == x, "%jump does not equal discarding the same number of words")
        if (allocated(error)) return

        ! A negative jump seeks backwards; the int32 specific agrees with the int64 one.
        call rng%jump(-2_int32)
        call rng%uniform(x)
        call check(error, x == pf_random_at(5_int64, 2_int64, 4_int64), &
            "a backward %jump did not return to the draw just taken")
        if (allocated(error)) return

        ! %rewind round-trips through whatever %position gave.
        call rng%seed(5_int64, 2_int64)
        call rng%uniform(x)
        saved = rng%position()
        do k = 1, 4
            call rng%uniform(a(k))
        end do
        call rng%rewind(saved)
        do k = 1, 4
            call rng%uniform(b(k))
        end do
        do k = 1, 4
            call check(error, a(k) == b(k), "%rewind to a saved %position did not reproduce the same values")
            if (allocated(error)) return
        end do
        call rng%rewind(int(saved, int32))
        call rng%uniform(x)
        call check(error, x == a(1), "the int32 %rewind specific disagrees with the int64 one")
        if (allocated(error)) return

        ! %rewind with no argument is position 1.
        call rng%rewind()
        call check(error, rng%position() == 1_int64, "%rewind with no argument is not position 1")
        if (allocated(error)) return
        call rng%uniform(x)
        call check(error, x == pf_random_at(5_int64, 2_int64, 1_int64), &
            "%rewind with no argument did not return to the first draw")
        if (allocated(error)) return

        ! Reseeding must drop the held block: it indexes the PREVIOUS family's words.
        call rng%seed(5_int64, 2_int64)
        call rng%uniform(x)
        call rng%seed(6_int64, 2_int64)
        call rng%uniform(x)
        call check(error, x == pf_random_at(6_int64, 2_int64, 1_int64), &
            "a reseeded stream answered from the previous seed's block")
    end subroutine test_stream_position

    !> `%fill` equals a loop of the matching scalar binding, from both aligned and unaligned starts.
    subroutine test_stream_fill(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        type(pf_random_stream) :: rng, ref
        integer :: k
        real(real64) :: v(5), w(5)
        real(real32) :: s(5), t(5)
        integer(int64) :: iv(5), iw(5)
        integer(int32) :: jv(5), jw(5)
        real(real64) :: empty(0)

        ! real64, from a pair-aligned start: the fast path.
        call rng%seed(31_int64, 4_int64)
        call rng%fill(v)
        call ref%seed(31_int64, 4_int64)
        do k = 1, 5
            call ref%uniform(w(k))
        end do
        do k = 1, 5
            call check(error, v(k) == w(k), "a real64 %fill disagrees with a loop of %uniform")
            if (allocated(error)) return
        end do
        call check(error, rng%position() == ref%position(), &
            "a real64 %fill left the position somewhere a loop of %uniform would not")
        if (allocated(error)) return

        ! real64, from an UNALIGNED start: the slow path, which no tier-2 entry point can serve.
        call rng%seed(31_int64, 4_int64)
        call rng%uniform32(s(1))
        call rng%fill(v)
        call ref%seed(31_int64, 4_int64)
        call ref%uniform32(t(1))
        do k = 1, 5
            call ref%uniform(w(k))
        end do
        do k = 1, 5
            call check(error, v(k) == w(k), &
                "an unaligned real64 %fill disagrees with a loop of %uniform from the same position")
            if (allocated(error)) return
        end do
        call check(error, rng%position() == ref%position(), &
            "an unaligned real64 %fill left the position somewhere a loop of %uniform would not")
        if (allocated(error)) return

        ! real32: one value per word, so every start is aligned.
        call rng%seed(31_int64, 4_int64)
        call rng%uniform32(s(1))                     ! deliberately start off a block boundary
        call rng%fill(s)
        call ref%seed(31_int64, 4_int64)
        call ref%uniform32(t(1))
        do k = 1, 5
            call ref%uniform32(t(k))
        end do
        do k = 1, 5
            call check(error, s(k) == t(k), "a real32 %fill disagrees with a loop of %uniform32")
            if (allocated(error)) return
        end do

        ! Integer fills align to a block first, exactly as %int_range does.
        call rng%seed(31_int64, 4_int64)
        call rng%fill(iv, 1_int64, 50_int64)
        call ref%seed(31_int64, 4_int64)
        do k = 1, 5
            call ref%int_range(1_int64, 50_int64, iw(k))
        end do
        do k = 1, 5
            call check(error, iv(k) == iw(k), "an int64 %fill disagrees with a loop of %int_range")
            if (allocated(error)) return
        end do
        call check(error, rng%position() == ref%position(), &
            "an int64 %fill left the position somewhere a loop of %int_range would not")
        if (allocated(error)) return

        call rng%seed(31_int64, 4_int64)
        call rng%uniform32(s(1))                     ! unaligned, so the fill must align first
        call rng%fill(jv, 1_int32, 50_int32)
        call ref%seed(31_int64, 4_int64)
        call ref%uniform32(t(1))
        do k = 1, 5
            call ref%int_range(1_int32, 50_int32, jw(k))
        end do
        do k = 1, 5
            call check(error, jv(k) == jw(k), &
                "an unaligned int32 %fill disagrees with a loop of %int_range from the same position")
            if (allocated(error)) return
        end do

        ! A zero-sized fill is a defined no-op that moves nothing.
        call rng%seed(31_int64, 4_int64)
        call rng%fill(empty)
        call check(error, rng%position() == 1_int64, "a zero-sized %fill must not move the position")
    end subroutine test_stream_fill

    !> Golden vectors for `pf_random_perm_at`, from `tools/generate_random_perm_vectors.py`.
    !!
    !! **This is the only thing guarding the frozen contract, and it exists because mutation testing
    !! showed nothing else does.** Changing `perm_rounds` from 4 to 3 survives the bijection test
    !! completely: an odd round count leaves the two factors transposed, and `l*a + r` with `l < b`
    !! and `r < a` is still a bijective encoding of the same domain -- so the result is still a
    !! permutation, just a different one. The same is true of the multipliers, the key schedule and
    !! the width rule. Every one of them changes what `pf_random_perm_at` answers while leaving it a
    !! valid permutation, and these vectors are what notices.
    !!
    !! The generator is an independent arbitrary-precision transcription of the contract, never a
    !! Fortran run -- a vector produced by running the library would only prove it agrees with
    !! itself. It also asserts, in Python where nothing wraps, that no intermediate product reaches
    !! `2**63`, which is the evidence behind the kernel's no-overflow claim.
    !!
    !! Cases chosen for what they exercise: `m = 100` where `a*b == m` exactly, `m = 5` and `m = 7`
    !! where the cycle-walk must actually run, a population above int32, and a **negative seed** --
    !! the key schedule shifts the seed right, and Fortran's `ISHFT` is logical, so a negative seed
    !! is precisely the case an arithmetic shift would get wrong.
    subroutine test_perm_golden(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: NV = 27
        integer(int64) :: sd(NV), mm(NV), kk(NV), ex(NV)
        integer :: j
        character(len=80) :: msg

        sd = [20260816_int64, 20260816_int64, 20260816_int64, 20260816_int64, 20260816_int64, &
              20260816_int64, 20260816_int64, 20260816_int64, 20260816_int64, 20260816_int64, &
              20260816_int64, 20260816_int64, 20260816_int64, 20260816_int64, 1_int64, 1_int64, &
              1_int64, 1_int64, 1_int64, -7_int64, -7_int64, -7_int64, -7_int64, 20260816_int64, &
              20260816_int64, 20260816_int64, 20260816_int64]
        mm = [100_int64, 100_int64, 100_int64, 100_int64, 100_int64, 100_int64, 5_int64, 5_int64, &
              5_int64, 5_int64, 5_int64, 7_int64, 7_int64, 7_int64, 1000_int64, 1000_int64, &
              1000_int64, 1000_int64, 1000_int64, 1000_int64, 1000_int64, 1000_int64, 1000_int64, &
              4000000000_int64, 4000000000_int64, 4000000000_int64, 4000000000_int64]
        kk = [1_int64, 2_int64, 3_int64, 50_int64, 99_int64, 100_int64, 1_int64, 2_int64, 3_int64, &
              4_int64, 5_int64, 1_int64, 4_int64, 7_int64, 1_int64, 2_int64, 500_int64, 999_int64, &
              1000_int64, 1_int64, 2_int64, 500_int64, 1000_int64, 1_int64, 2_int64, &
              3999999999_int64, 4000000000_int64]
        ex = [50_int64, 57_int64, 15_int64, 70_int64, 60_int64, 63_int64, 1_int64, 5_int64, &
              4_int64, 2_int64, 3_int64, 7_int64, 1_int64, 6_int64, 879_int64, 530_int64, &
              65_int64, 176_int64, 547_int64, 614_int64, 185_int64, 562_int64, 298_int64, &
              2145434036_int64, 2404499848_int64, 2116569574_int64, 6342502_int64]

        do j = 1, NV
            if (pf_random_perm_at(sd(j), mm(j), kk(j)) /= ex(j)) then
                write(msg, '(a,i0,a,i0,a,i0,a,i0,a,i0)') "pf_random_perm_at(", sd(j), ", ", mm(j), &
                    ", ", kk(j), ") = ", pf_random_perm_at(sd(j), mm(j), kk(j)), " expected ", ex(j)
                call check(error, .false., trim(msg))
                return
            end if
        end do
        call check(error, .true., "unreachable")
    end subroutine test_perm_golden

    !> `pf_random_perm_at` really is a permutation, checked exhaustively over every small `m`.
    !!
    !! **This is the one property whose failure is unrecoverable**, and it is cheap to check
    !! completely rather than by sampling: for each `m` collect all `m` outputs and assert each value
    !! in `1 .. m` appears exactly once. A cycle-walk that terminated early, a round count that left
    !! the factors transposed, or an off-by-one in the split would all show up here and in nothing
    !! else -- the values would still look random and still lie in range.
    !!
    !! The sweep deliberately includes every `m` from 1 to 200 rather than a few round numbers,
    !! because the interesting cases are the ones where `a*b > m` and the walk is actually entered
    !! (m = 5 gives 3x2 = 6, m = 7 gives 3x3 = 9), and those are not where anyone would look.
    subroutine test_perm_bijection(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: m, k, v, seeds(3)
        logical :: seen(200)
        integer :: si
        character(len=64) :: msg

        seeds = [1_int64, 20260816_int64, -7_int64]
        do si = 1, 3
            do m = 1_int64, 200_int64
                seen(1:int(m, int32)) = .false.
                do k = 1_int64, m
                    v = pf_random_perm_at(seeds(si), m, k)
                    if (v < 1_int64 .or. v > m) then
                        write(msg, '(a,i0,a,i0)') "permutation escaped [1,m] at m=", m, " k=", k
                        call check(error, .false., trim(msg))
                        return
                    end if
                    if (seen(int(v, int32))) then
                        write(msg, '(a,i0,a,i0)') "permutation repeated a value at m=", m, " v=", v
                        call check(error, .false., trim(msg))
                        return
                    end if
                    seen(int(v, int32)) = .true.
                end do
            end do
        end do
        call check(error, .true., "unreachable")
    end subroutine test_perm_bijection

    !> The permutation's contract: prefixes, subsets, kind agreement, clamping and determinism.
    subroutine test_perm_contract(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: SD = 4242_int64
        integer(int64) :: m, k, a(10), b(10), big, v1, v2
        integer(int32) :: v32
        integer :: nfix, trial
        logical :: seen(64)

        ! A subset IS a prefix of the permutation -- by construction here, which is exactly why it
        ! must be asserted: the moment anyone restructures one side, this is what fails.
        m = 1000_int64
        do k = 1_int64, 10_int64
            a(k) = pf_random_perm_at(SD, m, k)
        end do
        do k = 1_int64, 4_int64
            b(k) = pf_random_perm_at(SD, m, k)
        end do
        do k = 1_int64, 4_int64
            call check(error, a(k) == b(k), "a size-4 subset is not a prefix of a size-10 one")
            if (allocated(error)) return
        end do

        ! The subset's members are distinct.
        seen = .false.
        do k = 1_int64, 10_int64
            call check(error, .not. seen(int(a(k), int32) / 16 + 1), "subset bucket check misconfigured")
            if (allocated(error)) return
        end do

        ! Both kinds agree, and the result follows the argument kind.
        v32 = pf_random_perm_at(SD, 1000_int32, 7_int32)
        v1 = pf_random_perm_at(SD, 1000_int64, 7_int64)
        call check(error, int(v32, int64) == v1, "the int32 permutation specific disagrees with the int64 one")
        if (allocated(error)) return

        ! `k` outside [1, m] is clamped, not reported -- this is pure elemental and cannot abort.
        call check(error, pf_random_perm_at(SD, 1000_int64, 0_int64) == &
                          pf_random_perm_at(SD, 1000_int64, 1_int64), "k below 1 must clamp to 1")
        if (allocated(error)) return
        call check(error, pf_random_perm_at(SD, 1000_int64, 1001_int64) == &
                          pf_random_perm_at(SD, 1000_int64, 1000_int64), "k above m must clamp to m")
        if (allocated(error)) return

        ! A degenerate population is defined, not undefined.
        call check(error, pf_random_perm_at(SD, 1_int64, 1_int64) == 1_int64, &
            "the one-element permutation must be the identity")
        if (allocated(error)) return
        call check(error, pf_random_perm_at(SD, 0_int64, 1_int64) == 1_int64, &
            "a non-positive population must be absorbed, not undefined")
        if (allocated(error)) return

        ! Deterministic: the same coordinates always give the same value.
        call check(error, pf_random_perm_at(SD, 1000_int64, 500_int64) == &
                          pf_random_perm_at(SD, 1000_int64, 500_int64), "the permutation is not deterministic")
        if (allocated(error)) return

        ! A population far above int32, spot-checked for range and distinctness. The point is that
        ! the factor arithmetic and the multiply-shift stay in range where a 32-bit form would not.
        big = 4000000000_int64
        v1 = pf_random_perm_at(SD, big, 1_int64)
        v2 = pf_random_perm_at(SD, big, 2_int64)
        call check(error, v1 >= 1_int64 .and. v1 <= big .and. v2 >= 1_int64 .and. v2 <= big, &
            "a permutation of a population above int32 escaped its range")
        if (allocated(error)) return
        call check(error, v1 /= v2, "two positions of a large permutation collided")
        if (allocated(error)) return

        ! A weak but real quality floor: over many seeds the fixed-point count of a size-64
        ! permutation averages about 1 (Poisson(1)). A construction that had collapsed -- an identity
        ! map, a constant, a two-round network -- would miss this by a wide margin.
        nfix = 0
        do trial = 1, 200
            do k = 1_int64, 64_int64
                if (pf_random_perm_at(int(trial, int64) * 7919_int64, 64_int64, k) == k) nfix = nfix + 1
            end do
        end do
        call check(error, nfix >= 80 .and. nfix <= 320, &
            "fixed points over 200 permutations of 64 elements are far from the expected ~200")
    end subroutine test_perm_contract

    !> The module's `real64` mapping, repeated here so a straddling pair can be checked directly.
    pure function to_r64_local(bits) result(r)
        integer(int64), intent(in) :: bits          !! any 64-bit pattern
        real(real64) :: r                           !! `[0, 1)`
        r = real(ishft(bits, -11), real64) * 2.0_real64**(-53)
    end function to_r64_local

    !> `pf_random_at` is exactly the top 53 bits of `pf_random_bits_at`, over a sweep.
    subroutine test_bits_identity(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: mismatches
        integer(int64) :: seed, i, d
        real(real64) :: expected

        mismatches = 0
        do seed = -3_int64, 3_int64
            do i = -2_int64, 4_int64
                do d = 1_int64, 5_int64
                    expected = real(ishft(pf_random_bits_at(seed, i, d), -11), real64) * 2.0_real64**(-53)
                    if (pf_random_at(seed, i, d) /= expected) mismatches = mismatches + 1
                end do
            end do
        end do
        call check(error, mismatches == 0, &
            "pf_random_at is not the top 53 bits of pf_random_bits_at everywhere -- the two are contractually identical")
    end subroutine test_bits_identity

    !> **The integer draw and the real draws read the SAME block at the same coordinate**, so they
    !! are not independent draws — this pins that mechanism rather than the wording that describes
    !! it.
    !!
    !! `int_at_impl`'s block index is `draw - 1` and `bits_of`'s is `(draw - 1)/2`; those coincide
    !! at draw 1, which is the default and the commonest call. The consequence is measurable and is
    !! what this asserts: at a small range the integer is a **deterministic function** of the real,
    !! agreeing for every stream tried, where taking the integer one draw along agrees at the 1-in-6
    !! rate independence predicts.
    !!
    !! **Why this test exists at all.** Nothing else in the suite asserts the relationship in either
    !! direction, so a refactor that changed either block index would silently leave both the
    !! doc-comment on `int_at_impl` and the "Endpoints and identities" section of
    !! `doc/pages/utilities/random.md` describing something the code no longer does — and that has
    !! already happened once, in the direction of claiming the words are *not* shared. Both texts
    !! were corrected against measurements taken on two machines; this is what stops them drifting
    !! back.
    !!
    !! The draw-2 arm is a **negative control**, not decoration: without it, a change that made the
    !! integer rule agree with the real draw at *every* draw would pass the first assertion while
    !! destroying the very separation the documentation tells callers to rely on.
    !!
    !! Note the module passes the **seed itself** as Philox's key, so `parquet_debug_random_block`
    !! at block index 0 reaches exactly the block a draw-1 call uses — no key derivation stands
    !! between them.
    subroutine test_int_shares_block(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: seed = 987654321_int64
        integer(int64), parameter :: nstream = 2000_int64
        integer(int64) :: i, w0, w1, w2, w3, bits, want, agree_d1, agree_d2
        real(real64) :: rv

        agree_d1 = 0_int64
        agree_d2 = 0_int64
        do i = 1_int64, nstream
            call parquet_debug_random_block(seed, i, 0_int64, w0, w1, w2, w3)
            bits = ior(ishft(w1, 32), w0)
            call check(error, pf_random_bits_at(seed, i, 1_int64) == bits, &
                "draw 1 of pf_random_bits_at must be block 0's first two words, low half first")
            if (allocated(error)) return
            rv = real(ishft(bits, -11), real64) * 2.0_real64**(-53)
            call check(error, pf_random_at(seed, i, 1_int64) == rv, &
                "draw 1 of pf_random_at must be that same block's top 53 bits scaled into [0, 1)")
            if (allocated(error)) return
            want = 1_int64 + int(6.0_real64 * rv, int64)
            if (pf_random_int_at(seed, i, 1_int64, 6_int64) == want) agree_d1 = agree_d1 + 1_int64
            if (pf_random_int_at(seed, i, 1_int64, 6_int64, 2_int64) == want) agree_d2 = agree_d2 + 1_int64
        end do
        call check(error, agree_d1 == nstream, &
            "at draw 1 the integer draw must be a deterministic function of the real draw -- both read " // &
            "block 0, so pf_random_int_at(seed, i, 1, 6) is 1 + floor(6 * pf_random_at(seed, i))")
        if (allocated(error)) return
        call check(error, agree_d2 < nstream / 2_int64, &
            "negative control: taking the integer at draw 2 must BREAK that agreement -- it reads block 1 " // &
            "while the real draw reads block 0, so agreement should fall to roughly 1 in 6")
    end subroutine test_int_shares_block

    !> The `int32` specifics must be bit-identical to the `int64` ones at equal argument values.
    !!
    !! All five generics are covered, `pf_random_fill_draws` included -- nothing else in the suite
    !! calls its two `int32`-stream specifics, so half of that generic would otherwise ship
    !! untested. The integer specific is additionally driven across the FULL `int32` range, where
    !! the width is 2**32: every other range tested here is small enough that a truncating narrowing
    !! from the `int64` worker would still return the right answer, so only this one can tell that
    !! the worker was handed the widened bounds.
    subroutine test_kind_specifics(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int32), parameter :: lo32 = -huge(1_int32) - 1_int32, hi32 = huge(1_int32)
        integer :: mismatches, bad_fill, bad_wide
        integer(int32) :: i32
        integer(int64) :: seed
        real(real64) :: f64_a(4), f64_b(4)
        real(real32) :: f32_a(4), f32_b(4)

        mismatches = 0
        bad_fill = 0
        bad_wide = 0
        seed = 12345_int64
        do i32 = -4_int32, 6_int32
            if (pf_random_bits_at(seed, i32) /= pf_random_bits_at(seed, int(i32, int64))) mismatches = mismatches + 1
            if (pf_random_at(seed, i32) /= pf_random_at(seed, int(i32, int64))) mismatches = mismatches + 1
            if (pf_random32_at(seed, i32) /= pf_random32_at(seed, int(i32, int64))) mismatches = mismatches + 1
            if (int(pf_random_int_at(seed, i32, 1_int32, 1000_int32), int64) /= &
                pf_random_int_at(seed, int(i32, int64), 1_int64, 1000_int64)) mismatches = mismatches + 1
            if (pf_random_key(seed, i32) /= pf_random_key(seed, int(i32, int64))) mismatches = mismatches + 1

            ! The fill generic's int32-stream specifics, once with the default start draw and once
            ! with an explicit one, so the argument is not merely defaulted past.
            call pf_random_fill_draws(seed, i32, f64_a)
            call pf_random_fill_draws(seed, int(i32, int64), f64_b)
            if (any(f64_a /= f64_b)) bad_fill = bad_fill + 1
            call pf_random_fill_draws(seed, i32, f32_a, 3_int64)
            call pf_random_fill_draws(seed, int(i32, int64), f32_b, 3_int64)
            if (any(f32_a /= f32_b)) bad_fill = bad_fill + 1

            if (int(pf_random_int_at(seed, i32, lo32, hi32), int64) /= &
                pf_random_int_at(seed, int(i32, int64), int(lo32, int64), int(hi32, int64))) bad_wide = bad_wide + 1
        end do
        call check(error, mismatches == 0, &
            "an int32 specific disagrees with its int64 twin -- an int32 stream or label must sign-extend")
        if (allocated(error)) return
        call check(error, bad_fill == 0, &
            "an int32-stream pf_random_fill_draws disagrees with its int64-stream twin")
        if (allocated(error)) return
        call check(error, bad_wide == 0, &
            "the int32 pf_random_int_at disagrees with its int64 twin over the full int32 range, whose width is 2**32")
    end subroutine test_kind_specifics

    !> An elemental call over an array equals the scalar calls, elementwise, at rank 1 and rank 2.
    !!
    !! Swept over each of the three elemental arguments in turn, not just the stream index: every
    !! dummy of every tier-0 procedure is elemental, and a specific that read its seed or its draw
    !! from element 1 and reused it across the array would pass a stream-only sweep unchanged. Each
    !! sweep carries a guard that the values actually vary, since a procedure ignoring the swept
    !! argument altogether would otherwise agree with itself on every element.
    subroutine test_elemental(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, r, c
        integer(int64) :: idx(6), grid(2, 3), seeds(6), draws(6)
        real(real64) :: vec(6), mat(2, 3), svec(6), dvec(6)

        idx = [(int(k, int64), k = 1, 6)]
        vec = pf_random_at(12345_int64, idx)
        do k = 1, 6
            call check(error, vec(k) == pf_random_at(12345_int64, idx(k)), &
                "a rank-1 elemental call disagrees with the scalar call for that element")
            if (allocated(error)) return
        end do

        grid = reshape([(int(k, int64), k = 1, 6)], [2, 3])
        mat = pf_random_at(12345_int64, grid)
        do c = 1, 3
            do r = 1, 2
                call check(error, mat(r, c) == pf_random_at(12345_int64, grid(r, c)), &
                    "a rank-2 elemental call disagrees with the scalar call for that element")
                if (allocated(error)) return
            end do
        end do

        seeds = [(int(k, int64) * 1000_int64 + 7_int64, k = 1, 6)]
        svec = pf_random_at(seeds, 1_int64)
        do k = 1, 6
            call check(error, svec(k) == pf_random_at(seeds(k), 1_int64), &
                "an elemental call over seed disagrees with the scalar call for that element")
            if (allocated(error)) return
        end do
        call check(error, any(svec /= svec(1)), &
            "an elemental sweep over seed returned one repeated value, so the seed argument is not reaching the stream")
        if (allocated(error)) return

        draws = [(int(k, int64), k = 1, 6)]
        dvec = pf_random_at(12345_int64, 1_int64, draws)
        do k = 1, 6
            call check(error, dvec(k) == pf_random_at(12345_int64, 1_int64, draws(k)), &
                "an elemental call over draw disagrees with the scalar call for that element")
            if (allocated(error)) return
        end do
        call check(error, any(dvec /= dvec(1)), &
            "an elemental sweep over draw returned one repeated value, so the draw argument is not reaching the counter")
    end subroutine test_elemental

    !> The totality rules: `lo > hi` swaps, `draw < 1` clamps, degenerate and extreme arguments.
    subroutine test_edges(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        ! 2**62 is the ceiling the reference used to overflow at; huge is the true top of the axis.
        integer(int64), parameter :: draw_tops(2) = [4611686018427387904_int64, huge(1_int64)]
        integer :: g
        integer(int64) :: k, top

        call check(error, pf_random_int_at(12345_int64, 1_int64, 999999_int64, 0_int64) == &
                          pf_random_int_at(12345_int64, 1_int64, 0_int64, 999999_int64), &
            "lo > hi must swap, giving the same value as the ordered call")
        if (allocated(error)) return

        call check(error, pf_random_int_at(12345_int64, 1_int64, 7_int64, 7_int64) == 7_int64, &
            "a degenerate range must return its only value")
        if (allocated(error)) return

        do k = -3_int64, 0_int64
            call check(error, pf_random_bits_at(12345_int64, 1_int64, k) == pf_random_bits_at(12345_int64, 1_int64), &
                "a draw below 1 must clamp to 1")
            if (allocated(error)) return
            call check(error, pf_random_at(12345_int64, 1_int64, k) == pf_random_at(12345_int64, 1_int64), &
                "a draw below 1 must clamp to 1 for pf_random_at too")
            if (allocated(error)) return
            call check(error, pf_random32_at(12345_int64, 1_int64, k) == pf_random32_at(12345_int64, 1_int64), &
                "a draw below 1 must clamp to 1 for pf_random32_at too")
            if (allocated(error)) return
        end do

        ! Extreme seeds and streams are ordinary values, not special cases.
        call check(error, pf_random_bits_at(huge(1_int64), 1_int64) /= &
                          pf_random_bits_at(-huge(1_int64) - 1_int64, 1_int64), &
            "the two extreme seeds must give different streams")
        if (allocated(error)) return
        call check(error, pf_random_bits_at(12345_int64, huge(1_int64)) /= &
                          pf_random_bits_at(12345_int64, -huge(1_int64) - 1_int64), &
            "the two extreme stream indices must give different values")
        if (allocated(error)) return

        ! Distinct coordinates give distinct streams: the property the whole design rests on.
        call check(error, pf_random_bits_at(1_int64, 1_int64) /= pf_random_bits_at(2_int64, 1_int64), &
            "two seeds must not give the same first value")
        if (allocated(error)) return
        call check(error, pf_random_bits_at(1_int64, 1_int64) /= pf_random_bits_at(1_int64, 2_int64), &
            "two streams must not give the same first value")
        if (allocated(error)) return

        ! Extreme DRAW indices, which the golden tables stop far short of. Both the library and the
        ! reference derive the block index without ever forming a word index -- (draw-1)/2 with the
        ! slot taken modulo -- so every draw representable in int64 is reachable by both, and the
        ! two can be compared against each other right at the top.
        !
        ! That was not always true, and the way it failed is why this sweep spans two bases rather
        ! than one. The reference used to reach its block through the word index 2*(draw-1)+1, which
        ! overflows above draw 2**62; this sweep had to stop at that ceiling, and above it the
        ! reference reported the LIBRARY as wrong when the library was right. 2**62 is kept as a
        ! base precisely because it is where that used to break. If a future change reintroduces a
        ! multiplied index on either side, this is the test that reports it -- as a disagreement at
        ! the top of the range and nowhere else.
        do g = 1, 2
            do k = 0_int64, 3_int64
                top = draw_tops(g) - k
                call check(error, pf_random_bits_at(12345_int64, 1_int64, top) == ref_bits(12345_int64, 1_int64, top), &
                    "pf_random_bits_at disagrees with the strict reference near the top of the draw range")
                if (allocated(error)) return
                call check(error, pf_random_at(12345_int64, 1_int64, top) == ref_at(12345_int64, 1_int64, top), &
                    "pf_random_at disagrees with the strict reference near the top of the draw range")
                if (allocated(error)) return
                call check(error, pf_random32_at(12345_int64, 1_int64, top) == ref_at32(12345_int64, 1_int64, top), &
                    "pf_random32_at disagrees with the strict reference near the top of the draw range")
                if (allocated(error)) return
                ! The bits-to-value identity has to survive up here too, independently of the
                ! reference agreeing -- it is what would still catch a shared index mistake.
                call check(error, pf_random_at(12345_int64, 1_int64, top) == &
                                  real(ishft(pf_random_bits_at(12345_int64, 1_int64, top), -11), real64) &
                                  * 2.0_real64**(-53), &
                    "pf_random_at is not the top 53 bits of pf_random_bits_at near the top of the draw range")
                if (allocated(error)) return
            end do
        end do
        call check(error, pf_random_bits_at(12345_int64, 1_int64, huge(1_int64)) /= &
                          pf_random_bits_at(12345_int64, 1_int64, huge(1_int64) - 1_int64), &
            "the two topmost draws returned the same bits, so the draw index stopped reaching the counter")
        if (allocated(error)) return

        ! Extreme STREAM indices, which are the other half of the same property and were the last
        ! part of the counter left unpinned. `random_block` splits the stream across counter words
        ! c2 and c3, and c3 is `ishft(stream, -32)` -- so it is zero for every stream below 2**32
        ! and all-ones for every small negative one. Every other stream in this suite is small: the
        ! golden tables use {-5, 0, 1, 2, 10**6} and the agreement sweep runs -40..40, so nothing
        ! outside this loop has ever compared a value whose c3 was anything else.
        !
        ! What that left undetectable is not subtle. A `c3` derived from the stream's SIGN alone
        ! reproduces every value in the rest of the suite exactly, and makes streams `i` and
        ! `i + 2**32` identical -- one draw per row of a table with more than 4.3 billion rows is
        ! an ordinary use of this module, and it would silently repeat. Confirmed by mutation:
        ! before this loop existed, that change passed all nineteen tests.
        !
        ! The two assertions below fail for different reasons and both are wanted. The agreement
        ! against the reference is the one that catches a wrong c3; the inequality is what still
        ! catches a c3 that has stopped varying at all, which agreement alone would not report if
        ! the reference ever acquired the same fault. Unlike the draw axis there is no ceiling to
        ! respect: neither side derives a stream index by arithmetic, so every int64 stream is
        ! directly comparable.
        do k = 32_int64, 62_int64
            top = ishft(1_int64, int(k, int32)) + 12345_int64
            call check(error, pf_random_bits_at(777_int64, top) == ref_bits(777_int64, top, 1_int64), &
                "pf_random_bits_at disagrees with the strict reference at a stream index above 2**32")
            if (allocated(error)) return
            call check(error, pf_random_bits_at(777_int64, -top) == ref_bits(777_int64, -top, 1_int64), &
                "pf_random_bits_at disagrees with the strict reference at a stream index below -2**32")
            if (allocated(error)) return
            call check(error, pf_random32_at(777_int64, top) == ref_at32(777_int64, top, 1_int64), &
                "pf_random32_at disagrees with the strict reference at a stream index above 2**32")
            if (allocated(error)) return
            call check(error, pf_random_bits_at(777_int64, top) /= pf_random_bits_at(777_int64, top - 4294967296_int64), &
                "streams 2**32 apart returned the same bits, so the stream's high word is not reaching counter word c3")
            if (allocated(error)) return
        end do
    end subroutine test_edges

    !> A derived key is a seed, so derivations nest.
    subroutine test_key_composition(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: outer, inner

        outer = pf_random_key(12345_int64, 7_int64)
        inner = pf_random_key(outer, 3_int64)
        call check(error, inner == ref_key(ref_key(12345_int64, 7_int64), 3_int64), &
            "a nested pf_random_key does not agree with the reference")
        if (allocated(error)) return
        call check(error, pf_random_at(inner, 1_int64) /= pf_random_at(outer, 1_int64), &
            "a derived family must not share its parent's stream")
        if (allocated(error)) return
        call check(error, pf_random_key(12345_int64, 1_int64) /= pf_random_key(12345_int64, 2_int64), &
            "two labels must derive different keys")
    end subroutine test_key_composition

    !> `pf_random_seed` stays in range and does not repeat.
    !!
    !! Nondeterministic by design, so it carries no golden vectors -- these are the only properties
    !! it promises. Distinctness across THREADS is asserted in the `random_omp` suite, where a real
    !! OpenMP team exists; test-drive's own parallelism runs whole tests concurrently, so nothing
    !! here would compare one thread's values against another's.
    subroutine test_seed(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: n = 100
        integer(int64) :: s(n)
        integer :: k, j, duplicates

        do k = 1, n
            s(k) = pf_random_seed()
        end do
        call check(error, all(s >= 1_int64), "pf_random_seed returned a value below 1")
        if (allocated(error)) return
        duplicates = 0
        do k = 1, n
            do j = k + 1, n
                if (s(k) == s(j)) duplicates = duplicates + 1
            end do
        end do
        call check(error, duplicates == 0, &
            "pf_random_seed repeated within 100 consecutive calls -- the process-wide counter is not reaching the mixer")
    end subroutine test_seed

    !> Both real kinds stay inside `[0, 1)` over a large fixed-seed sweep.
    !!
    !! 1.0 is unreachable by construction in both -- which is why `pf_random32_at` has its own
    !! sequence rather than narrowing a `real64` draw, since that narrowing could round up to 1.0.
    !! 0.0 is attainable, with probability 2**-53; it is documented rather than searched for.
    subroutine test_containment(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: n = 200000
        integer :: k, out64, out32
        real(real64) :: x
        real(real32) :: y

        out64 = 0
        out32 = 0
        do k = 1, n
            x = pf_random_at(20260816_int64, int(k, int64))
            if (.not. (x >= 0.0_real64 .and. x < 1.0_real64)) out64 = out64 + 1
            y = pf_random32_at(20260816_int64, int(k, int64))
            if (.not. (y >= 0.0_real32 .and. y < 1.0_real32)) out32 = out32 + 1
        end do
        call check(error, out64 == 0, "pf_random_at produced a value outside [0, 1)")
        if (allocated(error)) return
        call check(error, out32 == 0, "pf_random32_at produced a value outside [0, 1)")
    end subroutine test_containment

    ! ============================================================================================
    ! Layer 3 -- the fork, and agreement with the strict reference
    ! ============================================================================================

    !> The compiled route (e) fork must match what the compiler can actually do.
    !!
    !! The fork is selected by a compiler ALLOWLIST, so it has a silent direction the preprocessor
    !! cannot close: a compiler that has a 128-bit kind but is not named in the list would quietly
    !! compile the wrapping kernel. This single assertion is what closes it, evaluated with the
    !! consuming compiler at the moment the suite builds.
    subroutine test_fork_selection(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion

        call check(error, parquet_debug_random_uses_int128() .eqv. (selected_int_kind(38) > 0), &
            "the compiled multiply path disagrees with this compiler's 128-bit capability: either a capable compiler " // &
            "is silently shipping the wrapping kernel, or the fork was selected on a target that cannot support it")
    end subroutine test_fork_selection

    !> Scalar draws against the strict, overflow-free reference.
    !!
    !! COMMENT DISCIPLINE FOR THIS AND EVERY OTHER AGREEMENT LOOP BELOW: plain integer counters
    !! only. No `write`, no recorded first mismatch, no running checksum. Three separate such
    !! instruments were once added inside a loop like this one and each made a REAL fault vanish --
    !! observing the comparison changed the code the compiler generated for it. A future tidy-up
    !! that adds "just a diagnostic" here switches the test off without changing its verdict.
    subroutine test_agreement_scalar(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: seeds(5) = [0_int64, 1_int64, 12345_int64, -7_int64, huge(1_int64)]
        integer :: bad_bits, bad_at, bad_at32, bad_lit, bad_pos, bad_neg
        integer(int64) :: si, i, d

        bad_bits = 0
        bad_at = 0
        bad_at32 = 0
        do si = 1_int64, 5_int64
            do i = -40_int64, 40_int64
                do d = -2_int64, 7_int64
                    if (pf_random_bits_at(seeds(si), i, d) /= ref_bits(seeds(si), i, max(d, 1_int64))) &
                        bad_bits = bad_bits + 1
                    if (pf_random_at(seeds(si), i, d) /= ref_at(seeds(si), i, max(d, 1_int64))) &
                        bad_at = bad_at + 1
                    if (pf_random32_at(seeds(si), i, d) /= ref_at32(seeds(si), i, max(d, 1_int64))) &
                        bad_at32 = bad_at32 + 1
                end do
            end do
        end do
        call check(error, bad_bits == 0, "pf_random_bits_at disagrees with the strict reference")
        if (allocated(error)) return
        call check(error, bad_at == 0, "pf_random_at disagrees with the strict reference")
        if (allocated(error)) return
        call check(error, bad_at32 == 0, "pf_random32_at disagrees with the strict reference")
        if (allocated(error)) return

        ! The same comparison with the seed written as a LITERAL CONSTANT at the call site, and
        ! with `draw` both present and absent. This is not redundant with the sweep above, and the
        ! reason is a measured cross-machine finding rather than caution: a compiler clones and
        ! specialises a procedure on whatever is constant at the call site, so the literal-seed and
        ! all-variable forms are frequently DIFFERENT compiled code.
        !
        ! Neither shape is reliably the safe one. Building the wrapping kernel under LTO,
        ! gfortran 15.2 returned wrong `pf_random32_at` values for literal-seed shapes while the
        ! all-variable shape stayed correct, and gfortran 14.2.1 did the exact reverse on the same
        ! source -- all-variable wrong, literal-seed clean. A suite that swept only one of them
        ! would have passed on one of those two compilers. So both are swept, and a future reader
        ! should resist merging them back together.
        bad_lit = 0
        do i = -8_int64, 8_int64
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) bad_lit = bad_lit + 1
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) bad_lit = bad_lit + 1
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) bad_lit = bad_lit + 1
            end do
            ! `draw` absent -- the documented loop idiom, and a distinct specialisation again.
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) bad_lit = bad_lit + 1
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) bad_lit = bad_lit + 1
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) bad_lit = bad_lit + 1
        end do
        call check(error, bad_lit == 0, &
            "a literal-constant-seed call disagrees with the strict reference: literal-seed and all-variable call " // &
            "shapes are specialised separately by the compiler, and each has been caught while the other was clean")
        if (allocated(error)) return

        ! ONE-SIDED stream ranges, each as its own loop. This axis is what decides whether either
        ! sweep above can see the fault class they exist for, and both of them are written with
        ! bounds that SPAN ZERO -- which is the shape measured NOT to detect it. Against the
        ! wrapping kernel under LTO, where gfortran returns wrong `pf_random32_at` values, these
        ! two loops report 200 and 520 mismatches while the `-40..40` and `-8..8` sweeps above
        ! report none, and the shipped kernel reports none anywhere. The bounds of the loop, not
        ! the values it visits, are what the optimiser reasons from: `-40..40` already visits
        ! 1..40 and still sees nothing.
        !
        ! **Both signs are swept because both were measured to fire, and that is wider than
        ! feature_risks.md Risk-101 records.** Its table lists the broken ranges as the
        ! non-negative ones (`1..1`, `0..7`, `1..8`, `1..24`, `1..64`) against zero-spanning ones
        ! that are clean; it never tried a strictly negative range. `-40..-1` fires here, harder
        ! than `1..40` does. So the rule is one-sided-versus-spanning-zero rather than anything
        ! about the sign itself, and a sweep that covered only the non-negative half on the
        ! strength of that table would be resting on an untested asymmetry.
        !
        ! Two things a future reader must not undo. `do i = 1, n` is the module's own documented
        ! idiom, so a broken range is one users actually write; and "cover the negatives too" is
        ! the natural instinct, which -- written as one symmetric loop -- lands squarely in the
        ! clean shape. Keep these as two loops with their own literal bounds and never merge them.
        !
        ! One caveat, so the numbers above are not read as more than they are: whether a given
        ! loop is miscompiled depends on the compiled form as a whole, not on its bounds alone.
        ! The same comparison written with the bounds passed in as dummy arguments detects nothing
        ! at all, on either kernel. These loops keep literal bounds for that reason.
        bad_pos = 0
        bad_neg = 0
        do i = 1_int64, 40_int64                        ! provably non-negative, as its own loop
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) bad_pos = bad_pos + 1
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) bad_pos = bad_pos + 1
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) bad_pos = bad_pos + 1
            end do
            ! `draw` absent as well: the loop idiom in full, and a distinct specialisation again.
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) bad_pos = bad_pos + 1
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) bad_pos = bad_pos + 1
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) bad_pos = bad_pos + 1
        end do
        do i = -40_int64, -1_int64                      ! strictly negative, as its own loop
            do d = 1_int64, 4_int64
                if (pf_random_bits_at(12345_int64, i, d) /= ref_bits(12345_int64, i, d)) bad_neg = bad_neg + 1
                if (pf_random_at(12345_int64, i, d) /= ref_at(12345_int64, i, d)) bad_neg = bad_neg + 1
                if (pf_random32_at(12345_int64, i, d) /= ref_at32(12345_int64, i, d)) bad_neg = bad_neg + 1
            end do
            if (pf_random_bits_at(12345_int64, i) /= ref_bits(12345_int64, i, 1_int64)) bad_neg = bad_neg + 1
            if (pf_random_at(12345_int64, i) /= ref_at(12345_int64, i, 1_int64)) bad_neg = bad_neg + 1
            if (pf_random32_at(12345_int64, i) /= ref_at32(12345_int64, i, 1_int64)) bad_neg = bad_neg + 1
        end do
        call check(error, bad_pos == 0, &
            "a PROVABLY NON-NEGATIVE stream range disagrees with the strict reference -- this is the range an " // &
            "optimiser can reason about and the one `do i = 1, n` produces, and it is the range the sweeps above " // &
            "cannot see, because their bounds span zero (feature_risks.md Risk-101)")
        if (allocated(error)) return
        call check(error, bad_neg == 0, &
            "a strictly negative stream range disagrees with the strict reference: it is swept as its own loop " // &
            "because a range spanning zero is compiled differently from either one-sided range")
    end subroutine test_agreement_scalar

    !> Fills against the strict reference, over several lengths and start offsets.
    subroutine test_agreement_fill(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: bad64, bad32, m, k, bad_pos, bad_neg
        integer(int64) :: start, i
        real(real64) :: v64(9)
        real(real32) :: v32(9)

        bad64 = 0
        bad32 = 0
        do i = -3_int64, 3_int64
            do m = 1, 9
                do start = 1_int64, 6_int64
                    call pf_random_fill_draws(12345_int64, i, v64(1:m), start)
                    call pf_random_fill_draws(12345_int64, i, v32(1:m), start)
                    do k = 1, m
                        if (v64(k) /= ref_at(12345_int64, i, start + int(k, int64) - 1_int64)) bad64 = bad64 + 1
                        if (v32(k) /= ref_at32(12345_int64, i, start + int(k, int64) - 1_int64)) bad32 = bad32 + 1
                    end do
                end do
            end do
        end do
        call check(error, bad64 == 0, "a real64 fill disagrees with the strict reference")
        if (allocated(error)) return
        call check(error, bad32 == 0, "a real32 fill disagrees with the strict reference")
        if (allocated(error)) return

        ! The same one-sided stream ranges `test_agreement_scalar` closes with, for the same
        ! reason: the sweep above runs `-3..3`, whose bounds span zero, and that is the shape the
        ! Risk-101 fault class is invisible in. Both fills reach the same cipher the scalar draws
        ! do, so a miscompiled `random_block` can reach them too.
        !
        ! **Precautionary rather than demonstrated, and the distinction is the honest one to keep
        ! here.** Unlike its counterpart in `test_agreement_scalar` -- which fires 200 and 520
        ! times against the known instance -- these two loops detect NOTHING against it: that
        ! instance lands on `pf_random32_at`'s own path and leaves `fill_r32`/`fill_r64`, which
        ! are separate procedures with their own specialisation, correct. So this has no negative
        ! control and must not be described as though it had one. It is kept because the shape is
        ! one line either way and the next instance need not pick the same procedure; if it is
        ! ever removed, remove it for that reason and not on the belief that it was covering
        ! something.
        bad_pos = 0
        bad_neg = 0
        do i = 1_int64, 6_int64                         ! provably non-negative, as its own loop
            do m = 1, 9
                call pf_random_fill_draws(12345_int64, i, v64(1:m))
                call pf_random_fill_draws(12345_int64, i, v32(1:m))
                do k = 1, m
                    if (v64(k) /= ref_at(12345_int64, i, int(k, int64))) bad_pos = bad_pos + 1
                    if (v32(k) /= ref_at32(12345_int64, i, int(k, int64))) bad_pos = bad_pos + 1
                end do
            end do
        end do
        do i = -6_int64, -1_int64                       ! strictly negative, as its own loop
            do m = 1, 9
                call pf_random_fill_draws(12345_int64, i, v64(1:m))
                call pf_random_fill_draws(12345_int64, i, v32(1:m))
                do k = 1, m
                    if (v64(k) /= ref_at(12345_int64, i, int(k, int64))) bad_neg = bad_neg + 1
                    if (v32(k) /= ref_at32(12345_int64, i, int(k, int64))) bad_neg = bad_neg + 1
                end do
            end do
        end do
        call check(error, bad_pos == 0, &
            "a fill over a provably non-negative stream range disagrees with the strict reference -- the sweep " // &
            "above cannot see this, because its bounds span zero (feature_risks.md Risk-101)")
        if (allocated(error)) return
        call check(error, bad_neg == 0, &
            "a fill over a strictly negative stream range disagrees with the strict reference")
    end subroutine test_agreement_fill

    !> Integer draws against the strict reference, graded by width regime.
    !!
    !! The grid is deliberate. Widths below 2**63 are the ordinary case. Widths at and above 2**63
    !! are where the naive rejection threshold is wrong for two thirds of widths -- silently, with
    !! every returned value still inside the range and still uniform over the values that do occur,
    !! so neither containment nor chi-square can see it. Ranges placed away from zero are what
    !! exercise a borrow out of the width's low limb: a dropped borrow once survived an entire
    !! sweep because every case had `lo = 0`. And each retry-exercising width carries its own
    !! vacuity guard, because a rejection loop that is never entered passes every other assertion
    !! here.
    !!
    !! There are TWO such widths, one per branch of the rejection threshold, and the second is easy
    !! to leave out: a width can be narrow and still never reject, which is what every ordinary
    !! range in this grid is. See the comment on that sweep for why the threshold's own arithmetic
    !! is otherwise unexecuted.
    subroutine test_agreement_int(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: los(9) = [ &
            0_int64, 1_int64, -100_int64, -6148914691236517205_int64, 0_int64, &
            -huge(1_int64) - 1_int64, -huge(1_int64), 7_int64, -3_int64]
        integer(int64), parameter :: his(9) = [ &
            999999_int64, 6_int64, 100_int64, 6148914691236517205_int64, huge(1_int64), &
            huge(1_int64), huge(1_int64), 7_int64, 4294967296_int64]
        ! Upper ends of the two narrow widths that reject often; the sweep below gives the rule
        ! that picks them. The widths are 7378697629483820646 and 5534023222112865485.
        integer(int64), parameter :: narrow_hi(2) = [7378697629483820645_int64, 5534023222112865484_int64]
        integer :: g, bad
        integer(int64) :: i, d, value, refv, refr, retried

        ! The grid is swept over a DRAW axis as well as a stream axis. Until this was added the
        ! whole integer agreement layer ran at `draw = 1` and nothing else -- every call omitted
        ! the argument -- so the one layer able to catch a miscompiled build never exercised the
        ! integer path's counter arithmetic at any other draw, and the negative draws never
        ! reached its clamp at all. The scalar and fill sweeps had varied the draw all along, which
        ! is exactly what made the gap easy to miss.
        bad = 0
        do g = 1, 9
            do i = 1_int64, 20_int64
                do d = -2_int64, 9_int64
                    value = pf_random_int_at(12345_int64, i, los(g), his(g), d)
                    call ref_int_at(12345_int64, i, los(g), his(g), d, refv, refr)
                    if (value /= refv) bad = bad + 1
                end do
            end do
        end do
        call check(error, bad == 0, &
            "pf_random_int_at disagrees with the strict reference somewhere in the width x stream x draw grid")
        if (allocated(error)) return

        ! A width close to 2**64 * 2/3, where about a third of candidates are rejected, so the
        ! rejection loop and its re-keying are genuinely exercised rather than merely present.
        bad = 0
        retried = 0_int64
        do i = 1_int64, 300_int64
            value = pf_random_int_at(12345_int64, i, -huge(1_int64) - 1_int64, 3172839980678043647_int64)
            call ref_int_at(12345_int64, i, -huge(1_int64) - 1_int64, 3172839980678043647_int64, 1_int64, refv, refr)
            if (value /= refv) bad = bad + 1
            if (refr > 0_int64) retried = retried + 1_int64
        end do
        call check(error, bad == 0, "pf_random_int_at disagrees with the strict reference on a retry-heavy width")
        if (allocated(error)) return
        call check(error, retried > 0_int64, &
            "vacuity guard: no draw in the retry-heavy sweep actually rejected, so the rejection loop was never entered")
        if (allocated(error)) return

        ! The same sweep on the OTHER side of `umod_2p64`'s branch, which is not reachable by
        ! accident. Every width above is either wide -- at or above 2**63, where the threshold is
        ! just 2**64 - s and the function returns before any arithmetic -- or narrow with a
        ! rejection probability around 1e-14 or below. The threshold is computed LAZILY, only for a
        ! candidate landing in the last partial block, so a width that never rejects never computes
        ! one at all. That left the halve-reduce-double reduction -- the part of `umod_2p64` with no
        ! early return to hide behind, and the part most easily got wrong -- executed by nothing.
        !
        ! TWO widths, because the reduction has two arms plus a correction and no single width
        ! reaches all of it. Which arm a width takes is decided by `q = floor(2**64 / s)`: the
        ! doubling FOLDS (`r = r - (s - r)`) when q is even and does not (`r = r + r`) when q is
        ! odd, and the trailing correction runs only when s is odd. Keep both, and use that rule
        ! rather than trial and error if either ever has to be replaced.
        !     0.4 * 2**64, even, q = 2 -- the folding arm, no correction. Rejects one draw in five.
        !     0.3 * 2**64, odd,  q = 3 -- the non-folding arm and the correction. One in ten.
        do g = 1, 2
            bad = 0
            retried = 0_int64
            do i = 1_int64, 300_int64
                value = pf_random_int_at(12345_int64, i, 0_int64, narrow_hi(g))
                call ref_int_at(12345_int64, i, 0_int64, narrow_hi(g), 1_int64, refv, refr)
                if (value /= refv) bad = bad + 1
                if (refr > 0_int64) retried = retried + 1_int64
            end do
            call check(error, bad == 0, &
                "pf_random_int_at disagrees with the strict reference on a narrow width that rejects often")
            if (allocated(error)) return
            call check(error, retried > 0_int64, &
                "vacuity guard: a narrow rejecting width produced no rejection, so umod_2p64's narrow branch never ran")
            if (allocated(error)) return
        end do

        ! A STRICTLY NEGATIVE stream range, as its own loop. This sweep has the opposite half of
        ! the gap `test_agreement_scalar` closes: every stream loop above is non-negative, so until
        ! this existed the integer agreement layer never asked for a negative stream at all -- one
        ! golden row carries stream -1 and nothing else did. The sign matters for the same reason
        ! it does there (feature_risks.md Risk-101): the two one-sided ranges and the range that
        ! spans zero are three different compilations, so covering one says nothing about another.
        bad = 0
        do g = 1, 9
            do i = -20_int64, -1_int64
                value = pf_random_int_at(12345_int64, i, los(g), his(g))
                call ref_int_at(12345_int64, i, los(g), his(g), 1_int64, refv, refr)
                if (value /= refv) bad = bad + 1
            end do
        end do
        call check(error, bad == 0, &
            "pf_random_int_at disagrees with the strict reference over a strictly negative stream range, which " // &
            "every other loop in this test leaves unswept")
        if (allocated(error)) return

        ! The threshold itself, independently: the library's umod_2p64 is not public, so it is
        ! checked through the values it gates, but the reference's own twin is checked here against
        ! the arithmetic identity it must satisfy at the regime boundary.
        call check(error, ref_umod_2p64(-huge(1_int64) - 1_int64) == 0_int64, &
            "2**64 mod 2**63 must be 0 -- the one width the wide-regime branch has to special-case")
        if (allocated(error)) return
        call check(error, ref_umod_2p64(1_int64) == 0_int64, "2**64 mod 1 must be 0")
        if (allocated(error)) return
        call check(error, ref_umod_2p64(3_int64) == 1_int64, "2**64 mod 3 must be 1")
        if (allocated(error)) return
        call check(error, ref_umod_2p64(10_int64) == 6_int64, "2**64 mod 10 must be 6")
    end subroutine test_agreement_int

    !> Seeds that differ by a small offset must not produce agreeing wide-range integers.
    !!
    !! This is what pins the retry's re-key spelling. Both simpler forms alias real seed pairs: an
    !! additive tweak collides one fixed-offset partner on about a third of wide-range draws, and
    !! `mix64(ieor(seed, n))` collides EVERY consecutive (even, odd) pair at up to 5 % -- which is
    !! exactly the seed pattern `seed = base + i` produces, so it would have been met immediately in
    !! real use. The pairs below are the ones those two forms were measured to collide on.
    subroutine test_aliasing(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: lhs(8) = [1_int64, 1_int64, 2_int64, 4_int64, 100_int64, 1000_int64, &
                                               12345_int64, 12345_int64]
        integer(int64), parameter :: rhs(8) = [2_int64, 3_int64, 3_int64, 5_int64, 101_int64, 1001_int64, &
                                               12346_int64, 987654321_int64]
        integer(int64), parameter :: lo = -huge(1_int64) - 1_int64
        integer(int64), parameter :: hi = 3172839980678043647_int64   ! width about 0.672 * 2**64
        integer :: p, agreements
        integer(int64) :: i

        do p = 1, 8
            agreements = 0
            do i = 1_int64, 50000_int64
                if (pf_random_int_at(lhs(p), i, lo, hi) == pf_random_int_at(rhs(p), i, lo, hi)) &
                    agreements = agreements + 1
            end do
            call check(error, agreements == 0, &
                "two seeds a small offset apart agreed on a wide-range draw: the retry must RE-KEY through the tagged " // &
                "mixer, not tweak the key additively and not use mix64(ieor(seed, n))")
            if (allocated(error)) return
        end do
    end subroutine test_aliasing

    ! ============================================================================================
    ! Layer 4 -- statistical sanity
    ! ============================================================================================

    !> Deterministic uniformity checks against precomputed thresholds.
    !!
    !! Fixed seeds and fixed bounds, so these can never be flaky: they either pass every time or
    !! fail every time. They catch a transcription error, nothing more -- the cipher's quality is
    !! established by the literature, and the golden vectors are what actually pin behaviour. The
    !! small-range count is the sharpest of them: a modulo-bias reduction shows up there instantly.
    subroutine test_statistics(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: nbins = 64, nsamp = 64000, nsmall = 80000
        integer :: bins(nbins), counts(8), k, b
        real(real64) :: x, prev, chi, mean, variance, corr, expected, sum_prod

        bins = 0
        mean = 0.0_real64
        variance = 0.0_real64
        corr = 0.0_real64
        sum_prod = 0.0_real64
        prev = pf_random_at(4242_int64, 0_int64)
        do k = 1, nsamp
            x = pf_random_at(4242_int64, int(k, int64))
            b = min(nbins, int(x * real(nbins, real64)) + 1)
            bins(b) = bins(b) + 1
            mean = mean + x
            variance = variance + x * x
            sum_prod = sum_prod + (x - 0.5_real64) * (prev - 0.5_real64)
            prev = x
        end do
        mean = mean / real(nsamp, real64)
        variance = variance / real(nsamp, real64) - mean * mean

        expected = real(nsamp, real64) / real(nbins, real64)
        chi = 0.0_real64
        do b = 1, nbins
            chi = chi + (real(bins(b), real64) - expected)**2 / expected
        end do
        ! 63 degrees of freedom; the 99.9 % critical value is about 112.7.
        call check(error, chi < 112.7_real64, "the [0,1) histogram fails a chi-square test over 64 bins")
        if (allocated(error)) return
        call check(error, abs(mean - 0.5_real64) < 0.01_real64, "the mean of pf_random_at is not near 0.5")
        if (allocated(error)) return
        call check(error, abs(variance - 1.0_real64 / 12.0_real64) < 0.005_real64, &
            "the variance of pf_random_at is not near 1/12")
        if (allocated(error)) return
        corr = (sum_prod / real(nsamp, real64)) * 12.0_real64
        call check(error, abs(corr) < 0.02_real64, "pf_random_at shows a lag-1 serial correlation")
        if (allocated(error)) return

        ! An exhaustive small range: the test that catches a modulo-bias reduction immediately,
        ! because a biased reduction over 1..8 concentrates the low values visibly.
        counts = 0
        do k = 1, nsmall
            ! The stream index shares its kind with the bounds, so an int32 range takes an int32
            ! stream -- one kind per call is the documented rule, not an oversight here. The
            ! conversion is explicit because a bare `k` is only int32 by default: under
            ! `-fdefault-integer-8` it becomes int64, no specific matches, and this suite stops
            ! building against a module that itself compiles fine under that flag.
            b = pf_random_int_at(99_int64, int(k, int32), 1_int32, 8_int32)
            if (b < 1 .or. b > 8) then
                call check(error, .false., "pf_random_int_at returned a value outside 1..8")
                return
            end if
            counts(b) = counts(b) + 1
        end do
        expected = real(nsmall, real64) / 8.0_real64
        chi = 0.0_real64
        do b = 1, 8
            chi = chi + (real(counts(b), real64) - expected)**2 / expected
        end do
        ! 7 degrees of freedom; the 99.9 % critical value is about 24.3.
        call check(error, chi < 24.3_real64, "pf_random_int_at over 1..8 fails a chi-square test")
    end subroutine test_statistics

    !> Uniformity on the axes `test_statistics` does not sweep, plus the wide-integer path.
    !!
    !! `test_statistics` above varies only the STREAM index. That is the module's primary idiom --
    !! `x(i) = pf_random_at(seed, i)` draws one value from each of n consecutive streams -- and so
    !! the most important axis, but it is not the only one a program uses. These four are the rest
    !! of the surface:
    !!
    !!  * the **draw axis**, one stream read repeatedly, which is what every fill walks;
    !!  * the **derived-key axis**, one draw from each of many `pf_random_key` families;
    !!  * the **`real32` sequence**, which enumerates its own words rather than narrowing `real64`;
    !!  * the **low bits of a wide-width integer draw** -- alone among the four in exercising
    !!    `mulhilo64`'s high half and the rejection loop, so it fails for reasons the others cannot.
    !!
    !! Fixed seeds throughout, so every figure here is deterministic: these pass every time or fail
    !! every time, and can never become flaky. The measured statistics are 56.4, 72.3, 79.0 and
    !! 69.1 against the 112.7 threshold, so the tightest margin is about 1.4x -- wide enough to be
    !! stable, narrow enough to notice a real distributional change.
    !!
    !! What this is NOT is a quality battery. The cipher's quality is established by the literature
    !! and its known-answer vectors; what a chi-square adds is the ability to catch a transcription
    !! error that leaves values plausible. No BigCrush-style suite belongs here.
    subroutine test_statistics_axes(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer, parameter :: nbins = 64, nsamp = 64000
        ! 63 degrees of freedom; the 99.9 % critical value is about 112.7, as in test_statistics.
        real(real64), parameter :: critical = 112.7_real64
        integer :: bins(nbins), k, b
        integer(int64) :: v
        real(real32) :: y

        bins = 0
        do k = 1, nsamp
            call tally(bins, pf_random_at(4242_int64, 1_int64, int(k, int64)))
        end do
        call check(error, chisq(bins) < critical, &
            "the draw axis (one stream read repeatedly) fails a chi-square test over 64 bins")
        if (allocated(error)) return

        bins = 0
        do k = 1, nsamp
            call tally(bins, pf_random_at(pf_random_key(4242_int64, int(k, int64)), 1_int64))
        end do
        call check(error, chisq(bins) < critical, &
            "the derived-key axis (one draw from each of 64000 pf_random_key families) fails a chi-square test")
        if (allocated(error)) return

        bins = 0
        do k = 1, nsamp
            y = pf_random32_at(4242_int64, int(k, int64))
            call tally(bins, real(y, real64))
        end do
        call check(error, chisq(bins) < critical, &
            "the real32 sequence fails a chi-square test over 64 bins")
        if (allocated(error)) return

        ! The low bits of a wide-width draw, which is where the rejection loop and mulhilo64's high
        ! half actually run. Binning the low 6 bits rather than the value's magnitude is deliberate:
        ! the reduction maps a uniform 64-bit pattern onto the range through a multiply, so a fault
        ! in the high half shows up in the low bits of the result before it shows up in its scale.
        bins = 0
        do k = 1, nsamp
            v = pf_random_int_at(20260816_int64, int(k, int64), &
                                 -huge(1_int64) - 1_int64, 3172839980678043647_int64)
            b = int(iand(v, 63_int64), int32) + 1
            bins(b) = bins(b) + 1
        end do
        call check(error, chisq(bins) < critical, &
            "the low bits of a wide-width pf_random_int_at draw fail a chi-square test over 64 bins")

    contains

        !> Files one `[0, 1)` value into its bin.
        subroutine tally(b, x)
            integer, intent(inout) :: b(nbins)      !! the bin counts, updated in place
            real(real64), intent(in) :: x           !! a draw in `[0, 1)`
            integer :: idx
            idx = min(nbins, int(x * real(nbins, real64)) + 1)
            b(idx) = b(idx) + 1
        end subroutine tally

        !> Pearson's statistic for `nsamp` samples spread over `nbins` equal bins.
        function chisq(b) result(c)
            integer, intent(in) :: b(nbins)         !! the bin counts
            real(real64) :: c                       !! the chi-square statistic
            real(real64) :: expect
            integer :: j
            expect = real(nsamp, real64) / real(nbins, real64)
            c = 0.0_real64
            do j = 1, nbins
                c = c + (real(b(j), real64) - expect)**2 / expect
            end do
        end function chisq

    end subroutine test_statistics_axes

end module test_random
