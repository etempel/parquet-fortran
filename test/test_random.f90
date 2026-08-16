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
!>     passing on another machine is the cross-machine one.
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
            new_unittest("golden vectors: pf_random_fill_at", test_golden_fill), &
            new_unittest("cross-form: fill agrees with the scalar draws, prefixes are prefixes", test_cross_form), &
            new_unittest("pf_random_at is exactly to_real64(pf_random_bits_at)", test_bits_identity), &
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
            new_unittest("uniformity: chi-square, moments, small-range counts, serial correlation", test_statistics) &
            ]
    end subroutine collect_tests_parquet_random

    ! ============================================================================================
    ! Layer 1 -- the cipher's own known-answer vectors
    ! ============================================================================================

    !> The three published Random123 `philox4x32 10` vectors.
    !!
    !! Asserted against the strict reference rather than the library, because two of the three use
    !! counter words no draw index can produce -- `ctr1` would have to reach 2**32-1, which needs a
    !! block index no `draw` in `integer(int64)` can reach. KAT 1 IS reachable through the public
    !! surface (seed 0, stream 0, block 0 is draws 1 and 2), so it is checked there as well, which
    !! is what ties the reference and the library to the same cipher.
    !!
    !! What this test does NOT prove is more important than what it does: it cannot detect a
    !! miscompiled build. That is `test_agreement_*`'s job.
    subroutine test_kat_vectors(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k
        integer(int64) :: o0, o1, o2, o3, bits1, bits2

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
    end subroutine test_kat_vectors

    ! ============================================================================================
    ! Layer 2 -- the golden vectors
    ! ============================================================================================

    !> Every row of the scalar golden grid, for all three scalar draws.
    !!
    !! Real values are compared as `transfer` bit patterns, which is exact and unambiguous; the
    !! decimal literals beside them are compared too, which is a genuine assertion that the
    !! compiler's decimal conversion agrees, and is the form a human can read.
    subroutine test_golden_scalar(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k
        real(real64) :: got64
        real(real32) :: got32

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
            call pf_random_fill_at(fill_seed(c), fill_stream(c), v64(1:fill_count(c)), fill_start(c))
            call pf_random_fill_at(fill_seed(c), fill_stream(c), v32(1:fill_count(c)), fill_start(c))
            do k = 1, fill_count(c)
                call check(error, transfer(v64(k), 0_int64) == fill_at_bits(fill_first(c) + k - 1), &
                    "pf_random_fill_at (real64) does not match its golden bit pattern")
                if (allocated(error)) return
                call check(error, transfer(v32(k), 0_int32) == fill_at32_bits(fill_first(c) + k - 1), &
                    "pf_random_fill_at (real32) does not match its golden bit pattern")
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
        real(real64) :: one(1), three(3), six(6), tail(3)
        real(real32) :: s_one(1), s_six(6)
        real(real64) :: empty(0)

        call pf_random_fill_at(12345_int64, 1_int64, one)
        call check(error, one(1) == pf_random_at(12345_int64, 1_int64), &
            "a one-element fill is not the scalar draw")
        if (allocated(error)) return

        call pf_random_fill_at(12345_int64, 1_int64, six)
        do k = 1, 6
            call check(error, six(k) == pf_random_at(12345_int64, 1_int64, int(k, int64)), &
                "element k of a fill is not the scalar draw at position k")
            if (allocated(error)) return
        end do

        call pf_random_fill_at(12345_int64, 1_int64, three)
        call check(error, all(three == six(1:3)), "a short fill is not a prefix of a longer one")
        if (allocated(error)) return

        call pf_random_fill_at(12345_int64, 1_int64, tail, 4_int64)
        call check(error, all(tail == six(4:6)), "a fill starting at draw 4 is not the tail of the whole prefix")
        if (allocated(error)) return

        call pf_random_fill_at(12345_int64, 1_int64, s_one)
        call check(error, s_one(1) == pf_random32_at(12345_int64, 1_int64), &
            "a one-element real32 fill is not the scalar real32 draw")
        if (allocated(error)) return

        call pf_random_fill_at(12345_int64, 1_int64, s_six)
        do k = 1, 6
            call check(error, s_six(k) == pf_random32_at(12345_int64, 1_int64, int(k, int64)), &
                "element k of a real32 fill is not the scalar real32 draw at position k")
            if (allocated(error)) return
        end do

        ! A zero-sized fill is a defined no-op, not an error and not an out-of-bounds write.
        call pf_random_fill_at(12345_int64, 1_int64, empty)
        call check(error, size(empty) == 0, "a zero-sized fill must be a defined no-op")
    end subroutine test_cross_form

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

    !> The `int32` specifics must be bit-identical to the `int64` ones at equal argument values.
    subroutine test_kind_specifics(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: mismatches
        integer(int32) :: i32
        integer(int64) :: seed

        mismatches = 0
        seed = 12345_int64
        do i32 = -4_int32, 6_int32
            if (pf_random_bits_at(seed, i32) /= pf_random_bits_at(seed, int(i32, int64))) mismatches = mismatches + 1
            if (pf_random_at(seed, i32) /= pf_random_at(seed, int(i32, int64))) mismatches = mismatches + 1
            if (pf_random32_at(seed, i32) /= pf_random32_at(seed, int(i32, int64))) mismatches = mismatches + 1
            if (int(pf_random_int_at(seed, i32, 1_int32, 1000_int32), int64) /= &
                pf_random_int_at(seed, int(i32, int64), 1_int64, 1000_int64)) mismatches = mismatches + 1
            if (pf_random_key(seed, i32) /= pf_random_key(seed, int(i32, int64))) mismatches = mismatches + 1
        end do
        call check(error, mismatches == 0, &
            "an int32 specific disagrees with its int64 twin -- an int32 stream or label must sign-extend")
    end subroutine test_kind_specifics

    !> An elemental call over an array equals the scalar calls, elementwise, at rank 1 and rank 2.
    subroutine test_elemental(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: k, r, c
        integer(int64) :: idx(6), grid(2, 3)
        real(real64) :: vec(6), mat(2, 3)

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
    end subroutine test_elemental

    !> The totality rules: `lo > hi` swaps, `draw < 1` clamps, degenerate and extreme arguments.
    subroutine test_edges(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64) :: k

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
        integer :: bad_bits, bad_at, bad_at32
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
    end subroutine test_agreement_scalar

    !> Fills against the strict reference, over several lengths and start offsets.
    subroutine test_agreement_fill(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer :: bad64, bad32, m, k
        integer(int64) :: start, i
        real(real64) :: v64(9)
        real(real32) :: v32(9)

        bad64 = 0
        bad32 = 0
        do i = -3_int64, 3_int64
            do m = 1, 9
                do start = 1_int64, 6_int64
                    call pf_random_fill_at(12345_int64, i, v64(1:m), start)
                    call pf_random_fill_at(12345_int64, i, v32(1:m), start)
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
    end subroutine test_agreement_fill

    !> Integer draws against the strict reference, graded by width regime.
    !!
    !! The grid is deliberate. Widths below 2**63 are the ordinary case. Widths at and above 2**63
    !! are where the naive rejection threshold is wrong for two thirds of widths -- silently, with
    !! every returned value still inside the range and still uniform over the values that do occur,
    !! so neither containment nor chi-square can see it. Ranges placed away from zero are what
    !! exercise a borrow out of the width's low limb: a dropped borrow once survived an entire
    !! sweep because every case had `lo = 0`. And the retry-exercising width carries its own
    !! vacuity guard, because a rejection loop that is never entered passes every other assertion
    !! here.
    subroutine test_agreement_int(error)
        type(error_type), allocatable, intent(out) :: error   !! set on the first failed assertion
        integer(int64), parameter :: los(9) = [ &
            0_int64, 1_int64, -100_int64, -6148914691236517205_int64, 0_int64, &
            -huge(1_int64) - 1_int64, -huge(1_int64), 7_int64, -3_int64]
        integer(int64), parameter :: his(9) = [ &
            999999_int64, 6_int64, 100_int64, 6148914691236517205_int64, huge(1_int64), &
            huge(1_int64), huge(1_int64), 7_int64, 4294967296_int64]
        integer :: g, bad
        integer(int64) :: i, value, refv, refr, retried

        bad = 0
        do g = 1, 9
            do i = 1_int64, 60_int64
                value = pf_random_int_at(12345_int64, i, los(g), his(g))
                call ref_int_at(12345_int64, i, los(g), his(g), 1_int64, refv, refr)
                if (value /= refv) bad = bad + 1
            end do
        end do
        call check(error, bad == 0, &
            "pf_random_int_at disagrees with the strict reference somewhere in the width grid")
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
            ! stream -- one kind per call is the documented rule, not an oversight here.
            b = pf_random_int_at(99_int64, k, 1_int32, 8_int32)
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

end module test_random
