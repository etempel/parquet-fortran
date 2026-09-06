!> Tests for `parquet_index`: `pf_index_map`'s three backends and `pf_index_pool`.
!!
!! **The cross-backend oracle is the central pattern here, and it is what defends against this
!! module's worst failure mode.** A hash, probe or tuple-combine defect does not crash and does not
!! corrupt anything -- it answers **0 for a key that is present**, which is indistinguishable from
!! a legitimate miss at every call site. So every functional assertion is made against an
!! INDEPENDENT oracle: a linear scan of the same fixture, written out in `expected_value` below,
!! which shares no code with any backend. `check_lookups_against_oracle` then runs that comparison
!! for each `method=` the fixture's shape allows, so the three backends check each other as well as
!! the scan.
!!
!! Two consequences worth stating, because both are easy to undo:
!!
!! * **Probing the fixture's own keys is not enough.** Every probe set here also contains keys that
!!   are absent -- below the range, above it, and interleaved between stored keys -- because a
!!   backend that answers every stored key correctly and also answers a nonzero value for an absent
!!   one is just as broken, and only the miss probes can see it.
!! * **`method="direct"` and `method="sorted"` are asked for explicitly**, never left to the
!!   automatic choice. A test that lets the heuristic pick is a test of whatever the heuristic did
!!   today; `test_auto_*` asserts the choice itself, and everything else forces the backend it
!!   means to exercise.
!!
!! **Fixture values are row-distinct on purpose.** A fixture whose values repeat, or whose values
!! are all equal, passes against an accessor that returns the wrong element -- the same weakened
!! assertion CLAUDE.md records for `test_table_codegen.f90`. Every fixture value below is unique,
!! so a misaligned answer is a failure rather than a coincidence.
!!
!! Threading tests are NOT here: they live in `test_index_omp.f90`, whose suite is excluded from
!! test-drive's own per-suite parallelism. The reason is not only the nested-team hazard --
!! `parquet_auto_thread_count` answers 1 whenever `omp_get_level() > 0`, so inside test-drive's
!! parallel region an automatic build is always serial and a "threaded equals serial" assertion
!! would hold for the wrong reason.
!!
!! This suite touches no file and no process-global state, so it stays out of the runner's
!! parallelism exclusion list and runs concurrently.
module test_index
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use parquet_index
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private

    public :: collect_tests_index

contains

    !> Registers every test in this suite.
    subroutine collect_tests_index(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("every backend agrees with an independent scan", test_backends_agree), &
            new_unittest("every backend agrees on a negative and wide key set", test_backends_agree_wide), &
            new_unittest("default values number the keys 1..n", test_default_values), &
            new_unittest("explicit values are stored as given", test_explicit_values), &
            new_unittest("int32 and int64 entry points answer alike", test_kind_entry_points), &
            new_unittest("a scalar key and a 1-tuple are the same key", test_scalar_is_one_tuple), &
            new_unittest("contains agrees with get on every backend", test_contains_agrees), &
            new_unittest("get_many matches a loop of get", test_get_many_matches), &
            new_unittest("get_many rejects a length mismatch", test_get_many_length), &
            new_unittest("get_many answers 0 for a masked row and looks up the rest", &
                test_get_many_valid_mask), &
            new_unittest("get_or_add_many equals a loop of get_or_add", test_get_or_add_many_matches), &
            new_unittest("get_or_add_many skips masked rows and adds nothing for them", &
                test_get_or_add_many_valid), &
            new_unittest("build with valid= skips masked rows on every backend", test_build_valid_mask), &
            new_unittest("build with valid= over composite keys and int32 forms", &
                test_build_valid_composite), &
            new_unittest("an unbuilt map answers 0 and reports no method", test_unbuilt_map), &
            new_unittest("a cleared map answers 0 and releases its storage", test_clear_releases), &
            new_unittest("reset empties the map but keeps the allocation", test_reset_keeps_storage), &
            new_unittest("a zero-key build is valid on every backend", test_empty_build), &
            new_unittest("keys() returns every stored key", test_keys_enumeration), &
            new_unittest("the automatic choice takes direct when keys are dense", test_auto_direct), &
            new_unittest("the automatic choice takes hash when keys are sparse", test_auto_hash), &
            new_unittest("the automatic choice never takes sorted", test_auto_never_sorted), &
            new_unittest("a composite product too large for direct falls to hash", test_auto_product_guard), &
            new_unittest("composite keys need every component to identify a row", test_composite_tuple_unique), &
            new_unittest("composite key order matters on every backend", test_composite_order_matters), &
            new_unittest("a composite map reports its component count", test_composite_ncomponents), &
            new_unittest("composite direct and hash agree on the same fixture", test_composite_backends_agree), &
            new_unittest("three components work as well as two", test_composite_three), &
            new_unittest("set inserts, replaces, and raises the watermark", test_set_upsert), &
            new_unittest("set on a fresh map starts a hash map", test_set_autoinit), &
            new_unittest("get_or_add numbers distinct keys densely", test_get_or_add_dense), &
            new_unittest("get_or_add continues past a built map's values", test_get_or_add_after_build), &
            new_unittest("remove forgets one key and keeps the rest", test_remove_keeps_rest), &
            new_unittest("remove reports an absent key through found=", test_remove_found), &
            new_unittest("every key survives many table doublings", test_growth_rehash), &
            new_unittest("reserve avoids the rehash it promises to", test_reserve_no_rehash), &
            new_unittest("removal then reinsertion leaves every key findable", test_remove_reinsert), &
            new_unittest("strided keys do not collapse onto one slot", test_clustering_strided), &
            new_unittest("sequential keys probe barely at all", test_clustering_sequential), &
            new_unittest("composite keys do not collapse either", test_clustering_composite), &
            new_unittest("probe_stats describes the backend it is asked about", test_probe_stats_by_backend), &
            new_unittest("memory_bytes tracks what each backend allocates", test_memory_bytes), &
            new_unittest("a sorted map searches both ends correctly", test_sorted_boundaries), &
            new_unittest("the pool hands out, recycles and counts", test_pool_basics), &
            new_unittest("the pool reuses before it grows", test_pool_reuse_before_growth), &
            new_unittest("is_used answers for every index and none other", test_pool_is_used), &
            new_unittest("used_indexes lists what is held, ascending", test_pool_used_indexes), &
            new_unittest("compact hands out the smallest free index first", test_pool_compact_order), &
            new_unittest("without compact the pool stays LIFO", test_pool_lifo_control), &
            new_unittest("compact keeps every held index and lowers the watermark", test_pool_compact_state), &
            new_unittest("compact gives storage back", test_pool_compact_shrinks), &
            new_unittest("the watermark tightens only at compact", test_pool_watermark_monotone), &
            new_unittest("a cleared pool issues 1 again", test_pool_clear), &
            new_unittest("an empty pool answers every query", test_pool_empty), &
            new_unittest("reserve changes no answer the pool gives", test_pool_reserve) &
            ]
    end subroutine collect_tests_index

    ! ---- The independent oracle ----

    !> The value a lookup must return for `key`, by linear scan of the fixture.
    !!
    !! **Shares no code with any backend**, which is the whole point: it is a `findloc`-style scan
    !! written out in three lines, so a defect in the mixer, the probe loop, the mixed-radix offset
    !! or the binary search cannot hide behind it. Returns 0 for an absent key, exactly as `%get`
    !! must.
    pure function expected_value(keys, values, key) result(v)
        integer(int64), intent(in) :: keys(:)   !! the fixture's keys.
        integer(int64), intent(in) :: values(:) !! the fixture's values, one per key.
        integer(int64), intent(in) :: key       !! the key being looked up.
        integer(int64) :: v                     !! its value, or 0 when it is not in the fixture.
        integer(int64) :: i

        v = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (keys(i) == key) then
                v = values(i)
                return
            end if
        end do
    end function expected_value

    !> The value a composite lookup must return for `key`, by linear scan.
    pure function expected_value_n(keys, values, key) result(v)
        integer(int64), intent(in) :: keys(:,:) !! the fixture's key tuples, one per row.
        integer(int64), intent(in) :: values(:) !! the fixture's values, one per row.
        integer(int64), intent(in) :: key(:)    !! the tuple being looked up.
        integer(int64) :: v                     !! its value, or 0 when it is not in the fixture.
        integer(int64) :: i

        v = 0_int64
        do i = 1_int64, size(keys, 1, kind=int64)
            if (all(keys(i, :) == key)) then
                v = values(i)
                return
            end if
        end do
    end function expected_value_n

    !> Builds the fixture with `method` and checks every probe against the scan oracle.
    subroutine check_lookups_against_oracle(error, keys, values, probes, method)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer(int64), intent(in) :: keys(:)               !! the fixture's keys.
        integer(int64), intent(in) :: values(:)             !! the fixture's values.
        integer(int64), intent(in) :: probes(:)             !! keys to look up; hits and misses.
        character(len=*), intent(in) :: method              !! the backend to force.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: i, want, have

        call m%build(keys, values, method=method)
        call m%get_method(got)
        call check(error, got == method, "method=""" // method // """ resolved to """ // got // """")
        if (allocated(error)) return
        call check(error, m%nkeys() == size(keys, kind=int64), &
            "method=""" // method // """: nkeys does not match the fixture")
        if (allocated(error)) return
        do i = 1_int64, size(probes, kind=int64)
            want = expected_value(keys, values, probes(i))
            have = m%get(probes(i))
            call check(error, have == want, "method=""" // method // &
                """: a lookup disagreed with an independent scan of the fixture")
            if (allocated(error)) return
        end do
    end subroutine check_lookups_against_oracle

    ! ---- Cross-backend agreement ----

    !> Every backend answers what a linear scan answers, hits and misses alike.
    subroutine test_backends_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer(int64) :: keys(7), values(7), probes(13)

        keys = [10_int64, 12_int64, 14_int64, 16_int64, 18_int64, 20_int64, 22_int64]
        values = [3_int64, 8_int64, 1_int64, 6_int64, 9_int64, 2_int64, 7_int64]
        ! Hits, the gaps between them, and keys outside the range at both ends.
        probes = [10_int64, 11_int64, 12_int64, 13_int64, 14_int64, 22_int64, 23_int64, &
            9_int64, 0_int64, -5_int64, 100_int64, 21_int64, 19_int64]
        call check_lookups_against_oracle(error, keys, values, probes, "direct")
        if (allocated(error)) return
        call check_lookups_against_oracle(error, keys, values, probes, "hash")
        if (allocated(error)) return
        call check_lookups_against_oracle(error, keys, values, probes, "sorted")
    end subroutine test_backends_agree

    !> The same agreement over negative keys and a range too wide for the direct backend.
    !!
    !! Negative keys matter for their own reason: the direct backend's offset is
    !! `key - kmin + 1`, so a fixture that never goes below zero cannot tell a correct offset from
    !! one that forgot the subtraction.
    subroutine test_backends_agree_wide(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer(int64) :: keys(5), values(5), probes(11)

        keys = [-1000000_int64, -3_int64, 0_int64, 7_int64, 999999999_int64]
        values = [5_int64, 1_int64, 4_int64, 2_int64, 3_int64]
        probes = [-1000000_int64, -999999_int64, -3_int64, -2_int64, 0_int64, 1_int64, &
            7_int64, 999999999_int64, 1000000000_int64, -1000001_int64, 500_int64]
        ! Too wide for `direct` at this key count, so only the two backends that can hold it.
        call check_lookups_against_oracle(error, keys, values, probes, "hash")
        if (allocated(error)) return
        call check_lookups_against_oracle(error, keys, values, probes, "sorted")
    end subroutine test_backends_agree_wide

    !> With no `values=`, each key's value is its position in the key array.
    subroutine test_default_values(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(4), i
        character(len=6) :: methods(3)
        integer :: k

        keys = [40_int64, 10_int64, 30_int64, 20_int64]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            do i = 1_int64, 4_int64
                call check(error, m%get(keys(i)) == i, "default values must number the keys " // &
                    "1..n in the order given, on backend " // trim(methods(k)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_default_values

    !> `values=` is stored verbatim, and need not be a permutation of anything.
    subroutine test_explicit_values(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(3), values(3)

        keys = [5_int64, 6_int64, 7_int64]
        ! Deliberately not a permutation of 1..n, and with a repeat: only KEYS must be unique.
        values = [100_int64, 100_int64, 3_int64]
        call m%build(keys, values, method="direct")
        call check(error, m%get(5_int64) == 100_int64, "explicit value for key 5")
        if (allocated(error)) return
        call check(error, m%get(6_int64) == 100_int64, "a repeated VALUE is allowed")
        if (allocated(error)) return
        call check(error, m%get(7_int64) == 3_int64, "explicit value for key 7")
    end subroutine test_explicit_values

    !> The `int32` and `int64` spellings of every entry point answer identically.
    subroutine test_kind_entry_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int32) :: k32(4), v32(4), out32(4)
        integer(int64) :: k64(4), out64(4)

        k32 = [3_int32, 9_int32, 27_int32, 81_int32]
        v32 = [4_int32, 3_int32, 2_int32, 1_int32]
        k64 = int(k32, int64)
        call m%build(k32, v32, method="hash")
        call check(error, m%get(9_int32) == 3_int64, "int32 key, int32 values: get(9)")
        if (allocated(error)) return
        call check(error, m%get(9_int64) == 3_int64, "int64 key finds an int32-built key")
        if (allocated(error)) return
        call m%get_many(k32, out32)
        call m%get_many(k64, out64)
        call check(error, all(int(out32, int64) == out64), &
            "get_many must answer alike whatever the key and index kinds")
        if (allocated(error)) return
        call m%build(k64, method="hash")
        call check(error, m%get(81_int32) == 4_int64, "int32 key finds an int64-built key")
        if (allocated(error)) return
        call m%build(k64, int(v32, int64), method="direct")
        call check(error, m%get(27_int32) == 2_int64, "int64 keys with int64 values, int32 lookup")
    end subroutine test_kind_entry_points

    !> A single-component map answers `%get(k)` and `%get([k])` identically, on every backend.
    !!
    !! Not a curiosity: the design makes the scalar forms sugar for a 1-tuple, so the two spellings
    !! must reach the same slot. They take different code paths to get there -- one through the
    !! scalar range test or the scalar hash, the other through the mixed-radix offset or the tuple
    !! chain -- and only this asserts that those paths agree.
    subroutine test_scalar_is_one_tuple(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(5), i
        character(len=6) :: methods(3)
        integer :: k

        keys = [2_int64, 4_int64, 8_int64, 16_int64, 32_int64]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            do i = 1_int64, 5_int64
                call check(error, m%get([keys(i)]) == m%get(keys(i)), &
                    "the 1-tuple and scalar spellings must agree on backend " // trim(methods(k)))
                if (allocated(error)) return
            end do
            call check(error, m%get([99_int64]) == 0_int64, &
                "a 1-tuple miss answers 0 on backend " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%ncomponents() == 1, &
                "a map built from rank-1 keys has one component, on " // trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_scalar_is_one_tuple

    !> `%contains` is exactly `%get(...) > 0`, for hits and misses, on every backend.
    subroutine test_contains_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(4), probes(7), i
        character(len=6) :: methods(3)
        integer :: k

        keys = [1_int64, 5_int64, 9_int64, 13_int64]
        probes = [1_int64, 2_int64, 5_int64, 9_int64, 13_int64, 14_int64, -1_int64]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            do i = 1_int64, 7_int64
                call check(error, m%contains(probes(i)) .eqv. (m%get(probes(i)) > 0_int64), &
                    "contains must agree with get on backend " // trim(methods(k)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_contains_agrees

    !> The bulk form answers exactly what a loop of the scalar form answers.
    subroutine test_get_many_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(6), probes(9), bulk(9), one(9), i
        character(len=6) :: methods(3)
        integer :: k

        keys = [100_int64, 200_int64, 300_int64, 400_int64, 500_int64, 600_int64]
        probes = [100_int64, 150_int64, 200_int64, 600_int64, 700_int64, 0_int64, &
            300_int64, 400_int64, 500_int64]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            call m%get_many(probes, bulk)
            do i = 1_int64, 9_int64
                one(i) = m%get(probes(i))
            end do
            call check(error, all(bulk == one), &
                "get_many must equal a loop of get on backend " // trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_get_many_matches

    !> A `get_many` whose answer array is the wrong length is refused rather than truncated.
    !!
    !! Asserted through the OUT-OF-PROCESS scenario, because the refusal is an `error stop`; this
    !! test covers the accepted case, which is the negative control that stops the guard from
    !! passing by firing always.
    subroutine test_get_many_length(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(3), out(3)

        keys = [1_int64, 2_int64, 3_int64]
        call m%build(keys)
        call m%get_many(keys, out)
        call check(error, all(out == [1_int64, 2_int64, 3_int64]), &
            "a correctly sized get_many must be accepted")
    end subroutine test_get_many_length

    !> A masked row answers 0 whether or not its key is present, and every other row answers
    !! exactly what `%get` answers, on every backend and for both answer kinds.
    !!
    !! The discriminating probe is a PRESENT key under a `.false.` mask entry: a `%get_many` that
    !! ignored the mask would answer its stored value there, so the test asserts that row is 0
    !! while `%get` of the same key is not.
    subroutine test_get_many_valid_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(6), probes(9), bulk(9), want(9), pairs(6, 2), pprobe(9, 2), i
        integer(int32) :: bulk32(9)
        logical :: mask(9)
        character(len=6) :: methods(3)
        integer :: k

        keys = [100_int64, 200_int64, 300_int64, 400_int64, 500_int64, 600_int64]
        probes = [100_int64, 150_int64, 200_int64, 600_int64, 700_int64, 0_int64, &
            300_int64, 400_int64, 500_int64]
        mask = [.true., .false., .true., .true., .false., .true., .false., .true., .true.]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            do i = 1_int64, 9_int64
                if (mask(i)) then
                    want(i) = m%get(probes(i))
                else
                    want(i) = 0_int64
                end if
            end do
            call m%get_many(probes, bulk, valid=mask)
            call check(error, all(bulk == want), &
                "a masked get_many must answer 0 for masked rows and %get for the rest on " // &
                trim(methods(k)))
            if (allocated(error)) return
            call check(error, bulk(7) == 0_int64 .and. m%get(probes(7)) == 3_int64, &
                "row 7 is a PRESENT key under a .false. mask entry: it must answer 0 while %get " // &
                "still finds it, or the mask was ignored (" // trim(methods(k)) // ")")
            if (allocated(error)) return
            call m%get_many(probes, bulk32, valid=mask)
            call check(error, all(int(bulk32, int64) == want), &
                "the int32 answer form must honour the mask too on " // trim(methods(k)))
            if (allocated(error)) return
        end do
        ! The composite form: the same mask over key tuples, on the two backends that take them.
        pairs(:, 1) = keys
        pairs(:, 2) = [1_int64, 2_int64, 1_int64, 2_int64, 1_int64, 2_int64]
        pprobe(:, 1) = probes
        pprobe(:, 2) = [1_int64, 1_int64, 2_int64, 2_int64, 1_int64, 1_int64, 1_int64, 2_int64, 1_int64]
        do k = 1, 2
            call m%build(pairs, method=trim(methods(k)))
            do i = 1_int64, 9_int64
                if (mask(i)) then
                    want(i) = m%get(pprobe(i, :))
                else
                    want(i) = 0_int64
                end if
            end do
            call m%get_many(pprobe, bulk, valid=mask)
            call check(error, all(bulk == want), &
                "a masked composite get_many must equal masked %get on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, bulk(7) == 0_int64 .and. m%get(pprobe(7, :)) == 3_int64, &
                "row 7 of the composite probe is present and masked, so it must answer 0 while " // &
                "%get finds it (" // trim(methods(k)) // ")")
            if (allocated(error)) return
        end do
    end subroutine test_get_many_valid_mask

    !> `%get_or_add_many` numbers exactly as a loop of `%get_or_add` does, for both key kinds,
    !! both code kinds and both key ranks, and continues above a built map's values.
    subroutine test_get_or_add_many_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: bulk_map, loop_map
        integer(int64) :: stream(9), codes(9), one(9), idx, i, pairs(7, 2), pc(7), po(7)
        integer(int32) :: codes32(9)
        integer(int64), allocatable :: none(:), nocode(:)

        ! A stream with repeats: three distinct keys, so codes 1..3 in first-appearance order.
        stream = [500_int64, 700_int64, 500_int64, 900_int64, 700_int64, 500_int64, &
            900_int64, 900_int64, 700_int64]
        call loop_map%init()
        do i = 1_int64, 9_int64
            call loop_map%get_or_add(stream(i), idx)
            one(i) = idx
        end do
        call bulk_map%init()
        call bulk_map%get_or_add_many(stream, codes)
        call check(error, all(codes == one), "get_or_add_many must equal a loop of get_or_add")
        if (allocated(error)) return
        call check(error, bulk_map%nkeys() == 3_int64, "three distinct keys were added")
        if (allocated(error)) return
        call check(error, all(codes >= 1_int64 .and. codes <= 3_int64), &
            "every code is within 1..k -- that is what dense means")
        if (allocated(error)) return
        ! int32 keys and int32 codes, on a fresh map.
        call bulk_map%clear()
        call bulk_map%get_or_add_many(int(stream, int32), codes32)
        call check(error, all(int(codes32, int64) == one), &
            "the int32 key and int32 code form must number identically")
        if (allocated(error)) return
        ! A second call sees the keys the first added, and continues the numbering above them.
        call bulk_map%get_or_add_many([700_int64, 1100_int64, 500_int64], codes(1:3))
        call check(error, all(codes(1:3) == [2_int64, 4_int64, 1_int64]), &
            "a later call must answer the codes already assigned and continue above them")
        if (allocated(error)) return
        ! Above a built map's values, as %get_or_add does.
        call bulk_map%build([1_int64, 2_int64, 3_int64], [10_int64, 20_int64, 30_int64], method="hash")
        call bulk_map%get_or_add_many([2_int64, 4_int64, 5_int64, 4_int64], codes(1:4))
        call check(error, all(codes(1:4) == [20_int64, 31_int64, 32_int64, 31_int64]), &
            "on a built map the new keys continue above the largest stored value")
        if (allocated(error)) return
        ! An empty call is legal and changes nothing.
        allocate(none(0), nocode(0))
        call bulk_map%get_or_add_many(none, nocode)
        call check(error, bulk_map%nkeys() == 5_int64, "an empty get_or_add_many adds nothing")
        if (allocated(error)) return
        ! Composite keys: tuples with repeats, against a loop of the tuple get_or_add.
        pairs(:, 1) = [1_int64, 2_int64, 1_int64, 3_int64, 2_int64, 1_int64, 3_int64]
        pairs(:, 2) = [7_int64, 7_int64, 7_int64, 8_int64, 7_int64, 8_int64, 8_int64]
        call loop_map%clear()
        call loop_map%init(ncomp=2)
        do i = 1_int64, 7_int64
            call loop_map%get_or_add(pairs(i, :), idx)
            po(i) = idx
        end do
        call bulk_map%clear()
        call bulk_map%get_or_add_many(pairs, pc)
        call check(error, all(pc == po), &
            "the composite get_or_add_many must equal a loop of the tuple get_or_add")
        if (allocated(error)) return
        call check(error, bulk_map%nkeys() == 4_int64 .and. bulk_map%ncomponents() == 2, &
            "four distinct pairs were added to a two-component map")
    end subroutine test_get_or_add_many_matches

    !> A masked row of `%get_or_add_many` gets the code 0 and adds no key.
    !!
    !! The fixture's key 900 appears ONLY on masked rows, so a bulk form that ignored the mask
    !! would add it: `%contains(900)` is the discriminating assertion, and the vacuity guard is
    !! that the fixture really does mask every 900 and nothing else.
    subroutine test_get_or_add_many_valid(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m, ref
        integer(int64) :: stream(9), codes(9), want(9), idx, i
        logical :: mask(9)

        stream = [500_int64, 700_int64, 500_int64, 900_int64, 700_int64, 500_int64, &
            900_int64, 900_int64, 700_int64]
        mask = [.true., .true., .true., .false., .true., .true., .false., .false., .true.]
        call check(error, all(mask .neqv. (stream == 900_int64)), &
            "fixture: the mask must hide exactly the rows holding 900, or the assertions below " // &
            "cannot tell a masked row from an absent key")
        if (allocated(error)) return
        call ref%init()
        do i = 1_int64, 9_int64
            if (mask(i)) then
                call ref%get_or_add(stream(i), idx)
                want(i) = idx
            else
                want(i) = 0_int64
            end if
        end do
        call m%init()
        call m%get_or_add_many(stream, codes, valid=mask)
        call check(error, all(codes == want), &
            "a masked get_or_add_many must code the unmasked rows as the masked loop does and " // &
            "the masked rows as 0")
        if (allocated(error)) return
        call check(error, m%nkeys() == 2_int64, "only the two unmasked distinct keys were added")
        if (allocated(error)) return
        call check(error, .not. m%contains(900_int64), &
            "a key that appears only on masked rows must not be added")
        if (allocated(error)) return
        call check(error, codes(4) == 0_int64 .and. codes(7) == 0_int64 .and. codes(8) == 0_int64, &
            "every masked row must answer 0")
    end subroutine test_get_or_add_many_valid

    !> `%build` with `valid=` skips masked rows: they are neither stored nor counted, their keys
    !! may repeat a stored one, and the values stay the ORIGINAL row numbers.
    !!
    !! The fixture masks a duplicate of a stored key (row 3 repeats row 1's key 5) and one key
    !! that appears nowhere else (row 5, key 11): a build that ignored the mask would abort on the
    !! duplicate, and one that compacted the rows first would answer wrong row numbers, so both
    !! failure modes are discriminated on every backend.
    subroutine test_build_valid_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(6), idx, i
        integer(int64), allocatable :: list(:), big(:)
        logical :: mask(6), none(6)
        logical, allocatable :: bigmask(:)
        character(len=6) :: methods(3)
        character(len=:), allocatable :: method
        integer :: k

        keys = [5_int64, 7_int64, 5_int64, 9_int64, 11_int64, 13_int64]
        mask = [.true., .true., .false., .true., .false., .true.]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)), valid=mask)
            call check(error, m%nkeys() == 4_int64, &
                "four rows are unmasked, so four keys are stored on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get(5_int64) == 1_int64 .and. m%get(7_int64) == 2_int64 .and. &
                m%get(9_int64) == 4_int64 .and. m%get(13_int64) == 6_int64, &
                "the stored values must be the ORIGINAL row numbers, not compacted ones, on " // &
                trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get(11_int64) == 0_int64, &
                "a key that appears only on a masked row must be absent on " // trim(methods(k)))
            if (allocated(error)) return
            call m%get_method(method)
            call check(error, method == trim(methods(k)), &
                "the masked build must land on the backend that was asked for")
            if (allocated(error)) return
            call m%keys(list)
            call check(error, size(list) == 4, "keys() lists only the stored keys after a masked build")
            if (allocated(error)) return
            call check(error, count(list == 11_int64) == 0 .and. count(list == 5_int64) == 1, &
                "keys() must hold each stored key once and no masked-only key")
            if (allocated(error)) return
        end do
        ! Explicit values under the mask: a masked row's value is neither stored nor checked, so
        ! it may even be 0, which an unmasked row's value may not be.
        call m%build(keys, [10_int64, 20_int64, 0_int64, 40_int64, 0_int64, 60_int64], &
            method="hash", valid=mask)
        call check(error, m%get(9_int64) == 40_int64 .and. m%get(11_int64) == 0_int64, &
            "explicit values are stored for unmasked rows only")
        if (allocated(error)) return
        ! %get_or_add continues above the last STORED row number, not above the array length.
        call m%build(keys, method="hash", valid=[.true., .true., .false., .true., .false., .false.])
        call m%get_or_add(11_int64, idx)
        call check(error, idx == 5_int64, &
            "after a masked build the next automatic index is one above the last stored row")
        if (allocated(error)) return
        ! A mask that keeps nothing builds an empty map on the requested backend.
        none = .false.
        call m%build(keys, method="hash", valid=none)
        call m%get_method(method)
        call check(error, m%nkeys() == 0_int64 .and. method == "hash" .and. m%get(5_int64) == 0_int64, &
            "an all-false mask builds an empty map that still reports its backend")
        if (allocated(error)) return
        ! The automatic choice sizes its direct budget by the STORED rows: 30000 presented rows
        ! would buy a 120000-slot direct array, but the two stored keys span 100000 positions
        ! against the 65536-slot floor two keys are entitled to, so this must be a hash map.
        allocate(big(30000), bigmask(30000))
        do i = 1_int64, 30000_int64
            big(i) = i
        end do
        big(2) = 100000_int64
        bigmask = .false.
        bigmask(1:2) = .true.
        call m%build(big, valid=bigmask)
        call m%get_method(method)
        call check(error, method == "hash" .and. m%nkeys() == 2_int64, &
            "the automatic backend choice must budget by the stored rows, not the presented ones")
        if (allocated(error)) return
        call check(error, m%get(100000_int64) == 2_int64 .and. m%get(3_int64) == 0_int64, &
            "and the masked build still answers the two stored keys and nothing else")
    end subroutine test_build_valid_mask

    !> The composite form of the masked build, on both backends that take tuples, and the int32
    !! forwarders, which must pass the mask through as the int64 ones do.
    subroutine test_build_valid_composite(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: pairs(5, 2)
        logical :: mask(5)
        character(len=6) :: methods(2)
        integer :: k

        pairs(:, 1) = [1_int64, 2_int64, 1_int64, 3_int64, 4_int64]
        pairs(:, 2) = [7_int64, 7_int64, 7_int64, 8_int64, 9_int64]
        ! Row 3 repeats row 1's tuple; row 5 is the only (4, 9). Both are masked.
        mask = [.true., .true., .false., .true., .false.]
        methods = ["direct", "hash  "]
        do k = 1, 2
            call m%build(pairs, method=trim(methods(k)), valid=mask)
            call check(error, m%nkeys() == 3_int64, &
                "three unmasked tuples are stored on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get([1_int64, 7_int64]) == 1_int64 .and. &
                m%get([2_int64, 7_int64]) == 2_int64 .and. m%get([3_int64, 8_int64]) == 4_int64, &
                "stored values are the original row numbers on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get([4_int64, 9_int64]) == 0_int64, &
                "a tuple that appears only on a masked row is absent on " // trim(methods(k)))
            if (allocated(error)) return
        end do
        ! Explicit values, int32 keys, with the mask: the int32 forwarders pass it through too.
        call m%build(int(pairs, int32), [10_int32, 20_int32, 30_int32, 40_int32, 50_int32], &
            method="hash", valid=mask)
        call check(error, m%get([3_int32, 8_int32]) == 40_int64 .and. m%get([4_int32, 9_int32]) == 0_int64, &
            "the int32 composite build honours the mask and stores the given values")
        if (allocated(error)) return
        call m%build(int(pairs(:, 1), int32), method="sorted", valid=mask)
        call check(error, m%nkeys() == 3_int64 .and. m%get(3_int32) == 4_int64 .and. m%get(4_int32) == 0_int64, &
            "the int32 rank-1 build honours the mask on the sorted backend")
    end subroutine test_build_valid_composite

    ! ---- Lifecycle ----

    !> A map that was never built answers 0 for everything and reports no backend.
    subroutine test_unbuilt_map(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: out(2)

        call check(error, m%get(0_int64) == 0_int64, "an unbuilt map answers 0 for key 0")
        if (allocated(error)) return
        call check(error, m%get(12345_int64) == 0_int64, "an unbuilt map answers 0")
        if (allocated(error)) return
        call check(error, .not. m%contains(1_int64), "an unbuilt map contains nothing")
        if (allocated(error)) return
        call check(error, m%nkeys() == 0_int64, "an unbuilt map holds no keys")
        if (allocated(error)) return
        call check(error, m%ncomponents() == 0, "an unbuilt map reports 0 components")
        if (allocated(error)) return
        call m%get_method(got)
        call check(error, len(got) == 0, "an unbuilt map reports no method, not ""direct""")
        if (allocated(error)) return
        call check(error, m%memory_bytes() == 0_int64, "an unbuilt map holds no memory")
        if (allocated(error)) return
        call m%get_many([1_int64, 2_int64], out)
        call check(error, all(out == 0_int64), "a bulk lookup on an unbuilt map answers all zeros")
    end subroutine test_unbuilt_map

    !> `%clear` releases storage and restores the as-new answers.
    subroutine test_clear_releases(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: keys(4)
        integer :: k
        character(len=6) :: methods(3)

        keys = [3_int64, 4_int64, 5_int64, 6_int64]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            call check(error, m%memory_bytes() > 0_int64, &
                "a built map holds memory on backend " // trim(methods(k)))
            if (allocated(error)) return
            call m%clear()
            call check(error, m%get(4_int64) == 0_int64, &
                "a cleared map answers 0, from backend " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%nkeys() == 0_int64, "a cleared map holds no keys")
            if (allocated(error)) return
            call check(error, m%memory_bytes() == 0_int64, &
                "clear must RELEASE, matching parquet_column%clear")
            if (allocated(error)) return
            call m%get_method(got)
            call check(error, len(got) == 0, "a cleared map reports no method")
            if (allocated(error)) return
        end do
    end subroutine test_clear_releases

    !> `%reset` empties the map without giving the allocation back.
    subroutine test_reset_keeps_storage(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: keys(4), before

        keys = [3_int64, 4_int64, 5_int64, 6_int64]
        call m%build(keys, method="hash")
        before = m%memory_bytes()
        call m%reset()
        call check(error, m%get(4_int64) == 0_int64, "a reset map answers 0")
        if (allocated(error)) return
        call check(error, m%nkeys() == 0_int64, "a reset map holds no keys")
        if (allocated(error)) return
        call check(error, m%memory_bytes() == before, &
            "reset must KEEP the allocation -- that is what distinguishes it from clear")
        if (allocated(error)) return
        call m%get_method(got)
        call check(error, got == "hash", "a reset map keeps its backend")
        if (allocated(error)) return
        ! And it is usable again afterwards.
        call m%set(4_int64, 9_int64)
        call check(error, m%get(4_int64) == 9_int64, "a reset map accepts new keys")
    end subroutine test_reset_keeps_storage

    !> Building from no keys at all is valid, and reports the backend it was asked for.
    subroutine test_empty_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: empty(0), pairs(0, 2)
        integer(int64), allocatable :: klist(:)
        character(len=:), allocatable :: got
        character(len=6) :: methods(3)
        integer :: k

        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(empty, method=trim(methods(k)))
            call check(error, m%nkeys() == 0_int64, &
                "a zero-key build holds no keys, on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get(1_int64) == 0_int64, &
                "a zero-key map answers 0, on " // trim(methods(k)))
            if (allocated(error)) return
            call m%get_method(got)
            call check(error, got == trim(methods(k)), &
                "a zero-key build still reports its resolved backend")
            if (allocated(error)) return
            call m%keys(klist)
            call check(error, allocated(klist), "keys() on an empty map allocates rather than not")
            if (allocated(error)) return
            call check(error, size(klist) == 0, "keys() on an empty map is zero-length")
            if (allocated(error)) return
        end do
        ! And the composite shape, which reaches a different build worker.
        call m%build(pairs)
        call check(error, m%ncomponents() == 2, "a zero-row composite build keeps its width")
        if (allocated(error)) return
        call check(error, m%get([1_int64, 1_int64]) == 0_int64, "an empty composite map answers 0")
    end subroutine test_empty_build

    !> `%keys` returns every stored key, and nothing else.
    subroutine test_keys_enumeration(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(5), i, j
        integer(int64), allocatable :: klist(:)
        logical :: seen(5)
        character(len=6) :: methods(3)
        integer :: k

        keys = [11_int64, 13_int64, 17_int64, 19_int64, 23_int64]
        methods = ["direct", "hash  ", "sorted"]
        do k = 1, 3
            call m%build(keys, method=trim(methods(k)))
            call m%keys(klist)
            call check(error, size(klist, kind=int64) == 5_int64, &
                "keys() returns one entry per stored key, on " // trim(methods(k)))
            if (allocated(error)) return
            ! Set-equality rather than order: the hash backend's order is unspecified.
            seen = .false.
            do i = 1_int64, 5_int64
                do j = 1_int64, 5_int64
                    if (klist(i) == keys(j)) seen(j) = .true.
                end do
            end do
            call check(error, all(seen), &
                "keys() must return exactly the stored keys, on " // trim(methods(k)))
            if (allocated(error)) return
        end do
        ! Direct and sorted promise ascending order; assert it where it is promised.
        call m%build(keys, method="direct")
        call m%keys(klist)
        call check(error, all(klist == keys), "the direct backend enumerates keys ascending")
        if (allocated(error)) return
        call m%build(keys, method="sorted")
        call m%keys(klist)
        call check(error, all(klist == keys), "the sorted backend enumerates keys ascending")
    end subroutine test_keys_enumeration

    ! ---- The automatic backend choice ----

    !> Dense keys take the direct backend.
    subroutine test_auto_direct(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: keys(100), i

        do i = 1_int64, 100_int64
            keys(i) = 1000_int64 + i
        end do
        call m%build(keys)
        call m%get_method(got)
        call check(error, got == "direct", &
            "100 consecutive keys must take the direct backend, not """ // got // """")
    end subroutine test_auto_direct

    !> Keys spread over a wide range take the hash backend.
    !!
    !! **The assertion is on the method TOKEN, not on the absence of a huge allocation**, which is
    !! deliberate: choosing direct for `{1, 10**9}` would ask for 8 GB, and a test that only
    !! noticed by running out of memory would take the machine down with it rather than failing.
    subroutine test_auto_hash(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got

        call m%build([1_int64, 1000000000_int64])
        call m%get_method(got)
        call check(error, got == "hash", &
            "two keys a billion apart must take the hash backend, not """ // got // """")
        if (allocated(error)) return
        call check(error, m%get(1_int64) == 1_int64, "and must still answer correctly")
        if (allocated(error)) return
        call check(error, m%get(1000000000_int64) == 2_int64, "and for the far key too")
        if (allocated(error)) return
        call check(error, m%memory_bytes() < 100000_int64, &
            "a hash map for two keys must be small -- if this grew, the heuristic chose direct")
    end subroutine test_auto_hash

    !> The automatic choice never selects the sorted backend: it must be asked for.
    subroutine test_auto_never_sorted(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: keys(50), i

        ! A fixture that would suit sorted well -- already ascending, wide range, few keys.
        do i = 1_int64, 50_int64
            keys(i) = i * 10000000_int64
        end do
        call m%build(keys)
        call m%get_method(got)
        call check(error, got /= "sorted", "the automatic choice must never select sorted")
        if (allocated(error)) return
        call m%build(keys, method="sorted")
        call m%get_method(got)
        call check(error, got == "sorted", "and sorted must be reachable when asked for")
    end subroutine test_auto_never_sorted

    !> A composite key whose components are each small but whose product is not takes hash.
    !!
    !! This is the division-guarded product in `ix_product_ok`. The two single-component control
    !! builds are what make the test specific: each component alone is dense enough for direct, so
    !! a failure here is the product guard and not the per-component span test.
    subroutine test_auto_product_guard(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: pairs(4, 2), col(4)

        ! Each component spans 301 positions; the product is 90601, above the 65536 floor.
        pairs(:, 1) = [1_int64, 100_int64, 200_int64, 301_int64]
        pairs(:, 2) = [1_int64, 150_int64, 250_int64, 301_int64]
        call m%build(pairs)
        call m%get_method(got)
        call check(error, got == "hash", &
            "a composite whose component product exceeds the budget must take hash, not """ // &
            got // """")
        if (allocated(error)) return
        call check(error, m%get([200_int64, 250_int64]) == 3_int64, "and must answer correctly")
        if (allocated(error)) return
        ! The controls: either component ALONE is dense enough for direct.
        col = pairs(:, 1)
        call m%build(col)
        call m%get_method(got)
        call check(error, got == "direct", &
            "control: component 1 alone is dense enough for direct, so the product guard is " // &
            "what sent the composite to hash")
        if (allocated(error)) return
        col = pairs(:, 2)
        call m%build(col)
        call m%get_method(got)
        call check(error, got == "direct", "control: component 2 alone is dense enough too")
    end subroutine test_auto_product_guard

    ! ---- Composite keys ----

    !> The stated use case: neither column is unique, and only the tuple identifies a row.
    !!
    !! **The fixture is shaped to the point of the feature.** Every value of component 1 repeats
    !! and every value of component 2 repeats, so a backend that ignored the second component
    !! entirely would answer some lookups correctly -- and the assertions that separate the rows
    !! sharing a first component are what catch it. A fixture whose first column happened to be
    !! unique would pass against exactly that defect.
    subroutine test_composite_tuple_unique(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: pairs(6, 2), i
        character(len=6) :: methods(2)
        integer :: k

        ! object_id x band: ids 7,8,9 each appear twice; bands 1,2 each appear three times.
        pairs(:, 1) = [7_int64, 7_int64, 8_int64, 8_int64, 9_int64, 9_int64]
        pairs(:, 2) = [1_int64, 2_int64, 1_int64, 2_int64, 1_int64, 2_int64]
        methods = ["direct", "hash  "]
        do k = 1, 2
            call m%build(pairs, method=trim(methods(k)))
            do i = 1_int64, 6_int64
                call check(error, m%get(pairs(i, :)) == i, &
                    "each tuple must find its own row on backend " // trim(methods(k)))
                if (allocated(error)) return
            end do
            ! Rows sharing a first component must NOT be confused with one another.
            call check(error, m%get([7_int64, 1_int64]) /= m%get([7_int64, 2_int64]), &
                "two rows sharing component 1 must answer differently on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get([8_int64, 1_int64]) /= m%get([8_int64, 2_int64]), &
                "and again for the next shared component-1 value")
            if (allocated(error)) return
            ! A tuple built from values that are each present, but never together, is absent.
            call check(error, m%get([7_int64, 3_int64]) == 0_int64, &
                "an unseen band for a known id is absent on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get([10_int64, 1_int64]) == 0_int64, &
                "an unseen id with a known band is absent on " // trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_composite_tuple_unique

    !> `[1, 2]` and `[2, 1]` are different keys, so the combine must be position-sensitive.
    !!
    !! This is what a symmetric tuple combine -- XOR-ing or summing per-component hashes -- would
    !! break, and the reason `ix_hash_chain` is a chain rather than a fold.
    subroutine test_composite_order_matters(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: pairs(2, 2)
        character(len=6) :: methods(2)
        integer :: k

        pairs(1, :) = [1_int64, 2_int64]
        pairs(2, :) = [2_int64, 1_int64]
        methods = ["direct", "hash  "]
        do k = 1, 2
            call m%build(pairs, method=trim(methods(k)))
            call check(error, m%nkeys() == 2_int64, &
                "[1,2] and [2,1] are two distinct keys on backend " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get([1_int64, 2_int64]) == 1_int64, &
                "[1,2] must find row 1 on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%get([2_int64, 1_int64]) == 2_int64, &
                "[2,1] must find row 2 on " // trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_composite_order_matters

    !> `%ncomponents` reports the width, and it is fixed for the map's lifetime.
    subroutine test_composite_ncomponents(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: pairs(3, 2), triples(2, 3)

        pairs(:, 1) = [1_int64, 2_int64, 3_int64]
        pairs(:, 2) = [1_int64, 1_int64, 2_int64]
        call m%build(pairs)
        call check(error, m%ncomponents() == 2, "a rank-2 build reports its column count")
        if (allocated(error)) return
        triples(:, 1) = [1_int64, 2_int64]
        triples(:, 2) = [3_int64, 4_int64]
        triples(:, 3) = [5_int64, 6_int64]
        call m%build(triples)
        call check(error, m%ncomponents() == 3, "rebuilding changes the width")
        if (allocated(error)) return
        call m%init(ncomp=4)
        call check(error, m%ncomponents() == 4, "init(ncomp=) sets the width")
    end subroutine test_composite_ncomponents

    !> The mixed-radix direct arm and the tuple hash agree over the same composite fixture.
    subroutine test_composite_backends_agree(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: md, mh
        integer(int64) :: pairs(9, 2), values(9), probe(2)
        integer(int64) :: a, b, want
        character(len=:), allocatable :: got

        pairs(:, 1) = [-2_int64, -2_int64, -2_int64, 0_int64, 0_int64, 0_int64, 5_int64, 5_int64, 5_int64]
        pairs(:, 2) = [10_int64, 11_int64, 12_int64, 10_int64, 11_int64, 12_int64, 10_int64, 11_int64, 12_int64]
        values = [9_int64, 8_int64, 7_int64, 6_int64, 5_int64, 4_int64, 3_int64, 2_int64, 1_int64]
        call md%build(pairs, values, method="direct")
        call mh%build(pairs, values, method="hash")
        call md%get_method(got)
        call check(error, got == "direct", "the direct arm was requested and taken")
        if (allocated(error)) return
        ! Sweep the whole rectangle the fixture spans, plus a margin outside it.
        do a = -4_int64, 7_int64
            do b = 8_int64, 14_int64
                probe = [a, b]
                want = expected_value_n(pairs, values, probe)
                call check(error, md%get(probe) == want, &
                    "the mixed-radix direct arm disagreed with an independent scan")
                if (allocated(error)) return
                call check(error, mh%get(probe) == want, &
                    "the tuple hash disagreed with an independent scan")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_composite_backends_agree

    !> Three components behave as two do, on both backends that support them.
    subroutine test_composite_three(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: t(4, 3), i
        character(len=6) :: methods(2)
        integer :: k

        t(:, 1) = [1_int64, 1_int64, 1_int64, 1_int64]
        t(:, 2) = [2_int64, 2_int64, 3_int64, 3_int64]
        t(:, 3) = [4_int64, 5_int64, 4_int64, 5_int64]
        methods = ["direct", "hash  "]
        do k = 1, 2
            call m%build(t, method=trim(methods(k)))
            call check(error, m%ncomponents() == 3, "three components on " // trim(methods(k)))
            if (allocated(error)) return
            do i = 1_int64, 4_int64
                call check(error, m%get(t(i, :)) == i, &
                    "each 3-tuple finds its row on " // trim(methods(k)))
                if (allocated(error)) return
            end do
            call check(error, m%get([1_int64, 2_int64, 6_int64]) == 0_int64, &
                "an absent 3-tuple answers 0 on " // trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_composite_three

    ! ---- Mutation ----

    !> `%set` inserts a new key, replaces an existing one, and tracks the watermark.
    subroutine test_set_upsert(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: idx

        call m%init()
        call m%set(42_int64, 1_int64)
        call check(error, m%get(42_int64) == 1_int64, "set inserts")
        if (allocated(error)) return
        call check(error, m%nkeys() == 1_int64, "set of a new key raises nkeys")
        if (allocated(error)) return
        call m%set(42_int64, 7_int64)
        call check(error, m%get(42_int64) == 7_int64, "set replaces")
        if (allocated(error)) return
        call check(error, m%nkeys() == 1_int64, "set of an existing key does not raise nkeys")
        if (allocated(error)) return
        ! The watermark followed the larger stored value, so the next auto number is above it.
        call m%get_or_add(43_int64, idx)
        call check(error, idx == 8_int64, &
            "set must raise the get_or_add watermark, so the next new key gets 8")
        if (allocated(error)) return
        ! The same on the direct backend, within its built range.
        call m%build([10_int64, 11_int64, 12_int64], method="direct")
        call m%set(11_int64, 99_int64)
        call check(error, m%get(11_int64) == 99_int64, "set replaces on the direct backend")
        if (allocated(error)) return
        call check(error, m%nkeys() == 3_int64, "replacing on direct does not change nkeys")
    end subroutine test_set_upsert

    !> `%set` on a map that was never built starts a hash map rather than refusing.
    subroutine test_set_autoinit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got

        call m%set(1000_int64, 5_int64)
        call check(error, m%get(1000_int64) == 5_int64, "set works without an init")
        if (allocated(error)) return
        call m%get_method(got)
        call check(error, got == "hash", "an auto-initialised map is a hash map")
        if (allocated(error)) return
        call check(error, m%ncomponents() == 1, "and single-component by default")
        if (allocated(error)) return
        ! The tuple form picks its width up from the key it is given.
        block
            type(pf_index_map) :: t
            call t%set([3_int64, 4_int64], 1_int64)
            call check(error, t%ncomponents() == 2, "a tuple set sets the width")
            if (allocated(error)) return
            call check(error, t%get([3_int64, 4_int64]) == 1_int64, "and stores the tuple")
        end block
    end subroutine test_set_autoinit

    !> `%get_or_add` numbers distinct keys 1..k in first-appearance order.
    subroutine test_get_or_add_dense(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: stream(9), idx, i
        integer(int64) :: seen(9)

        ! A stream with repeats: three distinct keys, so three distinct indexes.
        stream = [500_int64, 700_int64, 500_int64, 900_int64, 700_int64, 500_int64, &
            900_int64, 900_int64, 700_int64]
        call m%init()
        do i = 1_int64, 9_int64
            call m%get_or_add(stream(i), idx)
            seen(i) = idx
        end do
        call check(error, m%nkeys() == 3_int64, "three distinct keys were added")
        if (allocated(error)) return
        call check(error, seen(1) == 1_int64, "the first key gets index 1")
        if (allocated(error)) return
        call check(error, seen(2) == 2_int64, "the second distinct key gets index 2")
        if (allocated(error)) return
        call check(error, seen(4) == 3_int64, "the third distinct key gets index 3")
        if (allocated(error)) return
        call check(error, seen(3) == seen(1), "a repeat gets the same index back")
        if (allocated(error)) return
        call check(error, seen(5) == seen(2), "and again")
        if (allocated(error)) return
        call check(error, seen(9) == seen(2), "and at the end of the stream")
        if (allocated(error)) return
        call check(error, all(seen >= 1_int64 .and. seen <= 3_int64), &
            "every index handed out is within 1..k -- that is what dense means")
    end subroutine test_get_or_add_dense

    !> `%get_or_add` on a built map continues above the values already stored.
    subroutine test_get_or_add_after_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: idx

        call m%build([1_int64, 2_int64, 3_int64], [10_int64, 20_int64, 30_int64], method="hash")
        call m%get_or_add(2_int64, idx)
        call check(error, idx == 20_int64, "an existing key returns its stored value")
        if (allocated(error)) return
        call m%get_or_add(4_int64, idx)
        call check(error, idx == 31_int64, &
            "a new key continues above the largest value the build stored")
        if (allocated(error)) return
        call m%get_or_add(5_int64, idx)
        call check(error, idx == 32_int64, "and again for the next one")
        if (allocated(error)) return
        call check(error, m%nkeys() == 5_int64, "both new keys were added")
    end subroutine test_get_or_add_after_build

    !> `%remove` forgets exactly one key.
    subroutine test_remove_keeps_rest(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(6), i
        character(len=6) :: methods(2)
        integer :: k

        keys = [2_int64, 4_int64, 6_int64, 8_int64, 10_int64, 12_int64]
        methods = ["direct", "hash  "]
        do k = 1, 2
            call m%build(keys, method=trim(methods(k)))
            call m%remove(6_int64)
            call check(error, m%get(6_int64) == 0_int64, &
                "the removed key is gone on " // trim(methods(k)))
            if (allocated(error)) return
            call check(error, m%nkeys() == 5_int64, "nkeys fell by one on " // trim(methods(k)))
            if (allocated(error)) return
            do i = 1_int64, 6_int64
                if (keys(i) == 6_int64) cycle
                call check(error, m%get(keys(i)) == i, &
                    "every other key kept its value on " // trim(methods(k)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_remove_keeps_rest

    !> `%remove(key, found)` reports absence rather than aborting.
    subroutine test_remove_found(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        logical :: found

        call m%build([1_int64, 2_int64, 3_int64], method="hash")
        call m%remove(2_int64, found)
        call check(error, found, "removing a present key reports found")
        if (allocated(error)) return
        call m%remove(2_int64, found)
        call check(error, .not. found, "removing it twice reports not-found rather than aborting")
        if (allocated(error)) return
        call m%remove(99_int64, found)
        call check(error, .not. found, "removing a key that was never there reports not-found")
        if (allocated(error)) return
        call check(error, m%nkeys() == 2_int64, "and neither attempt changed the key count twice")
    end subroutine test_remove_found

    !> Every key is still findable after many table doublings.
    !!
    !! The rehash is the most mutation-prone path in the module: it recomputes every slot, so a
    !! defect there loses keys silently. Checking after EACH doubling rather than only at the end
    !! is what localises it.
    subroutine test_growth_rehash(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: i, j, n

        call m%init()
        n = 3000_int64
        do i = 1_int64, n
            call m%set(i * 7919_int64, i)
            ! At each power of two, every key inserted so far must still be there.
            if (iand(i, i - 1_int64) == 0_int64) then
                do j = 1_int64, i
                    if (m%get(j * 7919_int64) /= j) then
                        call check(error, .false., &
                            "a key was lost across a table doubling")
                        return
                    end if
                end do
            end if
        end do
        call check(error, m%nkeys() == n, "every key was inserted")
        if (allocated(error)) return
        do i = 1_int64, n
            call check(error, m%get(i * 7919_int64) == i, &
                "every key survives to the end of the growth run")
            if (allocated(error)) return
        end do
    end subroutine test_growth_rehash

    !> `%reserve` makes room, and the map still answers correctly afterwards.
    subroutine test_reserve_no_rehash(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: before, i

        call m%init()
        call m%reserve(5000_int64)
        before = m%memory_bytes()
        call check(error, before > 0_int64, "reserve allocates up front")
        if (allocated(error)) return
        do i = 1_int64, 2000_int64
            call m%set(i, i)
        end do
        call check(error, m%memory_bytes() == before, &
            "no rehash happened below the reserved capacity")
        if (allocated(error)) return
        do i = 1_int64, 2000_int64
            call check(error, m%get(i) == i, "and every key is findable")
            if (allocated(error)) return
        end do
        ! The int32 spelling works too.
        block
            type(pf_index_map) :: t
            call t%init()
            call t%reserve(100_int32)
            call check(error, t%memory_bytes() > 0_int64, "reserve accepts an int32 count")
        end block
    end subroutine test_reserve_no_rehash

    !> After removing many keys and reinserting them, every key is findable.
    !!
    !! This is what backward-shift deletion has to get right: a removal that broke a probe chain
    !! makes some LATER key unreachable, not the removed one, so the defect shows up as a lost key
    !! that was never touched. Interleaving removals and insertions is what exercises it.
    subroutine test_remove_reinsert(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: i, n

        n = 600_int64
        call m%init()
        do i = 1_int64, n
            call m%set(i * 3_int64, i)
        end do
        ! Remove every third key, then put them back.
        do i = 1_int64, n, 3_int64
            call m%remove(i * 3_int64)
        end do
        do i = 1_int64, n
            if (mod(i - 1_int64, 3_int64) == 0_int64) then
                call check(error, m%get(i * 3_int64) == 0_int64, "a removed key is gone")
            else
                call check(error, m%get(i * 3_int64) == i, &
                    "a key that was never removed is still findable")
            end if
            if (allocated(error)) return
        end do
        do i = 1_int64, n, 3_int64
            call m%set(i * 3_int64, i)
        end do
        call check(error, m%nkeys() == n, "every key is back")
        if (allocated(error)) return
        do i = 1_int64, n
            call check(error, m%get(i * 3_int64) == i, "and every key is findable again")
            if (allocated(error)) return
        end do
    end subroutine test_remove_reinsert

    ! ---- Clustering: what pins the mixer ----

    !> Keys that are multiples of a power of two must not collapse against the capacity mask.
    !!
    !! **This is the test that holds the mixer's constants to their job.** A slot index is
    !! `iand(hash, cap - 1)`, so a hash that passes the low bits through unchanged would send every
    !! multiple of 65536 to the same handful of slots and turn every lookup into a linear scan. The
    !! bound below is loose enough not to be a tuning test and tight enough that an identity mixer
    !! fails it by orders of magnitude.
    subroutine test_clustering_strided(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(4000), i, maxp
        real(real64) :: meanp

        do i = 1_int64, 4000_int64
            keys(i) = i * 65536_int64
        end do
        call m%build(keys, method="hash")
        call m%probe_stats(maxp, meanp)
        call check(error, maxp <= 40_int64, &
            "stride-2**16 keys must not cluster: the longest probe should stay short")
        if (allocated(error)) return
        call check(error, meanp < 3.0_real64, &
            "and the mean probe length should be near 1, not near the table size")
        if (allocated(error)) return
        ! And the map must still be correct, which clustering alone would not break.
        do i = 1_int64, 4000_int64
            call check(error, m%get(keys(i)) == i, "every strided key is findable")
            if (allocated(error)) return
        end do
        ! A stride of 2**32 varies only the high word, which is the case a half-width mixer
        ! would send to a single slot.
        do i = 1_int64, 500_int64
            keys(i) = i * 4294967296_int64
        end do
        call m%build(keys(1:500), method="hash")
        call m%probe_stats(maxp, meanp)
        call check(error, maxp <= 40_int64, &
            "stride-2**32 keys vary only in the high word and must still spread")
        if (allocated(error)) return
        call check(error, meanp < 3.0_real64, "and their mean probe length must stay near 1")
    end subroutine test_clustering_strided

    !> Consecutive keys, the commonest real key set, probe barely at all.
    subroutine test_clustering_sequential(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(5000), i, maxp
        real(real64) :: meanp

        do i = 1_int64, 5000_int64
            keys(i) = i
        end do
        call m%build(keys, method="hash")
        call m%probe_stats(maxp, meanp)
        call check(error, maxp <= 40_int64, "consecutive keys must not cluster")
        if (allocated(error)) return
        call check(error, meanp < 3.0_real64, "and must average close to one probe")
    end subroutine test_clustering_sequential

    !> A composite key set spread over a lattice must not cluster either.
    subroutine test_clustering_composite(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: pairs(2500, 2), maxp
        integer(int64) :: a, b, r
        real(real64) :: meanp

        r = 0_int64
        do a = 1_int64, 50_int64
            do b = 1_int64, 50_int64
                r = r + 1_int64
                pairs(r, 1) = a * 4096_int64
                pairs(r, 2) = b * 4096_int64
            end do
        end do
        call m%build(pairs, method="hash")
        call m%probe_stats(maxp, meanp)
        call check(error, maxp <= 40_int64, "a strided composite lattice must not cluster")
        if (allocated(error)) return
        call check(error, meanp < 3.0_real64, "and must average close to one probe")
        if (allocated(error)) return
        do r = 1_int64, 2500_int64
            call check(error, m%get(pairs(r, :)) == r, "every lattice tuple is findable")
            if (allocated(error)) return
        end do
    end subroutine test_clustering_composite

    !> `%probe_stats` answers about whichever backend it is asked about.
    subroutine test_probe_stats_by_backend(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(64), i, maxp
        real(real64) :: meanp

        do i = 1_int64, 64_int64
            keys(i) = i
        end do
        call m%build(keys, method="direct")
        call m%probe_stats(maxp, meanp)
        call check(error, maxp == 1_int64, "the direct backend does not probe at all")
        if (allocated(error)) return
        call check(error, meanp == 1.0_real64, "so its mean probe length is exactly 1")
        if (allocated(error)) return
        call m%build(keys, method="sorted")
        call m%probe_stats(maxp)
        call check(error, maxp == 7_int64, &
            "the sorted backend reports its binary search depth: 7 for 64 keys")
        if (allocated(error)) return
        ! An empty map reports nothing rather than a stale figure.
        call m%clear()
        call m%probe_stats(maxp, meanp)
        call check(error, maxp == 0_int64, "an empty map reports no probe length")
        if (allocated(error)) return
        call check(error, meanp == 0.0_real64, "and no mean")
        if (allocated(error)) return
        ! The optional argument really is optional.
        call m%build(keys, method="hash")
        call m%probe_stats(maxp)
        call check(error, maxp >= 1_int64, "probe_stats works without the mean")
    end subroutine test_probe_stats_by_backend

    !> `%memory_bytes` reflects what each backend actually holds.
    subroutine test_memory_bytes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(1000), i, direct_b, hash_b, sorted_b

        do i = 1_int64, 1000_int64
            keys(i) = i
        end do
        call m%build(keys, method="direct")
        direct_b = m%memory_bytes()
        call m%build(keys, method="hash")
        hash_b = m%memory_bytes()
        call m%build(keys, method="sorted")
        sorted_b = m%memory_bytes()
        ! The sorted backend is the exact-fit one: 16 bytes per key and no slack at all.
        call check(error, sorted_b == 16000_int64, &
            "the sorted backend must be exactly 16 bytes per key")
        if (allocated(error)) return
        call check(error, direct_b == 8000_int64 + 32_int64, &
            "the direct backend is 8 bytes per slot over a dense range, plus its geometry")
        if (allocated(error)) return
        ! At a load factor of 0.6 the table is 2048 slots of 16 bytes for 1000 keys.
        call check(error, hash_b == 32768_int64, &
            "the hash table rounds up to a power of two under the load factor")
        if (allocated(error)) return
        call check(error, hash_b > sorted_b, &
            "and so costs more than the exact-fit sorted backend, which is why sorted exists")
    end subroutine test_memory_bytes

    !> The binary search finds the first and last keys, which is where an off-by-one lives.
    subroutine test_sorted_boundaries(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(9), i

        keys = [-40_int64, -30_int64, -20_int64, -10_int64, 0_int64, 10_int64, 20_int64, &
            30_int64, 40_int64]
        call m%build(keys, method="sorted")
        call check(error, m%get(-40_int64) == 1_int64, "the first key is found")
        if (allocated(error)) return
        call check(error, m%get(40_int64) == 9_int64, "the last key is found")
        if (allocated(error)) return
        call check(error, m%get(-41_int64) == 0_int64, "one below the first is absent")
        if (allocated(error)) return
        call check(error, m%get(41_int64) == 0_int64, "one above the last is absent")
        if (allocated(error)) return
        do i = 1_int64, 9_int64
            call check(error, m%get(keys(i)) == i, "every key in between is found")
            if (allocated(error)) return
            call check(error, m%get(keys(i) + 1_int64) == 0_int64, &
                "and every gap between them is absent")
            if (allocated(error)) return
        end do
        ! A single-key map: both search bounds are the same element.
        call m%build([7_int64], method="sorted")
        call check(error, m%get(7_int64) == 1_int64, "a one-key sorted map finds its key")
        if (allocated(error)) return
        call check(error, m%get(6_int64) == 0_int64, "and misses below it")
        if (allocated(error)) return
        call check(error, m%get(8_int64) == 0_int64, "and above it")
    end subroutine test_sorted_boundaries

    ! ---- pf_index_pool ----

    !> The pool hands out consecutive indexes, takes them back, and keeps its counts.
    subroutine test_pool_basics(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: a, b, c

        a = p%get_index()
        b = p%get_index()
        c = p%get_index()
        call check(error, a == 1_int64, "the first index is 1")
        if (allocated(error)) return
        call check(error, b == 2_int64, "the second is 2")
        if (allocated(error)) return
        call check(error, c == 3_int64, "the third is 3")
        if (allocated(error)) return
        call check(error, p%get_max_index() == 3_int64, "the watermark is 3")
        if (allocated(error)) return
        call check(error, p%get_used_count() == 3_int64, "three are held")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 0_int64, "none are free")
        if (allocated(error)) return
        call p%free_index(b)
        call check(error, p%get_used_count() == 2_int64, "two are held after one free")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 1_int64, "and one is free")
        if (allocated(error)) return
        call check(error, p%get_max_index() == 3_int64, "the watermark did not move")
        if (allocated(error)) return
        ! The int32 spelling of free_index reaches the same state.
        call p%free_index(1_int32)
        call check(error, p%get_used_count() == 1_int64, "the int32 free_index works too")
    end subroutine test_pool_basics

    !> An index is reused before the pool grows, which is what keeps the set dense.
    subroutine test_pool_reuse_before_growth(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx

        do i = 1_int64, 5_int64
            idx = p%get_index()
        end do
        call p%free_index(3_int64)
        idx = p%get_index()
        call check(error, idx == 3_int64, "the freed index comes back before any new one")
        if (allocated(error)) return
        call check(error, p%get_max_index() == 5_int64, "and the watermark did not grow")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 6_int64, "only once nothing is free does the pool grow")
        if (allocated(error)) return
        call check(error, p%get_max_index() == 6_int64, "and then the watermark follows")
    end subroutine test_pool_reuse_before_growth

    !> `%is_used` answers for held indexes, free ones, and anything out of range.
    subroutine test_pool_is_used(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx

        do i = 1_int64, 4_int64
            idx = p%get_index()
        end do
        call p%free_index(2_int64)
        call check(error, p%is_used(1_int64), "1 is held")
        if (allocated(error)) return
        call check(error, .not. p%is_used(2_int64), "2 was freed")
        if (allocated(error)) return
        call check(error, p%is_used(3_int64), "3 is held")
        if (allocated(error)) return
        call check(error, .not. p%is_used(5_int64), "5 was never handed out")
        if (allocated(error)) return
        call check(error, .not. p%is_used(0_int64), "0 is not an index this pool issues")
        if (allocated(error)) return
        call check(error, .not. p%is_used(-1_int64), "nor is a negative one")
        if (allocated(error)) return
        call check(error, p%is_used(3_int32), "the int32 spelling agrees")
    end subroutine test_pool_is_used

    !> `%used_indexes` lists exactly what is held, ascending.
    subroutine test_pool_used_indexes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64), allocatable :: held(:)
        integer(int64) :: i, idx

        do i = 1_int64, 200_int64
            idx = p%get_index()
        end do
        do i = 2_int64, 200_int64, 2_int64
            call p%free_index(i)
        end do
        call p%used_indexes(held)
        call check(error, size(held, kind=int64) == 100_int64, "one entry per held index")
        if (allocated(error)) return
        do i = 1_int64, 100_int64
            call check(error, held(i) == 2_int64 * i - 1_int64, &
                "the odd indexes are held, in ascending order")
            if (allocated(error)) return
        end do
        ! The bit-scan walk must skip a wholly free word without losing what follows it.
        call p%clear()
        do i = 1_int64, 200_int64
            idx = p%get_index()
        end do
        do i = 65_int64, 128_int64
            call p%free_index(i)
        end do
        call p%used_indexes(held)
        call check(error, size(held, kind=int64) == 136_int64, &
            "freeing a whole 64-bit word leaves the rest held")
        if (allocated(error)) return
        call check(error, held(64) == 64_int64, "the word before the gap is intact")
        if (allocated(error)) return
        call check(error, held(65) == 129_int64, "and the walk resumes after the empty word")
    end subroutine test_pool_used_indexes

    !> After `%compact` the smallest free index is handed out first, ascending.
    subroutine test_pool_compact_order(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx

        do i = 1_int64, 100_int64
            idx = p%get_index()
        end do
        ! Free out of order, and across word boundaries.
        call p%free_index(90_int64)
        call p%free_index(10_int64)
        call p%free_index(64_int64)
        call p%free_index(50_int64)
        call p%compact()
        idx = p%get_index()
        call check(error, idx == 10_int64, "compact hands out the smallest free index first")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 50_int64, "then the next smallest")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 64_int64, "then the next")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 90_int64, "then the largest of the free ones")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 101_int64, "and only then a new index")
    end subroutine test_pool_compact_order

    !> Without `%compact` the pool is LIFO -- the negative control for the test above.
    !!
    !! **This is what makes the ordering test evidence rather than a coincidence.** Freeing
    !! 90, 10, 64, 50 and then getting 10 back proves nothing on its own: it could be an artifact
    !! of the fixture. Here the same frees without the compact return 50, 64, 10, 90 -- the reverse
    !! of the order they were freed in -- so the ascending order above is the resort doing its job.
    subroutine test_pool_lifo_control(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx

        do i = 1_int64, 100_int64
            idx = p%get_index()
        end do
        call p%free_index(90_int64)
        call p%free_index(10_int64)
        call p%free_index(64_int64)
        call p%free_index(50_int64)
        idx = p%get_index()
        call check(error, idx == 50_int64, "without compact, the most recently freed comes back")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 64_int64, "then the one before it")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 10_int64, "then the one before that")
        if (allocated(error)) return
        idx = p%get_index()
        call check(error, idx == 90_int64, "and the first freed comes back last")
    end subroutine test_pool_lifo_control

    !> `%compact` keeps every held index and lowers the watermark to the highest one.
    subroutine test_pool_compact_state(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx
        logical :: before(20)

        do i = 1_int64, 20_int64
            idx = p%get_index()
        end do
        ! Keep 1..7 and 12; free everything else, including the top.
        do i = 8_int64, 20_int64
            if (i /= 12_int64) call p%free_index(i)
        end do
        do i = 1_int64, 20_int64
            before(i) = p%is_used(i)
        end do
        call p%compact()
        do i = 1_int64, 20_int64
            call check(error, p%is_used(i) .eqv. before(i), &
                "compact must not change which indexes are held")
            if (allocated(error)) return
        end do
        call check(error, p%get_max_index() == 12_int64, &
            "the watermark falls to the highest index actually held")
        if (allocated(error)) return
        call check(error, p%get_used_count() == 8_int64, "the held count is unchanged")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 4_int64, &
            "and the free count is now the holes below the new watermark: 8, 9, 10, 11")
        if (allocated(error)) return
        ! Those four come back ascending, then the pool grows from the new watermark.
        call check(error, p%get_index() == 8_int64, "8 comes back first")
        if (allocated(error)) return
        call check(error, p%get_index() == 9_int64, "then 9")
        if (allocated(error)) return
        call check(error, p%get_index() == 10_int64, "then 10")
        if (allocated(error)) return
        call check(error, p%get_index() == 11_int64, "then 11")
        if (allocated(error)) return
        call check(error, p%get_index() == 13_int64, &
            "and then growth resumes above the new watermark")
    end subroutine test_pool_compact_state

    !> `%compact` gives storage back when the watermark has fallen far behind it.
    subroutine test_pool_compact_shrinks(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx, before, after

        ! The maintainer's own example, scaled down: take many, free most of them.
        do i = 1_int64, 100000_int64
            idx = p%get_index()
        end do
        do i = 11_int64, 100000_int64
            call p%free_index(i)
        end do
        before = p%memory_bytes()
        call check(error, before > 0_int64, "the pool holds storage before compacting")
        if (allocated(error)) return
        call p%compact()
        after = p%memory_bytes()
        call check(error, after < before, &
            "compact must give back storage that grew with get_index")
        if (allocated(error)) return
        call check(error, p%get_max_index() == 10_int64, "and lower the watermark to 10")
        if (allocated(error)) return
        do i = 1_int64, 10_int64
            call check(error, p%is_used(i), "every held index survived the shrink")
            if (allocated(error)) return
        end do
        call check(error, p%get_index() == 11_int64, "and the next index continues from there")
    end subroutine test_pool_compact_shrinks

    !> The watermark is monotone between compacts, including when the top index is freed.
    subroutine test_pool_watermark_monotone(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx

        do i = 1_int64, 10_int64
            idx = p%get_index()
        end do
        call p%free_index(10_int64)
        call check(error, p%get_max_index() == 10_int64, &
            "freeing the top index does not lower the watermark -- only compact does")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 1_int64, &
            "so the freed top index counts as a hole until then")
        if (allocated(error)) return
        call p%compact()
        call check(error, p%get_max_index() == 9_int64, "and compact is where it tightens")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 0_int64, "leaving no holes at all")
    end subroutine test_pool_watermark_monotone

    !> A cleared pool is as new.
    subroutine test_pool_clear(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64) :: i, idx

        do i = 1_int64, 50_int64
            idx = p%get_index()
        end do
        call p%clear()
        call check(error, p%get_max_index() == 0_int64, "a cleared pool has issued nothing")
        if (allocated(error)) return
        call check(error, p%get_used_count() == 0_int64, "and holds nothing")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 0_int64, "and has no holes")
        if (allocated(error)) return
        call check(error, p%memory_bytes() == 0_int64, "and released its storage")
        if (allocated(error)) return
        call check(error, .not. p%is_used(1_int64), "and holds no index 1")
        if (allocated(error)) return
        call check(error, p%get_index() == 1_int64, "and issues 1 again")
    end subroutine test_pool_clear

    !> Every query answers on a pool that has never issued anything.
    subroutine test_pool_empty(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        integer(int64), allocatable :: held(:)

        call check(error, p%get_max_index() == 0_int64, "a fresh pool's watermark is 0")
        if (allocated(error)) return
        call check(error, p%get_used_count() == 0_int64, "it holds nothing")
        if (allocated(error)) return
        call check(error, p%get_free_index_count() == 0_int64, "it has no holes")
        if (allocated(error)) return
        call check(error, p%memory_bytes() == 0_int64, "it holds no memory")
        if (allocated(error)) return
        call check(error, .not. p%is_used(1_int64), "nothing is used")
        if (allocated(error)) return
        call p%used_indexes(held)
        call check(error, allocated(held), "used_indexes allocates rather than not")
        if (allocated(error)) return
        call check(error, size(held) == 0, "and is zero-length")
        if (allocated(error)) return
        ! Compacting a pool that has never issued anything must be a no-op, not a fault.
        call p%compact()
        call check(error, p%get_max_index() == 0_int64, "compacting an empty pool is harmless")
        if (allocated(error)) return
        call check(error, p%get_index() == 1_int64, "and it still issues 1 afterwards")
    end subroutine test_pool_empty

    !> `%reserve` changes no answer the pool gives.
    subroutine test_pool_reserve(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p, q
        integer(int64) :: i, a, b

        call p%reserve(10000_int64)
        call check(error, p%memory_bytes() > 0_int64, "reserve allocates up front")
        if (allocated(error)) return
        call check(error, p%get_max_index() == 0_int64, "but hands nothing out")
        if (allocated(error)) return
        ! A reserved pool and a fresh one issue the same sequence.
        do i = 1_int64, 100_int64
            a = p%get_index()
            b = q%get_index()
            call check(error, a == b, "reserve must not change which index comes next")
            if (allocated(error)) return
        end do
        call p%free_index(50_int64)
        call q%free_index(50_int64)
        call check(error, p%get_index() == q%get_index(), "nor after a free")
        if (allocated(error)) return
        call q%reserve(5_int32)
        call check(error, q%get_max_index() == 100_int64, &
            "reserving below the watermark changes nothing")
    end subroutine test_pool_reserve

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module test_index ! GCOVR_EXCL_LINE
