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
            new_unittest("a composite hash map survives doublings and removals", &
                test_composite_growth_remove), &
            new_unittest("bulk composite lookups compare every component, in blocks", &
                test_composite_bulk_shared_first), &
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
            new_unittest("the sorted prefix table answers every key shape", test_sorted_prefix_shapes), &
            new_unittest("the pool hands out, recycles and counts", test_pool_basics), &
            new_unittest("the pool reuses before it grows", test_pool_reuse_before_growth), &
            new_unittest("is_used answers for every index and none other", test_pool_is_used), &
            new_unittest("used_indexes lists what is held, ascending", test_pool_used_indexes), &
            new_unittest("used_indexes and keys answer the same list in int32 as in int64", &
                         test_index_int32_lists), &
            new_unittest("compact hands out the smallest free index first", test_pool_compact_order), &
            new_unittest("without compact the pool stays LIFO", test_pool_lifo_control), &
            new_unittest("compact keeps every held index and lowers the watermark", test_pool_compact_state), &
            new_unittest("compact gives storage back", test_pool_compact_shrinks), &
            new_unittest("a sorted map answers get_or_add for a key it holds", &
                test_sorted_get_or_add_present), &
            new_unittest("compact over holes below the watermark keeps only the bitmap", &
                test_pool_compact_holes), &
            new_unittest("the watermark tightens only at compact", test_pool_watermark_monotone), &
            new_unittest("a cleared pool issues 1 again", test_pool_clear), &
            new_unittest("an empty pool answers every query", test_pool_empty), &
            new_unittest("reserve changes no answer the pool gives", test_pool_reserve), &
            new_unittest("the int32 scalar entry points answer as the int64 ones do", &
                test_int32_scalar_entry_points), &
            new_unittest("the int32 composite entry points answer as the int64 ones do", &
                test_int32_tuple_entry_points), &
            new_unittest("set, get_or_add and remove agree in every kind combination", &
                test_int32_mutation_entry_points), &
            new_unittest("the composite mutations agree in every kind combination", &
                test_int32_tuple_mutation_entry_points), &
            new_unittest("get_or_add_many codes alike whatever the rank and kinds", &
                test_int32_get_or_add_many_kinds), &
            new_unittest("a composite direct map is mutable inside its ranges", &
                test_direct_composite_mutation), &
            new_unittest("remove on an unbuilt map reports the key as absent", &
                test_remove_on_unbuilt_map), &
            new_unittest("a one-column rank-2 build equals a rank-1 build", &
                test_rank2_single_column_build), &
            new_unittest("a sorted build takes valid= and values= together", &
                test_sorted_build_masked_values), &
            new_unittest("reset empties every backend and keeps its storage", &
                test_reset_every_backend), &
            new_unittest("a one-column rank-2 get_many equals the rank-1 one", &
                test_rank2_single_column_get_many), &
            new_unittest("a one-column rank-2 build takes valid= on every backend", &
                test_rank2_single_column_masked_build), &
            new_unittest("an explicit auto token chooses as an absent method= does", &
                test_explicit_auto_token), &
            new_unittest("a map answers 0 between init and its first insert", &
                test_init_before_first_insert) &
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
        logical :: found

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
        ! The MUTATION side spells it the same way: the tuple `%remove` hands a 1-component map
        ! to the scalar remove rather than probing a one-wide tuple table that does not exist.
        call m%build(keys, method="hash")
        call m%remove([keys(3)], found)
        call check(error, found .and. m%get(keys(3)) == 0_int64 .and. m%nkeys() == 4_int64, &
            "a 1-tuple remove takes the key out of a scalar map")
        if (allocated(error)) return
        call m%remove([keys(3)], found)
        call check(error, .not. found, "and reports the second attempt as not-found")
        if (allocated(error)) return
        call check(error, m%get(keys(2)) == 2_int64 .and. m%get(keys(4)) == 4_int64, &
            "its neighbours keep their values")
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

    !> A composite hash map keeps every tuple through table doublings and backward-shift
    !! removals: the interleaved record layout's insert, rehash, remove and enumerate paths, on
    !! integer tuples rather than only through the string map that shares them.
    subroutine test_composite_growth_remove(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64), allocatable :: listed(:,:)
        integer(int64) :: i, a, b, v
        integer :: kept

        ! 6000 tuples through %set on a map that starts at the minimum capacity, so the table
        ! doubles many times; the first component walks a prime modulus, the second is a
        ! multiple of 4096, which is the shape a weak per-half mixer would cluster on.
        call m%init(ncomp=2)
        do i = 1_int64, 6000_int64
            a = mod(i * 7919_int64, 1000003_int64)
            b = i * 4096_int64
            call m%set([a, b], i)
        end do
        call check(error, m%nkeys() == 6000_int64, "every tuple set is stored through the doublings")
        if (allocated(error)) return
        ! Remove every third tuple; every survivor must still be found and every removed one
        ! absent -- the backward shift must move exactly the entries whose chain it broke.
        do i = 3_int64, 6000_int64, 3_int64
            a = mod(i * 7919_int64, 1000003_int64)
            b = i * 4096_int64
            call m%remove([a, b])
        end do
        call check(error, m%nkeys() == 4000_int64, "removal lowers the count by the removed tuples")
        if (allocated(error)) return
        do i = 1_int64, 6000_int64
            a = mod(i * 7919_int64, 1000003_int64)
            b = i * 4096_int64
            v = m%get([a, b])
            if (mod(i, 3_int64) == 0_int64) then
                call check(error, v == 0_int64, "a removed tuple must answer 0")
            else
                call check(error, v == i, "a surviving tuple must keep its value through the removals")
            end if
            if (allocated(error)) return
        end do
        ! And %keys lists exactly the survivors, each of which the map still answers.
        call m%keys(listed)
        call check(error, size(listed, 1, kind=int64) == 4000_int64 .and. size(listed, 2) == 2, &
            "keys() is shaped (survivors, 2)")
        if (allocated(error)) return
        kept = 0
        do i = 1_int64, 4000_int64
            v = m%get(listed(i, :))
            if (v > 0_int64 .and. mod(v, 3_int64) /= 0_int64) kept = kept + 1
        end do
        call check(error, kept == 4000, "every listed tuple is a stored survivor")
    end subroutine test_composite_growth_remove

    !> The bulk composite lookup compares EVERY component of a record, for pairs and for wider
    !! tuples alike, and gets every block boundary right.
    !!
    !! Every tuple here shares its first component with EVERY other -- `(7, i)`, and `(7, i, 11)`
    !! -- so a probe that matched a record on its first component alone answers wrongly whenever
    !! its walk meets any other record before its own, which at a load of 0.6 is a fifth of the
    !! lookups. (A fixture sharing the first component among three tuples only let exactly that
    !! defect survive in the general kernel: a walk rarely met one of two neighbours.) Pairs and
    !! triples take different kernels underneath (`ix_probe_2_block` and `ix_probe_n_block`), so
    !! both are built; 3000 rows is 46 full probe blocks and a partial one; and the `int32` key
    !! form goes through its own chunk loop. The scalar `%get` is the oracle for every row.
    subroutine test_composite_bulk_shared_first(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m2, m3
        integer(int64), parameter :: n = 3000_int64
        integer(int64) :: pairs(n, 2), triples(n, 3), got(n), i
        integer(int32) :: pairs32(n, 2), got32(n)
        integer :: bad

        do i = 1_int64, n
            pairs(i, 1) = 7_int64
            pairs(i, 2) = i
            triples(i, 1) = 7_int64
            triples(i, 2) = i
            triples(i, 3) = 11_int64
        end do
        pairs32 = int(pairs, int32)
        call m2%build(pairs, method="hash")
        call m3%build(triples, method="hash")
        call check(error, m2%nkeys() == n .and. m3%nkeys() == n, "every tuple is distinct and stored")
        if (allocated(error)) return
        call m2%get_many(pairs, got, threads=1)
        bad = 0
        do i = 1_int64, n
            if (got(i) /= i) bad = bad + 1
        end do
        call check(error, bad == 0, "a bulk pair lookup must compare both components: " // &
            "every tuple shares its first one")
        if (allocated(error)) return
        call m2%get_many(pairs32, got32, threads=1)
        bad = 0
        do i = 1_int64, n
            if (int(got32(i), int64) /= i) bad = bad + 1
        end do
        call check(error, bad == 0, "the int32 pair form answers the same rows")
        if (allocated(error)) return
        call m3%get_many(triples, got, threads=1)
        bad = 0
        do i = 1_int64, n
            if (got(i) /= i) bad = bad + 1
        end do
        call check(error, bad == 0, "a bulk triple lookup must compare every component")
        if (allocated(error)) return
        do i = 1_int64, n, 7_int64
            call check(error, m2%get(pairs(i, :)) == i .and. m3%get(triples(i, :)) == i, &
                "the scalar lookups agree with the bulk ones")
            if (allocated(error)) return
        end do
        ! And a tuple whose first component is stored but whose later ones are not is a miss,
        ! through the bulk kernels as well as the scalar probe.
        call check(error, m2%get([7_int64, n + 1_int64]) == 0_int64 .and. &
            m3%get([7_int64, 5_int64, 8_int64]) == 0_int64, &
            "a tuple differing only in a later component is absent")
        if (allocated(error)) return
        do i = 1_int64, n
            pairs(i, 2) = n + i
            triples(i, 3) = 12_int64
        end do
        call m2%get_many(pairs, got, threads=1)
        call m3%get_many(triples(:, :), got32, threads=1)
        bad = 0
        do i = 1_int64, n
            if (got(i) /= 0_int64 .or. got32(i) /= 0_int32) bad = bad + 1
        end do
        call check(error, bad == 0, "bulk lookups of tuples differing only in a later " // &
            "component must all miss")
    end subroutine test_composite_bulk_shared_first

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
        ! And a lattice that varies only in the HIGH words: the tuple hash runs one chain over
        ! the components' low halves and another over their high halves, so this is the case
        ! the second chain exists for -- a hash reading the low halves alone would send all 2500
        ! tuples to one slot.
        r = 0_int64
        do a = 1_int64, 50_int64
            do b = 1_int64, 50_int64
                r = r + 1_int64
                pairs(r, 1) = a * 4294967296_int64
                pairs(r, 2) = b * 4294967296_int64
            end do
        end do
        call m%build(pairs, method="hash")
        call m%probe_stats(maxp, meanp)
        call check(error, maxp <= 40_int64, &
            "a composite lattice varying only in the high words must not cluster")
        if (allocated(error)) return
        call check(error, meanp < 3.0_real64, "and must average close to one probe too")
        if (allocated(error)) return
        do r = 1_int64, 2500_int64, 7_int64
            call check(error, m%get(pairs(r, :)) == r, "every high-word lattice tuple is findable")
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
        call m%probe_stats(maxp, meanp)
        ! 64 dense keys get a prefix table of 8 buckets (the largest power of two with eight or
        ! more keys per bucket); a span of 63 shifts by 3, so the buckets hold keys 1..7, 8..15,
        ! ..., 56..63 and 64 alone -- at most 8 keys, a search 4 deep (2**4 > 8) after the one
        ! table load: 5. Without the table the whole array's search is 7 deep, which is what a
        ! table that shaped nothing would report.
        call check(error, maxp == 5_int64, &
            "the sorted backend reports one table load plus the widest bucket's search: 5 for 64 keys")
        if (allocated(error)) return
        call check(error, meanp > 1.0_real64 .and. meanp <= 5.0_real64, &
            "and a key-weighted mean between the load alone and the widest bucket")
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
        ! The sorted backend is the exact-fit one: 16 bytes per key and no slack, plus a prefix
        ! table of at most an eighth of the key count in 8-byte entries (one sixteenth of the
        ! keys' bytes) and one sentinel -- present here, since 1000 keys are enough for a table.
        call check(error, sorted_b > 16000_int64, &
            "a thousand-key sorted map carries a prefix table on top of its 16 bytes per key")
        if (allocated(error)) return
        call check(error, sorted_b <= 16000_int64 + 1000_int64 + 8_int64, &
            "and that table is at most a sixteenth of the keys' bytes plus its sentinel")
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

    !> The prefix table answers every key shape a sorted map can meet: dense, strided and
    !! sparse keys, keys at both ends of `int64` in one map (the span past `huge`, where no
    !! difference of the ends may be formed), a cluster beside one far outlier (one bucket holds
    !! nearly every key and the search inside it is the plain one), negative keys, and a map too
    !! small for a table at all. Every shape is checked against an independent linear scan over
    !! hits, the gaps beside every key, and the extremes of `int64`; the table's presence is
    !! asserted through `%memory_bytes` so that a build which silently skipped it could not pass.
    subroutine test_sorted_prefix_shapes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64), allocatable :: keys(:), values(:), probes(:), many(:)
        integer(int64) :: i, n, maxp, hg
        real(real64) :: meanp
        integer :: shape

        hg = huge(0_int64)
        do shape = 1, 7
            select case (shape)
            case (1)
                ! Dense: the case a raw top-16-bit prefix would put in ONE bucket.
                n = 5000_int64
                allocate(keys(n))
                do i = 1_int64, n
                    keys(i) = i
                end do
            case (2)
                ! Strided by 2**16: keys that share their low sixteen bits.
                n = 3000_int64
                allocate(keys(n))
                do i = 1_int64, n
                    keys(i) = i * 65536_int64
                end do
            case (3)
                ! The benchmark's sparse walk, spread over a range far wider than the count.
                n = 4000_int64
                allocate(keys(n))
                do i = 1_int64, n
                    keys(i) = 1_int64 + i * 2654435761_int64
                end do
            case (4)
                ! Both ends of int64 in one map: the span exceeds huge and must never be formed.
                n = 300_int64
                allocate(keys(n))
                do i = 1_int64, 150_int64
                    keys(i) = -hg + (i - 1_int64) * 7_int64
                    keys(150_int64 + i) = hg - (i - 1_int64) * 7_int64
                end do
            case (5)
                ! Negative keys straddling zero, three apart.
                n = 1334_int64
                allocate(keys(n))
                do i = 1_int64, n
                    keys(i) = -2000_int64 + (i - 1_int64) * 3_int64
                end do
            case (6)
                ! A cluster of a thousand beside one key at huge: the cluster's bucket holds
                ! them all, every other bucket is empty, and the search inside degrades to the
                ! plain one.
                n = 1001_int64
                allocate(keys(n))
                do i = 1_int64, 1000_int64
                    keys(i) = 1000000_int64 + i
                end do
                keys(n) = hg
            case default
                ! Fifteen keys: below the sixteen a table needs, so the plain search alone.
                n = 15_int64
                allocate(keys(n))
                do i = 1_int64, n
                    keys(i) = i * i
                end do
            end select
            allocate(values(n))
            do i = 1_int64, n
                values(i) = n + 1_int64 - i
            end do
            ! Probes: every key, the position after every key, the ends and both extremes.
            allocate(probes(2_int64 * n + 4_int64))
            probes(1:n) = keys
            do i = 1_int64, n
                if (keys(i) < hg) then
                    probes(n + i) = keys(i) + 1_int64
                else
                    probes(n + i) = keys(i) - 1_int64
                end if
            end do
            probes(2_int64 * n + 1_int64) = -hg
            probes(2_int64 * n + 2_int64) = hg
            probes(2_int64 * n + 3_int64) = 0_int64
            probes(2_int64 * n + 4_int64) = keys(1) - 1_int64
            if (keys(1) == -hg) probes(2_int64 * n + 4_int64) = keys(1) + 3_int64
            call check_lookups_against_oracle(error, keys, values, probes, "sorted")
            if (allocated(error)) return
            ! The same map through the bulk path, and the table's presence or absence.
            call m%build(keys, values, method="sorted")
            allocate(many(size(probes, kind=int64)))
            call m%get_many(probes, many)
            do i = 1_int64, size(probes, kind=int64)
                call check(error, many(i) == m%get(probes(i)), &
                    "sorted %get_many disagrees with %get on a prefix shape")
                if (allocated(error)) return
            end do
            if (n >= 16_int64) then
                call check(error, m%memory_bytes() > 16_int64 * n, &
                    "a sorted map of sixteen keys or more must carry a prefix table")
            else
                call check(error, m%memory_bytes() == 16_int64 * n, &
                    "a sorted map of fewer than sixteen keys carries no prefix table")
            end if
            if (allocated(error)) return
            call m%probe_stats(maxp, meanp)
            call check(error, maxp >= 1_int64 .and. meanp >= 1.0_real64 .and. meanp <= real(maxp, real64), &
                "sorted probe stats: the mean lies between one load and the widest bucket's search")
            if (allocated(error)) return
            deallocate(keys, values, probes, many)
        end do
        ! The cluster-plus-outlier shape (6) reports the plain search's depth inside its one
        ! full bucket: 1000 keys need 10 levels, plus the table load.
        n = 1001_int64
        allocate(keys(n))
        do i = 1_int64, 1000_int64
            keys(i) = 1000000_int64 + i
        end do
        keys(n) = hg
        call m%build(keys, method="sorted")
        call m%probe_stats(maxp)
        call check(error, maxp == 11_int64, &
            "a cluster beside one outlier degrades to the plain search inside its bucket: 1 + 10")
    end subroutine test_sorted_prefix_shapes

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

    !> `%used_indexes` and `%keys` answer the same list in `int32` as in `int64`.
    !>
    !> Parity element for element: the `int32` forms are a second route through the same walk and
    !> the same key collection, so only comparing them forbids one of the two drifting.
    subroutine test_index_int32_lists(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_pool) :: p
        type(pf_index_map) :: m
        integer(int64), allocatable :: held64(:), keys64(:), tup64(:,:)
        integer(int32), allocatable :: held32(:), keys32(:), tup32(:,:)
        integer(int64) :: i, idx, keys(6), vals(6), pairs(6, 2)

        do i = 1_int64, 50_int64
            idx = p%get_index()
        end do
        call p%free_index(7_int64)
        call p%used_indexes(held64)
        call p%used_indexes(held32)
        call check(error, size(held32, kind=int64) == size(held64, kind=int64), &
            "used_indexes must answer the same length in both kinds")
        if (allocated(error)) return
        call check(error, all(int(held32, kind=int64) == held64) .and. size(held64, kind=int64) == 49_int64, &
            "used_indexes must answer the same indexes in both kinds")
        if (allocated(error)) return
        ! **A negative key inside the int32 range.** Keys are the caller's own values, so the
        ! narrowing has to carry the sign; the out-of-range case is an error scenario, since it
        ! aborts.
        keys = [-2000000000_int64, -7_int64, 0_int64, 5_int64, 11_int64, 2000000000_int64]
        vals = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]
        call m%build(keys, vals, method="hash")
        call m%keys(keys64)
        call m%keys(keys32)
        call check(error, size(keys32, kind=int64) == 6_int64 .and. all(int(keys32, kind=int64) == keys64), &
            "keys must answer the same keys in both kinds, negative ones included")
        if (allocated(error)) return
        call m%clear()
        pairs(:, 1) = [-9_int64, -9_int64, 3_int64, 3_int64, 8_int64, 8_int64]
        pairs(:, 2) = [1_int64, 2_int64, 1_int64, 2_int64, 1_int64, 2_int64]
        call m%build(pairs, vals, method="hash")
        call m%keys(tup64)
        call m%keys(tup32)
        call check(error, all(shape(tup32) == shape(tup64)) .and. all(int(tup32, kind=int64) == tup64), &
            "the rank-2 keys form must answer the same tuples in both kinds")
    end subroutine test_index_int32_lists

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

    !> `%compact` over a pool whose holes lie BELOW its watermark: the storage left is the bitmap's
    !! alone, the holes still come back smallest first, a free after the compact still comes back
    !! next, and every hole is issued exactly once before the pool grows.
    !!
    !! `test_pool_compact_shrinks` frees the top of the range, so its watermark falls and there is
    !! nothing left to list; here every tenth index stays held, the watermark cannot fall, and the
    !! ninety thousand holes are what a free list of one entry per hole would cost 720 kB for.
    subroutine test_pool_compact_holes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer(int64), parameter :: n = 100000_int64
        type(pf_index_pool) :: p
        integer(int64) :: i, idx, bitmap, issued
        logical, allocatable :: held(:)

        do i = 1_int64, n
            idx = p%get_index()
        end do
        do i = 1_int64, n
            if (mod(i, 10_int64) /= 0_int64) call p%free_index(i)
        end do
        call p%compact()
        call check(error, p%get_max_index() == n .and. p%get_free_index_count() == 90000_int64, &
            "the watermark cannot fall, and ninety thousand holes lie below it (vacuity guard)")
        if (allocated(error)) return
        ! The bitmap's exact size, and the half again its geometric growth may have left on it.
        bitmap = 8_int64 * ((n + 63_int64) / 64_int64)
        call check(error, p%memory_bytes() <= bitmap + bitmap / 2_int64, &
            "after compact the pool holds its bitmap and nothing per free index")
        if (allocated(error)) return
        call check(error, p%get_index() == 1_int64 .and. p%get_index() == 2_int64, &
            "the holes come back smallest first")
        if (allocated(error)) return
        call p%free_index(20_int64)
        call check(error, p%get_index() == 20_int64, "an index freed after the compact comes back next")
        if (allocated(error)) return
        call check(error, p%get_index() == 3_int64, "and then the holes resume where they left off")
        if (allocated(error)) return
        ! Drain the rest: each index handed out must be one not held a moment before, and the
        ! pool must grow only once every hole below the watermark is out.
        allocate(held(n + 1_int64))
        do i = 1_int64, n
            held(i) = p%is_used(i)
        end do
        held(n + 1_int64) = .false.
        issued = 0_int64
        do while (p%get_free_index_count() > 0_int64)
            idx = p%get_index()
            call check(error, idx >= 1_int64 .and. idx <= n, "a hole is issued before the pool grows")
            if (allocated(error)) return
            call check(error, .not. held(idx), "no index is issued twice")
            if (allocated(error)) return
            held(idx) = .true.
            issued = issued + 1_int64
        end do
        call check(error, issued == 90000_int64 - 3_int64 .and. all(held(1:n)), &
            "every hole is issued exactly once")
        if (allocated(error)) return
        call check(error, p%get_index() == n + 1_int64, "and only then does the pool grow")
    end subroutine test_pool_compact_holes

    !> A sorted map answers `%get_or_add` and `%get_or_add_many` for keys it holds -- the stored
    !! value, nothing mutated -- as `%get` would. The other half, a NEW key aborting, is the
    !! `index_sorted_get_or_add_absent` scenario (test/error_scenarios.f90).
    subroutine test_sorted_get_or_add_present(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: idx, codes(3)
        integer(int32) :: idx32
        character(len=:), allocatable :: tok

        call m%build([10_int64, 20_int64, 30_int64], [7_int64, 8_int64, 9_int64], method="sorted")
        call m%get_method(tok)
        call check(error, tok == "sorted", "the fixture is a sorted map (vacuity guard)")
        if (allocated(error)) return
        call m%get_or_add(20_int64, idx)
        call check(error, idx == 8_int64, "get_or_add answers the stored value of a key it holds")
        if (allocated(error)) return
        call m%get_or_add(30_int32, idx32)
        call check(error, idx32 == 9_int32, "and so does the int32 spelling")
        if (allocated(error)) return
        call m%get_or_add_many([30_int64, 10_int64, 20_int64], codes, threads=1)
        call check(error, all(codes == [9_int64, 7_int64, 8_int64]), &
            "get_or_add_many answers the stored values of keys it holds")
        if (allocated(error)) return
        call check(error, m%nkeys() == 3_int64 .and. m%get(20_int64) == 8_int64, "and nothing was mutated")
    end subroutine test_sorted_get_or_add_present

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


    ! ---- The int32 and mixed-kind entry points ----

    !> Every `int32`-keyed scalar entry point answers exactly what its `int64` twin answers.
    !!
    !! The typed tier is a widen-and-forward layer: each `int32` specific copies the caller's keys
    !! or values into `int64` and calls the same worker the `int64` specific calls. That shape makes
    !! a defect in it silent in the worst way -- a widening that dropped the sign, transposed a
    !! mixed-kind pair, or forwarded the wrong argument answers a plausible number rather than
    !! failing -- so the assertion here is against the `int64` twin AND against the scan oracle,
    !! not against the same layer spelled differently. Negative keys are in the fixture because an
    !! `int(k, int64)` that went through an unsigned step would only show there.
    subroutine test_int32_scalar_entry_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m32, m64
        integer(int32) :: k32(6), v32(6), p32(9), a32(9), b32(9)
        integer(int64) :: k64(6), v64(6), p64(9), a64(9), b64(9), i, want
        logical :: mask(9)
        character(len=6) :: methods(3)
        integer :: t

        k32 = [-7_int32, 11_int32, 12_int32, 13_int32, 17_int32, 23_int32]
        v32 = [6_int32, 5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
        p32 = [-9_int32, -7_int32, 10_int32, 11_int32, 13_int32, 17_int32, 23_int32, 24_int32, 12_int32]
        mask = [.true., .false., .true., .true., .false., .true., .true., .false., .true.]
        k64 = int(k32, int64)
        v64 = int(v32, int64)
        p64 = int(p32, int64)
        methods = ["direct", "hash  ", "sorted"]
        do t = 1, 3
            ! The two mixed-kind build forms -- int32 keys with int64 values, and int64 keys with
            ! int32 values -- must produce the same map as each other and as the oracle.
            call m32%build(k32, v64, method=trim(methods(t)))
            call m64%build(k64, v32, method=trim(methods(t)))
            do i = 1_int64, 9_int64
                want = expected_value(k64, v64, p64(i))
                call check(error, m32%get(p64(i)) == want, &
                    "int32 keys with int64 values must store what the scan says, on " // trim(methods(t)))
                if (allocated(error)) return
                call check(error, m64%get(p64(i)) == want, &
                    "int64 keys with int32 values must store what the scan says, on " // trim(methods(t)))
                if (allocated(error)) return
                call check(error, m32%contains(p32(i)) .eqv. (want > 0_int64), &
                    "contains with an int32 key must agree with the scan, on " // trim(methods(t)))
                if (allocated(error)) return
            end do
            ! %get_many over int32 keys, into both answer kinds, against the int64-keyed twin.
            call m32%get_many(p32, a32)
            call m32%get_many(p32, a64)
            call m64%get_many(p64, b32)
            call m64%get_many(p64, b64)
            do i = 1_int64, 9_int64
                want = expected_value(k64, v64, p64(i))
                call check(error, int(a32(i), int64) == want .and. a64(i) == want .and. &
                    int(b32(i), int64) == want .and. b64(i) == want, &
                    "get_many must answer the scan whatever the key and answer kinds, on " // &
                    trim(methods(t)))
                if (allocated(error)) return
            end do
            ! The same four shapes with a mask: a masked row answers 0 without being looked up.
            call m32%get_many(p32, a32, valid=mask)
            call m32%get_many(p32, a64, valid=mask)
            call m64%get_many(p64, b32, valid=mask)
            call m64%get_many(p64, b64, valid=mask)
            do i = 1_int64, 9_int64
                want = 0_int64
                if (mask(i)) want = expected_value(k64, v64, p64(i))
                call check(error, int(a32(i), int64) == want .and. a64(i) == want .and. &
                    int(b32(i), int64) == want .and. b64(i) == want, &
                    "a masked get_many must answer 0 for a masked row and the scan for the rest, on " // &
                    trim(methods(t)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_int32_scalar_entry_points

    !> Every `int32` composite entry point answers exactly what its `int64` twin answers.
    !!
    !! `test_int32_scalar_entry_points`' argument, one component wider: the tuple forms widen into
    !! a fixed stack buffer and re-check the width, so a transposed or short-copied tuple is the
    !! failure to look for, and only a fixture whose components differ per row can see it.
    !! `method="sorted"` is absent because it rejects composite keys by design.
    subroutine test_int32_tuple_entry_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m32, m64
        integer(int32) :: k32(6, 2), v32(6), p32(8, 2), a32(8), b32(8)
        integer(int64) :: k64(6, 2), v64(6), p64(8, 2), a64(8), b64(8), i, want
        logical :: mask(8)
        character(len=6) :: methods(2)
        integer :: t

        k32(:, 1) = [-3_int32, -3_int32, 1_int32, 1_int32, 4_int32, 4_int32]
        k32(:, 2) = [5_int32, 9_int32, 5_int32, 9_int32, 5_int32, 9_int32]
        v32 = [6_int32, 5_int32, 4_int32, 3_int32, 2_int32, 1_int32]
        ! Probes: four stored tuples, and four misses whose components are each stored but not
        ! together -- the only shape that catches a lookup comparing one component and stopping.
        p32(:, 1) = [-3_int32, 1_int32, 4_int32, 4_int32, -3_int32, 1_int32, 9_int32, 4_int32]
        p32(:, 2) = [5_int32, 9_int32, 5_int32, 9_int32, 7_int32, 4_int32, 5_int32, 99_int32]
        mask = [.true., .false., .true., .true., .false., .true., .true., .false.]
        k64 = int(k32, int64)
        v64 = int(v32, int64)
        p64 = int(p32, int64)
        methods = ["direct", "hash  "]
        do t = 1, 2
            ! build from int32 tuples with no values: the row numbers are the values.
            call m32%build(k32, method=trim(methods(t)))
            do i = 1_int64, 6_int64
                call check(error, m32%get(k64(i, :)) == i, &
                    "an int32 composite build must number its rows, on " // trim(methods(t)))
                if (allocated(error)) return
            end do
            call check(error, m32%ncomponents() == 2, &
                "an int32 composite build must report two components, on " // trim(methods(t)))
            if (allocated(error)) return
            ! The two mixed-kind composite build forms, against each other and the scan.
            call m32%build(k32, v64, method=trim(methods(t)))
            call m64%build(k64, v32, method=trim(methods(t)))
            do i = 1_int64, 8_int64
                want = expected_value_n(k64, v64, p64(i, :))
                call check(error, m32%get(p64(i, :)) == want .and. m64%get(p64(i, :)) == want, &
                    "the mixed-kind composite builds must agree with the scan, on " // trim(methods(t)))
                if (allocated(error)) return
                call check(error, m32%contains(p32(i, :)) .eqv. (want > 0_int64), &
                    "contains with an int32 tuple must agree with the scan, on " // trim(methods(t)))
                if (allocated(error)) return
                call check(error, m32%contains(p64(i, :)) .eqv. (want > 0_int64), &
                    "contains with an int64 tuple must agree with the scan, on " // trim(methods(t)))
                if (allocated(error)) return
            end do
            call m32%get_many(p32, a32)
            call m32%get_many(p32, a64)
            call m64%get_many(p64, b32)
            call m64%get_many(p64, b64)
            do i = 1_int64, 8_int64
                want = expected_value_n(k64, v64, p64(i, :))
                call check(error, int(a32(i), int64) == want .and. a64(i) == want .and. &
                    int(b32(i), int64) == want .and. b64(i) == want, &
                    "composite get_many must answer the scan whatever the kinds, on " // &
                    trim(methods(t)))
                if (allocated(error)) return
            end do
            call m32%get_many(p32, a32, valid=mask)
            call m32%get_many(p32, a64, valid=mask)
            call m64%get_many(p64, b32, valid=mask)
            call m64%get_many(p64, b64, valid=mask)
            do i = 1_int64, 8_int64
                want = 0_int64
                if (mask(i)) want = expected_value_n(k64, v64, p64(i, :))
                call check(error, int(a32(i), int64) == want .and. a64(i) == want .and. &
                    int(b32(i), int64) == want .and. b64(i) == want, &
                    "a masked composite get_many must answer 0 for a masked row, on " // &
                    trim(methods(t)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_int32_tuple_entry_points

    !> `%set`, `%get_or_add` and `%remove` answer alike in every key/value kind combination.
    !!
    !! The mutation tier has the same widen-and-forward shape as the lookup tier, and one extra
    !! thing to get wrong: `%set` raises the `%get_or_add` watermark, so an `int32` form that
    !! forwarded its value unwidened would reissue an index that is already in use. The final
    !! `%get_or_add` of a new key is what sees that.
    subroutine test_int32_mutation_entry_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int32) :: i32
        integer(int64) :: i64
        logical :: found

        call m%init(method="hash")
        call m%set(-4_int32, 7_int32)   ! int32 key, int32 value
        call m%set(11_int32, 8_int64)   ! int32 key, int64 value
        call m%set(12_int64, 9_int32)   ! int64 key, int32 value
        call check(error, m%get(-4_int64) == 7_int64 .and. m%get(11_int64) == 8_int64 .and. &
            m%get(12_int64) == 9_int64, "every set kind combination must store the value given")
        if (allocated(error)) return
        ! get_or_add of a stored key returns what set stored, in either answer kind.
        call m%get_or_add(-4_int32, i32)
        call check(error, i32 == 7_int32, "get_or_add: int32 key, int32 answer")
        if (allocated(error)) return
        call m%get_or_add(11_int32, i64)
        call check(error, i64 == 8_int64, "get_or_add: int32 key, int64 answer")
        if (allocated(error)) return
        call m%get_or_add(12_int64, i32)
        call check(error, i32 == 9_int32, "get_or_add: int64 key, int32 answer")
        if (allocated(error)) return
        ! A new key takes the next index above the watermark the three sets raised, which is 9.
        call m%get_or_add(30_int32, i32)
        call check(error, i32 == 10_int32, &
            "an int32 set must raise the watermark, so a new key numbers above it")
        if (allocated(error)) return
        call m%remove(30_int32, found=found)
        call check(error, found .and. m%get(30_int64) == 0_int64, &
            "remove with an int32 key must forget exactly that key")
        if (allocated(error)) return
        call m%remove(31_int32, found=found)
        call check(error, .not. found, "remove of an absent int32 key must report found=.false.")
        if (allocated(error)) return
        call check(error, m%nkeys() == 3_int64, "the other three keys must survive")
    end subroutine test_int32_mutation_entry_points

    !> The composite `%set`, `%get_or_add` and `%remove` forms, in every kind combination.
    !!
    !! `test_int32_mutation_entry_points` for tuples. The tuples share a first component on
    !! purpose: a widening that wrote only the first component would still answer for the first
    !! key and is only visible once a second tuple shares it.
    subroutine test_int32_tuple_mutation_entry_points(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int32) :: i32
        integer(int64) :: i64
        logical :: found

        call m%init(ncomp=2, method="hash")
        call m%set([-1_int32, 2_int32], 5_int32)   ! int32 tuple, int32 value
        call m%set([-1_int32, 3_int32], 6_int64)   ! int32 tuple, int64 value
        call m%set([-1_int64, 4_int64], 7_int32)   ! int64 tuple, int32 value
        call check(error, m%get([-1_int64, 2_int64]) == 5_int64 .and. &
            m%get([-1_int64, 3_int64]) == 6_int64 .and. m%get([-1_int64, 4_int64]) == 7_int64, &
            "every composite set kind combination must store the value given")
        if (allocated(error)) return
        call m%get_or_add([-1_int32, 2_int32], i32)
        call check(error, i32 == 5_int32, "composite get_or_add: int32 tuple, int32 answer")
        if (allocated(error)) return
        call m%get_or_add([-1_int32, 3_int32], i64)
        call check(error, i64 == 6_int64, "composite get_or_add: int32 tuple, int64 answer")
        if (allocated(error)) return
        call m%get_or_add([-1_int64, 4_int64], i32)
        call check(error, i32 == 7_int32, "composite get_or_add: int64 tuple, int32 answer")
        if (allocated(error)) return
        call m%get_or_add([-1_int32, 9_int32], i32)
        call check(error, i32 == 8_int32, &
            "an int32 composite set must raise the watermark, so a new tuple numbers above it")
        if (allocated(error)) return
        call m%remove([-1_int32, 9_int32], found=found)
        call check(error, found .and. m%get([-1_int64, 9_int64]) == 0_int64, &
            "remove with an int32 tuple must forget exactly that tuple")
        if (allocated(error)) return
        call m%remove([-1_int32, 99_int32], found=found)
        call check(error, .not. found, "remove of an absent int32 tuple must report found=.false.")
        if (allocated(error)) return
        call check(error, m%nkeys() == 3_int64, "the other three tuples must survive")
    end subroutine test_int32_tuple_mutation_entry_points

    !> `%get_or_add_many` codes identically whatever the key rank and the two kinds are.
    !!
    !! The four shapes are asserted against a loop of the scalar `%get_or_add` over a map built the
    !! same way, so a bulk form that numbered in a different order, or lost the mask, fails rather
    !! than merely differing from its twin.
    subroutine test_int32_get_or_add_many_kinds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m, one
        integer(int32) :: k32(7), t32(7, 2), c32(7)
        integer(int64) :: k64(7), t64(7, 2), c64(7), want(7), i
        logical :: mask(7)

        k32 = [5_int32, -2_int32, 5_int32, 8_int32, -2_int32, 13_int32, 8_int32]
        mask = [.true., .true., .false., .true., .true., .true., .false.]
        k64 = int(k32, int64)
        ! The scalar loop's answer, which is the order the bulk form must reproduce.
        call one%init(method="hash")
        do i = 1_int64, 7_int64
            want(i) = 0_int64
            if (mask(i)) call one%get_or_add(k64(i), want(i))
        end do
        call m%init(method="hash")
        call m%get_or_add_many(k32, c64, valid=mask)
        call check(error, all(c64 == want), &
            "get_or_add_many: int32 keys, int64 codes must equal a loop of get_or_add")
        if (allocated(error)) return
        ! The same keys as 1-tuples, through the three rank-2 kind combinations.
        t32(:, 1) = k32
        t32(:, 2) = [1_int32, 1_int32, 1_int32, 2_int32, 1_int32, 2_int32, 2_int32]
        t64 = int(t32, int64)
        call one%init(ncomp=2, method="hash")
        do i = 1_int64, 7_int64
            want(i) = 0_int64
            if (mask(i)) call one%get_or_add(t64(i, :), want(i))
        end do
        call m%init(ncomp=2, method="hash")
        call m%get_or_add_many(t32, c32, valid=mask)
        call check(error, all(int(c32, int64) == want), &
            "get_or_add_many: int32 tuples, int32 codes must equal a loop of get_or_add")
        if (allocated(error)) return
        call m%init(ncomp=2, method="hash")
        call m%get_or_add_many(t32, c64, valid=mask)
        call check(error, all(c64 == want), &
            "get_or_add_many: int32 tuples, int64 codes must equal a loop of get_or_add")
        if (allocated(error)) return
        call m%init(ncomp=2, method="hash")
        call m%get_or_add_many(t64, c32, valid=mask)
        call check(error, all(int(c32, int64) == want), &
            "get_or_add_many: int64 tuples, int32 codes must equal a loop of get_or_add")
    end subroutine test_int32_get_or_add_many_kinds

    !> A composite DIRECT map is mutable inside the key ranges it was built for.
    !!
    !! The direct backend reaches a tuple's slot through the mixed-radix offset rather than a hash,
    !! and `%set`/`%remove` are the only callers of it that write. An offset that used the wrong
    !! stride would still land inside the array and overwrite a DIFFERENT key's slot -- silent, and
    !! visible only by checking the neighbours after the write, which is what this does.
    subroutine test_direct_composite_mutation(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(4, 2)
        logical :: found

        keys(:, 1) = [1_int64, 1_int64, 2_int64, 2_int64]
        keys(:, 2) = [10_int64, 11_int64, 10_int64, 11_int64]
        call m%build(keys, method="direct")
        ! Replace one stored tuple; every other slot must be untouched.
        call m%set([2_int64, 10_int64], 99_int64)
        call check(error, m%get([2_int64, 10_int64]) == 99_int64 .and. &
            m%get([1_int64, 10_int64]) == 1_int64 .and. m%get([1_int64, 11_int64]) == 2_int64 .and. &
            m%get([2_int64, 11_int64]) == 4_int64, &
            "a direct composite set must write only the slot it addressed")
        if (allocated(error)) return
        call check(error, m%nkeys() == 4_int64, "replacing a stored tuple must not add a key")
        if (allocated(error)) return
        ! A slot inside the ranges but never built: the count rises.
        call m%remove([1_int64, 11_int64], found=found)
        call check(error, found .and. m%get([1_int64, 11_int64]) == 0_int64 .and. &
            m%nkeys() == 3_int64, "a direct composite remove must forget exactly that tuple")
        if (allocated(error)) return
        call m%set([1_int64, 11_int64], 42_int64)
        call check(error, m%get([1_int64, 11_int64]) == 42_int64 .and. m%nkeys() == 4_int64, &
            "setting an empty in-range slot must add a key back")
        if (allocated(error)) return
        call m%remove([1_int64, 11_int64], found=found)
        call m%remove([1_int64, 11_int64], found=found)
        call check(error, .not. found, "removing an already-removed tuple must report found=.false.")
        if (allocated(error)) return
        ! Out of range in the SECOND component only: the offset must refuse it rather than
        ! wrapping into the next first-component block.
        call m%remove([1_int64, 12_int64], found=found)
        call check(error, .not. found, &
            "a tuple outside the built ranges must not be found by remove")
        if (allocated(error)) return
        call m%remove([1_int32, 12_int32], found=found)
        call check(error, .not. found, "nor by the int32 spelling of it")
    end subroutine test_direct_composite_mutation

    !> `%remove` on a map that was never built reports "not there" instead of aborting.
    !!
    !! A map with no components has no storage to search, and `found=` is the caller's statement
    !! that an absent key is acceptable. The early return this takes is separate from the one an
    !! absent key in a built map takes, and only an unbuilt map reaches it.
    subroutine test_remove_on_unbuilt_map(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        logical :: found

        call m%remove([1_int64, 2_int64], found=found)
        call check(error, .not. found, "an unbuilt map must report an int64 tuple as absent")
        if (allocated(error)) return
        call m%remove([1_int32, 2_int32], found=found)
        call check(error, .not. found, "an unbuilt map must report an int32 tuple as absent")
        if (allocated(error)) return
        call check(error, m%ncomponents() == 0, "and must still be unbuilt afterwards")
    end subroutine test_remove_on_unbuilt_map

    !> A rank-2 key array with ONE column builds the same map a rank-1 array does.
    !!
    !! A one-column composite build is the shape a caller gets from slicing a table, and it takes
    !! the composite build path with `ncomp == 1` -- a path that has to fill the scalar range
    !! (`kmin1`/`kmax1`) as well as the per-component geometry, because the resulting map still has
    !! to answer a scalar `%get(k)`. That cross-fill is what this asserts; `sorted` is included
    !! because it reads the single column directly and fills the range last.
    subroutine test_rank2_single_column_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m2, m1
        integer(int64) :: keys(5, 1), flat(5), probes(7), i
        character(len=6) :: methods(3)
        integer :: t

        flat = [3_int64, 4_int64, 5_int64, 9_int64, 12_int64]
        keys(:, 1) = flat
        probes = [2_int64, 3_int64, 5_int64, 6_int64, 9_int64, 12_int64, 13_int64]
        methods = ["direct", "hash  ", "sorted"]
        do t = 1, 3
            call m2%build(keys, method=trim(methods(t)))
            call m1%build(flat, method=trim(methods(t)))
            call check(error, m2%ncomponents() == 1, &
                "a one-column composite build has one component, on " // trim(methods(t)))
            if (allocated(error)) return
            do i = 1_int64, 7_int64
                call check(error, m2%get(probes(i)) == m1%get(probes(i)), &
                    "a one-column build must answer a scalar get as a rank-1 build does, on " // &
                    trim(methods(t)))
                if (allocated(error)) return
                call check(error, m2%get([probes(i)]) == m1%get([probes(i)]), &
                    "and must answer a 1-tuple get the same way, on " // trim(methods(t)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_rank2_single_column_build

    !> A sorted build takes `valid=` and `values=` together.
    !!
    !! The sorted backend compacts the unmasked rows into fresh arrays before sorting them, and
    !! that compaction has two arms -- carry the caller's value across, or number the row. Only a
    !! build given both arguments reaches the first, and only a fixture whose values are neither
    !! the row numbers nor in key order can tell the two apart.
    subroutine test_sorted_build_masked_values(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(6), values(6), i
        logical :: mask(6)

        keys = [50_int64, 10_int64, 40_int64, 20_int64, 30_int64, 60_int64]
        values = [11_int64, 12_int64, 13_int64, 14_int64, 15_int64, 16_int64]
        mask = [.true., .true., .false., .true., .true., .false.]
        call m%build(keys, values, method="sorted", valid=mask)
        call check(error, m%nkeys() == 4_int64, "a masked sorted build stores the unmasked rows")
        if (allocated(error)) return
        do i = 1_int64, 6_int64
            if (mask(i)) then
                call check(error, m%get(keys(i)) == values(i), &
                    "a masked sorted build must carry the caller's value across")
            else
                call check(error, m%get(keys(i)) == 0_int64, &
                    "a masked sorted build must not store a masked row")
            end if
            if (allocated(error)) return
        end do
    end subroutine test_sorted_build_masked_values

    !> `%reset` empties a map on every backend while keeping its allocation.
    !!
    !! `test_reset_keeps_storage` covers the hash backend. The direct backend empties by zeroing
    !! its slot array instead, a separate arm, and a reset that skipped it would leave every key
    !! still findable -- an emptied map that answers is the silent failure worth a test.
    subroutine test_reset_every_backend(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got
        integer(int64) :: keys(4), bytes, i
        character(len=6) :: methods(3)
        integer :: t

        keys = [2_int64, 3_int64, 5_int64, 7_int64]
        methods = ["direct", "hash  ", "sorted"]
        do t = 1, 3
            call m%build(keys, method=trim(methods(t)))
            bytes = m%memory_bytes()
            call m%reset()
            call check(error, m%nkeys() == 0_int64, &
                "reset must empty the map on " // trim(methods(t)))
            if (allocated(error)) return
            do i = 1_int64, 4_int64
                call check(error, m%get(keys(i)) == 0_int64, &
                    "no key may still answer after reset on " // trim(methods(t)))
                if (allocated(error)) return
            end do
            call check(error, m%memory_bytes() == bytes, &
                "reset must keep the allocation on " // trim(methods(t)))
            if (allocated(error)) return
            call m%get_method(got)
            call check(error, got == trim(methods(t)), &
                "and must keep the backend on " // trim(methods(t)))
            if (allocated(error)) return
        end do
    end subroutine test_reset_every_backend

    !> A rank-2 `%get_many` over a ONE-column map answers what the rank-1 form answers.
    !!
    !! A one-column composite map is the scalar table underneath, and each bulk worker has an arm
    !! that says so: the hash arm hands `keys(:, 1)` to the SCALAR block kernel rather than the
    !! tuple one, and the sorted map falls through to the general per-row arm. Neither is reached
    !! by a rank-1 call or by a two-component one, and an arm that gathered the wrong column would
    !! answer a plausible index rather than failing. All four key/answer kind combinations are
    !! driven because the widening happens inside each worker, not before it.
    subroutine test_rank2_single_column_get_many(error)
        type(pf_index_map) :: m2, m1
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        integer(int64) :: keys(9, 1), flat(9), p64(11, 1), f64(11), a64(11), b64(11), i, want
        integer(int32) :: k32(9, 1), q32(11, 1), a32(11), b32(11)
        logical :: mask(11)
        character(len=6) :: methods(3)
        integer :: t

        flat = [3_int64, 4_int64, 5_int64, 9_int64, 12_int64, 20_int64, 21_int64, 25_int64, 30_int64]
        ! Probes: hits and misses, below the range, above it, and interleaved between stored keys.
        f64 = [2_int64, 3_int64, 5_int64, 6_int64, 9_int64, 12_int64, 13_int64, 21_int64, 25_int64, &
            30_int64, 31_int64]
        mask = [.true., .false., .true., .true., .false., .true., .true., .false., .true., .true., .false.]
        keys(:, 1) = flat
        p64(:, 1) = f64
        k32 = int(keys, int32)
        q32 = int(p64, int32)
        methods = ["direct", "hash  ", "sorted"]
        do t = 1, 3
            call m2%build(keys, method=trim(methods(t)))
            call m1%build(flat, method=trim(methods(t)))
            ! The rank-1 form is the oracle: it is the same map, reached by the path this suite
            ! already covers everywhere else.
            call m1%get_many(f64, b64)
            call m2%get_many(p64, a64)
            call check(error, all(a64 == b64), &
                "a one-column bulk lookup must equal the rank-1 one, on " // trim(methods(t)))
            if (allocated(error)) return
            call m2%get_many(q32, a64)
            call check(error, all(a64 == b64), &
                "and with int32 keys, on " // trim(methods(t)))
            if (allocated(error)) return
            call m2%get_many(p64, a32)
            call check(error, all(int(a32, int64) == b64), &
                "and with int32 answers, on " // trim(methods(t)))
            if (allocated(error)) return
            call m2%get_many(q32, a32)
            call check(error, all(int(a32, int64) == b64), &
                "and with both int32, on " // trim(methods(t)))
            if (allocated(error)) return
            ! The same four shapes with a mask: a masked row answers 0 and is not looked up.
            call m2%get_many(p64, a64, valid=mask)
            call m2%get_many(q32, b64, valid=mask)
            call m2%get_many(p64, a32, valid=mask)
            call m2%get_many(q32, b32, valid=mask)
            do i = 1_int64, 11_int64
                want = 0_int64
                if (mask(i)) want = m1%get(f64(i))
                call check(error, a64(i) == want .and. b64(i) == want .and. &
                    int(a32(i), int64) == want .and. int(b32(i), int64) == want, &
                    "a masked one-column bulk lookup must answer 0 for a masked row, on " // &
                    trim(methods(t)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_rank2_single_column_get_many

    !> A one-column rank-2 build takes `valid=` on every backend, sorted included.
    !!
    !! The sorted arm of the composite build compacts the unmasked rows out of the single column
    !! before sorting them, which is a different call from the one an unmasked build makes.
    subroutine test_rank2_single_column_masked_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: keys(6, 1), i
        logical :: mask(6)
        character(len=6) :: methods(3)
        integer :: t

        keys(:, 1) = [50_int64, 10_int64, 40_int64, 20_int64, 30_int64, 60_int64]
        mask = [.true., .true., .false., .true., .true., .false.]
        methods = ["direct", "hash  ", "sorted"]
        do t = 1, 3
            call m%build(keys, method=trim(methods(t)), valid=mask)
            call check(error, m%nkeys() == 4_int64, &
                "a masked one-column build stores the unmasked rows, on " // trim(methods(t)))
            if (allocated(error)) return
            do i = 1_int64, 6_int64
                if (mask(i)) then
                    call check(error, m%get(keys(i, 1)) == i, &
                        "and numbers each by its own row, on " // trim(methods(t)))
                else
                    call check(error, m%get(keys(i, 1)) == 0_int64, &
                        "and stores no masked row, on " // trim(methods(t)))
                end if
                if (allocated(error)) return
            end do
        end do
    end subroutine test_rank2_single_column_masked_build

    !> `method="auto"` spelled out is the same as leaving `method=` absent.
    !!
    !! The token is accepted and resolves to the automatic choice rather than to a backend of its
    !! own; a resolver that fell through to a fixed backend for it would silently stop honouring
    !! the heuristic for every caller who spells the default.
    subroutine test_explicit_auto_token(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: spelt, absent
        character(len=:), allocatable :: a, b
        integer(int64) :: dense(6), sparse(4), i

        dense = [1_int64, 2_int64, 3_int64, 4_int64, 5_int64, 6_int64]
        sparse = [1_int64, 10_int64**9, 2_int64 * 10_int64**9, 3_int64 * 10_int64**9]
        call spelt%build(dense, method="auto")
        call absent%build(dense)
        call spelt%get_method(a)
        call absent%get_method(b)
        call check(error, a == b, "method=""auto"" must choose what an absent method= chooses")
        if (allocated(error)) return
        do i = 1_int64, 6_int64
            call check(error, spelt%get(dense(i)) == i, "and must answer every key")
            if (allocated(error)) return
        end do
        ! And on a fixture the heuristic sends the other way, so the assertion is not about one
        ! backend happening to be chosen twice.
        call spelt%build(sparse, method="AUTO")
        call absent%build(sparse)
        call spelt%get_method(a)
        call absent%get_method(b)
        call check(error, a == b, "the token is case-insensitive and still chooses automatically")
        if (allocated(error)) return
        call check(error, a /= "direct", "fixture: this key set must not be dense enough for direct")
    end subroutine test_explicit_auto_token

    !> A map started by `%init` but never filled answers 0 rather than probing an absent table.
    !!
    !! `%init` with no `capacity=` leaves the hash backend selected with no slots allocated, so
    !! every lookup before the first insert reaches an arm no other state reaches. It must answer
    !! "not found", not read the unallocated table.
    subroutine test_init_before_first_insert(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: got

        call m%init(method="hash")
        call m%get_method(got)
        call check(error, got == "hash", "%init selects the hash backend")
        if (allocated(error)) return
        call check(error, m%nkeys() == 0_int64, "and holds no keys yet")
        if (allocated(error)) return
        call check(error, m%get(1_int64) == 0_int64, &
            "a lookup before the first insert must answer 0")
        if (allocated(error)) return
        call check(error, m%get([1_int64]) == 0_int64, "and so must the 1-tuple spelling")
        if (allocated(error)) return
        call check(error, .not. m%contains(1_int64), "and contains must agree")
        if (allocated(error)) return
        ! And the map is usable immediately afterwards.
        call m%set(1_int64, 5_int64)
        call check(error, m%get(1_int64) == 5_int64, "the map accepts a key straight after")
    end subroutine test_init_before_first_insert

    ! gcov attribution artifact: an `end module` line is not a statement and reports 0 hits.
end module test_index ! GCOVR_EXCL_LINE
