!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for `parquet_index`'s STRING keys: the string forms of `pf_index_map` and
!> `pf_index_multimap`, the store beside the map, and the `(hash, occurrence)` chain.
!!
!! **The oracles are independent of the string map.** A scalar answer is checked against a
!! linear scan of the fixture written out below; a bulk CSR answer against `pf_match_all` over
!! the same columns, the sort engine's m:m match, whose string equality (exact bytes, `memcmp`
!! order) is the one the map must reproduce element for element.
!!
!! **The collision chain is reached on purpose, through the debug hook.** Two distinct strings
!! share a 64-bit hash about once in 2**64 probes, so the code that handles a genuine collision
!! -- the second and later occurrences of a hash, their lookup and their removal with the chain
!! kept dense -- would otherwise never run. `parquet_debug_set_index_string_hash_bits(2)` narrows
!! every hash to four values, so sixty keys collide fifteen deep; and because that hook is
!! process-global and read on every hash, **this suite is registered SERIALLY**
!! (`suite_is_safe_to_parallelize` in `test/test_runner_support.f90` excludes `index_strings`):
!! a test setting the hook while a sibling built a map would hand that sibling a map whose keys
!! hash differently at lookup than at build. Every test that sets the hook restores it on every
!! exit path, through a wrapper that resets it after the checked body returns.
!!
!! **Every fixture's first element is its shortest**, and elements are blank-padded to one
!! declared length, so a key sized from the first element or left untrimmed would fail here
!! (CLAUDE.md, "sized/typed from the first element"). Fixture values are row-distinct for the
!! reason `test_index.f90` gives.
!!
!! This suite touches no file; its one piece of process-global state is the hook above.
module test_index_strings
    use testdrive, only: new_unittest, unittest_type, error_type, check, skip_test
    use parquet_index
    use parquet_strings, only: parquet_string_column
    use parquet_sorting, only: pf_match_all
    use iso_fortran_env, only: int32, int64, real64
    implicit none
    private

    public :: collect_tests_index_strings

    !> The fixture every map test starts from: seven distinct keys, the first the shortest, one
    !! of them the empty string, padded to a common length so that trimming is exercised.
    character(len=8), parameter :: KEYS(7) = [character(len=8) :: &
        "a", "bb", "", "dddd", "e e", "ff ff ff", "g"]

contains

    !> Registers every test in this suite.
    subroutine collect_tests_index_strings(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("a string map answers a scan: hits, misses, exact bytes", test_str_build_and_lookup), &
            new_unittest("a character array and a string column build equal maps", &
                test_str_column_equals_array), &
            new_unittest("keys() round-trips the stored strings", test_str_keys_round_trip), &
            new_unittest("get_many: nulls and masked rows answer 0, int32 and int64 agree", &
                test_str_get_many_forms), &
            new_unittest("set, get_or_add, get_or_add_many, remove and reset", test_str_mutation), &
            new_unittest("a forced hash collision chain answers every key and survives removal", &
                test_str_collision_chain), &
            new_unittest("near-identical strings do not cluster", test_str_clustering), &
            new_unittest("an unbuilt map answers 0, empty, and not found", test_str_unbuilt), &
            new_unittest("init(strings=) and reserve start an incremental string map", &
                test_str_init_and_reserve), &
            new_unittest("a string multimap answers a scan of a fixture with repeats", &
                test_str_multimap_scan), &
            new_unittest("probe_many over strings equals pf_match_all", test_str_probe_equals_match_all), &
            new_unittest("the multimap's bulk forms and int32 answers agree with the scalar ones", &
                test_str_multimap_bulk_forms), &
            new_unittest("a heavily repeated key set takes the right-sized rebuild and keeps its ids", &
                test_str_rightsize_rebuild), &
            new_unittest("edge cases: no keys, one empty key, all keys equal", test_str_edges), &
            new_unittest("a partitioned string build answers every key, from an array and a column", &
                test_str_partitioned_build), &
            new_unittest("a threaded string get_or_add_many codes densely and lists keys in code order", &
                test_str_partitioned_get_or_add_many), &
            new_unittest("colliding strings send the threaded passes to the serial loop, which answers", &
                test_str_collision_threaded) &
            ]
    end subroutine collect_tests_index_strings

    ! ---- helpers -------------------------------------------------------------------------------

    !> The fixture as a string column, verbatim (so trailing blanks would survive) -- built from
    !! the trimmed elements so that the two forms hold the same keys.
    subroutine fixture_column(sc)
        type(parquet_string_column), intent(out) :: sc !! receives the seven keys.
        integer :: i

        call sc%clear()
        do i = 1, size(KEYS)
            call sc%append_string(trim(KEYS(i)))
        end do
    end subroutine fixture_column

    !> A linear scan: the 1-based position of `key` in the trimmed fixture, or 0.
    pure function scan_fixture(key) result(pos)
        character(len=*), intent(in) :: key !! the key, exact bytes.
        integer(int64) :: pos               !! its position, or 0.
        integer :: i

        pos = 0_int64
        do i = 1, size(KEYS)
            if (len_trim(KEYS(i)) == len(key)) then
                if (KEYS(i)(1:len(key)) == key) then
                    pos = int(i, int64)
                    return
                end if
            end if
        end do
    end function scan_fixture

    !> A column of `n` distinct identifiers `k<i>`, in order.
    subroutine ident_column(n, sc)
        integer(int64), intent(in) :: n                !! how many.
        type(parquet_string_column), intent(out) :: sc !! receives them.
        integer(int64) :: i
        character(len=16) :: txt

        call sc%clear()
        do i = 1_int64, n
            write (txt, "(a,i0)") "k", i
            call sc%append_string(trim(txt))
        end do
    end subroutine ident_column

    ! ---- pf_index_map --------------------------------------------------------------------------

    !> Build from the padded character array; every key found at its position, every miss 0,
    !! and the exact-bytes rule on the scalar forms.
    subroutine test_str_build_and_lookup(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        character(len=:), allocatable :: tok
        integer :: i

        call m%build(KEYS)
        call check(error, m%nkeys() == 7_int64, "seven keys stored")
        if (allocated(error)) return
        call check(error, m%ncomponents() == 1, "a string map reports one component per key")
        if (allocated(error)) return
        call m%get_method(tok)
        call check(error, tok == "hash", "a string map is always hashed")
        if (allocated(error)) return
        do i = 1, size(KEYS)
            call check(error, m%get(trim(KEYS(i))) == int(i, int64), &
                "every key is found at its position: " // trim(KEYS(i)))
            if (allocated(error)) return
            call check(error, m%contains(trim(KEYS(i))), "contains agrees for " // trim(KEYS(i)))
            if (allocated(error)) return
        end do
        call check(error, m%get("") == 3_int64, "the empty string is an ordinary key")
        if (allocated(error)) return
        call check(error, m%get("zz") == 0_int64 .and. m%get("b") == 0_int64 .and. m%get("bbb") == 0_int64, &
            "a prefix, an extension and an absent key all miss")
        if (allocated(error)) return
        ! Exact bytes on the scalar forms: the array's elements were trimmed on the way in, so
        ! the padded spelling of a stored key is a DIFFERENT key, as it is to pf_in and pf_match.
        call check(error, m%get("bb ") == 0_int64, "a scalar key is taken as written: 'bb ' is not 'bb'")
        if (allocated(error)) return
        call check(error, m%get(KEYS(2)) == 0_int64 .and. m%get(trim(KEYS(2))) == 2_int64, &
            "a padded variable needs trim(); the trimmed form finds")
        if (allocated(error)) return
        call check(error, m%get("e e") == 5_int64 .and. m%get("ff ff ff") == 6_int64, &
            "interior blanks are significant and preserved")
        if (allocated(error)) return
        call check(error, .not. m%contains("E E"), "case is significant")
        if (allocated(error)) return
        call check(error, m%memory_bytes() > 0_int64, "the store and the table are counted")
    end subroutine test_str_build_and_lookup

    !> The same strings from a parquet_string_column build the same map: same values for every
    !! key, same misses, same bulk answers; and a null element is skipped, keeping row numbers.
    subroutine test_str_column_equals_array(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: ma, mc, mn
        type(parquet_string_column) :: sc, withnull
        character(len=6) :: probes(9)
        integer(int64) :: va(9), vc(9)
        integer :: i

        call fixture_column(sc)
        call ma%build(KEYS)
        call mc%build(sc)
        call check(error, mc%nkeys() == ma%nkeys(), "the same key count from a column")
        if (allocated(error)) return
        probes = [character(len=6) :: "a", "bb", "", "dddd", "e e", "zz", "g", "a ", "ff"]
        call ma%get_many(probes, va)
        call mc%get_many(probes, vc)
        call check(error, all(va == vc), "get_many over a character array agrees between the two maps")
        if (allocated(error)) return
        do i = 1, size(probes)
            call check(error, va(i) == scan_fixture(trim(probes(i))), &
                "get_many agrees with the scan at probe " // trim(probes(i)))
            if (allocated(error)) return
        end do
        call check(error, count(va == 0_int64) == 2, "the probe set carries misses (vacuity guard)")
        if (allocated(error)) return
        ! A column with a null in the middle: the null is skipped and the OTHER rows keep their
        ! row numbers, exactly as valid= does for the integer forms.
        call withnull%clear()
        call withnull%append_string("x")
        call withnull%append_null()
        call withnull%append_string("y")
        call mn%build(withnull)
        call check(error, mn%nkeys() == 2_int64, "a null element is not a key")
        if (allocated(error)) return
        call check(error, mn%get("x") == 1_int64 .and. mn%get("y") == 3_int64, &
            "rows after a null keep their original row numbers")
        if (allocated(error)) return
        call check(error, mn%get("") == 0_int64, "a null element is not the empty string either")
    end subroutine test_str_column_equals_array

    !> `%keys` returns exactly the stored strings, and pairs with `%get_many` to give the values.
    subroutine test_str_keys_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m, back
        type(parquet_string_column) :: list
        integer(int64), allocatable :: vals(:)
        integer(int64) :: i
        logical :: seen(7)

        call m%build(KEYS)
        call m%keys(list)
        call check(error, list%size() == 7_int64, "keys() returns one entry per stored key")
        if (allocated(error)) return
        call check(error, list%null_count() == 0_int64, "keys() holds no null")
        if (allocated(error)) return
        ! Set-equality rather than order, through the values each key pairs with.
        allocate(vals(list%size()))
        call m%get_many(list, vals)
        seen = .false.
        do i = 1_int64, list%size()
            call check(error, vals(i) >= 1_int64 .and. vals(i) <= 7_int64, "every listed key is stored")
            if (allocated(error)) return
            seen(vals(i)) = .true.
        end do
        call check(error, all(seen), "keys() lists every stored key exactly once")
        if (allocated(error)) return
        ! And a map built from the list finds every original key.
        call back%build(list)
        do i = 1_int64, 7_int64
            call check(error, back%get(trim(KEYS(i))) > 0_int64, "a map rebuilt from keys() finds " // trim(KEYS(i)))
            if (allocated(error)) return
        end do
        call check(error, back%get("") > 0_int64, "the empty string survives the round trip")
    end subroutine test_str_keys_round_trip

    !> The bulk lookup: a masked row and a null element answer 0 unprobed, the int32 and int64
    !! answer kinds agree, and an explicit team agrees with the serial run.
    subroutine test_str_get_many_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        type(parquet_string_column) :: keys, probes
        integer(int64), parameter :: n = 5000_int64
        integer(int64), allocatable :: v64(:), serial(:)
        integer(int32), allocatable :: v32(:)
        logical, allocatable :: mask(:)
        integer(int64) :: i, j, nhit
        character(len=16) :: txt

        call ident_column(n, keys)
        call m%build(keys)
        ! Probes: every third one altered to miss, every fifth one null.
        call probes%clear()
        do i = 1_int64, n
            j = 1_int64 + mod(i * 7919_int64, n)
            if (mod(i, 5_int64) == 0_int64) then
                call probes%append_null()
            else if (mod(i, 3_int64) == 0_int64) then
                write (txt, "(a,i0,a)") "k", j, "x"
                call probes%append_string(trim(txt))
            else
                write (txt, "(a,i0)") "k", j
                call probes%append_string(trim(txt))
            end if
        end do
        allocate(v64(n), v32(n), serial(n), mask(n))
        call m%get_many(probes, serial, threads=1)
        nhit = 0_int64
        do i = 1_int64, n
            j = 1_int64 + mod(i * 7919_int64, n)
            if (mod(i, 5_int64) == 0_int64 .or. mod(i, 3_int64) == 0_int64) then
                call check(error, serial(i) == 0_int64, "a null or altered probe answers 0")
            else
                call check(error, serial(i) == j, "a present probe answers its row")
                nhit = nhit + 1_int64
            end if
            if (allocated(error)) return
        end do
        call check(error, nhit > 0_int64 .and. nhit < n, "the probe set mixes hits and misses (vacuity guard)")
        if (allocated(error)) return
        call m%get_many(probes, v64, threads=4)
        call check(error, all(v64 == serial), "an explicit team answers as the serial run does")
        if (allocated(error)) return
        call m%get_many(probes, v32)
        call check(error, all(int(v32, int64) == serial), "int32 answers agree with int64 ones")
        if (allocated(error)) return
        mask = .true.
        mask(1:n:2) = .false.
        call m%get_many(probes, v64, valid=mask)
        call check(error, all(v64(1:n:2) == 0_int64), "a masked row answers 0")
        if (allocated(error)) return
        call check(error, all(v64(2:n:2) == serial(2:n:2)), "an unmasked row answers as before")
    end subroutine test_str_get_many_forms

    !> The incremental surface: `%set` starts a string map on a fresh object and replaces,
    !! `%get_or_add` numbers new keys densely from the watermark, the bulk form agrees with a
    !! loop, `%remove` forgets one key and reports absence, `%reset` empties without releasing
    !! and `%clear` releases.
    subroutine test_str_mutation(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m, loop, bulk
        type(parquet_string_column) :: sc
        integer(int64) :: idx, codes(7), c2(7)
        integer(int32) :: codes32(7)
        logical :: found
        character(len=:), allocatable :: tok
        integer :: i

        call m%set("alpha", 10_int64)
        call m%get_method(tok)
        call check(error, tok == "hash" .and. m%ncomponents() == 1, "set on a fresh map starts a string map")
        if (allocated(error)) return
        call m%set("beta", 20_int32)
        call m%set("alpha", 11_int64)
        call check(error, m%get("alpha") == 11_int64 .and. m%get("beta") == 20_int64 .and. m%nkeys() == 2_int64, &
            "set inserts and replaces")
        if (allocated(error)) return
        call m%get_or_add("gamma", idx)
        call check(error, idx == 21_int64, "get_or_add continues above the largest stored value")
        if (allocated(error)) return
        call m%get_or_add("beta", idx)
        call check(error, idx == 20_int64, "get_or_add returns a stored key's value unchanged")
        if (allocated(error)) return
        ! The bulk form equals a loop of the scalar one, on both key sources, with a mask.
        call fixture_column(sc)
        do i = 1, 7
            call loop%get_or_add(trim(KEYS(i)), codes(i))
        end do
        call bulk%get_or_add_many(KEYS, c2)
        call check(error, all(c2 == codes), "get_or_add_many over a character array equals the loop")
        if (allocated(error)) return
        call check(error, all(codes == [(int(i, int64), i = 1, 7)]), "codes are dense in first-appearance order")
        if (allocated(error)) return
        call bulk%clear()
        call bulk%get_or_add_many(sc, codes32, valid=[.true., .true., .false., .true., .true., .true., .true.])
        call check(error, codes32(3) == 0_int32 .and. all(codes32([1, 2, 4, 5, 6, 7]) == [1, 2, 3, 4, 5, 6]), &
            "a masked row gets the code 0 and is not added; the rest are numbered densely")
        if (allocated(error)) return
        call check(error, bulk%nkeys() == 6_int64, "the masked key was not added")
        if (allocated(error)) return
        ! Removal: the key is gone, the rest stay, absence is reported through found=.
        call m%remove("beta")
        call check(error, m%get("beta") == 0_int64 .and. m%nkeys() == 2_int64 .and. m%get("gamma") == 21_int64, &
            "remove forgets one key and keeps the rest")
        if (allocated(error)) return
        call m%remove("beta", found)
        call check(error, .not. found, "removing an absent key reports found=.false.")
        if (allocated(error)) return
        call m%get_or_add("beta", idx)
        call check(error, idx == 22_int64, "a removed key re-added takes a fresh index; the watermark never lowers")
        if (allocated(error)) return
        ! reset keeps the allocation; clear releases it.
        call m%reset()
        call check(error, m%nkeys() == 0_int64 .and. m%get("alpha") == 0_int64 .and. m%memory_bytes() > 0_int64, &
            "reset empties the map and keeps its storage")
        if (allocated(error)) return
        call m%set("delta", 1_int64)
        call check(error, m%get("delta") == 1_int64 .and. m%nkeys() == 1_int64, "a reset map refills")
        if (allocated(error)) return
        call m%clear()
        call check(error, m%memory_bytes() == 0_int64 .and. m%ncomponents() == 0 .and. m%get("delta") == 0_int64, &
            "clear releases everything and the map answers as a fresh one")
    end subroutine test_str_mutation

    !> Sixty keys hashed to four values: every key still found with its own value, every miss a
    !! miss, and after removing a third of them -- first, middle and last occurrences of their
    !! chains among them -- the rest still found. The hook is reset on every exit path.
    subroutine test_str_collision_chain(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.

        call parquet_debug_set_index_string_hash_bits(2)
        call collision_chain_body(error)
        call parquet_debug_set_index_string_hash_bits(0)
    end subroutine test_str_collision_chain

    !> The checked body of `test_str_collision_chain`, run with the hook set.
    subroutine collision_chain_body(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        type(pf_index_multimap) :: wide
        type(parquet_string_column) :: keys, list
        integer(int64), parameter :: n = 60_int64
        integer(int64) :: i, maxp, nfound
        real(real64) :: meanp
        character(len=16) :: txt
        logical :: found

        call ident_column(n, keys)
        call m%build(keys)
        call check(error, m%nkeys() == n, "sixty colliding keys are all stored")
        if (allocated(error)) return
        ! Vacuity guard: the hook must have narrowed the hash, or this test exercises nothing. A
        ! map built at two bits answers nothing at sixty-four -- the two hash differently.
        call parquet_debug_set_index_string_hash_bits(0)
        call check(error, m%get("k1") == 0_int64 .and. m%get("k60") == 0_int64, &
            "the hook changed the hash (vacuity guard): a narrow-built map misses at full width")
        call parquet_debug_set_index_string_hash_bits(2)
        if (allocated(error)) return
        do i = 1_int64, n
            write (txt, "(a,i0)") "k", i
            call check(error, m%get(trim(txt)) == i, "every colliding key answers its own value: " // trim(txt))
            if (allocated(error)) return
        end do
        call check(error, m%get("k0") == 0_int64 .and. m%get("k61") == 0_int64 .and. m%get("") == 0_int64, &
            "a miss is a miss however deep the chain")
        if (allocated(error)) return
        ! Remove every third key: that takes the first occurrence of some chains (the hash of
        ! "k3" may be occurrence 0 of its value), the last of others, and interior ones.
        do i = 3_int64, n, 3_int64
            write (txt, "(a,i0)") "k", i
            call m%remove(trim(txt), found)
            call check(error, found, "a colliding key can be removed: " // trim(txt))
            if (allocated(error)) return
        end do
        call check(error, m%nkeys() == n - n / 3_int64, "the key count follows the removals")
        if (allocated(error)) return
        nfound = 0_int64
        do i = 1_int64, n
            write (txt, "(a,i0)") "k", i
            if (mod(i, 3_int64) == 0_int64) then
                call check(error, m%get(trim(txt)) == 0_int64, "a removed key misses: " // trim(txt))
            else
                call check(error, m%get(trim(txt)) == i, "a surviving key is still found after the chain moved: " // &
                    trim(txt))
                nfound = nfound + 1_int64
            end if
            if (allocated(error)) return
        end do
        call check(error, nfound == n - n / 3_int64, "every survivor was found")
        if (allocated(error)) return
        ! keys() lists exactly the survivors, and a removed key can come back.
        call m%keys(list)
        call check(error, list%size() == n - n / 3_int64, "keys() lists the survivors only")
        if (allocated(error)) return
        call m%set("k3", 3_int64)
        call check(error, m%get("k3") == 3_int64 .and. m%get("k6") == 0_int64, "a removed key re-inserted is found again")
        if (allocated(error)) return
        call m%probe_stats(maxp, meanp)
        call check(error, maxp >= 1_int64, "probe_stats reports on a string map")
        if (allocated(error)) return
        ! A multimap inherits the chain: repeats of colliding keys still group correctly.
        call wide%build(["p", "q", "p", "r", "q", "p"])
        call check(error, wide%ngroups() == 3_int64 .and. wide%count("p") == 3_int64 .and. wide%count("r") == 1_int64, &
            "a multimap over colliding keys groups them correctly")
    end subroutine collision_chain_body

    !> Near-identical identifiers must spread over the table as integers do.
    subroutine test_str_clustering(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        type(parquet_string_column) :: keys
        integer(int64) :: maxp
        real(real64) :: meanp

        call ident_column(4000_int64, keys)
        call m%build(keys)
        call m%probe_stats(maxp, meanp)
        call check(error, maxp <= 40_int64, "strings differing in a few trailing digits must not cluster")
        if (allocated(error)) return
        call check(error, meanp < 3.0_real64, "and the mean probe length must stay near 1")
    end subroutine test_str_clustering

    !> A map that was never built answers 0 to every string question, as it does to integers.
    subroutine test_str_unbuilt(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        type(parquet_string_column) :: list, probes
        integer(int64) :: v(2)
        logical :: found

        call check(error, m%get("x") == 0_int64 .and. .not. m%contains("x"), "get and contains answer 0 unbuilt")
        if (allocated(error)) return
        call m%get_many(["x", "y"], v)
        call check(error, all(v == 0_int64), "get_many over a character array answers 0 unbuilt")
        if (allocated(error)) return
        call probes%clear()
        call probes%append_string("x")
        call probes%append_null()
        call m%get_many(probes, v)
        call check(error, all(v == 0_int64), "get_many over a column answers 0 unbuilt")
        if (allocated(error)) return
        call m%keys(list)
        call check(error, list%size() == 0_int64, "keys() is empty unbuilt")
        if (allocated(error)) return
        call m%remove("x", found)
        call check(error, .not. found, "remove reports absence unbuilt")
        if (allocated(error)) return
        call check(error, m%ncomponents() == 0, "an unbuilt map has no component count")
    end subroutine test_str_unbuilt

    !> `%init(strings=.true.)` with a capacity, then a run of inserts past it; `%reserve` too.
    subroutine test_str_init_and_reserve(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        integer(int64) :: i, idx
        character(len=16) :: txt
        character(len=:), allocatable :: tok

        call m%init(strings=.true., capacity=8)
        call m%get_method(tok)
        call check(error, tok == "hash" .and. m%ncomponents() == 1 .and. m%nkeys() == 0_int64, &
            "init(strings=) starts an empty string map")
        if (allocated(error)) return
        call m%reserve(3000_int64)
        do i = 1_int64, 3000_int64
            write (txt, "(a,i0)") "id", i
            call m%get_or_add(trim(txt), idx)
            if (idx /= i) then
                call check(error, .false., "get_or_add numbers densely through the growth")
                return
            end if
        end do
        call check(error, m%nkeys() == 3000_int64, "every inserted key is stored after growth")
        if (allocated(error)) return
        do i = 1_int64, 3000_int64, 97_int64
            write (txt, "(a,i0)") "id", i
            call check(error, m%get(trim(txt)) == i, "keys survive the store's and the table's growth")
            if (allocated(error)) return
        end do
    end subroutine test_str_init_and_reserve

    ! ---- pf_index_multimap ---------------------------------------------------------------------

    !> The fixture with repeats every multimap test uses: nine rows over four distinct keys,
    !! interleaved, the first element the shortest.
    subroutine repeats_fixture(chr, sc)
        character(len=6), allocatable, intent(out) :: chr(:) !! receives the rows, padded.
        type(parquet_string_column), intent(out) :: sc     !! receives them verbatim (trimmed).
        integer :: i

        chr = [character(len=6) :: "a", "bb", "a", "cc cc", "bb", "a", "dd", "cc cc", "a"]
        call sc%clear()
        do i = 1, size(chr)
            call sc%append_string(trim(chr(i)))
        end do
    end subroutine repeats_fixture

    !> The scalar answers against a scan, from both key sources, with a mask and with values.
    subroutine test_str_multimap_scan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm, mc, mv
        type(parquet_string_column) :: sc, withnull, list
        character(len=6), allocatable :: chr(:)
        integer(int64), allocatable :: rows(:), offs(:), csr(:)
        integer(int64) :: lo, hi
        integer :: i

        call repeats_fixture(chr, sc)
        call mm%build(chr)
        call mc%build(sc)
        call check(error, mm%ngroups() == 4_int64 .and. mm%nkeys() == 9_int64 .and. mm%max_multiplicity() == 4_int64, &
            "four groups over nine rows, the largest of four")
        if (allocated(error)) return
        call check(error, mm%count("a") == 4_int64 .and. mm%count("bb") == 2_int64 .and. mm%count("cc cc") == 2_int64 &
            .and. mm%count("dd") == 1_int64 .and. mm%count("zz") == 0_int64, "count agrees with the scan")
        if (allocated(error)) return
        call check(error, mm%get_first("a") == 1_int64 .and. mm%get_first("bb") == 2_int64 &
            .and. mm%get_first("dd") == 7_int64 .and. mm%get_first("zz") == 0_int64, "get_first is the lowest row")
        if (allocated(error)) return
        call mm%get_all("a", rows)
        call check(error, size(rows) == 4 .and. all(rows == [1_int64, 3_int64, 6_int64, 9_int64]), &
            "get_all lists every row ascending")
        if (allocated(error)) return
        call mm%get_all("zz", rows)
        call check(error, size(rows) == 0, "get_all of an absent key is zero-length")
        if (allocated(error)) return
        call mm%get_range("cc cc", lo, hi)
        call mm%csr(offs, csr)
        call check(error, hi - lo + 1_int64 == 2_int64 .and. all(csr(lo:hi) == [4_int64, 8_int64]), &
            "get_range and csr describe the same rows")
        if (allocated(error)) return
        call check(error, mm%get("a") >= 1_int64 .and. mm%get("a") <= 4_int64 .and. mm%get("zz") == 0_int64, &
            "get answers a group id in range, or 0")
        if (allocated(error)) return
        ! The column form agrees with the array form on every scalar question.
        do i = 1, size(chr)
            call check(error, mc%count(trim(chr(i))) == mm%count(trim(chr(i))) &
                .and. mc%get_first(trim(chr(i))) == mm%get_first(trim(chr(i))), &
                "the column-built multimap agrees at " // trim(chr(i)))
            if (allocated(error)) return
        end do
        call mm%keys(list)
        call check(error, list%size() == 4_int64, "keys() lists the distinct keys")
        if (allocated(error)) return
        ! A null row is not grouped; a masked row neither; explicit values are stored in order.
        call withnull%clear()
        call withnull%append_string("a")
        call withnull%append_null()
        call withnull%append_string("a")
        call withnull%append_string("")
        call mv%build(withnull, values=[10_int64, 99_int64, 30_int64, 40_int64], &
            valid=[.true., .true., .true., .false.])
        call check(error, mv%ngroups() == 1_int64 .and. mv%nkeys() == 2_int64, &
            "a null row and a masked row are not grouped")
        if (allocated(error)) return
        call mv%get_all("a", rows)
        call check(error, size(rows) == 2 .and. all(rows == [10_int64, 30_int64]), "explicit values, in position order")
        if (allocated(error)) return
        call check(error, mv%count("") == 0_int64, "the masked empty string is absent")
    end subroutine test_str_multimap_scan

    !> `%probe_many` over string columns and over character arrays equals `pf_match_all`, the
    !! sort engine's m:m match, element for element -- with repeats on both sides and misses.
    subroutine test_str_probe_equals_match_all(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        type(parquet_string_column) :: keys, probes
        character(len=6), allocatable :: chr(:)
        character(len=6) :: pchr(6)
        integer(int64), allocatable :: off(:), m(:), off2(:), m2(:)
        integer(int64) :: nm
        logical, allocatable :: hit(:)
        integer :: i

        call repeats_fixture(chr, keys)
        pchr = [character(len=6) :: "a", "zz", "cc cc", "a", "", "dd"]
        call probes%clear()
        do i = 1, size(pchr)
            call probes%append_string(trim(pchr(i)))
        end do
        call mm%build(keys)
        call mm%probe_many(probes, off, m, n_matched=nm, group_hit=hit)
        call pf_match_all(probes, keys, off2, m2)
        call check(error, size(off) == size(off2) .and. size(m) == size(m2), "probe_many and pf_match_all agree on shape")
        if (allocated(error)) return
        call check(error, all(off == off2) .and. all(m == m2), "probe_many equals pf_match_all over string columns")
        if (allocated(error)) return
        call check(error, nm == 4_int64, "four probes matched something")
        if (allocated(error)) return
        call check(error, size(hit) == 4 .and. count(hit) == 3, "group_hit marks the three groups reached")
        if (allocated(error)) return
        call check(error, size(m) == 4 + 2 + 4 + 1, "the pair count is the sum of the matched groups' sizes")
        if (allocated(error)) return
        ! The character-array form, against pf_match_all over the same arrays.
        call mm%probe_many(pchr, off, m)
        call pf_match_all(pchr, chr, off2, m2)
        call check(error, all(off == off2) .and. all(m == m2), "probe_many equals pf_match_all over character arrays")
    end subroutine test_str_probe_equals_match_all

    !> `%get_first_many` and `%get_many` equal loops of the scalar forms, on both key sources and
    !! both answer kinds; `%get_all` in int32; `valid=` and `n_found`.
    subroutine test_str_multimap_bulk_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        type(parquet_string_column) :: keys, probes
        character(len=6), allocatable :: chr(:)
        character(len=6) :: pchr(6)
        integer(int64) :: f64(6), g64(6), nf
        integer(int32) :: f32(6), g32(6)
        integer(int32), allocatable :: r32(:)
        integer(int64), allocatable :: r64(:)
        integer :: i

        call repeats_fixture(chr, keys)
        pchr = [character(len=6) :: "a", "zz", "cc cc", "a", "", "dd"]
        call probes%clear()
        do i = 1, size(pchr)
            call probes%append_string(trim(pchr(i)))
        end do
        call mm%build(keys)
        call mm%get_first_many(pchr, f64, n_found=nf)
        call mm%get_first_many(probes, f32)
        call mm%get_many(pchr, g64)
        call mm%get_many(probes, g32)
        do i = 1, size(pchr)
            call check(error, f64(i) == mm%get_first(trim(pchr(i))) .and. int(f32(i), int64) == f64(i), &
                "get_first_many equals get_first at " // trim(pchr(i)))
            if (allocated(error)) return
            call check(error, g64(i) == mm%get(trim(pchr(i))) .and. int(g32(i), int64) == g64(i), &
                "get_many equals get at " // trim(pchr(i)))
            if (allocated(error)) return
        end do
        call check(error, nf == 4_int64, "n_found counts the found probes")
        if (allocated(error)) return
        call mm%get_first_many(pchr, f64, valid=[.true., .true., .false., .true., .true., .true.])
        call check(error, f64(3) == 0_int64 .and. f64(1) == 1_int64, "a masked probe answers 0")
        if (allocated(error)) return
        call mm%get_all("a", r32)
        call mm%get_all("a", r64)
        call check(error, size(r32) == 4 .and. all(int(r32, int64) == r64), "get_all in int32 equals int64")
    end subroutine test_str_multimap_bulk_forms

    !> Two thousand rows over ten keys: the grouping pass reserves for every row, the
    !! right-sized rebuild follows, and every group id and every row survives it.
    subroutine test_str_rightsize_rebuild(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        type(parquet_string_column) :: keys
        integer(int64), allocatable :: rows(:)
        integer(int64), parameter :: n = 2000_int64
        integer(int64) :: i, g, cnt
        character(len=8) :: txt

        call keys%clear()
        do i = 1_int64, n
            write (txt, "(a,i0)") "grp", mod(i, 10_int64)
            call keys%append_string(trim(txt))
        end do
        call mm%build(keys)
        call check(error, mm%ngroups() == 10_int64 .and. mm%nkeys() == n, "ten groups over two thousand rows")
        if (allocated(error)) return
        call check(error, mm%memory_bytes() < 64_int64 * 1024_int64, &
            "the rebuilt map is right-sized for ten keys, not two thousand rows")
        if (allocated(error)) return
        cnt = 0_int64
        do g = 0_int64, 9_int64
            write (txt, "(a,i0)") "grp", g
            call mm%get_all(trim(txt), rows)
            call check(error, size(rows, kind=int64) == n / 10_int64, "every group has two hundred rows")
            if (allocated(error)) return
            do i = 1_int64, size(rows, kind=int64)
                if (mod(rows(i), 10_int64) /= g) then
                    call check(error, .false., "a row landed in the wrong group after the rebuild")
                    return
                end if
            end do
            cnt = cnt + size(rows, kind=int64)
        end do
        call check(error, cnt == n, "every row is in exactly one group")
    end subroutine test_str_rightsize_rebuild

    !> No keys at all; a single empty-string key; every key equal.
    subroutine test_str_edges(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m
        type(pf_index_multimap) :: mm
        type(parquet_string_column) :: none, list
        character(len=1) :: one(1)
        integer(int64), allocatable :: rows(:)
        character(len=:), allocatable :: tok

        call none%clear()
        call m%build(none)
        call m%get_method(tok)
        call check(error, m%nkeys() == 0_int64 .and. tok == "hash" .and. m%get("a") == 0_int64, &
            "a zero-key string build is a valid, empty hash map")
        if (allocated(error)) return
        one(1) = " "
        call m%build(one)
        call check(error, m%nkeys() == 1_int64 .and. m%get("") == 1_int64 .and. m%get(" ") == 0_int64, &
            "a blank array element is the empty-string key, and ' ' as written is not")
        if (allocated(error)) return
        call m%keys(list)
        call check(error, list%size() == 1_int64 .and. list%length(1_int64) == 0_int64, &
            "keys() returns the empty string as an element")
        if (allocated(error)) return
        call mm%build(["x", "x", "x"])
        call mm%get_all("x", rows)
        call check(error, mm%ngroups() == 1_int64 .and. size(rows) == 3, "an all-equal array is one group of three")
        if (allocated(error)) return
        call mm%build(none)
        call check(error, mm%ngroups() == 0_int64 .and. mm%count("x") == 0_int64, "an empty multimap build is valid")
    end subroutine test_str_edges

    !> Whether two codings of one stream agree on which rows share a key -- `a(i) == a(j)` exactly
    !! when `b(i) == b(j)` -- with both dense in `1 .. k`; a row coded 0 in both is a masked row
    !! and is skipped, a row coded 0 in one only is a disagreement.
    pure function codes_consistent(a, b, k) result(ok)
        integer(int64), intent(in) :: a(:) !! one coding.
        integer(int64), intent(in) :: b(:) !! the other.
        integer(int64), intent(in) :: k    !! the distinct-key count both must be dense over.
        logical :: ok                      !! `.true.` when the two are relabellings of each other.
        integer(int64), allocatable :: ab(:), ba(:)
        integer(int64) :: i

        ok = .false.
        if (size(a) /= size(b)) return
        if (minval(a) < 0_int64 .or. minval(b) < 0_int64) return
        if (maxval(a) /= k .or. maxval(b) /= k) return
        allocate(ab(k), ba(k))
        ab = 0_int64
        ba = 0_int64
        do i = 1_int64, size(a, kind=int64)
            if (a(i) == 0_int64 .and. b(i) == 0_int64) cycle
            if (a(i) == 0_int64 .or. b(i) == 0_int64) return
            if (ab(a(i)) == 0_int64) ab(a(i)) = b(i)
            if (ab(a(i)) /= b(i)) return
            if (ba(b(i)) == 0_int64) ba(b(i)) = a(i)
            if (ba(b(i)) /= a(i)) return
        end do
        ok = all(ab /= 0_int64) .and. all(ba /= 0_int64)
    end function codes_consistent

    !> The partitioned string build -- every key hashed on the team, the store filled on the
    !! team, the tuples through the partitioned insert -- answers every key and every miss as the
    !! serial build does, from a character array and from a column with nulls, under a mask and
    !! with values; and it did partition, which the spill count says (-1 is the serial loop).
    subroutine test_str_partitioned_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m, s
        type(parquet_string_column) :: sc, list
        character(len=12), allocatable :: keys(:)
        integer(int64), allocatable :: vals(:)
        logical, allocatable :: mask(:)
        integer(int64), parameter :: n = 50000_int64
        integer(int64) :: i, bad, kept, want
        integer :: nt
        character(len=16) :: txt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it a string build never partitions")
        return
#endif
        nt = pf_index_threads(n)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no build " // &
                "ever partitions")
            return
        end if
        allocate(keys(n), vals(n), mask(n))
        call sc%clear()
        do i = 1_int64, n
            write (txt, "(a,i0)") "obj_", i * 7_int64
            keys(i) = txt
            if (mod(i, 97_int64) == 0_int64) then
                call sc%append_null()
            else
                call sc%append_string(trim(txt))
            end if
            vals(i) = i + 1000_int64
            mask(i) = mod(i, 5_int64) /= 0_int64
        end do
        call m%build(keys)
        call check(error, parquet_debug_index_threads_used() == nt, "a string build over 50000 keys resolves the team")
        if (allocated(error)) return
        call check(error, parquet_debug_index_spills() >= 0_int64, &
            "the string build ran its partitioned pass (a spill count of -1 is the serial loop)")
        if (allocated(error)) return
        call check(error, m%nkeys() == n, "every key is stored")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n
            if (m%get(trim(keys(i))) /= i) bad = bad + 1_int64
            if (m%get(trim(keys(i)) // "x") /= 0_int64) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every key answers its row and every absent key 0")
        if (allocated(error)) return
        call s%build(keys, threads=1)
        call check(error, parquet_debug_index_spills() == -1_int64, "threads=1 is the serial loop")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n, 13_int64
            if (s%get(trim(keys(i))) /= m%get(trim(keys(i)))) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "the partitioned build answers as the serial one")
        if (allocated(error)) return
        call m%keys(list)
        call s%build(list, threads=1)
        call check(error, list%size() == n .and. s%nkeys() == n, "keys() lists every stored string, once")
        if (allocated(error)) return
        ! The column: nulls skipped, then a mask and values on top.
        call m%build(sc)
        call check(error, parquet_debug_index_spills() >= 0_int64, "the column build ran its partitioned pass")
        if (allocated(error)) return
        want = n - n / 97_int64
        call check(error, m%nkeys() == want, "a null element is neither stored nor counted")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n
            if (mod(i, 97_int64) == 0_int64) then
                if (m%get(trim(keys(i))) /= 0_int64) bad = bad + 1_int64
            else
                if (m%get(trim(keys(i))) /= i) bad = bad + 1_int64
            end if
        end do
        call check(error, bad == 0_int64, "every non-null element answers its row and a null one's text answers 0")
        if (allocated(error)) return
        call m%build(sc, vals, valid=mask)
        kept = 0_int64
        bad = 0_int64
        do i = 1_int64, n
            if (mask(i) .and. mod(i, 97_int64) /= 0_int64) then
                kept = kept + 1_int64
                if (m%get(trim(keys(i))) /= vals(i)) bad = bad + 1_int64
            else
                if (m%get(trim(keys(i))) /= 0_int64) bad = bad + 1_int64
            end if
        end do
        call check(error, m%nkeys() == kept .and. bad == 0_int64, &
            "under a mask and values, an unmasked non-null element answers its value and the rest 0")
    end subroutine test_str_partitioned_build

    !> A threaded string `%get_or_add_many` on a fresh map numbers the distinct strings densely
    !! and consistently with the serial pass, lists them through `%keys` in code order (a code is
    !! its store position, as on the serial pass), takes a column's nulls and a mask as code 0,
    !! and on a map already holding keys looks them up on the team and adds the new ones.
    subroutine test_str_partitioned_get_or_add_many(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: s, p, q
        type(parquet_string_column) :: sc, list
        character(len=12), allocatable :: stream(:), more(:)
        integer(int64), allocatable :: c1(:), c2(:), c3(:), cm1(:), cm2(:)
        integer(int32), allocatable :: c32(:)
        logical, allocatable :: mask(:)
        character(len=:), allocatable :: tok
        integer(int64), parameter :: n = 60000_int64, k = 20000_int64, nmore = 10000_int64
        integer(int64) :: i, bad, c
        integer :: nt
        character(len=16) :: txt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it every string get_or_add_many is the serial pass")
        return
#endif
        nt = pf_index_threads(n)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no call " // &
                "ever partitions")
            return
        end if
        allocate(stream(n), more(nmore), c1(n), c2(n), c3(nmore), cm1(n), cm2(n), c32(n), mask(n))
        call sc%clear()
        do i = 1_int64, n
            write (txt, "(a,i0)") "s", 1_int64 + mod(i * 7919_int64, k)
            stream(i) = txt
            mask(i) = mod(i, 4_int64) /= 0_int64
            if (mod(i, 101_int64) == 0_int64) then
                call sc%append_null()
            else
                call sc%append_string(trim(txt))
            end if
        end do
        call s%init(strings=.true.)
        call s%get_or_add_many(stream, c1, threads=1)
        call p%init(strings=.true.)
        call p%get_or_add_many(stream, c2)
        call check(error, parquet_debug_index_threads_used() == nt, "a string get_or_add_many over 60000 rows resolves the team")
        if (allocated(error)) return
        call check(error, parquet_debug_index_spills() >= 0_int64, "the fresh map took the partitioned pass")
        if (allocated(error)) return
        call check(error, p%nkeys() == k .and. s%nkeys() == k, "both passes store exactly the distinct strings")
        if (allocated(error)) return
        call check(error, codes_consistent(c1, c2, k), &
            "the threaded codes are a relabelling of the serial ones: dense in 1..k, equal strings equal codes")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n
            if (p%get(trim(stream(i))) /= c2(i)) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every row's code is what %get answers for its string afterwards")
        if (allocated(error)) return
        call p%keys(list)
        call check(error, list%size() == k, "keys() lists one entry per code")
        if (allocated(error)) return
        bad = 0_int64
        do c = 1_int64, k, 7_int64
            call list%get(c, tok)
            if (p%get(tok) /= c) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "keys() lists the strings in code order: element c has code c")
        if (allocated(error)) return
        ! The filled map: half old strings, half new -- old codes kept, new ones above k.
        do i = 1_int64, nmore
            if (mod(i, 2_int64) == 0_int64) then
                more(i) = stream(i)
            else
                write (txt, "(a,i0)") "t", i
                more(i) = txt
            end if
        end do
        call p%get_or_add_many(more, c3)
        bad = 0_int64
        do i = 1_int64, nmore
            if (mod(i, 2_int64) == 0_int64) then
                if (c3(i) /= c2(i)) bad = bad + 1_int64
            else
                if (c3(i) <= k) bad = bad + 1_int64
            end if
        end do
        call check(error, bad == 0_int64 .and. p%nkeys() == k + nmore / 2_int64, &
            "on a filled map an old string keeps its code and a new one is added above the watermark")
        if (allocated(error)) return
        ! The column with nulls, under a mask: null and masked rows are 0 and add nothing.
        call s%clear()
        call s%init(strings=.true.)
        call s%get_or_add_many(sc, cm1, valid=mask, threads=1)
        call q%init(strings=.true.)
        call q%get_or_add_many(sc, cm2, valid=mask)
        call check(error, q%nkeys() == s%nkeys() .and. all(pack(cm2, .not. mask) == 0_int64), &
            "a masked row gets 0 and adds nothing under the threaded pass")
        if (allocated(error)) return
        bad = 0_int64
        do i = 101_int64, n, 101_int64
            if (cm2(i) /= 0_int64) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64 .and. codes_consistent(cm1, cm2, s%nkeys()), &
            "a null element gets 0, and the rest are coded consistently with the serial pass")
        if (allocated(error)) return
        call q%clear()
        call q%init(strings=.true.)
        call q%get_or_add_many(stream, c32)
        call check(error, codes_consistent(c1, int(c32, int64), k), "the int32 form codes consistently")
        if (allocated(error)) return
        ! Fewer new strings than the threading floor, on a team asked for explicitly: the tuple
        ! pass adds them serially inside the threaded call, and the string map must take that
        ! path as any map does (its guard-only workers would refuse a string map).
        call s%clear()
        call s%init(strings=.true.)
        call s%get_or_add_many(stream(1:1500), cm1(1:1500), threads=1)
        call q%clear()
        call q%init(strings=.true.)
        call q%get_or_add_many(stream(1:1500), cm2(1:1500), threads=4)
        call check(error, parquet_debug_index_spills() == -1_int64 .and. q%nkeys() == s%nkeys(), &
            "a small fresh string get_or_add_many on a team adds its strings serially")
        if (allocated(error)) return
        call check(error, codes_consistent(cm1(1:1500), cm2(1:1500), s%nkeys()), &
            "the serially added strings are coded consistently with the serial pass")
        if (allocated(error)) return
        call q%keys(list)
        call list%get(1_int64, tok)
        call check(error, list%size() == s%nkeys() .and. q%get(tok) == 1_int64, &
            "keys() of the serially filled map lists the strings in code order")
    end subroutine test_str_partitioned_get_or_add_many

    !> Under the collision hook the partitioned string passes meet equal tuples for distinct
    !! strings and hand the whole call to the serial loop, which walks the occurrence chain:
    !! every key still answers, on a build and on a fresh-map `%get_or_add_many`, and the spill
    !! count says the fallback happened (-1 is the serial loop's mark). The same build with the
    !! hook off partitions, which is what shows the hook forced the fallback.
    subroutine test_str_collision_threaded(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.

        call parquet_debug_set_index_string_hash_bits(6)
        call collision_threaded_body(error)
        call parquet_debug_set_index_string_hash_bits(0)
    end subroutine test_str_collision_threaded

    !> The checked body of `test_str_collision_threaded`, run with the hook set.
    subroutine collision_threaded_body(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_map) :: m, p
        type(parquet_string_column) :: keys
        integer(int64), allocatable :: codes(:)
        integer(int64), parameter :: n = 20000_int64
        integer(int64) :: i, bad
        integer :: nt
        character(len=16) :: txt

#ifndef _OPENMP
        call skip_test(error, "needs OpenMP: without it the build is the serial loop whether or not it collides")
        return
#endif
        nt = pf_index_threads(n)
        if (nt < 2) then
            call skip_test(error, "this machine's affinity mask allows one processor, so no build " // &
                "ever partitions and none can fall back")
            return
        end if
        call ident_column(n, keys)
        allocate(codes(n))
        call m%build(keys)
        call check(error, parquet_debug_index_threads_used() == nt, "the colliding build resolved its team")
        if (allocated(error)) return
        call check(error, parquet_debug_index_spills() == -1_int64, &
            "with colliding hashes the partitioned pass met equal tuples and the serial loop took over")
        if (allocated(error)) return
        call check(error, m%nkeys() == n, "every colliding key is stored")
        if (allocated(error)) return
        bad = 0_int64
        do i = 1_int64, n
            write (txt, "(a,i0)") "k", i
            if (m%get(trim(txt)) /= i) bad = bad + 1_int64
        end do
        call check(error, bad == 0_int64, "every colliding key answers its row after the fallback")
        if (allocated(error)) return
        call p%init(strings=.true.)
        call p%get_or_add_many(keys, codes)
        call check(error, parquet_debug_index_spills() == -1_int64 .and. p%nkeys() == n, &
            "the fresh-map pass found the collision, and the serial pass stored every key")
        if (allocated(error)) return
        call check(error, all(codes == [(i, i = 1_int64, n)]), &
            "the serial pass numbered the distinct colliding strings by first appearance")
        if (allocated(error)) return
        ! Vacuity guard: without the hook the same build partitions.
        call parquet_debug_set_index_string_hash_bits(0)
        call m%build(keys)
        call check(error, parquet_debug_index_spills() >= 0_int64, &
            "the hook is what forced the fallback: with the full hash the build partitions")
        call parquet_debug_set_index_string_hash_bits(6)
    end subroutine collision_threaded_body

end module test_index_strings
