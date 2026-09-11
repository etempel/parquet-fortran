!> Tests for `pf_index_multimap`: keys with repeats over the three backends.
!!
!! **Two oracles, and neither shares code with the multimap.** The scalar answers (`%count`,
!! `%get_first`, `%get_all`, `%get_range`) are checked against a linear scan of the fixture written
!! out below; the bulk CSR answer (`%probe_many`) is checked against `pf_match_all`, the sorting
!! tier's m:m match, whose contract is the same -- `offsets(1) == 1`, one range per probe,
!! ascending within a range -- and whose engine is a sort rather than a hash. Identical CSR is the
!! assertion, element for element, because two independent engines agreeing on a random fixture
!! with repeats and misses is the strongest statement this suite can make.
!!
!! **Every fixture has repeats and every probe set has misses, and a vacuity guard says so.** A
!! multimap over unique keys is a map, and a probe set of hits alone cannot see a lookup that
!! answers something for an absent key; both are asserted on rather than assumed, per CLAUDE.md's
!! "A test that searches a live fixture for its own case needs a vacuity guard".
!!
!! **Backends are asked for explicitly**, as in `test_index.f90`, except in `test_mm_auto_choice`,
!! which asserts the choice itself -- including the two-step rule that applies the map's budget to
!! the DISTINCT count, not to the rows presented.
!!
!! Threading tests are in `test_index_omp.f90`, for the reason its header gives. This suite
!! touches no file and no process-global state, so it runs concurrently.
module test_index_multimap
    use testdrive, only: new_unittest, unittest_type, error_type, check
    use parquet_index
    use parquet_sorting, only: pf_match_all
    use iso_fortran_env, only: int32, int64
    implicit none
    private

    public :: collect_tests_index_multimap

    !> Backends every single-component fixture is built with, in turn.
    character(len=6), parameter :: METHODS(3) = ["direct", "hash  ", "sorted"]

contains

    !> Registers every test in this suite.
    subroutine collect_tests_index_multimap(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! Receives the suite's tests.

        testsuite = [ &
            new_unittest("every backend agrees with a scan of a fixture with repeats", &
                test_mm_build_matches_scan), &
            new_unittest("probe_many equals pf_match_all on random keys with repeats", &
                test_mm_csr_equals_match_all), &
            new_unittest("rows are ascending by position within every group", &
                test_mm_ascending_within_group), &
            new_unittest("group ids follow first appearance on the serial path", &
                test_mm_group_ids_first_appearance), &
            new_unittest("build with valid= skips masked rows and keeps row numbers", &
                test_mm_valid_mask_build), &
            new_unittest("get_first_many equals a loop of get_first", test_mm_get_first_many_matches), &
            new_unittest("get_many answers the group id per key", test_mm_get_many_groups), &
            new_unittest("composite keys group by the whole tuple", test_mm_composite), &
            new_unittest("int32 and int64 entry points answer alike", test_mm_int32_forms), &
            new_unittest("csr and keys answer the same arrays in int32 as in int64", &
                test_mm_csr_and_keys_int32), &
            new_unittest("every mixed int32/int64 build and scalar lookup answers alike", &
                test_mm_int32_mixed_builds_and_scalars), &
            new_unittest("every mixed int32/int64 bulk lookup answers alike", &
                test_mm_int32_mixed_bulk_forms), &
            new_unittest("an empty build and an all-equal key array are valid", test_mm_empty_and_all_equal), &
            new_unittest("explicit values are stored in position order", test_mm_explicit_values), &
            new_unittest("get_range and csr describe the same ranges", test_mm_get_range_and_csr), &
            new_unittest("introspection reports the build, and clear releases it", &
                test_mm_clear_and_introspection), &
            new_unittest("the automatic choice is the map's rule over the distinct keys", &
                test_mm_auto_choice), &
            new_unittest("a rebuild forgets the previous keys", test_mm_rebuild), &
            new_unittest("a one-column rank-2 build groups as a rank-1 build", &
                test_mm_rank2_single_column), &
            new_unittest("a composite build stores the values it was given", &
                test_mm_composite_explicit_values) &
            ]
    end subroutine collect_tests_index_multimap

    ! ---- Fixtures and the scan oracle ----

    !> The next value of a Park-Miller generator: no intermediate exceeds 2**47, so nothing wraps.
    pure function lcg(x) result(y)
        integer(int64), intent(in) :: x !! the previous state, in `1 .. 2**31 - 2`.
        integer(int64) :: y             !! the next state.

        y = mod(x * 48271_int64, 2147483647_int64)
    end function lcg

    !> `n` keys drawn with repeats from `ndistinct` values spaced `step` apart from `base`.
    subroutine fill_repeating_keys(keys, ndistinct, step, base, seed)
        integer(int64), intent(out) :: keys(:)  !! receives the keys.
        integer(int64), intent(in) :: ndistinct !! distinct values to draw from.
        integer(int64), intent(in) :: step      !! spacing of the distinct values.
        integer(int64), intent(in) :: base      !! the smallest distinct value.
        integer(int64), intent(in) :: seed      !! generator seed, in `1 .. 2**31 - 2`.
        integer(int64) :: i, x

        x = seed
        do i = 1_int64, size(keys, kind=int64)
            x = lcg(x)
            keys(i) = base + step * mod(x, ndistinct)
        end do
    end subroutine fill_repeating_keys

    !> Every position holding `key`, ascending, by linear scan; zero-length when none does.
    subroutine scan_positions(keys, key, pos, valid)
        integer(int64), intent(in) :: keys(:)               !! the fixture's keys.
        integer(int64), intent(in) :: key                   !! the key being looked up.
        integer(int64), allocatable, intent(out) :: pos(:)  !! receives the positions.
        logical, intent(in), optional :: valid(:)           !! a masked row is never a hit.
        integer(int64) :: i, k

        k = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (keys(i) == key) then
                if (present(valid)) then
                    if (.not. valid(i)) cycle
                end if
                k = k + 1_int64
            end if
        end do
        allocate(pos(k))
        k = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (keys(i) == key) then
                if (present(valid)) then
                    if (.not. valid(i)) cycle
                end if
                k = k + 1_int64
                pos(k) = i
            end if
        end do
    end subroutine scan_positions

    !> Every row holding tuple `key`, ascending, by linear scan.
    subroutine scan_positions_n(keys, key, pos)
        integer(int64), intent(in) :: keys(:,:)             !! the fixture's tuples, one per row.
        integer(int64), intent(in) :: key(:)                !! the tuple being looked up.
        integer(int64), allocatable, intent(out) :: pos(:)  !! receives the positions.
        integer(int64) :: i, k

        k = 0_int64
        do i = 1_int64, size(keys, 1, kind=int64)
            if (all(keys(i, :) == key)) k = k + 1_int64
        end do
        allocate(pos(k))
        k = 0_int64
        do i = 1_int64, size(keys, 1, kind=int64)
            if (all(keys(i, :) == key)) then
                k = k + 1_int64
                pos(k) = i
            end if
        end do
    end subroutine scan_positions_n

    !> The distinct values of `keys`, in first-appearance order, by quadratic scan.
    subroutine distinct_in_order(keys, out)
        integer(int64), intent(in) :: keys(:)               !! the fixture's keys.
        integer(int64), allocatable, intent(out) :: out(:)  !! receives the distinct keys.
        integer(int64), allocatable :: buf(:)
        integer(int64) :: i, k

        allocate(buf(size(keys)))
        k = 0_int64
        do i = 1_int64, size(keys, kind=int64)
            if (k > 0_int64) then
                if (any(buf(1:k) == keys(i))) cycle
            end if
            k = k + 1_int64
            buf(k) = keys(i)
        end do
        allocate(out(k))
        out = buf(1:k)
    end subroutine distinct_in_order

    ! ---- The tests ----

    !> `%count`, `%get_first`, `%get_all` and `%get` agree with a scan, on every backend.
    subroutine test_mm_build_matches_scan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(2000), probes(60), i, g, total, repeats
        integer(int64), allocatable :: pos(:), rows(:), dist(:)
        integer(int32), allocatable :: rows32(:)
        logical, allocatable :: seen(:)
        integer :: k

        call fill_repeating_keys(keys, 300_int64, 7_int64, 100_int64, 12345_int64)
        call distinct_in_order(keys, dist)
        ! Probes: every distinct key, plus absent keys below, between and above the fixture.
        do i = 1_int64, 60_int64
            if (i <= 40_int64) then
                probes(i) = dist(i)
            else
                probes(i) = 100_int64 + 7_int64 * (i - 41_int64) + 3_int64   ! between two keys
            end if
        end do
        probes(60) = 3_int64                                           ! below the fixture
        probes(59) = 100_int64 + 7_int64 * 300_int64 + 1_int64         ! above it
        repeats = 0_int64
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            call check(error, mm%ngroups() == size(dist, kind=int64), &
                "ngroups must equal the distinct count of the fixture on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, mm%nkeys() == 2000_int64, &
                "nkeys counts every stored row, repeats included, on " // trim(METHODS(k)))
            if (allocated(error)) return
            allocate(seen(mm%ngroups()))
            seen = .false.
            total = 0_int64
            do i = 1_int64, size(dist, kind=int64)
                call scan_positions(keys, dist(i), pos)
                call check(error, mm%count(dist(i)) == size(pos, kind=int64), &
                    "count disagreed with a scan of the fixture on " // trim(METHODS(k)))
                if (allocated(error)) return
                call check(error, mm%get_first(dist(i)) == pos(1), &
                    "get_first must be the lowest position holding the key on " // trim(METHODS(k)))
                if (allocated(error)) return
                call mm%get_all(dist(i), rows)
                call check(error, size(rows) == size(pos), &
                    "get_all must return one value per stored row on " // trim(METHODS(k)))
                if (allocated(error)) return
                call check(error, all(rows == pos), &
                    "get_all must list the positions ascending on " // trim(METHODS(k)))
                if (allocated(error)) return
                call mm%get_all(dist(i), rows32)
                call check(error, all(int(rows32, int64) == pos), &
                    "the int32 get_all must agree with the int64 one on " // trim(METHODS(k)))
                if (allocated(error)) return
                g = mm%get(dist(i))
                call check(error, g >= 1_int64 .and. g <= mm%ngroups(), &
                    "a group id must lie in 1 .. ngroups on " // trim(METHODS(k)))
                if (allocated(error)) return
                seen(g) = .true.
                total = total + size(pos, kind=int64)
                if (size(pos) > 1) repeats = repeats + 1_int64
            end do
            call check(error, all(seen), "the group ids must be dense: every id in 1 .. ngroups " // &
                "is some key's, on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, total == 2000_int64, &
                "the counts over the distinct keys must sum to the row count on " // trim(METHODS(k)))
            if (allocated(error)) return
            deallocate(seen)
            do i = 41_int64, 60_int64
                call check(error, mm%get(probes(i)) == 0_int64 .and. mm%count(probes(i)) == 0_int64 &
                    .and. mm%get_first(probes(i)) == 0_int64, &
                    "an absent key must answer 0 from get, count and get_first on " // trim(METHODS(k)))
                if (allocated(error)) return
                call mm%get_all(probes(i), rows)
                call check(error, allocated(rows), "get_all on an absent key allocates rather than not")
                if (allocated(error)) return
                call check(error, size(rows) == 0, &
                    "get_all on an absent key is zero-length on " // trim(METHODS(k)))
                if (allocated(error)) return
            end do
            call check(error, mm%max_multiplicity() >= 2_int64, &
                "the fixture repeats, so max_multiplicity must exceed 1 on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
        call check(error, repeats > 100_int64, &
            "fixture: most distinct keys must repeat, or this is a test of a map")
        if (allocated(error)) return
        ! `probes(1:40)` are hits; the vacuity guard for the misses is the loop above, which
        ! asserted on twenty of them.
        call check(error, all(probes(1:40) == dist(1:40)), "fixture: the first forty probes are hits")
    end subroutine test_mm_build_matches_scan

    !> `%probe_many`'s CSR equals `pf_match_all`'s, with and without masks on either side, for
    !! both answer kinds, on every backend; `n_matched` and `group_hit` equal what the test counts.
    subroutine test_mm_csr_equals_match_all(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(3000), probes(1500), i, nm, g
        integer(int64), allocatable :: off(:), m(:), off2(:), m2(:), moff2(:), mm2(:)
        integer(int32), allocatable :: m32(:)
        logical, allocatable :: hit(:), want_hit(:)
        logical :: pmask(1500), kmask(3000)
        integer :: k

        call fill_repeating_keys(keys, 400_int64, 1000_int64, 5000_int64, 777_int64)
        ! Probes: a hit for an even index, a miss (a key plus 7, between two keys) for an odd one.
        do i = 1_int64, 1500_int64
            probes(i) = keys(1_int64 + mod(i * 13_int64, 3000_int64))
            if (mod(i, 2_int64) == 1_int64) probes(i) = probes(i) + 7_int64
            pmask(i) = mod(i, 3_int64) /= 0_int64
        end do
        do i = 1_int64, 3000_int64
            kmask(i) = mod(i, 5_int64) /= 0_int64
        end do
        call pf_match_all(probes, keys, off2, m2)
        call check(error, size(m2, kind=int64) > 1500_int64, &
            "fixture: the pair count must exceed the probe count, or the keys did not repeat")
        if (allocated(error)) return
        call check(error, count(off2(2:1501) == off2(1:1500)) > 500, &
            "fixture: hundreds of probes must miss")
        if (allocated(error)) return
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            call mm%probe_many(probes, off, m, n_matched=nm, group_hit=hit)
            call check(error, size(off) == 1501, "offsets has one entry per probe plus one")
            if (allocated(error)) return
            call check(error, all(off == off2), &
                "probe_many's offsets must equal pf_match_all's on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, size(m) == size(m2), &
                "probe_many's pair count must equal pf_match_all's on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, all(m == m2), &
                "probe_many's matches must equal pf_match_all's, in order, on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, nm == count(off2(2:1501) > off2(1:1500), kind=int64), &
                "n_matched must count the probes with a non-empty range on " // trim(METHODS(k)))
            if (allocated(error)) return
            ! group_hit: a group is hit when some probe's key maps to it.
            allocate(want_hit(mm%ngroups()))
            want_hit = .false.
            do i = 1_int64, 1500_int64
                g = mm%get(probes(i))
                if (g > 0_int64) want_hit(g) = .true.
            end do
            call check(error, size(hit, kind=int64) == mm%ngroups(), &
                "group_hit has one entry per group on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, all(hit .eqv. want_hit), &
                "group_hit must flag exactly the groups some probe reached on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, count(want_hit) > 100 .and. count(.not. want_hit) > 0, &
                "fixture: some groups are hit and some are not, or group_hit was not tested")
            if (allocated(error)) return
            deallocate(want_hit)
            ! The int32 matches form.
            call mm%probe_many(probes, off, m32)
            call check(error, all(int(m32, int64) == m2), &
                "the int32 matches form must equal pf_match_all's on " // trim(METHODS(k)))
            if (allocated(error)) return
            ! Masks on both sides: pf_match_all's is_valid_left/right are the same contract.
            call pf_match_all(probes, keys, moff2, mm2, is_valid_left=pmask, is_valid_right=kmask)
            call mm%build(keys, method=trim(METHODS(k)), valid=kmask)
            call mm%probe_many(probes, off, m, valid=pmask, n_matched=nm)
            call check(error, all(off == moff2) .and. size(m) == size(mm2), &
                "with masks on both sides the CSR shape must equal pf_match_all's on " // &
                trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, all(m == mm2), &
                "with masks on both sides the matches must equal pf_match_all's on " // &
                trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, nm == count(moff2(2:1501) > moff2(1:1500), kind=int64), &
                "n_matched under masks must count the non-empty ranges on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, size(mm2) > 0 .and. size(mm2) < size(m2), &
                "fixture: the masked answer is non-empty and smaller than the unmasked one")
            if (allocated(error)) return
        end do
    end subroutine test_mm_csr_equals_match_all

    !> Within every group the stored values are in ascending order of POSITION, which is what
    !! `%get_first` rests on -- and with `values=` it is still position order, not value order.
    subroutine test_mm_ascending_within_group(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(1200), vals(1200), i, g, lo, hi, n_multi
        integer(int64), allocatable :: off(:), rows(:), dist(:)
        integer :: k

        call fill_repeating_keys(keys, 150_int64, 3_int64, 1_int64, 4242_int64)
        call distinct_in_order(keys, dist)
        do i = 1_int64, 1200_int64
            vals(i) = 1201_int64 - i                  ! DESCENDING with position
        end do
        n_multi = 0_int64
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            call mm%csr(off, rows)
            call check(error, off(1) == 1_int64 .and. off(mm%ngroups() + 1_int64) == 1201_int64, &
                "csr offsets start at 1 and end past the last row on " // trim(METHODS(k)))
            if (allocated(error)) return
            do g = 1_int64, mm%ngroups()
                lo = off(g)
                hi = off(g + 1_int64) - 1_int64
                call check(error, hi >= lo, "every group holds at least one row on " // trim(METHODS(k)))
                if (allocated(error)) return
                if (hi > lo) then
                    n_multi = n_multi + 1_int64
                    call check(error, all(rows(lo + 1:hi) > rows(lo:hi - 1)), &
                        "rows must be strictly ascending within a group on " // trim(METHODS(k)))
                    if (allocated(error)) return
                end if
                call check(error, all(keys(rows(lo:hi)) == keys(rows(lo))), &
                    "every row of a group holds the same key on " // trim(METHODS(k)))
                if (allocated(error)) return
            end do
            ! With descending values, the first value of a group is the LARGEST, because the
            ! order is by position: a scatter that sorted by value would fail here.
            call mm%build(keys, vals, method=trim(METHODS(k)))
            do i = 1_int64, size(dist, kind=int64)
                call mm%get_all(dist(i), rows)
                if (size(rows) > 1) then
                    call check(error, all(rows(2:) < rows(1:size(rows) - 1)), &
                        "with values descending in position, a group's values must descend: " // &
                        "the order is by position, not by value, on " // trim(METHODS(k)))
                    if (allocated(error)) return
                    call check(error, mm%get_first(dist(i)) == rows(1) .and. rows(1) == maxval(rows), &
                        "get_first is the value at the lowest position, here the largest, on " // &
                        trim(METHODS(k)))
                    if (allocated(error)) return
                end if
            end do
        end do
        call check(error, n_multi > 300_int64, "fixture: hundreds of groups have more than one row")
    end subroutine test_mm_ascending_within_group

    !> On the serial grouping pass, the k-th distinct key to appear gets group id k. Documented
    !! as this version's behaviour rather than a contract; the test pins what the doc says.
    subroutine test_mm_group_ids_first_appearance(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(800), i
        integer(int64), allocatable :: dist(:), list(:), groups(:)
        integer :: k

        call fill_repeating_keys(keys, 90_int64, 5_int64, 10_int64, 99_int64)
        call distinct_in_order(keys, dist)
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            do i = 1_int64, size(dist, kind=int64)
                call check(error, mm%get(dist(i)) == i, &
                    "the i-th distinct key in first-appearance order must be group i on " // &
                    trim(METHODS(k)))
                if (allocated(error)) return
            end do
            ! keys() paired with get_many is a permutation of 1 .. ngroups.
            call mm%keys(list)
            allocate(groups(size(list)))
            call mm%get_many(list, groups)
            call check(error, size(list, kind=int64) == mm%ngroups(), &
                "keys() lists one entry per group on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, minval(groups) == 1_int64 .and. maxval(groups) == mm%ngroups() .and. &
                sum(groups) == mm%ngroups() * (mm%ngroups() + 1_int64) / 2_int64, &
                "the groups of keys() are a permutation of 1 .. ngroups on " // trim(METHODS(k)))
            if (allocated(error)) return
            deallocate(groups)
        end do
        call check(error, size(dist) == 90, "fixture: all ninety distinct keys appear")
    end subroutine test_mm_group_ids_first_appearance

    !> A masked row is neither stored nor counted; the values stay the original row numbers; a
    !! key present only in masked rows is absent; an all-false mask builds an empty multimap.
    subroutine test_mm_valid_mask_build(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(1000), i, only_masked, nmask
        integer(int64), allocatable :: pos(:), rows(:), dist(:)
        logical :: mask(1000), none(1000)
        character(len=:), allocatable :: tok
        integer :: k

        call fill_repeating_keys(keys, 120_int64, 2_int64, 1_int64, 31_int64)
        do i = 1_int64, 1000_int64
            mask(i) = mod(i, 4_int64) /= 0_int64
        end do
        ! Row 8 is masked; give it a key nothing else has, so that key must come out absent.
        only_masked = 100000_int64
        keys(8) = only_masked
        call distinct_in_order(keys, dist)
        nmask = count(mask, kind=int64)
        none = .false.
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)), valid=mask)
            call check(error, mm%nkeys() == nmask, &
                "nkeys counts the unmasked rows only on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, mm%count(only_masked) == 0_int64 .and. mm%get(only_masked) == 0_int64, &
                "a key held only by a masked row is absent on " // trim(METHODS(k)))
            if (allocated(error)) return
            do i = 1_int64, size(dist, kind=int64)
                call scan_positions(keys, dist(i), pos, valid=mask)
                call mm%get_all(dist(i), rows)
                call check(error, size(rows) == size(pos), &
                    "get_all under a build mask must list the unmasked rows only on " // &
                    trim(METHODS(k)))
                if (allocated(error)) return
                if (size(pos) > 0) then
                    call check(error, all(rows == pos), &
                        "the values stay the ORIGINAL row numbers under a build mask on " // &
                        trim(METHODS(k)))
                    if (allocated(error)) return
                end if
            end do
            ! Row 4 is masked and its key is shared: that key's count is over the unmasked rows
            ! holding it, which is fewer than the rows holding it.
            call scan_positions(keys, keys(4), pos)
            call scan_positions(keys, keys(4), rows, valid=mask)
            call check(error, mm%count(keys(4)) == size(rows, kind=int64), &
                "a masked row's shared key is counted over the unmasked rows only on " // &
                trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, size(rows) >= 1 .and. size(rows) < size(pos), &
                "fixture: row 4's key repeats in an unmasked row, and row 4 itself is excluded")
            if (allocated(error)) return
            ! An all-false mask.
            call mm%build(keys, method=trim(METHODS(k)), valid=none)
            call check(error, mm%ngroups() == 0_int64 .and. mm%nkeys() == 0_int64, &
                "an all-false mask builds an empty multimap on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, mm%get(keys(1)) == 0_int64 .and. mm%count(keys(1)) == 0_int64, &
                "an empty multimap answers 0 on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_method(tok)
            call check(error, tok == trim(METHODS(k)), &
                "an empty build still reports the backend it was asked for")
            if (allocated(error)) return
        end do
    end subroutine test_mm_valid_mask_build

    !> `%get_first_many` equals a loop of `%get_first`, for both key kinds, both answer kinds,
    !! both ranks, with a mask, and reports `n_found`.
    subroutine test_mm_get_first_many_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(900), probes(400), want(400), got(400), pairs(900, 2), pprobe(400, 2), &
            i, nf
        integer(int32) :: got32(400), probes32(400)
        logical :: mask(400)
        integer :: k

        call fill_repeating_keys(keys, 80_int64, 3_int64, 50_int64, 555_int64)
        do i = 1_int64, 400_int64
            probes(i) = keys(1_int64 + mod(i * 7_int64, 900_int64))
            if (mod(i, 3_int64) == 0_int64) probes(i) = probes(i) + 1_int64   ! a miss
            mask(i) = mod(i, 5_int64) /= 0_int64
        end do
        probes32 = int(probes, int32)
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            do i = 1_int64, 400_int64
                want(i) = mm%get_first(probes(i))
            end do
            call mm%get_first_many(probes, got, n_found=nf)
            call check(error, all(got == want), &
                "get_first_many must equal a loop of get_first on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, nf == count(want /= 0_int64, kind=int64), &
                "n_found counts the non-zero answers on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes, got32)
            call check(error, all(int(got32, int64) == want), &
                "the int32 answer form of get_first_many agrees on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes32, got)
            call check(error, all(got == want), &
                "int32 keys answer the same on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_first_many(probes, got, valid=mask, n_found=nf)
            call check(error, all(got == merge(want, 0_int64, mask)), &
                "a masked probe answers 0 and the rest as before on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, nf == count(want /= 0_int64 .and. mask, kind=int64), &
                "n_found under a mask counts the unmasked hits on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
        call check(error, count(want /= 0_int64) > 200 .and. count(want == 0_int64) > 100, &
            "fixture: the probes mix hundreds of hits with hundreds of misses")
        if (allocated(error)) return
        ! Rank-2 keys, on the two backends that take them.
        pairs(:, 1) = keys
        pairs(:, 2) = mod(keys, 4_int64)
        pprobe(:, 1) = probes
        pprobe(:, 2) = mod(probes, 4_int64)
        do k = 1, 2
            call mm%build(pairs, method=trim(METHODS(k)))
            do i = 1_int64, 400_int64
                want(i) = mm%get_first(pprobe(i, :))
            end do
            call mm%get_first_many(pprobe, got)
            call check(error, all(got == want), &
                "a composite get_first_many must equal a loop of get_first on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
        call check(error, count(want /= 0_int64) > 200, "fixture: the composite probes hit in the hundreds")
    end subroutine test_mm_get_first_many_matches

    !> `%get_many` answers the group id per key, equal to a loop of `%get`, with `n_found`.
    subroutine test_mm_get_many_groups(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(700), probes(300), want(300), got(300), i, nf
        integer(int32) :: got32(300)
        integer :: k

        call fill_repeating_keys(keys, 60_int64, 10_int64, 1000_int64, 8_int64)
        do i = 1_int64, 300_int64
            probes(i) = keys(1_int64 + mod(i * 11_int64, 700_int64))
            if (mod(i, 4_int64) == 0_int64) probes(i) = probes(i) + 5_int64
        end do
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            do i = 1_int64, 300_int64
                want(i) = mm%get(probes(i))
            end do
            call mm%get_many(probes, got, n_found=nf)
            call check(error, all(got == want), "get_many must equal a loop of get on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, nf == count(want /= 0_int64, kind=int64), &
                "get_many's n_found counts the hits on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_many(probes, got32)
            call check(error, all(int(got32, int64) == want), &
                "the int32 group form agrees on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
        call check(error, count(want /= 0_int64) > 150 .and. count(want == 0_int64) > 50, &
            "fixture: hits and misses both in numbers")
    end subroutine test_mm_get_many_groups

    !> Composite keys group by the whole tuple, on both backends that take them, including at
    !! the widest tuple the module allows.
    subroutine test_mm_composite(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: pairs(1500, 2), probes(200, 2), wide(300, pf_index_max_components), i, j, &
            pairs_total, n_multi
        integer(int64), allocatable :: pos(:), rows(:), off(:), m(:), tuples(:,:)
        integer :: k

        do i = 1_int64, 1500_int64
            pairs(i, 1) = 1_int64 + mod(i * 7_int64, 30_int64)
            pairs(i, 2) = 1_int64 + mod(i * 13_int64, 11_int64)
        end do
        do i = 1_int64, 200_int64
            probes(i, 1) = 1_int64 + mod(i * 3_int64, 30_int64)
            probes(i, 2) = 1_int64 + mod(i * 5_int64, 13_int64)     ! 12 and 13 never appear
        end do
        do k = 1, 2
            call mm%build(pairs, method=trim(METHODS(k)))
            call check(error, mm%ncomponents() == 2, "a pair multimap has two components")
            if (allocated(error)) return
            n_multi = 0_int64
            do i = 1_int64, 1500_int64, 37_int64
                call scan_positions_n(pairs, pairs(i, :), pos)
                call check(error, mm%count(pairs(i, :)) == size(pos, kind=int64), &
                    "a tuple's count equals a scan on " // trim(METHODS(k)))
                if (allocated(error)) return
                call mm%get_all(pairs(i, :), rows)
                call check(error, all(rows == pos), "a tuple's rows equal a scan on " // trim(METHODS(k)))
                if (allocated(error)) return
                if (size(pos) > 1) n_multi = n_multi + 1_int64
            end do
            call check(error, n_multi > 20_int64, "fixture: the sampled tuples repeat")
            if (allocated(error)) return
            ! probe_many against a scan-built expectation.
            call mm%probe_many(probes, off, m)
            pairs_total = 0_int64
            do i = 1_int64, 200_int64
                call scan_positions_n(pairs, probes(i, :), pos)
                call check(error, off(i + 1_int64) - off(i) == size(pos, kind=int64), &
                    "a probe's range length equals the scan's count on " // trim(METHODS(k)))
                if (allocated(error)) return
                if (size(pos) > 0) then
                    call check(error, all(m(off(i):off(i + 1_int64) - 1_int64) == pos), &
                        "a probe's range holds the scan's rows, ascending, on " // trim(METHODS(k)))
                    if (allocated(error)) return
                end if
                pairs_total = pairs_total + size(pos, kind=int64)
            end do
            call check(error, size(m, kind=int64) == pairs_total .and. pairs_total > 200_int64, &
                "the pair count equals the scan's total and exceeds the probe count on " // &
                trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, count(off(2:201) == off(1:200)) > 20, "fixture: some tuple probes miss")
            if (allocated(error)) return
            call mm%keys(tuples)
            call check(error, size(tuples, 1, kind=int64) == mm%ngroups() .and. size(tuples, 2) == 2, &
                "keys() of a composite multimap is shaped (ngroups, 2) on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
        ! The widest tuple: only the first two components vary.
        wide = 3_int64
        do i = 1_int64, 300_int64
            wide(i, 1) = mod(i, 7_int64)
            wide(i, 2) = mod(i, 5_int64)
        end do
        do k = 1, 2
            call mm%build(wide, method=trim(METHODS(k)))
            call check(error, mm%ncomponents() == pf_index_max_components .and. mm%ngroups() == 35_int64, &
                "a 32-component multimap groups by the whole tuple on " // trim(METHODS(k)))
            if (allocated(error)) return
            do j = 1_int64, 300_int64, 41_int64
                call scan_positions_n(wide, wide(j, :), pos)
                call mm%get_all(wide(j, :), rows)
                call check(error, all(rows == pos), &
                    "a 32-component tuple's rows equal a scan on " // trim(METHODS(k)))
                if (allocated(error)) return
            end do
        end do
    end subroutine test_mm_composite

    !> `int32` keys, values and answers give the same multimap as `int64` ones.
    subroutine test_mm_int32_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: a, b
        integer(int64) :: keys(600), vals(600), probes(200), i
        integer(int32) :: keys32(600), vals32(600), probes32(200), pairs32(600, 2)
        integer(int64), allocatable :: offa(:), ma(:), offb(:), mb(:)
        integer(int32), allocatable :: mb32(:)
        integer :: k

        call fill_repeating_keys(keys, 70_int64, 4_int64, 20_int64, 2024_int64)
        do i = 1_int64, 600_int64
            vals(i) = 5_int64 * i + 1_int64
        end do
        keys32 = int(keys, int32)
        vals32 = int(vals, int32)
        do i = 1_int64, 200_int64
            probes(i) = keys(1_int64 + mod(i * 17_int64, 600_int64)) + merge(2_int64, 0_int64, mod(i, 3_int64) == 0_int64)
        end do
        probes32 = int(probes, int32)
        do k = 1, 3
            call a%build(keys, vals, method=trim(METHODS(k)))
            call b%build(keys32, vals32, method=trim(METHODS(k)))
            call a%csr(offa, ma)
            call b%csr(offb, mb)
            call check(error, size(offa) == size(offb) .and. size(ma) == size(mb), &
                "int32 and int64 builds have the same CSR shape on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, all(offa == offb) .and. all(ma == mb), &
                "int32 and int64 builds have the same CSR content on " // trim(METHODS(k)))
            if (allocated(error)) return
            call a%probe_many(probes, offa, ma)
            call b%probe_many(probes32, offb, mb32)
            call check(error, all(offa == offb) .and. all(ma == int(mb32, int64)), &
                "int32 probes with int32 matches equal the int64 forms on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, size(ma) > 200, "fixture: the probes match with repeats")
            if (allocated(error)) return
        end do
        ! int32 tuples.
        pairs32(:, 1) = keys32
        pairs32(:, 2) = int(mod(keys, 3_int64), int32)
        call b%build(pairs32, method="hash")
        call check(error, b%count([keys32(5), int(mod(keys(5), 3_int64), int32)]) == &
            b%count([keys(5), mod(keys(5), 3_int64)]), &
            "an int32 tuple lookup equals the int64 one")
        if (allocated(error)) return
        call check(error, b%count([keys(5), mod(keys(5), 3_int64)]) >= 1_int64, &
            "fixture: the looked-up tuple is present")
    end subroutine test_mm_int32_forms

    !> `%csr` and `%keys` answer the same arrays in `int32` as in `int64`.
    subroutine test_mm_csr_and_keys_int32(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(300), vals(300), pairs(300, 2), i
        integer(int64), allocatable :: off64(:), rows64(:), k64(:), t64(:,:)
        integer(int32), allocatable :: off32(:), rows32(:), k32(:), t32(:,:)

        call fill_repeating_keys(keys, 40_int64, 3_int64, 15_int64, 4242_int64)
        do i = 1_int64, 300_int64
            vals(i) = i
        end do
        call mm%build(keys, vals, method="sorted")
        call mm%csr(off64, rows64)
        call mm%csr(off32, rows32)
        call check(error, size(off64, kind=int64) > 1_int64, "the csr fixture must hold some groups")
        if (allocated(error)) return
        call check(error, size(off32, kind=int64) == size(off64, kind=int64) .and. &
                   size(rows32, kind=int64) == size(rows64, kind=int64), &
            "csr must answer the same shape in both kinds")
        if (allocated(error)) return
        call check(error, all(int(off32, kind=int64) == off64) .and. all(int(rows32, kind=int64) == rows64), &
            "csr must answer the same offsets and rows in both kinds")
        if (allocated(error)) return
        call mm%keys(k64)
        call mm%keys(k32)
        call check(error, size(k32, kind=int64) == size(k64, kind=int64) .and. all(int(k32, kind=int64) == k64), &
            "keys must answer the same keys in both kinds")
        if (allocated(error)) return
        ! A composite multimap, whose keys come back rank 2, and negative components with them.
        call mm%clear()
        do i = 1_int64, 300_int64
            pairs(i, 1) = mod(i, 7_int64) - 3_int64
            pairs(i, 2) = mod(i, 5_int64)
        end do
        call mm%build(pairs, vals, method="hash")
        call mm%keys(t64)
        call mm%keys(t32)
        call check(error, all(shape(t32) == shape(t64)) .and. all(int(t32, kind=int64) == t64), &
            "the rank-2 keys form must answer the same tuples in both kinds")
    end subroutine test_mm_csr_and_keys_int32

    !> Every MIXED `int32`/`int64` build and scalar lookup answers exactly what its `int64` sibling
    !! does.
    !!
    !! `test_mm_int32_forms` reaches the pairings where keys and values are narrow together; the
    !! generics also accept each MIXED pairing -- wide keys with narrow values, narrow keys with
    !! wide values, narrow keys with no values at all -- and every one of those is a specific of its
    !! own, carrying its own converted copy of one argument and forwarding the rest. A specific that
    !! converted the wrong argument, or dropped `method=`, would build a PLAUSIBLE multimap rather
    !! than fail, so each form is compared here against the `int64` build of the same fixture, CSR
    !! for CSR. The scalar lookups are compared the same way, answer against answer.
    subroutine test_mm_int32_mixed_builds_and_scalars(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: a, b, c, fresh
        integer(int64) :: keys(300), vals(300), pairs(300, 2), i
        integer(int64) :: probe64, tup64(2), lo64, hi64, lo32, hi32
        integer(int32) :: keys32(300), vals32(300), pairs32(300, 2), probe32, tup32(2)
        integer(int64), allocatable :: offa(:), ma(:), offb(:), mb(:), rows64(:)
        integer(int32), allocatable :: off32(:), rows32(:)

        call fill_repeating_keys(keys, 40_int64, 3_int64, 15_int64, 9797_int64)
        do i = 1_int64, 300_int64
            vals(i) = 2_int64 * i + 1_int64
            pairs(i, 1) = keys(i)
            pairs(i, 2) = mod(keys(i), 3_int64)
        end do
        keys32 = int(keys, int32)
        vals32 = int(vals, int32)
        pairs32 = int(pairs, int32)

        ! ---- rank-1 builds ----
        call a%build(keys, vals, method="hash")
        call a%csr(offa, ma)
        call b%build(keys, vals32, method="hash")
        call b%csr(offb, mb)
        call check(error, size(offa) == size(offb) .and. all(offa == offb) .and. all(ma == mb), &
            "build(int64 keys, int32 values) must equal the all-int64 build")
        if (allocated(error)) return
        call b%build(keys32, vals, method="hash")
        call b%csr(offb, mb)
        call check(error, size(offa) == size(offb) .and. all(offa == offb) .and. all(ma == mb), &
            "build(int32 keys, int64 values) must equal the all-int64 build")
        if (allocated(error)) return
        ! With no values a row's stored value is its POSITION, so the comparison is against the
        ! int64 no-values build rather than against the one carrying `vals`.
        call b%build(keys, method="hash")
        call b%csr(offb, mb)
        call c%build(keys32, method="hash")
        call c%csr(offa, ma)
        call check(error, size(offa) == size(offb) .and. all(offa == offb) .and. all(ma == mb), &
            "build(int32 keys) with no values must equal the int64 no-values build")
        if (allocated(error)) return

        ! ---- rank-2 builds ----
        call a%build(pairs, vals, method="hash")
        call a%csr(offa, ma)
        call b%build(pairs, vals32, method="hash")
        call b%csr(offb, mb)
        call check(error, size(offa) == size(offb) .and. all(offa == offb) .and. all(ma == mb), &
            "build(int64 tuples, int32 values) must equal the all-int64 build")
        if (allocated(error)) return
        call b%build(pairs32, vals, method="hash")
        call b%csr(offb, mb)
        call check(error, size(offa) == size(offb) .and. all(offa == offb) .and. all(ma == mb), &
            "build(int32 tuples, int64 values) must equal the all-int64 build")
        if (allocated(error)) return
        call b%build(pairs32, vals32, method="hash")
        call b%csr(offb, mb)
        call check(error, size(offa) == size(offb) .and. all(offa == offb) .and. all(ma == mb), &
            "build(int32 tuples, int32 values) must equal the all-int64 build")
        if (allocated(error)) return

        ! ---- scalar-key lookups, int32 against int64 ----
        call a%build(keys, vals, method="hash")
        probe64 = keys(7)
        probe32 = int(probe64, int32)
        call check(error, a%count(probe64) > 1_int64, "fixture: the probed key must repeat")
        if (allocated(error)) return
        call check(error, a%get(probe32) == a%get(probe64), "get(int32 key) must equal get(int64 key)")
        if (allocated(error)) return
        call check(error, a%count(probe32) == a%count(probe64), &
            "count(int32 key) must equal count(int64 key)")
        if (allocated(error)) return
        call check(error, a%get_first(probe32) == a%get_first(probe64), &
            "get_first(int32 key) must equal get_first(int64 key)")
        if (allocated(error)) return
        call a%get_all(probe64, rows64)
        call a%get_all(probe32, rows32)
        call check(error, size(rows32) == size(rows64) .and. all(int(rows32, int64) == rows64), &
            "get_all(int32 key) into int32 rows must equal the int64 answer")
        if (allocated(error)) return
        deallocate(rows64)
        call a%get_all(probe32, rows64)
        call check(error, size(rows64) == size(rows32) .and. all(int(rows32, int64) == rows64), &
            "get_all(int32 key) into int64 rows must equal the int32 answer")
        if (allocated(error)) return
        call a%get_range(probe64, lo64, hi64)
        call a%get_range(probe32, lo32, hi32)
        call check(error, lo32 == lo64 .and. hi32 == hi64, &
            "get_range(int32 key) must equal get_range(int64 key)")
        if (allocated(error)) return

        ! ---- tuple lookups, int32 against int64 ----
        call c%build(pairs, vals, method="hash")
        tup64 = [pairs(7, 1), pairs(7, 2)]
        tup32 = int(tup64, int32)
        call check(error, c%count(tup64) >= 1_int64, "fixture: the probed tuple must be present")
        if (allocated(error)) return
        call check(error, c%get(tup32) == c%get(tup64), "get(int32 tuple) must equal get(int64 tuple)")
        if (allocated(error)) return
        call check(error, c%get_first(tup32) == c%get_first(tup64), &
            "get_first(int32 tuple) must equal get_first(int64 tuple)")
        if (allocated(error)) return
        deallocate(rows64, rows32)
        call c%get_all(tup64, rows64)
        call c%get_all(tup32, rows32)
        call check(error, size(rows32) == size(rows64) .and. all(int(rows32, int64) == rows64), &
            "get_all(int32 tuple) into int32 rows must equal the int64 answer")
        if (allocated(error)) return
        deallocate(rows64)
        call c%get_all(tup32, rows64)
        call check(error, size(rows64) == size(rows32) .and. all(int(rows32, int64) == rows64), &
            "get_all(int32 tuple) into int64 rows must equal the int32 answer")
        if (allocated(error)) return
        call c%get_range(tup64, lo64, hi64)
        call c%get_range(tup32, lo32, hi32)
        call check(error, lo32 == lo64 .and. hi32 == hi64 .and. hi64 >= lo64, &
            "get_range(int32 tuple) must equal get_range(int64 tuple)")
        if (allocated(error)) return

        ! ---- the int32 CSR of a multimap that was never built ----
        call fresh%csr(off32, rows32)
        call check(error, size(off32) == 1 .and. off32(1) == 1_int32 .and. size(rows32) == 0, &
            "an unbuilt multimap must answer an empty int32 CSR starting at 1")
    end subroutine test_mm_int32_mixed_builds_and_scalars

    !> Every MIXED `int32`/`int64` BULK lookup answers exactly what its `int64` sibling does.
    !!
    !! The bulk forms multiply out further than the scalar ones -- the key array and the answer
    !! array carry independent kinds -- and each combination is a specific with its own loop, its
    !! own narrowing check and its own optional forwarding. The probe sets below contain absent
    !! keys as well as present ones, for the reason this suite's header gives: a specific that
    !! answered something for an absent key would pass a hits-only comparison.
    subroutine test_mm_int32_mixed_bulk_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: a, c
        integer(int64) :: keys(300), vals(300), pairs(300, 2), pk(120), pp(120, 2), i, nf64, nf32
        integer(int32) :: keys32(300), pk32(120), pp32(120, 2)
        integer(int64) :: want64(120), got64(120)
        integer(int32) :: got32(120)
        integer(int64), allocatable :: offw(:), mw(:), offg(:), mg(:)
        integer(int32), allocatable :: mg32(:)

        call fill_repeating_keys(keys, 40_int64, 3_int64, 15_int64, 31337_int64)
        do i = 1_int64, 300_int64
            vals(i) = 2_int64 * i + 1_int64
            pairs(i, 1) = keys(i)
            pairs(i, 2) = mod(keys(i), 3_int64)
        end do
        keys32 = int(keys, int32)
        ! Every third probe is displaced by 1, which no key can equal (the distinct values are
        ! three apart), so the set is a mixture of hits and misses.
        do i = 1_int64, 120_int64
            pk(i) = keys(1_int64 + mod(i * 7_int64, 300_int64)) + merge(1_int64, 0_int64, mod(i, 3_int64) == 0_int64)
            pp(i, 1) = pk(i)
            pp(i, 2) = mod(pk(i), 3_int64)
        end do
        pk32 = int(pk, int32)
        pp32 = int(pp, int32)
        call a%build(keys, vals, method="hash")
        call c%build(pairs, vals, method="hash")

        ! ---- get_first_many ----
        call a%get_first_many(pk, want64, n_found=nf64)
        call check(error, nf64 > 0_int64 .and. nf64 < 120_int64, &
            "fixture: the probe set must contain both hits and misses")
        if (allocated(error)) return
        call a%get_first_many(pk32, got32, n_found=nf32)
        call check(error, nf32 == nf64 .and. all(int(got32, int64) == want64), &
            "get_first_many(int32 keys, int32 rows) must equal the int64 answer")
        if (allocated(error)) return
        call c%get_first_many(pp, want64)
        call c%get_first_many(pp32, got32)
        call check(error, all(int(got32, int64) == want64), &
            "get_first_many(int32 tuples, int32 rows) must equal the int64 answer")
        if (allocated(error)) return
        call c%get_first_many(pp32, got64)
        call check(error, all(got64 == want64), &
            "get_first_many(int32 tuples, int64 rows) must equal the int64 answer")
        if (allocated(error)) return

        ! ---- get_many ----
        call a%get_many(pk, want64)
        call a%get_many(pk32, got32)
        call check(error, all(int(got32, int64) == want64), &
            "get_many(int32 keys, int32 groups) must equal the int64 answer")
        if (allocated(error)) return
        call a%get_many(pk32, got64)
        call check(error, all(got64 == want64), &
            "get_many(int32 keys, int64 groups) must equal the int64 answer")
        if (allocated(error)) return
        call c%get_many(pp, want64)
        call c%get_many(pp32, got32)
        call check(error, all(int(got32, int64) == want64), &
            "get_many(int32 tuples, int32 groups) must equal the int64 answer")
        if (allocated(error)) return
        call c%get_many(pp32, got64)
        call check(error, all(got64 == want64), &
            "get_many(int32 tuples, int64 groups) must equal the int64 answer")
        if (allocated(error)) return
        call c%get_many(pp, got32)
        call check(error, all(int(got32, int64) == want64), &
            "get_many(int64 tuples, int32 groups) must equal the int64 answer")
        if (allocated(error)) return

        ! ---- probe_many ----
        call a%probe_many(pk, offw, mw)
        call check(error, size(mw) > 120, "fixture: the probes must match with repeats")
        if (allocated(error)) return
        call a%probe_many(pk32, offg, mg)
        call check(error, all(offg == offw) .and. all(mg == mw), &
            "probe_many(int32 keys, int64 matches) must equal the int64 answer")
        if (allocated(error)) return
        call c%probe_many(pp, offw, mw)
        call c%probe_many(pp32, offg, mg32)
        call check(error, all(offg == offw) .and. all(int(mg32, int64) == mw), &
            "probe_many(int32 tuples, int32 matches) must equal the int64 answer")
        if (allocated(error)) return
        call c%probe_many(pp32, offg, mg)
        call check(error, all(offg == offw) .and. all(mg == mw), &
            "probe_many(int32 tuples, int64 matches) must equal the int64 answer")
        if (allocated(error)) return
        call c%probe_many(pp, offg, mg32)
        call check(error, all(offg == offw) .and. all(int(mg32, int64) == mw), &
            "probe_many(int64 tuples, int32 matches) must equal the int64 answer")
    end subroutine test_mm_int32_mixed_bulk_forms

    !> A zero-key build is valid on every backend, and an all-equal key array is one group.
    subroutine test_mm_empty_and_all_equal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: empty(0), pairs(0, 2), same(500), i, nm
        integer(int64), allocatable :: off(:), rows(:), m(:), list(:)
        logical, allocatable :: hit(:)
        character(len=:), allocatable :: tok
        integer :: k

        do k = 1, 3
            call mm%build(empty, method=trim(METHODS(k)))
            call check(error, mm%ngroups() == 0_int64 .and. mm%nkeys() == 0_int64 .and. &
                mm%max_multiplicity() == 0_int64, "a zero-key build is empty on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_method(tok)
            call check(error, tok == trim(METHODS(k)), "a zero-key build reports its backend")
            if (allocated(error)) return
            call mm%csr(off, rows)
            call check(error, size(off) == 1 .and. off(1) == 1_int64 .and. size(rows) == 0, &
                "an empty multimap's CSR is [1] and no rows on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%probe_many([1_int64, 2_int64, 3_int64], off, m, n_matched=nm, group_hit=hit)
            call check(error, all(off == 1_int64) .and. size(m) == 0 .and. nm == 0_int64 .and. &
                size(hit) == 0, "probing an empty multimap answers empty ranges on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%keys(list)
            call check(error, allocated(list) .and. size(list) == 0, &
                "keys() of an empty multimap is zero-length, never unallocated")
            if (allocated(error)) return
        end do
        call mm%build(pairs)
        call check(error, mm%ncomponents() == 2 .and. mm%get([1_int64, 1_int64]) == 0_int64, &
            "a zero-row composite build keeps its width and answers 0")
        if (allocated(error)) return
        same = 7_int64
        do k = 1, 3
            call mm%build(same, method=trim(METHODS(k)))
            call check(error, mm%ngroups() == 1_int64 .and. mm%max_multiplicity() == 500_int64, &
                "an all-equal key array is one group of every row on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%get_all(7_int64, rows)
            call check(error, size(rows) == 500 .and. all(rows == [(i, i = 1_int64, 500_int64)]), &
                "the one group lists every row ascending on " // trim(METHODS(k)))
            if (allocated(error)) return
            call mm%probe_many([7_int64, 8_int64], off, m)
            call check(error, all(off == [1_int64, 501_int64, 501_int64]) .and. size(m) == 500, &
                "a probe of the one key takes every row and a miss takes none on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
    end subroutine test_mm_empty_and_all_equal

    !> `values=` are stored as given, in position order, and reported by every lookup.
    subroutine test_mm_explicit_values(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(400), vals(400), i
        integer(int64), allocatable :: pos(:), rows(:), dist(:), got(:)
        integer :: k

        call fill_repeating_keys(keys, 50_int64, 1_int64, 1_int64, 17_int64)
        do i = 1_int64, 400_int64
            vals(i) = 100_int64 + 3_int64 * i
        end do
        call distinct_in_order(keys, dist)
        allocate(got(size(dist)))
        do k = 1, 3
            call mm%build(keys, vals, method=trim(METHODS(k)))
            do i = 1_int64, size(dist, kind=int64)
                call scan_positions(keys, dist(i), pos)
                call mm%get_all(dist(i), rows)
                call check(error, all(rows == vals(pos)), &
                    "get_all returns the values of the key's rows, in position order, on " // &
                    trim(METHODS(k)))
                if (allocated(error)) return
                call check(error, mm%get_first(dist(i)) == vals(pos(1)), &
                    "get_first returns the value at the lowest position on " // trim(METHODS(k)))
                if (allocated(error)) return
            end do
            call mm%get_first_many(dist, got)
            call check(error, all(got == [(mm%get_first(dist(i)), i = 1_int64, size(dist, kind=int64))]), &
                "get_first_many reports the explicit values on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
        call check(error, size(dist) == 50, "fixture: every one of the fifty keys appears")
    end subroutine test_mm_explicit_values

    !> `%get_range` names the same slice of `%csr`'s rows that `%get_all` returns; absent is `1 .. 0`.
    subroutine test_mm_get_range_and_csr(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(500), lo, hi, i
        integer(int64), allocatable :: off(:), rows(:), all_rows(:), dist(:)
        integer :: k

        call fill_repeating_keys(keys, 40_int64, 6_int64, 3_int64, 1234_int64)
        call distinct_in_order(keys, dist)
        do k = 1, 3
            call mm%build(keys, method=trim(METHODS(k)))
            call mm%csr(off, rows)
            do i = 1_int64, size(dist, kind=int64)
                call mm%get_range(dist(i), lo, hi)
                call mm%get_all(dist(i), all_rows)
                call check(error, hi - lo + 1_int64 == size(all_rows, kind=int64) .and. &
                    lo == off(mm%get(dist(i))) .and. hi == off(mm%get(dist(i)) + 1_int64) - 1_int64, &
                    "get_range must name the group's slice of the CSR rows on " // trim(METHODS(k)))
                if (allocated(error)) return
                call check(error, all(rows(lo:hi) == all_rows), &
                    "the CSR slice get_range names must equal get_all on " // trim(METHODS(k)))
                if (allocated(error)) return
            end do
            call mm%get_range(dist(1) + 1_int64, lo, hi)
            call check(error, lo == 1_int64 .and. hi == 0_int64, &
                "an absent key's range is 1 .. 0 on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, mm%count(dist(1) + 1_int64) == 0_int64, "fixture: that probe is absent")
            if (allocated(error)) return
        end do
    end subroutine test_mm_get_range_and_csr

    !> The counts and the memory report describe the build, and `%clear` undoes all of it; an
    !! unbuilt multimap answers every query without aborting.
    subroutine test_mm_clear_and_introspection(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm, fresh
        integer(int64) :: keys(300), got(3), lo, hi, nm
        integer(int64), allocatable :: rows(:), off(:), m(:), list(:)
        character(len=:), allocatable :: tok

        call fill_repeating_keys(keys, 25_int64, 2_int64, 1_int64, 5_int64)
        ! Unbuilt: every lookup answers 0 or empty, and the counts are 0.
        call check(error, fresh%get(1_int64) == 0_int64 .and. fresh%count(1_int64) == 0_int64 .and. &
            fresh%get_first(1_int64) == 0_int64, "an unbuilt multimap answers 0")
        if (allocated(error)) return
        call fresh%get_all(1_int64, rows)
        call fresh%get_range(1_int64, lo, hi)
        call check(error, size(rows) == 0 .and. lo == 1_int64 .and. hi == 0_int64, &
            "an unbuilt multimap answers an empty range")
        if (allocated(error)) return
        call fresh%get_first_many([1_int64, 2_int64, 3_int64], got)
        call check(error, all(got == 0_int64), "an unbuilt multimap's bulk lookup answers zeros")
        if (allocated(error)) return
        call fresh%probe_many([1_int64, 2_int64], off, m, n_matched=nm)
        call check(error, all(off == 1_int64) .and. size(m) == 0 .and. nm == 0_int64, &
            "an unbuilt multimap's probe answers empty ranges")
        if (allocated(error)) return
        call fresh%get_method(tok)
        call check(error, tok == "" .and. fresh%ncomponents() == 0 .and. fresh%memory_bytes() == 0_int64, &
            "an unbuilt multimap has no backend, no components and no memory")
        if (allocated(error)) return
        call fresh%csr(off, m)
        call check(error, size(off) == 1 .and. off(1) == 1_int64 .and. size(m) == 0, &
            "an unbuilt multimap's CSR is [1] and no rows")
        if (allocated(error)) return
        ! Built.
        call mm%build(keys, method="hash")
        call check(error, mm%memory_bytes() >= 8_int64 * (300_int64 + 26_int64), &
            "memory_bytes covers at least the CSR pair")
        if (allocated(error)) return
        call check(error, mm%ngroups() == 25_int64 .and. mm%nkeys() == 300_int64 .and. &
            mm%ncomponents() == 1 .and. mm%max_multiplicity() >= 12_int64, &
            "the counts describe the build")
        if (allocated(error)) return
        call mm%get_method(tok)
        call check(error, tok == "hash", "get_method reports the forced backend")
        if (allocated(error)) return
        call mm%keys(list)
        call check(error, size(list) == 25, "keys() lists every distinct key")
        if (allocated(error)) return
        ! Cleared.
        call mm%clear()
        call check(error, mm%memory_bytes() == 0_int64 .and. mm%ngroups() == 0_int64 .and. &
            mm%nkeys() == 0_int64 .and. mm%max_multiplicity() == 0_int64 .and. mm%ncomponents() == 0, &
            "clear releases everything and zeroes every count")
        if (allocated(error)) return
        call check(error, mm%get(keys(1)) == 0_int64 .and. mm%count(keys(1)) == 0_int64, &
            "a cleared multimap answers 0")
        if (allocated(error)) return
        call mm%get_method(tok)
        call check(error, tok == "", "a cleared multimap reports no backend")
        if (allocated(error)) return
        call mm%get_all(keys(1), rows)
        call check(error, size(rows) == 0, "a cleared multimap's get_all is zero-length")
    end subroutine test_mm_clear_and_introspection

    !> The automatic backend is the map's rule applied to the DISTINCT keys: dense keys with
    !! repeats go direct, sparse go hash, and a key set whose span fits the budget of the ROWS but
    !! not of the distinct count goes hash -- the two-step rule.
    subroutine test_mm_auto_choice(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64), allocatable :: keys(:)
        character(len=:), allocatable :: tok
        integer(int64) :: i

        ! Dense with repeats: direct.
        allocate(keys(5000))
        do i = 1_int64, 5000_int64
            keys(i) = 1_int64 + mod(i * 7_int64, 1000_int64)
        end do
        call mm%build(keys)
        call mm%get_method(tok)
        call check(error, tok == "direct", "dense keys with repeats take the direct backend")
        if (allocated(error)) return
        call check(error, mm%ngroups() == 1000_int64 .and. mm%count(1_int64) == 5_int64, &
            "and the direct multimap answers")
        if (allocated(error)) return
        ! Sparse: hash.
        do i = 1_int64, 5000_int64
            keys(i) = (1_int64 + mod(i, 700_int64)) * 2654435761_int64
        end do
        call mm%build(keys)
        call mm%get_method(tok)
        call check(error, tok == "hash", "sparse keys take the hash backend")
        if (allocated(error)) return
        call check(error, mm%ngroups() == 700_int64, "and the hash multimap groups them")
        if (allocated(error)) return
        ! The two-step rule: 100000 rows over 20 keys spaced 10000 apart. The span, 190001, is
        ! within the budget for 100000 rows (400000 slots) but not within the budget for 20
        ! distinct keys (65536), so a rule applied to the rows would take direct and the map's
        ! own rule takes hash.
        deallocate(keys)
        allocate(keys(100000))
        do i = 1_int64, 100000_int64
            keys(i) = 10000_int64 * (1_int64 + mod(i, 20_int64))
        end do
        call mm%build(keys)
        call mm%get_method(tok)
        call check(error, tok == "hash", &
            "a span within the rows' budget but outside the distinct keys' budget must take hash: " // &
            "the automatic rule is the map's, applied to the distinct keys")
        if (allocated(error)) return
        call check(error, mm%ngroups() == 20_int64 .and. mm%count(10000_int64) == 5000_int64, &
            "and it groups the twenty keys")
        if (allocated(error)) return
        ! The same twenty keys spaced 1000 apart span 19001 slots, inside the 65536 floor: direct.
        do i = 1_int64, 100000_int64
            keys(i) = 1000_int64 * (1_int64 + mod(i, 20_int64))
        end do
        call mm%build(keys)
        call mm%get_method(tok)
        call check(error, tok == "direct", "twenty keys within the direct floor take direct")
        if (allocated(error)) return
        call check(error, mm%count(1000_int64) == 5000_int64, "and the direct multimap counts them")
        if (allocated(error)) return
        ! An explicit request is honoured either way.
        call mm%build(keys, method="hash")
        call mm%get_method(tok)
        call check(error, tok == "hash", "method=hash is honoured on dense keys")
        if (allocated(error)) return
        call mm%build(keys(1:200), method="direct")
        call mm%get_method(tok)
        call check(error, tok == "direct" .and. mm%count(1000_int64) == 10_int64, &
            "method=direct is honoured and answers")
    end subroutine test_mm_auto_choice

    !> A second build replaces the first: the old keys are absent and the new ones answer.
    subroutine test_mm_rebuild(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: a(300), b(200), i
        integer :: k

        call fill_repeating_keys(a, 30_int64, 1_int64, 1_int64, 3_int64)
        do i = 1_int64, 200_int64
            b(i) = 1000_int64 + mod(i, 40_int64)
        end do
        do k = 1, 3
            call mm%build(a, method=trim(METHODS(k)))
            call check(error, mm%count(a(1)) >= 1_int64, "the first build answers")
            if (allocated(error)) return
            call mm%build(b, method=trim(METHODS(k)))
            call check(error, mm%count(a(1)) == 0_int64 .and. mm%nkeys() == 200_int64 .and. &
                mm%ngroups() == 40_int64, "a rebuild forgets the previous keys on " // trim(METHODS(k)))
            if (allocated(error)) return
            call check(error, mm%count(1000_int64) == 5_int64 .and. mm%get_first(1000_int64) == 40_int64, &
                "and answers for the new ones on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
    end subroutine test_mm_rebuild

    !> A one-column rank-2 key array groups exactly as the rank-1 array does, on every backend.
    !!
    !! A multimap built from a slice of a table arrives rank-2 with one column, which takes the
    !! composite build path with `ncomp == 1`. Two of its arms exist only for that width: the
    !! sorted grouping reads the single column directly, and the direct grouping fills the scalar
    !! range beside the per-component geometry. A map that grouped differently there would answer
    !! a plausible group id rather than failing, so the assertion is against the rank-1 build.
    subroutine test_mm_rank2_single_column(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: m2, m1
        integer(int64) :: keys(200, 1), flat(200), i
        integer(int64), allocatable :: r2(:), r1(:), dist(:)
        integer :: k

        call fill_repeating_keys(flat, 25_int64, 4_int64, 2_int64, 99_int64)
        keys(:, 1) = flat
        call distinct_in_order(flat, dist)
        do k = 1, 3
            call m2%build(keys, method=trim(METHODS(k)))
            call m1%build(flat, method=trim(METHODS(k)))
            call check(error, m2%ngroups() == m1%ngroups(), &
                "a one-column build must find the same groups as a rank-1 build, on " // &
                trim(METHODS(k)))
            if (allocated(error)) return
            do i = 1_int64, size(dist, kind=int64)
                call m2%get_all(dist(i), r2)
                call m1%get_all(dist(i), r1)
                call check(error, size(r2, kind=int64) == size(r1, kind=int64), &
                    "and must hold the same rows per key, on " // trim(METHODS(k)))
                if (allocated(error)) return
                call check(error, all(r2 == r1), &
                    "with the same row numbers in the same order, on " // trim(METHODS(k)))
                if (allocated(error)) return
            end do
            ! A miss must stay a miss through the composite path too.
            call m2%get_all(maxval(flat) + 1_int64, r2)
            call check(error, size(r2, kind=int64) == 0_int64, &
                "an absent key must return no rows from a one-column build, on " // trim(METHODS(k)))
            if (allocated(error)) return
        end do
    end subroutine test_mm_rank2_single_column

    !> A composite multimap build stores the `values=` it was given, not the row numbers.
    !!
    !! `test_mm_explicit_values` makes this assertion for rank-1 keys. The composite build validates
    !! and carries the values through a separate path, and a fixture whose values are neither the row
    !! numbers nor in key order is what separates the two.
    subroutine test_mm_composite_explicit_values(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(pf_index_multimap) :: mm
        integer(int64) :: keys(12, 2), vals(12), probe(2), i, j
        integer(int64), allocatable :: rows(:)
        logical :: mask(12)
        character(len=6) :: methods(2)
        integer :: k

        ! Three distinct tuples, each appearing four times, interleaved.
        do i = 1_int64, 12_int64
            keys(i, 1) = 5_int64 + mod(i - 1_int64, 3_int64)
            keys(i, 2) = 40_int64 - mod(i - 1_int64, 3_int64)
            vals(i) = 1000_int64 - 7_int64 * i
        end do
        mask = .true.
        mask(4) = .false.
        mask(9) = .false.
        methods = ["direct", "hash  "]
        do k = 1, 2
            call mm%build(keys, vals, method=trim(methods(k)))
            do j = 0_int64, 2_int64
                probe = [5_int64 + j, 40_int64 - j]
                call mm%get_all(probe, rows)
                call check(error, size(rows, kind=int64) == 4_int64, &
                    "a composite group must hold every row of its tuple, on " // trim(methods(k)))
                if (allocated(error)) return
                call check(error, all(rows == [(vals(j + 1_int64 + 3_int64 * i), i = 0_int64, 3_int64)]), &
                    "and must report the caller's values in position order, on " // trim(methods(k)))
                if (allocated(error)) return
            end do
            ! The same build with a mask: the masked rows contribute neither a row nor a value.
            call mm%build(keys, vals, method=trim(methods(k)), valid=mask)
            call mm%get_all([5_int64, 40_int64], rows)
            call check(error, size(rows, kind=int64) == 3_int64 .and. all(rows /= vals(4)), &
                "a masked composite build must drop the masked row, on " // trim(methods(k)))
            if (allocated(error)) return
        end do
    end subroutine test_mm_composite_explicit_values

end module test_index_multimap
