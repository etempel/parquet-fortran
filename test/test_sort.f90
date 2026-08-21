!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for read-time sorting: `parquet_open_reader(..., sort_by=)`, the post-open
!> `parquet_reader_set_sort`, the `parquet_sortkey` key grammar, and the ordering rules
!> (direction, null and NaN placement, stability, multi-key precedence).
!>
!> Four things shape this suite:
!>
!> * **Ordering is asserted through the values that come back**, never through the permutation
!>   itself, which is not observable from Fortran. Every fixture therefore carries an `id` column
!>   whose value identifies the row it came from, so a sorted read of `id` states exactly which
!>   rows landed where.
!> * **Stability is a contract, not an accident.** The engine sorts with std::sort plus a
!>   tiebreaker on the row index, and the counting fast path is stable by construction; the
!>   all-keys-tied test is what fails if either loses that property.
!> * **The counting fast path is a second code path producing the same answer**, so it gets its
!>   own comparison against the comparator path via a debug hook, rather than being trusted.
!> * **Abort paths live elsewhere.** `error stop` kills the process, so every rejection (unknown
!>   column, vector key, unsupported type, malformed key, chunked read under a sort, ...) is a
!>   `sort_*` scenario in test/error_scenarios.f90, driven from test_errors.f90.
!>
!> Every test writes its own fixture under test_run/, with its own filename -- test-drive runs
!> the tests in a suite concurrently, so a shared path can be truncated by one test while
!> another is reading it.
module test_sort
    use parquet
    ! The C++ sort engine is TEST-ONLY and is not re-exported by the `parquet` facade: reaching it
    ! needs this import, which is what keeps it out of every other program's dependency graph.
    ! Importing it is also what BINDS it -- `parquet_debug_use_fortran_sort_engine` lives here and
    ! registers the engine's entry points as a side effect of being called.
    use parquet_sorting_oracle, only : parquet_debug_use_fortran_sort_engine
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_sort
    !
contains
    !
    subroutine collect_tests_sort(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("a single ascending key orders the rows", test_single_key_ascending), &
            new_unittest("a descending key reverses that order", test_single_key_descending), &
            new_unittest("the '-' shorthand equals an explicit desc", test_minus_shorthand), &
            new_unittest("direction words are case-insensitive", test_direction_case_insensitive), &
            new_unittest("a second key breaks the first key's ties", test_multi_key_tiebreak), &
            new_unittest("rows tied on every key keep file order", cpp_test_stability_full_tie), &
            new_unittest("nulls sort last by default", test_nulls_last_default), &
            new_unittest("nulls_first= puts them first instead", test_nulls_first_option), &
            new_unittest("descending does not move nulls", test_descending_keeps_nulls_last), &
            new_unittest("NaN sits between values and nulls", test_nan_between_values_and_nulls), &
            new_unittest("int64, float32 and boolean keys", test_other_scalar_key_types), &
            new_unittest("a string key orders lexicographically", test_string_key), &
            new_unittest("date, time and timestamp keys", test_temporal_keys), &
            new_unittest("a struct-leaf path is a valid key", test_struct_leaf_key), &
            new_unittest("sorting by a column that is never read", test_key_column_not_read), &
            new_unittest("the counting fast path matches the comparator", cpp_test_counting_path_matches), &
            new_unittest("table top_n selects rather than fully sorting", cpp_test_top_n_selects), &
            new_unittest("sort composes with a filter", test_sort_with_filter), &
            new_unittest("sort composes with a sample", test_sort_with_sample), &
            new_unittest("parquet_get_nrows is unchanged by sorting", test_nrows_unchanged), &
            new_unittest("row mode returns the SORTED row", test_row_mode_sorted), &
            new_unittest("element mode spans the sorted rows", test_element_mode_sorted), &
            new_unittest("parquet_reader_set_sort matches open-time sorting", test_set_sort_post_open), &
            new_unittest("a prefetched column comes back sorted", test_prefetch_is_sorted), &
            new_unittest("an empty sort_by at open time is a no-op", test_open_time_empty_sort_by_is_noop), &
            new_unittest("a TIME32 key and a foreign UINT64 key", test_time32_and_uint64_keys), &
            new_unittest("the read-time sort runs on the Fortran engine", test_read_time_sort_uses_fortran_engine), &
            new_unittest("two keys of DIFFERENT families do not share a staged reduction", &
                test_two_keys_different_families), &
            new_unittest("the same column as two keys binds twice, correctly", test_same_column_twice), &
            new_unittest("a released key column re-reads in sorted order", test_sort_key_read_back), &
            new_unittest("the install releases its key columns, and keeps them under prefetch", &
                test_sort_releases_key_columns) &
            ]
    end subroutine collect_tests_sort
    !
    !> Writes the fixture most tests below sort: `id` = 1..n identifies each row, `v` is the key
    !> to order by, and `txt` is a string companion. `v` is deliberately NOT monotone in `id`, so
    !> a sorted read of `id` shows the permutation directly.
    subroutine write_basic_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: id(6) = [1, 2, 3, 4, 5, 6]
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        character(len=4) :: txt(6) = ["dd  ", "bb  ", "ff  ", "aa  ", "cc  ", "ee  "]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "txt", txt)
        call parquet_close_writer(writer)
    end subroutine write_basic_fixture
    !
    !> Opens `file` sorted by one key and reads the `id` column back, which is the standard way
    !> every test here states "these rows, in this order".
    subroutine sorted_ids(file, key, ids, nulls_first)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: key !! one sort key, as %add takes it.
        integer(int32), allocatable, intent(out) :: ids(:) !! `id` column in sorted order.
        logical, intent(in), optional :: nulls_first !! forwarded to %add.
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int64) :: nrows

        if (present(nulls_first)) then
            call srt%add(key, nulls_first=nulls_first)
        else
            call srt%add(key)
        end if
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_get_nrows(reader, nrows)
        allocate(ids(nrows))
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
    end subroutine sorted_ids
    !
    !> The base case: v = [30,10,50,20,60,40] ascending puts row 2 (v=10) first and row 5 (v=60)
    !> last.
    subroutine test_single_key_ascending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_asc.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "v asc", ids)
        call check(error, all(ids == [2, 4, 1, 6, 3, 5]), "ascending by v must order the rows 2,4,1,6,3,5")
    end subroutine test_single_key_ascending
    !
    !> Descending is the exact reverse here, since v has no ties and no nulls.
    subroutine test_single_key_descending(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_desc.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "v desc", ids)
        call check(error, all(ids == [5, 3, 6, 1, 4, 2]), "descending by v must order the rows 5,3,6,1,4,2")
    end subroutine test_single_key_descending
    !
    !> "-v" is shorthand for "v desc" and must produce the identical order.
    subroutine test_minus_shorthand(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: shorthand(:), spelled(:)
        character(len=*), parameter :: file = "test_run/sort_minus.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "-v", shorthand)
        call sorted_ids(file, "v desc", spelled)
        call check(error, all(shorthand == spelled), "'-v' must order exactly as 'v desc' does")
    end subroutine test_minus_shorthand
    !
    !> Direction words are case-insensitive, and the long spellings are accepted too.
    subroutine test_direction_case_insensitive(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: upper(:), long(:), plain(:)
        character(len=*), parameter :: file = "test_run/sort_case.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "v DESC", upper)
        call sorted_ids(file, "v Descending", long)
        call sorted_ids(file, "v desc", plain)
        call check(error, all(upper == plain), "'v DESC' must order exactly as 'v desc' does")
        if (allocated(error)) return
        call check(error, all(long == plain), "'v Descending' must order exactly as 'v desc' does")
    end subroutine test_direction_case_insensitive
    !
    !> The primary key deliberately ties, so only a working second key can produce the expected
    !> order. `grp` is [1,1,2,2,1,2] and `v` breaks each group's tie.
    subroutine test_multi_key_tiebreak(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: id(6) = [1, 2, 3, 4, 5, 6]
        integer(int32) :: grp(6) = [1, 1, 2, 2, 1, 2]
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_multikey.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "grp", grp)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call srt%add("grp asc")
        call srt%add("v desc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        ! grp=1 rows are 1(v=30), 2(v=10), 5(v=60) -> by v desc: 5, 1, 2
        ! grp=2 rows are 3(v=50), 4(v=20), 6(v=40) -> by v desc: 3, 6, 4
        call check(error, all(ids == [5, 1, 2, 3, 6, 4]), "grp asc then v desc must order the rows 5,1,2,3,6,4")
    end subroutine test_multi_key_tiebreak
    !
    !> Every row ties on the only key, so the result must be the original file order.
    !>
    !> This is the stability contract, and it needs two things a naive version of this test does
    !> not have. First, it has to be checked on BOTH permutation paths, which achieve stability by
    !> different means: the counting fast path is stable by construction (it places its input in
    !> index order), while the comparator path is stable only because of its final tiebreaker on
    !> the row index. Second, the fixture has to be LARGE ENOUGH: std::sort falls back to insertion
    !> sort for small inputs, which never moves anything when every element compares equal, so a
    !> handful of rows passes even with the tiebreaker deleted. Both gaps were found by mutation
    !> testing -- deleting the tiebreaker survived a 6-row version of this test twice.
    subroutine test_stability_full_tie(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        integer, parameter :: n = 400
        integer(int32) :: id(n), flat(n)
        real(real64) :: flat_r(n)
        character(len=4) :: flat_s(n)
        integer(int32), allocatable :: ids(:)
        integer(int64) :: cmp_fast, cmp_slow
        integer :: i
        character(len=*), parameter :: file = "test_run/sort_stable.parquet"

        do i = 1, n
            id(i) = i
            flat(i) = 7
            flat_r(i) = 7.5_real64
            flat_s(i) = "same"
        end do
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "flat", flat)
        call parquet_write_column(writer, "flat_r", flat_r)
        call parquet_write_column(writer, "flat_s", flat_s)
        call parquet_close_writer(writer)

        ! 1. integer key, small range -> the counting fast path.
        call arm_sort_comparisons()
        call sorted_ids(file, "flat asc", ids)
        cmp_fast = sort_comparisons()
        call check(error, all(ids == id), "counting path: tied rows must keep their file order")
        if (allocated(error)) return
        deallocate(ids)

        ! 2. the same key with the fast path forced off -> the comparator and its tiebreaker.
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call sorted_ids(file, "flat asc", ids)
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, all(ids == id), "comparator path: tied rows must keep their file order")
        if (allocated(error)) return
        deallocate(ids)
        ! Steps 1 and 2 are the same assertion twice unless they reached different engines -- and
        ! step 2 is the only one of the four that exercises the comparator's index tiebreaker.
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "steps 1 and 2 must reach DIFFERENT engines, or the comparator tiebreaker goes untested")
        if (allocated(error)) return

        ! 3. and two key types that can never take the fast path at all.
        call sorted_ids(file, "flat_r asc", ids)
        call check(error, all(ids == id), "float key: tied rows must keep their file order")
        if (allocated(error)) return
        deallocate(ids)
        call sorted_ids(file, "flat_s asc", ids)
        call check(error, all(ids == id), "string key: tied rows must keep their file order")
    end subroutine test_stability_full_tie
    !
    !> Writes a key column with nulls: v = [30, null, 50, null, 60, 40] against id = 1..6.
    subroutine write_null_key_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: id(6) = [1, 2, 3, 4, 5, 6]
        integer(int32) :: v(6) = [30, 0, 50, 0, 60, 40]
        logical :: valid(6) = [.true., .false., .true., .false., .true., .true.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_close_writer(writer)
    end subroutine write_null_key_fixture
    !
    !> Nulls last is the default, and the two null rows keep their own relative order.
    subroutine test_nulls_last_default(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_nulls_last.parquet"

        call write_null_key_fixture(file)
        call sorted_ids(file, "v asc", ids)
        call check(error, all(ids == [1, 6, 3, 5, 2, 4]), "nulls must sort last, after 30,40,50,60")
    end subroutine test_nulls_last_default
    !
    !> nulls_first=.true. moves that key's nulls to the front, values otherwise unchanged.
    subroutine test_nulls_first_option(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_nulls_first.parquet"

        call write_null_key_fixture(file)
        call sorted_ids(file, "v asc", ids, nulls_first=.true.)
        call check(error, all(ids == [2, 4, 1, 6, 3, 5]), "nulls_first must put the null rows before the values")
    end subroutine test_nulls_first_option
    !
    !> Null placement is ABSOLUTE: ordering descending reverses the values but leaves the nulls at
    !> the end. Getting this backwards is the easiest possible mistake, and matching Arrow here is
    !> what keeps a cross-check against pyarrow agreeing row for row.
    subroutine test_descending_keeps_nulls_last(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_desc_nulls.parquet"

        call write_null_key_fixture(file)
        call sorted_ids(file, "v desc", ids)
        call check(error, all(ids == [5, 3, 6, 1, 2, 4]), "descending must reverse the values but keep nulls last")
    end subroutine test_descending_keeps_nulls_last
    !
    !> NaN is neither a value nor a null: it sorts after every real value but before the nulls.
    subroutine test_nan_between_values_and_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        integer(int32) :: id(5) = [1, 2, 3, 4, 5]
        real(real64) :: v(5)
        logical :: valid(5) = [.true., .true., .false., .true., .true.]
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_nan.parquet"

        v = [3.0_real64, 1.0_real64, 0.0_real64, 0.0_real64, 2.0_real64]
        v(4) = ieee_quiet_nan()
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_close_writer(writer)

        call sorted_ids(file, "v asc", ids)
        ! values 1.0(id 2), 2.0(id 5), 3.0(id 1), then NaN (id 4), then null (id 3)
        call check(error, all(ids == [2, 5, 1, 4, 3]), "NaN must sort after the values and before the nulls")
    end subroutine test_nan_between_values_and_nulls
    !
    !> A quiet NaN, built without tripping gfortran's compile-time checks on a literal 0/0.
    function ieee_quiet_nan() result(x)
        use ieee_arithmetic, only : ieee_value, ieee_quiet_nan_kind => ieee_quiet_nan
        real(real64) :: x !! a quiet NaN.
        x = ieee_value(x, ieee_quiet_nan_kind)
    end function ieee_quiet_nan
    !
    !> int64, float32 and boolean keys go through the same three comparison families the engine
    !> has (integer, real, and boolean-as-integer), so one test covers all three.
    subroutine test_other_scalar_key_types(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        integer(int32) :: id(4) = [1, 2, 3, 4]
        integer(int64) :: big(4) = [40_int64, 10_int64, 30_int64, 20_int64]
        real(real32) :: small(4) = [4.5_real32, 1.5_real32, 3.5_real32, 2.5_real32]
        logical :: flag(4) = [.true., .false., .true., .false.]
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_types.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "big", big)
        call parquet_write_column(writer, "small", small)
        call parquet_write_column(writer, "flag", flag)
        call parquet_close_writer(writer)

        call sorted_ids(file, "big asc", ids)
        call check(error, all(ids == [2, 4, 3, 1]), "int64 key must order the rows 2,4,3,1")
        if (allocated(error)) return
        deallocate(ids)
        call sorted_ids(file, "small asc", ids)
        call check(error, all(ids == [2, 4, 3, 1]), "float32 key must order the rows 2,4,3,1")
        if (allocated(error)) return
        deallocate(ids)
        ! .false. sorts before .true., and each group keeps file order (stability again).
        call sorted_ids(file, "flag asc", ids)
        call check(error, all(ids == [2, 4, 1, 3]), "boolean key must put .false. first, stably")
    end subroutine test_other_scalar_key_types
    !
    !> A string key orders lexicographically by bytes.
    subroutine test_string_key(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_string.parquet"

        call write_basic_fixture(file)
        ! txt = [dd, bb, ff, aa, cc, ee] -> aa(4), bb(2), cc(5), dd(1), ee(6), ff(3)
        call sorted_ids(file, "txt asc", ids)
        call check(error, all(ids == [4, 2, 5, 1, 6, 3]), "string key must order the rows 4,2,5,1,6,3")
    end subroutine test_string_key
    !
    !> date/time/timestamp keys: all three bind as integers in the engine, but each has its own
    !> concrete Arrow array class, so all three are exercised.
    subroutine test_temporal_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        integer(int32) :: id(4) = [1, 2, 3, 4]
        type(parquet_date) :: d(4)
        type(parquet_time) :: t(4)
        type(parquet_timestamp) :: ts(4)
        integer(int32), allocatable :: ids(:)
        integer :: i
        integer, parameter :: day(4) = [4, 1, 3, 2]
        character(len=*), parameter :: file = "test_run/sort_temporal.parquet"

        do i = 1, 4
            d(i) = parquet_date(2024, 1, day(i))
            call t(i)%set(day(i), 0, 0)
            call ts(i)%set(2024, 1, day(i), 0, 0, 0)
        end do
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "d", d)
        call parquet_write_column(writer, "t", t)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)

        call sorted_ids(file, "d asc", ids)
        call check(error, all(ids == [2, 4, 3, 1]), "date key must order the rows 2,4,3,1")
        if (allocated(error)) return
        deallocate(ids)
        call sorted_ids(file, "t asc", ids)
        call check(error, all(ids == [2, 4, 3, 1]), "time key must order the rows 2,4,3,1")
        if (allocated(error)) return
        deallocate(ids)
        call sorted_ids(file, "ts desc", ids)
        call check(error, all(ids == [1, 3, 4, 2]), "timestamp key descending must order the rows 1,3,4,2")
    end subroutine test_temporal_keys
    !
    !> A dotted struct-leaf path is a legal sort key, exactly as it is a legal filter column.
    subroutine test_struct_leaf_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int64) :: nrows
        integer(int32), allocatable :: ages(:)
        integer :: i
        logical :: ascending

        call srt%add("main.inner.age asc")
        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet", sort_by=srt)
        call parquet_get_nrows(reader, nrows)
        allocate(ages(nrows))
        ! The leaf carries nulls, and nulls sort last -- so filling them with huge() keeps the
        ! monotonicity check below valid rather than needing a separate null-aware pass.
        call parquet_read_column(reader, "main.inner.age", ages, null_value=huge(0_int32))
        call parquet_close_reader(reader)

        ascending = .true.
        do i = 2, int(nrows)
            if (ages(i) < ages(i - 1)) ascending = .false.
        end do
        call check(error, ascending, "a struct-leaf sort key must return that leaf in ascending order")
    end subroutine test_struct_leaf_key
    !
    !> The key column need not be one the caller ever reads -- it is decoded to build the
    !> permutation and then RELEASED, so nothing pays to reorder it. See
    !> `test_sort_releases_key_columns` for the half of that which is about the cache rather than
    !> the ordering, and `test_sort_key_read_back` for what happens when the caller does read it.
    subroutine test_key_column_not_read(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_key_unread.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "txt desc", ids)
        call check(error, all(ids == [3, 6, 1, 5, 2, 4]), "sorting by a column never read must still order the rows")
    end subroutine test_key_column_not_read
    !
    !> **Reading the sort key back after sorting by it.** The key column is decoded to build the
    !> permutation and then released (`parquet_reader_sort_install`), so this read decodes it a
    !> second time and is permuted by `apply_row_transform` on the ordinary read path rather than
    !> by anything the install did. That is the branch the release trades against, and the values
    !> coming back in order is what says the ordinary path really does apply the permutation.
    !>
    !> `id` is read alongside and cross-checked, so a permutation applied to one column but not
    !> the other cannot pass: the pairing (v, id) is fixed by the fixture.
    subroutine test_sort_key_read_back(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: vs(6), ids(6)
        !> `v` indexed by `id`, i.e. the fixture's own pairing, used as the cross-check below.
        integer(int32), parameter :: v_by_id(6) = [30, 10, 50, 20, 60, 40]
        character(len=*), parameter :: file = "test_run/sort_key_read_back.parquet"

        call write_basic_fixture(file)
        call srt%add("v asc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_column(reader, "v", vs)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, all(vs == [10, 20, 30, 40, 50, 60]), &
            "reading the sort key back must give it in sorted order, not physical order")
        if (allocated(error)) return
        call check(error, all(ids == [2, 4, 1, 6, 3, 5]), &
            "the row identities must agree with the key order")
        if (allocated(error)) return
        ! The pairing is the fixture's own, so this fails if one column was permuted and the
        ! other was not -- which is the shape a half-applied transform would take.
        call check(error, all(vs == v_by_id(ids)), &
            "the key values must still belong to the rows the identities name")
    end subroutine test_sort_key_read_back
    !
    !> **The install releases the key columns it decoded, and keeps them only under `prefetch=`.**
    !>
    !> Four arms, and the first and last are what stop the assertion being vacuous. A bare
    !> `released == 1` proves nothing on its own: the counter is process-global, so it has to be
    !> shown reading 0 for an open that installs no sort at all, and the `prefetch=.true.` arm has
    !> to be shown taking the other branch, or `keep_cache` could be deleted outright with only a
    !> benchmark to notice. `taken` and `released` are mutually exclusive per install by
    !> construction, so each arm asserts both.
    !>
    !> Both counters are maintainer-only C++ hooks, reached through a local `bind(C)` interface
    !> rather than `src/parquet_bindings.f90` -- this project's convention for anything that exists
    !> only for a test. The `sort` suite is excluded from test-drive's per-test parallelism
    !> (`test/run_tester.f90`), which is what makes a process-global counter safe to assert here.
    subroutine test_sort_releases_key_columns(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt1, srt2, srt3
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_release_keys.parquet"

        call write_basic_fixture(file)
        !
        ! (1) Negative control: no sort, so nothing is installed and neither counter moves.
        call reset_sort_phases()
        call parquet_open_reader(reader, file)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, sort_released_columns() == 0, "an unsorted open must release nothing")
        if (allocated(error)) return
        call check(error, sort_taken_columns() == 0, "an unsorted open must Take nothing")
        if (allocated(error)) return
        !
        ! (2) One key: the one column the bind decoded is released, and nothing is Taken.
        call reset_sort_phases()
        call srt1%add("v asc")
        call parquet_open_reader(reader, file, sort_by=srt1)
        call parquet_close_reader(reader)
        call check(error, sort_released_columns() == 1, "a one-key sort must release its one key column")
        if (allocated(error)) return
        call check(error, sort_taken_columns() == 0, "a one-key sort must not Take the key column")
        if (allocated(error)) return
        !
        ! (3) Two keys over two columns: both are released, which is what says the counter tracks
        ! the columns the bind actually decoded rather than being a constant.
        call reset_sort_phases()
        call srt2%add("v asc")
        call srt2%add("txt asc")
        call parquet_open_reader(reader, file, sort_by=srt2)
        call parquet_close_reader(reader)
        call check(error, sort_released_columns() == 2, "a two-key sort must release both key columns")
        if (allocated(error)) return
        !
        ! (4) prefetch=.true. takes the keep_cache branch instead: releasing there would only make
        ! the prefetch decode the same column again.
        call reset_sort_phases()
        call srt3%add("v asc")
        call parquet_open_reader(reader, file, sort_by=srt3, prefetch=.true.)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, sort_taken_columns() == 1, "a prefetching open must sort the cached key column in place")
        if (allocated(error)) return
        call check(error, sort_released_columns() == 0, "a prefetching open must not release anything")
        if (allocated(error)) return
        call check(error, all(ids == [2, 4, 1, 6, 3, 5]), "the prefetching open must still order the rows")
    end subroutine test_sort_releases_key_columns
    !
    !> Zeroes the read-time sort's phase counters, including the two column counts.
    subroutine reset_sort_phases()
        interface
            subroutine reset_phases() bind(C, name="parquet_debug_reset_sort_phase_nanos")
            end subroutine reset_phases
        end interface
        call reset_phases()
    end subroutine reset_sort_phases
    !
    !> Columns `parquet_reader_sort_install` re-Took in place since the last reset.
    function sort_taken_columns() result(k)
        integer(int64) :: k !! number of columns Taken.
        interface
            function got_taken() bind(C, name="parquet_debug_get_sort_take_columns") result(n)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: n
            end function got_taken
        end interface
        k = int(got_taken(), int64)
    end function sort_taken_columns
    !
    !> Columns `parquet_reader_sort_install` dropped from the cache since the last reset.
    function sort_released_columns() result(k)
        integer(int64) :: k !! number of columns released.
        interface
            function got_released() bind(C, name="parquet_debug_get_sort_released_columns") result(n)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: n
            end function got_released
        end interface
        k = int(got_released(), int64)
    end function sort_released_columns
    !
    !> Arms and zeroes the sort's comparison counter.
    !>
    !> **This is what makes the counting-path A/B tests in this file non-vacuous.** They run one
    !> fixture down both sort engines and assert the two agree -- but "both engines" is a claim about
    !> which code ran, and an equality assertion cannot see it. Turn parquet_set_sort_counting_path
    !> the wrong way round, or stop it reaching C++, and both halves take the SAME path: the
    !> comparison holds trivially and the test passes while testing nothing (feature_risks.md
    !> Risk-35). The counting path performs exactly zero comparisons by construction, so `0` on one
    !> half and nonzero on the other proves they really diverged.
    subroutine arm_sort_comparisons()
        interface
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero arms and zeroes the counter.
            end subroutine count_cmp
        end interface
        call count_cmp(1)
    end subroutine arm_sort_comparisons

    !> Comparisons counted since the last arm_sort_comparisons, then disarms the counter.
    integer(int64) function sort_comparisons() result(n)
        interface
            function got_cmp() bind(C, name="parquet_debug_get_sort_comparisons") result(k)
                use iso_c_binding, only : c_long_long
                integer(c_long_long) :: k !! comparisons since the counter was armed.
            end function got_cmp
            subroutine count_cmp(enable) bind(C, name="parquet_debug_set_count_sort_comparisons")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable
            end subroutine count_cmp
        end interface
        n = int(got_cmp(), int64)
        call count_cmp(0)
    end function sort_comparisons
    !
    !> `parquet_table%top_n` must SELECT, not sort the whole table and keep the front of it.
    !>
    !> **This is the only test that can tell those two apart.** A `%top_n` written as `%sort_by`
    !> followed by `%truncate` returns exactly the right rows in exactly the right order, so every
    !> correctness assertion in test_table.f90 passes against it -- the whole asymptotic argument for
    !> the feature would be gone with no test failing. Only the comparison counter sees it.
    !>
    !> Its shape is deliberate in three ways, each of which a simplification would undo:
    !>
    !> * **The key is float64 with distinct values**, so the integer counting fast path declines it.
    !>   That path performs zero comparisons by construction, and zero is not less than zero -- a
    !>   low-cardinality integer key would make both halves count 0 and the test would pass while
    !>   measuring nothing (feature_risks.md Risk-35).
    !> * **The fixture is 2000 rows, not a handful.** std::partial_sort saves roughly log(N)/log(n)
    !>   comparisons, which is only a visible margin once N is well past n.
    !> * **Sorting is forced serial**, because the counter is one process-global integer and a
    !>   threaded sort would have several threads incrementing it.
    !>
    !> It lives in this suite, not in `table`, because that counter and `parquet_set_sort_threads`
    !> are process-global: `run_tester.f90` excludes `sort` from per-test parallelism, and `table` is
    !> deliberately not excluded.
    subroutine test_top_n_selects(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t1, t2
        type(parquet_writer) :: w
        integer, parameter :: N = 2000
        real(real64) :: v(N)
        integer(int32) :: id(N)
        integer(int64) :: cmp_top, cmp_sort
        integer :: i, saved
        character(len=*), parameter :: f1 = "test_run/sort_top_n_sel1.parquet"
        character(len=*), parameter :: f2 = "test_run/sort_top_n_sel2.parquet"
        !
        do i = 1, N
            id(i) = int(i, int32)
            ! Distinct, unordered float64 keys: distinct so the counting path declines the column,
            ! unordered so neither engine gets a sorted input to shortcut on.
            v(i) = real(mod(i*7919, N), real64) + 0.5_real64
        end do
        call parquet_open_writer(w, f1)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        call parquet_open_writer(w, f2)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "v", v)
        call parquet_close_writer(w)
        !
        saved = parquet_get_sort_threads()
        call parquet_set_sort_threads(1)
        call parquet_open_table(t1, f1)
        call t1%materialize_all()
        call arm_sort_comparisons()
        call t1%top_n(["v"], 5)
        cmp_top = sort_comparisons()
        !
        call parquet_open_table(t2, f2)
        call t2%materialize_all()
        call arm_sort_comparisons()
        call t2%sort_by(["v"])
        call t2%truncate(5)
        cmp_sort = sort_comparisons()
        call parquet_set_sort_threads(saved)
        !
        ! The negative control: without this, a counter that never fires would pass the comparison
        ! below with two zeroes.
        call check(error, cmp_sort > 0_int64, &
            "the full-sort control must actually reach the comparator, or the margin below is vacuous")
        if (allocated(error)) return
        call check(error, cmp_top > 0_int64, &
            "top_n must reach the comparator too, or it took a fast path this test cannot measure")
        if (allocated(error)) return
        call check(error, cmp_top < cmp_sort, &
            "top_n must select rather than sort the whole table and keep the front of it")
        if (allocated(error)) return
        !
        ! And it must still be right: selection is only worth having if it answers correctly.
        call check(error, t1%nrows() == 5_int64 .and. t2%nrows() == 5_int64, &
            "both routes must leave five rows")
    end subroutine test_top_n_selects
    !
    !> The integer counting fast path and the comparator path must produce the SAME permutation.
    !> The fast path is the one place in the engine where a wrong answer would be fast rather than
    !> slow, so the two are compared directly on one fixture rather than assumed to agree. The
    !> debug hook is a local bind(C) interface, never part of the public API.
    subroutine test_counting_path_matches(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: fast(:), slow(:)
        integer(int64) :: cmp_fast, cmp_slow
        character(len=*), parameter :: file = "test_run/sort_counting.parquet"

        call write_basic_fixture(file)
        call arm_sort_comparisons()
        call sorted_ids(file, "v asc", fast)          ! small integer range -> counting path
        cmp_fast = sort_comparisons()
        call parquet_set_sort_counting_path(.false.)
        call arm_sort_comparisons()
        call sorted_ids(file, "v asc", slow)          ! same key, comparator path
        cmp_slow = sort_comparisons()
        call parquet_set_sort_counting_path(.true.)
        call check(error, cmp_fast == 0_int64 .and. cmp_slow > 0_int64, &
            "the two halves must reach DIFFERENT engines, or the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, all(fast == slow), "the counting fast path and the comparator must agree exactly")
        if (allocated(error)) return
        call check(error, all(fast == [2, 4, 1, 6, 3, 5]), "both paths must give the expected order")
    end subroutine test_counting_path_matches
    !
    !> The read-time sort must be ordered by the FORTRAN engine, and must agree with the C++
    !> reference implementation while doing it.
    !>
    !> **Two assertions, and the first is what stops the second being vacuous.** `parquet_apply_sort`
    !> builds its permutation with `pf_argsort`, which honours `dbg_fortran_engine` -- so flipping
    !> the selector really does move the read-time sort onto the other engine, and the Fortran-side
    !> observable moving in one arm and not the other is the proof that two different engines ran.
    !> Without that check an "identical results" test would pass just as happily against a routing
    !> that had been reverted to C++ on both arms.
    !>
    !> This is the regression test for R1: if the read-time sort is ever routed back to the C++
    !> engine, the first check fails rather than the suite quietly continuing to pass.
    subroutine test_read_time_sort_uses_fortran_engine(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: cmp_fortran, cmp_cpp
        integer(int32), allocatable :: ids_fortran(:), ids_cpp(:)
        character(len=*), parameter :: file = "test_run/sort_engine_routing.parquet"

        call write_basic_fixture(file)
        ! The counting path is disabled for BOTH arms, so the C++ arm reaches its comparator and
        ! the comparison counter can tell the engines apart at all -- on this fixture's small
        ! integer range it would otherwise take its own counting path and report zero comparisons,
        ! which is exactly what the Fortran arm reports, and the discriminator would be blind.
        call parquet_set_sort_counting_path(.false.)

        call arm_sort_comparisons()
        call sorted_ids(file, "v asc", ids_fortran)          ! shipped configuration
        cmp_fortran = sort_comparisons()

        call parquet_debug_use_fortran_sort_engine(.false.)
        call arm_sort_comparisons()
        call sorted_ids(file, "v asc", ids_cpp)              ! forced onto the C++ reference
        cmp_cpp = sort_comparisons()
        call parquet_debug_use_fortran_sort_engine(.true.)
        call parquet_set_sort_counting_path(.true.)

        call check(error, cmp_fortran == 0_int64, &
            "the read-time sort must reach the Fortran engine, which never calls the C++ comparator")
        if (allocated(error)) return
        call check(error, cmp_cpp > 0_int64, &
            "the forced arm must reach the C++ comparator, or the two arms ran the same engine " // &
            "and the agreement below is vacuous")
        if (allocated(error)) return
        call check(error, all(ids_fortran == ids_cpp), &
            "the Fortran engine and the C++ reference must order a read-time sort identically")
        if (allocated(error)) return
        call check(error, all(ids_fortran == [2, 4, 1, 6, 3, 5]), &
            "the read-time sort must give the expected order")
    end subroutine test_read_time_sort_uses_fortran_engine
    !
    !> Filter first, then sort within the survivors -- the documented composition order.
    subroutine test_sort_with_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_with_filter.parquet"

        call write_basic_fixture(file)
        call filt%add("v > 25")                        ! keeps rows 1(30), 3(50), 5(60), 6(40)
        call srt%add("v asc")
        call parquet_open_reader(reader, file, filter=filt, sort_by=srt)
        call parquet_get_nrows(reader, nrows)
        allocate(ids(nrows))
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, nrows == 4_int64, "the filter must leave 4 rows")
        if (allocated(error)) return
        call check(error, all(ids == [1, 6, 3, 5]), "the survivors must come back sorted among themselves")
    end subroutine test_sort_with_filter
    !
    !> A sample is the same mechanism as a filter, so a sort composes with it identically. The
    !> near-1.0 draw keeps every row, which makes the expected order exact.
    subroutine test_sort_with_sample(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int64) :: nrows
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_with_sample.parquet"

        call write_basic_fixture(file)
        call srt%add("v asc")
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=3_int64, sort_by=srt)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, nrows == 6_int64, "the near-1.0 draw must keep every row")
        if (allocated(error)) return
        call check(error, all(ids == [2, 4, 1, 6, 3, 5]), "a sampled reader must still sort its rows")
    end subroutine test_sort_with_sample
    !
    !> Sorting reorders rows, it never adds or removes them.
    subroutine test_nrows_unchanged(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int64) :: plain_rows, sorted_rows
        character(len=*), parameter :: file = "test_run/sort_nrows.parquet"

        call write_basic_fixture(file)
        call parquet_open_reader(reader, file)
        call parquet_get_nrows(reader, plain_rows)
        call parquet_close_reader(reader)

        call srt%add("v desc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_get_nrows(reader, sorted_rows)
        call parquet_close_reader(reader)
        call check(error, sorted_rows == plain_rows, "sorting must not change the row count")
    end subroutine test_nrows_unchanged
    !
    !> The trap this whole guard layer exists for: on a sorted reader, row-mode row `i` must be
    !> the i-th SORTED row, not the i-th physical file row. If the guard were missed, row mode
    !> would keep resolving a row group and quietly return physically ordered data -- everything
    !> else in this suite would still pass.
    subroutine test_row_mode_sorted(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: key(4) = [40, 10, 30, 20]
        integer(int32) :: vec(2, 4), row_back(2)
        integer :: i
        character(len=*), parameter :: file = "test_run/sort_row_mode.parquet"

        do i = 1, 4
            vec(:, i) = [10 * i + 1, 10 * i + 2]
        end do
        call parquet_open_writer(writer, file, chunk_size=2)
        call parquet_write_column(writer, "key", key)
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call srt%add("key asc")                        ! order: rows 2, 4, 3, 1
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_array_row_mode(reader, "vec", row_back, 1)
        call parquet_close_reader(reader)
        call check(error, all(row_back == [21, 22]), "sorted row 1 must be physical row 2, not physical row 1")
    end subroutine test_row_mode_sorted
    !
    !> Element mode's counterpart of the test above: one element position across every sorted row.
    subroutine test_element_mode_sorted(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: key(4) = [40, 10, 30, 20]
        integer(int32) :: vec(2, 4), elem_back(4)
        integer :: i
        character(len=*), parameter :: file = "test_run/sort_element_mode.parquet"

        do i = 1, 4
            vec(:, i) = [10 * i + 1, 10 * i + 2]
        end do
        call parquet_open_writer(writer, file, chunk_size=2)
        call parquet_write_column(writer, "key", key)
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call srt%add("key asc")                        ! order: rows 2, 4, 3, 1
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 1)
        call parquet_close_reader(reader)
        call check(error, all(elem_back == [21, 41, 31, 11]), &
            "element 1 across the sorted rows must follow the sorted order")
    end subroutine test_element_mode_sorted
    !
    !> The post-open setter must land the reader in exactly the state opening with sort_by= does.
    subroutine test_set_sort_post_open(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: ids(6)
        integer(int32), allocatable :: at_open(:)
        character(len=*), parameter :: file = "test_run/sort_set_post_open.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "v asc", at_open)

        call srt%add("v asc")
        call parquet_open_reader(reader, file)
        call parquet_reader_set_sort(reader, srt)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, all(ids == at_open), "parquet_reader_set_sort must match opening with sort_by=")
    end subroutine test_set_sort_post_open
    !
    !> A column decoded before the sort was built (here by prefetch=) must be re-ordered too, not
    !> left in physical order -- the cache is re-Taken when the permutation is installed.
    subroutine test_prefetch_is_sorted(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_prefetch.parquet"

        call write_basic_fixture(file)
        call srt%add("v asc")
        call parquet_open_reader(reader, file, sort_by=srt, prefetch=.true.)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, all(ids == [2, 4, 1, 6, 3, 5]), "a prefetched column must come back sorted")
    end subroutine test_prefetch_is_sorted
    !
    !> A `parquet_sortkey` with no `%add` calls yet is a legal (if pointless) `sort_by=` value --
    !! `parquet_apply_sort` (parquet_read.f90) short-circuits on `sort_by%n == 0` and installs
    !! nothing, so the file reads back in ordinary physical order. Only the open-time path can
    !! reach that short-circuit: `parquet_reader_set_sort` itself already declines an empty
    !! `sort_by` before ever calling `parquet_apply_sort`.
    subroutine test_open_time_empty_sort_by_is_noop(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_open_time_empty.parquet"

        call write_basic_fixture(file)
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, all(ids == [1, 2, 3, 4, 5, 6]), &
            "an empty sort_by must leave the file in its original physical row order")
    end subroutine test_open_time_empty_sort_by_is_noop
    !
    !> Every other temporal key in this suite (test_temporal_keys) uses a schema-less time column,
    !! which this library always stores as TIME64 (microseconds) -- so sort_bind_arrow_key's own
    !! TIME32 branch (parquet_wrapper.cpp) needs a column explicitly declared "time[ms]", which
    !! forces the TIME32 (milliseconds) physical representation. Paired here with a foreign-file
    !! UINT64 sort key, which this library's own writer never produces at all (it has no unsigned
    !! public type) -- test/fixtures/extended_types.parquet's v_uint64 column (values 1000, 0,
    !! 2000000000, id order 1,2,3 -- see scenario_print_stat_default_scalar_type in
    !! error_scenarios.f90 for the same fixture used the same way) is the only source of one.
    subroutine test_time32_and_uint64_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_schema) :: schema
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        type(parquet_time) :: clock(4)
        integer(int32) :: id(4) = [1, 2, 3, 4]
        integer(int32), allocatable :: ids(:)
        integer, parameter :: sec(4) = [4, 1, 3, 2]
        integer :: i
        character(len=*), parameter :: file = "test_run/sort_time32.parquet"

        call schema%init(table="t")
        call schema%add_field("id", "int32")
        call schema%add_field("clock", "time[ms]")
        call parquet_validate_maml(schema%maml)
        do i = 1, 4
            call clock(i)%set(0, 0, sec(i))
        end do
        call parquet_open_writer(writer, file, schema)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "clock", clock)
        call parquet_close_writer(writer)

        call sorted_ids(file, "clock asc", ids)
        call check(error, all(ids == [2, 4, 3, 1]), "TIME32 key must order the rows 2,4,3,1")
        if (allocated(error)) return

        block
            integer(int32) :: uids(3)
            call srt%add("v_uint64 asc")
            call parquet_open_reader(reader, "test/fixtures/extended_types.parquet", sort_by=srt)
            call parquet_read_column(reader, "id", uids)
            call parquet_close_reader(reader)
            call check(error, all(uids == [2, 1, 3]), "a foreign UINT64 key must order the rows 2,1,3")
        end block
    end subroutine test_time32_and_uint64_keys
    !
    ! ---- C++-engine pins ------------------------------------------------------------------
    !
    ! Every test wrapped below observes a C++-SIDE counter (`engine_comparisons`,
    ! `parquet_debug_sort_threads_used`, `parquet_debug_sort_merge_threads_used`), which the
    ! Fortran engine does not populate. Stage 6 made the Fortran engine the default, so each of
    ! these went from testing something to testing nothing -- and every one of them FAILED loudly
    ! rather than passing vacuously, because each carries the "this arm must really reach the
    ! path" control this project requires. That is the controls working exactly as intended.
    !
    ! Pinning is the right fix rather than re-pointing them at Fortran observables, because the
    ! C++ engine still ships and is still user-reachable: `parquet_open_reader(..., sort_by=)`
    ! and `parquet_reader_set_sort` call `sort_build_permutation_threaded` directly, with no
    ! engine selector anywhere in that path. These are that engine's only tests.
    !
    ! The wrapper shape (rather than a pin at the top of each body) is deliberate: these tests
    ! have up to five early `return`s, and a selector leaked on one of them would not fail the
    ! test that leaked it -- it would silently change which engine a LATER test measures.

    !> Pins the C++ engine for `test_top_n_selects` -- see the note above.
    subroutine cpp_test_top_n_selects(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_top_n_selects(error)
        call parquet_debug_use_fortran_sort_engine(.true.)   ! the shipped default; see the note above
    end subroutine cpp_test_top_n_selects

    !> Pins the C++ engine for `test_stability_full_tie` -- see the note above.
    subroutine cpp_test_stability_full_tie(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_stability_full_tie(error)
        call parquet_debug_use_fortran_sort_engine(.true.)   ! the shipped default
    end subroutine cpp_test_stability_full_tie
    !> Pins the C++ engine for `test_counting_path_matches` -- see the note above.
    subroutine cpp_test_counting_path_matches(error)
        type(error_type), allocatable, intent(out) :: error !! forwarded from the wrapped test.
        !
        call parquet_debug_use_fortran_sort_engine(.false.)
        call test_counting_path_matches(error)
        call parquet_debug_use_fortran_sort_engine(.true.)   ! the shipped default
    end subroutine cpp_test_counting_path_matches

    !
    !> **The regression test for the one-slot sort-key cache** (`sort_key_cache`,
    !! `src/parquet_wrapper.cpp`). Installing a read-time sort makes two crossings per key --
    !! `_key_info` binds the key to report its family and size, then `_key_fetch` copies the values
    !! out -- and the reduction is now handed from the first to the second through a single slot on
    !! the reader handle instead of being rebuilt.
    !!
    !! **What this catches, established by mutation rather than asserted.** Moving the `std::move`
    !! in `_key_info` above the size reads -- the live hazard in the change, since a moved-from
    !! vector reports size 0 and Fortran allocates its buffers from that -- aborts this suite
    !! outright: **exit 134, zero tests run**. Read the exit STATUS, not just the failure count; a
    !! grep for `[FAILED]` reports 0 for that mutation and looks like a survival.
    !!
    !! **What it does NOT catch, and no fixture here can.** Replacing the slot's four-field identity
    !! match with a bare "is anything staged" leaves all of this suite passing. `_key_info` and
    !! `_key_fetch` have one caller (`add_read_sort_key`, `src/parquet_read.f90`) which pairs them
    !! per key with identical arguments, so the slot always holds the key being fetched and the
    !! mismatch arm is unreachable from production. It is kept as defence and is `GCOVR_EXCL`'d;
    !! see its own note in `src/parquet_wrapper.cpp`.
    !!
    !! Both orderings are still exercised, because the staged key and the fetched key swap roles
    !! between them: integer-then-string stages an `ints` vector and fetches a `strs` one,
    !! string-then-integer the reverse. The first key deliberately TIES so the second is
    !! load-bearing.
    subroutine test_two_keys_different_families(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: id(6) = [1, 2, 3, 4, 5, 6]
        integer(int32) :: grp(6) = [2, 1, 2, 1, 2, 1]
        character(len=4) :: txt(6) = ["dd  ", "bb  ", "ff  ", "aa  ", "cc  ", "ee  "]
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_two_families.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "grp", grp)
        call parquet_write_column(writer, "txt", txt)
        call parquet_close_writer(writer)

        ! Integer key staged, string key fetched.
        ! grp=1 rows are 2(bb), 4(aa), 6(ee) -> by txt asc: 4, 2, 6
        ! grp=2 rows are 1(dd), 3(ff), 5(cc) -> by txt asc: 5, 1, 3
        call srt%add("grp asc")
        call srt%add("txt asc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, all(ids == [4, 2, 6, 5, 1, 3]), &
            "an int key followed by a string key must order the rows 4,2,6,5,1,3")
        if (allocated(error)) return

        ! String key staged, integer key fetched. txt is unique, so grp never breaks a tie here --
        ! the point is the family of the staged key, not the tiebreak.
        block
            type(parquet_sortkey) :: srt2
            call srt2%add("txt asc")
            call srt2%add("grp asc")
            call parquet_open_reader(reader, file, sort_by=srt2)
            call parquet_read_column(reader, "id", ids)
            call parquet_close_reader(reader)
        end block
        call check(error, all(ids == [4, 2, 5, 1, 6, 3]), &
            "a string key followed by an int key must order the rows 4,2,5,1,6,3")
    end subroutine test_two_keys_different_families
    !
    !> The same column used as BOTH keys: the slot must be filled, consumed and refilled once per
    !! key rather than served stale on the second.
    !!
    !! **What this does NOT test, stated so it is not mistaken for a guard.** The slot's identity
    !! match compares direction and null placement as well as the name, but the reduction itself is
    !! direction-independent -- `sort_bind_arrow_key` stores `descending`/`nulls_first` on the key
    !! and never lets them touch the values -- so a match on the name alone would return the same
    !! bytes here and this test would pass against it. The direction fields are defence against a
    !! future reduction that does depend on them; `test_two_keys_different_families` above is the
    !! one that actually discriminates today.
    subroutine test_same_column_twice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: id(6) = [1, 2, 3, 4, 5, 6]
        integer(int32) :: grp(6) = [2, 1, 2, 1, 2, 1]
        integer(int32) :: ids(6)
        character(len=*), parameter :: file = "test_run/sort_same_column_twice.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "grp", grp)
        call parquet_close_writer(writer)

        ! Every row within a group ties on the second key too, so the answer is the first key's
        ! order with file order inside each group -- i.e. the stability contract, reached through
        ! two binds of one column.
        call srt%add("grp asc")
        call srt%add("grp desc")
        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_column(reader, "id", ids)
        call parquet_close_reader(reader)
        call check(error, all(ids == [2, 4, 6, 1, 3, 5]), &
            "the same column as both keys must order the rows 2,4,6,1,3,5")
    end subroutine test_same_column_twice
end module test_sort
