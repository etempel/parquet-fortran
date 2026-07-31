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
            new_unittest("rows tied on every key keep file order", test_stability_full_tie), &
            new_unittest("nulls sort last by default", test_nulls_last_default), &
            new_unittest("nulls_first= puts them first instead", test_nulls_first_option), &
            new_unittest("descending does not move nulls", test_descending_keeps_nulls_last), &
            new_unittest("NaN sits between values and nulls", test_nan_between_values_and_nulls), &
            new_unittest("int64, float32 and boolean keys", test_other_scalar_key_types), &
            new_unittest("a string key orders lexicographically", test_string_key), &
            new_unittest("date, time and timestamp keys", test_temporal_keys), &
            new_unittest("a struct-leaf path is a valid key", test_struct_leaf_key), &
            new_unittest("sorting by a column that is never read", test_key_column_not_read), &
            new_unittest("the counting fast path matches the comparator", test_counting_path_matches), &
            new_unittest("sort composes with a filter", test_sort_with_filter), &
            new_unittest("sort composes with a sample", test_sort_with_sample), &
            new_unittest("parquet_get_nrows is unchanged by sorting", test_nrows_unchanged), &
            new_unittest("row mode returns the SORTED row", test_row_mode_sorted), &
            new_unittest("element mode spans the sorted rows", test_element_mode_sorted), &
            new_unittest("parquet_reader_set_sort matches open-time sorting", test_set_sort_post_open), &
            new_unittest("a prefetched column comes back sorted", test_prefetch_is_sorted), &
            new_unittest("an empty sort_by at open time is a no-op", test_open_time_empty_sort_by_is_noop) &
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
        integer :: i
        character(len=*), parameter :: file = "test_run/sort_stable.parquet"
        interface
            subroutine disable_counting(enable) bind(C, name="parquet_debug_set_disable_sort_counting_path")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the comparator path.
            end subroutine disable_counting
        end interface

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
        call sorted_ids(file, "flat asc", ids)
        call check(error, all(ids == id), "counting path: tied rows must keep their file order")
        if (allocated(error)) return
        deallocate(ids)

        ! 2. the same key with the fast path forced off -> the comparator and its tiebreaker.
        call disable_counting(1)
        call sorted_ids(file, "flat asc", ids)
        call disable_counting(0)
        call check(error, all(ids == id), "comparator path: tied rows must keep their file order")
        if (allocated(error)) return
        deallocate(ids)

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
    !> permutation and then simply stays cached, sorted like everything else.
    subroutine test_key_column_not_read(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: ids(:)
        character(len=*), parameter :: file = "test_run/sort_key_unread.parquet"

        call write_basic_fixture(file)
        call sorted_ids(file, "txt desc", ids)
        call check(error, all(ids == [3, 6, 1, 5, 2, 4]), "sorting by a column never read must still order the rows")
    end subroutine test_key_column_not_read
    !
    !> The integer counting fast path and the comparator path must produce the SAME permutation.
    !> The fast path is the one place in the engine where a wrong answer would be fast rather than
    !> slow, so the two are compared directly on one fixture rather than assumed to agree. The
    !> debug hook is a local bind(C) interface, never part of the public API.
    subroutine test_counting_path_matches(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: fast(:), slow(:)
        character(len=*), parameter :: file = "test_run/sort_counting.parquet"
        interface
            subroutine disable_counting(enable) bind(C, name="parquet_debug_set_disable_sort_counting_path")
                use iso_c_binding, only : c_int
                integer(c_int), value :: enable !! nonzero forces the comparator path.
            end subroutine disable_counting
        end interface

        call write_basic_fixture(file)
        call sorted_ids(file, "v asc", fast)          ! small integer range -> counting path
        call disable_counting(1)
        call sorted_ids(file, "v asc", slow)          ! same key, comparator path
        call disable_counting(0)
        call check(error, all(fast == slow), "the counting fast path and the comparator must agree exactly")
        if (allocated(error)) return
        call check(error, all(fast == [2, 4, 1, 6, 3, 5]), "both paths must give the expected order")
    end subroutine test_counting_path_matches
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
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=3, sort_by=srt)
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
end module test_sort
