!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `%build_index` and the `parquet_table_index` wrapper.
!!
!! **The oracle is the sorting layer, never a hand-written expectation.** `%find_many` must equal
!! `pf_match` over the key column and `%find_all` must equal `pf_match_all`'s ranges, on the same
!! arrays, so the tests assert that the index reproduces an established engine's answers rather
!! than numbers someone typed in -- and both oracles are independent of `parquet_index`, which is
!! the point.
!!
!! **Every fixture holds repeats, absent probes and a null row, and says so with a vacuity guard**
!! where a loop could otherwise assert nothing (`.claude/rules/testing.md`, "Assertions").
!!
!! The wrapper's two silent failure modes each get a test of their own: the key conversion (a NaN
!! that is TWO keys by payload, a -0.0 that misses +0.0, a null element that finds 1970-01-01) and
!! the staleness stamp (`%is_current` after a value write versus after a row change). The ABORTS
!! -- a stale query, a wrong key kind, a string column, a repeated key under `unique=.true.` --
!! live in test/error_scenarios.f90 as `table_index_*`. Every test that writes a file uses its own
!! path, since the suite runs its tests concurrently.
module test_table_index
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_index

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_index(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("find_many equals pf_match, find and count agree", test_find_matches_pf_match), &
            new_unittest("unique=.false.: find_all equals pf_match_all", test_find_all_matches_pf_match_all), &
            new_unittest("a real column: every NaN is one key, -0.0 finds +0.0", test_real_nan_is_one_key), &
            new_unittest("a float32 column takes real32 and real64 keys", test_real32_column), &
            new_unittest("a null key row is never found, and is not a repeat", test_null_rows_never_found), &
            new_unittest("date, time and timestamp columns, in memory", test_temporal_columns), &
            new_unittest("a timestamp[ms] file column matches an element by instant", &
                test_timestamp_across_units), &
            new_unittest("is_current: a value write keeps it, a row change stales it", test_stale_and_current), &
            new_unittest("find_many: int32 rows, threads=, n_found=", test_find_many_forms), &
            new_unittest("introspection and %clear", test_introspection_and_clear), &
            new_unittest("building into an object again replaces the index", test_rebuild_replaces), &
            new_unittest("an empty table indexes, and every query answers absent", test_empty_table), &
            new_unittest("build_index reads a column the table has not read yet", test_lazy_column_is_read), &
            new_unittest("queries are reads: no detach, no generation bump", test_query_is_a_read), &
            new_unittest("lookups from several threads agree with the serial ones", test_lookups_in_parallel), &
            new_unittest("a string column: find_all equals pf_match_all, find_many equals pf_match", &
                test_string_column), &
            new_unittest("find/find_all/find_many in their int32 row forms", &
                test_query_every_int32_row_form), &
            new_unittest("find_all over every temporal key, in both row widths", &
                test_find_all_every_temporal_key), &
            new_unittest("the remaining string and real64 query forms", &
                test_query_remaining_key_forms), &
            new_unittest("a null temporal key finds nothing in find_all, both widths", &
                test_find_all_null_temporal_key), &
            new_unittest("find_many on a unique index, in every key shape", &
                test_find_many_unique_index_key_shapes) &
            ]
    end subroutine collect_tests_table_index

    ! ---- fixtures ------------------------------------------------------------------------------

    !> Five rows, distinct int32 keys in no particular order, so a wrong row is a wrong answer.
    subroutine build_unique(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: id(5)
        real(real64) :: x(5)
        id = [40, 10, 30, 20, 50]
        x = [4.0_real64, 1.0_real64, 3.0_real64, 2.0_real64, 5.0_real64]
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("x", x)
    end subroutine build_unique

    !> Seven rows over three int64 keys, INTERLEAVED: 7 at rows 1, 3, 6; 3 at rows 2, 4, 7; 9 at
    !! row 5 alone -- so a wrapper that assumed equal keys were adjacent would fail here.
    subroutine build_groups(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int64) :: g(7)
        g = [7_int64, 3_int64, 7_int64, 3_int64, 9_int64, 7_int64, 3_int64]
        call parquet_new_table(t)
        call t%add_column("g", g)
    end subroutine build_groups

    ! ---- the tests -----------------------------------------------------------------------------

    !> `%find_many` against `pf_match`, then `%find` and `%count` against `%find_many`, on both
    !! key widths: an index over an int32 column answers an int64 key and the reverse, widened.
    subroutine test_find_matches_pf_match(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int32), allocatable :: id(:)
        integer(int32) :: probes32(6)
        integer(int64) :: probes64(6), rows(6), row, want_row
        integer(int64), allocatable :: oracle(:)
        integer :: i
        !
        call build_unique(t)
        call t%get("id", id)
        probes32 = [30, 10, 99, 50, 7, 40]
        probes64 = int(probes32, int64)
        call t%build_index("id", ix)
        call check(error, ix%is_current(), "a freshly built index is current")
        if (allocated(error)) return
        call check(error, ix%is_unique(), "the default is a unique index")
        if (allocated(error)) return
        call pf_match(probes32, id, oracle)
        call ix%find_many(probes32, rows)
        call check(error, all(rows == oracle), "find_many over int32 keys must equal pf_match")
        if (allocated(error)) return
        call ix%find_many(probes64, rows)
        call check(error, all(rows == oracle), "find_many over int64 keys must equal pf_match")
        if (allocated(error)) return
        call check(error, count(oracle == 0_int64) == 2, "the fixture must carry absent probes (vacuity guard)")
        if (allocated(error)) return
        do i = 1, 6
            call ix%find(probes32(i), row)
            call check(error, row == oracle(i), "find(int32) must agree with pf_match at every probe")
            if (allocated(error)) return
            call ix%find(probes64(i), want_row)
            call check(error, want_row == oracle(i), "find(int64) must agree with pf_match at every probe")
            if (allocated(error)) return
            call check(error, ix%count(probes64(i)) == merge(1_int64, 0_int64, oracle(i) > 0_int64), &
                "count on a unique index is 1 for a stored key and 0 otherwise")
            if (allocated(error)) return
        end do
        call check(error, ix%nkeys() == 5_int64, "nkeys is the number of indexed rows")
    end subroutine test_find_matches_pf_match

    !> A multimap index: `%find_all` equals `pf_match_all`'s range for every probe, `%find` is the
    !! lowest row of it, `%count` its length, and an absent key answers empty, 0 and 0.
    subroutine test_find_all_matches_pf_match_all(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64), allocatable :: g(:), off(:), matches(:), rows(:), rows_i32_want(:)
        integer(int32), allocatable :: rows32(:)
        integer(int64) :: probes(4), row, lo, hi
        integer :: i
        !
        call build_groups(t)
        call t%get("g", g)
        probes = [7_int64, 3_int64, 9_int64, 11_int64]
        call t%build_index("g", ix, unique=.false.)
        call check(error, .not. ix%is_unique(), "unique=.false. builds a multimap")
        if (allocated(error)) return
        call check(error, ix%nkeys() == 3_int64, "nkeys on a multimap is the DISTINCT key count")
        if (allocated(error)) return
        call pf_match_all(probes, g, off, matches)
        do i = 1, 4
            lo = off(i)
            hi = off(i + 1) - 1_int64
            call ix%find_all(probes(i), rows)
            call check(error, size(rows, kind=int64) == hi - lo + 1_int64, &
                "find_all must list exactly pf_match_all's rows for the key")
            if (allocated(error)) return
            if (hi >= lo) then
                call check(error, all(rows == matches(lo:hi)), "find_all must equal pf_match_all's range, in order")
                if (allocated(error)) return
                call ix%find(probes(i), row)
                call check(error, row == matches(lo), "find on a multimap is the LOWEST row of the key")
                if (allocated(error)) return
                ! The int32 rows form must carry the same rows.
                call ix%find_all(probes(i), rows32)
                rows_i32_want = int(rows32, int64)
                call check(error, all(rows_i32_want == matches(lo:hi)), "find_all into int32 rows agrees")
                if (allocated(error)) return
            else
                call ix%find(probes(i), row)
                call check(error, row == 0_int64, "find of an absent key is 0")
                if (allocated(error)) return
            end if
            call check(error, ix%count(probes(i)) == hi - lo + 1_int64, "count is the group's size")
            if (allocated(error)) return
        end do
        call check(error, off(5) - off(1) == 7_int64, "the fixture must have every row matched by a probe (vacuity)")
        if (allocated(error)) return
        call check(error, off(2) - off(1) == 3_int64, "the fixture must carry a repeated key (vacuity guard)")
    end subroutine test_find_all_matches_pf_match_all

    !> The canonicalising real key: two NaN rows are ONE key (and so a repeat), -0.0 and +0.0 are
    !! one key, and a NaN is a legal query key on both index kinds.
    subroutine test_real_nan_is_one_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        real(real64) :: x(6), nan
        integer(int64), allocatable :: rows(:)
        integer(int64) :: row
        !
        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        x = [1.5_real64, nan, -0.0_real64, 2.5_real64, -nan, 0.0_real64]
        call parquet_new_table(t)
        call t%add_column("x", x)
        call t%build_index("x", ix, unique=.false.)
        call check(error, ix%count(nan) == 2_int64, "every NaN is one key: two NaN rows count 2")
        if (allocated(error)) return
        call ix%find_all(nan, rows)
        call check(error, size(rows) == 2, "find_all(NaN) lists both NaN rows")
        if (allocated(error)) return
        call check(error, all(rows == [2_int64, 5_int64]), "find_all(NaN) lists rows 2 and 5, ascending")
        if (allocated(error)) return
        call ix%find(-0.0_real64, row)
        call check(error, row == 3_int64, "find(-0.0) is the first zero row")
        if (allocated(error)) return
        call ix%find(0.0_real64, row)
        call check(error, row == 3_int64, "find(+0.0) is the same row: the two zeros are one key")
        if (allocated(error)) return
        call ix%find_all(0.0_real64, rows)
        call check(error, all(rows == [3_int64, 6_int64]), "find_all(0.0) lists both zero rows")
        if (allocated(error)) return
        call ix%find(real(1.5_real64, real32), row)
        call check(error, row == 1_int64, "a real32 key finds a real64 column's row when the value is exact")
        if (allocated(error)) return
        ! A unique index over a column with ONE NaN row: NaN is a legal key.
        call parquet_new_table(t)
        call t%add_column("y", [1.5_real64, nan, 2.5_real64])
        call t%build_index("y", ix)
        call ix%find(nan, row)
        call check(error, row == 2_int64, "a unique index finds its NaN row through a NaN key")
    end subroutine test_real_nan_is_one_key

    !> A float32 column answers a real32 key and a real64 key with the same value; a real64 key
    !! the column cannot hold exactly is absent, not rounded onto a neighbour.
    subroutine test_real32_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        real(real32) :: x(4)
        integer(int64) :: row
        !
        x = [0.5_real32, 1.25_real32, 0.1_real32, 8.0_real32]
        call parquet_new_table(t)
        call t%add_column("x", x)
        call t%build_index("x", ix)
        call ix%find(1.25_real32, row)
        call check(error, row == 2_int64, "a real32 key finds the float32 row")
        if (allocated(error)) return
        call ix%find(1.25_real64, row)
        call check(error, row == 2_int64, "a real64 key with the same exact value finds it too")
        if (allocated(error)) return
        call ix%find(0.1_real64, row)
        call check(error, row == 0_int64, "the real64 0.1 is not the float32 0.1, so it is absent")
        if (allocated(error)) return
        call ix%find(real(0.1_real32, real64), row)
        call check(error, row == 3_int64, "the float32 0.1 widened to real64 is the stored key")
    end subroutine test_real32_column

    !> A null row is skipped at build: it is never found, it does not count, and its key
    !! repeating a stored one is NOT a duplicate under `unique=.true.`.
    subroutine test_null_rows_never_found(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: v(5), rows(3)
        !
        v = [5_int64, 6_int64, 5_int64, 8_int64, 6_int64]
        call parquet_new_table(t)
        call t%add_column("v", v)
        call t%set_null("v", 3_int64)
        call t%set_null("v", 5_int64)
        ! Rows 3 and 5 repeat rows 1 and 2; both are null, so this unique build must succeed.
        call t%build_index("v", ix)
        call check(error, ix%nkeys() == 3_int64, "three non-null rows are indexed")
        if (allocated(error)) return
        call ix%find_many([5_int64, 6_int64, 8_int64], rows)
        call check(error, all(rows == [1_int64, 2_int64, 4_int64]), "each key finds its non-null row")
        if (allocated(error)) return
        call check(error, ix%count(5_int64) == 1_int64, "the null repeat is not counted")
        if (allocated(error)) return
        ! And on a multimap: the null rows are not in any group.
        call t%build_index("v", ix, unique=.false.)
        call check(error, ix%count(5_int64) == 1_int64 .and. ix%count(6_int64) == 1_int64, &
            "a multimap over the same column has one row per key, the null rows excluded")
    end subroutine test_null_rows_never_found

    !> Date, time and timestamp columns built in memory: an element finds its row, a null element
    !! finds nothing (its raw storage is 0, which must not match a stored 1970-01-01), and a null
    !! ROW is never found through the element that would otherwise match it.
    subroutine test_temporal_columns(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_date) :: d(4), dq, dnull
        type(parquet_time) :: tm(4), tq
        type(parquet_timestamp) :: ts(4), tsq(3)
        integer(int64) :: row, rows(3)
        integer(int64), allocatable :: all_rows(:)
        !
        call d(1)%set(2024, 3, 1)
        call d(2)%set_raw(0_int32)        ! 1970-01-01: the day whose raw key is 0
        call d(3)%set(2024, 3, 1)
        call d(4)%set(1999, 12, 31)
        call tm(1)%set(12, 0, 0)
        call tm(2)%set(0, 0, 0)           ! midnight: raw key 0
        call tm(3)%set(23, 59, 59)
        call tm(4)%set(12, 0, 0)
        call ts(1)%set(2024, 3, 1, 12, 30, 0)
        call ts(2)%set(1970, 1, 1, 0, 0, 0)
        call ts(3)%set(2024, 3, 1, 12, 30, 0, 500)
        call ts(4)%set(2024, 3, 1, 12, 30, 1)
        call parquet_new_table(t)
        call t%add_column("d", d)
        call t%add_column("tm", tm)
        call t%add_column("ts", ts)
        call t%set_null("tm", 4_int64)
        !
        call t%build_index("d", ix, unique=.false.)
        call check(error, ix%kind() == PK_DATE, "the index reports the key column's kind")
        if (allocated(error)) return
        call dq%set(2024, 3, 1)
        call ix%find_all(dq, all_rows)
        call check(error, all(all_rows == [1_int64, 3_int64]), "a date finds both of its rows")
        if (allocated(error)) return
        call dnull%set_null()
        call ix%find(dnull, row)
        call check(error, row == 0_int64, "a NULL date key finds nothing -- not the 1970-01-01 row")
        if (allocated(error)) return
        call check(error, ix%count(dnull) == 0_int64, "a null date key counts 0")
        if (allocated(error)) return
        call dq%set_raw(0_int32)
        call ix%find(dq, row)
        call check(error, row == 2_int64, "the genuine 1970-01-01 row is found by its own date")
        if (allocated(error)) return
        !
        call t%build_index("tm", ix)
        call tq%set(12, 0, 0)
        call ix%find(tq, row)
        call check(error, row == 1_int64, "a time finds its row (row 4 held it too, but is null)")
        if (allocated(error)) return
        call check(error, ix%nkeys() == 3_int64, "the null time row is not indexed")
        if (allocated(error)) return
        call tq%set_null()
        call ix%find(tq, row)
        call check(error, row == 0_int64, "a null time key finds nothing -- not the midnight row")
        if (allocated(error)) return
        !
        call t%build_index("ts", ix)
        call check(error, ix%kind() == PK_TIMESTAMP .and. ix%nkeys() == 4_int64, "four distinct instants")
        if (allocated(error)) return
        call tsq(1)%set(2024, 3, 1, 12, 30, 0, 500)
        call tsq(2)%set(2024, 3, 1, 12, 30, 0)
        call tsq(3)%set_null()
        call ix%find_many(tsq, rows)
        call check(error, all(rows == [3_int64, 1_int64, 0_int64]), &
            "instants differing by 500 ns are two keys, and a null element answers 0")
        if (allocated(error)) return
        call ix%find(tsq(1), row)
        call check(error, row == 3_int64 .and. ix%count(tsq(2)) == 1_int64, "find and count on a timestamp key")
    end subroutine test_temporal_columns

    !> A column read from a `timestamp[ms]` file is keyed by the instant, not by the stored unit:
    !! an element set at the same civil time finds it, and one 500 microseconds later -- which the
    !! column could never hold -- is absent rather than rounded.
    subroutine test_timestamp_across_units(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: file = "test_run/table_index_ts_units.parquet"
        type(parquet_writer) :: w
        type(parquet_schema) :: schema
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_timestamp) :: ts(3), q(3)
        integer(int64) :: rows(3)
        integer :: i
        !
        do i = 1, 3
            call ts(i)%set(2024, 1, 31, 12, 30, i - 1)
        end do
        call schema%init("table_index_ts")
        call schema%add_field("ts", "timestamp[ms]")
        call parquet_open_writer(w, file, schema=schema)
        call parquet_write_column(w, "ts", ts)
        call parquet_close_writer(w)
        call parquet_open_table(t, file)
        call t%build_index("ts", ix)
        call q(1)%set(2024, 1, 31, 12, 30, 2)
        call q(2)%set(2024, 1, 31, 12, 30, 0)
        call q(3)%set(2024, 1, 31, 12, 30, 1, 500000)   ! +500 us: not representable at [ms]
        call ix%find_many(q, rows)
        call check(error, all(rows == [3_int64, 1_int64, 0_int64]), &
            "a timestamp[ms] column is found by instant; a finer instant is absent, not rounded")
    end subroutine test_timestamp_across_units

    !> The staleness stamp: a value write (`%set`, `%fillna`) leaves the index usable and
    !! answering; a row-structural change stales it -- `%is_current()` answers .false., and the
    !! abort every query then raises is `table_index_stale_*` in test/error_scenarios.f90.
    subroutine test_stale_and_current(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        logical :: keep(5)
        !
        call build_unique(t)
        call t%set_null("x", 2_int64)
        call t%build_index("id", ix)
        call t%set_element("x", 4_int64, 22.0_real64)
        call check(error, ix%is_current(), "a value write does not stale the index")
        if (allocated(error)) return
        call t%fillna(["x"], -1.0_real64)
        call check(error, ix%is_current(), "a fill does not stale the index")
        if (allocated(error)) return
        call ix%find(30_int64, row)
        call check(error, row == 3_int64, "and it still answers")
        if (allocated(error)) return
        keep = [.true., .false., .true., .true., .true.]
        call t%filter_rows(keep)
        call check(error, .not. ix%is_current(), "a row change stales the index")
        if (allocated(error)) return
        call t%build_index("id", ix)
        call check(error, ix%is_current(), "rebuilding makes it current again")
        if (allocated(error)) return
        call ix%find(30_int64, row)
        call check(error, row == 2_int64, "and the rebuilt index answers the NEW row numbers")
    end subroutine test_stale_and_current

    !> `%find_many` into int32 rows, with an explicit team, and with the hit count; every form
    !! must give the int64 form's answers.
    subroutine test_find_many_forms(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: keys(6), want(6), got(6), nf
        integer(int32) :: got32(6)
        !
        call build_groups(t)
        call t%build_index("g", ix, unique=.false.)
        keys = [3_int64, 11_int64, 7_int64, 9_int64, 3_int64, 0_int64]
        call ix%find_many(keys, want)
        call check(error, all(want == [2_int64, 0_int64, 1_int64, 5_int64, 2_int64, 0_int64]), &
            "find_many on a multimap answers each key's first row")
        if (allocated(error)) return
        call ix%find_many(keys, got32, n_found=nf)
        call check(error, all(int(got32, int64) == want) .and. nf == 4_int64, "int32 rows and n_found agree")
        if (allocated(error)) return
        call ix%find_many(keys, got, threads=1)
        call check(error, all(got == want), "threads=1 answers the same")
        if (allocated(error)) return
        call ix%find_many(keys, got, threads=4, n_found=nf)
        call check(error, all(got == want) .and. nf == 4_int64, "threads=4 answers the same, with the count")
        if (allocated(error)) return
        call t%build_index("g", ix, unique=.false., threads=2)
        call ix%find_many(keys, got, n_found=nf)
        call check(error, all(got == want) .and. nf == 4_int64, "a build with threads= answers the same")
        if (allocated(error)) return
        ! The unique map counts its hits in the wrapper, since the map's %get_many has no n_found.
        call build_unique(t)
        call t%build_index("id", ix)
        call ix%find_many([10_int64, 99_int64, 50_int64], got(1:3), n_found=nf)
        call check(error, nf == 2_int64 .and. all(got(1:3) == [2_int64, 0_int64, 5_int64]), &
            "n_found on a unique index counts the non-zero answers")
    end subroutine test_find_many_forms

    !> `%is_unique`, `%nkeys`, `%kind`, `%name` before and after a build, and `%clear` returning
    !! the object to its never-built state.
    subroutine test_introspection_and_clear(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        character(len=:), allocatable :: nm
        !
        call check(error, .not. ix%is_current() .and. ix%nkeys() == 0_int64 .and. ix%kind() == PK_NONE, &
            "a never-built index is not current, holds no keys and has no kind")
        if (allocated(error)) return
        call ix%name(nm)
        call check(error, nm == "", "a never-built index has no column name")
        if (allocated(error)) return
        call build_unique(t)
        call t%build_index("id", ix)
        call ix%name(nm)
        call check(error, nm == "id" .and. ix%kind() == PK_INT32 .and. ix%nkeys() == 5_int64 .and. ix%is_unique(), &
            "name, kind, nkeys and uniqueness after a build")
        if (allocated(error)) return
        call ix%clear()
        call ix%name(nm)
        call check(error, .not. ix%is_current() .and. ix%nkeys() == 0_int64 .and. nm == "" .and. ix%kind() == PK_NONE, &
            "clear returns the object to its never-built state")
    end subroutine test_introspection_and_clear

    !> `ix` is `intent(out)`: a second build into the same object -- over another column, of
    !! another kind, with the other uniqueness -- replaces the first entirely.
    subroutine test_rebuild_replaces(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        character(len=:), allocatable :: nm
        !
        call build_unique(t)
        call t%add_column("dup", [1_int64, 2_int64, 1_int64, 2_int64, 3_int64])
        call t%build_index("id", ix)
        call t%build_index("dup", ix, unique=.false.)
        call ix%name(nm)
        call check(error, nm == "dup" .and. .not. ix%is_unique() .and. ix%kind() == PK_INT64, &
            "the second build's column, uniqueness and kind are what the object reports")
        if (allocated(error)) return
        call check(error, ix%count(1_int64) == 2_int64 .and. ix%nkeys() == 3_int64, "and its answers are the second's")
        if (allocated(error)) return
        call ix%find(2_int64, row)
        call check(error, row == 2_int64, "find answers over the second column")
    end subroutine test_rebuild_replaces

    !> A zero-row table indexes without complaint, and every query answers "absent".
    subroutine test_empty_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64), allocatable :: none(:), rows(:)
        integer(int64) :: got(2), row
        !
        allocate(none(0))
        call parquet_new_table(t)
        call t%add_column("k", none)
        call t%build_index("k", ix)
        call check(error, ix%is_current() .and. ix%nkeys() == 0_int64, "an empty table gives an empty, current index")
        if (allocated(error)) return
        call ix%find(1_int64, row)
        call ix%find_all(1_int64, rows)
        call ix%find_many([1_int64, 2_int64], got)
        call check(error, row == 0_int64 .and. size(rows) == 0 .and. all(got == 0_int64) .and. ix%count(1_int64) == 0_int64, &
            "every query on an empty index answers absent")
        if (allocated(error)) return
        call t%build_index("k", ix, unique=.false.)
        call ix%find_all(1_int64, rows)
        call check(error, size(rows) == 0 .and. ix%count(1_int64) == 0_int64, "an empty multimap index answers absent too")
    end subroutine test_empty_table

    !> `%build_index` on a file-backed table reads the key column if nothing has read it yet --
    !! the ordinary lazy first touch -- and leaves the table attached.
    subroutine test_lazy_column_is_read(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: file = "test_run/table_index_lazy.parquet"
        type(parquet_writer) :: w
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: row
        !
        call parquet_open_writer(w, file)
        call parquet_write_column(w, "id", [300_int32, 100_int32, 200_int32])
        call parquet_write_column(w, "v", [3.0_real64, 1.0_real64, 2.0_real64])
        call parquet_close_writer(w)
        call parquet_open_table(t, file)
        call check(error, t%residency("id") == RES_EMPTY, "the key column is not resident before the build")
        if (allocated(error)) return
        call t%build_index("id", ix)
        call check(error, t%residency("id") == RES_FULL, "build_index read the key column")
        if (allocated(error)) return
        call check(error, t%residency("v") == RES_EMPTY, "and read nothing else")
        if (allocated(error)) return
        call ix%find(200_int32, row)
        call check(error, row == 3_int64 .and. .not. t%is_detached(), "the index answers and the table is still attached")
    end subroutine test_lazy_column_is_read

    !> A query is a read: it bumps no generation and detaches nothing, so an index can be used
    !! any number of times without ever going stale on its own account.
    subroutine test_query_is_a_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: gen0, row, got(2)
        integer(int64), allocatable :: rows(:)
        !
        call build_groups(t)
        gen0 = t%generation()
        call t%build_index("g", ix, unique=.false.)
        call ix%find(7_int64, row)
        call ix%find_all(3_int64, rows)
        call ix%find_many([7_int64, 9_int64], got)
        call check(error, ix%count(9_int64) == 1_int64 .and. row == 1_int64 .and. size(rows) == 3, "the queries answered")
        if (allocated(error)) return
        call check(error, t%generation() == gen0, "neither the build nor any query bumped the generation")
        if (allocated(error)) return
        call check(error, .not. t%is_detached() .and. ix%is_current(), "nothing detached, and the index is current")
    end subroutine test_query_is_a_read

    !> Lookups are lock-free: every thread of a parallel region querying one index at once gets
    !! the serial answers. Without OpenMP the loop runs on one thread and the assertion still
    !! holds, so the test passes without proving anything -- which is acceptable here because the
    !! property is the engine's, pinned by parquet_index's own concurrent tests; this one only
    !! checks the WRAPPER adds no shared state to a query.
    subroutine test_lookups_in_parallel(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int64) :: keys(64), serial(64), threaded(64), counts(64), want_counts(64), four(4)
        integer :: i
        !
        call build_groups(t)
        call t%build_index("g", ix, unique=.false.)
        four = [3_int64, 7_int64, 9_int64, 11_int64]
        do i = 1, 64
            keys(i) = four(mod(i, 4) + 1)
            call ix%find(keys(i), serial(i))
            want_counts(i) = ix%count(keys(i))
        end do
        !$omp parallel do default(shared) schedule(static)
        do i = 1, 64
            call ix%find(keys(i), threaded(i))
            counts(i) = ix%count(keys(i))
        end do
        !$omp end parallel do
        call check(error, all(threaded == serial), "concurrent finds equal the serial ones")
        if (allocated(error)) return
        call check(error, all(counts == want_counts), "concurrent counts equal the serial ones")
        if (allocated(error)) return
        call check(error, count(want_counts == 3_int64) == 32 .and. count(want_counts == 0_int64) == 16, &
            "the probes cover a repeated key and an absent one (vacuity guard)")
    end subroutine test_lookups_in_parallel

    !> A string column, unique and not: `%find_all` equals `pf_match_all` over a mirror of the
    !! column (the sort engine's exact-bytes equality), `%find_many` equals `pf_match` from a
    !! trimmed character array and from a verbatim string column alike, a null row is never
    !! found, and a scalar key is taken as written.
    subroutine test_string_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, u
        type(parquet_table_index) :: ix
        type(parquet_string_column) :: mirror, probes
        character(len=3) :: s(6), pchr(5)
        integer(int64), allocatable :: rows(:), oracle(:), off(:), m(:)
        integer(int64) :: many(5), row
        integer(int32) :: many32(5)
        integer(int64) :: nf
        integer :: i
        !
        ! Row 6 is null; "a" repeats at rows 2 and 4, "bb" at 1 and 5; the first element is not
        ! the shortest, so a scan sized from it would be wrong here.
        s = [character(len=3) :: "bb", "a", "ccc", "a", "bb", "q"]
        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%set_null("s", 6_int64)
        call mirror%clear()
        do i = 1, 5
            call mirror%append_string(trim(s(i)))
        end do
        call mirror%append_null()
        pchr = [character(len=3) :: "a", "bb", "zz", "ccc", "q"]
        call probes%clear()
        do i = 1, 5
            call probes%append_string(trim(pchr(i)))
        end do
        !
        call t%build_index("s", ix, unique=.false.)
        call check(error, ix%kind() == PK_STRING .and. ix%nkeys() == 3_int64, &
            "a string multimap index over three distinct non-null keys")
        if (allocated(error)) return
        call pf_match_all(probes, mirror, off, m)
        do i = 1, 5
            call ix%find_all(trim(pchr(i)), rows)
            call check(error, size(rows, kind=int64) == off(i + 1) - off(i), &
                "find_all has pf_match_all's count at probe " // trim(pchr(i)))
            if (allocated(error)) return
            if (size(rows) > 0) then
                call check(error, all(rows == m(off(i):off(i + 1) - 1)), &
                    "find_all lists pf_match_all's rows at probe " // trim(pchr(i)))
                if (allocated(error)) return
            end if
        end do
        call check(error, off(6) - 1 == 5_int64, "the probes match five rows in all (vacuity guard)")
        if (allocated(error)) return
        call check(error, ix%count("q") == 0_int64, "the null row's former value is never found")
        if (allocated(error)) return
        call pf_match(probes, mirror, oracle)
        call ix%find_many(pchr, many, n_found=nf)
        call check(error, all(many == oracle) .and. nf == 3_int64, "find_many over a character array equals pf_match")
        if (allocated(error)) return
        call ix%find_many(probes, many32)
        call check(error, all(int(many32, int64) == oracle), "find_many over a string column equals pf_match")
        if (allocated(error)) return
        call ix%find("a", row)
        call check(error, row == 2_int64 .and. ix%count("a") == 2_int64 .and. ix%count("bb") == 2_int64, &
            "find is the first row, count the group size")
        if (allocated(error)) return
        call check(error, ix%count("a ") == 0_int64 .and. ix%count("A") == 0_int64, &
            "a scalar key is taken as written: 'a ' and 'A' are not 'a'")
        if (allocated(error)) return
        ! A unique index over a distinct column.
        call parquet_new_table(u)
        call u%add_column("name", [character(len=5) :: "x", "yy", "zzz"])
        call u%build_index("name", ix)
        call ix%find("yy", row)
        call check(error, ix%is_unique() .and. row == 2_int64 .and. ix%count("zzz") == 1_int64 .and. ix%nkeys() == 3_int64, &
            "a unique string index finds each key at its row")
        if (allocated(error)) return
        call ix%find_all("x", rows)
        call check(error, size(rows) == 1 .and. rows(1) == 1_int64, "find_all on a unique string index is one row")
    end subroutine test_string_column

    !> The int32-row half of `%find`, `%find_all` and `%find_many`, over int32 and real32 keys.
    !!
    !! Every query binding comes in two row widths, and the tests above take the int64 one almost
    !! everywhere. The int32 forms are not thin casts of it: each routes through `tix_narrow`,
    !! which is the guard that refuses a row number too large for int32, and each has its own
    !! absent-key arm that must hand back a ZERO-LENGTH array rather than a wrong one. Both
    !! index flavours are swept, because `%find_all`'s unique and multimap branches are separate
    !! code and the unique one's absent-key arm is reached by nothing else.
    subroutine test_query_every_int32_row_form(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        integer(int32) :: row32, many32(3)
        integer(int32), allocatable :: rows32(:)
        integer(int64) :: nf
        !
        ! Rows 1, 3 and 6 hold 10; rows 2 and 5 hold 20; row 4 holds 30. Interleaved, so a
        ! binding that assumed equal keys were adjacent would fail here.
        call parquet_new_table(t)
        call t%add_column("k", [10_int32, 20_int32, 10_int32, 30_int32, 20_int32, 10_int32])
        call t%add_column("r", [1.5_real32, 2.5_real32, 1.5_real32, 3.5_real32, 2.5_real32, 1.5_real32])

        ! ---- multimap ----
        call t%build_index("k", ix, unique=.false.)
        call ix%find(10_int32, row32)
        call check(error, row32 == 1_int32, "find(int32 key, int32 row) is the FIRST row of the group")
        if (allocated(error)) return
        call ix%find_all(10_int32, rows32)
        call check(error, size(rows32) == 3 .and. all(rows32 == [1_int32, 3_int32, 6_int32]), &
            "find_all(int32 key, int32 rows) lists every row of the group, ascending")
        if (allocated(error)) return
        call ix%find_all(99_int32, rows32)
        call check(error, allocated(rows32) .and. size(rows32) == 0, &
            "an absent key gives a zero-length array, not an unallocated one")
        if (allocated(error)) return
        call ix%find_many([10_int32, 99_int32, 30_int32], many32, n_found=nf)
        call check(error, all(many32 == [1_int32, 0_int32, 4_int32]) .and. nf == 2_int64, &
            "find_many(int32 keys, int32 rows) answers 0 for the absent key and counts the rest")
        if (allocated(error)) return

        call t%build_index("r", ix, unique=.false.)
        call ix%find(1.5_real32, row32)
        call check(error, row32 == 1_int32, "find(real32 key, int32 row)")
        if (allocated(error)) return
        call ix%find_all(2.5_real32, rows32)
        call check(error, all(rows32 == [2_int32, 5_int32]), "find_all(real32 key, int32 rows)")
        if (allocated(error)) return
        call ix%find_all(9.5_real32, rows32)
        call check(error, size(rows32) == 0, "an absent real32 key gives a zero-length array")
        if (allocated(error)) return
        call ix%find_many([1.5_real32, 9.5_real32, 3.5_real32], many32)
        call check(error, all(many32 == [1_int32, 0_int32, 4_int32]), &
            "find_many(real32 keys, int32 rows)")
        if (allocated(error)) return

        ! ---- unique, which is the other branch of find_all entirely ----
        call parquet_new_table(t)
        call t%add_column("k", [10_int32, 20_int32, 30_int32])
        call t%add_column("r", [1.5_real32, 2.5_real32, 3.5_real32])
        call t%build_index("k", ix)
        call ix%find_all(20_int32, rows32)
        call check(error, size(rows32) == 1 .and. rows32(1) == 2_int32, &
            "find_all on a UNIQUE int32 index is the one row")
        if (allocated(error)) return
        call ix%find_all(99_int32, rows32)
        call check(error, size(rows32) == 0, "and a zero-length array when the key is absent")
        if (allocated(error)) return
        call t%build_index("r", ix)
        call ix%find_all(2.5_real32, rows32)
        call check(error, size(rows32) == 1 .and. rows32(1) == 2_int32, &
            "find_all on a UNIQUE real32 index is the one row")
        if (allocated(error)) return
        call ix%find_all(9.5_real32, rows32)
        call check(error, size(rows32) == 0, "and a zero-length array when that key is absent")
    end subroutine test_query_every_int32_row_form

    !> `%find_all` over each temporal key type, in BOTH row widths.
    !!
    !! A timestamp key is a PAIR -- seconds and nanoseconds -- so it reaches a different engine
    !! helper (`tix_all_pair_*`) from the single-component date and time keys, and each of those
    !! helpers has its own int32 and int64 form. Six cells, none of them reachable from the
    !! scalar `%find` the temporal test above uses.
    !!
    !! Each column repeats one value so the multimap branch returns more than one row; a
    !! find_all that answered only the first would be indistinguishable from `%find` otherwise.
    subroutine test_find_all_every_temporal_key(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_date) :: d(4), dq
        type(parquet_time) :: tm(4), tq
        type(parquet_timestamp) :: ts(4), tsq
        integer(int32), allocatable :: r32(:)
        integer(int64), allocatable :: r64(:)
        !
        ! Rows 1 and 3 share a value in each column; rows 2 and 4 are distinct.
        call d(1)%set(2024, 3, 1)
        call d(2)%set(1999, 12, 31)
        call d(3)%set(2024, 3, 1)
        call d(4)%set(2000, 1, 1)
        call tm(1)%set(12, 0, 0)
        call tm(2)%set(1, 2, 3)
        call tm(3)%set(12, 0, 0)
        call tm(4)%set(23, 59, 59)
        call ts(1)%set(2024, 3, 1, 12, 30, 0)
        call ts(2)%set(1999, 12, 31, 1, 2, 3)
        call ts(3)%set(2024, 3, 1, 12, 30, 0)
        call ts(4)%set(2000, 1, 1, 0, 0, 0)
        call parquet_new_table(t)
        call t%add_column("d", d)
        call t%add_column("tm", tm)
        call t%add_column("ts", ts)

        call dq%set(2024, 3, 1)
        call tq%set(12, 0, 0)
        call tsq%set(2024, 3, 1, 12, 30, 0)

        call t%build_index("d", ix, unique=.false.)
        call ix%find_all(dq, r32)
        call check(error, all(r32 == [1_int32, 3_int32]), "find_all(date, int32 rows)")
        if (allocated(error)) return
        call ix%find_all(dq, r64)
        call check(error, all(r64 == [1_int64, 3_int64]), "find_all(date, int64 rows)")
        if (allocated(error)) return

        call t%build_index("tm", ix, unique=.false.)
        call ix%find_all(tq, r32)
        call check(error, all(r32 == [1_int32, 3_int32]), "find_all(time, int32 rows)")
        if (allocated(error)) return
        call ix%find_all(tq, r64)
        call check(error, all(r64 == [1_int64, 3_int64]), "find_all(time, int64 rows)")
        if (allocated(error)) return

        call t%build_index("ts", ix, unique=.false.)
        call ix%find_all(tsq, r32)
        call check(error, all(r32 == [1_int32, 3_int32]), "find_all(timestamp, int32 rows)")
        if (allocated(error)) return
        call ix%find_all(tsq, r64)
        call check(error, all(r64 == [1_int64, 3_int64]), "find_all(timestamp, int64 rows)")
        if (allocated(error)) return

        ! The unique branch of the PAIR helper, including its absent-key arm -- a different
        ! path from the multimap `get_all` the six cells above take.
        call parquet_new_table(t)
        call t%add_column("ts", ts(1:2))
        call t%build_index("ts", ix)
        call ix%find_all(tsq, r64)
        call check(error, size(r64) == 1 .and. r64(1) == 1_int64, &
            "find_all(timestamp) on a unique index is the one row")
        if (allocated(error)) return
        call ix%find_all(tsq, r32)
        call check(error, size(r32) == 1 .and. r32(1) == 1_int32, "and in int32 rows too")
        if (allocated(error)) return
        call tsq%set(1900, 1, 1, 0, 0, 0)
        call ix%find_all(tsq, r64)
        call check(error, size(r64) == 0, "an absent timestamp gives a zero-length array")
        if (allocated(error)) return
        call ix%find_all(tsq, r32)
        call check(error, size(r32) == 0, "and the same in int32 rows")
    end subroutine test_find_all_every_temporal_key

    !> The remaining `%find_many` and `%find_all` string and real64 cells.
    !!
    !! `test_string_column` above drives the character-array form into int64 answers and the
    !! `parquet_string_column` form into int32 ones. The OPPOSITE pairing of each is what is
    !! swept here, together with `%find_all` over a string key in int32 rows and `%find_many`
    !! over real64 keys in int64 rows -- the cells a caller picks freely and nothing else calls.
    subroutine test_query_remaining_key_forms(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_string_column) :: probes
        character(len=3) :: keys(3)
        integer(int32) :: m32(3)
        integer(int64) :: m64(3), nf
        integer(int32), allocatable :: r32(:)
        integer :: i
        !
        ! "bb" at rows 1 and 4, "a" at rows 2 and 5, "ccc" at row 3 alone.
        call parquet_new_table(t)
        call t%add_column("s", [character(len=3) :: "bb", "a", "ccc", "bb", "a"])
        call t%add_column("x", [1.5_real64, 2.5_real64, 3.5_real64, 1.5_real64, 2.5_real64])
        keys = [character(len=3) :: "a", "zz", "ccc"]
        call probes%clear()
        do i = 1, 3
            call probes%append_string(trim(keys(i)))
        end do

        call t%build_index("s", ix, unique=.false.)
        call ix%find_many(keys, m32, n_found=nf)
        call check(error, all(m32 == [2_int32, 0_int32, 3_int32]) .and. nf == 2_int64, &
            "find_many(character keys, int32 rows): first row per key, 0 for the absent one")
        if (allocated(error)) return
        call ix%find_many(probes, m64, n_found=nf)
        call check(error, all(m64 == [2_int64, 0_int64, 3_int64]) .and. nf == 2_int64, &
            "find_many(parquet_string_column keys, int64 rows) agrees with the character form")
        if (allocated(error)) return
        call ix%find_all("bb", r32)
        call check(error, all(r32 == [1_int32, 4_int32]), &
            "find_all(string key, int32 rows) on a multimap")
        if (allocated(error)) return

        call t%build_index("x", ix, unique=.false.)
        call ix%find_many([1.5_real64, 9.5_real64, 3.5_real64], m64, n_found=nf)
        call check(error, all(m64 == [1_int64, 0_int64, 3_int64]) .and. nf == 2_int64, &
            "find_many(real64 keys, int64 rows)")
        if (allocated(error)) return

        ! The unique branch of the string find_all, both widths, present and absent.
        call parquet_new_table(t)
        call t%add_column("s", [character(len=3) :: "x", "yy", "zzz"])
        call t%build_index("s", ix)
        call ix%find_all("yy", r32)
        call check(error, size(r32) == 1 .and. r32(1) == 2_int32, &
            "find_all(string key, int32 rows) on a UNIQUE index is the one row")
        if (allocated(error)) return
        call ix%find_all("nope", r32)
        call check(error, size(r32) == 0, "and a zero-length array when the key is absent")
        if (allocated(error)) return
        block
            integer(int64), allocatable :: r64(:)
            call ix%find_all("nope", r64)
            call check(error, allocated(r64) .and. size(r64) == 0, &
                "the int64 form's absent-key arm allocates a zero-length array too")
        end block
    end subroutine test_query_remaining_key_forms

    !> A NULL temporal key finds nothing, in every temporal type and both row widths.
    !!
    !! Null is not a value: every temporal `%find_all` specific answers a zero-length array for a
    !! null key WITHOUT consulting the engine at all, because a null element's raw storage is a
    !! real key -- 1970-01-01 for a date, midnight for a time -- and asking the engine would hand
    !! back the rows holding that instant. `test_temporal_columns` pins that rule for `%find` and
    !! `%count`; `%find_all` has its own copy of the guard in each of its six specifics.
    !!
    !! The fixture therefore CONTAINS the epoch row that a leaked null would find, which is what
    !! makes the assertion discriminating: without it, a specific that dropped the guard would
    !! answer zero rows anyway and pass.
    subroutine test_find_all_null_temporal_key(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_date) :: d(3), dq
        type(parquet_time) :: tm(3), tq
        type(parquet_timestamp) :: ts(3), tsq
        integer(int32), allocatable :: r32(:)
        integer(int64), allocatable :: r64(:)
        !
        ! Row 1 of each column is the epoch instant -- the value a null element's raw storage
        ! carries -- so a guard that was dropped would return row 1 rather than nothing.
        call d(1)%set_raw(0_int32)
        call d(2)%set(2024, 3, 1)
        call d(3)%set(1999, 12, 31)
        call tm(1)%set(0, 0, 0)
        call tm(2)%set(12, 0, 0)
        call tm(3)%set(23, 59, 59)
        call ts(1)%set(1970, 1, 1, 0, 0, 0)
        call ts(2)%set(2024, 3, 1, 12, 30, 0)
        call ts(3)%set(1999, 12, 31, 1, 2, 3)
        call parquet_new_table(t)
        call t%add_column("d", d)
        call t%add_column("tm", tm)
        call t%add_column("ts", ts)

        call dq%set_null()
        call tq%set_null()
        call tsq%set_null()

        call t%build_index("d", ix, unique=.false.)
        ! The positive control first: the epoch row IS findable by its own genuine value, so the
        ! zero-length answers below are about the null and not about an empty index.
        call d(1)%set_raw(0_int32)
        call ix%find_all(d(1), r64)
        call check(error, size(r64) == 1 .and. r64(1) == 1_int64, &
            "control: the epoch date row is found by its own value")
        if (allocated(error)) return
        call ix%find_all(dq, r32)
        call check(error, allocated(r32) .and. size(r32) == 0, &
            "a null date key finds nothing, in int32 rows")
        if (allocated(error)) return
        call ix%find_all(dq, r64)
        call check(error, allocated(r64) .and. size(r64) == 0, &
            "and nothing in int64 rows -- not the 1970-01-01 row")
        if (allocated(error)) return

        call t%build_index("tm", ix, unique=.false.)
        call ix%find_all(tq, r32)
        call check(error, size(r32) == 0, "a null time key finds nothing, in int32 rows")
        if (allocated(error)) return
        call ix%find_all(tq, r64)
        call check(error, size(r64) == 0, "and nothing in int64 rows -- not the midnight row")
        if (allocated(error)) return

        call t%build_index("ts", ix, unique=.false.)
        call ix%find_all(tsq, r32)
        call check(error, size(r32) == 0, "a null timestamp key finds nothing, in int32 rows")
        if (allocated(error)) return
        call ix%find_all(tsq, r64)
        call check(error, size(r64) == 0, "and nothing in int64 rows -- not the epoch row")
    end subroutine test_find_all_null_temporal_key

    !> `%find_many` on a UNIQUE index, in the three key shapes whose unique arm nothing else runs.
    !!
    !! Each bulk specific branches on `%is_unique()`: a unique index goes through the map's
    !! `get_many` and counts the non-zero answers itself, while a multimap goes through
    !! `get_first_many`, which counts them for it. The two are different engines with different
    !! `n_found` bookkeeping, and the tests above drive the MULTIMAP arm for every shape -- so the
    !! unique arm of the timestamp-pair, character-array and string-column forms rests on nothing.
    !!
    !! `n_found` is asserted beside the rows in each, because that is the half a unique arm
    !! computes rather than receives: a count taken over the wrong comparison would leave the row
    !! answers right and the count wrong.
    subroutine test_find_many_unique_index_key_shapes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_table_index) :: ix
        type(parquet_string_column) :: probes
        type(parquet_timestamp) :: ts(3), tq(3)
        character(len=3) :: keys(3)
        integer(int32) :: m32(3)
        integer(int64) :: m64(3), nf
        integer :: i
        !
        call ts(1)%set(2024, 3, 1, 12, 30, 0)
        call ts(2)%set(1999, 12, 31, 1, 2, 3)
        call ts(3)%set(2000, 1, 1, 0, 0, 0)
        call parquet_new_table(t)
        call t%add_column("ts", ts)
        call t%build_index("ts", ix)
        call check(error, ix%is_unique(), "the timestamp index must be the unique flavour")
        if (allocated(error)) return
        tq(1) = ts(3)
        call tq(2)%set(1900, 1, 1, 0, 0, 0)   ! absent
        tq(3) = ts(1)
        call ix%find_many(tq, m32, n_found=nf)
        call check(error, all(m32 == [3_int32, 0_int32, 1_int32]) .and. nf == 2_int64, &
            "timestamp keys into int32 rows on a unique index, with the absent key counted out")
        if (allocated(error)) return

        keys = [character(len=3) :: "yy", "no", "x"]
        call parquet_new_table(t)
        call t%add_column("s", [character(len=3) :: "x", "yy", "zzz"])
        call t%build_index("s", ix)
        call check(error, ix%is_unique(), "the string index must be the unique flavour")
        if (allocated(error)) return
        call ix%find_many(keys, m32, n_found=nf)
        call check(error, all(m32 == [2_int32, 0_int32, 1_int32]) .and. nf == 2_int64, &
            "character keys into int32 rows on a unique index")
        if (allocated(error)) return
        call probes%clear()
        do i = 1, 3
            call probes%append_string(trim(keys(i)))
        end do
        call ix%find_many(probes, m64, n_found=nf)
        call check(error, all(m64 == [2_int64, 0_int64, 1_int64]) .and. nf == 2_int64, &
            "a parquet_string_column of the same keys into int64 rows must agree exactly")
    end subroutine test_find_many_unique_index_key_shapes
end module test_table_index
