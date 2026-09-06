!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the table's row-set verbs: `%explode`, `%duplicated`/`%drop_duplicates` and
!! `%sort_by_values`/`%argsort_by_values`.
!!
!! **Every one of these verbs has a silent failure mode, and the tests are shaped around them
!! rather than around the happy path.** An explode that repeats the wrong row still returns a
!! table of the right size; a `%drop_duplicates` that keeps the wrong row of a group still
!! returns one row per group; a `%sort_by_values` that mis-associates the values with the rows
!! still returns an ordered table. So none of these is asserted by counting rows:
!!
!! * `%explode` is asserted on the CONTENT of every output row and on `origin` beside it, over a
!!   table whose columns hold row-distinct values -- a fixture of constants would pass against a
!!   verb that repeated an arbitrary row.
!! * `%drop_duplicates` is asserted with a PAYLOAD column that differs across each group, which
!!   is the only thing that can tell `keep="first"` from `keep="last"`; a fixture whose other
!!   columns are equal within a group cannot distinguish them at all.
!! * `%sort_by_values` is asserted against the composition it replaces -- `%add_column`,
!!   `%sort_by`, `%drop_column` -- rather than against a hand-written expected order, so the
!!   oracle is the library's own established sort rather than a second copy of it.
!!
!! **The no-op cases are negative controls, not conveniences.** `parquet_tables_rowmutate.f90`'s
!! rule is that a call which changes nothing must not touch a column, must not invalidate a
!! `%col` pointer, must not advance `%generation()` and must not detach -- and a verb that
!! detaches unconditionally passes every other test in this file. So each of the three mutators
!! has a paired test: one that must detach and bump the counter, one that must do neither.
!!
!! **Equality here is the sort comparator's**, so a null group and a NaN group each collapse to
!! one row. That is asserted directly (`test_dedup_null_and_nan_group`) because it is the rule a
!! future change is most likely to "fix" into something else.
!!
!! Abort paths live in test/error_scenarios.f90 as `explode_*`, `drop_duplicates_*` and
!! `sort_by_values_*` scenarios. Every test that writes a file uses its own path -- the suite
!! runs its tests concurrently, so a shared one would be truncated out from under its neighbour.
module test_table_rowverbs
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_rowverbs

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_rowverbs(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("explode repeats each row its count times, in order", test_explode_repeats), &
            new_unittest("explode origin names each output row's source row", test_explode_origin), &
            new_unittest("explode keeps a zero-count row once by default", test_explode_keep_empty), &
            new_unittest("explode keep_empty=.false. drops a zero-count row", test_explode_drop_empty), &
            new_unittest("explode with every count 1 changes nothing", test_explode_identity), &
            new_unittest("explode detaches once it changes a row", test_explode_detaches), &
            new_unittest("explode takes an int32 count list too", test_explode_int32), &
            new_unittest("explode carries a string column along", test_explode_string_column), &
            new_unittest("duplicated marks every row but the first of a group", test_duplicated_first), &
            new_unittest("duplicated keep=last spares the highest row index", test_duplicated_last), &
            new_unittest("duplicated keep=none spares no repeated row", test_duplicated_none), &
            new_unittest("duplicated neither mutates nor detaches", test_duplicated_is_a_read), &
            new_unittest("duplicated over a separated key string agrees", test_duplicated_string_keys), &
            new_unittest("drop_duplicates keeps the original row order", test_dedup_keeps_order), &
            new_unittest("drop_duplicates then duplicated is all .false.", test_dedup_is_idempotent), &
            new_unittest("drop_duplicates over every resident column", test_dedup_all_columns), &
            new_unittest("a null group and a NaN group each collapse to one row", test_dedup_null_and_nan_group), &
            new_unittest("drop_duplicates with nothing to drop changes nothing", test_dedup_no_duplicates), &
            new_unittest("drop_duplicates detaches once it drops a row", test_dedup_detaches), &
            new_unittest("sort_by_values equals add_column + sort_by + drop_column", test_sort_by_values_matches), &
            new_unittest("sort_by_values descending reverses the order", test_sort_by_values_descending), &
            new_unittest("sort_by_values places nulls by nulls_first", test_sort_by_values_nulls), &
            new_unittest("sort_by_values over character values", test_sort_by_values_character), &
            new_unittest("sort_by_values over already-ordered values changes nothing", &
                test_sort_by_values_noop), &
            new_unittest("argsort_by_values hands back the order without applying it", &
                test_argsort_by_values), &
            new_unittest("argsort_by_values fills an int32 permutation too", test_argsort_by_values_i32), &
            new_unittest("value_counts orders by count descending, then by value ascending", &
                test_value_counts_order), &
            new_unittest("value_counts descending=.false. puts the rarest first", &
                test_value_counts_ascending), &
            new_unittest("value_counts drops the null group, and dropna=.false. keeps it last", &
                test_value_counts_dropna), &
            new_unittest("value_counts agrees with pf_value_counts over the same values", &
                test_value_counts_matches_array_form), &
            new_unittest("value_counts is a read: nothing mutates, nothing detaches", &
                test_value_counts_is_a_read), &
            new_unittest("value_counts keeps the counted column's name, kind and unit", &
                test_value_counts_keeps_name_kind_unit), &
            new_unittest("value_counts counts a string column, and count_name renames the count", &
                test_value_counts_strings_and_count_name) &
            ]
    end subroutine collect_tests_table_rowverbs

    ! ---- fixtures -------------------------------------------------------------------------

    !> A three-row table whose every column is row-distinct, so a verb that moved or repeated the
    !! wrong row cannot produce this table's contents by accident.
    subroutine build_distinct(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: id(3)
        real(real64) :: x(3)
        !
        id = [10, 20, 30]
        x = [1.5_real64, 2.5_real64, 3.5_real64]
        call parquet_new_table(t)
        call t%add_column("id", id)
        call t%add_column("x", x)
    end subroutine build_distinct

    !> Six rows in three groups by `key`, each group carrying a `payload` that differs across its
    !! rows -- which is the only thing that can tell `keep="first"` from `keep="last"`.
    !!
    !! Rows (1-based): key = [7, 3, 7, 3, 9, 7]. Group 7 is rows 1, 3, 6; group 3 is rows 2, 4;
    !! group 9 is row 5 alone. The groups are deliberately INTERLEAVED, so a verb that assumed
    !! equal rows are adjacent would fail here and pass on a sorted fixture.
    subroutine build_groups(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: key(6), payload(6)
        !
        key = [7, 3, 7, 3, 9, 7]
        payload = [101, 102, 103, 104, 105, 106]
        call parquet_new_table(t)
        call t%add_column("key", key)
        call t%add_column("payload", payload)
    end subroutine build_groups

    !> Six rows over four distinct values, with a deliberate COUNT TIE: `tag` is
    !! `[5, 1, 5, 1, 9, 2]`, so 1 and 5 occur twice each and 2 and 9 once each.
    !!
    !! The tie is the point. Without one, "count descending" alone determines the whole order and
    !! a tie-break that had been dropped, reversed, or left to whatever the sort happened to do
    !! would pass every assertion. With it, the two pairs pin the documented "then by value
    !! ascending" in both directions of `descending`.
    subroutine build_tied(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: tag(6)
        !
        tag = [5, 1, 5, 1, 9, 2]
        call parquet_new_table(t)
        call t%add_column("tag", tag)
    end subroutine build_tied

    ! ---- %explode --------------------------------------------------------------------------

    !> Row `i` becomes `counts(i)` consecutive rows of every column.
    subroutine test_explode_repeats(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: counts(3)
        integer(int32), allocatable :: id(:)
        real(real64), allocatable :: x(:)
        !
        call build_distinct(t)
        counts = [2_int64, 1_int64, 3_int64]
        call t%explode(counts)
        call check(error, t%nrows() == 6_int64, "sum(counts) rows afterwards")
        if (allocated(error)) return
        call t%get("id", id)
        call t%get("x", x)
        call check(error, all(id == [10, 10, 20, 30, 30, 30]), &
            "each row repeated its count times, consecutively, in the original order")
        if (allocated(error)) return
        call check(error, all(abs(x - [1.5_real64, 1.5_real64, 2.5_real64, 3.5_real64, &
            3.5_real64, 3.5_real64]) < 1.0e-12_real64), "a second column followed the same rows")
    end subroutine test_explode_repeats

    !> `origin` names the source row of every output row, and is non-decreasing.
    subroutine test_explode_origin(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: counts(3)
        integer(int64), allocatable :: origin(:)
        integer :: k
        logical :: rising
        !
        call build_distinct(t)
        counts = [2_int64, 1_int64, 3_int64]
        call t%explode(counts, origin=origin)
        call check(error, size(origin) == 6, "one origin entry per OUTPUT row")
        if (allocated(error)) return
        call check(error, all(origin == [1_int64, 1_int64, 2_int64, 3_int64, 3_int64, 3_int64]), &
            "origin names the source row of each output row")
        if (allocated(error)) return
        rising = .true.
        do k = 2, size(origin)
            if (origin(k) < origin(k - 1)) rising = .false.
        end do
        call check(error, rising, "origin is non-decreasing")
    end subroutine test_explode_origin

    !> A zero count keeps its row once by default -- pandas' `explode`.
    subroutine test_explode_keep_empty(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: counts(3)
        integer(int32), allocatable :: id(:)
        !
        call build_distinct(t)
        counts = [0_int64, 2_int64, 0_int64]
        call t%explode(counts)
        call t%get("id", id)
        call check(error, all(id == [10, 20, 20, 30]), &
            "a zero-count row survives once, so sum(max(1, counts)) rows come back")
    end subroutine test_explode_keep_empty

    !> `keep_empty=.false.` drops a zero-count row -- polars' and SQL's `UNNEST`.
    subroutine test_explode_drop_empty(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: counts(3)
        integer(int32), allocatable :: id(:)
        integer(int64), allocatable :: origin(:)
        !
        call build_distinct(t)
        counts = [0_int64, 2_int64, 0_int64]
        call t%explode(counts, keep_empty=.false., origin=origin)
        call check(error, t%nrows() == 2_int64, "sum(counts) rows, the zero-count rows gone")
        if (allocated(error)) return
        call t%get("id", id)
        call check(error, all(id == [20, 20]), "only the row with a positive count survives")
        if (allocated(error)) return
        call check(error, all(origin == [2_int64, 2_int64]), "origin follows the surviving rows")
    end subroutine test_explode_drop_empty

    !> The negative control: every effective count 1 must touch nothing at all.
    subroutine test_explode_identity(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: counts(3), gen
        integer(int64), allocatable :: origin(:)
        integer(int32), allocatable :: id(:)
        !
        call build_distinct(t)
        gen = t%generation()
        counts = [1_int64, 1_int64, 1_int64]
        call t%explode(counts, origin=origin)
        call check(error, t%generation() == gen, &
            "a call that changes no row does not advance the generation counter")
        if (allocated(error)) return
        call check(error, t%nrows() == 3_int64, "the row count is unchanged")
        if (allocated(error)) return
        call t%get("id", id)
        call check(error, all(id == [10, 20, 30]), "the values are unchanged")
        if (allocated(error)) return
        call check(error, all(origin == [1_int64, 2_int64, 3_int64]), &
            "origin is still answered, as the identity, even though nothing was done")
    end subroutine test_explode_identity

    !> The positive control beside it: a real explode of a FILE-backed table detaches.
    subroutine test_explode_detaches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=*), parameter :: fname = "test_run/rowverbs_explode_detach.parquet"
        integer(int64) :: counts(4), gen
        !
        call write_small_file(fname)
        call parquet_open_table(t, fname)
        call t%materialize_all()
        gen = t%generation()
        counts = [1_int64, 1_int64, 1_int64, 1_int64]
        call t%explode(counts)
        call check(error, .not. t%is_detached(), &
            "the negative control first: an identity explode keeps the file")
        if (allocated(error)) return
        counts = [2_int64, 1_int64, 1_int64, 1_int64]
        call t%explode(counts)
        call check(error, t%is_detached(), "an explode that repeats a row detaches")
        if (allocated(error)) return
        call check(error, t%generation() > gen, "and advances the generation counter")
    end subroutine test_explode_detaches

    !> The int32 count list reaches the same place.
    subroutine test_explode_int32(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: counts(3)
        integer(int32), allocatable :: id(:)
        integer(int64), allocatable :: origin(:)
        !
        call build_distinct(t)
        counts = [1, 3, 1]
        call t%explode(counts, origin=origin)
        call t%get("id", id)
        call check(error, all(id == [10, 20, 20, 20, 30]), "an int32 count list explodes the same")
        if (allocated(error)) return
        call check(error, all(origin == [1_int64, 2_int64, 2_int64, 2_int64, 3_int64]), &
            "origin is int64 whichever kind the counts were")
    end subroutine test_explode_int32

    !> A string column is rebuilt by its own packed-store gather, so it needs its own case.
    subroutine test_explode_string_column(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: counts(3)
        character(len=:), allocatable :: s(:)
        !
        call parquet_new_table(t)
        call t%add_column("name", ["alpha  ", "beta   ", "gamma  "])
        counts = [1_int64, 2_int64, 1_int64]
        call t%explode(counts)
        call t%get("name", s)
        call check(error, t%nrows() == 4_int64, "four rows afterwards")
        if (allocated(error)) return
        call check(error, trim(s(1)) == "alpha" .and. trim(s(2)) == "beta" .and. &
            trim(s(3)) == "beta" .and. trim(s(4)) == "gamma", &
            "the packed string store followed the same row list")
    end subroutine test_explode_string_column

    ! ---- %duplicated / %drop_duplicates ------------------------------------------------------

    !> Default `keep="first"`: every row of a group but its LOWEST index is marked.
    subroutine test_duplicated_first(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        logical, allocatable :: mask(:)
        !
        call build_groups(t)
        call t%duplicated(["key"], mask)
        ! key = [7, 3, 7, 3, 9, 7]: rows 1, 2 and 5 are each their group's lowest index.
        call check(error, all(mask .eqv. [.false., .false., .true., .true., .false., .true.]), &
            "exactly the rows a keep=first drop would remove are marked")
    end subroutine test_duplicated_first

    !> `keep="last"`: every row of a group but its HIGHEST index is marked.
    subroutine test_duplicated_last(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        logical, allocatable :: mask(:)
        !
        call build_groups(t)
        call t%duplicated(["key"], mask, keep="last")
        ! group 7's highest index is row 6, group 3's is row 4, group 9 is row 5 alone.
        call check(error, all(mask .eqv. [.true., .true., .true., .false., .false., .false.]), &
            "exactly the rows a keep=last drop would remove are marked")
    end subroutine test_duplicated_last

    !> `keep="none"`: every row of every group of more than one is marked.
    subroutine test_duplicated_none(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        logical, allocatable :: mask(:)
        !
        call build_groups(t)
        call t%duplicated(["key"], mask, keep="none")
        call check(error, all(mask .eqv. [.true., .true., .true., .true., .false., .true.]), &
            "only the row that is alone in its group is spared")
    end subroutine test_duplicated_none

    !> The read must leave the table exactly as it found it -- the negative control for the
    !! detach rule, which a `%duplicated` that quietly ran the drop would fail.
    subroutine test_duplicated_is_a_read(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=*), parameter :: fname = "test_run/rowverbs_dup_read.parquet"
        logical, allocatable :: mask(:)
        integer(int64) :: gen
        !
        call write_small_file(fname)
        call parquet_open_table(t, fname)
        call t%materialize_all()
        gen = t%generation()
        call t%duplicated(["id"], mask)
        call check(error, .not. t%is_detached(), "%duplicated never detaches")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "and never advances the generation counter")
        if (allocated(error)) return
        call check(error, t%nrows() == 4_int64, "and drops no row")
    end subroutine test_duplicated_is_a_read

    !> The separated-string key form answers the same as the array form.
    subroutine test_duplicated_string_keys(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        logical, allocatable :: m1(:), m2(:)
        !
        call build_groups(t)
        call t%duplicated(["key    ", "payload"], m1)
        call t%duplicated("key, payload", m2)
        call check(error, all(m1 .eqv. m2), "the two key spellings answer the same mask")
        if (allocated(error)) return
        call check(error, .not. any(m1), "no two rows are equal on (key, payload)")
    end subroutine test_duplicated_string_keys

    !> A mask never reorders, so the survivors come back in their original order.
    subroutine test_dedup_keeps_order(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32), allocatable :: key(:), payload(:)
        !
        call build_groups(t)
        call t%drop_duplicates(["key"])
        call t%get("key", key)
        call t%get("payload", payload)
        call check(error, all(key == [7, 3, 9]), &
            "the survivors keep the table's own order, not the sort's")
        if (allocated(error)) return
        call check(error, all(payload == [101, 102, 105]), &
            "and each survivor is its group's LOWEST row, which only the payload can show")
    end subroutine test_dedup_keeps_order

    !> Dropping and then asking again must find nothing left to drop.
    subroutine test_dedup_is_idempotent(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        logical, allocatable :: mask(:)
        !
        call build_groups(t)
        call t%drop_duplicates("key")
        call t%duplicated(["key"], mask)
        call check(error, size(mask) == 3, "one row per group survives")
        if (allocated(error)) return
        call check(error, .not. any(mask), "and none of them is a duplicate of another")
    end subroutine test_dedup_is_idempotent

    !> The form that names no column uses every resident one.
    subroutine test_dedup_all_columns(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: a(4), b(4)
        integer(int32), allocatable :: ga(:)
        !
        ! Rows 1 and 3 are equal in BOTH columns; rows 2 and 4 share `a` but differ in `b`, so a
        ! verb that looked only at the first column would drop one of them too.
        a = [1, 2, 1, 2]
        b = [5, 6, 5, 7]
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%drop_duplicates(keep="first")
        call check(error, t%nrows() == 3_int64, "only the row equal on EVERY column is dropped")
        if (allocated(error)) return
        call t%get("a", ga)
        call check(error, all(ga == [1, 2, 2]), "and the survivors keep their order")
    end subroutine test_dedup_all_columns

    !> Equality is the sort comparator's: all nulls are one value, all NaNs are one value.
    subroutine test_dedup_null_and_nan_group(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        real(real64) :: v(6), nan
        logical :: ok(6)
        !
        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        ! Two nulls, two NaNs and two ordinary equal values, so all three collapse to one row
        ! each and the answer is unambiguous at 3.
        v = [1.0_real64, 1.0_real64, nan, nan, 0.0_real64, 0.0_real64]
        ok = [.true., .true., .true., .true., .false., .false.]
        call parquet_new_table(t)
        call t%add_column("v", v)
        call t%set_null("v", ok)
        call t%drop_duplicates(["v"])
        call check(error, t%nrows() == 3_int64, &
            "the null pair, the NaN pair and the equal pair each collapse to one row")
    end subroutine test_dedup_null_and_nan_group

    !> The negative control: with nothing to drop, nothing at all happens.
    subroutine test_dedup_no_duplicates(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: gen
        !
        call build_distinct(t)
        gen = t%generation()
        call t%drop_duplicates(["id"])
        call check(error, t%nrows() == 3_int64, "every row survives")
        if (allocated(error)) return
        call check(error, t%generation() == gen, &
            "and the generation counter does not move, so no column was rewritten")
    end subroutine test_dedup_no_duplicates

    !> The positive control beside it: a drop that removes a row detaches.
    subroutine test_dedup_detaches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=*), parameter :: fname = "test_run/rowverbs_dedup_detach.parquet"
        !
        call write_dup_file(fname)
        call parquet_open_table(t, fname)
        call t%materialize_all()
        call t%drop_duplicates(["id"])
        call check(error, t%is_detached(), "dropping a row detaches the table from its file")
        if (allocated(error)) return
        call check(error, t%nrows() == 3_int64, "and the duplicates really went")
    end subroutine test_dedup_detaches

    ! ---- %sort_by_values / %argsort_by_values ------------------------------------------------

    !> The composition this verb replaces is the oracle: add the values as a column, sort by it,
    !! drop it again. Anything else would be a second implementation of the same order.
    subroutine test_sort_by_values_matches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: a, b
        real(real64) :: v(6)
        integer(int32), allocatable :: ka(:), kb(:), pa(:), pb(:)
        !
        v = [3.0_real64, -1.0_real64, 2.5_real64, 0.0_real64, 9.0_real64, -4.0_real64]
        call build_groups(a)
        call build_groups(b)
        call a%sort_by_values(v)
        call b%add_column("k", v)
        call b%sort_by("k")
        call b%drop_column("k")
        call a%get("key", ka)
        call b%get("key", kb)
        call a%get("payload", pa)
        call b%get("payload", pb)
        call check(error, all(ka == kb), "the key column is ordered exactly as the composition")
        if (allocated(error)) return
        call check(error, all(pa == pb), "and so is every other column")
        if (allocated(error)) return
        call check(error, all(pa == [106, 102, 104, 103, 101, 105]), &
            "which is the order the values themselves imply")
    end subroutine test_sort_by_values_matches

    !> `descending` reverses it.
    subroutine test_sort_by_values_descending(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: v(3)
        integer(int32), allocatable :: id(:)
        !
        call build_distinct(t)
        v = [2, 3, 1]
        call t%sort_by_values(v, descending=.true.)
        call t%get("id", id)
        call check(error, all(id == [20, 10, 30]), "high to low")
    end subroutine test_sort_by_values_descending

    !> `is_valid` marks a value null, and `nulls_first` decides where those rows land.
    subroutine test_sort_by_values_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: v(3)
        logical :: ok(3)
        integer(int32), allocatable :: id(:)
        !
        v = [2, 0, 1]
        ok = [.true., .false., .true.]
        call build_distinct(t)
        call t%sort_by_values(v, is_valid=ok)
        call t%get("id", id)
        call check(error, all(id == [30, 10, 20]), "nulls last by default")
        if (allocated(error)) return
        call build_distinct(t)
        call t%sort_by_values(v, is_valid=ok, nulls_first=.true.)
        call t%get("id", id)
        call check(error, all(id == [20, 30, 10]), "nulls first when asked for")
    end subroutine test_sort_by_values_nulls

    !> A character value list reaches `pf_argsort`'s own string comparator.
    subroutine test_sort_by_values_character(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32), allocatable :: id(:)
        !
        call build_distinct(t)
        call t%sort_by_values(["pear ", "apple", "mango"])
        call t%get("id", id)
        call check(error, all(id == [20, 30, 10]), "ordered by the text, not by the row")
    end subroutine test_sort_by_values_character

    !> The negative control: values already in order must touch nothing.
    subroutine test_sort_by_values_noop(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=*), parameter :: fname = "test_run/rowverbs_sortvals_noop.parquet"
        integer(int64) :: gen
        integer(int32) :: v(4)
        !
        call write_small_file(fname)
        call parquet_open_table(t, fname)
        call t%materialize_all()
        gen = t%generation()
        v = [1, 2, 3, 4]
        call t%sort_by_values(v)
        call check(error, .not. t%is_detached(), &
            "a permutation that moves no row keeps the table's file")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "and does not advance the generation counter")
        if (allocated(error)) return
        v = [4, 3, 2, 1]
        call t%sort_by_values(v)
        call check(error, t%is_detached(), "the positive control: a real reorder detaches")
    end subroutine test_sort_by_values_noop

    !> The non-mutating twin hands the order back and leaves the table alone.
    subroutine test_argsort_by_values(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        real(real64) :: v(3)
        integer(int64), allocatable :: perm(:)
        integer(int64) :: gen
        integer(int32), allocatable :: id(:)
        !
        call build_distinct(t)
        gen = t%generation()
        v = [3.0_real64, 1.0_real64, 2.0_real64]
        call t%argsort_by_values(v, perm)
        call check(error, all(perm == [2_int64, 3_int64, 1_int64]), "the order the values imply")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "%argsort_by_values changes nothing")
        if (allocated(error)) return
        call t%get("id", id)
        call check(error, all(id == [10, 20, 30]), "the table's own rows are untouched")
    end subroutine test_argsort_by_values

    !> The int32 permutation form fills the same answer.
    subroutine test_argsort_by_values_i32(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: v(3)
        integer(int32), allocatable :: perm(:)
        !
        call build_distinct(t)
        v = [30_int64, 10_int64, 20_int64]
        call t%argsort_by_values(v, perm)
        call check(error, all(perm == [2, 3, 1]), "an int32 permutation says the same thing")
    end subroutine test_argsort_by_values_i32

    ! ---- fixture files ----------------------------------------------------------------------

    !> A four-row, two-column file, for the tests that need a table still attached to one.
    subroutine write_small_file(fname)
        character(len=*), intent(in) :: fname !! the path to write.
        type(parquet_writer) :: w
        integer(int32) :: id(4)
        real(real64) :: x(4)
        !
        id = [1, 2, 3, 4]
        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", id)
        call parquet_write_column(w, "x", x)
        call parquet_close_writer(w)
    end subroutine write_small_file

    !> A five-row file with two repeated ids, for the drop that must detach.
    subroutine write_dup_file(fname)
        character(len=*), intent(in) :: fname !! the path to write.
        type(parquet_writer) :: w
        integer(int32) :: id(5)
        !
        id = [1, 2, 1, 3, 2]
        call parquet_open_writer(w, fname)
        call parquet_write_column(w, "id", id)
        call parquet_close_writer(w)
    end subroutine write_dup_file

    ! ---- %value_counts ----------------------------------------------------------------------

    !> The documented order: count descending, and value ascending among equal counts.
    !!
    !! Asserted on the values AND the counts together, in order, rather than on the multiset of
    !! pairs -- the order is the contract here, and a result carrying the right pairs in the wrong
    !! order is exactly the failure this binding exists to prevent a caller from writing by hand.
    subroutine test_value_counts_order(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        integer(int32), allocatable :: vals(:)
        integer(int64), allocatable :: cnts(:)
        !
        call build_tied(t)
        call t%value_counts("tag", vc)
        call check(error, vc%ncols() == 2, "the result is a two-column table")
        if (allocated(error)) return
        call check(error, vc%nrows() == 4_int64, "one row per distinct value")
        if (allocated(error)) return
        call vc%get("tag", vals)
        call vc%get("count", cnts)
        call check(error, all(cnts == [2_int64, 2_int64, 1_int64, 1_int64]), &
            "counts must come back descending")
        if (allocated(error)) return
        call check(error, all(vals == [1, 5, 2, 9]), &
            "and equal counts must be broken by value ASCENDING: 1 before 5, 2 before 9")
    end subroutine test_value_counts_order

    !> `descending=.false.` reverses only the COUNT direction. The tie-break stays ascending by
    !! value, which is what makes the result fully determined in both directions rather than
    !! merely reversed.
    subroutine test_value_counts_ascending(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        integer(int32), allocatable :: vals(:)
        integer(int64), allocatable :: cnts(:)
        !
        call build_tied(t)
        call t%value_counts("tag", vc, descending=.false.)
        call vc%get("tag", vals)
        call vc%get("count", cnts)
        call check(error, all(cnts == [1_int64, 1_int64, 2_int64, 2_int64]), &
            "the rarest values must come first")
        if (allocated(error)) return
        call check(error, all(vals == [2, 9, 1, 5]), &
            "and the tie-break must still be value ascending, not reversed with the counts")
    end subroutine test_value_counts_ascending

    !> The null group is dropped by default and, with `dropna=.false.`, becomes one extra row --
    !! placed LAST whatever its count.
    !!
    !! The fixture is built so the null group is the LARGEST, which is the only shape that can
    !! tell "placed last" from "sorted with the others": with three nulls against two 4s and one
    !! 7, a null row sorted by count would come first.
    subroutine test_value_counts_dropna(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        integer(int32) :: v(6)
        integer(int32), allocatable :: vals(:)
        integer(int64), allocatable :: cnts(:)
        logical, allocatable :: valid(:)
        integer :: k
        !
        ! Rows 2, 4 and 6 are made Null AFTER the column is added: %add_column takes values only,
        ! and the stored value under a null is deliberately left as it was -- which is also what
        ! makes this fixture sharper, since those three rows hold 0 and a version that counted
        ! them as ordinary values would report a fourth group rather than a wrong count.
        v = [4, 0, 4, 0, 7, 0]
        call parquet_new_table(t)
        call t%add_column("v", v)
        do k = 2, 6, 2
            call t%set_null("v", k)
        end do
        call t%value_counts("v", vc)
        call vc%get("v", vals)
        call vc%get("count", cnts)
        call check(error, vc%nrows() == 2_int64, "by default the null group is not a row at all")
        if (allocated(error)) return
        call check(error, all(vals == [4, 7]) .and. all(cnts == [2_int64, 1_int64]), &
            "and the real values are counted as if the nulls were not there")
        if (allocated(error)) return

        call t%value_counts("v", vc, dropna=.false.)
        call vc%get("v", vals, is_valid=valid)
        call vc%get("count", cnts)
        call check(error, vc%nrows() == 3_int64, "dropna=.false. adds one row for the null group")
        if (allocated(error)) return
        call check(error, all(cnts == [2_int64, 1_int64, 3_int64]), &
            "the null row carries the null count and is placed LAST despite being the largest")
        if (allocated(error)) return
        call check(error, all(valid .eqv. [.true., .true., .false.]), &
            "and that row's VALUE is Null, gathered from a row of the column that really is null")
    end subroutine test_value_counts_dropna

    !> The table verb and the array form must agree about which values there are and how often
    !! each occurs. Two independent code paths -- `%argsort_by(group_offsets=)` against
    !! `pf_unique`'s run walk -- so this is a genuine cross-check rather than a restatement.
    !!
    !! Compared as a SET of pairs, since the two deliberately order their answers differently:
    !! the array form is in value order and the table form in count order.
    subroutine test_value_counts_matches_array_form(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        integer(int32) :: v(9)
        integer(int32), allocatable :: vals(:), dist(:)
        integer(int64), allocatable :: cnts(:), acnts(:)
        integer :: i, j
        logical :: ok
        !
        v = [3, 8, 3, 1, 8, 3, 1, 8, 8]
        call parquet_new_table(t)
        call t%add_column("v", v)
        call t%value_counts("v", vc)
        call vc%get("v", vals)
        call vc%get("count", cnts)
        call pf_value_counts(v, dist, acnts)
        call check(error, size(vals) == size(dist), "both forms must find the same distinct values")
        if (allocated(error)) return
        ok = .true.
        do i = 1, size(dist)
            do j = 1, size(vals)
                if (vals(j) == dist(i)) then
                    if (cnts(j) /= acnts(i)) ok = .false.
                    exit
                end if
                if (j == size(vals)) ok = .false.
            end do
        end do
        call check(error, ok, "and must agree on every value's count")
    end subroutine test_value_counts_matches_array_form

    !> A read: the source table keeps its rows, its generation counter and its file.
    !!
    !! The generation assertion is the sharp one. `%value_counts` gathers a COPY of the counted
    !! column, and a version that gathered the column in place would answer correctly here and
    !! silently reorder the caller's table -- which only `%generation()` and the row contents can
    !! see.
    subroutine test_value_counts_is_a_read(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        integer(int64) :: gen
        integer(int32), allocatable :: key(:)
        !
        call build_groups(t)
        gen = t%generation()
        call t%value_counts("key", vc)
        call check(error, t%nrows() == 6_int64, "the source table keeps every row")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "and its generation counter does not move")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "a read never detaches")
        if (allocated(error)) return
        call t%get("key", key)
        call check(error, all(key == [7, 3, 7, 3, 9, 7]), &
            "and the counted column is left in its original order, not gathered in place")
    end subroutine test_value_counts_is_a_read

    !> The value column IS the counted column, gathered -- so it keeps that column's name, kind
    !! and unit, and the count column is `int64`.
    !!
    !! The unit is the part a hand-written version drops: it lives on the column rather than in
    !! the table's descriptor, and only `%deep_copy` carries it across.
    subroutine test_value_counts_keeps_name_kind_unit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        real(real32) :: flux(5)
        character(len=:), allocatable :: u, n1, n2
        !
        flux = [1.0_real32, 2.0_real32, 1.0_real32, 3.0_real32, 1.0_real32]
        call parquet_new_table(t)
        call t%add_column("flux", flux, unit="Jy")
        call t%value_counts("flux", vc)
        call check(error, vc%has_column("flux"), "the value column keeps the counted column's name")
        if (allocated(error)) return
        call check(error, vc%kind("flux") == PK_FLOAT32, "and its kind")
        if (allocated(error)) return
        call vc%unit("flux", u)
        call check(error, u == "Jy", "and its unit")
        if (allocated(error)) return
        call check(error, vc%kind("count") == PK_INT64, "the count column is int64")
        if (allocated(error)) return
        call vc%column_name(1, n1)
        call vc%column_name(2, n2)
        call check(error, n1 == "flux" .and. n2 == "count", &
            "the value column comes first and the count second")
    end subroutine test_value_counts_keeps_name_kind_unit

    !> A string column counts on content like any other -- one binding covers every kind because
    !! nothing in it reads a value -- and `count_name` renames the second column.
    subroutine test_value_counts_strings_and_count_name(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t, vc
        character(len=5) :: band(6)
        character(len=:), allocatable :: got(:)
        integer(int64), allocatable :: cnts(:)
        !
        band = ["r    ", "g    ", "r    ", "i    ", "r    ", "g    "]
        call parquet_new_table(t)
        call t%add_column("band", band)
        call t%value_counts("band", vc, count_name="n")
        call check(error, vc%has_column("n") .and. .not. vc%has_column("count"), &
            "count_name must name the count column")
        if (allocated(error)) return
        call vc%get("band", got)
        call vc%get("n", cnts)
        call check(error, all(cnts == [3_int64, 2_int64, 1_int64]), &
            "r three times, g twice, i once")
        if (allocated(error)) return
        call check(error, got(1) == "r" .and. got(2) == "g" .and. got(3) == "i", &
            "and the values follow their counts, with the tie-break unused here")
    end subroutine test_value_counts_strings_and_count_name

end module test_table_rowverbs
