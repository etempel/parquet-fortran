!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for `%group_by` and the `parquet_grouping` object.
!!
!! **The oracle is the composition the object replaces, never a hand-written expectation.**
!! `%csr` must equal `%argsort_by(keys, perm, group_offsets=)` with the dropped groups removed,
!! `%key_table` must equal `%drop_duplicates(keys)` on a clone sorted by the keys, `%first_rows`
!! must be the `minval` of each group's rows and `%count` the group's size less the nulls
!! counted by hand -- so a change to the sort engine cannot desynchronise the two without a
!! failing test.
!!
!! **Fixtures carry row-distinct payloads**, so a query handed the wrong group's rows gives a
!! wrong VALUE rather than a plausible one; ties in the keys, so groups have several rows; a
!! null and a NaN in a key; two keys with a null in one of them (the "any key" rule); a
!! reversed copy of a fixture, so "first" cannot be the engine's tie rule in disguise.
!!
!! **`%apply`'s callbacks are MODULE procedures with their context in module variables, or
!! reducer objects with it in components** -- never internal procedures, which crash under one
!! supported compiler before they are called (.claude/rules/fortran-gotchas.md, flang). Each
!! module variable belongs to ONE test, because the suite runs its tests concurrently. The
!! oracle for `%apply` is the arithmetic over `%csr` it replaces; the matrix form is checked
!! against the one-value forms and the object form against the procedure form, all with `==`.
!! The team (`threads=`) is asserted in test/test_table_parallel.f90, which runs its tests with
!! no enclosing region, so a team opened there is a real one.
!!
!! The ABORTS -- no key, an unknown or unorderable key, a direction token, every per-group
!! query on a stale or never-built grouping, a group number out of range, `%apply` with `nout`
!! or `threads` below 1 -- live in test/error_scenarios.f90 as `table_group_*`. Every test that
!! writes a file uses its own path, since the suite runs its tests concurrently.
module test_table_group
    use parquet
    use iso_fortran_env, only : int32, int64, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_group

    !> Context for the procedure-form callbacks below: one module variable per test that uses
    !! it (see the header).
    real(real64), pointer :: ctx_sum_p(:) => null() !! test_apply_scalar_equals_csr_arithmetic's payload.
    real(real64), pointer :: ctx_obj_p(:) => null() !! test_apply_object_equals_procedure's payload.
    integer, allocatable :: visits(:)               !! test_apply_visits_each_group_once: calls per group.
    integer(int64) :: last_g = 0_int64              !! ... the previous call's g, for the order check.
    logical :: order_ok = .true.                    !! ... every call so far came in group order.
    logical :: rows_ok = .true.                     !! ... every call so far had ascending, non-empty rows.
    integer :: calls_seen = 0                       !! test_apply_empty_grouping: calls made.

    !> A reducer holding a `%col` pointer and a tunable, for the object form's tests: `out(1)`
    !! is `scale * sum(p(rows))`; `out(2)`, when there is room, the group's size; `out(3)` the
    !! group number -- so one reducer serves the one-value form (called with `out(g:g)`) and the
    !! matrix form.
    type, extends(parquet_group_reducer) :: scaled_sum_reducer
        real(real64), pointer :: p(:) => null() !! the payload column.
        real(real64) :: scale = 1.0_real64      !! multiplies the sum.
    contains
        procedure :: reduce => scaled_sum_reduce !! See the type.
    end type scaled_sum_reducer

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_group(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("csr equals argsort_by(group_offsets=); rows, size and the counts agree", &
                test_csr_equals_argsort_by), &
            new_unittest("csr under dropna is argsort_by's partition with the null group removed", &
                test_csr_drops_null_groups), &
            new_unittest("first_rows is minval and last_rows maxval of each group, reversed too", &
                test_first_and_last_rows_are_computed), &
            new_unittest("group_ids round-trips: the group's rows carry its number, dropped rows 0", &
                test_group_ids_round_trip), &
            new_unittest("key_table equals drop_duplicates on a clone, sorted by the keys", &
                test_key_table_equals_drop_duplicates), &
            new_unittest("key_table keeps each key's kind and unit, and a null key group's null", &
                test_key_table_kinds_unit_and_null_group), &
            new_unittest("a NaN key is one group under every dropna; a null key only under .false.", &
                test_nan_and_null_keys), &
            new_unittest("two keys: a row null in EITHER key belongs to no group under dropna", &
                test_two_keys_any_null_drops_the_row), &
            new_unittest("count is the group size less the nulls, for a column of any kind", &
                test_count_per_group), &
            new_unittest("a grouping is a read: rows, order, generation and a %col pointer survive", &
                test_grouping_is_a_read), &
            new_unittest("is_current: a value write keeps it, a row change stales it", &
                test_stale_and_current), &
            new_unittest("a string key partitions exactly as the integer codes of the same rows", &
                test_string_key_equals_integer_codes), &
            new_unittest("an empty table gives zero groups and allocated outputs", test_empty_table), &
            new_unittest("dropna control: a null-free key drops nothing under either setting", &
                test_dropna_null_free_control), &
            new_unittest("rebuilding replaces, threads= agrees with serial, clear resets", &
                test_rebuild_clear_and_threads), &
            new_unittest("group_by reads a key column the table has not read yet; count too", &
                test_lazy_columns_are_read), &
            new_unittest("apply's one-value form equals the arithmetic over csr, and is a read", &
                test_apply_scalar_equals_csr_arithmetic), &
            new_unittest("apply's matrix form gives, row by row, what the one-value forms give", &
                test_apply_matrix_equals_scalar), &
            new_unittest("apply calls the procedure once per group, in order, with ascending rows", &
                test_apply_visits_each_group_once), &
            new_unittest("apply's object form equals its procedure form; two objects are two contexts", &
                test_apply_object_equals_procedure), &
            new_unittest("apply over an empty grouping allocates zero-length outputs and calls nothing", &
                test_apply_empty_grouping) &
            ]
    end subroutine collect_tests_table_group

    ! ---- fixtures ------------------------------------------------------------------------------

    !> Nine rows over three int32 keys, INTERLEAVED, with a row-distinct real payload: 3 at rows
    !! 2, 4, 7; 7 at rows 1, 3, 6, 9; 9 at rows 5, 8.
    subroutine build_basic(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: key(9)
        real(real64) :: payload(9)
        integer :: i
        key = [7, 3, 7, 3, 9, 7, 3, 9, 7]
        do i = 1, 9
            payload(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("key", key)
        call t%add_column("payload", payload)
    end subroutine build_basic

    !> `build_basic` with its rows in the opposite order, so the lowest row of a group is the
    !! LAST one the sort's tie rule lists if that rule were ever reversed.
    subroutine build_basic_reversed(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: key(9)
        real(real64) :: payload(9)
        integer :: i
        key = [7, 9, 3, 7, 9, 3, 7, 3, 7]
        do i = 1, 9
            payload(i) = real(10 - i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("key", key)
        call t%add_column("payload", payload)
    end subroutine build_basic_reversed

    !> Eight rows over a real64 key holding two NaNs and two nulls: 2.0 at rows 1, 5; NaN at
    !! rows 2, 6; 1.0 at rows 3, 8; null at rows 4, 7.
    subroutine build_nullnan(t)
        type(parquet_table), intent(out) :: t !! the table.
        real(real64) :: k(8), payload(8), nan
        integer :: i
        nan = ieee_value(0.0_real64, ieee_quiet_nan)
        k = [2.0_real64, nan, 1.0_real64, 0.0_real64, 2.0_real64, nan, 0.0_real64, 1.0_real64]
        do i = 1, 8
            payload(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("payload", payload)
        call t%set_null("k", 4_int64)
        call t%set_null("k", 7_int64)
    end subroutine build_nullnan

    !> Seven rows over two keys, an int64 `a` null at row 6 only and a string `b` null at row 7
    !! only, so the "any key null" rule and the "null is one more value" rule can be told apart.
    subroutine build_twokeys(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int64) :: a(7)
        character(len=1) :: b(7)
        real(real64) :: payload(7)
        integer :: i
        a = [1_int64, 1_int64, 2_int64, 2_int64, 1_int64, 1_int64, 2_int64]
        b = ["x", "y", "x", "x", "x", "y", "x"]
        do i = 1, 7
            payload(i) = real(i, real64)
        end do
        call parquet_new_table(t)
        call t%add_column("a", a)
        call t%add_column("b", b)
        call t%add_column("payload", payload)
        call t%set_null("a", 6_int64)
        call t%set_null("b", 7_int64)
    end subroutine build_twokeys

    !> Eight rows over an int32 `k`, a string `s`, a real64 `r` with a unit and a date `d`, with
    !! ties in every one, for the key-table tests.
    subroutine build_kinds(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: k(8)
        character(len=1) :: s(8)
        real(real64) :: r(8)
        type(parquet_date) :: d(8)
        integer :: i
        k = [2, 1, 2, 1, 3, 1, 2, 3]
        s = ["b", "a", "a", "a", "b", "a", "b", "a"]
        r = [1.5_real64, 0.5_real64, 1.5_real64, 0.5_real64, 2.5_real64, 0.5_real64, 1.5_real64, 2.5_real64]
        do i = 1, 8
            call d(i)%set(2024, 6, 1 + mod(i, 3))
        end do
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("s", s)
        call t%add_column("r", r, unit="Jy")
        call t%add_column("d", d)
    end subroutine build_kinds

    ! ---- the tests -----------------------------------------------------------------------------

    !> `%csr` against `%argsort_by(group_offsets=)` on a null-free key, `%rows(g)` against the
    !! slice of it in both kinds, `%size` against the offset differences and summing to
    !! `%nrows()`, the introspection, the string key form, and the groups in ascending key order.
    subroutine test_csr_equals_argsort_by(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp, grp2
        integer(int64), allocatable :: perm(:), go(:), off(:), rows(:), r64(:), c64(:), off2(:), rows2(:), first(:)
        integer(int32), allocatable :: r32(:), c32(:), key(:)
        character(len=:), allocatable :: names(:)
        integer(int64) :: g
        !
        call build_basic(t)
        call t%group_by(["key"], grp)
        call t%argsort_by(["key"], perm, group_offsets=go)
        call grp%csr(off, rows)
        call check(error, size(off) == size(go) .and. size(rows) == size(perm), "csr has the oracle's shape")
        if (allocated(error)) return
        call check(error, all(off == go) .and. all(rows == perm), "csr equals argsort_by's partition")
        if (allocated(error)) return
        call check(error, grp%ngroups() == 3_int64 .and. grp%nrows() == 9_int64 .and. &
            grp%max_size() == 4_int64 .and. grp%nkeys() == 1_int64, "ngroups, nrows, max_size and nkeys")
        if (allocated(error)) return
        call grp%key_names(names)
        call check(error, size(names) == 1 .and. names(1) == "key", "key_names lists the one key")
        if (allocated(error)) return
        do g = 1_int64, 3_int64
            call grp%rows(g, r64)
            call check(error, all(r64 == perm(go(g):go(g + 1) - 1)), "rows(g) is the slice of the csr")
            if (allocated(error)) return
            call grp%rows(g, r32)
            call check(error, all(int(r32, int64) == r64), "the int32 rows agree")
            if (allocated(error)) return
        end do
        call grp%size(c64)
        call check(error, all(c64 == go(2:4) - go(1:3)) .and. sum(c64) == grp%nrows(), "size is the group lengths")
        if (allocated(error)) return
        call grp%size(c32)
        call check(error, all(int(c32, int64) == c64), "the int32 counts agree")
        if (allocated(error)) return
        call t%group_by("key", grp2)
        call grp2%csr(off2, rows2)
        call check(error, all(off2 == off) .and. all(rows2 == rows), "the separated-string key form agrees")
        if (allocated(error)) return
        call grp%first_rows(first)
        call t%get("key", key)
        call check(error, all(key(first) == [3, 7, 9]), "the groups come in ascending key order")
    end subroutine test_csr_equals_argsort_by

    !> The acceptance property in full: under `dropna=.true.` `%csr` equals `%argsort_by`'s
    !! partition with the null group removed -- found by asking the column, as the object does --
    !! and under `dropna=.false.` it equals that partition exactly.
    subroutine test_csr_drops_null_groups(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: perm(:), go(:), off(:), rows(:), want_off(:), want_rows(:)
        integer(int64) :: g, ng, w, j, sz
        logical :: found_null
        !
        call build_nullnan(t)
        call t%argsort_by(["k"], perm, group_offsets=go)
        ng = size(go, kind=int64) - 1_int64
        ! The oracle without its null group: every other group's rows, in the order they came.
        allocate(want_off(ng + 1_int64), want_rows(size(perm, kind=int64)))
        found_null = .false.
        w = 0_int64
        j = 0_int64
        want_off(1) = 1_int64
        do g = 1_int64, ng
            if (t%is_null("k", perm(go(g)))) then
                found_null = .true.
                cycle
            end if
            sz = go(g + 1_int64) - go(g)
            want_rows(w + 1_int64:w + sz) = perm(go(g):go(g + 1_int64) - 1_int64)
            w = w + sz
            j = j + 1_int64
            want_off(j + 1_int64) = w + 1_int64
        end do
        call check(error, found_null .and. j == ng - 1_int64, "the oracle had exactly one null group to drop (vacuity guard)")
        if (allocated(error)) return
        call t%group_by(["k"], grp)
        call grp%csr(off, rows)
        call check(error, size(off) == j + 1_int64 .and. size(rows) == w, "dropna: the csr has the oracle's shape less one group")
        if (allocated(error)) return
        call check(error, all(off == want_off(1:j + 1_int64)) .and. all(rows == want_rows(1:w)), &
            "dropna: the csr is the oracle's partition with the null group removed")
        if (allocated(error)) return
        call t%group_by(["k"], grp, dropna=.false.)
        call grp%csr(off, rows)
        call check(error, size(off) == size(go) .and. size(rows) == size(perm), "dropna=.false.: the csr has the oracle's shape")
        if (allocated(error)) return
        call check(error, all(off == go) .and. all(rows == perm), "dropna=.false.: the csr is the oracle's partition exactly")
    end subroutine test_csr_drops_null_groups

    !> `%first_rows` is the `minval` and `%last_rows` the `maxval` of each group's rows, on the
    !! fixture and on its reversed copy, in both kinds -- computed, never the permutation's ends.
    subroutine test_first_and_last_rows_are_computed(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, tr
        !
        call build_basic(t)
        call first_last_check(error, t, "the fixture")
        if (allocated(error)) return
        call build_basic_reversed(tr)
        call first_last_check(error, tr, "the reversed fixture")
    end subroutine test_first_and_last_rows_are_computed

    !> The assertion `test_first_and_last_rows_are_computed` makes on each table.
    subroutine first_last_check(error, t, label)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table), intent(in) :: t     !! the table.
        character(len=*), intent(in) :: label    !! which fixture, for the messages.
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:), first(:), last(:), want_first(:), want_last(:)
        integer(int32), allocatable :: first32(:), last32(:)
        integer(int64) :: g
        !
        call t%group_by(["key"], grp)
        call grp%csr(off, rows)
        allocate(want_first(grp%ngroups()), want_last(grp%ngroups()))
        do g = 1_int64, grp%ngroups()
            want_first(g) = minval(rows(off(g):off(g + 1) - 1))
            want_last(g) = maxval(rows(off(g):off(g + 1) - 1))
        end do
        call grp%first_rows(first)
        call grp%last_rows(last)
        call check(error, all(first == want_first), label // ": first_rows is each group's lowest row")
        if (allocated(error)) return
        call check(error, all(last == want_last), label // ": last_rows is each group's highest row")
        if (allocated(error)) return
        call check(error, all(first < last), label // ": every group has more than one row (vacuity guard)")
        if (allocated(error)) return
        call grp%first_rows(first32)
        call grp%last_rows(last32)
        call check(error, all(int(first32, int64) == first) .and. all(int(last32, int64) == last), &
            label // ": the int32 forms agree")
    end subroutine first_last_check

    !> `%group_ids` carries each group's number on exactly its rows, 0 on the rows `dropna`
    !! left out, in both kinds.
    subroutine test_group_ids_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: codes(:), rows(:)
        integer(int32), allocatable :: codes32(:)
        integer(int64) :: g
        !
        call build_nullnan(t)
        call t%group_by(["k"], grp)
        call grp%group_ids(codes)
        call check(error, size(codes) == 8, "one code per TABLE row, dropped rows included")
        if (allocated(error)) return
        do g = 1_int64, grp%ngroups()
            call grp%rows(g, rows)
            call check(error, all(codes(rows) == g), "the rows of group g carry g")
            if (allocated(error)) return
        end do
        call check(error, count(codes == 0_int64) == 2 .and. codes(4) == 0_int64 .and. codes(7) == 0_int64, &
            "the two null-key rows belong to no group")
        if (allocated(error)) return
        call grp%group_ids(codes32)
        call check(error, all(int(codes32, int64) == codes), "the int32 codes agree")
    end subroutine test_group_ids_round_trip

    !> `%key_table` over two keys equals `%drop_duplicates` on a clone sorted by the same keys,
    !! row for row; it is sorted by the keys; `size_name=` adds `%size` as an int64 column.
    subroutine test_key_table_equals_drop_duplicates(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, c, kt
        type(parquet_grouping) :: grp
        integer(int32), allocatable :: k1(:), k2(:)
        character(len=:), allocatable :: s1(:), s2(:)
        integer(int64), allocatable :: n(:), c64(:)
        !
        call build_kinds(t)
        call t%group_by(["k", "s"], grp)
        call grp%key_table(kt, size_name="n")
        call t%clone(c)
        call c%drop_duplicates(["k", "s"])
        call c%sort_by(["k", "s"])
        call check(error, kt%nrows() == 5_int64 .and. kt%nrows() == c%nrows() .and. kt%nrows() == grp%ngroups(), &
            "one row per group, and as many as drop_duplicates keeps")
        if (allocated(error)) return
        call kt%get("k", k1)
        call c%get("k", k2)
        call check(error, all(k1 == k2), "the int32 key column equals the clone's")
        if (allocated(error)) return
        call kt%get("s", s1)
        call c%get("s", s2)
        call check(error, all(s1 == s2), "the string key column equals the clone's")
        if (allocated(error)) return
        call check(error, kt%is_sorted_by(["k", "s"]), "the key table is sorted by the keys")
        if (allocated(error)) return
        call check(error, kt%ncols() == 3 .and. kt%kind("n") == PK_INT64, "the count column is a third, int64, column")
        if (allocated(error)) return
        call kt%get("n", n)
        call grp%size(c64)
        call check(error, all(n == c64) .and. sum(n) == 8_int64, "and it equals %size")
    end subroutine test_key_table_equals_drop_duplicates

    !> Each key column comes through with its kind and unit -- the part a hand-written version
    !! drops, since the unit lives on the column -- and under `dropna=.false.` the null key
    !! group's key is null in the key table, last; under the default it is not there at all.
    subroutine test_key_table_kinds_unit_and_null_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt, kt2
        type(parquet_grouping) :: grp
        character(len=:), allocatable :: u
        integer(int64) :: g
        !
        call build_kinds(t)
        call t%set_null("r", 5_int64)
        call t%group_by(["r", "d"], grp, dropna=.false.)
        call grp%key_table(kt)
        call check(error, kt%kind("r") == PK_FLOAT64 .and. kt%kind("d") == PK_DATE, "the kinds are the keys' own")
        if (allocated(error)) return
        call kt%unit("r", u)
        call check(error, u == "Jy", "and so is the unit")
        if (allocated(error)) return
        call check(error, kt%nrows() == grp%ngroups() .and. kt%is_null("r", kt%nrows()), &
            "the null key group is last, and its key is null in the key table")
        if (allocated(error)) return
        do g = 1_int64, kt%nrows() - 1_int64
            call check(error, .not. kt%is_null("r", g), "no other group's key is null")
            if (allocated(error)) return
        end do
        call t%group_by(["r", "d"], grp)
        call grp%key_table(kt2)
        call check(error, kt2%nrows() == kt%nrows() - 1_int64 .and. .not. kt2%has_nulls("r"), &
            "under the default dropna the null key group is not there")
    end subroutine test_key_table_kinds_unit_and_null_group

    !> A NaN is a value: one group, present under both `dropna` settings, after the values; a
    !! null is subject to `dropna`: no group under the default, one group last under `.false.`.
    subroutine test_nan_and_null_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: first(:), rows(:)
        real(real64), allocatable :: k(:)
        integer(int64) :: g
        !
        call build_nullnan(t)
        call t%get("k", k)
        call t%group_by(["k"], grp)
        call check(error, grp%ngroups() == 3_int64 .and. grp%nrows() == 6_int64, "dropna: 1.0, 2.0 and NaN; 6 rows")
        if (allocated(error)) return
        call grp%first_rows(first)
        call check(error, k(first(1)) == 1.0_real64 .and. k(first(2)) == 2.0_real64 .and. ieee_is_nan(k(first(3))), &
            "values ascending, then the NaN group")
        if (allocated(error)) return
        do g = 1_int64, 3_int64
            call check(error, .not. t%is_null("k", first(g)), "no group is the null one")
            if (allocated(error)) return
        end do
        call grp%rows(3_int64, rows)
        call check(error, all(rows == [2_int64, 6_int64]), "both NaN rows are in the one NaN group")
        if (allocated(error)) return
        call t%group_by(["k"], grp, dropna=.false.)
        call check(error, grp%ngroups() == 4_int64 .and. grp%nrows() == 8_int64, "dropna=.false.: the null group too")
        if (allocated(error)) return
        call grp%first_rows(first)
        call check(error, t%is_null("k", first(4)) .and. ieee_is_nan(k(first(3))), &
            "the null group is last, after the NaN group")
        if (allocated(error)) return
        call grp%rows(4_int64, rows)
        call check(error, all(rows == [4_int64, 7_int64]), "and holds both null rows")
    end subroutine test_nan_and_null_keys

    !> Over two keys a row null in EITHER belongs to no group under `dropna=.true.`, and is one
    !! more key value under `dropna=.false.`, the null groups placed last per key.
    subroutine test_two_keys_any_null_drops_the_row(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: codes(:), rows(:)
        !
        call build_twokeys(t)
        call t%group_by(["a", "b"], grp)
        call check(error, grp%ngroups() == 3_int64 .and. grp%nrows() == 5_int64, "three groups over five rows")
        if (allocated(error)) return
        call grp%group_ids(codes)
        call check(error, codes(6) == 0_int64 .and. codes(7) == 0_int64 .and. count(codes == 0_int64) == 2, &
            "the row null in a and the row null in b are both out")
        if (allocated(error)) return
        call grp%rows(1_int64, rows)
        call check(error, all(rows == [1_int64, 5_int64]), "(1, x) is rows 1 and 5")
        if (allocated(error)) return
        call grp%rows(2_int64, rows)
        call check(error, all(rows == [2_int64]), "(1, y) is row 2")
        if (allocated(error)) return
        call grp%rows(3_int64, rows)
        call check(error, all(rows == [3_int64, 4_int64]), "(2, x) is rows 3 and 4")
        if (allocated(error)) return
        call t%group_by(["a", "b"], grp, dropna=.false.)
        call check(error, grp%ngroups() == 5_int64 .and. grp%nrows() == 7_int64, "dropna=.false.: five groups, every row")
        if (allocated(error)) return
        call grp%rows(4_int64, rows)
        call check(error, all(rows == [7_int64]), "(2, null) follows (2, x)")
        if (allocated(error)) return
        call grp%rows(5_int64, rows)
        call check(error, all(rows == [6_int64]), "(null, y) is last")
        if (allocated(error)) return
        call grp%key_table(kt)
        call check(error, kt%is_null("b", 4_int64) .and. kt%is_null("a", 5_int64) .and. &
            .not. kt%is_null("a", 4_int64) .and. .not. kt%is_null("b", 5_int64), &
            "the key table carries each null where the key was null, and nowhere else")
    end subroutine test_two_keys_any_null_drops_the_row

    !> `%count` is the group's size less the nulls counted by hand through `%is_null`, on a
    !! real column, a string column and a null-free column, in both kinds.
    subroutine test_count_per_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64) :: x(9)
        character(len=1) :: s(9)
        integer(int64), allocatable :: off(:), rows(:), want(:), cx(:), cs(:), cp(:), sizes(:)
        integer(int32), allocatable :: cx32(:)
        integer(int64) :: g, i
        !
        call build_basic(t)
        x = [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, 6.0_real64, 7.0_real64, 8.0_real64, 9.0_real64]
        s = ["p", "q", "r", "s", "t", "u", "v", "w", "z"]
        call t%add_column("x", x)
        call t%add_column("s", s)
        call t%set_null("x", 1_int64)
        call t%set_null("x", 4_int64)
        call t%set_null("x", 5_int64)
        call t%set_null("s", 3_int64)
        call t%group_by(["key"], grp)
        call grp%csr(off, rows)
        allocate(want(grp%ngroups()))
        do g = 1_int64, grp%ngroups()
            want(g) = 0_int64
            do i = off(g), off(g + 1) - 1
                if (.not. t%is_null("x", rows(i))) want(g) = want(g) + 1_int64
            end do
        end do
        call grp%count("x", cx)
        call check(error, all(cx == want) .and. all(cx == [2_int64, 3_int64, 1_int64]), &
            "count(x) is the size less the nulls, per group")
        if (allocated(error)) return
        call grp%count("x", cx32)
        call check(error, all(int(cx32, int64) == cx), "the int32 counts agree")
        if (allocated(error)) return
        call grp%count("s", cs)
        call check(error, all(cs == [3_int64, 3_int64, 2_int64]), "a string column counts through the same path")
        if (allocated(error)) return
        call grp%count("payload", cp)
        call grp%size(sizes)
        call check(error, all(cp == sizes), "a null-free column counts as its group sizes")
    end subroutine test_count_per_group

    !> Every binding is a read: after all of them the rows, their order, the generation and a
    !! `%col` pointer taken beforehand are as they were, and nothing detached.
    subroutine test_grouping_is_a_read(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        real(real64), pointer :: p(:)
        integer(int64) :: gen0
        integer(int64), allocatable :: off(:), rows(:), c64(:), r64(:), first(:), last(:), codes(:), cnt(:)
        integer(int32), allocatable :: key(:)
        real(real64), allocatable :: payload(:)
        character(len=:), allocatable :: names(:)
        !
        call build_basic(t)
        gen0 = t%generation()
        call t%col("payload", p)
        call t%group_by(["key"], grp)
        call grp%csr(off, rows)
        call grp%size(c64)
        call grp%rows(2_int64, r64)
        call grp%first_rows(first)
        call grp%last_rows(last)
        call grp%group_ids(codes)
        call grp%key_names(names)
        call grp%key_table(kt, size_name="n")
        call grp%count("payload", cnt)
        call check(error, grp%ngroups() == 3_int64 .and. grp%is_current() .and. kt%nrows() == 3_int64, "everything answered")
        if (allocated(error)) return
        call check(error, t%generation() == gen0, "no binding bumped the generation")
        if (allocated(error)) return
        call check(error, .not. t%is_detached() .and. t%nrows() == 9_int64, "nothing detached, and every row is there")
        if (allocated(error)) return
        call t%get("key", key)
        call t%get("payload", payload)
        call check(error, all(key == [7, 3, 7, 3, 9, 7, 3, 9, 7]), "the key column is in its original order")
        if (allocated(error)) return
        call check(error, all(payload == [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, &
            6.0_real64, 7.0_real64, 8.0_real64, 9.0_real64]), "and so is the payload")
        if (allocated(error)) return
        call check(error, associated(p) .and. p(1) == 1.0_real64 .and. p(9) == 9.0_real64, &
            "a %col pointer taken before the grouping still points at the same values")
    end subroutine test_grouping_is_a_read

    !> The staleness stamp: a value write (`%set_element`, `%fillna`) leaves the grouping usable
    !! and answering; a row-structural change stales it -- `%is_current()` answers .false., and
    !! the abort every per-group query then raises is `table_group_stale_*` in
    !! test/error_scenarios.f90 -- and rebuilding answers the NEW row numbers.
    subroutine test_stale_and_current(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: rows(:)
        real(real64) :: x(9)
        logical :: keep(9)
        !
        call build_basic(t)
        x = 1.0_real64
        call t%add_column("x", x)
        call t%set_null("x", 2_int64)
        call t%group_by(["key"], grp)
        call t%set_element("x", 4_int64, 22.0_real64)
        call check(error, grp%is_current(), "a value write does not stale the grouping")
        if (allocated(error)) return
        call t%fillna(["x"], -1.0_real64)
        call check(error, grp%is_current(), "a fill does not stale it")
        if (allocated(error)) return
        call grp%rows(1_int64, rows)
        call check(error, all(rows == [2_int64, 4_int64, 7_int64]), "and it still answers")
        if (allocated(error)) return
        keep = .true.
        keep(2) = .false.
        call t%filter_rows(keep)
        call check(error, .not. grp%is_current(), "a row change stales it")
        if (allocated(error)) return
        call t%group_by(["key"], grp)
        call check(error, grp%is_current(), "rebuilding makes it current again")
        if (allocated(error)) return
        call grp%rows(1_int64, rows)
        call check(error, all(rows == [3_int64, 6_int64]), "and the rebuilt grouping answers the NEW row numbers")
    end subroutine test_stale_and_current

    !> A string key partitions the rows exactly as an integer column coding the same values does
    !! -- one comparator, the sort's -- and its key table lists the strings ascending.
    subroutine test_string_key_equals_integer_codes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: gs, gc
        character(len=1) :: s(6)
        integer(int32) :: code(6)
        integer(int64), allocatable :: offs(:), rowss(:), offc(:), rowsc(:)
        character(len=:), allocatable :: got(:)
        !
        s = ["b", "a", "c", "a", "b", "a"]
        code = [2, 1, 3, 1, 2, 1]
        call parquet_new_table(t)
        call t%add_column("s", s)
        call t%add_column("code", code)
        call t%group_by(["s"], gs)
        call t%group_by(["code"], gc)
        call gs%csr(offs, rowss)
        call gc%csr(offc, rowsc)
        call check(error, size(offs) == size(offc) .and. size(rowss) == size(rowsc), "the same number of groups and rows")
        if (allocated(error)) return
        call check(error, all(offs == offc) .and. all(rowss == rowsc), "the string partition equals the code partition")
        if (allocated(error)) return
        call gs%key_table(kt)
        call kt%get("s", got)
        call check(error, size(got) == 3 .and. got(1) == "a" .and. got(2) == "b" .and. got(3) == "c", &
            "the key table lists the strings ascending")
    end subroutine test_string_key_equals_integer_codes

    !> A zero-row table groups without complaint: zero groups, and every output allocated at
    !! zero length so a caller's loop runs zero times rather than touching an unallocated array.
    subroutine test_empty_table(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: none(:), off(:), rows(:), c64(:), first(:), last(:), codes(:), cnt(:)
        !
        allocate(none(0))
        call parquet_new_table(t)
        call t%add_column("k", none)
        call t%group_by("k", grp)
        call check(error, grp%is_current() .and. grp%ngroups() == 0_int64 .and. grp%nrows() == 0_int64 .and. &
            grp%max_size() == 0_int64 .and. grp%nkeys() == 1_int64, "an empty table gives a current, empty grouping")
        if (allocated(error)) return
        call grp%csr(off, rows)
        call check(error, size(off) == 1 .and. off(1) == 1_int64 .and. size(rows) == 0, "csr is the sentinel alone")
        if (allocated(error)) return
        call grp%size(c64)
        call grp%first_rows(first)
        call grp%last_rows(last)
        call grp%group_ids(codes)
        call grp%count("k", cnt)
        call check(error, size(c64) == 0 .and. size(first) == 0 .and. size(last) == 0 .and. size(codes) == 0 .and. &
            size(cnt) == 0, "every per-group array is allocated at zero length")
        if (allocated(error)) return
        call grp%key_table(kt, size_name="n")
        call check(error, kt%nrows() == 0_int64 .and. kt%has_column("k") .and. kt%has_column("n"), &
            "the key table has the columns and no row")
    end subroutine test_empty_table

    !> The negative control for the `dropna` pass: a key without a null drops nothing under
    !! either setting, so the two partitions are identical and cover every row.
    subroutine test_dropna_null_free_control(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: g1, g2
        integer(int64), allocatable :: off1(:), rows1(:), off2(:), rows2(:)
        !
        call build_basic(t)
        call t%group_by(["key"], g1, dropna=.true.)
        call t%group_by(["key"], g2, dropna=.false.)
        call g1%csr(off1, rows1)
        call g2%csr(off2, rows2)
        call check(error, g1%nrows() == t%nrows() .and. g2%nrows() == t%nrows(), "every row is in a group")
        if (allocated(error)) return
        call check(error, all(off1 == off2) .and. all(rows1 == rows2), "and the two partitions are the same")
    end subroutine test_dropna_null_free_control

    !> Building into an object again replaces its grouping; an explicit `threads=` gives the
    !! serial partition; `%clear` returns it to the never-built state.
    subroutine test_rebuild_clear_and_threads(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp, serial
        integer(int64), allocatable :: off1(:), rows1(:), off2(:), rows2(:)
        character(len=:), allocatable :: names(:)
        !
        call build_basic(t)
        call t%group_by(["key"], grp)
        call check(error, grp%ngroups() == 3_int64, "three groups over the key")
        if (allocated(error)) return
        call t%group_by(["payload"], grp)
        call grp%key_names(names)
        call check(error, grp%ngroups() == 9_int64 .and. names(1) == "payload", "rebuilding replaces the grouping")
        if (allocated(error)) return
        call t%group_by(["key"], serial, threads=1)
        call t%group_by(["key"], grp, threads=2)
        call serial%csr(off1, rows1)
        call grp%csr(off2, rows2)
        call check(error, all(off1 == off2) .and. all(rows1 == rows2), "threads=2 gives the serial partition")
        if (allocated(error)) return
        call grp%clear()
        call grp%key_names(names)
        call check(error, .not. grp%is_current() .and. grp%ngroups() == 0_int64 .and. grp%nrows() == 0_int64 .and. &
            grp%nkeys() == 0_int64 .and. size(names) == 0, "clear returns it to the never-built state")
    end subroutine test_rebuild_clear_and_threads

    !> `%group_by` on a file-backed table reads a key column nothing has read yet -- the ordinary
    !! lazy first touch -- and reads nothing else; `%count` reads its column the same way; the
    !! table stays attached and the grouping current through both.
    subroutine test_lazy_columns_are_read(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: file = "test_run/table_group_lazy.parquet"
        type(parquet_writer) :: w
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: cnt(:)
        !
        call parquet_open_writer(w, file)
        call parquet_write_column(w, "id", [300_int32, 100_int32, 200_int32, 100_int32])
        call parquet_write_column(w, "v", [3.0_real64, 1.0_real64, 2.0_real64, 4.0_real64])
        call parquet_close_writer(w)
        call parquet_open_table(t, file)
        call check(error, t%residency("id") == RES_EMPTY, "the key column is not resident before the build")
        if (allocated(error)) return
        call t%group_by("id", grp)
        call check(error, t%residency("id") == RES_FULL, "group_by read the key column")
        if (allocated(error)) return
        call check(error, t%residency("v") == RES_EMPTY, "and read nothing else")
        if (allocated(error)) return
        call check(error, grp%ngroups() == 3_int64 .and. grp%is_current() .and. .not. t%is_detached(), &
            "the grouping answers and the table is still attached")
        if (allocated(error)) return
        call grp%count("v", cnt)
        call check(error, t%residency("v") == RES_FULL .and. all(cnt == [2_int64, 1_int64, 1_int64]), &
            "count read its column and counted it")
        if (allocated(error)) return
        call check(error, grp%is_current() .and. .not. t%is_detached(), "a lazy read stales nothing")
    end subroutine test_lazy_columns_are_read

    ! ---- %apply ---------------------------------------------------------------------------------

    !> `%apply` in its one-value procedure form against the arithmetic it replaces: for every
    !! group, the callback's `sum(payload(rows))` -- through a `%col` pointer taken before the
    !! call, which the loop leaves valid because nothing in an `%apply` moves a row -- equals the
    !! same sum over `%csr`'s rows. And the call is a read: generation, rows and pointer survive.
    subroutine test_apply_scalar_equals_csr_arithmetic(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:)
        real(real64), allocatable :: got(:), want(:), payload(:)
        integer(int64) :: g, gen0
        !
        call build_basic(t)
        gen0 = t%generation()
        call t%col("payload", ctx_sum_p)
        call t%group_by(["key"], grp)
        call grp%apply(cb_sum_ctx, got)
        call grp%csr(off, rows)
        call t%get("payload", payload)
        allocate(want(grp%ngroups()))
        do g = 1_int64, grp%ngroups()
            want(g) = sum(payload(rows(off(g) : off(g + 1_int64) - 1_int64)))
        end do
        call check(error, size(got) == 3 .and. all(got == want), "one sum per group, equal to the arithmetic over csr")
        if (allocated(error)) return
        call check(error, all(want == [13.0_real64, 19.0_real64, 13.0_real64]), "and they are the fixture's sums")
        if (allocated(error)) return
        call check(error, t%generation() == gen0 .and. t%nrows() == 9_int64 .and. grp%is_current(), &
            "apply is a read: the generation and the row count are as they were")
        if (allocated(error)) return
        call check(error, associated(ctx_sum_p) .and. ctx_sum_p(9) == 9.0_real64, &
            "and the %col pointer the callback read through still points at the payload")
    end subroutine test_apply_scalar_equals_csr_arithmetic

    !> The matrix form against the one-value forms: `out(k, :)` of a three-value callback equals
    !! the k-th one-value callback's answer for each k, bit for bit. The values are row-distinct
    !! (a row sum, a size, the group number with its lowest row), so a result stored at the
    !! wrong group would differ.
    subroutine test_apply_matrix_equals_scalar(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: m(:, :), s1(:), s2(:), s3(:)
        !
        call build_basic(t)
        call t%group_by(["key"], grp)
        call grp%apply(cb_three, 3, m)
        call grp%apply(cb_row_sum, s1)
        call grp%apply(cb_size, s2)
        call grp%apply(cb_g_and_first, s3)
        call check(error, size(m, 1) == 3 .and. size(m, 2) == 3, "out is (nout, ngroups)")
        if (allocated(error)) return
        call check(error, all(m(1, :) == s1) .and. all(m(2, :) == s2) .and. all(m(3, :) == s3), &
            "each row of the matrix is the matching one-value answer")
        if (allocated(error)) return
        call check(error, all(s1 == [13.0_real64, 19.0_real64, 13.0_real64]) .and. &
            all(s2 == [3.0_real64, 4.0_real64, 2.0_real64]) .and. &
            all(s3 == [1002.0_real64, 2001.0_real64, 3005.0_real64]), "and the answers are the fixture's")
    end subroutine test_apply_matrix_equals_scalar

    !> The call contract, serially: the procedure is called exactly once per group, in group
    !! order, with a non-empty ascending row list each time -- recorded by the callback in
    !! module variables and read back here. Over a key with a NaN and nulls, so the dropped rows
    !! reach no call and the NaN group is one call.
    subroutine test_apply_visits_each_group_once(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: got(:)
        !
        call build_nullnan(t)
        call t%group_by("k", grp)
        allocate(visits(grp%ngroups()))
        visits = 0
        last_g = 0_int64
        order_ok = .true.
        rows_ok = .true.
        call grp%apply(cb_visit, got)
        call check(error, grp%ngroups() == 3_int64 .and. all(visits == 1), "each of the three groups was visited once")
        if (allocated(error)) return
        call check(error, order_ok, "in group order")
        if (allocated(error)) return
        call check(error, rows_ok, "each with a non-empty, ascending row list")
        if (allocated(error)) return
        call check(error, all(got == [1.0_real64, 2.0_real64, 3.0_real64]), "and each result landed at its own group")
    end subroutine test_apply_visits_each_group_once

    !> The object form against the procedure form: a reducer holding a `%col` pointer and a
    !! scale as components gives, bit for bit, what a procedure with the same context in module
    !! variables gives, in both output shapes -- and the one-value object form calls `%reduce`
    !! with `out(g:g)`, which the reducer sees as a one-entry `out`. Two reducers with different
    !! scales in one program are two contexts, which module variables cannot be.
    subroutine test_apply_object_equals_procedure(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        type(scaled_sum_reducer) :: red, red2
        real(real64), allocatable :: o1(:), p1(:), o2(:, :), o3(:)
        !
        call build_basic(t)
        call t%col("payload", ctx_obj_p)
        call t%col("payload", red%p)
        red%scale = 2.5_real64
        call t%group_by(["key"], grp)
        call grp%apply(red, o1)
        call grp%apply(cb_scaled_sum_ctx, p1)
        call check(error, size(o1) == 3 .and. all(o1 == p1), "the one-value object form equals the procedure form")
        if (allocated(error)) return
        call check(error, all(p1 == 2.5_real64 * [13.0_real64, 19.0_real64, 13.0_real64]), "and both are the scaled sums")
        if (allocated(error)) return
        call grp%apply(red, 3, o2)
        call check(error, all(o2(1, :) == p1) .and. all(o2(2, :) == [3.0_real64, 4.0_real64, 2.0_real64]) .and. &
            all(o2(3, :) == [1.0_real64, 2.0_real64, 3.0_real64]), &
            "the matrix object form: the scaled sum, the size and the group number per group")
        if (allocated(error)) return
        call t%col("payload", red2%p)
        red2%scale = -1.0_real64
        call grp%apply(red2, o3)
        call check(error, all(o3 == -[13.0_real64, 19.0_real64, 13.0_real64]), &
            "a second reducer with its own scale is a second context in the same program")
    end subroutine test_apply_object_equals_procedure

    !> Zero groups -- an empty table, and a table whose every key is null under `dropna` -- give
    !! zero-length outputs in every form, allocated, and the callback is never called.
    subroutine test_apply_empty_grouping(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, u
        type(parquet_grouping) :: grp
        type(scaled_sum_reducer) :: red
        integer(int64), allocatable :: none(:)
        real(real64), allocatable :: s(:), m(:, :)
        real(real64) :: k(3)
        !
        allocate(none(0))
        call parquet_new_table(t)
        call t%add_column("k", none)
        call t%group_by("k", grp)
        calls_seen = 0
        call grp%apply(cb_count_call, s)
        call check(error, allocated(s) .and. size(s) == 0, "the one-value procedure form: allocated, zero-length")
        if (allocated(error)) return
        call grp%apply(cb_three, 3, m)
        call check(error, allocated(m) .and. size(m, 1) == 3 .and. size(m, 2) == 0, "the matrix form: (3, 0)")
        if (allocated(error)) return
        call grp%apply(red, s)
        call grp%apply(red, 2, m)
        call check(error, size(s) == 0 .and. size(m, 1) == 2 .and. size(m, 2) == 0, "both object forms too")
        if (allocated(error)) return
        call check(error, calls_seen == 0, "and the procedure was never called")
        if (allocated(error)) return
        ! Every key null: the table has rows, the grouping has none.
        k = 1.0_real64
        call parquet_new_table(u)
        call u%add_column("k", k)
        call u%set_null("k", 1_int64)
        call u%set_null("k", 2_int64)
        call u%set_null("k", 3_int64)
        call u%group_by("k", grp)
        call grp%apply(cb_count_call, s)
        call check(error, grp%ngroups() == 0_int64 .and. size(s) == 0 .and. calls_seen == 0, &
            "an all-null key under dropna: zero groups, zero calls")
    end subroutine test_apply_empty_grouping

    ! ---- the callbacks: module procedures, never internal ones (see the header) ----------------

    !> `scaled_sum_reducer%reduce`; see the type.
    subroutine scaled_sum_reduce(self, g, rows, out)
        class(scaled_sum_reducer), intent(in) :: self !! the reducer.
        integer(int64), intent(in) :: g               !! the group number.
        integer(int64), intent(in) :: rows(:)         !! the group's rows.
        real(real64), intent(out) :: out(:)           !! receives the results.
        out(1) = self%scale * sum(self%p(rows))
        if (size(out) > 1) out(2) = real(size(rows), real64)
        if (size(out) > 2) out(3) = real(g, real64)
    end subroutine scaled_sum_reduce

    !> The payload's sum over the group, through `ctx_sum_p`.
    function cb_sum_ctx(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! the sum.
        r = sum(ctx_sum_p(rows))
        if (g < 1_int64) r = -huge(r)   ! a group number below 1 is a library defect: poison the answer
    end function cb_sum_ctx

    !> 2.5 times the payload's sum over the group, through `ctx_obj_p`: the procedure twin of a
    !! `scaled_sum_reducer` at scale 2.5.
    function cb_scaled_sum_ctx(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! the scaled sum.
        r = 2.5_real64 * sum(ctx_obj_p(rows))
        if (g < 1_int64) r = -huge(r)
    end function cb_scaled_sum_ctx

    !> The sum of the group's row numbers: row-distinct, with no context at all.
    function cb_row_sum(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! the sum.
        r = real(sum(rows), real64)
        if (g < 1_int64) r = -huge(r)
    end function cb_row_sum

    !> The group's size.
    function cb_size(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! how many rows.
        r = real(size(rows), real64)
        if (g < 1_int64) r = -huge(r)
    end function cb_size

    !> A thousand times the group number, plus the group's lowest row.
    function cb_g_and_first(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! 1000 g + minval(rows).
        r = real(g, real64) * 1000.0_real64 + real(minval(rows), real64)
    end function cb_g_and_first

    !> The matrix twin of `cb_row_sum`, `cb_size` and `cb_g_and_first`, in that order.
    subroutine cb_three(g, rows, out)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64), intent(out) :: out(:)   !! receives the three values.
        out(1) = real(sum(rows), real64)
        out(2) = real(size(rows), real64)
        out(3) = real(g, real64) * 1000.0_real64 + real(minval(rows), real64)
    end subroutine cb_three

    !> Records the call: how often group `g` was seen, whether the calls came in group order,
    !! and whether `rows` was non-empty and ascending; answers `g`.
    function cb_visit(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! g.
        integer(int64) :: i
        visits(g) = visits(g) + 1
        if (g /= last_g + 1_int64) order_ok = .false.
        last_g = g
        if (size(rows) < 1) rows_ok = .false.
        do i = 2_int64, size(rows, kind=int64)
            if (rows(i) <= rows(i - 1_int64)) rows_ok = .false.
        end do
        r = real(g, real64)
    end function cb_visit

    !> Counts its calls; answers 0.
    function cb_count_call(g, rows) result(r)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64) :: r                     !! 0.
        calls_seen = calls_seen + 1
        r = 0.0_real64
        if (g < 1_int64 .or. size(rows) < 1) r = -huge(r)
    end function cb_count_call

end module test_table_group
