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
!! **`%agg`'s oracle is the named `pf_*` procedure over `%get_slice` of each group's rows**,
!! compared with `==` on the bits (a NaN against a NaN counts as equal), for every token of the
!! real64 vocabulary on the real64 column and for a few tokens on the typed slices of the
!! int32, real32 and logical columns -- the statistics module widens those to real64 itself, and
!! the typed oracle pins that the grouping's widening is the same. The exact int64 family is
!! checked against int64 arithmetic over `%csr` on values above 2**53, where the real64 form is
!! shown to differ. `%nunique`'s oracle is `pf_unique_count` per gathered group.
!!
!! **`%broadcast`'s oracle is the lookup through `%group_ids`** (`per_row(i)` is
!! `per_group(codes(i))`, `fill` where the code is 0) **and `%gather`'s is `%get_slice` over
!! the group's rows**, for each of the five buffer kinds, the typed slice widened by hand where
!! the buffer is wider than the column; a canary past `n` pins that the buffer's tail is left
!! alone.
!!
!! The ABORTS -- no key, an unknown or unorderable key, a direction token, every per-group
!! query on a stale or never-built grouping, a group number out of range, `%apply` with `nout`
!! or `threads` below 1, `%agg`'s refusals (an unknown token, an option a token does not take,
!! the exact family on a real column, an overflowing exact sum, a group with no exact answer,
!! a string column, bad weights), `%gather` with a short buffer or a kind its buffer cannot take,
!! `%broadcast` with `per_group` of the wrong length -- live in test/error_scenarios.f90 as
!! `table_group_*`. Every
!! test that writes a file uses its own path, since the suite runs its tests concurrently.
module test_table_group
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
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
    integer :: col_calls = 0                        !! test_agg_callback_presence ALONE: calls made.
    integer :: col_valid_present = 0                !! ... calls that saw is_valid present.
    integer :: col_weights_present = 0              !! ... calls that saw weights present.
    integer :: empty_col_calls = 0                  !! test_add_forms_on_empty_grouping: calls made.
    real(real64), pointer :: ctx_add_p(:) => null() !! test_add_apply_names_and_forms' payload.

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

    !> `test_add_apply_names_and_forms`' second reducer, the object twin of `cb_add_nan_small`:
    !! `out(1)` is the payload's sum over the group and `out(2)` a NaN for a group of fewer than
    !! three rows -- the "could not compute" answer, so that the Null rule is asserted on the
    !! OBJECT form as well as the procedure one.
    type, extends(parquet_group_reducer) :: nan_small_reducer
        real(real64), pointer :: p(:) => null() !! the payload column.
    contains
        procedure :: reduce => nan_small_reduce !! See the type.
    end type nan_small_reducer

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
            new_unittest("key_table reserve= leaves room for that many further adds, relocating none", &
                test_key_table_reserve), &
            new_unittest("nkeys answers a default integer, before a build and after one", &
                test_nkeys_is_default_integer), &
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
            new_unittest("the five questions about the object itself answer on a STALE grouping", &
                test_stale_object_questions_answer), &
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
                test_apply_empty_grouping), &
            new_unittest("agg: every real64 token equals its pf_* oracle over the group's slice, bit for bit", &
                test_agg_tokens_equal_pf_oracles), &
            new_unittest("agg: the typed slices of int32, real32 and logical columns give the same bits", &
                test_agg_typed_kinds_equal_oracles), &
            new_unittest("agg: the exact int64 family is exact above 2**53, where the real64 form is not", &
                test_agg_int64_family_exact), &
            new_unittest("agg: weights= equals the weighted oracle, a zero or null weight drops its row", &
                test_agg_weights_and_weight_column), &
            new_unittest("agg: a procedure sees is_valid only for a group with a null, weights only when given", &
                test_agg_callback_presence), &
            new_unittest("agg: the options reach their statistic, and a logical mean is its fraction", &
                test_agg_options_and_logical_mean), &
            new_unittest("agg: first and last are the first and last non-null value, NaN when there is none", &
                test_agg_first_and_last), &
            new_unittest("nunique equals pf_unique_count per gathered group, under both dropna values", &
                test_nunique_equals_pf_unique_count), &
            new_unittest("agg and nunique over an empty grouping allocate zero-length outputs", &
                test_agg_empty_grouping), &
            new_unittest("add_agg equals agg plus add_column, with the unit and NaN rules on top", &
                test_add_agg_equals_agg_and_add_column), &
            new_unittest("add_agg exact= gives an int64 column equal to the exact family's array", &
                test_add_agg_exact_family), &
            new_unittest("add_agg's procedure form equals agg's, and a NaN it returns is Null", &
                test_add_agg_procedure_form), &
            new_unittest("the table-target forms are reads of the source: order, generation, pointer", &
                test_add_forms_are_reads_of_the_source), &
            new_unittest("add_agg force= replaces values and kind and moves the target's generation", &
                test_add_agg_force_replaces), &
            new_unittest("an empty grouping gives zero-row columns; a column-less target takes the count", &
                test_add_forms_on_empty_grouping), &
            new_unittest("add_size equals size and key_table(size_name=), bit for bit", &
                test_add_size_equals_size_and_size_name), &
            new_unittest("add_apply gives one column per name in as=, in both forms", &
                test_add_apply_names_and_forms), &
            new_unittest("add_apply adds exactly as many columns as as= names", &
                test_add_apply_validates_before_writing), &
            new_unittest("broadcast equals the lookup through group_ids; a dropped row takes fill, in both kinds", &
                test_broadcast_equals_group_ids), &
            new_unittest("gather equals get_slice over the group's rows for five kinds, widening as get does", &
                test_gather_equals_get_slice), &
            new_unittest("broadcast over an empty grouping is all fill, zero-length over an empty table", &
                test_broadcast_empty_grouping), &
            new_unittest("every int32 form answers what its int64 sibling answers", &
                test_int32_forms_equal_int64) &
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

    !> Twelve rows over an int32 key with four groups of unequal size -- 1 at rows 1, 3, 6, 8,
    !! 10; 2 at rows 2, 5, 9; 3 at rows 4, 7, 12; 4 at row 11 alone (the `std` NaN case) -- and
    !! a row-distinct column of every kind `%agg` takes, each with a null somewhere: `x`
    !! (real64, a NaN at row 6 and a null at row 8, both in group 1, and a null at row 12), `i`
    !! (int32, null at row 3), `big` (int64, values above 2**53 whose group sums are exact only
    !! in int64; null at row 7), `f` (real32, null at row 9), `b` (logical, null at row 5), `w`
    !! (real64 weights, a zero at row 3 and a null at row 10), `z` (real64, null throughout
    !! groups 3 and 4) and `s` (string, null at row 12). `x` carries the unit `"mag"` and `big`
    !! the unit `"ct"`, which is what `%add_agg`'s unit rule inherits from (nothing else here
    !! reads a unit).
    subroutine build_agg(t)
        type(parquet_table), intent(out) :: t !! the table.
        integer(int32) :: k(12), i(12)
        integer(int64) :: big(12)
        real(real64) :: x(12), w(12), z(12)
        real(real32) :: f(12)
        logical :: b(12)
        character(len=1) :: s(12)
        integer :: r
        !
        k = [1, 2, 1, 3, 2, 1, 3, 1, 2, 1, 4, 3]
        x = [1.5_real64, 2.5_real64, -3.0_real64, 4.25_real64, 5.5_real64, 6.0_real64, 7.75_real64, 8.0_real64, &
            9.5_real64, 10.0_real64, 11.0_real64, 12.5_real64]
        x(6) = ieee_value(0.0_real64, ieee_quiet_nan)
        do r = 1, 12
            i(r) = int(10 * r, int32)
            f(r) = real(r, real32) + 0.25_real32
        end do
        big = [2_int64**53 + 1_int64, 2_int64**60, 2_int64**53 + 3_int64, 7_int64, 2_int64**61, &
            2_int64**53 + 5_int64, 8_int64, -(2_int64**53) - 1_int64, 2_int64**61, 3_int64, 2_int64**53, 9_int64]
        b = [.true., .false., .true., .true., .false., .true., .false., .false., .true., .true., .false., .true.]
        w = [1.0_real64, 2.0_real64, 0.0_real64, 1.5_real64, 1.0_real64, 2.0_real64, 1.0_real64, 1.0_real64, &
            3.0_real64, 0.5_real64, 1.0_real64, 2.0_real64]
        z = x
        s = ["a", "b", "a", "c", "b", "a", "c", "a", "d", "a", "e", "c"]
        call parquet_new_table(t)
        call t%add_column("k", k)
        call t%add_column("x", x, unit="mag")
        call t%add_column("i", i)
        call t%add_column("big", big, unit="ct")
        call t%add_column("f", f)
        call t%add_column("b", b)
        call t%add_column("w", w)
        call t%add_column("z", z)
        call t%add_column("s", s)
        call t%set_null("x", 8_int64)
        call t%set_null("x", 12_int64)
        call t%set_null("i", 3_int64)
        call t%set_null("big", 7_int64)
        call t%set_null("f", 9_int64)
        call t%set_null("b", 5_int64)
        call t%set_null("w", 10_int64)
        do r = 1, 12
            if (k(r) == 3 .or. k(r) == 4) call t%set_null("z", int(r, int64))
        end do
        call t%set_null("s", 12_int64)
    end subroutine build_agg

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

    !> `%key_table(reserve=)` is an INCREMENT on the columns the call itself creates, and what
    !! it buys is `%reserve_columns`' published guarantee without the caller counting the keys:
    !! after `reserve=10` on a two-key grouping with `size_name=`, ten further adds under NEW
    !! names relocate nothing -- a `%col` pointer taken before them still reads the key column
    !! and `%generation()` has not moved. The negative control is the same key table built
    !! WITHOUT `reserve=`, where `parquet_new_table`'s headroom carries five adds and the ninth
    !! column is the one that grows the slot array.
    subroutine test_key_table_reserve(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt, kt2
        type(parquet_grouping) :: grp
        integer(int32), pointer :: p(:)
        integer(int32), allocatable :: k0(:)
        real(real64), allocatable :: v(:)
        integer(int64) :: gen0
        character(len=8) :: nm
        integer :: i
        !
        call build_kinds(t)
        call t%group_by(["k", "s"], grp)
        call grp%key_table(kt, size_name="n", reserve=10)
        call check(error, kt%ncols() == 3, "the call itself creates the two keys and the count column")
        if (allocated(error)) return
        call check(error, kt%column_capacity(free=.true.) >= 10, &
            "and reserve= is an increment on top of those: ten slots are spare")
        if (allocated(error)) return
        call kt%get("k", k0)
        call kt%col("k", p)
        gen0 = kt%generation()
        allocate(v(kt%nrows()))
        v = 1.0_real64
        do i = 1, 10
            write(nm, "(a,i0)") "c", i
            call kt%add_column(trim(nm), v)
        end do
        call check(error, kt%ncols() == 13, "ten more columns went on")
        if (allocated(error)) return
        call check(error, kt%generation() == gen0, "...without advancing %generation()")
        if (allocated(error)) return
        call check(error, associated(p), "the %col pointer taken before them is still associated")
        if (allocated(error)) return
        call check(error, size(p) == size(k0), "...at the same length")
        if (allocated(error)) return
        call check(error, all(p == k0), "...and still reads the key column's own values")
        if (allocated(error)) return
        call grp%key_table(kt2, size_name="n")
        gen0 = kt2%generation()
        do i = 1, 5
            write(nm, "(a,i0)") "c", i
            call kt2%add_column(trim(nm), v)
        end do
        call check(error, kt2%ncols() == 8 .and. kt2%generation() == gen0, &
            "without reserve= the default headroom carries five adds")
        if (allocated(error)) return
        call kt2%add_column("c6", v)
        call check(error, kt2%generation() /= gen0, "and the ninth column is the one that grows the slot array")
    end subroutine test_key_table_reserve

    !> `%nkeys()` answers a DEFAULT `integer`, as `t%ncols()` does: a key count is a column
    !! count. `kind()` is an inquiry and never calls the procedure, so the value is asserted
    !! beside the kind (.claude/rules/testing.md).
    subroutine test_nkeys_is_default_integer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer :: nk
        !
        call check(error, kind(grp%nkeys()) == kind(0), "%nkeys() is a default integer")
        if (allocated(error)) return
        nk = grp%nkeys()
        call check(error, nk == 0, "and it answers 0 before a build")
        if (allocated(error)) return
        call build_kinds(t)
        call t%group_by(["k", "s"], grp)
        nk = grp%nkeys()
        call check(error, nk == 2, "a two-key grouping has two keys")
    end subroutine test_nkeys_is_default_integer

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
        integer(int64), allocatable :: nu(:), exact(:)
        integer(int32), allocatable :: key(:)
        real(real64), allocatable :: payload(:), means(:), per_row(:)
        real(real64) :: gbuf(9)
        integer(int64) :: gn
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
        ! The verbs that sort, gather or scatter of their own: %nunique runs a second sort over a
        ! non-owning handle on this very table, and a gather on the source column rather than a
        ! copy would reorder it with nothing to show for it (feature_risks.md).
        call grp%nunique("payload", nu)
        call grp%agg("payload", "mean", means)
        call grp%agg("key", "sum", exact)
        call grp%broadcast(means, per_row)
        call grp%gather("payload", 1_int64, gbuf, gn)
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

    !> **A claimed ABSENCE**: `doc/pages/tables/table-group.md` ("When a grouping goes stale") says
    !! the five questions about the object itself -- `%ngroups`, `%nrows`, `%max_size`, `%nkeys`
    !! and `%key_names` -- "answer without the check", i.e. on a grouping the table has outrun.
    !! Nothing pinned that: `test_rebuild_clear_and_threads` covers four of the five after a
    !! `%clear()`, which is the NEVER-BUILT half, and `%max_size` was in no test at all. Adding a
    !! `grp_resolve` call to any of the five would turn a documented non-abort into an abort and
    !! break no other test in the suite.
    !!
    !! The values asserted are the ones the grouping held BEFORE the table changed, which is the
    !! second half of the claim: a stale grouping answers about the partition it built, not about
    !! the table as it now stands. Reading them first, on the current grouping, is the negative
    !! control -- without it the test would pass against five queries that all returned zero.
    subroutine test_stale_object_questions_answer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        character(len=:), allocatable :: names0(:), names1(:)
        integer(int64) :: ng0, nr0, mx0, nk0
        logical :: keep(9)
        !
        call build_basic(t)                       ! keys 7,3,7,3,9,7,3,9,7 -- groups of 4, 3 and 2
        call t%group_by(["key"], grp)
        ! The negative control: the same five on a CURRENT grouping, with their values named.
        ng0 = grp%ngroups()
        nr0 = grp%nrows()
        mx0 = grp%max_size()
        nk0 = grp%nkeys()
        call grp%key_names(names0)
        call check(error, ng0 == 3_int64 .and. nr0 == 9_int64 .and. mx0 == 4_int64 .and. nk0 == 1_int64, &
            "the five answer on a current grouping: 3 groups, 9 rows, largest 4, one key")
        if (allocated(error)) return
        call check(error, size(names0) == 1 .and. trim(names0(1)) == "key", "and name the key column")
        if (allocated(error)) return
        !
        ! Stale it the way test_stale_and_current does: a row-structural change.
        keep = .true.
        keep(2) = .false.
        call t%filter_rows(keep)
        call check(error, .not. grp%is_current(), "the grouping is stale")
        if (allocated(error)) return
        !
        ! The claim: all five still answer, with the values they held. No abort, no rebuild.
        call check(error, grp%ngroups() == ng0, "%ngroups answers on a stale grouping")
        if (allocated(error)) return
        call check(error, grp%nrows() == nr0, "%nrows answers on a stale grouping")
        if (allocated(error)) return
        call check(error, grp%max_size() == mx0, "%max_size answers on a stale grouping")
        if (allocated(error)) return
        call check(error, grp%nkeys() == nk0, "%nkeys answers on a stale grouping")
        if (allocated(error)) return
        call grp%key_names(names1)
        call check(error, size(names1) == size(names0), "%key_names answers on a stale grouping")
        if (allocated(error)) return
        call check(error, trim(names1(1)) == trim(names0(1)), "and names the same key column")
        if (allocated(error)) return
        ! The table really did move underneath it: the grouping's 9 rows are no longer the table's.
        call check(error, t%nrows() == 8_int64 .and. grp%nrows() == 9_int64, &
            "a stale grouping answers about the partition it built, not about the table now")
    end subroutine test_stale_object_questions_answer

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

    ! ---- %agg and %nunique ----------------------------------------------------------------------
    !
    ! `build_agg`'s largest group is its FIRST, so nothing here sees `%agg` buffers sized from the
    ! first group rather than `%max_size()`; only `test_agg_group_team` (test_table_parallel.f90) can.
    ! A new group-buffer fixture in this file puts its largest group somewhere other than first.

    !> Whether two real64 values are the same answer: equal bits, or both NaN.
    logical function same_answer(a, b)
        real(real64), intent(in) :: a, b !! the two values.
        if (ieee_is_nan(a) .and. ieee_is_nan(b)) then
            same_answer = .true.
        else
            same_answer = a == b
        end if
    end function same_answer

    !> `same_answer` over two buffers of equal length: the element-wise comparison the gather
    !! arms need, since a gathered column may hold a NaN.
    function same_answer_v(a, b) result(same)
        real(real64), intent(in) :: a(:)    !! one buffer.
        real(real64), intent(in) :: b(:)    !! the other, of the same length.
        logical :: same(size(a))            !! element by element: equal bits, or both NaN.
        integer :: i
        do i = 1, size(a)
            same(i) = same_answer(a(i), b(i))
        end do
    end function same_answer_v

    !> The oracle of one real64 token over one group's real64 values: the named `pf_*`
    !! procedure, `is_valid` given only when the group holds a null (as the grouping passes it)
    !! and `weights` when given.
    subroutine oracle_real(stat, v, valid, r, q, weights, ddof, method, scale)
        character(len=*), intent(in) :: stat             !! the token.
        real(real64), intent(in) :: v(:)                 !! the group's values.
        logical, intent(in) :: valid(:)                  !! their validity.
        real(real64), intent(out) :: r                   !! the oracle's answer.
        real(real64), intent(in), optional :: q          !! the quantile's probability.
        real(real64), intent(in), optional :: weights(:) !! the group's weights.
        integer, intent(in), optional :: ddof            !! degrees of freedom.
        character(len=*), intent(in), optional :: method !! the quantile method.
        character(len=*), intent(in), optional :: scale  !! the mad's scale.
        if (all(valid)) then
            call oracle_real_core(stat, v, r, q=q, weights=weights, ddof=ddof, method=method, scale=scale)
        else
            call oracle_real_core(stat, v, r, is_valid=valid, q=q, weights=weights, ddof=ddof, method=method, &
                scale=scale)
        end if
    end subroutine oracle_real

    !> `oracle_real`'s body, with `is_valid` an optional it passes on as it arrived.
    subroutine oracle_real_core(stat, v, r, is_valid, q, weights, ddof, method, scale)
        character(len=*), intent(in) :: stat             !! the token.
        real(real64), intent(in) :: v(:)                 !! the group's values.
        real(real64), intent(out) :: r                   !! the oracle's answer.
        logical, intent(in), optional :: is_valid(:)     !! their validity, when the group holds a null.
        real(real64), intent(in), optional :: q          !! the quantile's probability.
        real(real64), intent(in), optional :: weights(:) !! the group's weights.
        integer, intent(in), optional :: ddof            !! degrees of freedom.
        character(len=*), intent(in), optional :: method !! the quantile method.
        character(len=*), intent(in), optional :: scale  !! the mad's scale.
        real(real64) :: lo, hi
        integer(int64) :: n
        select case (stat)
        case ("size")
            r = real(size(v), real64)
        case ("count")
            call pf_count_valid(v, n, is_valid=is_valid, weights=weights)
            r = real(n, real64)
        case ("sum")
            call pf_sum(v, r, is_valid=is_valid, weights=weights)
        case ("mean")
            call pf_mean(v, r, is_valid=is_valid, weights=weights)
        case ("var")
            call pf_variance(v, r, is_valid=is_valid, weights=weights, ddof=ddof)
        case ("std")
            call pf_stddev(v, r, is_valid=is_valid, weights=weights, ddof=ddof)
        case ("sem")
            call pf_sem(v, r, is_valid=is_valid, weights=weights, ddof=ddof)
        case ("min")
            call pf_moments(v, vmin=r, is_valid=is_valid, weights=weights)
        case ("max")
            call pf_moments(v, vmax=r, is_valid=is_valid, weights=weights)
        case ("range")
            call pf_moments(v, vmin=lo, vmax=hi, is_valid=is_valid, weights=weights)
            r = hi - lo
        case ("median")
            call pf_median(v, r, is_valid=is_valid, weights=weights, method=method)
        case ("quantile")
            call pf_quantile(v, q, r, is_valid=is_valid, weights=weights, method=method)
        case ("iqr")
            call pf_iqr(v, r, is_valid=is_valid, weights=weights, method=method)
        case ("mad")
            call pf_mad(v, r, is_valid=is_valid, weights=weights, scale=scale)
        case default
            error stop "test_table_group: oracle_real_core: no oracle for " // stat
        end select
    end subroutine oracle_real_core

    !> Every token of the real64 vocabulary over the real64 column `x` -- a NaN and a null in
    !! group 1, a null in group 3, a group of one -- against `oracle_real` over `%get_slice` of
    !! each group's rows, bit for bit; `"nunique"` against `%nunique`. And the call is a read.
    subroutine test_agg_tokens_equal_pf_oracles(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), parameter :: tokens(14) = [character(len=8) :: "size", "count", "sum", "mean", "var", &
            "std", "sem", "min", "max", "range", "median", "quantile", "iqr", "mad"]
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:), nu(:)
        real(real64), allocatable :: got(:), v(:)
        logical, allocatable :: valid(:)
        real(real64) :: want
        integer(int64) :: g, gen0
        integer :: k
        !
        call build_agg(t)
        gen0 = t%generation()
        call t%group_by("k", grp)
        call grp%csr(off, rows)
        do k = 1, size(tokens)
            if (tokens(k) == "quantile") then
                call grp%agg("x", trim(tokens(k)), got, q=0.3_real64)
            else
                call grp%agg("x", trim(tokens(k)), got)
            end if
            if (size(got) /= 4) then
                call check(error, .false., "one answer per group for " // trim(tokens(k)))
                return
            end if
            do g = 1_int64, 4_int64
                call t%get_slice("x", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), v, is_valid=valid)
                call oracle_real(trim(tokens(k)), v, valid, want, q=0.3_real64)
                if (.not. same_answer(got(g), want)) then
                    call check(error, .false., trim(tokens(k)) // " differs from its pf_* oracle in a group")
                    return
                end if
            end do
        end do
        call grp%agg("x", "nunique", got)
        call grp%nunique("x", nu)
        call check(error, all(got == real(nu, real64)) .and. all(nu == [4_int64, 3_int64, 2_int64, 1_int64]), &
            "the nunique token is %nunique widened (group 1: 1.5, -3.0, the NaN and 10.0)")
        if (allocated(error)) return
        call check(error, t%generation() == gen0 .and. t%nrows() == 12_int64 .and. grp%is_current(), &
            "agg is a read")
    end subroutine test_agg_tokens_equal_pf_oracles

    !> The int32, real32 and logical columns against the oracles over their TYPED slices, for
    !! `"mean"`, `"median"` and `"std"`: the statistics module widens those kinds to real64
    !! itself, and the grouping's own widening must give the same bits.
    subroutine test_agg_typed_kinds_equal_oracles(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:)
        real(real64), allocatable :: got(:)
        integer(int32), allocatable :: vi(:)
        real(real32), allocatable :: vf(:)
        logical, allocatable :: vb(:), valid(:)
        character(len=1), parameter :: cols(3) = ["i", "f", "b"]
        character(len=6), parameter :: toks(3) = [character(len=6) :: "mean", "median", "std"]
        real(real64) :: want
        integer(int64) :: g
        integer :: k, c
        character(len=6) :: tok
        character(len=1) :: col
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%csr(off, rows)
        do c = 1, 3
            col = cols(c)
            do k = 1, 3
                tok = toks(k)
                call grp%agg(col, trim(tok), got)
                do g = 1_int64, 4_int64
                    select case (col)
                    case ("i")
                        call t%get_slice("i", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vi, &
                            is_valid=valid)
                        call typed_oracle_i32(trim(tok), vi, valid, want)
                    case ("f")
                        call t%get_slice("f", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vf, &
                            is_valid=valid)
                        call typed_oracle_f32(trim(tok), vf, valid, want)
                    case default
                        call t%get_slice("b", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vb, &
                            is_valid=valid)
                        call typed_oracle_bool(trim(tok), vb, valid, want)
                    end select
                    if (.not. same_answer(got(g), want)) then
                        call check(error, .false., trim(tok) // " over column " // col // " differs from the typed oracle")
                        return
                    end if
                end do
            end do
        end do
        call check(error, .true., "every typed oracle agreed")
    end subroutine test_agg_typed_kinds_equal_oracles

    !> `oracle_real` over an int32 slice, through the statistics module's own int32 specifics.
    subroutine typed_oracle_i32(stat, v, valid, r)
        character(len=*), intent(in) :: stat  !! the token.
        integer(int32), intent(in) :: v(:)    !! the group's values.
        logical, intent(in) :: valid(:)       !! their validity.
        real(real64), intent(out) :: r        !! the oracle's answer.
        if (all(valid)) then
            select case (stat)
            case ("mean");   call pf_mean(v, r)
            case ("median"); call pf_median(v, r)
            case default;    call pf_stddev(v, r)
            end select
        else
            select case (stat)
            case ("mean");   call pf_mean(v, r, is_valid=valid)
            case ("median"); call pf_median(v, r, is_valid=valid)
            case default;    call pf_stddev(v, r, is_valid=valid)
            end select
        end if
    end subroutine typed_oracle_i32

    !> `oracle_real` over a real32 slice, through the real32 specifics.
    subroutine typed_oracle_f32(stat, v, valid, r)
        character(len=*), intent(in) :: stat  !! the token.
        real(real32), intent(in) :: v(:)      !! the group's values.
        logical, intent(in) :: valid(:)       !! their validity.
        real(real64), intent(out) :: r        !! the oracle's answer.
        if (all(valid)) then
            select case (stat)
            case ("mean");   call pf_mean(v, r)
            case ("median"); call pf_median(v, r)
            case default;    call pf_stddev(v, r)
            end select
        else
            select case (stat)
            case ("mean");   call pf_mean(v, r, is_valid=valid)
            case ("median"); call pf_median(v, r, is_valid=valid)
            case default;    call pf_stddev(v, r, is_valid=valid)
            end select
        end if
    end subroutine typed_oracle_f32

    !> `oracle_real` over a logical slice, through the logical specifics (0 and 1).
    subroutine typed_oracle_bool(stat, v, valid, r)
        character(len=*), intent(in) :: stat  !! the token.
        logical, intent(in) :: v(:)           !! the group's values.
        logical, intent(in) :: valid(:)       !! their validity.
        real(real64), intent(out) :: r        !! the oracle's answer.
        if (all(valid)) then
            select case (stat)
            case ("mean");   call pf_mean(v, r)
            case ("median"); call pf_median(v, r)
            case default;    call pf_stddev(v, r)
            end select
        else
            select case (stat)
            case ("mean");   call pf_mean(v, r, is_valid=valid)
            case ("median"); call pf_median(v, r, is_valid=valid)
            case default;    call pf_stddev(v, r, is_valid=valid)
            end select
        end if
    end subroutine typed_oracle_bool

    !> The exact family on `big`, whose group-1 values straddle 2**53: every token against
    !! int64 arithmetic over `%csr` with the null skipped, and the real64 `"sum"` of the same
    !! group shown to differ from the exact one -- which is what the family exists for. Also the
    !! family on a logical column, counting .true. as 1.
    subroutine test_agg_int64_family_exact(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:), got(:), nu(:), cnt(:), sz(:), vals(:)
        real(real64), allocatable :: approx(:)
        logical, allocatable :: valid(:)
        character(len=5), parameter :: toks(5) = [character(len=5) :: "sum", "min", "max", "first", "last"]
        integer(int64) :: g, want(4), s, lo, hi, first, last, r
        logical :: seen
        integer :: k
        character(len=5) :: tok
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%csr(off, rows)
        do k = 1, 5
            tok = toks(k)
            call grp%agg("big", trim(tok), got)
            do g = 1_int64, 4_int64
                call t%get_slice("big", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vals, &
                    is_valid=valid)
                s = 0_int64
                seen = .false.
                do r = 1_int64, size(vals, kind=int64)
                    if (.not. valid(r)) cycle
                    s = s + vals(r)
                    if (.not. seen) then
                        lo = vals(r); hi = vals(r); first = vals(r)
                    end if
                    lo = min(lo, vals(r))
                    hi = max(hi, vals(r))
                    last = vals(r)
                    seen = .true.
                end do
                select case (tok)
                case ("sum");   want(g) = s
                case ("min");   want(g) = lo
                case ("max");   want(g) = hi
                case ("first"); want(g) = first
                case default;   want(g) = last
                end select
            end do
            if (.not. all(got == want)) then
                call check(error, .false., "exact " // trim(tok) // " differs from int64 arithmetic over csr")
                return
            end if
        end do
        call grp%agg("big", "sum", got)
        call check(error, got(1) == 2_int64**54 + 11_int64 .and. got(2) == 2_int64**60 + 2_int64**62, &
            "the exact sums of groups 1 and 2 are the fixture's")
        if (allocated(error)) return
        call grp%agg("big", "sum", approx)
        call check(error, nint(approx(1), int64) /= got(1), &
            "the real64 sum of group 1 is not the exact one: its values do not fit a real64")
        if (allocated(error)) return
        call grp%agg("big", "count", cnt)
        call grp%agg("big", "size", sz)
        call grp%agg("big", "nunique", nu)
        call check(error, all(cnt == [5_int64, 3_int64, 2_int64, 1_int64]) .and. all(sz == [5_int64, 3_int64, &
            3_int64, 1_int64]) .and. all(nu == [5_int64, 2_int64, 2_int64, 1_int64]), &
            "count, size and nunique of the exact family")
        if (allocated(error)) return
        call grp%agg("b", "sum", got)
        call check(error, all(got == [4_int64, 1_int64, 2_int64, 0_int64]), "a logical column sums its trues")
    end subroutine test_agg_int64_family_exact

    !> `weights=` against the weighted oracle over each group's gathered weights, bit for bit;
    !! the zero weight at row 3 drops that row from the population (`"count"` says so);
    !! `weight_column=` equals `weights=` given the column's values with its null read as zero,
    !! so the null weight at row 10 drops its row too; and the procedure form takes both.
    subroutine test_agg_weights_and_weight_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:)
        real(real64), allocatable :: w(:), got(:), viacol(:), cnt(:), v(:), wg(:), fcol(:), farr(:)
        logical, allocatable :: valid(:), wvalid(:)
        real(real64) :: want
        integer(int64) :: g
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%csr(off, rows)
        call t%get("w", w, is_valid=wvalid)
        where (.not. wvalid) w = 0.0_real64
        call grp%agg("x", "mean", got, weights=w)
        do g = 1_int64, 4_int64
            call t%get_slice("x", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), v, is_valid=valid)
            wg = w(rows(off(g) : off(g + 1_int64) - 1_int64))
            call oracle_real("mean", v, valid, want, weights=wg)
            if (.not. same_answer(got(g), want)) then
                call check(error, .false., "the weighted mean differs from pf_mean with the gathered weights")
                return
            end if
        end do
        call grp%agg("x", "count", cnt, weights=w)
        ! Group 1 holds rows 1, 3, 6, 8, 10: row 3's weight is zero, row 6 is NaN, row 8 is null
        ! and row 10's weight is null (zero): one row is left. Group 2's three rows all count.
        call check(error, cnt(1) == 1.0_real64 .and. cnt(2) == 3.0_real64, &
            "a zero weight and a null weight both drop their row from the population")
        if (allocated(error)) return
        call grp%agg("x", "mean", viacol, weight_column="w")
        call check(error, all(got == viacol .or. (ieee_is_nan(got) .and. ieee_is_nan(viacol))), &
            "weight_column= equals weights= over the column's values with the null as zero")
        if (allocated(error)) return
        call grp%agg("x", cb_col_weighted_sum, fcol, weight_column="w")
        call grp%agg("x", cb_col_weighted_sum, farr, weights=w)
        call check(error, all(fcol == farr .or. (ieee_is_nan(fcol) .and. ieee_is_nan(farr))), &
            "the procedure form takes weight_column= and weights= alike (group 1's NaN value makes both NaN)")
        if (allocated(error)) return
        ! A weight column is any of the four scalar numeric kinds, each widened to real64 the way
        ! the values are. The int32 and real64 arms are exercised above; these are the other two,
        ! against the same weights written as real64.
        call t%add_column("w64", int(w, int64))
        call t%add_column("w32", real(int(w, int64), real32))
        call t%add_column("wr8", real(int(w, int64), real64))
        call t%group_by("k", grp)
        call grp%agg("x", "mean", got, weight_column="wr8")
        call grp%agg("x", "mean", viacol, weight_column="w64")
        call check(error, all(got == viacol .or. (ieee_is_nan(got) .and. ieee_is_nan(viacol))), &
            "an int64 weight column weighs exactly as the same values in real64")
        if (allocated(error)) return
        call grp%agg("x", "mean", viacol, weight_column="w32")
        call check(error, all(got == viacol .or. (ieee_is_nan(got) .and. ieee_is_nan(viacol))), &
            "and so does a real32 one")
    end subroutine test_agg_weights_and_weight_column

    !> The procedure form's presence contract: `is_valid` arrives exactly for the groups that
    !! hold a null, `weights` exactly when weights were given -- counted by the procedure -- and
    !! the values it sees are the widened ones: its median equals the `"median"` token's.
    subroutine test_agg_callback_presence(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: cnt(:), sz(:)
        real(real64), allocatable :: got(:), want(:), w(:)
        integer :: groups_with_null
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%count("x", cnt)
        call grp%size(sz)
        groups_with_null = count(cnt < sz)
        col_calls = 0
        col_valid_present = 0
        col_weights_present = 0
        call grp%agg("x", cb_col_median, got)
        call grp%agg("x", "median", want)
        call check(error, col_calls == 4 .and. col_valid_present == groups_with_null .and. col_weights_present == 0, &
            "called once per group, is_valid present for the null-holding groups only, no weights")
        if (allocated(error)) return
        call check(error, all(got == want .or. (ieee_is_nan(got) .and. ieee_is_nan(want))), &
            "the procedure's median equals the token's: it saw the same widened values")
        if (allocated(error)) return
        allocate(w(12))
        w = 1.0_real64
        col_calls = 0
        col_weights_present = 0
        call grp%agg("i", cb_col_median, got, weights=w)
        call check(error, col_calls == 4 .and. col_weights_present == 4, "weights present in every call when given")
    end subroutine test_agg_callback_presence

    !> The options reach their statistic: `q=` and `method=` the quantile, `ddof=` the
    !! variance, `scale=` the mad, `"median"` without `q=` as the control; a logical column's
    !! `"mean"` is its fraction of .true.
    subroutine test_agg_options_and_logical_mean(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:)
        real(real64), allocatable :: got(:), v(:)
        logical, allocatable :: valid(:), vb(:)
        real(real64) :: want
        integer(int64) :: g
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%csr(off, rows)
        call grp%agg("x", "quantile", got, q=0.8_real64, method="inverted_cdf")
        do g = 1_int64, 4_int64
            call t%get_slice("x", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), v, is_valid=valid)
            call oracle_real("quantile", v, valid, want, q=0.8_real64, method="inverted_cdf")
            if (.not. same_answer(got(g), want)) then
                call check(error, .false., "q= and method= did not reach pf_quantile")
                return
            end if
        end do
        call grp%agg("x", "var", got, ddof=0)
        do g = 1_int64, 4_int64
            call t%get_slice("x", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), v, is_valid=valid)
            call oracle_real("var", v, valid, want, ddof=0)
            if (.not. same_answer(got(g), want)) then
                call check(error, .false., "ddof= did not reach pf_variance")
                return
            end if
        end do
        call grp%agg("x", "mad", got, scale="raw")
        do g = 1_int64, 4_int64
            call t%get_slice("x", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), v, is_valid=valid)
            call oracle_real("mad", v, valid, want, scale="raw")
            if (.not. same_answer(got(g), want)) then
                call check(error, .false., "scale= did not reach pf_mad")
                return
            end if
        end do
        call grp%agg("x", "median", got)
        call check(error, size(got) == 4, "median without q= is the control: it answers")
        if (allocated(error)) return
        call grp%agg("b", "mean", got)
        do g = 1_int64, 4_int64
            call t%get_slice("b", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vb, is_valid=valid)
            want = real(count(vb .and. valid), real64) / real(count(valid), real64)
            if (.not. same_answer(got(g), want)) then
                call check(error, .false., "a logical mean is not its fraction of .true.")
                return
            end if
        end do
        call check(error, got(1) == 0.8_real64 .and. got(2) == 0.5_real64, "and the fixture's fractions are as built")
    end subroutine test_agg_options_and_logical_mean

    !> `"first"` and `"last"` are the first and last NON-NULL value in row order -- pandas'
    !! rule, not the value at the group's lowest and highest row -- and NaN for a group with no
    !! non-null value (`z` is null throughout groups 3 and 4). A NaN VALUE is a value: group 1's
    !! last non-null `x` is at row 10, its first the 1.5 at row 1.
    subroutine test_agg_first_and_last(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        real(real64), allocatable :: first(:), last(:)
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%agg("x", "first", first)
        call grp%agg("x", "last", last)
        call check(error, first(1) == 1.5_real64 .and. last(1) == 10.0_real64, "group 1: first 1.5, last 10.0")
        if (allocated(error)) return
        call check(error, first(3) == 4.25_real64 .and. last(3) == 7.75_real64, &
            "group 3: the null at row 12 is skipped, so last is row 7's value")
        if (allocated(error)) return
        call grp%agg("z", "first", first)
        call grp%agg("z", "last", last)
        call check(error, ieee_is_nan(first(3)) .and. ieee_is_nan(last(3)) .and. ieee_is_nan(first(4)) .and. &
            first(1) == 1.5_real64, "an all-null group answers NaN, a group with values its value")
    end subroutine test_agg_first_and_last

    !> `%nunique` against `pf_unique_count` over each group's gathered values, under both
    !! `dropna` values, for a real64 column with a NaN and nulls, an int32 column, a string
    !! column and the logical column; and of the key column itself, which is one value per group.
    subroutine test_nunique_equals_pf_unique_count(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: off(:), rows(:), got(:), keep(:)
        integer(int32), allocatable :: got32(:)
        real(real64), allocatable :: vx(:)
        integer(int32), allocatable :: vi(:)
        character(len=:), allocatable :: vs(:)
        logical, allocatable :: vb(:), valid(:)
        character(len=1), parameter :: cols(4) = ["x", "i", "s", "b"]
        integer(int64) :: g, want, n_null
        integer :: c, d
        logical :: drop
        character(len=1) :: col
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%csr(off, rows)
        do c = 1, 4
            col = cols(c)
            do d = 1, 2
                drop = d == 1
                call grp%nunique(col, got, dropna=drop)
                do g = 1_int64, 4_int64
                    select case (col)
                    case ("x")
                        call t%get_slice("x", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vx, &
                            is_valid=valid)
                        call pf_unique_count(vx, want, is_valid=valid, n_null=n_null)
                    case ("i")
                        call t%get_slice("i", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vi, &
                            is_valid=valid)
                        call pf_unique_count(vi, want, is_valid=valid, n_null=n_null)
                    case ("s")
                        call t%get_slice("s", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vs, &
                            is_valid=valid)
                        call pf_unique_count(vs, want, is_valid=valid, n_null=n_null)
                    case default
                        call t%get_slice("b", parquet_slice_list(rows(off(g) : off(g + 1_int64) - 1_int64)), vb, &
                            is_valid=valid)
                        call pf_unique_count(vb, want, is_valid=valid, n_null=n_null)
                    end select
                    if (.not. drop .and. n_null > 0_int64) want = want + 1_int64
                    if (got(g) /= want) then
                        call check(error, .false., "nunique of column " // col // " differs from pf_unique_count")
                        return
                    end if
                end do
            end do
        end do
        call grp%nunique("x", got)
        call grp%nunique("x", keep, dropna=.false.)
        call check(error, all(got == [4_int64, 3_int64, 2_int64, 1_int64]) .and. all(keep == [5_int64, 3_int64, &
            3_int64, 1_int64]), "x: the NaN is one value, the null one more only under dropna=.false.")
        if (allocated(error)) return
        call grp%nunique("k", got32)
        call check(error, all(got32 == 1_int32), "the key column has one value per group")
    end subroutine test_nunique_equals_pf_unique_count

    !> Zero groups: every `%agg` form and `%nunique` allocate zero-length outputs.
    subroutine test_agg_empty_grouping(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: none(:), gi(:), nu(:)
        real(real64), allocatable :: gr(:), gf(:)
        !
        allocate(none(0))
        call parquet_new_table(t)
        call t%add_column("k", none)
        call t%add_column("v", none)
        call t%group_by("k", grp)
        call grp%agg("v", "mean", gr)
        call grp%agg("v", "sum", gi)
        call grp%agg("v", cb_col_plain_median, gf)
        call grp%nunique("v", nu)
        call check(error, allocated(gr) .and. allocated(gi) .and. allocated(gf) .and. allocated(nu) .and. &
            size(gr) == 0 .and. size(gi) == 0 .and. size(gf) == 0 .and. size(nu) == 0, &
            "every output is allocated at zero length")
    end subroutine test_agg_empty_grouping

    ! ---- %add_agg and %add_size ------------------------------------------------------------------

    !> **The wrapper computes nothing**: every real64 token, with the options it takes, written
    !! onto the key table by `%add_agg` must equal the array `%agg` fills, bit for bit, with the
    !! NaN rows marked Null instead. The oracle is `%agg` itself, which is the composition this
    !! call replaces; `%is_null` and `%get(..., is_valid=)` must agree with each other and with
    !! where `%agg`'s array is NaN, the kind must be `PK_FLOAT64`, and the unit must be the source
    !! column's for the statistics that keep its dimension and absent for the counts and `"var"`,
    !! with `unit=` overriding either way.
    subroutine test_add_agg_equals_agg_and_add_column(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), parameter :: tokens(15) = [character(len=8) :: "size", "count", "nunique", "sum", &
            "mean", "var", "std", "sem", "min", "max", "range", "median", "quantile", "iqr", "mad"]
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        real(real64), allocatable :: want(:), got(:), w(:)
        logical, allocatable :: ok(:)
        character(len=:), allocatable :: u
        character(len=8) :: nm, tok
        integer(int64) :: g
        integer :: k
        logical :: keeps, agree
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%key_table(kt, reserve=20)
        do k = 1, size(tokens)
            tok = tokens(k)
            write(nm, "(a,i0)") "c", k
            select case (trim(tok))
            case ("quantile")
                call grp%agg("x", trim(tok), want, q=0.3_real64)
                call grp%add_agg("x", trim(tok), kt, trim(nm), q=0.3_real64)
            case ("var", "std", "sem")
                call grp%agg("x", trim(tok), want, ddof=2)
                call grp%add_agg("x", trim(tok), kt, trim(nm), ddof=2)
            case ("median", "iqr")
                call grp%agg("x", trim(tok), want, method="lower")
                call grp%add_agg("x", trim(tok), kt, trim(nm), method="lower")
            case ("mad")
                call grp%agg("x", trim(tok), want, scale="raw")
                call grp%add_agg("x", trim(tok), kt, trim(nm), scale="raw")
            case default
                call grp%agg("x", trim(tok), want)
                call grp%add_agg("x", trim(tok), kt, trim(nm))
            end select
            call kt%get(trim(nm), got, is_valid=ok)
            if (size(got) /= size(want)) then
                call check(error, .false., "one row per group for " // trim(tok))
                return
            end if
            agree = .true.
            do g = 1_int64, size(want, kind=int64)
                if (.not. same_answer(got(g), want(g))) agree = .false.
                if (ok(g) .neqv. .not. ieee_is_nan(want(g))) agree = .false.
                if (kt%is_null(trim(nm), g) .neqv. ieee_is_nan(want(g))) agree = .false.
            end do
            if (.not. agree) then
                call check(error, .false., trim(tok) // ": the column differs from %agg's array, or its nulls do")
                return
            end if
            if (kt%kind(trim(nm)) /= PK_FLOAT64) then
                call check(error, .false., trim(tok) // " gives a real64 column")
                return
            end if
            ! The dimension rule: a count and a squared quantity carry no unit, everything else
            ! carries the source column's.
            keeps = .not. (trim(tok) == "size" .or. trim(tok) == "count" .or. trim(tok) == "nunique" &
                .or. trim(tok) == "var")
            call kt%unit(trim(nm), u)
            if (keeps .neqv. (u == "mag")) then
                call check(error, .false., trim(tok) // ": the unit is inherited exactly when the " // &
                    "statistic keeps the column's dimension")
                return
            end if
        end do
        ! unit= wins over both halves of that rule.
        call grp%add_agg("x", "mean", kt, "u1", unit="Jy")
        call kt%unit("u1", u)
        call check(error, u == "Jy", "unit= overrides the inherited unit")
        if (allocated(error)) return
        call grp%add_agg("x", "count", kt, "u2", unit="Jy")
        call kt%unit("u2", u)
        call check(error, u == "Jy", "...and gives one to a statistic that would carry none")
        if (allocated(error)) return
        call grp%add_agg("x", "mean", kt, "u3", unit="")
        call kt%unit("u3", u)
        call check(error, u == "", "unit="""" is how a caller asks for no unit at all")
        if (allocated(error)) return
        ! The weighted route, forwarded verbatim: still %agg's array.
        call t%get("w", w)
        call grp%agg("x", "mean", want, weights=w)
        call grp%add_agg("x", "mean", kt, "wm", weights=w)
        call kt%get("wm", got)
        agree = .true.
        do g = 1_int64, size(want, kind=int64)
            if (.not. same_answer(got(g), want(g))) agree = .false.
        end do
        call check(error, agree, "weights= reaches the statistic unchanged")
        if (allocated(error)) return
        call grp%agg("x", "mean", want, weight_column="w")
        call grp%add_agg("x", "mean", kt, "wc", weight_column="w")
        call kt%get("wc", got)
        agree = .true.
        do g = 1_int64, size(want, kind=int64)
            if (.not. same_answer(got(g), want(g))) agree = .false.
        end do
        call check(error, agree, "and so does weight_column=")
        if (allocated(error)) return
        ! nan_to_null=.false. is the negative control of the default: the NaN stays a value,
        ! which is exactly what %add_column of %agg's array leaves.
        call grp%agg("x", "std", want)
        call grp%add_agg("x", "std", kt, "raw", nan_to_null=.false.)
        call kt%get("raw", got, is_valid=ok)
        call check(error, ieee_is_nan(want(4)), "group 4 has one row, so its std is NaN")
        if (allocated(error)) return
        call check(error, all(ok), "with nan_to_null=.false. no row is Null")
        if (allocated(error)) return
        call check(error, ieee_is_nan(got(4)), "...and the NaN is stored as a value")
    end subroutine test_add_agg_equals_agg_and_add_column

    !> `exact=.true.` gives an `integer(int64)` column equal to what the exact family writes into
    !! an array, on the fixture whose group-1 values straddle 2**53 -- the real64 column of the
    !! same statistic is shown to differ, which is what the argument is for. The unit rule is the
    !! token form's; `nan_to_null=`, `weights=` and the options are refused rather than ignored,
    !! out of process (`table_group_add_agg_nan_to_null_exact`).
    subroutine test_add_agg_exact_family(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=8), parameter :: toks(8) = [character(len=8) :: "size", "count", "nunique", "sum", &
            "min", "max", "first", "last"]
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: want(:), got(:)
        real(real64), allocatable :: approx(:)
        character(len=:), allocatable :: u
        character(len=8) :: nm, tok
        integer :: k
        logical :: keeps
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%key_table(kt, reserve=12)
        do k = 1, size(toks)
            tok = toks(k)
            write(nm, "(a,i0)") "e", k
            call grp%agg("big", trim(tok), want)
            call grp%add_agg("big", trim(tok), kt, trim(nm), exact=.true.)
            if (kt%kind(trim(nm)) /= PK_INT64) then
                call check(error, .false., "exact " // trim(tok) // " gives an int64 column")
                return
            end if
            call kt%get(trim(nm), got)
            if (.not. all(got == want)) then
                call check(error, .false., "exact " // trim(tok) // " differs from %agg's int64 array")
                return
            end if
            keeps = .not. (trim(tok) == "size" .or. trim(tok) == "count" .or. trim(tok) == "nunique")
            call kt%unit(trim(nm), u)
            if (keeps .neqv. (u == "ct")) then
                call check(error, .false., "exact " // trim(tok) // ": the unit rule is the token form's")
                return
            end if
        end do
        call kt%get("e4", got)
        call grp%agg("big", "sum", approx)
        call check(error, got(1) == 2_int64**54 + 11_int64, "the exact sum of group 1 is the fixture's")
        if (allocated(error)) return
        call check(error, nint(approx(1), int64) /= got(1), &
            "and it is not the real64 sum: exact= is what keeps it exact")
        if (allocated(error)) return
        ! No null anywhere: the exact family aborts rather than answering NaN, so no row can be
        ! the "no answer" one, and the int64 half of the helper applies no NaN rule.
        call check(error, .not. kt%has_nulls("e4"), "an exact column carries no Null row")
    end subroutine test_add_agg_exact_family

    !> The procedure form onto a table equals `%agg(name, func, out)` bit for bit, carries no
    !! unit unless `unit=` is given, and marks Null the groups whose answer the procedure could
    !! not compute -- `nan_to_null=.false.` keeping the NaN a value is the negative control.
    subroutine test_add_agg_procedure_form(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        real(real64), allocatable :: want(:), got(:)
        logical, allocatable :: ok(:)
        character(len=:), allocatable :: u
        integer(int64) :: g
        logical :: agree
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%key_table(kt, reserve=6)
        call grp%agg("x", cb_col_plain_median, want)
        call grp%add_agg("x", cb_col_plain_median, kt, "med")
        call kt%get("med", got)
        agree = .true.
        do g = 1_int64, size(want, kind=int64)
            if (.not. same_answer(got(g), want(g))) agree = .false.
        end do
        call check(error, agree, "the procedure form's column is %agg's array")
        if (allocated(error)) return
        call kt%unit("med", u)
        call check(error, u == "", "and carries no unit: the library cannot know a callback's dimension")
        if (allocated(error)) return
        call grp%add_agg("x", cb_col_plain_median, kt, "med_u", unit="mag")
        call kt%unit("med_u", u)
        call check(error, u == "mag", "unit= is how a callback's column gets one")
        if (allocated(error)) return
        ! cb_col_sum_or_nan answers NaN for a group of fewer than two valid values: group 4 has
        ! one row, so its answer is "could not compute" and its row is Null.
        call grp%add_agg("x", cb_col_sum_or_nan, kt, "s")
        call kt%get("s", got, is_valid=ok)
        call check(error, .not. ok(4), "a NaN the procedure returned marks its group Null")
        if (allocated(error)) return
        call check(error, all(ok(1:3)), "...and only that group")
        if (allocated(error)) return
        call grp%add_agg("x", cb_col_sum_or_nan, kt, "s_raw", nan_to_null=.false.)
        call kt%get("s_raw", got, is_valid=ok)
        call check(error, all(ok), "nan_to_null=.false. leaves the NaN a value")
        if (allocated(error)) return
        call check(error, ieee_is_nan(got(4)), "...stored as it came back")
    end subroutine test_add_agg_procedure_form

    !> Every table-target binding is a READ of the SOURCE table: `%generation()`, the row order
    !! and a `%col` pointer taken before the calls all survive `%add_agg` in both forms and
    !! `%add_size`. The target is the only thing that changes.
    subroutine test_add_forms_are_reads_of_the_source(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        real(real64), pointer :: p(:)
        real(real64), allocatable :: payload(:)
        integer(int32), allocatable :: key(:)
        integer(int64) :: gen0
        !
        call build_basic(t)
        gen0 = t%generation()
        call t%col("payload", p)
        call t%group_by(["key"], grp)
        call grp%key_table(kt, reserve=8)
        call grp%add_agg("payload", "mean", kt, "m")
        call grp%add_agg("key", "sum", kt, "s", exact=.true.)
        call grp%add_agg("payload", cb_col_plain_median, kt, "cm")
        call grp%add_size(kt, "n")
        call check(error, kt%ncols() == 5, "the target gained four columns")
        if (allocated(error)) return
        call check(error, t%generation() == gen0, "and the source table's generation did not move")
        if (allocated(error)) return
        call check(error, grp%is_current(), "so the grouping is still current")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "nothing detached the source")
        if (allocated(error)) return
        call check(error, t%nrows() == 9_int64, "every source row is still there")
        if (allocated(error)) return
        call t%get("key", key)
        call check(error, all(key == [7, 3, 7, 3, 9, 7, 3, 9, 7]), "the key column is in its original order")
        if (allocated(error)) return
        call t%get("payload", payload)
        call check(error, all(payload == [1.0_real64, 2.0_real64, 3.0_real64, 4.0_real64, 5.0_real64, &
            6.0_real64, 7.0_real64, 8.0_real64, 9.0_real64]), "and so is the payload")
        if (allocated(error)) return
        call check(error, associated(p), "a %col pointer into the source taken before the adds is still valid")
        if (allocated(error)) return
        call check(error, p(1) == 1.0_real64, "...reading its own first value")
        if (allocated(error)) return
        call check(error, p(9) == 9.0_real64, "...and its own last")
    end subroutine test_add_forms_are_reads_of_the_source

    !> `force=.true.` replaces an existing column's values AND its kind, advancing the TARGET's
    !! `%generation()` as `%add_column(force=.true.)` documents; an add under a NEW name inside
    !! the reserved capacity does not. Without `force` a taken name aborts, out of process
    !! (`table_group_add_size_name_taken`).
    subroutine test_add_agg_force_replaces(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        real(real64), allocatable :: got(:), want(:)
        integer(int64), allocatable :: igot(:)
        integer(int64) :: gen0
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%key_table(kt, reserve=6)
        call grp%add_agg("x", "min", kt, "v")
        gen0 = kt%generation()
        call grp%add_agg("x", "max", kt, "w")
        call check(error, kt%generation() == gen0, "a new name inside the reserved capacity moves no slot")
        if (allocated(error)) return
        call grp%add_agg("x", "max", kt, "v", force=.true.)
        call grp%agg("x", "max", want)
        call kt%get("v", got)
        call check(error, all(got == want), "force= replaced the column's values")
        if (allocated(error)) return
        call check(error, kt%generation() /= gen0, "...and replacing a column advances the target's generation")
        if (allocated(error)) return
        call grp%add_agg("big", "sum", kt, "v", exact=.true., force=.true.)
        call check(error, kt%kind("v") == PK_INT64, "force= replaces the kind too")
        if (allocated(error)) return
        call kt%get("v", igot)
        call check(error, igot(1) == 2_int64**54 + 11_int64, "...with the new statistic's values")
        if (allocated(error)) return
        call grp%add_size(kt, "v", force=.true.)
        call kt%get("v", igot)
        call check(error, all(igot == [5_int64, 3_int64, 3_int64, 1_int64]), "%add_size forwards force= too")
    end subroutine test_add_agg_force_replaces

    !> Zero groups: every form adds a zero-row column, on a target that has no columns yet and on
    !! an empty key table, and the callback form never calls the procedure. Then the other half of
    !! the column-less rule, which an empty grouping cannot see because 0 rows and 0 groups agree
    !! whatever the check is: a fresh `parquet_new_table` under a grouping of FOUR groups takes
    !! `%ngroups()` rows from the first add.
    subroutine test_add_forms_on_empty_grouping(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt, bare, u, fresh
        type(parquet_grouping) :: grp, g4
        integer(int64), allocatable :: none(:), counts(:)
        real(real64), allocatable :: got(:)
        !
        allocate(none(0))
        call parquet_new_table(t)
        call t%add_column("k", none)
        call t%add_column("v", none)
        call t%group_by("k", grp)
        call grp%key_table(kt)
        empty_col_calls = 0
        call grp%add_agg("v", "mean", kt, "m")
        call grp%add_agg("v", "sum", kt, "s", exact=.true.)
        call grp%add_agg("v", cb_col_count_empty, kt, "cm")
        call grp%add_size(kt, "n")
        call check(error, kt%ncols() == 5, "four columns went onto the empty key table")
        if (allocated(error)) return
        call check(error, kt%nrows() == 0_int64, "...each of zero rows")
        if (allocated(error)) return
        call kt%get("m", got)
        call check(error, size(got) == 0, "and reading one back gives a zero-length array")
        if (allocated(error)) return
        call check(error, empty_col_calls == 0, "the callback was never called: there is no group to call it for")
        if (allocated(error)) return
        ! A target with no columns at all takes its row count from the first add, exactly as
        ! %add_column fixes a new table's.
        call parquet_new_table(bare)
        call grp%add_size(bare, "n")
        call check(error, bare%ncols() == 1, "a column-less target is accepted")
        if (allocated(error)) return
        call check(error, bare%nrows() == 0_int64, "...and its row count becomes %ngroups()")
        if (allocated(error)) return
        ! The same rule where the two counts can disagree: a fresh table has 0 rows and this
        ! grouping has 4 groups, so a row-count check that did not ask "has it any columns yet?"
        ! would refuse it.
        call build_agg(u)
        call u%group_by("k", g4)
        call parquet_new_table(fresh)
        call g4%add_size(fresh, "n")
        call check(error, fresh%nrows() == g4%ngroups(), &
            "a column-less target under a NON-empty grouping takes the group count as its row count")
        if (allocated(error)) return
        call fresh%get("n", counts)
        call check(error, all(counts == [5_int64, 3_int64, 3_int64, 1_int64]), "...carrying the group sizes")
        if (allocated(error)) return
        call g4%add_agg("x", "mean", fresh, "m")
        call check(error, fresh%ncols() == 2, "and a second add goes on beside it")
    end subroutine test_add_forms_on_empty_grouping

    !> `%add_size(kt, as)` is `%key_table(size_name=)`'s column, bit for bit, added later: the
    !! same values as `%size`'s `int64` answer, kind `PK_INT64`, no unit, on a fixture with groups
    !! of four different sizes. So a summary gains its count when it is built or at any time
    !! after, and the two spellings cannot disagree.
    subroutine test_add_size_equals_size_and_size_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt, kt2
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: counts(:), got(:), at_build(:)
        character(len=:), allocatable :: u
        integer(int64) :: gen0
        !
        call build_agg(t)
        gen0 = t%generation()
        call t%group_by("k", grp)
        call grp%size(counts)
        call grp%key_table(kt, reserve=2)
        call grp%add_size(kt, "n")
        call kt%get("n", got)
        call check(error, size(got) == size(counts), "one count per group")
        if (allocated(error)) return
        call check(error, all(got == counts), "and each is %size's own answer")
        if (allocated(error)) return
        call check(error, all(got == [5_int64, 3_int64, 3_int64, 1_int64]), "...the fixture's unequal groups")
        if (allocated(error)) return
        call check(error, kt%kind("n") == PK_INT64, "the column is int64")
        if (allocated(error)) return
        call kt%unit("n", u)
        call check(error, u == "", "and carries no unit: a count has none")
        if (allocated(error)) return
        call check(error, .not. kt%has_nulls("n"), "no row is Null: a count is never NaN")
        if (allocated(error)) return
        ! The column %key_table(size_name=) creates, built the other way round.
        call grp%key_table(kt2, size_name="n")
        call kt2%get("n", at_build)
        call check(error, all(got == at_build), "%add_size later is %key_table(size_name=) at build")
        if (allocated(error)) return
        call check(error, t%generation() == gen0, "and the source table is untouched")
    end subroutine test_add_size_equals_size_and_size_name

    !> **The wrapper computes nothing, and the k-th name takes the k-th result.** `%add_apply`
    !! with `as="m,e"` must give two columns equal to `out(1, :)` and `out(2, :)` of
    !! `%apply(func, 2, out)`, bit for bit; one name is the one-result case and equals the
    !! arithmetic over `%csr`; the reducer object form equals the procedure form; `unit=` reaches
    !! every column named; a result the callback could not compute is a Null row in ITS column
    !! only; and `as` is read by the tokenizer `%group_by`'s keys go through, so blanks around a
    !! comma and a `;` separator name the same two columns.
    subroutine test_add_apply_names_and_forms(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        type(scaled_sum_reducer) :: red
        type(nan_small_reducer) :: nred
        real(real64), allocatable :: want(:, :), got1(:), got2(:), payload(:)
        integer(int64), allocatable :: offsets(:), rows(:)
        logical, allocatable :: ok(:)
        character(len=:), allocatable :: u
        real(real64) :: csr_sum(3)
        integer(int64) :: g
        integer :: nadded
        !
        call build_basic(t)
        call t%col("payload", ctx_add_p)
        call t%group_by(["key"], grp)
        call grp%key_table(kt, reserve=20)
        nadded = kt%ncols()
        ! The oracle: the matrix %apply this call runs for itself.
        call grp%apply(cb_add_sum_and_size, 2, want)
        call grp%add_apply(cb_add_sum_and_size, kt, "m,e")
        call check(error, kt%ncols() == nadded + 2, "two names added two columns")
        if (allocated(error)) return
        call kt%get("m", got1)
        call kt%get("e", got2)
        call check(error, size(got1) == 3 .and. size(got2) == 3, "one row per group in each")
        if (allocated(error)) return
        call check(error, all(got1 == want(1, :)) .and. all(got2 == want(2, :)), &
            "the k-th name holds the k-th result of %apply's matrix, bit for bit")
        if (allocated(error)) return
        call check(error, all(got1 == [13.0_real64, 19.0_real64, 13.0_real64]) .and. &
            all(got2 == [3.0_real64, 4.0_real64, 2.0_real64]), &
            "...which are the fixture's sums and sizes, in that order and not the other")
        if (allocated(error)) return
        call check(error, kt%kind("m") == PK_FLOAT64 .and. kt%kind("e") == PK_FLOAT64, &
            "both columns are real64")
        if (allocated(error)) return
        call kt%unit("m", u)
        call check(error, u == "", "and carry no unit: the library cannot know a callback's dimension")
        if (allocated(error)) return
        ! One name is the one-result case: nout comes from the names, and the answer is the
        ! arithmetic a caller would write over %csr.
        call grp%add_apply(cb_add_sum_and_size, kt, "one")
        call check(error, kt%ncols() == nadded + 3, "one name added one column")
        if (allocated(error)) return
        call t%get("payload", payload)
        call grp%csr(offsets, rows)
        do g = 1_int64, 3_int64
            csr_sum(g) = sum(payload(rows(offsets(g) : offsets(g + 1_int64) - 1_int64)))
        end do
        call kt%get("one", got1)
        call check(error, all(got1 == csr_sum), "the one-name column is the sum over the group's own rows")
        if (allocated(error)) return
        ! The object form: the same context in components rather than module variables.
        call t%col("payload", red%p)
        red%scale = 1.0_real64
        call grp%add_apply(red, kt, "om,oe")
        call kt%get("om", got1)
        call kt%get("oe", got2)
        call check(error, all(got1 == want(1, :)) .and. all(got2 == want(2, :)), &
            "the reducer object form gives the procedure form's columns")
        if (allocated(error)) return
        ! The `as` list is read by the library's one tokenizer: blanks are trimmed and a
        ! semicolon separates, as they do in %group_by's key list.
        call grp%add_apply(cb_add_sum_and_size, kt, " s1 ; s2 ")
        call grp%add_apply(cb_add_sum_and_size, kt, "q1 , q2")
        call check(error, kt%has_column("s1") .and. kt%has_column("s2") .and. &
            kt%has_column("q1") .and. kt%has_column("q2"), "blanks and a semicolon name the same two columns")
        if (allocated(error)) return
        call kt%get("s2", got1)
        call kt%get("q2", got2)
        call check(error, all(got1 == want(2, :)) .and. all(got2 == want(2, :)), &
            "...with the second result in each")
        if (allocated(error)) return
        ! unit= applies to every column named: the several-result case is a value and its error.
        call grp%add_apply(cb_add_sum_and_size, kt, "u1,u2", unit="Jy")
        call kt%unit("u1", u)
        call check(error, u == "Jy", "unit= reaches the first column named")
        if (allocated(error)) return
        call kt%unit("u2", u)
        call check(error, u == "Jy", "...and every other one")
        if (allocated(error)) return
        ! A result the callback could not compute is a NaN by the interface's contract, and the
        ! Null lands in ITS column and its group alone.
        call grp%add_apply(cb_add_nan_small, kt, "n1,n2")
        call kt%get("n2", got2, is_valid=ok)
        call check(error, .not. ok(3), "a NaN result marks its own group Null")
        if (allocated(error)) return
        call check(error, all(ok(1:2)), "...and only that group")
        if (allocated(error)) return
        call check(error, .not. kt%has_nulls("n1"), "the sibling column, whose results are all numbers, has no Null")
        if (allocated(error)) return
        call grp%add_apply(cb_add_nan_small, kt, "r1,r2", nan_to_null=.false.)
        call kt%get("r2", got2, is_valid=ok)
        call check(error, all(ok), "nan_to_null=.false. leaves the NaN a value")
        if (allocated(error)) return
        call check(error, ieee_is_nan(got2(3)), "...stored as the callback returned it")
        if (allocated(error)) return
        ! The object form goes through the same helper, so both rules hold there too.
        call t%col("payload", nred%p)
        call grp%add_apply(nred, kt, "x1,x2", unit="Jy")
        call kt%get("x2", got2, is_valid=ok)
        call check(error, .not. ok(3) .and. all(ok(1:2)), "the object form marks the same group Null")
        if (allocated(error)) return
        call kt%unit("x2", u)
        call check(error, u == "Jy", "...and unit= reaches its columns too")
        if (allocated(error)) return
        call grp%add_apply(nred, kt, "y1,y2", nan_to_null=.false.)
        call kt%get("y2", got2, is_valid=ok)
        call check(error, all(ok), "the object form's nan_to_null=.false. is the same negative control")
        if (allocated(error)) return
        call check(error, ieee_is_nan(got2(3)), "...with the NaN stored as the reducer returned it")
    end subroutine test_add_apply_names_and_forms

    !> **Every name is checked before the first column is written.** In process that is visible
    !! as a count: a successful call adds exactly as many columns as `as` names, never one per
    !! name plus something else, and `force=.true.` over a mix of a taken name and a free one
    !! replaces the first in place and adds only the second. The refusal half -- a later name the
    !! target already carries, where the count printed before the abort proves nothing was
    !! written -- is out of process (`table_group_add_apply_name_taken`).
    subroutine test_add_apply_validates_before_writing(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, kt
        type(parquet_grouping) :: grp
        type(scaled_sum_reducer) :: red
        real(real64), allocatable :: got(:), sums(:)
        integer :: base
        !
        call build_basic(t)
        call t%col("payload", red%p)
        red%scale = 1.0_real64
        call t%group_by(["key"], grp)
        call grp%key_table(kt, reserve=10)
        base = kt%ncols()
        call grp%add_apply(red, kt, "a,b")
        call check(error, kt%ncols() == base + 2, "two names added exactly two columns")
        if (allocated(error)) return
        call grp%add_apply(red, kt, "c")
        call check(error, kt%ncols() == base + 3, "...and one name exactly one")
        if (allocated(error)) return
        call kt%get("a", sums)
        call check(error, all(sums == [13.0_real64, 19.0_real64, 13.0_real64]), "the first result is the group's sum")
        if (allocated(error)) return
        ! A mix under force=: the taken name is replaced in place and only the free one is added.
        red%scale = 3.0_real64
        call grp%add_apply(red, kt, "a,d", force=.true.)
        call check(error, kt%ncols() == base + 4, "force= over one taken name and one free one added exactly one column")
        if (allocated(error)) return
        call kt%get("a", got)
        call check(error, all(got == 3.0_real64 * sums), "the taken column holds the new call's values")
        if (allocated(error)) return
        call kt%get("d", got)
        call check(error, all(got == [3.0_real64, 4.0_real64, 2.0_real64]), "and the new one holds the second result")
    end subroutine test_add_apply_validates_before_writing

    ! ---- %broadcast and %gather ------------------------------------------------------------------

    !> `%broadcast` against `%group_ids`: `per_row(i)` is `per_group(codes(i))` where `codes(i)`
    !! names a group and `fill` where it is 0, for the int64 form over `%size` and the real64
    !! form over an `%agg` mean, with the default fill (0, a NaN) and an explicit one; the call
    !! is a read; and under `dropna=.false.` no row takes the fill.
    subroutine test_broadcast_equals_group_ids(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: codes(:), counts(:), per_row(:)
        real(real64), allocatable :: means(:), per_row_r(:)
        integer(int64) :: gen, i
        logical :: ok
        !
        call build_nullnan(t)
        gen = t%generation()
        call t%group_by(["k"], grp)
        call grp%group_ids(codes)
        call grp%size(counts)
        call grp%broadcast(counts, per_row)
        call check(error, size(per_row) == 8 .and. count(codes == 0_int64) == 2, &
            "one entry per TABLE row, and the fixture drops two rows")
        if (allocated(error)) return
        ok = .true.
        do i = 1_int64, 8_int64
            if (codes(i) > 0_int64) then
                ok = ok .and. per_row(i) == counts(codes(i))
            else
                ok = ok .and. per_row(i) == 0_int64
            end if
        end do
        call check(error, ok, "each row carries its group's size, the two dropped rows the default 0")
        if (allocated(error)) return
        call grp%broadcast(counts, per_row, fill=-1_int64)
        call check(error, all((per_row == -1_int64) .eqv. (codes == 0_int64)), &
            "an explicit int64 fill lands on exactly the dropped rows")
        if (allocated(error)) return
        call grp%agg("payload", "mean", means)
        call grp%broadcast(means, per_row_r)
        ok = .true.
        do i = 1_int64, 8_int64
            if (codes(i) > 0_int64) then
                ok = ok .and. per_row_r(i) == means(codes(i))
            else
                ok = ok .and. ieee_is_nan(per_row_r(i))
            end if
        end do
        call check(error, ok, "the real64 form: each row its group's mean, a dropped row a NaN")
        if (allocated(error)) return
        call grp%broadcast(means, per_row_r, fill=-99.0_real64)
        call check(error, all((per_row_r == -99.0_real64) .eqv. (codes == 0_int64)), &
            "an explicit real64 fill lands on exactly the dropped rows")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "a broadcast is a read")
        if (allocated(error)) return
        call t%group_by(["k"], grp, dropna=.false.)
        call grp%size(counts)
        call grp%broadcast(counts, per_row, fill=-1_int64)
        call check(error, all(per_row > 0_int64) .and. sum(per_row) == sum(counts * counts), &
            "under dropna=.false. no row takes the fill, and the sizes broadcast sum to their squares")
    end subroutine test_broadcast_equals_group_ids

    !> `%gather` against `%get_slice` over the group's rows, for every group of the twelve-row
    !! `%agg` fixture (one of them a single row) and each of the five buffer kinds: the real64
    !! column with its NaN and nulls, `is_valid` against the slice's; the int32 column into an
    !! int32 buffer and, widened, into an int64 one; the int64 column above 2**53; the real32
    !! column into a real32 buffer and, widened, into a real64 one; the logical column with its
    !! validity. `n` is the group's size, and the entries past `n` keep their canary.
    subroutine test_gather_equals_get_slice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: rows(:), counts(:)
        real(real64), allocatable :: xs(:), b64(:)
        real(real32), allocatable :: fs(:), b32(:)
        integer(int32), allocatable :: iv(:), i32(:)
        integer(int64), allocatable :: bigs(:), i64(:)
        logical, allocatable :: bs(:), bb(:), vm(:), mask(:)
        integer(int64) :: g, n, m, cap
        !
        call build_agg(t)
        call t%group_by("k", grp)
        call grp%size(counts)
        cap = grp%max_size() + 1_int64
        allocate(b64(cap), b32(cap), i32(cap), i64(cap), bb(cap), mask(cap))
        call check(error, grp%ngroups() == 4_int64 .and. cap == 6_int64 .and. minval(counts) == 1_int64, &
            "four unequal groups, the largest of five rows, one of a single row")
        if (allocated(error)) return
        do g = 1_int64, grp%ngroups()
            call grp%rows(g, rows)
            b64 = -7.0_real64
            mask = .false.
            call t%get_slice("x", parquet_slice_list(rows), xs, is_valid=vm)
            call grp%gather("x", g, b64, n, is_valid=mask)
            if (n /= size(rows, kind=int64) .or. n /= counts(g)) then
                call check(error, .false., "n is the group's size"); return
            end if
            if (.not. all(b64(1:n) == xs .or. (ieee_is_nan(b64(1:n)) .and. ieee_is_nan(xs)))) then
                call check(error, .false., "the real64 values equal the slice's, NaN included"); return
            end if
            if (.not. all(mask(1:n) .eqv. vm)) then
                call check(error, .false., "is_valid equals the slice's validity"); return
            end if
            if (.not. (all(b64(n + 1:) == -7.0_real64) .and. .not. any(mask(n + 1:)))) then
                call check(error, .false., "the entries past n keep their canary"); return
            end if
            call t%get_slice("i", parquet_slice_list(rows), iv)
            call grp%gather("i", g, i32, m)
            if (m /= n .or. .not. all(i32(1:n) == iv)) then
                call check(error, .false., "the int32 column into an int32 buffer"); return
            end if
            call grp%gather("i", g, i64, m)
            if (.not. all(i64(1:n) == int(iv, int64))) then
                call check(error, .false., "the int32 column widened into an int64 buffer"); return
            end if
            call t%get_slice("big", parquet_slice_list(rows), bigs)
            call grp%gather("big", g, i64, m)
            if (.not. all(i64(1:n) == bigs)) then
                call check(error, .false., "the int64 column, exact above 2**53"); return
            end if
            call t%get_slice("f", parquet_slice_list(rows), fs)
            call grp%gather("f", g, b32, m)
            if (.not. all(b32(1:n) == fs)) then
                call check(error, .false., "the real32 column into a real32 buffer"); return
            end if
            call grp%gather("f", g, b64, m)
            if (.not. all(b64(1:n) == real(fs, real64))) then
                call check(error, .false., "the real32 column widened into a real64 buffer"); return
            end if
            call t%get_slice("b", parquet_slice_list(rows), bs, is_valid=vm)
            call grp%gather("b", g, bb, m, is_valid=mask)
            if (.not. (all(bb(1:n) .eqv. bs) .and. all(mask(1:n) .eqv. vm))) then
                call check(error, .false., "the logical column, and its validity"); return
            end if
        end do
        call check(error, .true., "every kind agreed with get_slice over every group")
    end subroutine test_gather_equals_get_slice

    !> Zero groups: `%broadcast` of a zero-length `per_group` fills every row of a table whose
    !! key is null throughout, in both kinds, and allocates zero rows over an empty table.
    subroutine test_broadcast_empty_grouping(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, e
        type(parquet_grouping) :: grp
        integer(int64), allocatable :: none(:), per_row(:)
        real(real64), allocatable :: none_r(:), per_row_r(:)
        integer(int64) :: i
        !
        call parquet_new_table(t)
        call t%add_column("k", [1_int32, 2_int32, 3_int32])
        do i = 1_int64, 3_int64
            call t%set_null("k", i)
        end do
        call t%group_by("k", grp)
        allocate(none(0), none_r(0))
        call grp%broadcast(none, per_row)
        call grp%broadcast(none_r, per_row_r, fill=2.5_real64)
        call check(error, grp%ngroups() == 0_int64 .and. size(per_row) == 3 .and. all(per_row == 0_int64) .and. &
            size(per_row_r) == 3 .and. all(per_row_r == 2.5_real64), &
            "an all-null key: zero groups, every row the fill, in both kinds")
        if (allocated(error)) return
        call parquet_new_table(e)
        call e%add_column("k", none)
        call e%group_by("k", grp)
        call grp%broadcast(none, per_row)
        call check(error, allocated(per_row) .and. size(per_row) == 0, "an empty table: zero rows, allocated")
    end subroutine test_broadcast_empty_grouping

    !> **An `int32` form is a second route onto the same walk, and must answer the same thing.**
    !! With `int32` loop counters as `g`, `%rows` in both `rows` kinds, `%csr`, `%gather` over all
    !! five buffer kinds with `is_valid=` given and absent, and `%broadcast` with `fill=` given
    !! and absent each equal their `int64` siblings, entry for entry -- on a fixture with four
    !! unequal groups and a null in every value column, then on one with dropped rows so that the
    !! fill is observable. A forwarder that dropped an optional or narrowed the wrong quantity
    !! would answer something plausible and different.
    subroutine test_int32_forms_equal_int64(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: t, u
        type(parquet_grouping) :: grp, gnn
        integer(int64), allocatable :: r64(:), r64b(:), o64(:), w64(:), c64(:), pr64(:)
        integer(int32), allocatable :: r32(:), o32(:), w32(:), c32(:), pr32(:), i32b(:)
        integer(int64), allocatable :: i64b(:)
        real(real64), allocatable :: b64(:), b64b(:)
        real(real32), allocatable :: b32(:), b32b(:)
        logical, allocatable :: lb(:), lbb(:), m1(:), m2(:)
        integer(int64) :: n64, cap
        integer(int32) :: g32, n32
        integer :: ng
        logical :: agree
        !
        call build_agg(t)
        call t%group_by("k", grp)
        ng = int(grp%ngroups(), int32)
        cap = grp%max_size() + 1_int64
        allocate(b64(cap), b64b(cap), b32(cap), b32b(cap), i32b(cap), i64b(cap), lb(cap), lbb(cap))
        allocate(m1(cap), m2(cap))
        call check(error, ng == 4 .and. cap == 6_int64, "four unequal groups, the largest of five rows")
        if (allocated(error)) return
        ! ---- %rows: an int32 g with either kind of answer, against the int64 g.
        agree = .true.
        do g32 = 1_int32, int(ng, int32)
            call grp%rows(int(g32, int64), r64)
            call grp%rows(g32, r64b)
            call grp%rows(g32, r32)
            if (size(r64b) /= size(r64) .or. size(r32) /= size(r64)) then
                agree = .false.
            else
                if (.not. all(r64b == r64)) agree = .false.
                if (.not. all(int(r32, int64) == r64)) agree = .false.
            end if
        end do
        call check(error, agree, "%rows with an int32 g gives the int64 g's rows, in either answer kind")
        if (allocated(error)) return
        ! ---- %csr: one kind per call, both arrays.
        call grp%csr(o64, w64)
        call grp%csr(o32, w32)
        call check(error, size(o32) == size(o64) .and. size(w32) == size(w64), &
            "the int32 %csr hands back arrays of the same lengths")
        if (allocated(error)) return
        call check(error, all(int(o32, int64) == o64) .and. all(int(w32, int64) == w64), &
            "...holding the int64 partition, offsets and rows alike")
        if (allocated(error)) return
        ! ---- %gather: five buffer kinds, n in g's kind, is_valid given and absent.
        agree = .true.
        do g32 = 1_int32, int(ng, int32)
            b64 = -7.0_real64
            b64b = -7.0_real64
            m1 = .false.
            m2 = .false.
            call grp%gather("x", int(g32, int64), b64, n64, is_valid=m1)
            call grp%gather("x", g32, b64b, n32, is_valid=m2)
            if (int(n32, int64) /= n64) agree = .false.
            if (.not. all(same_answer_v(b64b, b64))) agree = .false.
            if (.not. all(m2 .eqv. m1)) agree = .false.
            ! is_valid ABSENT on both sides: the optional must be forwarded as absent, not as a
            ! buffer of the forwarder's own.
            b64 = -7.0_real64
            b64b = -7.0_real64
            call grp%gather("x", int(g32, int64), b64, n64)
            call grp%gather("x", g32, b64b, n32)
            if (int(n32, int64) /= n64 .or. .not. all(same_answer_v(b64b, b64))) agree = .false.
            call grp%gather("i", int(g32, int64), i32b, n64)
            call grp%gather("i", g32, i32b, n32, is_valid=m2)
            if (int(n32, int64) /= n64) agree = .false.
            call grp%gather("big", g32, i64b, n32, is_valid=m2)
            call grp%gather("big", int(g32, int64), i64b, n64)
            if (int(n32, int64) /= n64) agree = .false.
            ! Both buffers are blanked to the SAME sentinel before each pair, exactly as `b64` is
            ! above: `%gather` writes `buf(1:n)` and leaves the tail alone, so comparing the whole
            ! array reads what the library never wrote -- heap garbage that differs between two
            ! allocations, and an intermittent failure rather than a finding (`testing.md`,
            ! "Tests run concurrently"). Blanked, the full-length comparison still asserts that
            ! neither specific wrote past `n`.
            b32 = -7.0_real32
            b32b = -7.0_real32
            call grp%gather("f", int(g32, int64), b32, n64, is_valid=m1)
            call grp%gather("f", g32, b32b, n32, is_valid=m2)
            if (int(n32, int64) /= n64 .or. .not. all(b32b == b32) .or. .not. all(m2 .eqv. m1)) agree = .false.
            lb = .false.
            lbb = .false.
            call grp%gather("b", int(g32, int64), lb, n64, is_valid=m1)
            call grp%gather("b", g32, lbb, n32, is_valid=m2)
            if (int(n32, int64) /= n64 .or. .not. all(lbb .eqv. lb) .or. .not. all(m2 .eqv. m1)) agree = .false.
        end do
        call check(error, agree, "%gather with an int32 g fills the same buffers and hands n back in g's kind")
        if (allocated(error)) return
        ! The int32 halves of the two widening buffers, read back against their own int64 runs.
        call grp%gather("i", 2_int32, i32b, n32)
        call grp%gather("i", 2_int64, i64b, n64)
        call check(error, all(int(i32b(1:n32), int64) == i64b(1:n64)), &
            "an int32 column reaches an int32 buffer and an int64 one alike through the int32 g")
        if (allocated(error)) return
        ! ---- %broadcast: on a fixture with dropped rows, so the fill is observable.
        call build_nullnan(u)
        call u%group_by(["k"], gnn)
        call gnn%size(c64)
        call gnn%size(c32)
        call gnn%broadcast(c64, pr64)
        call gnn%broadcast(c32, pr32)
        call check(error, size(pr32) == size(pr64) .and. all(int(pr32, int64) == pr64), &
            "the int32 %broadcast equals the int64 one, the default fill included")
        if (allocated(error)) return
        call gnn%broadcast(c64, pr64, fill=-1_int64)
        call gnn%broadcast(c32, pr32, fill=-1_int32)
        call check(error, all(int(pr32, int64) == pr64), "...and with an explicit fill in its own kind")
        if (allocated(error)) return
        call check(error, count(pr32 == -1_int32) == 2, "which landed on exactly the fixture's two dropped rows")
    end subroutine test_int32_forms_equal_int64

    ! ---- the callbacks: module procedures, never internal ones (see the header) ----------------

    !> The per-column procedure of `test_agg_callback_presence`: records what it was given and
    !! answers `pf_median` over it, passing the optionals on as they arrived.
    function cb_col_median(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! the median.
        col_calls = col_calls + 1
        if (present(is_valid)) col_valid_present = col_valid_present + 1
        if (present(weights)) col_weights_present = col_weights_present + 1
        call pf_median(values, r, is_valid=is_valid, weights=weights)
    end function cb_col_median

    !> `pf_median` over the group, recording nothing: the callback every test that wants only the
    !! VALUES passes, so that `cb_col_median`'s three counters belong to `test_agg_callback_presence`
    !! alone -- the suite dispatches its tests concurrently (see the header).
    function cb_col_plain_median(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! the median.
        call pf_median(values, r, is_valid=is_valid, weights=weights)
    end function cb_col_plain_median

    !> The weighted sum of the valid values, by plain arithmetic.
    function cb_col_weighted_sum(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! the weighted sum.
        real(real64), allocatable :: w(:)
        allocate(w(size(values)))
        w = 1.0_real64
        if (present(weights)) w = weights
        if (present(is_valid)) then
            where (.not. is_valid) w = 0.0_real64
        end if
        r = sum(values * w, mask=w > 0.0_real64)
    end function cb_col_weighted_sum


    !> Counts its calls in `empty_col_calls`, the counter `test_add_forms_on_empty_grouping` owns
    !! (one module variable per test, see the header); answers 0.
    function cb_col_count_empty(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! 0.
        empty_col_calls = empty_col_calls + 1
        r = 0.0_real64
        if (size(values) < 0) r = -huge(r)
        if (present(is_valid) .or. present(weights)) r = 0.0_real64
    end function cb_col_count_empty

    !> The sum of the group's usable values -- non-null, non-NaN, positively weighted -- or a
    !! NaN when there are fewer than two of those: the
    !! "could not compute" answer `%add_agg`'s `nan_to_null` rule turns into a Null row. A NaN of
    !! the procedure's own choosing is what the callback contract asks for, never an abort.
    function cb_col_sum_or_nan(values, is_valid, weights) result(r)
        real(real64), intent(in) :: values(:)            !! the group's values.
        logical, intent(in), optional :: is_valid(:)     !! present for a group holding a null.
        real(real64), intent(in), optional :: weights(:) !! present when weights were given.
        real(real64) :: r                                !! the sum, or a NaN.
        logical, allocatable :: m(:)
        allocate(m(size(values)))
        m = .true.
        if (present(is_valid)) m = is_valid
        if (present(weights)) then
            where (weights <= 0.0_real64) m = .false.
        end if
        ! Per element over a values array: `x /= x` rather than ieee_is_nan, the hot-path
        ! spelling (.claude/rules/fortran-gotchas.md); both are quiet on a quiet NaN.
        where (values /= values) m = .false.
        if (count(m) < 2) then
            r = ieee_value(0.0_real64, ieee_quiet_nan)
            return
        end if
        r = sum(values, mask=m)
    end function cb_col_sum_or_nan

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

    !> `nan_small_reducer%reduce`; see the type.
    subroutine nan_small_reduce(self, g, rows, out)
        class(nan_small_reducer), intent(in) :: self !! the reducer.
        integer(int64), intent(in) :: g              !! the group number.
        integer(int64), intent(in) :: rows(:)        !! the group's rows.
        real(real64), intent(out) :: out(:)          !! receives the two values.
        out(1) = sum(self%p(rows))
        out(2) = real(size(rows), real64)
        if (size(rows) < 3) out(2) = ieee_value(0.0_real64, ieee_quiet_nan)
        if (g < 1_int64) out(1) = -huge(out(1))
    end subroutine nan_small_reduce

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

    !> `test_add_apply_names_and_forms`' matrix callback, and the procedure twin of a
    !! `scaled_sum_reducer` at scale 1: the payload's sum over the group through `ctx_add_p`,
    !! and, when there is room for a second result, the group's size. Guarded on `size(out)`, so
    !! one name and two names both reach it.
    subroutine cb_add_sum_and_size(g, rows, out)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64), intent(out) :: out(:)   !! receives the one or two values.
        out(1) = sum(ctx_add_p(rows))
        if (size(out) > 1) out(2) = real(size(rows), real64)
        if (g < 1_int64) out(1) = -huge(out(1))
    end subroutine cb_add_sum_and_size

    !> The same two results, except that a group of fewer than three rows has no second result to
    !! give: a NaN of the callback's own choosing, which is the "could not compute" answer the
    !! `nan_to_null` rule turns into a Null row -- in that result's column and no other.
    subroutine cb_add_nan_small(g, rows, out)
        integer(int64), intent(in) :: g       !! the group number.
        integer(int64), intent(in) :: rows(:) !! the group's rows.
        real(real64), intent(out) :: out(:)   !! receives the two values.
        out(1) = sum(ctx_add_p(rows))
        out(2) = real(size(rows), real64)
        if (size(rows) < 3) out(2) = ieee_value(0.0_real64, ieee_quiet_nan)
        if (g < 1_int64) out(1) = -huge(out(1))
    end subroutine cb_add_nan_small

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
