!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the JOIN: the engine (`table_join_pairs`, reached through `%join`'s own
!! `pairs=`/`other_pairs=` output) and `%join`'s own column rewrite on top of it.
!!
!! **The engine is asserted over row indices, not over a joined table, and that separation is the
!! point.** Everything that can make a join produce a silent wrong answer -- the null rule, the
!! output count, the cardinality assertion, the ordering -- is arithmetic on row indices, so it
!! can be asserted directly rather than inferred from the columns two phases later. `pairs=` is
!! what makes that reachable without a test-only hook; `pairs_of` below is how these tests get
!! at it without the mutation `%join` performs alongside.
!!
!! Three things shape the suite:
!!
!! * **A brute-force oracle, not a second engine path.** The expected pairs are computed by a
!!   nested loop over the two key arrays, which shares nothing with the sort. Comparing two
!!   engine paths against each other would pass just as happily against a defect in the run walk.
!! * **Every `how` is asserted by its exact row sequence**, not by its row count. A count is
!!   satisfied by the right number of wrong pairs.
!! * **Nulls carry a negative control.** The same fixture without the nulls must match everywhere,
!!   or every null assertion would also pass against an engine that matched nothing at all.
!!
!! The rewrite's own tests sit at the end of the file and add a fourth rule: **the engine is
!! the oracle for the rewrite**, applied by hand to the source arrays, so an agreement says the
!! columns really carry the rows the pair list named rather than that two joins agree.
!!
!! **The suite is run TWICE, under each pair-list engine.** `collect_tests_table_join` registers
!! it with the sort engine forced through the test-only engine hook and
!! `collect_tests_table_join_hash` registers the same tests with the hash engine forced;
!! `pairs_of` asserts after every join which engine actually ran, against the suite's own
!! expectation and the one exception (`order="key"` always costs the sort engine), so neither
!! suite can pass vacuously and a shape the hash engine quietly declined would be named. What the
!! AUTOMATIC choice is -- the hash engine whenever a call is eligible -- is asserted by two tests
!! of the hash suite that clear the hook themselves. The hook and the observable are
!! process-global, so `table_join_hash` runs serially (`suite_is_safe_to_parallelize`): an
!! ineligible join in a concurrently running sibling would otherwise overwrite the observable
!! between a test's join and its own read of it.
!!
!! Abort paths live in test/error_scenarios.f90 as `join_*` scenarios, since they kill the
!! process. Two tests here write a fixture, each to its own path -- the suite runs its tests
!! concurrently, so a shared one would be truncated out from under the other.
module test_table_join
    use parquet
    use iso_fortran_env, only : int32, int64, real64
    use iso_c_binding, only : c_int64_t
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_copy_sign, ieee_is_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_join
    public :: collect_tests_table_join_hash
    !
    !> The shared fixture's left key column. Chosen so that every case is present: a key with no
    !! counterpart (30, 99), a key with two (10), a key with one (20), and a repeated left key.
    integer(int64), parameter :: LKEY(5) = [30_int64, 10_int64, 99_int64, 20_int64, 10_int64]
    !> The shared fixture's right key column: one unmatched (40) and one duplicated (10).
    integer(int64), parameter :: RKEY(4) = [20_int64, 10_int64, 40_int64, 10_int64]
    !> A right key holding EVERY value `LKEY` holds and nothing else, so a `how="semi"` join keeps
    !! every left row and a `how="anti"` join keeps none. The two are what let one fixture assert
    !! both ends of the partition, and the detach behaviour at both ends with it.
    integer(int64), parameter :: ALLKEY(4) = [30_int64, 10_int64, 99_int64, 20_int64]
    !> A right key with NO duplicate, so a left join against it leaves every left row exactly
    !! where it was. `RKEY` is the same fixture with 10 repeated, which is what makes the two a
    !! matched pair: the non-detaching tests below use this one and their negative controls use
    !! `RKEY`, so the only difference between the two arms is the uniqueness of one key.
    integer(int64), parameter :: UKEY(3) = [10_int64, 20_int64, 40_int64]
    !> Engine tokens, matching the join's own (`ENGINE_SORT`/`ENGINE_HASH` in
    !! src/parquet_tables_join.f90) and the values the hook takes and reports.
    integer, parameter :: ENGINE_SORT = 1, ENGINE_HASH = 2
    !> The engine every ELIGIBLE join in the running suite is expected to report, set by the
    !! suite's collector: `table_join` forces the sort engine and `table_join_hash` the hash
    !! engine. Process-global, like the hook it mirrors.
    integer, save :: suite_engine = ENGINE_SORT
    !
    !> The engine hook and its observable. Declared locally here rather than in
    !> `src/parquet_bindings.f90`, the convention every other `parquet_debug_*` hook follows.
    interface
        !> Forces the join's pair-list engine: 0 automatic, 1 the sort engine, 2 the hash engine.
        subroutine parquet_debug_set_join_engine(mode) bind(C, name="parquet_debug_set_join_engine")
            import :: c_int64_t
            integer(c_int64_t), value :: mode !! 0, 1 or 2.
        end subroutine parquet_debug_set_join_engine
        !> The engine the last join ran on, written by the engine body itself.
        function parquet_debug_join_engine_used() result(res) &
            bind(C, name="parquet_debug_join_engine_used")
            import :: c_int64_t
            integer(c_int64_t) :: res !! ENGINE_SORT or ENGINE_HASH.
        end function parquet_debug_join_engine_used
    end interface
    !
contains
    !
    !> Collects this suite's tests with the sort engine forced.
    !!
    !! Forced rather than left automatic: the automatic choice IS the hash engine for every
    !! eligible call, so a suite that cleared the hook would run the hash engine twice and the
    !! sort engine never. The automatic rule itself is asserted in the hash suite
    !! (`test_join_engine_rule_is_data_independent`, `test_join_engine_hook_switches`).
    subroutine collect_tests_table_join(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests.
        !
        ! Set HERE, outside test-drive's parallel loop and before any test runs: the hook is
        ! process-global. run_tester_cpp registers `table_join_hash` immediately before this
        ! suite, so no forced mode leaks past the two of them into a suite that joins.
        call parquet_debug_set_join_engine(int(ENGINE_SORT, c_int64_t))
        suite_engine = ENGINE_SORT
        call join_tests(testsuite)
    end subroutine collect_tests_table_join
    !
    !> Collects the same tests with the hash engine forced, plus the two tests that clear the hook
    !! themselves: one proving it switches engines, one pinning the automatic rule. The suite runs
    !! serially (`suite_is_safe_to_parallelize`); see the module header for why.
    subroutine collect_tests_table_join_hash(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests.
        type(unittest_type), allocatable :: shared(:)
        !
        call parquet_debug_set_join_engine(int(ENGINE_HASH, c_int64_t))
        suite_engine = ENGINE_HASH
        call join_tests(shared)
        ! Concatenated from a separate local rather than appended to itself: the self-referential
        ! `x = [x, ...]` form double-frees under nagfor 7.2 (test/test_errors.f90's own note).
        testsuite = [shared, &
            new_unittest("the engine hook switches engines, and automatic means the hash engine", &
                test_join_engine_hook_switches), &
            new_unittest("the automatic engine choice reads the key kinds and order=, never the data", &
                test_join_engine_rule_is_data_independent)]
    end subroutine collect_tests_table_join_hash
    !
    !> The tests both suites share, in one list so the two cannot drift apart.
    !!
    !! Every join test is registered here, never in one collector only. A join abort the hash engine
    !! re-implements gets a `_hash` twin scenario as well (`scenario_join_hash_twin`,
    !! test/error_scenarios.f90), or the hash engine's copy of that check goes untested.
    subroutine join_tests(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the tests.
        testsuite = [ &
            new_unittest("the inner pair list agrees with a brute-force oracle", &
                test_join_inner_oracle), &
            new_unittest("every how= emits exactly the rows it should, in left order", &
                test_join_how_variants), &
            new_unittest("a null key matches nothing on either side", &
                test_join_nulls), &
            new_unittest("unmatched right rows come back in right-table order", &
                test_join_unmatched_right_order), &
            new_unittest("order=key holds the same pairs as order=left, in key order", &
                test_join_order_key), &
            new_unittest("matched= reports over the PRE-join left rows", &
                test_join_matched), &
            new_unittest("a two-column key matches only when both columns agree", &
                test_join_multikey), &
            new_unittest("other_on= joins columns whose names differ", &
                test_join_other_on), &
            new_unittest("an empty table on either side joins to nothing", &
                test_join_empty), &
            new_unittest("require= accepts a unique key and ignores null-keyed duplicates", &
                test_join_require_ok), &
            new_unittest("%join(inner) builds the table the pair list describes", &
                test_join_apply_inner), &
            new_unittest("%join(left) nulls the incoming columns of an unmatched row", &
                test_join_apply_left), &
            new_unittest("the incoming column's own nulls survive the join", &
                test_join_carries_source_nulls), &
            new_unittest("the merged key, the carried key and the suffix rule", &
                test_join_names), &
            new_unittest("a clashing suffixed name takes the suffix again", &
                test_join_suffix_applied_twice), &
            new_unittest("a join on two nullable keys combines both validity masks", &
                test_join_two_nullable_keys), &
            new_unittest("the separated-string key form joins as the array form does", &
                test_join_string_form), &
            new_unittest("columns= and residency, all four combinations", &
                test_join_columns_residency), &
            new_unittest("a join against an unread table, written with no schema, holds the left columns", &
                test_join_then_schemaless_write), &
            new_unittest("a join detaches, and skips a left column that was never read", &
                test_join_detaches), &
            new_unittest("a zero-row right table joins to all-nulls", &
                test_join_empty_right), &
            new_unittest("a join that moves no row does not detach; one that does, does", &
                test_join_no_detach_control), &
            new_unittest("a non-detaching join leaves the unread columns readable", &
                test_join_no_detach_lazy), &
            new_unittest("a reserved slot keeps %generation() and a %col pointer across a join", &
                test_join_no_detach_generation), &
            new_unittest("a slice joined in place keeps its own rows and its own scope", &
                test_join_no_detach_slice), &
            new_unittest("an incoming column keeps its own unit", &
                test_join_units), &
            new_unittest("matched= is one entry per PRE-join left row", &
                test_join_applies_matched), &
            new_unittest("order=key rewrites the columns in the engine's own order", &
                test_join_applies_order_key), &
            new_unittest("require= accepts every cardinality that holds and changes nothing", &
                test_join_applies_require), &
            new_unittest("max_rows= at exactly the output size is accepted, in both kinds", &
                test_join_applies_max_rows), &
            new_unittest("the string key form honours require=, order=, matched= and max_rows=", &
                test_join_string_form_new_args), &
            new_unittest("%join(right) nulls this table's columns and merges the key from there", &
                test_join_apply_right), &
            new_unittest("%join(outer) keeps both sides, and the three counts add up", &
                test_join_apply_outer), &
            new_unittest("semi and anti partition this table and carry no column", &
                test_join_apply_semi_anti), &
            new_unittest("a right join with a differing key name keeps both keys", &
                test_join_right_other_on), &
            new_unittest("a right join onto an empty table gives all-null rows", &
                test_join_right_empty_left), &
            new_unittest("a semi join that keeps every row does not detach; anti does", &
                test_join_semi_no_detach), &
            new_unittest("pairs= and other_pairs= name the rows the output was built from", &
                test_join_pairs_output), &
            new_unittest("pairs= can be asked for on its own, and costs the join nothing", &
                test_join_pairs_alone), &
            new_unittest("a NaN key matches every other NaN, where a null matches none", &
                test_join_nan_key), &
            new_unittest("the right table's file metadata is not merged into this one", &
                test_join_keeps_own_metadata), &
            new_unittest("columns= naming the merged key carries no second copy of it", &
                test_join_columns_names_the_key), &
            new_unittest("a container column on this side is carried across a join", &
                test_join_carries_left_container), &
            new_unittest("a left join null-fills two string payload columns over 200 unmatched rows", &
                test_join_left_string_nullfill), &
            new_unittest("the pair-count guard accepts the largest legal total exactly", &
                test_join_add_checked_boundary), &
            new_unittest("order=key takes the sort engine whatever the hook asks", &
                test_join_order_key_is_the_sort_engines), &
            new_unittest("a logical key takes the sort engine whatever the hook asks", &
                test_join_logical_key_is_the_sort_engines), &
            new_unittest("a string key beside another key takes the sort engine", &
                test_join_string_beside_int_key_is_the_sort_engines), &
            new_unittest("a null-keyed right row comes out of an outer join unmatched", &
                test_join_null_right_key_outer), &
            new_unittest("a timestamp key matches by instant across file units", &
                test_join_timestamp_key_across_units), &
            new_unittest("a two-column key with a null in a different column on each side", &
                test_join_multikey_null_on_each_side), &
            new_unittest("a real key matches -0.0 to +0.0 and two NaN payloads to each other", &
                test_join_real_key_signed_zero_and_nan_payloads), &
            new_unittest("a string key matches by its exact bytes", &
                test_join_string_key_exact_bytes) &
            ]
    end subroutine join_tests
    !
    !> Puts the hook back to the running suite's own mode, for a test that moved it.
    subroutine restore_suite_engine()
        if (suite_engine == ENGINE_HASH) then
            call parquet_debug_set_join_engine(int(ENGINE_HASH, c_int64_t))
        else
            call parquet_debug_set_join_engine(0_c_int64_t)
        end if
    end subroutine restore_suite_engine
    !
    !> The engine an ELIGIBLE join in the running suite must report: the suite's own, except
    !! that `order="key"` always costs the sort engine.
    integer function engine_expected(order) result(want)
        character(len=*), intent(in), optional :: order !! the call's `order=`, if any.
        !
        want = suite_engine
        if (present(order)) then
            if (trim(order) == "key") want = ENGINE_SORT
        end if
    end function engine_expected
    !
    !> Asserts the last join ran on engine `want`, naming both when it did not.
    subroutine check_engine(error, want)
        type(error_type), allocatable, intent(out) :: error !! set when the wrong engine ran.
        integer, intent(in) :: want                         !! ENGINE_SORT or ENGINE_HASH.
        character(len=96) :: msg
        integer :: got
        !
        got = int(parquet_debug_join_engine_used())
        write (msg, "(a,i0,a,i0,a)") "the join ran on engine ", got, " where the suite expected ", &
            want, " (1 sort, 2 hash)"
        call check(error, got == want, trim(msg))
    end subroutine check_engine
    !
    !> Builds a two-column table: an int64 key and an int64 payload numbered from 1.
    subroutine build(t, name, keys)
        type(parquet_table), intent(out) :: t     !! the table.
        character(len=*), intent(in) :: name      !! the key column's name.
        integer(int64), intent(in) :: keys(:)     !! the key values.
        integer(int64), allocatable :: payload(:)
        integer(int64) :: k
        !
        allocate(payload(size(keys)))
        do k = 1_int64, size(keys, kind=int64)
            payload(k) = k
        end do
        call parquet_new_table(t)
        call t%add_column(name, keys)
        call t%add_column("payload", payload)
    end subroutine build
    !
    !> The pair list `%join` worked from, read off a THROWAWAY CLONE of the left table.
    !!
    !! `%join` mutates its left table, and most tests below assert several joins over one
    !! fixture -- so the clone is what keeps that fixture the constant it is meant to be. It is
    !! also the non-mutating form the guide page tells a caller to use, so the engine assertions
    !! exercise the documented idiom rather than a route of their own.
    !!
    !! `%join` is the ONLY way in: `table_join_pairs` is a submodule procedure behind a private
    !! interface, so nothing here can call the engine directly, and a test-only hook that could
    !! would be a second route to one pair list -- a second thing to keep correct, and public API
    !! in `parquet_tables` for every user of the library, since a Fortran-side debug hook has
    !! nowhere else to live.
    !!
    !! There is deliberately no `max_rows=` here: `%join`'s ceiling lives in REQUIRED dummies on
    !! four of the six specifics, so an absent optional cannot be forwarded into one. The ceiling
    !! is asserted where it belongs -- `test_join_applies_max_rows` and the four `join_max_rows_*`
    !! error scenarios, each of which names its specific.
    !!
    !! **Every call also asserts which engine ran** (`check_engine`), against the running suite's
    !! expectation -- so a caller checks `error` after it, as after any `check`.
    subroutine pairs_of(error, a, b, on, il, ir, n_out, other_on, how, order, require, matched)
        type(error_type), allocatable, intent(out) :: error !! set when the wrong engine ran.
        type(parquet_table), intent(in) :: a  !! the LEFT table; cloned, never mutated.
        type(parquet_table), intent(in) :: b  !! the RIGHT table.
        character(len=*), intent(in) :: on(:) !! left key columns.
        integer(int64), allocatable, intent(out) :: il(:) !! per output row: left row, or 0.
        integer(int64), allocatable, intent(out) :: ir(:) !! per output row: right row, or 0.
        integer(int64), intent(out) :: n_out  !! output rows; `size(il)`.
        character(len=*), intent(in), optional :: other_on(:) !! right key columns.
        character(len=*), intent(in), optional :: how     !! join kind.
        character(len=*), intent(in), optional :: order   !! output ordering.
        character(len=*), intent(in), optional :: require !! cardinality assertion.
        logical, allocatable, intent(out), optional :: matched(:) !! per PRE-join left row.
        type(parquet_table) :: w
        !
        call a%clone(w)
        call w%join(b, on, other_on=other_on, how=how, order=order, require=require, &
            matched=matched, pairs=il, other_pairs=ir)
        n_out = size(il, kind=int64)
        call check_engine(error, engine_expected(order))
    end subroutine pairs_of
    !
    !> Whether the pair list holds exactly `want`, in exactly that order.
    logical function seq_is(il, ir, want) result(ok)
        integer(int64), intent(in) :: il(:)     !! emitted left rows.
        integer(int64), intent(in) :: ir(:)     !! emitted right rows.
        integer(int64), intent(in) :: want(:,:) !! expected (left, right) pairs, one per column.
        integer(int64) :: k
        !
        ok = size(il, kind=int64) == size(want, 2, kind=int64) .and. &
            size(ir, kind=int64) == size(want, 2, kind=int64)
        if (.not. ok) return
        do k = 1_int64, size(want, 2, kind=int64)
            if (il(k) /= want(1, k) .or. ir(k) /= want(2, k)) then
                ok = .false.
                return
            end if
        end do
    end function seq_is
    !
    !> `how="inner"` against a nested-loop oracle, plus the two documented ordering properties.
    subroutine test_join_inner_oracle(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out, want, i, j, k
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        ! Oracle: every (i, j) with LKEY(i) == RKEY(j), counted independently of the engine.
        want = 0_int64
        do i = 1_int64, size(LKEY, kind=int64)
            do j = 1_int64, size(RKEY, kind=int64)
                if (LKEY(i) == RKEY(j)) want = want + 1_int64
            end do
        end do
        call check(error, n_out == want, "an inner join must emit one row per equal (i, j) pair")
        if (allocated(error)) return
        call check(error, n_out == size(il, kind=int64), "n_out must be size(il)")
        if (allocated(error)) return
        ! Every emitted pair really is a match, and no match is missing.
        ok = .true.
        do k = 1_int64, n_out
            if (LKEY(il(k)) /= RKEY(ir(k))) ok = .false.
        end do
        call check(error, ok, "every emitted inner pair must hold two equal keys")
        if (allocated(error)) return
        ! Ordering: left rows ascending, and right rows ascending within one left row.
        ok = .true.
        do k = 2_int64, n_out
            if (il(k) < il(k - 1_int64)) ok = .false.
            if (il(k) == il(k - 1_int64) .and. ir(k) <= ir(k - 1_int64)) ok = .false.
        end do
        call check(error, ok, "order=left must emit left rows ascending and right ascending within")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([2_int64, 2_int64, 2_int64, 4_int64, &
            4_int64, 1_int64, 5_int64, 2_int64, 5_int64, 4_int64], [2, 5])), &
            "the inner join must emit exactly the five pairs the fixture implies")
    end subroutine test_join_inner_oracle
    !
    !> All six `how` values on one fixture, each asserted by its exact emitted sequence.
    subroutine test_join_how_variants(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 0_int64, 2_int64, 2_int64, &
            2_int64, 4_int64, 3_int64, 0_int64, 4_int64, 1_int64, 5_int64, 2_int64, &
            5_int64, 4_int64], [2, 7])), "how=left must keep every left row, matched or not")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="right")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([2_int64, 2_int64, 2_int64, 4_int64, &
            4_int64, 1_int64, 5_int64, 2_int64, 5_int64, 4_int64, 0_int64, 3_int64], [2, 6])), &
            "how=right must append the unmatched right rows in right-table order")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="outer")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 0_int64, 2_int64, 2_int64, &
            2_int64, 4_int64, 3_int64, 0_int64, 4_int64, 1_int64, 5_int64, 2_int64, &
            5_int64, 4_int64, 0_int64, 3_int64], [2, 8])), &
            "how=outer must keep every row from both sides")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="semi")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([2_int64, 0_int64, 4_int64, 0_int64, &
            5_int64, 0_int64], [2, 3])), &
            "how=semi must emit each matching left row ONCE, however many matches it has")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="anti")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 0_int64, 3_int64, 0_int64], [2, 2])), &
            "how=anti must emit exactly the left rows with no counterpart")
        if (allocated(error)) return
        ! The default is "inner", and an unrecognized token aborts (a join_* error scenario).
        call pairs_of(error, a, b, ["id"], il, ir, n_out)
        if (allocated(error)) return
        call check(error, n_out == 5_int64, "how= absent must mean inner")
    end subroutine test_join_how_variants
    !
    !> A null key matches nothing on either side, including another null -- with its control.
    subroutine test_join_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        integer(int64), parameter :: SAME(3) = [7_int64, 8_int64, 9_int64]
        !
        ! Control first: with no nulls at all, all three rows match one-to-one.
        call build(a, "id", SAME)
        call build(b, "id", SAME)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, n_out == 3_int64, &
            "the control: three equal keys on each side must give three pairs")
        if (allocated(error)) return
        ! Now null row 2 on the left and row 3 on the right. Row 2 is null on ONE side and row 3
        ! on the other, so between them they cover null-vs-value, value-vs-null and, when both
        ! are nulled below, null-vs-null.
        call a%set_null("id", 2_int64)
        call b%set_null("id", 3_int64)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64], [2, 1])), &
            "a null key must match nothing: only row 1 is valid on both sides")
        if (allocated(error)) return
        ! Null the SAME row on both sides: two nulls of what was the same value must not match.
        call b%set_null("id", 2_int64)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64], [2, 1])), &
            "two nulls must not match each other -- unknown is not equal to unknown")
        if (allocated(error)) return
        ! And a null-keyed left row is an UNMATCHED left row, not a dropped one.
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 0_int64, &
            3_int64, 0_int64], [2, 3])), &
            "how=left must keep a null-keyed left row, with no counterpart")
        if (allocated(error)) return
        ! And the mirror image, which is the ONLY shape that exercises the right-hand null rule:
        ! a null group holding rows from BOTH sides. `how=inner` and `how=left` never look at
        ! whether a right row matched, so dropping the null test on that side is invisible to
        ! them -- confirmed by mutation. Right rows 2 and 3 are both null here (the null tier
        ! collects them whatever value they used to hold), so both must come back unmatched.
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="outer")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 0_int64, &
            3_int64, 0_int64, 0_int64, 2_int64, 0_int64, 3_int64], [2, 5])), &
            "a null-keyed RIGHT row must be reported as unmatched, not silently dropped")
    end subroutine test_join_nulls
    !
    !> Under `order="left"` the unmatched right rows come back in RIGHT-TABLE order.
    !>
    !> They are collected group by group, so the natural walk would emit them in KEY order --
    !> which is a different sequence, and the argument name promises it is not the one used. The
    !> fixture is built so the two disagree: the unmatched right rows are 99 (row 2) and 40
    !> (row 4), so key order puts row 4 first and row order puts row 2 first. With only one
    !> unmatched right row, as `test_join_how_variants` has, the two orders coincide and the
    !> distinction is untestable -- confirmed by mutation.
    subroutine test_join_unmatched_right_order(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        !
        call build(a, "id", [10_int64, 20_int64])
        call build(b, "id", [20_int64, 99_int64, 10_int64, 40_int64])
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="right")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 3_int64, 2_int64, 1_int64, &
            0_int64, 2_int64, 0_int64, 4_int64], [2, 4])), &
            "order=left must append the unmatched right rows in right-table order, not key order")
        if (allocated(error)) return
        ! order=key is where the other sequence is correct: each unmatched right row sits in its
        ! own group's place, so 40 precedes 99.
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="right", order="key")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 3_int64, 2_int64, 1_int64, &
            0_int64, 4_int64, 0_int64, 2_int64], [2, 4])), &
            "order=key must place each unmatched right row in its own group's position")
    end subroutine test_join_unmatched_right_order
    !
    !> `order="key"` holds the same pairs as `order="left"`, in the sort's own order.
    subroutine test_join_order_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out, k
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner", order="key")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, "order= must not change how many rows a join emits")
        if (allocated(error)) return
        ! Key order for this fixture: 10 (left rows 2 and 5) before 20 (left row 4). So the
        ! sequence differs from order=left, which would put left row 2 first and left row 4 third.
        call check(error, seq_is(il, ir, reshape([2_int64, 2_int64, 2_int64, 4_int64, &
            5_int64, 2_int64, 5_int64, 4_int64, 4_int64, 1_int64], [2, 5])), &
            "order=key must emit the groups in key order, not left-table order")
        if (allocated(error)) return
        ! Every emitted pair is still a real match -- the reordering must not change the SET.
        ok = .true.
        do k = 1_int64, n_out
            if (LKEY(il(k)) /= RKEY(ir(k))) ok = .false.
        end do
        call check(error, ok, "order=key must emit the same pairs, only in a different order")
    end subroutine test_join_order_key
    !
    !> `matched=` is indexed by the PRE-join left row, whatever the join does to the row set.
    subroutine test_join_matched(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        logical, allocatable :: matched(:)
        integer(int64) :: n_out
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner", matched=matched)
        if (allocated(error)) return
        call check(error, size(matched) == size(LKEY), &
            "matched= must have one entry per PRE-join left row, not per output row")
        if (allocated(error)) return
        call check(error, all(matched .eqv. [.false., .true., .false., .true., .true.]), &
            "matched= must mark exactly the left rows that found a counterpart")
        if (allocated(error)) return
        call check(error, count(matched) == 3, "three of the five left rows have a counterpart")
        if (allocated(error)) return
        ! It does not depend on `how`: the question is about the left table, not the output.
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="anti", matched=matched)
        if (allocated(error)) return
        call check(error, all(matched .eqv. [.false., .true., .false., .true., .true.]), &
            "matched= must not depend on which rows the how= happened to emit")
    end subroutine test_join_matched
    !
    !> With two key columns a row matches only when BOTH agree -- the case one key cannot show.
    subroutine test_join_multikey(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        !
        call parquet_new_table(a)
        call a%add_column("f", [1_int64, 1_int64, 2_int64])
        call a%add_column("g", [10_int64, 20_int64, 10_int64])
        call parquet_new_table(b)
        call b%add_column("f", [1_int64, 2_int64, 1_int64])
        call b%add_column("g", [20_int64, 20_int64, 10_int64])
        call pairs_of(error, a, b, ["f", "g"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        ! (1,10) meets right row 3; (1,20) meets right row 1; (2,10) meets nothing -- right row 2
        ! is (2,20), which agrees on `f` alone and must NOT match.
        call check(error, seq_is(il, ir, reshape([1_int64, 3_int64, 2_int64, 1_int64], [2, 2])), &
            "a two-column key must match only where both columns agree")
        if (allocated(error)) return
        ! The control: on `f` alone the same fixture matches far more widely, so the assertion
        ! above is about the second key rather than about the fixture being sparse.
        call pairs_of(error, a, b, ["f"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, "on `f` alone the same rows must match five ways")
    end subroutine test_join_multikey
    !
    !> `other_on=` joins columns whose names differ on the two sides.
    subroutine test_join_other_on(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:), il2(:), ir2(:)
        integer(int64) :: n_out, n2
        !
        call build(a, "id", LKEY)
        call build(b, "ref_id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, other_on=["ref_id"], how="inner")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, "other_on= must resolve the right table's own name")
        if (allocated(error)) return
        ! Identical to the same-name join, which is the only way to show other_on= changed the
        ! lookup and nothing else.
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il2, ir2, n2, how="inner")
        if (allocated(error)) return
        call check(error, n2 == n_out .and. all(il == il2) .and. all(ir == ir2), &
            "renaming the right key must not change a single emitted pair")
    end subroutine test_join_other_on
    !
    !> The degenerate sizes, where an off-by-one in the counting shows up first.
    subroutine test_join_empty(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64), allocatable :: none(:)
        logical, allocatable :: matched(:)
        integer(int64) :: n_out
        !
        allocate(none(0))
        call build(a, "id", LKEY)
        call build(b, "id", none)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, n_out == 0_int64 .and. size(il) == 0, &
            "nothing can match against an empty right table")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left", matched=matched)
        if (allocated(error)) return
        call check(error, n_out == size(LKEY, kind=int64), &
            "how=left against an empty right table must keep every left row")
        if (allocated(error)) return
        call check(error, .not. any(matched), "no left row can have matched an empty table")
        if (allocated(error)) return
        call build(a, "id", none)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="outer")
        if (allocated(error)) return
        call check(error, n_out == size(RKEY, kind=int64), &
            "an empty left table under how=outer must leave the right rows unmatched")
        if (allocated(error)) return
        call build(b, "id", none)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="outer")
        if (allocated(error)) return
        call check(error, n_out == 0_int64, "two empty tables must join to nothing")
    end subroutine test_join_empty
    !
    !> `require=` accepts what it should, and null-keyed duplicates are not duplicates.
    !!
    !! The second half is the interesting one. A null key matches nothing, so several null-keyed
    !! rows are several rows whose key is UNKNOWN rather than several copies of one key -- and
    !! counting them would make `require="m:1"` abort on a lookup table with two blank
    !! identifiers, a false alarm on a join that would have produced exactly the right answer.
    subroutine test_join_require_ok(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        !
        ! A unique right key: "m:1" and "1:1" both hold, and "1:m" does too since the left keys
        ! [30, 10, 99, 20, 10] repeat only on 10 -- which has no counterpart here.
        call build(a, "id", LKEY)
        call build(b, "id", [20_int64, 40_int64])
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left", require="m:1")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, "require='m:1' must accept a unique right key")
        if (allocated(error)) return
        ! Case folding, and the same assertion spelled the other way round.
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left", require="M:1")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, "require= must be matched case-insensitively")
        if (allocated(error)) return
        ! Two NULL right keys are not a duplicate key: neither can match anything.
        call build(b, "id", [20_int64, 40_int64, 50_int64])
        call b%set_null("id", 2_int64)
        call b%set_null("id", 3_int64)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left", require="m:1")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, &
            "two null right keys must not trip require='m:1' -- neither can match anything")
        if (allocated(error)) return
        ! The control: make them a genuine duplicate VALUE and the same call must abort. That
        ! abort is join_require_m1 in test/error_scenarios.f90; here we only prove the fixture
        ! above is one edit away from tripping it, by showing the duplicate really is joinable.
        call build(b, "id", [20_int64, 20_int64])
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left")
        if (allocated(error)) return
        call check(error, n_out == 6_int64, &
            "the duplicate right key really does multiply the output when it is not refused")
    end subroutine test_join_require_ok
    !
    !
    ! ======================================================================================
    !  %join -- the column rewrite
    ! ======================================================================================
    !
    !> `%join(how="inner")` builds exactly the table the pair list describes.
    !!
    !! **The oracle is the ENGINE, applied by hand**, which is the one comparison that says the
    !! rewrite is faithful: the pair list is itself asserted against a brute-force nested loop
    !! further up this file, so agreement here means the columns really do carry left row `il(k)`
    !! beside right row `ir(k)`. Comparing one joined table against another would compare the
    !! rewrite with itself.
    subroutine test_join_apply_inner(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c
        integer(int64), allocatable :: il(:), ir(:), lp(:), rp(:), key(:)
        integer(int64) :: n_out, k, gen
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call build(c, "id", LKEY)
        gen = c%generation()
        call c%join(b, ["id"])
        ! Every column was gathered, so every %col pointer and row handle taken before the call is
        ! dead -- and the generation counter is the documented way a caller finds that out.
        call check(error, c%generation() > gen, "%join must advance the generation counter")
        if (allocated(error)) return
        call check(error, c%nrows() == n_out, "%join must emit the row count the pair list counted")
        if (allocated(error)) return
        ! Three columns, not four: `id` and `payload` are this table's own and `payload_2` is the
        ! incoming one under the default suffix -- the merged key appears ONCE.
        call check(error, c%ncols() == 3, "a merged key must leave three columns, not four")
        if (allocated(error)) return
        call check(error, .not. c%has_column("id_2"), "a merged key must not also arrive as id_2")
        if (allocated(error)) return
        call c%get("id", key)
        call c%get("payload", lp)
        call c%get("payload_2", rp)
        ok = .true.
        do k = 1_int64, n_out
            if (key(k) /= LKEY(il(k))) ok = .false.
            if (lp(k) /= il(k)) ok = .false.
            if (rp(k) /= ir(k)) ok = .false.
        end do
        call check(error, ok, "every joined row must carry left row il(k) beside right row ir(k)")
    end subroutine test_join_apply_inner
    !
    !> `%join(how="left")` keeps every left row and nulls the INCOMING columns of the rows that
    !! found no counterpart, leaving this table's own columns of those rows alone.
    subroutine test_join_apply_left(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:), lp(:), rp(:)
        integer(int64) :: n_out, k
        logical :: ok, seen_unmatched
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call a%join(b, ["id"], how="left", pairs=il, other_pairs=ir)
        n_out = size(il, kind=int64)
        call check(error, a%nrows() == n_out, "a left join must emit the counted number of rows")
        if (allocated(error)) return
        call a%get("payload", lp)
        call a%get("payload_2", rp)
        ok = .true.
        seen_unmatched = .false.
        do k = 1_int64, n_out
            ! This table's own column is never nulled: a left join always has a left row.
            if (a%is_null("payload", k)) ok = .false.
            if (lp(k) /= il(k)) ok = .false.
            if (ir(k) == 0_int64) then
                seen_unmatched = .true.
                if (.not. a%is_null("payload_2", k)) ok = .false.
            else
                if (a%is_null("payload_2", k)) ok = .false.
                if (rp(k) /= ir(k)) ok = .false.
            end if
        end do
        call check(error, seen_unmatched, &
            "the fixture must contain an unmatched left row, or the assertion below is vacuous")
        if (allocated(error)) return
        call check(error, ok, "an unmatched left row must be null in the incoming columns only")
    end subroutine test_join_apply_left
    !
    !> An incoming column's OWN nulls survive the join, and the unmatched rows are ADDED to them.
    !!
    !! This is the property the whole rewrite rests on. `%set_validity` only ever adds nulls, so
    !! the mask handed to it may describe the unmatched rows and nothing else, and the source
    !! column's validity never has to be read back. Turn its `ior` into an assignment and this is
    !! the test that notices: the right table said row 2's payload was null, and after the join
    !! the rows that matched it come back holding a value.
    subroutine test_join_carries_source_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: key(:)
        integer(int64) :: k
        logical :: ok, seen_source_null, seen_unmatched_null
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        ! Right row 2 holds key 10, which left rows 2 and 5 both match -- so this null travels
        ! into rows the join DID match, which is where an unmatched-row mask could have put it.
        call b%set_null("payload", 2_int64)
        call a%join(b, ["id"], how="left")
        call a%get("id", key)
        ok = .true.
        seen_source_null = .false.
        seen_unmatched_null = .false.
        do k = 1_int64, a%nrows()
            if (.not. a%is_null("payload_2", k)) cycle
            if (key(k) == 10_int64) then
                seen_source_null = .true.
            else
                seen_unmatched_null = .true.
            end if
            if (a%is_null("id", k) .or. a%is_null("payload", k)) ok = .false.
        end do
        call check(error, seen_source_null, &
            "a null the incoming column already held must survive the gather")
        if (allocated(error)) return
        call check(error, seen_unmatched_null, &
            "an unmatched row must be null too -- the negative control for the assertion above")
        if (allocated(error)) return
        call check(error, ok, "a join must null no column of this table's own")
    end subroutine test_join_carries_source_nulls
    !
    !> Keys whose names DIFFER keep both columns; a clashing payload name takes the suffix, and
    !! only the incoming column is ever renamed.
    subroutine test_join_names(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c
        integer(int64), allocatable :: rid(:)
        integer(int64) :: k, lrow
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "rid", RKEY)
        call a%join(b, ["id"], other_on=["rid"], how="left")
        call check(error, a%has_column("rid"), &
            "a right key under a different name is a different column and must come across")
        if (allocated(error)) return
        call check(error, a%has_column("payload_2"), &
            "a clashing payload name must take the default _2 suffix")
        if (allocated(error)) return
        ! EXACTLY four columns: `id` and `payload` are this table's own, `rid` is the carried
        ! right key and `payload_2` the suffixed payload. The count is the assertion that stops a
        ! differing key being carried TWICE -- once for being a key and once by the residency
        ! default -- which would arrive silently as `rid` beside `rid_2`.
        call check(error, a%ncols() == 4 .and. .not. a%has_column("rid_2"), &
            "a carried right key must come across exactly once")
        if (allocated(error)) return
        call a%get("rid", rid)
        ok = .true.
        do k = 1_int64, a%nrows()
            if (a%is_null("rid", k)) cycle
            call a%get_element("payload", k, lrow)
            if (rid(k) /= LKEY(lrow)) ok = .false.
        end do
        call check(error, ok, "the carried right key must hold the value that matched")
        if (allocated(error)) return
        call build(c, "id", LKEY)
        call c%join(b, ["id"], other_on=["rid"], other_suffix="_r")
        call check(error, c%has_column("payload_r") .and. c%has_column("payload"), &
            "other_suffix= must rename the incoming column and leave this table's own alone")
    end subroutine test_join_names

    !> A suffixed name that clashes with a name ALREADY decided takes the suffix a second time.
    !!
    !! `join_name_taken` asks two questions: is the candidate a column of this table, and is it a
    !! destination name some earlier incoming column already claimed? `test_join_names` above
    !! exercises only the first. The second needs an incoming column whose own name is what a
    !! PREVIOUS column's suffixing produced -- here the right table carries both `x` and `x_2`, so
    !! `x` becomes `x_2` and then `x_2` finds that taken and becomes `x_2_2`.
    !!
    !! Without that second question the join would emit `x_2` twice, which is not an abort but a
    !! table with two columns of one name -- the silent outcome this guard exists to prevent.
    subroutine test_join_suffix_applied_twice(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: got(:)
        !
        call parquet_new_table(a)
        call a%add_column("id", [1_int64, 2_int64])
        call a%add_column("x", [10_int64, 20_int64])
        call parquet_new_table(b)
        call b%add_column("id", [1_int64, 2_int64])
        call b%add_column("x", [100_int64, 200_int64])
        call b%add_column("x_2", [1000_int64, 2000_int64])
        call a%join(b, ["id"], how="left")
        call check(error, a%has_column("x") .and. a%has_column("x_2") .and. a%has_column("x_2_2"), &
            "the second clash must take the suffix again rather than reusing x_2")
        if (allocated(error)) return
        call check(error, a%ncols() == 4, &
            "exactly four columns: id, this table's x, and the two incoming ones")
        if (allocated(error)) return
        ! WHICH column ended up where, which the names alone cannot show: a join that renamed
        ! correctly but wired the values the other way round would satisfy every check above.
        call a%get("x", got)
        call check(error, all(got == [10_int64, 20_int64]), "this table's own x is untouched")
        if (allocated(error)) return
        call a%get("x_2", got)
        call check(error, all(got == [100_int64, 200_int64]), &
            "the incoming x became x_2 and kept its own values")
        if (allocated(error)) return
        call a%get("x_2_2", got)
        call check(error, all(got == [1000_int64, 2000_int64]), &
            "and the incoming x_2 became x_2_2, not the other way round")
    end subroutine test_join_suffix_applied_twice

    !> A join on TWO key columns that are both nullable, so two validity masks are combined.
    !!
    !! `hash_key_codes` builds one tuple column per key and carries a validity mask beside it. The
    !! first key to report nulls MOVES its mask in; every later one must be AND-ed onto what is
    !! already there. With a single nullable key the move-in arm alone runs, and a build that
    !! dropped the AND would keep only the first key's nulls -- so a row null in the SECOND key
    !! only would be treated as a real key and could match.
    !!
    !! The fixture puts the two nulls in different rows for exactly that reason: row 2 is null in
    !! the first key and row 3 in the second, so neither mask covers the other.
    subroutine test_join_two_nullable_keys(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: p(:)
        integer(int64) :: k, matched
        !
        call parquet_new_table(a)
        call a%add_column("k1", [1_int64, 2_int64, 3_int64, 4_int64])
        call a%add_column("k2", [10_int64, 20_int64, 30_int64, 40_int64])
        call a%add_column("pa", [1_int64, 2_int64, 3_int64, 4_int64])
        call a%set_null("k1", [.true., .false., .true., .true.])
        call a%set_null("k2", [.true., .true., .false., .true.])
        call parquet_new_table(b)
        call b%add_column("k1", [1_int64, 2_int64, 3_int64, 4_int64])
        call b%add_column("k2", [10_int64, 20_int64, 30_int64, 40_int64])
        call b%add_column("pb", [100_int64, 200_int64, 300_int64, 400_int64])
        call a%join(b, ["k1", "k2"], how="left")
        call check(error, a%nrows() == 4_int64, "a left join keeps every left row")
        if (allocated(error)) return
        call a%get("pb", p)
        matched = 0_int64
        do k = 1_int64, a%nrows()
            if (.not. a%is_null("pb", k)) matched = matched + 1_int64
        end do
        ! Rows 1 and 4 are valid in both keys and must match; rows 2 and 3 are each null in ONE
        ! key, and a null key matches nothing -- which is what the AND of the two masks decides.
        call check(error, matched == 2_int64, &
            "exactly the two rows valid in BOTH keys may match")
        if (allocated(error)) return
        call check(error, .not. a%is_null("pb", 1_int64) .and. .not. a%is_null("pb", 4_int64), &
            "and they are rows 1 and 4")
        if (allocated(error)) return
        call check(error, a%is_null("pb", 2_int64) .and. a%is_null("pb", 3_int64), &
            "a row null in EITHER key is unmatched, not only one null in the first")
        if (allocated(error)) return
        call check(error, p(1) == 100_int64 .and. p(4) == 400_int64, &
            "and the matched rows carried their own counterpart")
    end subroutine test_join_two_nullable_keys
    !
    !> The separated-string key form joins exactly as the array form does.
    subroutine test_join_string_form(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c
        integer(int64), allocatable :: p1(:), p2(:)
        integer(int64) :: k
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call build(c, "id", LKEY)
        call a%join(b, ["id"], how="left")
        call c%join(b, "id", how="left")
        call check(error, a%nrows() == c%nrows(), &
            "the string and array key forms must join identically")
        if (allocated(error)) return
        call a%get("payload_2", p1)
        call c%get("payload_2", p2)
        ok = .true.
        do k = 1_int64, a%nrows()
            if (a%is_null("payload_2", k) .neqv. c%is_null("payload_2", k)) ok = .false.
            if (a%is_null("payload_2", k)) cycle
            if (p1(k) /= p2(k)) ok = .false.
        end do
        call check(error, ok, "the two key forms must produce the same incoming column")
    end subroutine test_join_string_form
    !
    !> `columns=` and residency, all four combinations.
    !!
    !! The rule a caller is most likely to be caught by, and the one whose default failure is
    !! SILENT: a join that carried no payload, followed by a schema-less `parquet_write_table`,
    !! emits a valid file quietly missing columns (`test_join_then_schemaless_write` pins that
    !! composition). Case (3) is the negative control that stops "carry everything" passing as
    !! "carry what is resident".
    subroutine test_join_columns_residency(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_residency.parquet"
        type(parquet_table) :: src, a, b
        integer(int64), allocatable :: extra(:)
        integer(int64) :: k, want
        logical :: ok, seen
        !
        call build(src, "id", RKEY)
        allocate(extra(size(RKEY)))
        do k = 1_int64, size(RKEY, kind=int64)
            extra(k) = 100_int64 * k
        end do
        call src%add_column("extra", extra)
        call parquet_write_table(src, f, overwrite=.true.)
        ! (1) freshly opened: nothing is resident, so nothing comes across.
        call build(a, "id", LKEY)
        call parquet_open_table(b, f)
        call a%join(b, "id", how="left")
        call check(error, a%ncols() == 2, &
            "columns= absent against an unread right table must carry no payload at all")
        if (allocated(error)) return
        ! (2) %materialize_all first: everything comes across.
        call build(a, "id", LKEY)
        call parquet_open_table(b, f)
        call b%materialize_all()
        call a%join(b, "id", how="left")
        call check(error, a%has_column("payload_2") .and. a%has_column("extra"), &
            "columns= absent after materialize_all must carry every column")
        if (allocated(error)) return
        ! (3) exactly one materialized: exactly that one.
        call build(a, "id", LKEY)
        call parquet_open_table(b, f)
        call b%materialize("extra")
        call a%join(b, "id", how="left")
        call check(error, a%has_column("extra") .and. .not. a%has_column("payload_2"), &
            "columns= absent must carry the resident columns only, not all of them")
        if (allocated(error)) return
        ! (4) columns= naming a column that is not resident READS it, and carries only it.
        call build(a, "id", LKEY)
        call parquet_open_table(b, f)
        call a%join(b, "id", how="left", columns="extra")
        call check(error, a%has_column("extra") .and. .not. a%has_column("payload_2"), &
            "columns= must read a column it names, and carry only what it names")
        if (allocated(error)) return
        ! ... and its VALUES arrive, not merely its name. Without this the join could carry an
        ! unread column across as a column of nulls -- which is exactly what would happen if the
        ! first touch were skipped, since a zero-row source takes the all-null path.
        call a%get("extra", extra)
        ok = .true.
        seen = .false.
        do k = 1_int64, a%nrows()
            if (a%is_null("extra", k)) cycle
            seen = .true.
            call a%get_element("id", k, want)
            ! `extra` is 100*r for right row r, so the value names the row it came from -- and
            ! that row's key must be this row's key. Exact even where two right rows share a key.
            if (mod(extra(k), 100_int64) /= 0_int64 .or. extra(k) < 100_int64 .or. &
                    extra(k) > 100_int64 * size(RKEY, kind=int64)) then
                ok = .false.
            else if (RKEY(extra(k) / 100_int64) /= want) then
                ok = .false.
            end if
        end do
        call check(error, seen, "some row must have matched, or the assertion below is vacuous")
        if (allocated(error)) return
        call check(error, ok, "a column columns= named must arrive holding its own values")
    end subroutine test_join_columns_residency
    !
    !> A join against an unread right table, written with no schema, reopens with the LEFT
    !! table's columns only; after `%materialize_all()` on the right table it reopens with every
    !! column and the carried values.
    !!
    !! Each half is asserted in memory by `test_join_columns_residency`; this pins the composition
    !! on the file a caller keeps. A join that carried an unread column across as nulls, or a
    !! schema-less write that emitted a column the table does not hold, fails the first half; the
    !! second half is the negative control that stops a write dropping everything from passing.
    subroutine test_join_then_schemaless_write(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_schemaless_right.parquet"
        character(len=*), parameter :: g1 = "test_run/join_schemaless_unread.parquet"
        character(len=*), parameter :: g2 = "test_run/join_schemaless_all.parquet"
        type(parquet_table) :: src, a, b, c
        integer(int64), allocatable :: extra(:), got(:), id(:)
        integer(int64) :: k
        logical :: ok, seen
        !
        call build(src, "id", RKEY)
        allocate(extra(size(RKEY)))
        do k = 1_int64, size(RKEY, kind=int64)
            extra(k) = 100_int64 * k
        end do
        call src%add_column("extra", extra)
        call parquet_write_table(src, f, overwrite=.true.)
        ! Unread right table: the file holds the left table's two columns and nothing else.
        call build(a, "id", LKEY)
        call parquet_open_table(b, f)
        call a%join(b, "id", how="left")
        call parquet_write_table(a, g1, overwrite=.true.)
        call parquet_open_table(c, g1)
        call check(error, c%ncols() == 2 .and. c%has_column("id") .and. c%has_column("payload"), &
            "a schema-less write after a join against an unread table must hold the left columns only")
        if (allocated(error)) return
        call check(error, .not. c%has_column("extra") .and. .not. c%has_column("payload_2"), &
            "a right column the join never read must not reach the file, not even as nulls")
        if (allocated(error)) return
        ! Negative control: %materialize_all() on the right table puts every column in the file.
        call build(a, "id", LKEY)
        call parquet_open_table(b, f)
        call b%materialize_all()
        call a%join(b, "id", how="left")
        call parquet_write_table(a, g2, overwrite=.true.)
        call parquet_open_table(c, g2)
        call check(error, c%ncols() == 4 .and. c%has_column("payload_2") .and. c%has_column("extra"), &
            "after materialize_all the schema-less write must hold every joined column")
        if (allocated(error)) return
        call c%get("extra", got)
        call c%get("id", id)
        ok = .true.
        seen = .false.
        do k = 1_int64, c%nrows()
            if (c%is_null("extra", k)) cycle
            seen = .true.
            ! `extra` is 100*r for right row r, so the value names the row whose key it must carry.
            if (mod(got(k), 100_int64) /= 0_int64 .or. got(k) < 100_int64 .or. &
                    got(k) > 100_int64 * size(RKEY, kind=int64)) then
                ok = .false.
            else if (RKEY(got(k) / 100_int64) /= id(k)) then
                ok = .false.
            end if
        end do
        call check(error, seen, "some row must have matched, or the value assertion is vacuous")
        if (allocated(error)) return
        call check(error, ok, "the written file must carry each right column's own values")
    end subroutine test_join_then_schemaless_write
    !
    !> A join detaches, and a left column that was never read is skipped rather than read.
    !!
    !! The skipped column is then unreadable for good, which fails LOUDLY through the detach
    !! guard -- the difference between this table's side, where a lost column is an error at the
    !! point of use, and the right table's, where a column that never came across is simply
    !! absent. Only the residency is asserted here; the guard's abort is an error scenario.
    subroutine test_join_detaches(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_detach.parquet"
        type(parquet_table) :: src, a, b
        integer(int64), allocatable :: got(:)
        !
        call build(src, "id", LKEY)
        call parquet_write_table(src, f, overwrite=.true.)
        call parquet_open_table(a, f)
        call build(b, "id", RKEY)
        call a%join(b, "id", how="left")
        call check(error, a%is_detached(), "a join that rewrites the row set must detach")
        if (allocated(error)) return
        call check(error, a%residency("payload") == RES_EMPTY, &
            "a left column that was never read must be skipped by the gather, not read")
        if (allocated(error)) return
        call check(error, a%has_column("payload"), &
            "a skipped column is still a column; it is only unreadable")
        if (allocated(error)) return
        ! The key column WAS read, by the join's own lazy first touch, so it survives.
        call a%get("id", got)
        call check(error, size(got, kind=int64) == a%nrows(), &
            "the key column the join read must still be readable, at the new row count")
    end subroutine test_join_detaches
    !
    !> A zero-row right table joins to all-nulls -- the one path that cannot gather at all,
    !! because there is no row 1 for the unmatched rows to be pointed at.
    subroutine test_join_empty_right(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64) :: empty(0)
        integer(int64) :: k
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "id", empty)
        call a%join(b, "id", how="left")
        call check(error, a%nrows() == size(LKEY, kind=int64), &
            "a left join against an empty table must keep every left row exactly once")
        if (allocated(error)) return
        call check(error, a%has_column("payload_2"), &
            "an empty right column still arrives -- as a column of nulls")
        if (allocated(error)) return
        ok = .true.
        do k = 1_int64, a%nrows()
            if (.not. a%is_null("payload_2", k)) ok = .false.
        end do
        call check(error, ok, "every row carried from an empty table must be null")
    end subroutine test_join_empty_right
    !
    !> The non-detaching join's computed condition, and the negative control beside it.
    !!
    !! The two arms differ in ONE thing: whether the right key repeats. Everything else -- the
    !! file, the left table, `how=`, `columns=` -- is identical, so a guard that never fires and a
    !! guard that always fires each fail exactly one arm. That pairing is the whole point of this
    !! test; splitting it into two would let either half be quietly deleted.
    subroutine test_join_no_detach_control(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_nodetach_ctl.parquet"
        type(parquet_table) :: src, a, u, d, p
        integer(int64), allocatable :: got(:)
        !
        call build(src, "id", LKEY)
        call parquet_write_table(src, f, overwrite=.true.)
        call build(u, "id", UKEY)
        call build(d, "id", RKEY)
        ! ARM 1 -- a right key with no duplicate, so every left row survives exactly once and in
        ! its original position, and the join is `%add_column` and nothing else.
        call parquet_open_table(a, f)
        call a%join(u, "id", how="left", columns="payload")
        call check(error, a%nrows() == size(LKEY, kind=int64), &
            "a left join must keep every left row")
        if (allocated(error)) return
        call check(error, .not. a%is_detached(), &
            "a join that leaves every row where it was must not detach")
        if (allocated(error)) return
        call check(error, a%has_column("payload_2"), &
            "the payload must still arrive on the non-detaching path")
        if (allocated(error)) return
        ! ARM 2 -- the same join against a key that repeats once. Two left rows are duplicated, so
        ! the row set has changed and the table must cut its file loose like any other mutation.
        call parquet_open_table(a, f)
        call a%join(d, "id", how="left", columns="payload")
        call check(error, a%nrows() > size(LKEY, kind=int64), &
            "a duplicated right key must emit more rows than the left table had")
        if (allocated(error)) return
        call check(error, a%is_detached(), &
            "a join that duplicates a row must detach, exactly as every row mutation does")
        if (allocated(error)) return
        ! ARM 3 -- the second negative control, for the OTHER half of the condition. An inner join
        ! matching only the FIRST left row emits `il = [1]`: every entry equals its own position,
        ! so the identity walk alone reports that nothing moved. Only the row COUNT says
        ! otherwise, and without it the table would keep its old row count beside a one-row
        ! payload -- rows that are simply gone, reported as present. Mutation found this arm
        ! missing; the two arms above cannot reach it, because neither shrinks the row set.
        call parquet_open_table(a, f)
        call build(p, "id", LKEY(1:1))
        call a%join(p, "id", how="inner", columns="payload")
        call check(error, a%nrows() == 1_int64, &
            "an inner join matching one left row must leave one row")
        if (allocated(error)) return
        call check(error, a%is_detached(), &
            "a join that DROPS rows must detach even though the rows it kept did not move")
        if (allocated(error)) return
        call a%get("payload_2", got)
        call check(error, size(got, kind=int64) == 1_int64, &
            "and the carried column must be one row long, not the left table's old count")
    end subroutine test_join_no_detach_control
    !
    !> The payoff: a column the join never read is still readable afterwards, at the right length
    !! and holding its own values.
    !!
    !! This is `feature_risks.md` Risk-184's failure mode stated as an assertion. Widening the
    !! condition to "any left join" leaves the table attached with its rows duplicated, and the
    !! next read of an unread column then comes back at the FILE's row count -- a column of the
    !! wrong length aligned to nothing, with no error anywhere. Asserting the length as well as
    !! the values is what makes that visible; the values alone would still line up at the front.
    subroutine test_join_no_detach_lazy(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_nodetach_lazy.parquet"
        type(parquet_table) :: src, a, u
        integer(int64), allocatable :: got(:)
        integer(int64) :: k
        logical :: ok
        !
        call build(src, "id", LKEY)
        call parquet_write_table(src, f, overwrite=.true.)
        call parquet_open_table(a, f)
        call build(u, "id", UKEY)
        call a%join(u, "id", how="left", columns="payload")
        call check(error, .not. a%is_detached(), "the fixture must take the non-detaching path")
        if (allocated(error)) return
        call check(error, a%residency("payload") == RES_EMPTY, &
            "a join that moves no row must not read a column it was not asked for")
        if (allocated(error)) return
        call a%get("payload", got)
        call check(error, size(got, kind=int64) == a%nrows(), &
            "a column read after the join must come back at the table's row count")
        if (allocated(error)) return
        ok = .true.
        do k = 1_int64, a%nrows()
            if (got(k) /= k) ok = .false.
        end do
        call check(error, ok, "and holding its own rows, in their own order")
    end subroutine test_join_no_detach_lazy
    !
    !> `%reserve_columns` interaction: on the non-detaching path the counter is left to
    !! `table_new_slot`, which bumps only when the slot array had to grow.
    !!
    !! Both arms are needed and neither is the interesting one alone: the reserved arm alone would
    !! pass against a join that never bumps at all, and the unreserved arm alone against one that
    !! always does.
    subroutine test_join_no_detach_generation(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table), target :: a
        type(parquet_table) :: u
        integer(int64), pointer :: p(:)
        integer(int64) :: g0
        integer(int64) :: filler(size(LKEY))
        character(len=16) :: nm
        integer :: k
        logical :: ok
        !
        ! ARM 1 -- room reserved for the incoming column, so no descriptor moves.
        call build(a, "id", LKEY)
        call build(u, "id", UKEY)
        call a%reserve_columns(a%ncols() + 1)
        call a%col("payload", p)
        g0 = a%generation()
        call a%join(u, "id", how="left", columns="payload")
        call check(error, .not. a%is_detached(), "the fixture must take the non-detaching path")
        if (allocated(error)) return
        call check(error, a%generation() == g0, &
            "a join that moves no row and grows no slot array must not bump %generation()")
        if (allocated(error)) return
        ! Only now, and only because the counter said so: reading through a pointer whose storage
        ! had been rebuilt would be undefined rather than a failed assertion.
        ok = associated(p)
        if (ok) ok = size(p, kind=int64) == a%nrows()
        if (ok) then
            do k = 1, size(LKEY)
                if (p(k) /= int(k, int64)) ok = .false.
            end do
        end if
        call check(error, ok, "and an outstanding %col pointer must still read its own column")
        if (allocated(error)) return
        ! ARM 2 -- the negative control: no spare slot, so the array grows and every descriptor
        ! relocates. The counter must say so, because that is the one thing a caller holding a
        ! pointer has to go on.
        call build(a, "id", LKEY)
        filler = LKEY
        do while (a%column_capacity(free=.true.) > 0)
            write(nm, "(A,I0)") "fill_", a%ncols()
            call a%add_column(trim(nm), filler)
        end do
        g0 = a%generation()
        call a%join(u, "id", how="left", columns="payload")
        call check(error, a%generation() > g0, &
            "a join with no spare slot relocates every descriptor and must bump %generation()")
    end subroutine test_join_no_detach_generation
    !
    !> A slice joined on the non-detaching path keeps its own rows and its own scope: the rows
    !! did not move, so the slice still means what it did and a later read still reads its half of
    !! the file rather than the whole of it.
    subroutine test_join_no_detach_slice(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_nodetach_slice.parquet"
        type(parquet_table) :: src, a, u
        integer(int64), allocatable :: got(:)
        integer(int64) :: k
        logical :: ok
        !
        call build(src, "id", LKEY)
        call parquet_write_table(src, f, overwrite=.true.)
        call parquet_open_table(a, f, 2_int64, 4_int64)
        call build(u, "id", UKEY)
        call a%join(u, "id", how="left", columns="payload")
        call check(error, a%nrows() == 3_int64, "the slice must keep its own three rows")
        if (allocated(error)) return
        call check(error, .not. a%is_detached(), &
            "a slice whose rows did not move must keep its file and its scope")
        if (allocated(error)) return
        call a%get("payload", got)
        call check(error, size(got, kind=int64) == 3_int64, &
            "a column read after the join must come back at the SLICE's row count")
        if (allocated(error)) return
        ok = .true.
        do k = 1_int64, 3_int64
            ! The file's payload is 1..5, so the slice's own rows are 2, 3, 4 -- reading the whole
            ! file, or reading it from the top, would both show up here.
            if (got(k) /= k + 1_int64) ok = .false.
        end do
        call check(error, ok, "and holding the slice's own rows, not the file's first three")
    end subroutine test_join_no_detach_slice
    !
    !> An incoming column keeps its own unit.
    subroutine test_join_units(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        character(len=:), allocatable :: u
        integer(int64) :: mag(size(RKEY))
        integer(int64) :: k
        !
        call build(a, "id", LKEY)
        call parquet_new_table(b)
        call b%add_column("id", RKEY)
        mag = [(10_int64 * k, k = 1_int64, size(RKEY, kind=int64))]
        call b%add_column("mag", mag, unit="mag")
        call a%join(b, "id", how="left")
        call a%unit("mag", u)
        call check(error, u == "mag", "an incoming column must keep its own unit")
    end subroutine test_join_units

    !
    !> `%join(matched=)` answers over the rows this table had ON ENTRY, which is the only
    !! coordinate system in which the question survives the mutation: a left join against `RKEY`
    !! emits seven rows from five, so a mask sized to the RESULT could not say which of the five
    !! found nothing.
    !!
    !! Checked twice on purpose. Against the ENGINE's own mask, which says the argument reaches
    !! the engine at all -- a `%join` that quietly dropped it would return an unallocated mask,
    !! and one that built its own would be a second answer to keep correct. And against the
    !! literal expectation, which says the engine's answer is the right one. The literal is mixed,
    !! so it is its own negative control: a mask filled with a constant fails it either way round.
    subroutine test_join_applies_matched(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        logical, allocatable :: want(:), got(:)
        integer(int64) :: n_out
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="left", matched=want)
        if (allocated(error)) return
        call a%join(b, ["id"], how="left", matched=got)
        call check(error, allocated(got), "%join must allocate matched= when it is asked for")
        if (allocated(error)) return
        call check(error, size(got, kind=int64) == size(LKEY, kind=int64), &
            "matched= must hold one entry per PRE-join left row, not per output row")
        if (allocated(error)) return
        ! Without this the size assertion above proves nothing -- it would hold just as well for a
        ! mask over the OUTPUT rows if the two counts happened to agree, which for this library's
        ! other join fixture (LKEY against RKEY, inner) they do.
        call check(error, a%nrows() /= size(LKEY, kind=int64), &
            "the fixture must change the row count, or the size assertion above is satisfied twice")
        if (allocated(error)) return
        call check(error, all(got .eqv. want), &
            "%join's matched= must be the engine's own, not a second computation of it")
        if (allocated(error)) return
        call check(error, all(got .eqv. [.false., .true., .false., .true., .true.]), &
            "and must mark exactly the left rows whose key appears in the right table")
    end subroutine test_join_applies_matched
    !
    !> `order="key"` reaches the column rewrite, not only the pair list: the joined table's rows
    !! come out in the engine's own (key) order.
    !!
    !! The two orders hold the SAME pairs, so a comparison of row counts, or of any per-column
    !! aggregate, is satisfied by both -- which is why this asserts the exact sequence each order
    !! produces and then asserts that the two sequences DIFFER. Without that last check an
    !! `order=` that never reached the engine would pass every assertion above it.
    subroutine test_join_applies_order_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c
        integer(int64), allocatable :: il(:), ir(:), lp(:), rp(:), lp2(:)
        integer(int64) :: n_out, k
        logical :: ok
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, order="key")
        if (allocated(error)) return
        call build(c, "id", LKEY)
        call c%join(b, ["id"], order="key")
        call check(error, c%nrows() == n_out, "order=key must emit the counted number of rows")
        if (allocated(error)) return
        call c%get("payload", lp)
        call c%get("payload_2", rp)
        ok = .true.
        do k = 1_int64, n_out
            if (lp(k) /= il(k)) ok = .false.
            if (rp(k) /= ir(k)) ok = .false.
        end do
        call check(error, ok, "every row must carry left row il(k) beside right row ir(k)")
        if (allocated(error)) return
        ! The same join in the default order. Same pairs, different sequence -- and the difference
        ! is the whole of what order= does, so it has to be asserted rather than assumed.
        call build(c, "id", LKEY)
        call c%join(b, ["id"])
        call c%get("payload", lp2)
        call check(error, size(lp2, kind=int64) == size(lp, kind=int64), &
            "the two orderings must hold the same number of rows")
        if (allocated(error)) return
        call check(error, .not. all(lp2 == lp), &
            "order=key must not leave the rows in the order order=left produces")
    end subroutine test_join_applies_order_key
    !
    !> `require=` accepts every cardinality that actually holds, and changes nothing when it does.
    !!
    !! This is the negative control the assertion needs: the abort paths live in
    !! `test/error_scenarios.f90` (`join_require_m1`, `join_require_1m`, `join_bad_require`), and
    !! a check that fired unconditionally would pass every one of them. Each arm therefore joins
    !! twice -- once with the assertion and once without -- and requires the two results to be
    !! identical, which a `require=` that silently dropped rows or reordered them would fail.
    !!
    !! The last arm is the flagship shape: a file-backed left table, `how="left"`, a unique right
    !! key, and `require="m:1"` -- the lookup-table join the guide page leads with. It must still
    !! take the non-detaching path, because an assertion is not a mutation.
    subroutine test_join_applies_require(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_p5_require.parquet"
        type(parquet_table) :: src, a, b, u, c
        logical, allocatable :: m(:)
        !
        call build(b, "id", RKEY)
        call build(u, "id", UKEY)
        ! m:1 -- many rows here may share a key, each finds at most one there. UKEY has no
        ! duplicate; LKEY repeats 10, which is exactly what "m" on the left permits.
        call build(a, "id", LKEY)
        call a%join(u, ["id"], how="left", require="m:1")
        call build(c, "id", LKEY)
        call c%join(u, ["id"], how="left")
        call same_join(error, a, c, "require='m:1' on a unique right key")
        if (allocated(error)) return
        ! 1:m -- the mirror. LKEY(1:4) has no duplicate; RKEY repeats 10.
        call build(a, "id", LKEY(1:4))
        call a%join(b, ["id"], require="1:m")
        call build(c, "id", LKEY(1:4))
        call c%join(b, ["id"])
        call same_join(error, a, c, "require='1:m' on a unique left key")
        if (allocated(error)) return
        ! 1:1 -- both at once, against a fixture where neither side repeats a matched key.
        call build(a, "id", LKEY(1:4))
        call a%join(u, ["id"], require="1:1")
        call build(c, "id", LKEY(1:4))
        call c%join(u, ["id"])
        call same_join(error, a, c, "require='1:1' on two unique keys")
        if (allocated(error)) return
        ! m:m -- the default spelled out, over the fixture that violates all three others.
        call build(a, "id", LKEY)
        call a%join(b, ["id"], require="m:m")
        call build(c, "id", LKEY)
        call c%join(b, ["id"])
        call same_join(error, a, c, "require='m:m' asserts nothing")
        if (allocated(error)) return
        ! The flagship: an assertion must not cost the non-detaching path.
        call build(src, "id", LKEY)
        call parquet_write_table(src, f, overwrite=.true.)
        call parquet_open_table(a, f)
        call a%join(u, "id", how="left", columns="payload", require="m:1", matched=m)
        call check(error, .not. a%is_detached(), &
            "require= must not stop a join that moves no row from keeping its file")
        if (allocated(error)) return
        call check(error, count(m) == 3, &
            "and matched= must still report over the pre-join rows beside it")
    end subroutine test_join_applies_require
    !
    !> Two joined tables hold the same rows: the row count, and both payload columns in order.
    !!
    !! Enough for the `require=`/`max_rows=` controls above, whose failure mode is a guard that
    !! drops or duplicates rows rather than one that corrupts a value: `payload` is this table's
    !! own row number and `payload_2` the other's, so the pair identifies the source row of every
    !! output row exactly.
    subroutine same_join(error, got, want, what)
        type(error_type), allocatable, intent(out) :: error !! set when they differ.
        type(parquet_table), intent(inout) :: got  !! the table joined with the argument under test.
        type(parquet_table), intent(inout) :: want !! the same join without it.
        character(len=*), intent(in) :: what       !! names the argument, for the message.
        integer(int64), allocatable :: gl(:), gr(:), wl(:), wr(:)
        !
        call check(error, got%nrows() == want%nrows(), what // " must not change the row count")
        if (allocated(error)) return
        call got%get("payload", gl)
        call got%get("payload_2", gr)
        call want%get("payload", wl)
        call want%get("payload_2", wr)
        call check(error, all(gl == wl), what // " must not change which left rows are emitted")
        if (allocated(error)) return
        call check(error, all(gr == wr), what // " must not change which right rows are emitted")
    end subroutine same_join
    !
    !> `max_rows=` at EXACTLY the output size is accepted, in both integer kinds.
    !!
    !! Two things at once, and neither is reachable from the abort scenario. The boundary pins the
    !! comparison as `>` rather than `>=`, which is the one place an off-by-one would refuse a
    !! join that fits. And writing the ceiling as a plain literal and again as `_int64` is what
    !! exercises both specifics of the generic -- the whole reason there are six of them.
    subroutine test_join_applies_max_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c
        integer(int64), allocatable :: il(:), ir(:)
        logical, allocatable :: am(:), wm(:)
        integer(int64) :: n_out
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out)
        if (allocated(error)) return
        call check(error, n_out == 5_int64, &
            "the fixture must emit five rows, or the literal ceilings below are not the boundary")
        if (allocated(error)) return
        call build(c, "id", LKEY)
        call c%join(b, ["id"], how="left", require="m:m", order="key", matched=wm)
        ! int32: a plain literal, which is the kind a caller reaches for without thinking about it.
        ! Every other P5 argument travels with it, because a ceiling-carrying specific forwards
        ! them on its own line and dropping one there is invisible to the tests that use the
        ! ceiling-free specific -- which is every other test in this file.
        call build(a, "id", LKEY)
        call a%join(b, ["id"], how="left", require="m:m", order="key", max_rows=7, matched=am)
        call same_join(error, a, c, "an int32 max_rows= beside the other three arguments")
        if (allocated(error)) return
        call check(error, all(am .eqv. wm), "the int32 ceiling's specific must forward matched=")
        if (allocated(error)) return
        ! int64: the kind a row count above 2**31 has to be written in.
        call build(a, "id", LKEY)
        call a%join(b, ["id"], how="left", require="m:m", order="key", max_rows=7_int64, matched=am)
        call same_join(error, a, c, "an int64 max_rows= beside the other three arguments")
        if (allocated(error)) return
        call check(error, all(am .eqv. wm), "the int64 ceiling's specific must forward matched=")
        if (allocated(error)) return
        ! And at EXACTLY the inner join's five rows, in both kinds: the boundary is where an
        ! off-by-one would refuse a join that fits.
        call build(c, "id", LKEY)
        call c%join(b, ["id"])
        call build(a, "id", LKEY)
        call a%join(b, ["id"], max_rows=5)
        call same_join(error, a, c, "an int32 max_rows= at exactly the output size")
        if (allocated(error)) return
        call build(a, "id", LKEY)
        call a%join(b, ["id"], max_rows=5_int64)
        call same_join(error, a, c, "an int64 max_rows= at exactly the output size")
    end subroutine test_join_applies_max_rows
    !
    !> The separated-string key form carries all four of P5's arguments through to the same place
    !! the array form does, in both `max_rows=` kinds and with `other_on=` present and absent.
    !!
    !! The string specifics are a separate three-way split of the generic, each forwarding through
    !! its own path, so a wiring gap here is invisible to every test above.
    subroutine test_join_string_form_new_args(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, r, c
        logical, allocatable :: ms(:), ma(:)
        !
        call build(b, "id", RKEY)
        call build(r, "rid", RKEY)
        ! The array form's answer, with every P5 argument set.
        call build(c, "id", LKEY)
        call c%join(b, ["id"], how="left", require="m:m", order="key", matched=ma)
        ! ... and the string form's, plus an int32 ceiling well clear of the seven rows emitted.
        call build(a, "id", LKEY)
        call a%join(b, "id", how="left", require="m:m", order="key", max_rows=99, matched=ms)
        call same_join(error, a, c, "the string key form with require=, order= and max_rows=")
        if (allocated(error)) return
        call check(error, all(ms .eqv. ma), &
            "the string form's matched= must be the array form's")
        if (allocated(error)) return
        ! The int64 ceiling, through the same path.
        call build(a, "id", LKEY)
        call a%join(b, "id", how="left", require="m:m", order="key", max_rows=99_int64)
        call same_join(error, a, c, "the string key form with an int64 max_rows=")
        if (allocated(error)) return
        ! And with other_on= present, which is join_from_strings' other branch: two name splits
        ! rather than one, and every optional argument forwarded across it.
        call build(a, "id", LKEY)
        call a%join(r, "id", other_on="rid", how="left", require="m:m", order="key", &
            max_rows=99_int64, matched=ms)
        call check(error, a%nrows() == c%nrows(), &
            "a differing right key name must not change which rows are emitted")
        if (allocated(error)) return
        call check(error, all(ms .eqv. ma), "nor which pre-join left rows matched")
        if (allocated(error)) return
        call check(error, a%has_column("rid"), &
            "and a right key whose name differs must arrive as a column of its own")
    end subroutine test_join_string_form_new_args

    !
    !> `%join(how="right")` keeps every row of the OTHER table, so an output row can have no
    !! counterpart here -- and this table's own columns are then null at that row, while the
    !! merged key takes its value from the other side.
    !!
    !! **The merged key is the assertion this test exists for.** Every other column of an
    !! unmatched row is simply null, which a gather plus a validity mask produces; the key is the
    !! one column that must hold a real value the other table supplied, and it is the one place
    !! the key merge is not a free simplification. `seen_unmatched` is the control: without an
    !! unmatched row in the fixture every assertion below the branch is vacuous.
    subroutine test_join_apply_right(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:), key(:), lp(:), rp(:)
        integer(int64) :: n_out, k
        logical :: ok, seen_unmatched
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call a%join(b, ["id"], how="right", pairs=il, other_pairs=ir)
        n_out = size(il, kind=int64)
        call check(error, a%nrows() == n_out, "a right join must emit the counted number of rows")
        if (allocated(error)) return
        call check(error, a%ncols() == 3, "the merged key must leave three columns, not four")
        if (allocated(error)) return
        call a%get("id", key)
        call a%get("payload", lp)
        call a%get("payload_2", rp)
        ok = .true.
        seen_unmatched = .false.
        do k = 1_int64, n_out
            ! Every output row of a right join has a right row, so the incoming column is never
            ! null and `ir` never 0 -- asserted rather than assumed, since it is what makes the
            ! reads below safe.
            if (ir(k) == 0_int64) ok = .false.
            if (a%is_null("payload_2", k)) ok = .false.
            if (rp(k) /= ir(k)) ok = .false.
            if (a%is_null("id", k)) ok = .false.
            if (il(k) == 0_int64) then
                seen_unmatched = .true.
                if (.not. a%is_null("payload", k)) ok = .false.
                ! ... and the key comes from the OTHER table, which is the whole point.
                if (key(k) /= RKEY(ir(k))) ok = .false.
            else
                if (a%is_null("payload", k)) ok = .false.
                if (lp(k) /= il(k)) ok = .false.
                if (key(k) /= LKEY(il(k))) ok = .false.
            end if
        end do
        call check(error, seen_unmatched, &
            "the fixture must contain a right row with no counterpart here, or this is vacuous")
        if (allocated(error)) return
        call check(error, ok, "a right join must null this table's columns and merge the key")
    end subroutine test_join_apply_right
    !
    !> `%join(how="outer")` keeps both sides, so BOTH halves of a row can be missing -- and the
    !! merged key is non-null on every row of the result whichever half that is.
    !!
    !! Also the counting property the design document asks for: `outer = left + right - inner` on
    !! any fixture, because the two one-sided joins each hold the pairs plus their own unmatched
    !! rows. Four independent runs of the counting pass have to agree on one identity, which no
    !! single one of them can be satisfied by alone.
    subroutine test_join_apply_outer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_inner, n_left, n_right, n_outer
        logical :: seen_l, seen_r
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_inner, how="inner")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_left, how="left")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_right, how="right")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_outer, how="outer")
        if (allocated(error)) return
        call check(error, n_inner < n_left .and. n_inner < n_right, &
            "the fixture must have an unmatched row on each side, or the identity below is trivial")
        if (allocated(error)) return
        call check(error, n_outer == n_left + n_right - n_inner, &
            "outer must hold the pairs once and each side's unmatched rows once")
        if (allocated(error)) return
        call a%join(b, ["id"], how="outer", pairs=il, other_pairs=ir)
        n_outer = size(il, kind=int64)
        call check(error, a%nrows() == n_outer, "an outer join must emit the counted rows")
        if (allocated(error)) return
        call check(error, outer_rows_ok(a, il, ir, n_outer, seen_l, seen_r), &
            "an outer join must null whichever half of a row is absent")
        if (allocated(error)) return
        call check(error, seen_l .and. seen_r, &
            "the fixture must reach a row missing on each side, or half of this is vacuous")
        if (allocated(error)) return
        ! And again in KEY order, where the unmatched rows are INTERLEAVED by group rather than
        ! appended at the end. That is the arrangement that decides how the merged key can be
        ! built at all: a fix that patched a contiguous range would pass the arm above and fail
        ! this one, so the two together are what pin the index-based rebuild.
        call build(a, "id", LKEY)
        call a%join(b, ["id"], how="outer", order="key", pairs=il, other_pairs=ir)
        n_outer = size(il, kind=int64)
        call check(error, a%nrows() == n_outer, "order=key must not change how many rows come out")
        if (allocated(error)) return
        call check(error, il(1) == 0_int64 .or. ir(1) == 0_int64 .or. il(n_outer) /= 0_int64, &
            "key order must not simply append the unmatched right rows, or this repeats the arm above")
        if (allocated(error)) return
        call check(error, outer_rows_ok(a, il, ir, n_outer, seen_l, seen_r), &
            "an outer join in key order must null and merge exactly as in left order")
    end subroutine test_join_apply_outer
    !
    !> Every row of an outer-joined table against the pair list that produced it: the half that
    !! is absent is null, the half that is present carries its own row, and the merged key is
    !! never null because one side or the other always supplied it.
    !!
    !! A function rather than four inline blocks: the same assertions are made in both orderings,
    !! and two copies would let one be tightened and the other left behind.
    logical function outer_rows_ok(a, il, ir, n, seen_l, seen_r) result(ok)
        type(parquet_table), intent(inout) :: a  !! the joined table.
        integer(int64), intent(in) :: il(:)      !! per output row: left row, or 0.
        integer(int64), intent(in) :: ir(:)      !! per output row: right row, or 0.
        integer(int64), intent(in) :: n          !! output rows.
        logical, intent(out) :: seen_l           !! a row with no left half was reached.
        logical, intent(out) :: seen_r           !! a row with no right half was reached.
        integer(int64), allocatable :: key(:), lp(:), rp(:)
        integer(int64) :: k
        !
        call a%get("id", key)
        call a%get("payload", lp)
        call a%get("payload_2", rp)
        ok = .true.
        seen_l = .false.
        seen_r = .false.
        do k = 1_int64, n
            ! The key is never null: every output row came from a row on one side or the other,
            ! and that row's key is what the merge takes.
            if (a%is_null("id", k)) ok = .false.
            if (il(k) == 0_int64) then
                seen_l = .true.
                if (.not. a%is_null("payload", k)) ok = .false.
                if (key(k) /= RKEY(ir(k))) ok = .false.
            else
                if (a%is_null("payload", k)) ok = .false.
                if (lp(k) /= il(k)) ok = .false.
                if (key(k) /= LKEY(il(k))) ok = .false.
            end if
            if (ir(k) == 0_int64) then
                seen_r = .true.
                if (.not. a%is_null("payload_2", k)) ok = .false.
            else
                if (a%is_null("payload_2", k)) ok = .false.
                if (rp(k) /= ir(k)) ok = .false.
            end if
        end do
    end function outer_rows_ok
    !
    !> `how="semi"` keeps the rows of this table that matched and `how="anti"` the ones that did
    !! not, and neither brings a single column across.
    !!
    !! **They partition this table**, which is the design document's property test and is what
    !! makes the two assertions here more than a pair of row counts: a rule that put a row in
    !! both, or in neither, satisfies each count on its own and fails the sum. The column count is
    !! asserted for both, because "carries no payload" is the other half of what these two mean.
    subroutine test_join_apply_semi_anti(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, c, b
        integer(int64), allocatable :: sp(:), ap(:)
        !
        call build(b, "id", RKEY)
        call build(a, "id", LKEY)
        call a%join(b, ["id"], how="semi")
        call build(c, "id", LKEY)
        call c%join(b, ["id"], how="anti")
        call check(error, a%nrows() + c%nrows() == size(LKEY, kind=int64), &
            "semi and anti must partition this table's rows between them")
        if (allocated(error)) return
        call check(error, a%nrows() > 0_int64 .and. c%nrows() > 0_int64, &
            "the fixture must put rows on both sides of the partition, or the sum is trivial")
        if (allocated(error)) return
        call check(error, a%ncols() == 2 .and. c%ncols() == 2, &
            "neither semi nor anti may carry a column across")
        if (allocated(error)) return
        call a%get("payload", sp)
        call c%get("payload", ap)
        ! LKEY rows 2, 4 and 5 have a counterpart in RKEY; rows 1 and 3 do not. Written out rather
        ! than derived from the engine, because a row SET is what these two select and the engine
        ! would be answering the same question with the same code.
        call check(error, all(sp == [2_int64, 4_int64, 5_int64]), &
            "semi must keep exactly the rows whose key appears in the other table, in order")
        if (allocated(error)) return
        call check(error, all(ap == [1_int64, 3_int64]), &
            "anti must keep exactly the rows whose key does not, in order")
    end subroutine test_join_apply_semi_anti
    !
    !> A right join whose key names DIFFER keeps both key columns: nothing is merged, so this
    !! table's own key is null where the row has no counterpart here and the incoming key carries
    !! the value.
    !!
    !! The negative half of `test_join_apply_right`: there the key had to hold the other table's
    !! value at an unmatched row, and here it must not, because the two columns are genuinely
    !! different columns rather than one column named twice.
    subroutine test_join_right_other_on(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:), rk(:)
        integer(int64) :: n_out, k
        logical :: ok, seen_unmatched
        !
        call build(a, "id", LKEY)
        call build(b, "rid", RKEY)
        call a%join(b, ["id"], other_on=["rid"], how="right", pairs=il, other_pairs=ir)
        n_out = size(il, kind=int64)
        call check(error, a%nrows() == n_out, "a right join must emit the counted number of rows")
        if (allocated(error)) return
        call check(error, a%ncols() == 4, &
            "two differently named keys are two columns, so nothing is merged away")
        if (allocated(error)) return
        call a%get("rid", rk)
        ok = .true.
        seen_unmatched = .false.
        do k = 1_int64, n_out
            if (rk(k) /= RKEY(ir(k))) ok = .false.
            if (il(k) == 0_int64) then
                seen_unmatched = .true.
                ! Unmerged, so this table's key is as absent as the rest of its row.
                if (.not. a%is_null("id", k)) ok = .false.
            else
                if (a%is_null("id", k)) ok = .false.
            end if
        end do
        call check(error, seen_unmatched, &
            "the fixture must contain a right row with no counterpart here, or this is vacuous")
        if (allocated(error)) return
        call check(error, ok, "an unmerged left key must be null where the row has no left half")
    end subroutine test_join_right_other_on
    !
    !> A right join onto a table with NO rows: every output row is unmatched here, so there is no
    !! row 1 for the gather to have named at all.
    !!
    !! This is the mirror of the zero-row guard the incoming side has had since P3, and it is
    !! reachable by an ordinary program -- an output catalogue built empty and then filled from a
    !! reference table. The merged key still has to work, and it is built from a concatenation
    !! whose left half is empty.
    subroutine test_join_right_empty_left(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: none(:), key(:), rp(:)
        integer(int64) :: k
        logical :: ok
        !
        allocate(none(0))
        call build(a, "id", none)
        call build(b, "id", RKEY)
        call a%join(b, ["id"], how="right")
        call check(error, a%nrows() == size(RKEY, kind=int64), &
            "a right join onto an empty table must keep every row of the other one")
        if (allocated(error)) return
        call a%get("id", key)
        call a%get("payload_2", rp)
        ok = .true.
        do k = 1_int64, a%nrows()
            ! Never read: this table contributed no row, so `payload`'s value bytes are whatever
            ! %init left there and only its null state means anything.
            if (.not. a%is_null("payload", k)) ok = .false.
            if (a%is_null("id", k)) ok = .false.
            if (key(k) /= RKEY(k)) ok = .false.
            if (rp(k) /= k) ok = .false.
        end do
        call check(error, ok, "every row must be null here, and carry the other table's own")
    end subroutine test_join_right_empty_left
    !
    !> `how="semi"` and `how="anti"` are row selections, so the computed detach condition governs
    !! them exactly as it governs a left join -- with no clause of their own anywhere.
    !!
    !! Both arms are needed and each is the other's control. A semi join against a key that covers
    !! every row here selects all of them, which is a `%filter_rows` with an all-true mask: no row
    !! moves, so the table keeps its file. The anti join of the same pair selects none, which
    !! plainly does change the row set. A rule written in terms of `how` rather than of the pair
    !! list would have to get both of those wrong to pass either.
    subroutine test_join_semi_no_detach(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: f = "test_run/join_p6_semi.parquet"
        type(parquet_table) :: src, a, b
        !
        call build(src, "id", LKEY)
        call parquet_write_table(src, f, overwrite=.true.)
        call build(b, "id", ALLKEY)
        call parquet_open_table(a, f)
        call a%join(b, "id", how="semi")
        call check(error, a%nrows() == size(LKEY, kind=int64), &
            "a semi join against a key covering every row must keep every row")
        if (allocated(error)) return
        call check(error, .not. a%is_detached(), &
            "and having moved no row, it must keep its file")
        if (allocated(error)) return
        call parquet_open_table(a, f)
        call a%join(b, "id", how="anti")
        call check(error, a%nrows() == 0_int64, &
            "the anti join of the same pair must keep no row at all")
        if (allocated(error)) return
        call check(error, a%is_detached(), &
            "and having dropped every row, it must detach like any other row mutation")
    end subroutine test_join_semi_no_detach

    !> `pairs=`/`other_pairs=` name the rows of the two tables AS THEY WERE ON ENTRY.
    !!
    !! That is the whole contract, and it is the one a caller applying the same match to an array
    !! the table does not hold depends on: by the time they read `pairs`, `%join` has already
    !! rewritten the left table, so an index into the POST-join rows would be useless and, worse,
    !! would look plausible. The payload column is `k` at pre-join row `k`, which is what pins the
    !! numbering rather than merely being consistent with it.
    !!
    !! `matched=` is the independent cross-check: two separate outputs of one call, computed by
    !! different passes, have to agree about which left rows found a counterpart. Neither can be
    !! satisfied by the other being wrong the same way.
    subroutine test_join_pairs_output(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:), lp(:), rp(:)
        logical, allocatable :: m(:), seen(:)
        integer(int64) :: n, k
        logical :: ok, seen_l0, seen_r0
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call a%join(b, ["id"], how="outer", pairs=il, other_pairs=ir, matched=m)
        n = a%nrows()
        call check(error, allocated(il) .and. allocated(ir), &
            "%join must allocate both halves of the pair list when they are asked for")
        if (allocated(error)) return
        call check(error, size(il, kind=int64) == n .and. size(ir, kind=int64) == n, &
            "both halves must hold one entry per row of the joined table")
        if (allocated(error)) return
        ! Every index is in range for the table it names, or 0.
        ok = .true.
        seen_l0 = .false.
        seen_r0 = .false.
        do k = 1_int64, n
            if (il(k) < 0_int64 .or. il(k) > size(LKEY, kind=int64)) ok = .false.
            if (ir(k) < 0_int64 .or. ir(k) > size(RKEY, kind=int64)) ok = .false.
            if (il(k) == 0_int64) seen_l0 = .true.
            if (ir(k) == 0_int64) seen_r0 = .true.
        end do
        call check(error, ok, "every pair index must name a row of its own table, or be 0")
        if (allocated(error)) return
        call check(error, seen_l0 .and. seen_r0, &
            "the fixture must contain an unmatched row on EACH side, or the 0 convention is " // &
            "untested on one of them")
        if (allocated(error)) return
        ! The numbering is the PRE-join one: payload was k at pre-join row k on both sides.
        call a%get("payload", lp)
        call a%get("payload_2", rp)
        ok = .true.
        do k = 1_int64, n
            if (il(k) == 0_int64) then
                if (.not. a%is_null("payload", k)) ok = .false.
            else if (lp(k) /= il(k)) then
                ok = .false.
            end if
            if (ir(k) == 0_int64) then
                if (.not. a%is_null("payload_2", k)) ok = .false.
            else if (rp(k) /= ir(k)) then
                ok = .false.
            end if
        end do
        call check(error, ok, &
            "output row k must carry pre-join left row pairs(k) beside right row other_pairs(k)")
        if (allocated(error)) return
        ! Cross-check against matched=, which the counting pass fills independently.
        allocate(seen(size(LKEY)))
        seen = .false.
        do k = 1_int64, n
            if (il(k) /= 0_int64 .and. ir(k) /= 0_int64) seen(il(k)) = .true.
        end do
        call check(error, all(seen .eqv. m), &
            "a left row is matched= exactly when the pair list gives it a counterpart")
    end subroutine test_join_pairs_output
    !
    !> Either half can be asked for alone, and asking changes nothing about the joined table.
    !!
    !! Both are handed over with `move_alloc` after the rewrite rather than copied, so a caller
    !! who wants only one still gets it -- and a caller who wants neither pays nothing. The
    !! same-table comparison is what says the request is inert: without it, a `pairs=` that
    !! quietly reordered the output would satisfy every assertion above.
    subroutine test_join_pairs_alone(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c, d
        integer(int64), allocatable :: il(:), ir(:), both_l(:), both_r(:)
        !
        call build(b, "id", RKEY)
        call build(a, "id", LKEY)
        call a%join(b, ["id"], how="left", pairs=il)
        call build(c, "id", LKEY)
        call c%join(b, ["id"], how="left", other_pairs=ir)
        call build(d, "id", LKEY)
        call d%join(b, ["id"], how="left", pairs=both_l, other_pairs=both_r)
        call check(error, allocated(il) .and. allocated(ir), &
            "each half must be allocated by a call that asked for it alone")
        if (allocated(error)) return
        call check(error, size(il, kind=int64) == size(both_l, kind=int64) .and. &
            all(il == both_l), "pairs= alone must hold what it holds beside other_pairs=")
        if (allocated(error)) return
        call check(error, size(ir, kind=int64) == size(both_r, kind=int64) .and. &
            all(ir == both_r), "other_pairs= alone must hold what it holds beside pairs=")
        if (allocated(error)) return
        ! Asking for the pair list must not change the table the join builds.
        call same_join(error, a, d, "a join asked for only half the pair list")
        if (allocated(error)) return
        call same_join(error, c, d, "a join asked for only the other half")
    end subroutine test_join_pairs_alone
    !
    !> A container column on THIS side of a join is carried along with every other column.
    !!
    !! `join_check_left_containers` (`src/parquet_tables_join.f90`) refuses one only for
    !! `how="right"` and `how="outer"`, the two values that can emit a row with no counterpart
    !! here; for every other `how` the column is gathered by the ordinary shared path
    !! (`join_rewrite_left` -> `table_colwork(..., PCW_GATHER, ...)`), the same one `%sort_by` and
    !! `%filter_rows` use. Both REFUSALS have error scenarios (`join_left_container_outer`,
    !! `join_container_payload`); nothing asserted that the permitted case actually works, and
    !! before this test no test in the suite joined a table holding a container column at all.
    !!
    !! **The fingerprint is the row LENGTH, chosen so it names the source row.** Row k holds k
    !! elements and row 5 is null, so the sequence of lengths coming out of the join IS the
    !! sequence of source row indices, and a container left unreordered -- or dropped, or rebuilt
    !! from scratch -- cannot produce it. Values are not read back: `%view` is the only route to
    !! them and its handle would have to be declared `target` at the call site (see
    !! `check_view_call_sites_declare_target`), which buys nothing here that the length and the
    !! nullness do not already pin.
    !!
    !! **Two arms, and the second is the negative control.** The inner join reorders and
    !! DUPLICATES rows, so `il` is far from the identity and the assertion has to see motion. The
    !! `how="left"` arm against ALLKEY matches every left row exactly once and in place, so the
    !! table does not detach and the container must come back UNCHANGED -- which is the same
    !! observation reporting the other outcome. Without it, an implementation that simply left the
    !! container alone would pass the first arm whenever `il` happened to be the identity.
    subroutine test_join_carries_left_container(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c, d
        type(parquet_list_column) :: lc
        type(parquet_list_column), pointer :: lp
        integer(int64), allocatable :: il(:), ir(:), payload(:)
        integer(int64) :: n_out, o, src
        integer(int64) :: want_len(5), got_len(5)
        logical :: want_null(5), got_null(5)
        !
        ! Row k holds k elements; row 5 is null. Length therefore names the source row.
        call lc%init(PK_INT32)
        call lc%append_row([11_int32])
        call lc%append_row([21_int32, 22_int32])
        call lc%append_row([31_int32, 32_int32, 33_int32])
        call lc%append_row([41_int32, 42_int32, 43_int32, 44_int32])
        call lc%append_null_row()
        !
        ! ---- Arm 1: an inner join, which reorders and duplicates rows. ----
        call build(a, "id", LKEY)
        call a%add_column("lst", lc)
        call build(b, "id", RKEY)
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        do o = 1_int64, n_out
            src = il(o)
            want_len(o) = merge(0_int64, src, src == 5_int64)
            want_null(o) = (src == 5_int64)
        end do
        ! The arm proves nothing unless the join really moves rows: if `il` were the identity, a
        ! join that left the container untouched would satisfy every assertion below.
        call check(error, n_out == 5_int64 .and. any(il /= [(o, o = 1_int64, n_out)]), &
            "the inner fixture must reorder rows, or this arm is satisfied by doing nothing")
        if (allocated(error)) return
        !
        call a%join(b, ["id"], how="inner")
        call check(error, a%nrows() == n_out, "the joined table must hold the engine's own row count")
        if (allocated(error)) return
        call check(error, a%kind("lst") == PK_LIST, "the container column must survive the join as a list")
        if (allocated(error)) return
        call a%col("lst", lp)
        do o = 1_int64, n_out
            got_len(o) = lp%length(o)
            got_null(o) = lp%is_null(o)
        end do
        call check(error, all(got_len(1:n_out) == want_len(1:n_out)), &
            "each output row's list must be the source row the pair list named, by its length")
        if (allocated(error)) return
        call check(error, all(got_null(1:n_out) .eqv. want_null(1:n_out)), &
            "and a null source row must arrive null")
        if (allocated(error)) return
        ! Ties the container to the scalar gather: both must name the same source row.
        call a%get("payload", payload)
        call check(error, all(payload(1:n_out) == il(1:n_out)), &
            "the scalar column must name the same source rows the container was gathered by")
        if (allocated(error)) return
        !
        ! ---- Arm 2, the negative control: every row matches once, in place. ----
        call build(c, "id", LKEY)
        call c%add_column("lst", lc)
        call build(d, "id", ALLKEY)
        call c%join(d, ["id"], how="left")
        call check(error, .not. c%is_detached(), &
            "a join in which every row survives once and in place must not detach")
        if (allocated(error)) return
        call check(error, c%nrows() == size(LKEY, kind=int64), &
            "and must leave the row count alone")
        if (allocated(error)) return
        call c%col("lst", lp)
        do o = 1_int64, c%nrows()
            got_len(o) = lp%length(o)
            got_null(o) = lp%is_null(o)
        end do
        call check(error, all(got_len == [1_int64, 2_int64, 3_int64, 4_int64, 0_int64]), &
            "an in-place join must leave the container's rows exactly as they were")
        if (allocated(error)) return
        call check(error, all(got_null .eqv. [.false., .false., .false., .false., .true.]), &
            "including which of them are null")
    end subroutine test_join_carries_left_container
    !
    !> A NaN key is an ordinary VALUE and matches every other NaN; a null key matches nothing.
    !!
    !! The two halves are one test on purpose, because each is the other's negative control. The
    !! fixture is the same in both arms -- same shape, same row count, same 2.0 pair -- and the
    !! only difference is whether the four exceptional entries are NaNs or nulls. So an engine
    !! that matched everything, or one that matched nothing, fails one arm or the other; an
    !! assertion on either arm alone would pass against both.
    !!
    !! Neither is obvious from reading the code and both are silent when broken: the join would go
    !! on returning uniform-looking pairs of the wrong count, which is why
    !! `doc/pages/tables/table-join.md` warns about the NaN case by name ("a column where the
    !! missing values were written as NaN rather than as nulls will match every such row against
    !! every other one"). The engine is `pf_argsort`'s comparator, whose own NaN and null tiers
    !! are tested in `test/test_sorting.f90` -- what is untested there is that a JOIN inherits
    !! them, which is what this asserts.
    !!
    !! The NaN comes from `ieee_value`, never a `transfer` of a bit pattern: nagfor constant-folds
    !! such a transfer and then refuses its own result (`.claude/rules/fortran-gotchas.md`, "-C=undefined").
    subroutine test_join_nan_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        real(real64) :: nan, lk(4), rk(2)
        !
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        lk = [1.0_real64, nan, 2.0_real64, nan]
        rk = [nan, 2.0_real64]
        !
        ! ---- Arm 1: NaNs. Every NaN matches every other one, so left rows 2 and 4 both pair
        ! with right row 1, and 2.0 pairs with 2.0. Row 1 (1.0) has no counterpart and is dropped.
        call build_real(a, "k", lk)
        call build_real(b, "k", rk)
        call pairs_of(error, a, b, ["k"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, n_out == 3_int64, &
            "a NaN key must match every other NaN, giving three inner pairs, not one")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([2_int64, 1_int64, 3_int64, 2_int64, &
            4_int64, 1_int64], [2, 3])), &
            "and the pairs must be (2,1), (3,2), (4,1) in left order")
        if (allocated(error)) return
        !
        ! ---- Arm 2, the control: the SAME four entries as nulls instead. A null matches nothing,
        ! including another null, so only the 2.0 pair survives. Without this arm every assertion
        ! above would also pass against an engine that matched any two exceptional values.
        call build_real(a, "k", lk)
        call build_real(b, "k", rk)
        call a%set_null("k", 2_int64)
        call a%set_null("k", 4_int64)
        call b%set_null("k", 1_int64)
        call pairs_of(error, a, b, ["k"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, n_out == 1_int64, &
            "a null key must match nothing, so only the 2.0 pair survives")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([3_int64, 2_int64], [2, 1])), &
            "and that pair must be (3,2)")
    end subroutine test_join_nan_key
    !
    !> `other`'s file metadata is not merged: the joined table still answers about its OWN file.
    !!
    !! A **claimed absence**, which is the class no ordinary test asserts -- nothing fails when a
    !! future change starts merging the two, and the page states it outright ("`other`'s metadata
    !! is not merged, because there is no defensible rule for what to do with a key both files
    !! define"). The negative control is the same query on `other` itself, which must find the
    !! key: without it the test would pass just as happily against a fixture whose right file
    !! never carried any metadata at all.
    !!
    !! The join is an m:1 left join, so the table keeps its file -- which also pins the easier
    !! half, that a join does not lose its own metadata.
    subroutine test_join_keeps_own_metadata(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        character(len=:), allocatable :: val
        logical :: ok
        character(len=*), parameter :: fa = "test_run/join_meta_left.parquet"
        character(len=*), parameter :: fb = "test_run/join_meta_right.parquet"
        !
        call write_meta_fixture(fa, "left_key", "L", LKEY)
        call write_meta_fixture(fb, "right_key", "R", UKEY)
        call parquet_open_table(a, fa)
        call parquet_open_table(b, fb)
        call b%materialize_all()
        !
        ! The control first, so a later miss cannot be blamed on a fixture that never had the key.
        call b%get_file_metadata("right_key", val, found=ok)
        call check(error, ok .and. val == "R", &
            "precondition: the right file must carry the metadata key this test looks for")
        if (allocated(error)) return
        !
        call a%join(b, "id", how="left", require="m:1")
        call check(error, .not. a%is_detached(), &
            "precondition: an m:1 left join must keep its file, or the metadata question changes")
        if (allocated(error)) return
        call a%get_file_metadata("left_key", val, found=ok)
        call check(error, ok .and. val == "L", &
            "a joined table must still answer for its own source file's metadata")
        if (allocated(error)) return
        call a%get_file_metadata("right_key", val, found=ok)
        call check(error, .not. ok, &
            "and must NOT have merged the right file's metadata into it")
    end subroutine test_join_keeps_own_metadata
    !
    !> `columns=` names what comes across IN ADDITION to the keys, so naming a key is not a way to
    !! get a second copy of it.
    !!
    !! Two rules in one test, because they are the same rule seen from both ends. A key whose name
    !! is the SAME on both sides is merged into one column, and naming it in `columns=` must not
    !! produce `id_2` beside `id` holding identical values (`join_plan_payload` skips it, exactly
    !! as the residency default already did). A key whose name DIFFERS really is another column,
    !! and comes across whatever `columns=` says -- so `columns="payload"` still yields `rid`.
    !!
    !! The negative control is the same join with the key left out of `columns=`: it must give
    !! the identical column set, which is what makes "naming the key changed nothing" a claim
    !! about the code rather than about this fixture.
    subroutine test_join_columns_names_the_key(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, c
        integer(int64), allocatable :: got(:)
        !
        ! ---- Naming the merged key must add nothing. ----
        call build(a, "id", LKEY)
        call build(b, "id", UKEY)
        call b%materialize_all()
        call a%join(b, "id", how="left", columns="id,payload")
        call check(error, .not. a%has_column("id_2"), &
            "naming the merged key in columns= must not carry a second copy of it")
        if (allocated(error)) return
        call check(error, a%ncols() == 3, &
            "the result must hold id, payload and payload_2 and nothing else")
        if (allocated(error)) return
        call a%get("id", got)
        call check(error, all(got == LKEY), "and the key column must still hold this table's keys")
        if (allocated(error)) return
        !
        ! The control: the same join without the key in columns= must give the same columns, so
        ! the assertions above are about the rule and not about this fixture.
        call build(c, "id", LKEY)
        call c%join(b, "id", how="left", columns="payload")
        call check(error, c%ncols() == a%ncols() .and. .not. c%has_column("id_2"), &
            "leaving the key out of columns= must give the identical column set")
        if (allocated(error)) return
        !
        ! ---- A key whose name DIFFERS is a genuine second column, and columns= cannot suppress
        ! it: it is the only place the right-hand key value appears in the result at all.
        call build(a, "id", LKEY)
        call build(b, "rid", UKEY)
        call b%materialize_all()
        call a%join(b, on="id", other_on="rid", how="left", columns="payload")
        call check(error, a%has_column("rid"), &
            "a right key whose name differs must come across even when columns= omits it")
    end subroutine test_join_columns_names_the_key
    !
    !> A two-column table with a `real64` key, for the NaN test. `build`'s int64 key cannot hold
    !! one, and a NaN is the whole point of that fixture.
    subroutine build_real(t, name, keys)
        type(parquet_table), intent(out) :: t !! the table.
        character(len=*), intent(in) :: name  !! the key column's name.
        real(real64), intent(in) :: keys(:)   !! the key values.
        integer(int64), allocatable :: payload(:)
        integer(int64) :: k
        !
        allocate(payload(size(keys)))
        do k = 1_int64, size(keys, kind=int64)
            payload(k) = k
        end do
        call parquet_new_table(t)
        call t%add_column(name, keys)
        call t%add_column("payload", payload)
    end subroutine build_real
    !
    !> Writes a one-column file carrying one metadata key, for the metadata test.
    subroutine write_meta_fixture(fname, key, val, keys)
        character(len=*), intent(in) :: fname !! the file to write.
        character(len=*), intent(in) :: key   !! the metadata key to put in it.
        character(len=*), intent(in) :: val   !! its value.
        integer(int64), intent(in) :: keys(:) !! the `id` column's values.
        type(parquet_writer) :: w
        type(parquet_schema) :: s
        !
        call s%init("jointable", survey="TESTSURVEY")
        call s%add_field("id", "int64")
        call s%add_metadata(key, val)
        call parquet_open_writer(w, fname, s)
        call parquet_write_column(w, "id", keys)
        call parquet_close_writer(w)
    end subroutine write_meta_fixture
    !
    !> A left join that null-fills TWO string payload columns over 200 unmatched rows of 300 --
    !! the shape whose per-row string null-fill was quadratic -- asserting every value and every
    !! null, with the carried values of differing lengths (shortest first). The fixture is small on
    !! purpose: no in-process test can assert a complexity class, and
    !! `bench/benchmark_join.sh --mode=nullfill` is what holds the cost.
    subroutine test_join_left_string_nullfill(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64) :: lk(300), rk(100), k
        character(len=12) :: s1(100), s2(100), ls(300), want
        character(len=:), allocatable :: g1(:), g2(:), gl(:)
        logical :: ok
        integer :: n_null
        !
        do k = 1_int64, 300_int64
            lk(k) = k
            write (ls(k), "(a,i0)") "L", k
        end do
        do k = 1_int64, 100_int64
            rk(k) = 3_int64*k
            write (s1(k), "(a,i0)") "s", k
            write (s2(k), "(a,i0)") "value", k
        end do
        call parquet_new_table(a)
        call a%add_column("id", lk)
        call a%add_column("ls", ls)
        call parquet_new_table(b)
        call b%add_column("id", rk)
        call b%add_column("s1", s1)
        call b%add_column("s2", s2)
        call a%join(b, ["id"], how="left", require="m:1")
        call check(error, a%nrows() == 300_int64, "a left join keeps every left row")
        if (allocated(error)) return
        call a%get("s1", g1)
        call a%get("s2", g2)
        call a%get("ls", gl)
        ok = .true.
        n_null = 0
        do k = 1_int64, 300_int64
            write (want, "(a,i0)") "L", k
            if (a%is_null("ls", k) .or. trim(gl(k)) /= trim(want)) ok = .false.
            if (mod(k, 3_int64) == 0_int64) then
                write (want, "(a,i0)") "s", k/3_int64
                if (a%is_null("s1", k) .or. trim(g1(k)) /= trim(want)) ok = .false.
                write (want, "(a,i0)") "value", k/3_int64
                if (a%is_null("s2", k) .or. trim(g2(k)) /= trim(want)) ok = .false.
            else
                if (.not. a%is_null("s1", k) .or. .not. a%is_null("s2", k)) ok = .false.
                n_null = n_null + 1
            end if
        end do
        call check(error, n_null == 200, "the fixture must leave exactly 200 rows unmatched")
        if (allocated(error)) return
        call check(error, ok, "every matched row carries its two strings and every unmatched row is null in both")
    end subroutine test_join_left_string_nullfill
    !
    !> The pair-count guard at its boundary: `huge - 1` plus 1 is the largest legal total and comes
    !! back exact. One past it is the `join_pair_count_overflow` scenario in test/error_scenarios.f90,
    !! since the refusal is an abort that no join fixture can reach.
    subroutine test_join_add_checked_boundary(error)
        type(error_type), allocatable, intent(out) :: error
        !
        call check(error, parquet_debug_join_add_checked(0_int64, 0_int64) == 0_int64, "0 + 0 is 0")
        if (allocated(error)) return
        call check(error, parquet_debug_join_add_checked(3_int64, 4_int64) == 7_int64, "3 + 4 is 7")
        if (allocated(error)) return
        call check(error, parquet_debug_join_add_checked(huge(0_int64) - 1_int64, 1_int64) == huge(0_int64), &
            "the largest legal total is accepted and exact")
        if (allocated(error)) return
        call check(error, parquet_debug_join_add_checked(huge(0_int64), 0_int64) == huge(0_int64), &
            "adding nothing to the largest total is accepted")
    end subroutine test_join_add_checked_boundary
    !
    ! ======================================================================================
    !  The two engines
    ! ======================================================================================
    !
    !> The engine hook switches engines, and the automatic mode is the hash engine.
    !!
    !! **Without this both join suites are potentially vacuous** -- the shape of
    !! `test_fortran_engine_switches` in test/test_sorting_cpp.f90: if the hook were never read,
    !! or the observable never written, both suites would run one engine and every agreement
    !! assertion would pass while testing nothing. Registered in the hash suite only, which runs
    !! serially: it writes the process-global hook mid-test, which a concurrently running sibling
    !! would see between its own join and its engine assertion.
    !!
    !! The automatic arm pins the rule `join_choose_engine` applies: the hash engine for every
    !! eligible call, this one included. A size clause added to the rule would be the change that
    !! moves this assertion, deliberately, rather than one that fails silently.
    subroutine test_join_engine_hook_switches(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, w
        integer(int64), allocatable :: il1(:), ir1(:), il2(:), ir2(:), il3(:), ir3(:)
        integer :: got_sort, got_hash, got_auto
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call parquet_debug_set_join_engine(int(ENGINE_SORT, c_int64_t))
        call a%clone(w)
        call w%join(b, ["id"], how="outer", pairs=il1, other_pairs=ir1)
        got_sort = int(parquet_debug_join_engine_used())
        call parquet_debug_set_join_engine(int(ENGINE_HASH, c_int64_t))
        call a%clone(w)
        call w%join(b, ["id"], how="outer", pairs=il2, other_pairs=ir2)
        got_hash = int(parquet_debug_join_engine_used())
        call parquet_debug_set_join_engine(0_c_int64_t)
        call a%clone(w)
        call w%join(b, ["id"], how="outer", pairs=il3, other_pairs=ir3)
        got_auto = int(parquet_debug_join_engine_used())
        call restore_suite_engine()
        call check(error, got_sort == ENGINE_SORT, "forcing the sort engine must run the sort engine")
        if (allocated(error)) return
        call check(error, got_hash == ENGINE_HASH, "forcing the hash engine must run the hash engine")
        if (allocated(error)) return
        call check(error, got_auto == ENGINE_HASH, &
            "automatic mode must be the hash engine for an eligible call")
        if (allocated(error)) return
        call check(error, size(il1) == size(il2) .and. size(il1) == size(il3) .and. size(il1) == 8, &
            "the three runs must emit the outer join's eight rows")
        if (allocated(error)) return
        call check(error, all(il1 == il2) .and. all(ir1 == ir2) .and. all(il1 == il3) .and. all(ir1 == ir3), &
            "the two engines must emit the identical pair list")
    end subroutine test_join_engine_hook_switches
    !
    !> The automatic engine choice is a function of the key kinds and `order=` alone: the same
    !! call reports the same engine at `threads=1` and `threads=64`, on a fixture of distinct keys
    !! and on one where every key is the same value, and on a table of 5 rows and one of 5000 --
    !! and an ineligible call reports the sort engine on every one of those. feature_risks.md
    !! Risk-218: a data-dependent choice would make one program's join take different paths on
    !! different inputs, with correct answers, which is a debugging trap that hides behind the
    !! floor.
    subroutine test_join_engine_rule_is_data_independent(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, w
        integer(int64), allocatable :: il(:), ir(:), same(:), big(:)
        integer(int64) :: k
        integer :: got(6)
        !
        call parquet_debug_set_join_engine(0_c_int64_t)
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call a%clone(w)
        call w%join(b, ["id"], pairs=il, other_pairs=ir, threads=1)
        got(1) = int(parquet_debug_join_engine_used())
        call a%clone(w)
        call w%join(b, ["id"], pairs=il, other_pairs=ir, threads=64)
        got(2) = int(parquet_debug_join_engine_used())
        ! Every key one value: the m:m shape a rule reading multiplicity would treat differently.
        allocate(same(5))
        same = 7_int64
        call build(a, "id", same)
        call build(b, "id", same(1:3))
        call a%clone(w)
        call w%join(b, ["id"], pairs=il, other_pairs=ir)
        got(3) = int(parquet_debug_join_engine_used())
        ! Five thousand distinct keys against five: the size a rule with a floor would test.
        allocate(big(5000))
        do k = 1_int64, 5000_int64
            big(k) = k
        end do
        call build(a, "id", big)
        call build(b, "id", UKEY)
        call a%clone(w)
        call w%join(b, ["id"], pairs=il, other_pairs=ir)
        got(4) = int(parquet_debug_join_engine_used())
        call a%clone(w)
        call w%join(b, ["id"], order="key", pairs=il, other_pairs=ir)
        got(5) = int(parquet_debug_join_engine_used())
        call build(b, "id", UKEY)
        call build(a, "id", LKEY)
        call a%clone(w)
        call w%join(b, ["id"], how="anti", require="m:1", threads=1, pairs=il, other_pairs=ir)
        got(6) = int(parquet_debug_join_engine_used())
        call restore_suite_engine()
        call check(error, got(1) == ENGINE_HASH .and. got(2) == ENGINE_HASH, &
            "the automatic choice must be the hash engine at threads=1 and at threads=64 alike")
        if (allocated(error)) return
        call check(error, got(3) == ENGINE_HASH .and. got(4) == ENGINE_HASH, &
            "and the same on an all-equal key and on a 5000-row table: the rule reads no data")
        if (allocated(error)) return
        call check(error, got(5) == ENGINE_SORT, "order=key takes the sort engine under automatic mode")
        if (allocated(error)) return
        call check(error, got(6) == ENGINE_HASH, "how= and require= do not change the engine")
    end subroutine test_join_engine_rule_is_data_independent
    !
    !> `order="key"` always costs the sort engine, under both suites; the same join under the
    !! default order reports the suite's own engine, which is the control that makes the first
    !! half a statement about `order=` rather than about the hook.
    subroutine test_join_order_key_is_the_sort_engines(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, w
        integer(int64), allocatable :: il(:), ir(:)
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call a%clone(w)
        call w%join(b, ["id"], pairs=il, other_pairs=ir)
        call check_engine(error, suite_engine)
        if (allocated(error)) return
        call a%clone(w)
        call w%join(b, ["id"], order="key", pairs=il, other_pairs=ir)
        call check_engine(error, ENGINE_SORT)
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([2_int64, 2_int64, 2_int64, 4_int64, &
            5_int64, 2_int64, 5_int64, 4_int64, 4_int64, 1_int64], [2, 5])), &
            "and still emit the five inner pairs in key order")
    end subroutine test_join_order_key_is_the_sort_engines
    !
    !> A `PK_LOGICAL` key takes the sort engine whatever the hook asks, and the answer is the
    !! brute-force one. The int64 control first.
    subroutine test_join_logical_key_is_the_sort_engines(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, w
        integer(int64), allocatable :: il(:), ir(:)
        !
        call build(a, "id", LKEY)
        call build(b, "id", RKEY)
        call a%clone(w)
        call w%join(b, ["id"], pairs=il, other_pairs=ir)
        call check_engine(error, suite_engine)
        if (allocated(error)) return
        call parquet_new_table(a)
        call a%add_column("flag", [.true., .false., .true.])
        call parquet_new_table(b)
        call b%add_column("flag", [.false., .true.])
        call a%join(b, ["flag"], pairs=il, other_pairs=ir)
        call check_engine(error, ENGINE_SORT)
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 2_int64, 2_int64, 1_int64, &
            3_int64, 2_int64], [2, 3])), &
            "a logical key must match true to true and false to false, in left order")
    end subroutine test_join_logical_key_is_the_sort_engines
    !
    !> A string key BESIDE another key takes the sort engine (the multimap's tuple is
    !! integer-only), while the same string key ALONE is eligible and reports the suite's engine
    !! -- the control that pins the exclusion to the combination rather than to strings.
    subroutine test_join_string_beside_int_key_is_the_sort_engines(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b, w
        integer(int64), allocatable :: il(:), ir(:)
        !
        call parquet_new_table(a)
        call a%add_column("id", [1_int64, 2_int64, 1_int64])
        call a%add_column("s", ["x", "y", "x"])
        call parquet_new_table(b)
        call b%add_column("id", [1_int64, 1_int64])
        call b%add_column("s", ["x", "y"])
        call a%clone(w)
        call w%join(b, ["s"], pairs=il, other_pairs=ir)
        call check_engine(error, suite_engine)
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 2_int64, &
            3_int64, 1_int64], [2, 3])), "the control: the string key alone matches x to x and y to y")
        if (allocated(error)) return
        call a%clone(w)
        call w%join(b, "id,s", pairs=il, other_pairs=ir)
        call check_engine(error, ENGINE_SORT)
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 3_int64, 1_int64], [2, 2])), &
            "a string key beside an integer key must match only where both agree")
    end subroutine test_join_string_beside_int_key_is_the_sort_engines
    !
    !> A null-keyed RIGHT row is in no group of the hash engine's build, and an outer join must
    !! still emit it, unmatched, in right-table order -- the one right-side null shape the sort
    !! engine's group walk handles for free and the hash engine has to re-add by hand.
    subroutine test_join_null_right_key_outer(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        !
        call build(a, "id", [10_int64, 20_int64])
        call build(b, "id", [10_int64, 30_int64, 40_int64])
        call b%set_null("id", 2_int64)
        call check(error, b%is_null("id", 2_int64), "the fixture must hold a null right key")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["id"], il, ir, n_out, how="outer")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 0_int64, &
            0_int64, 2_int64, 0_int64, 3_int64], [2, 4])), &
            "an outer join must emit the null-keyed right row unmatched, in right-table order")
        if (allocated(error)) return
        call check(error, count(il == 0_int64) == 2, &
            "and exactly two right rows -- the null-keyed one and the unmatched one -- have no left half")
    end subroutine test_join_null_right_key_outer
    !
    !> A timestamp key is matched by INSTANT, not by the unit the file stored it in: a
    !! `timestamp[ms]` left table against a `timestamp[us]` right table, where an instant 500
    !! microseconds off matches nothing rather than being rounded onto its neighbour. The hash
    !! engine keys a timestamp as the unit-free (seconds, nanoseconds) tuple, the shape the sort
    !! comparator compares.
    subroutine test_join_timestamp_key_across_units(error)
        type(error_type), allocatable, intent(out) :: error
        character(len=*), parameter :: fl = "test_run/join_ts_units_left.parquet"
        character(len=*), parameter :: fr = "test_run/join_ts_units_right.parquet"
        type(parquet_table) :: a, b
        type(parquet_timestamp) :: lt(3), rt(3)
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        integer :: i
        !
        do i = 1, 3
            call lt(i)%set(2024, 1, 31, 12, 30, i - 1)
        end do
        call rt(1)%set(2024, 1, 31, 12, 30, 2)
        call rt(2)%set(2024, 1, 31, 12, 30, 0)
        call rt(3)%set(2024, 1, 31, 12, 30, 1, 500000)   ! +500 us: matches no left instant
        call write_ts_fixture(fl, "timestamp[ms]", lt)
        call write_ts_fixture(fr, "timestamp[us]", rt)
        call parquet_open_table(a, fl)
        call parquet_open_table(b, fr)
        call check(error, a%nrows() == 3_int64 .and. b%nrows() == 3_int64, "both fixtures hold three rows")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["ts"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 2_int64, 3_int64, 1_int64], [2, 2])), &
            "instants match across units, and the instant 500 us off matches nothing")
    end subroutine test_join_timestamp_key_across_units
    !
    !> Writes a one-column timestamp file in the unit `token` names, for the units test.
    subroutine write_ts_fixture(fname, token, ts)
        character(len=*), intent(in) :: fname        !! the file to write.
        character(len=*), intent(in) :: token        !! `"timestamp[ms]"` or another unit.
        type(parquet_timestamp), intent(in) :: ts(:) !! the column's values.
        type(parquet_writer) :: w
        type(parquet_schema) :: s
        !
        call s%init("join_ts_units")
        call s%add_field("ts", token)
        call parquet_open_writer(w, fname, s)
        call parquet_write_column(w, "ts", ts)
        call parquet_close_writer(w)
    end subroutine write_ts_fixture
    !
    !> A two-column key with a null in a DIFFERENT key column on each side: a row's validity is
    !! the conjunction over its keys, so each null row matches nothing even though its other key
    !! agrees with a row opposite. Under `outer` both come out unmatched; the single-key control
    !! shows the same rows do match on the non-null column alone.
    subroutine test_join_multikey_null_on_each_side(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        !
        call parquet_new_table(a)
        call a%add_column("f", [1_int64, 1_int64, 2_int64, 1_int64])
        call a%add_column("g", [10_int64, 20_int64, 10_int64, 20_int64])
        call a%set_null("f", 2_int64)
        call parquet_new_table(b)
        call b%add_column("f", [1_int64, 1_int64, 2_int64])
        call b%add_column("g", [10_int64, 20_int64, 10_int64])
        call b%set_null("g", 2_int64)
        call pairs_of(error, a, b, ["f", "g"], il, ir, n_out, how="outer")
        if (allocated(error)) return
        ! Left row 4 is (1, 20) and right row 2 is (1, null): they agree on `f` and must not
        ! match; left row 2 is (null, 20) against right row 2's 20 on `g`, and must not either.
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 0_int64, &
            3_int64, 3_int64, 4_int64, 0_int64, 0_int64, 2_int64], [2, 5])), &
            "a null in either key column must leave that row unmatched on both sides")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["f"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, n_out == 5_int64, &
            "the control: on `f` alone the same rows match five ways, so the nulls above did the work")
    end subroutine test_join_multikey_null_on_each_side
    !
    !> A real key matches `-0.0` to `+0.0` and two NaNs of different payload to each other: the
    !! sort comparator's equality, and the one `parquet_index_real_key` canonicalises to. The
    !! fixture asserts its own preconditions first -- that the two zeros and the two NaNs really
    !! differ bit for bit -- or a fixture holding two identical values would pass this for
    !! nothing.
    subroutine test_join_real_key_signed_zero_and_nan_payloads(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        real(real64) :: nan, nan2, pz, nz, lk(3), rk(3)
        !
        nan = ieee_value(1.0_real64, ieee_quiet_nan)
        ! The second payload is the first with its lowest mantissa bit flipped -- still a quiet
        ! NaN, different bits. Built from the RUNTIME value rather than from a literal bit
        ! pattern (nagfor constant-folds the latter and refuses its own result), and not through
        ! `ieee_copy_sign`, which under ifx hands back the same bits for a NaN.
        nan2 = transfer(ieor(transfer(nan, 0_int64), 1_int64), nan)
        pz = 0.0_real64
        nz = ieee_copy_sign(0.0_real64, -1.0_real64)
        call check(error, transfer(nz, 0_int64) /= transfer(pz, 0_int64), &
            "precondition: -0.0 and +0.0 must differ in their bits")
        if (allocated(error)) return
        call check(error, ieee_is_nan(nan2) .and. transfer(nan, 0_int64) /= transfer(nan2, 0_int64), &
            "precondition: the two NaN payloads must differ in their bits and both be NaN")
        if (allocated(error)) return
        lk = [nz, nan, 1.0_real64]
        rk = [pz, nan2, 2.0_real64]
        call build_real(a, "k", lk)
        call build_real(b, "k", rk)
        call pairs_of(error, a, b, ["k"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 2_int64], [2, 2])), &
            "-0.0 must match +0.0 and one NaN must match another, and 1.0 must match nothing")
    end subroutine test_join_real_key_signed_zero_and_nan_payloads
    !
    !> A string key matches by its EXACT bytes: `"ab"` against `"ab "` is a miss, not a hit after
    !! a trim (the sort engine's rule, and the index tier's, feature_risks.md Risk-211). A
    !! `character` array is trimmed on the way into a column, so the padded value is put in with
    !! the scalar `%set_element`, which does not trim -- and the fixture asserts it kept the
    !! blank before anything is joined.
    subroutine test_join_string_key_exact_bytes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_table) :: a, b
        integer(int64), allocatable :: il(:), ir(:)
        integer(int64) :: n_out
        character(len=:), allocatable :: got
        !
        call parquet_new_table(a)
        call a%add_column("s", ["ab", "cd"])
        call a%add_column("payload", [1_int64, 2_int64])
        call parquet_new_table(b)
        call b%add_column("s", ["ab", "xx", "cd"])
        call b%add_column("payload", [1_int64, 2_int64, 3_int64])
        call b%set_element("s", 2_int64, "ab ")
        call b%get_element("s", 2_int64, got)
        call check(error, len(got) == 3, "precondition: the scalar set must keep the trailing blank")
        if (allocated(error)) return
        call check(error, got == "ab ", "precondition: and the value must read back as 'ab '")
        if (allocated(error)) return
        call pairs_of(error, a, b, ["s"], il, ir, n_out, how="inner")
        if (allocated(error)) return
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 3_int64], [2, 2])), &
            "'ab' must match 'ab' and 'cd' match 'cd', and 'ab ' must match neither")
    end subroutine test_join_string_key_exact_bytes
    !
end module test_table_join
