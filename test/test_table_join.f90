!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the JOIN: the engine (`table_join_pairs`, reached through the temporary
!! `parquet_debug_table_join_pairs` hook) and `%join`'s own column rewrite on top of it.
!!
!! **The engine is tested before any column is rewritten, and that separation is the point.**
!! Everything that can make a join produce a silent wrong answer -- the null rule, the output
!! count, the cardinality assertion, the ordering -- is arithmetic on row indices, so it can be
!! asserted directly here rather than inferred from a joined table two phases later. When
!! `%join`'s own `pairs=` output lands the hook goes away and these tests move onto it.
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
!! Abort paths live in test/error_scenarios.f90 as `join_*` scenarios, since they kill the
!! process. Two tests here write a fixture, each to its own path -- the suite runs its tests
!! concurrently, so a shared one would be truncated out from under the other.
module test_table_join
    use parquet
    use iso_fortran_env, only : int32, int64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_join
    !
    !> The shared fixture's left key column. Chosen so that every case is present: a key with no
    !! counterpart (30, 99), a key with two (10), a key with one (20), and a repeated left key.
    integer(int64), parameter :: LKEY(5) = [30_int64, 10_int64, 99_int64, 20_int64, 10_int64]
    !> The shared fixture's right key column: one unmatched (40) and one duplicated (10).
    integer(int64), parameter :: RKEY(4) = [20_int64, 10_int64, 40_int64, 10_int64]
    !> A right key with NO duplicate, so a left join against it leaves every left row exactly
    !! where it was. `RKEY` is the same fixture with 10 repeated, which is what makes the two a
    !! matched pair: the non-detaching tests below use this one and their negative controls use
    !! `RKEY`, so the only difference between the two arms is the uniqueness of one key.
    integer(int64), parameter :: UKEY(3) = [10_int64, 20_int64, 40_int64]
    !
contains
    !
    !> Collects this suite's tests.
    subroutine collect_tests_table_join(testsuite)
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
            new_unittest("the separated-string key form joins as the array form does", &
                test_join_string_form), &
            new_unittest("columns= and residency, all four combinations", &
                test_join_columns_residency), &
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
                test_join_units) &
            ]
    end subroutine collect_tests_table_join
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 0_int64, 2_int64, 2_int64, &
            2_int64, 4_int64, 3_int64, 0_int64, 4_int64, 1_int64, 5_int64, 2_int64, &
            5_int64, 4_int64], [2, 7])), "how=left must keep every left row, matched or not")
        if (allocated(error)) return
        call parquet_debug_table_join_pairs(a, b, ["id"], how="right", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([2_int64, 2_int64, 2_int64, 4_int64, &
            4_int64, 1_int64, 5_int64, 2_int64, 5_int64, 4_int64, 0_int64, 3_int64], [2, 6])), &
            "how=right must append the unmatched right rows in right-table order")
        if (allocated(error)) return
        call parquet_debug_table_join_pairs(a, b, ["id"], how="outer", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 0_int64, 2_int64, 2_int64, &
            2_int64, 4_int64, 3_int64, 0_int64, 4_int64, 1_int64, 5_int64, 2_int64, &
            5_int64, 4_int64, 0_int64, 3_int64], [2, 8])), &
            "how=outer must keep every row from both sides")
        if (allocated(error)) return
        call parquet_debug_table_join_pairs(a, b, ["id"], how="semi", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([2_int64, 0_int64, 4_int64, 0_int64, &
            5_int64, 0_int64], [2, 3])), &
            "how=semi must emit each matching left row ONCE, however many matches it has")
        if (allocated(error)) return
        call parquet_debug_table_join_pairs(a, b, ["id"], how="anti", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 0_int64, 3_int64, 0_int64], [2, 2])), &
            "how=anti must emit exactly the left rows with no counterpart")
        if (allocated(error)) return
        ! The default is "inner", and an unrecognized token aborts (a join_* error scenario).
        call parquet_debug_table_join_pairs(a, b, ["id"], il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, n_out=n_out)
        call check(error, n_out == 3_int64, &
            "the control: three equal keys on each side must give three pairs")
        if (allocated(error)) return
        ! Now null row 2 on the left and row 3 on the right. Row 2 is null on ONE side and row 3
        ! on the other, so between them they cover null-vs-value, value-vs-null and, when both
        ! are nulled below, null-vs-null.
        call a%set_null("id", 2_int64)
        call b%set_null("id", 3_int64)
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64], [2, 1])), &
            "a null key must match nothing: only row 1 is valid on both sides")
        if (allocated(error)) return
        ! Null the SAME row on both sides: two nulls of what was the same value must not match.
        call b%set_null("id", 2_int64)
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64], [2, 1])), &
            "two nulls must not match each other -- unknown is not equal to unknown")
        if (allocated(error)) return
        ! And a null-keyed left row is an UNMATCHED left row, not a dropped one.
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 1_int64, 2_int64, 0_int64, &
            3_int64, 0_int64], [2, 3])), &
            "how=left must keep a null-keyed left row, with no counterpart")
        if (allocated(error)) return
        ! And the mirror image, which is the ONLY shape that exercises the right-hand null rule:
        ! a null group holding rows from BOTH sides. `how=inner` and `how=left` never look at
        ! whether a right row matched, so dropping the null test on that side is invisible to
        ! them -- confirmed by mutation. Right rows 2 and 3 are both null here (the null tier
        ! collects them whatever value they used to hold), so both must come back unmatched.
        call parquet_debug_table_join_pairs(a, b, ["id"], how="outer", il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="right", il=il, ir=ir, n_out=n_out)
        call check(error, seq_is(il, ir, reshape([1_int64, 3_int64, 2_int64, 1_int64, &
            0_int64, 2_int64, 0_int64, 4_int64], [2, 4])), &
            "order=left must append the unmatched right rows in right-table order, not key order")
        if (allocated(error)) return
        ! order=key is where the other sequence is correct: each unmatched right row sits in its
        ! own group's place, so 40 precedes 99.
        call parquet_debug_table_join_pairs(a, b, ["id"], how="right", order="key", &
            il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", order="key", &
            il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, &
            n_out=n_out, matched=matched)
        call check(error, size(matched) == size(LKEY), &
            "matched= must have one entry per PRE-join left row, not per output row")
        if (allocated(error)) return
        call check(error, all(matched .eqv. [.false., .true., .false., .true., .true.]), &
            "matched= must mark exactly the left rows that found a counterpart")
        if (allocated(error)) return
        call check(error, count(matched) == 3, "three of the five left rows have a counterpart")
        if (allocated(error)) return
        ! It does not depend on `how`: the question is about the left table, not the output.
        call parquet_debug_table_join_pairs(a, b, ["id"], how="anti", il=il, ir=ir, &
            n_out=n_out, matched=matched)
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
        call parquet_debug_table_join_pairs(a, b, ["f", "g"], how="inner", il=il, ir=ir, n_out=n_out)
        ! (1,10) meets right row 3; (1,20) meets right row 1; (2,10) meets nothing -- right row 2
        ! is (2,20), which agrees on `f` alone and must NOT match.
        call check(error, seq_is(il, ir, reshape([1_int64, 3_int64, 2_int64, 1_int64], [2, 2])), &
            "a two-column key must match only where both columns agree")
        if (allocated(error)) return
        ! The control: on `f` alone the same fixture matches far more widely, so the assertion
        ! above is about the second key rather than about the fixture being sparse.
        call parquet_debug_table_join_pairs(a, b, ["f"], how="inner", il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], other_on=["ref_id"], how="inner", &
            il=il, ir=ir, n_out=n_out)
        call check(error, n_out == 5_int64, "other_on= must resolve the right table's own name")
        if (allocated(error)) return
        ! Identical to the same-name join, which is the only way to show other_on= changed the
        ! lookup and nothing else.
        call build(b, "id", RKEY)
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il2, ir=ir2, n_out=n2)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, n_out=n_out)
        call check(error, n_out == 0_int64 .and. size(il) == 0, &
            "nothing can match against an empty right table")
        if (allocated(error)) return
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", il=il, ir=ir, &
            n_out=n_out, matched=matched)
        call check(error, n_out == size(LKEY, kind=int64), &
            "how=left against an empty right table must keep every left row")
        if (allocated(error)) return
        call check(error, .not. any(matched), "no left row can have matched an empty table")
        if (allocated(error)) return
        call build(a, "id", none)
        call build(b, "id", RKEY)
        call parquet_debug_table_join_pairs(a, b, ["id"], how="outer", il=il, ir=ir, n_out=n_out)
        call check(error, n_out == size(RKEY, kind=int64), &
            "an empty left table under how=outer must leave the right rows unmatched")
        if (allocated(error)) return
        call build(b, "id", none)
        call parquet_debug_table_join_pairs(a, b, ["id"], how="outer", il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", require="m:1", &
            il=il, ir=ir, n_out=n_out)
        call check(error, n_out == 5_int64, "require='m:1' must accept a unique right key")
        if (allocated(error)) return
        ! Case folding, and the same assertion spelled the other way round.
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", require="M:1", &
            il=il, ir=ir, n_out=n_out)
        call check(error, n_out == 5_int64, "require= must be matched case-insensitively")
        if (allocated(error)) return
        ! Two NULL right keys are not a duplicate key: neither can match anything.
        call build(b, "id", [20_int64, 40_int64, 50_int64])
        call b%set_null("id", 2_int64)
        call b%set_null("id", 3_int64)
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", require="m:1", &
            il=il, ir=ir, n_out=n_out)
        call check(error, n_out == 5_int64, &
            "two null right keys must not trip require='m:1' -- neither can match anything")
        if (allocated(error)) return
        ! The control: make them a genuine duplicate VALUE and the same call must abort. That
        ! abort is join_require_m1 in test/error_scenarios.f90; here we only prove the fixture
        ! above is one edit away from tripping it, by showing the duplicate really is joinable.
        call build(b, "id", [20_int64, 20_int64])
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="inner", il=il, ir=ir, n_out=n_out)
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
        call parquet_debug_table_join_pairs(a, b, ["id"], how="left", il=il, ir=ir, n_out=n_out)
        call a%join(b, ["id"], how="left")
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
    !! SILENT (`feature_risks.md` R-f): a join that carried no payload, followed by a schema-less
    !! `parquet_write_table`, emits a valid file quietly missing columns. Case (3) is the negative
    !! control that stops "carry everything" passing as "carry what is resident".
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

end module test_table_join
