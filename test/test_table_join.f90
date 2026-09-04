!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the JOIN ENGINE -- `table_join_pairs`, reached through the temporary
!! `parquet_debug_table_join_pairs` hook.
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
!! Abort paths live in test/error_scenarios.f90 as `join_*` scenarios, since they kill the
!! process. Nothing here writes a file, so no test needs its own fixture path.
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
                test_join_require_ok) &
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
end module test_table_join
