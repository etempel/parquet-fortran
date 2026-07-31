!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the row-group statistics pre-screen: the footer-only screen that skips row
!> groups a filter provably cannot match, so a filtered read never touches them.
!>
!> The screen is an I/O optimization that must change NO answer, which makes it the one part of
!> the filter stage whose failure mode is a silent wrong result rather than an abort. Two things
!> follow, and they shape every test here:
!>
!> * **Correctness is tested as an EQUALITY, not against hand-written expectations.** Almost every
!>   test reads the same fixture twice under the same expression -- once normally, once with
!>   parquet_debug_set_disable_statistics_prescreen(1) -- and asserts the two results are
!>   element-wise identical. A hand-written expectation only proves the answer matches what the
!>   test author believed; the A/B comparison proves pruning changed nothing, which is the actual
!>   contract.
!> * **Every case also asserts how many row groups were pruned.** Equality alone passes just as
!>   happily against a screen that never prunes anything at all, so a screen that silently stopped
!>   working would look perfectly healthy. Cases that must prune assert a nonzero count; cases the
!>   screen is required to DECLINE (statistics absent, an unsupported column type, a float column
!>   under `not`, ...) assert exactly zero.
!>
!> **This suite runs its tests SEQUENTIALLY** (test/run_tester.f90's suite_is_safe_to_parallelize
!> excludes it). Both debug hooks are process-global -- parquet_reader's components are private, so
!> a test cannot pass a reader handle to a per-reader query, and a pruned-row-group count does not
!> belong in the public API just to make one testable. Running concurrently would let one test read
!> another's count.
!>
!> Every test writes its own fixture under test_run/, with its own filename, for the reason
!> CLAUDE.md gives: shared fixture paths fail intermittently and a green run does not disprove it.
module test_filter_screen
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use iso_c_binding, only : c_int, c_long_long
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_filter_screen
    !
    !> The two test-only hooks into the screen (parquet_wrapper.cpp). Declared locally here rather
    !> than in src/parquet_bindings.f90 -- the same convention every other parquet_debug_* hook
    !> follows, so no debug entry point ever becomes part of the library's own interface.
    interface
        !> Forces every row group live, so the same fixture can be read with and without pruning.
        subroutine parquet_debug_set_disable_statistics_prescreen(enable) &
            bind(C, name="parquet_debug_set_disable_statistics_prescreen")
            import :: c_int
            integer(c_int), value :: enable !! nonzero disables the screen.
        end subroutine parquet_debug_set_disable_statistics_prescreen
        !> How many row groups the most recent screen in this process ruled out.
        function parquet_debug_get_row_groups_pruned() result(res) &
            bind(C, name="parquet_debug_get_row_groups_pruned")
            import :: c_long_long
            integer(c_long_long) :: res !! pruned row-group count of the last screen.
        end function parquet_debug_get_row_groups_pruned
    end interface
    !
contains
    !
    subroutine collect_tests_filter_screen(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("a selective == prunes all but one row group", test_equality_prunes), &
            new_unittest("range operators prune the row groups outside them", test_range_operators_prune), &
            new_unittest("every operator agrees with an unpruned read", test_all_operators_equality), &
            new_unittest("and/or/not and a nested expression agree with an unpruned read", &
                test_expression_shapes_equality), &
            new_unittest("and prunes when either side rules the row group out", test_and_prunes), &
            new_unittest("or prunes only when both sides rule it out", test_or_prunes), &
            new_unittest("not swaps which row groups survive", test_not_prunes), &
            new_unittest("a float column agrees with an unpruned read", test_float_equality), &
            new_unittest("a float column under not never prunes (NaN is invisible to min/max)", &
                test_float_not_declines), &
            new_unittest("float /= never prunes", test_float_not_equal_declines), &
            new_unittest("a NaN-bearing float column agrees with an unpruned read", test_float_nan_equality), &
            new_unittest("is_nan / is_not_nan agree with an unpruned read and never prune", test_is_nan_declines), &
            new_unittest("a string column prunes and agrees with an unpruned read", test_string_equality), &
            new_unittest("a boolean column agrees with an unpruned read", test_bool_equality), &
            new_unittest("a date column prunes on an ISO literal", test_temporal_equality), &
            new_unittest("is_null / is_not_null prune on the footer null count", test_null_tests_prune), &
            new_unittest("a struct-leaf path agrees with an unpruned read", test_struct_leaf_equality), &
            new_unittest("a file without statistics is never pruned", test_no_stats_declines), &
            new_unittest("a column whose bounds exceed the statistics size limit is never pruned", &
                test_oversized_bounds_decline), &
            new_unittest("an unsupported column type is never pruned", test_unsupported_type_declines), &
            new_unittest("the answer is unchanged when every row group is pruned", test_prune_everything), &
            new_unittest("chunked reads still visit every row group under pruning", test_chunked_reads_under_pruning), &
            new_unittest("row and element mode agree with an unpruned read under pruning", &
                test_row_element_mode_under_pruning), &
            new_unittest("sample_fraction composes with pruning", test_sample_composes), &
            new_unittest("sort_by composes with pruning", test_sort_composes), &
            new_unittest("a scoped filter prunes its out-of-range row groups", test_scoped_filter_prunes), &
            new_unittest("prefetch and a second column read see the pruned row set", test_prefetch_under_pruning), &
            new_unittest("parquet_get_num_row_groups still reports every row group", test_num_row_groups_unchanged) &
            ]
    end subroutine collect_tests_filter_screen
    !
    !> Writes the workhorse fixture: `nrows` rows in row groups of `chunk`, with a monotone `id`
    !> so every row group owns a known, disjoint id range (row group k holds ids
    !> (k-1)*chunk+1 .. k*chunk). That is what makes an expected pruned count something a test can
    !> state exactly rather than approximate. `payload` is a second, deliberately unfiltered column,
    !> so a test can check that reading it afterwards also sees the pruned row set.
    subroutine write_screen_fixture(file, nrows, chunk)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        integer, intent(in) :: nrows !! total rows.
        integer, intent(in) :: chunk !! rows per row group.
        type(parquet_writer) :: writer
        integer(int32), allocatable :: id(:), payload(:)
        integer :: i

        allocate(id(nrows), payload(nrows))
        do i = 1, nrows
            id(i) = i
            payload(i) = 1000 + i
        end do
        call parquet_open_writer(writer, file, chunk_size=chunk)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "payload", payload)
        call parquet_close_writer(writer)
    end subroutine write_screen_fixture
    !
    !> Reads `column` of `file` under `rule` with the screen either on or off, returning the
    !> surviving values and how many row groups were pruned. The single primitive every equality
    !> test below is built from.
    subroutine read_under_rule(file, rule, column, screen_on, values, pruned)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: rule !! the filter expression.
        character(len=*), intent(in) :: column !! column to read back.
        logical, intent(in) :: screen_on !! .false. disables the statistics screen.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows of `column`.
        integer(int64), intent(out) :: pruned !! row groups the screen ruled out.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows

        call parquet_debug_set_disable_statistics_prescreen(merge(0_c_int, 1_c_int, screen_on))
        call filt%add(rule)
        call parquet_open_reader(reader, file, filter=filt)
        pruned = parquet_debug_get_row_groups_pruned()
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, column, values)
        call parquet_close_reader(reader)
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)
    end subroutine read_under_rule
    !
    !> The equality harness itself: reads `column` twice under `rule`, once screened and once not,
    !> and reports whether the two agree exactly plus the screened run's pruned count. Every
    !> correctness test in this suite goes through here.
    subroutine compare_screened(file, rule, column, agree, pruned, nrows)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: rule !! the filter expression.
        character(len=*), intent(in) :: column !! column to read back.
        logical, intent(out) :: agree !! .true. if screened and unscreened results are identical.
        integer(int64), intent(out) :: pruned !! row groups the screened run ruled out.
        integer, intent(out) :: nrows !! surviving row count (identical either way when agree).
        integer(int32), allocatable :: screened(:), plain(:)
        integer(int64) :: pruned_off

        call read_under_rule(file, rule, column, .true., screened, pruned)
        call read_under_rule(file, rule, column, .false., plain, pruned_off)
        nrows = size(screened)
        agree = (size(screened) == size(plain))
        if (agree) then
            if (size(screened) > 0) agree = all(screened == plain)
        end if
    end subroutine compare_screened
    !
    !> The headline case: `id == k` can match in exactly one row group, and the screen must rule
    !> out every other one from the footer alone.
    subroutine test_equality_prunes(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_equality.parquet"

        call write_screen_fixture(file, 100, 10)
        call compare_screened(file, "id == 55", "id", agree, pruned, nrows)
        call check(error, agree, "id == 55: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 1, "id == 55: expected exactly one surviving row")
        if (allocated(error)) return
        call check(error, pruned == 9_int64, "id == 55: expected 9 of 10 row groups pruned")
    end subroutine test_equality_prunes
    !
    !> A range operator prunes the row groups entirely on the wrong side of the bound, and keeps
    !> the one it straddles. Both directions, so an inverted comparison in the leaf rules shows up.
    subroutine test_range_operators_prune(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_range.parquet"

        call write_screen_fixture(file, 100, 10)
        ! Row groups hold 1..10, 11..20, ... 91..100; id > 75 can only match in the last three.
        call compare_screened(file, "id > 75", "id", agree, pruned, nrows)
        call check(error, agree, "id > 75: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 25, "id > 75: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "id > 75: expected 7 of 10 row groups pruned")
        if (allocated(error)) return
        call compare_screened(file, "id <= 25", "id", agree, pruned, nrows)
        call check(error, agree, "id <= 25: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 25, "id <= 25: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "id <= 25: expected 7 of 10 row groups pruned")
    end subroutine test_range_operators_prune
    !
    !> Every operator, each against a bound that lands exactly ON a row-group boundary value --
    !> the placement that catches an off-by-one in the leaf rules (`hi > v` written as `hi >= v`
    !> and so on), which a bound in the middle of a row group would not.
    subroutine test_all_operators_equality(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        character(len=16) :: ops(6) = [character(len=16) :: ">", ">=", "<", "<=", "==", "/="]
        character(len=*), parameter :: file = "test_run/screen_all_ops.parquet"

        call write_screen_fixture(file, 100, 10)
        do i = 1, 6
            ! 20 is the last id of row group 2 and 21 the first of row group 3, so a bound of 20
            ! sits on the boundary for every one of the six.
            call compare_screened(file, "id " // trim(ops(i)) // " 20", "id", agree, pruned, nrows)
            call check(error, agree, "operator " // trim(ops(i)) // ": pruned and unpruned reads must be identical")
            if (allocated(error)) return
        end do
    end subroutine test_all_operators_equality
    !
    !> The combinators and a parenthesised nesting, each compared against its own unpruned read.
    !> These are what exercise screen_combine/screen_negate rather than the leaf rules.
    subroutine test_expression_shapes_equality(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        character(len=64) :: rules(5) = [character(len=64) :: &
            "id > 30 and id < 45", &
            "id < 15 or id > 88", &
            "not (id > 30)", &
            "(id > 20 and id < 40) or id == 95", &
            "not (id < 10 or id > 90)"]
        character(len=*), parameter :: file = "test_run/screen_shapes.parquet"

        call write_screen_fixture(file, 100, 10)
        do i = 1, 5
            call compare_screened(file, trim(rules(i)), "id", agree, pruned, nrows)
            call check(error, agree, "expression '" // trim(rules(i)) // "': pruned and unpruned reads must agree")
            if (allocated(error)) return
            call check(error, pruned > 0_int64, "expression '" // trim(rules(i)) // "': expected some pruning")
            if (allocated(error)) return
        end do
    end subroutine test_expression_shapes_equality
    !
    !> and prunes a row group when EITHER side rules it out -- the intersection of two ranges keeps
    !> only the row groups both admit. Note the screen's and is deliberately an over-approximation
    !> ("some row satisfies a, some row satisfies b", not necessarily the same row), so it can only
    !> ever prune less than a perfect oracle, never more.
    subroutine test_and_prunes(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_and.parquet"

        call write_screen_fixture(file, 100, 10)
        ! id > 30 admits row groups 4..10; id < 45 admits 1..5; together only 4 and 5.
        call compare_screened(file, "id > 30 and id < 45", "id", agree, pruned, nrows)
        call check(error, agree, "and: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 14, "and: expected ids 31..44 to survive")
        if (allocated(error)) return
        call check(error, pruned == 8_int64, "and: expected 8 of 10 row groups pruned")
    end subroutine test_and_prunes
    !
    !> or prunes only the row groups BOTH sides rule out -- the union of the two ranges. A screen
    !> whose or wrongly intersected would prune the middle row groups here and lose their rows.
    subroutine test_or_prunes(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_or.parquet"

        call write_screen_fixture(file, 100, 10)
        ! id < 15 admits row groups 1 and 2; id > 88 admits 9 and 10; the six in between go.
        call compare_screened(file, "id < 15 or id > 88", "id", agree, pruned, nrows)
        call check(error, agree, "or: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 26, "or: expected ids 1..14 plus 89..100 to survive")
        if (allocated(error)) return
        call check(error, pruned == 6_int64, "or: expected 6 of 10 row groups pruned")
    end subroutine test_or_prunes
    !
    !> not must swap which row groups a leaf admits: `not (id > 30)` keeps exactly the row groups
    !> `id > 30` would have pruned. A screen whose negate did not swap may_true and may_false would
    !> keep the wrong half -- and, worse, prune row groups that do match.
    subroutine test_not_prunes(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_not.parquet"

        call write_screen_fixture(file, 100, 10)
        call compare_screened(file, "not (id > 30)", "id", agree, pruned, nrows)
        call check(error, agree, "not: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 30, "not: expected ids 1..30 to survive")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "not: expected 7 of 10 row groups pruned")
    end subroutine test_not_prunes
    !
    !> Writes a float fixture with a monotone `x` (so row groups own disjoint ranges, as with `id`)
    !> and an integer `u` identifying each row, plus an optional NaN in a known row group.
    subroutine write_float_fixture(file, with_nan)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        logical, intent(in) :: with_nan !! .true. puts a NaN in row group 3.
        type(parquet_writer) :: writer
        real(real64) :: x(100)
        real(real32) :: xf(100)
        integer(int32) :: u(100)
        integer :: i

        do i = 1, 100
            x(i) = real(i, real64)
            u(i) = i
        end do
        ! Row 25 sits in row group 3 (ids 21..30), whose bounds are then min=21, max=30 with the
        ! NaN excluded -- so the NaN is invisible to every bound the screen can see, which is
        ! exactly the situation the float rules exist for.
        if (with_nan) x(25) = ieee_value(0.0_real64, ieee_quiet_nan)
        ! A float32 twin of the same data: its bounds arrive as FloatStatistics rather than
        ! DoubleStatistics, which is a separate branch of the screen's leaf rules.
        xf = real(x, real32)
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "x", x)
        call parquet_write_column(writer, "xf", xf)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
    end subroutine write_float_fixture
    !
    !> A plain float comparison prunes exactly as an integer one does; DoubleStatistics is a
    !> different branch of the leaf rules from Int64Statistics, so it needs its own case.
    subroutine test_float_equality(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_float.parquet"

        call write_float_fixture(file, .false.)
        call compare_screened(file, "x > 75.0", "u", agree, pruned, nrows)
        call check(error, agree, "float >: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 25, "float >: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "float >: expected 7 of 10 row groups pruned")
        if (allocated(error)) return
        ! float32: the same expression against the FloatStatistics branch.
        call compare_screened(file, "xf > 75.0", "u", agree, pruned, nrows)
        call check(error, agree, "float32 >: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 25, "float32 >: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "float32 >: expected 7 of 10 row groups pruned")
    end subroutine test_float_equality
    !
    !> THE subtlest rule in the screen. Parquet excludes NaN from min/max and records no NaN count,
    !> so for a float column the bounds can never prove a comparison is false everywhere -- a NaN
    !> row makes every comparison false while sitting outside [min, max]. `not` consumes exactly
    !> that "could be false" answer, so a float leaf under `not` must never prune.
    !>
    !> The fixture has no NaN at all, deliberately: the screen cannot tell that from the footer, so
    !> the rule has to hold for every float column, and asserting zero pruning here proves the
    !> screen is not relying on a fact it cannot check.
    subroutine test_float_not_declines(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_float_not.parquet"

        call write_float_fixture(file, .false.)
        call compare_screened(file, "not (x > 75.0)", "u", agree, pruned, nrows)
        call check(error, agree, "float under not: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 75, "float under not: expected 75 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, &
            "float under not: must prune nothing -- a NaN row is false and invisible to min/max")
    end subroutine test_float_not_declines
    !
    !> The other half of the same rule: NaN /= anything is TRUE, so a row group whose statistics say
    !> min == max == v can still contain matching rows. Pruning float /= would drop them.
    subroutine test_float_not_equal_declines(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        real(real64) :: x(40)
        integer(int32) :: u(40)
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/screen_float_ne.parquet"

        ! The fixture this rule actually needs: a row group that is CONSTANT at the filtered value
        ! and also holds a NaN. Row group 2 (rows 11..20) is all 42.0 except row 15, which is NaN,
        ! so its statistics say min = max = 42.0 exactly -- "every value here equals 42" -- while
        ! row 15 genuinely satisfies "x /= 42" (NaN /= anything is true). A screen allowed to prune
        ! float /= drops that row group and loses row 15.
        !
        ! A monotone fixture would NOT catch this: with min < max no row group is ever constant at
        ! the bound, so the rule is never consulted and the mutation survives.
        do i = 1, 40
            x(i) = real(i, real64)
            u(i) = i
        end do
        x(11:20) = 42.0_real64
        x(15) = ieee_value(0.0_real64, ieee_quiet_nan)
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "x", x)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)

        call compare_screened(file, "x /= 42.0", "u", agree, pruned, nrows)
        call check(error, agree, "float /=: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "float /=: must prune nothing -- NaN /= v is true")
        if (allocated(error)) return
        call read_under_rule(file, "x /= 42.0", "u", .true., got, pruned)
        ! Rows 1..10 and 21..40 differ from 42; row 15 is the NaN. Rows 11..14 and 16..20 are 42.
        call check(error, size(got) == 31, "float /=: expected 30 differing rows plus the NaN row")
        if (allocated(error)) return
        call check(error, any(got == 15), "float /=: the NaN row (u = 15) must survive x /= 42.0")
    end subroutine test_float_not_equal_declines
    !
    !> The counter-example that made the float rule necessary, as an actual fixture: row group 3
    !> holds a NaN whose row IS in the result of `not (x > 20.5)` (NaN compares false, and false
    !> negates to true) while that row group's own bounds say min = 21 > 20.5, i.e. "nothing here
    !> can be false". A screen taking may_false from the bounds prunes row group 3 and loses row 25.
    subroutine test_float_nan_equality(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/screen_float_nan.parquet"

        call write_float_fixture(file, .true.)
        call compare_screened(file, "not (x > 20.5)", "u", agree, pruned, nrows)
        call check(error, agree, "NaN under not: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "NaN under not: a float column under not must not prune")
        if (allocated(error)) return
        ! Rows 1..20 (x <= 20.5) plus row 25 (NaN: x > 20.5 is false, so not(...) is true).
        call read_under_rule(file, "not (x > 20.5)", "u", .true., got, pruned)
        call check(error, size(got) == 21, "NaN under not: expected 21 surviving rows (20 below the bound, plus the NaN)")
        if (allocated(error)) return
        call check(error, got(21) == 25, "NaN under not: the NaN row (u = 25) must be the last survivor")
    end subroutine test_float_nan_equality
    !
    !> is_nan/is_not_nan can never prune: Parquet records no NaN count and excludes NaN from
    !> min/max, so no footer field can prove either that a chunk holds a NaN or that it holds none.
    subroutine test_is_nan_declines(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_is_nan.parquet"

        call write_float_fixture(file, .true.)
        call compare_screened(file, "x is_nan", "u", agree, pruned, nrows)
        call check(error, agree, "is_nan: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 1, "is_nan: expected the one NaN row")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "is_nan: must prune nothing -- no footer field counts NaNs")
        if (allocated(error)) return
        call compare_screened(file, "x is_not_nan", "u", agree, pruned, nrows)
        call check(error, agree, "is_not_nan: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 99, "is_not_nan: expected the 99 non-NaN rows")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "is_not_nan: must prune nothing either")
    end subroutine test_is_nan_declines
    !
    !> A string column's bounds are unsigned-byte-ordered ByteArray statistics -- its own branch of
    !> the leaf rules, and its own sort-order gate.
    subroutine test_string_equality(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        character(len=8) :: name(100)
        integer(int32) :: u(100)
        character(len=*), parameter :: file = "test_run/screen_string.parquet"

        do i = 1, 100
            write(name(i), '(a,i3.3)') "obj", i
            u(i) = i
        end do
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "name", name)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)

        call compare_screened(file, 'name > "obj075"', "u", agree, pruned, nrows)
        call check(error, agree, "string >: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 25, "string >: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "string >: expected 7 of 10 row groups pruned")
        if (allocated(error)) return
        call compare_screened(file, 'name == "obj042"', "u", agree, pruned, nrows)
        call check(error, agree, "string ==: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, pruned == 9_int64, "string ==: expected 9 of 10 row groups pruned")
    end subroutine test_string_equality
    !
    !> A boolean column only admits == and /=, and its statistics are a false/true pair. Row groups
    !> holding one value only are prunable against the other.
    subroutine test_bool_equality(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        logical :: flag(100)
        integer(int32) :: u(100)
        character(len=*), parameter :: file = "test_run/screen_bool.parquet"

        ! Row groups 1..5 are all .false., 6..10 all .true., so "flag == true" can prune the first
        ! five outright.
        do i = 1, 100
            flag(i) = (i > 50)
            u(i) = i
        end do
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "flag", flag)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)

        call compare_screened(file, "flag == true", "u", agree, pruned, nrows)
        call check(error, agree, "bool ==: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 50, "bool ==: expected 50 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 5_int64, "bool ==: expected the 5 all-false row groups pruned")
    end subroutine test_bool_equality
    !
    !> A temporal column stores a raw integer, and the Fortran side rewrites the ISO-8601 literal
    !> into that same raw integer before the screen ever sees it -- so this is the integer rule
    !> again, but it proves the conversion happens on the right side of the screen.
    subroutine test_temporal_equality(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        type(parquet_date) :: d(100)
        integer(int32) :: u(100)
        character(len=*), parameter :: file = "test_run/screen_date.parquet"

        do i = 1, 100
            d(i) = parquet_date(2024, 1, 1) + int(i - 1, int32)
            u(i) = i
        end do
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "obs", d)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)

        ! 2024-01-01 + 74 days = 2024-03-15, the last date of row group 8 (rows 71..80 hold
        ! 2024-03-11..2024-03-20), so this keeps the tail from row 75 onwards.
        call compare_screened(file, 'obs > "2024-03-14"', "u", agree, pruned, nrows)
        call check(error, agree, "date >: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 26, "date >: expected 26 surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 7_int64, "date >: expected 7 of 10 row groups pruned")
    end subroutine test_temporal_equality
    !
    !> The null tests need only the footer's null count -- no min/max, and so none of the
    !> ordering gates. A row group with no nulls is prunable against is_null, and one with nothing
    !> but nulls against is_not_null.
    subroutine test_null_tests_prune(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        integer(int32) :: v(100), u(100)
        logical :: valid(100)
        character(len=*), parameter :: file = "test_run/screen_nulls.parquet"

        ! Only row group 3 (rows 21..30) carries nulls; every other row group is null-free.
        do i = 1, 100
            v(i) = i
            u(i) = i
            valid(i) = .not. (i >= 21 .and. i <= 30)
        end do
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)

        call compare_screened(file, "v is_null", "u", agree, pruned, nrows)
        call check(error, agree, "is_null: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 10, "is_null: expected the 10 null rows")
        if (allocated(error)) return
        call check(error, pruned == 9_int64, "is_null: expected the 9 null-free row groups pruned")
        if (allocated(error)) return
        call compare_screened(file, "v is_not_null", "u", agree, pruned, nrows)
        call check(error, agree, "is_not_null: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 90, "is_not_null: expected the 90 non-null rows")
        if (allocated(error)) return
        call check(error, pruned == 1_int64, "is_not_null: expected the all-null row group pruned")
    end subroutine test_null_tests_prune
    !
    !> A dotted struct-leaf path is allowed by the screen, unlike in parquet_column_has_nulls: a
    !> Parquet leaf's null count includes ancestor-struct nulls, i.e. exactly the rows
    !> unwrap_struct_path produces as null, so the counts describe the unwrapped result. That is a
    !> question of fact rather than of design, which is why it is proven here against a real
    !> null-bearing nested fixture rather than assumed.
    subroutine test_struct_leaf_equality(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: pruned
        integer :: i
        integer(int32), allocatable :: screened(:), plain(:)
        character(len=32) :: rules(3) = [character(len=32) :: &
            "main.inner.age > 30", "main.inner.age is_null", "main.inner.age is_not_null"]
        character(len=*), parameter :: file = "test/fixtures/nested_struct.parquet"

        do i = 1, 3
            ! main.id is itself null-bearing (a row whose whole `main` struct is null), so the
            ! readback needs a sentinel rather than the strict default -- which also makes the
            ! comparison cover null POSITIONS, not just values.
            call read_struct_leaf(file, trim(rules(i)), .true., screened, pruned)
            call read_struct_leaf(file, trim(rules(i)), .false., plain, pruned)
            call check(error, size(screened) == size(plain), &
                "struct leaf '" // trim(rules(i)) // "': pruned and unpruned row counts must agree")
            if (allocated(error)) return
            if (size(screened) > 0) then
                call check(error, all(screened == plain), &
                    "struct leaf '" // trim(rules(i)) // "': pruned and unpruned values must agree")
                if (allocated(error)) return
            end if
            deallocate(screened, plain)
        end do
    end subroutine test_struct_leaf_equality
    !
    !> read_under_rule's null-tolerant sibling, for the one fixture whose readback column carries
    !> Nulls. The sentinel is what lets a null position take part in the equality comparison.
    subroutine read_struct_leaf(file, rule, screen_on, values, pruned)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: rule !! the filter expression.
        logical, intent(in) :: screen_on !! .false. disables the statistics screen.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows of main.id, nulls as -999.
        integer(int64), intent(out) :: pruned !! row groups the screen ruled out.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows

        call parquet_debug_set_disable_statistics_prescreen(merge(0_c_int, 1_c_int, screen_on))
        call filt%add(rule)
        call parquet_open_reader(reader, file, filter=filt)
        pruned = parquet_debug_get_row_groups_pruned()
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, "main.id", values, null_value=-999_int32)
        call parquet_close_reader(reader)
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)
    end subroutine read_struct_leaf
    !
    !> A file whose column chunks carry no statistics at all must be read exactly as before F4 --
    !> the regression proof that every gate declines rather than guessing.
    subroutine test_no_stats_declines(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: pruned, pruned_off, n_screened, n_plain
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        real(real64), allocatable :: screened(:), plain(:)
        character(len=*), parameter :: file = "test/fixtures/no_stats.parquet"

        ! Column "c" is the null-free companion of "v"; the file is written with statistics
        ! disabled outright, so every gate in the screen has to decline from the first test.
        call filt%add("c > 250.0")
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)
        call parquet_open_reader(reader, file, filter=filt)
        pruned = parquet_debug_get_row_groups_pruned()
        call parquet_get_nrows(reader, n_screened)
        allocate(screened(n_screened))
        if (n_screened > 0) call parquet_read_column(reader, "c", screened)
        call parquet_close_reader(reader)

        call parquet_debug_set_disable_statistics_prescreen(1_c_int)
        call parquet_open_reader(reader, file, filter=filt)
        pruned_off = parquet_debug_get_row_groups_pruned()
        call parquet_get_nrows(reader, n_plain)
        allocate(plain(n_plain))
        if (n_plain > 0) call parquet_read_column(reader, "c", plain)
        call parquet_close_reader(reader)
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)

        call check(error, n_screened == n_plain, "no statistics: pruned and unpruned row counts must agree")
        if (allocated(error)) return
        call check(error, all(screened == plain), "no statistics: pruned and unpruned values must agree")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "no statistics: nothing may be pruned")
    end subroutine test_no_stats_declines
    !
    !> parquet-cpp does not truncate an over-long min/max -- it DROPS it (see
    !> EncodedStatistics::ApplyStatSizeLimits, and the screen's own gate (d) comment). A string
    !> column whose values exceed max_statistics_size (4096 bytes by default) therefore arrives with
    !> statistics that are set, and carry a null count, but have no bounds at all -- the one way a
    !> file this library writes reaches the HasMinMax() gate. Nothing may be pruned, and the answer
    !> must be unaffected.
    subroutine test_oversized_bounds_decline(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows, i
        character(len=5000) :: big(20)
        integer(int32) :: u(20)
        character(len=*), parameter :: file = "test_run/screen_oversized_bounds.parquet"

        do i = 1, 20
            ! A shared 4990-character prefix, so min and max are both far past the 4096-byte limit
            ! and differ only in their tail -- exactly the shape a truncating writer would collapse
            ! to one value and parquet-cpp instead refuses to record at all.
            write(big(i), '(a,i4.4)') repeat("z", 4996), i
            u(i) = i
        end do
        call parquet_open_writer(writer, file, chunk_size=5)
        call parquet_write_column(writer, "big", big)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)

        call compare_screened(file, 'big > "a"', "u", agree, pruned, nrows)
        call check(error, agree, "oversized bounds: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 20, "oversized bounds: expected every row to survive")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "oversized bounds: no min/max means nothing may be pruned")
    end subroutine test_oversized_bounds_decline
    !
    !> A column type the screen declines (an unsigned integer, whose bounds are ordered unsigned
    !> while this screen only reads signed and unsigned-byte orderings) must prune nothing and
    !> still answer correctly.
    subroutine test_unsupported_type_declines(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test/fixtures/extended_types.parquet"

        ! v_uint32_ovf holds {1000, 0, 4294967295}. Its Parquet min/max are ordered UNSIGNED, so
        ! the max's raw bytes read back as int32 are -1, and a screen that compared them signed
        ! would conclude "nothing here exceeds 1" and prune the row group -- losing two rows that
        ! genuinely match. That makes this the case that gives the ordering gates something to do:
        ! a column merely being unusual is not enough, the signed misreading has to produce a
        ! different answer.
        call compare_screened(file, "v_uint32_ovf > 1", "id", agree, pruned, nrows)
        call check(error, agree, "declined unsigned type: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 2, "declined unsigned type: expected the two rows above 1")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "declined unsigned type: nothing may be pruned")
        if (allocated(error)) return
        call compare_screened(file, "v_uint32 > 1", "id", agree, pruned, nrows)
        call check(error, agree, "declined type: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "declined type: nothing may be pruned")
        if (allocated(error)) return
        call compare_screened(file, "v_decimal128 > 1", "id", agree, pruned, nrows)
        call check(error, agree, "declined decimal: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, pruned == 0_int64, "declined decimal: nothing may be pruned")
    end subroutine test_unsupported_type_declines
    !
    !> A filter matching nothing prunes every row group, which leaves a reader with no rows and no
    !> row group to read them from -- the degenerate case where a whole-column read has an empty
    !> row-group list to work with.
    subroutine test_prune_everything(error)
        type(error_type), allocatable, intent(out) :: error
        logical :: agree
        integer(int64) :: pruned
        integer :: nrows
        character(len=*), parameter :: file = "test_run/screen_prune_all.parquet"

        call write_screen_fixture(file, 100, 10)
        call compare_screened(file, "id > 1000", "id", agree, pruned, nrows)
        call check(error, agree, "prune-everything: pruned and unpruned reads must be identical")
        if (allocated(error)) return
        call check(error, nrows == 0, "prune-everything: expected no surviving rows")
        if (allocated(error)) return
        call check(error, pruned == 10_int64, "prune-everything: expected all 10 row groups pruned")
    end subroutine test_prune_everything
    !
    !> Pruning is not a view of the file: a chunked read still visits every row group, and a pruned
    !> one reads as an empty chunk -- the same path a row group with no surviving rows already
    !> took. The per-row-group sizes must still sum to parquet_get_nrows.
    subroutine test_chunked_reads_under_pruning(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows, total, size_rg, rg, num_rg
        integer(int32) :: chunk(10)
        character(len=*), parameter :: file = "test_run/screen_chunked.parquet"

        call write_screen_fixture(file, 100, 10)
        call filt%add("id > 75")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_num_row_groups(reader, num_rg)
        call parquet_get_nrows(reader, nrows)
        total = 0
        do rg = 1, num_rg
            call parquet_get_chunk_size(reader, size_rg, row_group=rg)
            total = total + size_rg
        end do
        ! Row group 1 is pruned, so its chunk is empty -- and reading it must still work.
        call parquet_read_column_chunk(reader, "id", 1, chunk(1:0))
        call parquet_read_column_chunk(reader, "id", 10, chunk)
        call parquet_close_reader(reader)
        call check(error, num_rg == 10_int64, "chunked under pruning: every row group is still reported")
        if (allocated(error)) return
        call check(error, total == nrows, "chunked under pruning: chunk sizes must sum to parquet_get_nrows")
        if (allocated(error)) return
        call check(error, all(chunk == [(91 + rg, rg = 0, 9)]), "chunked under pruning: row group 10 must yield 91..100")
    end subroutine test_chunked_reads_under_pruning
    !
    !> Row and element mode resolve an index against each row group's surviving count, and a pruned
    !> row group contributes zero -- exactly as a row group the filter emptied already did.
    subroutine test_row_element_mode_under_pruning(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: id(60), vec(3, 60)
        integer(int32) :: row_screened(3), row_plain(3), elem_screened(20), elem_plain(20)
        integer :: i, j
        character(len=*), parameter :: file = "test_run/screen_row_element.parquet"

        do i = 1, 60
            id(i) = i
            do j = 1, 3
                vec(j, i) = 100 * i + j
            end do
        end do
        call parquet_open_writer(writer, file, chunk_size=10)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "vec", vec)
        call parquet_close_writer(writer)

        call filt%add("id > 40")
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_read_array_row_mode(reader, "vec", row_screened, 1)
        call parquet_read_array_element_mode(reader, "vec", elem_screened, 2)
        call parquet_close_reader(reader)

        call parquet_debug_set_disable_statistics_prescreen(1_c_int)
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_read_array_row_mode(reader, "vec", row_plain, 1)
        call parquet_read_array_element_mode(reader, "vec", elem_plain, 2)
        call parquet_close_reader(reader)
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)

        call check(error, all(row_screened == row_plain), "row mode under pruning must match an unpruned read")
        if (allocated(error)) return
        call check(error, all(elem_screened == elem_plain), "element mode under pruning must match an unpruned read")
        if (allocated(error)) return
        call check(error, all(row_screened == [4101, 4102, 4103]), "row mode under pruning: filtered row 1 is physical row 41")
    end subroutine test_row_element_mode_under_pruning
    !
    !> A sample mask only ever removes rows, so pruning row groups the filter already excluded
    !> cannot interact with it. Checked by comparing against the same draw with the screen off.
    subroutine test_sample_composes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: n_screened, n_plain
        integer(int32), allocatable :: screened(:), plain(:)
        character(len=*), parameter :: file = "test_run/screen_sample.parquet"

        call write_screen_fixture(file, 100, 10)
        call filt%add("id > 75")
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)
        call parquet_open_reader(reader, file, filter=filt, sample_fraction=0.5_real64, sample_seed=7)
        call parquet_get_nrows(reader, n_screened)
        allocate(screened(n_screened))
        if (n_screened > 0) call parquet_read_column(reader, "id", screened)
        call parquet_close_reader(reader)

        call parquet_debug_set_disable_statistics_prescreen(1_c_int)
        call parquet_open_reader(reader, file, filter=filt, sample_fraction=0.5_real64, sample_seed=7)
        call parquet_get_nrows(reader, n_plain)
        allocate(plain(n_plain))
        if (n_plain > 0) call parquet_read_column(reader, "id", plain)
        call parquet_close_reader(reader)
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)

        call check(error, n_screened == n_plain, "filter + sample under pruning: same row count as unpruned")
        if (allocated(error)) return
        call check(error, all(screened == plain), "filter + sample under pruning: same rows as unpruned")
    end subroutine test_sample_composes
    !
    !> A sort runs after the mask and reads its key column through the normal path -- which now
    !> reads live row groups -- so the permutation covers the same surviving rows either way.
    subroutine test_sort_composes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_sortkey) :: srt
        integer(int64) :: n
        integer(int32) :: screened(25), plain(25)
        character(len=*), parameter :: file = "test_run/screen_sort.parquet"

        call write_screen_fixture(file, 100, 10)
        call filt%add("id > 75")
        call srt%add("id desc")
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)
        call parquet_open_reader(reader, file, filter=filt, sort_by=srt)
        call parquet_get_nrows(reader, n)
        call parquet_read_column(reader, "payload", screened)
        call parquet_close_reader(reader)

        call parquet_debug_set_disable_statistics_prescreen(1_c_int)
        call parquet_open_reader(reader, file, filter=filt, sort_by=srt)
        call parquet_read_column(reader, "payload", plain)
        call parquet_close_reader(reader)
        call parquet_debug_set_disable_statistics_prescreen(0_c_int)

        call check(error, n == 25_int64, "filter + sort under pruning: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, all(screened == plain), "filter + sort under pruning: same order as unpruned")
        if (allocated(error)) return
        call check(error, screened(1) == 1100, "filter + sort under pruning: descending id puts row 100 first")
    end subroutine test_sort_composes
    !
    !> A scoped filter's out-of-range row groups are all-false by construction, so they are pruned
    !> for the same reason a screened-out one is -- which means a whole-column read afterwards may
    !> skip them too.
    subroutine test_scoped_filter_prunes(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: n, pruned
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/screen_scoped.parquet"

        call write_screen_fixture(file, 100, 10)
        call filt%add("id > 15")
        call parquet_open_reader(reader, file)
        call parquet_reader_set_filter(reader, filt, 2_int64, 3_int64)
        pruned = parquet_debug_get_row_groups_pruned()
        call parquet_get_nrows(reader, n)
        allocate(got(n))
        if (n > 0) call parquet_read_column(reader, "id", got)
        call parquet_close_reader(reader)
        ! Row groups 2 and 3 hold 11..30; id > 15 keeps 16..30. Every other row group is out of
        ! scope, so 8 of the 10 are pruned.
        call check(error, n == 15_int64, "scoped filter: expected ids 16..30")
        if (allocated(error)) return
        call check(error, got(1) == 16 .and. got(15) == 30, "scoped filter: expected the range 16..30")
        if (allocated(error)) return
        call check(error, pruned == 8_int64, "scoped filter: expected the 8 out-of-scope row groups pruned")
    end subroutine test_scoped_filter_prunes
    !
    !> The whole point of pruning the payload rather than just the filter columns: a column read
    !> AFTER the filter -- whether through prefetch or an ordinary read -- must come back over the
    !> same live row groups and line up row for row with the filter column.
    subroutine test_prefetch_under_pruning(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: n
        integer(int32) :: ids(25), payload(25)
        character(len=*), parameter :: file = "test_run/screen_prefetch.parquet"

        call write_screen_fixture(file, 100, 10)
        call filt%add("id > 75")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_prefetch_columns(reader, "payload")
        call parquet_get_nrows(reader, n)
        call parquet_read_column(reader, "id", ids)
        call parquet_read_column(reader, "payload", payload)
        call parquet_close_reader(reader)
        call check(error, n == 25_int64, "prefetch under pruning: expected 25 surviving rows")
        if (allocated(error)) return
        call check(error, all(payload == ids + 1000), &
            "prefetch under pruning: the payload column must line up row for row with the filter column")
    end subroutine test_prefetch_under_pruning
    !
    !> Pruning is not a view of the file: the row-group count is a property of the file, and a
    !> caller looping over row groups must still see every one of them.
    subroutine test_num_row_groups_unchanged(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: num_rg
        character(len=*), parameter :: file = "test_run/screen_num_row_groups.parquet"

        call write_screen_fixture(file, 100, 10)
        call filt%add("id == 5")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_num_row_groups(reader, num_rg)
        call parquet_close_reader(reader)
        call check(error, num_rg == 10_int64, "parquet_get_num_row_groups must still report all 10 row groups")
    end subroutine test_num_row_groups_unchanged
    !
end module test_filter_screen
