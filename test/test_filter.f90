!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the row-filter expression layer: the and/or/not grammar, its three-valued
!> (Kleene) null semantics, temporal filter literals, and the post-open
!> parquet_reader_set_filter entry point.
!>
!> Three things shape this suite:
!>
!> * **Precedence and grouping are asserted through results, not through a parse tree.** The
!>   parser has no public API of its own, so every grammar property is checked by filtering a
!>   fixture whose rows make the two candidate parses disagree -- if `a or b and c` were parsed
!>   as `(a or b) and c`, the surviving row set would differ.
!> * **Null handling gets its own fixture and its own group.** Two-valued and three-valued logic
!>   coincide for AND-only expressions, which is exactly why the AND-only implementation this
!>   layer replaced looked correct; they diverge under `not` and `or`, so those cases are tested
!>   against a column that actually contains Nulls.
!> * **Abort paths are absent by construction.** `error stop` kills the process, so every
!>   rejection (bad syntax, an unquoted temporal literal, a second filter, ...) lives in
!>   test/error_scenarios.f90 as a `filter_*` scenario, driven from test_errors.f90.
!>
!> Pre-existing filter tests stay in test/test_reading.f90 on purpose: leaving them untouched
!> makes them a regression suite for this rewrite rather than part of it.
!>
!> Every test writes its own fixture under test_run/, with its own filename -- test-drive runs
!> the tests in a suite concurrently, so a shared path can be truncated by one test while
!> another is reading it.
module test_filter
    use parquet
    use iso_fortran_env, only : int32, int64, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_filter
    !
contains
    !
    subroutine collect_tests_filter(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:)
        testsuite = [ &
            new_unittest("a single clause still behaves exactly as before", test_single_clause), &
            new_unittest("and/or precedence: 'a or b and c' == 'a or (b and c)'", test_precedence_or_and), &
            new_unittest("parentheses override precedence", test_parens_override), &
            new_unittest("not binds tighter than and", test_not_binds_tighter), &
            new_unittest("not not a == a", test_double_negation), &
            new_unittest("keywords are case-insensitive", test_case_insensitive_keywords), &
            new_unittest("a column name containing a keyword is not a keyword", test_keyword_lookalike_column), &
            new_unittest("whitespace around parentheses is optional", test_whitespace_tolerance), &
            new_unittest("deeply nested but legal expressions parse", test_deep_but_legal_nesting), &
            new_unittest("an expression with more than three clauses", test_many_clauses), &
            new_unittest("two %add calls are AND-combined", test_two_adds_and_combined), &
            new_unittest("three %add calls, each an or-expression", test_three_adds_of_or_expressions), &
            new_unittest("a rule longer than the old 512-character cap", test_long_rule_accepted), &
            new_unittest("quoted string values may contain spaces, parens and keywords", test_quoted_value_contents), &
            new_unittest("a dotted struct-leaf name inside an expression", test_struct_leaf_in_expression), &
            new_unittest("Kleene: not (x > v) excludes Null rows", test_kleene_not_excludes_nulls), &
            new_unittest("Kleene: or does not resurrect a Null row", test_kleene_or_keeps_nulls_out), &
            new_unittest("Kleene: is_null is the only way in for Null rows", test_kleene_is_null_escape_hatch), &
            new_unittest("Kleene: not x is_null == x is_not_null", test_kleene_not_is_null), &
            new_unittest("temporal: date column against ISO literals", test_temporal_date_filter), &
            new_unittest("temporal: time and timestamp columns against ISO literals", test_temporal_time_and_ts_filter), &
            new_unittest("temporal: a date-only literal on a timestamp column means midnight", test_temporal_date_on_ts), &
            new_unittest("temporal: a temporal clause combined with a numeric one", test_temporal_in_expression), &
            new_unittest("parquet_reader_set_filter matches open-time filtering", test_set_filter_post_open), &
            new_unittest("parquet_reader_set_filter composes with sample_fraction", test_set_filter_with_sample) &
            ]
    end subroutine collect_tests_filter
    !
    !> Writes the shared numeric fixture the grammar tests filter against: v = 1..10, with a
    !> second column w = 10..1 so an expression can disagree with itself across two columns.
    subroutine write_grid_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: v(10), w(10)
        integer :: i

        do i = 1, 10
            v(i) = i
            w(i) = 11 - i
        end do
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "w", w)
        call parquet_close_writer(writer)
    end subroutine write_grid_fixture
    !
    !> Reads column "v" of `file` under `rule` (one %add call) and returns the surviving values.
    subroutine filtered_v(file, rule, values)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: rule !! the filter expression.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows of column "v".
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows

        call filt%add(rule)
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", values)
        call parquet_close_reader(reader)
    end subroutine filtered_v
    !
    !> One clause with no boolean operator anywhere must still mean exactly what it always did --
    !> the baseline the whole expression layer has to preserve.
    subroutine test_single_clause(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_single_clause.parquet"

        call write_grid_fixture(file)
        call filtered_v(file, "v > 7", got)
        call check(error, size(got) == 3, "one clause: expected 3 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [8, 9, 10]), "one clause: expected rows 8, 9, 10")
    end subroutine test_single_clause
    !
    !> "a or b and c" must parse as "a or (b and c)". Under the wrong parse, "(a or b) and c",
    !> row 1 would survive (it satisfies a and c) -- so the two parses disagree on a real row,
    !> which is what makes this an assertion about precedence rather than about one result.
    subroutine test_precedence_or_and(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_precedence_or_and.parquet"

        call write_grid_fixture(file)
        ! v == 1  or  (v > 4 and v < 7)  =>  1, 5, 6
        call filtered_v(file, "v == 1 or v > 4 and v < 7", got)
        call check(error, size(got) == 3, "or/and precedence: expected 3 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [1, 5, 6]), "or/and precedence: expected rows 1, 5, 6")
    end subroutine test_precedence_or_and
    !
    !> The same clauses, re-grouped by explicit parentheses, must produce the other parse's
    !> answer -- proving the parentheses are honoured rather than ignored.
    subroutine test_parens_override(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_parens_override.parquet"

        call write_grid_fixture(file)
        ! (v == 1 or v > 4) and v < 7  =>  1, 5, 6 is wrong here: v == 1 satisfies v < 7 too,
        ! so the answer is 1, 5, 6 ... which would not distinguish the parses. Use v < 7 -> v > 2
        ! instead, where row 1 is excluded by the second operand.
        call filtered_v(file, "(v == 1 or v > 4) and v > 2", got)
        call check(error, size(got) == 6, "parens: expected 6 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [5, 6, 7, 8, 9, 10]), "parens: expected rows 5..10")
    end subroutine test_parens_override
    !
    !> "not a and b" must be "(not a) and b". Under the wrong binding, "not (a and b)", almost
    !> every row would survive instead of the two that do.
    subroutine test_not_binds_tighter(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_not_binds_tighter.parquet"

        call write_grid_fixture(file)
        ! (not v > 3) and v > 1  =>  2, 3
        call filtered_v(file, "not v > 3 and v > 1", got)
        call check(error, size(got) == 2, "not/and binding: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [2, 3]), "not/and binding: expected rows 2, 3")
    end subroutine test_not_binds_tighter
    !
    !> "not not a" is legal (the not production is right-recursive) and is a no-op.
    subroutine test_double_negation(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: plain(:), negated(:)
        character(len=*), parameter :: file = "test_run/filter_double_negation.parquet"

        call write_grid_fixture(file)
        call filtered_v(file, "v > 6", plain)
        call filtered_v(file, "not not v > 6", negated)
        call check(error, size(plain) == size(negated), "not not: expected the same row count")
        if (allocated(error)) return
        call check(error, all(plain == negated), "not not: expected the same surviving rows")
    end subroutine test_double_negation
    !
    !> Keywords may be written in any case; the same expression in three casings must agree.
    subroutine test_case_insensitive_keywords(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: lower(:), upper(:), mixed(:)
        character(len=*), parameter :: file = "test_run/filter_case_insensitive.parquet"

        call write_grid_fixture(file)
        call filtered_v(file, "v > 2 and not v > 8 or v == 10", lower)
        call filtered_v(file, "v > 2 AND NOT v > 8 OR v == 10", upper)
        call filtered_v(file, "v > 2 And Not v > 8 Or v == 10", mixed)
        call check(error, size(lower) == size(upper) .and. size(lower) == size(mixed), &
            "keyword casing: expected the same row count for all three casings")
        if (allocated(error)) return
        call check(error, all(lower == upper) .and. all(lower == mixed), &
            "keyword casing: expected the same surviving rows for all three casings")
    end subroutine test_case_insensitive_keywords
    !
    !> A keyword is only a keyword as a whole token: a column called "android" or "nothing" must
    !> lex as an ordinary name, not as "and"/"not" plus leftovers.
    subroutine test_keyword_lookalike_column(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: android(4) = [1, 2, 3, 4], nothing(4) = [4, 3, 2, 1]
        integer(int32) :: got(2)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_keyword_lookalike.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "android", android)
        call parquet_write_column(writer, "nothing", nothing)
        call parquet_close_writer(writer)

        call filt%add("android > 1 and nothing > 1")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, "keyword lookalike: expected 2 surviving rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "android", got)
        call parquet_close_reader(reader)
        call check(error, all(got == [2, 3]), "keyword lookalike: expected android values 2, 3")
    end subroutine test_keyword_lookalike_column
    !
    !> Parentheses need no surrounding spaces, and runs of spaces anywhere are ignored. A clause's
    !> own operator still has to be space-separated ("v > 3", not "v>3"), exactly as it always
    !> has: the clause tokenizer is unchanged by the expression layer.
    subroutine test_whitespace_tolerance(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: spaced(:), tight(:)
        character(len=*), parameter :: file = "test_run/filter_whitespace.parquet"

        call write_grid_fixture(file)
        call filtered_v(file, "( v > 3   and v < 6 ) or v == 9", spaced)
        call filtered_v(file, "(v > 3 and v < 6)or v == 9", tight)
        call check(error, size(spaced) == size(tight), "whitespace: expected the same row count")
        if (allocated(error)) return
        call check(error, all(spaced == tight) .and. all(tight == [4, 5, 9]), &
            "whitespace: expected rows 4, 5, 9 either way")
    end subroutine test_whitespace_tolerance
    !
    !> Nesting well inside the depth cap parses and evaluates -- the companion to the
    !> filter_nesting_too_deep scenario, which checks the cap itself.
    subroutine test_deep_but_legal_nesting(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_deep_nesting.parquet"

        call write_grid_fixture(file)
        call filtered_v(file, repeat("(", 20)//"v > 8"//repeat(")", 20), got)
        call check(error, size(got) == 2, "deep nesting: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [9, 10]), "deep nesting: expected rows 9, 10")
    end subroutine test_deep_but_legal_nesting
    !
    !> More than three clauses in one expression -- the case the pre-expression implementation
    !> had no test for at all, over two columns so the clauses cannot collapse into one range.
    subroutine test_many_clauses(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_many_clauses.parquet"

        call write_grid_fixture(file)
        ! w == 11 - v, so w > 3 is v < 8 and w < 9 is v > 2.
        call filtered_v(file, "v > 2 and v < 8 and w > 3 and w < 9 and v /= 5", got)
        call check(error, size(got) == 4, "five clauses: expected 4 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [3, 4, 6, 7]), "five clauses: expected rows 3, 4, 6, 7")
    end subroutine test_many_clauses
    !
    !> Two %add calls mean (expr1) and (expr2) -- the rule that keeps every filter written before
    !> expressions existed meaning what it did, and the one a user who reads "%add appends a
    !> clause" might expect to be an or.
    subroutine test_two_adds_and_combined(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: got(3)
        integer(int32), allocatable :: combined(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_two_adds.parquet"

        call write_grid_fixture(file)
        call filt%add("v > 2 or v == 1")
        call filt%add("v < 5")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        ! (v > 2 or v == 1) and (v < 5)  =>  1, 3, 4. An or-combination would give 1..10.
        call check(error, nrows == 3, "two %add calls: expected 3 surviving rows (AND-combined)")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, all(got == [1, 3, 4]), "two %add calls: expected rows 1, 3, 4")
        if (allocated(error)) return

        ! The same thing written as one parenthesised expression must agree, which is what makes
        ! "several %add calls are AND-combined" a statement about the grammar, not a coincidence.
        call filtered_v(file, "(v > 2 or v == 1) and (v < 5)", combined)
        call check(error, size(combined) == 3, "two %add calls: one combined expression must give 3 rows too")
        if (allocated(error)) return
        call check(error, all(combined == got), "two %add calls: expected the same rows as one combined expression")
    end subroutine test_two_adds_and_combined
    !
    !> Three %add calls, each itself an or-expression: every rule is parenthesised implicitly, so
    !> the ors inside a rule must not leak across the AND that joins the rules.
    subroutine test_three_adds_of_or_expressions(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: got(2)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_three_adds.parquet"

        call write_grid_fixture(file)
        call filt%add("v == 2 or v == 4 or v == 6")
        call filt%add("v > 3 or v == 2")
        call filt%add("v < 6 or v == 2")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, "three %add calls: expected 2 surviving rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, all(got == [2, 4]), "three %add calls: expected rows 2 and 4")
    end subroutine test_three_adds_of_or_expressions
    !
    !> A rule past the old fixed 512-character component width, which a parenthesised
    !> multi-clause expression reaches quickly. Built from many redundant clauses so the answer
    !> stays easy to state.
    subroutine test_long_rule_accepted(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=:), allocatable :: rule
        integer :: i
        character(len=*), parameter :: file = "test_run/filter_long_rule.parquet"

        call write_grid_fixture(file)
        rule = "v > 8"
        do i = 2, 60
            rule = rule//" and v > 8"
        end do
        call check(error, len(rule) > 512, "long rule: the test's own rule must exceed 512 characters")
        if (allocated(error)) return
        call filtered_v(file, rule, got)
        call check(error, size(got) == 2, "long rule: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [9, 10]), "long rule: expected rows 9, 10")
    end subroutine test_long_rule_accepted
    !
    !> A quoted value is lexed as one token, so spaces, parentheses and even the keywords
    !> themselves inside the quotes are part of the value rather than expression syntax.
    subroutine test_quoted_value_contents(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=24) :: names(3)
        character(len=24) :: got(1)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_quoted_value.parquet"

        names(1) = "plain"
        names(2) = "a and (b) or c"
        names(3) = "other"
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "name", names)
        call parquet_close_writer(writer)

        call filt%add('name == "a and (b) or c"')
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 1, "quoted value: expected exactly the one matching row")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "name", got)
        call parquet_close_reader(reader)
        call check(error, trim(got(1)) == "a and (b) or c", "quoted value: expected the value read back verbatim")
    end subroutine test_quoted_value_contents
    !
    !> A dotted struct-leaf path is a single name token, so it works inside an expression exactly
    !> as it does as a lone clause.
    subroutine test_struct_leaf_in_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows_leaf, nrows_expr

        call filt%add("main.inner.age > 35")
        call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet", filter=filt)
        call parquet_get_nrows(reader, nrows_leaf)
        call parquet_close_reader(reader)

        block
            type(parquet_filter) :: expr_filter
            call expr_filter%add("main.inner.age > 35 or main.inner.age < 0")
            call parquet_open_reader(reader, "test/fixtures/nested_struct.parquet", filter=expr_filter)
            call parquet_get_nrows(reader, nrows_expr)
            call parquet_close_reader(reader)
        end block

        call check(error, nrows_expr == nrows_leaf, &
            "struct leaf in an expression: the impossible or-branch must not change the row count")
    end subroutine test_struct_leaf_in_expression
    !
    !> Writes the null-bearing fixture the Kleene group filters against: v = 1..6 with rows 3 and
    !> 4 null, plus a null-free companion column u so an or can have one known-good operand.
    subroutine write_null_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: v(6) = [1, 2, 0, 0, 5, 6]
        integer(int32) :: u(6) = [1, 2, 3, 4, 5, 6]
        logical :: valid(6) = [.true., .true., .false., .false., .true., .true.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v, is_valid=valid)
        call parquet_write_column(writer, "u", u)
        call parquet_close_writer(writer)
    end subroutine write_null_fixture
    !
    !> Reads column "u" (never null) of a null fixture under `rule`, so a test can identify which
    !> rows survived even when the filtered column itself is null in some of them.
    subroutine filtered_u(file, rule, values)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: rule !! the filter expression.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows' "u" values.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows

        call filt%add(rule)
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, "u", values)
        call parquet_close_reader(reader)
    end subroutine filtered_u
    !
    !> The headline Kleene case: a Null row is unknown, not false, so negating a comparison must
    !> NOT admit it. Under a two-valued flip, rows 3 and 4 would survive here.
    subroutine test_kleene_not_excludes_nulls(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_kleene_not.parquet"

        call write_null_fixture(file)
        call filtered_u(file, "not v > 4", got)
        call check(error, size(got) == 2, "Kleene not: expected only the two non-null rows below the bound")
        if (allocated(error)) return
        call check(error, all(got == [1, 2]), "Kleene not: expected rows 1, 2 (never the null rows 3, 4)")
    end subroutine test_kleene_not_excludes_nulls
    !
    !> An or whose other operand is false must not let a Null row through either -- unknown or
    !> false is unknown, which collapses to false at the end.
    subroutine test_kleene_or_keeps_nulls_out(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_kleene_or.parquet"

        call write_null_fixture(file)
        ! u == 3 is true exactly on the row where v is Null: unknown or true must still be true,
        ! so that row survives -- while row 4 (unknown or false) must not.
        call filtered_u(file, "v > 4 or u == 3", got)
        call check(error, size(got) == 3, "Kleene or: expected 3 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [3, 5, 6]), "Kleene or: expected rows 3, 5, 6")
    end subroutine test_kleene_or_keeps_nulls_out
    !
    !> is_null is the deliberate escape hatch: it is the only operator that answers true/false
    !> (never unknown) for a Null row, so it is the only way to select one.
    subroutine test_kleene_is_null_escape_hatch(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_kleene_is_null.parquet"

        call write_null_fixture(file)
        call filtered_u(file, "v is_null or v > 5", got)
        call check(error, size(got) == 3, "Kleene is_null: expected 3 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [3, 4, 6]), "Kleene is_null: expected the two null rows plus row 6")
    end subroutine test_kleene_is_null_escape_hatch
    !
    !> Negating is_null must give exactly is_not_null -- i.e. the negation of a never-unknown
    !> operator behaves classically, which is what makes the escape hatch usable in expressions.
    subroutine test_kleene_not_is_null(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: negated(:), direct(:)
        character(len=*), parameter :: file = "test_run/filter_kleene_not_is_null.parquet"

        call write_null_fixture(file)
        call filtered_u(file, "not v is_null", negated)
        call filtered_u(file, "v is_not_null", direct)
        call check(error, size(negated) == size(direct), "not is_null: expected the same row count as is_not_null")
        if (allocated(error)) return
        call check(error, all(negated == direct) .and. all(direct == [1, 2, 5, 6]), &
            "not is_null: expected rows 1, 2, 5, 6 either way")
    end subroutine test_kleene_not_is_null
    !
    !> Writes the temporal fixture: a date column, a time[us] column and a timestamp[ms] column,
    !> six rows one day/second apart.
    subroutine write_temporal_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        type(parquet_schema) :: schema
        type(parquet_date) :: d(6)
        type(parquet_time) :: t(6)
        type(parquet_timestamp) :: ts(6)
        integer :: i

        do i = 1, 6
            call d(i)%set(2024, 1, i)
            call t(i)%set(12, 0, i - 1)
            call ts(i)%set(2024, 1, 31, 12, 30, i - 1)
        end do
        call schema%init("temporal_filter")
        call schema%add_field("d", "date")
        call schema%add_field("t", "time[us]")
        call schema%add_field("ts", "timestamp[ms]")
        call parquet_parse_maml(schema)
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "d", d)
        call parquet_write_column(writer, "t", t)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)
    end subroutine write_temporal_fixture
    !
    !> A date column compares against a quoted ISO date, converted Fortran-side into the day
    !> count the column actually stores.
    subroutine test_temporal_date_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_date) :: got(2)
        integer(int64) :: nrows
        integer(int32) :: y, m, dd
        character(len=*), parameter :: file = "test_run/filter_temporal_date.parquet"

        call write_temporal_fixture(file)
        call filt%add('d >= "2024-01-05"')
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2, "date filter: expected 2 surviving rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "d", got)
        call parquet_close_reader(reader)
        call got(1)%get(y, m, dd)
        call check(error, y == 2024 .and. m == 1 .and. dd == 5, "date filter: expected the first survivor to be 2024-01-05")
    end subroutine test_temporal_date_filter
    !
    !> A time[us] and a timestamp[ms] column: both convert the literal into their own stored
    !> unit, which differs per column, so the same textual precision means different integers.
    subroutine test_temporal_time_and_ts_filter(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int64) :: nrows_time, nrows_ts
        type(parquet_reader) :: reader
        type(parquet_filter) :: time_filter, ts_filter
        character(len=*), parameter :: file = "test_run/filter_temporal_time_ts.parquet"

        call write_temporal_fixture(file)
        call time_filter%add('t > "12:00:03"')
        call parquet_open_reader(reader, file, filter=time_filter)
        call parquet_get_nrows(reader, nrows_time)
        call parquet_close_reader(reader)
        call check(error, nrows_time == 2, "time filter: expected 2 surviving rows (12:00:04 and 12:00:05)")
        if (allocated(error)) return

        call ts_filter%add('ts <= "2024-01-31T12:30:02"')
        call parquet_open_reader(reader, file, filter=ts_filter)
        call parquet_get_nrows(reader, nrows_ts)
        call parquet_close_reader(reader)
        call check(error, nrows_ts == 3, "timestamp filter: expected 3 surviving rows")
    end subroutine test_temporal_time_and_ts_filter
    !
    !> A date-only literal against a timestamp column is midnight of that date -- less precise
    !> than the column, which is fine (unlike more precise, which is rejected).
    subroutine test_temporal_date_on_ts(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_temporal_date_on_ts.parquet"

        call write_temporal_fixture(file)
        call filt%add('ts > "2024-01-31"')
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        call check(error, nrows == 6, "date literal on a timestamp column: every row is after midnight that day")
    end subroutine test_temporal_date_on_ts
    !
    !> A temporal clause inside a boolean expression alongside a non-temporal one: the temporal
    !> conversion is per leaf, so it must not disturb the other leaf's value.
    subroutine test_temporal_in_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_temporal_expression.parquet"

        call write_temporal_fixture(file)
        call filt%add('d >= "2024-01-05" or t < "12:00:01"')
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        call check(error, nrows == 3, "temporal in an expression: expected rows 1, 5 and 6")
    end subroutine test_temporal_in_expression
    !
    !> Applying a filter after the reader is open must produce exactly what passing it to
    !> parquet_open_reader would have.
    subroutine test_set_filter_post_open(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: at_open, post_open
        integer(int64) :: nrows_open, nrows_post
        integer(int32) :: got(3)
        character(len=*), parameter :: file = "test_run/filter_set_post_open.parquet"

        call write_grid_fixture(file)
        call at_open%add("v > 7")
        call parquet_open_reader(reader, file, filter=at_open)
        call parquet_get_nrows(reader, nrows_open)
        call parquet_close_reader(reader)

        call post_open%add("v > 7")
        call parquet_open_reader(reader, file)
        call parquet_reader_set_filter(reader, post_open)
        call parquet_get_nrows(reader, nrows_post)
        call check(error, nrows_post == nrows_open, "set_filter: expected the same row count as filtering at open")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, all(got == [8, 9, 10]), "set_filter: expected rows 8, 9, 10")
    end subroutine test_set_filter_post_open
    !
    !> A reader opened with sample_fraction= accepts a post-open filter: the sample mask is
    !> already installed, and the clauses combine with it rather than replacing it. With
    !> sample_fraction just under 1.0 and a fixed seed, every row is drawn, so the filter's own
    !> answer is what remains -- which is what makes this assertable without depending on the
    !> draw itself.
    subroutine test_set_filter_with_sample(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows_sampled, nrows_filtered
        character(len=*), parameter :: file = "test_run/filter_set_with_sample.parquet"

        call write_grid_fixture(file)
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=7)
        call parquet_get_nrows(reader, nrows_sampled)
        call check(error, nrows_sampled == 10, "set_filter with sample: expected the near-1.0 draw to keep every row")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call filt%add("v > 6 and v < 10")
        call parquet_reader_set_filter(reader, filt)
        call parquet_get_nrows(reader, nrows_filtered)
        call parquet_close_reader(reader)
        call check(error, nrows_filtered == 3, "set_filter with sample: expected the filter to apply on top of the sample")
    end subroutine test_set_filter_with_sample
    !
end module test_filter
