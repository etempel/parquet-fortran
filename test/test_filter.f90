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
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan
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
            new_unittest("parquet_reader_set_filter composes with sample_fraction", test_set_filter_with_sample), &
            new_unittest("chunked read on a filtered reader returns the surviving rows", test_chunked_read_filtered), &
            new_unittest("chunk sizes sum to parquet_get_nrows under a filter", test_chunk_sizes_sum_to_nrows), &
            new_unittest("a row group with no surviving rows reads as an empty chunk", test_zero_survivor_row_group), &
            new_unittest("a filter that removes every row", test_filter_removes_all_rows), &
            new_unittest("chunked read on a sampled reader", test_chunked_read_sampled), &
            new_unittest("filter and sample together, read chunked", test_chunked_read_filter_and_sample), &
            new_unittest("check_complete is satisfied by a filtered chunked pass", test_check_complete_filtered_chunks), &
            new_unittest("a scoped filter covers only its own row groups", test_scoped_filter_row_groups), &
            new_unittest("qc validates the filtered chunk, not the raw one", test_qc_runs_on_filtered_chunk), &
            new_unittest("row mode on a filtered reader addresses the filtered result", test_row_mode_filtered), &
            new_unittest("row mode steps over row groups with no survivors", &
                test_row_mode_filtered_skips_empty_row_groups), &
            new_unittest("element mode on a filtered reader spans the surviving rows", test_element_mode_filtered), &
            new_unittest("row and element mode, filtered, for logical and string columns", &
                test_row_and_element_mode_filtered_bool_string), &
            new_unittest("row and element mode, filtered, for a date column", &
                test_row_and_element_mode_filtered_temporal), &
            new_unittest("row and element mode on a sampled reader", test_row_and_element_mode_sampled), &
            new_unittest("NaN: a comparison excludes NaN rows, /= admits them", test_nan_under_comparisons), &
            new_unittest("NaN: a negated comparison admits NaN rows but not Null rows", test_nan_under_not), &
            new_unittest("is_nan / is_not_nan select on NaN", test_is_nan_selects), &
            new_unittest("is_nan / is_not_nan answer unknown for a Null row", test_is_nan_null_is_unknown), &
            new_unittest("is_nan == not (x >= 0 or x < 0)", test_is_nan_matches_comparison_oracle), &
            new_unittest("not x is_nan == x is_not_nan", test_not_is_nan_is_is_not_nan), &
            new_unittest("is_nan combines with other clauses", test_is_nan_in_expression), &
            new_unittest("is_nan on a float32 column", test_is_nan_float32), &
            new_unittest("is_nan on a half_float column", test_is_nan_half_float) &
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
    !> Writes a fixture with a known row-group layout: `nrows` rows, `chunk` rows per row group,
    !> column v = 1..nrows. Row groups are what every test below addresses, so the layout is the
    !> point of the fixture rather than the values.
    subroutine write_chunked_fixture(file, nrows, chunk)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        integer, intent(in) :: nrows !! total rows to write.
        integer, intent(in) :: chunk !! rows per row group.
        type(parquet_writer) :: writer
        integer(int32), allocatable :: v(:)
        integer :: i

        allocate(v(nrows))
        do i = 1, nrows
            v(i) = i
        end do
        call parquet_open_writer(writer, file, chunk_size=chunk)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)
    end subroutine write_chunked_fixture
    !
    !> A chunked read on a filtered reader hands back that row group's surviving rows -- the
    !> operation that used to abort outright, since the mask was believed to have no row-group
    !> structure. 12 rows in 4 row groups of 3; "v > 4" leaves row group 1 empty, row group 2 with
    !> two of its three rows, and row groups 3 and 4 intact.
    subroutine test_chunked_read_filtered(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: sizes(4), rg
        integer(int32) :: rg2(2), rg3(3)
        character(len=*), parameter :: file = "test_run/filter_chunked_read.parquet"

        call write_chunked_fixture(file, 12, 3)
        call filt%add("v > 4")
        call parquet_open_reader(reader, file, filter=filt)
        do rg = 1, 4
            call parquet_get_chunk_size(reader, sizes(rg), row_group=rg)
        end do
        call check(error, all(sizes == [0_int64, 2_int64, 3_int64, 3_int64]), &
            "filtered chunk sizes: expected 0, 2, 3, 3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column_chunk(reader, "v", 2, rg2)
        call parquet_read_column_chunk(reader, "v", 3, rg3)
        call parquet_close_reader(reader)
        call check(error, all(rg2 == [5, 6]), "filtered chunk: row group 2 should yield 5, 6")
        if (allocated(error)) return
        call check(error, all(rg3 == [7, 8, 9]), "filtered chunk: row group 3 should yield 7, 8, 9")
    end subroutine test_chunked_read_filtered
    !
    !> The invariant a chunked loop depends on: the per-row-group sizes sum to the reader's own
    !> row count. It is what lets a caller allocate per row group and still cover exactly the
    !> filtered result, with no separate bookkeeping.
    subroutine test_chunk_sizes_sum_to_nrows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows, total, size_rg, rg, num_rg
        character(len=*), parameter :: file = "test_run/filter_chunk_sum.parquet"

        call write_chunked_fixture(file, 17, 5)
        call filt%add("v > 3 and v /= 11")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_get_num_row_groups(reader, num_rg)
        total = 0_int64
        do rg = 1, num_rg
            call parquet_get_chunk_size(reader, size_rg, row_group=rg)
            total = total + size_rg
        end do
        call parquet_close_reader(reader)
        call check(error, total == nrows, "sum of filtered chunk sizes must equal parquet_get_nrows")
        if (allocated(error)) return
        call check(error, nrows == 13_int64, "expected 13 surviving rows (v > 3, excluding 11)")
    end subroutine test_chunk_sizes_sum_to_nrows
    !
    !> Under a selective filter a row group with no survivors is the normal case, not a curiosity:
    !> it must read cleanly as a zero-length chunk rather than aborting on a length mismatch.
    subroutine test_zero_survivor_row_group(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: chunk_rows
        integer(int32) :: empty(0)
        character(len=*), parameter :: file = "test_run/filter_zero_survivors.parquet"

        call write_chunked_fixture(file, 9, 3)
        call filt%add("v > 6")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_chunk_size(reader, chunk_rows, row_group=1_int64)
        call check(error, chunk_rows == 0_int64, "a row group with no survivors must report 0 rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        ! Reading it must be a clean no-op, not an abort.
        call parquet_read_column_chunk(reader, "v", 1, empty)
        call parquet_close_reader(reader)
        call check(error, .true., "reading a zero-survivor row group must not abort")
    end subroutine test_zero_survivor_row_group
    !
    !> A filter no row satisfies: every row group is empty and the reader reports zero rows.
    subroutine test_filter_removes_all_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows, chunk_rows, rg
        character(len=*), parameter :: file = "test_run/filter_removes_all.parquet"

        call write_chunked_fixture(file, 8, 4)
        call filt%add("v > 1000")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 0_int64, "a filter matching nothing must report zero rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        do rg = 1, 2
            call parquet_get_chunk_size(reader, chunk_rows, row_group=rg)
            call check(error, chunk_rows == 0_int64, "every row group must report 0 rows under a filter matching nothing")
            if (allocated(error)) exit
        end do
        call parquet_close_reader(reader)
    end subroutine test_filter_removes_all_rows
    !
    !> Sampling shares the filter's mask machinery, so chunked reads work there too. A fraction
    !> just under 1.0 with a fixed seed keeps every row, which makes the expected chunk sizes
    !> assertable without depending on the draw.
    subroutine test_chunked_read_sampled(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: chunk_rows
        integer(int32) :: back(3)
        character(len=*), parameter :: file = "test_run/filter_chunked_sampled.parquet"

        call write_chunked_fixture(file, 9, 3)
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=11)
        call parquet_get_chunk_size(reader, chunk_rows, row_group=2_int64)
        call check(error, chunk_rows == 3_int64, "a near-1.0 sample must keep all 3 rows of row group 2")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column_chunk(reader, "v", 2, back)
        call parquet_close_reader(reader)
        call check(error, all(back == [4, 5, 6]), "sampled chunk: row group 2 should yield 4, 5, 6")
    end subroutine test_chunked_read_sampled
    !
    !> Filter and sample together, read chunked: the two compose into one mask, and the chunked
    !> sizes still sum to the reader's row count.
    subroutine test_chunked_read_filter_and_sample(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows, total, size_rg, rg
        character(len=*), parameter :: file = "test_run/filter_chunked_both.parquet"

        call write_chunked_fixture(file, 12, 4)
        call filt%add("v > 5")
        call parquet_open_reader(reader, file, filter=filt, sample_fraction=0.999999_real64, sample_seed=3)
        call parquet_get_nrows(reader, nrows)
        total = 0_int64
        do rg = 1, 3
            call parquet_get_chunk_size(reader, size_rg, row_group=rg)
            total = total + size_rg
        end do
        call parquet_close_reader(reader)
        call check(error, nrows == 7_int64, "filter + near-1.0 sample: expected the filter's own 7 rows")
        if (allocated(error)) return
        call check(error, total == nrows, "filter + sample: chunk sizes must still sum to parquet_get_nrows")
    end subroutine test_chunked_read_filter_and_sample
    !
    !> check_complete tracks row groups, not rows, so a filtered chunked pass still proves every
    !> row group was visited -- including the ones that turned out to be empty.
    subroutine test_check_complete_filtered_chunks(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: chunk_rows, rg
        integer(int32), allocatable :: back(:)
        character(len=*), parameter :: file = "test_run/filter_check_complete.parquet"

        call write_chunked_fixture(file, 9, 3)
        call filt%add("v > 6")
        call parquet_open_reader(reader, file, filter=filt)
        do rg = 1, 3
            call parquet_get_chunk_size(reader, chunk_rows, row_group=rg)
            allocate(back(chunk_rows))
            call parquet_read_column_chunk(reader, "v", rg, back)
            deallocate(back)
        end do
        ! Aborts if any row group was skipped for a column read this way.
        call parquet_close_reader(reader, check_complete=.true.)
        call check(error, .true., "a filtered chunked pass over every row group must satisfy check_complete")
    end subroutine test_check_complete_filtered_chunks
    !
    !> A row-group-scoped filter narrows the reader to the surviving rows OF THOSE ROW GROUPS:
    !> rows outside the range never match, whether or not they satisfy the expression. This is
    !> what lets a caller filter part of a file without the mask (or the read behind it) covering
    !> the whole of it.
    subroutine test_scoped_filter_row_groups(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows, chunk_rows
        integer(int32) :: back(2)
        character(len=*), parameter :: file = "test_run/filter_scoped_rows.parquet"

        call write_chunked_fixture(file, 12, 3)
        ! "v > 4" matches rows 5..12 across the file, but the scope keeps only row groups 2 and 3
        ! (rows 4..9), so 5..9 survive.
        call filt%add("v > 4")
        call parquet_open_reader(reader, file)
        call parquet_reader_set_filter(reader, filt, 2, 3)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 5_int64, "scoped filter: expected the 5 matching rows inside row groups 2-3")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_get_chunk_size(reader, chunk_rows, row_group=1_int64)
        call check(error, chunk_rows == 0_int64, "scoped filter: a row group outside the scope must yield no rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column_chunk(reader, "v", 2, back)
        call parquet_close_reader(reader)
        call check(error, all(back == [5, 6]), "scoped filter: row group 2 should yield 5, 6")
        if (allocated(error)) return

        ! The same scope given as integer(int64) bounds must mean the same thing -- the bounds are
        ! generic over both kinds so a caller's plain INTEGER and an int64 row-group count are
        ! equally acceptable.
        block
            type(parquet_filter) :: wide_filter
            integer(int64) :: nrows64
            call wide_filter%add("v > 4")
            call parquet_open_reader(reader, file)
            call parquet_reader_set_filter(reader, wide_filter, 2_int64, 3_int64)
            call parquet_get_nrows(reader, nrows64)
            call parquet_close_reader(reader)
            call check(error, nrows64 == nrows, "scoped filter: int64 bounds must match int32 bounds")
        end block
    end subroutine test_scoped_filter_row_groups
    !
    !> Read-time qc runs AFTER filtering, so a value the filter removed cannot trigger it. The
    !> fixture's row group 1 holds a value far outside the declared qc range; the filter excludes
    !> exactly that row, and the chunked read must then pass qc in HARD mode -- which aborts the
    !> process on a violation, so this test simply completing is the assertion.
    !>
    !> The reverse case (a violation that survives the filter and does abort) is
    !> read_chunk_qc_hard_aborts in test/error_scenarios.f90, since it ends the process.
    subroutine test_qc_runs_on_filtered_chunk(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_schema) :: schema
        integer(int32) :: ra(4), back(1)
        integer :: unit
        character(len=*), parameter :: file = "test_run/filter_qc_filtered_chunk.parquet"
        character(len=*), parameter :: maml = "test_run/filter_qc_filtered_chunk.maml"

        ra = [400, 10, 20, 30] ! 400 violates the [0, 360] bound below; the filter removes it
        call parquet_open_writer(writer, file, chunk_size=2)
        call parquet_write_column(writer, "ra", ra)
        call parquet_close_writer(writer)

        open(newunit=unit, file=maml, status="replace", action="write")
        write(unit, '(a)') "fields:"
        write(unit, '(a)') "- name: ra"
        write(unit, '(a)') "  qc:"
        write(unit, '(a)') "    min: 0"
        write(unit, '(a)') "    max: 360"
        close(unit)

        schema = parquet_load_qc_maml_file(maml)
        call filt%add("ra < 100")
        call parquet_open_reader(reader, file, filter=filt, schema=schema, qc=.true.)
        ! Row group 1 holds rows 400 and 10; only 10 survives, so qc sees no violation.
        call parquet_read_column_chunk(reader, "ra", 1, back)
        call parquet_close_reader(reader)
        call check(error, back(1) == 10, "qc on a filtered chunk: expected the one surviving row of row group 1")
    end subroutine test_qc_runs_on_filtered_chunk
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
    !> Writes the fixture the row-mode/element-mode group below addresses: `nrows` rows in row
    !> groups of `chunk` rows, a scalar filter column v = 1..nrows, and one vector column per
    !> entry-point family -- int32 (the templated one), logical and string (both hand-written),
    !> and date (the temporal template). Every vector value encodes its own row number, so a
    !> wrong row-index mapping surfaces as a wrong value rather than as a crash.
    subroutine write_vector_fixture(file, nrows, chunk)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        integer, intent(in) :: nrows !! total rows to write.
        integer, intent(in) :: chunk !! rows per row group.
        type(parquet_writer) :: writer
        integer(int32), allocatable :: v(:), vec(:,:)
        logical, allocatable :: flg(:,:)
        character(len=8), allocatable :: txt(:,:)
        type(parquet_date), allocatable :: dt(:,:)
        integer :: i

        allocate(v(nrows), vec(3, nrows), flg(2, nrows), txt(2, nrows), dt(2, nrows))
        do i = 1, nrows
            v(i) = i
            vec(:, i) = [100 * i + 1, 100 * i + 2, 100 * i + 3]
            flg(1, i) = mod(i, 2) == 0
            flg(2, i) = i > nrows / 2
            write(txt(1, i), '(a,i0)') "a", i
            write(txt(2, i), '(a,i0)') "b", i
            dt(1, i) = parquet_date(2024, 1, 1) + int(i - 1, int32)
            dt(2, i) = parquet_date(2024, 6, 1) + int(i - 1, int32)
        end do

        call parquet_open_writer(writer, file, chunk_size=chunk)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "vec", vec)
        call parquet_write_column(writer, "flg", flg)
        call parquet_write_column(writer, "txt", txt)
        call parquet_write_column(writer, "dt", dt)
        call parquet_close_writer(writer)
    end subroutine write_vector_fixture
    !
    !> Row mode on a filtered reader: `row_index` addresses the filtered result, and the read
    !> resolves it to one row group by walking SURVIVING counts rather than the footer's physical
    !> ones. 12 rows in 4 row groups of 3, `v > 4` keeping physical rows 5..12, so filtered row 1
    !> is physical row 5 -- in row group 2, which is the point: a mapping that still walked
    !> physical counts would land in row group 1 and return row 1's values.
    subroutine test_row_mode_filtered(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: first(3), last(3), middle(3)
        character(len=*), parameter :: file = "test_run/filter_row_mode.parquet"

        call write_vector_fixture(file, 12, 3)
        call filt%add("v > 4")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_read_array_row_mode(reader, "vec", first, 1)
        call parquet_read_array_row_mode(reader, "vec", middle, 4)
        call parquet_read_array_row_mode(reader, "vec", last, 8)
        call parquet_close_reader(reader)
        call check(error, all(first == [501, 502, 503]), "filtered row 1 must be physical row 5")
        if (allocated(error)) return
        call check(error, all(middle == [801, 802, 803]), "filtered row 4 must be physical row 8")
        if (allocated(error)) return
        call check(error, all(last == [1201, 1202, 1203]), "filtered row 8 (the last) must be physical row 12")
    end subroutine test_row_mode_filtered
    !
    !> The stepping-over case in isolation: a filter that empties whole row groups. `v > 6` on 12
    !> rows in 4 groups of 3 leaves groups 1 and 2 with no survivors at all, so filtered row 1 is
    !> physical row 7 in row group 3 -- the resolver must skip two zero-survivor groups, exactly
    !> as it already skipped a physically empty one.
    subroutine test_row_mode_filtered_skips_empty_row_groups(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        integer(int32) :: first(3)
        character(len=*), parameter :: file = "test_run/filter_row_mode_skip.parquet"

        call write_vector_fixture(file, 12, 3)
        call filt%add("v > 6")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_read_array_row_mode(reader, "vec", first, 1)
        call parquet_close_reader(reader)
        call check(error, nrows == 6_int64, "v > 6 must leave 6 rows")
        if (allocated(error)) return
        call check(error, all(first == [701, 702, 703]), &
            "filtered row 1 must be physical row 7, two zero-survivor row groups later")
    end subroutine test_row_mode_filtered_skips_empty_row_groups
    !
    !> Element mode on a filtered reader: one element position across every SURVIVING row. Unlike
    !> row mode this needs every row group, so it is the streaming loop's own row counts that have
    !> to be the surviving ones -- a loop still stepping by physical counts would write past the
    !> filtered output or abort on the nrows mismatch.
    subroutine test_element_mode_filtered(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        integer(int32) :: elem(8)
        integer :: i
        character(len=*), parameter :: file = "test_run/filter_element_mode.parquet"

        call write_vector_fixture(file, 12, 3)
        call filt%add("v > 4")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 8_int64, "v > 4 must leave 8 rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_array_element_mode(reader, "vec", elem, 2)
        call parquet_close_reader(reader)
        call check(error, all(elem == [(100 * i + 2, i = 5, 12)]), &
            "element 2 across the filtered rows must be physical rows 5..12's second element")
    end subroutine test_element_mode_filtered
    !
    !> The two hand-written (non-templated) entry-point pairs, which carry their own copy of the
    !> row-group logic rather than sharing read_list_primitive_row/_element: logical and string.
    subroutine test_row_and_element_mode_filtered_bool_string(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        logical :: flag_row(2), flag_elem(8)
        character(len=8) :: txt_row(2), txt_elem(8)
        character(len=8) :: expect_elem(8)
        integer :: i
        character(len=*), parameter :: file = "test_run/filter_row_element_bool_string.parquet"

        call write_vector_fixture(file, 12, 3)
        call filt%add("v > 4")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_read_array_row_mode(reader, "flg", flag_row, 1)
        call parquet_read_array_row_mode(reader, "txt", txt_row, 1)
        call parquet_read_array_element_mode(reader, "flg", flag_elem, 1)
        call parquet_read_array_element_mode(reader, "txt", txt_elem, 2)
        call parquet_close_reader(reader)
        ! Physical row 5: flg = [mod(5,2)==0, 5>6] = [F, F]; txt = ["a5", "b5"].
        call check(error, .not. flag_row(1) .and. .not. flag_row(2), &
            "filtered logical row 1 must be physical row 5's flags")
        if (allocated(error)) return
        call check(error, txt_row(1) == "a5" .and. txt_row(2) == "b5", &
            "filtered string row 1 must be physical row 5's strings")
        if (allocated(error)) return
        call check(error, all(flag_elem .eqv. [(mod(i, 2) == 0, i = 5, 12)]), &
            "logical element 1 across the filtered rows must be physical rows 5..12")
        if (allocated(error)) return
        do i = 5, 12
            write(expect_elem(i - 4), '(a,i0)') "b", i
        end do
        call check(error, all(txt_elem == expect_elem), &
            "string element 2 across the filtered rows must be physical rows 5..12")
    end subroutine test_row_and_element_mode_filtered_bool_string
    !
    !> The temporal pair (read_temporal_row/read_temporal_element), which is its own template
    !> again: date vector columns read by row and by element on a filtered reader.
    subroutine test_row_and_element_mode_filtered_temporal(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_date) :: row_back(2), elem_back(8)
        type(parquet_date) :: expect(8)
        integer :: i
        character(len=*), parameter :: file = "test_run/filter_row_element_temporal.parquet"

        call write_vector_fixture(file, 12, 3)
        call filt%add("v > 4")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_read_array_row_mode(reader, "dt", row_back, 1)
        call parquet_read_array_element_mode(reader, "dt", elem_back, 1)
        call parquet_close_reader(reader)
        call check(error, row_back(1) == parquet_date(2024, 1, 5) .and. row_back(2) == parquet_date(2024, 6, 5), &
            "filtered date row 1 must be physical row 5's dates")
        if (allocated(error)) return
        do i = 5, 12
            expect(i - 4) = parquet_date(2024, 1, 1) + int(i - 1, int32)
        end do
        call check(error, all(elem_back == expect), &
            "date element 1 across the filtered rows must be physical rows 5..12")
    end subroutine test_row_and_element_mode_filtered_temporal
    !
    !> A sample mask reaches row/element mode through exactly the same field a filter does, so it
    !> gets the same treatment -- checked here against the filter-free path by drawing a sample
    !> that keeps every row, which must then agree row for row with an unsampled read.
    subroutine test_row_and_element_mode_sampled(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        integer(int64) :: nrows
        integer(int32) :: row_back(3), elem_back(12)
        integer :: i
        character(len=*), parameter :: file = "test_run/filter_row_element_sampled.parquet"

        call write_vector_fixture(file, 12, 3)
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=11)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 12_int64, "the near-1.0 draw must keep every row")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_array_row_mode(reader, "vec", row_back, 7)
        call parquet_read_array_element_mode(reader, "vec", elem_back, 3)
        call parquet_close_reader(reader)
        call check(error, all(row_back == [701, 702, 703]), "sampled row 7 must still be physical row 7")
        if (allocated(error)) return
        call check(error, all(elem_back == [(100 * i + 3, i = 1, 12)]), &
            "sampled element 3 must still span every physical row")
    end subroutine test_row_and_element_mode_sampled
    !
    !> Writes the NaN fixture the group below filters against: u = 1..6 (never null), and the same
    !> six rows as a real64 column x and a real32 column y holding a value, a NaN and a Null twice
    !> over. A NaN and a Null in one fixture is the point -- the two are routinely conflated, and
    !> every test here turns on their behaving differently.
    subroutine write_nan_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: u(6) = [1, 2, 3, 4, 5, 6]
        real(real64) :: x(6)
        real(real32) :: y(6)
        logical :: valid(6) = [.true., .true., .false., .true., .true., .false.]

        x = [1.0_real64, ieee_value(0.0_real64, ieee_quiet_nan), 0.0_real64, &
            4.0_real64, ieee_value(0.0_real64, ieee_quiet_nan), 0.0_real64]
        y = real(x, real32)
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "u", u)
        call parquet_write_column(writer, "x", x, is_valid=valid)
        call parquet_write_column(writer, "y", y, is_valid=valid)
        call parquet_close_writer(writer)
    end subroutine write_nan_fixture
    !
    !> The behaviour the documentation now states outright, and the reason is_nan exists: a NaN is
    !> a VALUE, not a missing one, so it is never unknown. Every ordering/equality comparison
    !> against it is false (IEEE), which excludes it -- but /= against it is true, which admits it.
    !> That asymmetry is the exact opposite of a Null's.
    subroutine test_nan_under_comparisons(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_nan_comparisons.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "x > 0", got)
        call check(error, size(got) == 2, "NaN under '>': expected only the two ordinary values")
        if (allocated(error)) return
        call check(error, all(got == [1, 4]), "NaN under '>': expected rows 1, 4 (NaN and Null both excluded)")
        if (allocated(error)) return
        deallocate(got)
        call filtered_u(file, "x /= 999", got)
        call check(error, size(got) == 4, "NaN under '/=': expected the four non-null rows")
        if (allocated(error)) return
        call check(error, all(got == [1, 2, 4, 5]), &
            "NaN under '/=': expected rows 1, 2, 4, 5 -- NaN /= anything is true, so NaN rows survive")
    end subroutine test_nan_under_comparisons
    !
    !> Negation separates the two cleanly: a NaN comparison is false, so negating it yields true
    !> and the NaN row survives; a Null comparison is unknown, so negating it stays unknown and the
    !> Null row does not. One expression, both rules.
    subroutine test_nan_under_not(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_nan_under_not.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "not x > 0", got)
        call check(error, size(got) == 2, "NaN under 'not': expected the two NaN rows only")
        if (allocated(error)) return
        call check(error, all(got == [2, 5]), &
            "NaN under 'not': expected rows 2, 5 -- the NaN rows come in, the Null rows stay out")
    end subroutine test_nan_under_not
    !
    !> The two new operators doing the job they were added for, without the caller having to know
    !> any of the above.
    subroutine test_is_nan_selects(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_is_nan_selects.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "x is_nan", got)
        call check(error, size(got) == 2, "is_nan: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [2, 5]), "is_nan: expected the two NaN rows 2, 5")
        if (allocated(error)) return
        deallocate(got)
        call filtered_u(file, "x is_not_nan", got)
        call check(error, size(got) == 2, "is_not_nan: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [1, 4]), "is_not_nan: expected the two ordinary-value rows 1, 4")
    end subroutine test_is_nan_selects
    !
    !> The deliberate design decision behind the pair: they are Kleene-honest about nullness, so a
    !> Null row is unknown for BOTH of them -- is_not_nan means "is a real number", not "is not a
    !> NaN, whatever else it might be". That keeps nullness governed solely by is_null/is_not_null,
    !> and "a real number or nothing at all" is written by saying so.
    subroutine test_is_nan_null_is_unknown(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_is_nan_null.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "x is_nan or x is_null", got)
        call check(error, size(got) == 4, "is_nan or is_null: expected 4 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [2, 3, 5, 6]), "is_nan or is_null: expected rows 2, 3, 5, 6")
        if (allocated(error)) return
        deallocate(got)
        call filtered_u(file, "x is_not_nan or x is_null", got)
        call check(error, size(got) == 4, "is_not_nan or is_null: expected 4 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [1, 3, 4, 6]), "is_not_nan or is_null: expected rows 1, 3, 4, 6")
    end subroutine test_is_nan_null_is_unknown
    !
    !> The oracle. Under Kleene logic "not (x >= 0 or x < 0)" already meant exactly is_nan before
    !> the operator existed: every non-NaN real satisfies precisely one of the two disjuncts, a NaN
    !> satisfies neither, and a Null is unknown for both. Asserting the two agree row for row
    !> checks the new operator against machinery that was already tested, independently of any
    !> expectation written by hand here.
    subroutine test_is_nan_matches_comparison_oracle(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: direct(:), oracle(:)
        character(len=*), parameter :: file = "test_run/filter_is_nan_oracle.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "x is_nan", direct)
        call filtered_u(file, "not (x >= 0 or x < 0)", oracle)
        call check(error, size(direct) == size(oracle), &
            "is_nan vs oracle: expected the same row count as 'not (x >= 0 or x < 0)'")
        if (allocated(error)) return
        call check(error, all(direct == oracle), "is_nan vs oracle: expected the same surviving rows")
        if (allocated(error)) return
        deallocate(direct, oracle)
        call filtered_u(file, "x is_not_nan", direct)
        call filtered_u(file, "x >= 0 or x < 0", oracle)
        call check(error, size(direct) == size(oracle), &
            "is_not_nan vs oracle: expected the same row count as 'x >= 0 or x < 0'")
        if (allocated(error)) return
        call check(error, all(direct == oracle), "is_not_nan vs oracle: expected the same surviving rows")
    end subroutine test_is_nan_matches_comparison_oracle
    !
    !> Negating either one gives the other -- but only because a Null is unknown for both, so the
    !> unknown rows stay out from either direction. This is the is_nan counterpart of the
    !> "not x is_null == x is_not_null" test above.
    subroutine test_not_is_nan_is_is_not_nan(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: negated(:), direct(:)
        character(len=*), parameter :: file = "test_run/filter_not_is_nan.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "not x is_nan", negated)
        call filtered_u(file, "x is_not_nan", direct)
        call check(error, size(negated) == size(direct), &
            "not is_nan: expected the same row count as is_not_nan")
        if (allocated(error)) return
        call check(error, all(negated == direct) .and. all(direct == [1, 4]), &
            "not is_nan: expected rows 1, 4 either way -- a Null row stays out from both directions")
    end subroutine test_not_is_nan_is_is_not_nan
    !
    !> A valueless operator has to survive the clause scanner's "how many tokens does this clause
    !> take" logic when a combinator follows it, exactly as is_null already had to.
    subroutine test_is_nan_in_expression(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_is_nan_expression.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "x is_not_nan and u > 1", got)
        call check(error, size(got) == 1, "is_nan in an expression: expected 1 surviving row")
        if (allocated(error)) return
        call check(error, all(got == [4]), "is_nan in an expression: expected row 4")
        if (allocated(error)) return
        deallocate(got)
        call filtered_u(file, "u == 1 or x is_nan", got)
        call check(error, size(got) == 3, "is_nan after 'or': expected 3 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [1, 2, 5]), "is_nan after 'or': expected rows 1, 2, 5")
    end subroutine test_is_nan_in_expression
    !
    !> real32 goes through the same accessor as real64 but a different Arrow type id, so the gate
    !> that decides which columns accept the operator has to admit it too.
    subroutine test_is_nan_float32(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_is_nan_float32.parquet"

        call write_nan_fixture(file)
        call filtered_u(file, "y is_nan", got)
        call check(error, size(got) == 2, "is_nan on a float32 column: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [2, 5]), "is_nan on a float32 column: expected rows 2, 5")
    end subroutine test_is_nan_float32
    !
    !> The third accepted type. This library's writer never emits a half_float column, so the
    !> fixture is the shared extended-types one -- whose values are all ordinary, making this an
    !> assertion that the type is ACCEPTED (rather than rejected as a non-floating-point column)
    !> and answers all-false, not that it finds anything.
    subroutine test_is_nan_half_float(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: nan_filt, not_nan_filt
        integer(int64) :: n_nan, n_not_nan
        character(len=*), parameter :: file = "test/fixtures/extended_types.parquet"

        call nan_filt%add("v_half_float is_nan")
        call parquet_open_reader(reader, file, filter=nan_filt)
        call parquet_get_nrows(reader, n_nan)
        call parquet_close_reader(reader)
        call not_nan_filt%add("v_half_float is_not_nan")
        call parquet_open_reader(reader, file, filter=not_nan_filt)
        call parquet_get_nrows(reader, n_not_nan)
        call parquet_close_reader(reader)
        call check(error, n_nan == 0_int64, "is_nan on a half_float column of ordinary values: expected no rows")
        if (allocated(error)) return
        call check(error, n_not_nan == 3_int64, "is_not_nan on a half_float column: expected all 3 rows")
    end subroutine test_is_nan_half_float
    !
end module test_filter
