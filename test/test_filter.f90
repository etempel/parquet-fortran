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
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_is_nan
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
            new_unittest("an ordering comparison on a string column selects the right rows", &
                test_string_ordering_filter), &
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
            new_unittest("a row-bounded filter cuts inside a row group", test_row_bounded_filter), &
            new_unittest("a row-bounded filter with no rules selects the range alone", &
                test_row_bounded_filter_no_rules), &
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
            new_unittest("is_nan on a half_float column", test_is_nan_half_float), &
            new_unittest("remap_column_names: every grammar form survives the round trip", &
                test_remap_round_trip_forms), &
            new_unittest("remap_column_names: an identity rename changes nothing", test_remap_identity), &
            new_unittest("remap_column_names: a name no rule mentions is a no-op", test_remap_unmentioned_name), &
            new_unittest("remap_column_names: two names swap in one call", test_remap_simultaneous_swap), &
            new_unittest("remap_column_names: a quoted literal spelling a column name is untouched", &
                test_remap_leaves_literals_alone), &
            new_unittest("remap_column_names: only whole names match, not prefixes", test_remap_matches_whole_name), &
            new_unittest("remap_column_names: an unparseable rule is left for the reader to report", &
                test_remap_keeps_bad_rule), &
            new_unittest("sortkey remap_column_names: direction and nulls_first survive", &
                test_sortkey_remap_round_trip), &
            new_unittest("in: a bound set keeps its members, repeats collapsed", test_in_basic), &
            new_unittest("in: agrees with the ==-chain it is shorthand for", test_in_equals_chain_oracle), &
            new_unittest("not_in: complements in, and agrees with the /= chain", test_not_in_complements_in), &
            new_unittest("in: an empty set matches nothing, not_in everything", test_in_empty_set), &
            new_unittest("in/not_in: a null row is UNKNOWN under both", test_in_null_is_unknown), &
            new_unittest("in: a bound set composes with or and >", test_in_composes_with_expression), &
            new_unittest("not (v in @s) equals v not_in @s", test_not_of_in_equals_not_in), &
            new_unittest("not (v in @s) does not admit null rows", test_not_of_in_keeps_nulls_out), &
            new_unittest("is_null is the only way a null row enters a set result", &
                test_is_null_readmits_null_rows), &
            new_unittest("two bound sets on two columns are AND-combined", test_two_bound_sets), &
            new_unittest("in: an integer set widens across kinds", test_in_integer_kinds_widen), &
            new_unittest("in: a real set matches exactly, and -0.0 keys as +0.0", &
                test_in_real_set_and_zero_signs), &
            new_unittest("in: a NaN row matches nothing and survives not_in", &
                test_in_nan_row_matches_nothing), &
            new_unittest("in: a string set, from an array and from a string column", test_in_string_set), &
            new_unittest("bind: is_valid= drops the masked-off members", test_bind_mask_drops_elements), &
            new_unittest("in: a set clause survives remap_column_names", test_in_survives_remap), &
            new_unittest("remap_column_names cannot rewrite a set NAME", &
                test_remap_leaves_set_name_alone), &
            new_unittest("a copied filter carries its bound sets", test_filter_copy_carries_sets), &
            new_unittest("in: the bounded engine agrees with the unscoped one", &
                test_in_bounded_matches_unscoped) &
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
    !> An ORDERING comparison on a string column, which nothing else in this suite covers: every
    !> other string-valued filter test uses `==`, and equality is symmetric, so it cannot tell a
    !> correct comparison from one whose operands are the wrong way round. Confirmed by mutation:
    !> swapping compare_op's operands at eval_filter_clause's string arm passes the entire suite
    !> without this test and fails with it.
    !>
    !> The fixture is deliberately asymmetric -- `s > "banana"` keeps two rows while the reversed
    !> reading keeps one -- so the row COUNT alone separates the two, before any value is compared.
    subroutine test_string_ordering_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=8) :: names(4)
        character(len=8) :: got(2)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_string_ordering.parquet"

        names(1) = "apple"
        names(2) = "mango"
        names(3) = "zebra"
        names(4) = "banana"
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "name", names)
        call parquet_close_writer(writer)

        call filt%add('name > "banana"')
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 2_int64, &
            "a string ordering filter must keep exactly the lexicographically greater rows")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "name", got)
        call parquet_close_reader(reader)
        call check(error, trim(got(1)) == "mango" .and. trim(got(2)) == "zebra", &
            "a string ordering filter kept the wrong rows, or lost their file order")
    end subroutine test_string_ordering_filter
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
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=11_int64)
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
        call parquet_open_reader(reader, file, filter=filt, sample_fraction=0.999999_real64, sample_seed=3_int64)
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
    !> The row-BOUNDED form narrows one step further than the row-group-scoped one: a row-group
    !> range can only start and end on a row-group boundary, so a caller wanting rows 5..8 of a
    !> file chunked in threes would otherwise get back every survivor of row groups 2 and 3
    !> (rows 4..9) with no way to trim them -- only the mask knows which physical rows those are.
    !> With the row range the answer is exactly the requested rows, and parquet_get_nrows is that
    !> range's own surviving count.
    subroutine test_row_bounded_filter(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        integer(int32), allocatable :: back(:)
        character(len=*), parameter :: file = "test_run/filter_row_bounded.parquet"

        call write_chunked_fixture(file, 12, 3)
        ! "v > 4" matches rows 5..12; row groups 2-3 cover rows 4..9; the row range then cuts that
        ! to 5..8, so exactly 5, 6, 7, 8 survive -- one row narrower at each end than the
        ! row-group scope alone would give.
        call filt%add("v > 4")
        call parquet_open_reader(reader, file)
        call parquet_reader_set_filter(reader, filt, 2, 3, 5, 8)
        call parquet_get_nrows(reader, nrows)
        allocate(back(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", back)
        call parquet_close_reader(reader)
        call check(error, nrows == 4_int64, "row-bounded filter: rows 5..8 of a 3-row-group scope leave 4 rows")
        if (allocated(error)) return
        call check(error, all(back == [5, 6, 7, 8]), "row-bounded filter: must yield exactly rows 5, 6, 7, 8")
        if (allocated(error)) return

        ! Same range in integer(int64) bounds must mean the same thing.
        block
            type(parquet_filter) :: wide_filter
            integer(int64) :: nrows64
            call wide_filter%add("v > 4")
            call parquet_open_reader(reader, file)
            call parquet_reader_set_filter(reader, wide_filter, 2_int64, 3_int64, 5_int64, 8_int64)
            call parquet_get_nrows(reader, nrows64)
            call parquet_close_reader(reader)
            call check(error, nrows64 == nrows, "row-bounded filter: int64 bounds must match int32 bounds")
        end block
    end subroutine test_row_bounded_filter
    !
    !> A row-bounded call may carry NO rules at all: the range alone then decides which rows match.
    !> This is how a row scope is installed for its own sake -- a caller that wants only a row
    !> range (a parquet_table slice carrying sample_fraction= but no filter=, for instance) has no
    !> clause to hang it on, and a rule-less filter would otherwise be a silent no-op.
    subroutine test_row_bounded_filter_no_rules(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: empty_filter
        integer(int64) :: nrows, chunk_rows
        integer(int32), allocatable :: back(:)
        character(len=*), parameter :: file = "test_run/filter_row_bounded_no_rules.parquet"

        call write_chunked_fixture(file, 12, 3)
        call parquet_open_reader(reader, file)
        call parquet_reader_set_filter(reader, empty_filter, 2, 3, 5, 8)
        call parquet_get_nrows(reader, nrows)
        allocate(back(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", back)
        call parquet_get_chunk_size(reader, chunk_rows, row_group=1_int64)
        call parquet_close_reader(reader)
        call check(error, nrows == 4_int64, "row-bounded no-rules filter: rows 5..8 leave 4 rows")
        if (allocated(error)) return
        call check(error, all(back == [5, 6, 7, 8]), "row-bounded no-rules filter: must yield exactly rows 5, 6, 7, 8")
        if (allocated(error)) return
        call check(error, chunk_rows == 0_int64, &
            "row-bounded no-rules filter: a row group outside the scope must yield no rows")
    end subroutine test_row_bounded_filter_no_rules
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
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=7_int64)
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
        call parquet_open_reader(reader, file, sample_fraction=0.999999_real64, sample_seed=11_int64)
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
    ! ------------------------------------------------------------------------------
    ! %remap_column_names (parquet_filter / parquet_sortkey)
    !
    ! The property under test throughout is EQUIVALENCE, not rendered text: a rule
    ! written in alias names and then remapped must select exactly the rows the same
    ! rule written directly in the file's own names selects. That is what makes these
    ! tests a real check on parquet_render_filter_expr, which %remap_column_names is
    ! the first caller to depend on semantically rather than for display.
    ! ------------------------------------------------------------------------------
    !
    !> Reads column "v" of `file` under `rule`, after renaming `from` to `to` throughout it.
    subroutine filtered_v_remapped(file, rule, from, to, values)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: rule !! the filter expression, in the `from` vocabulary.
        character(len=*), intent(in) :: from(:) !! names to replace.
        character(len=*), intent(in) :: to(:) !! replacement for each entry of `from`.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows of column "v".
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows

        call filt%add(rule)
        call filt%remap_column_names(from, to)
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", values)
        call parquet_close_reader(reader)
    end subroutine filtered_v_remapped
    !
    !> Every shape the grammar can take -- precedence, parentheses, not, double negation, a
    !> multi-clause range, a null test -- written once in alias names and remapped, and once in the
    !> file's own names directly. The two must select the same rows for every one of them.
    !>
    !> This is the parse -> rename -> render -> parse fidelity check: the remapped side goes
    !> through parquet_render_filter_expr and back through the parser, the direct side does not.
    subroutine test_remap_round_trip_forms(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:), want(:)
        integer :: i
        character(len=*), parameter :: file = "test_run/filter_remap_forms.parquet"
        character(len=48) :: aliased(8), direct(8)

        aliased(1) = "V > 5";                          direct(1) = "v > 5"
        aliased(2) = "V > 8 or V < 3 and W > 9";       direct(2) = "v > 8 or v < 3 and w > 9"
        aliased(3) = "(V > 8 or V < 3) and W > 9";     direct(3) = "(v > 8 or v < 3) and w > 9"
        aliased(4) = "not V > 5";                      direct(4) = "not v > 5"
        aliased(5) = "not (V > 3 and V < 8)";          direct(5) = "not (v > 3 and v < 8)"
        aliased(6) = "not not V > 5";                  direct(6) = "not not v > 5"
        aliased(7) = "V >= 4 and V <= 7";              direct(7) = "v >= 4 and v <= 7"
        aliased(8) = "V is_not_null or W == 3";        direct(8) = "v is_not_null or w == 3"

        call write_grid_fixture(file)
        do i = 1, size(aliased)
            call filtered_v_remapped(file, trim(aliased(i)), ["V", "W"], ["v", "w"], got)
            call filtered_v(file, trim(direct(i)), want)
            call check(error, size(got) == size(want) .and. all(got == want), &
                "remap round trip disagreed with the direct rule: " // trim(aliased(i)))
            if (allocated(error)) return
            deallocate(got, want)
        end do
    end subroutine test_remap_round_trip_forms
    !
    !> Renaming a name to itself must be a no-op end to end. Worth its own test because it is the
    !> one case where a round-trip defect could not be blamed on the substitution.
    subroutine test_remap_identity(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:), want(:)
        character(len=*), parameter :: file = "test_run/filter_remap_identity.parquet"

        call write_grid_fixture(file)
        call filtered_v_remapped(file, "(v > 8 or v < 3) and w > 9", ["v", "w"], ["v", "w"], got)
        call filtered_v(file, "(v > 8 or v < 3) and w > 9", want)
        call check(error, size(got) == size(want) .and. all(got == want), &
            "an identity remap changed which rows the filter selects")
    end subroutine test_remap_identity
    !
    !> A `from` entry no rule mentions must change nothing -- the rename is total, never an
    !> assertion that the name occurs.
    subroutine test_remap_unmentioned_name(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:), want(:)
        character(len=*), parameter :: file = "test_run/filter_remap_unmentioned.parquet"

        call write_grid_fixture(file)
        call filtered_v_remapped(file, "v > 5", ["absent_column"], ["also_absent "], got)
        call filtered_v(file, "v > 5", want)
        call check(error, size(got) == size(want) .and. all(got == want), &
            "renaming a column the rule never mentions changed the result")
    end subroutine test_remap_unmentioned_name
    !
    !> Two names exchanged in ONE call. Sequential text substitution would get this wrong (the
    !> first rename's output would be re-matched by the second); substituting in the parsed leaf
    !> array, once per leaf, cannot.
    subroutine test_remap_simultaneous_swap(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:), want(:)
        character(len=*), parameter :: file = "test_run/filter_remap_swap.parquet"

        call write_grid_fixture(file)
        call filtered_v_remapped(file, "v > 5 and w > 5", ["v", "w"], ["w", "v"], got)
        call filtered_v(file, "w > 5 and v > 5", want)
        call check(error, size(got) == size(want) .and. all(got == want), &
            "swapping two column names in one call did not produce the swapped filter")
    end subroutine test_remap_simultaneous_swap
    !
    !> A quoted string value that happens to spell the column name being renamed must come through
    !> untouched. This is the whole reason the substitution works on the parsed leaf array instead
    !> of the rule text: a text-level rename would corrupt the literal.
    subroutine test_remap_leaves_literals_alone(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        character(len=24) :: names(3)
        character(len=24) :: got(1)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_remap_literal.parquet"

        names(1) = "plain"
        names(2) = "V and (W) or V"
        names(3) = "other"
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "name", names)
        call parquet_close_writer(writer)

        ! The column is called V in the rule and `name` in the file; the VALUE also says "V".
        call filt%add('V == "V and (W) or V"')
        call filt%remap_column_names(["V"], ["name"])
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call check(error, nrows == 1_int64, &
            "remap over a literal spelling the column name: expected exactly the one matching row")
        if (allocated(error)) then
            call parquet_close_reader(reader)
            return
        end if
        call parquet_read_column(reader, "name", got)
        call parquet_close_reader(reader)
        call check(error, trim(got(1)) == "V and (W) or V", &
            "remap over a literal spelling the column name: the literal was altered")
    end subroutine test_remap_leaves_literals_alone
    !
    !> "v" must not match the column "vv". A prefix/substring match here would silently rewrite a
    !> different column's clause, which is a wrong-answer bug with nothing to notice it.
    subroutine test_remap_matches_whole_name(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32) :: a(4) = [1, 2, 3, 4], b(4) = [40, 30, 20, 10]
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_remap_prefix.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "vv", a)
        call parquet_write_column(writer, "vvv", b)
        call parquet_close_writer(writer)

        ! Renaming "vv" must leave the "vvv" clause alone; if it did not, the rule would name a
        ! column that does not exist and the open below would abort rather than return 1 row.
        call filt%add("vv > 3 and vvv > 5")
        call filt%remap_column_names(["vv"], ["vv"])
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        call check(error, nrows == 1_int64, "remap matched a name prefix instead of the whole name")
    end subroutine test_remap_matches_whole_name
    !
    !> A rule that does not parse is deliberately left as-is, so the reader that applies it still
    !> reports the parse failure with the file named. Checked here through the surviving TEXT (the
    !> abort itself is covered out of process by the filter_remap_keeps_bad_rule scenario): a
    !> renamed-away rule would no longer be recognizable.
    subroutine test_remap_keeps_bad_rule(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_filter) :: filt

        call filt%add("v >")                    ! missing value: does not parse
        call filt%add("v > 5")                  ! parses, and does get renamed
        call filt%remap_column_names(["v"], ["w"])
        call check(error, trim(filt%rules(1)) == "v >", &
            "an unparseable rule was rewritten instead of being left for the reader to report")
        if (allocated(error)) return
        call check(error, index(filt%rules(2), "w") > 0, &
            "a parseable rule alongside an unparseable one was not renamed")
    end subroutine test_remap_keeps_bad_rule
    !
    !> The sort twin: an aliased key must order rows exactly as the direct key does, for both
    !> directions and with nulls_first carried across (it lives in its own component, so the check
    !> is that the rewrite does not disturb it).
    subroutine test_sortkey_remap_round_trip(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_sortkey) :: srt
        integer(int32) :: id(6) = [1, 2, 3, 4, 5, 6]
        integer(int32) :: v(6) = [30, 10, 50, 20, 60, 40]
        integer(int32) :: got(6)
        character(len=*), parameter :: file = "test_run/filter_remap_sortkey.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "id", id)
        call parquet_write_column(writer, "v", v)
        call parquet_close_writer(writer)

        call srt%add("-KEY")                       ! '-' shorthand, so direction must survive too
        call srt%add("ID", nulls_first=.true.)
        call srt%remap_column_names(["KEY", "ID "], ["v  ", "id "])
        call check(error, trim(srt%keys(1)) == "v desc", &
            "sortkey remap did not rewrite the '-' shorthand key as an explicit descending key")
        if (allocated(error)) return
        call check(error, srt%nulls_first(2), "sortkey remap did not carry nulls_first across")
        if (allocated(error)) return

        call parquet_open_reader(reader, file, sort_by=srt)
        call parquet_read_column(reader, "id", got)
        call parquet_close_reader(reader)
        call check(error, all(got == [5, 3, 6, 1, 4, 2]), &
            "a remapped sort key did not order the rows the way the direct key does")
    end subroutine test_sortkey_remap_round_trip
    !
    !
    ! ---- Set-valued clauses: `in` / `not_in` over a bound set ----
    !
    !> Reads column "v" of `file` under a filter carrying one bound integer set, and returns the
    !> surviving values. The set is bound with %add_in, the one-set-one-column sugar.
    subroutine filtered_in(file, column, want, values, negate)
        character(len=*), intent(in) :: file !! fixture to read.
        character(len=*), intent(in) :: column !! the column the clause tests.
        integer(int32), intent(in) :: want(:) !! the set's members.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows of column "v".
        logical, intent(in), optional :: negate !! .true. spells the clause `not_in`.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows

        call filt%add_in(column, want, negate=negate)
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(values(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", values)
        call parquet_close_reader(reader)
    end subroutine filtered_in
    !
    !> The baseline: `v in @s` keeps exactly the rows whose value is a member, and a REPEATED
    !> member in the caller's array is collapsed rather than refused.
    !>
    !> The repeat is not incidental. A real identifier list comes out of a join or a group-by and
    !> very often carries one; pf_index_map%build refuses a duplicate key by contract, so %bind has
    !> to deduplicate before it ever gets there. Drop that dedup and this test aborts.
    subroutine test_in_basic(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        character(len=*), parameter :: file = "test_run/filter_in_basic.parquet"

        call write_grid_fixture(file)
        call filtered_in(file, "v", [3, 7, 7, 9], got)
        call check(error, size(got) == 3, "in: expected 3 surviving rows from a set with one repeat")
        if (allocated(error)) return
        call check(error, all(got == [3, 7, 9]), "in: expected rows 3, 7, 9")
    end subroutine test_in_basic
    !
    !> `in` must agree with the `==` chain it is shorthand for, row for row.
    !>
    !> An INDEPENDENT oracle in the strict sense: the chain never touches %bind, pf_index_map, the
    !> verdict array or the pre-evaluated leaf kind, so the two spellings share no machinery below
    !> the parser. The same pattern test_filter.f90 already uses for `is_nan` against
    !> `not (x >= 0 or x < 0)`.
    subroutine test_in_equals_chain_oracle(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: by_set(:), by_chain(:)
        character(len=*), parameter :: file = "test_run/filter_in_oracle.parquet"

        call write_grid_fixture(file)
        call filtered_in(file, "v", [2, 5, 6, 10], by_set)
        call filtered_v(file, "v == 2 or v == 5 or v == 6 or v == 10", by_chain)
        call check(error, size(by_set) == size(by_chain), &
            "in vs ==-chain: the two spellings kept different row counts")
        if (allocated(error)) return
        call check(error, all(by_set == by_chain), "in vs ==-chain: the two spellings kept different rows")
    end subroutine test_in_equals_chain_oracle
    !
    !> `not_in` is the complement of `in` over the non-null rows, and both spellings agree with
    !> their `==`/`/=` chains.
    subroutine test_not_in_complements_in(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: kept(:), dropped(:), by_chain(:)
        character(len=*), parameter :: file = "test_run/filter_not_in.parquet"

        call write_grid_fixture(file)
        call filtered_in(file, "v", [4, 8], kept)
        call filtered_in(file, "v", [4, 8], dropped, negate=.true.)
        call check(error, size(kept) + size(dropped) == 10, &
            "not_in: in and not_in must partition a null-free column")
        if (allocated(error)) return
        call filtered_v(file, "v /= 4 and v /= 8", by_chain)
        call check(error, size(dropped) == size(by_chain), "not_in: row count differs from the /= chain")
        if (allocated(error)) return
        call check(error, all(dropped == by_chain), "not_in: rows differ from the /= chain")
    end subroutine test_not_in_complements_in
    !
    !> An EMPTY set matches nothing under `in` and everything (non-null) under `not_in`.
    !>
    !> Worth its own test precisely because "nothing survived" is otherwise the signature of a bug:
    !> here it is the right answer, and the screen's own counterpart (every row group pruned) is
    !> asserted in test_filter_screen.f90.
    subroutine test_in_empty_set(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: none(:), all_rows(:)
        character(len=*), parameter :: file = "test_run/filter_in_empty.parquet"

        call write_grid_fixture(file)
        call filtered_in(file, "v", [integer(int32) ::], none)
        call check(error, size(none) == 0, "in with an empty set must keep no row")
        if (allocated(error)) return
        call filtered_in(file, "v", [integer(int32) ::], all_rows, negate=.true.)
        call check(error, size(all_rows) == 10, "not_in with an empty set must keep every row")
    end subroutine test_in_empty_set
    !
    !> A NULL row is UNKNOWN under both `in` and `not_in`, exactly as under every comparison --
    !> `is_null` stays the only way a null row enters a result.
    !>
    !> The negative control is the second half: if `not_in` admitted null rows (two-valued
    !> negation), rows 3 and 4 would survive it. They must not.
    subroutine test_in_null_is_unknown(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: f_in, f_not
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_nulls.parquet"

        call write_null_fixture(file)
        ! v = [1, 2, NULL, NULL, 5, 6]; the set names one present value and one absent one.
        call f_in%add_in("v", [2, 99])
        call parquet_open_reader(reader, file, filter=f_in)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "u", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 1, "in: a null row must not match, so only row 2 survives")
        if (allocated(error)) return
        call check(error, got(1) == 2, "in: the surviving row must be row 2")
        if (allocated(error)) return

        deallocate(got)
        call f_not%add_in("v", [2, 99], negate=.true.)
        call parquet_open_reader(reader, file, filter=f_not)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "u", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 3, &
            "not_in: a null row is UNKNOWN, not true -- only the three non-null non-members survive")
        if (allocated(error)) return
        call check(error, all(got == [1, 5, 6]), "not_in: expected rows 1, 5 and 6")
    end subroutine test_in_null_is_unknown
    !
    !> A bound set composes with the rest of the grammar: `or`, `not` and parentheses all see an
    !> ordinary leaf, which is the whole reason the clause is text and only its payload an array.
    subroutine test_in_composes_with_expression(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_expression.parquet"

        call write_grid_fixture(file)
        call filt%bind("wanted", [3_int32, 7_int32])
        call filt%add("v in @wanted or v > 8")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 4, "in inside an expression: expected 4 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [3, 7, 9, 10]), "in inside an expression: expected rows 3, 7, 9, 10")
    end subroutine test_in_composes_with_expression
    !
    !> `not (v in @s)` equals `v not_in @s` on a null-free column -- the enclosing `not` node
    !> negates a pre-evaluated leaf through the ordinary Kleene negation, so the two spellings meet.
    subroutine test_not_of_in_equals_not_in(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: negated(:), direct(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_negated.parquet"

        call write_grid_fixture(file)
        call filt%bind("s", [2_int32, 4_int32, 6_int32])
        call filt%add("not (v in @s)")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(negated(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", negated)
        call parquet_close_reader(reader)

        call filtered_in(file, "v", [2, 4, 6], direct, negate=.true.)
        call check(error, size(negated) == size(direct), "not(in) vs not_in: different row counts")
        if (allocated(error)) return
        call check(error, all(negated == direct), "not(in) vs not_in: different rows")
    end subroutine test_not_of_in_equals_not_in
    !
    !> Two independent sets in one filter, on two columns, AND-combined.
    subroutine test_two_bound_sets(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_two_sets.parquet"

        call write_grid_fixture(file)
        ! v = 1..10 and w = 10..1, so row k has v = k and w = 11 - k.
        call filt%bind("vs", [2_int32, 4_int32, 6_int32, 8_int32])
        call filt%bind("ws", [7_int32, 5_int32])
        call filt%add("v in @vs")
        call filt%add("w in @ws")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "two sets: expected rows 4 and 6 (w = 7 and 5)")
        if (allocated(error)) return
        call check(error, all(got == [4, 6]), "two sets: expected v = 4 and 6")
    end subroutine test_two_bound_sets
    !
    !> An int32 set matches an int64 column and the reverse: an integer set widens to int64 and is
    !> accepted against any integer column, exactly as an integer literal is.
    subroutine test_in_integer_kinds_widen(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: f32, f64
        integer(int64) :: big(6) = [10_int64, 20_int64, 30_int64, 40_int64, 50_int64, 60_int64]
        integer(int64), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_kinds.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "big", big)
        call parquet_close_writer(writer)

        call f32%add_in("big", [20_int32, 50_int32])
        call parquet_open_reader(reader, file, filter=f32)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "big", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "an int32 set against an int64 column: expected 2 rows")
        if (allocated(error)) return
        call check(error, all(got == [20_int64, 50_int64]), "an int32 set against an int64 column: wrong rows")
        if (allocated(error)) return

        deallocate(got)
        call f64%add_in("big", [30_int64, 60_int64])
        call parquet_open_reader(reader, file, filter=f64)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "big", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "an int64 set against an int64 column: expected 2 rows")
        if (allocated(error)) return
        call check(error, all(got == [30_int64, 60_int64]), "an int64 set against an int64 column: wrong rows")
    end subroutine test_in_integer_kinds_widen
    !
    !> A REAL set matches by exact IEEE equality, and -0.0 and +0.0 are ONE key in both directions.
    !>
    !> The zero pair is the sharp half, and it is the mutation this test exists for: drop the
    !> normalisation on either side and a column holding -0.0 stops matching a set holding +0.0,
    !> which is a bit pattern nothing produces unless a test asks for it. Both zeros are built at
    !> RUNTIME (negated from a variable) rather than written as literals, because the sign of a
    !> negative zero in a constant expression is its own portability question.
    subroutine test_in_real_set_and_zero_signs(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        real(real64) :: col(4), zero, neg_zero
        real(real64), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_real.parquet"

        zero = 0.0_real64
        neg_zero = -zero
        ! The fixture's precondition: this toolchain really does distinguish the two patterns.
        ! Without it a compiler that loses the sign would fail this test blaming the library.
        call check(error, transfer(neg_zero, 0_int64) /= transfer(zero, 0_int64), &
            "fixture precondition: this build does not represent -0.0 distinctly, so the test " // &
            "cannot say anything about the normalisation")
        if (allocated(error)) return

        col = [1.5_real64, neg_zero, 2.5_real64, 3.5_real64]
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "x", col)
        call parquet_close_writer(writer)

        ! A set holding +0.0 must match the column's -0.0.
        call filt%add_in("x", [zero, 2.5_real64])
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "x", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "a real set: +0.0 in the set must match -0.0 in the column")
        if (allocated(error)) return
        call check(error, got(1) == 0.0_real64 .and. got(2) == 2.5_real64, &
            "a real set: expected the zero row and 2.5")
    end subroutine test_in_real_set_and_zero_signs
    !
    !> A NaN ROW matches nothing under `in`, and therefore survives `not_in` -- the filter's own
    !> IEEE rule, not pf_in's (where a NaN equals another NaN).
    !>
    !> It holds by construction rather than by a check: %bind refuses a NaN, so no NaN pattern is
    !> ever a key and a NaN row's lookup finds nothing whatever payload bits it carries. See
    !> feature_risks.md R-j.
    subroutine test_in_nan_row_matches_nothing(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: f_in, f_not
        real(real64) :: col(4)
        real(real64), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_nan.parquet"

        col = [1.0_real64, ieee_value(1.0_real64, ieee_quiet_nan), 2.0_real64, 3.0_real64]
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "x", col)
        call parquet_close_writer(writer)

        call f_in%add_in("x", [1.0_real64, 2.0_real64])
        call parquet_open_reader(reader, file, filter=f_in)
        call parquet_get_nrows(reader, nrows)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "in: a NaN row must not match a set of ordinary values")
        if (allocated(error)) return

        call f_not%add_in("x", [1.0_real64, 2.0_real64], negate=.true.)
        call parquet_open_reader(reader, file, filter=f_not)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "x", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, &
            "not_in: a NaN row is a VALUE that matches nothing, so it survives -- unlike a Null")
        if (allocated(error)) return
        call check(error, ieee_is_nan(got(1)) .or. ieee_is_nan(got(2)), &
            "not_in: the NaN row must be among the survivors")
    end subroutine test_in_nan_row_matches_nothing
    !
    !> A STRING set, bound from a character array and from a parquet_string_column, selects the
    !> same rows -- and each element of the character array is trimmed on the way in.
    subroutine test_in_string_set(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_writer) :: writer
        type(parquet_reader) :: reader
        type(parquet_filter) :: f_arr, f_col
        type(parquet_string_column) :: set_col
        character(len=8) :: names(5) = ["alpha   ", "beta    ", "gamma   ", "delta   ", "epsilon "]
        character(len=16), allocatable :: got(:)
        integer(int64) :: n_arr, n_col
        character(len=*), parameter :: file = "test_run/filter_in_strings.parquet"

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "name", names)
        call parquet_close_writer(writer)

        ! The array form: elements are blank-padded to len=8 and must be trimmed, or nothing matches.
        call f_arr%add_in("name", ["beta ", "delta"])
        call parquet_open_reader(reader, file, filter=f_arr)
        call parquet_get_nrows(reader, n_arr)
        allocate(got(n_arr))
        if (n_arr > 0) call parquet_read_column(reader, "name", got)
        call parquet_close_reader(reader)
        call check(error, n_arr == 2, "a string set from a character array: expected 2 rows")
        if (allocated(error)) return
        call check(error, trim(got(1)) == "beta" .and. trim(got(2)) == "delta", &
            "a string set from a character array: expected beta and delta")
        if (allocated(error)) return

        ! The parquet_string_column form must select exactly the same rows.
        call set_col%append_string("beta")
        call set_col%append_string("delta")
        call f_col%add_in("name", set_col)
        call parquet_open_reader(reader, file, filter=f_col)
        call parquet_get_nrows(reader, n_col)
        call parquet_close_reader(reader)
        call check(error, n_col == n_arr, &
            "a string set: the character-array and parquet_string_column forms disagree")
    end subroutine test_in_string_set
    !
    !> An `is_valid=` mask drops the elements it marks, and a NULL element of a bound
    !> parquet_string_column is likewise not in the set.
    subroutine test_bind_mask_drops_elements(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: got(:)
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_masked.parquet"

        call write_grid_fixture(file)
        ! Only the first and third elements are admitted, so 5 and 9 are not in the set at all.
        call filt%add_in("v", [3, 5, 8, 9], is_valid=[.true., .false., .true., .false.])
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "is_valid= on %bind must drop the masked-off members")
        if (allocated(error)) return
        call check(error, all(got == [3, 8]), "is_valid= on %bind: expected rows 3 and 8")
    end subroutine test_bind_mask_drops_elements
    !
    !> A set-valued clause survives %remap_column_names, which is the property that made the
    !> payload an array and the clause TEXT rather than a second, non-text path.
    subroutine test_in_survives_remap(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_remap.parquet"

        call write_grid_fixture(file)
        call filt%add_in("value", [3, 7])
        ! The rule was written in the caller's own vocabulary; the file calls that column "v".
        call filt%remap_column_names(["value"], ["v    "])
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, "a renamed set clause: expected 2 surviving rows")
        if (allocated(error)) return
        call check(error, all(got == [3, 7]), "a renamed set clause: expected rows 3 and 7")
    end subroutine test_in_survives_remap
    !
    !> A filter is COPIED by intrinsic assignment, sets and all -- which is how a parquet_table
    !> keeps the filter it was opened with, and how %clone reattaches it. A set that was referenced
    !> rather than copied would read freed memory here with nothing to notice.
    subroutine test_filter_copy_carries_sets(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: original, copy
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_copied.parquet"

        call write_grid_fixture(file)
        block
            integer(int32), allocatable :: transient(:)
            transient = [2, 5, 8]
            call original%add_in("v", transient)
            deallocate(transient)   ! the caller's array is gone before the filter is ever applied
        end block
        copy = original
        call parquet_open_reader(reader, file, filter=copy)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 3, "a copied filter must carry its bound sets")
        if (allocated(error)) return
        call check(error, all(got == [2, 5, 8]), "a copied filter: expected rows 2, 5 and 8")
    end subroutine test_filter_copy_carries_sets
    !
    !> A set clause reaches the same answer through the BOUNDED (row-group-scoped) engine as
    !> through the caching one, on a fixture whose row groups the screen prunes unevenly.
    !>
    !> This is the coordinate-system test, and the pruning is what gives it teeth: the unscoped
    !> engine gathers a pre-evaluated leaf's verdicts into the LIVE layout, which skips pruned row
    !> groups entirely, while the scoped engine reads them at a physical base per row group. Slice
    !> either one wrongly and the two arms disagree by exactly a pruned row group's length, with
    !> both answers still looking like plausible row sets. See feature_risks.md R-b.
    subroutine test_in_bounded_matches_unscoped(error)
        type(error_type), allocatable, intent(out) :: error
        integer(int32), allocatable :: unscoped(:), bounded(:)
        character(len=*), parameter :: file = "test_run/filter_in_bounded.parquet"

        call write_chunked_fixture(file, 40, 5)
        call in_through_table(file, [3, 7, 33, 38], .false., unscoped)
        call in_through_table(file, [3, 7, 33, 38], .true., bounded)
        call check(error, size(unscoped) == 4, "the unscoped engine kept the wrong number of rows")
        if (allocated(error)) return
        call check(error, size(bounded) == size(unscoped), &
            "bounded vs unscoped: a set clause kept different row counts on the two engines")
        if (allocated(error)) return
        call check(error, all(bounded == unscoped), &
            "bounded vs unscoped: a set clause kept different rows on the two engines")
    end subroutine test_in_bounded_matches_unscoped
    !
    !> Opens `file` as a table with one bound integer set, optionally memory-bounded, and returns
    !> the surviving values of column "v".
    subroutine in_through_table(file, want, bounded, values)
        character(len=*), intent(in) :: file !! fixture to read.
        integer(int32), intent(in) :: want(:) !! the set's members.
        logical, intent(in) :: bounded !! .true. reads one row group at a time.
        integer(int32), allocatable, intent(out) :: values(:) !! surviving rows of column "v".
        type(parquet_table) :: tbl
        type(parquet_filter) :: filt

        call filt%add_in("v", want)
        call parquet_open_table(tbl, file, filter=filt, bounded=bounded)
        call tbl%get("v", values)
    end subroutine in_through_table

    !
    !> `not (v in @s)` must NOT admit a null row -- the negation of UNKNOWN is UNKNOWN.
    !>
    !> This is the ONE observation that separates a null answering UNKNOWN from a null answering
    !> FALSE. Under a bare `in` or `not_in` clause the two are indistinguishable, because unknown
    !> collapses to false at the end anyway; it is only an enclosing `not` that turns a two-valued
    !> false into a surviving true. A mutation replacing KL_UNKNOWN with KL_FALSE passes every
    !> other test in this suite and fails this one.
    subroutine test_not_of_in_keeps_nulls_out(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_not_nulls.parquet"

        call write_null_fixture(file)   ! v = [1, 2, NULL, NULL, 5, 6]
        call filt%bind("s", [2_int32])
        call filt%add("not (v in @s)")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "u", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 3, &
            "not (v in @s): a null row is UNKNOWN and negating it leaves it UNKNOWN, so only the " // &
            "three non-null non-members survive -- five rows would mean the nulls were negated in")
        if (allocated(error)) return
        call check(error, all(got == [1, 5, 6]), "not (v in @s): expected rows 1, 5 and 6")
    end subroutine test_not_of_in_keeps_nulls_out
    !
    !> The positive control for the rule above: `is_null` really is the way a null row gets in, and
    !> it composes with a set clause through an ordinary `or`.
    subroutine test_is_null_readmits_null_rows(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_or_isnull.parquet"

        call write_null_fixture(file)   ! v = [1, 2, NULL, NULL, 5, 6]
        call filt%bind("s", [5_int32])
        call filt%add("v in @s or v is_null")
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "u", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 3, "in or is_null: the one member plus the two null rows")
        if (allocated(error)) return
        call check(error, all(got == [3, 4, 5]), "in or is_null: expected rows 3, 4 and 5")
    end subroutine test_is_null_readmits_null_rows

    !
    !> A set name is in the VALUE position, so `%remap_column_names` must leave it alone even when a
    !> column happens to share its spelling.
    !>
    !> Structurally guaranteed today -- the remap matches on `leaf_name` and assigns only
    !> `leaf_name`, never `leaf_value` -- but the guarantee lives in one loop that a future change
    !> to the renderer or the matcher could quietly widen, and the failure would be an unbound-set
    !> abort in a program that had renamed something unrelated. The fixture is deliberately
    !> adversarial: the set and the column being renamed have the SAME name.
    subroutine test_remap_leaves_set_name_alone(error)
        type(error_type), allocatable, intent(out) :: error
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        integer(int64) :: nrows
        character(len=*), parameter :: file = "test_run/filter_in_remap_collision.parquet"

        call write_grid_fixture(file)
        call filt%bind("w", [3_int32, 7_int32])   ! a set named exactly like the column below
        call filt%add("v in @w")
        call filt%remap_column_names(["w"], ["z"])  ! renames the COLUMN w, which this rule never names
        call parquet_open_reader(reader, file, filter=filt)
        call parquet_get_nrows(reader, nrows)
        allocate(got(nrows))
        if (nrows > 0) call parquet_read_column(reader, "v", got)
        call parquet_close_reader(reader)
        call check(error, nrows == 2, &
            "remap must not rewrite '@w' into '@z' -- a set name is a value, not a column name")
        if (allocated(error)) return
        call check(error, all(got == [3, 7]), "remap collision: expected rows 3 and 7")
    end subroutine test_remap_leaves_set_name_alone

end module test_filter
