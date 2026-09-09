!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the table's in-memory filter evaluator: `%row_mask` and the two expression forms
!! of `%filter_rows`.
!!
!! **The suite's spine is an A/B against the READER, and it is the only assertion that can catch
!! the failure this feature actually has.** Two engines answer one grammar -- the C++
!! `eval_filter_clause` over Arrow arrays, and `parquet_eval_filter_leaf` over resident
!! `parquet_column` storage -- so the way this breaks is not a crash or an abort but a row set that
!! is plausible and different. `expect_same_rows` therefore opens the SAME file twice: once through
!! `parquet_open_reader(..., filter=)`, whose surviving physical row indices
!! `parquet_get_physical_row_indices` reports, and once as a fully materialized `parquet_table`,
!! whose `%row_mask` it turns into the same list. A rule passes only when the two lists are equal
!! element for element.
!!
!! Three properties make that a real oracle rather than two spellings of one implementation:
!!
!! * **the reader side never touches the in-memory evaluator.** It goes through the bind(C)
!!   boundary into Arrow, so a mistake in either engine shows up as a difference.
!! * **the rules swept are the ones that are hard**, not the ones that are easy: nulls under
!!   negation, NaN under every operator, a float32 column compared against a real64 bound, a
!!   string with a trailing space (where Fortran's own `==` and C++'s `string_view` disagree),
!!   every temporal type at its own stored unit, and both set spellings.
!! * **every A/B is paired with a check that the row set is neither empty nor everything.** A rule
!!   selecting nothing agrees with a broken engine that also selects nothing, so `expect_same_rows`
!!   refuses to pass on a degenerate answer unless the test says it expects one.
!!
!! What is asserted DIRECTLY rather than by A/B is the part the reader has no equivalent for: that
!! `%row_mask` does not mutate or detach, that `%filter_rows` does, that an all-`.true.` result is
!! a no-op, and that a clause naming an unread column reads it.
!!
!! Abort paths live in test/error_scenarios.f90 as `row_mask_*`/`table_filter_*` scenarios.
!! Every test writes its own fixture path -- the suite runs its tests concurrently, so a shared
!! one would be truncated out from under its neighbour.
module test_table_verbs
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use ieee_arithmetic, only : ieee_value, ieee_quiet_nan, ieee_positive_inf, ieee_negative_inf
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_verbs

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_verbs(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("row_mask matches the reader on a numeric rule", test_ab_numeric), &
            new_unittest("row_mask matches the reader on and/or/not", test_ab_boolean_algebra), &
            new_unittest("row_mask matches the reader on nulls", test_ab_nulls), &
            new_unittest("row_mask matches the reader on NaN", test_ab_nan), &
            new_unittest("row_mask matches the reader on float32", test_ab_float32), &
            new_unittest("row_mask matches the reader on strings", test_ab_strings), &
            new_unittest("row_mask matches the reader on a boolean column", test_ab_boolean_column), &
            new_unittest("row_mask matches the reader on temporal columns", test_ab_temporal), &
            new_unittest("row_mask matches the reader on a literal list", test_ab_literal_list), &
            new_unittest("row_mask matches the reader on a bound set", test_ab_bound_set), &
            new_unittest("row_mask matches the reader on is_finite", test_ab_is_finite), &
            new_unittest("row_mask matches the reader on starts_with/ends_with/contains", &
                test_ab_string_match), &
            new_unittest("row_mask equals the equality-chain oracle", test_oracle_equality_chain), &
            new_unittest("row_mask reads a column nothing has touched", test_row_mask_touches), &
            new_unittest("row_mask neither mutates nor detaches", test_row_mask_is_a_read), &
            new_unittest("row_mask on a rule-less filter keeps every row", test_row_mask_no_rules), &
            new_unittest("filter_rows(expr) drops the rows the mask drops", test_filter_rows_expr), &
            new_unittest("filter_rows(filter) carries a bound set", test_filter_rows_bound_set), &
            new_unittest("filter_rows selecting everything is a no-op", test_filter_rows_no_op), &
            new_unittest("filter_rows on an in-memory table", test_filter_rows_in_memory), &
            new_unittest("row_mask on a slice covers the slice only", test_row_mask_slice), &
            new_unittest("row_mask on a zero-row table", test_row_mask_zero_rows) &
            ]
    end subroutine collect_tests_table_verbs

    ! ---- The A/B oracle -----------------------------------------------------------------------

    !> Asserts that `filt` selects the same physical rows through the reader and through
    !> `%row_mask` over a fully materialized table of the same file.
    !>
    !> `allow_degenerate` is the guard against a vacuous pass: unless a test says otherwise, a rule
    !> that selects no row, or every row, is refused -- both agree with an engine that has stopped
    !> discriminating, so neither is evidence about anything.
    subroutine expect_same_rows(error, file, filt, label, allow_degenerate)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), intent(in) :: file !! the fixture to read.
        type(parquet_filter), intent(in) :: filt !! the filter both engines are given.
        character(len=*), intent(in) :: label !! names the rule in a failure message.
        logical, intent(in), optional :: allow_degenerate !! .true. permits an empty or full row set.
        type(parquet_reader) :: rdr
        type(parquet_table) :: t
        integer(int64), allocatable :: phys(:)
        logical, allocatable :: keep(:)
        integer(int64) :: n, i, k
        integer(int64), allocatable :: mine(:)
        logical :: degenerate_ok
        character(len=32) :: na, nb

        degenerate_ok = .false.
        if (present(allow_degenerate)) degenerate_ok = allow_degenerate

        ! (a) the reader's answer: the physical row indices that survived its own filter.
        call parquet_open_reader(rdr, file, filter=filt)
        call parquet_get_physical_row_indices(rdr, phys)
        call parquet_close_reader(rdr)

        ! (b) the in-memory answer, over a table that has read everything. Materializing up front
        ! keeps this test about the EVALUATOR: the lazy-touch path has its own test below.
        call parquet_open_table(t, file)
        call t%materialize_all()
        n = t%nrows()
        allocate(keep(n))
        call t%row_mask(filt, keep)

        allocate(mine(count(keep, kind=int64)))
        k = 0_int64
        do i = 1_int64, n
            if (keep(i)) then
                k = k + 1_int64
                mine(k) = i
            end if
        end do

        if (.not. degenerate_ok) then
            call check(error, k > 0_int64 .and. k < n, label // ": the rule selects " // &
                "every row or none, so an agreement would be vacuous -- pick a sharper rule or " // &
                "pass allow_degenerate=.true.")
            if (allocated(error)) return
        end if
        write (na, "(I0)") size(mine, kind=int64)
        write (nb, "(I0)") size(phys, kind=int64)
        call check(error, size(mine, kind=int64) == size(phys, kind=int64), label // &
            ": the in-memory evaluator kept " // trim(na) // " rows and the reader kept " // trim(nb))
        if (allocated(error)) return
        call check(error, all(mine == phys), label // &
            ": the two engines kept the same NUMBER of rows but not the same ones")
    end subroutine expect_same_rows

    !> `expect_same_rows` for a single rule, which is what most tests want.
    subroutine expect_rule(error, file, rule, allow_degenerate)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), intent(in) :: file !! the fixture to read.
        character(len=*), intent(in) :: rule !! the one rule to add.
        logical, intent(in), optional :: allow_degenerate !! .true. permits an empty or full row set.
        type(parquet_filter) :: filt
        call filt%add(rule)
        call expect_same_rows(error, file, filt, rule, allow_degenerate)
    end subroutine expect_rule

    ! ---- Fixtures -----------------------------------------------------------------------------

    !> Ten rows of two int32 columns, one ascending and one descending, plus an int64 twin.
    subroutine write_numeric_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: v(10), w(10)
        integer(int64) :: b(10)
        real(real64) :: x(10)
        integer :: i

        do i = 1, 10
            v(i) = i
            w(i) = 11 - i
            b(i) = int(i, int64)*1000000000_int64
            x(i) = real(i, real64)*0.5_real64
        end do
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "v", v)
        call parquet_write_column(writer, "w", w)
        call parquet_write_column(writer, "b", b)
        call parquet_write_column(writer, "x", x)
        call parquet_close_writer(writer)
    end subroutine write_numeric_fixture

    !> Six rows with nulls in one column and none in the other.
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

    !> Six rows carrying NaNs, infinities and nulls in both float widths.
    subroutine write_nan_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: u(6) = [1, 2, 3, 4, 5, 6]
        real(real64) :: x(6)
        real(real32) :: y(6), z(6)
        logical :: valid(6) = [.true., .true., .false., .true., .true., .false.]

        x = [1.0_real64, ieee_value(0.0_real64, ieee_quiet_nan), 0.0_real64, &
            ieee_value(0.0_real64, ieee_positive_inf), 4.0_real64, 0.0_real64]
        y = real(x, real32)
        ! `z` exists for ONE assertion: 0.1 is not representable in float32, and the nearest
        ! float32 is slightly LARGER than the double 0.1 -- so `z > 0.1` keeps row 1 when the
        ! value is widened to real64 first (what both engines must do) and drops it when the
        ! comparison happens at float32 width. Without a value of this shape, an engine comparing
        ! at the wrong width agrees with the reader on every row of every other fixture.
        z = [0.1_real32, 0.05_real32, 0.3_real32, 1.0_real32, 2.0_real32, 3.0_real32]
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "u", u)
        call parquet_write_column(writer, "x", x, is_valid=valid)
        call parquet_write_column(writer, "y", y, is_valid=valid)
        call parquet_write_column(writer, "z", z)
        call parquet_close_writer(writer)
    end subroutine write_nan_fixture

    !> Six string rows, deliberately including a trailing space and an empty string -- the two
    !> values Fortran's blank-padding comparison and C++'s byte comparison disagree about.
    subroutine write_string_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        character(len=8) :: s(6) = ["ab      ", "ab      ", "abc     ", "b       ", "        ", "aa      "]
        type(parquet_string_column) :: sc
        integer(int32) :: u(6) = [1, 2, 3, 4, 5, 6]
        integer :: i

        call sc%clear()
        do i = 1, 6
            if (i == 2) then
                call sc%append_string("ab ")   ! a real trailing space, not padding
            else
                call sc%append_string(trim(s(i)))
            end if
        end do
        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "u", u)
        call parquet_write_column(writer, "s", sc)
        call parquet_close_writer(writer)
    end subroutine write_string_fixture

    !> Six rows of a boolean column with a null.
    subroutine write_bool_fixture(file)
        character(len=*), intent(in) :: file !! fixture path (one per test).
        type(parquet_writer) :: writer
        integer(int32) :: u(6) = [1, 2, 3, 4, 5, 6]
        logical :: f(6) = [.true., .false., .true., .true., .false., .false.]
        logical :: valid(6) = [.true., .true., .false., .true., .true., .true.]

        call parquet_open_writer(writer, file)
        call parquet_write_column(writer, "u", u)
        call parquet_write_column(writer, "f", f, is_valid=valid)
        call parquet_close_writer(writer)
    end subroutine write_bool_fixture

    !> Six rows of date, time[us] and timestamp[ms].
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
        call schema%init("temporal_verbs")
        call schema%add_field("d", "date")
        call schema%add_field("t", "time[us]")
        call schema%add_field("ts", "timestamp[ms]")
        call parquet_open_writer(writer, file, schema=schema)
        call parquet_write_column(writer, "d", d)
        call parquet_write_column(writer, "t", t)
        call parquet_write_column(writer, "ts", ts)
        call parquet_close_writer(writer)
    end subroutine write_temporal_fixture

    ! ---- The A/B sweep --------------------------------------------------------------------------

    !> Every ordering operator, on int32, int64 and float64 columns.
    subroutine test_ab_numeric(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_numeric.parquet"
        call write_numeric_fixture(file)
        call expect_rule(error, file, "v > 3"); if (allocated(error)) return
        call expect_rule(error, file, "v >= 3"); if (allocated(error)) return
        call expect_rule(error, file, "v < 8"); if (allocated(error)) return
        call expect_rule(error, file, "v <= 8"); if (allocated(error)) return
        call expect_rule(error, file, "v == 4"); if (allocated(error)) return
        call expect_rule(error, file, "v /= 4"); if (allocated(error)) return
        ! An int64 column against a bound no int32 could hold -- the widening the two engines both
        ! have to do, and the case an accidental int32 comparison would get wrong.
        call expect_rule(error, file, "b > 4000000000"); if (allocated(error)) return
        call expect_rule(error, file, "x >= 2.5"); if (allocated(error)) return
        call expect_rule(error, file, "x < 1.75")
    end subroutine test_ab_numeric

    !> and / or / not, including a NOT over a whole parenthesised group.
    subroutine test_ab_boolean_algebra(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_algebra.parquet"
        type(parquet_filter) :: filt
        call write_numeric_fixture(file)
        call expect_rule(error, file, "v > 3 and w > 4"); if (allocated(error)) return
        call expect_rule(error, file, "v < 3 or v > 8"); if (allocated(error)) return
        call expect_rule(error, file, "not (v > 3)"); if (allocated(error)) return
        call expect_rule(error, file, "not (v < 3 or v > 8)"); if (allocated(error)) return
        call expect_rule(error, file, "(v > 2 and v < 5) or (w > 2 and w < 5)")
        if (allocated(error)) return
        ! Two %add calls, which the parser AND-folds -- the one rule that lives in
        ! parquet_parse_filter_rules rather than in either engine.
        call filt%add("v > 2")
        call filt%add("v < 9")
        call expect_same_rows(error, file, filt, "two %add rules")
    end subroutine test_ab_boolean_algebra

    !> The null rule, including the asymmetry that makes it worth a test: a Null row is excluded by
    !> a comparison AND by its negation, but selected by is_null.
    subroutine test_ab_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_nulls.parquet"
        call write_null_fixture(file)
        call expect_rule(error, file, "v > 1"); if (allocated(error)) return
        call expect_rule(error, file, "not (v > 1)"); if (allocated(error)) return
        call expect_rule(error, file, "v is_null"); if (allocated(error)) return
        call expect_rule(error, file, "v is_not_null"); if (allocated(error)) return
        ! A Null row must not be resurrected by OR, nor let through by an AND whose other operand
        ! is true -- the two mistakes a two-valued evaluator would make.
        call expect_rule(error, file, "v > 1 or u > 5"); if (allocated(error)) return
        call expect_rule(error, file, "v > 1 and u > 1"); if (allocated(error)) return
        call expect_rule(error, file, "not (v > 1) or v is_null")
    end subroutine test_ab_nulls

    !> A NaN is a VALUE, not a missing one -- so it fails every ordering comparison and `==`, and
    !> SURVIVES `/=` and a negated comparison, which is exactly where a Null does the opposite.
    subroutine test_ab_nan(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_nan.parquet"
        call write_nan_fixture(file)
        call expect_rule(error, file, "x > 0.0"); if (allocated(error)) return
        call expect_rule(error, file, "x <= 4.0"); if (allocated(error)) return
        call expect_rule(error, file, "x == 1.0"); if (allocated(error)) return
        call expect_rule(error, file, "x /= 1.0"); if (allocated(error)) return
        call expect_rule(error, file, "not (x > 0.0)"); if (allocated(error)) return
        call expect_rule(error, file, "x is_nan"); if (allocated(error)) return
        call expect_rule(error, file, "x is_not_nan"); if (allocated(error)) return
        ! An infinity is an ordinary comparable bound, unlike a NaN.
        call expect_rule(error, file, "x < inf")
    end subroutine test_ab_nan

    !> A float32 column widened to real64 before the comparison, exactly as the reader's
    !> real_family_value_at does -- so a bound that is not representable in float32 selects the
    !> same rows on both engines.
    subroutine test_ab_float32(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_f32.parquet"
        call write_nan_fixture(file)
        call expect_rule(error, file, "y > 0.5"); if (allocated(error)) return
        call expect_rule(error, file, "y is_nan"); if (allocated(error)) return
        call expect_rule(error, file, "y is_not_finite"); if (allocated(error)) return
        ! The width assertion: see write_nan_fixture's own note on `z`.
        call expect_rule(error, file, "z > 0.1")
    end subroutine test_ab_float32

    !> String comparison, byte-lexicographic on both engines.
    !>
    !> `s == "ab"` is the sharp one: row 2 holds `"ab "` with a real trailing space, which Fortran's
    !> own `==` treats as equal to `"ab"` and which C++'s string_view does not. An evaluator written
    !> with the intrinsic operators passes every other assertion here and fails this one.
    subroutine test_ab_strings(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_strings.parquet"
        call write_string_fixture(file)
        call expect_rule(error, file, 's == "ab"'); if (allocated(error)) return
        call expect_rule(error, file, 's /= "ab"'); if (allocated(error)) return
        call expect_rule(error, file, 's < "b"'); if (allocated(error)) return
        call expect_rule(error, file, 's >= "ab"'); if (allocated(error)) return
        ! The empty string sorts before everything, so this selects exactly the one empty row.
        call expect_rule(error, file, 's < "a"')
    end subroutine test_ab_strings

    !> A boolean column: equality only, and a null still unknown.
    subroutine test_ab_boolean_column(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_bool.parquet"
        call write_bool_fixture(file)
        call expect_rule(error, file, "f == true"); if (allocated(error)) return
        call expect_rule(error, file, "f /= true"); if (allocated(error)) return
        call expect_rule(error, file, "f == FALSE"); if (allocated(error)) return
        call expect_rule(error, file, "f is_null")
    end subroutine test_ab_boolean_column

    !> Date, time and timestamp, each compared against an ISO-8601 literal at the column's own
    !> stored unit -- the conversion both engines run through temporal_literal_to_raw.
    subroutine test_ab_temporal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_temporal.parquet"
        call write_temporal_fixture(file)
        call expect_rule(error, file, 'd >= "2024-01-03"'); if (allocated(error)) return
        call expect_rule(error, file, 'd == "2024-01-05"'); if (allocated(error)) return
        call expect_rule(error, file, 't < "12:00:03"'); if (allocated(error)) return
        call expect_rule(error, file, 'ts > "2024-01-31T12:30:02"'); if (allocated(error)) return
        ! A date-only literal against a timestamp column means midnight of that date, so this keeps
        ! every row -- degenerate on purpose, and the point is that BOTH engines say so.
        call expect_rule(error, file, 'ts >= "2024-01-31"', allow_degenerate=.true.)
    end subroutine test_ab_temporal

    !> A literal list, on each element family.
    subroutine test_ab_literal_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_list.parquet"
        character(len=*), parameter :: sfile = "test_run/test_verbs_list_s.parquet"
        call write_numeric_fixture(file)
        call write_string_fixture(sfile)
        call expect_rule(error, file, "v in (2, 5, 9)"); if (allocated(error)) return
        call expect_rule(error, file, "v not_in (2, 5, 9)"); if (allocated(error)) return
        call expect_rule(error, file, "x in (1.0, 2.5, 4.0)"); if (allocated(error)) return
        call expect_rule(error, file, "b in (2000000000, 7000000000)"); if (allocated(error)) return
        call expect_rule(error, sfile, 's in ("ab", "b")')
    end subroutine test_ab_literal_list

    !> A bound set, which only the filter-object form can carry -- and a set containing a value the
    !> column does not hold, so a "matches everything it was given" bug would show.
    subroutine test_ab_bound_set(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_bound.parquet"
        type(parquet_filter) :: filt, filt2, filt3
        call write_numeric_fixture(file)
        call filt%bind("want", [3_int32, 7_int32, 99_int32])
        call filt%add("v in @want")
        call expect_same_rows(error, file, filt, "v in @want")
        if (allocated(error)) return
        call filt2%bind("want", [3_int32, 7_int32, 99_int32])
        call filt2%add("v not_in @want")
        call expect_same_rows(error, file, filt2, "v not_in @want")
        if (allocated(error)) return
        ! An EMPTY set matches nothing and its negation matches everything -- degenerate by
        ! definition, and the case worth pinning because "everything pruned" is also what a bug
        ! looks like.
        call filt3%bind("none", [integer(int32) ::])
        call filt3%add("v in @none")
        call expect_same_rows(error, file, filt3, "v in @none", allow_degenerate=.true.)
    end subroutine test_ab_bound_set

    !> The four value-class operators.
    subroutine test_ab_is_finite(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_finite.parquet"
        call write_nan_fixture(file)
        call expect_rule(error, file, "x is_finite"); if (allocated(error)) return
        call expect_rule(error, file, "x is_not_finite"); if (allocated(error)) return
        call expect_rule(error, file, "not (x is_finite)")
    end subroutine test_ab_is_finite

    !> The three substring operators, on both engines.
    !>
    !> Risk-198 makes this obligatory rather than optional: the reader answers them in
    !> eval_filter_clause over Arrow arrays and the table answers them in
    !> parquet_eval_string_match_leaf over resident storage, and the two bodies do not even look
    !> alike -- C++ && short-circuits so the length guard is one expression there, while Fortran's
    !> .and. does not, so the guard has to be a nested if. Nothing but an A/B can see them
    !> disagree; a single-engine test is satisfied by either one being wrong on a case the fixture
    !> does not contain.
    !>
    !> write_string_fixture stores `"ab "` beside `"ab"`, so the trailing-space value that Risk-199
    !> exists for is in every rule below. `"aa"` and the empty value keep each rule off the
    !> degenerate answers expect_rule refuses.
    subroutine test_ab_string_match(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_string_match.parquet"
        call write_string_fixture(file)
        call expect_rule(error, file, 's starts_with "ab"'); if (allocated(error)) return
        call expect_rule(error, file, 's starts_with "a"'); if (allocated(error)) return
        ! Only "ab" and "b" end with b -- "ab " ends with the space, which is the one row the two
        ! engines could differ on if either of them trimmed.
        call expect_rule(error, file, 's ends_with "b"'); if (allocated(error)) return
        call expect_rule(error, file, 's contains "b"'); if (allocated(error)) return
        ! Under `not`, where an engine that answered false instead of unknown would show up.
        call expect_rule(error, file, 'not (s starts_with "ab")')
    end subroutine test_ab_string_match

    ! ---- Independent oracles and behaviour ------------------------------------------------------

    !> `v in (2, 5, 9)` must equal `v == 2 or v == 5 or v == 9`, row for row, on the in-memory
    !> engine alone.
    !>
    !> Independent of the A/B above in the way that matters: the chain never touches the set
    !> machinery, so this pins the set leaf against the comparison leaf rather than against the
    !> reader. A defect shared by both engines -- which the A/B cannot see at all -- shows up here.
    subroutine test_oracle_equality_chain(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_oracle.parquet"
        type(parquet_table) :: t
        logical :: a(10), b(10)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%row_mask("v in (2, 5, 9)", a)
        call t%row_mask("v == 2 or v == 5 or v == 9", b)
        call check(error, count(a) == 3, "the set clause should select three rows")
        if (allocated(error)) return
        call check(error, all(a .eqv. b), "`in` and the == chain must agree row for row")
        if (allocated(error)) return
        call t%row_mask("v not_in (2, 5, 9)", a)
        call t%row_mask("not (v == 2 or v == 5 or v == 9)", b)
        call check(error, all(a .eqv. b), "`not_in` and the negated == chain must agree row for row")
    end subroutine test_oracle_equality_chain

    !> A rule may name a column nothing has read yet: `table_resolve`'s lazy touch reads it, the
    !> same way `%get` would.
    subroutine test_row_mask_touches(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_touch.parquet"
        type(parquet_table) :: t
        logical :: keep(10)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        ! Nothing materialized: `w` is not resident, and the mask still has to be right.
        call check(error, t%residency("w") /= RES_FULL, "the fixture column should start unread")
        if (allocated(error)) return
        call t%row_mask("w >= 8", keep)
        call check(error, count(keep) == 3, "w >= 8 should keep three rows (w = 10, 9, 8)")
        if (allocated(error)) return
        call check(error, all(keep(1:3)) .and. .not. any(keep(4:)), &
            "w descends, so the kept rows are the first three")
        if (allocated(error)) return
        call check(error, t%residency("w") == RES_FULL, "the clause's column should now be resident")
    end subroutine test_row_mask_touches

    !> `%row_mask` is a READ: it changes no row, bumps no generation and detaches nothing, so the
    !> table is still readable from its file afterwards.
    subroutine test_row_mask_is_a_read(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_read.parquet"
        type(parquet_table) :: t
        logical :: keep(10)
        integer(int64) :: gen
        integer(int32), allocatable :: got(:)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        call t%prefetch("v")
        gen = t%generation()
        call t%row_mask("v > 3", keep)
        call check(error, t%nrows() == 10_int64, "%row_mask must not drop a row")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "%row_mask must not bump the generation")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "%row_mask must not detach the table")
        if (allocated(error)) return
        ! The negative control for the detach assertion: a column nothing had read is still
        ! readable, which is exactly what a detached table could not do.
        call t%get("w", got)
        call check(error, size(got) == 10, "an unread column must still be readable afterwards")
    end subroutine test_row_mask_is_a_read

    !> A filter carrying no rules selects every row -- what an absent `filter=` does at the reader.
    subroutine test_row_mask_no_rules(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_norules.parquet"
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        logical :: keep(10)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        call t%row_mask(filt, keep)
        call check(error, all(keep), "a rule-less filter must keep every row")
    end subroutine test_row_mask_no_rules

    !> `%filter_rows(expr)` drops exactly the rows `%row_mask` did not select, and detaches.
    subroutine test_filter_rows_expr(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_fr.parquet"
        type(parquet_table) :: t
        integer(int32), allocatable :: got(:)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call t%filter_rows("v > 3 and v <= 7")
        call check(error, t%nrows() == 4_int64, "four rows should survive v > 3 and v <= 7")
        if (allocated(error)) return
        call t%get("v", got)
        call check(error, all(got == [4_int32, 5_int32, 6_int32, 7_int32]), &
            "the surviving rows should be v = 4..7, in order")
        if (allocated(error)) return
        call check(error, t%is_detached(), "%filter_rows must detach when it drops a row")
    end subroutine test_filter_rows_expr

    !> The object form is the only one that can carry a bound set.
    subroutine test_filter_rows_bound_set(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_frset.parquet"
        type(parquet_table) :: t
        type(parquet_filter) :: filt
        integer(int32), allocatable :: got(:)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        call t%materialize_all()
        call filt%bind("keepme", [2_int32, 8_int32])
        call filt%add("v in @keepme")
        call t%filter_rows(filt)
        call t%get("v", got)
        call check(error, size(got) == 2, "the bound set should keep two rows")
        if (allocated(error)) return
        call check(error, all(got == [2_int32, 8_int32]), "the kept rows should be the set's members")
    end subroutine test_filter_rows_bound_set

    !> A rule every row satisfies removes nothing, so it changes nothing: the same no-op the mask
    !> form has, inherited through table_apply_keep rather than repeated. The negative control for
    !> the detach assertion above.
    subroutine test_filter_rows_no_op(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_noop.parquet"
        type(parquet_table) :: t
        integer(int64) :: gen
        call write_numeric_fixture(file)
        call parquet_open_table(t, file)
        call t%prefetch("v")
        gen = t%generation()
        call t%filter_rows("v >= 1")
        call check(error, t%nrows() == 10_int64, "a rule every row satisfies must drop nothing")
        if (allocated(error)) return
        call check(error, t%generation() == gen, "a no-op filter must not bump the generation")
        if (allocated(error)) return
        call check(error, .not. t%is_detached(), "a no-op filter must not detach the table")
    end subroutine test_filter_rows_no_op

    !> A table built in memory has no file at all, so nothing here can come from the reader --
    !> which also pins that a temporal column with no stored unit is compared at nanoseconds.
    subroutine test_filter_rows_in_memory(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_timestamp) :: ts(5)
        integer(int32) :: v(5) = [5, 3, 9, 1, 7]
        integer(int32), allocatable :: got(:)
        logical :: keep(5)
        integer :: i
        call parquet_new_table(t)
        do i = 1, 5
            call ts(i)%set(2024, 6, 1, 0, 0, i - 1)
        end do
        call t%add_column("v", v)
        call t%add_column("ts", ts)
        call t%row_mask('ts >= "2024-06-01T00:00:02"', keep)
        call check(error, count(keep) == 3, "three of the five instants are at or after 00:00:02")
        if (allocated(error)) return
        call t%filter_rows("v > 4")
        call t%get("v", got)
        call check(error, all(got == [5_int32, 9_int32, 7_int32]), &
            "an in-memory table filters in its own row order")
        if (allocated(error)) return
        ! A table that never had a file cannot lose one, so it is not detached by a row change.
        call check(error, .not. t%is_detached(), "a table with no file must not report itself detached")
    end subroutine test_filter_rows_in_memory

    !> On a SLICE the mask covers the slice's own rows, not the file's -- so the reader answering
    !> the same rule over the same range is the oracle.
    subroutine test_row_mask_slice(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        character(len=*), parameter :: file = "test_run/test_verbs_slice.parquet"
        type(parquet_table) :: t
        logical :: keep(4)
        integer(int32), allocatable :: got(:)
        call write_numeric_fixture(file)
        call parquet_open_table(t, file, row_lo=4_int64, row_hi=7_int64)
        call check(error, t%nrows() == 4_int64, "the slice should hold four rows")
        if (allocated(error)) return
        call t%row_mask("v > 5", keep)
        call check(error, count(keep) == 2, "v = 6 and 7 are the slice rows above 5")
        if (allocated(error)) return
        call t%filter_rows("v > 5")
        call t%get("v", got)
        call check(error, all(got == [6_int32, 7_int32]), "the slice keeps its own rows 6 and 7")
    end subroutine test_row_mask_slice

    !> A zero-row table: the mask is empty and nothing aborts. Degenerate on purpose -- the case
    !> where every array section is zero-trip and a pointer into unallocated storage would show.
    subroutine test_row_mask_zero_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: none(0)
        logical :: keep(0)
        call parquet_new_table(t)
        call t%add_column("v", none)
        call t%row_mask("v > 3", keep)
        call check(error, size(keep) == 0, "a zero-row table yields an empty mask")
        if (allocated(error)) return
        call t%filter_rows("v > 3")
        call check(error, t%nrows() == 0_int64, "filtering a zero-row table leaves it empty")
    end subroutine test_row_mask_zero_rows
end module test_table_verbs
