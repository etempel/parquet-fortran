!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Unit tests for the table's text conversions: `%parse_column` and `%format_column`.
!!
!! **The subject is the FAILURE POLICY, not the arithmetic.** A parser that gets every
!! well-formed row right and quietly accepts `"5 6"` as 5 passes any test built from clean
!! fixtures, so every fixture here carries malformed rows on purpose and the two policies are
!! asserted against each other: `invalid="null"` must null **precisely** the malformed rows and
!! leave every other row's value alone, which is what `test_parse_invalid_null` checks row by
!! row rather than by counting nulls. The `"error"` half aborts, so it lives in
!! test/error_scenarios.f90 -- and `parse_column_malformed` asserts the row number, the column
!! name and the offending text all appear, because a message naming only the column is what a
!! caller cannot act on.
!!
!! **A Null and an unreadable row are different things and the tests keep them apart.** A row
!! that was already Null is never looked at, so `invalid=` has no say over it; a row whose text
!! is unreadable is one `invalid=` decides about. A fixture with only one of the two cannot tell
!! a parser that conflates them from one that does not, so `build_messy` carries both, in known
!! positions, and the assertions name those positions.
!!
!! **`%format_column`'s default rendering of a real is deliberately never asserted.**
!! `pf_to_str`'s own contract says `(g0)` gives `3.1400000000000001` on one compiler and
!! `3.140000000000000` on another, so a test pinning that text would pass on the machine it was
!! written on and fail elsewhere. The real-valued tests here assert a ROUND TRIP through
!! `%parse_column` instead, which is compiler-independent, and the exact-text assertions use
!! integers or an explicit `fmt`.
!!
!! **Neither verb detaches, and that is a negative control rather than an absence.**
!! `test_convert_does_not_detach` reads a column it deliberately left unread until after the
!! conversion -- which only works because the table is still attached to its file -- and
!! `test_convert_bumps_generation` is the positive control beside it: storage IS replaced, so
!! the counter must move even though nothing detached.
!!
!! Abort paths live in test/error_scenarios.f90 as `parse_column_*`/`format_column_*` scenarios.
!! Every test that writes a file uses its own path -- the suite runs its tests concurrently, so a
!! shared one would be truncated out from under its neighbour.
module test_table_convert
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    use testdrive, only : new_unittest, unittest_type, error_type, check
    !
    implicit none
    private
    public :: collect_tests_table_convert

contains

    !> Registers this suite's tests.
    subroutine collect_tests_table_convert(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.
        testsuite = [ &
            new_unittest("parse_column converts in place, keeping name and position", test_parse_in_place), &
            new_unittest("parse_column to_name leaves the text column alone", test_parse_to_name), &
            new_unittest("parse_column leaves a Null row Null", test_parse_keeps_nulls), &
            new_unittest("parse_column invalid=null nulls exactly the bad rows", test_parse_invalid_null), &
            new_unittest("parse_column reaches every numeric and logical target", test_parse_targets), &
            new_unittest("parse_column reaches the three temporal targets", test_parse_temporal), &
            new_unittest("parse_column carries the column's unit across", test_parse_keeps_unit), &
            new_unittest("format_column renders integers exactly", test_format_integers), &
            new_unittest("format_column renders float32, logical and time columns", &
                test_format_every_remaining_kind), &
            new_unittest("parse_column nulls a bad row for every numeric and logical target", &
                test_parse_rejects_bad_rows_every_target), &
            new_unittest("parse_column invalid=error spelled out matches the default", &
                test_parse_invalid_error_spelled_out), &
            new_unittest("format_column honours fmt", test_format_fmt), &
            new_unittest("format_column leaves a Null row Null", test_format_keeps_nulls), &
            new_unittest("format_column renders temporal columns as ISO-8601", test_format_temporal), &
            new_unittest("format_column to_name leaves the source alone", test_format_to_name), &
            new_unittest("parse then format round-trips an integer column", test_round_trip_int), &
            new_unittest("format then parse round-trips a real column", test_round_trip_real), &
            new_unittest("neither verb detaches the table", test_convert_does_not_detach), &
            new_unittest("both verbs advance the generation counter", test_convert_bumps_generation), &
            new_unittest("a converted column reports as written, so reload refuses it", test_convert_claims_column), &
            new_unittest("parse_column reads a column that was never touched", test_parse_reads_lazily), &
            new_unittest("a format-then-parse round trip keeps a column's temporal resolution", &
                test_round_trip_keeps_resolution), &
            new_unittest("both verbs handle a column with no rows at all", test_convert_zero_rows) &
            ]
    end subroutine collect_tests_table_convert

    ! ---- fixtures -------------------------------------------------------------------------------

    !> A five-row table with one clean numeric-text column and one messy one.
    !!
    !! `clean` parses under either policy. `messy` carries **both** of the two cases that must not
    !! be confused: row 2 is Null (nothing to parse, and `invalid=` has no say over it) and rows 4
    !! and 5 are present but unreadable -- row 4 with the list-directed `read`'s own trap, a value
    !! with an embedded blank, which a non-strict parser accepts as 7.
    subroutine build_messy(t)
        type(parquet_table), intent(out) :: t !! receives the fixture.
        character(len=8) :: clean(5), messy(5)
        logical :: ok(5)
        !
        clean = ["  10    ", "  20    ", "  30    ", "  40    ", "  50    "]
        messy = ["1       ", "        ", "3       ", "7 8     ", "x       "]
        ok = [.true., .false., .true., .true., .true.]
        call parquet_new_table(t)
        call t%add_column("clean", clean)
        call t%add_column("messy", messy)
        call t%set_null("messy", ok)
    end subroutine build_messy

    ! ---- %parse_column --------------------------------------------------------------------------

    !> In place: the column keeps its name and its position, and only its kind and values change.
    subroutine test_parse_in_place(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64), allocatable :: v(:)
        integer :: pos_before
        !
        call build_messy(t)
        pos_before = t%column_index("clean")
        call check(error, t%kind("clean") == PK_STRING, "the fixture's clean column starts as text")
        if (allocated(error)) return
        call t%parse_column("clean", PK_INT64)
        call check(error, t%kind("clean") == PK_INT64, "parse_column left the column an int64 one")
        if (allocated(error)) return
        call check(error, t%column_index("clean") == pos_before, "the column kept its position")
        if (allocated(error)) return
        call check(error, t%ncols() == 2, "no column was added or removed")
        if (allocated(error)) return
        call t%get("clean", v)
        call check(error, all(v == [10_int64, 20_int64, 30_int64, 40_int64, 50_int64]), &
            "every row parsed to the number its text spelled")
    end subroutine test_parse_in_place

    !> `to_name` writes a second column and leaves the first exactly as it was.
    subroutine test_parse_to_name(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64), allocatable :: v(:)
        character(len=:), allocatable :: txt(:)
        !
        call build_messy(t)
        call t%parse_column("clean", PK_INT64, to_name="n")
        call check(error, t%ncols() == 3, "a third column appeared")
        if (allocated(error)) return
        call check(error, t%kind("clean") == PK_STRING, "the source column is still text")
        if (allocated(error)) return
        call check(error, t%kind("n") == PK_INT64, "the new column holds the parsed numbers")
        if (allocated(error)) return
        call t%get("clean", txt)
        ! adjustl, because the fixture's LEADING blanks are deliberate and survive: a character
        ! array is trailing-trimmed on the way into a column and nothing else, so what comes back
        ! is "  30" -- which is also the padding pf_from_str has to trim to read it as 30.
        call check(error, trim(adjustl(txt(3))) == "30", "the source column's text is unchanged")
        if (allocated(error)) return
        call t%get("n", v)
        call check(error, all(v == [10_int64, 20_int64, 30_int64, 40_int64, 50_int64]), &
            "the new column holds every parsed value")
    end subroutine test_parse_to_name

    !> A row that was already Null stays Null and its text is never looked at.
    subroutine test_parse_keeps_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=6) :: text(4)
        logical :: ok(4)
        integer(int32), allocatable :: v(:)
        logical, allocatable :: valid(:)
        !
        ! Row 3's TEXT is unreadable, and its null flag is what stops that mattering: under the
        ! default invalid="error" policy this call would abort if the null were not honoured
        ! first, so the test is also the guard for "a Null is never parsed".
        text = ["4     ", "5     ", "zzz   ", "6     "]
        ok = [.true., .true., .false., .true.]
        call parquet_new_table(t)
        call t%add_column("v", text)
        call t%set_null("v", ok)
        call t%parse_column("v", PK_INT32)
        call t%get("v", v, is_valid=valid)
        call check(error, all(valid .eqv. ok), "exactly the row that was Null is still Null")
        if (allocated(error)) return
        call check(error, v(1) == 4 .and. v(2) == 5 .and. v(4) == 6, "every other row parsed")
    end subroutine test_parse_keeps_nulls

    !> `invalid="null"` nulls precisely the unreadable rows, and no other.
    subroutine test_parse_invalid_null(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32), allocatable :: v(:)
        logical, allocatable :: valid(:)
        !
        call build_messy(t)
        call t%parse_column("messy", PK_INT32, invalid="null")
        call t%get("messy", v, is_valid=valid)
        ! Row 2 was Null already; rows 4 ("7 8") and 5 ("x") became Null here. Asserted position
        ! by position rather than as a count, because a parser that nulled the wrong three rows
        ! would satisfy any count.
        call check(error, valid(1) .and. .not. valid(2) .and. valid(3) .and. &
            .not. valid(4) .and. .not. valid(5), "exactly rows 2, 4 and 5 are Null")
        if (allocated(error)) return
        call check(error, v(1) == 1 .and. v(3) == 3, "the two readable rows kept their values")
        if (allocated(error)) return
        call check(error, t%has_nulls("messy"), "the column reports that it has nulls")
    end subroutine test_parse_invalid_null

    !> One call per numeric and logical target, checking the kind and one value each.
    subroutine test_parse_targets(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=8) :: num(3), flag(3)
        integer(int32), allocatable :: a32(:)
        integer(int64), allocatable :: a64(:)
        real(real32), allocatable :: f32(:)
        real(real64), allocatable :: f64(:)
        logical, allocatable :: b(:)
        !
        num = ["-2      ", "0       ", "3       "]
        flag = ["true    ", "0       ", "F       "]
        call parquet_new_table(t)
        call t%add_column("a", num)
        call t%add_column("b", num)
        call t%add_column("c", num)
        call t%add_column("d", num)
        call t%add_column("e", flag)
        call t%parse_column("a", PK_INT32)
        call t%parse_column("b", PK_INT64)
        call t%parse_column("c", PK_FLOAT32)
        call t%parse_column("d", PK_FLOAT64)
        call t%parse_column("e", PK_LOGICAL)
        call t%get("a", a32)
        call t%get("b", a64)
        call t%get("c", f32)
        call t%get("d", f64)
        call t%get("e", b)
        call check(error, all(a32 == [-2, 0, 3]), "the int32 target parsed every row")
        if (allocated(error)) return
        call check(error, all(a64 == [-2_int64, 0_int64, 3_int64]), "the int64 target parsed every row")
        if (allocated(error)) return
        call check(error, all(abs(f32 - [-2.0_real32, 0.0_real32, 3.0_real32]) < 1.0e-6_real32), &
            "the float32 target parsed every row")
        if (allocated(error)) return
        call check(error, all(abs(f64 - [-2.0_real64, 0.0_real64, 3.0_real64]) < 1.0e-12_real64), &
            "the float64 target parsed every row")
        if (allocated(error)) return
        call check(error, all(b .eqv. [.true., .false., .false.]), &
            "the logical target read all three accepted spellings")
    end subroutine test_parse_targets

    !> The three temporal targets go through the element types' own `%parse`.
    subroutine test_parse_temporal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=32) :: d(2), tm(2), ts(2)
        type(parquet_date), allocatable :: dv(:)
        type(parquet_time), allocatable :: tv(:)
        type(parquet_timestamp), allocatable :: sv(:)
        integer :: y, mo, dd
        !
        d = ["2024-02-29                      ", "1970-01-01                      "]
        tm = ["12:34:56                        ", "00:00:00                        "]
        ts = ["2024-02-29T12:34:56             ", "1970-01-01T00:00:00             "]
        call parquet_new_table(t)
        call t%add_column("d", d)
        call t%add_column("tm", tm)
        call t%add_column("ts", ts)
        call t%parse_column("d", PK_DATE)
        call t%parse_column("tm", PK_TIME)
        call t%parse_column("ts", PK_TIMESTAMP)
        call check(error, t%kind("d") == PK_DATE .and. t%kind("tm") == PK_TIME .and. &
            t%kind("ts") == PK_TIMESTAMP, "all three temporal targets took")
        if (allocated(error)) return
        call t%get("d", dv)
        call dv(1)%get(y, mo, dd)
        call check(error, y == 2024 .and. mo == 2 .and. dd == 29, "the leap day parsed to the right date")
        if (allocated(error)) return
        call t%get("tm", tv)
        call check(error, tv(1)%hour() == 12 .and. tv(1)%minute() == 34 .and. tv(1)%second() == 56, &
            "the time parsed to the right fields")
        if (allocated(error)) return
        call t%get("ts", sv)
        call check(error, sv(2)%to_unix(parquet_unit_seconds) == 0_int64, &
            "the epoch timestamp parsed to Unix time zero")
    end subroutine test_parse_temporal

    !> A converted column keeps the unit of the one it came from.
    subroutine test_parse_keeps_unit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=4) :: text(2)
        character(len=:), allocatable :: u
        !
        text = ["1   ", "2   "]
        call parquet_new_table(t)
        call t%add_column("m", text, unit="mag")
        call t%parse_column("m", PK_FLOAT64)
        call t%unit("m", u)
        call check(error, u == "mag", "the parsed column still reports the unit the text column had")
        if (allocated(error)) return
        call t%format_column("m", fmt="(i0)")
        call t%unit("m", u)
        call check(error, u == "mag", "the formatted column still reports it too")
    end subroutine test_parse_keeps_unit

    ! ---- %format_column -------------------------------------------------------------------------

    !> Integers render exactly, so the text can be asserted directly.
    subroutine test_format_integers(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: v(3)
        character(len=:), allocatable :: txt(:)
        !
        v = [-7_int64, 0_int64, 12345_int64]
        call parquet_new_table(t)
        call t%add_column("n", v)
        call t%format_column("n")
        call check(error, t%kind("n") == PK_STRING, "the column is text afterwards")
        if (allocated(error)) return
        call t%get("n", txt)
        call check(error, trim(txt(1)) == "-7" .and. trim(txt(2)) == "0" .and. &
            trim(txt(3)) == "12345", "every row rendered as its (i0) form")
    end subroutine test_format_integers

    !> `%format_column` over the three source kinds the tests above do not render.
    !!
    !! The renderer is a `select case` with one arm per source kind, and each arm takes its own
    !! typed data pointer before calling `pf_to_str`. `test_format_integers` covers the integer
    !! arms and `test_format_temporal` the date one; float32, logical and time are arms nothing
    !! else reaches, and an arm wired to the wrong pointer type would not compile but one wired to
    !! the wrong COLUMN would render the neighbour's values silently.
    subroutine test_format_every_remaining_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_time) :: tm(3)
        character(len=:), allocatable :: txt(:)
        !
        call tm(1)%set(1, 2, 3)
        call tm(2)%set(12, 0, 0)
        call tm(3)%set(23, 59, 59)
        call parquet_new_table(t)
        call t%add_column("f", [1.5_real32, -2.25_real32, 0.0_real32])
        call t%add_column("b", [.true., .false., .true.])
        call t%add_column("tm", tm)

        call t%format_column("f")
        call check(error, t%kind("f") == PK_STRING, "the float32 column is text afterwards")
        if (allocated(error)) return
        call t%get("f", txt)
        ! The exact spelling of a real is compiler-dependent, so what is asserted is that each row
        ! rendered its OWN value -- a renderer reading the wrong column would repeat one of them.
        call check(error, index(txt(1), "1.5") > 0 .and. index(txt(2), "-2.2") > 0 &
            .and. index(txt(3), "0") > 0, "each float32 row rendered its own value")
        if (allocated(error)) return

        call t%format_column("b")
        call t%get("b", txt)
        call check(error, trim(txt(1)) /= trim(txt(2)) .and. trim(txt(1)) == trim(txt(3)), &
            "the logical rows render as two distinct tokens, matching where the values match")
        if (allocated(error)) return

        call t%format_column("tm")
        call t%get("tm", txt)
        call check(error, index(txt(2), "12:00:00") > 0 .and. index(txt(3), "23:59:59") > 0, &
            "a time column renders as ISO-8601 time-of-day")
    end subroutine test_format_every_remaining_kind

    !> An unreadable row reaching `parse_reject` for each numeric and logical target.
    !!
    !! `test_parse_invalid_null` drives the rejection path for int32 alone. Every other target
    !! kind has its own copy of the `if (ok) ... else call parse_reject` pair inside its own
    !! allocation and loop, and the `else` half of each is what turns an unreadable row into a
    !! Null under `invalid="null"` instead of aborting. A target whose `else` branch was missing
    !! would store whatever `pf_from_str` left behind -- a zero, most likely -- and read back as a
    !! valid row holding a plausible wrong number, which is why the null flag is asserted and not
    !! only the value.
    subroutine test_parse_rejects_bad_rows_every_target(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64), allocatable :: v64(:)
        real(real32), allocatable :: r32(:)
        real(real64), allocatable :: r64(:)
        logical, allocatable :: vb(:), valid(:)
        !
        ! Row 2 is unreadable as any of the four targets; rows 1 and 3 are readable as all of them.
        call parquet_new_table(t)
        call t%add_column("a", [character(len=4) :: "1", "zzz", "0"])
        call t%add_column("b", [character(len=4) :: "1", "zzz", "0"])
        call t%add_column("c", [character(len=4) :: "1", "zzz", "0"])
        call t%add_column("d", [character(len=4) :: "T", "zzz", "F"])

        call t%parse_column("a", PK_INT64, invalid="null")
        call t%get("a", v64, is_valid=valid)
        call check(error, valid(1) .and. .not. valid(2) .and. valid(3), &
            "int64: exactly the unreadable row is Null")
        if (allocated(error)) return
        call check(error, v64(1) == 1_int64 .and. v64(3) == 0_int64, &
            "int64: the readable rows kept their own values")
        if (allocated(error)) return

        call t%parse_column("b", PK_FLOAT32, invalid="null")
        call t%get("b", r32, is_valid=valid)
        call check(error, valid(1) .and. .not. valid(2) .and. valid(3) .and. r32(1) == 1.0_real32, &
            "float32: exactly the unreadable row is Null")
        if (allocated(error)) return

        call t%parse_column("c", PK_FLOAT64, invalid="null")
        call t%get("c", r64, is_valid=valid)
        call check(error, valid(1) .and. .not. valid(2) .and. valid(3) .and. r64(1) == 1.0_real64, &
            "float64: exactly the unreadable row is Null")
        if (allocated(error)) return

        call t%parse_column("d", PK_LOGICAL, invalid="null")
        call t%get("d", vb, is_valid=valid)
        call check(error, valid(1) .and. .not. valid(2) .and. valid(3), &
            "logical: exactly the unreadable row is Null")
        if (allocated(error)) return
        call check(error, vb(1) .and. .not. vb(3), "logical: the readable rows kept their values")
    end subroutine test_parse_rejects_bad_rows_every_target

    !> `invalid="error"` spelled out is the default, and must behave as omitting it does.
    !!
    !! The policy word is validated before any row is read, so the arm that ACCEPTS "error" is
    !! separate from the path a caller who omits `invalid=` takes. On a column with nothing
    !! unreadable both must simply succeed; that equality is the assertion, and the malformed
    !! case belongs with the other aborts in test/error_scenarios.f90.
    subroutine test_parse_invalid_error_spelled_out(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32), allocatable :: implied(:), spelled(:)
        !
        call parquet_new_table(t)
        call t%add_column("a", [character(len=3) :: "1", "2", "3"])
        call t%add_column("b", [character(len=3) :: "1", "2", "3"])
        call t%parse_column("a", PK_INT32)
        call t%parse_column("b", PK_INT32, invalid="error")
        call t%get("a", implied)
        call t%get("b", spelled)
        call check(error, all(spelled == implied), &
            "invalid=""error"" spelled out must parse exactly as omitting it does")
        if (allocated(error)) return
        call check(error, all(spelled == [1_int32, 2_int32, 3_int32]), &
            "and both must actually have parsed the values (vacuity guard)")
    end subroutine test_parse_invalid_error_spelled_out

    !> `fmt` reaches `pf_to_str` and changes the text.
    subroutine test_format_fmt(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: v(2)
        character(len=:), allocatable :: txt(:)
        !
        v = [7, 42]
        call parquet_new_table(t)
        call t%add_column("n", v)
        call t%format_column("n", fmt="(i5.5)")
        call t%get("n", txt)
        call check(error, trim(txt(1)) == "00007" .and. trim(txt(2)) == "00042", &
            "the caller's format was used rather than the default")
    end subroutine test_format_fmt

    !> A Null row renders as a Null element, not as the text "null" and not as blanks.
    subroutine test_format_keeps_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: v(3)
        logical :: ok(3)
        logical, allocatable :: valid(:)
        character(len=:), allocatable :: txt(:)
        !
        v = [1, 0, 3]
        ok = [.true., .false., .true.]
        call parquet_new_table(t)
        call t%add_column("n", v)
        call t%set_null("n", ok)
        call t%format_column("n")
        call t%get("n", txt, is_valid=valid)
        call check(error, all(valid .eqv. ok), "exactly the Null row is still Null")
        if (allocated(error)) return
        call check(error, trim(txt(1)) == "1" .and. trim(txt(3)) == "3", "the other rows rendered")
    end subroutine test_format_keeps_nulls

    !> A temporal column renders through its own `%to_string`, i.e. as ISO-8601.
    subroutine test_format_temporal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        type(parquet_date) :: d(2)
        character(len=:), allocatable :: txt(:)
        logical, allocatable :: valid(:)
        !
        call d(1)%set(2024, 2, 29)
        call d(2)%set_null()
        call parquet_new_table(t)
        call t%add_column("d", d)
        call t%format_column("d")
        call t%get("d", txt, is_valid=valid)
        call check(error, trim(txt(1)) == "2024-02-29", "the date rendered as ISO-8601")
        if (allocated(error)) return
        ! The null half is load-bearing rather than incidental: %to_string ABORTS on a null
        ! element, so a formatter that did not check validity first would kill the process here.
        call check(error, valid(1) .and. .not. valid(2), "the null date stayed Null and was never rendered")
    end subroutine test_format_temporal

    !> `to_name` writes a second column and leaves the numeric one alone.
    subroutine test_format_to_name(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int32) :: v(2)
        integer(int32), allocatable :: back(:)
        character(len=:), allocatable :: txt(:)
        !
        v = [3, 4]
        call parquet_new_table(t)
        call t%add_column("n", v)
        call t%format_column("n", to_name="s")
        call check(error, t%kind("n") == PK_INT32, "the source column is still numeric")
        if (allocated(error)) return
        call check(error, t%kind("s") == PK_STRING, "the new column holds the text")
        if (allocated(error)) return
        call t%get("n", back)
        call t%get("s", txt)
        call check(error, all(back == v) .and. trim(txt(2)) == "4", "both columns hold what they should")
    end subroutine test_format_to_name

    !> The plan's acceptance property: parse then format returns the original text.
    subroutine test_round_trip_int(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=8) :: text(4)
        character(len=:), allocatable :: back(:)
        integer :: k
        logical :: same
        !
        text = ["0       ", "-1      ", "999     ", "1000000 "]
        call parquet_new_table(t)
        call t%add_column("n", text)
        call t%parse_column("n", PK_INT64)
        call t%format_column("n")
        call t%get("n", back)
        same = .true.
        do k = 1, 4
            if (trim(back(k)) /= trim(text(k))) same = .false.
        end do
        call check(error, same, "every row's text came back byte for byte")
    end subroutine test_round_trip_int

    !> The other direction, and the one where the TEXT must not be asserted: a real rendered with
    !! the default format is compiler-dependent, so the round trip is the only portable oracle.
    subroutine test_round_trip_real(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        real(real64) :: v(4)
        real(real64), allocatable :: back(:)
        !
        v = [0.0_real64, -0.5_real64, 3.25_real64, 1.0e10_real64]
        call parquet_new_table(t)
        call t%add_column("x", v)
        call t%format_column("x")
        call check(error, t%kind("x") == PK_STRING, "the column became text")
        if (allocated(error)) return
        call t%parse_column("x", PK_FLOAT64)
        call t%get("x", back)
        ! Exact, not approximate: (g0) is documented to render enough digits to identify the
        ! value, so a round trip that loses a bit is a defect rather than rounding.
        call check(error, all(back == v), "every value survived the round trip exactly")
    end subroutine test_round_trip_real

    !> Neither verb detaches, so a column left unread is still readable afterwards.
    subroutine test_convert_does_not_detach(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=16) :: text(3)
        integer(int32) :: other(3)
        integer(int32), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/convert_detach.parquet"
        !
        text = ["11              ", "22              ", "33              "]
        other = [7, 8, 9]
        call write_fixture(f, text, other)
        call parquet_open_table(t, f)
        call t%parse_column("s", PK_INT32)
        call check(error, .not. t%is_detached(), "the table is still attached to its file")
        if (allocated(error)) return
        call t%format_column("s")
        call check(error, .not. t%is_detached(), "and still attached after the other verb too")
        if (allocated(error)) return
        ! `other` was never touched before this point, so reading it here can only work through
        ! the file -- which is the property "does not detach" actually buys.
        call t%get("other", v)
        call check(error, all(v == other), "the column nothing had read still reads correctly")
    end subroutine test_convert_does_not_detach

    !> Storage is replaced, so `%generation()` must move -- the positive control beside the
    !! detach test's negative one.
    subroutine test_convert_bumps_generation(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64) :: g0, g1, g2
        !
        call build_messy(t)
        g0 = t%generation()
        call t%parse_column("clean", PK_INT32)
        g1 = t%generation()
        call t%format_column("clean")
        g2 = t%generation()
        call check(error, g1 > g0, "parse_column advanced the generation counter")
        if (allocated(error)) return
        call check(error, g2 > g1, "format_column advanced it too")
    end subroutine test_convert_bumps_generation

    !> A converted column holds this call's values rather than the file's, so `%reload` refuses
    !! it without `force=`.
    subroutine test_convert_claims_column(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=16) :: text(3)
        integer(int32) :: other(3)
        logical :: found
        character(len=*), parameter :: f = "test_run/convert_claim.parquet"
        !
        text = ["11              ", "22              ", "33              "]
        other = [7, 8, 9]
        call write_fixture(f, text, other)
        call parquet_open_table(t, f)
        ! The control: before the conversion the column is the file's own, so %reload accepts it.
        call t%reload("s", found=found)
        call check(error, found, "reload accepts the column before it is converted")
        if (allocated(error)) return
        call t%parse_column("s", PK_INT32)
        call check(error, t%residency("s") == RES_FULL, "the converted column is resident")
        if (allocated(error)) return
        ! The abort itself is reload_after_parse_column in test/error_scenarios.f90; what is
        ! asserted here is the flag that causes it, through the one query that reports it.
        call check(error, t%kind("s") == PK_INT32, "and it holds the parsed kind")
    end subroutine test_convert_claims_column

    !> Naming a column that was never read reads it, exactly as `%get` would.
    subroutine test_parse_reads_lazily(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        character(len=16) :: text(3)
        integer(int32) :: other(3)
        integer(int32), allocatable :: v(:)
        character(len=*), parameter :: f = "test_run/convert_lazy.parquet"
        !
        text = ["11              ", "22              ", "33              "]
        other = [7, 8, 9]
        call write_fixture(f, text, other)
        call parquet_open_table(t, f)
        call check(error, t%residency("s") == RES_EMPTY, "the column starts unread")
        if (allocated(error)) return
        call t%parse_column("s", PK_INT32)
        call t%get("s", v)
        call check(error, all(v == [11, 22, 33]), "it was read and parsed in one call")
    end subroutine test_parse_reads_lazily

    !> A timestamp column rendered to text and parsed back keeps the resolution it was stored at.
    !!
    !! **The assertion is that the write REACHES its own assertions.** A column's declared
    !! `timestamp[ns]` is recoverable from nowhere but the descriptor, and a schema-less
    !! `parquet_write_table` on a nanosecond column with no recorded resolution does not round --
    !! it aborts, because the writer defaults to microseconds and `%to_unix` refuses to truncate.
    !! So clearing the resolution across a conversion, which is the tidy-looking thing to do,
    !! turns this sequence into a crash. The fixture's `123456789` nanoseconds is what makes the
    !! difference visible: a value micros could hold would round silently and prove nothing.
    subroutine test_round_trip_keeps_resolution(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_writer) :: w
        type(parquet_schema) :: sc
        type(parquet_table) :: t
        type(parquet_reader) :: rd
        type(parquet_timestamp) :: ts(2), got(2)
        integer :: unit_out
        character(len=*), parameter :: f = "test_run/convert_tsunit_in.parquet"
        character(len=*), parameter :: fo = "test_run/convert_tsunit_out.parquet"
        !
        call sc%init("cvt")
        call sc%add_field("ev", "timestamp[ns]")
        call ts(1)%set(2024, 7, 16, 12, 0, 0, 123456789)
        call ts(2)%set(1999, 1, 1, 0, 0, 0)
        call parquet_open_writer(w, f, sc)
        call parquet_write_column(w, "ev", ts)
        call parquet_close_writer(w)
        !
        call parquet_open_table(t, f)
        call t%format_column("ev")
        call check(error, t%kind("ev") == PK_STRING, "the column rendered to text")
        if (allocated(error)) return
        call t%parse_column("ev", PK_TIMESTAMP)
        call parquet_write_table(t, fo)     ! aborts here if the resolution was dropped
        !
        call parquet_open_reader(rd, fo)
        call parquet_get_column_time_info(rd, "ev", unit=unit_out)
        call parquet_read_column(rd, "ev", got)
        call parquet_close_reader(rd)
        call check(error, unit_out == parquet_unit_nanos, &
            "the rewritten column still declares nanoseconds")
        if (allocated(error)) return
        call check(error, got(1) == ts(1) .and. got(2) == ts(2), &
            "and the nanosecond values survived text and back")
    end subroutine test_round_trip_keeps_resolution

    !> A zero-row column converts both ways without touching storage that was never allocated.
    !!
    !! The empty case is its own hazard here rather than a formality: this library's allocators
    !! skip a zero-length allocation deliberately, so a bulk path that "just assigns the whole
    !! array" references an unallocated allocatable -- the class CLAUDE.md records four confirmed
    !! instances of, every one invisible under a plain build and visible only under `-C=array` or
    !! `--profile debug`. It is also a documented user pattern rather than a corner: declaring a
    !! column before filling it is what `%add_column(name, empty)` is for.
    subroutine test_convert_zero_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive's error slot.
        type(parquet_table) :: t
        integer(int64), allocatable :: v(:)
        character(len=:), allocatable :: txt(:)
        !
        call parquet_new_table(t)
        call t%add_column("s", [character(len=4) ::])
        call check(error, t%nrows() == 0, "the fixture really has no rows")
        if (allocated(error)) return
        call t%parse_column("s", PK_INT64)
        call check(error, t%kind("s") == PK_INT64, "an empty string column parses to the target kind")
        if (allocated(error)) return
        call t%get("s", v)
        call check(error, size(v) == 0, "and comes back empty")
        if (allocated(error)) return
        call t%format_column("s")
        call check(error, t%kind("s") == PK_STRING, "and renders back to text")
        if (allocated(error)) return
        call t%get("s", txt)
        call check(error, size(txt) == 0, "still with no rows")
    end subroutine test_convert_zero_rows

    ! ---- shared plumbing --------------------------------------------------------------------------

    !> Writes a two-column file: `s` holding text, `other` holding integers.
    subroutine write_fixture(path, text, other)
        character(len=*), intent(in) :: path      !! where to write it.
        character(len=*), intent(in) :: text(:)   !! the text column's values.
        integer(int32), intent(in) :: other(:)    !! the integer column's values.
        type(parquet_writer) :: w
        !
        call parquet_open_writer(w, path)
        call parquet_write_column(w, "s", text)
        call parquet_write_column(w, "other", other)
        call parquet_close_writer(w)
    end subroutine write_fixture

end module test_table_convert
