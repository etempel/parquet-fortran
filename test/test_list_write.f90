!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for writing a variable-length `LIST` column to a Parquet file -- the
!> whole-column and row-group-scoped specifics of
!> `parquet_write_column`/`parquet_write_column_chunk`, plus the MAML
!> `list[<elemtype>]` declaration a schema-enforced writer needs.
!!
!! **Three oracles, deliberately, because a writer cannot be its own.** Every test here
!! establishes what the file really contains in one of three ways, and the third is what stops
!! the first two passing against a writer that quietly emitted something else:
!!
!! * **The round trip.** Build a `parquet_list_column`, write it, read it back with
!!   `parquet_read_column`, and compare row count, per-row length, per-row nullness, per-element
!!   value and per-element nullness. Not circular: the read path is tested in its own suite
!!   against fixtures this library did not write.
!! * **The re-write.** Read `test/fixtures/list_payloads.parquet` -- authored by Arrow, not by
!!   this library -- write every one of its list columns straight back out, re-read, and compare
!!   against the first read. One test covering all nine payload kinds, `large_list`, null rows,
!!   empty rows, null elements and three row groups, against an oracle nothing here produced.
!! * **`parquet_get_column_shape`.** On the written file it must answer `"list"`, INCLUDING for a
!!   column every row of which happens to hold the same number of elements. That is what
!!   distinguishes "a variable-length LIST was written" from "a `fixed_size_list` was written and
!!   the reader was tolerant" -- and without it a round trip would pass against the latter.
!!
!! Every test writes to its own fixture path: test-drive runs a suite's tests concurrently, so a
!! shared output filename is a truncation race (CLAUDE.md, "Tests run concurrently").
module test_list_write
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_list_write

    !> The Arrow-authored fixture the re-write test uses as its oracle. Never written to.
    character(len=*), parameter :: PAYLOADS = "test/fixtures/list_payloads.parquet"

contains

    !> Registers every test in this suite.
    subroutine collect_tests_parquet_list_write(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("a ragged column round-trips with its own per-row lengths", test_ragged_round_trip), &
            new_unittest("a null row, an empty row and a null element all survive the write", &
                test_all_three_null_levels), &
            new_unittest("the written column is a LIST, not a vector, even when every row is equal", &
                test_uniform_writes_as_list), &
            new_unittest("every payload kind round-trips", test_every_payload_kind), &
            new_unittest("a string payload keeps exact bytes, empty and trailing space", test_string_payload), &
            new_unittest("a temporal payload keeps its element nulls and its declared unit", &
                test_temporal_payload), &
            new_unittest("every list column of an Arrow-written file re-writes identically", &
                test_rewrite_fixture), &
            new_unittest("the chunked form produces the same file as the whole-column form", &
                test_chunked_agrees), &
            new_unittest("a row mask drops whole rows and compacts their elements", test_row_mask), &
            new_unittest("a zero-row list column writes and reads back empty", test_zero_rows), &
            new_unittest("a column whose every row is null or empty writes", test_no_elements), &
            new_unittest("a schema-enforced writer accepts list[int32]", test_schema_enforced), &
            new_unittest("a declared but unwritten list column is written with zero rows", &
                test_declared_but_unwritten), &
            new_unittest("a schema-declared temporal element keeps its unit and utc", &
                test_schema_temporal_unit), &
            new_unittest("a list column can be written beside ordinary columns", test_beside_other_columns) &
            ]
    end subroutine collect_tests_parquet_list_write

    !> Builds the canonical ragged test column: row r (1-based) holds `mod(r-1,4)+1` int32
    !> elements, element e being `(r-1)*100 + (e-1)`. No two rows have the same length within a
    !> group of four and no two elements the same value, so an offsets error of any size shows up
    !> as a wrong value rather than as a coincidence.
    subroutine build_ragged(lc, nrows)
        type(parquet_list_column), intent(out), target :: lc !! the column, built here.
        integer(int64), intent(in) :: nrows          !! rows to build.
        integer(int64) :: r, e, n
        integer(int32), allocatable :: v(:)
        call lc%init(PK_INT32)
        do r = 1_int64, nrows
            n = mod(r - 1_int64, 4_int64) + 1_int64
            allocate(v(n))
            do e = 1_int64, n
                v(e) = int((r - 1_int64)*100_int64 + (e - 1_int64), int32)
            end do
            call lc%append_row(v)
            deallocate(v)
        end do
    end subroutine build_ragged

    !> Whether two list columns agree in EVERY respect a write could get wrong: row count, payload
    !> kind, total element count, and then per row its length and nullness, and per element its
    !> value and its nullness.
    !>
    !> Kind-dispatched over all nine payloads rather than comparing only int32, because the
    !> re-write test's whole point is that the eight other arms are exercised too. `why` names the
    !> first disagreement, so a failure says which property broke rather than only that one did.
    logical function lists_equal(a, b, why) result(same)
        type(parquet_list_column), intent(in), target :: a !! the expected column.
        type(parquet_list_column), intent(in), target :: b !! the column under test.
        character(len=:), allocatable, intent(out) :: why  !! "" when equal, else the first difference.
        type(parquet_list_row) :: ra, rb
        integer(int64) :: i, e, n
        integer(int32), allocatable :: ai32(:), bi32(:)
        integer(int64), allocatable :: ai64(:), bi64(:)
        real(real32), allocatable :: af32(:), bf32(:)
        real(real64), allocatable :: af64(:), bf64(:)
        logical, allocatable :: abool(:), bbool(:)
        character(len=:), allocatable :: astr(:), bstr(:)
        type(parquet_date), allocatable :: ad(:), bd(:)
        type(parquet_time), allocatable :: at(:), bt(:)
        type(parquet_timestamp), allocatable :: ats(:), bts(:)
        logical, allocatable :: av(:), bv(:)

        same = .false.
        why = ""
        if (a%size() /= b%size()) then
            why = "row count differs"
            return
        end if
        if (a%element_kind() /= b%element_kind()) then
            why = "payload kind differs"
            return
        end if
        if (a%total_elements() /= b%total_elements()) then
            why = "total element count differs"
            return
        end if
        do i = 1_int64, a%size()
            if (a%length(i) /= b%length(i)) then
                why = "a row length differs"
                return
            end if
            if (a%is_null(i) .neqv. b%is_null(i)) then
                why = "a row's nullness differs"
                return
            end if
            if (a%is_null(i)) cycle
            n = a%length(i)
            ra = a%view(i)
            rb = b%view(i)
            select case (a%element_kind())
            case (PK_INT32)
                call ra%get(ai32, is_valid=av)
                call rb%get(bi32, is_valid=bv)
                if (n > 0_int64) then
                    if (.not. all(ai32 == bi32)) why = "an int32 element value differs"
                end if
            case (PK_INT64)
                call ra%get(ai64, is_valid=av)
                call rb%get(bi64, is_valid=bv)
                if (n > 0_int64) then
                    if (.not. all(ai64 == bi64)) why = "an int64 element value differs"
                end if
            case (PK_FLOAT32)
                call ra%get(af32, is_valid=av)
                call rb%get(bf32, is_valid=bv)
                if (n > 0_int64) then
                    do e = 1_int64, n
                        if (.not. av(e)) cycle
                        if (af32(e) /= bf32(e)) why = "a float32 element value differs"
                    end do
                end if
            case (PK_FLOAT64)
                call ra%get(af64, is_valid=av)
                call rb%get(bf64, is_valid=bv)
                if (n > 0_int64) then
                    do e = 1_int64, n
                        if (.not. av(e)) cycle
                        if (af64(e) /= bf64(e)) why = "a float64 element value differs"
                    end do
                end if
            case (PK_LOGICAL)
                call ra%get(abool, is_valid=av)
                call rb%get(bbool, is_valid=bv)
                if (n > 0_int64) then
                    do e = 1_int64, n
                        if (.not. av(e)) cycle
                        if (abool(e) .neqv. bbool(e)) why = "a logical element value differs"
                    end do
                end if
            case (PK_STRING)
                call ra%get(astr, is_valid=av)
                call rb%get(bstr, is_valid=bv)
                do e = 1_int64, n
                    if (.not. av(e)) cycle
                    if (astr(e) /= bstr(e)) why = "a string element value differs"
                end do
            case (PK_DATE)
                call ra%get(ad, is_valid=av)
                call rb%get(bd, is_valid=bv)
                do e = 1_int64, n
                    if (ad(e)%is_null() .neqv. bd(e)%is_null()) why = "a date element's nullness differs"
                    if (ad(e)%is_null()) cycle
                    if (ad(e)%raw() /= bd(e)%raw()) why = "a date element value differs"
                end do
            case (PK_TIME)
                call ra%get(at, is_valid=av)
                call rb%get(bt, is_valid=bv)
                do e = 1_int64, n
                    if (at(e)%is_null() .neqv. bt(e)%is_null()) why = "a time element's nullness differs"
                    if (at(e)%is_null()) cycle
                    if (at(e)%raw() /= bt(e)%raw()) why = "a time element value differs"
                end do
            case (PK_TIMESTAMP)
                call ra%get(ats, is_valid=av)
                call rb%get(bts, is_valid=bv)
                do e = 1_int64, n
                    if (ats(e)%is_null() .neqv. bts(e)%is_null()) why = "a timestamp element's nullness differs"
                    if (ats(e)%is_null()) cycle
                    if (ats(e)%to_unix(parquet_unit_nanos) /= bts(e)%to_unix(parquet_unit_nanos)) then
                        why = "a timestamp element value differs"
                    end if
                end do
            case default
                why = "unexpected payload kind"
            end select
            if (len(why) > 0) return
            ! The temporal kinds carry their nullness inside the element and have just been
            ! compared on it directly; the other six carry it in the payload bitmap, which is what
            ! `is_valid` reports.
            if (a%element_kind() /= PK_DATE .and. a%element_kind() /= PK_TIME .and. &
                a%element_kind() /= PK_TIMESTAMP) then
                do e = 1_int64, n
                    if (av(e) .neqv. bv(e)) then
                        why = "an element's nullness differs"
                        return
                    end if
                end do
            end if
        end do
        same = .true.
    end function lists_equal

    !> Writes `lc` whole, reads it back, and compares. The shape query is asserted too: a round
    !> trip alone would pass against a writer that emitted a fixed_size_list.
    subroutine round_trip(lc, file, error)
        type(parquet_list_column), intent(in), target :: lc !! the column to write.
        character(len=*), intent(in) :: file        !! this test's own output path.
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_list_column), target :: back
        character(len=:), allocatable :: shape, why
        logical :: same

        call parquet_open_writer(w, file)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, file)
        call parquet_get_column_shape(r, "lst", shape)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, shape == "list", "a written list column must report shape 'list'")
        if (allocated(error)) return
        same = lists_equal(lc, back, why)
        call check(error, same, "the column read back must equal the one written: "//why)
    end subroutine round_trip

    !> The core case: rows of differing length.
    subroutine test_ragged_round_trip(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc
        call build_ragged(lc, 11_int64)
        call round_trip(lc, "test_run/test_list_write_ragged.parquet", error)
    end subroutine test_ragged_round_trip

    !> A null row, a present-but-empty row and a null element inside a present row, in one column.
    !> The three are stored in three different places, so a write that collapsed any two of them
    !> would still pass a test asserting only one.
    subroutine test_all_three_null_levels(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        logical, allocatable :: ok(:)
        character(len=*), parameter :: FILE = "test_run/test_list_write_nulls.parquet"

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32, 3_int32])
        call lc%append_null_row()
        call lc%append_row([integer(int32) ::])
        call lc%append_row([10_int32, 20_int32], is_valid=[.true., .false.])

        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, back%size() == 4_int64, "four rows must come back")
        if (allocated(error)) return
        call check(error, .not. back%is_null(1_int64), "row 1 must be present")
        if (allocated(error)) return
        call check(error, back%is_null(2_int64), "row 2 must come back a NULL list")
        if (allocated(error)) return
        call check(error, .not. back%is_null(3_int64), "row 3 must come back PRESENT, not null")
        if (allocated(error)) return
        call check(error, back%length(3_int64) == 0_int64, "row 3 must come back empty")
        if (allocated(error)) return
        call check(error, back%length(4_int64) == 2_int64, "row 4 must keep both its elements")
        if (allocated(error)) return
        row = back%view(4_int64)
        call row%get(v, is_valid=ok)
        call check(error, ok(1), "row 4's first element must be present")
        if (allocated(error)) return
        call check(error, .not. ok(2), "row 4's second element must come back NULL")
        if (allocated(error)) return
        call check(error, v(1) == 10_int32, "row 4's first element must keep its value")
    end subroutine test_all_three_null_levels

    !> A column every row of which holds three elements is still a variable-length LIST, because
    !> the caller asked for one. This is the assertion that stops the whole suite passing against
    !> a writer that emitted a fixed_size_list and let the tolerant read path hide it.
    subroutine test_uniform_writes_as_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: shape
        integer(int64) :: i
        character(len=*), parameter :: FILE = "test_run/test_list_write_uniform.parquet"

        call lc%init(PK_INT32)
        do i = 1_int64, 6_int64
            call lc%append_row([int(i, int32), int(i, int32) + 100_int32, int(i, int32) + 200_int32])
        end do
        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_get_column_shape(r, "lst", shape)
        call parquet_close_reader(r)
        call check(error, shape == "list", &
            "a uniform-length list column must still be written as a LIST, not a vector")
    end subroutine test_uniform_writes_as_list

    !> One round trip per non-string, non-temporal payload kind; the other four have their own
    !> tests because their element nullness or their bytes need separate assertions.
    subroutine test_every_payload_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc

        call lc%init(PK_INT64)
        call lc%append_row([1_int64, 2_int64])
        call lc%append_row([3_int64])
        call round_trip(lc, "test_run/test_list_write_i64.parquet", error)
        if (allocated(error)) return

        call lc%init(PK_FLOAT32)
        call lc%append_row([1.5_real32, 2.25_real32])
        call lc%append_row([3.125_real32])
        call round_trip(lc, "test_run/test_list_write_f32.parquet", error)
        if (allocated(error)) return

        call lc%init(PK_FLOAT64)
        call lc%append_row([1.5_real64, 2.25_real64, 8.5_real64])
        call lc%append_row([3.125_real64])
        call round_trip(lc, "test_run/test_list_write_f64.parquet", error)
        if (allocated(error)) return

        call lc%init(PK_LOGICAL)
        call lc%append_row([.true., .false., .true.])
        call lc%append_row([.false.])
        call round_trip(lc, "test_run/test_list_write_bool.parquet", error)
    end subroutine test_every_payload_kind

    !> A string payload's bytes cross as a packed offsets+bytes buffer rather than as a padded
    !> block, so an empty element and one with a trailing space are the two cases a padded path
    !> would silently change.
    subroutine test_string_payload(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_list_row) :: row
        character(len=:), allocatable :: s(:)
        logical, allocatable :: ok(:)
        character(len=*), parameter :: FILE = "test_run/test_list_write_string.parquet"

        call lc%init(PK_STRING)
        call lc%append_row([character(len=5) :: "alpha", "", "pad  "])
        call lc%append_row([character(len=2) :: "hi"], is_valid=[.false.])

        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, back%size() == 2_int64, "two rows must come back")
        if (allocated(error)) return
        row = back%view(1_int64)
        call row%get(s)
        call check(error, size(s) == 3, "row 1 must hold three strings")
        if (allocated(error)) return
        call check(error, trim(s(1)) == "alpha", "the first element must keep its bytes")
        if (allocated(error)) return
        call check(error, len_trim(s(2)) == 0, "an empty element must come back empty")
        if (allocated(error)) return
        call check(error, s(3) == "pad  ", "a trailing space must survive the write verbatim")
        if (allocated(error)) return
        row = back%view(2_int64)
        call row%get(s, is_valid=ok)
        call check(error, .not. ok(1), "a null string element must come back null")
    end subroutine test_string_payload

    !> A temporal element carries its own null state, so it is the one payload family whose
    !> element nullness never touches the payload bitmap. The declared unit is asserted too:
    !> a schema-less writer defaults to microseconds and a timestamp written in one unit and
    !> read in another would still round-trip its VALUES while changing the column's type.
    subroutine test_temporal_payload(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_list_row) :: row
        type(parquet_date), allocatable :: d(:)
        type(parquet_date) :: d1, d2, dnull
        character(len=:), allocatable :: why
        logical :: same
        character(len=*), parameter :: FILE = "test_run/test_list_write_date.parquet"

        call d1%set(2024, 3, 1)
        call d2%set(1999, 12, 31)
        call lc%init(PK_DATE)
        call lc%append_row([d1, dnull, d2])
        call lc%append_row([d1])
        call lc%append_null_row()

        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        same = lists_equal(lc, back, why)
        call check(error, same, "a date payload must round-trip: "//why)
        if (allocated(error)) return
        row = back%view(1_int64)
        call row%get(d)
        call check(error, .not. d(1)%is_null(), "element 1 must be present")
        if (allocated(error)) return
        call check(error, d(2)%is_null(), "element 2 must come back a null DATE, not a null bit")
        if (allocated(error)) return
        call check(error, .not. d(3)%is_null(), "element 3 must be present")
        if (allocated(error)) return
        call check(error, back%is_null(3_int64), "the null ROW must still be a null row")
    end subroutine test_temporal_payload

    !> The strongest test in the suite: every list column of an Arrow-written fixture is read,
    !> written straight back out, re-read and compared against the first read. Covers all nine
    !> payload kinds, a `large_list`, element nulls, a null row, an empty row and three row groups
    !> at once, against an oracle no part of this library produced.
    !>
    !> The comparison is first-read against second-read rather than against the file: two of the
    !> fixture's columns are stored in element types this library reports as a WIDER Fortran kind
    !> (`int8`->int32, `uint32`->int64), so writing them back produces a different physical type
    !> by design -- and that is exactly the property being checked, since the VALUES must survive.
    subroutine test_rewrite_fixture(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_writer) :: w
        type(parquet_list_column), target :: first(15), again
        character(len=:), allocatable :: why, shape
        logical :: same
        integer :: k
        character(len=*), parameter :: FILE = "test_run/test_list_write_rewrite.parquet"
        character(len=10), parameter :: COLS(15) = [character(len=10) :: &
            "i32", "i64", "f32", "f64", "flag", "text", "day", "clock", "stamp", &
            "narrow8", "wide32", "big", "elem_nulls", "day_nulls", "mixed"]

        call parquet_open_reader(r, PAYLOADS)
        do k = 1, size(COLS)
            call parquet_read_column(r, trim(COLS(k)), first(k))
        end do
        call parquet_close_reader(r)

        call parquet_open_writer(w, FILE)
        do k = 1, size(COLS)
            call parquet_write_column(w, trim(COLS(k)), first(k))
        end do
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        do k = 1, size(COLS)
            call parquet_get_column_shape(r, trim(COLS(k)), shape)
            call check(error, shape == "list", "re-written column "//trim(COLS(k))//" must still be a list")
            if (allocated(error)) then
                call parquet_close_reader(r)
                return
            end if
            call parquet_read_column(r, trim(COLS(k)), again)
            same = lists_equal(first(k), again, why)
            call check(error, same, "column "//trim(COLS(k))//" must re-write identically: "//why)
            if (allocated(error)) then
                call parquet_close_reader(r)
                return
            end if
        end do
        call parquet_close_reader(r)
    end subroutine test_rewrite_fixture

    !> The same column written one row group at a time must produce the same rows as the
    !> whole-column write. Three row groups, so the second and third exercise the path where the
    !> field is already fixed by the first.
    subroutine test_chunked_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, part, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: why, shape
        logical :: same
        integer(int64) :: g, lo, i
        integer(int64), allocatable :: idx(:)
        character(len=*), parameter :: FILE = "test_run/test_list_write_chunked.parquet"

        call build_ragged(lc, 9_int64)

        call parquet_open_writer(w, FILE)
        do g = 0_int64, 2_int64
            lo = g*3_int64
            idx = [(lo + i, i = 1_int64, 3_int64)]
            call lc%deep_copy(part)
            call part%gather_rows(idx)
            call parquet_new_row_group(w, 3_int64)
            call parquet_write_column_chunk(w, "lst", part)
            call parquet_finish_row_group(w)
        end do
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_get_column_shape(r, "lst", shape)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, shape == "list", "a chunk-written list column must report shape 'list'")
        if (allocated(error)) return
        same = lists_equal(lc, back, why)
        call check(error, same, "the chunked write must produce the same rows: "//why)
    end subroutine test_chunked_agrees

    !> A whole-file row mask drops rows before they are written. The masked rebuild goes through
    !> %gather_rows, which also drops the dropped rows' elements -- so the surviving rows' lengths
    !> and values must be exactly the surviving rows', with no gap left behind.
    subroutine test_row_mask(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, expect, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: why
        logical :: same
        logical :: keep(6)
        integer(int64) :: i
        character(len=*), parameter :: FILE = "test_run/test_list_write_masked.parquet"

        call build_ragged(lc, 6_int64)
        keep = [.true., .false., .true., .true., .false., .true.]
        call lc%deep_copy(expect)
        call expect%gather_rows(pack([(i, i = 1_int64, 6_int64)], keep))

        call parquet_open_writer(w, FILE)
        call parquet_write_row_mask(w, keep)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, back%size() == 4_int64, "the mask must drop two of the six rows")
        if (allocated(error)) return
        same = lists_equal(expect, back, why)
        call check(error, same, "the surviving rows must be exactly the kept ones: "//why)
    end subroutine test_row_mask

    !> A list column with no rows at all. The offsets buffer is one entry long here, so this is
    !> also the smallest case in which an off-by-one in the offsets copy would show.
    subroutine test_zero_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: shape
        character(len=*), parameter :: FILE = "test_run/test_list_write_zero.parquet"

        call lc%init(PK_INT32)
        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_get_column_shape(r, "lst", shape)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, shape == "list", "a zero-row list column must still be a list column")
        if (allocated(error)) return
        call check(error, back%size() == 0_int64, "a zero-row column must read back with no rows")
        if (allocated(error)) return
        call check(error, back%element_kind() == PK_INT32, "its payload kind must survive")
    end subroutine test_zero_rows

    !> Rows but no elements: every row either null or present-and-empty. The child array is then
    !> zero-length, which is the case a bulk buffer path most easily gets wrong.
    subroutine test_no_elements(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=*), parameter :: FILE = "test_run/test_list_write_noelems.parquet"

        call lc%init(PK_FLOAT64)
        call lc%append_null_row()
        call lc%append_row([real(real64) ::])
        call lc%append_null_row()

        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)
        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, back%size() == 3_int64, "three rows must come back")
        if (allocated(error)) return
        call check(error, back%total_elements() == 0_int64, "no element may be invented")
        if (allocated(error)) return
        call check(error, back%is_null(1_int64), "row 1 must stay null")
        if (allocated(error)) return
        call check(error, .not. back%is_null(2_int64), "row 2 must stay present and empty")
        if (allocated(error)) return
        call check(error, back%is_null(3_int64), "row 3 must stay null")
    end subroutine test_no_elements

    !> A schema-enforced writer, with the column declared `list[int32]` in an in-code schema.
    subroutine test_schema_enforced(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_schema) :: schema
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: why
        logical :: same
        character(len=*), parameter :: FILE = "test_run/test_list_write_schema.parquet"

        call schema%init(table="list_write_schema")
        call schema%add_field("lst", "list[int32]")
        call parquet_parse_maml(schema)

        call build_ragged(lc, 5_int64)
        call parquet_open_writer(w, FILE, schema)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)
        same = lists_equal(lc, back, why)
        call check(error, same, "a schema-enforced list write must round-trip: "//why)
    end subroutine test_schema_enforced

    !> A declared list column that is never written must still appear in the file, with zero rows
    !> and its declared element kind -- the close-time empty-column path, which for a list column
    !> has to build the column rather than pass a zero-sized array.
    subroutine test_declared_but_unwritten(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_schema) :: schema
        type(parquet_list_column), target :: back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        character(len=:), allocatable :: shape
        character(len=*), parameter :: FILE = "test_run/test_list_write_unwritten.parquet"

        call schema%init(table="list_write_unwritten")
        call schema%add_field("lst", "list[float64]")
        call parquet_parse_maml(schema)

        call parquet_open_writer(w, FILE, schema)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_get_column_shape(r, "lst", shape)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, shape == "list", "an unwritten declared list column must still be a list")
        if (allocated(error)) return
        call check(error, back%size() == 0_int64, "it must have no rows")
        if (allocated(error)) return
        call check(error, back%element_kind() == PK_FLOAT64, "it must keep its declared element kind")
    end subroutine test_declared_but_unwritten

    !> `list[timestamp[ms,utc]]` declares the element's unit and UTC flag in the same fields a
    !> scalar timestamp column uses, so a value written in one unit and read back must be the same
    !> instant -- and the reported unit must be the declared one, not the default microseconds.
    subroutine test_schema_temporal_unit(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_schema) :: schema
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        type(parquet_list_row) :: row
        type(parquet_timestamp) :: ts1, ts2
        type(parquet_timestamp), allocatable :: got(:)
        character(len=*), parameter :: FILE = "test_run/test_list_write_tsunit.parquet"

        call schema%init(table="list_write_tsunit")
        call schema%add_field("lst", "list[timestamp[ms,utc]]")
        call parquet_parse_maml(schema)

        call ts1%set_unix(1700000000_int64, parquet_unit_seconds)
        call ts2%set_unix(1700000123_int64, parquet_unit_seconds)
        call lc%init(PK_TIMESTAMP)
        call lc%append_row([ts1, ts2])
        call lc%append_row([ts1])

        call parquet_open_writer(w, FILE, schema)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, back%size() == 2_int64, "two rows must come back")
        if (allocated(error)) return
        row = back%view(1_int64)
        call row%get(got)
        call check(error, got(1)%to_unix(parquet_unit_seconds) == 1700000000_int64, &
            "the first instant must survive the declared-unit write")
        if (allocated(error)) return
        call check(error, got(2)%to_unix(parquet_unit_seconds) == 1700000123_int64, &
            "the second instant must survive the declared-unit write")
    end subroutine test_schema_temporal_unit

    !> A list column beside ordinary scalar and vector columns: the row-count reconciliation at
    !> close has to accept a list column's row count as it accepts any other's.
    subroutine test_beside_other_columns(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_list_column), target :: lc, back
        type(parquet_writer) :: w
        type(parquet_reader) :: r
        integer(int32) :: ids(5), got_ids(5)
        integer(int32) :: vec(2, 5)
        character(len=:), allocatable :: why, shape_lst, shape_vec
        logical :: same
        integer :: i
        character(len=*), parameter :: FILE = "test_run/test_list_write_mixed.parquet"

        do i = 1, 5
            ids(i) = int(i, int32)
            vec(1, i) = int(i, int32)
            vec(2, i) = int(i, int32) + 10_int32
        end do
        call build_ragged(lc, 5_int64)

        call parquet_open_writer(w, FILE)
        call parquet_write_column(w, "id", ids)
        call parquet_write_column(w, "vec", vec)
        call parquet_write_column(w, "lst", lc)
        call parquet_close_writer(w)

        call parquet_open_reader(r, FILE)
        call parquet_get_column_shape(r, "lst", shape_lst)
        call parquet_get_column_shape(r, "vec", shape_vec)
        call parquet_read_column(r, "id", got_ids)
        call parquet_read_column(r, "lst", back)
        call parquet_close_reader(r)

        call check(error, shape_lst == "list", "the list column must be a list")
        if (allocated(error)) return
        call check(error, shape_vec == "vector", "the vector column beside it must stay a vector")
        if (allocated(error)) return
        call check(error, all(got_ids == ids), "the scalar column beside it must be unchanged")
        if (allocated(error)) return
        same = lists_equal(lc, back, why)
        call check(error, same, "the list column must round-trip beside the others: "//why)
    end subroutine test_beside_other_columns

end module test_list_write
