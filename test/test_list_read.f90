!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for reading a variable-length `LIST` column out of a Parquet file into a
!> `parquet_list_column` -- the whole-column and row-group-scoped specifics of
!> `parquet_read_column`/`parquet_read_column_chunk`, plus `parquet_get_column_shape`.
!!
!! **Why this suite is shaped the way it is.** Every failure mode of a list read is a plausible
!! wrong answer rather than a crash: offsets shifted by one still produce values in range and
!! lengths that sum correctly; a dropped slicing offset under a filtered read still produces the
!! right number of rows; collapsing the two null levels still passes any assertion that only
!! looks at one of them. So the tests here assert THREE things separately about every fixture --
!! per-row lengths, per-element values, and each null level on its own -- and the fixtures are
!! chosen so that no two rows have the same length and no two elements the same value.
!!
!! The two fixtures and what each is for:
!!
!! * `test/fixtures/list_widths.parquet` -- 16 rows in **4 row groups**, `int32` payloads only.
!!   Its columns cover the shapes: `ragged`, `with_null` (one null row), `null_avg` (four),
!!   `with_empty` (a present-but-empty row), `uniform` (the control that must keep reading as a
!!   width-3 vector column), `scalar` (an ordinary column), and `nested.vals` (a list under a
!!   struct, reached by its dotted path).
!! * `test/fixtures/list_payloads.parquet` -- 12 rows in 3 row groups, one ragged column per
!!   payload element type, plus a `large_list`, two columns whose element type must be CONVERTED
!!   on the way out (`int8`->int32, `uint32`->int64), columns carrying element nulls / a null row /
!!   an empty row, and a scalar `rowid` control. Without it, eight of the nine payload arms would
!!   never be entered. `rowid` is the only column in it a filter rule or a sort key can name --
!!   neither can be written against a list column -- so it is what makes a transformed read of this
!!   fixture possible at all.
!!
!! Its value convention -- element `e` (0-based) of row `r` (0-based) is `r*100 + e`, and row `r`
!! holds `mod(r,4) + 1` elements -- is what lets an assertion name any cell from its coordinates.
module test_list_read
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_list_read

    !> The two fixtures every test here reads. Neither is ever written to.
    character(len=*), parameter :: WIDTHS = "test/fixtures/list_widths.parquet"
    character(len=*), parameter :: PAYLOADS = "test/fixtures/list_payloads.parquet"

contains

    !> Registers every test in this suite.
    subroutine collect_tests_parquet_list_read(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("a ragged column reads with its own per-row lengths", test_ragged_reads), &
            new_unittest("a null row survives the read and is not an empty row", test_null_row_survives), &
            new_unittest("an empty row survives and is not a null row", test_empty_row_survives), &
            new_unittest("several null rows across row groups all survive", test_many_null_rows), &
            new_unittest("a null element survives inside a present row", test_element_null_survives), &
            new_unittest("every payload kind reads", test_every_payload_kind), &
            new_unittest("a string payload keeps exact bytes, empty and trailing space", test_string_payload), &
            new_unittest("a narrowing element type is converted, not reinterpreted", test_converted_payloads), &
            new_unittest("a large_list column reads like a list column", test_large_list), &
            new_unittest("a temporal payload carries its nulls inside the element", test_temporal_element_nulls), &
            new_unittest("null row, empty row and null element in one column", test_all_three_null_levels), &
            new_unittest("a filtered read returns the surviving rows with their own lengths", test_filtered_read), &
            new_unittest("a filter matching nothing yields an empty typed column", &
                test_filter_matching_nothing), &
            new_unittest("an empty read works for every payload kind", &
                test_empty_read_for_every_payload_kind), &
            new_unittest("a chunked read of an emptied row group yields nothing", &
                test_chunk_matching_nothing), &
            new_unittest("a sorted read returns the rows in key order", test_sorted_read), &
            new_unittest("a sampled read returns a subset of whole rows", test_sampled_read), &
            new_unittest("the chunked form agrees with the whole-column form", test_chunked_agrees), &
            new_unittest("a uniform list column still reads into a list column", test_uniform_into_list), &
            new_unittest("a uniform list column still reads as a width-3 vector", test_uniform_still_vector), &
            new_unittest("a fixed-size list column reads into a list column too", test_vector_into_list), &
            new_unittest("a struct-nested list is addressable by its dotted path", test_struct_nested_list), &
            new_unittest("a ragged struct-nested list behaves like a top-level one", &
                test_struct_nested_ragged_matches_top_level), &
            new_unittest("get_column_shape reports the container shape", test_column_shape), &
            new_unittest("get_column_type still reports the element type", test_column_type_unchanged), &
            new_unittest("reading a list column twice replaces rather than appends", test_read_twice_replaces), &
            new_unittest("a map under a struct resolves; an intermediate struct does not", test_map_stays_unreadable) &
            ]
    end subroutine collect_tests_parquet_list_read

    !> The number of elements row `r` (1-based) holds in either fixture: 1,2,3,4 repeating.
    pure function want_len(r) result(n)
        integer(int64), intent(in) :: r !! 1-based row index.
        integer(int64) :: n             !! elements that row holds.
        n = mod(r - 1_int64, 4_int64) + 1_int64
    end function want_len

    !> The canonical value of element `e` (1-based) of row `r` (1-based): `(r-1)*100 + (e-1)`.
    pure function want_val(r, e) result(v)
        integer(int64), intent(in) :: r !! 1-based row index.
        integer(int64), intent(in) :: e !! 1-based element index within the row.
        integer(int64) :: v             !! the canonical value.
        v = (r - 1_int64)*100_int64 + (e - 1_int64)
    end function want_val

    !> Reads `ragged` and checks every row's length and every element's value.
    subroutine test_ragged_reads(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i, e
        logical :: ok

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "ragged", lc)
        call parquet_close_reader(r)

        call check(error, lc%size() == 16_int64, "ragged must read 16 rows")
        if (allocated(error)) return
        call check(error, lc%element_kind() == PK_INT32, "ragged's payload must be int32")
        if (allocated(error)) return
        ok = lc%validate()
        call check(error, ok, "the column read from ragged must satisfy its own invariants")
        if (allocated(error)) return
        do i = 1_int64, 16_int64
            call check(error, lc%length(i) == want_len(i), "row length must match the file's own")
            if (allocated(error)) return
            row = lc%view(i)
            call row%get(v)
            call check(error, size(v, kind=int64) == want_len(i), "%get must size to the row")
            if (allocated(error)) return
            do e = 1_int64, want_len(i)
                call check(error, int(v(e), int64) == want_val(i, e), "element value must match the file's own")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_ragged_reads

    !> `with_null` has exactly one null row (row 6). A null row is not an empty row, so both
    !> properties are asserted, and the null row's own neighbours are checked too -- a read that
    !> mislaid the row bitmap by one would otherwise pass on the count alone.
    subroutine test_null_row_survives(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        integer(int64) :: i, nulls

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "with_null", lc)
        call parquet_close_reader(r)

        nulls = 0_int64
        do i = 1_int64, lc%size()
            if (lc%is_null(i)) nulls = nulls + 1_int64
        end do
        call check(error, nulls == 1_int64, "with_null must read back exactly one null row")
        if (allocated(error)) return
        call check(error, lc%is_null(6_int64), "row 6 must be the null one")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(5_int64), "row 5 must not be null")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(7_int64), "row 7 must not be null")
        if (allocated(error)) return
        call check(error, lc%length(6_int64) == 0_int64, "a null row has no elements")
        if (allocated(error)) return
        call check(error, lc%length(5_int64) == 3_int64, "with_null's present rows all hold 3 elements")
    end subroutine test_null_row_survives

    !> `with_empty`'s row 6 is PRESENT and holds nothing. That is the case a read collapsing the
    !> two null levels gets wrong in the other direction from test_null_row_survives.
    subroutine test_empty_row_survives(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i, nulls

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "with_empty", lc)
        call parquet_close_reader(r)

        nulls = 0_int64
        do i = 1_int64, lc%size()
            if (lc%is_null(i)) nulls = nulls + 1_int64
        end do
        call check(error, nulls == 0_int64, "with_empty holds no NULL row, only an empty one")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(6_int64), "the empty row must not read back as null")
        if (allocated(error)) return
        call check(error, lc%is_empty(6_int64), "the empty row must read back as empty")
        if (allocated(error)) return
        call check(error, lc%length(6_int64) == 0_int64, "the empty row holds no elements")
        if (allocated(error)) return
        row = lc%view(6_int64)
        call row%get(v)
        call check(error, size(v) == 0, "%get on an empty row yields a zero-size array")
        if (allocated(error)) return
        call check(error, lc%length(7_int64) == 3_int64, "the row after the empty one is unaffected")
    end subroutine test_empty_row_survives

    !> `null_avg` is null on every 4th row, i.e. once per row group -- so a read that lost the row
    !> bitmap only at a row-group boundary would still pass test_null_row_survives.
    subroutine test_many_null_rows(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        integer(int64) :: i

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "null_avg", lc)
        call parquet_close_reader(r)

        call check(error, lc%null_count() == 4_int64, "null_avg must read back four null rows")
        if (allocated(error)) return
        do i = 1_int64, 16_int64
            if (mod(i, 4_int64) == 0_int64) then
                call check(error, lc%is_null(i), "every 4th row of null_avg is null")
            else
                call check(error, .not. lc%is_null(i), "every other row of null_avg is present")
                if (allocated(error)) return
                call check(error, lc%length(i) == 5_int64, "null_avg's present rows hold 5 elements")
            end if
            if (allocated(error)) return
        end do
    end subroutine test_many_null_rows

    !> `elem_nulls`' every row is PRESENT and its first element is Null. Row nullness and element
    !> nullness live in different storage, so both are asserted here.
    subroutine test_element_null_survives(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        logical, allocatable :: ok(:)
        integer(int64) :: i, e

        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "elem_nulls", lc)
        call parquet_close_reader(r)

        call check(error, lc%null_count() == 0_int64, "elem_nulls has no NULL ROW")
        if (allocated(error)) return
        do i = 1_int64, 12_int64
            call check(error, .not. lc%is_null(i), "every row of elem_nulls is present")
            if (allocated(error)) return
            call check(error, lc%length(i) == want_len(i), "an element null does not shorten the row")
            if (allocated(error)) return
            row = lc%view(i)
            call row%get(v, is_valid=ok)
            call check(error, size(ok, kind=int64) == want_len(i), "the mask has one entry per element")
            if (allocated(error)) return
            call check(error, .not. ok(1), "element 1 of every row is null")
            if (allocated(error)) return
            do e = 2_int64, want_len(i)
                call check(error, ok(e), "the other elements are not null")
                if (allocated(error)) return
                call check(error, int(v(e), int64) == want_val(i, e), "the other elements keep their values")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_element_null_survives

    !> One column per payload kind, checked for kind, lengths and one representative value each.
    !> The point is coverage of all nine dispatch arms in one place; the deeper per-kind checks are
    !> in the tests that follow.
    subroutine test_every_payload_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: vi32(:)
        integer(int64), allocatable :: vi64(:)
        real(real32), allocatable :: vf32(:)
        real(real64), allocatable :: vf64(:)
        logical, allocatable :: vb(:)
        type(parquet_date), allocatable :: vd(:)
        type(parquet_time), allocatable :: vt(:)
        type(parquet_timestamp), allocatable :: vs(:)
        integer(int64) :: i
        integer :: yy, mm, dd, hh, mi, ss

        call parquet_open_reader(r, PAYLOADS)

        call parquet_read_column(r, "i32", lc)
        call check(error, lc%element_kind() == PK_INT32, "i32 must read as an int32 payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vi32)
        call check(error, size(vi32) == 4 .and. vi32(3) == 302_int32, "i32 row 4 element 3 is 302")
        if (allocated(error)) return

        call parquet_read_column(r, "i64", lc)
        call check(error, lc%element_kind() == PK_INT64, "i64 must read as an int64 payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vi64)
        call check(error, vi64(3) == 302_int64, "i64 row 4 element 3 is 302")
        if (allocated(error)) return

        call parquet_read_column(r, "f32", lc)
        call check(error, lc%element_kind() == PK_FLOAT32, "f32 must read as a float32 payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vf32)
        call check(error, abs(vf32(3) - 302.5_real32) < 1.0e-4_real32, "f32 row 4 element 3 is 302.5")
        if (allocated(error)) return

        call parquet_read_column(r, "f64", lc)
        call check(error, lc%element_kind() == PK_FLOAT64, "f64 must read as a float64 payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vf64)
        call check(error, abs(vf64(3) - 302.25_real64) < 1.0e-9_real64, "f64 row 4 element 3 is 302.25")
        if (allocated(error)) return

        call parquet_read_column(r, "flag", lc)
        call check(error, lc%element_kind() == PK_LOGICAL, "flag must read as a logical payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vb)
        ! row index 3 (0-based), element index 2 (0-based) => (3+2) even => .false. ... .true.
        call check(error, vb(1) .eqv. .false., "flag row 4 element 1: 3+0 is odd, so .false.")
        if (allocated(error)) return
        call check(error, vb(2) .eqv. .true., "flag row 4 element 2: 3+1 is even, so .true.")
        if (allocated(error)) return

        call parquet_read_column(r, "day", lc)
        call check(error, lc%element_kind() == PK_DATE, "day must read as a date payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vd)
        call check(error, vd(3)%raw() == 302_int32, "day row 4 element 3 is 302 days after the epoch")
        if (allocated(error)) return

        call parquet_read_column(r, "clock", lc)
        call check(error, lc%element_kind() == PK_TIME, "clock must read as a time payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vt)
        ! stored as microseconds = value*1000, read back as canonical nanoseconds-of-day.
        call check(error, vt(3)%raw() == 302000_int64*1000_int64, "clock row 4 element 3 is 302000 us in ns")
        if (allocated(error)) return

        call parquet_read_column(r, "stamp", lc)
        call check(error, lc%element_kind() == PK_TIMESTAMP, "stamp must read as a timestamp payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vs)
        call vs(3)%get(yy, mm, dd, hh, mi, ss)
        ! stored as milliseconds = value*1000, i.e. 302 seconds after the epoch.
        call check(error, yy == 1970 .and. mm == 1 .and. dd == 1, "stamp row 4 element 3 is on the epoch day")
        if (allocated(error)) return
        call check(error, hh == 0 .and. mi == 5 .and. ss == 2, "stamp row 4 element 3 is 302 seconds in")
        if (allocated(error)) return

        do i = 1_int64, 12_int64
            call check(error, lc%length(i) == want_len(i), "every payload column is ragged the same way")
            if (allocated(error)) return
        end do
        call parquet_close_reader(r)
    end subroutine test_every_payload_kind

    !> A string payload is stored as bytes, not as a fixed-width padded field: an empty value and
    !> a value ending in a space are what a padded read cannot represent, so both are asserted.
    subroutine test_string_payload(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        character(len=:), allocatable :: v(:)
        integer(int64) :: i

        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "text", lc)
        call parquet_close_reader(r)

        call check(error, lc%element_kind() == PK_STRING, "text must read as a string payload")
        if (allocated(error)) return
        do i = 1_int64, 12_int64
            call check(error, lc%length(i) == want_len(i), "a string payload is ragged like the rest")
            if (allocated(error)) return
        end do
        row = lc%view(1_int64)
        call row%get(v)
        call check(error, size(v) == 1, "row 1 holds one string")
        if (allocated(error)) return
        call check(error, len_trim(v(1)) == 0, "row 1's only value is the empty string")
        if (allocated(error)) return
        row = lc%view(2_int64)
        call row%get(v)
        call check(error, size(v) == 2, "row 2 holds two strings")
        if (allocated(error)) return
        ! The declared length comes from the LONGEST value in the row, so element 1 ("pad ") is
        ! not distinguishable from "pad" by len_trim alone -- assert the exact bytes instead.
        call check(error, len(v) == 4, "row 2's values are declared 4 characters wide")
        if (allocated(error)) return
        call check(error, v(1) == "pad ", "a trailing space in the file survives the read")
        if (allocated(error)) return
        call check(error, v(2) == "v101", "row 2's second value is v101")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(v)
        call check(error, v(3) == "v302", "row 4's third value is v302")
    end subroutine test_string_payload

    !> `narrow8` is a `list<int8>` and `wide32` a `list<uint32>`: this library reports the
    !> narrowest LOSSLESS Fortran kind, so they must read as int32 and int64 respectively. wide32's
    !> values sit above huge(int32), which is what makes it prove the conversion rather than merely
    !> exercise it -- a reinterpretation would come back negative or truncated.
    subroutine test_converted_payloads(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: vi32(:)
        integer(int64), allocatable :: vi64(:)

        call parquet_open_reader(r, PAYLOADS)

        call parquet_read_column(r, "narrow8", lc)
        call check(error, lc%element_kind() == PK_INT32, "a list<int8> reads into an int32 payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vi32)
        ! The file stores mod(value, 100), so row 4 element 3 is mod(302, 100) = 2.
        call check(error, vi32(3) == 2_int32, "narrow8 row 4 element 3 is 2")
        if (allocated(error)) return

        call parquet_read_column(r, "wide32", lc)
        call check(error, lc%element_kind() == PK_INT64, "a list<uint32> reads into an int64 payload")
        if (allocated(error)) return
        row = lc%view(4_int64)
        call row%get(vi64)
        call check(error, vi64(3) == 3000000302_int64, "wide32 row 4 element 3 is 3000000302")
        if (allocated(error)) return
        call check(error, vi64(3) > int(huge(1_int32), int64), "wide32's values do not fit an int32")
        call parquet_close_reader(r)
    end subroutine test_converted_payloads

    !> `big` is a genuine `large_list<int32>` (int64 offsets), which no other fixture in this
    !> repository contains -- Parquet cannot store the LIST/LARGE_LIST distinction, so the fixture
    !> has to be written with `store_schema()`. It must read exactly like the `list` column beside
    !> it, which holds the same values.
    subroutine test_large_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: big, plain
        type(parquet_list_row) :: rb, rp
        integer(int32), allocatable :: vb(:), vp(:)
        integer(int64) :: i

        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "big", big)
        call parquet_read_column(r, "i32", plain)
        call parquet_close_reader(r)

        call check(error, big%element_kind() == PK_INT32, "big's payload is int32")
        if (allocated(error)) return
        call check(error, big%size() == plain%size(), "big has as many rows as i32")
        if (allocated(error)) return
        do i = 1_int64, big%size()
            call check(error, big%length(i) == plain%length(i), "big is ragged exactly like i32")
            if (allocated(error)) return
            rb = big%view(i)
            rp = plain%view(i)
            call rb%get(vb)
            call rp%get(vp)
            call check(error, all(vb == vp), "big holds the same values as i32")
            if (allocated(error)) return
        end do
    end subroutine test_large_list

    !> `day_nulls` is a date payload whose first element per row is Null. A temporal element
    !> carries its null state INSIDE the element (a default-initialized parquet_date IS null), so
    !> this combination -- a temporal payload with element nulls -- is reached by no other fixture.
    subroutine test_temporal_element_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        type(parquet_date), allocatable :: v(:)
        logical, allocatable :: ok(:)
        integer(int64) :: i, e

        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "day_nulls", lc)
        call parquet_close_reader(r)

        call check(error, lc%element_kind() == PK_DATE, "day_nulls reads as a date payload")
        if (allocated(error)) return
        call check(error, lc%null_count() == 0_int64, "day_nulls has no null ROW")
        if (allocated(error)) return
        do i = 1_int64, 12_int64
            row = lc%view(i)
            call row%get(v)
            call check(error, size(v, kind=int64) == want_len(i), "an element null does not shorten the row")
            if (allocated(error)) return
            call check(error, v(1)%is_null(), "element 1 of every row is a null date")
            if (allocated(error)) return
            do e = 2_int64, want_len(i)
                call check(error, .not. v(e)%is_null(), "the other elements are real dates")
                if (allocated(error)) return
                call check(error, int(v(e)%raw(), int64) == want_val(i, e), "and carry the file's own day count")
                if (allocated(error)) return
            end do
            ! A temporal element carries its null state inside itself rather than in the payload's
            ! bitmap, so the two views of the same fact are reached by different code and must
            ! agree. Asserting only the element form would miss a read that stored the nullness in
            ! neither place, or in only one.
            call row%get(v, is_valid=ok)
            call check(error, size(ok, kind=int64) == want_len(i), "the mask has one entry per element")
            if (allocated(error)) return
            call check(error, .not. ok(1), "and reports element 1 null, like the element itself does")
            if (allocated(error)) return
            call check(error, all(ok(2:)), "and the rest valid")
            if (allocated(error)) return
        end do
    end subroutine test_temporal_element_nulls

    !> `mixed` carries a null row (6), an empty row (7) and a null element (row 8's last) at once,
    !> which is the combination that distinguishes all three storage levels in a single read.
    subroutine test_all_three_null_levels(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        logical, allocatable :: ok(:)

        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "mixed", lc)
        call parquet_close_reader(r)

        call check(error, lc%size() == 12_int64, "mixed reads 12 rows")
        if (allocated(error)) return
        call check(error, lc%null_count() == 1_int64, "mixed has exactly one null row")
        if (allocated(error)) return
        call check(error, lc%is_null(6_int64), "row 6 is the null one")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(7_int64), "row 7 is present, not null")
        if (allocated(error)) return
        call check(error, lc%is_empty(7_int64), "row 7 is empty")
        if (allocated(error)) return
        call check(error, lc%length(8_int64) == 4_int64, "row 8 holds four elements")
        if (allocated(error)) return
        row = lc%view(8_int64)
        call row%get(v, is_valid=ok)
        call check(error, .not. ok(4), "row 8's last element is null")
        if (allocated(error)) return
        call check(error, ok(3), "row 8's third element is not null")
    end subroutine test_all_three_null_levels

    !> A filtered read must yield the SURVIVING rows with their own lengths. A filtered array is a
    !> sliced Arrow array, which is the case in which a list's three independent slicing offsets
    !> (row validity, child slice start, element validity) all become nonzero -- so a read that
    !> dropped any of them would return values in range and lengths that still summed correctly.
    !>
    !> The negative control is the unfiltered read in the same test: without it, the assertion
    !> would hold just as well against a filter that never applied.
    subroutine test_filtered_read(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_filter) :: f
        type(parquet_list_column), target :: all_rows, kept
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i, src

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "ragged", all_rows)
        call parquet_close_reader(r)
        call check(error, all_rows%size() == 16_int64, "the unfiltered control reads every row")
        if (allocated(error)) return

        ! scalar is row-1 (0-based), so scalar >= 8 keeps rows 9..16.
        call f%add("scalar >= 8")
        call parquet_open_reader(r, WIDTHS, filter=f)
        call parquet_read_column(r, "ragged", kept)
        call parquet_close_reader(r)

        call check(error, kept%size() == 8_int64, "the filter must keep 8 of 16 rows")
        if (allocated(error)) return
        call check(error, kept%size() /= all_rows%size(), "the filtered read must differ from the control")
        if (allocated(error)) return
        do i = 1_int64, 8_int64
            src = i + 8_int64
            call check(error, kept%length(i) == all_rows%length(src), "a kept row keeps its own length")
            if (allocated(error)) return
            row = kept%view(i)
            call row%get(v)
            call check(error, int(v(1), int64) == want_val(src, 1_int64), "a kept row keeps its own values")
            if (allocated(error)) return
        end do
    end subroutine test_filtered_read

    !> A filter matching nothing yields a column with no rows and no elements. That is an ordinary
    !> state, not an error -- and it is the one that exercises every zero-length allocation on the
    !> read path at once, which is where a zero-sized array crossing the `bind(C)` boundary would
    !> show up (and where nagfor's checked build would see it).
    subroutine test_filter_matching_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_filter) :: f
        type(parquet_list_column), target :: lc
        logical :: ok

        call f%add("scalar > 1000")
        call parquet_open_reader(r, WIDTHS, filter=f)
        call parquet_read_column(r, "ragged", lc)
        call parquet_close_reader(r)

        call check(error, lc%size() == 0_int64, "a filter matching nothing yields no rows")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 0_int64, "and no elements")
        if (allocated(error)) return
        ! The payload kind still comes from the file, so the column is typed even with no rows --
        ! which is what a caller inspecting %element_kind() before looking at any value needs.
        call check(error, lc%element_kind() == PK_INT32, "but the payload kind still comes from the file")
        if (allocated(error)) return
        ok = lc%validate()
        call check(error, ok, "and the empty column satisfies its own invariants")
    end subroutine test_filter_matching_nothing

    !> The same, swept over EVERY payload kind. Each arm allocates its own typed scratch buffer and
    !> decides for itself whether to adopt it, so a zero-element column is nine separate cases
    !> rather than one -- and the one that was wrong (the logical arm adopted its one-element
    !> scratch buffer, giving the payload a row the offsets did not account for) was reachable only
    !> through a `list<bool>` and would have aborted at `%adopt_rows`' final-offset check.
    subroutine test_empty_read_for_every_payload_kind(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_filter) :: f
        type(parquet_list_column), target :: lc
        character(len=12) :: cols(11)
        integer :: c
        logical :: ok

        cols = [character(len=12) :: "i32", "i64", "f32", "f64", "flag", "text", "day", "clock", &
            "stamp", "big", "mixed"]
        ! `rowid` is the fixture's scalar control and the only column a filter rule can name --
        ! a rule cannot be written against a list column, which is why the fixture has one at all.
        call f%add("rowid > 1000")
        call parquet_open_reader(r, PAYLOADS, filter=f)
        do c = 1, size(cols)
            call parquet_read_column(r, trim(cols(c)), lc)
            call check(error, lc%size() == 0_int64, "a filter matching nothing yields no rows: "//trim(cols(c)))
            if (allocated(error)) return
            call check(error, lc%total_elements() == 0_int64, "and no elements: "//trim(cols(c)))
            if (allocated(error)) return
            call check(error, lc%element_kind() /= PK_NONE, "but a payload kind: "//trim(cols(c)))
            if (allocated(error)) return
            ok = lc%validate()
            call check(error, ok, "and a valid empty column: "//trim(cols(c)))
            if (allocated(error)) return
        end do
        call parquet_close_reader(r)
    end subroutine test_empty_read_for_every_payload_kind

    !> The same for a chunked read of a row group the filter emptied, through both row_group
    !> kind-specifics.
    subroutine test_chunk_matching_nothing(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_filter) :: f
        type(parquet_list_column), target :: lc, lc32
        logical :: ok
        integer(int64) :: i

        ! scalar is row-1 (0-based), so scalar >= 8 empties row groups 1 and 2 entirely.
        call f%add("scalar >= 8")
        call parquet_open_reader(r, WIDTHS, filter=f)
        call parquet_read_column_chunk(r, "ragged", 1_int64, lc)
        call check(error, lc%size() == 0_int64, "a fully filtered row group yields no rows")
        if (allocated(error)) return
        ok = lc%validate()
        call check(error, ok, "and still satisfies its own invariants")
        if (allocated(error)) return
        ! The negative control: a row group the filter did NOT empty still yields its rows.
        call parquet_read_column_chunk(r, "ragged", 3_int64, lc)
        call check(error, lc%size() == 4_int64, "a surviving row group still yields its four rows")
        if (allocated(error)) return
        ! The same two row groups named with a default INTEGER, which is the other row_group
        ! kind-specific of the list form. It converts and forwards, so a truncation or an
        ! off-by-one in that conversion is what the comparison against the int64 answers catches.
        call parquet_read_column_chunk(r, "ragged", 1, lc32)
        call check(error, lc32%size() == 0_int64, &
            "an int32 row_group must empty the same row group the int64 one empties")
        if (allocated(error)) return
        call parquet_read_column_chunk(r, "ragged", 3, lc32)
        call check(error, lc32%size() == lc%size(), &
            "and must yield the same rows for the row group the filter left alone")
        if (allocated(error)) return
        ok = .true.
        do i = 1_int64, lc%size()
            if (lc32%length(i) /= lc%length(i)) ok = .false.
        end do
        call check(error, ok, "the two row_group spellings disagree on the row lengths")
        call parquet_close_reader(r)
    end subroutine test_chunk_matching_nothing

    !> A sorted read installs a permutation, so every row is taken from somewhere else in the file
    !> -- the other way a sliced/reordered array reaches the list read path. Sorting descending on
    !> `scalar` reverses the row order, so row k must be the file's row 17-k, lengths and all.
    subroutine test_sorted_read(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: plain, sorted
        type(parquet_sortkey) :: srt
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i, src

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "ragged", plain)
        call parquet_close_reader(r)

        call srt%add("scalar desc")
        call parquet_open_reader(r, WIDTHS, sort_by=srt)
        call parquet_read_column(r, "ragged", sorted)
        call parquet_close_reader(r)

        call check(error, sorted%size() == 16_int64, "a sorted read still returns every row")
        if (allocated(error)) return
        do i = 1_int64, 16_int64
            src = 17_int64 - i
            call check(error, sorted%length(i) == plain%length(src), "a sorted row brings its own length")
            if (allocated(error)) return
            row = sorted%view(i)
            call row%get(v)
            call check(error, int(v(1), int64) == want_val(src, 1_int64), "a sorted row brings its own values")
            if (allocated(error)) return
        end do
        ! The negative control: the two reads must not have produced the same order.
        call check(error, plain%length(1_int64) /= sorted%length(1_int64) .or. &
            plain%length(2_int64) /= sorted%length(2_int64), "the sorted read must differ from the plain one")
    end subroutine test_sorted_read

    !> A sampled read keeps a random SUBSET of whole rows: every row that survives must arrive
    !> intact, with a length and values that belong together. Seeded, so the subset is fixed.
    subroutine test_sampled_read(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i, e, base

        call parquet_open_reader(r, WIDTHS, sample_fraction=0.5_real64, sample_seed=12345_int64)
        call parquet_read_column(r, "ragged", lc)
        call parquet_close_reader(r)

        call check(error, lc%size() > 0_int64, "a half sample of 16 rows keeps at least one")
        if (allocated(error)) return
        call check(error, lc%size() < 16_int64, "a half sample of 16 rows does not keep them all")
        if (allocated(error)) return
        do i = 1_int64, lc%size()
            row = lc%view(i)
            call row%get(v)
            call check(error, size(v, kind=int64) == lc%length(i), "a sampled row's length matches its values")
            if (allocated(error)) return
            ! Every element of one row shares the same hundreds digit, whichever file row it came
            ! from -- so this catches a row assembled out of two different source rows' elements.
            base = (int(v(1), int64)/100_int64)*100_int64
            do e = 1_int64, size(v, kind=int64)
                call check(error, int(v(e), int64) == base + e - 1_int64, "a sampled row's elements are its own")
                if (allocated(error)) return
            end do
        end do
    end subroutine test_sampled_read

    !> Reading row group by row group must reproduce the whole-column read exactly. list_widths has
    !> 4 row groups of 4 rows and `ragged`'s lengths are 1,2,3,4 within each, so a chunk that
    !> ignored its row-group argument and read the whole column would be caught on the first one.
    subroutine test_chunked_agrees(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: whole, chunk
        type(parquet_list_row) :: rw, rc
        integer(int32), allocatable :: vw(:), vc(:)
        integer(int64) :: rg, i, at, ngroups

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "ragged", whole)
        call parquet_get_num_row_groups(r, ngroups)
        call check(error, ngroups == 4_int64, "list_widths has 4 row groups")
        if (allocated(error)) return
        at = 0_int64
        do rg = 1_int64, ngroups
            call parquet_read_column_chunk(r, "ragged", rg, chunk)
            call check(error, chunk%size() == 4_int64, "each row group holds 4 rows")
            if (allocated(error)) return
            do i = 1_int64, chunk%size()
                at = at + 1_int64
                call check(error, chunk%length(i) == whole%length(at), "a chunk row has the whole read's length")
                if (allocated(error)) return
                rw = whole%view(at)
                rc = chunk%view(i)
                call rw%get(vw)
                call rc%get(vc)
                call check(error, all(vw == vc), "a chunk row has the whole read's values")
                if (allocated(error)) return
            end do
        end do
        call check(error, at == 16_int64, "the four chunks account for every row")
        call parquet_close_reader(r)
    end subroutine test_chunked_agrees

    !> Uniformity is a property of the data, not of the request: a plain LIST column whose rows all
    !> happen to hold 3 elements reads into a list column like any other. Refusing it would make
    !> every caller write a fallback for a distinction they did not ask about.
    subroutine test_uniform_into_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i

        call parquet_open_reader(r, WIDTHS)
        call parquet_read_column(r, "uniform", lc)
        call parquet_close_reader(r)

        call check(error, lc%size() == 16_int64, "uniform reads 16 rows into a list column")
        if (allocated(error)) return
        do i = 1_int64, 16_int64
            call check(error, lc%length(i) == 3_int64, "every row of uniform holds 3 elements")
            if (allocated(error)) return
        end do
        row = lc%view(5_int64)
        call row%get(v)
        call check(error, int(v(2), int64) == want_val(5_int64, 2_int64), "and the values are the file's own")
    end subroutine test_uniform_into_list

    !> The other half of the same rule, and the one D7 protects: `uniform` must go on reading as a
    !> width-3 vector column into a 2-D array, exactly as it did before list columns existed. The
    !> caller's chosen output type is what picks the interpretation.
    subroutine test_uniform_still_vector(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        integer(int32) :: v(3, 16)
        integer :: col_size

        call parquet_open_reader(r, WIDTHS)
        call parquet_get_col_size(r, "uniform", col_size)
        call check(error, col_size == 3, "uniform still measures as a width-3 vector column")
        if (allocated(error)) return
        call parquet_read_column(r, "uniform", v)
        call parquet_close_reader(r)
        call check(error, int(v(2, 5), int64) == want_val(5_int64, 2_int64), "and still reads into a 2-D array")
    end subroutine test_uniform_still_vector

    !> A fixed-size list (this library's own vector-column layout) is a list whose rows all have
    !> the same length, so it reads into a list column too -- there is no third rule about which
    !> representation is allowed when.
    subroutine test_vector_into_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_writer) :: w
        type(parquet_schema) :: schema
        character(len=*), parameter :: f = "test_run/list_read_vector_into_list.parquet"
        integer(int32) :: v(2, 3)
        integer(int64) :: i

        v = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        call schema%init("vec_fixture")
        call schema%add_field("vec", "int32", col_size=2)
        call parquet_parse_maml(schema)
        call parquet_open_writer(w, f, schema)
        call parquet_write_column(w, "vec", v)
        call parquet_close_writer(w)

        call parquet_open_reader(r, f)
        call parquet_read_column(r, "vec", lc)
        call parquet_close_reader(r)

        call check(error, lc%size() == 3_int64, "the vector column reads 3 rows")
        if (allocated(error)) return
        call check(error, lc%element_kind() == PK_INT32, "its payload is int32")
        if (allocated(error)) return
        do i = 1_int64, 3_int64
            call check(error, lc%length(i) == 2_int64, "every row of a width-2 vector holds 2 elements")
            if (allocated(error)) return
        end do
    end subroutine test_vector_into_list

    !> A LIST leaf under a STRUCT used to give three answers that contradicted each other: listed
    !> by parquet_get_column_names, `.false.` from parquet_column_exists, and an ABORT from
    !> parquet_get_column_type saying "column not found" about a path the same reader had listed.
    !> All three now agree, and the column reads.
    subroutine test_struct_nested_list(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        character(len=:), allocatable :: names(:), type_name, shape
        integer(int32), allocatable :: v(:)
        integer(int64) :: i
        logical :: listed

        call parquet_open_reader(r, WIDTHS)
        call parquet_get_column_names(r, names)
        listed = .false.
        do i = 1_int64, size(names, kind=int64)
            if (trim(names(i)) == "nested.vals") listed = .true.
        end do
        call check(error, listed, "parquet_get_column_names lists nested.vals")
        if (allocated(error)) return
        call check(error, parquet_column_exists(r, "nested.vals"), "and parquet_column_exists agrees")
        if (allocated(error)) return
        call parquet_get_column_type(r, "nested.vals", type_name)
        call check(error, type_name == "int32", "and parquet_get_column_type answers rather than aborting")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "nested.vals", shape)
        call check(error, shape == "list", "and the shape query calls it a list")
        if (allocated(error)) return

        call parquet_read_column(r, "nested.vals", lc)
        call parquet_close_reader(r)
        call check(error, lc%size() == 16_int64, "nested.vals reads 16 rows")
        if (allocated(error)) return
        do i = 1_int64, 16_int64
            call check(error, lc%length(i) == 3_int64, "nested.vals holds 3 elements per row")
            if (allocated(error)) return
        end do
        row = lc%view(5_int64)
        call row%get(v)
        call check(error, int(v(2), int64) == want_val(5_int64, 2_int64), "with the file's own values")
    end subroutine test_struct_nested_list

    !> parquet_get_column_shape answers the container question, which parquet_get_column_type
    !> deliberately does not. Note `uniform`: a plain LIST answers "list" even though its data is
    !> uniform and it reads perfectly well as a width-3 vector -- the schema declares a list and
    !> says nothing about the lengths.
    subroutine test_column_shape(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_writer) :: w
        type(parquet_schema) :: schema
        character(len=*), parameter :: f = "test_run/list_read_column_shape.parquet"
        character(len=:), allocatable :: shape
        integer(int32) :: v(2, 3), s(3)

        call parquet_open_reader(r, WIDTHS)
        call parquet_get_column_shape(r, "scalar", shape)
        call check(error, shape == "scalar", "an ordinary int32 column is a scalar")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "ragged", shape)
        call check(error, shape == "list", "a ragged LIST column is a list")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "uniform", shape)
        call check(error, shape == "list", "a UNIFORM plain LIST column is still a list")
        if (allocated(error)) return
        call parquet_close_reader(r)

        v = reshape([1_int32, 2_int32, 3_int32, 4_int32, 5_int32, 6_int32], [2, 3])
        s = [7_int32, 8_int32, 9_int32]
        call schema%init("shape_fixture")
        call schema%add_field("vec", "int32", col_size=2)
        call schema%add_field("plain", "int32")
        call parquet_parse_maml(schema)
        call parquet_open_writer(w, f, schema)
        call parquet_write_column(w, "vec", v)
        call parquet_write_column(w, "plain", s)
        call parquet_close_writer(w)

        call parquet_open_reader(r, f)
        call parquet_get_column_shape(r, "vec", shape)
        call check(error, shape == "vector", "a fixed-size list column is a vector")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "plain", shape)
        call check(error, shape == "scalar", "and a scalar column beside it is a scalar")
        call parquet_close_reader(r)
    end subroutine test_column_shape

    !> D7's promise, asserted directly: parquet_get_column_type still unwraps a list to its
    !> ELEMENT type, which is what makes the pair (type, shape) a complete description.
    subroutine test_column_type_unchanged(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        character(len=:), allocatable :: t

        call parquet_open_reader(r, PAYLOADS)
        call parquet_get_column_type(r, "i32", t)
        call check(error, t == "int32", "a list<int32> still reports int32")
        if (allocated(error)) return
        call parquet_get_column_type(r, "f64", t)
        call check(error, t == "float64", "a list<double> still reports float64")
        if (allocated(error)) return
        call parquet_get_column_type(r, "text", t)
        call check(error, t == "string", "a list<string> still reports string")
        if (allocated(error)) return
        call parquet_get_column_type(r, "wide32", t)
        call check(error, t == "int64", "a list<uint32> reports the narrowest lossless kind")
        call parquet_close_reader(r)
    end subroutine test_column_type_unchanged

    !> The read REPLACES the destination rather than appending to it, so reading twice into the
    !> same variable -- or into one that already held a different column -- yields the column that
    !> was asked for and nothing else.
    subroutine test_read_twice_replaces(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        type(parquet_list_column), target :: lc

        call parquet_open_reader(r, PAYLOADS)
        call parquet_read_column(r, "i32", lc)
        call check(error, lc%size() == 12_int64, "the first read yields 12 rows")
        if (allocated(error)) return
        call parquet_read_column(r, "i32", lc)
        call check(error, lc%size() == 12_int64, "and the second yields 12, not 24")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 30_int64, "with 30 elements, not 60")
        if (allocated(error)) return
        ! And a read into a column that already held a DIFFERENT payload kind must retype it.
        call parquet_read_column(r, "f64", lc)
        call check(error, lc%element_kind() == PK_FLOAT64, "reading a float64 column retypes the destination")
        call parquet_close_reader(r)
    end subroutine test_read_twice_replaces

    !> The other half of the struct-nested boundary, and the one `test_deferred_list_width` (the
    !> `table` suite) cannot show: a RAGGED list under a struct behaves exactly as a ragged list at
    !> the top level does. `parquet_table` resolves it to width 1 and the scalar kind, which is
    !> `feature_risks.md` Risk-152 -- a state a 2-D read cannot use -- and Phase 6 owns it. Pinned
    !> here so the equivalence is explicit: accepting a LIST leaf through a dotted path introduced
    !> no dotted-path-specific behaviour, good or bad.
    subroutine test_struct_nested_ragged_matches_top_level(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_table) :: t
        type(parquet_reader) :: r
        type(parquet_list_column), target :: nested_lc, top_lc
        character(len=*), parameter :: f = "test/fixtures/map_list_types.parquet"

        ! struct_of_list.values holds 3, 0 and 2 elements; list_col beside it holds 2, 0 and 3.
        call parquet_open_table(t, f)
        call check(error, t%is_supported("struct_of_list.values"), &
            "a ragged struct-nested LIST is classified like any other plain LIST")
        if (allocated(error)) return
        call check(error, t%width("struct_of_list.values") == t%width("list_col"), &
            "and resolves to the same width a ragged TOP-LEVEL list does")
        if (allocated(error)) return
        call check(error, t%width("struct_of_list.values") == 1, &
            "which is 1 -- no width covers rows of 3, 0 and 2 (feature_risks.md Risk-152)")
        if (allocated(error)) return
        call check(error, t%kind("struct_of_list.values") == t%kind("list_col"), &
            "and to the same kind")
        if (allocated(error)) return

        ! The list read is what actually gets the rows out, at either nesting depth.
        call parquet_open_reader(r, f)
        call parquet_read_column(r, "struct_of_list.values", nested_lc)
        call parquet_read_column(r, "list_col", top_lc)
        call parquet_close_reader(r)
        call check(error, nested_lc%size() == 3_int64, "the nested list reads 3 rows")
        if (allocated(error)) return
        call check(error, nested_lc%length(1_int64) == 3_int64 .and. &
            nested_lc%length(2_int64) == 0_int64 .and. nested_lc%length(3_int64) == 2_int64, &
            "with its own 3, 0, 2 lengths")
        if (allocated(error)) return
        call check(error, top_lc%length(1_int64) == 2_int64 .and. &
            top_lc%length(2_int64) == 0_int64 .and. top_lc%length(3_int64) == 3_int64, &
            "and the top-level one with its own 2, 0, 3 -- the two are not the same column")
    end subroutine test_struct_nested_ragged_matches_top_level

    !> What struct-path resolution accepts, and what it still refuses -- the control that keeps
    !> "the dotted path now resolves" from meaning "the dotted path now resolves to anything".
    !!
    !! **This test was inverted in Phase 7 and its history is worth knowing.** It used to assert
    !! that a MAP under a struct did NOT resolve, with the comment *"and must stay that way"* --
    !! correct while nothing could receive one, and wrong the moment `parquet_map_column` could.
    !! `collect_column_leaf_paths` has always LISTED `struct_of_map.attrs`, so refusing to resolve
    !! it made `parquet_get_column_names` advertise a name that then failed: the same "listing that
    !! lies" T6 had Phase 2 fix for lists, left open for maps. See feature_container_phase7.md's
    !! M4/D5/Q3.
    !!
    !! **The refusal that REMAINS is what makes this a control**: an intermediate STRUCT still does
    !! not resolve, because a dotted path names a LEAF by this library's long-standing convention
    !! and lifting that would change what a column-iterating caller sees. If a later phase lifts it
    !! too, this becomes an equality test against a struct column rather than a deletion -- what it
    !! asserts today is that the two leaf rules are genuinely different, not that maps are special.
    subroutine test_map_stays_unreadable(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error.
        type(parquet_reader) :: r
        character(len=*), parameter :: f = "test/fixtures/map_list_types.parquet"
        character(len=:), allocatable :: shape, t

        call parquet_open_reader(r, f)
        ! A top-level MAP resolves as a name (it is a real top-level field) but is not readable,
        ! and both queries say so rather than aborting.
        call parquet_get_column_shape(r, "map_col", shape)
        call check(error, shape == "map", "a MAP column reports its shape")
        if (allocated(error)) return
        call parquet_get_column_type(r, "map_col", t)
        call check(error, t == "unknown", "and reports an unknown element type")
        if (allocated(error)) return
        ! A MAP under a STRUCT now resolves and reads, exactly as the LIST beside it does.
        call check(error, parquet_column_exists(r, "struct_of_map.attrs"), &
            "a MAP under a struct resolves")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "struct_of_map.attrs", shape)
        call check(error, shape == "map", "and reports itself as a map")
        if (allocated(error)) return
        ! ... and its TYPE query still answers "unknown", which is the frozen contract: a container
        ! is not a leaf kind, so widening resolution must not have widened that.
        call parquet_get_column_type(r, "struct_of_map.attrs", t)
        call check(error, t == "unknown", "while its element type stays unknown")
        if (allocated(error)) return
        ! The LIST under a struct in the same file must too, which is what makes the pair evidence
        ! rather than a claim that everything now resolves.
        call check(error, parquet_column_exists(r, "struct_of_list.values"), &
            "a LIST under a struct must resolve")
        if (allocated(error)) return
        call parquet_get_column_shape(r, "struct_of_list.values", shape)
        call check(error, shape == "list", "and report itself as a list")
        if (allocated(error)) return
        ! THE SURVIVING REFUSAL, and the reason this test is still a control: an intermediate
        ! STRUCT does not resolve. A dotted path names a leaf; `struct_of_struct.inner.a` is how
        ! its contents are reached, at any depth.
        call check(error, .not. parquet_column_exists(r, "struct_of_struct.inner"), &
            "an intermediate struct must still not resolve")
        if (allocated(error)) return
        call check(error, parquet_column_exists(r, "struct_of_struct.inner.a"), &
            "while its own leaf does, at depth two")
        call parquet_close_reader(r)
    end subroutine test_map_stays_unreadable

end module test_list_read
