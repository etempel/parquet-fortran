!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for `parquet_list`: the variable-length list column and its row handle.
!!
!! **What this suite is for, beyond "does it work".** A list column is the first storage in this
!! library whose rows differ in length, so most of its failure modes are off-by-one errors in the
!! offsets that still produce a perfectly plausible answer. The tests are therefore written so
!! that no two rows have the same length and no two elements have the same value wherever that is
!! affordable: a fixture whose rows are all the same width, or whose values repeat, cannot
!! distinguish a correct read from one that is shifted by a row (see CLAUDE.md's note on
!! self-comparisons and on "sized/typed from the first element" regressions).
!!
!! Row nullness and element nullness are asserted SEPARATELY throughout. They are different
!! properties with different storage, and a test that only ever checks one of them would pass
!! against an implementation that had collapsed the two.
module test_list
    use testdrive, only : new_unittest, unittest_type, error_type, check, skip_test
    use parquet_list
    use parquet_columns, only : parquet_column_is_null
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_list

contains

    !> Registers every test in this suite.
    subroutine collect_tests_parquet_list(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the suite's tests.

        testsuite = [ &
            new_unittest("a fresh list column is empty and has no payload kind", test_fresh_is_empty), &
            new_unittest("ragged rows keep their own lengths and values", test_ragged_rows), &
            new_unittest("a null row is not an empty row", test_null_row_vs_empty_row), &
            new_unittest("element nulls do not make the row null", test_element_nulls), &
            new_unittest("every payload kind round-trips", test_every_payload_kind), &
            new_unittest("a string payload keeps per-row lengths", test_string_payload), &
            new_unittest("append_null_row and grow_rows agree", test_grow_rows), &
            new_unittest("set_null and clear_null move one row only", test_set_and_clear_null), &
            new_unittest("deep_copy is independent of its source", test_deep_copy_is_independent), &
            new_unittest("move_from leaves the source empty", test_move_from), &
            new_unittest("gather_rows reorders, repeats and drops rows", test_gather_rows), &
            new_unittest("append_from concatenates two list columns and rebases the offsets", &
                test_append_from), &
            new_unittest("set_null drops a row's elements at once and a rebuild agrees", &
                test_gather_drops_nulled), &
            new_unittest("growth is geometric and shrink_to_fit gives it back", test_capacity_policy), &
            new_unittest("reserve does not add rows", test_reserve_adds_no_rows), &
            new_unittest("ensure_validity materializes both levels", test_ensure_validity), &
            new_unittest("a list column adopts into a parquet_column", test_adopt_container), &
            new_unittest("a parquet_column deep-copies a container kind", test_column_deep_copy), &
            new_unittest("append_nulls on an adopted column adds null rows", test_column_append_nulls), &
            new_unittest("a handle stays valid across appends", test_handle_survives_append), &
            new_unittest("an unassigned handle reports itself invalid", test_unassigned_handle), &
            new_unittest("kind_text and summary describe the column", test_kind_text_and_summary), &
            new_unittest("validate accepts a well-formed column", test_validate) ]
    end subroutine collect_tests_parquet_list

    !> A default-initialized column has no rows, no payload kind and no elements.
    subroutine test_fresh_is_empty(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc

        call check(error, lc%size() == 0_int64, "a fresh list column has no rows")
        if (allocated(error)) return
        call check(error, lc%element_kind() == PK_NONE, "a fresh list column has no payload kind")
        if (allocated(error)) return
        call check(error, .not. lc%is_init(), "a fresh list column reports itself uninitialized")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 0_int64, "a fresh list column has no elements")
        if (allocated(error)) return
        call check(error, lc%null_count() == 0_int64, "a fresh list column has no null rows")
        if (allocated(error)) return
        ! %init must answer for an EMPTY column too -- that is one of the reasons the payload kind
        ! is a required argument rather than inferred from the first row appended (Q2).
        call lc%init(PK_FLOAT64)
        call check(error, lc%element_kind() == PK_FLOAT64, "%element_kind answers before any row exists")
        if (allocated(error)) return
        call check(error, lc%size() == 0_int64, "%init with no nrows creates no rows")
    end subroutine test_fresh_is_empty

    !> Rows of three different lengths, with values that are distinct across the whole column.
    subroutine test_ragged_rows(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)

        call lc%init(PK_INT32)
        call lc%append_row([11_int32, 12_int32, 13_int32])
        call lc%append_row([21_int32])
        call lc%append_row([31_int32, 32_int32])
        call check(error, lc%size() == 3_int64, "three appends make three rows")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 6_int64, "3 + 1 + 2 elements were stored")
        if (allocated(error)) return
        call check(error, lc%length(1_int64) == 3_int64, "row 1 is three elements long")
        if (allocated(error)) return
        call check(error, lc%length(2_int64) == 1_int64, "row 2 is one element long")
        if (allocated(error)) return
        call check(error, lc%length(3_int64) == 2_int64, "row 3 is two elements long")
        if (allocated(error)) return
        ! The int32 index form must reach the same rows as the int64 one -- CLAUDE.md's dual-kind
        ! rule is only useful if both specifics are actually wired to the same storage.
        call check(error, lc%length(2_int32) == 1_int64, "the int32 index form agrees with the int64 one")
        if (allocated(error)) return

        row = lc%view(1_int64)
        call row%get(v)
        call check(error, size(v) == 3, "row 1 reads back three values")
        if (allocated(error)) return
        call check(error, all(v == [11_int32, 12_int32, 13_int32]), "row 1 reads back its own values")
        if (allocated(error)) return
        row = lc%view(3_int64)
        call row%get(v)
        call check(error, all(v == [31_int32, 32_int32]), "row 3 reads back its own values, not row 2's")
        if (allocated(error)) return
        ! The middle row is the one an off-by-one in either direction lands on.
        row = lc%view(2_int32)
        call row%get(v)
        call check(error, size(v) == 1 .and. v(1) == 21_int32, "row 2 reads back exactly its one value")
    end subroutine test_ragged_rows

    !> A null row and a genuinely zero-length row are different states, and both are representable.
    subroutine test_null_row_vs_empty_row(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int32), allocatable :: nothing(:)

        allocate(nothing(0))
        call lc%init(PK_INT32)
        call lc%append_null_row()          ! row 1: the list is ABSENT
        call lc%append_row(nothing)        ! row 2: the list is PRESENT and has length zero
        call lc%append_row([5_int32])      ! row 3: an ordinary row, to prove neither shifted it

        call check(error, lc%is_null(1_int64), "an appended null row is null")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(2_int64), "a zero-length row is NOT null")
        if (allocated(error)) return
        call check(error, lc%is_empty(1_int64), "a null row is empty")
        if (allocated(error)) return
        call check(error, lc%is_empty(2_int64), "a zero-length row is empty")
        if (allocated(error)) return
        call check(error, .not. lc%is_empty(3_int64), "a one-element row is not empty")
        if (allocated(error)) return
        call check(error, lc%null_count() == 1_int64, "exactly one row is null")
        if (allocated(error)) return

        ! Both read back as a zero-size array; only %is_null tells them apart, which is precisely
        ! why %is_empty alone is not enough and both queries exist.
        row = lc%view(1_int64)
        call row%get(v)
        call check(error, size(v) == 0, "a null row yields a zero-size result rather than aborting")
        if (allocated(error)) return
        call check(error, row%is_null(), "the handle agrees the row is null")
        if (allocated(error)) return
        row = lc%view(2_int64)
        call row%get(v)
        call check(error, size(v) == 0, "a zero-length row yields a zero-size result")
        if (allocated(error)) return
        call check(error, .not. row%is_null(), "the handle agrees a zero-length row is not null")
        if (allocated(error)) return
        row = lc%view(3_int64)
        call row%get(v)
        call check(error, size(v) == 1 .and. v(1) == 5_int32, "the row after two empty ones is intact")
    end subroutine test_null_row_vs_empty_row

    !> A row containing a null ELEMENT is not a null ROW. The two live in different storage and a
    !! collapse of one into the other is the failure this test exists to catch.
    subroutine test_element_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        logical, allocatable :: ok(:)

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32, 3_int32], is_valid=[.true., .false., .true.])
        call lc%append_row([4_int32, 5_int32])

        call check(error, .not. lc%is_null(1_int64), "a row with a null element is not a null row")
        if (allocated(error)) return
        call check(error, .not. lc%is_empty(1_int64), "a row with a null element is not empty")
        if (allocated(error)) return
        call check(error, lc%length(1_int64) == 3_int64, "a null element still occupies a slot")
        if (allocated(error)) return
        call check(error, lc%null_count() == 0_int64, "a null element does not count as a null row")
        if (allocated(error)) return

        row = lc%view(1_int64)
        call row%get(v, is_valid=ok)
        call check(error, size(ok) == 3, "the mask has one entry per element")
        if (allocated(error)) return
        call check(error, all(ok .eqv. [.true., .false., .true.]), "the mask names the element that is null")
        if (allocated(error)) return
        ! The next row must be unaffected: an element-null bit written at the wrong flat offset
        ! lands in a neighbouring row and nothing else would report it.
        row = lc%view(2_int64)
        call row%get(v, is_valid=ok)
        call check(error, all(ok), "the following row has no null elements")
        if (allocated(error)) return
        call check(error, all(v == [4_int32, 5_int32]), "the following row's values are intact")
    end subroutine test_element_nulls

    !> One ragged fixture per payload kind, so that no kind is covered only by its neighbour.
    subroutine test_every_payload_kind(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int64), allocatable :: v64(:)
        real(real32), allocatable :: r32(:)
        real(real64), allocatable :: r64(:)
        logical, allocatable :: lv(:)
        type(parquet_date), allocatable :: dv(:)
        type(parquet_time), allocatable :: tv(:)
        type(parquet_timestamp), allocatable :: sv(:)
        type(parquet_date) :: d1, d2
        type(parquet_time) :: t1
        type(parquet_timestamp) :: s1
        integer :: yy, mm, dd, hh, mi, ss

        call lc%init(PK_INT64)
        call lc%append_row([100000000000_int64, 200000000000_int64])
        call lc%append_row([300000000000_int64])
        row = lc%view(1_int64); call row%get(v64)
        call check(error, all(v64 == [100000000000_int64, 200000000000_int64]), "int64 payload round-trips")
        if (allocated(error)) return

        call lc%init(PK_FLOAT32)
        call lc%append_row([1.5_real32])
        call lc%append_row([2.5_real32, 3.5_real32])
        row = lc%view(2_int64); call row%get(r32)
        call check(error, all(r32 == [2.5_real32, 3.5_real32]), "float32 payload round-trips")
        if (allocated(error)) return

        call lc%init(PK_FLOAT64)
        call lc%append_row([1.25_real64, 2.25_real64, 3.25_real64])
        row = lc%view(1_int64); call row%get(r64)
        call check(error, all(r64 == [1.25_real64, 2.25_real64, 3.25_real64]), "float64 payload round-trips")
        if (allocated(error)) return

        call lc%init(PK_LOGICAL)
        call lc%append_row([.true., .false., .true.])
        row = lc%view(1_int64); call row%get(lv)
        call check(error, all(lv .eqv. [.true., .false., .true.]), "logical payload round-trips")
        if (allocated(error)) return

        call d1%set(2024, 2, 29)
        call d2%set(2025, 12, 31)
        call lc%init(PK_DATE)
        call lc%append_row([d1, d2])
        row = lc%view(1_int64); call row%get(dv)
        call check(error, size(dv) == 2, "date payload keeps both elements")
        if (allocated(error)) return
        call check(error, dv(1)%year() == 2024 .and. dv(1)%month() == 2 .and. dv(1)%day() == 29, &
            "date payload round-trips the leap day")
        if (allocated(error)) return
        call check(error, dv(2)%year() == 2025, "date payload round-trips the second element")
        if (allocated(error)) return

        call t1%set(13, 45, 6)
        call lc%init(PK_TIME)
        call lc%append_row([t1])
        row = lc%view(1_int64); call row%get(tv)
        call check(error, tv(1)%hour() == 13 .and. tv(1)%minute() == 45, "time payload round-trips")
        if (allocated(error)) return

        call s1%set(2030, 6, 1, 8, 9, 10)
        call lc%init(PK_TIMESTAMP)
        call lc%append_row([s1])
        row = lc%view(1_int64); call row%get(sv)
        call sv(1)%get(yy, mm, dd, hh, mi, ss)
        call check(error, yy == 2030 .and. mm == 6 .and. hh == 8 .and. ss == 10, &
            "timestamp payload round-trips")
    end subroutine test_every_payload_kind

    !> A string payload: rows of different lengths, holding strings of different lengths, with the
    !! SHORTEST string first -- the fixture shape CLAUDE.md prescribes for this bug class.
    subroutine test_string_payload(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        character(len=:), allocatable :: sv(:)

        call lc%init(PK_STRING)
        call lc%append_row(["a    ", "bcd  ", "efghi"])
        call lc%append_row(["zz"])
        call check(error, lc%total_elements() == 4_int64, "four strings were stored")
        if (allocated(error)) return

        row = lc%view(1_int64)
        call row%get(sv)
        call check(error, size(sv) == 3, "row 1 reads back three strings")
        if (allocated(error)) return
        ! Trailing blanks are trimmed on the way in, as they are for every character ARRAY entering
        ! a column -- so the result is sized to the longest REAL value, not to the declared width.
        call check(error, len(sv) == 5, "the result is as wide as the longest value in this row")
        if (allocated(error)) return
        call check(error, trim(sv(1)) == "a", "the shortest string, appended first, is not truncated")
        if (allocated(error)) return
        call check(error, trim(sv(2)) == "bcd", "the middle string round-trips")
        if (allocated(error)) return
        call check(error, trim(sv(3)) == "efghi", "the longest string round-trips")
        if (allocated(error)) return

        ! The second row must be sized from ITS OWN longest value, not from the column's.
        row = lc%view(2_int64)
        call row%get(sv)
        call check(error, size(sv) == 1, "row 2 reads back one string")
        if (allocated(error)) return
        call check(error, len(sv) == 2, "row 2 is sized from its own longest value")
        if (allocated(error)) return
        call check(error, trim(sv(1)) == "zz", "row 2's value round-trips")
    end subroutine test_string_payload

    !> `%grow_rows` is the deferred face's append-null-rows; it must agree with the public form.
    subroutine test_grow_rows(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: a, b
        integer(int64) :: i

        call a%init(PK_INT32)
        call b%init(PK_INT32)
        call a%grow_rows(3_int64)
        do i = 1_int64, 3_int64
            call b%append_null_row()
        end do
        call check(error, a%size() == b%size(), "grow_rows adds as many rows as the loop does")
        if (allocated(error)) return
        call check(error, a%null_count() == 3_int64, "every row grow_rows added is null")
        if (allocated(error)) return
        call check(error, a%total_elements() == 0_int64, "a null row holds no elements")
        if (allocated(error)) return
        ! %init's own nrows argument goes through the same path.
        call b%init(PK_INT32, 4_int64)
        call check(error, b%size() == 4_int64 .and. b%null_count() == 4_int64, &
            "%init(kind, nrows) creates that many null rows")
    end subroutine test_grow_rows

    !> `%set_null` marks exactly one row and drops that row's elements; `%clear_null` unmarks one
    !! row and brings it back EMPTY.
    !!
    !! The element drop is the invariant Arrow's Parquet writer requires -- a null list slot
    !! spanning elements is refused outright -- so `%total_elements` falling by the row's former
    !! length is asserted here rather than left implicit. The row AFTER the nulled one is read back
    !! in full: its payload elements moved down, and only its OFFSETS moving with them keeps it
    !! readable.
    subroutine test_set_and_clear_null(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)

        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%append_row([2_int32, 3_int32])
        call lc%append_row([4_int32, 5_int32, 6_int32])
        call check(error, .not. lc%has_validity_storage(), "a null-free column allocates no bitmap")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 6_int64, "six elements before anything is nulled")
        if (allocated(error)) return

        call lc%set_null(2_int64)
        call check(error, lc%is_null(2_int64), "the named row became null")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(1_int64) .and. .not. lc%is_null(3_int64), &
            "no neighbouring row became null")
        if (allocated(error)) return
        call check(error, lc%length(2_int64) == 0_int64, "a nulled row reports length 0")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 4_int64, &
            "the nulled row's two elements were dropped, not merely hidden")
        if (allocated(error)) return
        ! Row 3's elements moved down by two; its offsets moved with them, so it still reads back
        ! as itself. A drop that forgot the offsets tail would return row 2's old values here.
        row = lc%view(3_int64)
        call row%get(v)
        call check(error, size(v) == 3, "the row after a nulled one still holds three elements")
        if (allocated(error)) return
        call check(error, v(1) == 4_int32 .and. v(2) == 5_int32 .and. v(3) == 6_int32, &
            "and they are its own values, not the dropped row's")
        if (allocated(error)) return
        ! Row 1 is before the nulled row, so nothing about it may have moved at all.
        row = lc%view(1_int64)
        call row%get(v)
        call check(error, size(v) == 1 .and. v(1) == 1_int32, "the row before a nulled one is untouched")
        if (allocated(error)) return

        call lc%clear_null(2_int64)
        call check(error, .not. lc%is_null(2_int64), "clear_null makes the row present again")
        if (allocated(error)) return
        call check(error, lc%length(2_int64) == 0_int64, &
            "and it comes back EMPTY -- clear_null is one bit, not an undo")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 4_int64, "clearing the bit restores no element")
    end subroutine test_set_and_clear_null

    !> A deep copy shares nothing with its source: mutating either must not move the other.
    subroutine test_deep_copy_is_independent(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: src
        type(parquet_list_column), target :: cp
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)

        call src%init(PK_INT32)
        call src%append_row([1_int32, 2_int32])
        call src%append_null_row()
        call src%append_row([3_int32])
        call src%deep_copy(cp)

        call check(error, cp%size() == 3_int64, "the copy has the same row count")
        if (allocated(error)) return
        call check(error, cp%total_elements() == 3_int64, "the copy has the same element count")
        if (allocated(error)) return
        call check(error, cp%is_null(2_int64), "the copy carries the row-null bit")
        if (allocated(error)) return
        call check(error, cp%element_kind() == PK_INT32, "the copy carries the payload kind")
        if (allocated(error)) return

        ! Grow the SOURCE and check the copy did not follow.
        call src%append_row([9_int32, 9_int32, 9_int32])
        call check(error, cp%size() == 3_int64, "growing the source did not grow the copy")
        if (allocated(error)) return
        ! Null a row of the COPY and check the source did not follow.
        call cp%set_null(1_int64)
        call check(error, .not. src%is_null(1_int64), "nulling the copy did not null the source")
        if (allocated(error)) return
        row = src%view(1_int64)
        call row%get(v)
        call check(error, all(v == [1_int32, 2_int32]), "the source's values survived")
    end subroutine test_deep_copy_is_independent

    !> `%move_from` hands the storage over and leaves the source as a fresh column.
    subroutine test_move_from(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: src, dst

        call src%init(PK_FLOAT64)
        call src%append_row([1.0_real64, 2.0_real64])
        call src%append_null_row()
        call dst%move_from(src)

        call check(error, dst%size() == 2_int64, "the destination took the rows")
        if (allocated(error)) return
        call check(error, dst%element_kind() == PK_FLOAT64, "the destination took the payload kind")
        if (allocated(error)) return
        call check(error, dst%null_count() == 1_int64, "the destination took the row bitmap")
        if (allocated(error)) return
        call check(error, dst%total_elements() == 2_int64, "the destination took the payload")
        if (allocated(error)) return
        call check(error, src%size() == 0_int64, "the source has no rows left")
        if (allocated(error)) return
        call check(error, src%element_kind() == PK_NONE, "the source has no payload kind left")
    end subroutine test_move_from

    !> `%gather_rows` is what every row-structural mutation is built on: it must reorder, repeat
    !! and drop rows, carrying each row's own length and values with it.
    subroutine test_gather_rows(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)

        call lc%init(PK_INT32)
        call lc%append_row([11_int32])
        call lc%append_row([21_int32, 22_int32])
        call lc%append_null_row()
        call lc%append_row([41_int32, 42_int32, 43_int32])

        ! Reverse, which moves every row and is the permutation an off-by-one cannot survive.
        call lc%gather_rows([4_int64, 3_int64, 2_int64, 1_int64])
        call check(error, lc%size() == 4_int64, "the reversal kept every row")
        if (allocated(error)) return
        call check(error, lc%length(1_int64) == 3_int64, "the old last row is now first, with its length")
        if (allocated(error)) return
        call check(error, lc%is_null(2_int64), "the null row moved with the rest")
        if (allocated(error)) return
        call check(error, lc%length(4_int64) == 1_int64, "the old first row is now last")
        if (allocated(error)) return
        row = lc%view(1_int64)
        call row%get(v)
        call check(error, all(v == [41_int32, 42_int32, 43_int32]), "the moved row kept its own values")
        if (allocated(error)) return
        call check(error, lc%validate(), "the reversed column still satisfies its invariants")
        if (allocated(error)) return

        ! Repeat and drop: a gather list may be any length and may name a row twice.
        call lc%gather_rows([1_int64, 1_int64])
        call check(error, lc%size() == 2_int64, "the gather list's length is the new row count")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 6_int64, "a repeated row's elements are stored twice")
        if (allocated(error)) return
        call check(error, lc%null_count() == 0_int64, "the null row was dropped by the gather")
        if (allocated(error)) return
        row = lc%view(2_int64)
        call row%get(v)
        call check(error, all(v == [41_int32, 42_int32, 43_int32]), "the repeated copy holds the same values")
    end subroutine test_gather_rows

    !> `%set_null` drops the row's elements at once, and a later rebuild finds nothing left to
    !! drop. Nulling the FIRST row is the case that catches an offsets tail left unshifted: every
    !! remaining row is after it, so the damage is maximal and `%validate` sees it.
    !!
    !! Both halves are worth asserting. Dropping too LATE is what made Arrow's Parquet writer
    !! refuse the column; dropping without shifting the offsets would corrupt every later row.
    subroutine test_gather_drops_nulled(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_row([3_int32, 4_int32, 5_int32])
        call lc%set_null(1_int64)
        call check(error, lc%total_elements() == 3_int64, &
            "nulling a row removes its elements from the payload straight away")
        if (allocated(error)) return
        call check(error, lc%length(2_int64) == 3_int64, "the later row keeps its own length")
        if (allocated(error)) return
        call check(error, lc%validate(), "the column still satisfies its invariants after the drop")
        if (allocated(error)) return

        ! The identity gather is now a no-op as far as elements go; it used to be what performed
        ! the drop, so the two paths agreeing is the thing to pin.
        call lc%gather_rows([1_int64, 2_int64])
        call check(error, lc%size() == 2_int64, "the identity gather kept both rows")
        if (allocated(error)) return
        call check(error, lc%total_elements() == 3_int64, "the rebuild had nothing left to drop")
        if (allocated(error)) return
        call check(error, lc%is_null(1_int64), "the rebuilt row is still null")
        if (allocated(error)) return
        call check(error, lc%length(2_int64) == 3_int64, "the surviving row kept its length")
        if (allocated(error)) return
        call check(error, lc%validate(), "the rebuilt column satisfies its invariants")
    end subroutine test_gather_drops_nulled

    !> Growth is geometric, so appending row by row does not reallocate on every append; and
    !! `%shrink_to_fit` hands the slack back.
    subroutine test_capacity_policy(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc
        integer(int64) :: i, reallocations, cap

        call lc%init(PK_INT32)
        reallocations = 0_int64
        cap = lc%capacity()
        do i = 1_int64, 200_int64
            call lc%append_row([int(i, int32)])
            if (lc%capacity() /= cap) then
                reallocations = reallocations + 1_int64
                cap = lc%capacity()
            end if
        end do
        call check(error, lc%size() == 200_int64, "every append added a row")
        if (allocated(error)) return
        ! Exact-fit growth would reallocate 200 times; 1.5x growth from 8 needs about 10.
        call check(error, reallocations < 20_int64, "growth is geometric, not exact-fit")
        if (allocated(error)) return
        call check(error, lc%capacity() >= lc%size(), "capacity never falls below the row count")
        if (allocated(error)) return
        call check(error, lc%capacity() > lc%size(), "a geometrically grown column carries slack")
        if (allocated(error)) return

        call lc%shrink_to_fit()
        call check(error, lc%capacity() == lc%size(), "shrink_to_fit releases the slack")
        if (allocated(error)) return
        call check(error, lc%validate(), "the shrunk column satisfies its invariants")
        if (allocated(error)) return
        call check(error, lc%length(200_int64) == 1_int64, "the last row survived the shrink")
    end subroutine test_capacity_policy

    !> `%reserve` grows capacity and adds no rows -- the distinction `grow_rows` exists beside.
    subroutine test_reserve_adds_no_rows(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc

        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%reserve(1000_int64)
        call check(error, lc%capacity() >= 1000_int64, "reserve raised the capacity")
        if (allocated(error)) return
        call check(error, lc%size() == 1_int64, "reserve added no rows")
        if (allocated(error)) return
        call lc%reserve(500_int32)
        call check(error, lc%capacity() >= 1000_int64, "a smaller reserve does not shrink the column")
        if (allocated(error)) return
        call check(error, lc%validate(), "a reserved column satisfies its invariants")
    end subroutine test_reserve_adds_no_rows

    !> `%ensure_validity` must materialize BOTH null levels: the row bitmap here, and the
    !! payload's own element bitmap. Refusing it for a container kind, or covering only one level,
    !! would leave the first null racing with a lazy allocation.
    subroutine test_ensure_validity(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc
        type(parquet_column) :: col
        class(parquet_container_column), allocatable :: cc

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call check(error, .not. lc%has_validity_storage(), "the bitmap is lazy: absent until asked for")
        if (allocated(error)) return
        call lc%ensure_validity()
        call check(error, lc%has_validity_storage(), "ensure_validity materialized the row bitmap")
        if (allocated(error)) return
        call check(error, lc%null_count() == 0_int64, "materializing the bitmap made no row null")
        if (allocated(error)) return
        call check(error, .not. lc%is_null(1_int64), "materializing the bitmap made no row null")
        if (allocated(error)) return

        ! And through a parquet_column, which is the route a table would take.
        call lc%clone_into(cc)
        call col%adopt_container(cc)
        call col%ensure_validity()
        call check(error, col%has_validity_storage(), &
            "a container column reports validity storage rather than refusing the request")
    end subroutine test_ensure_validity

    !> Adopting a list column into a `parquet_column` is the whole point of the abstract face.
    subroutine test_adopt_container(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_column) :: col
        type(parquet_list_column), allocatable :: lc
        class(parquet_container_column), allocatable :: cc

        allocate(lc)
        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_null_row()
        call move_alloc(lc, cc)
        call col%adopt_container(cc)

        call check(error, col%kindof() == PK_LIST, "the column took the container's kind")
        if (allocated(error)) return
        call check(error, col%length() == 2_int64, "the column took the container's row count")
        if (allocated(error)) return
        call check(error, col%colwidth() == 1, "a container column has width 1, not a stride")
        if (allocated(error)) return
        call check(error, .not. allocated(cc), "adopt MOVED the container rather than copying it")
    end subroutine test_adopt_container

    !> `parquet_column%deep_copy` must fork to `clone_into` for a container kind -- routing it
    !! through `init` would abort on the DESTINATION, since `init` still refuses containers.
    subroutine test_column_deep_copy(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_column) :: col, cp
        type(parquet_list_column) :: lc
        class(parquet_container_column), allocatable :: cc

        call lc%init(PK_INT64)
        call lc%append_row([7_int64, 8_int64, 9_int64])
        call lc%append_row([10_int64])
        call lc%clone_into(cc)
        call col%adopt_container(cc)

        call col%deep_copy(cp)
        call check(error, cp%kindof() == PK_LIST, "the copy is a container column")
        if (allocated(error)) return
        call check(error, cp%length() == 2_int64, "the copy has the same row count")
        if (allocated(error)) return
        ! Grow the source and check the copy did not follow -- an aliased container would.
        call col%append_nulls(1_int64)
        call check(error, cp%length() == 2_int64, "the copy is independent of the source")
        if (allocated(error)) return
        call check(error, col%length() == 3_int64, "the source did grow")
    end subroutine test_column_deep_copy

    !> `append_nulls` on a container column must reach the container's own null rows, and must not
    !! also write into the column's own bitmap -- two answers to "is row i null?" cannot be kept
    !! in step.
    subroutine test_column_append_nulls(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_column) :: col
        type(parquet_list_column) :: lc
        class(parquet_container_column), allocatable :: cc

        call lc%init(PK_INT32)
        call lc%append_row([1_int32])
        call lc%clone_into(cc)
        call col%adopt_container(cc)
        call check(error, col%length() == 1_int64, "the adopted column has one row")
        if (allocated(error)) return

        call col%append_nulls(3_int64)
        call check(error, col%length() == 4_int64, "append_nulls added three rows")
        if (allocated(error)) return
        call check(error, col%is_null(4_int64), "the appended rows are null")
        if (allocated(error)) return
        call check(error, .not. col%is_null(1_int64), "the original row is not null")
    end subroutine test_column_append_nulls

    !> A handle resolves lazily, so it survives an append that reallocates the offsets under it.
    subroutine test_handle_survives_append(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: lc
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)
        integer(int64) :: i

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        row = lc%view(1_int64)
        call check(error, row%is_valid(), "the handle refers to a live row")
        if (allocated(error)) return
        call check(error, row%row_index() == 1_int64, "the handle knows which row it is")
        if (allocated(error)) return
        call check(error, row%element_kind() == PK_INT32, "the handle reports the payload kind")
        if (allocated(error)) return

        ! Enough appends to force several reallocations of both the offsets and the payload.
        do i = 1_int64, 100_int64
            call lc%append_row([int(i, int32), int(i, int32)])
        end do
        call row%get(v)
        call check(error, size(v) == 2, "the handle still reads its own row after 100 appends")
        if (allocated(error)) return
        call check(error, all(v == [1_int32, 2_int32]), "the handle's row still holds its own values")
    end subroutine test_handle_survives_append

    !> A default-constructed handle reports itself invalid rather than answering plausibly.
    !!
    !! `%is_valid` is the ONLY accessor that answers on a dead handle; every other one aborts, which
    !! is why this test cannot assert anything more here. `%row_index` used to be the exception --
    !! it was `pure`, so it could not `error stop` and returned 0 -- and the abort it now raises is
    !! covered out of process by the `list_row_index_unassigned` scenario, with
    !! `list_row_index_live` as its negative control.
    subroutine test_unassigned_handle(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_row) :: row

        call check(error, .not. row%is_valid(), "an unassigned handle is not valid")
    end subroutine test_unassigned_handle

    !> `%kind_text` and `%summary` describe the column in the spelling a schema uses.
    subroutine test_kind_text_and_summary(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc
        character(len=:), allocatable :: txt

        call lc%init(PK_STRING)
        call lc%kind_text(txt)
        call check(error, txt == "list<string>", "kind_text names the payload in schema spelling")
        if (allocated(error)) return
        call lc%init(PK_TIMESTAMP)
        call lc%kind_text(txt)
        call check(error, txt == "list<timestamp>", "kind_text follows the payload kind")
        if (allocated(error)) return

        call lc%init(PK_INT32)
        call lc%append_row([1_int32, 2_int32])
        call lc%append_null_row()
        call lc%summary(txt)
        call check(error, txt == "list<int32>: 2 rows, 2 elements, 1 null", &
            "summary reports the kind, the rows, the elements and the nulls")
    end subroutine test_kind_text_and_summary

    !> `%validate` accepts a well-formed column at every stage of its life.
    subroutine test_validate(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column) :: lc
        character(len=:), allocatable :: why

        call check(error, lc%validate(why), "a fresh column is well-formed: "//why)
        if (allocated(error)) return
        call lc%init(PK_INT32)
        call check(error, lc%validate(why), "an initialized empty column is well-formed: "//why)
        if (allocated(error)) return
        call lc%append_row([1_int32, 2_int32])
        call lc%append_null_row()
        call lc%append_row([3_int32])
        call check(error, lc%validate(why), "a filled column is well-formed: "//why)
        if (allocated(error)) return
        call check(error, why == "", "a passing validate reports no diagnostic")
        if (allocated(error)) return
        call lc%clear()
        call check(error, lc%validate(why), "a cleared column is well-formed: "//why)
    end subroutine test_validate

    !> `append_from` concatenates one list column onto another -- the primitive a table's slice
    !> materialization assembles a column from its row groups with.
    !>
    !> **The assertion that matters is the one on row LENGTHS after the append**, not the row
    !> count: offsets are stored relative to the source's own element run, so an implementation
    !> that copied them instead of rebasing them onto the destination's element count produces a
    !> column with the right number of rows whose contents are shifted, overlapping or negative.
    !> A row-count check passes against all of that.
    subroutine test_append_from(error)
        type(error_type), allocatable, intent(out) :: error !! set on failure.
        type(parquet_list_column), target :: dst, src, empty
        type(parquet_list_row) :: row
        integer(int32), allocatable :: v(:)

        call dst%init(PK_INT32)
        call dst%append_row([11_int32])
        call dst%append_null_row()

        call src%init(PK_INT32)
        call src%append_row([21_int32, 22_int32])
        call src%append_row([31_int32, 32_int32, 33_int32])
        call src%append_null_row()

        call dst%append_from(src)
        call check(error, dst%size() == 5_int64, "every row of both columns survives")
        if (allocated(error)) return
        call check(error, dst%total_elements() == 6_int64, "and every element of both")
        if (allocated(error)) return
        ! Row by row, because this is what a copied-rather-than-rebased offsets array breaks.
        call check(error, dst%length(1_int64) == 1_int64, "the destination's own first row is intact")
        if (allocated(error)) return
        call check(error, dst%is_null(2_int64), "and its null row is still null")
        if (allocated(error)) return
        call check(error, dst%length(3_int64) == 2_int64, "the appended row 1 has its own length")
        if (allocated(error)) return
        call check(error, dst%length(4_int64) == 3_int64, "the appended row 2 has its own length")
        if (allocated(error)) return
        call check(error, dst%is_null(5_int64), "and the appended NULL row arrived null, not empty")
        if (allocated(error)) return
        row = dst%view(4_int64)
        call row%get(v)
        call check(error, all(v == [31_int32, 32_int32, 33_int32]), &
            "an appended row's values are its own, at the rebased offset")
        if (allocated(error)) return
        call check(error, dst%validate(), "the concatenated column satisfies its own invariants")
        if (allocated(error)) return
        ! The source is left alone -- it is intent(in), and a caller appending the same chunk
        ! onto two destinations must get the same result twice.
        call check(error, src%size() == 3_int64, "the source column is unchanged")
        if (allocated(error)) return

        ! Appending nothing is a no-op rather than an error: a row group can legitimately be
        ! empty, and materialize_slice would otherwise have to special-case it.
        call empty%init(PK_INT32)
        call dst%append_from(empty)
        call check(error, dst%size() == 5_int64, "appending an empty column changes nothing")
    end subroutine test_append_from

end module test_list
