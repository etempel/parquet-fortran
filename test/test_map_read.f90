!===========================================
! Author: Elmo Tempel (elmo.tempel@ut.ee)
!===========================================
!
!> Tests for reading a `MAP` column from a Parquet file into a `parquet_map_column`.
!!
!! Every test here reads `test/fixtures/map_payloads.parquet`, which was written by
!! `tools/generate_fixtures.cpp` against the Arrow API directly -- so these are assertions against
!! a file this library did NOT write, which is what makes them catch an error the library would
!! otherwise make symmetrically in both directions. The round trip through this library's own
!! writer is `test_map_write`'s subject.
!!
!! The fixture's four rows are the same in every column, which is what lets one column's null
!! pattern be predicted from another's:
!!
!!   row 1  two entries ("alpha", "beta"), both values present
!!   row 2  the MAP ROW is null
!!   row 3  the map is PRESENT but EMPTY
!!   row 4  one entry ("solo") whose VALUE is null
!!
!! Rows 1-2 are row group 1 and rows 3-4 are row group 2, so the null/empty pair straddles the
!! boundary and a row-group-scoped read has to get it right in both halves.
module test_map_read
    use testdrive, only : new_unittest, unittest_type, error_type, check
    use parquet
    use iso_fortran_env, only : int32, int64, real32, real64
    implicit none
    private

    public :: collect_tests_parquet_map_read

    !> The fixture every test here reads.
    character(len=*), parameter :: FIX = "test/fixtures/map_payloads.parquet"

contains

    !> Registers this module's tests with test-drive.
    subroutine collect_tests_parquet_map_read(testsuite)
        type(unittest_type), allocatable, intent(out) :: testsuite(:) !! the collected tests.
        testsuite = [ &
            new_unittest("map read: the four null shapes come back distinct", test_null_shapes), &
            new_unittest("map read: every value family reads", test_every_family), &
            new_unittest("map read: duplicate keys and their order survive", test_duplicates), &
            new_unittest("map read: a temporal value family reads", test_temporal), &
            new_unittest("map read: chunked read matches the whole column", test_chunked), &
            new_unittest("map read: a filter composes with a map read", test_filtered), &
            new_unittest("map read: the shape query answers map", test_shape_query), &
            new_unittest("map read: an empty row group reads as no rows", test_row_group_bounds) &
            ]
    end subroutine collect_tests_parquet_map_read

    !> The four rows, read whole. This is the test the whole fixture is shaped around: an
    !> implementation that conflates a null row with an empty one passes everything that only
    !> counts entries, so the pair is checked directly and in both directions.
    subroutine test_null_shapes(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        logical :: ok, got

        call parquet_open_reader(reader, FIX)
        call parquet_read_column(reader, "m_int32", mc)
        call parquet_close_reader(reader)

        call check(error, mc%size() == 4_int64, "the fixture has four rows")
        if (allocated(error)) return
        call check(error, mc%element_kind() == PK_INT32, "the value kind came from the file")
        if (allocated(error)) return
        call check(error, mc%length(1_int64) == 2_int64, "row 1 holds two entries")
        if (allocated(error)) return
        call check(error, mc%is_null(2_int64), "row 2 is a null map")
        if (allocated(error)) return
        call check(error, .not. mc%is_null(3_int64), "row 3 is present")
        if (allocated(error)) return
        call check(error, mc%is_empty(3_int64), "and empty")
        if (allocated(error)) return
        call check(error, .not. mc%is_null(4_int64), "row 4 is present")
        if (allocated(error)) return
        call check(error, mc%length(4_int64) == 1_int64, "and holds its one entry")
        if (allocated(error)) return
        call check(error, mc%null_count() == 1_int64, "exactly one row is null")
        if (allocated(error)) return
        call check(error, mc%total_entries() == 3_int64, "three entries across the column")
        if (allocated(error)) return

        row = mc%view(4_int64)
        call row%get("solo", v, is_valid=ok, found=got)
        call check(error, got, "row 4's key is found")
        if (allocated(error)) return
        call check(error, .not. ok, "and its value is null")
        if (allocated(error)) return
        call check(error, mc%validate(), "the column read back satisfies its invariants")
    end subroutine test_null_shapes

    !> One read per non-temporal value family, checking row 1's two values. What this pins is the
    !> nine-way dispatch: a family wired to the wrong fill entry point produces plausible garbage.
    subroutine test_every_family(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: vi32
        integer(int64) :: vi64
        real(real32) :: vf32
        real(real64) :: vf64
        logical :: vb
        character(len=:), allocatable :: vs

        call parquet_open_reader(reader, FIX)

        call parquet_read_column(reader, "m_int32", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vi32)
        call check(error, vi32 == 0_int32, "m_int32 alpha")
        if (allocated(error)) return
        call row%get("beta", vi32)
        call check(error, vi32 == 1_int32, "m_int32 beta")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_int64", mc)
        row = mc%view(1_int64)
        call row%get("beta", vi64)
        call check(error, vi64 == 1_int64, "m_int64 beta")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_float32", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vf32)
        call check(error, vf32 == 0.5_real32, "m_float32 alpha")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_float64", mc)
        row = mc%view(1_int64)
        call row%get("beta", vf64)
        call check(error, vf64 == 0.5_real64, "m_float64 beta")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_bool", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vb)
        call check(error, vb, "m_bool alpha is .true.")
        if (allocated(error)) return
        call row%get("beta", vb)
        call check(error, .not. vb, "m_bool beta is .false.")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_string", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vs)
        call check(error, vs == "x", "m_string alpha, got '"//vs//"'")
        if (allocated(error)) return
        call row%get("beta", vs)
        call check(error, vs == "xx", "m_string beta, got '"//vs//"'")

        call parquet_close_reader(reader)
    end subroutine test_every_family

    !> `m_dup`'s row 1 is "a" -> 1, "b" -> 2, "a" -> 3, in that order. Three separate promises are
    !> pinned here at once, and only the fixture (written by another writer) can pin the first:
    !> that Parquet PRESERVES duplicates and their order, that `%get` takes the first match, and
    !> that `occurrence=` reaches the later one.
    subroutine test_duplicates(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v
        character(len=:), allocatable :: k

        call parquet_open_reader(reader, FIX)
        call parquet_read_column(reader, "m_dup", mc)
        call parquet_close_reader(reader)

        row = mc%view(1_int64)
        call check(error, row%size() == 3, "row 1 holds three entries")
        if (allocated(error)) return
        call check(error, row%key_count("a") == 2, "two of them carry key 'a'")
        if (allocated(error)) return
        call row%get("a", v)
        call check(error, v == 1_int32, "%get returns the first 'a'")
        if (allocated(error)) return
        call row%get("a", v, occurrence=2)
        call check(error, v == 3_int32, "occurrence=2 returns the second 'a'")
        if (allocated(error)) return
        ! The stored ORDER, which nothing else in the repository asserts.
        call row%key_at(1, k)
        call check(error, k == "a", "entry 1 is 'a', got '"//k//"'")
        if (allocated(error)) return
        call row%key_at(2, k)
        call check(error, k == "b", "entry 2 is 'b', got '"//k//"'")
        if (allocated(error)) return
        call row%key_at(3, k)
        call check(error, k == "a", "entry 3 is 'a' again, got '"//k//"'")
    end subroutine test_duplicates

    !> The three temporal families, whose null state lives INSIDE the element rather than in a
    !> bitmap -- so row 4's null value must come back as a null ELEMENT, not as a zero.
    subroutine test_temporal(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        type(parquet_date) :: vd
        type(parquet_time) :: vt
        type(parquet_timestamp) :: vts

        call parquet_open_reader(reader, FIX)

        call parquet_read_column(reader, "m_date", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vd)
        call check(error, vd%raw() == 19000_int32, "m_date alpha is day 19000")
        if (allocated(error)) return
        row = mc%view(4_int64)
        call row%get("solo", vd)
        call check(error, vd%is_null(), "a null date value comes back as a null element")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_time", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vt)
        ! The fixture stores microseconds; parquet_time's raw form is canonical nanoseconds of
        ! day, so one hour is 3.6e12 ns rather than the 3.6e9 us the generator appended.
        call check(error, vt%raw() == 3600000000000_int64, "m_time alpha is one hour past midnight")
        if (allocated(error)) return
        row = mc%view(4_int64)
        call row%get("solo", vt)
        call check(error, vt%is_null(), "a null time value comes back as a null element")
        if (allocated(error)) return

        call parquet_read_column(reader, "m_timestamp", mc)
        row = mc%view(1_int64)
        call row%get("alpha", vts)
        call check(error, vts%to_unix(parquet_unit_micros) == 1700000000000000_int64, &
            "m_timestamp alpha is the fixture's instant")
        if (allocated(error)) return
        row = mc%view(4_int64)
        call row%get("solo", vts)
        call check(error, vts%is_null(), "a null timestamp value comes back as a null element")

        call parquet_close_reader(reader)
    end subroutine test_temporal

    !> Reading the two row groups separately must reproduce the whole column exactly. The fixture
    !> puts the null row in group 1 and the empty row in group 2, so a chunked read that got the
    !> row-level validity wrong in only one half would still produce the right entry counts.
    subroutine test_chunked(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: whole, rg1, rg2
        type(parquet_map_row) :: row
        integer(int32) :: v

        call parquet_open_reader(reader, FIX)
        call parquet_read_column(reader, "m_int32", whole)
        call parquet_read_column_chunk(reader, "m_int32", 1_int64, rg1)
        call parquet_read_column_chunk(reader, "m_int32", 2_int64, rg2)
        call parquet_close_reader(reader)

        call check(error, rg1%size() == 2_int64, "row group 1 holds two rows")
        if (allocated(error)) return
        call check(error, rg2%size() == 2_int64, "row group 2 holds two rows")
        if (allocated(error)) return
        call check(error, rg1%size() + rg2%size() == whole%size(), &
            "the two groups account for the whole column")
        if (allocated(error)) return
        ! Group 1 = rows 1-2 of the whole column: two entries, then the null row.
        call check(error, rg1%length(1_int64) == 2_int64, "group 1 row 1 holds two entries")
        if (allocated(error)) return
        call check(error, rg1%is_null(2_int64), "group 1 row 2 is the null map")
        if (allocated(error)) return
        ! Group 2 = rows 3-4: the empty row, then the null-valued entry.
        call check(error, .not. rg2%is_null(1_int64), "group 2 row 1 is present")
        if (allocated(error)) return
        call check(error, rg2%is_empty(1_int64), "and empty")
        if (allocated(error)) return
        call check(error, rg2%length(2_int64) == 1_int64, "group 2 row 2 holds one entry")
        if (allocated(error)) return
        row = rg1%view(1_int64)
        call row%get("beta", v)
        call check(error, v == 1_int32, "a chunked read's values match the whole-column read")
    end subroutine test_chunked

    !> A row filter composes with a map read exactly as with any other column: the map column
    !> comes back holding only the surviving rows, in file order.
    subroutine test_filtered(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_filter) :: filt
        type(parquet_map_column), target :: mc
        type(parquet_map_row) :: row
        integer(int32) :: v

        call filt%add("rowid >= 2")
        call parquet_open_reader(reader, FIX, filter=filt)
        call parquet_read_column(reader, "m_int32", mc)
        call parquet_close_reader(reader)

        ! rowid is 0..3, so rows 3 and 4 of the file survive: the empty map and the null-valued one.
        call check(error, mc%size() == 2_int64, "two rows survive the filter")
        if (allocated(error)) return
        call check(error, mc%is_empty(1_int64) .and. .not. mc%is_null(1_int64), &
            "the first surviving row is the present-but-empty one")
        if (allocated(error)) return
        call check(error, mc%length(2_int64) == 1_int64, "the second holds its one entry")
        if (allocated(error)) return
        row = mc%view(2_int64)
        call check(error, row%contains_key("solo"), "and that entry's key survived with it")
        if (allocated(error)) return
        call check(error, mc%validate(), "the filtered column satisfies its invariants")
        v = 0_int32
    end subroutine test_filtered

    !> `parquet_get_column_shape` answers `"map"` from the schema alone, and
    !> `parquet_get_column_type` answers `"unknown"` -- the split Phase 4 established for a struct
    !> column and the reason the table layer still declines a map.
    subroutine test_shape_query(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        character(len=:), allocatable :: txt

        call parquet_open_reader(reader, FIX)
        call parquet_get_column_shape(reader, "m_int32", txt)
        call check(error, txt == "map", "a map column's shape is 'map', got '"//txt//"'")
        if (allocated(error)) return
        call parquet_get_column_type(reader, "m_int32", txt)
        call check(error, trim(txt) == "unknown", &
            "a map column has no single element type, got '"//trim(txt)//"'")
        if (allocated(error)) return
        call check(error, parquet_column_exists(reader, "m_int32"), "the map column exists")
        if (allocated(error)) return
        call check(error, .not. parquet_column_exists(reader, "m_int32", types="int32"), &
            "but matches no scalar types= token, which is what keeps the table layer declining it")
        call parquet_close_reader(reader)
    end subroutine test_shape_query

    !> The row-group-scoped read reports the same row counts the reader does, so a caller can size
    !> its loop from the file rather than from the column it is about to read.
    subroutine test_row_group_bounds(error)
        type(error_type), allocatable, intent(out) :: error !! test-drive error handle.
        type(parquet_reader) :: reader
        type(parquet_map_column), target :: mc
        integer(int64) :: nrg, rg, total, nrows

        call parquet_open_reader(reader, FIX)
        call parquet_get_num_row_groups(reader, nrg)
        call check(error, nrg == 2_int64, "the fixture has two row groups")
        if (allocated(error)) return
        total = 0_int64
        do rg = 1_int64, nrg
            call parquet_read_column_chunk(reader, "m_string", rg, mc)
            total = total + mc%size()
        end do
        call parquet_get_nrows(reader, nrows)
        call check(error, total == nrows, "the row groups account for every row of the file")
        call parquet_close_reader(reader)
    end subroutine test_row_group_bounds

end module test_map_read
